%% -------------------------------------------------------------------
%%
%% ezk_connection: A GenServer to manage the connection. It has the access to the 
%%                 Socket (stored in the State), keeps track of send requests,
%%                 and manages the watches. The Main Module.
%%
%% Copyright (c) 2011 Marco Grebe. All Rights Reserved.
%% Copyright (c) 2011 global infinipool GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(ezk_connection).

-behaviour(gen_server).

-record(cstate, {open_requests = dict:new(), 
		 socket :: port(), 
		 ip, 
		 port :: 0..65535, 
		 timeout, 
		 sessionid, 
		 iteration :: pos_integer(),
		 outstanding_heartbeats = 0,
		 outstanding_auths = 0,
		 watchtable,
		 heartbeattime
 	       }).

%% API
-export([start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([establish_connection/4]).


-include_lib("../include/ezk.hrl").

-define(SERVER, ?MODULE). 
-define(HEARTBEATTIME, 10000).

start(Args) ->
    ?LOG(1,"Connection: Start link called with Args: ~w",[Args]),
    gen_server:start(?MODULE, Args , []).


%% inits the random function and chooeses a Server from the list.
%% then establishes a connection to that server.
%% returns {ok, State} or {error, ErrorMessage} or {unknown, Message, ErrorCode}
init([Servers, TryTimes]) ->
    random:seed(erlang:now()),
    K = n_init_trys(Servers, TryTimes),
    ?LOG(1, "Connection established"), 
    K.

n_init_trys(_Servers, 0) ->
    error;
n_init_trys(Servers, N) ->
    ?LOG(1,"Connect init : incomming args: ~w",[Servers]),
    WhichServer = random:uniform(length(Servers)),
    ?LOG(0,"Choose server ~w",[WhichServer]),
    {Ip, Port, WantedTimeout, HeartBeatTime} =  lists:nth(WhichServer, Servers),
    case establish_connection(Ip, Port, WantedTimeout, HeartBeatTime) of
	{ok, State} ->
	    {ok, State};
	error ->
	    n_init_trys(Servers, N-1)
    end.


%% Handles calls for Number of Iteration
handle_call({info, get_iterations}, _From, State) ->
    {reply, {ok, State#cstate.iteration}, State};
handle_call({info, get_watches}, _From, State) ->
    {reply, {ok, ets:first(State#cstate.watchtable)}, State};
%% Handles normal commands (get/1, set/2, ...) by
%% a) determinate the corresponding packet to send this request to zk_server
%% b) Save that the Request was send in the open_requests dict (key: actual iteration)
%% c) set noreply. Real answer is triggered by incoming tcp message
handle_call({command, Args}, From, State) ->
    Iteration = State#cstate.iteration,
    {ok, CommId, Path, Packet} = ezk_message_2_packet:make_packet(Args, Iteration),
    gen_tcp:send(State#cstate.socket, Packet),
    ?LOG(1, "Connection: Packet send."),
    ?LOG(1, "Connection: Command started by ~w.",[From]),
    NewOpen  = dict:store(Iteration, {CommId, Path, {blocking, From}}, State#cstate.open_requests),
    ?LOG(3, "Connection: Saved open Request."),
    NewState = State#cstate{iteration = Iteration+1, open_requests = NewOpen },    
    ?LOG(3, "Connection: Returning to wait status"),  
    {noreply, NewState};
%% Handles commands which set a watch(get/3, ls/3, ...) by
%% a) Save an entry in the watchtable (key: {Typ, Path})
%% b) Look up if already a watch of this type is set to the node.
%% c) if yes the command is user without setting a new one.
handle_call({watchcommand, {Command, CommandW, Path, {WType, WO, WM}}}, From, State) ->
    ?LOG(1," Connection: Got a WatchSetter"),
    Watchtable = State#cstate.watchtable,
    AllIn = ets:lookup(Watchtable, {WType,Path}),
    ?LOG(3," Connection: Searched Table"),
    true = ets:insert(Watchtable, {{WType, Path}, WO, WM}),
    ?LOG(3," Connection: Inserted new Entry: ~w",[{{WType, Path}, WO, WM}]),
    case AllIn of
	[] -> 
	    ?LOG(3," Connection: Search got []"),
	    handle_call({command, {CommandW, Path}}, From, State);
	_Else -> 
	    ?LOG(3," Connection: Already Watches set to this path/typ"),
	    handle_call({command, {Command, Path}}, From, State)
    end;
%% Handles orders to die by dying
handle_call({die, Reason}, _From, State) ->
    ?LOG(3," Connection: exiting myself"),
    {stop, Reason, ok, State};
%% Handles auth requests
handle_call({addauth, Scheme, Auth}, From, State) ->
    OutstandingAuths = State#cstate.outstanding_auths,
    case OutstandingAuths of
	1 ->
	    {reply,  {error, auth_in_progress}, State};
	0 ->
	    {ok, Packet} = ezk_message_2_packet:make_addauth_packet({add_auth, Scheme,
								     Auth}),
	    gen_tcp:send(State#cstate.socket, Packet),
	    NewOpen  = dict:store(auth, From, State#cstate.open_requests),
	    NewState = State#cstate{outstanding_auths = 1, open_requests = NewOpen },   
	    {noreply, NewState}
    end.
%% handles non blocking commands
%% the difference in handling compared with blocking commands is the prefix
%% nonblocking in the open requests table which gets the server to answer with
%% a normal message sending instead of the gen_server:reply command.
handle_cast({nbcommand, Args, Receiver, Tag}, State) ->
    Iteration = State#cstate.iteration,
    {ok, CommId, Path, Packet} = ezk_message_2_packet:make_packet(Args, Iteration),
    gen_tcp:send(State#cstate.socket, Packet),
    ?LOG(1, "Connection: Packet send"),
    NewOpen  = dict:store(Iteration, {CommId, Path, {nonblocking, Receiver, Tag}},
			  State#cstate.open_requests),
    ?LOG(3, "Connection: Saved open Request."),
    NewState = State#cstate{iteration = Iteration+1, open_requests = NewOpen },    
    ?LOG(3, "Connection: Returning to wait status"),  
    {noreply, NewState}.
%% tcp events arrive
%% parses the first part of the message and determines of which type it is and then does
%% the corresponding (see below).
handle_info({tcp, _Port, Info}, State) ->
    ?LOG(3, "Connection: Got a message from Server"), 
    TypedMessage = ezk_packet_2_message:get_message_typ(Info), 
    ?LOG(3, "Connection: Typedmessage is ~w",[TypedMessage]),     
    handle_typed_incomming_message(TypedMessage, State);
%% Its time to let the Heart bump one more time
handle_info(heartbeat, State) ->
    case State#cstate.outstanding_heartbeats of
	%% if there is no outstanding Heartbeat everything is ok.
	%% The new beat is send and a notice left when the next bump is scheduled
	0 ->
            ?LOG(4, "Send a Heartbeat"),
            Heartbeat = << 255,255,255,254, 11:32>>,
	    gen_tcp:send(State#cstate.socket, Heartbeat),
            NewState = State#cstate{outstanding_heartbeats = 1},
	    HeartBeatTime = State#cstate.heartbeattime,
	    erlang:send_after(HeartBeatTime, self(), heartbeat),
	    {noreply, NewState};
	%% Last bump got no reply. Thats bad.
        _Else ->
	    ezk:die("Heartattack")
    end.

%% if server dies all owners who are waiting for watchevents get a Message
%% M = {watchlost, WatchMessage, Data}.
%% All Outstanding requests are answered with {error, client_broke, CommId, Path}
terminate(_Reason, State) ->
    Watchtable = State#cstate.watchtable,
    AllWatches = ets:match(Watchtable, '$1'),
    lists:map(fun({Data, WO, WM}) -> 
		      WO ! {watchlost, WM, Data} 
	      end, AllWatches),

    OpenRequests = State#cstate.open_requests,
    Keys = dict:fetch_keys(OpenRequests),
    lists:map(fun(Key) -> 
		      {CommId, Path, From}  = dict:fetch(Key, OpenRequests),
		      From ! {error, client_broke, CommId, Path}
	      end, Keys),
    ?LOG(1,"Connection: TERMINATING"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Updates the Watchtable if a watchevent arrived and sends the Message to the owner
send_watch_events_and_erase_receivers(Table, Receivers, Path, Typ, SyncCon) ->
    case Receivers of
	[] ->
            ?LOG(1, "Connection: Receiver List completely processed"),
	    ok;
	[{Key, WatchOwner, WatchMessage}|T] ->
            ?LOG(1, "Connection: Send something to ~w", [WatchOwner]),
            WatchOwner ! {WatchMessage, {Path, Typ, SyncCon}},
	    ets:delete(Table, Key),
	    send_watch_events_and_erase_receivers(Table, T, Path, Typ, SyncCon)
    end.  

%% Sets up a connection, performs the Handshake and saves the data to the initial State 
establish_connection(Ip, Port, WantedTimeout, HeartBeatTime) ->
    ?LOG(1, "Connection: Server starting"),
    ?LOG(3, "Connection: IP: ~s , Port: ~w, Timeout: ~w.",[Ip,Port,WantedTimeout]),  
    case  gen_tcp:connect(Ip,Port,[binary,{packet,4}]) of
	{ok, Socket} ->
	    ?LOG(3, "Connection: Socket open"),    
	    HandshakePacket = <<0:64, WantedTimeout:64, 0:64, 16:64, 0:128>>,
	    ?LOG(3, "Connection: Handshake build"),    
	    ok = gen_tcp:send(Socket, HandshakePacket),
	    ?LOG(3, "Connection: Handshake send"),    
	    ok = inet:setopts(Socket,[{active,once}]),
	    ?LOG(3, "Connection: Channel set to Active"),    
	    receive
		{tcp,Socket,Reply} ->
		    ?LOG(3, "Connection: Handshake Reply there"),    
		    <<RealTimeout:64, SessionId:64, 16:32, _Hash:128>> = Reply,
		    Watchtable    = ets:new(watchtable, [duplicate_bag, private]),
		    InitialState  = #cstate{  
		      socket = Socket, ip = Ip, 
		      port = Port, timeout = RealTimeout,
		      sessionid = SessionId, iteration = 1,
		      watchtable = Watchtable, heartbeattime = HeartBeatTime},   
		    ?LOG(3, "Connection: Initial state build"),         
		    ok = inet:setopts(Socket,[{active,once}]),
		    ?LOG(3, "Connection: Startup complete",[]),
		    ?LOG(3, "Connection: Initial State : ~w",[InitialState])
	    end,
	    erlang:send_after(HeartBeatTime, self(), heartbeat),
	    ?LOG(3,"Connection established with server ~s, ~w ~n",[Ip, Port]),
	    {ok, InitialState};
	_Else ->
	    error
    end.
	    

%%% heartbeatreply: decrement the number of outstanding Heartbeats.
handle_typed_incomming_message({heartbeat,_HeartBeat}, State) -> 
    ?LOG(4, "Got a Heartbeat"),
    Outstanding = State#cstate.outstanding_heartbeats,
    NewState = State#cstate{outstanding_heartbeats = Outstanding-1},
    ok = inet:setopts(State#cstate.socket,[{active,once}]),
    {noreply, NewState};    
%%% Watchevents happened:
%%% a) parse it by using get_watch_data
%%% b) Look at the watchtable if this event was supposed to arive
%%% c) use send_... to send the event to the owner
handle_typed_incomming_message({watchevent, Payload}, State) ->
    ?LOG(1,"Connection: A Watch Event arrived. Halleluja"),
    {Typ, Path, SyncCon} = ezk_packet_2_message:get_watch_data(Payload), 
    Watchtable = State#cstate.watchtable,
    ?LOG(3,"Connection: Got the data of the watchevent: ~w",[{Typ, Path, SyncCon}]),
    Receiver = ets:lookup(Watchtable, {Typ, Path}),
    ?LOG(3,"Connection: Receivers are: ~w",[Receiver]),
    ok = send_watch_events_and_erase_receivers(Watchtable, Receiver, Path,
					       Typ, SyncCon),
    ?LOG(3,"Connection: the first element in WT ~w",[ets:first(Watchtable)]), 
    ?LOG(3,"Connection: Receivers notified"),
    ok = inet:setopts(State#cstate.socket,[{active,once}]),
    {noreply, State};
%%% Answers to normal requests (set, get,....)
%%% a) Look at the dict if there is a corresponding open request
%%% b) Erase it
%%% c) Parse the Payload 
%%% d) Send the answer
handle_typed_incomming_message({normal, MessId, _Zxid, PayloadWithErrorcode}, State) ->
    ?LOG(3, "Connection: Normal Message"),  
    {ok, {CommId, Path, From}}  = dict:find(MessId, State#cstate.open_requests),
    ?LOG(1, "Connection: Found dictonary entry: CommId = ~w, Path = ~w, From = ~w",[CommId, Path, From]),
    NewDict = dict:erase(MessId, State#cstate.open_requests),
    NewState = State#cstate{open_requests = NewDict},
    ?LOG(3, "Connection: Dictionary updated"),
    Reply = ezk_packet_2_message:replymessage_2_reply(CommId, Path,
						      PayloadWithErrorcode),
    ?LOG(3, "Connection: determinated reply"),
    ok = inet:setopts(State#cstate.socket,[{active,once}]),
    case From of
	{blocking, PId} ->
	    ?LOG(1, "Connection: Trying to send to ~w",[PId]),
	    gen_server:reply(PId, Reply);
	{nonblocking, ReceiverPId, Tag} ->
	    ReceiverPId ! {Tag, Reply}
    end,
    ?LOG(1, "Connection: Starting if in handle typed normal"),
    {noreply, NewState};
%%% Answers to a addauth. 
%%% if there is an errorcode then there wa an error. if not there wasn't
handle_typed_incomming_message({authreply, Errorcode}, State) ->
    {ok, From}  = dict:find(auth, State#cstate.open_requests),
    case Errorcode of
	<<0,0,0,0>> ->
	    Reply = {ok, authed};
	<<255,255,255,141>> ->
%% The first part of the Message determines the type
	    Reply = {error, auth_failed};
	Else  -> 
	    Reply = {error, unknown, Else}
    end,
    gen_server:reply(From, Reply),
    NewDict = dict:erase(auth, State#cstate.open_requests),
    NewState = State#cstate{open_requests = NewDict, outstanding_auths = 0},
    {noreply, NewState}.



