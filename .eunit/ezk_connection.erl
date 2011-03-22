%%%-------------------------------------------------------------------
%%% @author Marco <marco@gauss>
%%% @copyright (C) 2011, Marco
%%% @doc
%%%
%%% @end
%%% Created : 14 Mar 2011 by Marco <marco@gauss>
%%%-------------------------------------------------------------------
-module(ezk_connection).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
%normal functions
-export([create/2, create/3, create/4, delete/1, set/2, set_acl/2, get/1, get_acl/1,
	 ls2/1, ls/1, die/1]).
%functions dealing with watches
-export([ls/3, get/3, ls2/3]).
%macros
-export([delete_all/1]).
%infos
-export([info_get_iterations/0]).

-include_lib("../include/ezk.hrl").

-define(SERVER, ?MODULE). 
-define(HEARTBEATTIME, 10000).

start_link(Args) ->
    ?LOG(1,"Connection: Start link called with Args: ~w",[Args]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).


%% inits the random function and chooeses a Server from the list.
%% then establishes a connection to that server.
%% returns {ok, State} or {error, ErrorMessage} or {unknown, Message, ErrorCode}
init(Servers) ->
    random:seed(erlang:now()),
    ?LOG(1,"Connect init : incomming args: ~w",[Servers]),
    WhichServer = random:uniform(length(Servers)),
    ?LOG(0,"Choose server ~w",[WhichServer]),
    {Ip, Port, WantedTimeout} =  lists:nth(WhichServer, Servers),
    establish_connection(Ip, Port, WantedTimeout).
    
%% Kills the Server (not the supervisor!)
die(Reason) -> 
    gen_server:call(?SERVER, {exit, Reason}).

%%--------------------------- Zookeeper Functions ---------------------
%% Return {ok, Reply}.
%% These functions are all synchronous.

%% Creates a new ZK_Node
%% Reply = Path where Path = String
create(Path, Data) ->
   gen_server:call(?SERVER, {command, {create, Path, Data, [], [undef]}}).
%% Typ = e | s | es (stands for etheremal, sequenzed or both)
create(Path, Data, Typ) ->
   gen_server:call(?SERVER, {command, {create, Path, Data, Typ, [undef]}}).
%% Acls = [Acl] where Acl = {Scheme, Id, Permission} 
%% with Scheme and Id = String
%% and Permission = [Per] | String 
%% where Per = r | w | c | d | a
create(Path, Data, Typ, Acls)  ->
   gen_server:call(?SERVER, {command, {create, Path, Data, Typ, Acls}}).

%% Deletes a ZK_Node
%% Only working if Node has no children.
%% Reply = Path where Path = String
delete(Path) ->
   gen_server:call(?SERVER, {command, {delete,  Path, []}}).

%% Deletes a ZK_Node and all his childs.
%% Reply = Path where Path = String
delete_all(Path) ->
   macro_delete_all_childs(Path).    

%% Reply = {Data, Parameters} where Data = The Data stored in the Node
%% and Parameters = [{ParameterName, Value}]
%% where ParameterName = czxid | mzxid | pzxid | ctime | mtime | dataversion | 
%%                       datalength | number_children | cversion | aclversion
get(Path) ->
   gen_server:call(?SERVER, {command, {get, Path}}).
%% Like the one above but sets a datawatch to Path.
%% If watch is triggered a Message M is send to the PId WatchOwner
%% M = {WatchMessage, {Path, Type, SyncCon}
%% with Type = child
get(Path, WatchOwner, WatchMessage) ->
    gen_server:call(?SERVER, {watchcommand, {get, getw, Path, {data, WatchOwner,
							       WatchMessage}}}).

%% Returns the actual Acls of a Node
%% Reply = {[ACL],Parameters} with ACl and Parameters like above
get_acl(Path) ->
    gen_server:call(?SERVER, {command, {get_acl, Path}}).

%% Sets new Data in a Node. Old ones are lost.
%% Reply = Parameters with Data like at get
set(Path, Data) ->
   gen_server:call(?SERVER, {command, {set, Path, Data}}).

%% Sets new Acls in a Node. Old ones are lost.
%% ACL like above.
%% Reply = Parameters with Data like at get
set_acl(Path, Acls) ->
    gen_server:call(?SERVER, {command, {set_acl, Path, Acls}}).

%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = [ChildName] where ChildName = <<"Name">>
ls(Path) ->
   gen_server:call(?SERVER, {command, {ls, Path}}).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls(Path, WatchOwner, WatchMessage) ->
    ?LOG(3,"Connection: Send lsw"),
    gen_server:call(?SERVER, {watchcommand, {ls, lsw,  Path, {child, WatchOwner, 
							      WatchMessage}}}).

%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = {[ChildName],Parameters} with Parameters and ChildName like above.
ls2(Path) ->
   gen_server:call(?SERVER, {command, {ls2, Path}}).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls2(Path, WatchOwner, WatchMessage) ->
    gen_server:call(?SERVER, {watchcommand, {ls2, ls2w, Path ,{child, WatchOwner, 
							       WatchMessage}}}).

%% Returns the Actual Transaction Id of the Client.
%% Reply = Iteration = Int.
info_get_iterations() ->
    gen_server:call(?SERVER, {info, get_iterations}).

%% Handles calls for Number of Iteration
handle_call({info, get_iterations}, _From, State) ->
    {reply, {ok, State#cstate.iteration}, State};
%% Handles normal commands (get/1, set/2, ...) by
%% a) determinate the corresponding packet to send this request to zk_server
%% b) Save that the Request was send in the open_requests dict (key: actual iteration)
%% c) set noreply. Real answer is triggered by incoming tcp message
handle_call({command, Args}, From, State) ->
    Iteration = State#cstate.iteration,
    {ok, CommId, Path, Packet} = ezk_message_2_packet:make_packet(Args, Iteration),
    gen_tcp:send(State#cstate.socket, Packet),
    ?LOG(1, "Connection: Packet send"),
    NewOpen  = dict:store(Iteration, {CommId, Path, From}, State#cstate.open_requests),
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
	    ?LOG(3," Connection: Already Watches set tot this path/typ"),
	    handle_call({command, {Command, Path}}, From, State)
    end;
%% Handles orders to die by dying
handle_call({exit, Reason}, _From, _State) ->
    Self = self(),
    erlang:exit(Self, Reason).

%% useless
handle_cast(_Msg, State) ->
    {noreply, State}.

%% tcp events arrive
%% parses the first part of the message and determines of which type it is and then does
%% the corresponding (see below).
handle_info({tcp, _Port, Info}, State) ->
    ?LOG(1, "Connection: Got a message from Server"), 
    TypedMessage = ezk_packet_2_message:get_message_typ(Info), 
    ?LOG(3, "Connection: Typedmessage is ~w",[TypedMessage]),     
    case TypedMessage of 
%%% heartbeatreply: decrement the number of outstanding Heartbeats.
        {heartbeat, _Heartbeat} ->
            ?LOG(4, "Got a Heartbeat"),
            Outstanding = State#cstate.outstanding_heartbeats,
            NewState = State#cstate{outstanding_heartbeats = Outstanding-1},
	    ok = inet:setopts(State#cstate.socket,[{active,once}]),
            {noreply, NewState};
%%% Watchevents happened:
%%% a) parse it by using get_watch_data
%%% b) Look at the watchtable if this event was supposed to arive
%%% c) use send_... to send the event to the owner
	{watchevent, Payload} ->
            ?LOG(1,"Connection: A Watch Event arrived. Halleluja"),
            {Typ, Path, SyncCon} = ezk_packet_2_message:get_watch_data(Payload), 
	    Watchtable = State#cstate.watchtable,
            ?LOG(3,"Connection: Got the data of the watchevent: ~w",[{Typ, Path, SyncCon}]),
	    Receiver = ets:lookup(Watchtable, {Typ, Path}),
            ?LOG(3,"Connection: Receivers are: ~w",[Receiver]),
            ok = send_watch_events_and_erase_receivers(Watchtable, Receiver, Path,
						       Typ, SyncCon),
            ?LOG(3,"Connection: Receivers notified"),
	    ok = inet:setopts(State#cstate.socket,[{active,once}]),
	    {noreply, State};
%%% Answers to normal requests (set, get,....)
%%% a) Look at the dict if there is a corresponding open request
%%% b) Erase it
%%% c) Parse the Payload 
%%% d) Send the answer
	{normal, MessId, _Zxid, PayloadWithErrorcode} ->
            ?LOG(3, "Connection: Normal Message"),  
	    {ok, {CommId, Path, From}}  = dict:find(MessId, State#cstate.open_requests),
            ?LOG(3, "Connection: Found dictonary entry"),
	    NewDict = dict:erase(MessId, State#cstate.open_requests),
	    NewState = State#cstate{open_requests = NewDict},
            ?LOG(3, "Connection: Dictionary updated"),
	    Reply = ezk_packet_2_message:replymessage_2_reply(CommId, Path,
							      PayloadWithErrorcode),
            ?LOG(3, "Connection: determinated reply"),
	    gen_server:reply(From, Reply),
	    ok = inet:setopts(State#cstate.socket,[{active,once}]),
	    {noreply, NewState}
    end;
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
	    erlang:send_after(?HEARTBEATTIME, self(), heartbeat),
	    {noreply, NewState};
	%% Last bump got no reply. Thats bad.
        _Else ->
	    die("Heartattack")
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
	    ets:delete(Table, {Key, WatchOwner, WatchMessage}),
	    send_watch_events_and_erase_receivers(Table, T, Path, Typ, SyncCon)
    end.       

%% A Macro which deletes a Node and all his Childs.
%% a) List children of Node. If he has none everything is all right.
%% b) If he has some: kill them and their Children rekursively.
%% c) Kill the Node with delete
macro_delete_all_childs(Path) ->
    ?LOG(1, "Delete All: Trying to Erase ~s",[Path]),
    {ok, Childs} = ls(Path),
    case Childs of
        [] ->
	    ok;
	ListOfChilds ->
	    ?LOG(3, "Delete All: List of Childs: ~s",[ListOfChilds]),
            case Path of
		"/" ->
		    lists:map(fun(A) ->
				      (delete_all(Path++(binary_to_list(A))))
			      end, ListOfChilds);
		_Else  -> 
		    lists:map(fun(A) ->
				      (delete_all(Path++"/"++(binary_to_list(A)))) 
                              end, ListOfChilds)
  
	    end
    end,
    ?LOG(3, "Killing ~s",[Path]),
    delete(Path).

%% Sets up a connection, performs the Handshake and saves the data to the initial State 
establish_connection(Ip, Port, WantedTimeout) ->
    ?LOG(1, "Connection: Server starting"),
    ?LOG(3, "Connection: IP: ~s , Port: ~w, Timeout: ~w.",[Ip,Port,WantedTimeout]),    
    {ok, Socket} = gen_tcp:connect(Ip,Port,[binary,{packet,4}]),
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
	    Watchtable = ets:new(watchtable, [duplicate_bag, private]),
	    InitialState  = #cstate{  
	      socket = Socket, ip = Ip, 
	      port = Port, timeout = RealTimeout,
	      sessionid = SessionId, iteration = 1,
	      watchtable = Watchtable},   
	    ?LOG(3, "Connection: Initial state build"),         
	    ok = inet:setopts(Socket,[{active,once}]),
	    ?LOG(3, "Connection: Startup complete",[]),
	    ?LOG(3, "Connection: Initial State : ~w",[InitialState])
    end,
    erlang:send_after(?HEARTBEATTIME, self(), heartbeat),
    ?LOG(3,"Connection established with server ~s, ~w ~n",[Ip, Port]),
    {ok, InitialState}.
