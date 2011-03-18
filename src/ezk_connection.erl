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
-export([create/2, create/3, delete/1, set/2, get/1, ls2/1, ls/1, die/0]).
%functions with watches
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


init([Ip, Port, WantedTimeout]) ->
    ?LOG(1, "Connection: Server starting"),
    ?LOG(3, "IP: ~w , Port: ~w, Timeout: ~w.",[Ip,Port,WantedTimeout]),    
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
	    ?LOG(1, "Connection: Startup complete",[]),
	    ?LOG(3, "Connection: Initial State : ~w",[InitialState])
    end,
    erlang:send_after(?HEARTBEATTIME, self(), heartbeat),
    {ok, InitialState}.

die() ->
   erlang:exit(?SERVER, "You stood in my way!").

%% {ok, Path} where Path = String
create(Path, Data) ->
   gen_server:call(?SERVER, {command, {create, Path, Data, []}}).

%% Typ = e | s | es
create(Path, Data, Typ) ->
   gen_server:call(?SERVER, {command, {create, Path, Data, Typ}}).
%% {ok, Path}
delete(Path) ->
   gen_server:call(?SERVER, {command, {delete,  Path, []}}).

delete_all(Path) ->
   macro_delete_all_childs(Path).    

%% {ok, {Name, [Data]}} where Name = String
%% and Data = [{argument, Value}]
get(Path) ->
   gen_server:call(?SERVER, {command, {get, Path}}).

get(Path, WatchOwner, WatchMessage) ->
    gen_server:call(?SERVER, {watchcommand, {get, getw, Path, {data, WatchOwner,
                                                              WatchMessage}}}).
%% {ok, [Data]} with Data like at get
set(Path, Data) ->
   gen_server:call(?SERVER, {command, {set, Path, Data}}).

%% {ok, [ChildName]} where ChildName = <<"Name">>
ls(Path) ->
   gen_server:call(?SERVER, {command, {ls, Path}}).

ls(Path, WatchOwner, WatchMessage) ->
    ?LOG(3,"Connection: Send lsw"),
    gen_server:call(?SERVER, {watchcommand, {ls, lsw,  Path, {child, WatchOwner, 
							      WatchMessage}}}).
ls2(Path) ->
   gen_server:call(?SERVER, {command, {ls2, Path}}).

ls2(Path, WatchOwner, WatchMessage) ->
    gen_server:call(?SERVER, {watchcommand, {ls2, ls2w, Path ,{child, WatchOwner, 
							       WatchMessage}}}).

info_get_iterations() ->
    gen_server:call(?SERVER, {info, get_iterations}).
handle_call({info, get_iterations}, _From, State) ->
    {reply, {ok, State#cstate.iteration}, State};
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

handle_call(Rest, _From, State) ->
    ?LOG(1,"FOUND : ~w",[Rest]),
    {noreply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Port, Info}, State) ->
    ?LOG(1, "Connection: Got a message from Server"), 
    TypedMessage = ezk_packet_2_message:get_message_typ(Info), 
    ?LOG(3, "Connection: Typedmessage is ~w",[TypedMessage]),     
    case TypedMessage of 
        {heartbeat, _Heartbeat} ->
            ?LOG(4, "Got a Heartbeat"),
            Outstanding = State#cstate.outstanding_heartbeats,
            NewState = State#cstate{outstanding_heartbeats = Outstanding-1},
	    ok = inet:setopts(State#cstate.socket,[{active,once}]),
            {noreply, NewState};
						%Watchevents
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
						%Other Messages
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

handle_info(heartbeat, State) ->
    case State#cstate.outstanding_heartbeats of
	0 ->
            ?LOG(4, "Send a Heartbeat"),
            Heartbeat = << 255,255,255,254, 11:32>>,
	    gen_tcp:send(State#cstate.socket, Heartbeat),
            NewState = State#cstate{outstanding_heartbeats = 1},
	    erlang:send_after(?HEARTBEATTIME, self(), heartbeat),
	    {noreply, NewState};
        _Else ->
	    error
    end.

terminate(_Reason, State) ->
    Watchtable = State#cstate.watchtable,
    AllWatches = ets:match(Watchtable, '$1'),
    lists:map(fun({Data, WO, WM}) -> WO ! {watchlost, WM, Data} end, AllWatches),
    ?LOG(1,"Connection: TERMINATING"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
