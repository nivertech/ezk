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

-export([create/2, create/3, delete/1, set/2, get/1, ls2/1, ls/1, die/0]).
-include_lib("../include/ezk.hrl").

-define(SERVER, ?MODULE). 
-define(HEARTBEATTIME, 10000).

start_link(Args) ->
    ?LOG(1,"Connection: Start link called with Args: ~w",[Args]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).


init([Ip, Port, WantedTimeout]) ->
    ?LOG(1, "Connection: Server starting"),    
    {ok, Socket} = gen_tcp:connect(Ip,Port,[binary,{packet,4}]),
    HandshakePacket = <<0:64, WantedTimeout:64, 0:64, 16:64, 0:128>>,
    ok = gen_tcp:send(Socket, HandshakePacket),
    ok = inet:setopts(Socket,[{active,once}]),
    receive
	{tcp,Socket,Reply} ->
	    <<RealTimeout:64, SessionId:64, 16:32, _Hash:128>> = Reply,
	    InitialState  = #cstate{  
	      socket = Socket, ip = Ip, 
	      port = Port, timeout = RealTimeout,
	      sessionid = SessionId, iteration = 1},        
	    ok = inet:setopts(Socket,[{active,once}]),
	    ?LOG(1, "Connection: Startup complete",[]),
	    ?LOG(3, "Connection: Initial State : ~w",[InitialState])
    end,
    erlang:send_after(?HEARTBEATTIME, self(), heartbeat),
    {ok, InitialState}.

die() ->
   erlang:exit(?SERVER, "You stood in my way!").

create(Path, Data) ->
   gen_server:call(?SERVER, {command, {create, Path, Data, []}}).

create(Path, Data, Typ) ->
   gen_server:call(?SERVER, {command, {create, Path, Data, Typ}}).

delete(Path) ->
   gen_server:call(?SERVER, {command, {delete,  Path}}).

get(Path) ->
   gen_server:call(?SERVER, {command, {get, Path}}).

%% get(Path, WatchOwner, WatchMessage) ->
%%    gen_server:cast(?SERVER, {getw, Path, WatchOwner, WatchMessage}).

set(Path, Data) ->
   gen_server:call(?SERVER, {command, {set, Path, Data}}).

ls(Path) ->
   gen_server:call(?SERVER, {command, {ls, Path}}).

%% ls(Path, WatchOwner, WatchMessage) ->
%%    gen_server:cast(?SERVER, {lsw, Path ,WatchOwner, WatchMessage}).

ls2(Path) ->
   gen_server:call(?SERVER, {command, {ls2, Path}}).

%% ls2(Path, WatchOwner, WatchMessage) ->
%%    gen_server:cast(?SERVER, {ls2w, Path ,WatchOwner, WatchMessage}).

handle_call({command, Args}, From, State) ->
    Iteration = State#cstate.iteration,
    {ok, CommId, Path, Packet} = ezk_message_2_packet:make_packet(Args, Iteration),
    gen_tcp:send(State#cstate.socket, Packet),
    ?LOG(1, "Connection: Packet send"),
    NewOpen  = dict:store(Iteration, {CommId, Path, From}, State#cstate.open_requests),
    ?LOG(3, "Connection: Saved open Request."),
    NewState = State#cstate{iteration = Iteration+1, open_requests = NewOpen },    
    ?LOG(3, "Connection: Returning to wait status"),  
    {noreply, NewState}.

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
	{watchevent} -> 
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

terminate(_Reason, _State) ->
    ?LOG(1,"Connection: TERMINATING"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
