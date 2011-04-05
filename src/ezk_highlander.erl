-module(ezk_highlander).

-include_lib("../include/ezk.hrl").

-record(high_state, {connected = false, ident, my_path, child_pid, module, parameters,
		     wait_for_active = []}).

-behaviour(gen_server).

-export([start/3, get_child_id/1, failover/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([behaviour_info/1]).
-export([is_active/1, wait_until_active/1]).


behaviour_info(callbacks) ->
    [{terminate,2}, {run,3}].

start(Module, Parameters, NodeList) ->
    gen_server:start( ?MODULE, [Module, Parameters, NodeList], []).


init([Module, Parameters, NodeList]) ->
    %% ensure the needed Paths
    ?LOG(1,"Highlander: Init: got parameters module, Para, NodeList: ~w, ~w, ~w",
	 [Module, Parameters, NodeList]), 
    lists:map(fun(Path) ->
		      Father = get_father(Path),
		      ?LOG(1,"Highlander: erstelle Pfad ~s",[Father]),
		      ezk:ensure_path(Father) 
	      end, NodeList),
    %% set initial state
    Ident = pid_to_list(self())++ " " ++ atom_to_list(node()),
    State = #high_state{ident = Ident, my_path = "", module = Module, 
			parameters = Parameters, child_pid = 0},
    %% first try to get at every node
    case try_first(NodeList, Ident) of
	no_luck ->
	    {ok, State};
	{ok, Path} ->
	    ?LOG(1, "Highlander:  Init: ~w trying to start its child", [Ident]),
	    {ok, ChildPid} = start_child(Module, Parameters, State),
	    {ok, State#high_state{connected = true, my_path = Path, child_pid = ChildPid}}
    end.

is_active(PId) ->
    gen_server:call(PId, isactive).

wait_until_active(PId) ->
    gen_server:call(PId, wait_until_active, infinity).

get_child_id(PId) ->
    gen_server:call(PId, get_child_id).

failover(PId, Reason) ->
    gen_server:call(PId, {failover, Reason}).

terminate(Reason, State) ->
    ?LOG(1, "Highlander: Failover  of ~w",[self()]),
    ChildPId = State#high_state.child_pid,
    Module = State#high_state.module,
    Module:terminate(ChildPId, Reason),
    ezk:delete(State#high_state.my_path),
    timer:sleep(2000),
    erlang:exit(ChildPId, Reason),
    exit(Reason).

handle_call({failover, Reason}, _From, State) ->    
    {stop, Reason, ok, State};
handle_call(get_child_id, _From, State) -> 
    case State#high_state.connected of
	true ->
	    {reply, {ok, State#high_state.child_pid}, State};
	false ->
	    {reply, error, State}
    end;
handle_call(isactive, _From, State) ->
    {reply, State#high_state.connected, State};
handle_call(wait_until_active, From, State) ->
    case State#high_state.connected of
	true ->
	    ok;
	false ->
	    NewWaiter = [From | State#high_state.wait_for_active],
	    {noreply, State#high_state{wait_for_active = NewWaiter}}
    end.

%% handles nodechanges and try to get a highlander
handle_info({{nodechanged, Path}, _I}, State) ->
    ?LOG(1," Highlander: nodechangenotify: ~w got one for ~s",[self(), Path]),
    case State#high_state.connected of
	true ->
	    ?LOG(1,"~w is already a highlander", [self()]),
	    {noreply, State};
	false ->
	    case (try_to_get(Path, State#high_state.ident)) of
		{ok, Path} ->
		    ?LOG(1,"~w was lucky in retry", [self()]),
		    Module = State#high_state.module,
		    Parameters = State#high_state.parameters,
		    {ok, ChildPid} = start_child(Module, Parameters, State),
		    NewState = State#high_state{connected = true, my_path = Path,
						child_pid = ChildPid},
		    {noreply, NewState};	     
		{error, _I1} ->
		    ?LOG(1,"~w was not lucky in retry", [self()]),
		    {noreply, State};
		Else  ->
		    ?LOG(1,"~w got the reply ~w by trying to get highlander",
			 [self(), Else]),
		    {noreply, State}
	    end
    end;
handle_info(got_active, State) ->
    lists:map(fun(PId) ->
		      gen_server:reply(PId, ok) end, State#high_state.wait_for_active),
    NewState = State#high_state{wait_for_active = []},
    {noreply, NewState};
handle_info(Something, State) ->
    ?LOG(1,"Something unexpected arrived: ~w",[Something]),
    {noreply, State}.


handle_cast(_A, State) ->
    {noreply, State}.


%% starts the highlandchild
start_child(Module, Parameters, State) ->
    Father = self(),
    Path   = State#high_state.my_path,
    Child  = spawn_link(Module, run ,[Father, Path,  Parameters]),
    Father ! got_active,
    {ok, Child}.

%% makes a first try to get highlander with every node in the list.
%% After this every father has a watch.
try_first([], Ident) ->
    ?LOG(1, "Highlander: First Try: ~s tried all without luck",[Ident]),
    no_luck;
try_first([Node | NodeList] , Ident) ->
    case (try_to_get(Node, Ident)) of
	{ok, Path} ->
	    ?LOG(1, "Highlander: First Try: ~s got lucky",[Ident]),
	    {ok, Path};
	{error, _I}  ->
	    try_first(NodeList, Ident)
    end.  

%% Trys to create a the in Path specified node with data Ident.
%% Also sets a childwatch to its father with message {nodechanged, Path}.
try_to_get(Path, Ident) ->
    Father = get_father(Path),
    ezk:ls(Father, self(), {nodechanged, Path}),
    ?LOG(1, "Highlander: Watch set by ~w",[self()]),
    ezk:create(Path, Ident, e).

get_father(Path) ->
    FullPath = string:tokens(Path, "/"),
    get_father_from_list(FullPath).

get_father_from_list([_H]) ->
    "";
get_father_from_list([ H | T ]) ->
    "/" ++ H ++ get_father_from_list(T).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
