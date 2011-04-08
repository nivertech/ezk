
%% -------------------------------------------------------------------
%%
%% ezk_highlander: A behaviour that exactly one instance of a module per given path runs.
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

%% The startfunction.
start(Module, Parameters, NodeList) ->
    gen_server:start( ?MODULE, [Module, Parameters, NodeList], []).


%% The init function tryes every given path once and ensures that all
%% needed ZK Nodes are there. After trying once without success it goes 
%% into normal genserver state and waits for messages ( = changes in the important nodes)
%% Return type is a record of type high_state, which is the State format in this Server.
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
    %% first try to get highlander at every node
    case try_first(NodeList, Ident) of
	no_luck ->
	    {ok, State};
	{ok, Path} ->
	    ?LOG(1, "Highlander:  Init: ~w trying to start its child", [Ident]),
	    {ok, ChildPid} = start_child(Module, Parameters, State),
	    ?LOG(1, "Highlander:  Init: ~w Child started", [Ident]),
	    {ok, State#high_state{connected = true, my_path = Path, child_pid = ChildPid}}
    end.

%% Gives true or false, depending on if the instance has become a highlander.
is_active(PId) ->
    gen_server:call(PId, isactive).

%% Returns ok if the instance is an active highlander. If not the functions waits for 
%% it to become one and then returns ok.
wait_until_active(PId) ->
    gen_server:call(PId, wait_until_active, infinity).

%% Gives back {ok, PID} with the PId of the process started by the highlander 
%% if there is one. If not it gives replys error.
get_child_id(PId) ->
    gen_server:call(PId, get_child_id).

%% Triggers a stopping of the highlander.
failover(PId, Reason) ->
    gen_server:call(PId, {failover, Reason}).

%% Used if the server dies (also in case of a failover)
%% calls terminate/2 of the child. After 2 seconds kills the child
%% and the itself.
terminate(Reason, State) ->
    ?LOG(1, "Highlander: Failover  of ~w",[self()]),
    ChildPId = State#high_state.child_pid,
    Module = State#high_state.module,
    Module:terminate(ChildPId, Reason),
    ezk:delete(State#high_state.my_path),
    timer:sleep(2000),
    erlang:exit(ChildPId, Reason),
    exit(Reason).

%% Called by the failover function
handle_call({failover, Reason}, _From, State) ->    
    {stop, Reason, ok, State};
%% Called by the get_child_id function. Looks if the highlander is active and 
%% if yes returns the started childs id.
handle_call(get_child_id, _From, State) -> 
    case State#high_state.connected of
	true ->
	    {reply, {ok, State#high_state.child_pid}, State};
	false ->
	    {reply, error, State}
    end;
%% Called by the is_active function.
handle_call(isactive, _From, State) ->
    {reply, State#high_state.connected, State};
%% Called by the wait_until_active function. If active it returns immediately.
%% If not it saves the waiters pid and let it wait until a connection triggers
%% a message going to the handle_info block.
handle_call(wait_until_active, From, State) ->
    case State#high_state.connected of
	true ->
	    ok;
	false ->
	    NewWaiter = [From | State#high_state.wait_for_active],
	    {noreply, State#high_state{wait_for_active = NewWaiter}}
    end.

%% If a watch is triggered this Message comes to the Highlander. 
%% Path is the Path of the Node to make, not the one of the Father. 
handle_info({{nodechanged, _Path}, _I}, #high_state{connected=true} = State) ->
    %% IF the highlander is already connected we ignore this messages. 
    %% This way we don't have to determine how many unused watches we left
    %% triggered and are save from miscalculations happening while doing so.
    ?LOG(1,"~w is already a highlander", [self()]),
    {noreply, State};
handle_info({{nodechanged, Path}, _I}, #high_state{connected=false} = State) ->
    ?LOG(1," Highlander: nodechangenotify: ~w got one for ~s",[self(), Path]),
    %% if not active we try to get. 
    case try_to_get(Path, State#high_state.ident) of
	%% If successful we can modify the State and start the child
	{ok, Path} ->
	    ?LOG(1,"~w was lucky in retry", [self()]),
	    Module = State#high_state.module,
	    Parameters = State#high_state.parameters,
	    {ok, ChildPid} = start_child(Module, Parameters, State),
	    NewState = State#high_state{connected = true, my_path = Path,
					child_pid = ChildPid},
	    {noreply, NewState};
	%% If we do not win the race we go back to start.
	{error, _I1} ->
	    ?LOG(1,"~w was not lucky in retry", [self()]),
	    {noreply, State}
    end;
%% This message is comming if the node got the one. 
%% It is there to allow a notification of the waiting_for_active processes.
handle_info(got_active, State) ->
    lists:map(fun(PId) ->
		      gen_server:reply(PId, ok) end, State#high_state.wait_for_active),
    NewState = State#high_state{wait_for_active = []},
    {noreply, NewState}.


handle_cast(_A, State) ->
    {noreply, State}.


%% Starts the Highlandchild.
%% The run function gets the information 
%% a) who his father is, b) the highlander of which path it is
%% and c) the parameters provided when start was called.
start_child(Module, Parameters, State) ->
    ?LOG(1, "Highlander : start_child: Function called."),
    Father = self(),
    Path   = State#high_state.my_path,
    ?LOG(1, "Highlander : start_child: Starting Child function run"),
    %% {ok, HandlerState} = Module:run(Father, Path, Parameters),
    %% ?LOG(1, "Highlander : Starting Child function run"),

    Child  = spawn_link(Module, run ,[Father, Path,  Parameters]),
    ?LOG(1, "Highlander : start_child: run is running"),
    Father ! got_active,
    {ok, Child}.

%% Makes a first try to get highlander on every path from the list.
%% After this every fathernode  has a childwatch.
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

%% gets the father's address by looking at the child's
get_father(Path) ->
    FullPath = string:tokens(Path, "/"),
    get_father_from_list(FullPath).
get_father_from_list([_H]) ->
    "";
get_father_from_list([ H | T ]) ->
    "/" ++ H ++ get_father_from_list(T).

%% the needed swap function. 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
