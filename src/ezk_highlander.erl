
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
 
-behaviour(gen_server).

-record(high_state, {is_active = false,
		     ident,
		     my_path,
		     module, 
		     parameters,
		     wait_for_active = [],
		     module_state,
		     connection_pid
		    }).

-export([start/4, failover/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([behaviour_info/1]).
-export([is_active/1]).


behaviour_info(callbacks) ->
    [{terminate,2}, {init,2}].

%% The startfunction.
start(ConnectionPId, Module, Parameters, NodeList) ->
    gen_server:start( ?MODULE, [ConnectionPId, Module, Parameters, NodeList], []).


%% The init function tryes every given path once and ensures that all
%% needed ZK Nodes are there. After trying once without success it goes 
%% into normal genserver state and waits for messages ( = changes in the important nodes)
%% Return type is a record of type high_state, which is the State format in this Server.
init([ConnectionPId, Module, Parameters, NodeList]) ->
    %% ensure the needed Paths
    ?LOG(1,"Highlander: Init: got parameters module, Para, NodeList: ~w, ~w, ~w",
	 [Module, Parameters, NodeList]), 
    lists:map(fun(Path) ->
		      Father = get_father(Path),
		      ?LOG(1,"Highlander: erstelle Pfad ~s",[Father]),
		      ezk:ensure_path(ConnectionPId, Father) 
	      end, NodeList),
    %% set initial state
    Ident = pid_to_list(self())++ " " ++ atom_to_list(node()),
    State = #high_state{ident = Ident, my_path = "", module = Module, 
			parameters = Parameters,  module_state = false,
		       connection_pid = ConnectionPId},
    %% first try to get highlander at every node
    case try_first(ConnectionPId, NodeList, Ident) of
	no_luck ->
	    {ok, State};
	{ok, Path} ->
	    ?LOG(1, "Highlander:  Init: ~w trying to start its child", [Ident]),
	    {ok, NewState} = start_init(Module, Parameters, 
					State#high_state{my_path = Path}),
	    ?LOG(1, "Highlander:  Init: ~w Child started", [Ident]),
	    {ok, NewState#high_state{is_active = true}}
    end.

%% Gives true or false, depending on if the instance has become a highlander.
is_active(PId) ->
    gen_server:call(PId, isactive).

%% Triggers a stopping of the highlander.
failover(PId, Reason) ->
    gen_server:call(PId, {failover, Reason}).

%% Used if the server dies (also in case of a failover)
%% calls terminate/2 of the child. After 2 seconds kills the child
%% and the itself.
terminate(Reason, State) ->
    ?LOG(1, "Highlander: Failover  of ~w",[self()]),
    Module = State#high_state.module,
    Module:terminate(State#high_state.module_state, Reason),
    %% if terminate already deleted the node it must not be deleted again.
    Ident = State#high_state.ident,
    ConnectionPId = State#high_state.connection_pid,
    case ezk:get(ConnectionPId, State#high_state.my_path) of
	{ok,{Ident, _I}} ->
	    ezk:delete(ConnectionPId, State#high_state.my_path);
	_Else -> 
	    ok
    end,
    timer:sleep(2000).

%% Called by the failover function
handle_call({failover, Reason}, _From, State) ->    
    {stop, Reason, ok, State};
%% Called by the is_active function.
handle_call(isactive, _From, State) ->
    {reply, State#high_state.is_active, State}.

%% If a watch is triggered this Message comes to the Highlander. 
%% Path is the Path of the Node to make, not the one of the Father. 
handle_info({{nodechanged, _Path}, _I}, #high_state{is_active=true} = State) ->
    %% IF the highlander is already active  we ignore this messages. 
    %% This way we don't have to determine how many unused watches we left
    %% triggered and are save from miscalculations happening while doing so.
    ?LOG(1,"~w is already a highlander", [self()]),
    {noreply, State};
handle_info({{nodechanged, Path}, _I}, #high_state{is_active=false} = State) ->
    ?LOG(1," Highlander: nodechangenotify: ~w got one for ~s",[self(), Path]),
    %% if not active we try to get. 
    ConnectionPId = State#high_state.connection_pid,
    case try_to_get(ConnectionPId, Path, State#high_state.ident) of
	%% If successful we can modify the State and trigger the init
	{ok, Path} ->
	    ?LOG(1,"~w was lucky in retry", [self()]),
	    Module = State#high_state.module,
	    Parameters = State#high_state.parameters,
	    {ok, NewState} = start_init(Module, Parameters, 
					State#high_state{my_path = Path}),
	    {noreply, NewState#high_state{is_active = true}};
	%% If we do not win the race we go back to start.
	{error, _I1} ->
	    ?LOG(1,"~w was not lucky in retry", [self()]),
	    {noreply, State}
    end.


handle_cast(_A, State) ->
    {noreply, State}.


%% Starts the Highlandchild.
%% The run function gets the information 
%% a) who his father is, b) the highlander of which path it is
%% and c) the parameters provided when start was called.
start_init(Module, Parameters, State) ->
    ?LOG(1, "Highlander : start_child: Function called."),
    Path   = State#high_state.my_path,
    ?LOG(1, "Highlander : start_child: Starting Child function run"),
    {ok, HandlerState} = Module:init(Path, Parameters),
    ?LOG(1, "Highlander : start_child: run is running"),
    {ok, State#high_state{module_state = HandlerState}}.

%% Makes a first try to get highlander on every path from the list.
%% After this every fathernode  has a childwatch.
try_first(_ConnectionPId, [], Ident) ->
    ?LOG(1, "Highlander: First Try: ~s tried all without luck",[Ident]),
    no_luck;
try_first(ConnectionPId, [Node | NodeList] , Ident) ->
    case (try_to_get(ConnectionPId, Node, Ident)) of
	{ok, Path} ->
	    ?LOG(1, "Highlander: First Try: ~s got lucky",[Ident]),
	    {ok, Path};
	{error, _I}  ->
	    try_first(ConnectionPId, NodeList, Ident)
    end.  

%% Trys to create a the in Path specified node with data Ident.
%% Also sets a childwatch to its father with message {nodechanged, Path}.
try_to_get(ConnectionPId, Path, Ident) ->
    Father = get_father(Path),
    ezk:ls(ConnectionPId, Father, self(), {nodechanged, Path}),
    ?LOG(1, "Highlander: Watch set by ~w",[self()]),
    ezk:create(ConnectionPId, Path, Ident, e).

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
