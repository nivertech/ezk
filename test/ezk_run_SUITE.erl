%% -------------------------------------------------------------------
%%
%% ezk_run_SUITE: performs various loops of different commands in parallel 
%%                to test parallel workflow
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

-module(ezk_run_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-define(RUN_ROUNDS,20).
-define(DATALENGTH, 4).
-define(PAR_RUNS, 500).
-define(MULTIRUN_RUNS, 150).


suite() ->
    [{timetrap,{seconds,450}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    {ok, ConnectionPId} = ezk:start_connection(),
    [{connection_pid, ConnectionPId}  | Config].

end_per_suite(Config) ->
    {connection_pid, ConnectionPId} = lists:keyfind(connection_pid, 1, Config),
    ezk:end_connection(ConnectionPId, "Test finished"),
    application:stop(ezk),
    application:stop(sasl),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
      [rt1, rt5, rt10, rt50, rt75, rt100,
      multirun_test].
     %% {skip, test}.

rt1(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 100), ezk_run_SUITE, run_test, 
			 [?RUN_ROUNDS, ConPId]).
rt5(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 20), ezk_run_SUITE, run_test,
			 [?RUN_ROUNDS, ConPId]).
rt10(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 10), ezk_run_SUITE, run_test,
			 [?RUN_ROUNDS, ConPId]).
rt50(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 5), ezk_run_SUITE, run_test, 
			 [?RUN_ROUNDS, ConPId]).
rt75(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 2), ezk_run_SUITE, run_test, 
			 [?RUN_ROUNDS, ConPId]).
rt100(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS), ezk_run_SUITE, run_test,
			 [?RUN_ROUNDS, ConPId]).

multirun_test(_Config) ->
    {ok, Con1} = ezk:start_connection(),
    {ok, Con2} = ezk:start_connection(),
    {ok, Con3} = ezk:start_connection(),
    {ok, Con4} = ezk:start_connection(),
    {ok, Con5} = ezk:start_connection(),
    parteststarter:start(5, ezk_run_SUITE, multirun_tester, [{Con1, Con2, Con3, Con4,
							      Con5}]).

multirun_tester(Number, {Con1, Con2, Con3, Con4, Con5}) ->
    case Number of
	1 -> Con = Con1;
	2 -> Con = Con2;
	3 -> Con = Con3;
	4 -> Con = Con4;
	5 -> Con = Con5;
        _Else -> Con = Con1
    end,
    io:format("Number ~w is starting the run_test with Connection ~w",[Number, Con]),
    run_test(Number, ?MULTIRUN_RUNS, Con).
    
    


run_test(_Number, Cycles, ConPId) ->
    io:format("Start ~w with ~w cycles",[self(), Cycles]),
    List  = sequenzed_create(ConPId, "/run_multi",Cycles,[]),
    io:format("test data  ~w with ~w cycles",[self(), Cycles]),
    ok    = test_data(ConPId, List),
    io:format("change data  ~w with ~w cycles",[self(), Cycles]),
    List2 = change_data(ConPId, List,[]),
    io:format("test data again  ~w with ~w cycles",[self(), Cycles]),
    ok    = test_data(ConPId, List2),    

%% ------------ datawatches
    io:format("set datawatch ~w with ~w cycles",[self(), Cycles]),
    ok    = set_watch_and_test( ConPId, List2),
    io:format("spawn changer ~w with ~w cycles",[self(), Cycles]),
    spawn(fun() -> change_data(ConPId, List2, []) end),
    io:format("wait for watch ~w with ~w cycles",[self(), Cycles]),
    ok    = wait_datawatches(List2),

%% ------------ childwatches
    io:format("set childwatches ~w with ~w cycles",[self(), Cycles]),
    ok    = set_childwatches( ConPId, List2),
    io:format("spawn childchanger ~w with ~w cycles",[self(), Cycles]),
    spawn(fun() -> change_childs(ConPId, List2) end),
    io:format("wait for childwatches ~w with ~w cycles",[self(), Cycles]),
    ok    = wait_childwatches(List2),

%% ----------- child and datawatches if nodes deleted
    io:format("alternating watches ~w with ~w cycles",[self(), Cycles]),
    ok    = set_alternating_datachildwatches( ConPId, 0, List2),
    io:format("spawn childkiller ~w with ~w cycles",[self(), Cycles]),
    spawn(fun() -> delete_list(ConPId, List2) end),
    io:format("wait for alternating watches ~w with ~w cycles",[self(), Cycles]),
    ok    = wait_nodedeleted_watches(List2),
    
    io:format("finished ~w with ~w cycles",[self(), Cycles]).

set_alternating_datachildwatches(_ConPId, _Number, []) ->  
    ok;
set_alternating_datachildwatches(ConPId, 1, [{Path, _Data} | Tail]) -> 
    Self = self(),
    io:format("try to set ls watch for ~w on ~s",[Self,Path]),
    {ok, _Childs} = ezk:ls(ConPId, Path, Self, {mixedwatch, Path}),
    io:format("ls watch set for ~w on ~s",[Self,Path]),
    set_alternating_datachildwatches(ConPId, 0, Tail);
set_alternating_datachildwatches(ConPId, 0, [{Path, _Data} | Tail]) -> 
    Self = self(),
    io:format("try to set get watch for ~w on ~s",[Self,Path]),
    {ok, _Data1} = ezk:get(ConPId, Path, Self, {mixedwatch, Path}),
    io:format("get watch set for ~w on ~s",[Self,Path]),
    set_alternating_datachildwatches(ConPId, 1, Tail).

delete_list(_ConPId, []) ->
    ok;
delete_list(ConPId, [{Path, _Data} | Tail]) ->
    io:format("deleting node ~w and ~w left",[Path, length(Tail)]),
    {ok, Path} = ezk:delete(ConPId, Path),
    delete_list(ConPId, Tail).

wait_nodedeleted_watches([]) ->
    ok;
wait_nodedeleted_watches([{Path, _Data} | Tail]) ->
    Self = self(),
    io:format("~w waiting for a watch",[Self]),
    receive
       {{mixedwatch, Path}, _Left} ->
    	    io:format("Got a nodedeleted, ~w left",[length(Tail)]),
       	    wait_nodedeleted_watches(Tail)
    end.    

change_childs(_ConPId, []) ->
    ok;
change_childs(ConPId, [{Path, _Data} | Tail]) ->
    NewData = datamaker(?DATALENGTH),
    {ok, ChildName} = ezk:create(ConPId, Path ++ "/testchild", NewData),
    {ok, ChildName} = ezk:delete(ConPId, ChildName),
    change_childs(ConPId, Tail).

wait_childwatches([]) ->
    ok;
wait_childwatches([{Path, _Data} | Tail]) ->
    receive
       {{childwatch, Path}, _Left} ->
	    wait_childwatches(Tail)
    end.

set_childwatches(_ConPId, []) ->
    ok;
set_childwatches(ConPId, [{Path, _Data} | Tail]) ->
    Self = self(),
    {ok, _Childs} = ezk:ls(ConPId, Path, Self, {childwatch, Path}),
    set_childwatches(ConPId, Tail).


wait_datawatches([]) ->
    ok;
wait_datawatches([{Path, _Data} | Tail]) ->
    receive
       {{datawatch, Path}, _Left} ->
	    wait_datawatches(Tail)
    end.
    
sequenzed_delete(_ConPId, []) ->
    ok;
sequenzed_delete(ConPId, [{Path,_Data} | Tail]) ->
    {ok, Path} = ezk:delete(ConPId, Path),
    sequenzed_delete(ConPId, Tail).

set_watch_and_test(_ConPId, [])->
    ok;
set_watch_and_test(ConPId, [{Path,Data} | Tail]) ->
    Self = self(),
    {ok, {Data, _I}} = ezk:get(ConPId, Path, Self, {datawatch, Path}),
    set_watch_and_test(ConPId, Tail).

change_data(_ConPId, [], NewList) ->
    NewList;
change_data(ConPId, [{Path, _Data} | Tail], NewList) ->
    NewData = datamaker(?DATALENGTH),
    {ok, _I} = ezk:set(ConPId, Path, NewData),
    change_data(ConPId, Tail, [{Path, NewData} | NewList]).

test_data(_ConPId, []) ->
    ok;
test_data(ConPId, [{Path, Data} | Tail]) ->
    {ok, {Data, _I}} = ezk:get(ConPId, Path),
    test_data(ConPId, Tail).

sequenzed_create(_ConPId, _Path, 0, List) ->
    List;
sequenzed_create(ConPId, Path, CyclesLeft, List) ->
    Data = datamaker(?DATALENGTH),
    {ok, Name} = ezk:create(ConPId, Path, Data, s),
    sequenzed_create(ConPId, Path, CyclesLeft-1, [{Name, Data} | List]).

stringmaker(N) ->
    list_to_binary(lists:seq(1,N)).
    
