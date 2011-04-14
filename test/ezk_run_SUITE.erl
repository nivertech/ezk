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
-define(RUN_ROUNDS,25).
-define(DATALENGTH, 8).
-define(PAR_RUNS, 900).
-define(MULTIRUN_RUNS, 550).


suite() ->
    [{timetrap,{seconds,200}}].

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
     %%{skip, test}.

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
    io:format("set watch ~w with ~w cycles",[self(), Cycles]),
    ok    = set_watch_and_test( ConPId, List2),
    io:format("spawn changer ~w with ~w cycles",[self(), Cycles]),
    spawn(fun() -> change_data(ConPId, List2, []) end),
    io:format("wait for watch ~w with ~w cycles",[self(), Cycles]),
    ok    = wait_watches(List2),
    io:format("delete all nodes  ~w with ~w cycles",[self(), Cycles]),
    ok    = sequenzed_delete(ConPId, List2),
    io:format("finished ~w with ~w cycles",[self(), Cycles]).
    
wait_watches([]) ->
    ok;
wait_watches([{Path, _Data} | Tail]) ->
    receive
       {{datawatch, Path}, _Left} ->
	    wait_watches(Tail)
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
    NewData = stringmaker(?DATALENGTH),
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
    Data = stringmaker(?DATALENGTH),
    {ok, Name} = ezk:create(ConPId, Path, Data, s),
    sequenzed_create(ConPId, Path, CyclesLeft-1, [{Name, Data} | List]).

stringmaker(N) ->
    lists:seq(1,N).
    
