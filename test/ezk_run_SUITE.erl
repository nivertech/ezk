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

-export([run_test/1]).

suite() ->
    [{timetrap,{seconds,1000}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
    application:stop(ezk),
    application:stop(sasl),
    ok.

init_per_group(GroupName, Config) ->
    Config.

end_per_group(GroupName, Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [rt1, rt5, rt10, rt50, rt75, rt100].
    %% {skip, test}.

rt1(_Config) -> parteststarter:start((?PAR_RUNS div 100), 
				     ezk_run_SUITE, run_test, [?RUN_ROUNDS]).
rt5(_Config) -> parteststarter:start((?PAR_RUNS div 20),
				     ezk_run_SUITE, run_test, [?RUN_ROUNDS]).
rt10(_Config) -> parteststarter:start((?PAR_RUNS div 10), 
				      ezk_run_SUITE, run_test, [?RUN_ROUNDS]).
rt50(_Config) -> parteststarter:start((?PAR_RUNS div 5), 
				      ezk_run_SUITE, run_test, [?RUN_ROUNDS]).
rt75(_Config) -> parteststarter:start((?PAR_RUNS div 2), 
				      ezk_run_SUITE, run_test, [?RUN_ROUNDS]).
rt100(_Config) -> parteststarter:start((?PAR_RUNS), 
				       ezk_run_SUITE, run_test, [?RUN_ROUNDS]).

run_test(Cycles) ->
    io:format("Start ~w with ~w cycles",[self(), Cycles]),
    List  = sequenzed_create("/run_multi",Cycles,[]),
    io:format("test data  ~w with ~w cycles",[self(), Cycles]),
    ok    = test_data(List),
    io:format("change data  ~w with ~w cycles",[self(), Cycles]),
    List2 = change_data(List,[]),
    io:format("test data again  ~w with ~w cycles",[self(), Cycles]),
    ok    = test_data(List2),    
    io:format("set watch ~w with ~w cycles",[self(), Cycles]),
    ok    = set_watch_and_test( List2),
    io:format("spawn changer ~w with ~w cycles",[self(), Cycles]),
    spawn(fun() -> change_data(List2, []) end),
    io:format("wait for watch ~w with ~w cycles",[self(), Cycles]),
    ok    = wait_watches(List2),
    io:format("delete all nodes  ~w with ~w cycles",[self(), Cycles]),
    ok    = sequenzed_delete(List2),
    io:format("finished ~w with ~w cycles",[self(), Cycles]).
    
wait_watches([]) ->
    ok;
wait_watches([{Path, _Data} | Tail]) ->
    receive
       {{datawatch, Path}, _Left} ->
	    wait_watches(Tail)
    end.
    
sequenzed_delete([]) ->
    ok;
sequenzed_delete([{Path,_Data} | Tail]) ->
    {ok, Path} = ezk:delete(Path),
    sequenzed_delete(Tail).

set_watch_and_test([])->
    ok;
set_watch_and_test([{Path,Data} | Tail]) ->
    Self = self(),
    {ok, {Data, _I}} = ezk:get(Path, Self, {datawatch, Path}),
    set_watch_and_test(Tail).

change_data([], NewList) ->
    NewList;
change_data([{Path, _Data} | Tail], NewList) ->
    NewData = stringmaker(?DATALENGTH),
    {ok, _I} = ezk:set(Path, NewData),
    change_data(Tail, [{Path, NewData} | NewList]).

test_data([]) ->
    ok;
test_data([{Path, Data} | Tail]) ->
    {ok, {Data, _I}} = ezk:get(Path),
    test_data(Tail).

sequenzed_create(_Path, 0, List) ->
    List;
sequenzed_create(Path, CyclesLeft, List) ->
    Data = stringmaker(?DATALENGTH),
    {ok, Name} = ezk:create(Path, Data, s),
    sequenzed_create(Path, CyclesLeft-1, [{Name, Data} | List]).

stringmaker(N) ->
    lists:seq(1,N).
    
