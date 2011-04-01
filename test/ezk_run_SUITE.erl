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

-define(LOG, ct_log:log).
-define(LOGSUITEINIT, ct_log:suite_init).
-define(LOGSUITEEND, ct_log:suite_end).
-define(LOGGROUPINIT, ct_log:group_init).
-define(LOGGROUPEND, ct_log:group_end).


suite() ->
    [{timetrap,{seconds,1000}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),  
    ezk:delete_all("/"),
    {ok, StartIter} = ezk:info_get_iterations(),
    ?LOGSUITEINIT("RUN"),
    [{suitetime, erlang:now()} |  [{suiteiter, StartIter}  | Config]].

end_per_suite(Config) ->
    FinishTime = erlang:now(),
    {suitetime, StartTime} = lists:keyfind(suitetime, 1, Config), 
    {suiteiter, StartIter} = lists:keyfind(suiteiter, 1, Config),
    {ok, FinishIter} = ezk:info_get_iterations(),
    Elapsed = timer:now_diff(FinishTime, StartTime),
    Iter = FinishIter - StartIter,
    ?LOGSUITEEND("RUN",Elapsed, Iter),
    application:stop(ezk),
    application:stop(sasl),
    ok.

init_per_group(GroupName, Config) ->
    ?LOGGROUPINIT(GroupName),
    {ok, StartIter} = ezk:info_get_iterations(),
    [{grouptime, erlang:now()} | [{groupiter, StartIter } | Config]].

end_per_group(GroupName, Config) ->
    FinishTime = erlang:now(),
    {grouptime, StartTime} = lists:keyfind(grouptime, 1, Config),  
    {groupiter, StartIter} = lists:keyfind(groupiter, 1, Config),
    {ok, FinishIter} = ezk:info_get_iterations(),
    Iter = FinishIter - StartIter,
    Elapsed = timer:now_diff(FinishTime, StartTime),
    ?LOGGROUPEND(GroupName, Elapsed, Iter),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    RunCases = [{run100,100},
		{run200,200},
		{run500,500},
		{run700,700},
		{run900,900}
	       ],
    [{Name, [parallel], [run_test || _Id <- lists:seq(1,Para)]} 
     || {Name, Para} <- RunCases ]. 

all() -> 
    %% {skip, test}.
    [{group, N} || {N, _, _} <- groups()].

run_test(Config) ->
    ok = run_test(Config, ?RUN_ROUNDS).
run_test(_Config, Cycles) ->
    List  = sequenzed_create("/run_multi",Cycles,[]),
    ok    = test_data(List),
    List2 = change_data(List,[]),
    ok    = test_data(List2),    
    ok    = set_watch_and_test( List2),
    spawn(fun() -> change_data(List2, []) end),
    ok    = wait_watches(List2),
    ok    = sequenzed_delete(List2).
    
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
    
