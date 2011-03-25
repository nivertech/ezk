
%% -------------------------------------------------------------------
%%
%% ezk_ls_SUITE: performs various loops of  ls commands in parallel to test high load
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
-module(ezk_ls_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(LS_RUNS, 200).

-define(LOG, ct_log:log).
-define(LOGSUITEINIT, ct_log:suite_init).
-define(LOGSUITEEND, ct_log:suite_end).
-define(LOGGROUPINIT, ct_log:group_init).
-define(LOGGROUPEND, ct_log:group_end).

suite() ->
    [{timetrap,{seconds,700}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    ezk:delete_all("/"),
    {ok, StartIter} = ezk:info_get_iterations(),
    ?LOGSUITEINIT("LS"),
    [{suitetime, erlang:now()} |  [{suiteiter, StartIter}  | Config]].

end_per_suite(Config) ->
    FinishTime = erlang:now(),
    {suitetime, StartTime} = lists:keyfind(suitetime, 1, Config), 
    {suiteiter, StartIter} = lists:keyfind(suiteiter, 1, Config),
    {ok, FinishIter} = ezk:info_get_iterations(),
    Elapsed = timer:now_diff(FinishTime, StartTime),
    Iter = FinishIter - StartIter,
    ?LOGSUITEEND("LS",Elapsed, Iter),
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
    LsCases = [{ls100,100},
    	      {ls200,200},
    	      {ls500,500},
    	      {ls700,700},
    	      {ls900,900}
    	      ],
 [{Name, [parallel], [ls_test || _Id <- lists:seq(1,Para)]} 
	    || {Name, Para} <- LsCases ].

all() -> 
    [{group, N} || {N, _, _} <- groups()].

ls_test(Config) ->
   ok = ls_test(Config, ?LS_RUNS).

ls_test(_Config, 0) ->
    ok;
ls_test(Config, N) ->
    {ok, _E} = ezk:ls("/"),
    ls_test(Config, N-1).

