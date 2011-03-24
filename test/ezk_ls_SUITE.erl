%%%-------------------------------------------------------------------
%%% @author Marco <marco@gauss.gi.net>
%%% @copyright (C) 2011, Marco
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2011 by Marco <marco@gauss.gi.net>
%%%-------------------------------------------------------------------
-module(ezk_ls_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(LS_RUNS, 250).

-define(LOG, ct_log:log).
-define(LOGSUITEINIT, ct_log:suite_init).
-define(LOGSUITEEND, ct_log:suite_end).
-define(LOGGROUPINIT, ct_log:group_init).
-define(LOGGROUPEND, ct_log:group_end).

suite() ->
    [{timetrap,{seconds,400}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
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

