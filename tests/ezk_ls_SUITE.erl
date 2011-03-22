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

-define(LS_RUNS, 10).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:start(ezk),
    Config.

end_per_suite(_Config) ->
    application:stop(ezk),
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
    LsCases = [{ls100,100},
	      {ls500,500},
	      {ls1000,100},
	      {ls5000,500},
	      {ls10000,100}
	      ],
    [{Name, [parallel], [ls_test || _Id <- lists:seq(1,Para)]} 
	    || {Name, Para} <- LsCases ].

all() -> 
    [{group, N} || {N, _, _} <- groups()].

ls_test(Config) ->
    ls_test(Config, ?LS_RUNS).

ls_test(_Config, 0) ->
    ok;
ls_test(Config, N) ->
    {ok, _Ok} =  ezk_connection:ls("/"),
    ls_test(Config, N-1).

