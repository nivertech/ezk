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

-define(LS_RUNS, 100).

suite() ->
    [{timetrap,{seconds,400}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
    application:stop(ezk),
    application:stop(sasl),
    ok.

init_per_group(GroupName, Config) ->
    io:format("Group starting: ~w", [GroupName]),
    Config.

end_per_group(GroupName, _Config) ->
    io:format("Group finished: ~w", [GroupName]),
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
    {ok, _E} = ezk_connection:ls("/"),
    ls_test(Config, N-1).

