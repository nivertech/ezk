-module(test_highlander2_impl).

-export([start/0, run/2, terminate/2]).

-behaviour(ezk_highlander).

start() ->
    ezk_highlander:start_link("highl++", {test_highlander2_impl, run, []},5).


run(Id, _Father) ->
    io:format("start"),
    loop(Id).

loop(Id) ->
    io:format("I am Number ~w ~n",[Id]),
    timer:sleep(4000),
    loop(Id).


terminate(_PId, _I) ->
    io:format("I am Dying ~n").
