-module(testit).

-export([start/0]).
-export([tls/0, tls2/0, tg/0, ts/0, tcr/0, tlong/0]).

start() ->
    {ok, A} = ezk_connection:start_link(["192.168.1.111", 2181, 30000]),
    A.

tls() ->
    ezk_connection:ls("/").

tls2() ->
    ezk_connection:ls2("/").

tg() ->
    ezk_connection:get("/").

ts() ->
    ezk_connection:set("/","tester").

tcr() ->
    ezk_connection:create("/creationtest"),
    ezk_connection:delete("/creationtest").

tlong() ->
    {ok, A} = ezk_connection:start_link(["192.168.1.111", 2181, 30000]),
    io:format("ls: ~w ~n: ",[ezk_connection:ls("/")]),
    io:format("ls: ~w ~n: ",[ezk_connection:ls("/")]),
    io:format("ls2: ~w ~n: ",[ezk_connection:ls2("/")]),
    io:format("get: ~w ~n: ",[ezk_connection:get("/")]),
    A.
    
     
