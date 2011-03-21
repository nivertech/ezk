-module(testit).

-export([start/0]).
-export([tls/0, tls2/0, tg/0, ts/0, tcr/0, tlsw/0, tlong/0, tls2w/0, tgw/0]).

start() ->
    {ok, A} = ezk_sup:start_link(),
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
    ezk_connection:create("/creationtest", "de"),
    ezk_connection:delete("/creationtest").

tlsw() ->
    Result = ezk_connection:ls("/", self(), testmes),
    io:format("Result: ~w ~n",[Result]),
    receive 
       {testmes, Rest} -> 
	    io:format("testmes : ~w ~n",[Rest])
    end.

tls2w() ->
    Result = ezk_connection:ls2("/", self(), testmes),
    io:format("Result: ~w ~n",[Result]),
    receive 
       {testmes, Rest} -> 
	    io:format("testmes : ~w ~n",[Rest])
    end.

tgw() ->
    Result = ezk_connection:get("/", self(), testmes),
    io:format("Result: ~w ~n",[Result]),
    receive 
       {testmes, Rest} -> 
	    io:format("testmes : ~w ~n",[Rest])
    end.
   

tlong() ->
    {ok, A} = ezk_connection:start_link(["192.168.1.111", 2181, 30000]),
    io:format("ls: ~w ~n: ",[ezk_connection:ls("/")]),
    io:format("ls: ~w ~n: ",[ezk_connection:ls("/")]),
    io:format("ls2: ~w ~n: ",[ezk_connection:ls2("/")]),
    io:format("get: ~w ~n: ",[ezk_connection:get("/")]),
    A.
    
     
