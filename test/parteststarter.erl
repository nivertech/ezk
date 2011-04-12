-module(parteststarter).

-export([start/4]).


start(Number, M, F, A)  ->
    Self = self(),
    K = spawn(fun() ->
		      spawn_left(Number, M, F, A, Self) end),
    K!ended,
    receive 
	ended ->
	    ok
    end.
				

spawn_left(0, _M, _F, _A, Father) -> 
    receive 
	ended ->
	    Father ! ended
    end;
spawn_left(Number, M, F, A, Father) -> 
    io:format("spawn number ~w",[Number]),
    K = spawn(fun()->
		      spawn_left(Number-1, M, F, A, Father) end),
    apply(M, F, [Number | A]),
    K!ended.
