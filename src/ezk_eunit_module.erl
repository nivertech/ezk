-module(ezk_eunit_module).
-include_lib("eunit/include/eunit.hrl").
-define(LS_CLIENTS,1000).
-define(LS_LSES,1500).

test_test_() ->
    {setup, %%receive afters are there to wait till zkServer stops dropping messages.
     fun() -> application:start(ezk), receive after 1000 -> ok end  end,
     fun(_) -> receive after 1000 -> application:stop(ezk) end end,      
     [ls_performance()]}.

ls_performance() ->
    [{timeout, 100, ?_assertEqual(ok, ls_single())}, 
     {timeout, 100, ?_assertEqual(ok, ls_multi())}]. %clients, lses

ls_single()->
    lses(?LS_LSES),
    ok.

ls_multi()->
    Self = self(),
    spawn(fun() -> startall(?LS_CLIENTS, Self, ?LS_LSES) end),
    receive
      papi ->
	   ok
    end.

startall(0, Father, _Lses) ->
    Father ! papi,
    ok;
startall(I, Father, Lses) ->
    Self = self(),
    spawn(fun() -> startall(I-1, Self, Lses) end),
    lses(Lses),
    receive
	papi -> Father ! papi
    end,
    ok.

lses(0) ->
    ok;
lses(I) ->
    ezk_connection:ls("/"),
    lses(I-1).

    
