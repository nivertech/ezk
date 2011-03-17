-module(ezk_eunit_module).
-include_lib("eunit/include/eunit.hrl").
-define(RUN_SINGLE_ROUNDS, 1000).
-define(LS_CLIENTS,1000).
-define(LS_LSES,1500).

test_test_() ->
    {setup, %%receive afters are there to wait till zkServer stops dropping messages.
     fun() -> application:start(ezk), receive after 3000 -> ok end  end,
     fun(_) -> receive after 3000 -> application:stop(ezk) end end,      
     [ls_performance(), run()]}.


%% -----------------------------------------
%% Run
%% -----------------------------------------
 
run() ->
   [{"Single Run", {timeout, 300, ?_assertEqual(ok,run_single())}}].


%% -----------------------------------------
%% Single Run
%% -----------------------------------------

run_single() ->
    Ls = ezk_connection:ls("/"),
    List = run_s_sequenzed_create("/run_single","nodata",?RUN_SINGLE_ROUNDS,[]),
    ?assertEqual(ok, run_s_test_data("nodata", List)),
    ?assertEqual(ok, run_s_delete_list(List)),
    ?assertEqual(Ls, ezk_connection:ls("/")),
    ok.
    
run_s_delete_list([]) ->
    ok;
run_s_delete_list([H | T]) ->
    ?assertEqual({ok, H}, ezk_connection:delete(H)),
    run_s_delete_list(T).

run_s_test_data(_Data, []) ->
    ok;
run_s_test_data(Data, [H | T]) ->
    {ok, {Got, _I}} = ezk_connection:get(H),
    ?assertEqual(Data, Got),
    run_s_test_data(Data, T).

    
run_s_sequenzed_create(_Path, _Data, 0, List) -> 
    List;
run_s_sequenzed_create(Path, Data, I, List) ->
    {ok,NewNode} = ezk_connection:create(Path, Data, s),
    run_s_sequenzed_create(Path, Data, I-1, [NewNode | List]).


    %% -----------------------------------------
    %% LS
    %% -----------------------------------------
ls_performance() ->
    [{"LS: One Single Client", {timeout, 100, ?_assertEqual(ok, ls_single())}}, 
     {"LS: Multiple Clients", {timeout, 600, ?_assertEqual(ok, ls_multi())}}].

ls_single()->
    ls_lses(?LS_LSES),
    ok.

ls_multi()->
    Self = self(),
    spawn(fun() -> ls_startall(?LS_CLIENTS, Self, ?LS_LSES) end),
    receive
      papi ->
	   ok
    end.

ls_startall(0, Father, _Lses) ->
    Father ! papi,
    ok;
ls_startall(I, Father, Lses) ->
    Self = self(),
    spawn(fun() -> ls_startall(I-1, Self, Lses) end),
    ls_lses(Lses),
    receive
	papi -> Father ! papi
    end,
    ok.

ls_lses(0) ->
    ok;
ls_lses(I) ->
    ?assertEqual({ok,[<<"zookeeper">>]}, ezk_connection:ls("/")),
    ls_lses(I-1).

    
