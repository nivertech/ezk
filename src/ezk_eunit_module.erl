-module(ezk_eunit_module).
-include_lib("eunit/include/eunit.hrl").

-define(TO_RUN_SINGLE,600).  %% 2000 rounds -> 95s  
-define(TO_RUN_MULTI,10000). %% 
-define(TO_LS_SINGLE,10).    %% 900 lses -> 0,4s
-define(TO_LS_MULTI,300).    %% 2000 clients, 900 lses --> 173s

-define(RUN_CYCLES, 50).
-define(RUN_CLIENTS, 200).
-define(RUN_SINGLE_ROUNDS, 2000).

-define(LS_CLIENTS, 2000).
-define(LS_LSES, 900).

-export([random_str/1]).

test_test_() ->
    {setup, %%receive afters are there to wait till zkServer stops dropping messages.
     fun() -> application:start(ezk), receive after 3000 -> ok end  end,
     fun(_) -> receive after 3000 -> application:stop(ezk) end end,      
     [ls_performance(), run()]}.


%% -----------------------------------------
%% Run
%% -----------------------------------------
 
run() ->
   [{"Single Run", {timeout, ?TO_RUN_SINGLE, ?_assertEqual(ok,run_single())}},
    {"Multi Run", {timeout, ?TO_RUN_MULTI, ?_assertEqual(ok,run_multi())}}].

%% -----------------------------------------
%% Multi Run
%% -----------------------------------------

run_multi() ->
    Self = self(),
    spawn(fun() -> run_multi_startall(?RUN_CLIENTS, Self, ?RUN_CYCLES) end),
    receive
      papi ->
	   ok
    end.


run_multi_startall(0, Father, _Cycles) ->
    Father ! papi,
    ok;
run_multi_startall(I, Father, Cycles) ->
    Self = self(),
    spawn(fun() -> run_multi_startall(I-1, Self, Cycles) end),
    List = run_s_sequenzed_create("/run_multi",Cycles,[]),
    ?assertEqual(ok, run_s_test_data( List)),
    List2 = run_s_change_data(List,[]),
    ?assertEqual(ok, run_s_test_data( List2)),
    ?assertEqual(ok, run_s_delete_list(List2)),    
    receive
	papi -> Father ! papi
    end,
    ok.

%% -----------------------------------------
%% Single Run
%% -----------------------------------------

run_single() ->
    Ls = ezk_connection:ls("/"),
    List = run_s_sequenzed_create("/run_single", ?RUN_SINGLE_ROUNDS,[]),
    ?assertEqual(ok, run_s_test_data(List)),
    List2 = run_s_change_data(List,[]),
    ?assertEqual(ok, run_s_test_data( List2)),
    ?assertEqual(ok, run_s_delete_list(List2)),
    ?assertEqual(Ls, ezk_connection:ls("/")),
    ok.

run_s_change_data([], List2) ->    
    List2;
run_s_change_data([{Path, Data} | T], List2) ->
    Data2 = random_str(1000),
    {C, _I}  = ezk_connection:set(Path, Data2),
    ?assertEqual( ok, C), 
    run_s_change_data(T, [{Path, Data2} | List2 ]).

run_s_delete_list([]) ->
    ok;
run_s_delete_list([{Path, _Data} | T]) ->
    ?assertEqual({ok, Path}, ezk_connection:delete(Path)),
    run_s_delete_list(T).

run_s_test_data([]) ->
    ok;
run_s_test_data([{Path, Data} | T]) ->
    {ok, {Got, _I}} = ezk_connection:get(Path),
    ?assertEqual(Data, Got),
    run_s_test_data(T).

    
run_s_sequenzed_create(_Path, 0, List) -> 
    List;
run_s_sequenzed_create(Path, I, List) ->
    Data = random_str(1000),
    {ok,NewNode} = ezk_connection:create(Path, Data, s),
    run_s_sequenzed_create(Path, I-1, [{NewNode, Data} | List]).


    %% -----------------------------------------
    %% LS
    %% -----------------------------------------
ls_performance() ->
    [{"LS: One Single Client", {timeout, ?TO_LS_SINGLE, ?_assertEqual(ok, ls_single())}}, 
     {"LS: Multiple Clients", {timeout, ?TO_LS_MULTI, ?_assertEqual(ok, ls_multi())}}].

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

%%---------------------------------
%% Random
%%---------------------------------

    
random_str(0) -> [];
random_str(Len) -> [random_char()|random_str(Len-1)].
random_char() -> element(random:uniform(tuple_size(normalChars())), normalChars()).

%very ugly. don't try that at home kids.
normalChars() ->
    list_to_tuple(lists:seq(65,90) ++ lists:seq(97,122)).
