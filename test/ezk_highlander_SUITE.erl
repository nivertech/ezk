
%% -------------------------------------------------------------------
%%
%% ezk_highlander_SUITE: CT Suite for the highlander behaviour.
%%
%% Copyright (c) 2011 Marco Grebe. All Rights Reserved.
%% Copyright (c) 2011 global infinipool GmbH.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------


-module(ezk_highlander_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(LOG, ct_log:log).
-define(LOGSUITEINIT, ct_log:suite_init).
-define(LOGSUITEEND, ct_log:suite_end).
-define(LOGGROUPINIT, ct_log:group_init).
-define(LOGGROUPEND, ct_log:group_end).
-define(HIGHIMPL, test_highlander_impl:start_link).
-define(HIGH2NUMBER,20).

suite() ->
    [{timetrap,{seconds,2000}}].

init_per_suite(Config) ->
    random:seed(erlang:now()),
    application:start(ezk),
    application:start(sasl),
    ezk:delete_all("/"),
    {ok, StartIter} = ezk:info_get_iterations(),
    ?LOGSUITEINIT("HIGHLANDER"),
    [{suitetime, erlang:now()} |  [{suiteiter, StartIter}  | Config]].

end_per_suite(Config) ->
    FinishTime = erlang:now(),
    {suitetime, StartTime} = lists:keyfind(suitetime, 1, Config), 
    {suiteiter, StartIter} = lists:keyfind(suiteiter, 1, Config),
    {ok, FinishIter} = ezk:info_get_iterations(),
    Elapsed = timer:now_diff(FinishTime, StartTime),
    Iter = FinishIter - StartIter,
    ?LOGSUITEEND("HIGHLANDER",Elapsed, Iter),
    application:stop(ezk),
    application:stop(sasl),
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
    [].

all() -> 
    [high_test, high2_test].

high_test(_Config) ->
    Paras = [20],
    lists:map(
      fun(I) -> high_tester(I) end, Paras),
    ok.

high_tester(I) ->
    io:format("Start testing with ~w highlanderwannabes~n",[I]),
    Dict = spawn_list_highlander(self(), dict:new(), I, 1),
    ok = high_wait(Dict, I),
    io:format("Test finished~n").

high_wait(_Dict, 0) ->
    ok;
high_wait(Dict, I) ->
    receive 
	{init, PId, N, FatherPId} ->
	    io:format("Starting with highlaner number ~w~n",[N]),
	    NewDict = dict:erase(FatherPId, Dict),
	    Cycles  = random:uniform(80),
	    io:format("Doing ~w rounds with Node ~w", [Cycles, N]),
	    ok      = send_receive_n(PId, Cycles),
	    ezk_highlander:failover(FatherPId, "test"),
	    io:format("Finished with highlaner number ~w~n",[N]),
	    high_wait(NewDict,I-1)
    end.


%% ---------------------------- high 2 test------------------------

high2_test(_Config) ->
    Paras = [100],
    lists:map(
      fun(I) -> high2_tester(I) end, Paras),
    ok.
    
high2_tester(I) ->
    Dict = spawn_list_highlander(self(), dict:new(), I, ?HIGH2NUMBER),
    ok = high2_wait(Dict, I, ?HIGH2NUMBER).

high2_wait(_Dict, 0, _) ->
    ok;
high2_wait(Dict, Left, FreeSlots) ->
    receive
	{init, PId, Number, FatherPId} ->
	    io:format("Starting with highlaner number ~w and afterwards ~w Slots left",
		      [Number, FreeSlots-1]),
	    if
		FreeSlots > 0 ->
		    Self = self(),
		    Cycles  = random:uniform(100000),
		    io:format("Doing ~w rounds with Node ~w", [Cycles, Number]),	    
		    spawn(fun() -> receiver2(PId, Self, FatherPId, Number, Cycles) end),
		    NewDict = dict:erase(FatherPId, Dict),
		    high2_wait(NewDict, Left-1, FreeSlots-1);
		true ->
		    error_logger:error_msg("To many Highlanders")
	    end;
	{ended, FatherPId, Number} ->
	    io:format("Finished with highlaner number ~w and now ~w Slots left",
		      [Number, FreeSlots+1]),
	    ezk_highlander:failover(FatherPId, "test"),
	    high2_wait(Dict, Left, FreeSlots+1)
    end.
	    

receiver2(Child, Caller, Father, Number, Cycles) ->
    if 
	Cycles > 60000 ->
	    ok = send_receive_n(Child, Cycles);
	true ->
	    timer:sleep(Cycles)
    end,
    Caller ! {ended, Father, Number}.
    


%% ---------------------------- free for all ---------------------------
    
send_receive_n(_PId, 0) ->
    ok;
send_receive_n(PId, Cycles) ->
    %%io:format("Doing a round ~w",[self()]),
    PId ! {ping, self()},
    receive 
	{pong, PId} ->
	    send_receive_n(PId, Cycles-1)
    end.


spawn_list_highlander(_Waiter, Dict, 0, _Number) ->
    Dict;
spawn_list_highlander(Waiter, Dict, I, Number) ->
    {ok, NewPId} = ?HIGHIMPL(Waiter, I, Number),
    io:format("Spawned ~w with pid ~w~n",[I, NewPId]),
    spawn_list_highlander(Waiter, dict:append(NewPId, I, Dict), I-1, Number).

    
		      


