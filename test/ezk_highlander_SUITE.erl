
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

-define(HIGH_SERVER, 20).
-define(HIGH_RANDOM_RANGE, 100).
-define(HIGH2_SERVER, 100).
-define(HIGH2_NUMBER,20).
-define(HIGH2_SLEEP_SEND_THRESHOLD,60000).
-define(HIGH2_RANDOM_RANGE, 100000).

suite() ->
    [{timetrap,{seconds,2000}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
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
    [high_test
     , high2_test
    ].

high_test(_Config) ->
    Paras = [?HIGH_SERVER],
    lists:map(
      fun(I) -> high_tester(I) end, Paras),
    ok.

high_tester(I) ->
    io:format("Start testing with ~w highlanderwannabes~n",[I]),
    Dict = spawn_list_highlander(self(), dict:new(), 1, I),
    ok = high_wait(Dict, I),
    io:format("Test finished~n").

high_wait(_Dict, 0) ->
    ok;
high_wait(Dict, I) ->
    receive 
	{init, PId, _Path, FatherPId} ->
	    io:format("Starting with highlaner pid ~w",[PId]),
	    NewDict = dict:erase(FatherPId, Dict),
	    Cycles  = random:uniform(?HIGH_RANDOM_RANGE),
	    io:format("Doing ~w rounds with Node ~w", [Cycles, PId]),
	    ok      = send_receive_n(PId, Cycles),
	    ezk_highlander:failover(FatherPId, "test"),
	    io:format("Finished with highlaner number ~w~n",[PId]),
	    high_wait(NewDict,I-1)
    end.


%% ---------------------------- high 2 test------------------------

high2_test(_Config) ->
    Paras = [?HIGH2_SERVER],
    lists:map(
      fun(I) -> high2_tester(I) end, Paras),
    ok.
    
high2_tester(I) ->
    Dict = spawn_list_highlander(self(), dict:new(), ?HIGH2_NUMBER, I),
    ok = high2_wait(Dict, I, ?HIGH2_NUMBER).

high2_wait(_Dict, 0, _) ->
    ok;
high2_wait(Dict, Left, FreeSlots) ->
    receive
	{init, PId, Path, FatherPId} ->
	    io:format("Starting with highlander number ~s, afterwards ~w Slots left" ++
			  "and ~w instances waiting to get Highlanders",
		      [Path, FreeSlots-1, Left-1]),
	    if
		FreeSlots > 0 ->
		    Self = self(),
		    Cycles  = random:uniform(?HIGH2_RANDOM_RANGE),
		    io:format("Doing ~w rounds with Node ~s", [Cycles, Path]),	    
		    spawn(fun() -> receiver2(PId, Self, FatherPId, Path, Cycles) end),
		    NewDict = dict:erase(FatherPId, Dict),
		    high2_wait(NewDict, Left-1, FreeSlots-1);
		true ->
		    error_logger:error_msg("To many Highlanders")
	    end;
	{ended, FatherPId, Path} ->
	    io:format("Finished with highlaner on path ~s and now ~w Slots left~n",
		      [Path, FreeSlots+1]),
	    ezk_highlander:failover(FatherPId, "test"),
	    high2_wait(Dict, Left, FreeSlots+1)
    end.
	    

receiver2(Child, Caller, Father, Path, Cycles) ->
    if 
	Cycles > ?HIGH2_SLEEP_SEND_THRESHOLD ->
	    ok = send_receive_n(Child, Cycles);
	true ->
	    timer:sleep(Cycles)
    end,
    Caller ! {ended, Father, Path}.
    


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


spawn_list_highlander(_Butler, Dict, _Number, 0) ->
    Dict;
spawn_list_highlander(Butler, Dict, Number, I) ->
    {ok, FatherPId} = ?HIGHIMPL(Butler, Number),    
    io:format("Spawned Number ~w with pid ~w",[I, FatherPId]),
    spawn_list_highlander(Butler, dict:append(FatherPId, I, Dict), Number, I-1).

    
		      


