
%% -------------------------------------------------------------------
%%
%% test_highlander_impl: Testimplementation of a highlander which is used for testing.
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
%% --------------------------------------------------------------------

-module(test_highlander_impl).

-export([start_link/3, init/2, terminate/2]).

-behaviour(ezk_highlander).

start_link(ConnectionPId, ButlerPId, Number) ->
    io:format("Try to start number ~w",[Number]),
    Numbers = lists:seq(1,Number),
    Paths = lists:map(fun(Num) -> ("/highlander/test/node" ++ [Num+48]) end, Numbers),
    %% io:format("Spawn with Paths ~s",[Paths]),
    Ergo = ezk_highlander:start(ConnectionPId, test_highlander_impl, [ButlerPId], Paths),
    %% io:format("Result of spawning is ~w",[Ergo]),
    Ergo.

init(_Path, [ButlerPId]) ->
    Father = self(),
    ChildPId = spawn(fun() ->
			  run(Father, ButlerPId) end),
    io:format("Child sucessfully spawned with pid ~w from ~w",[ChildPId, Father]),
    _State = {ok,ChildPId}.

run(Father, ButlerPId) ->
    io:format("~w is now father of the one.",[Father]),
    ButlerPId ! {init, self(), path, Father},
    io:format("~w 's child is now entering the loop.",[Father]),
    loop().

loop() ->
    receive
	{ping, PingPongPId} ->
	    PingPongPId ! {pong, self()},
	    loop();
	die -> 
	    ok
    end.

terminate(State, _Reason) ->
    LoopPId = State,
    LoopPId ! die.
