
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

-export([start_link/2, run/4, terminate/2]).

-behaviour(ezk_highlander).

start_link(WaiterPId, I) ->
    ezk_highlander:start_link("test", {test_highlander_impl, run, [WaiterPId, I]},1).

run(Id, Father, WaiterPId, I) ->
    io:format("The one is now ~w", [I]),
    WaiterPId ! {init, self(), I, Father},
    loop().

loop() ->
    receive
	{ping, WaiterPId} ->
	    WaiterPId ! {pong, self()},
	    loop();
	die -> 
	    ok
    end.

terminate(PId, _I) ->
    PId ! die.
