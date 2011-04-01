
%% -------------------------------------------------------------------
%%
%% ezk_ls_SUITE: performs various loops of  ls commands in parallel to test high load
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
-module(ezk_ls_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(LS_RUNS, 50).
-define(PAR_RUNS, 900).

suite() ->
    [{timetrap,{seconds,700}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
    application:stop(ezk),
    application:stop(sasl),
    ok.

init_per_group(GroupName, Config) ->
    Config.

end_per_group(GroupName, Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [ls1, ls5, ls10, ls50, ls75, ls100].
    %% {skip, test}.

ls1(_Config) -> parteststarter:start((?PAR_RUNS div 100),
				       ezk_ls_SUITE, ls_test, [?LS_RUNS]).
ls5(_Config) -> parteststarter:start((?PAR_RUNS div 20), 
				       ezk_ls_SUITE, ls_test, [?LS_RUNS]).
ls10(_Config) -> parteststarter:start((?PAR_RUNS div 10), 
				       ezk_ls_SUITE, ls_test, [?LS_RUNS]).
ls50(_Config) -> parteststarter:start((?PAR_RUNS div 5), 
				       ezk_ls_SUITE, ls_test, [?LS_RUNS]).
ls75(_Config) -> parteststarter:start((?PAR_RUNS div 2), 
				       ezk_ls_SUITE, ls_test, [?LS_RUNS]).
ls100(_Config) -> parteststarter:start((?PAR_RUNS), 
				       ezk_ls_SUITE, ls_test, [?LS_RUNS]).


ls_test(0) ->
    ok;
ls_test(N) ->
    {ok, _E} = ezk:ls("/"),
    ls_test(N-1).

