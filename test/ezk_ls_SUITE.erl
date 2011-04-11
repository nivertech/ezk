
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
-define(PAR_RUNS, 350).

suite() ->
    [{timetrap,{seconds,700}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    {ok, ConnectionPId} = ezk:start_connection(),
    [{connection_pid, ConnectionPId}  | Config].

end_per_suite(Config) ->
    {connection_pid, ConnectionPId} = lists:keyfind(connection_pid, 1, Config),
    ezk:end_connection(ConnectionPId, "Test finished"),
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
    [ ls1,  ls5,  ls10,  ls20,  ls50,  ls100,
    nls1, nls5, nls10, nls20, nls50, nls100].
     %% {skip, test}.

ls1(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    io:format("Starting test with ConPid ~w",[ConPId]),
    parteststarter:start((?PAR_RUNS div 100), ezk_ls_SUITE, ls_test, [?LS_RUNS, ConPId]).
ls5(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 20), ezk_ls_SUITE, ls_test, [?LS_RUNS, ConPId]).
ls10(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 10), ezk_ls_SUITE, ls_test, [?LS_RUNS, ConPId]).
ls20(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 5), ezk_ls_SUITE, ls_test, [?LS_RUNS, ConPId]).
ls50(Config) ->
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
     parteststarter:start((?PAR_RUNS div 2), ezk_ls_SUITE, ls_test, [?LS_RUNS, ConPId]).
ls100(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS), ezk_ls_SUITE, ls_test, [?LS_RUNS, ConPId]).

nls1(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 100), ezk_ls_SUITE, nls_test, [?LS_RUNS, ConPId]).
nls5(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 20), ezk_ls_SUITE, nls_test, [?LS_RUNS, ConPId]).
nls10(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 10), ezk_ls_SUITE, nls_test, [?LS_RUNS, ConPId]).
nls20(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 5), ezk_ls_SUITE, nls_test, [?LS_RUNS, ConPId]).
nls50(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS div 2), ezk_ls_SUITE, nls_test, [?LS_RUNS, ConPId]).
nls100(Config) -> 
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),    
    parteststarter:start((?PAR_RUNS), ezk_ls_SUITE, nls_test, [?LS_RUNS, ConPId]).

ls_test(0, _ConPId) ->
    ok;
ls_test(N, ConPId) ->
    {ok, _E} = ezk:ls(ConPId, "/"),
    ls_test(N-1, ConPId).

nls_test(N, ConPId) ->
    Self = self(),
    io:format("starting receiverchild with ~w Rounds",[N]),
    K = spawn(fun() ->
		      receive_ls(N, Self) end),
    io:format("starting to send"),
    send_ls(N, K, ConPId),
    io:format("all send"),
    receive
	all_ls_received ->
	    ok
    end.

send_ls(0, _Child, _ConPId) ->
    ok;
send_ls(N, Child, ConPId) ->
    ezk:n_ls(ConPId, "/", Child, ls),
    send_ls(N-1, Child, ConPId).


receive_ls(0, Father) ->
    Father ! all_ls_received,
    ok;
receive_ls(N, Father) ->
    receive
	{ls, {ok, _I}} ->
	    io:format("got one ~w", [self()]),
	    receive_ls(N-1, Father);
	Else  ->
	    io:format("I got Something: ~w ~n",[Else])		
    end.
