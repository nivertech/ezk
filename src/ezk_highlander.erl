
%% -------------------------------------------------------------------
%%
%% ezk_highlander: A Behaviour that assures that exactly one instance of a given
%%                 Module is active.
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

-module(ezk_highlander).

-export([behaviour_info/1]).

-export([start_link/2, failover/2, get_child_pid/1]).

-record(runargs, {highland_root, mfa, runpid}).

-define(LEVEL, 0).

behaviour_info(callbacks) ->
    [{terminate,2}].

%% The Startfunction
%% DirName is the name of the highlander directory which should be used in 
%% ZooKeeper.
%% MFA is the MFA of the function that should be run with highlanderstrategy.
start_link(DirName, MFA) ->
    Caller = self(),
    Callee = spawn(fun() ->
			   start_intern(DirName, MFA, Caller) end),
    {ok, Callee}.

%% The function which manages the rise of the highlander.
%% a) starts ezk
%% b) creates the needed directories
%% c) goes in highlanderelection state
%% d) when successfully fought his way to the top sends the caller an ok.
%% e) waits for further requests.
start_intern(DirName, MFA, Caller)  ->
    ok = start_ezk(),
    ModuleRoot = "/highlander/" ++ DirName,
    ok = make_directorys(ModuleRoot),
    RunArgs = try_to_get_the_one(ModuleRoot, MFA),
    log(1, "The Caller id is ~w, my own is ~w ~n ",[Caller, self()]),
    running(RunArgs).
    
%% assures that ezk runs (returns ok | error)
start_ezk() ->
    Erg = application:start(ezk),
    case Erg of
	ok -> ok;
	{error, {already_started, ezk}}  -> ok;
	_Else  -> error
    end.

%% trys to get the highlander.
%% a) creating the epheremal highlandernode
%% if succeeded starts the mfa and determines the data needed for own loop
%% if not sets a childwatch to the father of the highlander
%% and when watch is triggered starts all over again.
try_to_get_the_one(ModuleRoot, MFA = {Module, Function, Args}) ->
    Self = self(),
    log(1, "try to get the one ~w ~n", [Self]),
    Identifier = atom_to_list(node()) ++ " " ++ pid_to_list(Self),
    TheOneNode = ModuleRoot ++ "/theone/iamtheone",
    TheActualOne = ezk:create(TheOneNode, Identifier, e),
    case TheActualOne of
	{ok, _I} ->
	    Id = spawn_link(Module, Function, [Self | Args]),
	    #runargs{runpid = Id, mfa = MFA, highland_root = ModuleRoot};
	_Else ->
	    log(1, "failed to get the one ~w ~n", [Self]),
	    TheOneDir = ModuleRoot ++ "/theone",
	    {ok, _I2} = ezk:ls(TheOneDir, Self, theonedied),
	    log(1, "set watch ~w ~n", [Self]),
	    %%It can happen that the one dies before the childwatch is set.
	    %%Therefor we look if it happened.
	    NoOneKilledTheNode = ezk:get(TheOneNode),
	    case NoOneKilledTheNode of
		{ok, _I} ->
		    log(1, "watch works ~w ~n", [Self]),
		    receive
			{theonedied, _I1} ->
			    try_to_get_the_one(ModuleRoot, MFA)
		    end;
		_Else2 -> 
		    log(1, "watch corrupted ~w ~n", [Self]),
		    try_to_get_the_one(ModuleRoot, MFA)
	    end
    end.

%% called to stop an actual highlander.
failover(PId, Reason) ->
    PId ! {failover, self(), Reason},
    receive
	{failover, ok} ->
	    ok
    end.

get_child_pid(PId) ->
    log(1,"send childpid reqest~n"),
    PId ! {get_child_pid, self()},
    log(1,"wait for childpid reply~n"),
    receive
	{get_child_pid, ChildPId} ->
	    {ok, ChildPId}
    end.
    
%% the loop the highlander gets into if he got the choosen.
running(RunArgs) ->
    log(1,"waiting~n"),
    receive
	{failover, From, Reason} ->
	    log(1,"Shutting down"),
	    {Module, _F, _A} = RunArgs#runargs.mfa,
	    ChildPId = RunArgs#runargs.runpid,
	    ModuleRoot = RunArgs#runargs.highland_root,
	    Module:terminate(ChildPId, Reason),
	    log(1,"stated terminate to child"),
	    timer:sleep(2000),
	    process_flag(trap_exit, true),
	    log(1,"kill remaining childs"),

	    erlang:exit(ChildPId, Reason),
	    TheOne = ModuleRoot ++ "/theone/iamtheone",
	    log(1,"delete zoonode"),
	    ezk:delete(TheOne),
	    log(1,"Shutdown complete"),
	    From ! {failover, ok},
	    {terminated, Reason};
	{get_child_pid, From} ->
	    log(1,"got childpid request~n"),
	    ChildPId = RunArgs#runargs.runpid,
	    log(1,"childid is ~w~n",[ChildPId]),
	    From ! {get_child_pid, ChildPId},
	    log(1,"sended childpid reply~n"),
	    running(RunArgs);
	Else ->
	    log(0,"GOT A MESSAGE: ~w ~n",[Else]),
	    running(RunArgs)	    
    end.

%% secures that the zkNodes
%% /highlander/${ModuleRoot}/states and /highlander/${ModuleRoot}/theone are there. 
make_directorys(ModuleRoot) ->
    ezk:create("/highlander", "The Highlands"),
    StateDir  = ModuleRoot ++ "/states",
    TheOneDir = ModuleRoot ++ "/theone",
    case (ezk:ls(ModuleRoot)) of
	{error, _I1} ->
	    ezk:create(ModuleRoot, "The Domain of " ++ ModuleRoot);
	{ok, _I1} ->
	    ok
    end,
    case ( ezk:ls(ModuleRoot ++ "/states")) of
	{error, _I2} ->
	    ezk:create(StateDir, "Replica of the ones state");
	{ok, _I2} ->
	    ok
    end,
    case ( ezk:ls(ModuleRoot ++ "/theone")) of
	{error, _I3} ->
	    ezk:create(TheOneDir, "Home of the choosen");
	{ok, _I3} ->
	    ok
    end,
    ok.

log(Level, String) ->
    log(Level, String, []).
log(Level, String, Args) ->
    if
	Level =< ?LEVEL ->
	    io:format(String, Args);
	true ->
	    ok
    end.
