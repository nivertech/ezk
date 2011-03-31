
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

-export([start_link/2, start_link/3, failover/2, get_child_pid/1]).
-export([change_number/2, change_number_hard/2]).

-record(tryargs, {mfa, mod_root, theonedir, theonenode, ident_str, number, child_pid}).

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
			   start_intern(DirName, MFA, Caller, 1) end),
    {ok, Callee}.

%% The Startfunction
%% DirName is the name of the highlander directory which should be used in 
%% ZooKeeper.
%% MFA is the MFA of the function that should be run with highlanderstrategy.
start_link(DirName, MFA, N) ->
    Caller = self(),
    Callee = spawn(fun() ->
			   start_intern(DirName, MFA, Caller, N) end),
    {ok, Callee}.
%% The function which manages the rise of the highlander.
%% a) starts ezk
%% b) creates the needed directories
%% c) goes in highlanderelection state
%% d) when successfully fought his way to the top sends the caller an ok.
%% e) waits for further requests.
start_intern(DirName, MFA, Caller, Number)  ->
    ok = start_ezk(),
    ModuleRoot = "/highlander/" ++ DirName,
    log(1,"Starting tryer"),
    ok = make_directorys(ModuleRoot),
    State = init_state(ModuleRoot, MFA),
    RunArgs = try_to_get_the_one(State, 0, Number),
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

%% builds the String paths
init_state(ModuleRoot, MFA) ->
    Identifier = atom_to_list(node()) ++ " " ++ pid_to_list(self()),
    TheOneNode = ModuleRoot ++ "/theone/iamtheone",
    TheOneDir = ModuleRoot ++ "/theone",
    #tryargs{mfa = MFA, mod_root = ModuleRoot, theonedir = TheOneDir,
		     theonenode = TheOneNode, ident_str =Identifier}.

%% creates an epheremal node with the state path and the actual number as suffix
try_create_e(State, Number) ->
    TheOneNode = State#tryargs.theonenode ++ [Number+48],
    ezk:create(TheOneNode, State#tryargs.ident_str, e).
    
%% Starts a child process which computes the mfa given in state.
start_child(State, Number) ->
    receive
	{theonedied, _I1} -> ok
    end,
    {Module, Function, Args}  = State#tryargs.mfa,
    log(1, "Got the one number ~w~n",[Number]),
    Self = self(),
    Id = spawn_link(Module, Function, [Number | [Self | Args]]),
    State#tryargs{number = Number, child_pid = Id}.
    

%% trys to get the highlander.
%% If the node can be created it is the highlander and starts. 
%% 
try_to_get_the_one(State, 0, MaxNumber) ->
    log(1,"watch set"),
    TheOneDir = State#tryargs.theonedir,
    {ok, _I2} = ezk:ls(TheOneDir, self(), theonedied),
    try_to_get_the_one(State, 1, MaxNumber);
try_to_get_the_one(State, MaxNumber, MaxNumber) ->
    case try_create_e(State, MaxNumber) of
	{ok, _I} ->
	    start_child(State, MaxNumber);
	_Else ->
	    receive
		{theonedied, _I1} ->
		    log(1,"Received watch"),
		    try_to_get_the_one(State, 0, MaxNumber);
		{change_number, NewNumber} ->
		    try_to_get_the_one(State, 1, NewNumber);
		{change_number_hard, NewNumber} ->
		    try_to_get_the_one(State, 1, NewNumber)
	    end
    end;
try_to_get_the_one(State, TryToGet, MaxNumber) ->	
    case try_create_e(State, TryToGet) of
	{ok, _I} ->
	    log(1,"I am Number ~w",[TryToGet]),
	    start_child(State, TryToGet);
	_Else ->
	    try_to_get_the_one(State, TryToGet+1, MaxNumber)
    end.

call(PId, Atom, Para) ->
    PId ! {Atom, self(), Para},
    receive
	{Atom, Reply} ->
	    Reply
    end.
	


%% called to stop an actual highlander.
failover(PId, Reason) ->
    call(PId, failover, Reason).

change_number(PId, NewNumber) ->
    call(PId, change_number, NewNumber).

change_number_hard(PId, NewNumber) ->
    call(PId, change_number_hard, NewNumber).
    
get_child_pid(PId) ->
    call(PId, get_child_pid, undef).


    
%% the loop the highlander gets into if he got the choosen.
running(State) ->
    log(1,"waiting~n"),
    receive
	{failover, From, Reason} ->
	    log(1,"Shutting down"),
	    {Module, _F, _A} = State#tryargs.mfa,
	    ChildPId = State#tryargs.child_pid,
	    Module:terminate(ChildPId, Reason),
	    log(1,"stated terminate to child"),
	    timer:sleep(2000),
	    process_flag(trap_exit, true),
	    log(1,"kill remaining childs"),
	    erlang:exit(ChildPId, Reason),
	    TheOneNode = State#tryargs.theonenode ++ [State#tryargs.number+48],
	    log(1,"delete zoonode"),
	    ezk:delete(TheOneNode),
	    log(1,"Shutdown complete"),
	    From ! {failover, ok},
	    {terminated, Reason};
	{get_child_pid, From, undef} ->
	    log(1,"got childpid request~n"),
	    ChildPId = State#tryargs.child_pid,
	    log(1,"childid is ~w~n",[ChildPId]),
	    From ! {get_child_pid, {ok, ChildPId}},
	    log(1,"sended childpid reply~n"),
	    running(State);
	{change_number_hard, From, NewNumber} ->
	    OwnNumber = State#tryargs.number,
	    if
		OwnNumber > NewNumber ->
		    From ! {change_number_hard, {ok, killed}},
		    exit("To many Highlander");
		true -> 
		    From ! {change_number_hard, {ok, alive}}
	    end;	    
	Else ->
	    log(0,"GOT A MESSAGE: ~w ~n",[Else]),
	    running(State)	    
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
