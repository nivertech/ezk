-module(ct_log).

-define(LOGFILE, ct_log.txt).
-export([log/2]).
-export([suite_init/1, suite_end/3]).
-export([group_init/1, group_end/3]).

%% its not very cute, but get_config did not work and this seems to be the savest way.
log(String, Var) ->
    {ok, Logfile}  = file:open(?LOGFILE, [append]),
    io:format(Logfile, String, Var),
    file:close(Logfile).

suite_init(SuiteName) ->
    log("------------------------------------~n",[]),
    log(" Start test for suite ~s.~n~n",[SuiteName]).

suite_end(SuiteName, Elapsed, Iter) ->
    SecElapsed = (trunc(Elapsed/10000)/100),
    IterPerSecond = trunc((Iter/SecElapsed)*100)/100,
    log(" Finished test for suite ~s in ~w seconds with ~w Iterations .~n",
	[SuiteName, SecElapsed, Iter]),
    log(" Thats ~w Iterations per Second~n",[IterPerSecond]),
    log("------------------------------------~n",[]).

group_init(GroupName) ->
    log(" Start test for group ~w.~n",[GroupName]).

group_end(GroupName, Elapsed, Iter) ->
    SecElapsed = (trunc(Elapsed/10000)/100),
    IterPerSecond = trunc((Iter/SecElapsed)*100)/100,
    log(" Finished test for group ~w in ~w seconds with ~w iterations .~n",
	[GroupName, SecElapsed, Iter]),
    log(" Thats ~w Iterations per Second~n~n",[IterPerSecond]).

