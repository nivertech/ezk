-module(ct_log).

-define(LOGFILE, ct_log.txt).
-export([log/2]).
-export([suite_init/1, suite_end/2]).
-export([group_init/1, group_end/2]).

%% its not very cute, but get_config did not work and this seems to be the savest way.
log(String, Var) ->
    {ok, Logfile}  = file:open(?LOGFILE, [append]),
    io:format(Logfile, String, Var),
    file:close(Logfile).

suite_init(SuiteName) ->
    log("------------------------------------~n",[]),
    log(" Start test for suite ~s.~n~n",[SuiteName]).

suite_end(SuiteName, Elapsed) ->
    log(" Finished test for suite ~s in ~w seconds .~n",
	[SuiteName, (trunc(Elapsed/10000)/100)]),
    log("------------------------------------~n",[]).

group_init(GroupName) ->
    log(" Start test for group ~w.~n",[GroupName]).

group_end(GroupName, Elapsed) ->
    log(" Finished test for group ~w in ~w seconds .~n~n",
	[GroupName, (trunc(Elapsed/10000)/100)]).

