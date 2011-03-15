-module(ezk_log).
-export([put/3, put/2]).
-define(LEVEL,4).
%% 0: nothing
%% 1: important things
%% 2: also sequenzer things
%% 3: most things
%% 4: also heartbeats

put(NeededLevel, Message, Parameter) ->
    if
       NeededLevel =< ?LEVEL ->
	    %%error_logger:info_report([{message, Message}, {parameter, Parameter}]),
	    io:format(("Logger: " ++ Message++"~n"), Parameter);
       true -> {}
    end.

put(NeededLevel, Message) ->
    if
       NeededLevel =< ?LEVEL ->
	    io:format(("Logger: " ++ Message++"~n"));
       true -> {}
    end.
			   
