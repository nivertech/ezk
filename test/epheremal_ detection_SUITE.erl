
-module('epheremal_ detection_SUITE').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,120}}].

init_per_suite(Config) ->
    application:start(ezk),
    application:start(sasl),
    {ok, Con0} = ezk:start_connection(),
    ezk:delete_all(Con0, "/"),
    ezk:end_connection(Con0, "cleaned up"),
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
    %% {skip, test}.
     [epheremal_monitoring].

epheremal_monitoring(_Config) ->
    Self = self(),
    {ok, Con0} = ezk:start_connection(),
    {ok, Con1} = ezk:start_connection(),
    {ok, Con2} = ezk:start_connection(),
    {ok, Con3} = ezk:start_connection(),
    io:format("main : Connections started"),
    _Kid1 = spawn(fun() -> makewait(Con1, 90000, "/kid1") end),
    _Kid2 = spawn(fun() -> makemonwaitdie(Con2, 10000, "/kid2", Self) end),
    Kid3 = spawn(fun() -> timer:sleep(100000) end),
    Kid4 = spawn(fun() -> timer:sleep(100000) end),
    io:format("main : Kids spawned"),
    timer:sleep(3000),
    {ok, _I0} = ezk:get(Con0, "/kid1"),
    {ok, _I1} = ezk:get(Con0, "/kid2"),
    {ok, _I3} = ezk:create(Con3, "/kid3", "datac", e),
    ezk:add_monitors(Con3, [Kid3, Kid4]),
    timer:sleep(3000),
    exit(Kid4, "test"),
    io:format("main : lses ok"),
    receive
	{killing, Con2} ->
	    io:format("main : got killing message, now waiting"),
	    timer:sleep(33000),
	    io:format("main : trying to verify nodestatus"),
	    Status1 = ezk:get(Con0, "/kid1"),
	    Status2 = ezk:get(Con0, "/kid2"),
	    Status3 = ezk:get(Con0, "/kid3"),
	    io:format("main: status 1 and 2 and 3 are:~n ~w ~n and ~w ~n and ~w",
		      [Status1, Status2, Status3]),
	    {ok, _S1} = Status1,
	    {error, _S2} = Status2,
	    {error, _S2} = Status3
    end,
	    io:format("main : Killing connections"),
    ezk:end_connection(Con0, "test"),
    ezk:end_connection(Con1, "test").
    
    

makewait(Con, Time, Node) ->
    io:format("makewait: create node"),
    ezk:create(Con, Node, "data", e),
    io:format("makewait: waiting"),
    timer:sleep(Time),
    io:format("makewait: ending"),
    ezk:delete(Con, Node).

makemonwaitdie(Con, Time, Node, Father) ->
    io:format("makemonwaitdie: create node"),
    ezk:create(Con, Node, "datb", e),
    io:format("makemonwaitdie: add monitor"),
    ezk:add_monitors(Con, [self()]),
    io:format("makemonwaitdie: waiting"),
    timer:sleep(Time),    
    io:format("makemonwaitdie: send message to father"),
    Father ! {killing, Con},
    io:format("makemonwaitdie: die"),
    exit("testreason").
			 
		  
    
