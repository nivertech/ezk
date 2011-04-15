%%%-------------------------------------------------------------------
%%% @author Marco <marco@gauss.gi.net>
%%% @copyright (C) 2011, Marco
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2011 by Marco <marco@gauss.gi.net>
%%%-------------------------------------------------------------------
-module(ezk_other_functions).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

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
    {skip, test}.
    %% [help_test, acl_test].

help_test(_Config) ->
    ezk:help().

acl_test(Config) ->    
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),   
    {ok, Node} = ezk:create(ConPId, "test", "data", es, 
			    [{"r", "r", [r]}, {"w", "w", [w]}]),
    true       = test_an_acl(ConPId, Node, "r", r),
    true       = test_an_acl(ConPId, Node, "w", w),
    false      = test_an_acl(ConPId, Node, "a", a),
    {ok, _I}   = ezk:set_acl(ConPId, Node, [{"a","a", [a]}]),
    true       = test_an_acl(ConPId, Node, "r", r),
    true       = test_an_acl(ConPId, Node, "w", w),
    true       = test_an_acl(ConPId, Node, "a", a).

test_an_acl(ConPId, Node, String, Atom) ->
    Bin                     = list_to_binary(String),
    {ok, {ACLS, _I}}        = ezk:get_acl(ConPId, Node),
    {Permissions, Bin, Bin} = lists:keyfind(Bin, 2, ACLS), 
    is_elem(Atom, Permissions).
    
is_elem(_Elem, []) ->    
    false;
is_elem(Elem, [First | Tail]) ->
    if
	Elem == First ->
	    true;
	true ->
	    is_elem(Elem, Tail)
    end.


    
    
			    
    
