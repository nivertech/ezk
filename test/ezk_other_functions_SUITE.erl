%%%-------------------------------------------------------------------
%%% @author Marco <marco@gauss.gi.net>
%%% @copyright (C) 2011, Marco
%%% @doc
%%%
%%% @end
%%% Created : 14 Apr 2011 by Marco <marco@gauss.gi.net>
%%%-------------------------------------------------------------------
-module(ezk_other_functions_SUITE).

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
    %% {skip, test}.
    [help_test, acl_test].

help_test(_Config) ->
    ezk:help().

acl_test(Config) ->    
    {connection_pid, ConPId} = lists:keyfind(connection_pid, 1, Config),   
    {ok, Node} = ezk:create(ConPId, "/test", "data", es, 
			    [
			     {[a], "world", "anyone"}, {[w,r], "world", "anyone"}
			    ]),
    true       = test_an_acl(ConPId, Node, a),
    true       = test_an_acl(ConPId, Node, w),
    true       = test_an_acl(ConPId, Node, r),
    false      = test_an_acl(ConPId, Node, c),
    false      = test_an_acl(ConPId, Node, d),
    {ok, {OldACLSBin, _I}} = ezk:get_acl(ConPId, Node),
    OldACLS = make_acls_to_lists(OldACLSBin),
    NewACLS = [ {[d], "world","anyone"} | OldACLS],   
    io:format("Setting new acls: ~w",[NewACLS]),
    {ok, _I1}   = ezk:set_acl(ConPId, Node, NewACLS),
    io:format("ACLS set."),
    true       = test_an_acl(ConPId, Node, a),
    true       = test_an_acl(ConPId, Node, w),
    true       = test_an_acl(ConPId, Node, r),
    false      = test_an_acl(ConPId, Node, c),
    true       = test_an_acl(ConPId, Node, d).

make_acls_to_lists([]) ->
    [];
make_acls_to_lists([{Permissions, Scheme, Id} | List]) ->
    NewScheme = binary_to_list(Scheme),
    NewId = binary_to_list(Id),
    [{Permissions, NewScheme, NewId} | make_acls_to_lists(List)].

test_an_acl(ConPId, Node, Atom) ->
    {ok, {ACLS, _I}}        = ezk:get_acl(ConPId, Node),
    io:format("outer: testing for ~w in list:  ~w",[Atom, ACLS]),
    test_an_acl_list(Atom, ACLS).

test_an_acl_list(_Atom, []) ->    
    false;
test_an_acl_list(Atom, [{Permissions, Scheme, Id} | List]) -> 
    io:format("inner: testing for ~w in ~w",[Atom, Permissions]),
    Any = list_to_binary("anyone"),
    Wor = list_to_binary("world"),
    case {Scheme, Id} of 
	{Wor, Any} ->
	    case is_elem(Atom, Permissions) of
		true ->
		    true;
		false  ->
		    test_an_acl_list(Atom , List)
	    end;
	_Else ->
	    test_an_acl_list(Atom, List)
    end.

is_elem(_Elem, []) ->    
    false;
is_elem(Elem, [First | Tail]) ->
    if
	Elem == First ->
	    true;
	true ->
	    is_elem(Elem, Tail)
    end.


    
    
			    
    
