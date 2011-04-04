-module(ezk_highlander).

-define(MODULE,ezk_highlander).
behaviour(gen_server).

behaviour_info(callbacks) ->
    [{terminate,2}].

start_link(Args = [Nodelist, Module, Parameters]) ->
    gen_server:start_link( ?MODULE, Args, []).

init(NodeList, Module, Parameters) ->
    lists:map(fun(Path) ->
		      Father = get_father(Path),
		      ezk:ensure_path(Father) 
	      end, NodeList),
    case try_first(NodeList, Module, Parameters) of
	no_luck ->
	    Ident = pid_to_list(self())++ " " atom_to_list(node()),
	    try_again(Module, Parameters, Ident);
	{ok, State} ->
	    State
    end.

try_again(Module, Parameters, Ident) ->
    receive
	{{nodechanged, Path}, _I} ->
	    case (try_to_get(Path, Ident)) of
		{ok, _I} ->
		    spawn_link(
		    ok;
		{error, _I} ->
		    try_again(Module, Parameters, Ident)
	    end
    end.


make_dirs


%% Trys to create a the in Path specified node with data Ident.
%% Also sets a childwatch to its father with message {nodechanged, Path}.
try_to_get(Path, Ident) ->
    Father = get_father(Path),
    ezk:ls(Father, self(), {nodechanged, Path}),
    ezk:create(Path, Ident, e).

get_father(Path) ->
    FullPath = string:tokens(Path, "/"),
    get_father_from_list(FullPath).

get_father_from_list([]) ->
    "";
get_father_from_list([ H | T ]) ->
    "/" ++ H ++ get_father_from_list(T).

    
    
%% watches immer auf die elternknoten koennte ein guter trick sein
%% aufpassen beim rootknoten!
