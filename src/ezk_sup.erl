
-module(ezk_sup).

-behaviour(supervisor).
-include_lib("../include/ezk.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Args) ->
supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Args) ->
?LOG(0,"args: ~w",[Args]),
    ?LOG(1,"Supervisor: making Childspec."),
    Connection = ?CHILD(ezk_connection,worker,Args),
    ChildSpec = [Connection],
    ?LOG(1,"Supervisor: done Childspec: ~w.",[ChildSpec]),
    {ok, { {one_for_one, 500, 10}, ChildSpec} }.

