%% -------------------------------------------------------------------
%%
%% ezk_connection_manager: manages the ezk_connections
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

-module(ezk_connection_manager).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([start_connection/0, start_connection/1, end_connection/2]).

-include_lib("../include/ezk.hrl").

-define(SERVER, ?MODULE).

start_link(Args) ->
    gen_server:start_link({local,?SERVER}, ?MODULE, Args, []).

init(DefaultServers) ->	
    {ok, #con_man_state{defaultserverlist = DefaultServers,
			  connections       = [],
			  monitorings       = []}}.

start_connection() ->
    gen_server:call(?SERVER, {start_connection, []}).
start_connection(Servers) ->
    gen_server:call(?SERVER, {start_connection, Servers}).

end_connection(ConnectionPId, Reason) ->
    ?LOG(3, "Connection manager: Sending endconn message to myself"),
    gen_server:call(?SERVER, {end_connection, ConnectionPId, Reason}).

handle_call({start_connection, Servers}, _From, State) ->
    case Servers of
	[] ->
	    UsedServers = State#con_man_state.defaultserverlist;
	_Else -> 
	    UsedServers = Servers
    end,
    {ok, ConnectionPId} = ezk_connection:start(UsedServers),
    OldConnectionList   = State#con_man_state.connections,
    NewConnectionList   = [ConnectionPId | OldConnectionList], 
    NewState            = State#con_man_state{connections = NewConnectionList},
    {reply, {ok, ConnectionPId} , NewState};
handle_call({end_connection, ConnectionPId, Reason},  _From, State) ->
    ?LOG(3, "COnnection manager: got the endcon message."),
    ezk:die(ConnectionPId, Reason),
    {reply, ok, State}.


handle_cast(_Mes, State) ->
    {noreply, State}.

handle_info(_Mes, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    lists:map(fun(PId) ->
		      ezk:die(PId, Reason) end,
	      State#con_man_state.connections).
    
    

%% the needed swap function. 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
