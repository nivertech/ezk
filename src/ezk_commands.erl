%% -------------------------------------------------------------------
%%
%% ezk_commands: The Ezk commands for the ezk_connections gen_server
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

-module(ezk_commands).

%% gen_server callbacks
-export([addauth/3, die/2]).
%normal functions
-export([  create/3,   create/4,   create/5,   delete/2,   set/3,   set_acl/3]).
-export([n_create/5, n_create/6, n_create/7, n_delete/4, n_set/5, n_set_acl/5]).
-export([  get/2,   get_acl/2,   ls2/2,   ls/2]).
-export([n_get/4, n_get_acl/4, n_ls2/4, n_ls/4]).
%functions dealing with watches
-export([ls/4, get/4, ls2/4]).
%macros
-export([delete_all/2, ensure_path/2]).
%infos
-export([info_get_iterations/1]).

-include_lib("../include/ezk.hrl").

-define(HEARTBEATTIME, 10000).
    
%% Kills the Server (not the supervisor!)
die(ConnectionPId, Reason) -> 
    ?LOG(3,"Killcommand arrived in commands"),
    gen_server:call(ConnectionPId, {die, Reason}).

%%--------------------------- Zookeeper Functions ---------------------
%% All Return {ok, Reply} if it worked.

%% Reply = authed 
%% Returns {error, auth_in_progress}  if the authslot is already in use.
%% Returns {error, auth_failed} if server rejected auth
%% Returns {error, unknown, ErrorCodeBin} if something new happened
addauth(ConnectionPId, Scheme, Auth) ->
   gen_server:call(ConnectionPId, {addauth, Scheme, Auth}).

%% Creates a new ZK_Node
%% Reply = Path where Path = String
create(ConnectionPId, Path, Data) ->
     
		  gen_server:call(ConnectionPId, {command, {create, Path, 
							    Data, [], [undef]}}).
n_create(ConnectionPId, Path, Data, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {nbcommand, {create, Path,
							      Data, [], [undef]}, 
				   Receiver, Tag}).
%% Typ = e | s | es (stands for etheremal, sequenzed or both)
create(ConnectionPId, Path, Data, Typ) ->
     
		  gen_server:call(ConnectionPId, {command, {create, Path,
							    Data, Typ, [undef]}}).
n_create(ConnectionPId, Path, Data, Typ, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {nbcommand, {create, Path,
							      Data, Typ, [undef]}, 
				   Receiver, Tag}).

%% Acls = [Acl] where Acl = {Permissions, Scheme, Id} 
%% with Scheme and Id = String
%% and Permission = [Per] | String 
%% where Per = r | w | c | d | a
create(ConnectionPId, Path, Data, Typ, Acls)  ->
     
		  gen_server:call(ConnectionPId, {command, {create, Path,
							    Data, Typ, Acls}}).
n_create(ConnectionPId, Path, Data, Typ, Acls, Receiver, Tag)  ->
     
		  gen_server:cast(ConnectionPId, {nbcommand, {create, Path,
							      Data, Typ, Acls}, 
				   Receiver, Tag}).

ensure_path(ConnectionPId, Path) ->
     
		  macro_ensure_path(ConnectionPId, Path).

%% Deletes a ZK_Node
%% Only working if Node has no children.
%% Reply = Path where Path = String
delete(ConnectionPId, Path) ->
     
		  gen_server:call(ConnectionPId, {command, {delete,  Path,
							    []}}).
n_delete(ConnectionPId, Path, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {nbcommand, {delete,  Path,
							      []}, Receiver, Tag}).

%% Deletes a ZK_Node and all his childs.
%% Reply = Path where Path = String
%% If deleting some nodes violates the acl
%% or gets other errors the function tries the
%% other nodes befor giving the error back, so a 
%% maximum number of nodes is deleted.
delete_all(ConnectionPId, Path) ->
     
		  macro_delete_all_childs(ConnectionPId, Path).    

%% Reply = {Data, Parameters} where Data = The Data stored in the Node
%% and Parameters = [{ParameterName, Value}]
%% where ParameterName = czxid | mzxid | pzxid | ctime | mtime | dataversion | 
%%                       datalength | number_children | cversion | aclversion
get(ConnectionPId, Path) ->
     
		  gen_server:call(ConnectionPId, {command, {get, Path}}).
n_get(ConnectionPId, Path, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {command, {get, Path},
						  Receiver, Tag}).
%% Like the one above but sets a datawatch to Path.
%% If watch is triggered a Message M is send to the PId WatchOwner
%% M = {WatchMessage, {Path, Type, SyncCon}
%% with Type = child
get(ConnectionPId, Path, WatchOwner, WatchMessage) ->
     
		  gen_server:call(ConnectionPId, {watchcommand, {get, getw, 
								 Path,
								 {data, WatchOwner,
								  WatchMessage}}}).

%% Returns the actual Acls of a Node
%% Reply = {[ACL],Parameters} with ACl and Parameters like above
get_acl(ConnectionPId, Path) ->
     
		  gen_server:call(ConnectionPId, {command, {get_acl, 
							    Path}}).
n_get_acl(ConnectionPId, Path, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {command, {get_acl, Path},
						  Receiver, Tag}).

%% Sets new Data in a Node. Old ones are lost.
%% Reply = Parameters with Data like at get
set(ConnectionPId, Path, Data) ->
     
		  gen_server:call(ConnectionPId, {command, {set, Path,
							    Data}}).
n_set(ConnectionPId, Path, Data, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {command, {set, Path,
							    Data}, Receiver, Tag}).

%% Sets new Acls in a Node. Old ones are lost.
%% ACL like above.
%% Reply = Parameters with Data like at get
set_acl(ConnectionPId, Path, Acls) ->
     
		  gen_server:call(ConnectionPId, {command, {set_acl, Path,
							    Acls}}).
n_set_acl(ConnectionPId, Path, Acls, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {command, {set_acl, Path,
							    Acls}, Receiver, Tag}).

%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = [ChildName] where ChildName = <<"Name">>
ls(ConnectionPId, Path) ->
     
		  gen_server:call(ConnectionPId, {command, {ls, Path}}).
n_ls(ConnectionPId, Path, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {nbcommand, {ls, Path},
						  Receiver, Tag}).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls(ConnectionPId, Path, WatchOwner, WatchMessage) ->
    ?LOG(3,"Connection: Send lsw"),
     
		  gen_server:call(ConnectionPId, {watchcommand, {ls, lsw,  
								 Path,
								 {child, WatchOwner,
								  WatchMessage}}}).

%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = {[ChildName],Parameters} with Parameters and ChildName like above.
ls2(ConnectionPId, Path) ->
     
		  gen_server:call(ConnectionPId, {command, {ls2, Path}}).
n_ls2(ConnectionPId, Path, Receiver, Tag) ->
     
		  gen_server:cast(ConnectionPId, {command, {ls2, Path},
						  Receiver, Tag}).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls2(ConnectionPId, Path, WatchOwner, WatchMessage) ->
     
		  gen_server:call(ConnectionPId, {watchcommand, {ls2, ls2w,
								 Path
								 ,{child, WatchOwner,
								   WatchMessage}}}).

%% Returns the Actual Transaction Id of the Client.
%% Reply = Iteration = Int.
info_get_iterations(ConnectionPId) ->
    gen_server:call(ConnectionPId, {info, get_iterations}).


%% A Macro which deletes a Node and all his Childs.
%% a) List children of Node. If he has none everything is all right.
%% b) If he has some: kill them and their Children rekursively.
%% c) Kill the Node with delete    
macro_delete_all_childs(ConnectionPId, Path) ->
    ?LOG(3, "Delete All: Trying to Erase ~s",[Path]),
    Childs = ls(ConnectionPId, Path),
    case Childs of
        {ok, []} ->
	    ?LOG(3, "Killing ~s",[Path]),
	    delete(ConnectionPId, Path);
	{ok, ListOfChilds} ->
	    ?LOG(3, "Delete All: List of Childs: ~s",[ListOfChilds]),
            case Path of
		"/" ->
		    lists:map(fun(A) ->
				      (delete_all(ConnectionPId, Path++(binary_to_list(A))))
			      end, ListOfChilds);
		_Else  -> 
		    lists:map(fun(A) ->
				      (delete_all(ConnectionPId, 
						  Path++"/"++(binary_to_list(A)))) 
                              end, ListOfChilds)

	    end,
            ?LOG(3, "Killing ~s",[Path]),
            delete(ConnectionPId, Path);
	{error, Message} ->
	    {error, Message}
    end.

%% Gets a path and looks if the corresponding node exists. If
%% not it is created (along with the whole path).
macro_ensure_path(ConnectionPId, Path) ->
    FolderList = string:tokens(Path, "/"),
    PrefixPaths = get_prefix_paths(FolderList),
    lists:map(fun(Folder) -> ensure_folder(ConnectionPId, Folder) end, PrefixPaths),
    ls(ConnectionPId, Path).

%% ----------- intern functions------------------------------------

%% Determines the path to every Node on the way to a special node (all parents).
get_prefix_paths([]) ->
    [];
get_prefix_paths([ Head | Tail]) ->
    PrefixTails = get_prefix_paths(Tail),
    HeadedPrefixTails = lists:map(fun(PathTail) ->
					   ("/"++ Head++ PathTail) end, PrefixTails),
    ["/" ++ Head | HeadedPrefixTails].

%% Ensures one single node exists. (parent node is expected to exist.	      
ensure_folder(ConnectionPId, PrefixPath) ->
    case ls(ConnectionPId, PrefixPath) of
	{ok, _I} ->
	    ok;
	{error, _I} ->
	    create(ConnectionPId, PrefixPath, "Created by ensure_path macro")
    end.

	   
    
