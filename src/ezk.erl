-module(ezk).

%functions creating and deleting zkNodes
-export([create/2, create/3, create/4, delete/1]).
%functions dealing with node informations
-export([set/2, get/1, ls/1, ls2/1, set_acl/2, get_acl/1]).
%functions dealing with watches
-export([ls/3, get/3, ls2/3]).
%macros
-export([delete_all/1]).
%infos
-export([info_get_iterations/0, help/0]).
%Stop commands (forcing Client to choose a new random Server from List)
-export([die/0, die/1]).



help() ->
    io:format("-----------------------------------------------------~n"),
    io:format("| The Commands this Client knows about:             |~n"),
    io:format("|---------------------------------------------------|~n"),
    io:format("| ezk:create/2     : Path, Data                     |~n"),
    io:format("| ezk:create/3     : Path, Data, Typ                |~n"),
    io:format("| ezk:create/4     : Path, Data, Typ, [Acl]         |~n"),
    io:format("| ezk:delete/1     : Path                           |~n"),
    io:format("| ezk:delete_all/1 : Path                           |~n"),
    io:format("| ezk:get/1        : Path                           |~n"),
    io:format("| ezk:get/3        : Path, WatchOwner, Watchmessage |~n"),
    io:format("| ezk:get_acl/1    : Path                           |~n"),
    io:format("| ezk:set/2        : Path, Data                     |~n"),
    io:format("| ezk:set_acl/2    : Path, [Acl]                    |~n"),
    io:format("| ezk:ls/1         : Path                           |~n"),
    io:format("| ezk:ls/3         : Path, WatchOwner, Watchmessage |~n"),
    io:format("| ezk:ls2/1        : Path                           |~n"),
    io:format("| ezk:ls2/3        : Path, WatchOwner, Watchmessage |~n"),
    io:format("| ezk:die/0                                         |~n"),
    io:format("| ezk:die/1        : Reason                         |~n"),
    io:format("| ezk:info_get_iterations/0                         |~n"),
    io:format("|---------------------------------------------------|~n"),
    io:format("| In Progress:                                      |~n"),
    io:format("| ezk:auth/2       : Scheme, Id                     |~n"),
    io:format("|---------------------------------------------------|~n"),
    io:format("| Datatypes:                                        |~n"),
    io:format("| Acl = {Scheme,Id, [Permission]}                   |~n"),
    io:format("| Path = Scheme = Id = Reason = String              |~n"),  
    io:format("| Permission = r | w | c | d | a                    |~n"),
    io:format("| WatchOwner = PId           WatchMessage = String  |~n"),
    io:format("| Data = All Things          Typ = e | s | es       |~n"),
    io:format("|---------------------------------------------------|~n").


%%--------------------------- Zookeeper Functions ---------------------
%% Return {ok, Reply}.
%% All functions are blocking.

%% Creates a new ZK_Node
%% Reply = Path where Path = String
create(Path, Data) ->
     ezk_connection:create(Path, Data).

%% Typ = e | s | es (stands for etheremal, sequenzed or both)
create(Path, Data, Typ) ->
    ezk_connection:create(Path, Data, Typ).

%% Acls = [Acl] where Acl = {Scheme, Id, Permission} 
%% with Scheme and Id = String
%% and Permission = [Per] | String 
%% where Per = r | w | c | d | a
create(Path, Data, Typ, Acls)  ->
   ezk_connection:create(Path, Data, Typ, Acls).

%% Deletes a ZK_Node
%% Only working if Node has no children.
%% Reply = Path where Path = String
delete(Path) ->
    ezk_connection:delete(Path).

%% Deletes a ZK_Node and all his childs.
%% Reply = Path where Path = String
delete_all(Path) ->
   ezk_connection:delete_all(Path).    

%% Reply = {Data, Parameters} where Data = The Data stored in the Node
%% and Parameters = [{ParameterName, Value}]
%% where ParameterName = czxid | mzxid | pzxid | ctime | mtime | dataversion | 
%%                       datalength | number_children | cversion | aclversion
get(Path) ->
    ezk_connection:get(Path).
   
%% Like the one above but sets a datawatch to Path.
%% If watch is triggered a Message M is send to the PId WatchOwner
%% M = {WatchMessage, {Path, Type, SyncCon}
%% with Type = child
get(Path, WatchOwner, WatchMessage) ->
    ezk_connection:get(Path, WatchOwner, WatchMessage).

%% Returns the actual Acls of a Node
%% Reply = {[ACL],Parameters} with ACl and Parameters like above
get_acl(Path) ->
    ezk_connection:get_acl(Path).

%% Sets new Data in a Node. Old ones are lost.
%% Reply = Parameters with Data like at get
set(Path, Data) ->
   ezk_connection:set(Path, Data).

%% Sets new Acls in a Node. Old ones are lost.
%% ACL like above.
%% Reply = Parameters with Data like at get
set_acl(Path, Acls) ->
    ezk_connection:set_acl(Path, Acls).
%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = [ChildName] where ChildName = <<"Name">>
ls(Path) ->
   ezk_connection:ls(Path).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls(Path, WatchOwner, WatchMessage) ->
    ezk_connection:ls(Path, WatchOwner, WatchMessage).

%% Lists all Children of a Node. Paths are given as Binarys!
%% Reply = {[ChildName],Parameters} with Parameters and ChildName like above.
ls2(Path) ->
   ezk_connection:ls2(Path).
%% like above, but a Childwatch is set to the Node. 
%% Same Reaktion like at get with watch but Type = child
ls2(Path, WatchOwner, WatchMessage) ->
    ezk_connection:ls2(Path, WatchOwner, WatchMessage).

%% Returns the Actual Transaction Id of the Client.
%% Reply = Iteration = Int.
info_get_iterations() ->
    ezk_connection:info_get_iterations().

die() ->
    ezk_connection:die("No offence").

die(Reason) ->
    ezk_connection:die(Reason).
    
