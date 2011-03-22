-module(ezk_packet_2_message).
-export([get_message_typ/1, replymessage_2_reply/3, get_watch_data/1]).
-include_lib("../include/ezk.hrl").

%% TODO Interpreting answer on set_auth
 
get_message_typ(Data) ->
    case Data  of 		
        <<255,255,255,254, Heartbeat/binary>> ->
            {heartbeat, Heartbeat};
				%%%Watchevents
	<<255,255,255,255, 255,255,255,255, 255,255,255,255 , 0,0,0,0, Payload/binary>> ->
	    ?LOG(3, "packet_2_message: A Watchevent arrived"),
	    {watchevent, Payload};
						%%%Other Messages
        <<MessId:32, 0:32, Zxid:32, Payload/binary>> ->
	    ?LOG(3, "packet_2_message: A normal Message arrived"),
            {normal, MessId, Zxid, Payload}
            
    end.

get_watch_data(Binary) ->
     <<TypInt:32, SyncConnected:32, PackedPath/binary>> = Binary,
     {Path, _Nothing} = unpack(PackedPath),
     case TypInt of
        3 ->
           Typ = data;
        4 -> 
           Typ = child
     end,   
     {Typ, binary_to_list(Path), SyncConnected}.

replymessage_2_reply(CommId, Path, PayloadWithErrorCode) ->
    ?LOG(1,"packet_2_message: Trying to Interpret payload: ~w", [PayloadWithErrorCode]),
    case PayloadWithErrorCode of
	<<0,0,0,0,Payload/binary>> -> 
	    ?LOG(1, "packet_2_message: Interpreting the payload with commid ~w and Path ~w"
		 , [CommId, Path]),
            Replydata = interpret_reply_data(CommId, Path, Payload),
	    Reply = {ok, Replydata};
	<<255,255,255,146,_Payload/binary>> ->
	    Reply = {error, "Directory already exists!"};
	<<255,255,255,154,_Payload/binary>> ->
	    Reply = {error, "NoAuth to do this!"};   
	<<255,255,255,155,_Payload/binary>> ->
	    Reply = {error, "Directory not found!"};   
	<<255,255,255,248,_Payload/binary>> ->
	    Reply = {error, "You are not allowed to do this!"};   
	Cody -> 
	    Reply = {unknown, "Wow, you just found an unexpected Error.", Cody}
    end,      
    Reply.

%%% create
interpret_reply_data(1, _Path, Reply) ->
    <<LengthOfData:32, Data/binary>> = Reply,
    {ReplyPath, _Left} = split_binary(Data, LengthOfData),
    binary_to_list(ReplyPath);

%%% delete
interpret_reply_data(2, Path, _Reply) ->
    Path;

%%% get
interpret_reply_data(4, _Path, Reply) -> 
    ?LOG(3,"P2M: Got a get reply"),
    <<LengthOfData:32, Data/binary>> = Reply,
    ?LOG(3,"P2M: Length of data is ~w",[LengthOfData]),
    {ReplyData, Left} = split_binary(Data, LengthOfData),
    ?LOG(3,"P2M: The Parameterdata is ~w",[Left]),    
    Parameter = getbinary_2_list(Left),
    {binary_to_list(ReplyData), Parameter};

%%% set
interpret_reply_data(5, _Path, Reply) -> 
    getbinary_2_list(Reply);	       

%%% get_acl
interpret_reply_data(6, _Path, Reply) ->
    ?LOG(3,"P2M: Got a get acl reply"),
    <<NumberOfAcls:32, Data/binary>> = Reply,
    ?LOG(3,"P2M: There are ~w acls",[NumberOfAcls]),
    {Acls, Data2} = get_n_acls(NumberOfAcls, [],  Data),
    ?LOG(3,"P2M: Acls got parsed: ~w", [Acls]),
    Parameter = getbinary_2_list(Data2),
    ?LOG(3,"P2M: Data got also parsed."),
    {Acls, Parameter};

%%% set_acl
interpret_reply_data(7, _Path, Reply) ->
    getbinary_2_list(Reply);


%%% ls
%%% Returns Children as a List of binarys
interpret_reply_data(8, _Path, Reply) ->
    ?LOG(4,"packet_2_message: Interpreting a ls"),
    <<NumberOfAnswers:32, Data/binary>> = Reply,
    ?LOG(4,"packet_2_message: Number of Children: ~w",[NumberOfAnswers]),
    ?LOG(4,"packet_2_message: The Binary is: ~w",[Data]),
    {List, _Left} =  get_n_paths(NumberOfAnswers, Data),
    ?LOG(4,"packet_2_message: Paths extracted."),
    ?LOG(4,"packet_2_message: Paths are: ~w",[List]),    
    lists:map(fun(A) -> list_to_binary(A) end, List);

%%% ls2
interpret_reply_data(12, _Path, Reply) ->
    {<<NumberOfAnswers:32>>, Data} = split_binary(Reply, 4),
    {Children, Left} =  get_n_paths(NumberOfAnswers, Data),
    Parameter = getbinary_2_list(Left),
    [{children, Children}|Parameter].

%%----------------------------------------------------------------
%% Little Helpers
%%----------------------------------------------------------------

get_n_paths(0, Binary) ->    
    {[],Binary};
get_n_paths(N, Binary) ->
    {ThisPathBin, ToProcessBin} = unpack(Binary),
    {RekResult, Left2} = get_n_paths(N-1, ToProcessBin),
    {[binary_to_list(ThisPathBin) | RekResult ], Left2}.


getbinary_2_list(Binary) ->
    ?LOG(3,"p2m: Trying to match Parameterdata"),
    <<0:32,              Czxid:32,        0:32,         Mzxid:32,
      Ctime:64,                           Mtime:64,
      DaVer:32,          CVer:32,         AclVer:32,    0:64,  
                         DaLe:32,         NumChi:32,    0:32,
      Pzxid:32>> = Binary,
    ?LOG(3,"p2m: Matching Parameterdata Successfull"),
    [{czxid, Czxid}, {mzxid, Mzxid},
            {ctime, Ctime}, {mtime, Mtime},
            {dataversion, DaVer}, {datalength, DaLe},
            {numberChildren,NumChi}, {pzxid, Pzxid},
            {cversion, CVer}, {aclversion, AclVer}].
    
unpack(Binary) ->
    <<Length:32, Load/binary>> = Binary,
    split_binary(Load, Length).

get_n_acls(0, Acls, Binary) ->
    ?LOG(3,"P2M: Last Acl parsed."),
    {Acls, Binary};
get_n_acls(N, Acls,  Binary) ->
    ?LOG(3,"P2M: Parse next acl from this: ~w.",[Binary]),
    <<0:27, R:1, W:1, C:1, D:1, A:1, Left/binary>>  = Binary,
    {Scheme, Left2}         = unpack(Left),
    ?LOG(3,"P2M: Scheme is: ~w.",[Scheme]),
    {Id,     NowLeft}       = unpack(Left2),
    ?LOG(3,"P2M: Id is: ~w.",[Id]),
    ?LOG(3,"P2M: The Permissiontupel is: ~w.",[{R,W,C,D,A}]),
    Permi = get_perm_from_tupel({R,W,C,D,A}),
    ?LOG(3,"P2M: Permissions are: ~w.",[Permi]),
    NewAcls = [{Permi, Scheme, Id} | Acls ],    
    get_n_acls(N-1, NewAcls, NowLeft).

get_perm_from_tupel({1,W,C,D,A}) ->
    [r | get_perm_from_tupel({0,W,C,D,A})];
get_perm_from_tupel({0,1,C,D,A}) ->
    [w | get_perm_from_tupel({0,0,C,D,A})];
get_perm_from_tupel({0,0,1,D,A}) ->
    [c | get_perm_from_tupel({0,0,0,D,A})];
get_perm_from_tupel({0,0,0,1,A}) ->
    [d | get_perm_from_tupel({0,0,0,0,A})];
get_perm_from_tupel({0,0,0,0,1}) ->
    [a | get_perm_from_tupel({0,0,0,0,0})];
get_perm_from_tupel({0,0,0,0,0}) ->
    [].
    
