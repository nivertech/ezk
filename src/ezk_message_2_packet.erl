-module(ezk_message_2_packet).
-include_lib("../include/ezk.hrl").
-export([make_packet/2]).
-export([get_permi_int/2]).
make_packet({create, Path, Data, Typ, Acls}, Iteration) ->
    case Typ of
       e -> Mode = 1;
       s -> Mode = 2;
       es -> Mode = 3;
       se -> Mode = 3;
       _Else -> Mode = 0
    end,
    AclBin = acls_2_bin(Acls, <<>>, 0),
    Load = <<(pack_it_l2b(Path))/binary, 
              (pack_it_l2b(Data))/binary, 
              AclBin/binary,
              Mode:32>>,
    Command = 1,
    wrap_packet({Command, Path, Load}, Iteration);

make_packet({delete, Path, _Typ}, Iteration ) ->
    Load = <<(pack_it_l2b(Path))/binary, 255, 255, 255, 255 >>,
    Command = 2,
    wrap_packet({Command, Path, Load}, Iteration);

make_packet({get, Path}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary, 0>>,
    Command = 4,
    wrap_packet({Command, Path, Load}, Iteration );

make_packet({getw, Path}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary, 1>>,
    Command = 4,
    wrap_packet({Command, Path, Load}, Iteration );

make_packet({set, Path, Data}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary, 
	     (pack_it_l2b(Data))/binary,
	     255, 255, 255, 255>>,
    Command = 5,
    wrap_packet({Command, Path, Load}, Iteration );

make_packet({get_acl, Path}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary>>,
    Command = 6,
    wrap_packet({Command, Path, Load}, Iteration );    

make_packet({set_acl, Path, Acls}, Iteration) ->
    ?LOG(3,"m2p: trying to set an acl, starting to build package"),
    AclBin = acls_2_bin(Acls,<<>>,0),
    ?LOG(3,"m2p: trying to set an acl, AclBin constructed"),
    Load = <<(pack_it_l2b(Path))/binary,
	      AclBin/binary,
	      255, 255, 255, 255>>,
    Command = 7, 
    ?LOG(3,"m2p: trying to set an acl, Load constructed"),
    wrap_packet({Command, Path, Load}, Iteration );
    
make_packet({ls, Path}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary, 0:8>>,
    Command = 8, 
    wrap_packet({Command, Path, Load}, Iteration );
    
make_packet({lsw, Path}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary, 1:8>>,
    Command = 8, 
    wrap_packet({Command, Path, Load}, Iteration );
       
make_packet({ls2, Path}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary, 0:8>>,
    Command = 12, 
    wrap_packet({Command, Path, Load}, Iteration );
       
make_packet({ls2w, Path}, Iteration) ->
    Load = <<(pack_it_l2b(Path))/binary, 1:8>>,
    Command = 12, 
    wrap_packet({Command, Path, Load}, Iteration ).


%--------------------------------------------------------------------
%Little Helpers
%--------------------------------------------------------------------
pack_it_l2b(List) ->
    Length = length(List),
    <<Length:32,(list_to_binary(List))/binary>>.


wrap_packet({Command, Path, Load}, Iteration) ->
    ?LOG(3, "message_2_packet: Try send a request {command, Path, Load}: ~w",
	 [{Command, Path, Load}]),
    Packet = <<Iteration:32, Command:32, Load/binary>>,
    ?LOG(3, "message_2_packet: Request send"),
    {ok, Command, Path, Packet}.

acls_2_bin([], AclBin, Int) ->
    ?LOG(3,"m2p: Acl finished"),
    <<Int:32, AclBin/binary>>;

acls_2_bin([undef], _AclBin, Int) ->
    ?LOG(3,"m2p: undef Acl build"),
    NewAclBin = <<31:32,
	       (pack_it_l2b("world"))/binary, 
	       (pack_it_l2b("anyone"))/binary>>,
    acls_2_bin([], NewAclBin, Int+1);

acls_2_bin([{Scheme, Id, Permi}| Left], AclBin, Int) ->
    ?LOG(3,"m2p: one more element for acl build"),
    ?LOG(3,"m2p: ACL : ~w",[{Scheme, Id, Permi}]),
    PermiInt = get_permi_int(Permi,0),
    ?LOG(3,"m2p: PermiInt : ~w",[PermiInt]),
    SchemeBin = pack_it_l2b(Scheme),
    ?LOG(3,"m2p: SchemeBin : ~w",[SchemeBin]),
    IdBin     = pack_it_l2b(Id),
    ?LOG(3,"m2p: IdBin : ~w",[IdBin]),
    NewAclBin = <<PermiInt:32,
		  SchemeBin/binary,
		  IdBin/binary,
		  AclBin/binary>>,
    acls_2_bin(Left, NewAclBin, Int+1).
    

get_permi_int([], PermiInt) ->
    ?LOG(3,"All rights processed"),
    PermiInt;
get_permi_int([H | T], PermiInt) ->
    ?LOG(3,"Right ~w in process",[H]),
    case H of 
	r ->   CommandBit = 1;
	w ->   CommandBit = 2;
	c ->   CommandBit = 4;
	d ->   CommandBit = 8;
	a ->   CommandBit = 16;
        114 -> CommandBit = 1;
	119 -> CommandBit = 2;
	99  -> CommandBit = 4;
	100 -> CommandBit = 8;
	97  -> CommandBit = 16
    end,
    get_permi_int(T, (PermiInt bor CommandBit)). 
