-module(ezk_message_2_packet).
-include_lib("../include/ezk.hrl").
-export([make_packet/2]).
make_packet({create, Path, Data, Typ}, Iteration) ->
    case Typ of
       e -> Mode = 1;
       s -> Mode = 2;
       es -> Mode = 3;
       se -> Mode = 3;
       _Else -> Mode = 0
    end,
    Load = <<(pack_it_l2b(Path))/binary, 
              (pack_it_l2b(Data))/binary, 
              1:32, 
              31:32, 
              (pack_it_l2b("world"))/binary, 
              (pack_it_l2b("anyone"))/binary,
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
	     <<255,255,255,255>>/binary>>,
    Command = 5,
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
%LIttle Helpers
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
