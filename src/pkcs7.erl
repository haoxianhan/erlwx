-module (pkcs7).

-include_lib("eunit/include/eunit.hrl").

-export ([
          pad/1,
          unpad/1,
          test/0
         ]).

pad(<<>>) ->
    <<>>;
pad(Bin) ->
    Diff = byte_size(Bin) rem 16,
    pad(Bin, 16-Diff).

pad(Bin, PadAmount) ->
    list_to_binary([Bin | lists:duplicate(PadAmount, <<PadAmount>>)]).

unpad(<<>>) ->
    <<>>;
unpad(Bin) ->
    Last = binary:last(Bin),
    Size = byte_size(Bin) - Last,
    RemSize = Size rem 16,
    PadAmount = 16 - RemSize,
    PadBinary = list_to_binary(lists:duplicate(PadAmount, <<PadAmount>>)),
    case Bin of
        <<Data:Size/binary, PadBinaryX/binary>> when PadBinaryX == PadBinary ->
            Data;
        _ ->
            erlang:error(bad_padding)
    end.


test() ->
    ?assertEqual(pad(<<1, 1, 1, 1>>), <<1, 1, 1, 1, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>>),
    ?assertEqual(pad(<<1, 1, 1, 1, 1, 1, 1, 1>>), <<1, 1, 1, 1, 1, 1, 1, 1, 8, 8, 8, 8, 8, 8, 8, 8>>),
    ?assertEqual(pad(<<1, 1, 1, 1>>), <<1, 1, 1, 1, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>>),
    ?assertEqual(unpad(<<1, 1, 1, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13>>), <<1, 1, 1>>),
    ?assertEqual(unpad(<<1, 1, 1, 1, 1, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11>>), <<1, 1, 1, 1, 1>>).
