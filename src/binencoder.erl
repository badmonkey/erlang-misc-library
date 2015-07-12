
-module(binencoder).

-export([buffer/2, varint/1, utf16_string/1, utf16_list/1, utf8/1, json_buffer/1]).



%%%%% ------------------------------------------------------- %%%%%


buffer(1, Bytes) when is_binary(Bytes), byte_size(Bytes) =< 16#ff ->
    X = byte_size(Bytes),
    <<X:8, Bytes/binary>>;
    
buffer(2, Bytes) when is_binary(Bytes), byte_size(Bytes) =< 16#ffff ->
    X = byte_size(Bytes),
    <<X:16/big-unsigned-integer, Bytes/binary>>;
    
buffer(4, Bytes) when is_binary(Bytes), byte_size(Bytes) =< 16#ffffffff ->
    X = byte_size(Bytes),
    <<X:32/big-unsigned-integer, Bytes/binary>>;
    
buffer(varint, Bytes) when is_binary(Bytes) ->
    Header = varint(byte_size(Bytes)),
    <<Header/binary, Bytes/binary>>.
    
    
%%%%% ------------------------------------------------------- %%%%%

    
varint(Value) when is_integer(Value) ->
    encode_varint(Value, []).


encode_varint(I, Acc) when I =< 16#7f ->
    iolist_to_binary(lists:reverse([I | Acc]));
    
encode_varint(I, Acc) ->
    Last_Seven_Bits = (I - ((I bsr 7) bsl 7)),
    First_X_Bits = (I bsr 7),
    With_Leading_Bit = Last_Seven_Bits bor 16#80,
    encode_varint(First_X_Bits, [With_Leading_Bit|Acc]).
    
    
%%%%% ------------------------------------------------------- %%%%%


utf16_string(S) when is_list(S) ->
    unicode:characters_to_binary(S, utf8, utf16).
    
    
utf8(B) when is_binary(B) ->
    unicode:characters_to_list(B, utf16).
    
    
utf16_list(L) when is_list(L) ->
    encode_utf16_list(L, <<>>).
    
    
encode_utf16_list([], Acc) ->
    Acc;
    
encode_utf16_list([H | T], Acc) ->
    V = utf16_string(H),
    encode_utf16_list(T, <<Acc/binary, V/binary, 0:16>>).
    
    
%%%%% ------------------------------------------------------- %%%%%


json_buffer(Term) ->
    buffer(varint, jiffy:encode(Term)).

    