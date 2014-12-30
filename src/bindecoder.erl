
-module(bindecoder).

-export([byte/1, ushort/1, ulong/1, varint/1]).
-export([sequence/2, nbytes/2, packet_N/2]).
-export([match_sequence/1, match_nbytes/1, match_packet_N/1]).

%
% decoder(Bytes) ->
%       {ok, Value, Rest}
%       {more, Length}
%       {error, Reason}
%

%%%%% ------------------------------------------------------- %%%%%


byte(<<X:8, Rest/binary>>) -> {ok, X, Rest};
byte(_) -> {more, 1}.

ushort(<<X:16/big-unsigned-integer, Rest/binary>>) -> {ok, X, Rest};
ushort(Bytes) when is_binary(Bytes) ->
    {more, 2 - byte_size(Bytes)}.

ulong(<<X:32/big-unsigned-integer, Rest/binary>>) -> {ok, X, Rest};
ulong(Bytes) when is_binary(Bytes) ->
    {more, 4 - byte_size(Bytes)}.


%%%%% ------------------------------------------------------- %%%%%


varint(Bytes) when is_binary(Bytes) ->
    decode_varint(Bytes, []).
    
decode_varint(<<>>, _) -> {more, 1};

decode_varint(<<1:1, I:7, Rest/binary>>, Acc) ->
    decode_varint(Rest, [I | Acc]);
    
decode_varint(<<0:1, I:7, Rest/binary>>, Acc) ->
    Acc1 = [I|Acc],
    Result = 
        lists:foldl(
            fun(X, Acc0) ->
                (Acc0 bsl 7 bor X)
            end, 0, Acc1),
    {ok, Result, Rest}.

    
%%%%% ------------------------------------------------------- %%%%%


nbytes(N, Bytes) when is_integer(N), is_binary(Bytes) ->
    Len = byte_size(Bytes),
    if
        N =:= Len   -> {ok, Bytes, <<>>}
    ;   N > Len     -> {more, N - Len}
    ;   N < Len     ->
            Value = binary_part(Bytes, 0, N),
            Rest = binary_part(Bytes, N, Len - N),
            {ok, Value, Rest}
    end.
    
    
match_nbytes(N) ->
    fun(Bytes) ->
        nbytes(N, Bytes)
    end.

    
%%%%% ------------------------------------------------------- %%%%%


packet_N(LenFun, Bytes) when is_function(LenFun, 1), is_binary(Bytes) ->
    Header = LenFun(Bytes),
    case Header of
        {ok, Value, Rest}   -> nbytes(Value, Rest)
    ;   _                   -> Header
    end.
    
    
match_packet_N(LenFun) ->
    fun(Bytes) ->
        packet_N(LenFun, Bytes)
    end.
    

%%%%% ------------------------------------------------------- %%%%%


sequence(List, Bytes) ->
    {OutBytes, OutValues} =
        lists:foldl(
            fun
                (_, {error,_} = E)      -> E
            ;   (_, {more,_} = M)       -> M
            ;   (X, {InBytes, Values})    ->
                    Result = X(InBytes),
                    case Result of
                        {ok, Value, Rest}   ->
                            {Rest, [Value | Values]}
                    ;   _                   -> Result
                    end
            end,
            {Bytes, []},
            List),
            
    case OutValues of
        {error,_}   -> OutValues
    ;   {more,_}    -> OutValues
    ;   _           -> {ok, lists:reverse(OutValues), OutBytes}
    end.


match_sequence(List) ->
    fun(Bytes) ->
        sequence(List, Bytes)
    end.
    

%%%%% ------------------------------------------------------- %%%%%
    