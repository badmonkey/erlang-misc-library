
-module(bindecoder).

-export([byte/1, ushort/1, ulong/1, varint/1, zigzag/1]).
-export([arrayof/3, sequence/2, nbytes/2, packet_N/2]).
-export([match_sequence/1, match_nbytes/1, match_packet_N/1, match_utf16_string/0]).


-type result(V) :: {ok, Value :: V, Remaining :: binary()}
                 | {more, Needed :: pos_integer()}
                 | type:error().

-type decoderf() :: fun( ( binary() ) -> result(term()) ).
-type validatorf() :: fun( ( binary() ) -> result(binary()) ).

                
-export_type([result/1, decoderf/0, validatorf/0]).


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


zigzag(Bytes) when is_binary(Bytes) ->
    case decode_varint(Bytes, []) of
        {ok, Value, Rest}   -> {ok, dezigint(Value), Rest}
    ;   X                   -> X
    end.


-spec dezigint( non_neg_integer() ) -> integer().

dezigint(I) when I >= 0, I rem 2 == 0 -> I div 2;
dezigint(I) when I >= 0, I rem 2 == 1 -> - (I div 2) - 1.


%%%%% ------------------------------------------------------- %%%%%


nbytes(N, Bytes)
        when  is_integer(N)
            , is_binary(Bytes)  ->
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


arrayof(N, ElemFun, Bytes)
        when  is_integer(N)
            , is_function(ElemFun, 1)
            , is_binary(Bytes)  ->
    decode_arrayof(N, ElemFun, [], Bytes).
    
    
decode_arrayof(0, _, Acc, Rest) ->
    {ok, lists:reverse(Acc), Rest};
    
decode_arrayof(N, ElemFun, Acc, Data) ->
    case ElemFun(Data) of
        {ok, Value, Rest}   -> decode_arrayof(N - 1, ElemFun, [Value | Acc], Rest)
    ;   _ = Else            -> Else
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


packet_N(LenFun, Bytes)
        when  is_function(LenFun, 1)
            , is_binary(Bytes)  ->
    Header = LenFun(Bytes),
    case Header of
        {ok, Value, Rest}   -> nbytes(Value, Rest)
    ;   _                   -> Header
    end.
    
    
match_packet_N(LenFun) ->
    fun(Bytes) ->
        packet_N(LenFun, Bytes)
    end.
    
    
match_utf16_string() ->
    fun(Bytes) ->
        Header = ushort(Bytes),
        case Header of
            {ok, Value, Rest}   -> nbytes(Value * 2, Rest)
        ;   _                   -> Header
        end
    end.
    

%%%%% ------------------------------------------------------- %%%%%


sequence(List, Bytes)
        when  is_list(List)
            , is_binary(Bytes)  ->
    {OutBytes, OutValues} =
        lists:foldl(
            fun
                (_, {error,_} = E)      -> E
            ;   (_, {more,_} = M)       -> M
            ;   (F, {InBytes, Values})    ->
                    Result = F(InBytes),
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
   

%-type field() :: {atom(), fun(( binary() ) -> result() ) }.
%-type scheme() :: [ field() ].

%-spec decode_to_map( decode_scheme(), binary() ) -> map().
%decode_to_map(Scheme, Bytes) ->
%   ok.
 
