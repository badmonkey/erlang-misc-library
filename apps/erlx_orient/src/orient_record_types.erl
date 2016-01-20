

-module(orient_record_types).

-export([ decode_boolean/1, decode_integer/1, decode_short/1, decode_long/1
        , decode_string/1, decode_string/2
        , decode_varint/1, decode_pointer/1, decode_typeid/1 ]).



%%%%% ------------------------------------------------------- %%%%%


%boolean    0       byte
%integer    1       varint
%short      2       varint
%long       3       varint
%float      4       float32
%double     5       double64
%datetime   6       long
%string     7       varint:bytes[]
%binary     8       varint:bytes[]
%embedded   9       record
%embedlist  10      varint:byte:items[]     item = byte:bytes[]
%embedset   11      varint:byte:items[]
%embedmap   12      varint:header-struct:bytes[]
%link       13      int64:int64
%linklist   14      varint:links[]
%linkset    15      varint:links[]
%linkmap    16      varint:key-value-pairs[]
%byte       17      byte
%transient  18
%date       19
%custom     20
%decimal    21      int32:int32:bytes[]
%linkbag    22      ?
%any        23
%varint             varint
%pointer            int32
%typeid             byte


%%%%% ------------------------------------------------------- %%%%%


decode_boolean(<<0:8, Rest/binary>>) -> {false, Rest}.
decode_boolean(<<1:8, Rest/binary>>) -> {true, Rest}.
decode_boolean(_) -> error:throw_error(invalid_boolean).


%%%%% ------------------------------------------------------- %%%%%


decode_integer(Data) -> decode_varint(Data).
decode_short(Data) -> decode_varint(Data).
decode_long(Data) -> decode_varint(Data).


%%%%% ------------------------------------------------------- %%%%%


decode_string(<<>>) -> error:throw_error(invalid_string);
decode_string(Data) ->
    case bindecoder:zigzag(Data) of
        {ok, Size, Rest}    -> decode_string(Size, Rest)
    ;   _                   -> error:throw_error(invalid_string)
    end.
    
    
decode_string(0, Data) -> { <<>>, Data };
decode_string(Size, Data) ->
    case bindecoder:nbytes(Size, Data) of
        {ok, String, Rest}  -> { String, Rest }
    ;   _                   -> error:throw_error(invalid_string)
    end


%%%%% ------------------------------------------------------- %%%%%
    

decode_varint(<<>>) -> error:throw_error(invalid_varint);       
decode_varint(Data) ->    
    case bindecoder:zigzag(Data) of
        {ok, Ptr, Rest} -> { Ptr, Rest }
    ;   _               -> error:throw_error(invalid_varint)
    end.    
    
    
%%%%% ------------------------------------------------------- %%%%%

    
decode_pointer(Data) -> decode_integer(Data).
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_typeid( binary() ) -> {orient_record:fieldtype(), binary()} | type:exception().
    
decode_typeid(<<>>)                 -> error:throw_error(invalid_typeid);

decode_typeid(<<0:8, Rest/binary>>) -> {boolean, Rest};
decode_typeid(<<1:8, Rest/binary>>) -> {integer, Rest};
decode_typeid(<<2:8, Rest/binary>>) -> {short, Rest};
decode_typeid(<<3:8, Rest/binary>>) -> {long, Rest};
decode_typeid(<<4:8, Rest/binary>>) -> {float, Rest};
decode_typeid(<<5:8, Rest/binary>>) -> {double, Rest};

decode_typeid(_)                    -> error:throw_error(unknown_type).
