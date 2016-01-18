
-module(orient_decode_record).
-vsn("1.0.0").

-export([decode_v0_class/2, decode_v0_header/3, decode_v0_data/4]).


%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_class( binary(), type:properties() ) -> { orient_record:classname(), binary() } | type:exception().

decode_v0_class(Data, Opts) ->
    decode_type_string(Data, invalid_class_name).
   
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_header( binary(), orient_record:classname(), type:properties() ) -> { orient_record:header(), binary() } | type:exception().

decode_v0_header(Data, Class, Opts) ->
    decode_v0_header_item(Data, [], Class, Opts).
    

decode_v0_header_item(<<>>, _, _, _) ->
    type:throw_error(incomplete_header);
    
decode_v0_header_item(Data, Acc, Class, Opts) ->
    case bindecoder:zigzag(Data) of
        {ok, 0, Rest}               -> { lists:reverse(Acc), Rest }
        
    ;   {ok, I, Rest} when I > 0    ->
            {Field, Rest2} = decode_v0_named_field(I, Rest),
            decode_v0_header_item(Rest2, [Field | Acc], Class, Opts)
            
    ;   {ok, I, Rest} when I < 0    ->
            {Property, Rest2} = decode_v0_property(I, Rest),
            decode_v0_header_item(Rest2, [Property | Acc], Class, Opts)

    ;   _                           -> type:throw_error(bad_header)
    end.


%%%%% ------------------------------------------------------- %%%%%

    
decode_v0_named_field(Size, Data) ->
    {Name, Rest}    = decode_type_string(Size, Data, invalid_named_field),
    {Ptr, Rest2}    = decode_type_pointer(Rest2, invalid_named_field),
    {Type, Rest3}   = decode_type_specifier(Rest2, invalid_named_field),
    { {field, Name, Ptr, Type}, Rest3 }.
    
    
%%%%% ------------------------------------------------------- %%%%%

    
decode_v0_property(RawId, Data) ->
    Id = -RawId - 1,
    {Ptr, Rest} = decode_type_pointer(Data, invalid_property),
    { {property, Id, Ptr}, Rest }.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_data( binary(), orient_record:classname(), orient_record:header(), type:properties() ) -> orient_record:record() | type:exception().

decode_v0_data(Data, Class, Header, Opts) ->
    //type:throw_error(bad_record_header).
    Header.


%%%%% ------------------------------------------------------- %%%%%
    
    
decode_type_string(<<>>, Error) -> type:throw_error(Error);
decode_type_string(Data, Error) ->
    case bindecoder:varint(Data) of
        {ok, Size, Rest}    -> decode_type_string(Size, Rest, Error)
    ;   _                   -> type:throw_error(Error)
    end.
    
    
decode_type_string(0, Data, _) -> { <<>>, Data };
decode_type_string(Size, Data, Error) ->
    case bindecoder:nbytes(Size, Data) of
        {ok, String, Rest}  -> { String, Rest }
    ;   _                   -> type:throw_error(Error)
    end
    
    
decode_type_pointer(<<>>, Error) -> type:throw_error(Error);
decode_type_pointer(Data, Error) ->
    case bindecoder:zigzag(Data) of
        {ok, Ptr, Rest} -> { Ptr, Rest }
    ;   _               -> type:throw_error(Error)    
    end.
    
    
decode_type_specifier(<<>>, Error) -> type:throw_error(Error);
decode_type_specifier(<<Type:8, Rest/binary>>, _Error) -> {Type, Rest}.
