
-module(orient_decode_record).
-vsn("1.0.0").

-export([decode_v0_class/1, decode_v0_header/2, decode_v0_data/4]).


-include_lib("erlx_orient/include/constants.hrl").
-include_lib("erlx_orient/include/record.hrl").


%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_class( binary() ) -> { orient_record:classname(), binary() } | type:exception().

decode_v0_class(Data) ->
    orient_record_types:decode_string(Data).
   
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_header( binary(), orient_record:classname() ) -> { orient_record:header(), binary() } | type:exception().

decode_v0_header(Data, Class) ->
    decode_v0_header_item(Data, [], Class).
    

decode_v0_header_item(<<>>, _, _) ->
    error:throw_error(incomplete_header);
    
decode_v0_header_item(Data, Acc, Class) ->
    case bindecoder:zigzag(Data) of
        {ok, 0, Rest}               -> { lists:reverse(Acc), Rest }
        
    ;   {ok, I, Rest} when I > 0    ->
            {Field, Rest2} = decode_v0_named_field(I, Rest),
            decode_v0_header_item(Rest2, [Field | Acc], Class)
            
    ;   {ok, I, Rest} when I < 0    ->
            {Property, Rest2} = decode_v0_property(I, Rest),
            decode_v0_header_item(Rest2, [Property | Acc], Class)

    ;   _                           -> error:throw_error(bad_header)
    end.


%%%%% ------------------------------------------------------- %%%%%

    
decode_v0_named_field(Size, Data) ->
    {Name, Rest}    = orient_record_types:decode_string(Size, Data),
    {Ptr, Rest2}    = orient_record_types:decode_pointer(Rest),
    {Type, Rest3}   = orient_record_types:decode_typeid(Rest2),
    { {field, Name, Ptr, Type}, Rest3 }.
    
    
%%%%% ------------------------------------------------------- %%%%%

    
decode_v0_property(RawId, Data) ->
    Id = -RawId - 1,
    {Ptr, Rest} = orient_record_types:decode_pointer(Data),
    { {property, Id, Ptr, undefined}, Rest }.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_data( binary(), orient_record:classname(), orient_record:header(), orient_record:as_map() ) -> orient_record:as_map() | type:exception().

decode_v0_data(Data, Class, [], Map) ->
    Map;

decode_v0_data(Data, Class, [{field, Name, Ptr, Type} | Rest], Map) ->
    decode_v0_data(Data, Class, Rest, Map);

decode_v0_data(_Data, _Class, [{property, _Id, _Ptr, undefined} | _Rest], _Map) ->    
    error:throw_error(property_header_not_supported);
    
decode_v0_data(_Data, _Class, [_X | _Rest], _Map) ->
    error:throw_error(invalid_header_type).
