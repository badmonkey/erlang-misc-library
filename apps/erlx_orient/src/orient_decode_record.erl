
-module(orient_decode_record).
-vsn("1.0.0").

-export([decode_v0_class/2, decode_v0_header/3, decode_v0_data/4]).


-include_lib("erlx_orient/include/constants.hrl").
-include_lib("erlx_orient/include/record.hrl").


%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_class( binary(), type:properties() ) -> { orient_record:classname(), binary() } | type:exception().

decode_v0_class(Data, Opts) ->
    orient_record_types:decode_string(Data).
   
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_header( binary(), orient_record:classname(), type:properties() ) -> { orient_record:header(), binary() } | type:exception().

decode_v0_header(Data, Class, Opts) ->
    decode_v0_header_item(Data, [], Class, Opts).
    

decode_v0_header_item(<<>>, _, _, _) ->
    error:throw_error(incomplete_header);
    
decode_v0_header_item(Data, Acc, Class, Opts) ->
    case bindecoder:zigzag(Data) of
        {ok, 0, Rest}               -> { lists:reverse(Acc), Rest }
        
    ;   {ok, I, Rest} when I > 0    ->
            {Field, Rest2} = decode_v0_named_field(I, Rest),
            decode_v0_header_item(Rest2, [Field | Acc], Class, Opts)
            
    ;   {ok, I, Rest} when I < 0    ->
            {Property, Rest2} = decode_v0_property(I, Rest),
            decode_v0_header_item(Rest2, [Property | Acc], Class, Opts)

    ;   _                           -> error:throw_error(bad_header)
    end.


%%%%% ------------------------------------------------------- %%%%%

    
decode_v0_named_field(Size, Data) ->
    {Name, Rest}    = orient_record_types:decode_string(Size, Data),
    {Ptr, Rest2}    = orient_record_types:decode_pointer(Rest2),
    {Type, Rest3}   = orient_record_types:decode_typeid(Rest2),
    { {field, Name, Ptr, Type}, Rest3 }.
    
    
%%%%% ------------------------------------------------------- %%%%%

    
decode_v0_property(RawId, Data) ->
    Id = -RawId - 1,
    {Ptr, Rest} = orient_record_types:decode_pointer(Data),
    { {property, Id, Ptr, undefined}, Rest }.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_v0_data( binary(), orient_record:classname(), orient_record:header(), type:properties() ) -> orient_record:odb_record() | type:exception().

decode_v0_data(Data, Class, Header, Opts) ->
    //type:throw_error(bad_record_header).
    Header.

    
