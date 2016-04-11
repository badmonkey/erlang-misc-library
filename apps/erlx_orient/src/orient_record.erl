
-module(orient_record).
-vsn("1.0.0").

-export([decode/1, decode/2, encode/1, encode/2]).


-include_lib("erlx_orient/include/record.hrl").


%%%%% ------------------------------------------------------- %%%%%


-type classname() :: binary().

-type fieldtype() :: boolean | integer | short | long | float | double
                   | datetime | string | binary | byte | date | decimal
                   | link | linklist | linkset | linkmap | linkbag.

-type pointer() :: integer().

-type field() :: { field, Name :: binary(), pointer(), Type :: fieldtype() }.
-type property() :: { property, Id :: integer(), pointer(), Type :: fieldtype() | undefined }.

-type header() :: [ field() | property() ].

-type as_map() :: #{ binary() => term() }.
-type as_record() :: tuple().
-type odb_record() :: as_map() | as_record().

-type rid() :: { rid, integer(), integer() }.

-type property_schema() :: #{ integer() => { Name :: binary(), Type :: fieldtype() } }.


-export_type([ classname/0, header/0, field/0, fieldtype/0, property/0, pointer/0
             , odb_record/0, as_map/0, rid/0, property_schema/0]).


%%%%% ------------------------------------------------------- %%%%%


decode(Data) -> decode(Data, []).
%
% {schema, property_schema()}
% {update_map, as_map() }}
% {classname_is_field, boolean()} or classname_is_field
% {convert_keys_to_atoms, boolean()} or convert_keys_to_atoms
% {as_record, {atom(), list( atom() )}}
% {backtrace, boolean()} or backtrace
%
    
-spec decode( binary(), type:properties() ) -> type:okvalue_or_error( odb_record() ).

decode(<<>>, _Opts)                                 -> {error, incomplete_record};
decode(<<?ODB_RECORD_VER:8, Rest/binary>>, Opts)    -> decode_document(Rest, Opts).


decode_document(Data, Opts) ->
    try
        {Class, Rest}   = orient_decode_record:decode_v0_class(Data),
        {Header, Rest2} = orient_decode_record:decode_v0_header(Rest, Class),
        Resolved        = resolveHeader(Header, Opts),
        InitialMap      = makeInitialMap(Class, Opts),
        RecordMap       = orient_decode_record:decode_v0_data(Rest2, Class, Resolved, InitialMap),
        RecordData      = transform_record(RecordMap, Opts),
        {ok, RecordData}
    catch
        throw:{error, Reason} -> {error, Reason}
        %depending on Opts gather stacktrace info
    end.


%%%%% ------------------------------------------------------- %%%%%


encode(Record) -> encode(Record, []).


-spec encode( odb_record(), type:properties() ) -> type:okvalue_or_error( binary() ).

encode(Record, Opts) ->
    try
        {ok, Record}
    catch
        throw:{error, Reason} -> {error, Reason}
    end.


%%%%% ------------------------------------------------------- %%%%%


% {schema, property_schema()}

-spec resolveHeader( header(), type:properties() ) -> header() | type:exception().

resolveHeader(Header, Opts) ->
    Schema = property:get_value(schema, Opts, undefined),
    [ resolveProperty(X, Schema) || X <- Header ].
    
    
resolveProperty({ field, _, _, _ } = F, _) ->
    F;

resolveProperty({ property, _, _, _ }, undefined) ->
    error:throw_error(missing_schema);
    
resolveProperty({ property, Id, Ptr, undefined }, Schema) ->
    case maps:get(Id, Schema, undefined) of
        undefined       -> error:throw_error(unresolved_property)
    ;   {Name, Type}    -> {field, Name, Ptr, Type}
    ;   _               -> error:throw_error(invalid_schema)
    end.
    

%%%%% ------------------------------------------------------- %%%%%


% {update_map, as_map()}
% {classname_is_field, boolean()} or classname_is_field

-spec makeInitialMap( classname(), type:properties() ) -> as_map().

makeInitialMap(Classname, Opts) ->
    Map1 =  case property:get_value(update_map, Opts, undefined) of
                undefined           -> #{}
            ;   M when is_map(M)    -> M
            ;   _                   -> error:throw_error(invalid_value_for_update_map)
            end,
    case property:get_bool(classname_is_field, Opts) of
        true    -> maps:put(<<"class-name">>, Classname, Map1)
    ;   false   -> Map1
    end.
    

%%%%% ------------------------------------------------------- %%%%%

% {convert_keys_to_atoms, boolean()} or convert_keys_to_atoms
% {as_record, {atom(), list( atom() )}}
    
-spec transform_record( as_map(), type:properties() ) -> odb_record().

transform_record(RecMap, Opts) ->
    case { property:get_value(as_record, Opts, undefined)
         , property:get_bool(convert_keys_to_atoms, Opts) } of
        {undefined, false}      -> RecMap
        
    ;   {undefined, true}       ->
            xmaps:mutate(
                fun(K, V) ->
                    {value, binary_to_existing_atom(K, latin1), V}
                end, RecMap)

    ;   {{Name, Fields}, _}
                when is_atom(Name), is_list(Fields)  ->
            RecFields = [ get_value_or_throw(X, RecMap) || X <- Fields ],
            erlang:list_to_tuple([Name | RecFields])
    end.

    
get_value_or_throw(Name, RecMap)
        when is_atom(Name), is_map(RecMap) ->
    maps:get_value( erlang:atom_to_binary(Name, latin1), RecMap, undefined );
    
get_value_or_throw(_, _) ->
    error:throw_error(invalid_field_name).
 
