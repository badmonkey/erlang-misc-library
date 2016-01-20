
-module(orient_record).
-vsn("1.0.0").

-export([decode/1, decode/2, encode/1, encode/2]).


-include_lib("erlx_orient/include/record.hrl").


%%%%% ------------------------------------------------------- %%%%%


-type classname() :: binary().

-type fieldtype() :: boolean | integer | short | long | float | double.

-type pointer() :: integer().

-type field() :: { field, binary(), pointer(), fieldtype() }.
-type property() :: { property, integer(), pointer(), fieldtype() | undefined }.

-type header() :: [ field() | property() ].

-type as_map() :: #{ binary() -> term() }.
-type as_record() :: tuple().
-type odb_record() :: as_map() | as_record().


-export_type([classname/0, header/0, field/0, fieldtype/0, property/0, pointer/0, odb_record/0]).


%%%%% ------------------------------------------------------- %%%%%


decode(Data) -> decode(Data, []).
%
% {as_record, {atom(), list( atom() )}}
% {backtrace, boolean()} or backtrace
% {classname_is_field, boolean()} or classname_is_field
% {update_map, #{}}
%
    
-spec decode( binary(), type:properties() ) -> type:okvalue_or_error( odb_record() ).

decode(<<>>, _Opts) -> {error, incomplete_record};
decode(<<?ODB_RECORD_VER:8, Rest/binary>>, Opts) ->
    try
        {Class, Rest2}  = orient_decode_record:decode_v0_class(Rest, Opts),
        {Header, Rest3} = orient_decode_record:decode_v0_header(Rest2, Class, Opts),
        Record          = orient_decode_record:decode_v0_data(Rest3, Class, Header, Opts),
        {ok, Record}
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


