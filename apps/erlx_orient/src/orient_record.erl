
-module(orient_record).
-vsn("1.0.0").

-export([decode/1, decode/2]).


%%%%% ------------------------------------------------------- %%%%%


-type classname() :: binary().

-type pointer() :: integer().
-type field() :: { field, binary(), pointer(), integer() }.
-type property() :: { property, integer(), pointer() }.

-type header() :: [ field() | property() ].

-type record() :: term().


-export_type([classname/0, header/0, field/0, property/0, pointer/0, record/0]).


%%%%% ------------------------------------------------------- %%%%%


decode(Data) -> decode(Data, []).

    
-spec decode( binary(), type:properties() ) -> type:okvalue_or_error( record() ).

decode(<<>>, _Opts) -> {error, incomplete_record};
decode(<<0:8, Rest/binary>>, Opts) ->
    try
        {Class, Rest2}  = orient_decode_record:decode_v0_class(Rest, Opts),
        {Header, Rest3} = orient_decode_record:decode_v0_header(Rest2, Class, Opts),
        Record          = orient_decode_record:decode_v0_data(Rest3, Class, Header, Opts),
        {ok, Record}
    catch
        throw:{error, Reason} -> {error, Reason}
    end.


%%%%% ------------------------------------------------------- %%%%%


%-spec encode( record(), type:properties() ) -> type:okvalue_or_error( binary() ).


