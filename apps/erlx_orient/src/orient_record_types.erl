

-module(orient_record_types).

-export([ decode_boolean/1, decode_short/1, decode_integer/1, decode_long/1
        , decode_float/1, decode_double/1, decode_datetime/1
        , decode_string/1, decode_string/2, decode_binary/1, decode_binary/2
        , decode_link/1, decode_linklist/1, decode_linkset/1, decode_linkmap/1
        , decode_byte/1, decode_date/1
        , decode_varint/1, decode_pointer/1, decode_type/2, decode_typeid/1, decode_typestring/1 ]).

        
-include_lib("erlx_orient/include/constants.hrl").



%%%%% ------------------------------------------------------- %%%%%


decode_boolean(<<0:8, Rest/binary>>)    -> {false, Rest};
decode_boolean(<<1:8, Rest/binary>>)    -> {true, Rest};
decode_boolean(_)                       -> error:throw_error(invalid_boolean).


%%%%% ------------------------------------------------------- %%%%%


decode_short(Data)      -> decode_varint(Data).
decode_integer(Data)    -> decode_varint(Data).
decode_long(Data)       -> decode_varint(Data).


%%%%% ------------------------------------------------------- %%%%%


decode_float(<<F:?ODB_FLOAT, Rest/binary>>)     -> { F, Rest };
decode_float(_)                                 -> error:throw_error(invalid_float).

decode_double(<<D:?ODB_DOUBLE, Rest/binary>>)   -> { D, Rest };
decode_double(_)                                -> error:throw_error(invalid_double).


%%%%% ------------------------------------------------------- %%%%%


decode_datetime(Data) ->
    { MSecs_From_Epoch, Rest } = decode_long(Data),

    Epoch = calendar:datetime_to_gregorian_seconds( {{1970, 1, 1}, {0, 0, 0}} ),

    Total_Secs = Epoch + ( MSecs_From_Epoch div 1000 ),
    { calendar:gregorian_seconds_to_datetime(Total_Secs), Rest }.


%%%%% ------------------------------------------------------- %%%%%


decode_string(<<>>)         -> error:throw_error(invalid_string);
decode_string(Data)         -> decode_binary(Data).

decode_string(0, Data)      -> { <<>>, Data };
decode_string(Size, Data)   -> decode_binary(Size, Data).

    
%%%%% ------------------------------------------------------- %%%%%


decode_binary(<<>>) -> error:throw_error(invalid_binary);
decode_binary(Data) ->
    case bindecoder:zigzag(Data) of
        {ok, Size, Rest}    -> decode_binary(Size, Rest)
    ;   _                   -> error:throw_error(invalid_binary)
    end.
    
    
decode_binary(0, Data)      -> { <<>>, Data };
decode_binary(Size, Data)   ->
    case bindecoder:nbytes(Size, Data) of
        {ok, Binary, Rest}  -> { Binary, Rest }
    ;   _                   -> error:throw_error(invalid_binary)
    end.


%%%%% ------------------------------------------------------- %%%%%


%%%%% ------------------------------------------------------- %%%%%
%embedded   9       record
%embedlist  10      varint:byte:items[]     item = byte:bytes[]
%embedset   11      varint:byte:items[]
%embedmap   12      varint:header-struct:bytes[]
%%%%% ------------------------------------------------------- %%%%% 


decode_link(Data) ->
    { ClusterId, Rest } = decode_varint(Data),
    { Position, Rest2 } = decode_varint(Rest),
    { {rid, ClusterId, Position}, Rest2 }.


%%%%% ------------------------------------------------------- %%%%% 


decode_linklist(Data) ->
    { NElems, Rest } = decode_varint(Data),
    decode_array(NElems, fun decode_link/1, [], Rest).



%%%%% ------------------------------------------------------- %%%%% 


decode_linkset(Data) ->
    { LnkList, Rest } = decode_linklist(Data),
    { sets:from_list(LnkList), Rest }.


%%%%% ------------------------------------------------------- %%%%% 


decode_linkmap(Data) ->
    { NPairs, Rest } = decode_varint(Data),
    { KVlist, Final } = decode_array(NPairs,
                            fun(Bytes) ->
                                case decode_typeid(Bytes) of
                                    {string, MoRest}    ->
                                        {Key, MoRest2} = decode_string(MoRest),
                                        {Link, MoRest3} = decode_link(MoRest2),
                                        { {Key, Link}, MoRest3 }
                                        
                                ;   _                   -> error:throw_error(invalid_linkmap_key_type)
                                end
                            end, [], Rest),
    { maps:from_list(KVlist), Final }.

    
%%%%% ------------------------------------------------------- %%%%% 
    
%linkbag    22      ?
%  defp decode_simple_type(<<1, size :: 32, rest :: binary>>, :link_bag) do
%    {rids, rest} = Utils.reduce_n_times size, rest, fn(acc) ->
%      <<cluster_id :: 16, position :: 64>> <> acc = acc
%      {%RID{cluster_id: cluster_id, position: position}, acc}
%    end
%
%    {{:link_bag, rids}, rest}
%  end
%
%  defp decode_simple_type(<<0, _ :: binary>>, :link_bag) do
  
%%%%% ------------------------------------------------------- %%%%% 


decode_byte(<<>>)                           -> error:throw_error(invalid_byte);
decode_byte(<<B:?ODB_BYTE, Rest/binary>>)   -> { B, Rest }.


%%%%% ------------------------------------------------------- %%%%%  


decode_date(Data) ->
    { Days_From_Epoch, Rest } = decode_varint(Data),
    Days = calendar:date_to_gregorian_days(1970, 1, 1) + Days_From_Epoch,
    { calendar:gregorian_days_to_date(Days), Rest }.


%%%%% ------------------------------------------------------- %%%%%  


%decimal    21      int32:int32:bytes[]
%decode_decimal(Data) ->
%    <<scale :: 32, value_size :: 32, rest :: binary>>         = data
%    <<value :: big-size(value_size)-unit(8), rest :: binary>> = rest
%    value = value / round(:math.pow(10, scale))
%    {Decimal.new(value), rest}


%%%%% ------------------------------------------------------- %%%%%
% Virtual types used internally    

decode_varint(<<>>) -> error:throw_error(invalid_varint);       
decode_varint(Data) ->    
    case bindecoder:zigzag(Data) of
        {ok, Ptr, Rest} -> { Ptr, Rest }
    ;   _               -> error:throw_error(invalid_varint)
    end.    
    
    
%%%%% ------------------------------------------------------- %%%%%

    
decode_pointer(<<Ptr:?ODB_INT32, Rest/binary>>) ->
    { Ptr, Rest }.
    

%%%%% ------------------------------------------------------- %%%%%

    
decode_array(0, _, Acc, Rest) ->
    { lists:reverse(Acc), Rest };
    
decode_array(N, ElemFun, Acc, Data) ->
    { Value, Rest } = ElemFun(Data),
    decode_array(N - 1, ElemFun, [Value | Acc], Rest).    
    

%%%%% ------------------------------------------------------- %%%%%


-spec decode_type( orient_record:fieldtype(), binary() ) -> { term(), binary() } | type:exception().

decode_type(_, <<>>)        -> error:throw_error(incomplete_field);

decode_type(boolean, Data)  -> decode_boolean(Data);
decode_type(integer, Data)  -> decode_integer(Data);
decode_type(short, Data)    -> decode_short(Data);
decode_type(long, Data)     -> decode_long(Data);
decode_type(float, Data)    -> decode_float(Data);
decode_type(double, Data)   -> decode_double(Data);
decode_type(datetime, Data) -> decode_datetime(Data);
decode_type(string, Data)   -> decode_string(Data);
decode_type(binary, Data)   -> decode_binary(Data);

decode_type(link, Data)     -> decode_link(Data);
decode_type(linklist, Data) -> decode_linklist(Data);
decode_type(linkset, Data)  -> decode_linkset(Data);
decode_type(linkmap, Data)  -> decode_linkmap(Data);
decode_type(byte, Data)     -> decode_byte(Data);
decode_type(date, Data)     -> decode_date(Data);
%decode_type(decimal, Data)  -> decode_decimal(Data);
%decode_type(linkbag, Data)  -> decode_linkbag(Data);

decode_type(_, _)           -> error:throw_error(unsupported_type).

    
%%%%% ------------------------------------------------------- %%%%%


-spec decode_typeid( binary() ) -> { orient_record:fieldtype(), binary() } | type:exception().
    
decode_typeid(<<>>)                 -> error:throw_error(invalid_typeid);

decode_typeid(<<0:8, Rest/binary>>) -> { boolean, Rest };
decode_typeid(<<1:8, Rest/binary>>) -> { integer, Rest };
decode_typeid(<<2:8, Rest/binary>>) -> { short, Rest };
decode_typeid(<<3:8, Rest/binary>>) -> { long, Rest };
decode_typeid(<<4:8, Rest/binary>>) -> { float, Rest };
decode_typeid(<<5:8, Rest/binary>>) -> { double, Rest };
decode_typeid(<<6:8, Rest/binary>>) -> { datetime, Rest };
decode_typeid(<<7:8, Rest/binary>>) -> { string, Rest };
decode_typeid(<<8:8, Rest/binary>>) -> { binary, Rest };

decode_typeid(<<13:8, Rest/binary>>) -> { link, Rest };
decode_typeid(<<14:8, Rest/binary>>) -> { linklist, Rest };
decode_typeid(<<15:8, Rest/binary>>) -> { linkset, Rest };
decode_typeid(<<16:8, Rest/binary>>) -> { linkmap, Rest };
decode_typeid(<<17:8, Rest/binary>>) -> { byte, Rest };
decode_typeid(<<19:8, Rest/binary>>) -> { date, Rest };
%decode_typeid(<<21:8, Rest/binary>>) -> { decimal, Rest };
%decode_typeid(<<22:8, Rest/binary>>) -> { linkbag, Rest };
decode_typeid(<<23:8, Rest/binary>>) -> { any_type, Rest };

decode_typeid(_)                    -> error:throw_error(unknown_type).


%%%%% ------------------------------------------------------- %%%%%


-spec decode_typestring( binary() ) -> orient_record:fieldtype() | type:exception().
    
decode_typestring(<<>>)             -> error:throw_error(invalid_typeid);

decode_typestring(<<"BOOLEAN">>)    -> boolean;
decode_typestring(<<"INTEGER">>)    -> integer;
decode_typestring(<<"SHORT">>)      -> short;
decode_typestring(<<"LONG">>)       -> long;
decode_typestring(<<"FLOAT">>)      -> float;
decode_typestring(<<"DOUBLE">>)     -> double;
decode_typestring(<<"DATETIME">>)   -> datetime;
decode_typestring(<<"STRING">>)     -> string;
decode_typestring(<<"BINARY">>)     -> binary;

decode_typestring(<<"LINK">>)       -> link;
decode_typestring(<<"LINKLIST">>)   -> linklist;
decode_typestring(<<"LINKSET">>)    -> linkset;
decode_typestring(<<"LINKMAP">>)    -> linkmap;
decode_typestring(<<"BYTE">>)       -> byte;
decode_typestring(<<"DATE">>)       -> date;
%decode_typestring(<<"DECIMAL">>)    -> decimal;
%decode_typestring(<<"LINKBAG">>)    -> linkbag;
decode_typestring(<<"ANY">>)        -> any_type;

decode_typestring(_)                -> error:throw_error(unknown_type).
