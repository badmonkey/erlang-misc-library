
-module(uuid).

-export([version/1]).
-export([new_v1/0, new_v1/2, new_v3/2, new_v4/0, new_v4/4, new_v5/2]).
-export([to_string/1, to_string/2, to_binary/1]).


-define(UUIDv1, 1).
-define(UUIDv3, 3).
-define(UUIDv4, 4).
-define(UUIDv5, 5).

-define(VARIANT10, 2#10).

-type uuid_version() :: 1 | 3 | 4 | 5 .
-type uuid_rec() :: {uuid, uuid_version(), binary()}.
-type uuid_type() :: binary | string() | uuid_rec().


%
%% Based heavily on https://github.com/avtobiff/erlang-uuid
%


%%%%% ------------------------------------------------------- %%%%%


-spec version( uuid_type() ) -> {ok, pos_integer} | {error, term()}.

version(U) when is_binary(U) ->
    version_field(U);
    
version(S) when is_list(S) ->
    {ok, 1};

version({uuid, N, Data}) ->    
    case version_field(Data) of
        N   -> {ok, N}
    ;   _   -> {error, version_mismatch}
    end.
    

-spec version_field( binary() ) -> integer().

version_field(<<_:128>> = Uuid) ->
    <<_:48, Version:4, _:76>> = Uuid,
    Version.


%%%%% ------------------------------------------------------- %%%%%


new_v1() -> new_v1(null, null).


-spec new_v1( binary() | null, binary() | null ) -> {uuid, 1, binary()}.

new_v1(null, null) -> new_v1( get_node_id(), xrandom:bits(14) );
new_v1(Node, null) -> new_v1( Node, xrandom:bits(14) );
new_v1(null, ClockSeq) -> new_v1( get_node_id(), ClockSeq );

new_v1(<<Node:48>>, <<ClockSeq:14>>) ->
    <<TimeHi:12, TimeMid:16, TimeLow:32>> = xtime:gregorian_epoch(),
    <<ClockSeqHi:6, ClockSeqLow:8>> = ClockSeq,

    {uuid, 1, << TimeLow:32, TimeMid:16
               , ?UUIDv1:4
               , TimeHi:12
               , ?VARIANT10:2
               , ClockSeqLow:8, ClockSeqHi:6
               , Node:48>>}.


%%%%% ------------------------------------------------------- %%%%%


-spec new_v3( atom() | uuid_type(), string() ) -> {uuid, 3, binary()}.

new_v3(dns, Name)   -> create_namebased_uuid(md5, bincat(<<16#6ba7b8109dad11d180b400c04fd430c8:128>>, Name));
new_v3(url, Name)   -> create_namebased_uuid(md5, bincat(<<16#6ba7b8119dad11d180b400c04fd430c8:128>>, Name));
new_v3(oid, Name)   -> create_namebased_uuid(md5, bincat(<<16#6ba7b8129dad11d180b400c04fd430c8:128>>, Name));
new_v3(x500, Name)  -> create_namebased_uuid(md5, bincat(<<16#6ba7b8149dad11d180b400c04fd430c8:128>>, Name));
new_v3(nil, Name)   -> create_namebased_uuid(md5, bincat(<<0:128>>, Name));

new_v3(S, Name) when is_list(S) ->
    new_v3(nil, Name);
    %create_namebased_uuid(md5, list_to_binary([to_binary(UuidStr), Name]));
    
new_v3({uuid, _, Data}, Name) when is_binary(Data) ->
    create_namebased_uuid(md5, list_to_binary([Data, Name]));
    
new_v3(_, _) ->
    erlang:error(badarg).

    
%%%%% ------------------------------------------------------- %%%%%


-spec new_v4() -> {uuid, 4, binary()}.

new_v4() ->
    <<U0:32, U1:16, _:4, U2:12, _:2, U3:30, U4:32>> = crypto:rand_bytes(16),

    {uuid, 4, <<U0:32, U1:16, ?UUIDv4:4, U2:12, ?VARIANT10:2, U3:30, U4:32>>}.
    
    
-spec new_v4(<<_:48>>, <<_:12>>, <<_:14>>, <<_:48>>) -> {uuid, 4, binary()}.

new_v4(<<A:48>>, <<B:12>>, <<C:14>>, <<D:48>>) ->
    {uuid, 4, <<A:48, ?UUIDv4:4, B:12, ?VARIANT10:2, C:14, D:48>>}.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec new_v5( atom() | uuid_type(), string() ) -> {uuid, 5, binary()}.

new_v5(dns, Name)   -> create_namebased_uuid(sha1, bincat(<<16#6ba7b8109dad11d180b400c04fd430c8:128>>, Name));
new_v5(url, Name)   -> create_namebased_uuid(sha1, bincat(<<16#6ba7b8119dad11d180b400c04fd430c8:128>>, Name));
new_v5(oid, Name)   -> create_namebased_uuid(sha1, bincat(<<16#6ba7b8129dad11d180b400c04fd430c8:128>>, Name));
new_v5(x500, Name)  -> create_namebased_uuid(sha1, bincat(<<16#6ba7b8149dad11d180b400c04fd430c8:128>>, Name));
new_v5(nil, Name)   -> create_namebased_uuid(sha1, bincat(<<0:128>>, Name));

new_v5(S, Name) when is_list(S) ->
    new_v5(nil, Name);
    %create_namebased_uuid(sha1, list_to_binary([to_binary(UuidStr), Name]));
    
new_v5({uuid, _, Data}, Name) when is_binary(Data) ->
    create_namebased_uuid(sha1, list_to_binary([Data, Name]));
    
new_v5(_, _) ->
    erlang:error(badarg).
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec to_string( uuid_type() ) -> string().
    
to_string(U) -> to_string(U, rfc4122).


-spec to_string( uuid_type(), raw | rfc4122 ) -> string().

to_string(S, _) when is_list(S) -> S;

to_string(<<S:128>>, raw) ->
    xstring:format("~32.16.0b", [S]);
    
to_string(<<U0:32, U1:16, U2:16, U3:16, U4:48>>, rfc4122) ->
    xstring:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [U0, U1, U2, U3, U4]);

to_string({uuid, _, Data}, X) -> to_string(Data, X).


%%%%% ------------------------------------------------------- %%%%%


-spec to_binary( string() ) -> binary().

to_binary(S)
        when  is_list(S)
            , length(S) =:= 32  ->
    Num = xstring:hex_to_int(S),
    <<Num:128>>;
    
to_binary(S)
        when  is_list(S)
            , length(S) =:= 36  ->
    Parts = string:tokens(S, "$-"),
    [I0, I1, I2, I3, I4] = [xstring:hex_to_int(Part) || Part <- Parts],
    <<I0:32, I1:16, I2:16, I3:16, I4:48>>.


%%%%% ------------------------------------------------------- %%%%%


-spec get_node_id() -> binary().

get_node_id() ->
    case xos:get_first_hwaddr() of
        {ok, HwAddr}    -> HwAddr
    ;   _               -> xrandom:hwaddr()
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec create_namebased_uuid(md5 | sha1, binary()) -> {uuid, 3 | 5, binary()}.

create_namebased_uuid(md5, Data) ->
    Md5 = crypto:hash(md5, Data),
    compose_namebased_uuid(?UUIDv3, Md5);
    
create_namebased_uuid(sha1, Data) ->
    <<Sha1:128, _:32>> = crypto:hash(sha, Data),
    compose_namebased_uuid(?UUIDv5, <<Sha1:128>>).


-spec compose_namebased_uuid(3 | 5, <<_:128>>) -> {uuid, 3 | 5, binary()}.

compose_namebased_uuid(Version, Hash) ->
    << TimeLow:32, TimeMid:16, _AndVersion:4
     , TimeHi:12, _AndReserved:2
     , ClockSeqHi:6, ClockSeqLow:8, Node:48>>  = Hash,

    {uuid, version
         , << TimeLow:32, TimeMid:16
            , Version:4
            , TimeHi:12
            , ?VARIANT10:2
            , ClockSeqHi:6, ClockSeqLow:8
            , Node:48>>}.

            
%%%%% ------------------------------------------------------- %%%%%


-spec bincat( binary(), string() ) -> binary().

bincat(<<_:128>> = Bin, Str) -> erlang:list_to_binary([Bin, Str]).

