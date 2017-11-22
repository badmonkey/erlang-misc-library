
-module(type).

-export([ identify/1, check/2, wrap_okvalue/1, unwrap_okvalue/1 ]).

-export_type([ error/0, ok_or_error/0, value_or_error/1, okvalue_or_error/1
             , endpoint/0, type_id/0
             , start_result/0, server_name/0, server_from/0
             , one_or_many/1, atomlist/0, format/0
             , match_any/0, searchable/1, exception/0
             , natural/0, cardinal/0, ordinal/0, properties/0
             , mapping/0, mapping/2, transformf/1, transform2f/1
             , predicate/1, predicate/2, reducerf/2
             , mutatef/1, mutatef/2, mapfoldf/2, mapfoldf/3
             , filterfoldf/2, accumulation/2]).


%%%%% ------------------------------------------------------- %%%%%


-type error() :: { error, _ }.
-type ok_or_error() :: ok | error().
-type value_or_error(T) :: T | error().
-type okvalue_or_error(T) :: { ok, T } | error().


-type endpoint() :: undefined
                 | { inet:port_number() }
                 | { localhost | addr_any | inet:ip_address()
                   , inet:port_number() }.
                   

-type start_result() :: ignore | okvalue_or_error( pid() ).

-type server_name() :: undefined | atom() | pid() | {local, atom()} | {global, term()} | {via, atom(), term()}.
-type server_from() :: {pid(), term()}.


-type one_or_many(T) :: T | [T].
-type atomlist() :: one_or_many(atom()).

-type format() :: string().

-type match_any() :: '_' | '$1' | '$2' | '$3' | '$4' | '$5' | '$6' | '$7' | '$8' | '$9'.
-type searchable(T) :: match_any() | T.

-type exception() :: no_return().

-type natural() :: non_neg_integer().
-type cardinal() :: non_neg_integer().  % quantity of things numbers ie 0, 1, ...
-type ordinal() :: pos_integer().       % order of things numbers 1st, 2nd, 3rd


-type properties() :: #{ atom() => term() }
                    | [ atom() | { atom(), term() } ].


% higher order functions
                   
% maps 
-type mapping() :: mapping( term(), term() ).
-type mapping(X, Y) :: fun( (X) -> Y ).

-type transformf(T) :: mapping(T,T).
-type transform2f(T) :: fun( (T, T) -> T ).


% filters
-type predicate(T) :: fun( (T) -> boolean() ).
-type predicate(K, V) :: fun( (K, V) -> boolean() ).


% folds
-type reducerf(T, Acc) :: fun( (T, Acc) -> Acc ).


% filtermap
-type mutatef(T) :: fun( (T) -> boolean() | {true, T} ).
-type mutatef(K, V) :: fun( (K, V) -> boolean() | {true, V} | {true, K, V} ).


% mapfold
-type mapfoldf(T, Acc) :: mapfoldf(T, T, Acc).
-type mapfoldf(T, T2, Acc) :: fun( (T, Acc) -> {T2, Acc} ).


% filterfold
-type filterfoldf(T, Acc) :: fun( (T, Acc) -> {boolean(), Acc} ).


% accumulation
-type accumulation(T, Acc) :: {[T], Acc}.


-type type_id() :: integer | float | list | tuple | binary | bitstring
                 | boolean | function | pid | port | reference | atom | map
                 | undefined.


%%%%% ------------------------------------------------------- %%%%%
%
%
%
-spec identify( term() ) -> type_id().

identify(X) when is_integer(X)      -> integer;
identify(X) when is_float(X)        -> float;
identify(X) when is_list(X)         -> list;
identify(X) when is_tuple(X)        -> tuple;
identify(X) when is_binary(X)       -> binary;
identify(X) when is_bitstring(X)    -> bitstring;
identify(X) when is_boolean(X)      -> boolean;
identify(X) when is_function(X)     -> function;
identify(X) when is_pid(X)          -> pid;
identify(X) when is_port(X)         -> port;
identify(X) when is_reference(X)    -> reference;
identify(X) when is_atom(X)         -> atom;
identify(X) when is_map(X)          -> map;

identify(_X)                        -> undefined.


%%%%% ------------------------------------------------------- %%%%%


%get_extended(X, Attr) ->
% natural, ordinal, cardinal
% ok, error, okvalue
% property, propslist
% tuple( size=, element_types )
% record

%type:check( {tuple, 3}, X)
%type:check( {tuple, {integer, string, integer}}, X)
%type:check( #person{}, X )


%%%%% ------------------------------------------------------- %%%%%
%
%
%
-spec check( type_id(), term() ) -> ok | exception().

check(Type, X) ->
    check_1(Type, get(X)).
    
check_1(X, X) -> ok;
check_1(X, T) -> error:throw_error({failed_assert, X, T}).



%%%%% ------------------------------------------------------- %%%%%


-spec wrap_okvalue( okvalue_or_error(T) | T ) -> okvalue_or_error(T).

wrap_okvalue({error, _} = E)    -> E;
wrap_okvalue({ok, _} = Ok)      -> Ok;
wrap_okvalue(X)                 -> {ok, X}.


-spec unwrap_okvalue( okvalue_or_error(T) ) -> T | type:exception().

unwrap_okvalue({ok, X}) -> X;
unwrap_okvalue(_)       -> error:throw_error(invalid_okvalue).

