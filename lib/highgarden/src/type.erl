
-module(type).

-export([ get/1, wrap_okvalue/1, unwrap_okvalue/1 ]).

-export_type([ error/0, ok_or_error/0, value_or_error/1, okvalue_or_error/1
             , endpoint/0
             , start_result/0, server_name/0, server_from/0
             , one_or_many/1, atomlist/0, format/0
             , match_any/0, searchable/1, exception/0
             , natural/0, cardinal/0, ordinal/0, properties/0
             , map_function/0, map_function/1
             , predicate/1, predicate/2, mutator/1, mutator/2, transform/1]).


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
                    
-type map_function(X) :: fun( (X) -> X ).
-type map_function() :: map_function( term() ).

-type predicate(T) :: fun( (T) -> boolean() ).
-type predicate(K, V) :: fun( (K, V) -> boolean() ).

-type mutator(T) :: fun( (T) -> remove | {value, T} ).
-type mutator(K, V) :: fun( (K, V) -> remove | {value, V} | {value, K, V} ).

-type transform(T) :: fun( (T) -> T ).



%%%%% ------------------------------------------------------- %%%%%


-spec get( term() ) -> atom().

get(X) when is_integer(X)   -> integer;
get(X) when is_float(X)     -> float;
get(X) when is_list(X)      -> list;
get(X) when is_tuple(X)     -> tuple;
get(X) when is_binary(X)    -> binary;
get(X) when is_bitstring(X) -> bitstring;
get(X) when is_boolean(X)   -> boolean;
get(X) when is_function(X)  -> function;
get(X) when is_pid(X)       -> pid;
get(X) when is_port(X)      -> port;
get(X) when is_reference(X) -> reference;
get(X) when is_atom(X)      -> atom;
get(X) when is_map(X)       -> map;

get(_X)                     -> undefined.


%%%%% ------------------------------------------------------- %%%%%


-spec wrap_okvalue( okvalue_or_error(T) | T ) -> okvalue_or_error(T).

wrap_okvalue({error, _} = E)    -> E;
wrap_okvalue({ok, _} = Ok)      -> Ok;
wrap_okvalue(X)                 -> {ok, X}.


-spec unwrap_okvalue( okvalue_or_error(T) ) -> T | type:exception().

unwrap_okvalue({ok, X}) -> X;
unwrap_okvalue(_)       -> error:throw_error(invalid_okvalue).

