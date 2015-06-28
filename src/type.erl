
-module(type).

-export([get/1]).

-export_type([ error/0, ok_or_error/0, value_or_error/1
             , endpoint/0
             , start_result/0, server_name/0
             , atomlist/0, format/0
             , match_any/0, searchable/1]).


%%%%% ------------------------------------------------------- %%%%%


-type error() :: { error, _ }.
-type ok_or_error() :: ok | error().
-type value_or_error(T) :: { ok, T } | error().


-type endpoint() :: undefined
                 | { inet:port_number() }
                 | { localhost | addr_any | inet:ip_address()
                   , inet:port_number() }.


-type start_result() :: ignore | value_or_error( pid() ).

-type server_name() :: undefined | atom() | {local, term()} | {global, term()} | {via, atom(), term()}.


-type atomlist() :: atom() | [atom()].

-type format() :: string().


-type match_any() :: '_' | '$1' | '$2' | '$3' | '$4' | '$5' | '$6' | '$7' | '$8' | '$9'.
-type searchable(T) :: match_any() | T.


%%%%% ------------------------------------------------------- %%%%%


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

get(_X)                     -> unknown.


