
-module(type).

-export([get/1]).

-export_type([error/0, ok_or_error/0, endpoint/0, start_result/0, atomlist/0, format/0]).


%%%%% ------------------------------------------------------- %%%%%


-type error() :: {error, _}.
-type ok_or_error() :: ok | error().

-type endpoint() :: {inet:ip_address(), inet:port_number()}.

-type start_result() :: {ok, Pid :: pid()} | ignore | error().

-type atomlist() :: atom() | [atom()].

-type format() :: string().


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


