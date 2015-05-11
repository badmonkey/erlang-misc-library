
-module(type).

-export_type([error/0, ok_or_error/0, endpoint/0, start_result/0, atomlist/0]).


%%%%% ------------------------------------------------------- %%%%%


-type error() :: {error, _}.
-type ok_or_error() :: ok | error().

-type endpoint() :: {inet:ip_address(), inet:port_number()}.

-type start_result() :: {ok, Pid :: pid()} | ignore | error().

-type atomlist() :: atom() | [atom()].


