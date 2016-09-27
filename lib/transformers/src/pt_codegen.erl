
-module(pt_codegen).

-export([parse_transform/2]).


%%%%% ------------------------------------------------------- %%%%%


parse_transform(Forms, _Options) ->
    stdforms:transform(fun do_transform/1, Forms).


%%%%% ------------------------------------------------------- %%%%%


do_transform({call, _, {atom, _, quote}, Args}) ->
    case Args of
        []      -> {error, "quote() doesn't work on nothing"}
    ;   [Expr]  -> erl_parse:abstract(Expr)
    ;   _       -> erl_parse:abstract(Args)
    end;

do_transform(_) ->
    continue.
    


