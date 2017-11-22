
-module(pt_codegen).

-export([parse_transform/2, do_transform/1]).


%%%%% ------------------------------------------------------- %%%%%
%
% X = abstract:quote(X + 1)
%
% becomes
%
% X = {op, LINE, '+', {var, LINE, 'X'}, {integer, LINE, 1}}
%
%%%%% ------------------------------------------------------- %%%%%


parse_transform(Forms, _Options) ->
    stdforms:transform(fun do_transform/1, Forms).


%%%%% ------------------------------------------------------- %%%%%


do_transform({call, _, {remote,_,{atom, _, 'abstract'},{atom, _, 'quote'}}, Args}) ->
    case Args of
        []      -> {error, "quote() requires atleast one expression"}
    ;   [Expr]  -> erl_parse:abstract(Expr)
    ;   _       -> erl_parse:abstract(Args)
    end;

do_transform(_) ->
    continue.
    


