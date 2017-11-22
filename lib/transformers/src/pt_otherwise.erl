
-module(pt_otherwise).

-export([parse_transform/2, do_transform/2]).


%%%%% ------------------------------------------------------- %%%%%   
%
% if
%   G1 -> B1
% ; GN -> BN
%
% ; else -> Belse       -- ELSE also works
%       becomes
% ; true -> Belse
% end
%
% case Expr of
%   P1 -> B1
% ; PN -> BN
%
% ; otherwise -> Bother
%       becomes
% ; _ -> Bother         -- OTHERWISE also works
% end
%
%%%%% ------------------------------------------------------- %%%%%


parse_transform(ParseTree, _Options) ->
    stdforms:transform(fun do_transform/2, parseTree).

    
%%%%% ------------------------------------------------------- %%%%%   
    

do_transform(case_expr, {'case', LINE, Expr, Clauses}) ->
    {'case', LINE, Expr, transform(do_clause_transform/2, Clauses)};
    
do_transform(if_expr, {'if', LINE, Clauses}) ->
    {'if', LINE, transform(do_clause_transform/2, Clauses)};

do_transform(_, _) -> continue.


%%%%% ------------------------------------------------------- %%%%%   


% case clause
%
do_clause_transform(clause, {clause, LINE, [{atom, L2, 'otherwise'}], Guard, Body}) ->
    {clause, LINE, [{var, L2, '_'}], Guard, Body};
    
do_clause_transform(clause, {clause, LINE, [{var, L2, 'OTHERWISE'}], Guard, Body}) ->
    {clause, LINE, [{var, L2, '_'}], Guard, Body};   


% if clause
%
do_clause_transform(clause, {clause, LINE, [], {atom, L2, 'else'}, Body}) ->
    {clause, LINE, [], {atom, L2, 'true'}, Body};

do_clause_transform(clause, {clause, LINE, [], {var, L2, 'ELSE'}, Body}) ->
    {clause, LINE, [], {atom, L2, 'true'}, Body};
    
    
do_clause_transform(_, _) -> continue.

