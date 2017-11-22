    
-module(pt_with).

-export([parse_transform/2, do_transform/2]).


parse_transform(ParseTree, _Options) ->
    stdforms:transform(fun do_transform/2, ParseTree).
    
    
%%%%% ------------------------------------------------------- %%%%%


do_transform(list_comp, {lc, LINE, {atom, _, 'with'}, Sequence}) ->
    {block, LINE, make_with_sequence(Sequence));
    
do_transform(list_comp, {lc, LINE, {var, _, 'WITH'}, Sequence}) ->
    {block, LINE, make_with_sequence(Sequence));

do_transform(_, _) -> continue.    


%%%%% ------------------------------------------------------- %%%%%


-spec make_with_sequence( syntaxlist() ) -> syntaxlist().

make_with_sequence([]) -> [];

make_with_sequence([{generate, LINE, Pattern, Expr}]) ->
    [ make_case_expr(Expr, LINE, Pattern, [Pattern]) ];

make_with_sequence([Expr]) -> [Expr];

make_with_sequence([{generate, LINE, Pattern, Expr} | Rest]) ->
    [ make_case_expr(Expr, LINE, Pattern, [make_with_expr(Rest)]) ].

make_with_sequence([Expr | Rest]) ->
    [ Expr | make_with_sequence(Rest) ].


-spec make_case_expr( syntax(), location(), syntax(), syntaxlist() ) -> syntax().

make_case_expr(Expr, LINE, Pattern, Body) ->
    ELSE = {'var', LINE, stdforms:unique_name()},
    {'case', LINE, Expr
    , [ {clause, LINE, [Pattern], [], Body}
      , {clause, LINE, [ELSE], [], ELSE}
      ]
    }.

