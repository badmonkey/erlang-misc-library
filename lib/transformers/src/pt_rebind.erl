-module(pt_rebind).

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    {Forms2, _} = parse_trans:transform(fun do_transform/4, #{}, Forms, Options),
    Out = parse_trans:revert(Forms2),
    %% parse_trans:pp_src(Out, "transformed.rebind.erl"),
    Out.

do_transform(application,{call,_,{atom,_,rebind},[{var,Line,Name}]}, _Context, Acc) ->
    Count = maps:get(Name, Acc, 0) + 1,
    {rebind(Name, Line, Count), false, maps:put(Name, Count, Acc)};
do_transform(variable,{var,Line,Name}, _Context, Acc) ->
    {rebind(Name, Line, Acc), false, Acc};
do_transform(match_expr, {match,Line,LH,RH}, Context, Acc) ->
    %% reverse the recurse order
    {RH2, Acc2} = transform_children(RH,Acc,Context),
    {LH2, Acc3} = transform_children(LH,Acc2,Context),
    {{match,Line,LH2,RH2}, false, Acc3};

%% Don't pass on any acc'd vars
do_transform(block_expr, {block,Line,Body}, Context, Acc) ->
    {Body2, _} = transform_children(Body, Acc, Context),
    {{block,Line,Body2}, false, Acc};
do_transform(clause, {clause,Line,Pattern,Guards,Body}, Context, Acc) ->
    {Pattern2, Acc2} = transform_children(Pattern, Acc, Context),
    {Body2, _} = transform_children(Body, Acc2, Context),
    {{clause,Line,Pattern2,Guards,Body2}, false, Acc};
do_transform(function, {function,Line,Name,Arity,Clauses}, Context, Acc) ->
    Clauses2 = [begin
        {Out, _} = transform_children(Clause, #{}, Context),
        Out
    end || Clause <- Clauses],
    {{function,Line,Name,Arity,Clauses2}, false, Acc};

do_transform(_Type, Form, _Context, Acc) ->
    {Form, true, Acc}.

rebind(Name, Line, Acc) when is_map(Acc) ->
    case maps:find(Name, Acc) of
        error -> {var, Line, Name};
        {ok, Count} -> rebind(Name, Line, Count)
    end;
rebind(Name, Line, Count) when is_integer(Count) ->
    {var, Line, list_to_atom("_" ++ atom_to_list(Name) ++ "__REBIND__" ++ integer_to_list(Count))}.

transform_children(Children, Acc, Context) ->
    case catch do_transform_children(Children, Acc, Context) of
        {'EXIT', Error} -> Error;
        {Children2, Acc2} -> {Children2, Acc2}
    end.

do_transform_children(Children, Acc, Context) when is_list(Children) ->
    {Children2, Acc2} = parse_trans:do_transform(fun do_transform/4, Acc, Children, Context),
    {parse_trans:revert(Children2), Acc2};
do_transform_children(Child, Acc, Context) ->
    {[Child2], Acc2} = do_transform_children([Child], Acc, Context),
    {Child2, Acc2}.
