
-module(stdforms).

-export([ from_string/1, from_string/2, from_string/3, from_string_loc/2
        , from_term/1
        , to_string/1, assert_valid/1
        , type/1, is_syntax/1, is_form/1, is_leaf/1, get_location/1
        , unique_name/0, make_warning/1, make_paramlist_str/1
        , transform/2
        , mapreduce/2, mapreduce/3
        , reduce/2, reduce/3 ]).

-export_type([ form/0, formlist/0, syntax/0, syntaxlist/0
             , reduceA/1, line/0, column/0, location/0 ]).


-define(ELSE,true).
-define(OTHERWISE,_).


%%%%% ------------------------------------------------------- %%%%%


-type form() :: erl_parse:abstract_form().
-type formlist() :: [form()].

-type syntax() :: form() | erl_parse:abstract_expr().
-type syntaxlist() :: [syntax()].
-type one_or_more_syntax() :: syntax() | syntaxlist().

-type reduceA(T) :: T | {error, _}.

-type line() :: ordinal().
-type column() :: ordinal().
-type location() :: line() | {line(), column()}.


%%%%% ------------------------------------------------------- %%%%%
   
    
-spec from_string( string() ) -> syntax().

from_string(Input) ->
    {AbsForm, _} = from_string_loc(Input, {1, 1}),
    AbsForm.

    
from_string(Input, Params) ->
    {AbsForm, } = from_string_loc( io_lib:format(Input, Params), {1, 1} ),
    AbsForm.

    
from_string(Input, Params, StartLoc) ->
    {AbsForm, } = from_string_loc( io_lib:format(Input, Params), StartLoc ),
    AbsForm.    

    
-spec from_string_loc( string(), location() ) -> {syntax(), location}.
    
from_string_loc(Input, StartLoc) ->
    {ok, Tokens, EndLoc} = erl_scan:string(Input, StartLoc),
    {ok, AbsForm} = try
                        {ok, _} = erl_parse:parse_form(Tokens)
                    catch
                        _:_ ->
                            {ok, ExForm} = erl_parse:parse_exprs(Tokens),
                    end,
    { AbsForm, EndLoc }.


%%%%% ------------------------------------------------------- %%%%%


-spec from_term( term() ) -> syntax().

from_term(Term) ->
    erl_syntax:abstract(Term).
    
    
%%%%% ------------------------------------------------------- %%%%%
    

-spec to_string( form() ) -> string().

to_string(Forms)
    when is_list(Forms) -> erl_prettypr:format(erl_syntax:form_list(Forms));
    
to_string(Form) -> erl_prettypr:format(erl_syntax:form_list([Form])).
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec read(atom() | iolist()) -> forms().
read(Module) when is_atom(Module) ->
    case beam_lib:chunks(code:which(Module), [abstract_code]) of
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {no_debug_info, _}} ->
            throw({forms_not_found, Module});
        {error, beam_lib, {file_error, _, enoent}} ->
            throw({module_not_found, Module})
    end;
read(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            Forms;
        {ok, Forms, _Extra} ->
            Forms;
        {error, enoent} ->
            throw({file_not_found, File})
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%

    
type(T)         -> {Type, _} = info(T), Type.

is_syntax(T)    -> {Type, _} = info(T), Type =/= undefined.
is_leaf(T)      -> {_, IsL} = info(T), IsL.

is_form(Node)   ->
    case type(Node) of
        attribute       -> true
    ;   function        -> true
    ;   eof_marker      -> true
    ;   error_marker    -> true
    ;   warning_marker  -> true

    ;   ?OTHERWISE      -> false
    end.


%%%%% ------------------------------------------------------- %%%%%
    
    
-spec assert_valid( one_or_more_syntax() ) -> ok | no_return().

assert_valid(Nodes) when is_list(Nodes) ->
    [ assert_valid(X) || X <- Nodes ], ok;
    
assert_valid(Node) ->
    case info(Node) of
        {undefined, _}  -> throwerror(invalid_syntax, Node)
    ;   ?OTHERWISE      -> ok
    end.
                

%%%%% ------------------------------------------------------- %%%%%


-spec get_location( syntax() ) -> location() | undefined.

get_location({error, {Loc, _, _}})      -> Loc;
get_location({warning, {Loc, _, _}})    -> Loc;
get_location(Node)                      ->
    case info(Node) of
        {undefined, _}  -> undefined
    ;   ?OTHERWISE      -> element(2, Node)
    end.

  
%%%%% ------------------------------------------------------- %%%%%
    
    
unique_name() ->
    binary_to_atom(
        iolist_to_binary(
            io_lib:format("_~p", [make_ref()])
        ), utf8).
        
        
make_warning(Msg) -> {warning, Msg}.


-spec make_paramlist_str( pos_integer() ) -> string().

make_paramlist_str(1)   -> "_p1";
make_paramlist_str(Num) ->
    lists:join([ format("_p~p", [X]) || X <- lists:seq(1, Num) ], ",").


%%%%% ------------------------------------------------------- %%%%%


-spec throwerror( term(), syntax() ) -> no_return().

throwerror(Reason, Node) ->
    throw({error, {get_location(Node), erl_parse, [Reason]}}).
    

%%%%% ------------------------------------------------------- %%%%%


-type mapR() :: skip | drop | continue | done | {done, syntax()}
              | {before, one_or_more_syntax()} | {after, one_or_more_syntax()}
              | {error, _} | one_or_more_syntax().
              
-type mapF() :: fun( ( syntax() ) -> mapR() )
              | fun( ( atom(), syntax() ) -> mapR() ).

-spec transform( mapF(), syntaxlist() ) -> syntaxlist();
               ( mapF(), syntax() ) -> syntax().

transform(Fun, Forms)
        when is_list(Forms)
           , ( is_function(Fun, 1) orelse is_function(Fun, 2) ) ->
    try
        transform_1(Fun, Forms)
    catch
        throw:Error -> [Error]
    end;
    
transform(Fun, Syntax) ->
    [Out] = transform([Syntax]),
    Out.

%
% Based on Ulf Wiger's parse_trans:plain_transform()
% well originally it was but it's been transformed quite a bit since then
%    

transform_1(_, [])              -> [];
transform_1(Fun, [L | Rest])
        when is_list(L)         ->
    [transform_1(Fun, L) | transform_1(Fun, Rest)];
        
transform_1(Fun, [Node | Rest]) ->
    {Type, IsLeaf} = info(Node),
    case Type of
        undefined   -> [ Node | transform_1(Fun, Rest) ]
        
    ;   ?OTHERWISE  ->
        case apply_transform_fun(Node) of
            skip                -> [ Node | transform_1(Fun, Rest)]
            
        ;   drop                -> transform_1(Fun, Rest)

        ;   continue            ->
                if
                    IsLeaf  -> [ Node | transform_1(Fun, Rest)]
                ;   ?ELSE   -> [ transform_1_tuple(Fun, Node) | transform_1(Fun, Rest)]
                end
        
        ;   done                -> [Node | Fs]    
        ;   {done, NewNode}     -> assert_valid(NewNode), [NewNode | Fs]
        
        ;   {error, Reason}     -> throwerror(Reason, Node)
        
        ;   {before, NewNodes} when is_list(NewNodes) ->
                assert_valid(NewNodes),
                NewNodes ++ [ Node | transform_1(Fun, Rest) ]
                
        ;   {before, NewNode}   ->
                assert_valid(NewNode),
                [ NewNode, Node | transform_1(Fun, Rest) ]
        
        ;   {after, NewNodes} when is_list(NewNodes) ->
                assert_valid(NewNodes),
                [ Node | NewNodes ] ++ transform_1(Fun, Rest)
                
        ;   {after, NewNode}    ->
                assert_valid(NewNode),
                [ Node, NewNode | transform_1(Fun, Rest) ]
        
        ;   NewNodes when is_list(NewNodes) ->
                assert_valid(NewNodes),
                NewNodes ++ transform_1(Fun, Rest)
        
        ;   NewNode             ->
                assert_valid(NewNode),
                [ NewNode | transform_1(Fun, Rest) ]
        end
    end.
    
    
transform_1_tuple(Fun, Node) ->
    list_to_tuple( transform_1(Fun, tuple_to_list(Node)) ).
    
    
apply_transform_fun(Fun, Node) ->
    if
        is_function(Fun, 1) -> Fun(Node)
    ;   is_function(Fun, 2) -> Fun(type(Node), Node)
    end.    
    

%%%%% ------------------------------------------------------- %%%%%
    

-type mapredR(T) :: skip | {drop, T} | {continue, T} | {error, _}
                  | {done, T} | {done, syntax(), T}
                  | {before, one_or_more_syntax(), T}
                  | {after, one_or_more_syntax(), T}
                  | {one_or_more_syntax(), T} | T.
-type mapredF(T) :: fun( ( syntax(), T ) -> mapredR(T) )
                  | fun( ( atom(), syntax(), T ) -> mapredR(T) ).    


-spec mapreduce( mapredF(T), {reduceA(T), syntaxlist()} ) -> {reduceA(T), syntaxlist()}.    

mapreduce(Fun, {Acc, Forms}) ->
    mapreduce(Fun, Acc, Forms).


-spec mapreduce( mapredF(T), reduceA(T), syntaxlist() ) -> {reduceA(T), syntaxlist()}.    

mapreduce(_, {error, _} = E, Forms)     -> {E, Forms}; 
mapreduce(Fun, Acc, Forms)
        when is_list(Forms)
           , ( is_function(Fun, 2) orelse is_function(Fun, 3) ) ->
    try
        mapreduce_1(Fun, Acc, Forms)
    catch
        throw:Error -> {Error, [Error]}
    end.
    
    
apply_mapred_fun(Fun, Node, Acc) ->
    if
        is_function(Fun, 2) -> Fun(Node, Acc)
    ;   is_function(Fun, 3) -> Fun(type(Node), Node, Acc)
    end.


mapreduce_1(_, {error, _} = E, Forms)   -> {E, Forms};    
mapreduce_1(_, Acc, [])                 -> {Acc, []};
mapreduce_1(Fun, Acc, [L | Rest])
        when is_list(L)                 ->
    {Acc2, L2} = mapreduce_1(Fun, Acc, L),
    {Acc3, Rest2} = mapreduce_1(Fun, Acc2, Rest),
    {Acc3, [L2 | Rest2]};

    
mapreduce_1(Fun, Acc, [Node | Rest])    ->
    {Type, IsLeaf} = info(Node),
    case Type of
        undefined   ->
            {Acc2, Rest2} = mapreduce_1(Fun, Acc, Rest),
            {Acc2, [Node | Rest2]}
        
    ;   ?OTHERWISE  ->
        case apply_mapred_fun(Fun, Node, Acc) of
            skip                        ->
                {Acc2, Rest2} = mapreduce_1(Fun, Acc, Rest),
                {Acc2, [Node | Rest2]}
            
        ;   {drop, NewAcc}              -> mapreduce_1(Fun, NewAcc, Rest)

        ;   {continue, NewAcc}          ->
                if
                    IsLeaf  ->
                        {NewAcc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                        {NewAcc2, [Node | Rest2]}
                        
                ;   ?ELSE   ->
                        {NewAcc2, Node2} = mapreduce_1_tuple(Fun, NewAcc, Node),
                        {NewAcc3, Rest2} = mapreduce_1(Fun, NewAcc2, Rest),
                        {NewAcc3, [Node2 | Rest2]}
                end
                
        ;   {done, NewAcc}              -> {NewAcc, [Node | Rest]}
        ;   {done, NewNode, NewAcc}     -> assert_valid(NewNode), {NewAcc, [NewNode | Rest]}
        
        ;   {error, Reason}             -> throwerror(Reason, Node)
        
        ;   {{error, Reason},_}         -> throwerror(Reason, Node)
        
        ;   {before, NewNodes, NewAcc} when is_list(NewNodes) ->
                {Acc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                {Acc2, NewNodes ++ [Node | Rest2]}
                
        ;   {before, NewNode, NewAcc}   ->
                {Acc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                {Acc2, [NewNode, Node | Rest2]}
        
        ;   {after, NewNodes, NewAcc} when is_list(NewNodes) ->
                {Acc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                {Acc2, [ Node | NewNodes ] ++ Rest2}
                
        ;   {after, NewNode, NewAcc}    ->
                {Acc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                {Acc2, [Node, NewNode | Rest2]}
        
        ;   {NewNodes, NewAcc} when is_list(NewNodes) ->
                assert_valid(NewNodes),
                {Acc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                {Acc2, NewNode ++ Rest2}
                
        ;   {NewNode, NewAcc}       ->
                assert_valid(NewNode),
                {Acc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                {Acc2, [NewNode | Rest2]}
                
        ;   NewAcc                  ->
                {Acc2, Rest2} = mapreduce_1(Fun, NewAcc, Rest),
                {Acc2, [Node | Rest2]}
        end
    end.
        
    
%%%%% ------------------------------------------------------- %%%%%
    
    
-type reduceR(T) :: skip | {skip, T} | done | {done, T} | {error, _} | ok | T.
-type reduceF(T) :: fun( ( syntax(), T ) -> reduceR(T) )
                  | fun( ( atom(), syntax(), T ) -> reduceR(T) ).    


-spec reduce( reduceF(T), {reduceA(T), formlist()} ) -> reduceA(T).

reduce(Fun, {Acc, Forms}) ->
    reduce(Fun, Acc, Forms).


-spec reduce( reduceF(T), reduceA(T), formlist() ) -> reduceA(T).

reduce(_, {error, _} = E, _)    -> E;    
reduce(Fun, Acc, Forms)
        when is_list(Forms)
           , ( is_function(Fun, 2) orelse is_function(Fun, 3) ) ->
    try
        reduce_1(Fun, Acc, Forms)
    catch
        throw:Error -> Error
    end.
  

apply_reduce_fun(Fun, Node, Acc) ->
    if
        is_function(Fun, 2) -> Fun(Node, Acc)
    ;   is_function(Fun, 3) -> Fun(type(Node), Node, Acc)
    end.


reduce_1(_, {error, _} = E, _)      -> E;    
reduce_1(_, Acc, [])                -> Acc;
reduce_1(Fun, Acc, [L | Rest])
        when is_list(L)             ->
    Acc2 = reduce_1(Fun, Acc, L),
    reduce_1(Fun, Acc2, Rest);

    
reduce_1(Fun, Acc, [Node | Rest])   ->
    {Type, IsLeaf} = info(Node),
    case Type of
        undefined   -> reduce_1(Fun, Acc, Rest)
        
    ;   ?OTHERWISE  ->
        case apply_reduce_fun(Fun, Node, Acc) of
            skip            -> reduce_1(Fun, Acc, Rest)
            
        ;   {skip, NewAcc}  -> reduce_1(Fun, NewAcc, Rest)
        
        ;   done            -> Acc        
        
        ;   {done, NewAcc}  -> NewAcc
        
        ;   {error, Reason} -> throwerror(Reason, Node)

        ;   ok              ->
                if
                    IsLeaf  -> reduce_1(Fun, Acc, Rest)
                ;   ?ELSE   ->
                        NewAcc2 = reduce_1_tuple(Fun, Acc, Node),
                        reduce_1(Fun, NewAcc2, Rest)
                end
                
        ;   NewAcc         ->
                if
                    IsLeaf  -> reduce_1(Fun, NewAcc, Rest)
                ;   ?ELSE   ->
                        NewAcc2 = reduce_1_tuple(Fun, NewAcc, Node),
                        reduce_1(Fun, NewAcc2, Rest)
                end
        end
    end.
    
reduce_1_tuple(Fun, Acc, Node) ->
    reduce_1(Fun, Acc, tuple_to_list(Node)).
    
    
%%%%% ------------------------------------------------------- %%%%%    
    
    
% from erl_syntax.erl    
-spec info( syntax() ) -> {atom(), boolean()}.    

-define(LEAF(X), {X, true}).
-define(TREE(X), {X, false}).

info(X) when not is_tuple(X)            -> LEAF(undefined);
info(Node) ->
    %% Check for `erl_parse'-compatible nodes, and otherwise fail.
    case Node of
    %% Leaf types
        {atom, _, _}                    -> ?LEAF(atom)
    ;   {char, _, _}                    -> ?LEAF(char)
    ;   {float, _, _}                   -> ?LEAF(float)
    ;   {integer, _, _}                 -> ?LEAF(integer)
    ;   {nil, _}                        -> ?LEAF(nil)
    ;   {string, _, _}                  -> ?LEAF(string)
    ;   {var, _, Name} ->
            if
                Name =:= '_'            -> ?LEAF(underscore)
            ;   true                    -> ?LEAF(variable)
            end
    ;   {error, _}                      -> ?LEAF(error_marker)
    ;   {warning, _}                    -> ?LEAF(warning_marker)
    ;   {eof, _}                        -> ?LEAF(eof_marker)

    %% Composite types
    ;   {'case', _, _, _}               -> ?TREE(case_expr)
    ;   {'catch', _, _}                 -> ?TREE(catch_expr)
    ;   {'cond', _, _}                  -> ?TREE(cond_expr)
    ;   {'fun', _, {clauses, _}}        -> ?TREE(fun_expr)
    ;   {named_fun, _, _, _}            -> ?TREE(named_fun_expr)
    ;   {'fun', _, {function, _, _}}    -> ?TREE(implicit_fun)
    ;   {'fun', _, {function, _, _, _}} -> ?TREE(implicit_fun)
    ;   {'if', _, _}                    -> ?TREE(if_expr)
    ;   {'receive', _, _, _, _}         -> ?TREE(receive_expr)
    ;   {'receive', _, _}               -> ?TREE(receive_expr)
    ;   {attribute, _, _, _}            -> ?TREE(attribute)
    ;   {bin, _, _}                     -> ?TREE(binary)
    ;   {bin_element, _, _, _, _}       -> ?TREE(binary_field)
    ;   {block, _, _}                   -> ?TREE(block_expr)
    ;   {call, _, _, _}                 -> ?TREE(application)
    ;   {clause, _, _, _, _}            -> ?TREE(clause)
    ;   {cons, _, _, _}                 -> ?TREE(list)
    ;   {function, _, _, _, _}          -> ?TREE(function)
    ;   {b_generate, _, _, _}           -> ?TREE(binary_generator)
    ;   {generate, _, _, _}             -> ?TREE(generator)
    ;   {lc, _, _, _}                   -> ?TREE(list_comp)
    ;   {bc, _, _, _}                   -> ?TREE(binary_comp)
    ;   {match, _, _, _}                -> ?TREE(match_expr)
    ;   {map, _, _, _}                  -> {map_expr, map_expr_fields(Node) =:= [] andalso map_expr_argument(Node) =:= none}
    ;   {map, _, _}                     -> {map_expr, map_expr_fields(Node) =:= [] andalso map_expr_argument(Node) =:= none}
    ;   {map_field_assoc, _, _, _}      -> ?TREE(map_field_assoc)
    ;   {map_field_exact, _, _, _}      -> ?TREE(map_field_exact)
    ;   {op, _, _, _, _}                -> ?TREE(infix_expr)
    ;   {op, _, _, _}                   -> ?TREE(prefix_expr)
    ;   {record, _, _, _, _}            -> ?TREE(record_expr)
    ;   {record, _, _, _}               -> ?TREE(record_expr)
    ;   {record_field, _, _, _, _}      -> ?TREE(record_access)
    ;   {record_index, _, _, _}         -> ?TREE(record_index_expr)
    ;   {remote, _, _, _}               -> ?TREE(module_qualifier)
    ;   {'try', _, _, _, _, _}          -> ?TREE(try_expr)
    ;   {tuple, _, _}                   -> {tuple, erl_syntax:tuple_elements(Node) =:= []}

    %% Type types
    ;   {ann_type, _, _}                -> ?TREE(annotated_type)
    ;   {remote_type, _, _}             -> ?TREE(type_application)
    ;   {type, _, binary, [_, _]}       -> ?TREE(bitstring_type)
    ;   {type, _, bounded_fun, [_, _]}  -> ?TREE(constrained_function_type)
    ;   {type, _, constraint, [_, _]}   -> ?TREE(constraint)
    ;   {type, _, 'fun', []}            -> ?LEAF(fun_type)
    ;   {type, _, 'fun', [_, _]}        -> ?TREE(function_type)
    ;   {type, _, map, _}               -> {map_type, erl_syntax:map_type_fields(Node) =:= any_size}
    ;   {type, _, map_field_assoc, _}   -> ?TREE(map_type_assoc)
    ;   {type, _, map_field_exact, _}   -> ?TREE(map_type_exact)
    ;   {type, _, record, _}            -> ?TREE(record_type)
    ;   {type, _, field_type, _}        -> ?TREE(record_type_field)
    ;   {type, _, range, _}             -> ?TREE(integer_range_type)
    ;   {type, _, tuple, _}             -> {tuple_type, erl_syntax:tuple_type_elements(Node) =:= any_size}
    ;   {type, _, union, _}             -> ?TREE(type_union)
    ;   {type, _, _, _}                 -> ?TREE(type_application)
    ;   {user_type, _, _, _}            -> ?TREE(user_type_application)
    
    %% Invalid Node
    ;   ?OTHERWISE                      -> ?LEAF(undefined)
    end.    