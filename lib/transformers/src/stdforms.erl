
-module(stdforms).

-export([ from_string/1, from_string/2, from_string_ex/2
        , is_form/1, get_location/1, transform/2, transform/3
        , reset_locations/2 ]).

-export_type([ form/0
             , substitutions/0
             ]).


%%%%% ------------------------------------------------------- %%%%%


-type form() :: erl_parse:abstract_form().
-type substitutions() :: #{ string() => form() }.


%%%%% ------------------------------------------------------- %%%%%

% {ok, AbsForm} | {error, ErrorInfo}
% {ok, ExprList} | {error, ErrorInfo}
% error_info() = {erl_anno:line(), module(), error_description()}


from_string(Str) -> from_string(Str, {1, 1}).


-spec from_string( string(), type:location() ) -> form().

from_string(Str, Loc) ->
    {AbsForm, _} = from_string_ex(Str, Loc),
    AbsForm.

    
-spec from_string_ex( string(), type:location() ) -> { form(), type:location() }.

from_string_ex(Str, Loc) ->
    {ok, Tokens, Location} = erl_scan:string(Str, Loc),
    FirstLoc = erl_scan:location( hd(Tokens) ),
    {ok, AbsForm} = try
                        {ok, _} = erl_parse:parse_form(Tokens)
                    catch
                        _:_ ->
                            {ok, ExForm} = erl_parse:parse_exprs(Tokens),
                            { ok, {block, FirstLoc, ExForm} }
                    end,
    { AbsForm, Location }.
    
    
%%%%% ------------------------------------------------------- %%%%%
    
    
is_form(T) when element(1,T)==type -> true;
is_form(T) ->
    try erl_syntax:type(T),
        true
    catch
        error:_ -> false
    end.

    
%%%%% ------------------------------------------------------- %%%%%


get_location({error, {Loc, _, _}})      -> Loc;
get_location({warning, {Loc, _, _}})    -> Loc;
get_location(Node)
        when is_tuple(Node)
           , tuple_size(Node) >= 2      ->
    element(2, Node);
get_location(_)                         -> undefined.


%%%%% ------------------------------------------------------- %%%%%


throwerror(Reason, Form) ->
    throw({error, {get_location(Form), erl_parse, [Reason]}}).

    
%%%%% ------------------------------------------------------- %%%%%
    

-spec reset_locations( form(), type:location() | fun( ( type:location() ) -> type:location() ) ) -> form().

reset_locations(Form, X)
        when is_integer(X) ->
    reset_locations(Form, fun(_) -> {X, 1} end);
    
reset_locations(Form, {L, C})
        when is_integer(L), is_integer(C) ->
    reset_locations(Form, fun(_) -> {L, C} end);

reset_locations(Form, _Mutate) ->
    Form.

    
%-spec reset_locations( form(), fun( ( type:location(), term() ) -> {type:location(), term()} ), term() ) -> form().

%reset_locations(Form, _Mutate) -> Form.


%%%%% ------------------------------------------------------- %%%%%


transform(Fun, Forms)
        when is_function(Fun, 1)
           , is_list(Forms) ->
    try
        transform_1(Fun, Forms)
    catch
        throw:Reason -> [Reason]
    end.
    

%
% This is Ulf Wiger's parse_trans:plain_transform()
%    

transform_1(_, []) -> [];
transform_1(Fun, [F|Fs]) when is_atom(element(1,F)) ->
    case Fun(F) of
        skip                        -> transform_1(Fun, Fs)
    ;   continue                    ->
            [list_to_tuple(
                transform_1(Fun, tuple_to_list(F)) )
            | transform_1(Fun, Fs)]
            
    ;   {done, NewF}                -> [NewF | Fs]
    ;   {error, Reason}             -> throwerror(Reason, F)
    ;   NewF when is_tuple(NewF)    ->
            [NewF | transform_1(Fun, Fs)]
    end;
    
transform_1(Fun, [L|Fs]) when is_list(L) ->
    [transform_1(Fun, L) | transform_1(Fun, Fs)];
    
transform_1(Fun, [F|Fs]) ->
    [F | transform_1(Fun, Fs)];
    
transform_1(_, F) ->
    F.
    

%%%%% ------------------------------------------------------- %%%%%
    

transform(Fun, Acc, Forms)
        when is_list(Forms)
           , (is_function(Fun, 2) orelse is_function(Fun, 3))  ->
    try
        transform_2(Fun, Acc, Forms)
    catch
        throw:Reason -> [Reason]
    end.
  
    
%
% This is Ulf Wiger's parse_trans:plain_transform() but passing an accum state.
% A lighter version of parse_trans:transform()
%    
    
transform_2(_, Acc, []) ->
    {[], Acc};
    
transform_2(Fun, Acc, [F | Fs]) when is_atom(element(1,F)) ->
    Callback =  if
                    is_function(Fun, 2) -> catch Fun(F, Acc)
                ;   is_function(Fun, 3) -> catch Fun(erl_syntax:type(F), F, Acc)
                end,
                
    case Callback of
        {skip, NewAcc}              -> transform_2(Fun, NewAcc, Fs)
    ;   {continue, NewAcc}          ->
            {Forms2, Acc2} = transform_2(Fun, NewAcc, tuple_to_list(F)),
            {Forms3, Acc3} = transform_2(Fun, Acc2, Fs),
            {[ list_to_tuple(Forms2) | Forms3 ], Acc3}

    ;   {done, NewF, NewAcc}        -> {[NewF | Fs], NewAcc}
    ;   {error, Reason}             -> throwerror(Reason, F)
    ;   {NewF, NewAcc}
                when is_tuple(NewF) ->
            {Forms2, Acc2} = transform_2(Fun, NewAcc, Fs),
            {[NewF | Forms2], Acc2}
    end;
    
    
transform_2(Fun, Acc, [L | Fs]) when is_list(L) ->
    {Forms2, Acc2} = transform_2(Fun, Acc, L),
    {Forms3, Acc3} = transform_2(Fun, Acc2, Fs),
    
    {[Forms2 | Forms3], Acc3};
    
transform_2(Fun, Acc, [F | Fs]) ->
    {Forms2, Acc2} = transform_2(Fun, Acc, Fs),
    {[F | Forms2], Acc2};
    
transform_2(_, Acc, F) ->
    {F, Acc}.
        
    
%%%%% ------------------------------------------------------- %%%%%

