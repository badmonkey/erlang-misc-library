
-module(typed_property).


-export([new/0, get_raw_value/2, get_value/3, get_value/4, merge/2, merge/3, expand/1]).

-export_type([property_type/0, property_name/0, property/0]).


%%%%% ------------------------------------------------------- %%%%%


-type property_name() :: atom() | string().
-type property_type() :: atom | bool | integer | float | ipaddress | string | list | tuple | path.

-type property() :: [{ atom(), internal_type(), term() }].
-type internal_type() :: property_type() | group | variable | string_raw | list_raw | path_raw.


%%%%% ------------------------------------------------------- %%%%%


-spec new() -> property().

new() -> [].
 

%%%%% ------------------------------------------------------- %%%%%


get_value(Prop, Name, Type) -> get_value(Prop, Name, Type, undefined).


-spec get_value( property(), property_name(), property_type(), term() ) -> term().

get_value(Prop, Name, Type, Default)
        when is_atom(Name)  ->
    case lists:keyfind(Name, 1, Prop) of
        false               -> Default
    ;   {Name, Type, Value} -> Value    % Name and Type match
    ;   {Name, _, _}        -> Default  % Type doesn't match
    end;

get_value(Prop, Name, Type, Default) ->
    KeyList = make_key_list(Name),
    case find_group(Prop, KeyList) of
        {[], _}             -> Default
    ;   {[Key], TopProp}    -> get_value(TopProp, Key, Type, Default)
    ;   {_, _}              -> Default
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec get_raw_value( property(), property_name() ) -> { internal_type(), term() } | undefined.

get_raw_value(Prop, Name)
        when is_atom(Name)  ->
    case lists:keyfind(Name, 1, Prop) of
        false               -> undefined
    ;   {Name, Type, Value} -> {Type, Value}
    end;
    
get_raw_value(Prop, Name) ->
    KeyList = make_key_list(Name),
    find_key(Prop, KeyList).

    
%%%%% ------------------------------------------------------- %%%%%


merge(First, Second) -> merge(First, Second, keepgroups).


-spec merge( property(), property(), override | keepgroups ) -> property().

merge(First, Second, Mode) ->
    SortedFirst = lists:keysort(1, First),
    SortedSecond = lists:keysort(1, Second),
    merge(SortedFirst, SortedSecond, [], Mode).
    

merge([], Second, Acc, _Mode) ->
    Acc ++ Second;

merge(First, [], Acc, _Mode) ->
    Acc ++ First;
    
merge( [FHd | FRest] = First
      , [SHd | SRest] = Second
      , Acc, Mode)  ->
    case {FHd, SHd, Mode} of
        {{Name, group, Val1}, {Name, group, Val2}, _}   ->
            NewValue = merge(Val1, Val2, Mode),
            merge(FRest, SRest, Acc ++ [{Name, group, NewValue}], Mode)
            
    ;   {{Name, _, _}, {Name, group, _}, _}             ->
            merge(FRest, SRest, Acc ++ [SHd], Mode)
    
    ;   {{Name, group, _}, {Name, _, _}, override}      ->
            merge(FRest, SRest, Acc ++ [SHd], Mode)
            
    ;   {{Name, group, _}, {Name, _, _}, keepgroups}    ->
            merge(FRest, SRest, Acc ++ [FHd], Mode)
    
    ;   {{Name, _, _}, {Name, _, _}, _}                 ->
            merge(FRest, SRest, Acc ++ [SHd], Mode)
        
    ;   {{Nm1, _, _}, {Nm2, _, _}, _}  when Nm1 < Nm2   ->
            merge(FRest, Second, Acc ++ [FHd], Mode)
            
    ;   {{Nm1, _, _}, {Nm2, _, _}, _}  when Nm2 < Nm1   ->
            merge(First, SRest, Acc ++ [SHd], Mode)
    
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec expand( property() ) -> property().

expand(Prop) ->
    [ expand_property(Prop, Prop, X) || X <- Prop ].
    

expand_property(TopProp, _, {Name, group, Value}) ->
    {Name, group, [ expand_property(TopProp, Value, X) || X <- Value ]};

    
expand_property(TopProp, Prop, {_Name, variable, Value}) ->
    % TODO
    get_raw_value(TopProp, Value),
    get_raw_value(Prop, Value),
    undefined;


expand_property(TopProp, Prop, {Name, string_raw, Value}) ->
    NewVList = expand_string_list(TopProp, Prop, Value),
    {Name, string, lists:concat(NewVList)};


expand_property(TopProp, Prop, {Name, list_raw, Value}) ->
    NewVList = expand_value_list(TopProp, Prop, Value),
    {Name, list, NewVList};

    
expand_property(TopProp, Prop, {Name, path_raw, Value}) ->
    NewVList = expand_path_list(TopProp, Prop, Value),
    {Name, path, lists:concat(NewVList)};
    

expand_property(TopProp, Prop, {Name, tuple, Value} = P) ->
    case is_tuple(Value) of
        true    -> P
    ;   false   ->
            NewVList = expand_value_list(TopProp, Prop, Value),
            {Name, tuple, erlang:list_to_tuple(NewVList)}
    end;

    
expand_property(_TopProp, _Prop, P) ->
    P.
    

%%%%% ------------------------------------------------------- %%%%%


expand_value_list(TopProp, Prop, VList) ->
    [ naked_value(expand_property(TopProp, Prop, {nil, T, V})) || {T, V} <- VList ].
    

expand_path_list(TopProp, Prop, PList) ->
    [ expand_path_item(TopProp, Prop, P) || P <- PList ].   

    
expand_path_item(TopProp, Prop, {anchor, N}) ->
    AnchorName = lists:concat(["'$ystem'.", N]),
    naked_value(expand_property(TopProp, Prop, {nil, variable, AnchorName}));
    
expand_path_item(TopProp, Prop, {variable, N}) ->
    naked_value(expand_property(TopProp, Prop, {nil, variable, N}));
    
expand_path_item(_, _, P) -> P.


expand_string_list(TopProp, Prop, SList) ->
    [ expand_string_item(TopProp, Prop, S) || S <- SList ]. 

    
expand_string_item(TopProp, Prop, {variable, N}) ->
    naked_value(expand_property(TopProp, Prop, {nil, variable, N}));
    
expand_string_item(_, _, S) -> S.   


naked_value({_Name, group, _Value}) -> undefined;
naked_value({_Name, _Type, Value}) -> Value.


%%%%% ------------------------------------------------------- %%%%%


-spec find_group( property(), [atom()] ) -> { [atom()], property() }.

find_group(Prop, []) ->
    {[], Prop};
    
find_group(Prop, [Hd | Rest] = Key) ->
    case lists:keyfind(Hd, 1, Prop) of
        false                   -> {Key, Prop}
    ;   {Hd, group, NewProp}    -> find_group(NewProp, Rest)
    ;   {Hd, _, _}              -> {Key, Prop}  % we found a matching key, but it's not a group
    end.
    

%%%%% ------------------------------------------------------- %%%%%

    
-spec find_key( property(), [atom()] ) -> { internal_type(), term() } | undefined.

find_key(_, []) -> undefined;
     
find_key(Prop, [Hd]) ->
    case lists:keyfind(Hd, 1, Prop) of
        false                   -> undefined
    ;   {Hd, group, NewProp}    -> {group, NewProp}
    ;   {Hd, Type, Value}       -> {Type, Value}
    end;
    
find_key(Prop, [Hd | Rest]) ->
    case lists:keyfind(Hd, 1, Prop) of
        false                   -> undefined
    ;   {Hd, group, NewProp}    -> find_key(NewProp, Rest)
    ;   {Hd, _, _}              -> undefined
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec make_key_list( string() ) -> [atom()].

make_key_list(Name) ->
    [ erlang:list_to_atom(X) || X <- string:tokens(Name, ".") ].
    

%%%%% ------------------------------------------------------- %%%%%

    
%get_as_atom
%get_as_bool
%get_as_integer
%get_as_float
%get_as_ipaddress
%get_as_string
%get_as_list
%get_as_tuple
%get_as_path

