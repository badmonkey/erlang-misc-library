
-module(property).

-export([ update/3, set_multi/3, append/3, delete/2
        , select/2, select/3, merge/2
        , is_defined/2, is_multivalue/2
        , get_bool/2, get_value/2, get_value/3
        , keys/1, get_all_values/2 ]).


-define(MULTIVALUE_TAG, 'property$multivalue').


%%%%% ------------------------------------------------------- %%%%%
% replace all entries Key by single entry {Key, Value}


-spec update( atom(), term(), type:properties() ) -> type:properties().

update(Key, Value, Prop)
        when is_atom(Key), is_map(Prop) ->
    maps:put(Key, Value, Prop);

update(Key, Value, Prop)
        when is_atom(Key), is_list(Prop) ->
    proplists:delete(Key, Prop) ++ [{Key, Value}].


%%%%% ------------------------------------------------------- %%%%%
% replace all entries Key by multivalue entry or single entry 


-spec set_multi( atom(), list(), type:properties() ) -> type:properties().

set_multi(Key, Values, Prop)
        when is_atom(Key), is_list(Values), is_map(Prop) ->
    case Values of
        [X]     -> update(Key, X, Prop)
    ;   _       -> Prop2 = delete(Key, Prop),
                   set_impl(Key, Values, Prop2)
    end.


set_impl(Key, Values, Prop) when is_map(Prop) ->           
    maps:put(Key, {?MULTIVALUE_TAG, Values}, Prop);

set_impl(Key, Values, Prop) when is_list(Prop) ->
    Prop ++ [{Key, X} || X <- Values].


%%%%% ------------------------------------------------------- %%%%%
% Add new value to Key, if not a multivalue make it a multivalue


-spec append( atom(), term(), type:properties() ) -> type:properties().

append(Key, Value, Prop)
        when is_atom(Key), is_map(Prop) ->
    case maps:get(Key, Prop, undefined) of
        {?MULTIVALUE_TAG, Vals}     ->
            maps:put(Key, {?MULTIVALUE_TAG, Vals ++ [Value]}, Prop)

    ;   undefined                   ->
            maps:put(Key, Value, Prop)

    ;   X                           ->
            maps:put(Key, {?MULTIVALUE_TAG, [X, Value]}, Prop)
    end;

append(Key, Value, Prop)
        when is_atom(Key), is_list(Prop) ->
    Prop ++ [{Key, Value}].


%%%%% ------------------------------------------------------- %%%%%
% delete all entries for Key


-spec delete( atom(), type:properties() ) -> type:properties().

delete(Key, Prop)
        when is_atom(Key), is_map(Prop) ->
    maps:remove(Key, Prop);

delete(Key, Prop)
        when is_atom(Key), is_list(Prop) ->
    proplists:delete(Key, Prop).


%%%%% ------------------------------------------------------- %%%%%


-spec is_defined( atom(), type:properties() ) -> boolean().

is_defined(Key, Prop) when is_map(Prop) ->
    maps:is_key(Key, Prop);

is_defined(Key, Prop) when is_list(Prop) ->
    proplists:is_defined(Key, Prop).


%%%%% ------------------------------------------------------- %%%%%


-spec is_multivalue( atom(), type:properties() ) -> boolean().

is_multivalue(Key, Prop) when is_map(Prop) ->
    case maps:get(Key, Prop, undefined) of
        {?MULTIVALUE_TAG, [_|_]}    -> true
    ;   _                           -> false
    end;                                           

is_multivalue(Key, Prop) when is_list(Prop) ->
    case length( proplists:lookup_all(Key, Prop) ) of
        0   -> false
    ;   1   -> false
    ;   _   -> true
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec get_bool( atom(), type:properties() ) -> boolean().

get_bool(Key, Prop) when is_map(Prop) ->
    get_value(Key, Prop, undefined) =:= true;

get_bool(Key, Prop) when is_list(Prop) ->
    proplists:get_bool(Key, Prop).


%%%%% ------------------------------------------------------- %%%%%
% return Value associated with Key, or first Value of multivalue


get_value(Key, Prop) -> get_value(Key, Prop, undefined).


-spec get_value( atom(), type:properties(), term() ) -> term().

get_value(Key, Prop, Default) when is_map(Prop) ->
    case maps:get(Key, Prop, Default) of
        {?MULTIVALUE_TAG, [Fst | _]}    -> Fst
    ;   X                               -> X
    end;

get_value(Key, Prop, Default) when is_list(Prop) ->
    proplists:get_value(Key, Prop, Default).


%%%%% ------------------------------------------------------- %%%%%


-spec keys( type:properties() ) -> [ atom() ].

keys(Prop) when is_map(Prop) ->
    maps:keys(Prop);

keys(Prop) when is_list(Prop) ->
    proplists:get_keys().
    

%%%%% ------------------------------------------------------- %%%%%
% return all values of a multivalue or single value as list.

-spec get_all_values( atom(), type:properties() ) -> [ term() ].

get_all_values(Key, Prop) when is_map(Prop) ->
    case maps:get(Key, Prop, undefined) of
        {?MULTIVALUE_TAG, Vals} -> Vals
    ;   undefined               -> []
    ;   X                       -> [X]
    end;

get_all_values(Key, Prop) when is_list(Prop) ->
    proplists:get_all_values(Key, Prop).


%%%%% ------------------------------------------------------- %%%%%
% return a subset of a property by keys


select(Keys, Prop) ->
    select(Keys, with, Prop).


-spec select( [atom()], all | with | without, type:properties() ) -> type:value_or_error( type:properties() ).

select(Keys, with, Prop) when is_map(Prop) ->
    xmaps:with(Keys, Prop);

select(Keys, without, Prop) when is_map(Prop) ->
    xmaps:without(Keys, Prop);

select(Keys, all, Prop) when is_map(Prop) ->
    Prop;


select(Keys, with, Prop) when is_list(Prop) ->
    xproplists:with(Keys, Prop);

select(Keys, without, Prop) when is_list(Prop) ->
    xproplists:without(Keys, Prop);

select(Keys, all, Prop) when is_list(Prop) ->
    Prop.


%%%%% ------------------------------------------------------- %%%%%


%merge
merge(A, _) -> A.
