
-module(jsonx).

-export([ get/2, get/3, compare/3, compare/4
        , as_boolean/1, as_integer/1]).


%%%%% ------------------------------------------------------- %%%%%


-type js_string()       :: binary().
-type js_null()         :: null.
-type js_number()       :: integer() | float().
-type js_boolean()      :: 'true' | 'false'.
-type js_array()        :: [js_term()].
-type js_plist()        :: [{js_string(), js_term()}].
-type js_object()       :: { js_plist() } | #{ js_string() => js_term() }.
-type js_value()        :: js_string() | js_number() | js_null() | js_boolean().
-type js_term()         :: js_value() | js_array() | js_object() | undefined.


-type array_select()    :: { select
                           , all | {js_string(), js_value()}
                           }.
                           
-type select_result()   :: { from_select, [js_object() | js_array()] }.

-type key()             :: binary() | integer() | first | last
                         | array_select() | string() | atom().

-type key_path()        :: tuple() | [ key() ].


-export_type([ js_term/0, js_value/0, js_object/0, js_plist/0, js_array/0
             , js_boolean/0, js_number/0, js_null/0, js_string/0]).
-export_type([key_path/0, key/0, array_select/0]).


%%%%% ------------------------------------------------------- %%%%%


get(Key, Json) ->
    get(Key, Json, undefined).


-spec get( key_path(), js_object(), js_term() | jsthrow ) -> js_term() | type:exception().

get({}, _Json, jsthrow) ->
    throw({error, empty_key});

get({}, _Json, Default) ->
    Default;
    
get(Keys, Json, Default)
        when is_tuple(Keys)  ->
    get(tuple_to_list(Keys), Json, Default);
    
    
get(Keys, Json, jsthrow) ->
    case get(Keys, Json, undefined) of
        undefined   -> throw({error, {non_exists, Keys}})
    ;   X           -> X
    end;
    
    
get([], Json, _Default) ->
    Json;
    
get([Key | Rest], Json, Default) -> 
    case get_value(Key, Json) of
        undefined   -> Default
    ;   AValue      -> get(Rest, AValue, Default)
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec get_value( key(), js_object() | js_array() | select_result() ) -> js_term() | type:exception().

% convert key string() | atom()
get_value(Key, Obj)
        when is_list(Key)  ->
    get_value( iolist_to_binary(Key), Obj);
    
get_value(Key, Obj)
        when is_atom(Key)  ->
    get_value( atom_to_binary(Key, utf8), Obj);
   
    
% js_null()    
get_value(_Key, null) ->
    undefined;
    

% js_object() | js_plist()
get_value(Key, #{} = Map)
        when is_binary(Key)  ->
    maps:get(Key, Map, undefined);
    
get_value(Key, {L})
        when  is_binary(Key)
            , is_list(L)  ->
    get_value(Key, L);    
    
get_value(Key, [{_, _} | _T] = PL)
        when is_binary(Key)  ->
    case lists:keyfind(Key, 1, PL) of
        false       -> undefined;
        {_, Return} -> Return
    end;
    
    
% js_array()    
get_value(_Key, []) ->
    undefined;

get_value(first, [H | _T]) ->
    H;
    
get_value(last, [_H | _T] = List) ->
    lists:last(List);
    
get_value(Index, [_H | _T] = List)
        when is_integer(Index) ->
    lists:nth(Index, List);
    
get_value(Index, Obj)
        when is_integer(Index)  ->
    throw({error, {index_for_non_list, Index, Obj}});


% array_select()
get_value(_Key, {from_select, []}) ->
    undefined;
    
get_value(Key, {from_select, List})
        when is_list(List)  ->
    lists:flatten([get_value(Key, L) || L <- List]);    
    
get_value({select, KeyValue}, [_H | _T] = List)
        when is_tuple(KeyValue) orelse KeyValue =:= all  ->
    {from_select, matching_array_elements(KeyValue, List)};
    
get_value({select, KeyValue}, #{} = Map)
        when is_tuple(KeyValue) orelse KeyValue =:= all  ->
    {from_select, matching_array_elements(KeyValue, maps:to_list(Map))};    
    

get_value(_Key, [_H | _T]) ->
    undefined.
    
    
% catchall  error?    
%get_value(_Key, _Obj) ->
%    undefined.


%%%%% ------------------------------------------------------- %%%%%


compare(Key1, Key2, Json) ->
    compare(Key1, Key2, Json, undefined).
    
    
-spec compare( key_path(), key_path(), js_object(), undefined | jsthrow ) -> boolean() | type:exception().

compare(Key1, Key2, Json, Default) ->
    Value1 = get(Key1, Json, Default),
    Value2 = get(Key2, Json, Default),
    Value1 =:= Value2.


%%%%% ------------------------------------------------------- %%%%%


-spec as_boolean( binary() | string() | integer() ) -> boolean() | type:exception().

as_boolean(B) when is_binary(B) ->
    as_boolean( xerlang:binary_to_integer(B) );

as_boolean("false") -> false;
as_boolean("true") -> true;
as_boolean("no") -> false;
as_boolean("yes") -> true;
as_boolean(L) when is_list(L) ->
    as_boolean( list_to_integer(L) );
    
as_boolean(0) -> false;
as_boolean(1) -> true;

as_boolean(X) -> throw({error, {not_a_bool, X}}).


%%%%% ------------------------------------------------------- %%%%%


-spec as_integer( binary() | string() | integer() ) -> integer().

as_integer(B) when is_binary(B) ->
    as_integer( xerlang:binary_to_integer(B) );

as_integer(L) when is_list(L) ->
    as_integer( list_to_integer(L) );
    
as_integer(X) when is_integer(X) -> X.

    
%%%%% ------------------------------------------------------- %%%%%

    
matching_array_elements(all, List) ->
    List;
    
matching_array_elements(CompKey, List) ->
    lists:filter(fun(E) -> matching_element(CompKey, E) end, List).
    
    
matching_element({K, V}, E)
        when is_list(E)  ->
    Value = as_binary(V),
    case lists:keyfind(as_binary(K), 1, E) of
      false      -> false;
      {_, Value} -> true;
      _          -> false
    end;
    
matching_element(CompKey, {E})
        when is_list(E)  ->
    matching_element(CompKey, E);
    
matching_element(Key, E) ->
    throw({error, {error_matching_element, Key, E}}).
    

    
as_binary(Key) when is_binary(Key) ->
    Key;
as_binary(Key) when is_list(Key) ->
    iolist_to_binary(Key);
as_binary(Key) when is_tuple(Key) ->
    Key;
as_binary(Key) when is_integer(Key) orelse is_atom(Key) ->
    Key.
    