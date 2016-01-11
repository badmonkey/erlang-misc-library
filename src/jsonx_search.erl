
-module(jsonx_search).

-export([ search/3, parse_path/1 ]).



-type select_result()   :: { from_select, [jsonx:js_object() | jsonx:js_array()] }.


%%%%% ------------------------------------------------------- %%%%%


-spec search( jsonx:key_path(), jsonx:js_object(), jsonx:js_term() | jsthrow ) -> jsonx:js_term() | type:exception().


search(Keys, Json, jsthrow) ->
    case search(Keys, Json, undefined) of
        undefined   -> throw({error, {non_exists, Keys}})
    ;   X           -> X
    end;
    
    
search([], Json, _Default) ->
    Json;
    
search([Key | Rest], Json, Default) -> 
    case get_value(Key, Json) of
        undefined   -> Default
    ;   AValue      -> search(Rest, AValue, Default)
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec get_value( jsonx:key(), jsonx:js_object() | jsonx:js_array() | select_result() ) -> jsonx:js_term() | type:exception().

% convert key string() | atom()
get_value(Key, Obj)
        when is_list(Key)  ->
    get_value( iolist_to_binary(Key), Obj);
    
get_value(Key, Obj)
        when is_atom(Key)  ->
    get_value( atom_to_binary(Key, utf8), Obj);
   
    
% js_null()    
get_value(_Key, null) ->
    null;
    

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
    
    
%%%%% ------------------------------------------------------- %%%%%
    

parse_path(Path) ->
    Split = binary:split(Path, [<<".">>,<<"[">>,<<"]">>], [global]),
    xlists:mutate(
          fun(<<>>)         -> remove
          ;  (<<"*">>)      -> {select, all}
          ;  (<<"first">>)  -> first
          ;  (<<"last">>)   -> last
% @todo   handle foo[name = value]
          ;  (X)            ->
                case text:is_number(X) of
                    true    -> xerlang:binary_to_integer(X)
                ;   false   -> X
                end
          end
        , Split).


