
-module(jsonx).

-export([get/2, get/3]).


%%%%% ------------------------------------------------------- %%%%%


get(Key, Json) ->
	get(Key, Json, undefined).


get({}, _Json, Default) ->
	Default;
	
get(Keys, Json, Default)
		when is_tuple(Keys)  ->
	get(tuple_to_list(Keys), Json, Default);
	
	
get([], Json, _Default) ->
	Json;
	
get([Key | Rest], Json, Default) ->	
	case get_value(Key, Json) of
		undefined	-> Default
	;	AValue		-> get(Rest, AValue, Default)
	end.
	
	
%%%%% ------------------------------------------------------- %%%%%


get_value(Key, Obj)
		when is_list(Key)  ->
    get_value(iolist_to_binary(Key), Obj);
        
        
get_value(Key, {L})
		when is_binary(Key); is_tuple(Key)  ->
    get_value(Key, L);
    
get_value(_Key, null) ->
    undefined;
    
    
get_value(Key, #{} = Map) ->
	maps:get(Key, Map, undefined);
    
get_value(Key, [{_, _} | _T] = PL) ->
    case lists:keyfind(Key, 1, PL) of
        false		-> undefined;
        {_, Return}	-> Return
    end;
    
    
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
    erlang:error({index_for_non_list, {Index, Obj}});
    
get_value(_Key, [_H | _T]) ->
    undefined;    
    
    
get_value({startswith, KeyPrefix}, List)
		when is_binary(KeyPrefix) ->
    Res = lists:filter(fun({K, _}) -> matching_prefix(KeyPrefix, K) end, List),
    case Res of
        [] -> undefined;
        _ -> Res
    end;
    
    
get_value(Key, {from_select, []}) ->
    undefined;
    
get_value(Key, {from_select, List}) ->
    lists:flatten([get_value(Key, L) || L <- List]);    
    
get_value({select, KeyValue}, [_H | _T] = List)
		when is_tuple(KeyValue) orelse KeyValue =:= all  ->
    {from_select, matching_array_elements(KeyValue, List)}.

    
%%%%% ------------------------------------------------------- %%%%%

    
matching_array_elements(all, List) ->
    List;
    
matching_array_elements(CompKey, List) ->
    lists:filter(fun(E) -> matching_element(CompKey, E) end, List).

    
matching_prefix(Prefix, V) ->
    PrefixSize = byte_size(Prefix),
    case V of
        <<Prefix:PrefixSize/binary, _/binary>> -> true;
        _ -> false
    end.

    
matching_element({K, V}, {struct, E}) ->
    Value = as_binary(V),
    case lists:keyfind(as_binary(K), 1, E) of
      false      -> false;
      {_, Value} -> true;
      _          -> false
    end;
    
matching_element({K, V}, {E}) ->
    Value = as_binary(V),
    case lists:keyfind(as_binary(K), 1, E) of
      false      -> false;
      {_, Value} -> true;
      _          -> false
    end;
    
matching_element(Key, E) ->
    erlang:error({error_matching_element, {Key, E}}).
    

    
as_binary(Key) when is_binary(Key) ->
    Key;
as_binary(Key) when is_list(Key) ->
    iolist_to_binary(Key);
as_binary(Key) when is_tuple(Key) ->
    Key;
as_binary(Key) when is_integer(Key) orelse is_atom(Key) ->
    Key.
    