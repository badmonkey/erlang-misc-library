%%
%% jsonpath - json data retrieval and updates via
%%            javascript-like notation
%%
%% Copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% @doc Fast javascript-like "path" notation for querying and updating JSON
%% @author Gene Stevens <gene@triplenexus.org>
%% @copyright (c) 2012 Gene Stevens.  All Rights Reserved.
%%
%% Modified by Michael Fagan
%%

-module(jsonx_replace).


-export([search/2, replace/3]).
-export([parse_path/1]).

    
%%%%% ------------------------------------------------------- %%%%%    
    

search(Path, Data) when is_binary(Data) ->
    search(Path, jiffy:decode(Data));
    
search(Path, Data) ->
    search_data(parse_path(Path), Data).

    
%%%%% ------------------------------------------------------- %%%%%    

    
search_data([], Data) ->
    Data;
    
search_data([Head|Tail], Data) ->
    case Head of
        <<>> ->
            search_data(Tail, Data);
        _Other ->
            case Data of 
                {_Tuple} ->
                    search_tuple([Head|Tail], Data);
                _List ->
                    search_list([Head|Tail], Data)
            end
    end.

    
    
search_list([Head|Tail], List) ->
    try 
        Index = list_to_integer(binary_to_list(Head)) + 1,
        case (Index > length(List)) of
            true ->
                undefined;
            false ->
                search_data(Tail, lists:nth(Index, List))
        end
    catch
        _:_ -> 
            %?DEBUG("that wasn't an integer",[]),
            undefined
    end.

    
    
search_tuple([Head|Tail], Tuple) ->
    {TuplePayload} = Tuple,
    case lists:keyfind(Head, 1, TuplePayload) of
        false ->
            undefined;
        {Head,Value} ->
            search_data(Tail, Value)
    end.
    
    
%%%%% ------------------------------------------------------- %%%%%    
    
    
replace(Path, Replace, Data) when is_binary(Data) ->
    replace(Path, Replace, jiffy:decode(Data));
    
replace(Path, Replace, Data) ->
    replace_data(parse_path(Path), Replace, Data).

    
    
replace_data([SearchHead|SearchTail], Replace, Structure) ->
    case Structure of 
        {TupleList} ->
            { replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) };
        List ->
            replace_list([SearchHead|SearchTail], Replace, List)
    end.

    
    
replace_list([SearchHead|SearchTail], Replace, List) ->
    try 
        Index = list_to_integer(binary_to_list(SearchHead)) + 1,
        case (Index > length(List)) of
            true ->
                undefined;
            false ->
                replace_list([Index|SearchTail], Replace, List, 1, [])
        end
    catch
        _:_ ->
            undefined
    end.
    
replace_list([_SearchHead|_SearchTail], _Replace, [], _Count, Accum) ->
    lists:reverse(Accum);
    
replace_list([SearchHead|SearchTail], Replace, [Head|Tail], Count, Accum) ->
    Data = case SearchHead of 
        Count ->
            case SearchTail of
                [] ->
                    Replace;
                _SearchTail ->
                    replace_data(SearchTail, Replace, Head)
            end;
        _SearchHead ->
            Head
    end,
    replace_list([SearchHead|SearchTail], Replace, Tail, Count+1, [Data|Accum]).


replace_tuple_list([SearchHead|SearchTail], Replace, TupleList) ->
    replace_tuple_list([SearchHead|SearchTail], Replace, TupleList, []).
    
replace_tuple_list([_SearchHead|_SearchTail], _Replace, [], Accum) ->
    lists:reverse(Accum);
    
replace_tuple_list([SearchHead|SearchTail], Replace, [Head|Tail], Accum) ->
    Data = case Head of
        {SearchHead, Value} ->
            case SearchTail of 
                [] ->
                    {SearchHead, Replace};
                _SearchTail ->
                    {SearchHead, replace_data(SearchTail, Replace, Value) }
            end;
        _Other ->
            Head
    end,
    replace_tuple_list([SearchHead|SearchTail], Replace, Tail, [Data|Accum]).

    
%%%%% ------------------------------------------------------- %%%%%    

    
parse_path(Path) ->
    Split = binary:split(Path, [<<".">>,<<"[">>,<<"]">>], [global]),
    lists:filter(fun(X) -> X =/= <<>> end, Split).

