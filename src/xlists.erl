-module(xlists).

-export([find/2, sorted_find/2, subsets/2, unique/1, drop/2, take/2]).


%%%%% ------------------------------------------------------- %%%%%

    
find(_X, []) -> false;
find(X, [X | _Tl]) -> true;
find(X, [_Hd | Tl]) -> find(X, Tl).


%%%%% ------------------------------------------------------- %%%%%


sorted_find(_X, []) -> false;
sorted_find(X, [X | _Tl]) -> true;
sorted_find(X, [Hd, _Tl]) when X < Hd -> false;
sorted_find(X, [_Hd | Tl]) -> sorted_find(X, Tl).


%%%%% ------------------------------------------------------- %%%%%


% returns list of all sublists of L of size N
subsets(_N, []) -> [[]];
subsets(0, _L) -> [[]];
subsets(N, L) when N > length(L) -> [[]];
subsets(N, L) when N == length(L) -> [L];
subsets(N, [Hd | Tl] ) -> [ [Hd | T] || T <- subsets(N - 1, Tl) ] ++ subsets(N, Tl).
    
    
%%%%% ------------------------------------------------------- %%%%%


unique(List) when is_list(List) ->
    sets:to_list( sets:from_list(List) ).


%%%%% ------------------------------------------------------- %%%%%


drop(N, List) when is_list(List) ->
    lists:nthtail(N, List).

take(N, List) when is_list(List) ->
    lists:sublist(List, N).

    
%%%%% ------------------------------------------------------- %%%%%


foldl(F, [Hd | Tl]) ->
    lists:foldl(F, Hd, Tl).

foldr(F, [Hd | Tl]) ->
    lists:foldr(F, Hd, Tl).

