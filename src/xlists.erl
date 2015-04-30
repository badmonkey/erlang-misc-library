-module(xlists).

-export([sorted_member/2, subsets/2, unique/1, drop/2, take/2, foldl/2, foldr/2, filter_fold/3]).



%%%%% ------------------------------------------------------- %%%%%


sorted_member(_X, []) -> false;
sorted_member(X, [X | _Tl]) -> true;
sorted_member(X, [Hd, _Tl]) when X < Hd -> false;
sorted_member(X, [_Hd | Tl]) -> sorted_member(X, Tl).


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

    
%%%%% ------------------------------------------------------- %%%%%


-spec filter_fold( fun((V, Acc) -> {boolean(), Acc}), Acc, [V] ) -> {[V], Acc}.

filter_fold(F, Acc, List) when is_list(List) ->
	filter_fold(F, Acc, [], List).
	

filter_fold(F, Acc, Result, []) ->
	{lists:reverse(Result), Acc};

filter_fold(F, Acc, Result, [Hd | Tl]) ->
	case F(Hd, Acc) of
		{true, Acc1}	-> filter_fold(F, Acc1, [Hd | Result], Tl)
	;	{false, Acc1}	-> filter_fold(F, Acc1, Result, Tl)
	end.

