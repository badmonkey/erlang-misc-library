
-module(xlists).
-extends(lists).

-export([ sorted_member/2, sorted_insert/2, subsets/2, unique/1
        , drop/2, take/2, foldl/2, filter_fold/2, filter_fold/3
        , mapx/3, minmax/1, minmax/2, interval/2, shuffle/1, shuffle2/1]).



%%%%% ------------------------------------------------------- %%%%%


-spec sorted_member(T, [T]) -> boolean().

sorted_member(_X, []) -> false;
sorted_member(X, [X | _Tl]) -> true;
sorted_member(X, [Hd, _Tl]) when X < Hd -> false;
sorted_member(X, [_Hd | Tl]) -> sorted_member(X, Tl).


%%%%% ------------------------------------------------------- %%%%%


-spec sorted_insert(T, [T]) -> [T].

sorted_insert(X, []) -> [X];
sorted_insert(X, [Hd | Rest] = L) ->
    case X =< Hd of
        true    -> [ X | L ]
    ;   false   -> [ Hd | sorted_insert(X, Rest) ]
    end.


%%%%% ------------------------------------------------------- %%%%%


% returns list of all sublists of L of size N
-spec subsets( non_neg_integer(), [T] ) -> [[T]].

subsets(_N, []) -> [[]];
subsets(0, _L) -> [[]];
subsets(N, L) when N > length(L) -> [[]];
subsets(N, L) when N == length(L) -> [L];
subsets(N, [Hd | Tl] ) -> [ [Hd | T] || T <- subsets(N - 1, Tl) ] ++ subsets(N, Tl).
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec unique([T]) -> [T].

unique(List) when is_list(List) ->
    sets:to_list( sets:from_list(List) ).


%%%%% ------------------------------------------------------- %%%%%


-spec drop( pos_integer(), [T] ) -> [T].

drop(N, List) when is_list(List) ->
    lists:nthtail(N, List).

    
-spec take( pos_integer(), [T] ) -> [T].
    
take(N, List) when is_list(List) ->
    lists:sublist(List, N).

    
%%%%% ------------------------------------------------------- %%%%%


-spec foldl( fun((T, T) -> T), nonempty_list(T) ) -> T.

foldl(F, [Hd | Tl]) ->
    lists:foldl(F, Hd, Tl).

    
%%%%% ------------------------------------------------------- %%%%%


-spec filter_fold( fun((V, A) -> {boolean(), A}), {A, [V]} ) -> {[V], A}.

filter_fold(F, {Acc, List}) when is_list(List) ->
    filter_fold(F, Acc, [], List).


-spec filter_fold( fun((V, A) -> {boolean(), A}), A, [V] ) -> {[V], A}.

filter_fold(F, Acc, List) when is_list(List) ->
    filter_fold(F, Acc, [], List).
    

-spec filter_fold( fun((V, A) -> {boolean(), A}), A, [V], [V] ) -> {[V], A}.

filter_fold(_, Acc, Result, []) ->
    {lists:reverse(Result), Acc};

filter_fold(F, Acc, Result, [Hd | Tl]) ->
    case F(Hd, Acc) of
        {true, Acc1}    -> filter_fold(F, Acc1, [Hd | Result], Tl)
    ;   {false, Acc1}   -> filter_fold(F, Acc1, Result, Tl)
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec mapx( fun((...) -> R), list(), list() ) -> [R].

mapx(_Fun, _, []) -> [];
    
mapx(Fun, Args, [Head | Tail]) ->
    [ apply(Fun, [Head | Args]) | mapx(Fun, Args, Tail) ].


%%%%% ------------------------------------------------------- %%%%%


minmax(L) when is_list(L) ->
    minmax(fun(X,Y) -> X < Y end, L).
    

minmax(_, []) -> undefined;
minmax(F, [H | Rest]) -> minmax(F, Rest, {H, H}).


-spec minmax( fun((T, T) -> boolean()), [T], {T, T} ) -> {T, T}.

minmax(_, [], Acc) -> Acc;

minmax(F, [H | Rest], {Min, Max} = Res) ->
    Acc =   case F(H, Min) of
                true    -> {H, Max}
            ;   false   ->
                    case F(Max, H) of
                        true    -> {Min, H}
                    ;   false   -> Res
                    end
            end,
    minmax(F, Rest, Acc).


%%%%% ------------------------------------------------------- %%%%%


-spec interval( integer(), integer() ) -> [integer()].

interval(X, X) -> [X];
interval(Fir, Sec) when Fir < Sec  -> lists:seq(Fir, Sec);
interval(Fir, Sec) -> lists:seq(Fir, Sec, -1).
	

%%%%% ------------------------------------------------------- %%%%%


-spec shuffle( [T] ) -> [T].

shuffle(L) when is_list(L) -> shuffle(L, []).


shuffle([], Acc) -> Acc;
shuffle([X], Acc) -> [X | Acc];
shuffle(List, Acc) ->
	N = length(List),
	{Leading, [H, T]} = lists:split(random:uniform(N) - 1, List),
	{Leading, H, T, Leading ++ T}.
%	shuffle( Leading ++ T, [H | Acc]).
	

shuffle2(L) ->
	shuffle2(length(L), L, [], [], 0).


shuffle2(0, [], [], Acc, Steps) -> {Acc, Steps};
shuffle2(N, [], Over, Acc, Steps) -> shuffle2(N, Over, [], Acc, Steps);
shuffle2(1, [X], [], Acc, Steps) -> shuffle2(0, [], [], [X | Acc], Steps + 1);
shuffle2(N, [H | T], Over, Acc, Steps) ->
	case random:uniform(N) of
		1	-> shuffle2(N - 1, T, Over, [H | Acc], Steps + 1)
	;	_ 	-> shuffle2(N, T, [H | Over], Acc, Steps + 1)
	end.
		




