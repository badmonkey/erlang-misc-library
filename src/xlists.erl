
-module(xlists).
-extends(lists).

-export([ sorted_member/2, sorted_insert/2, subsets/2, unique/1
        , drop/2, take/2, foldl/2, filter_fold/2, filter_fold/3
        , mapx/3, minmax/1, minmax/2, interval/2, mutate/2
        , randmerge/2, shuffle/1]).



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

minmax(F, [H | Rest], {Min, Max} = Res)
        when is_function(F,2)  ->
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

             
-spec mutate( fun((V) -> remove | V ) , [V] ) -> [V].

mutate(Pred, List)
        when is_function(Pred,1), is_list(List)  ->
    lists:reverse(
        lists:foldl(
              fun(Xin, Acc) ->
                case Pred(Xin) of
                    remove  -> Acc
                ;   Xout    -> [Xout | Acc]
                end
              end
            , []
            , List) ).

    
%%%%% ------------------------------------------------------- %%%%%

% The function "randmerge(L,R)" should be fair - that is, it should
% produce all interleavings of L and R with equal likelihood, but this
% is not true with the coin-flip version.

% To see why, consider computing randmerge([1,3], [2,4])
% At each step, randmerge can choose the left list or the right list.
% Therefore the choices are:

% L L - 1,3,2,4
% L R L - 1,2,3,4
% L R R - 1,2,4,3
% R L L - 2,1,3,4
% R L R - 2,1,4,3
% R R - 2,4,1,3

% (note, after "L L" the left list is empty so there is no further random choice.
%  similarly for "R R")

% When using a coin-flip, the top and bottom cases both have probability
% 1/4. All the other cases have probability 1/8. Therefore coin-flip randmerge is
% not fair.


-spec randmerge( [T], [T] ) -> [T].

randmerge(L1, L2)
    when  is_list(L1)
        , is_list(L2)  ->
    randmerge(L1, length(L1), L2, length(L2)).


randmerge([], 0, L2, _) -> L2;
randmerge(L1, _, [], 0) -> L1;
   
randmerge(L1, L1_len, L2, L2_len) ->
    case random:uniform(L1_len + L2_len) of
        N when N =< L1_len  ->
            [hd(L1) | randmerge(tl(L1), L1_len - 1, L2, L2_len)]
            
    ;   _                   ->
            [hd(L2) | randmerge(L1, L1_len, tl(L2), L2_len - 1)]
    end.
    

%%%%% ------------------------------------------------------- %%%%%


-spec shuffle( [T] ) -> [T].

shuffle(L) when is_list(L) ->
    shuffle(L, length(L)).
    

shuffle(L, Len) when Len < 2  -> L;

shuffle(L, Len) ->
    L1_len = Len div 2,
    L2_len = Len - L1_len,
    
    {L1, L2} = lists:split(L1_len, L),
    
    randmerge( shuffle(L1, L1_len), L1_len
             , shuffle(L2, L2_len), L2_len ).

