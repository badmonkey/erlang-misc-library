
-module(xlists).

-export([ sorted_member/2, sorted_insert/2, subsets/2, unique/1
        , drop/2, take/2, foldl1/2, foldr1/2
        , filterfold/2, filterfold/3
        , mapfoldl/2, mapfoldr/2
        , mapapply/3, minmax/1, minmax/2, interval/2, mutate/2
        , randmerge/2, shuffle/1
        , keypartition/3, keywith/3, keywithout/3
        , select/3, prependall/2, zigzagI/2, zigzagU/2, unzagzig/1
        , zipI/2, zip3I/3, zipwithI/3, zipwith3I/4 ]).



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


-spec keypartition( list(), type:ordinal(), [tuple()] ) -> {[tuple()], [tuple()]}.

keypartition(Ks, N, Tuples) when is_list(Ks), is_list(Tuples) ->
    keypartition(Ks, N, Tuples, []).


keypartition([], _, Tuples, With) ->
    {With, Tuples};

keypartition([Hd | Rest] = Keys, N, Tuples, With) ->
    case lists:keytake(Hd, N, Tuples) of
        false                   -> keypartition(Rest, N, Tuples, With)
    ;   {value, X, TupleRest}   -> keypartition(Keys, N, TupleRest, [X | With])
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec keywith( list(), type:ordinal(), [tuple()] ) -> [tuple()].

keywith(Ks, N, Tuples) when is_list(Ks), is_list(Tuples) ->
    {With, _} = keypartition(Ks, N, Tuples).


%%%%% ------------------------------------------------------- %%%%%


-spec keywithout( list(), type:ordinal(), [tuple()] ) -> [tuple()].

keywithout([], _, Tuples) -> Tuples;

keywithout([Hd | Rest] = Keys, N, Tuples)
        when is_list(Keys), is_list(Tuples) ->
    case lists:keytake(Hd, N, Tuples) of
        false                   -> keywithout(Rest, N, Tuples)
    ;   {value, X, TupleRest}   -> keywithout(Keys, N, TupleRest)
    end.



%%%%% ------------------------------------------------------- %%%%%


-spec drop( type:cardinal(), [T] ) -> [T].

drop(0, List) when is_list(List) -> List;
drop(N, List) when is_list(List) ->
    lists:nthtail(N, List).

    
-spec take( type:cardinal(), [T] ) -> [T].
    
take(0, _)                       -> [];
take(N, List) when is_list(List) ->
    lists:sublist(List, N).

    
%%%%% ------------------------------------------------------- %%%%%
    

%drop_every(N, L)
%take_every(N, L)
    
    
%%%%% ------------------------------------------------------- %%%%%


% unfoldr
% scanl/scanr scanl1/scanr1


%%%%% ------------------------------------------------------- %%%%%


-spec foldl1( type:transform2f(T), nonempty_list(T) ) -> T.

foldl1(_, [Hd])      -> Hd;

foldl1(F, [Hd | Tl]) ->
    lists:foldl(F, Hd, Tl).

    
%%%%% ------------------------------------------------------- %%%%%


-spec foldr1( type:transform2f(T), nonempty_list(T) ) -> T.

foldr1(_, [Hd])      -> Hd;

foldr1(F, [Hd | Tl]) ->
    lists:foldr(F, Hd, Tl). % @todo fix should be (F, last(), droplast())
    
    
%%%%% ------------------------------------------------------- %%%%%


-spec filterfold( type:filterfoldf(T, Acc), type:accumulation(T, Acc) ) -> type:accumulation(T, Acc).

filterfold(F, {List, Acc}) when is_list(List) ->
    filterfold_1(F, Acc, [], List).


-spec filterfold( type:filterfoldf(T, Acc), Acc, [T] ) -> type:accumulation(T, Acc).

filterfold(F, Acc, List) when is_list(List) ->
    filterfold_1(F, Acc, [], List).
    

    
-spec filterfold_1( type:filterfoldf(T, Acc), Acc, [T], [T] ) -> type:accumulation(T, Acc).

filterfold_1(_, Acc, Result, []) ->
    { lists:reverse(Result), Acc };

filterfold_1(F, Acc, Result, [Hd | Tl]) ->
    case F(Hd, Acc) of
        {true, Acc1}    -> filterfold_1(F, Acc1, [Hd | Result], Tl)
    ;   {false, Acc1}   -> filterfold_1(F, Acc1, Result, Tl)
    end.


%%%%% ------------------------------------------------------- %%%%%
    

-spec mapfoldl( type:mapfoldf(T, T2, Acc), type:reduction(T, Acc) ) -> type:reduction(T2, Acc).

mapfoldl(F, {List, Acc}) -> lists:mapfoldl(F, Acc, List).


-spec mapfoldr( type:mapfoldf(T, T2, Acc), type:reduction(T, Acc) ) -> type:reduction(T2, Acc).

mapfoldr(F, {List, Acc}) -> lists:mapfoldr(F, Acc, List).


%%%%% ------------------------------------------------------- %%%%%


% mapapply(F, [A1, A2, ...], [V1, V2, ...])
% returns [ F(V1, A1, A2, ...), F(V2, A1, A2, ...), ...]

-spec mapapply( fun((...) -> R), list(), list() ) -> [R].

mapapply(_Fun, _, []) -> [];
    
mapapply(Fun, Args, [Head | Tail]) ->
    [ apply(Fun, [Head | Args]) | mapapply(Fun, Args, Tail) ].


%%%%% ------------------------------------------------------- %%%%%


minmax(L) when is_list(L) ->
    minmax(fun(X,Y) -> X < Y end, L).
    

-spec minmax( type:predicate(T,T), [T] ) -> {T, T}.
    
minmax(_, []) -> undefined;
minmax(F, [H | Rest]) -> minmax_1(F, Rest, {H, H}).


-spec minmax_1( type:predicate(T,T), [T], {T, T} ) -> {T, T}.

minmax_1(_, [], Acc) -> Acc;

minmax_1(F, [H | Rest], {Min, Max} = Res)
        when is_function(F,2)  ->
    Acc =   case F(H, Min) of
                true    -> {H, Max}
            ;   false   ->
                    case F(Max, H) of
                        true    -> {Min, H}
                    ;   false   -> Res
                    end
            end,
    minmax_1(F, Rest, Acc).


%%%%% ------------------------------------------------------- %%%%%


-spec interval( integer(), integer() ) -> [integer()].

interval(X, X) -> [X];
interval(Fir, Sec) when Fir < Sec  -> lists:seq(Fir, Sec);
interval(Fir, Sec) -> lists:seq(Fir, Sec, -1).


%%%%% ------------------------------------------------------- %%%%%

             
-spec mutate( type:mutatef(V), [V] ) -> [V].

mutate(Pred, List)
        when is_function(Pred,1), is_list(List)  ->
    lists:reverse(
        lists:foldl(
              fun(Xin, Acc) ->
                case Pred(Xin) of
                    false           -> Acc
                ;   true            -> [Xin | Acc]
                ;   {true, Xout}    -> [Xout | Acc]
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
    case rand:uniform(L1_len + L2_len) of
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

             
%%%%% ------------------------------------------------------- %%%%%


-spec select( type:predicate(T), T, {[T], [T]} ) -> {[T], [T]}.

select(P, X, {Tlist, Flist}) ->
    case P(X) of
        true    -> {[X | Tlist], Flist}
    ;   false   -> {Tlist, [X | Flist]}
    end.
             
             
%%%%% ------------------------------------------------------- %%%%%


prependall(_, []) -> [];
prependall(Sep, [Hd|Tl]) -> [Sep, Hd | prependall(Sep, Tl)].


%%%%% ------------------------------------------------------- %%%%%


zigzagI([], L2) -> [];
zigzagI(L1, []) -> [];
zigzagI([Hd1 | Tl1], [Hd2 | Tl2]) ->
    [ Hd1, Hd2 | zigzagI(Tl1, Tl2) ].

    
zigzagU([], L2) -> L2;
zigzagU(L1, []) -> L1;
zigzagU([Hd1 | Tl1], [Hd2 | Tl2]) ->
    [ Hd1, Hd2 | zigzagU(Tl1, Tl2) ].


%%%%% ------------------------------------------------------- %%%%%


-spec unzagzig([T]) -> {[T], [T]}.

unzagzig(L) when is_list(L) ->
    unzagzig_1({[], []}, L).
    
    
unzagzig_1(Acc, []) -> Acc;    
unzagzig_1({A, B}, [X]) -> {[X | A], B};
unzagzig_1({A, B}, [Hd1, Hd2 | Tl]) ->
    unzagzig_1( {[Hd1 | A], [Hd2 | B]}, Tl ).

    
%%%%% ------------------------------------------------------- %%%%%


-spec zipI( [A], [B] ) -> [{A, B}].

zipI([], _) -> [];
zipI(_, []) -> [];
zipI([Hd1 | Tl1], [Hd2 | Tl2]) ->
    [ {Hd1, Hd2} | zipI(Tl1, Tl2) ].


-spec zip3I( [A], [B], [C] ) -> [{A, B, C}].

zip3I([], _, _) -> [];
zip3I(_, [], _) -> [];
zip3I(_, _, []) -> [];
zip3I([Hd1 | Tl1], [Hd2 | Tl2], [Hd3 | Tl3]) ->
    [ {Hd1, Hd2, Hd3} | zip3I(Tl1, Tl2, Tl3) ].
    
    
%%%%% ------------------------------------------------------- %%%%%

    
-spec zipwithI( fun( (A, B) -> T ), [A], [B] ) -> [T].    

zipwithI(_F, [], _) -> [];
zipwithI(_F, _, []) -> [];
zipwithI(F, [Hd1 | Tl1], [Hd2 | Tl2]) ->
    [ F(Hd1, Hd2) | zipwithI(F, Tl1, Tl2) ].


-spec zipwith3I( fun( (A, B, C) -> T ), [A], [B], [C] ) -> [T].

zipwith3I(_F, [], _, _) -> [];
zipwith3I(_F, _, [], _) -> [];
zipwith3I(_F, _, _, []) -> [];
zipwith3I(F, [Hd1 | Tl1], [Hd2 | Tl2], [Hd3 | Tl3]) ->
    [ F(Hd1, Hd2, Hd3) | zipwith3I(F, Tl1, Tl2, Tl3) ].

    
%%%%% ------------------------------------------------------- %%%%%



% https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html


% intersperse

% span/break
% group
% inits/tails

% unzip_irregular
