
-module(unrolled).

-export([]).


%%%%% ------------------------------------------------------- %%%%%


-type type(T) :: empty
               | {T} | {T, T}
               | {tail, T, T, T}            % tuple/4
               | {head, T, type(T)}         % tuple/3
               | {head, T, T, type(T)}      % tuple/4
               | {head, T, T, T, type(T)}   % tuple/5
               | {Len :: integer(), T, T, T, T, type(T)} % tuple/6
               .
               
-type atmost4(T) :: {} | {T} | {T,T} | {T,T,T} | {T,T,T,T}.
               
                      
-export_type([type/1]).


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec new() -> type(T :: term()).

new() -> empty.


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec isEmpty( type(T :: term()) ) -> boolean().

isEmpty(empty)  -> true;
isEmpty(_)      -> false.  % @todo return false for non empty but valid members and error otherwise?


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec head( type(T) ) -> T.

head(empty)                 -> error:badarg();
head({X})                   -> X;
head({X, _})                -> X;
head({head, X, _})          -> X;
head({_, X, _, _})          -> X;
head({_, X, _, _, _})       -> X;
head({_, X, _, _, _, _})    -> X.



%%%%% ------------------------------------------------------- %%%%%
%
%
-spec tail( type(T) ) -> type(T).

tail(empty)                         -> error:badarg();
tail({_})                           -> empty;
tail({_, X})                        -> {X};
tail({tail, _, E1, E2})             -> {E1, E2};
tail({head, _, Tail})               -> Tail;
tail({head, _, E1, Tail})           -> {head, E1, Tail};
tail({head, _, E1, E2, Tail})       -> {head, E1, E2, Tail};
tail({Len, _, E1, E2, E3, Tail})    -> {head, E1, E2, E3, Tail}.


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec cons(T, type(T)) -> type(T).

cons(E, empty)                              -> {E};
cons(E, {X})                                -> {E, X};
cons(E, {X1, X2})                           -> {tail, E, X1, X2};
cons(E, {tail, X1, X2, X3})                 -> make4({E, X1, X2, X3});
cons(E, {head, X, Tail})                    -> {head, E, X, Tail};
cons(E, {head, X1, X2, Tail})               -> {head, E, X1, X2, Tail};
cons(E, {head, X1, X2, X3, Tail})           -> make4({E, X1, X2, X3}, Tail);
cons(E, {_Len, _, _, _, _, _Tail} = Chain)  -> {head, E, Chain}.


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec concat( type(T), type(T) ) -> type(T).

concat(empty, Second)                           -> Second;
concat({_} = In, Second)                        -> make4(In, Second);
concat({_, _} = In, Second)                     -> make4(In, Second);
concat({tail, X1, X2, X3}, Second)              -> make4({X1, X2, X3}, Second);

concat({head, X1, Tail}, Second)                -> {head, X1, concat(Tail, Second)};
concat({head, X1, X2, Tail}, Second)            -> {head, X1, X2, concat(Tail, Second)};
concat({head, X1, X2, X3, Tail}, Second)        -> {head, X1, X2, X3, concat(Tail, Second)};

concat({_Len, X1, X2, X3, X4, Tail}, Second)    -> make4({X1, X2, X3, X4}, concat(Tail, Second)).



%%%%% ------------------------------------------------------- %%%%%
%
%
-spec len( type(T :: term()) ) -> type:cardinal().

len(empty)                      -> 0;
len({_})                        -> 1;
len({_, _})                     -> 2;
len({head, _, Tail})            -> 1 + len(Tail);
len({tail, _, _, _})            -> 3;
len({head, _, _, Tail})         -> 2 + len(Tail);
len({head, _, _, _, Tail})      -> 3 + len(Tail);
len({Len, _, _, _, _, _Tail})   -> Len.


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec equal( type(T), type(T) ) -> boolean().

equal(empty, empty)     -> true;
equal(empty, _)         -> false;
equal(_, empty)         -> false;
equal(A, B) ->
    { Test, RestA } = split4(A),
    case split4(B) of
        {Test, RestB}   -> equal(RestA, RestB)
    ;   {_, _}          -> false
    end.


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec last( type(T) ) -> T.
    
last(empty)                     -> error:badarg();
last({X})                       -> X;
last({_, X})                    -> X;
last({tail, _, _, X})           -> X;

last({head, X, empty})          -> X;
last({head, _, X, empty})       -> X;
last({head, _, _, X, empty})    -> X;
last({_Len, _, _, _, X, empty}) -> X;

last({head, _, Tail})           -> last(Tail);
last({head, _, _, Tail})        -> last(Tail);
last({head, _, _, _, Tail})     -> last(Tail);
last({_Len, _, _, _, _, Tail})  -> last(Tail).


%%%%% ------------------------------------------------------- %%%%%
%
% leading
-spec leading( type(T) ) -> type(T).

leading(empty)                          -> error:badarg();

leading({_})                            -> empty;
leading({X, _})                         -> {X};
leading({tail, X1, X2, _})              -> {X1, X2};

leading({head, _, empty})               -> empty;
leading({head, X1, _, empty})           -> {X1};
leading({head, X1, X2, _, empty})       -> {X1, X2};
leading({_Len, X1, X2, X3, _, empty})   -> {tail, X1, X2, X3};

leading({head, X1, Tail})               -> {head, X1, leading(Tail)};
leading({head, X1, X2, Tail})           -> {head, X1, X2, leading(Tail)};
leading({head, X1, X2, X3, Tail})       -> {head, X1, X2, X3, leading(Tail)};
leading({_Len, X1, X2, X3, X4, Tail})   -> make4({X1, X2, X3, X4}, leading(Tail)).


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec reverse( type(T) ) -> type(T).

reverse(X) ->
    fromlist( lists:reverse( tolist(X) ) ).
    

%%%%% ------------------------------------------------------- %%%%%
%
%
-spec member( T, type(T) ) -> boolean().

member(X, empty)                        -> false;

member(X, {X})                          -> true;
member(X, {_})                          -> false;

member(X, {X, _})                       -> true;
member(X, {_, X})                       -> true;
member(X, {_, _})                       -> false;

member(X, {tail, X, _, _})              -> true;
member(X, {tail, _, X, _})              -> true;
member(X, {tail, _, _, X})              -> true;
member(X, {tail, _, _, _})              -> false;

member(X, {head, X, _Tail})             -> true;
member(X, {head, _,  Tail})             -> member(X, Tail);

member(X, {head, X, _, _Tail})          -> true;
member(X, {head, _, X, _Tail})          -> true;
member(X, {head, _, _,  Tail})          -> member(X, Tail);

member(X, {head, X, _, _, _Tail})       -> true;
member(X, {head, _, X, _, _Tail})       -> true;
member(X, {head, _, _, X, _Tail})       -> true;
member(X, {head, _, _, _,  Tail})       -> member(X, Tail);

member(X, {_Len, X, _, _, _, _Tail})    -> true;
member(X, {_Len, _, X, _, _, _Tail})    -> true;
member(X, {_Len, _, _, X, _, _Tail})    -> true;
member(X, {_Len, _, _, _, X, _Tail})    -> true;
member(X, {_Len, _, _, _, _,  Tail})    -> member(X, Tail).


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec tolist( type(T) ) -> list(T).

tolist(empty)                               -> [];
tolist({X})                                 -> [X];
tolist({X1, X2})                            -> [X1, X2];
tolist({tail, X1, X2, X3})                  -> [X1, X2, X3];

tolist({head, X, Tail})                     -> tolist_r(Tail, [X]);
tolist({head, X1, X2, Tail})                -> tolist_r(Tail, [X2, X1]);
tolist({head, X1, X2, X3, Tail})            -> tolist_r(Tail, [X3, X2, X1]);
tolist({_Len, X1, X2, X3, X4, Tail})        -> tolist_r(Tail, [X4, X3, X2, X1]).


tolist_r(empty, Acc)                        -> lists:reverse(Acc);
tolist_r({X}, Acc)                          -> lists:reverse([X | Acc]);
tolist_r({X1, X2}, Acc)                     -> lists:reverse([X2, X1 | Acc]);
tolist_r({tail, X1, X2, X3}, Acc)           -> lists:reverse([X3, X2, X1 | Acc]);

tolist_r({head, X, Tail}, Acc)              -> tolist_r(Tail, [X | Acc]);
tolist_r({head, X1, X2, Tail}, Acc)         -> tolist_r(Tail, [X2, X1 | Acc]);
tolist_r({head, X1, X2, X3, Tail}, Acc)     -> tolist_r(Tail, [X3, X2, X1 | Acc]);
tolist_r({_Len, X1, X2, X3, X4, Tail}, Acc) -> tolist_r(Tail, [X4, X3, X2, X1 | Acc]).
               

%%%%% ------------------------------------------------------- %%%%%
%
%
-spec fromlist( list(T) ) -> type(T).

fromlist([])                            -> empty;
fromlist([X1])                          -> {X1};
fromlist([X1, X2])                      -> {X1, X2};
fromlist([X1, X2, X3])                  -> {tail, X1, X2 ,X3};

fromlist([X1, X2, X3, X4])              -> make4({X1, X2 ,X3, X4});

fromlist([X1, X2, X3, X4 | Rest])       -> make4({X1, X2, X3, X4}, fromlist(Rest)).


-spec fromlist( list(T), type(T) ) -> type(T).

fromlist([], Tail)                      -> Tail;
fromlist([X1], Tail)                    -> make4({X1}, Tail);
fromlist([X1, X2], Tail)                -> make4({X1, X2}, Tail);
fromlist([X1, X2, X3], Tail)            -> make4({X1, X2 ,X3}, Tail);
fromlist([X1, X2, X3, X4], Tail)        -> make4({X1, X2 ,X3, X4}, Tail);

fromlist([X1, X2, X3, X4 | Rest], Tail) -> make4({X1, X2, X3, X4}, fromlist(Rest, Tail)).


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec foreach( fun( (T) -> _ ), type(T) ) -> ok.

foreach(_F, empty)                          -> ok;
foreach(F, {X})                             -> F(X), ok;
foreach(F, {X1, X2})                        -> F(X1), F(X2), ok;
foreach(F, {tail, X1, X2, X3})              -> F(X1), F(X2), F(X3), ok;

foreach(F, {head, X1, Tail})                -> F(X1), foreach(F, Tail);
foreach(F, {head, X1, X2, Tail})            -> F(X1), F(X2), foreach(F, Tail);
foreach(F, {head, X1, X2, X3, Tail})        -> F(X1), F(X2), F(X3), foreach(F, Tail);
foreach(F, {_Len, X1, X2, X3, X4, Tail})    -> F(X1), F(X2), F(X3), F(X4), foreach(F, Tail).


%%%%% ------------------------------------------------------- %%%%%

%filter
%foldl
%foldr
%map

%all
%any

%take
%drop
%takeWhile
%dropWhile

%nth

%zip
%zipWith


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec realign( type(T) ) -> type(T).

realign(empty)                  -> empty;
realign({_} = Urld)             -> Urld;
realign({_, _} = Urld)          -> Urld;
realign({tail, _, _, _} = Urld) -> Urld;

realign(Urld)                   ->
    case split4(Urld) of
        { Atm4, {} }    -> make4(Atm4)
    ;   { Atm4, Rest }  -> make4(Atm4, realign(Rest))
    end.



%%%%% ------------------------------------------------------- %%%%%
%
%
-spec split4( type(T) ) -> { atmost4(T), type(T) }.

split4(X) -> split4(4, X).


-spec split4( 1|2|3|4, type(T) ) -> { atmost4(T), type(T) }.

split4(_N, empty)                       -> { {}, empty };

split4(_N, {_} = Urld)                  -> { Urld, empty };

split4(1, {X1, X2})                     -> { {X1}, {X2} };
split4(_N, {_, _} = Urld)               -> { Urld, empty };

split4(1, {tail, X1, X2, X3})           -> { {X1}, {X2, X3} };
split4(2, {tail, X1, X2, X3})           -> { {X1, X2}, {X3} };
split4(_N, {tail, X1, X2, X3})          -> { {X1, X2, X3}, empty };

split4(1, {head, X, Tail})              -> { {X}, Tail };
split4(N, {head, X, Tail})              ->
    { Atm3, Rest } = split4(N - 1, Tail),
    { glue4(X, Atm3), Rest };

split4(1, {head, X1, X2, Tail})         -> { {X1}, {head, X2, Tail} };
split4(2, {head, X1, X2, Tail})         -> { {X1, X2}, Tail };
split4(N, {head, X1, X2, Tail})         ->
    { Atm2, Rest } = split4(N - 2, Tail),
    { glue4(X1, X2, Atm2), Rest };

split4(1, {head, X1, X2, X3, Tail})     -> { {X1}, {head, X2, X3, Tail} };
split4(2, {head, X1, X2, X3, Tail})     -> { {X1, X2}, {head, X3, Tail} };
split4(3, {head, X1, X2, X3, Tail})     -> { {X1, X2, X3}, Tail };
split4(4, {head, X1, X2, X3, Tail})     ->
    { Atm1, Rest } = split4(1, Tail),
    { glue4(X1, X2, X3, Atm1), Rest };

split4(1, {_Len, X1, X2, X3, X4, Tail}) -> { {X1}, {head, X2, X3, X4, Tail} };
split4(2, {_Len, X1, X2, X3, X4, Tail}) -> { {X1, X2}, {head, X3, X4, Tail} };
split4(3, {_Len, X1, X2, X3, X4, Tail}) -> { {X1, X2, X3}, {head, X4, Tail} };
split4(4, {_Len, X1, X2, X3, X4, Tail}) -> { {X1, X2, X3, X4}, Tail }.
       

%%%%% ------------------------------------------------------- %%%%%
%
%
-spec glue4( T, atmost4(T) ) -> atmost4(T).

glue4(A, {})            -> {A};
glue4(A, {E1})          -> {A, E1};
glue4(A, {E1, E2})      -> {A, E1, E2};
glue4(A, {E1, E2, E3})  -> {A, E1, E2, E3};
glue4(_A, _T)           -> error:badarg().


-spec glue4( T, T, atmost4(T) ) -> atmost4(T).

glue4(A, B, {})         -> {A, B};
glue4(A, B, {E1})       -> {A, B, E1};
glue4(A, B, {E1, E2})   -> {A, B, E1, E2};
glue4(_A, _B, _T)       -> error:badarg().


-spec glue4( T, T, T, atmost4(T) ) -> atmost4(T).

glue4(A, B, C, {})      -> {A, B, C};
glue4(A, B, C, {E1})    -> {A, B, C, E1};
glue4(_A, _B, _C, _T)   -> error:badarg().


%%%%% ------------------------------------------------------- %%%%%
%
%
-spec make4( atmost4(T) ) -> type(T).

make4({})                       -> empty;
make4({_} = In)                 -> In;
make4({_, _} = In)              -> In;
make4({X1, X2, X3})             -> {tail, X1, X2, X3};
make4({X1, X2, X3, X4})         -> {4, X1, X2, X3, X4, empty}.


-spec make4( atmost4(T), type(T) ) -> type(T).

make4(In, empty)                -> make4(In);
make4({}, Tail)                 -> Tail;
make4({X1}, Tail)               -> {head, X1, Tail};
make4({X1, X2}, Tail)           -> {head, X1, X2, Tail};
make4({X1, X2, X3}, Tail)       -> {head, X1, X2, X3, Tail};
make4({X1, X2, X3, X4}, Tail)   -> {4 + length(Tail), X1, X2, X3, X4, Tail}.





