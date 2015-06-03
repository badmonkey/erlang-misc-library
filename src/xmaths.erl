
-module(xmaths).

-export([list_sums/1, list_product/1, arithmetic_mean/1, geometric_mean/1,
         weighted_arithmetic_mean/1, histograph/1, median/1, mode/1, std_deviation/1,
         std_deviation2/1, root_mean_square/1, list_partial_sums/1, list_differences/1,
         std_spread/1, primes/1, as_list/1, as_list/2, round/2]).

         

% Copied from  http://fullof.bs/arithmetic-mean-geometric-mean-harmonic-mean-and-weighted-arithmetic-mean-statistics-in-erlang-part/
% It appears the ScUtils library isn't available anymore


%%%%% ------------------------------------------------------- %%%%%


% [ SqSum, Sum, Len }
list_sums(List) when is_list(List) ->
    lists:foldl(
        fun (X, {K,S,N}) ->
            {K+X*X, S+X, N+1}
        end,
        {0,0,0}, List).
            

%%%%% ------------------------------------------------------- %%%%%


list_partial_sums(List) when is_list(List) ->
    {Result, _} = lists:mapfoldl(
        fun(X, Sum) ->
            NewSum = Sum + X,
            {NewSum, NewSum}
        end,
        0, List),
    Result.

        
%%%%% ------------------------------------------------------- %%%%%


list_differences(List) when is_list(List) ->
    {Result,_} = lists:mapfoldl(
        fun(X, Prev) ->
            { X - Prev, X }
        end,
        0, List),
    Result.


%%%%% ------------------------------------------------------- %%%%%


list_product(List) when is_list(List) ->
    list_product(List, 1).

list_product([], Counter) ->
    Counter;

list_product([Head|Tail], Counter) ->
    list_product(Tail, Counter*Head).

    
%%%%% ------------------------------------------------------- %%%%%


arithmetic_mean(List) when is_list(List) ->
    lists:sum(List) / length(List).

geometric_mean(List) when is_list(List) ->
    math:pow(list_product(List), 1/length(List)).

    
%%%%% ------------------------------------------------------- %%%%%


weighted_arithmetic_mean(List) when is_list(List) ->
    weighted_arithmetic_mean(List, 0, 0).

weighted_arithmetic_mean([], Num, Denom) ->
    Num/Denom;

weighted_arithmetic_mean([{W,V}|Tail], Num, Denom) ->
    weighted_arithmetic_mean(Tail, Num+(W*V), Denom+W).

    
%%%%% ------------------------------------------------------- %%%%%
    
    
histograph(List) when is_list(List) ->
    [Head|Tail] = lists:sort(List),
    histo_count(Tail, Head, 1, []).

histo_count([], Current, Count, Work) ->
    lists:reverse([{Current,Count}]++Work);

histo_count([Current|Tail], Current, Count, Work) ->
    histo_count(Tail, Current, Count+1, Work);

histo_count([New|Tail], Current, Count, Work) ->
    histo_count(Tail, New, 1, [{Current,Count}]++Work).


%%%%% ------------------------------------------------------- %%%%%


median(List) when is_list(List) ->
    SList = lists:sort(List),
    Length = length(SList),
    case even_or_odd(Length) of
        even -> [A,B] = lists:sublist(SList, round(Length/2), 2),
                (A+B)/2
    ;   odd  -> lists:nth( round((Length+1)/2), SList )
    end.

even_or_odd(Num) when is_integer(Num) ->
    if
        Num band 1 == 0 -> even
    ;   true            -> odd
    end.

    
%%%%% ------------------------------------------------------- %%%%%

    
mode([]) -> [];

mode(List) when is_list(List) ->
    mode_front(lists:reverse(lists:keysort(2, histograph(List)))).

mode_front([{Item,Freq}|Tail]) ->
    mode_front(Tail, Freq, [Item]).

mode_front([ {Item, Freq} | Tail], Freq, Results) ->
    mode_front(Tail, Freq, [Item]++Results);

mode_front([{_Item,_Freq} |_Tail],_Better, Results) ->
    Results;

mode_front([], _Freq, Results) -> Results.


%%%%% ------------------------------------------------------- %%%%%


std_deviation2(Values) when is_list(Values) ->
    Mean = arithmetic_mean(Values),
    { Mean, math:sqrt(arithmetic_mean([ (Val-Mean)*(Val-Mean) || Val <- Values ])) }.
    
std_deviation(Values) ->
    { _, X } = std_deviation2(Values),
    X.

std_spread({M, S}) ->
    [{0.6827, M -  S, M +  S}
    ,{0.9545, M -2*S, M +2*S}
    ,{0.9973, M -3*S, M +3*S}].
    
    
%%%%% ------------------------------------------------------- %%%%%


root_mean_square(List) when is_list(List) ->
    math:sqrt(arithmetic_mean([ Val*Val || Val <- List ])).


%%%%% ------------------------------------------------------- %%%%%

    
primes(Prime, Max, Primes, Integers) when Prime > Max ->
    lists:reverse([Prime|Primes]) ++ Integers;
    
primes(Prime, Max, Primes, Integers) ->
    [NewPrime|NewIntegers] = [ X || X <- Integers, X rem Prime =/= 0 ],
    primes(NewPrime, Max, [Prime|Primes], NewIntegers).

primes(N) ->
    primes(2, round(math:sqrt(N)), [], lists:seq(3,N,2)). % skip odds
    

%%%%% ------------------------------------------------------- %%%%%

    
as_list(I) -> erlang:integer_to_list(I).


-spec as_list( integer(), integer() ) -> string().

as_list(I, 10) -> erlang:integer_to_list(I);
as_list(I, Base)
    when  is_integer(I)
        , is_integer(Base)
        , Base >= 2
        , Base =< 1+$Z-$A+10+1+$z-$a  ->
    if
        I < 0   -> [$- | as_list(-I, Base, [])]
    ;   true    -> as_list(I, Base, [])
    end;
as_list(I, Base) -> erlang:error(badarg, [I, Base]).


-spec as_list( integer(), integer(), string() ) -> string().

as_list(I0, Base, R0) ->
    D  = I0 rem Base,
    I1 = I0 div Base,
    R1 =    if
                D >= 36     -> [D-36+$a | R0]
            ;   D >= 10     -> [D-10+$A | R0]
            ;   true        -> [D+$0 | R0]
            end,
    if
        I1 =:= 0    -> R1
    ;   true        -> as_list(I1, Base, R1)
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec round( number(), non_neg_integer() ) -> type:number().    

round(Number, 0) ->    
    erlang:trunc(Number);
    
round(Number, Precision) ->
    P = math:pow(10, Precision),
    erlang:round(Number * P) / P. 
    
    