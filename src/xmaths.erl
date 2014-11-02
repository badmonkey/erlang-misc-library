-module(xmaths).

-export([list_sums/1, list_product/1, arithmetic_mean/1, geometric_mean/1, weighted_arithmetic_mean/1,
         histograph/1, median/1, mode/1, std_deviation/1, std_deviation2/1, root_mean_square/1,
         list_partial_sums/1, list_differences/1, std_spread/1]).

         

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
    {Result,_} = lists:foldl(
        fun(X, {L, Acc}) ->
            NewAcc = Acc + X,
            { L ++ [NewAcc], NewAcc }
        end,
        {[], 0}, List),
    Result.

        
%%%%% ------------------------------------------------------- %%%%%


list_differences(List) when is_list(List) ->
    {Result,_} = lists:foldl(
        fun(X, {L, Prev}) ->
            { L ++ [X - Prev], X }
        end,
        {[], 0}, List),
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
        even -> [A,B] = lists:sublist(SList, round(Length/2), 2), (A+B)/2;
        odd  -> lists:nth( round((Length+1)/2), SList )
    end.

even_or_odd(Num) when is_integer(Num) ->
    if
        Num band 1 == 0 -> even;
        true            -> odd
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
    [{0.6827, M -  S, M +  S},
     {0.9545, M -2*S, M +2*S},
     {0.9973, M -3*S, M +3*S} ].
    
    
%%%%% ------------------------------------------------------- %%%%%


root_mean_square(List) when is_list(List) ->
    math:sqrt(arithmetic_mean([ Val*Val || Val <- List ])).

    
%%%%% ------------------------------------------------------- %%%%%