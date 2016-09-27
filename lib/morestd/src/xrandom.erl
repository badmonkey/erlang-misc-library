
-module(xrandom).

-export([seed/0, seed/1, hwaddr/0, bits/1, string/2]).


%%%%% ------------------------------------------------------- %%%%%


-spec seed() -> random:ran().
seed() -> seed(pid).


-spec seed( stddoc | crypto | pid ) -> random:ran().
    
seed(pid) ->
    PidSum = erlang:phash2(self()),
    <<N0:32, N1:32, N2:32>> = crypto:rand_bytes(12),
    random:seed(N0 bxor PidSum, N1 bxor PidSum, N2 bxor PidSum);

seed(crypto) ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed(A, B, C);

seed(stddoc) ->
    random:seed( erlang:phash2([node()])
               , erlang:monotonic_time()
               , erlang:unique_integer()).
               
    
%%%%% ------------------------------------------------------- %%%%%
    
    
-spec hwaddr() -> <<_:48>>.

hwaddr() ->
    <<RndHi:7, _:1, RndLow:40>> = crypto:strong_rand_bytes(6),
    %% Set bit 8 to 1
    <<RndHi:7, 1:1, RndLow:40>>.    


%%%%% ------------------------------------------------------- %%%%%

    
-spec bits( pos_integer() ) -> bitstring().
    
bits(N) ->    
    Rnd = rand:uniform(2 bsl N - 1),
    <<Rnd:N>>.

    
%%%%% ------------------------------------------------------- %%%%%    
    
    
string(Length, AllowedChars) ->
    AllowLen = length(AllowedChars),
    lists:foldl(
        fun(_, Acc) ->
            Char = lists:nth(rand:uniform(AllowLen), AllowedChars),
            [ Char | Acc]
        end,
        [], lists:seq(1, Length)).    
