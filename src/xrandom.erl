
-module(xrandom).

-export([pidseed/0, hwaddr/0, bits/1]).


%%%%% ------------------------------------------------------- %%%%%


-spec pidseed() -> {pos_integer(), pos_integer(), pos_integer()}.

pidseed() ->
    PidSum = erlang:phash2(self()),
    <<N0:32, N1:32, N2:32>> = crypto:rand_bytes(12),
    {N0 bxor PidSum, N1 bxor PidSum, N2 bxor PidSum}.
    
    
%%%%% ------------------------------------------------------- %%%%%
    
    
-spec hwaddr() -> <<_:48>>.

hwaddr() ->
    <<RndHi:7, _:1, RndLow:40>> = crypto:rand_bytes(6),
    %% Set bit 8 to 1
    <<RndHi:7, 1:1, RndLow:40>>.    


%%%%% ------------------------------------------------------- %%%%%

    
-spec bits( pos_integer() ) -> bitstring().
    
bits(N) ->    
    _ = random:seed( pidseed() ),
    Rnd = random:uniform(2 bsl N - 1),
    <<Rnd:N>>.

    
%%%%% ------------------------------------------------------- %%%%%    
    
    