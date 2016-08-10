
-module(xssl).

-export([cipher_suites/1]).


%%%%% ------------------------------------------------------- %%%%%
    
    
cipher_suites(secure) ->
    secure( ssl:cipher_suites() );

cipher_suites(perfect_forward) ->
    perfect_forward( ssl:cipher_suites() );

cipher_suites(preferred) ->
    Ciphers = ssl:sipher_suites(),
    sets:to_list(
        sets:intersection(
              sets:from_list( secure(Ciphers) )
            , sets:from_list( perfect_forward(Ciphers) ) ) );
            
            
cipher_suites(Type) ->
    ssl:cipher_suites(Type).            


%%%%% ------------------------------------------------------- %%%%%


secure(Ciphers) ->
    lists:filter(
        fun ({_, des_cbc, _})       -> false
        ;   ({_, rc4_128, _})       -> false
        ;   ({dhe_rsa, _, _})       -> false
        ;   ({_, _, md5})           -> false
        ;   (_)                     -> true
        end, Ciphers).


perfect_forward(Ciphers) ->
    lists:filter(
        fun ({dhe_dss, _, _})       -> true
        ;   ({dhe_rsa, _, _})       -> true
        ;   ({ecdhe_ecdsa, _, _})   -> true
        ;   ({ecdhe_rsa, _, _})     -> true
        ;   (_)                     -> false
        end, Ciphers).



