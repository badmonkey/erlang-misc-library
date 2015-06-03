
-module(xerlang).
-extends(erlang).

-export([trace/1, trace/2, bin_to_hex/1, term_sha1/1]).


%%%%% ------------------------------------------------------- %%%%%


trace(Bin) when is_binary(Bin) ->
    lager:debug("TRACE binary ~p", [bin_to_hex(Bin)]),
    Bin;
    
trace(Term) ->
    lager:debug("TRACE ~p", [Term]),
    Term.
    
trace(Msg, Term) ->
    lager:debug("TRACE ~p ~p", [Msg, Term]),
    Term.


%%%%% ------------------------------------------------------- %%%%%

    
hex(X) -> integer_to_list(X,16).

bin_to_hex(Bin) when is_binary(Bin) ->
    lists:flatten([[hex(A),hex(B),hex(C),hex(D),32] || <<A:4,B:4,C:4,D:4>> <= Bin]).

    
%%%%% ------------------------------------------------------- %%%%%


term_sha1(Term) ->
    crypto:hash(sha1, term_to_binary(Term)).
    
    
    