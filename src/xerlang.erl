
-module(xerlang).

-export([trace/1, trace/2]).


%%%%% ------------------------------------------------------- %%%%%

trace(Bin) when is_binary(Bin) ->
    erlang:display({"TRACE", bin_to_hex(Bin)}),
    Bin;
    
trace(Term) ->
    erlang:display({"TRACE", Term}),
    Term.
    
trace(Msg, Term) ->
    erlang:display({"TRACE", Msg, Term}),
    Term.


%%%%% ------------------------------------------------------- %%%%%

    
hex(X) -> integer_to_list(X,16).

bin_to_hex(Bin) when is_binary(Bin) ->
    lists:flatten([[hex(A),hex(B),hex(C),hex(D),32] || <<A:4,B:4,C:4,D:4>> <= Bin]).
