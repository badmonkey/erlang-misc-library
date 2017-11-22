
-module(pt_export_from).

-export([parse_transform/2, do_transform/3, make_relay/1]).


%%%%% ------------------------------------------------------- %%%%%
%
% -export_from(Mod, [f1/a1, f2/a2, FN/aN]).
%
% becomes
%
% -export([f1/a1, f2/a2, FN/AN]).
% ...
% f1(_P_1, ..., _P_a1) -> Mod:f1(_P_1, ..., _P_a1).
% f2(_P_1, ..., _P_a2) -> Mod:f2(_P_1, ..., _P_a2).
% fN(_P_1, ..., _P_aN) -> Mod:fN(_P_1, ..., _P_aN).
%
%%%%% ------------------------------------------------------- %%%%%


parse_transform(ParseTree, _Options) ->
    {Relays, NewTree} = stdforms:mapreduce(fun do_transform/3, [], ParseTree),

    NewRelayForms = [ make_relay(X) || X <- Relays ],
    NewTree ++ NewRelayForms.

    
%%%%% ------------------------------------------------------- %%%%%


make_relay({Mod, Name, 0, LINE}) ->
    stdforms:from_string("~p() -> ~p:~p().", [Name, Mod, Name], LINE);

make_relay({Mod, Name, Arity, LINE}) ->
    PList = stdforms:make_paramlist_str(Arity),
    stdforms:from_string("~p(~s) -> ~p:~p(~s).", [Name, PList, Mod, Name, PList], LINE).

    
%%%%% ------------------------------------------------------- %%%%%


do_transform(attribute, {'attribute', LINE, 'export_from', Params}, Relays) ->
    [Mod, FList] = Params,
    NewRelays = [ {Mod, Name, Arity, LINE} || {Name, Arity} <- FList ],
    
    { {'attribute', LINE, 'export', FList}
    , lists:append(Relays, NewRelays)
    };


do_transform(_, _, _) ->
    skip.


%%%%% ------------------------------------------------------- %%%%%   
