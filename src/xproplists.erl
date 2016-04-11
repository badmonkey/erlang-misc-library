
-module(xproplists).

-export([merge/2, sort/1, delete_append/3, with/2, without/2]).

-export_type([property/0, proplist/0]).


-type property() :: proplists:property().
-type proplist() :: [property()].


%%%%% ------------------------------------------------------- %%%%%


-spec merge( proplist(), property() | proplist() ) -> proplist().

% merge(P1, P2) = P1 + P2 where P1 entries override P2 entries eg  merge(Overrides, Defaults)

merge(PropsIn, NewProps)
        when  is_list(PropsIn)
            , is_list(NewProps)  ->
    PropsIn2 = lists:keysort(1, proplists:unfold(PropsIn)),
    NewProps2 = lists:keysort(1, proplists:unfold(NewProps)),
    OutProps = lists:keymerge(1, PropsIn2, NewProps2),
    proplists:compact(OutProps);
    
    
merge(PropsIn, Property)
        when  is_list(PropsIn)
            , is_tuple(Property)  ->
    PropsIn2 = lists:keysort(1, proplists:unfold(PropsIn)),
    OutProps = lists:keymerge(1, PropsIn2, [Property]),
    proplists:compact(OutProps);
    
    
merge(PropsIn, Atom)
        when  is_list(PropsIn)
            , is_atom(atom)  ->
    PropsIn2 = lists:keysort(1, proplists:unfold(PropsIn)),
    OutProps = lists:keymerge(1, PropsIn2, [{Atom, true}]),
    proplists:compact(OutProps).
    

%%%%% ------------------------------------------------------- %%%%%


-spec sort( proplist() ) -> proplist().

sort(Props)
        when is_list(Props)  ->
    proplists:compact( lists:keysort(1, proplists:unfold(Props)) ).


%%%%% ------------------------------------------------------- %%%%%


-spec delete_append( [atom()], proplist(), proplist() ) -> proplist().

delete_append([], Append, Props)
        when  is_list(Append)
            , is_list(Props)  ->
    merge(Props, Append);


delete_append([Hd | Rest], Append, Props)
        when  is_list(Append)
            , is_list(Props)  ->
    delete_append(Rest, Append, proplists:delete(Hd, Props)).


%%%%% ------------------------------------------------------- %%%%%


-spec with( [atom()], proplist() ) -> proplist().

with(Keys, Props)
        when  is_list(Keys)
            , is_list(Props)  ->
    proplists:compact( xlists:keywith(Keys, 1, proplists:unfold(Props) ) ).



-spec without( [atom()], proplist() ) -> proplist().

without(Keys, Props)
        when  is_list(Keys)
            , is_list(Props)  ->
    proplists:compact( xlists:keywithout(Keys, 1, proplists:unfold(Props) ) ).



