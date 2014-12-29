-module(xproplists).

-export([merge/2]).



%%%%% ------------------------------------------------------- %%%%%


merge(PropsIn, NewProps)
        when is_list(PropsIn), is_list(NewProps) ->
    PropsIn2 = lists:keysort(1, proplists:unfold(PropsIn)),
    NewProps2 = lists:keysort(1, proplists:unfold(NewProps)),
    OutProps = lists:keymerge(1, PropsIn2, NewProps2),
    proplists:compact(OutProps);
    
    
merge(PropsIn, Property)
        when is_list(PropsIn), is_tuple(Property) ->
    PropsIn2 = lists:keysort(1, proplists:unfold(PropsIn)),
    OutProps = lists:keymerge(1, PropsIn2, [Property]),
    proplists:compact(OutProps);
    
    
merge(PropsIn, Atom)
        when is_list(PropsIn), is_atom(atom) ->
    PropsIn2 = lists:keysort(1, proplists:unfold(PropsIn)),
    OutProps = lists:keymerge(1, PropsIn2, [{Atom, true}]),
    proplists:compact(OutProps).
    

%%%%% ------------------------------------------------------- %%%%%

