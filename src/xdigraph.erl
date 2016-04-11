
-module(xdigraph).

-export([find_edge/3]).


-export_type([edge_type/0]).


-type edge_type() :: {digraph:edge(), digraph:vertex(), digraph:vertex(), Label :: term()}.


%%%%% ------------------------------------------------------- %%%%%


-spec find_edge( digraph:graph(), digraph:vertex(), digraph:vertex() ) -> false | edge_type().

find_edge(G, V1, V2) ->
    find_edge_scan( digraph:out_edges(G, V1), G, V2).
                 
    
find_edge_scan([], _, _) -> false;

find_edge_scan([E | Tail], G, V2) ->
    case digraph:edge(G, E) of
        {_, _, V2, _} = R   -> R
    ;   _                   -> find_edge_scan(Tail, G, V2)
    end.


%%%%% ------------------------------------------------------- %%%%%

