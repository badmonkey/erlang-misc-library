


-type distance() :: non_neg_integer().
-type metric(T) :: fun( (T, T) -> distance() ).


-opaque bknode(T) :: { T, #{ distance() => bknode(T) } }.
-type bktree(T) :: { bktree, bknode(T) | undefined, metric(T) }.



-spec new( metric(T) ) -> bknode(T).
new(F) -> {bktree, undefined, F}.

node_new(X) -> {X, #{}}.


-spec add( T, bktree(T) ) -> bktree(T).

add(X, {bktree, undefined, M}) ->
	{bktree, node_new(X), M};

add(X, {bktree, Node, M}) ->
	{bktree, node_add(X, M, Node), M}.


-spec node_add(T, metric(T), bknode(T)) -> bknode().

node_add(X, M, {R, Childs}) ->
	Distance = M(X, R),
	case maps:get(Distance, Childs, undefined) of
		undefined	-> {R, maps:put(Distance, node_new(X), Childs)}
	;	Node		-> {R, maps:put(Distance, node_add(X, M, Node), Childs)}
	end.


-spec search( T, distance(), bktree(T) ) -> [T].

search(_X, _Radius, {bktree, undefined, _}) -> [];
search(X, Radius, {bktree, Node, M}) -> 
	node_search(X, Radius, M, [Node], []).


-spec node_search( T, distance(), metric(T), In :: [bknode(T)], Acc :: [T] ) -> [T].

node_search(_X, _R, _M, [], Acc) -> lists:reverse(Acc);

node_search(X, R, M, [{Y, Children} | Rest], Acc) ->
	Distance = M(X, Y),
	{Low, High} = {Distance - R, Distance + R},
	NewAcc =	if
					Distance =< R	-> [Y | Acc]
					true			-> Acc
				end,
	Found = [ maps:get(K, Children)) || K <- maps:keys(Children)
									  , K >= Low, K =< High ],
	node_search(X, R, M, lists:append(Rest, Found), NewAcc).
	 
	  
	
