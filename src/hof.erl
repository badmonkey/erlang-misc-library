
-module(hof).

-export([ repeat/2 ]).


%%%%% ------------------------------------------------------- %%%%%


-spec repeat( type:natural(), fun(() -> _) ) -> ok | type:exception();
            ( type:natural(), fun(( type:natural() ) -> _) ) -> ok | type:exception().

repeat(0, _) -> ok;

repeat(N, ElemFun)
        when is_function(ElemFun, 0) ->
    ElemFun(),
    repeat(N - 1, ElemFun);
    
    
repeat(N, ElemFun)
        when is_function(ElemFun, 1) ->
    ElemFun(N),
    repeat(N - 1, ElemFun).

    