
-module(hof).

-export([ repeat/2
        , if_ok_then/2, if_ok_then/3, if_ok_then/4 ]).


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

    
%%%%% ------------------------------------------------------- %%%%%

%
% if_ok_then(C, fun(X) -> end)
% if_ok_then(C, fun() -> end)
% if_ok_then(ok, fun() -> end)
%
% if_ok_then(C, module, funcname)
% if_ok_then(C, fun(x, ...) -> end, [arg1, arg2,...])
% if_ok_then(ok, fun(x, ...) -> end, [arg1, arg2,...])
%
% if_ok_then(C, module, funcname, [arg1, arg2,...])
% if_ok_then(ok, module, funcname, [arg1, arg2,...])
%
% @TODO how do we handle exceptions (or don't handle them and update spec)?
%

-spec if_ok_then( type:okvalue_or_error(T) | ok, function() ) -> type:okvalue_or_error(T).    
    
if_ok_then({error, _} = Cond, _) ->
    Cond;   
    
if_ok_then({ok, Value}, Func)
        when is_function(Func, 1) ->    
    type:wrap_okvalue( erlang:apply(Func, [Value]) );
    
if_ok_then(ok, Func)
        when is_function(Func, 0) ->    
    type:wrap_okvalue( erlang:apply(Func, []) );
    
if_ok_then({ok, _}, Func)
        when is_function(Func, 0) ->    
    type:wrap_okvalue( erlang:apply(Func, []) ).
    
  
    
-spec if_ok_then( type:okvalue_or_error(T) | ok, function(), list() ) -> type:okvalue_or_error(T);    
                ( type:okvalue_or_error(T), module(), atom() )        -> type:okvalue_or_error(T).
    
if_ok_then({error, _} = Cond, _, _) ->
    Cond;   

if_ok_then({ok, Value}, Module, Func)
        when is_atom(Module), is_atom(Func) ->    
    type:wrap_okvalue( erlang:apply(Module, Func, [Value]) );
    
if_ok_then(ok, Func, Args)
        when is_function(Func), is_list(Args) ->    
    type:wrap_okvalue( erlang:apply(Func, Args) );        
    
if_ok_then({ok, _}, Func, Args)
        when is_function(Func), is_list(Args) ->    
    type:wrap_okvalue( erlang:apply(Func, Args) ).

    
    
-spec if_ok_then( type:okvalue_or_error(T) | ok, module(), atom(), list() ) -> type:okvalue_or_error(T).

if_ok_then({error, _} = Cond, _, _, _) ->
    Cond;   
    
if_ok_then(ok, Module, Func, Args) ->    
    type:wrap_okvalue( erlang:apply(Module, Func, Args) );
    
if_ok_then({ok, _}, Module, Func, Args) ->    
    type:wrap_okvalue( erlang:apply(Module, Func, Args) ).    
  

%%%%% ------------------------------------------------------- %%%%%
  
    