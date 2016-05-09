
-module(gen_server_base).


-export([ call/2, call/3
        , cast/2
        , reply/2
        , abcast/2, abcast/3
        , multi_call/2, multi_call/3, multi_call/4
        , wake_hib/5
        , is_name/1, is_alias_name/1
        , name/1, where/1, register_name/1, register_name/2]).
        
-export([start_link_name/3]).  
    

%%%%% ------------------------------------------------------- %%%%%


call(Name, Request) ->
    gen_server:call(Name, Request).
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).
    
cast(Name, Request) ->
    gen_server:cast(Name, Request).

reply(To, Reply) ->
    gen_server:reply(To, Reply).

abcast(Name, Request) ->
    gen_server:abcast(Name, Request).
abcast(Nodes, Name, Request) ->
    gen_server:abcast(Nodes, Name, Request).

multi_call(Name, Req) ->
    gen_server:multi_call(Name, Req).
multi_call(Nodes, Name, Req)  ->
    gen_server:multi_call(Nodes, Name, Req).
multi_call(Nodes, Name, Req, Timeout)  ->
    gen_server:multi_call(Nodes, Name, Req, Timeout).

wake_hib(Parent, Name, State, Mod, Debug) ->
    gen_server:wake_hib(Parent, Name, State, Mod, Debug).
  

%%%%% ------------------------------------------------------- %%%%%


-spec start_link_name( type:server_name(), atom(), list() ) -> type:start_result().

start_link_name(Name, Module, Args) ->
    case Name of
        undefined           -> gen_server:start_link(Module, Args, [])
    ;   X when is_atom(X)   -> gen_server:start_link({local, Name}, Module, Args, [])
    ;   _                   -> gen_server:start_link(Name, Module, Args, [])
    end.

    
%%%%% ------------------------------------------------------- %%%%%


-spec is_name( type:server_name() ) -> boolean().

is_name(undefined)          -> false;
is_name(X) when is_atom(X)  -> true;
is_name(X) when is_pid(X)   -> true;
is_name({local,_})          -> true;
is_name({global,_})         -> true;
is_name({via,_,_})          -> true.


-spec is_alias_name( type:server_name() ) -> boolean().

is_alias_name(X) when is_pid(X) -> false;
is_alias_name(X)                -> is_name(X).


-spec name( type:server_name() ) -> term().

name({local, Name})         -> Name;
name({global, Name})        -> Name;
name({via, _, Name})        -> Name;
name(X) when is_atom(X)     -> X;
name(Pid) when is_pid(Pid)  -> Pid.


-spec where( type:server_name() ) -> pid().

where(X) when is_atom(X)    -> erlang:whereis(X);
where(X) when is_pid(X)     -> X;
where({global, Name})       -> global:whereis_name(Name);
where({via, Module, Name})  -> Module:whereis_name(Name);
where({local, Name})        -> erlang:whereis(Name).


-spec register_name( type:server_name() ) -> true | {false, pid()}.

register_name(X) -> register_name(X, self()).


-spec register_name( type:server_name(), pid() ) -> true | {false, pid()}.

register_name(X, Pid) when is_atom(X)           ->
    register_name({local, X}, Pid);

register_name({local, Name} = LN, Pid)          ->
    try register(Name, Pid) of
        true    -> true
    catch
        error:_ -> {false, where(LN)}
    end;
    
register_name({global, Name} = GN, Pid)         ->
    case global:register_name(Name, Pid) of
        yes -> true
    ;   no  -> {false, where(GN)}
    end;
    
register_name({via, Module, Name} = GN, Pid)    ->
    case Module:register_name(Name, Pid) of
        yes -> true
    ;   no  -> {false, where(GN)}
    end.
    
    
    