
-module(error).

-export([ throw_if_error/1, throw_error/1, badarg/0 ]).
        

%%%%% ------------------------------------------------------- %%%%%


badarg() -> erlang:error(badarg).


%%%%% ------------------------------------------------------- %%%%%


-spec throw_if_error( type:error() )    -> type:exception()
                   ;( term() )          -> term().

throw_if_error({error, X})  -> throw( {error, X} );
throw_if_error(X)           -> X.


%%%%% ------------------------------------------------------- %%%%%


-spec get_reason( type:error() | term() ) -> term().

get_reason({error, X})  -> X;
get_reason(R)           -> R.


-spec throw_error( type:error() | term() ) -> type:exception().

throw_error(Err) -> throw( {error, get_reason(Err)} ).


%throw_trace() ->

% stacktrace
%erlang:get_stacktrace()    - only valid inside catch
%erlang:process_info(self, backtrace)
