
-module(publisher).

-export([new/1, new/2, notify/3, notify_many/3, subscribe/2]).


-record(publisher,
    { router
    , wrapper
    }).
    

%%%%% ------------------------------------------------------- %%%%%


-spec new( atom() ) -> #publisher{}.

new(Router) ->
    new(Router, fun(X) -> X end).
    
    
-spec new( atom(), fun( (term()) -> term() ) ) -> #publisher{}.

new(Router, Wrapper) ->
    router:new(Router),
    #publisher{ router = Router, wrapper = Wrapper }.    
    
    
%%%%% ------------------------------------------------------- %%%%%    
    

-spec notify( #publisher{}, router:publish_path(), term() ) -> type:ok_or_error().
    
notify(#publisher{} = Publisher, Path, Mesg) ->
    PidList = router:get(Publisher#publisher.router, Path),

    Wrapper = Publisher#publisher.wrapper,
    Wrapped = Wrapper(Mesg),
    
    %lager:debug("PUBLISH ~p to ~p -> ~p", [Wrapped, Path, PidList]),
    
    OldPri = process_flag(priority, high),
    lists:foreach(fun(Pid) -> Pid ! Wrapped end, PidList),
    process_flag(priority, OldPri),
    
    ok.
    
    
%%%%% ------------------------------------------------------- %%%%%    
    

-spec notify_many( #publisher{}, [router:publish_path()], term() ) -> type:ok_or_error().
    
notify_many(#publisher{} = Publisher, Paths, Mesg) ->
    PidList = router:get_many([Publisher#publisher.router], Paths),

    Wrapper = Publisher#publisher.wrapper,
    Wrapped = Wrapper(Mesg),
    
    OldPri = process_flag(priority, high),
    lists:foreach(fun(Pid) -> Pid ! Wrapped end, PidList),
    process_flag(priority, OldPri),
    
    ok.    

    
%%%%% ------------------------------------------------------- %%%%%    


-spec subscribe( #publisher{}, router:subscribe_path() ) -> type:ok_or_error().

subscribe(#publisher{} = Publisher, Path) ->
    %lager:debug("SUBSCRIBE ~p to ~p", [self(), Path]),
    router:add(Publisher#publisher.router, Path, self()).
