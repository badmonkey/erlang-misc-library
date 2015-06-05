
-module(publisher).

-export([new/1, notify/3, notify_many/3, subscribe/2]).


-record(publisher,
    { router
    }).
    

%%%%% ------------------------------------------------------- %%%%%


-spec new( router:routername() ) -> #publisher{}.

new(Router) ->
    #publisher{ router = Router }.
    
    
%%%%% ------------------------------------------------------- %%%%%    
    

-spec notify( #publisher{}, router:publish_path(), term() ) -> type:ok_or_error().
    
notify(#publisher{} = Publisher, Path, Mesg) ->
    PidList = router:get(Publisher#publisher.router, Path),

    OldPri = process_flag(priority, high),
    lists:foreach(fun(Pid) -> Pid ! Mesg end, PidList),
    process_flag(priority, OldPri),
    
    ok.
    
    
%%%%% ------------------------------------------------------- %%%%%    
    

-spec notify_many( #publisher{}, [router:publish_path()], term() ) -> type:ok_or_error().
    
notify_many(#publisher{} = Publisher, Paths, Mesg) ->
    PidList = router:get_many([Publisher#publisher.router], Paths),

    OldPri = process_flag(priority, high),
    lists:foreach(fun(Pid) -> Pid ! Mesg end, PidList),
    process_flag(priority, OldPri),
    
    ok.    

    
%%%%% ------------------------------------------------------- %%%%%    


-spec subscribe( #publisher{}, router:subscribe_path() ) -> type:ok_or_error().

subscribe(#publisher{} = Publisher, Path) ->
    router:add(Publisher#publisher.router, Path, self()),
    ok.
