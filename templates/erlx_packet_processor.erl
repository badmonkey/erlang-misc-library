
-module({{srvid}}),

-behaviour(erlx_packet_processor).


-export([ init/2, handle_data/2]).


%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    {
    }).
    
    
%%%%% ------------------------------------------------------- %%%%%


init(Socket, _InitParams) ->
    {ok, #state{}, raw}.


%%%%% ------------------------------------------------------- %%%%%


handle_data({raw, _Bytes}, State) ->
    {ok, State};
    
    
handle_data({packet, _Bytes}, State) ->
    {ok, State};


handle_data({closed, _Bytes}, State) ->
    {ok, State};

    
handle_data({error, _Reason, _Bytes}, State) ->
    {ok, State}.



%%%%% ------------------------------------------------------- %%%%%


terminate(Reason, #state}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%


