
-module({{srvid}}),

-behaviour(erlx_tcp_server).


-export([start_link/1]).

-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).
-export([ handle_connection/1, handle_error/3]).


%%%%% ------------------------------------------------------- %%%%%


-record(state, 
    {
    }).
    

%%%%% ------------------------------------------------------- %%%%%


start_link(Port) ->
    erlx_tcp_server:start_link(?MODULE, Port).
    
    
%%%%% ------------------------------------------------------- %%%%%


init(_InitParams) ->
    {ok, #state{}}.


%%%%% ------------------------------------------------------- %%%%%


handle_call(Request, From, State) ->
    {stop, {invalid_call_request, Request, From}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_cast(Msg, State) ->
    {stop, {invalid_cast_request, Msg}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_info(Info, State) ->
    {stop, {invalid_info_request, Info}, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_connection({_Ipaddr, _Port, _Socket, _UserData}, State) ->
    erlx_tcp_server:start_link(?MODULE, []).


%%%%% ------------------------------------------------------- %%%%%


handle_error({_Ipaddr, _Port, _UserData}, Reason, State) ->
    {stop, Reason, State}.


%%%%% ------------------------------------------------------- %%%%%


handle_data({raw, _Bytes}, _Socket, State) ->
    {ok, State};
    
    
handle_data({packet, _Bytes}, _Socket, State) ->
    {ok, State};


handle_data({closed, _Bytes}, _Socket, State) ->
    {ok, State};

    
handle_data({error, _Reason, _Bytes}, _Socket, State) ->
    {ok, State}.



%%%%% ------------------------------------------------------- %%%%%


terminate(Reason, #state}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%


