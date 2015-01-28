
-module({{srvid}}),

-behaviour(erlx_packet_processor).
-define(SERVER, ?MODULE).


-export([init/2, handle_data/2]).
-export([ handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).
        

%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state, 
    {
    }).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(Socket, _InitParams) ->
    {ok, #state{}, raw}.


%%%%% ------------------------------------------------------- %%%%%
% Process data


handle_data({raw, _Bytes}, State) ->
    {ok, State};
    
    
handle_data({packet, _Bytes}, State) ->
    {ok, State};


handle_data({closed, _Bytes}, State) ->
    {ok, State};

    
handle_data({error, _Reason, _Bytes}, State) ->
    {ok, State}.



%%%%% ------------------------------------------------------- %%%%%


handle_call(_Request, _From, State) ->
    {stop, invalid_call_request, State}.
    
    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    {stop, invalid_case_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_info(_Info, State) ->
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions



