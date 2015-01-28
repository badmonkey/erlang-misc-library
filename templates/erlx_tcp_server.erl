
-module({{srvid}}),

-behaviour(erlx_tcp_server).
-define(SERVER, ?MODULE).


-export([start_link/1]).

-export([ handle_connection/1, handle_error/3]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2
        , terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state, 
    {
    }).
    

%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link(Port) ->
    erlx_tcp_server:start_link(?MODULE, Port).
    
    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_InitParams) ->
    {ok, #state{}}.


%%%%% ------------------------------------------------------- %%%%%
% Handle Connection


handle_connection({_Ipaddr, _Port, _Socket, _UserData}, State) ->
    erlx_tcp_server:start_link(?MODULE, []).


%%%%% ------------------------------------------------------- %%%%%
% Report Error


handle_error({_Ipaddr, _Port, _UserData}, Reason, State) ->
    {stop, Reason, State}.


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


terminate(Reason, #state}) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%%%% ------------------------------------------------------- %%%%%
% Private Functions


