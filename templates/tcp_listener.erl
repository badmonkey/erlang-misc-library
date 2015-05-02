
-module({{listenerid}}),
-vsn("{{version}}").

-behaviour(tcp_listener).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/1, child_spec/2]).

-export([ handle_connection/3, handle_error/3]).
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
    tcp_listener:start_link(?MODULE, Port).
    
    
child_spec(Id, Args) -> ?SERVICE_SPEC(Id, ?MODULE, Args).

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_InitParams) ->
    {ok, #state{}}.


%%%%% ------------------------------------------------------- %%%%%
% Handle Connection


handle_connection({_Local, _Remote, _Socket}, _UserData, _State) ->
    packet_processor:start_link(REPLACE, []).


%%%%% ------------------------------------------------------- %%%%%
% Process Error


%handle_error({Endpoint, UserData}, {start_listener, Reason}, State)
%handle_error({Endpoint, UserData}, {copy_sockopts, Reason}, State)
%handle_error({Endpoint, UserData}, {async_accept, Reason}, State)
%handle_error({Local, Remote, ClientSocket}, {handle_connection, Reason}, State)

handle_error(_Who, Reason, State) ->
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


