
-module(fswatcher).

-behaviour(port_server).
-behaviour(supervisor_child).
-define(SERVER, ?MODULE).


-export([start_link/0, port_info/0, handle_port/2, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%


child_spec(Id, _Args) ->
    {Id, {fswatcher, start_link, []}, permanent, 5000, worker, [fswatcher]}.

         
%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    {
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    port_server:start_link(?SERVER, ?MODULE, []).

    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    {ok, #state{}}.

    
%%%%% ------------------------------------------------------- %%%%%


port_info() ->
    [{driver, {"fswatcher", []}}].
    

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

    
handle_port(Msg, State) ->
    erlang:display({from_port, Msg}),
    {ok, State}.
    
    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


