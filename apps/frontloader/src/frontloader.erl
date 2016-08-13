
-module(frontloader).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("highgarden/include/supervisors.hrl").


-export([start_link/0, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-type ref() :: { http|https, inet:port_number() }.

-record(site,
    {
    }).
    
-record(listener,
    { name
    , ref
    }).


-record(state,
    { name
    , site_order                :: [smart_routes:host_match()]
    , sites                     :: #{ smart_routes:host_match() => #site{} }
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


%%%%% ------------------------------------------------------- %%%%%


listener(Name, Scheme, Port, Acceptors) ->
    ok.
    

update_dispatch(Name, Dispatch) ->
    ok.


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    net_kernel:monitor_nodes(true),
    Nodes = erlang:nodes(), 
    
    {ok, #state{}}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_call(_Request, _From, State) ->
    lager:info("frontloader:call stopped ~p", [_Request]),
    {stop, invalid_call_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
handle_cast(_Msg, State) ->
    lager:info("frontloader:cast stopped ~p", [_Msg]),
    {stop, invalid_cast_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%

    
% {nodeup, Node} | {nodedown, Node}    
handle_info(_Info, State) ->
    lager:info("frontloader:info stopped ~p", [_Info]),
    {stop, invalid_info_request, State}.

    
%%%%% ------------------------------------------------------- %%%%%


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

    
%%%%% ------------------------------------------------------- %%%%%
% Private Functions


