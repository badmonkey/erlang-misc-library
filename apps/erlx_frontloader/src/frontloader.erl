
-module(frontloader).
-vsn("1.0.0").

-behaviour(gen_server).
-behaviour(supervisor_child).

-define(SERVER, ?MODULE).

-include_lib("erlangx/include/supervisors.hrl").


-export([start_link/0, child_spec/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%%%%% ------------------------------------------------------- %%%%%
% Server State


-record(state,
    { name
    }).

         
%%%%% ------------------------------------------------------- %%%%%
% Public API


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

    
child_spec(Id, _Args) -> ?SERVICE_SPEC(Id, ?MODULE, []).


    
%%%%% ------------------------------------------------------- %%%%%
% Initialise Server


init(_Args) ->
    Dispatch = cowboy_router:compile(
        [ { '_'
          , [ { "/:top/[...]", cowboy_default_handler, ["Bind test"] }
			, { '_', cowboy_default_handler, ["Catch All"] }
            ]
          }
        ]),
    

    {ok, _} = cowboy:start_http( http
                               , 16
                               , [ {port, 8080} ]
                               , [ {env, [{dispatch, Dispatch}]} ]
                               ),
    
    %PrivDir = xcode:priv_dir(erlx_frontloader),
    %lager:info("priv_dir: ~s", [PrivDir]),
    %{ok, _} = cowboy:start_https( https
    %                            , 16
    %                            , [ {port, 8443}
    %                              , {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"}
    %                              , {certfile, PrivDir ++ "/ssl/server.crt"}
    %                              , {keyfile, PrivDir ++ "/ssl/server.key"}
    %                              ]
    %                            , [ {env, [{dispatch, Dispatch}]} ]
    %                            ),
    
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


