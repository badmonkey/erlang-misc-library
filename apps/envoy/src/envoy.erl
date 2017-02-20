
-module(envoy).

-export([main/1, debug_hook/1, custom_404_hook/4]).

 
%%%%% ------------------------------------------------------- %%%%%


serve_local(App, Args) ->
    Spec = cmdline:build( App,
            [ {version, "0.1"}
            , {positional, wwwroot, string, "Directory root"}
            ]),
    Result = cmdline:process(Spec, Args),
    Global = cmdline:get_global_options(Result),
    Options = cmdline:get_options(Result),
    
    Port = proplists:get_value(port, Global),
    WWWroot = filename:absname( proplists:get_value(wwwroot, Options, "./") ),
    
    io:format("Mapping http://localhost:~p/[...]  to  ~s/...~n", [Port, WWWroot]),
    
    Routes =
        [ { '_'
          , [ { "/[...]"
			  , cowboy_static
			  , {dir, WWWroot, [ {mimetypes, cow_mimetypes, all}
							   , {dir_handler, directory_handler}
							   ]
				}
			  }
            ]
          }
        ],
        
    Dispatch = cowboy_router:compile(Routes),
    
    io:format("Starting ~p~n~n", [application:ensure_all_started(envoy)] ),

    cowboy:start_http( envoy_listener
                     , 16
                     , [ {port, Port} ]
                     , [ { env, [ {dispatch, Dispatch}
								]
						 }
					   , {middlewares, [cowboy_router, directory_lister, cowboy_handler]}
%					   , {onrequest, fun ?MODULE:debug_hook/1}
%					   , {onresponse, fun ?MODULE:custom_404_hook/4}
                       ]
					 ),
    
    cmdline:loop_forever(Result).
 

%%%%% ------------------------------------------------------- %%%%%


debug_hook(Req) ->
	erlang:display(Req),
	Req.


%%%%% ------------------------------------------------------- %%%%%


custom_404_hook(404, Headers, <<>>, Req) ->
    Body = <<"envoy: 404 Not Found.">>,

    Headers2 = lists:keyreplace( <<"content-length">>, 1, Headers
							   , { <<"content-length">>
								 , integer_to_list(byte_size(Body))
								 }
							   ),

    {ok, Req2} = cowboy_req:reply(404, Headers2, Body, Req),
    Req2;

custom_404_hook(_, _, _, Req) ->
    Req.


%%%%% ------------------------------------------------------- %%%%%


random_string(Length, AllowedChars) ->
    AllowLen = length(AllowedChars),
    lists:foldl(
        fun(_, Acc) ->
            Char = lists:nth(rand:uniform(AllowLen), AllowedChars),
            [ Char | Acc]
        end,
        [], lists:seq(1, Length)).
        

main(Args) ->
    Cookie = "envoy_" ++ random_string(8, "abcdefghijklmnopqrstuvwxwzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"),
    
    Spec = cmdline:build(
            [ {appname, "envoy"}
            , {version, "1.0"}
            , {option, port, {integer, 8080}, {$p, "port"}, "Port number to listen on"}
            , {option, cookie, {string, Cookie}, "cookie", "Set the cookie"}
            , {option, node, undefined, "node", "Connect to another envoy and share it's listeners"}
            , {command, ["serve", "http"], fun serve_local/2, "serve local files"}
            ]),

    Result = cmdline:process(Spec, Args),
    
    cmdline:application_halt(Result, 0).
