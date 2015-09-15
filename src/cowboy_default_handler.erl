
-module(cowboy_default_handler).

-export([init/2]).


init(Req, Opts) ->
	Host = cowboy_req:host(Req),
	Port = cowboy_req:port(Req),
	Path = cowboy_req:path(Req),
	PathInfo = cowboy_req:path_info(Req),
	Peer = cowboy_req:peer(Req),
	Url = cowboy_req:url(Req),
	Headers = cowboy_req:headers(Req),
	Bindings = cowboy_req:bindings(Req),
	
	Text = xstring:format("Hello world!~nHandlerOpts: ~p~nHost/Port/Path: ~p:~p ~p~nUrl: ~p~nInfo: ~p~nPeer: ~p~nHeaders: ~p~nBindings: ~p~n"
						 , [ Opts
						   , Host
						   , Port
						   , Path
						   , Url
						   , PathInfo
						   , Peer
						   , Headers
						   , Bindings
						   ]),
	
	BinText = erlang:list_to_binary(Text),
	
    Req2 = cowboy_req:reply( 200
                           , [ {<<"content-type">>, <<"text/plain">>} ]
                           , <<BinText/binary>>
                           , Req),
    {ok, Req2, Opts}.
    
    