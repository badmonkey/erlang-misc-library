erlang-misc-library
===================

Library of misc. functions for erlang




xcode - better data_dir, subdirs for mnesia and log
table_service - use data/mnesia dir


-include_lib("kernel/include/file.hrl").

protect_file(File)-> 
    {_,File_info} = file:read_file_info(File),
    file:write_file_info(File,File_info#file_info{access = read,mode = 33060}).

unprotect_file(File)->
    {_,File_info} = file:read_file_info(File),
    file:write_file_info(File,File_info#file_info{access = read_write,mode = 33206}).

    
    
    
    
-type options() :: { http|https, inet:port_number() }
                 | { acceptors, type:natural() }
                 .
                 
{ listener
, Name::atom()
, Options::one_or_many( options() )
}.

{ site
, Host::smart_routes:host_match()
, ListenerName::atom()
, PathList::smart_routes:routes()
}.

{ site
, Host::smart_routes:host_match()
, ListenerName::atom()
, smart_routes:route_provider(), smart_routes:opts()
}.

{ site
, Host::smart_routes:host_match()
, ListenerName::atom()
, constraint, cowboy:fields()
, PathList::smart_routes:routes()
}.



{ listener, mainserver
, [ {http, 80}
  , {https, 443}
  ]
}.

{ listener, devserver
, [ {acceptors, 10}
  , {http, 8080}
  , {https, 8443}
  ]
}.



{ site, "warbeard.monolith.one"
, mainserver
, [
  ]
}.


{ site, "something.monolith.one"
, devserver
, [
  ]
}.

{ site, "wiki.monolith.one"
, mainserver
, wiki_routes, {some, options}
}.

{ site, "something.monolith.one"
, devserver
, constraint, "cowboy:fields"
, [
  ]
}.


    Dispatch = cowboy_router:compile(
        [ { '_'
          , [ { "/:top/[...]", cowboy_debug_handler, ["Bind test"] }
            , { '_', cowboy_debug_handler, ["Catch All"] }
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
    

