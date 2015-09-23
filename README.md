erlang-misc-library
===================

Library of misc. functions for erlang







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
, "cowboy:fields"
, [
  ]
}.


