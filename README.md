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

    
    
    
    
    
{ listener
, Name::atom()
, SchemePorts::one_or_many( {http|https, natural()} )
}.

{ listener
, Name::atom()
, Options::one_or_many( {atom(), any()} )
, SchemePorts::one_or_many( {http|https, natural()} )
}.

{ site
, Host::string()
, ListenerName::one_or_many( atom() )
, PathList::[route_path()]
}.




{ listener, mainserver
, [ {http, 80}
  , {https, 443}
  ]
}.

{ listener, devserver
, [ {acceptors, 10} ]
, [ {http, 8080}
  , {https, 8443}
  ]
}.



{ site, "warbeard.monolith.one", mainserver
, [
  ]
}.


{ site, "something.monolith.one", [mainserver, devserver]
, [
  ]
}.
