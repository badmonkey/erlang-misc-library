
-module(smart_routes).

-export([resolve_routes/2, resolve_routes/3]).


%%%%% ------------------------------------------------------- %%%%%


%% parts copied from cowboy_router.erl
-type route_match() :: '_' | iodata().
-type host_match() :: '_' | iodata().

-type opts() :: tuple() | #{} | list() | undefined.

-type route_handler() :: module().
-type route_provider() :: module() | { module(), via, node() }.
                    
-type path() ::   route_provider()
              | { route_provider(), Opts::opts() }
              
              | { Path::route_match(), Paths::routes() }
              
              | { Path::route_match(), Handler::route_handler(), Opts::opts() }
              | { Path::route_match(), cowboy:fields(), Handler::route_handler(), Opts::opts()}
              .
              
-type routes() :: [ path() ].              
        
    
-type rule() :: { Host::host_match(), Paths::routes() }
              | { Host::host_match(), route_provider(), opts() }
              | { Host::host_match(), cowboy:fields(), Paths::routes() }    %% ambiguous?
              .

-type sites() :: [ rule() ].


% processed paths for use with cowboy
-type route_path() :: { Path::route_match(), Handler::module(), Opts::any() }
                    | { Path::route_match(), cowboy:fields(), Handler::module(), Opts::any() }. 

-export_type([ route_path/0
             , sites/0, rule/0
             , routes/0, path/0
             , route_handler/0, route_provider/0, opts/0
             , host_match/0, route_match/0]).

              
              
%%%%% ------------------------------------------------------- %%%%%


-spec resolve_routes( route_match(), route_handler(), opts() ) -> [ route_path() ] | type:error().

resolve_routes(Modules, Opts) ->
    resolve_routes("/", Modules, Opts).

    
resolve_routes('_', Modules, Opts) ->
    {error, invalid_prefix};
    
resolve_routes(Prefix, Modules, Opts) ->
    ok.
    

% 
%[ { "www.monolith.org"
%  , [ {"/", cowboy_static, {file, "www/index.html"}}
%    , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
%    , {"/stream", person_api_handler, ["some params"]}
%    , {"/game/:game_id", {notempty, ":game_id"}, cowboy_static, {file, "www/game.html"}}
%    , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
%    , warbeard_status_handler
%    , {"/overlay", include, "somefile.cfg"}
%    ]
%  }
%  
%, { ":subdomain.monolith.org"
%  , [ {"/", xcowboy_server_status}
%    , {some_module, via, 'remote@node1'}
%    , { "/:subdomain"
%      , [ { xcowboy_virthost_status, [":subdomain"]}
%        , { "/api", {some_remote_module, via, 'remote@node2'}, [some_value, "red"]}
%        ]
%      }
%    , {magic_module, []}
%    ]
%  }
%  
%, { any
%  , [ xcowboy_404_handler
%    ]
%  }
%}
%
  
