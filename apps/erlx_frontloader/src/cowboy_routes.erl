
-module(xcowboy_routes).


-type constraint() :: int | nonempty | fun().

-type fields() :: [ atom()
                  | {atom(), cowboy_constraints:constraint() | [cowboy_constraints:constraint()]}
                  | {atom(), cowboy_constraints:constraint() | [cowboy_constraints:constraint()], any()}
                  ].
                  
                  
-type route_match() :: '_' | iodata().
-type route_path() :: {Path::route_match(), Handler::module(), Opts::any()}
                    | {Path::route_match(), cowboy:fields(), Handler::module(), Opts::any()}.
-type route_rule() :: { Host::route_match()
                      , Paths::[route_path()] }
                    | { Host::route_match()
                      , cowboy:fields()
                      , Paths::[route_path()]}.
-type routes() :: [route_rule()].


-opaque trail() ::
  #{ path_match  => cowboy_router:route_match()
   , constraints => cowboy_router:constraints()
   , handler     => module()
   , options     => any()
   , metadata    => metadata(any())
   }.
   
-type trails() :: [trails:trail() | route_path()].
                  
                  
Handlers =
  [ spts_status_handler
  , spts_games_handler
  , spts_single_game_handler
  , spts_serpents_handler
  , spts_single_serpent_handler
  , spts_news_handler
  ],
Trails =
  [ {"/", cowboy_static, {file, "www/index.html"}}
  , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
  , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
  , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
  | trails:trails(Handlers)
  ],

  
%%%%% ------------------------------------------------------- %%%%%


-type module() :: atom() | {atom(), via, atom()} | {atom(), nodemap, string()}.
-type params() :: tuple() | list().

-type path() :: module()
              | { module(), params() }
              | { string(), module(), params() }
              | { string(), constraints(), module(), params() }
              
              | { string(), include, string }
              | { include, string }
              | { string(), [path()] }
              .
              
-type route_rule() :: { string(), [path()] }
                    | { string(), constraints(), [path()] }.
-type routes() :: [route_rule()].


% 
%[ { "www.monolith.org"
%  , [ {"/", cowboy_static, {file, "www/index.html"}}
%    , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
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
%        , {include, "subdomain_static.cfg"}
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
  
