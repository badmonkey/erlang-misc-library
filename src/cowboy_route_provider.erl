
-module(cowboy_route_provider).


-callback routes( smart_routes:opts() ) -> [ smart_routes:route_path() ].

