
-module(cowboy_route_provider).

% For use with erlx_frontloader


-callback routes( smart_routes:opts() ) -> smart_routes:routes().

