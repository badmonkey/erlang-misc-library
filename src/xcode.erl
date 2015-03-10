
-module(xcode).

-export([priv_dir/1]).


%%%%% ------------------------------------------------------- %%%%%


priv_dir(App) ->
    case code:priv_dir(App) of
        {error, bad_name}   ->
            {ok, Cwd} = file:get_cwd(),
            filename:join(Cwd, "priv")
            
    ;   Priv                -> Priv
    end.

