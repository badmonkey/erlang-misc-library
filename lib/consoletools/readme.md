

-module(test_nif).
-export([repeat/2]).
-on_load(init/0).

-define(APPNAME, test_nif).
-define(LIBNAME, test_nif).

repeat(_, _) ->
    not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).


    
----- applecore.lager -----

[  
    
{ lager
, [ { crash_log, false }
  , { colored, true }
  , { colors
    , [ {info,      "\e[0;38m" }
      ]
    }
},

{ lager_event   
, [ { handlers
    , [ { lager_console_backend, error }
      ]
    }
  ]
},

{ console_lager_event 
, [ { handlers
    , [ { lager_console_backend
        , [ debug
          , { lager_default_formatter
            , [ color, message, {eol, "\e[0m\r\n"} ]
            }
          ]
        }
      ]
    }
  , {async_threshold, 150}
  , {async_threshold_window, 5}
  ]
},
        
{ audit_lager_event
, [ { handlers
    , [ { lager_console_backend
        , [ critical
          , { lager_default_formatter
            , [ date, "#", time, "#", severity, "#", node, "#", pid, "#"
              , module, "#", function, "#", file, "#", line, "#", message
              , "\r\n"
              ]
            }
          ]
        } 
      ]
    }
  , {async_threshold, 50}
  , {async_threshold_window, 5}
  ]
},
        
{ error_logger_lager_event
, [ { handlers
    , [ { lager_console_backend, critical }
      ]
    }
  ]
}

]


----- applecore.lager -----


lager
{
    crash_log = false
    colored = true
    setcolor = [ {info, "\e[0;38m" } ]
}

lager_event
{
    handlers
    {
        lager_console_backend
        {
            level = info
        }
    }
}

console_lager_event
{
    handlers
    {
        lager_console_backend
        {
            level = debug
            lager_default_formatter = [ color, message, {eol, "\e[0m\r\n"} ]
        }
    }
    
    async_threshold = 150
    async_threshold_window = 5
}
