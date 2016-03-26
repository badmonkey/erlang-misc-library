
Project consoleapp 1.0.0


    
main(Args) ->
    App = consoleapp:build(
            [ {appname, "forge"}
            , {version, "1.0.0"}
            , {option, quiet, boolean, $q, "Help string"}
            , {option, count, {integer, 2}, "count", "Count string", fun set_count/1}
            , {option, verbose, integer, {$v, "verbose"}, "Verbosity", fun set_param/2}
%            , {positional, config, string, "Config file", fun process_cfg/1}
            , {command, "create", module_create}            %% calls module_create(App, Args)
            , {command, ["rm, "remove"], module_remove}
            , {command, "ls", fun handle_ls/2}              %% calls handle_ls(App, Args)
            , {command, undefined, fun main2/1}
            ]),  
                  
    App2 = consoleapp:process(App, Args).    


module_create:main(App, Args) ->
    SubApp = consoleapp:build(App,
                [ {option, boolean, list_templates, "list", "List templates"}
                , {positional, string, name, "Name of template"}
                ]),
            
    SubApp2 = consoleapp:process(SubApp, Args),

    Map = consoleapp:get_args_as_map(SubApp2),
    Arglist = consoleapp:get_args_as_proplist(SubApp2),
    TemplateArgs = console_app:get_unused_args(SubApp2),
    ExeName = consoleapp:get_command(SubApp2),     % "appname", "appname create", "appname create filter" etc
    
    consoleapp:usage(SubApp2),
    consoleapp:usage(SubApp2, "[var = value ...]"),
    
    expand_template(Map, TemplateArgs).

forge --version
forge --help
forge [-q] [--count=2] [-v | --verbose N] [command...]
forge create --help
forge create <template> <args>


appname [options] command * [options] [command] * [options] [commands] *
    

Usage: appname [-q] [--count N] [-v | --verbose X] [command ...]
  command               Command to execute (create, rm, remove, ls)

Usage: appname [options] create [--help] [--list] <name> [var=value ...]
    
    
    
