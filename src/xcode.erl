
-module(xcode).

-export([is_app_file/2, search_for_file/3]).
-export([priv_dir/1, config_dir/1, data_dir/1, data_dir/2]).


%%%%% ------------------------------------------------------- %%%%%


app_base_dir(App)
        when is_atom(App)  ->
    case code:lib_dir(App, priv) of
        {error, bad_name}   ->
            {ok, Cwd} = file:get_cwd(),
            filename:join(Cwd, priv)
            
    ;   Dir                 -> Dir
    end.

    
app_subdir(App, Subdir) ->
        when is_atom(Subdir)  ->
    filename:join( app_base_dir(App), Subdir ).

app_subdir(App, Subdirs) ->
        when is_list(Subdirs)  ->
    filename:join([app_base_dir(App) | Subdirs]).


is_app_file(App, Subparts)
        when is_list(Subparts)  ->    
    FilePath = app_subdir(App, Subparts),
    case filelib:is_regular(FilePath) of
        true    -> FilePath
    ;   _       -> undefined
    end;
    
is_app_file(App, Name) ->
    is_app_file(App, [Name]).

    
%%%%% ------------------------------------------------------- %%%%%
    

% App/
%  |-- priv/
%        |-- config/
%        |-- data/
%              |-- Type1/
%              |-- TypeN/


priv_dir(App)   -> app_base_dir(App).

config_dir(App) -> app_subdir(App, config).
data_dir(App)   -> app_subdir(App, data).

data_dir(App, Type)
        when is_atom(Type)  ->
    app_subdir(App, [data, Type]).


%%%%% ------------------------------------------------------- %%%%%
    

search_for_file(Name, _, []) ->
    undefined;
    
search_for_file(Name, Subdirs, [Hd | Rest])
        when  is_list(Subdirs)  ->
    case search_for_file(Name, Subdirs, Hd) of
        undefined   -> search_for_file(Name, Subdirs, Rest)
    ;   X           -> X
    end;
    
search_for_file(Name, [], App)
        when  is_atom(App)  ->
    app_file(App, [Name]);
    
search_for_file(Name, [Hd | Rest], App)
        when  is_atom(App)  ->
    case app_file(App, [Hd, Name]) of
        undefined   -> search_for_file(Name, Rest, App)
    ;   X           -> X
    end;
    
search_for_file(Name, Subdir, Apps)
        when  is_atom(Subdir)  ->
    search_for_file(Name, [Subdir], Apps).
    
    