
-module(xcode).

-export([is_app_file/2, search_for_file/3, find_executable/2]).
-export([priv_dir/1, config_dir/1, data_dir/1, data_dir/2]).


-type applist_type() :: atom() | [atom()].
-type filelist_type() :: atom() | file:filename() | [atom() | file:filename()].



%%%%% ------------------------------------------------------- %%%%%


-spec app_base_dir( atom() ) -> file:filename().

app_base_dir(App) ->
    app_dir(App, priv).
    
    
app_dir(App, Subdir)
        when  is_atom(App)
            , is_atom(Subdir)  ->
    case code:lib_dir(App, Subdir) of
        {error, bad_name}   ->
            {ok, Cwd} = file:get_cwd(),
            filename:join(Cwd, Subdir)
            
    ;   Dir                 -> Dir
    end.


%%%%% ------------------------------------------------------- %%%%%


-spec app_subdir( atom(), filelist_type() ) -> file:filename().
    
app_subdir(App, Subdir)
        when is_atom(Subdir)  ->
    filename:join( app_base_dir(App), Subdir );

app_subdir(App, Subdirs)
        when is_list(Subdirs)  ->
    filename:join([app_base_dir(App) | Subdirs]).


%%%%% ------------------------------------------------------- %%%%%


-spec is_app_file( atom(), filelist_type() ) -> non_existing | file:filename().

is_app_file(App, Subparts)
        when is_list(Subparts)  ->    
    FilePath = app_subdir(App, Subparts),
    case filelib:is_regular(FilePath) of
        true    -> FilePath
    ;   _       -> non_existing
    end;
    
is_app_file(App, Name) ->
    is_app_file(App, [Name]).

    
%%%%% ------------------------------------------------------- %%%%%
    

%
% App/
%  |-- ebin/
%  |-- priv/
%        |-- config/
%        |-- data/
%              |-- Type1/
%              |-- TypeN/
%


ebin_dir(App)   -> app_dir(App, ebin).
priv_dir(App)   -> app_base_dir(App).

config_dir(App) -> app_subdir(App, config).
data_dir(App)   -> app_subdir(App, data).

data_dir(App, Type)
        when is_atom(Type)  ->
    app_subdir(App, [data, Type]).


%%%%% ------------------------------------------------------- %%%%%
    

-spec search_for_file( file:filename(), applist_type(), applist_type() ) -> non_existing | file:filename().

search_for_file(_, _, []) ->
    non_existing;
    
search_for_file(Name, Subdirs, [Hd | Rest])
        when  is_list(Subdirs)  ->
    case search_for_file(Name, Subdirs, Hd) of
        non_existing    -> search_for_file(Name, Subdirs, Rest)
    ;   X               -> X
    end;
    
search_for_file(Name, [], App)
        when  is_atom(App)  ->
    is_app_file(App, [Name]);
    
search_for_file(Name, [Hd | Rest], App)
        when  is_atom(App)  ->
    case is_app_file(App, [Hd, Name]) of
        non_existing    -> search_for_file(Name, Rest, App)
    ;   X               -> X
    end;
    
search_for_file(Name, Subdir, Apps)
        when  is_atom(Subdir)  ->
    search_for_file(Name, [Subdir], Apps).


%%%%% ------------------------------------------------------- %%%%%


-spec find_executable( applist_type(), file:filename() ) -> non_existing | file:filename().

find_executable(undefined, Name) ->
    case os:find_executable(Name) of
        false   -> non_existing
    ;   Else    -> Else
    end;
    

% test for
%  App/ebin/Name
%  App/priv/bin/Name
%  App/priv/Name
find_executable(App, Name) when is_atom(App) ->
    FilePath = filename:join( ebin_dir(App), Name ),
    case filelib:is_regular(FilePath) of
        true    -> FilePath
    ;   _       -> search_for_file(Name, [bin], App)
    end;

    
find_executable([], _Name) ->    
    non_existing;
    
find_executable([Hd | Rest], Name) ->
    case find_executable(Hd, Name) of
        non_existing    -> find_executable(Rest, Name)
    ;   Else            -> Else
    end;

find_executable(_, _) -> non_existing.


