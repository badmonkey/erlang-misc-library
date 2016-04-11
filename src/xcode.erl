
-module(xcode).

-export([ search_for_file/3, find_all_files/3]).
-export([ ebin_dir/0, ebin_dir/1, priv_dir/0, priv_dir/1
        , config_dir/0, config_dir/1, data_dir/0, data_dir/1
        , data_dir_for/1]).
-export([ is_app_file/1, is_app_file/2, app_subdir/1, app_subdir/2]).


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


app_subdir(Subparts) ->
    app_subdir(application:get_application(), Subparts).


-spec app_subdir( atom(), filelist_type() ) -> file:filename().
    
app_subdir(App, Subdir)
        when is_atom(Subdir)  ->
    filename:join( app_base_dir(App), Subdir );

app_subdir(App, Subdirs)
        when is_list(Subdirs)  ->
    filename:join([app_base_dir(App) | Subdirs]).


%%%%% ------------------------------------------------------- %%%%%


is_app_file(Subparts) ->
    is_app_file(application:get_application(), Subparts).
    

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
%  |     |-- bin/
%  |     |-- config/
%  |-- data/
%        |-- mnesia/
%        |-- log/
%        |-- otherX/
%


ebin_dir()      -> app_dir(application:get_application(), ebin).
ebin_dir(App)   -> app_dir(App, ebin).

priv_dir()      -> app_base_dir(application:get_application()).
priv_dir(App)   -> app_base_dir(App).

config_dir()    -> app_subdir(application:get_application(), config).
config_dir(App) -> app_subdir(App, config).

data_dir()      -> app_dir(application:get_application(), data).
data_dir(App)   -> app_dir(App, data).

data_dir_for(Type)
        when is_atom(Type)  ->
    filename:join([data_dir(), Type]).


%%%%% ------------------------------------------------------- %%%%%
    

-spec search_for_file( file:filename(), type:atomlist(), type:atomlist() ) -> non_existing | file:filename().

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


-spec find_all_files( file:filename(), type:atomlist(), type:atomlist() ) -> [file:filename()].

find_all_files(_, _, []) ->
    [];
    
find_all_files(Name, Subdirs, [Hd | Rest])
        when  is_list(Subdirs)  ->
    find_all_files(Name, Subdirs, Hd) ++ find_all_files(Name, Subdirs, Rest);

    
find_all_files(Name, [], App)
        when  is_atom(App)  ->
    case is_app_file(App, [Name]) of
        non_existing    -> []
    ;   X               -> [X]
    end;
    
find_all_files(Name, [Hd | Rest], App)
        when  is_atom(App)  ->
    case is_app_file(App, [Hd, Name]) of
        non_existing    -> find_all_files(Name, Rest, App)
    ;   X               -> [X | find_all_files(Name, Rest, App)]
    end;

    
find_all_files(Name, Subdir, Apps)
        when  is_atom(Subdir)  ->
    find_all_files(Name, [Subdir], Apps).

