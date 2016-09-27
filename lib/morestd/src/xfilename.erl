
-module(xfilename).

-export([relativename/1, relativename/2]).



is_relative(Filename) ->
    {ok, CWD} = file:get_cwd(),
    lists:prefix(CWD, Filename).



relativename(Filename) ->
    {ok, CWD} = file:get_cwd(),
    relativename(Filename, CWD).


relativename(Filename, Base) ->
    case lists:prefix(Base, Filename) of
        true -> lists:nthtail( length(Base) + 1, Filename)
    ;   false -> Filename
    end.


join(undefined, Y) -> Y;
join(X, Y) -> filename:join(X, Y).


archivename(Filename) ->
    archive_scan(undefined, filename:split(Filename)).


archive_scan(Path, []) -> {ok, Path};

archive_scan(Path, [X | Rest]) ->
    Tst = join(Path, X),
    case { filelib:is_dir(Tst), filelib:is_regular(Tst) } of
        {true, false} -> archive_scan(Tst, Rest)
    ;   {false, true} -> {ok, Tst, filename:join(Rest)}
    ;   {T, T}        -> {error, no_exists}
    end.

