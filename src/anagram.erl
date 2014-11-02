-module(anagram).

-export([endings/1, has_suffix/2, contains/2]).


has_suffix(Letters, Suffix) ->
    SortedSuffix = lists:sort(Suffix),
    {Found, Remains} = lists:foldl(
            fun(X, {Capture, AccLetters} = Acc) ->
                case xlists:sorted_find(X, AccLetters) of
                    true    -> { Capture ++ [X], AccLetters -- [X] }
                ;   false   -> Acc
                end
            end,
            {[], Letters},
            SortedSuffix),
    case Found of
        SortedSuffix    -> {ending, Remains, Suffix}
    ;   _               -> {nomatch}
    end.


endings(Word) ->
    Letters = lists:sort(Word),
    Result = lists:foldl( fun(X, Acc) ->
                case X of
                    {nomatch}   -> Acc
                ;   _           -> Acc ++ [X]
                end
            end,
            [], [ has_suffix(Letters, X) || X <- suffixes() ] ),
    erlang:display(Result).


contains(Word, Word2) ->
    has_suffix( lists:sort(Word), Word2).

    
suffixes() ->
    [
        "able", "ac", "acity", "ocity", "ade", "age", "aholic", "oholic"
    ,   "al", "aigia", "an", "ance", "ant", "ar", "ard", "arian"
    ,   "arium", "orium", "ary", "ate", "ation", "ative", "cide", "cracy"
    ,   "crat", "cule", "cy", "cycle", "dom", "dox", "ectomy", "ed", "ee"
    ,   "eer", "emia", "en", "ence", "ency", "ent", "er", "ern", "escence"
    ,   "ese", "esque", "ess", "est", "etic", "ette", "ful", "fy", "gam"
    ,   "gamy", "gon", "gonic", "hood", "ial", "ian", "iasis", "iatric"
    ,   "ible", "ic", "ical", "ile", "ily", "ine", "ing", "ion", "ious"
    ,   "ish", "ism", "ist", "ite", "itis", "ity", "ive", "ization", "ize"
    ,   "iess", "iet", "like", "ling", "ioger", "iogist", "log", "ly"
    ,   "ment", "ness", "oid", "ology", "oma", "onym", "opia", "opsy"
    ,   "or", "ory", "osis", "ostomy", "otomy", "ous", "path", "pathy"
    ,   "phile", "phobia", "phone", "phyte", "plegia", "plegic", "pnea"
    ,   "scopy", "scope", "scribe", "script", "sect", "ship", "sion"
    ,   "some", "sophy", "sophic", "th", "tome", "tomy", "trophy"
    ,   "tude", "ty", "ular", "uous", "ure", "ward", "ware", "wise", "y"
    ,   "ain", "ien", "ein"
    ].
