
Definitions.

NL          = (\r?\n)
WS          = [\t\s]
DIGIT       = [0-9]
DEC256      = (25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])
FLOAT       = ([0-9]+\.[0-9]+([eE][-+]?[0-9]+)?)
SPECIAL     = [\[\]\{\}\,\=\;]
ATOM        = (([a-z][a-zA-Z0-9@_]*)|([\'][^\']+[\']))
VAR_BEGIN   = \$\{
VAR_END     = \}
QUOTE       = \"
SIMPLE      = [^\"\$\\]
PATH        = \/[a-zA-Z0-9_\.\-]*
ANCHOR      = [A-Z_]


Rules.


#.*{NL}                                         : skip_token.
{NL}+                                           : skip_token.
{WS}+                                           : skip_token.

ok|true|yes                                     : ?TOKEN(bool, TokenLine, true).
false|no                                        : ?TOKEN(bool, TokenLine, false).

{DIGIT}+                                        : ?TOKEN(integer, TokenLine, fun erlang:list_to_integer/1, TokenChars).
{FLOAT}                                         : ?TOKEN(float, TokenLine, fun erlang:list_to_float/1, TokenChars).
{DEC256}\.{DEC256}\.{DEC256}\.{DEC256}          : ?TOKEN(ipv4_address, TokenLine, TokenChars).
{ATOM}                                          : ?TOKEN(atom, TokenLine, fun erlang:list_to_atom/1, TokenChars).
{VAR_BEGIN}{ATOM}(\.{ATOM})*{VAR_END}           : ?TOKEN(variable, TokenLine, strip_var(TokenChars)).
{SPECIAL}                                       : ?FORWARD_TOKEN(TokenLine, TokenChars).

{QUOTE}{QUOTE}                                  : ?TOKEN(substring, TokenLine, "").
{QUOTE}{VAR_BEGIN}{ATOM}(\.{ATOM})*{VAR_END}    : ?TOKEN_PUSH(stringvar, TokenLine, "\"", strip_var(strip_junk(TokenChars))).
{QUOTE}{VAR_BEGIN}                              : ?TOKEN_PUSH(substring, TokenLine, "\"", "${").
{QUOTE}\\.                                      : ?TOKEN_PUSH(substring, TokenLine, "\"", [map_escaped_char(strip_junk(TokenChars))]).
{QUOTE}{SIMPLE}*                                : ?TOKEN_PUSH(substring, TokenLine, "\"", strip_junk(TokenChars)).

{ANCHOR}+                                       : ?TOKEN(anchor, TokenLine, TokenChars).
{PATH}                                          : ?TOKEN(pathfrag, TokenLine, TokenChars).
\/{VAR_BEGIN}{ATOM}(\.{ATOM})*{VAR_END}         : ?TOKEN(pathvar, TokenLine, strip_var(strip_junk(TokenChars))).


Erlang code.


-define(TOKEN(Type, Line, Value), make_token(Type, Line, Value, Value)).
-define(TOKEN(Type, Line, Value, Text), make_token(Type, Line, Text, Value)).
-define(FORWARD_TOKEN(Line, Text), {token, {list_to_atom(Text), Line}}).
-define(TOKEN_PUSH(Type, Line, Push, Text), {token, {Type, Line, Text}, Push}).


make_token(Type, Line, Text, Fun) when is_function(Fun, 1) ->
    {token, {Type, Line, Fun(Text)}};
    
make_token(Type, Line, _Text, Value) ->
    {token, {Type, Line, Value}}.
    

strip_junk([$\" | Rest]) -> strip_junk(Rest);
strip_junk([$\\ | Rest]) -> strip_junk(Rest);
strip_junk([$\/ | Rest]) -> strip_junk(Rest);
strip_junk(X) -> X.


strip_var([$\$, ${ | Rest]) ->
    lists:reverse( strip_var_end( lists:reverse(Rest) ) );
    
strip_var(X) -> X.


strip_var_end([$} | Rest]) -> Rest;
strip_var_end(X) -> X.


map_escaped_char(Escaped) when is_list(Escaped) ->
    map_escaped_char(hd(Escaped));

map_escaped_char(Escaped) ->
    case Escaped of
        $\\ -> $\\;
        $/  -> $/;
        $\" -> $\";
        $\' -> $\';
        $\( -> $(;
        $b  -> $\b;
        $d  -> $\d;
        $e  -> $\e;
        $f  -> $\f;
        $n  -> $\n;
        $r  -> $\r;
        $s  -> $\s;
        $t  -> $\t;
        $v  -> $\v;
        X   -> X
    end.
  