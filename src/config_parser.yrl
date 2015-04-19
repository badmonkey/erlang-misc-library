

Nonterminals    
    rule_Config
    rule_DeclList rule_Decl
    rule_ValueList rule_Value
    rule_String rule_Fragment
    rule_FilePath rule_PathList rule_PathItem
    .

Terminals
    bool
    integer
    float
    ipv4_address
    atom
    substring
    variable stringvar pathvar
    anchor pathfrag
    '[' ']' '{' '}' ',' '='
    .

Rootsymbol rule_Config.

Right 100 '='.


rule_Config -> rule_DeclList            : '$1'.

rule_DeclList -> rule_Decl rule_DeclList        : ['$1' | '$2'].
rule_DeclList -> '$empty'               : [].

rule_Decl -> atom '=' rule_Value        : ?NAMETYPEVALUE('$1', '$3').
rule_Decl -> atom                       : ?NAMETYPEVALUE('$1', bool, true).
rule_Decl -> atom '{' rule_DeclList '}' : ?NAMETYPEVALUE('$1', group, '$3').


rule_Value -> atom                      : ?TYPEDVALUE(atom, ?TOKEN('$1')).
rule_Value -> bool                      : ?TYPEDVALUE(bool, ?TOKEN('$1')).
rule_Value -> integer                   : ?TYPEDVALUE(integer, ?TOKEN('$1')).
rule_Value -> float                     : ?TYPEDVALUE(float, ?TOKEN('$1')).
rule_Value -> ipv4_address              : ?TYPEDVALUE(ipaddress, ?TOKEN('$1')).
rule_Value -> rule_String               : ?TYPEDVALUE(string, '$1').
rule_Value -> '[' ']'                   : ?TYPEDVALUE(list, []).
rule_Value -> '[' rule_ValueList ']'    : ?TYPEDVALUE(list, '$2').
rule_Value -> '{' rule_ValueList '}'    : ?TYPEDVALUE(tuple, erlang:list_to_tuple('$2')).
rule_Value -> variable                  : ?VARIABLE('$1').
rule_Value -> rule_FilePath             : ?TYPEDVALUE(path, '$1').

rule_ValueList -> rule_Value ',' rule_ValueList : ['$1' | '$3'].
rule_ValueList -> rule_Value                    : ['$1' | []].


rule_String -> rule_Fragment rule_String        : ['$1' | '$2'].
rule_String -> rule_Fragment                    : ['$1' | []].

rule_Fragment -> substring              : value_of('$1').
rule_Fragment -> stringvar              : ?VARIABLE('$1').


rule_FilePath -> anchor rule_PathList   : [?TYPEDVALUE(anchor, '$1') | '$2'].
rule_FilePath -> variable rule_PathList : [?VARIABLE('$1') | '$2'].
rule_FilePath -> rule_PathList          : '$1'.

rule_PathList -> rule_PathItem rule_PathList    : ['$1' | '$2'].
rule_PathList -> rule_PathItem                  : ['$1' | []].

rule_PathItem -> pathvar                : ?VARIABLE('$1').
rule_PathItem -> pathfrag               : value_of('$1').


Erlang code.


-define(CFG_FORMAT_VER, 1.0).

-define(TOKEN(T), value_of(T)).

-define(TYPEDVALUE(T,V), {T, V}).

-define(NAMETYPEVALUE(N,X),{value_of(N), X}).
-define(NAMETYPEVALUE(N,T,V),{value_of(N), ?TYPEDVALUE(T,V)}).

-define(VARIABLE(N), ?TYPEDVALUE(variable, value_of(N))).


value_of(Token) when is_tuple(Token)  ->
    element(3, Token);
    
value_of(List) when is_list(List)  ->
    List;
    
value_of(Token) ->
    return_error(0, io_lib:format("Bad token ~p", [Token])).
   
