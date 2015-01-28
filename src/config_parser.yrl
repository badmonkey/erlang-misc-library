

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


rule_Config -> rule_DeclList : {scope, '.', '$1'}.

rule_DeclList -> rule_Decl rule_DeclList : ['$1' | '$2'].
rule_DeclList -> '$empty' : [].

rule_Decl -> atom '=' rule_Value : {property, value_of('$1'), '$3'}.
rule_Decl -> atom : {property, value_of('$1'), {bool, true}}.
rule_Decl -> atom '{' rule_DeclList '}' : {scope, value_of('$1'), '$3'}.


rule_Value -> atom : {atom, value_of('$1')}.
rule_Value -> bool : {bool, value_of('$1')}.
rule_Value -> integer : {integer, value_of('$1')}.
rule_Value -> float : {float, value_of('$1')}.
rule_Value -> ipv4_address : {ipaddress, value_of('$1')}.
rule_Value -> rule_String : {string, '$1'}.
rule_Value -> '[' ']' : {list, []}.
rule_Value -> '[' rule_ValueList ']' : {list, '$2'}.
rule_Value -> '{' rule_ValueList '}' : {tuple, '$2'}.
rule_Value -> variable : {variable, value_of('$1')}.
rule_Value -> rule_FilePath : {path, '$1'}.

rule_ValueList -> rule_Value ',' rule_ValueList : ['$1' | '$3'].
rule_ValueList -> rule_Value : [ '$1' ].


rule_String -> rule_Fragment rule_String : ['$1' | '$2'].
rule_String -> rule_Fragment : ['$1'].

rule_Fragment -> substring : value_of('$1').
rule_Fragment -> stringvar : {variable, value_of('$1')}.


rule_FilePath -> anchor rule_PathList : [{anchor, value_of('$1')} | '$2'].
rule_FilePath -> variable rule_PathList : [{variable, value_of('$1')} | '$2'].
rule_FilePath -> rule_PathList : '$1'.

rule_PathList -> rule_PathItem rule_PathList : ['$1' | '$2'].
rule_PathList -> rule_PathItem : ['$1'].

rule_PathItem -> pathvar : {variable, value_of('$1')}.
rule_PathItem -> pathfrag : value_of('$1').


Erlang code.


value_of(Token) when is_tuple(Token) ->
  element(3, Token);
value_of(Token) ->
    return_error(0, io_lib:format("Bad token ~p", [Token])).
   
