

Nonterminals    
    rule_Config
    rule_DeclList rule_Decl
    rule_SimpleValueList rule_SimpleValue
    rule_ValueList rule_Value
    rule_String rule_Fragment
    .

Terminals
    bool
    integer
    float
    ipv4_address
    atom
    substring
    variable substitute
    '[' ']' '{' '}' ',' '='
    .

Rootsymbol rule_Config.

Endsymbol '$end'.


rule_Config -> rule_DeclList : '$1'.

rule_DeclList -> rule_Decl rule_DeclList : [ '$1' | '$2' ].
rule_DeclList -> '$empty' : [].

rule_Decl -> atom '=' rule_Value : {prop, value_of('$1'), '$3'}.
rule_Decl -> atom : {prop, value_of('$1'), {bool, true}}.
rule_Decl -> atom rule_SimpleValueList '{' rule_DeclList '}' : {scope, value_of('$1'), '$2', '$4'}.


rule_Value -> rule_SimpleValue : '$1'.
rule_Value -> '[' ']' : {list, []}.
rule_Value -> '[' rule_ValueList ']' : {list, '$2'}.
rule_Value -> '{' rule_ValueList '}' : {tuple, '$2'}.
rule_Value -> variable : {variable, value_of('$1')}.

rule_ValueList -> rule_Value ',' rule_ValueList : [ '$1' | '$3' ].
rule_ValueList -> rule_Value : [ '$1' ].


rule_SimpleValue -> atom : {atom, value_of('$1')}.
rule_SimpleValue -> bool : {bool, value_of('$1')}.
rule_SimpleValue -> integer : {integer, value_of('$1')}.
rule_SimpleValue -> float : {float, value_of('$1')}.
rule_SimpleValue -> ipv4_address : {ipv4, value_of('$1')}.
rule_SimpleValue -> rule_String : {string, '$1'}.

rule_SimpleValueList -> rule_SimpleValue rule_SimpleValueList : ['$1' | '$2'].
rule_SimpleValueList -> '$empty' : [].


rule_String -> rule_Fragment rule_String : '$1' ++ '$2'.
rule_String -> rule_Fragment : '$1'.

rule_Fragment -> substring : value_of('$1').
rule_Fragment -> substitute : value_of('$1').


Erlang code.


value_of(Token) when is_tuple(Token) ->
  element(3, Token);
value_of(Token) ->
   throw({error, {0, ?MODULE, io_lib:format("Bad token ~p", [Token])}}).
   
