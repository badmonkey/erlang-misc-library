
-module(chimera).

-export([parse_transform/2]).


-record( state
       , { relays   = []        :: [{atom(), atom(), non_neg_integer(), pos_integer()}]
         } ).
         
         

parse_transform(ParseTree, _Options) ->
    {NewState, NewTree}     = stdforms:mapreduce(fun do_pass_1/3, #state{}, ParseTree),
    {NewState2, NewTree2}   = stdforms:mapreduce(fun do_pass_2/3, NewState, NewTree),
    
    NewRelayForms = [ pt_export_from:make_relay(X) || X <- NewState#state.relays ],
    
    NewTree ++ NewRelayForms.

    
%%%%% ------------------------------------------------------- %%%%%   
%
% Handle forms and macro control structures
%

% pt_otherwise
%    
do_pass_1(case_expr, CaseNode, #state{} = State) ->
    { pt_otherwise:do_transform(case_expr, CaseNode), State };
    
do_pass_1(if_expr, IfNode, #state{} = State) ->
    { pt_otherwise:do_transform(case_expr, IfNode), State };
    
    
% pt_export_from
%    
do_pass_1(attribute, {'attribute', _, 'export_from', _} = Node, #state{} = State) ->
    {Node, Relays} = pt_export_from:do_transform(attribute, Node, State#state.relays),
    
    { Node
    , State#state{ relays = lists:append(State#state.relays, NewRelays)
                 }
    };

    
% otherwise    
do_pass_1(_, _, #state{} = State) ->
    {continue, State}.


%%%%% ------------------------------------------------------- %%%%%
%
% Handle expressions
%

% pt_codegen
% 
do_pass_2(application, {call, _, {remote,_,{atom, _, 'abstract'},{atom, _, 'quote'}}, _} = Node, #state{} = State) ->
    { pt_codegen:do_transform(Node), State };

    
% pt_with
%    
do_pass_2(list_comp, {lc, _, {atom, _, 'with'}, _} = Node, #state{} = State) ->
    { pt_with:do_transform(list_comp, Node), State };
    
do_pass_2(list_comp, {lc, _, {var, _, 'WITH'}, _} = Node, #state{} = State) ->    
    { pt_with:do_transform(list_comp, Node), State };
    
    
% otherwise    
do_pass_2(_, _, #state{} = State) ->
    {continue, State}.


%%%%% ------------------------------------------------------- %%%%%
    