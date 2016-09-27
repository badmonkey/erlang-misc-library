


%%% AST members %%%

% []    only the empty list
% atom()

%{atom, LINE, L}
%{char, LINE, L}
%{float, LINE, L}
%{integer, LINE, L}
%{string, LINE, string()}

%{attribute, LINE, export, LIST}
%{attribute, LINE, import, TUPLE}
%{attribute, LINE, module, Mod}
%{attribute, LINE, file, TUPLE}
%{attribute, LINE, spec/callback, {TUPLE, list(AST)}}
%{attribute, LINE, record, {Name, list(AST)}}
%{attribute, LINE, type/opaque, {Name, AST, list(AST)}}
%{attribute, LINE, A, T}    for -A(T)
%{ann_type, LINE, [AST, AST]}

%{bin, LINE, list(bin_element)}
%{bin_element, LINE, AST, AST|default, AST|default}
%{bc, LINE, AST, list(AST)}
%{block, LINE, AST}.
%{b_generate, LINE, AST, AST}

%{cons, LINE, AST, AST}
%{'case', LINE, AST, list(AST)}
%{'catch', LINE, AST}
%{call, LINE, AST, list(AST)}
%{call, LINE, {remote, LINE, AST, AST}, list(AST)}
%{clause, LINE, [AST], AST, AST}
%{clause, LINE, [{atom(), AST?, _}], AST, AST}
%{clause, LINE, AST, AST, AST}

%{function, LINE, Name, Arity, list(AST)}
%{'fun', LINE, {function, Name, Arity}}
%{'fun', LINE, {function, AST, AST, AST}}
%{'fun', LINE, {clauses, list(AST)}}

%{generate, LINE, AST, AST}

%{'if', LINE, list(AST)}

%{lc, LINE, AST, list(AST)}

%{match, LINE, AST, AST}
%{map, LINE, list(AST)}
%{map, LINE, AST, list(AST)}
%{map_field_assoc, LINE, AST, AST}
%{map_field_exact, LINE, AST, AST}

%{named_fun, LINE, Name, list(AST)}
%{nil, LINE}

%{op, LINE, Op, AST, AST}
%{op, LINE, Op, AST}.

%{record_index, LINE, Name, Rep(Field)}.
%{record, LINE, Name, list(record_field/4)}
%{record, LINE, AST, Name, list(record_field/4)}
%{record_field, LINE, AST}
%{record_field, LINE, AST, AST}
%{record_field, LINE, AST, Name, AST}
%{remote_type, LINE, [AST, AST, list(AST)]}
%{'receive', LINE, list(AST)}
%{'receive', LINE, list(AST), AST, AST}

%{type, LINE, N, list(AST)}
%{type, LINE, record, list(AST)}
%{type, LINE, tuple, list(AST)|any}
%{type, LINE, union, list(AST)}
%{type, LINE, bounded_fun, list(AST)}
%{type, LINE, 'fun', []}
%{type, LINE, 'fun', [{type, LINE, any}, AST]}
%{type, LINE, 'fun', [{type, LINE, product, list(AST)}, AST]}
%{type, LINE, map_field_assoc, [AST, AST]}
%{type, LINE, map_field_exact, [AST, AST]}
%{type, LINE, field_type, [AST, AST]}
%{type, LINE, binary, [AST, AST]}
%{type, LINE, nil, []}
%{type, LINE, range, [AST, AST]}
%{type, LINE, map, list(AST)|any}
%{typed_record_field, {record_field, LINE, AST}, AST}
%{typed_record_field, {record_field, LINE, AST, AST}, AST}
%{tuple, LINE, list(AST)}.
%{'try', LINE, AST, list(AST), list(AST), AST}

%{user_type, LINE, N, list(AST)}

%{var, LINE, Name}

