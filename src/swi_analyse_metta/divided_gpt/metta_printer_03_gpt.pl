

pp_sexi(V):- is_final_write(V),!.
pp_sexi(V):- is_dict(V),!,print(V).
pp_sexi((USER:Body)) :- USER==user,!, pp_sex(Body).
pp_sexi(V):- allow_concepts,!,with_concepts('False',pp_sex(V)),flush_output.
pp_sexi('Empty') :- !.
pp_sexi('') :- !, writeq('').
% Handling more cases for 'pp_sex', when the value is a number, a string, a symbol, or a compound.
%pp_sex('') :- format('(EmptyNode null)',[]).
pp_sexi(V):- number(V),!, writeq(V).
pp_sexi(V):- string(V),!, writeq(V).
pp_sexi(S):- string(S),!, print_concept('StringValue',S).
pp_sexi(V):- symbol(V), should_quote(V),!, symbol_string(V,S), write("'"),write(S),write("'").
% Base case: atoms are printed as-is.
%pp_sexi(S):- symbol(S), always_dash_functor(S,D), D \=@= S, pp_sax(D),!.
pp_sexi(V):- symbol(V),!,write(V).
pp_sexi(V) :- (number(V) ; is_dict(V)), !, print_concept('ValueAtom',V).
%pp_sex((Head:-Body)) :- !, print_metta_clause0(Head,Body).
%pp_sex(''):- !, write('()').

% Continuing with 'pp_sex', 'write_mobj', and related rules,
% handling different cases based on the valueï¿½s type and structure, and performing the appropriate writing action.
% Lists are printed with parentheses.
pp_sexi(V) :- \+ compound(V), !, format('~p',[V]).

%pp_sexi(V):-  is_list(V),!, pp_sex_l(V).
%pp_sex(V) :- (symbol(V),symbol_number(V,N)), !, print_concept('ValueAtom',N).
%pp_sex(V) :- V = '$VAR'(_), !, format('$~p',[V]).
pp_sexi(V) :- no_src_indents,!,pp_sex_c(V).
pp_sexi(V) :- w_proper_indent(2,w_in_p(pp_sex_c(V))).


write_mobj(H,_):- \+ symbol(H),!,fail.
write_mobj('$VAR',[S]):- write_dvar(S).
write_mobj(exec,[V]):- !, write('!'),write_src(V).
write_mobj('$OBJ',[_,S]):- write('['),write_src(S),write(' ]').
write_mobj('{}',[S]):- write('{'),write_src(S),write(' }').
write_mobj('{...}',[S]):- write('{'),write_src(S),write(' }').
write_mobj('[...]',[S]):- write('['),write_src(S),write(' ]').
write_mobj('$STRING',[S]):- !, writeq(S).
write_mobj(F,Args):- fail, mlog_sym(K),!,pp_sex_c([K,F|Args]).
%write_mobj(F,Args):- pp_sex_c([F|Args]).

print_items_list(X):- is_list(X),!,print_list_as_sexpression(X).
print_items_list(X):- write_src(X).

pp_sex_l(V):- pp_sexi_l(V),!.
pp_sexi_l(V) :- is_final_write(V),!.
%pp_sexi_l([F|V]):- integer(F), is_codelist([F|V]),!,format("|~s|",[[F|V]]).
pp_sexi_l([F|V]):- symbol(F), is_list(V),write_mobj(F,V),!.
pp_sexi_l([H|T]):-T ==[],!,write('('), pp_sex_nc(H),write(')').
pp_sexi_l([H,H2]):- write('('), pp_sex_nc(H), write(' '), with_indents(false,print_list_as_sexpression([H2])), write(')'),!.
pp_sexi_l([H|T]):- write('('),
  pp_sex_nc(H), write(' '), print_list_as_sexpression(T), write(')'),!.

pp_sexi_l([H,S]):-H=='[...]', write('['),print_items_list(S),write(' ]').
pp_sexi_l([H,S]):-H=='{...}', write('{'),print_items_list(S),write(' }').

%pp_sex_l(X):- \+ compound(X),!,write_src(X).
