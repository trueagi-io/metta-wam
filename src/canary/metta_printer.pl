% ===============================
%       PRINTERS
% ===============================
% 'ppc' and 'ppc1' rules pretty-print original terms and convert them to metta if different,
% printing the converted forms as well.
ppc(Msg,Term):- ppc1(Msg,Term), p2m(Term,MeTTa),!, (MeTTa\==Term -> ppc1(p2m(Msg),MeTTa) ; true).

ppc1(Msg,Term):- \+ \+ ( ppct(Msg,Term) ),!.

ppc1(Msg,Term):- \+ \+ ( ignore(guess_pretty(Term)),
   writeln('---------------------'),
   write(p(Msg)),write(':'),nl,
   portray_clause(Term),
   writeln('---------------------'),
   \+ \+ (print_tree(?-show_cvts(Term))),nl,
    writeln('---------------------'),
     write(s(Msg)),write(':'),nl,
     write_src(Term),nl).

ppct(Msg,Term):- is_list(Term),!,
  writeln('---------------------'),
  numbervars(Term,666,_,[attvar(bind)]),
  write((Msg)),write(':'),nl,
  write_src(Term),nl.
ppct(Msg,Term):- Term=(_ :- _),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  portray_clause(Term),nl.
ppct(Msg,Term):- Term=(_=_),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  numbervars(Term,444,_,[attvar(bind)]),
  write_src(Term),nl.
ppct(Msg,Term):- Term=(_ :- _),!,
  writeln('---------------------'),
  write((Msg)),write(':'),nl,
  numbervars(Term,222,_,[attvar(bind)]),
  print_tree(Term),nl.

% 'pp_metta' rule is responsible for pretty-printing metta terms.
pp_metta(P):- pretty_numbervars(P,PP),with_option(concepts=false,pp_fb(PP)).

string_height(Pt1,H1):- split_string(Pt1,"\r\n", "\s\t\n\n", L),length(L,H1).

:- dynamic(just_printed/1).
% 'print_pl_source' rule is responsible for printing the source of a Prolog term.


print_pl_source(P):- run_pl_source(print_pl_source0(P)).

run_pl_source(G):- catch(G,E,(fail,write_src_uo(G=E),rtrace(G))).
print_pl_source0(_):- notrace(is_compatio),!.
print_pl_source0(_):- notrace(silent_loading),!.
print_pl_source0(P):- notrace((just_printed(PP), PP=@=P)),!.
    print_pl_source0((A:-B)):-!, portray_clause((A:-B)).
    print_pl_source0((:-B)):-!, portray_clause((:-B)).
print_pl_source0(P):- format('~N'), print_tree(P),format('~N'),!.
print_pl_source0(P):-
    Actions = [print_tree, portray_clause, pp_fb1_e], % List of actions to apply
    findall(H-Pt,
      (member(Action, Actions),
       must_det_ll((
          run_pl_source(with_output_to(string(Pt), call(Action, P))),
            catch(string_height(Pt, H),_,H=0)))), HeightsAndOutputs),
    sort(HeightsAndOutputs, Lst), last(Lst, _-Pt), writeln(Pt),
    retractall(just_printed(_)),
    assert(just_printed(P)),
    !.


pp_fb1_a(P):- format("~N "),  \+ \+ (numbervars_w_singles(P), pp_fb1_e(P)), format("~N "),flush_output.

pp_fb1_e(P):- pp_fb2(print_tree,P).
pp_fb1_e(P):- pp_fb2(pp_ilp,P).
pp_fb1_e(P):- pp_fb2(pp_as,P).
pp_fb1_e(P):- pp_fb2(portray_clause,P).
pp_fb1_e(P):- pp_fb2(print,P).
pp_fb1_e(P):- pp_fb2(fbdebug1,P).
pp_fb1_e(P):- pp_fb2(fmt0(P)).
pp_fb2(F,P):- atom(F),current_predicate(F/1), call(F,P).



pp_sax(V) :- is_final_write(V),!.
pp_sax(S) :-  \+ allow_concepts,!, write_src(S).
pp_sax(S) :- is_englishy(S),!,print_concept("StringValue",S).
pp_sax(S) :- symbol_length(S,1),symbol_string(S,SS),!,print_concept("StringValue",SS).
pp_sax(S) :- is_an_arg_type(S,T),!,print_concept("TypeNode",T).
pp_sax(S) :- has_type(S,T),!,format('(~wValueNode "~w")',[T,S]).
pp_sax(S) :- sub_atom(S,0,4,Aft,FB),flybase_identifier(FB,Type),!,
 (Aft>0->format('(~wValueNode "~w")',[Type,S]);'format'('(TypeNode "~w")',[Type])).
pp_sax(S) :- print_concept("ConceptNode",S).

%print_concept( CType,V):- allow_concepts, !, write("("),write(CType),write(" "),ignore(with_concepts(false,write_src(V))),write(")").
print_concept(_CType,V):- ignore(write_src(V)).
write_val(V):- is_final_write(V),!.
write_val(V):- number(V),!, write_src(V).
write_val(V):- compound(V),!, write_src(V).
write_val(V):- write('"'),write(V),write('"').


% Handling the final write when the value is a variable or a '$VAR' structure.
is_final_write(V):- var(V), !, write_dvar(V),!.
is_final_write('$VAR'(S)):-  !, write_dvar(S),!.
is_final_write('#\\'(S)):-  !, format("'~w'",[S]).
is_final_write(V):- py_is_enabled,py_is_py(V),!,py_ppp(V),!.

is_final_write([VAR,V|T]):- '$VAR'==VAR, T==[], !, write_dvar(V).
is_final_write('[|]'):- write('Cons'),!.
is_final_write([]):- !, write('()').
%is_final_write([]):- write('Nil'),!.


write_dvar(S):- S=='_', !, write_dname(S).
write_dvar(S):- S=='__', !, write('$').
write_dvar(S):- var(S), get_var_name(S,N),write_dname(N),!.
write_dvar(S):- var(S), !, format('$~p',[S]).
write_dvar(S):- atom(S), symbol_concat('_',N,S),write_dname(N).
write_dvar(S):- string(S), symbol_concat('_',N,S),write_dname(N).
%write_dvar(S):- number(S), write_dname(S).
write_dvar(S):- write_dname(S).
write_dname(S):- write('$'),write(S).

pp_as(V) :- \+ \+ pp_sex(V),flush_output.
pp_sex_nc(V):- with_no_quoting_symbols(true,pp_sex(V)),!.

unlooped_fbug(Mesg):-
 fbug_message_hook(fbug_message_hook,fbug(Mesg)).

into_hyphens(D,U):- atom(D),!,always_dash_functor(D,U).
into_hyphens(D,U):- descend_and_transform(into_hyphens,D,U),!.


unlooped_fbug(W,Mesg):- nb_current(W,true),!,
  print(Mesg),nl,bt,break.
unlooped_fbug(W,Mesg):-
  setup_call_cleanup(nb_setval(W,true),
    once(Mesg),nb_setval(W,false)),nb_setval(W,false).

:- dynamic(py_is_enabled/0).
py_is_enabled:- predicate_property(py_ppp(_),defined), asserta((py_is_enabled:-!)).

%write_src(V):-  !, \+ \+ quietly(pp_sex(V)),!.
write_src(V):- \+ \+ notrace((
  guess_metta_vars(V),pp_sex(V))),!.
write_src_woi(Term):-
  notrace((with_indents(false,write_src(Term)))).
write_src_woi_nl(X):- \+ \+
 notrace((guess_metta_vars(X),
    format('~N'),write_src_woi(X),format('~N'))).


pp_sex(V):- pp_sexi(V),!.
% Various 'write_src' and 'pp_sex' rules are handling the writing of the source,
% dealing with different types of values, whether they are lists, atoms, numbers, strings, compounds, or symbols.
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
% handling different cases based on the value type and structure, and performing the appropriate writing action.
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
%pp_sex_l('$VAR'(S))):-
pp_sexi_l([=,H,B]):- pp_sexi_hb(H,B),!.

pp_sexi_l([H|T]) :- \+ no_src_indents, symbol(H),member(H,['If','cond','let','let*']),!,
  with_indents(true,w_proper_indent(2,w_in_p(pp_sex([H|T])))).

pp_sexi_l([H|T]) :- is_list(T), length(T,Args),Args =< 2, fail,
   wots(SS,((with_indents(false,(write('('), pp_sex_nc(H), write(' '), print_list_as_sexpression(T), write(')')))))),
   ((symbol_length(SS,Len),Len < 20) ->write(SS);
      with_indents(true,w_proper_indent(2,w_in_p(pp_sex_c([H|T]))))),!.
/*

pp_sexi_l([H|T]) :- is_list(T),symbol(H),upcase_atom(H,U),downcase_atom(H,U),!,
   with_indents(false,(write('('), pp_sex_nc(H), write(' '), print_list_as_sexpression(T), write(')'))).

%pp_sex([H,B,C|T]) :- T==[],!,
%  with_indents(false,(write('('), pp_sex(H), print_list_as_sexpression([B,C]), write(')'))).
*/

pp_sexi_hb(H,B):-
  write('(= '), with_indents(false,pp_sex(H)), write('  '),
        ((is_list(B),maplist(is_list,B))
      ->with_indents(true,maplist(write_src_inl,B))
      ;with_indents(true,pp_sex(B))),
    write(')').

write_src_inl(B):- nl, write('    '),pp_sex(B).

pp_sex_c(V):- pp_sexi_c(V),!.
pp_sexi_c(V) :- is_final_write(V),!.
pp_sexi_c((USER:Body)) :- USER==user,!, pp_sex(Body).
pp_sexi_c(exec([H|T])) :- is_list(T),!,write('!'),pp_sex_l([H|T]).
pp_sexi_c(!([H|T])) :- is_list(T),!,write('!'),pp_sex_l([H|T]).
%pp_sexi_c([H|T]) :- is_list(T),!,unlooped_fbug(pp_sexi_c,pp_sex_l([H|T])).
pp_sexi_c([H|T]) :- is_list(T),!,pp_sex_l([H|T]).
%pp_sexi_c(V) :- print(V),!.

pp_sexi_c(=(H,B)):- !, pp_sexi_hb(H,B),!.
pp_sexi_c(V):- compound_name_list(V,F,Args),write_mobj(F,Args),!.
% Compound terms.
%pp_sex(Term) :- compound(Term), Term =.. [Functor|Args], write('('),format('(~w ',[Functor]), write_args_as_sexpression(Args), write(')').
%pp_sex(Term) :- Term =.. ['=',H|Args], length(Args,L),L>2, write('(= '),  pp_sex(H), write('\n\t\t'), maplist(pp_sex(2),Args).
pp_sexi_c(V):- ( \+ compound(V) ; is_list(V)),!, pp_sex(V).
pp_sexi_c(listOf(S,_)) :- !,write_mobj(listOf(S)).
pp_sexi_c(listOf(S)) :- !,format('(ListValue ~@)',[pp_sex(S)]).
pp_sexi_c('!'(V)) :- write('!'),!,pp_sex(V).
%pp_sex_c('exec'(V)) :- write('!'),!,pp_sex(V).
pp_sexi_c('='(N,V)):- allow_concepts, !, format("~N;; ~w == ~n",[N]),!,pp_sex(V).
%pp_sex_c(V):- writeq(V).

pp_sexi_c(Term) :- compound_name_arity(Term,F,0),!,pp_sex_c([F]).
pp_sexi_c(Term) :- Term =.. [Functor|Args], always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]), write_args_as_sexpression(Args), write(')'),!.
pp_sexi_c(Term) :- allow_concepts, Term =.. [Functor|Args], format('(EvaluationLink (PredicateNode "~w") (ListLink ',[Functor]), write_args_as_sexpression(Args), write('))'),!.
pp_sexi_c(Term) :-
  Term =.. [Functor|Args],
   always_dash_functor(Functor,DFunctor), format('(~w ',[DFunctor]),
     write_args_as_sexpression(Args), write(')'),!.

pp_sexi(2,Result):- write('\t\t'),pp_sex(Result).


current_column(Column) :- current_output(Stream), line_position(Stream, Column),!.
current_column(Column) :- stream_property(current_output, position(Position)), stream_position_data(column, Position, Column).
min_indent(Sz):- current_column(Col),Col>Sz,nl,indent_len(Sz).
min_indent(Sz):- current_column(Col),Need is Sz-Col,indent_len(Need),!.
min_indent(Sz):- nl, indent_len(Sz).
indent_len(Need):- forall(between(1,Need,_),write(' ')).

w_proper_indent(N,G):-
  flag(w_in_p,X,X), %(X==0->nl;true),
  XX is (X*2)+N,setup_call_cleanup(min_indent(XX),G,true).
w_in_p(G):- setup_call_cleanup(flag(w_in_p,X,X+1),G,flag(w_in_p,_,X)).


always_dash_functor(A,B):- once(dash_functor(A,B)),A\=@=B,!.
always_dash_functor(A,A).


dash_functor(A,C):- \+ symbol(A),!,C=A.
% dash_functor(A,C):- p2m(A,B),A\==B,!,always_dash_functor(B,C).
dash_functor(ASymbolProc,O):- fail, symbol_contains(ASymbolProc,'_'),
    symbol_contains(ASymbolProc,'atom'),
    current_predicate(system:ASymbolProc/_),
    symbolic_list_concat(LS,'atom',ASymbolProc),
    symbolic_list_concat(LS,'symbol',SymbolProc),
    always_dash_functor(SymbolProc,O),!.
dash_functor(ASymbolProc,O):- symbol_concat('$',LS,ASymbolProc),!,
    symbol_concat('%',LS,SymbolProc),
    always_dash_functor(SymbolProc,O).

dash_functor(Functor,DFunctor):- fail,
   symbolic_list_concat(L,'_',Functor), L\=[_],
   symbolic_list_concat(L,'-',DFunctor).

% Print arguments of a compound term.
write_args_as_sexpression([]).
write_args_as_sexpression([H|T]) :- write(' '), pp_sex(H), write_args_as_sexpression(T).

% Print the rest of the list.
print_list_as_sexpression([]).
print_list_as_sexpression([H]):- pp_sex(H).
%print_list_as_sexpression([H]):- w_proper_indent(pp_sex(H)),!.
print_list_as_sexpression([H|T]):- pp_sex(H), write(' '), print_list_as_sexpression(T).



% The predicate with_indents/2 modifies the src_indents option value during the execution of a goal.
% The first argument is the desired value for src_indents,
% and the second argument is the Goal to be executed with the given src_indents setting.
with_indents(TF, Goal) :-
    % Set the value of the `src_indents` option to TF and then execute the Goal
    as_tf(TF,Value),
    with_option(src_indents, Value, Goal).

no_src_indents:- option_else(src_indents,TF,true),!,TF=='False'.

no_quoting_symbols:- option_else(no_quoting_symbols,TF,true),!,TF=='True'.

with_no_quoting_symbols(TF, Goal) :-
    % Set the value of the `no_src_indents` option to TF and then execute the Goal
    with_option(no_quoting_symbols, TF, Goal).

% The predicate allow_concepts/0 checks whether the use of concepts is allowed.
% It does this by checking the value of the concepts option and ensuring it is not false.
allow_concepts :- !, fail,
    % Check if the option `concepts` is not set to false
    option_else(concepts, TF, 'False'),
    \+ TF == 'False'.

% The predicate with_concepts/2 enables or disables the use of concepts during the execution of a given goal.
% The first argument is a Boolean indicating whether to enable (true) or disable (false) concepts.
% The second argument is the Goal to be executed with the given concepts setting.
with_concepts(TF, Goal) :-
    % Set the value of the `concepts` option to TF and then execute the Goal
    with_option(concepts, TF, Goal).

% Rules for determining when a symbol needs to be quoted in metta.
dont_quote(Atom):- symbol_length(Atom,1), !, char_type(Atom,punct).
dont_quote(Atom):- symbol(Atom),upcase_atom(Atom,Atom),downcase_atom(Atom,Atom).

should_quote(Atom) :- \+ symbol(Atom), \+ string(Atom),!,fail.
should_quote(Atom) :-
   \+ dont_quote(Atom),
   % symbol(Atom),  % Ensure that the input is an symbol
    symbol_chars(Atom, Chars),
    once(should_quote_chars(Chars);should_quote_symbol_chars(Atom,Chars)).

contains_unescaped_quote(['"']):- !, fail. % End with a quote
contains_unescaped_quote(['"'|_]) :- !.
contains_unescaped_quote(['\\', '"'|T]) :- !, contains_unescaped_quote(T).
contains_unescaped_quote([_|T]) :- contains_unescaped_quote(T).

% Check if the list of characters should be quoted based on various conditions
should_quote_chars([]).
should_quote_chars(['"'|Chars]):- !, contains_unescaped_quote(Chars).
should_quote_chars(Chars) :-
      member('"', Chars);         % Contains quote not captured with above clause
      member(' ', Chars);         % Contains space
      member('''', Chars);        % Contains single quote
    %  member('/', Chars);         % Contains slash
      member(',', Chars);         % Contains comma
      (fail,member('|', Chars)).         % Contains pipe
%should_quote_symbol_chars(Atom,_) :- symbol_number(Atom,_),!.
should_quote_symbol_chars(Atom,[Digit|_]) :- fail, char_type(Digit, digit), \+ symbol_number(Atom,_).

% Example usage:
% ?- should_quote('123abc').
% true.
% ?- should_quote('123.456').
% false.


:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_eval).
