:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
:- op(700,xfx,'=~').

% ===============================
%    TESTING
% ===============================
% Define 'fb', a rule that calls 'make' and writes information for each clause of 'fb0'.
% 'make' compiles the program.
% The 'forall' loop will write and call all goals of the 'fb0' clauses.


fb:- make,
   writeln(';; ==========================================='),
   forall((clause(fb0,Goal),write(';; '),writeq(?- Goal),nl,call(Goal)),
   writeln(';; ===========================================')).

% The 'fb0' rule showing mettalog sources with specific definitions.
fb0:- show_mettalog_src((two_pi(R):-(pi(A), +(A, A, B), R is B))).
fb0:- show_mettalog_src(factorial_tail_basic).
fb0:- show_mettalog_src(funct).

print_metta_src :-  show_mettalog_src.
% 'show_mettalog_src' rule compiles the program and shows mettalog sources for each source file containing 'metta'.
show_mettalog_src:- make,
  forall((source_file(AsPred,File),
          symbol_contains(File,metta)),
         show_mettalog_src(AsPred)).


% Handling different cases for 'show_mettalog_src' with different input parameters.
% These rules use nonvar, current_predicate, and other built-ins to perform various checks and actions
% based on the type and value of the input to 'show_mettalog_src'.
show_mettalog_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_mettalog_src(F,A)).
show_mettalog_src(C):- show_cvts(C),!.

% The 'show_space_src' rules compile the program and show space sources for each space predicate.
show_space_src:- make,
  forall(space_preds(AsPred),show_space_src(AsPred)).


% Similar to the 'show_mettalog_src' rules, these rules handle different cases for 'show_space_src'
% with different input parameters and perform various checks and actions based on the type and value of the input.
show_space_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_space_src(F,A)).
show_space_src(C):- show_cvts(C),!.

% 'show_cvts' rule processes a term, performing different actions based on the structure of the term.
show_cvts(Term):-
  once((is_list(Term), sexpr_s2p(Term,PF))), \+ is_list(PF),!,show_cvts(PF).

% 'show_cvts' continues processing, performing conversions between predicates and functions,
% and pretty-printing original terms, function forms, and Prolog forms.
show_cvts(Term):- iz_conz(Term),!, ppc(orig,Term),Term = FunctForm,
  functs_to_preds(FunctForm,Prolog), ppc(preds,Prolog),
  preds_to_functs(Prolog,NFunctForm), ppc(functs,NFunctForm).
show_cvts(Term):- ppc(orig,Term),
  preds_to_functs(Term,FunctForm), ppc(functs,FunctForm),
  functs_to_preds(FunctForm,Prolog), ppc(preds,Prolog).

% 'show_mettalog_src' for specific predicate, prints metta clauses if they exist in the source file containing 'metta'.
show_mettalog_src(F,A):- functor(Head,F,A),
  ignore((predicate_property(Head,number_of_clauses(_)),
    source_file(Head,File),atom_contains(File,metta),!,
    nl,findall((Head:-Body),
       clause(Head,Body), Clauses),
    print_metta_clauses(Clauses))),nl.

% 'print_metta_clauses' rule is handling the printing of metta clauses.
% It checks the form of the input clauses and calls 'print_metta_clause' accordingly.
print_metta_clauses([]):- !.
print_metta_clauses([Head:-Body]):- !, print_metta_clause(Head,Body).
print_metta_clauses(Clauses):- combine_clauses(Clauses,Head,Body),!,print_metta_clause(Head,Body).
print_metta_clause(Head,Body):-
  print_metta_clause0(Head,Body),
  show_cvts(Head:-Body).

% 'print_metta_clause0' rule prints metta clauses based on the body.
% It transforms the body to a list, if needed, and prints it in a sequential form.
print_metta_clause0(Head,Body):- Body == true,!, pp_metta([=,Head,'True']).
print_metta_clause0(Head,Body):- Body == false,!, pp_metta([=,Head,'False']).
print_metta_clause0(Head,Body):- conjuncts_to_list(Body,List), into_sequential(List,SP), pp_metta([=,Head,SP]).



% =========================================
%  STERM -> PTERM
% =========================================

iz_exact_symbol(N,_):- \+ atom(N),!,fail.
iz_exact_symbol(N,P):- nonvar(P),!,iz_exact_symbol(N,PP),zalwayz(P=PP).
iz_exact_symbol(':-',':-').
iz_exact_symbol('?-','?-').
iz_exact_symbol('??',_).

%:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

maybe_varz(S,Name,'$VAR'(Name)):- S=='?',atom(Name),!.

%% sexpr_s2p(Fn,?VAR, ?V) is det.
%
% S-expression Sterm Converted To Pterm.
%
sexpr_s2p(HB,P):- fail, compound(HB), HB=~ (H=B), compile_for_assert(H,B,Cl),
   clause_to_code(Cl,P),!.
sexpr_s2p(S,P):- sexpr_s2p(progn,1,S,P).


clause_to_code(P,P):- is_ftVar(P),!.
%clause_to_code(P:-True,P):- True == true,!.
clause_to_code((H:-B),P):- B==true, !, combine_code(B,H,P).
clause_to_code(P,P).

sexpr_s2p(_Fn,_Nth,VAR,VAR):-is_ftVar(VAR),!.
sexpr_s2p(_Fn,_Nth,S,P):- iz_exact_symbol(S,P),!.
sexpr_s2p(_Fn,_Nth,'#'(S),P):- iz_exact_symbol(S,P),!.
sexpr_s2p(_Fn,_Nth,VAR,'$VAR'(Name)):- atom(VAR),svar(VAR,Name),!.
sexpr_s2p(Fn,Nth,S,P):- S==[], iz_fun_argz(Fn,Nth),!,P=S.

sexpr_s2p(Fn,Nth,S,P):- expects_type(Fn,Nth,Type),will_become_type(Type,S,P),!.

sexpr_s2p(_Fn,_Nth,[F|SList],P):- is_list(SList), length(SList,Len),is_syspred(F,Len,Pred), sexpr_s2p_arglist(F,1,SList,PList), !, P=..[Pred|PList].
:- style_check(-singleton).

sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- iz_fun_argz(Fn,Nth),!,sexpr_s2p(S,P), sexpr_s2p(Fn,Nth,SList,PList).
sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- ( \+ atom(S) ; \+ is_list(SList)), !,sexpr_s2p(list(Fn),Nth,S,P), sexpr_s2p(list(Fn),Nth,SList,PList).
sexpr_s2p(_Fn,_Nth,[S,STERM0],PTERM):- iz_quoter(S),sexpr_s2p_pre_list(S,0,STERM0,STERM), !,PTERM=..[S,STERM],!.
sexpr_s2p(_Fn,_Nth,[S|SList],P):- atom(S), SList == [], compound_name_arity(P,S,0).
% sexpr_s2p(Fn,Nth,List,PTERM):- append(Left,[S,Name|TERM],List),maybe_varz(S,Name,Var),!,append(Left,[Var|TERM],NewList), sexpr_s2p(Fn,Nth,NewList,PTERM).
% sexpr_s2p(Fn,Nth,[S|TERM],dot_holds(PTERM)):- \+ (is_list(TERM)),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- \+ atom(S),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.
/*
sexpr_s2p(Fn,Nth,[S,Vars|TERM],PTERM):- nonvar(S),
   call_if_defined(common_logic_snark:iz_quantifier(S)),
   zalwayz((sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),
   PTERM =~ [S,Vars|PLIST])),!.
*/
% sexpr_s2p(progn,_,[S|TERM],PTERM):- S==AND,!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- (number(S);  (atom(S),fail,atom_concat_or_rtrace(_,'Fn',S))),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.
%sexpr_s2p(Fn,Nth,[S],O):- is_ftVar(S),sexpr_s2p(Fn,Nth,S,Y),!,z_univ(Fn,Nth,O,[Y]),!.
%sexpr_s2p(Fn,Nth,[S],O):- nonvar(S),sexpr_s2p(Fn,Nth,S,Y),!,z_univ(Fn,Nth,O,[Y]),!.
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- S==and,!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).
% sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- iz_va_relation(S),!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(S,PLIST,PTERM))).
%sexpr_s2p(Fn,Nth,[S|TERM],PTERM):- iz_relation_sexpr(S),zalwayz((sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),PTERM =~ [S|PLIST])),!.
%sexpr_s2p(Fn,Nth,STERM,PTERM):- STERM =~ [S|TERM],sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),z_univ(Fn,Nth,PTERM,[S|PLIST]),!.
sexpr_s2p(Fn,Nth,[S|STERM0],PTERM):-
  sexpr_s2p_pre_list(Fn,Nth,STERM0,STERM),
  sexpr_s2p_arglist(S,1,STERM,PLIST), z_univ(Fn,Nth,PTERM,[S|PLIST]),!.
sexpr_s2p(_Fn,_Nth,VAR,VAR).


expects_type(Fn,Nth,Type):-
  get_operator_typedef(Self,Fn,Params,RetType),
  nth0(Nth,[RetType|Params],Type),nonvar(Type).

will_become_type(Type,S,P):- try_adjust_arg_types(=,_RetType,88,_Self,[Type],[S],[PS]),PS=P,!.
will_become_type(Type,S,P):- is_ftVar(S),!,P=S.
will_become_type(Type,S,P):-
   get_type(S,T),!,
     (is_subtype(T,Type)->S=P; P=coerce(Type,S)).
will_become_type(_Type,S,P):-!,S=P.

is_subtype(T,TT):- T=@=TT,!,T=TT.
is_subtype(T,TT):- T=TT,!.

iz_quoter('#BQ'):- iz_common_lisp.
iz_quoter('#COMMA'):- iz_common_lisp.
iz_quoter('quote').
iz_quoter(superpose).

iz_fun_argz(list(_),_).
iz_fun_argz(defmacro,2).
iz_fun_argz(defun,2).
iz_fun_argz(let,1).
iz_fun_argz('let*',1).
iz_fun_argz('member',2).
%iz_fun_argz('let*',2).
iz_fun_argz(F,1):- iz_quoter(F).

z_functor(F):- \+ atom(F), !,fail.
z_functor(F):- \+ atom_concat('?',_,F).
z_functor(F):- \+ atom_concat('$',_,F).

%z_univ(_Fn,1,S,S):-!.
z_univ(_Fn,_,P,[F|ARGS]):- z_functor(F),is_list(ARGS),length(ARGS,A),l_arity_l(F,A),compound_name_arguments(P,F,ARGS),!.
z_univ(_Fn,0,P,[F|ARGS]):- z_functor(F),is_list(ARGS),compound_name_arguments(P,F,ARGS),!.
z_univ(_Fn,_Nth,P,[F|ARGS]):- z_functor(F),is_list(ARGS),compound_name_arguments(P,F,ARGS),!.
z_univ(_Fn,_Nth,P,S):-P=S.

l_arity_l(F,A):- clause_b(arity(F,A)).
l_arity_l(function,1).
l_arity_l(quote,1).
l_arity_l('#BQ',1):- iz_common_lisp.
l_arity_l(F,A):-current_predicate(F/A).
l_arity_l(_,1).

sexpr_s2p_arglist(_Fn,_,VAR,VAR):-is_ftVar(VAR),!.
sexpr_s2p_arglist(Fn,Nth,[S|SList],[P|PList]):-sexpr_s2p(Fn,Nth,S,P),
  (Nth>0->Nth2 is Nth+1;Nth2=0),sexpr_s2p_arglist(Fn,Nth2,SList,PList),!.
sexpr_s2p_arglist(Fn,Nth,S,P):-sexpr_s2p(Fn,Nth,S,P),!.
sexpr_s2p_arglist(_Fn,_Nth,VAR,VAR).

sexpr_s2p_pre_list(_Fn,_,STERM,STERM):- \+ compound(STERM), !.
sexpr_s2p_pre_list(_Fn,_,STERM,STERM):- \+ is_list(STERM), !.
% sexpr_s2p_pre_list(Fn,_,[S|STERM],[S|STERM]):- STERM == [], !.
sexpr_s2p_pre_list(Fn,Nth,[S0|STERM0],[S|STERM]):-
 (is_list(S0)->sexpr_s2p(Fn,Nth,S0,S);sexpr_s2p_pre_list(Fn,Nth,S0,S)),
 sexpr_s2p_pre_list(Fn,Nth,STERM0,STERM),!.
sexpr_s2p_pre_list(_Fn,_,STERM,STERM).




% p2m/2 is a translation utility to convert Prolog constructs to MeTTa constructs.
% It handles a variety of cases, including different types of compound terms,
% control structures, and predicate definitions.
% The first argument is the input in Prolog syntax,
% and the second argument is the output converted to MeTTa syntax.


p2m(NC, NC) :- var(NC), !.  % If NC is a variable, do not translate.
p2m(NC, NC) :- is_ftVar(NC), !.  % If NC is a free term variable, do not translate.
% Conversion for lists
p2m([], 'Nil'). % empty list
p2m([H|T], 'Cons'(OH, OT)):- p2m(H, OH), p2m(T, OT).
p2m(NC, OO) :-
    % If NC is a list, map each element of the list from Prolog to MeTTa
    is_list(NC),!,
    maplist(p2m, NC, OO).
p2m(!, '!').  % Translate the cut operation directly.
p2m(false, 'False').
% p2m(fail, 'False').  % Translate Prolog’s fail to MeTTa’s False.
p2m(true, 'True').  % Translate Prolog’s true to MeTTa’s True.
% p2m(prolog, meTTa).  % Translate the atom prolog to meTTa.

p2m('[|]','Cons').
p2m(( ';' ),or).
%p2m(( ',' ),and).
%p2m(( '\\+' ),unless).
%p2m(( ':-' ),entailed_by).
%p2m('=..','atom_2_list').

% Conversion for any atomic term
p2m(A, A):- atomic(A).

p2m(NC,NC):- \+ compound(NC),!.
p2m(NC,[F]):- compound_name_arity(NC,F,0),!.

% Conversion for the negation as failure
p2m((\+ A), O):- !, p2m(not(A), O).

p2m((G,E),O):-  conjuncts_to_list((G,E),List),!,into_sequential(List,O),!.

% Conversion for arithmetic evaluation
%p2m(is(A, B), O):- !, p2m(eval(B, A), O).
%p2m(is(V,Expr),let(V,Expr,'True')).
p2m((Head:-Body),O):- Body == true,!, O = (=(Head,'True')).
p2m((Head:-Body),O):- Body == fail,!, O = (=(Head,[empty])).
p2m((Head:-Body),O):- conjuncts_to_list(Body,List),into_sequential(List,SP),!,O=(=(Head,SP)).

% Conversion for if-then-else constructs
p2m((A->B;C),O):- !, p2m(if_then_else(A,B,C),O).
p2m((A;B),O):- !, p2m(or(A,B),O).
p2m((A*->B;C),O):- !, p2m(each_then_otherwise(A,B,C),O).
p2m((A->B),O):- !, p2m(if_then(A,B),O).
p2m((A*->B),O):- !, p2m(each_then(A,B),O).
p2m(metta_defn(Eq,Self,H,B),'add-atom'(Self,[Eq,H,B])).
p2m(metta_type,'add-atom').
p2m(get_metta_atom,'add-atom').
p2m(retractall(X),'remove-all-atoms'('&self',X)).
p2m(clause(H,B),'get-atoms'('&self',[=,H,B])).
p2m(retract(X),'remove-atom'('&self',X)).
p2m(assert(X),'add-atom'('&self',X)).
% The catch-all case for the other compound terms.
p2m(I,O):- I=..[F|II],maplist(p2m,[F|II],OO),O=..OO.

% It will break down compound terms into their functor and arguments and apply p2m recursively
p2m(I, O):-
    compound(I),
    I =.. [F|II], % univ operator to convert between a term and a list consisting of functor name and arguments
    maplist(p2m, II, OO), % applying p2m recursively on each argument of the compound term
    sexpr_s2p([F|OO],O). % constructing the output term with the converted arguments

% In the context of this conversion predicate, each branch of the p2m predicate
% is handling a different type or structure of term, translating it into its
% equivalent representation in another logic programming language named MeTTa.
% The actual transformations are dependent on the correspondence between Prolog
% constructs and MeTTa constructs, as defined by the specific implementations
% of Prolog and MeTTa being used.
prolog_to_metta(V, D) :-
    % Perform the translation from Prolog to MeTTa
    p2m(V, D),!.


% Define predicates to support the transformation from Prolog to MeTTa syntax
% (Continuing the translation from Prolog to MeTTa syntax as per the given code)
% Handle the case where the body is a conjunction of terms
into_sequential(Body, SP) :-
    % Check if Body is not a list and convert conjunctions in Body to a list of conjuncts.
    \+ is_list(Body),
    conjuncts_to_list(Body, List),
    is_list(List), % Converts a list of conjunctions into a sequential representation in MeTTa
    into_sequential(List, SP), !.
into_sequential(Nothing,'True'):- Nothing ==[],!.
 % If there's only one element
into_sequential([SP],O):- prolog_to_metta(SP,O).
% Otherwise, construct sequential representation using AND.
into_sequential(List, [AND|SPList]) :- is_compiled_and(AND), maplist(prolog_to_metta, List, SPList),!.





