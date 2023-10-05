:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_interp).
%:- ensure_loaded(metta_compiler).
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(swi_flybase).
% =======================================
% Scryer Rust Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% =======================================
:- set_option_value(encoding,iso_latin_1).

:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).






:- dynamic functional_predicate_arg/3.

% Converion is possible between a function and a predicate of arity A when the result is at the nth arg
functional_predicate_arg(append, 3, 3).  %  eval(append(L1,L2),Reslt) == append(L1,L2,Reslt)
functional_predicate_arg(is, 2, 1). %  eval(is(+(1,2)),Reslt) == is(Reslt,+(1,2)).
functional_predicate_arg(+, 3, 3).  %  eval(+(1,2),Reslt) == +(1,2,Reslt).
functional_predicate_arg(pi, 1, 1). %  eval(pi,Reslt) == pi(Reslt)

% Checks if Term is a function and retrieves the position
% in the pred that the function Result is stored/retreived
is_function(AsFunction, Nth) :-
    nonvar(AsFunction),
    functor(AsFunction, Functor, A),
    AA is A + 1,
    functional_predicate_arg(Functor, AA, Nth).


% Converts functions to predicates
% Example:
% ?- functs_to_preds(is(pi+pi),Converted),write_src(Converted).
% Converted = (pi(_A), +(_A, _A, _B), _C is _B, eval(_C, _)).
functs_to_preds(Convert, Converted) :-
    var(Convert), !,
    Converted = eval(Convert, _).
functs_to_preds(Convert, Converted) :-
    sub_term(AsFunction, Convert),
    is_function(AsFunction, Nth), !,
    funct_with_result_is_nth_of_pred(AsFunction, Nth, Result, AsPred),
    subst(Convert, AsFunction, Result, Converting),
    functs_to_preds((AsPred, Converting), Converted).
functs_to_preds((AsPred, Convert), (AsPred, Converted)) :- !,
    functs_to_preds(Convert, Converted).
functs_to_preds(Convert, Convert).

% Converion is possible between a function and a predicate of arity when the result is at the nth arg
funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred) :-
    AsFunction =.. [F | FuncArgs],
    nth1(Nth,PredArgs,Result,FuncArgs),
    AsPred =.. [F | PredArgs].


% Converts predicates back to functions
% Example:
% preds_to_functs((pi(_A), +(_A, _A, _B), _C is _B, eval(_C, _)), Converted).
% Converted = is(pi+pi)
preds_to_functs(Convert, Converted) :-
    var(Convert), !,
    Converted = Convert.
preds_to_functs((AsPred, Convert), Converted) :-
    pred_to_funct(AsPred, AsFunction, Result),
    sub_var(Result, Convert), !,
    subst(Convert, Result, AsFunction, Converting),
    preds_to_functs(Converting, Converted).
preds_to_functs(eval(AsFunction, _Result), AsFunction) :- !.
preds_to_functs((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs(Converting, Converted).
preds_to_functs(AsPred, eval(AsFunction, Result)) :-
    pred_to_funct(AsPred, AsFunction, Result), !.
preds_to_functs(X, X).

% Converts a predicate to its equivalent function term
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), !,
    functor(AsPred, F, A),
    functional_predicate_arg(F, A, Nth),
    arg(Nth, AsPred, Result),
    remove_funct_arg(AsPred, Nth, AsFunction).

% Removes the Nth argument from the predicate term
remove_funct_arg(AsPred, Nth, AsFunction) :-
    AsPred =.. [F | PredArgs],
    nth1(Nth,PredArgs,_Result,FuncArgs),
    AsFunction =.. [F | FuncArgs].


% print_metta_src(funct).

allow_concepts:- option_else(concepts,TF,true), \+ TF == false.
with_concepts(TF,Goal):- with_option(concepts,TF,Goal).
with_indents(TF,Goal):- with_option(src_indents,TF,Goal).

p2m(NC,NC):- var(NC),!.
p2m(NC,OO):- is_list(NC),!,maplist(p2m,NC,OO).
p2m(!,'!').
p2m(fail,'False').
p2m(true,'True').
p2m(prolog,meTTa).
p2m('[|]','Cons').
p2m(( ';' ),or).
p2m(( ',' ),and).
p2m(( '\\+' ),unless).
%p2m(( ':-' ),entailed_by).
p2m('=..','atom_2_list').

% Conversion for any atomic term
p2m(A, A):- atomic(A).

p2m(NC,NC):- \+ compound(NC),!.
p2m(NC,[F]):- compound_name_arity(NC,F,0),!.

% Conversion for the negation as failure
p2m((\+ A), O):- !, p2m(not(A), O).

% Conversion for arithmetic evaluation
%p2m(is(A, B), O):- !, p2m(eval(B, A), O).
%p2m(is(V,Expr),let(V,Expr,'True')).
p2m((Head:-Body),O):- Body == true,!, O = (=(Head,'True')).
p2m((Head:-Body),O):- Body == fail,!, O = (=(Head,'False')).
p2m((Head:-Body),O):- conjuncts_to_list(Body,List),into_sequential(List,SP),!,O=(=(Head,SP)).

p2m((G,E),O):- conjuncts_to_list((G,E),List),into_sequential(List,O),!.
% Conversion for if-then-else constructs
p2m((A->B;C),O):- !, p2m(if_then_else(A,B,C),O).
p2m((A;B),O):- !, p2m(or(A,B),O).
p2m((A*->B;C),O):- !, p2m(each_then_otherwise(A,B,C),O).
p2m((A->B),O):- !, p2m(if_then(A,B),O).
p2m((A*->B),O):- !, p2m(each_then(A,B),O).
p2m(metta_defn(Self,Eq,H,B),'add-atom'(Self,[Eq,H,B])).
p2m(metta_type,'add-atom').
p2m(metta_atom,'add-atom').
p2m(retractall(X),'remove-all-atoms'('&self',X)).
p2m(clause(H,B),'get-atoms'('&self',[=,H,B])).
p2m(retract(X),'remove-atom'('&self',X)).
p2m(assert(X),'add-atom'('&self',X)).
p2m(I,O):- I=..[F|II],maplist(p2m,[F|II],OO),O=..OO.

prolog_to_metta(V,D) :- p2m(V,D),!.

%into_sequential(Body,SP):- \+ is_list(Body), conjuncts_to_list(Body,List), maplist(p2m,List,MList), into_sequential(MList,SP).
into_sequential(List,SP):- length(List,L),L>1,   maplist(prolog_to_metta,List,MList), SP =.. ['and'|MList],!.
into_sequential([SP],O):- prolog_to_metta(SP,O).
into_sequential([],'True').


print_metta_src:- mmake,
  for_all((source_file(AsPred,File),
          symbol_contains(File,metta)),
         print_metta_src(AsPred)).

print_metta_src(F/A):- !, forall(current_predicate(F/A), print_metta_src(F,A)).
print_metta_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), print_metta_src(F,A)).
print_metta_src(F):-  \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), print_metta_src(F,A)).
print_metta_src(C):-  forall((current_predicate(F/A),once(atom_contains(F,C))), print_metta_src(F,A)).

print_metta_src(F,A):- functor(Head,F,A),
  ignore((predicate_property(Head,number_of_clauses(_)),
    source_file(Head,File),atom_contains(File,metta),!,
    nl,forall(clause(Head,Body), print_metta_clause(Head,Body)))).

print_metta_clause(Head,Body):- Body == true,!, pp_metta(=(Head,'True')).
print_metta_clause(Head,Body):- Body == false,!, pp_metta(=(Head,'False')).
print_metta_clause(Head,Body):- conjuncts_to_list(Body,List), into_sequential(List,SP), pp_metta(=(Head,SP)).


pp_metta(P):- pretty_numbervars(P,PP),with_option(concepts=false,pp_fb(PP)).

write_src(V):- notrace(write_src0(V)).
write_src0(V):- allow_concepts,!,with_concepts(false,write_src1(V)),flush_output.
write_src0(V):- is_list(V),!,pp_sexi(V).
write_src0(V):- write_src1(V),!.

write_src1(V):- var(V),!, ignore(pp_sex(V)).
write_src1(''):- !, writeq('').
write_src1(V):- number(V),!, writeq(V).
write_src1(V):- string(V),!, writeq(V).
write_src1(V):- symbol(V),needs_quoted_in_metta(V,_),!, symbol_string(V,S),writeq(S).
write_src1(V):- symbol(V),!,write(V).
write_src1(V):- compound(V), \+ is_list(V),!,write_mobj(V).
write_src1(V):- pp_sex(V),!.

write_mobj(V):- ( \+ compound(V) ; is_list(V)),!, write_src0(V).

write_mobj(V):- compound_name_arguments(V,F,Args),write_mobj(F,Args),!.
write_mobj(V):- writeq(V).
write_mobj(exec,[V]):- !, write('!'),with_indents(false,write_src(V)).
write_mobj('$STRING',[S]):- !, writeq(S).
write_mobj(F,Args):- mlog_sym(K),pp_sexi([K,F|Args]).

% Rules for determining when a symbol needs to be quoted in metta.
needs_quoted_in_metta(H,_):- upcase_atom(H,U),downcase_atom(H,U),!,fail.
needs_quoted_in_metta('','"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V," ").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"/").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,'"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V,'"').
needs_quoted_in_metta(V,'"'):- symbol_contains(V,',').
%needs_quoted_in_metta(V,"'"):- symbol_length(V,L),L==1.
%needs_quoted_in_metta(V,"'"):- symbol_contains(V,")").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"|").
needs_quoted_in_metta(V,'"'):- symbol_contains(V,"'").



