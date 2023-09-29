:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
:- ensure_loaded(swi_support).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_reader).
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_space).
% TODO move non flybase specific code between here and the compiler
:- ensure_loaded(swi_flybase).
% =======================================
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
% =======================================
:- set_option_value(encoding,iso_latin_1).

:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).


% ===============================
%       COMPILER/ OPTIMIZER
% ===============================


:- dynamic functional_predicate_arg/3.

% Converion is possible between a function and a predicate of arity A when the result is at the nth arg
functional_predicate_arg_tricky(is, 2, 1). %  eval(is(+(1,2)),Reslt) == is(Reslt,+(1,2)).

functional_predicate_arg(append, 3, 3).  %  eval(append(L1,L2),Reslt) == append(L1,L2,Reslt)
functional_predicate_arg(+, 3, 3).  %  eval(+(1,2),Reslt) == +(1,2,Reslt).
functional_predicate_arg(pi, 1, 1). %  eval(pi,Reslt) == pi(Reslt)
functional_predicate_arg(F, A, A):-  (atom(F)->true;trace), current_predicate(F/A),
  \+ functional_predicate_arg_tricky(F,A,_).
functional_predicate_arg(F, A, L):- functional_predicate_arg_tricky(F, A, L).
% functional_predicate_arg(F, A, A):- clause(arithmetic:eval(AsFunction, B, C),_)...

always_a_pred(!).
always_a_pred(print).
always_a_pred('True').

is_arity_0(AsFunction):- compound(AsFunction), compound_name_arity(AsFunction,_,0).

% Checks if Term is a function and retrieves the position
% in the pred that the function Result is stored/retreived
is_function(AsFunction, _):- is_ftVar(AsFunction),!,fail.
is_function(AsFunction, _):- always_a_pred(AsFunction),!,fail.
is_function(AsFunction, Nth) :- is_arity_0(AsFunction),!,Nth=1.
is_function(AsFunction, Nth) :-
    \+ number(AsFunction),
    nonvar(AsFunction),
    functor(AsFunction, Functor, A),
    AA is A + 1,
    functional_predicate_arg(Functor, AA, Nth).


% Converts functions to predicates
% Example:
% ?- functs_to_preds(RetResult,is(pi+pi),Converted),write_src(Converted).
% Converted = (pi(_A), +(_A, _A, _B), _C is _B, eval(_C, _)).
functs_to_preds(RetResult,Convert, Converted) :-
     is_ftVar(Convert), !,
     Converted = eval(Convert, RetResult).

functs_to_preds(RetResult,Convert, Converted) :-
   once((is_list(Convert), s2p(Convert,IS))), \+ is_list(IS),!,
   functs_to_preds(RetResult,IS, Converted).

functs_to_preds(RetResult,Convert, Converted) :-
     Convert = (AsFunction=AsBodyFn),!,
     ignore(Result = '$VAR'('HeadRes')),
     Converted = (Head :- NextBody),
     funct_with_result_is_nth_of_pred(AsFunction, Result, _Nth, Head),
     %numbervars(Convert,0,_,[]),
     verbose_unify(Convert),
     functs_to_preds(Result,AsBodyFn,NextBody),
     RetResult = Converted.

functs_to_preds(RetResult,is(Convert),(Converted,is(RetResult,Result))):-
   functs_to_preds(Result,Convert, Converted).

functs_to_preds(RetResult,or(AsPredI,Convert), (AsPredO; Converted)) :- !,
  functs_to_preds(RetResult,AsPredI, AsPredO),
  functs_to_preds(RetResult,Convert, Converted).
functs_to_preds(RetResult,(AsPredI; Convert), (AsPredO; Converted)) :- !,
  functs_to_preds(RetResult,AsPredI, AsPredO),
  functs_to_preds(RetResult,Convert, Converted).

functs_to_preds(RetResult,(AsPredI, Convert), (AsPredO, Converted)) :- !,
  functs_to_preds(_RetResul,AsPredI, AsPredO),
  functs_to_preds(RetResult,Convert, Converted).

functs_to_preds(RetResult,AsFunction,AsPred):-
   is_function(AsFunction, Nth),
   funct_with_result_is_nth_of_pred(AsFunction, RetResult, Nth, AsPred),
   \+ ( compound(AsFunction), arg(_,AsFunction, Arg), is_function(Arg,_)),!.

functs_to_preds(RetResult,Convert, Converted) :-
    sub_term_from_last(AsFunction, Convert),
    callable(AsFunction),
    is_function(AsFunction, Nth),
    funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred),
    subst(Convert, AsFunction, Result, Converting),
    functs_to_preds(RetResult,(AsPred,Converting), Converted).

functs_to_preds(_RetResult,Convert, Convert).

% Converion is possible between a function and a predicate of arity when the result is at the nth arg
funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred):- is_ftVar(AsFunction),!,
   compound(AsPred),
    compound_name_arguments(AsPred,F,PredArgs),
    nth1(Nth,PredArgs,Result,FuncArgs),
    compound_name_arguments(AsFunction,F,FuncArgs).

funct_with_result_is_nth_of_pred(AsFunction, Result, Nth, AsPred) :-
   (atom(AsFunction)->AsFunction =.. [F | FuncArgs]; compound_name_arguments(AsFunction,F,FuncArgs)),
    nth1(Nth,PredArgs,Result,FuncArgs),
    AsPred =.. [F | PredArgs].

% Removes the Nth argument from the predicate term
remove_funct_arg(AsPred, Nth, AsFunction) :-
    AsPred =.. [F | PredArgs],
    nth1(Nth,PredArgs,_Result,FuncArgs),
    compound_name_arguments(AsFunction,F,FuncArgs).


sub_term_from_last(ST, Term):- is_list(Term),!,rev_member(E,Term),sub_term_from_last(ST, E).
sub_term_from_last(ST, Term):- compound(Term), compound_name_arguments(Term,_,Args),sub_term_from_last(ST, Args).
sub_term_from_last(ST, ST):- nonvar(ST), ST\==[], callable(ST).

% Converts predicates back to functions
% Example:
% preds_to_functs((pi(_A), +(_A, _A, _B), _C is _B, eval(_C, _)), Converted).
% Converted = is(pi+pi)


rev_member(BE,[_|Body]):- is_list(Body),rev_member(BE,Body).
rev_member(BE,[BE|_]).

preds_to_functs(Convert, Converted):-
  verbose_unify(Convert),
  preds_to_functs0(Convert, Converted).

preds_to_functs0(Convert, Converted) :-
    is_ftVar(Convert), !,
    Converted = Convert.
preds_to_functs0((Head:-Body), Converted) :- !,
 ((
   pred_to_funct(Head, AsFunction, Result),
   %ignore(Result = '$VAR'('HeadRes')),
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   compound(BE),arg(Nth,BE,ArgRes),sub_var(Result,ArgRes),
   remove_funct_arg(BE, Nth, AsBodyFunction),
   append(Left,[eval(AsBodyFunction,Result)|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody),
   preds_to_functs0(NewBody,ConvertedBody),
   (Converted = (AsFunction=ConvertedBody)))).

preds_to_functs0((AsPred, Convert), Converted) :-
    \+ always_a_pred(AsPred),
    pred_to_funct(AsPred, AsFunction, Result),
    sub_var(Result, Convert), !,
    subst(Convert, Result, AsFunction, Converting),
    preds_to_functs0(Converting, Converted).

preds_to_functs0(eval(AsFunction, _Result), AsFunction) :- !.

preds_to_functs0((AsPred, Converting), (AsPred, Converted)) :- !,
    preds_to_functs0(Converting, Converted).

preds_to_functs0(AsPred, eval(AsFunction, Result)) :-
    pred_to_funct(AsPred, AsFunction, Result), !.

preds_to_functs0(X, X).

% Converts a predicate to its equivalent function term
pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred),
    functor(AsPred, F, A),
    functional_predicate_arg(F, A, Nth),!,
    arg(Nth, AsPred, Result),
    remove_funct_arg(AsPred, Nth, AsFunction).

pred_to_funct(AsPred, AsFunction, Result) :-
    compound(AsPred), !,
    functor(AsPred, _, Nth),
    arg(Nth, AsPred, Result),
    remove_funct_arg(AsPred, Nth, AsFunction).

body_member(Body,BE,NewBE,NewBody):-
   conjuncts_to_list(Body,List),
   reverse(List,RevList),append(Left,[BE|Right],RevList),
   append(Left,[NewBE|Right],NewRevList),
   reverse(NewRevList,NewList),
   list_to_conjuncts(NewList,NewBody).



% show_mettalog_src(funct).

allow_concepts:- option_else(concepts,TF,true), \+ TF == false.
with_concepts(TF,Goal):- with_option(concepts,TF,Goal).
with_indents(TF,Goal):- with_option(src_indents,TF,Goal).

p2m(NC,NC):- var(NC),!.
p2m(NC,NC):- is_ftVar(NC),!.
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
p2m(NC,NC):- \+ compound(NC),!.
p2m(NC,[F]):- compound_name_arity(NC,F,0),!.
%p2m(is(V,Expr),let(V,Expr,'True')).
p2m((Head:-Body),O):- Body == true,!, O = (=(Head,'True')).
p2m((Head:-Body),O):- Body == fail,!, O = (=(Head,'False')).
p2m((Head:-Body),O):- conjuncts_to_list(Body,List),into_sequential(List,SP),!,O=(=(Head,SP)).

p2m((G,E),O):- conjuncts_to_list((G,E),List),into_sequential(List,O),!.
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

p2m(I,O):- I=..[F|II],maplist(p2m,[F|II],OO),O=OO.

prolog_to_metta(V,D) :- p2m(V,D),!.

%into_sequential(Body,SP):- \+ is_list(Body), conjuncts_to_list(Body,List), maplist(p2m,List,MList), into_sequential(MList,SP).
into_sequential(List,SP):- length(List,L),L>1,   maplist(prolog_to_metta,List,MList), SP =.. ['and'|MList],!.
into_sequential([SP],O):- prolog_to_metta(SP,O).
into_sequential([],'True').

combine_clauses(HeadBodiesList, NewHead, NewCombinedBodies) :-
    (HeadBodiesList = [] -> NewCombinedBodies = false, NewHead = _ ;
    findall(Head, member((Head:-_), HeadBodiesList), Heads),
    least_general_head(Heads, NewHead),
    transform_and_combine_bodies(HeadBodiesList, NewHead, NewCombinedBodies)).

least_general_head(Heads, LeastGeneralHead) :-
    lgu(Heads, LeastGeneralHead).

% Base case: the LGU of a single head is the head itself.
lgu([Head], Head) :- !.
% Recursive case: find the LGU of the head and the rest of the list.
lgu([H1|T], LGU) :-
    lgu(T, TempLGU),
    generalization(H1, TempLGU, LGU).

% Conceptual representation of finding the generalization of two heads
% This will require more complex processing depending on the actual structures of the heads.
generalization(Head1, Head2, GeneralizedHead) :-
    functor(Head1, Name, Arity),
    functor(Head2, Name, Arity), % assuming the functor names and arities are the same
    functor(GeneralizedHead, Name, Arity),
    generalize_args(Arity, Head1, Head2, GeneralizedHead).

% Base case: no more arguments to generalize.
generalize_args(0, _, _, _) :- !.
% Recursive case: generalize the corresponding arguments of the heads.
generalize_args(N, Head1, Head2, GeneralizedHead) :-
    arg(N, Head1, Arg1),
    arg(N, Head2, Arg2),
    (Arg1 = Arg2 ->
        arg(N, GeneralizedHead, Arg1);
        arg(N, GeneralizedHead, _) % Create a new variable if the corresponding arguments are different.
    ),
    N1 is N - 1,
    generalize_args(N1, Head1, Head2, GeneralizedHead).


transform_and_combine_bodies([(Head:-Body)|T], NewHead, CombinedBodies) :-
    transform(Head, NewHead, Body, TransformedBody),
    combine_bodies(T, NewHead, TransformedBody, CombinedBodies).

combine_bodies([], _, Combined, Combined).
combine_bodies([(Head:-Body)|T], NewHead, Acc, CombinedBodies) :-
    transform(Head, NewHead, Body, TransformedBody),
    NewAcc = (Acc;TransformedBody),
    combine_bodies(T, NewHead, NewAcc, CombinedBodies).

combine_code(Code, Body, Code) :- Body==true, !.
combine_code(Code, Body, Body) :- Code==true, !.
combine_code(Code, Body, (Code, Body)).


create_unifier(OneHead,NewHead,Code):- OneHead=@=NewHead,OneHead=NewHead,Code=true.
create_unifier(OneHead,NewHead,unify_with_occurs_check(OneHead,NewHead)).

transform(OneHead, NewHead, Body, NewBody):- create_unifier(OneHead,NewHead,Code),
   combine_code(Code,Body,NewBody).



% ===============================
%    TESTING
% ===============================


fb:- make,
   writeln(';; ==========================================='),
   forall((clause(fb0,Goal),write(';; '),writeq(?- Goal),nl,call(Goal)),
   writeln(';; ===========================================')).

fb0:- show_mettalog_src((two_pi(R):-(pi(A), +(A, A, B), R is B))).
fb0:- show_mettalog_src(factorial_tail_basic).

show_mettalog_src:- make,
  for_all((source_file(AsPred,File),
          symbol_contains(File,metta)),
         show_mettalog_src(AsPred)).

show_mettalog_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_mettalog_src(F,A)).
show_mettalog_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_mettalog_src(F,A)).
show_mettalog_src(C):- show_cvts(C),!.


show_space_src:- make,
  for_all(space_preds(AsPred),show_space_src(AsPred)).

show_space_src(F/A):- nonvar(F),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(AsPred):- functor(AsPred,F,A), \+ \+ current_predicate(F/A), !, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(F):-  atom(F), \+ \+ current_predicate(F/_),!, forall(current_predicate(F/A), show_space_src(F,A)).
show_space_src(C):-  atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),!, forall((current_predicate(F/A),once(atom_contains(F,C))), show_space_src(F,A)).
show_space_src(C):- show_cvts(C),!.


show_cvts(Term):-
  once((is_list(Term), s2p(Term,PF))), \+ is_list(PF),!,show_cvts(PF).

show_cvts(Term):- compound(Term),Term=(_=_),!, ppc(orig,Term),Term = FunctForm,
  functs_to_preds(_RetResult,FunctForm,Prolog), ppc(preds,Prolog),
  preds_to_functs(Prolog,NFunctForm), ppc(functs,NFunctForm).
show_cvts(Term):- ppc(orig,Term),
  preds_to_functs(Term,FunctForm), ppc(functs,FunctForm),
  functs_to_preds(_RetResult,FunctForm,Prolog), ppc(preds,Prolog).

show_mettalog_src(F,A):- functor(Head,F,A),
  ignore((predicate_property(Head,number_of_clauses(_)),
    source_file(Head,File),atom_contains(File,metta),!,
    nl,findall((Head:-Body),
       clause(Head,Body), Clauses),
    print_metta_clauses(Clauses))),nl.

print_metta_clauses([]):- !.
print_metta_clauses([Head:-Body]):- !, print_metta_clause(Head,Body).
print_metta_clauses(Clauses):- combine_clauses(Clauses,Head,Body),!,print_metta_clause(Head,Body).
print_metta_clause(Head,Body):-
  print_metta_clause0(Head,Body),
  show_cvts(Head:-Body).

print_metta_clause0(Head,Body):- Body == true,!, pp_metta([=,Head,'True']).
print_metta_clause0(Head,Body):- Body == false,!, pp_metta([=,Head,'False']).
print_metta_clause0(Head,Body):- conjuncts_to_list(Body,List), into_sequential(List,SP), pp_metta([=,Head,SP]).


% ===============================
%       PRINTERS
% ===============================

ppc(Msg,Term):- ppc1(Msg,Term), p2m(Term,Metta),!, (Metta\==Term -> ppc1(p2m(Msg),Metta) ; true).

ppc1(Msg,Term):- \+ \+ ( ignore(guess_pretty(Term)),
   writeln('---------------------'),
   write(p(Msg)),write(':'),nl,
   portray_clause(Term),
   writeln('---------------------'),
   \+ \+ (print_tree(?-show_cvts(Term))),nl,
    writeln('---------------------'),
     write(s(Msg)),write(':'),nl,
     write_src(Term),nl).


pp_metta(P):- pretty_numbervars(P,PP),with_option(concepts=false,pp_fb(PP)).

write_src(V):- notrace(write_src0(V)).
write_src0(V):- allow_concepts,!,with_concepts(false,write_src1(V)),flush_output.
write_src0(V):- is_list(V),!,pp_sexi(V).
write_src0(V):- write_src1(V),!.

is_final_write(V):- var(V), !, format('$~p',[V]).
is_final_write('$VAR'(S)):- !, write('$'),write(S).

write_src1(V) :- is_final_write(V),!.
write_src1((Head:-Body)) :- !, print_metta_clause0(Head,Body).
write_src1(''):- !, writeq('').
write_src1(V):- number(V),!, writeq(V).
write_src1(V):- string(V),!, writeq(V).
write_src1(V):- symbol(V),needs_quoted_in_metta(V,_),!, symbol_string(V,S),writeq(S).
write_src1(V):- symbol(V),!,write(V).
write_src1(V):- compound(V), \+ is_list(V),!,write_mobj(V).
write_src1(V):- pp_sex(V),!.

write_mobj(V) :- is_final_write(V),!.
write_mobj(V):- ( \+ compound(V) ; is_list(V)),!, write_src0(V).

write_mobj(V):- compound_name_arguments(V,F,Args),write_mobj(F,Args),!.
write_mobj(V):- writeq(V).
write_mobj(exec,[V]):- !, write('!'),with_indents(false,write_src(V)).
write_mobj('$STRING',[S]):- !, writeq(S).
write_mobj(F,Args):- mlog_sym(K),pp_sexi([K,F|Args]).

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


