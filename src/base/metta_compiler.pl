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

% Meta-predicate that ensures that for every instance where G1 holds, G2 also holds.
:- meta_predicate(for_all(0,0)).
for_all(G1,G2):- forall(G1,G2).

:- op(700,xfx,'=~').

compound_non_cons(B):-  compound(B),  \+ B = [_|_].
iz_conz(B):- compound(B), B=[_|_].

/*
'=~'(A,B):- iz_conz(A),iz_conz(B),!,A=B.
'=~'(A,B):- var(A),iz_conz(B),!,A=B.
'=~'(A,B):- iz_conz(A),var(B),!,A=B.
'=~'(A,B):- compound_non_cons(A),var(B),!,A=..B.
*/
'=~'(A,B):- compound_non_cons(B),!,A=B.
'=~'(A,B):- '=..'(A,B).

into_list_args(C,[C]):- \+ compound(C),!.
into_list_args([H|T],[H|T]):- is_list(T),!.
into_list_args(u_assign(List, A),[H|T]):- append(List,[A],[H|T]),!.
into_list_args(holds(A),A):-!.
into_list_args(C,[F|Args]):- compound_name_arguments(C,F,Args),!.

compound_name_list(AsPred,FP,PredArgs):- var(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- iz_conz(AsPred),!,AsPred=[FP|PredArgs].
compound_name_list(AsPred,FP,PredArgs):- into_list_args(AsPred,[FP|PredArgs]),!.
compound_name_list(AsPred,FP,PredArgs):- compound_non_cons(AsPred),!,compound_name_arguments(AsPred,FP,PredArgs).
% ===============================
%       COMPILER / OPTIMIZER
% Scryer Compiler vs PySWIP ASM Compiler
%
% PySWIP is 222 times faster per join
%===============================




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

p2m(NC,NC):- var(NC),!.
p2m(NC,OO):- is_list(NC),!,maplist(p2m,NC,OO).
p2m(!, '!').  % Translate the cut operation directly.
p2m(false, 'False').
p2m(fail, 'False').  % Translate Prolog’s fail to MeTTa’s False.
p2m(true, 'True').  % Translate Prolog’s true to MeTTa’s True.
p2m(prolog, meTTa).  % Translate the atom prolog to meTTa.

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
    conjuncts_to_list(Body, List), % Converts a list of conjunctions into a sequential representation in MeTTa
    into_sequential(List, SP), !.
into_sequential(Nothing,'True'):- Nothing ==[],!.
 % If there's only one element
into_sequential([SP],O):- prolog_to_metta(SP,O).
% Otherwise, construct sequential representation using 'and'.
into_sequential(List, ['and'|SPList]) :- maplist(prolog_to_metta, List, SPList),!.



% combine_clauses/3 is the main predicate combining clauses with similar heads and bodies.
% HeadBodiesList is a list of clauses (Head:-Body)
% NewHead will be the generalized head representing all clauses in HeadBodiesList
% NewCombinedBodies will be the combined bodies of all clauses in HeadBodiesList.
combine_clauses(HeadBodiesList, NewHead, NewCombinedBodies) :-
    % If HeadBodiesList is empty, then NewCombinedBodies is 'false' and NewHead is an anonymous variable.
    (HeadBodiesList = [] -> NewCombinedBodies = false, NewHead = _ ;
    % Find all Heads in HeadBodiesList and collect them in the list Heads
    findall(Head, member((Head:-_), HeadBodiesList), Heads),
    % Find the least general head among the collected Heads
    least_general_head(Heads, LeastHead),
    functor(LeastHead,F,A),functor(NewHead,F,A),
    % Transform and combine bodies according to the new head found
    transform_and_combine_bodies(HeadBodiesList, NewHead, NewCombinedBodies)),
    \+ \+ (
     Print=[converting=HeadBodiesList,newHead=NewHead],
     numbervars(Print,0,_,[]),wdmsg(Print),in_cmt(portray_clause(( NewHead :- NewCombinedBodies)))),!.

% Predicate to find the least general unified head (LGU) among the given list of heads.
% Heads is a list of head terms, and LeastGeneralHead is the least general term that unifies all terms in Heads.
least_general_head(Heads, LeastGeneralHead) :-
    lgu(Heads, LeastGeneralHead).

% the LGU of a single head is the head itself.
lgu([Head], Head) :- !.
% find the LGU of the head and the rest of the list.
lgu([H1|T], LGU) :-
    lgu(T, TempLGU),
    % Find generalization between head H1 and temporary LGU
    generalization(H1, TempLGU, LGU).

% generalization/3 finds the generalization of two heads, Head1 and Head2, which is represented by GeneralizedHead.
% This predicate is conceptual and will require more complex processing depending on the actual structures of the heads.
generalization(Head1, Head2, GeneralizedHead) :-
    % Ensure the functor names and arities are the same between Head1 and Head2.
    functor(Head1, Name, Arity),
    functor(Head2, Name, Arity),
    functor(GeneralizedHead, Name, Arity),
    % Generalize the arguments of the heads.
    generalize_args(Arity, Head1, Head2, GeneralizedHead).

% no more arguments to generalize.
generalize_args(0, _, _, _) :- !.
% generalize the corresponding arguments of the heads.
generalize_args(N, Head1, Head2, GeneralizedHead) :-
    arg(N, Head1, Arg1),
    arg(N, Head2, Arg2),
    % If the corresponding arguments are equal, use them. Otherwise, create a new variable.
    (Arg1 = Arg2 -> arg(N, GeneralizedHead, Arg1); arg(N, GeneralizedHead, _)),
    % Continue with the next argument.
    N1 is N - 1,
    generalize_args(N1, Head1, Head2, GeneralizedHead).

% transform_and_combine_bodies/3 takes a list of clause heads and bodies, a new head, and produces a combined body representing all the original bodies.
% The new body is created according to the transformations required by the new head.
transform_and_combine_bodies([(Head:-Body)|T], NewHead, CombinedBodies) :-
    % Transform the body according to the new head.
    transform(Head, NewHead, Body, TransformedBody),
    % Combine the transformed body with the rest.
    combine_bodies(T, NewHead, TransformedBody, CombinedBodies).

/* OLD
% Define predicate combine_clauses to merge multiple Prolog clauses with the same head.
% It receives a list of clauses as input and returns a combined clause.
combine_clauses([Clause], Clause) :- !.  % If there's only one clause, return it as is.
combine_clauses(Clauses, (Head :- Body)) :-  % If there are multiple clauses, combine them.
    Clauses = [(Head :- FirstBody)|RestClauses],  % Decompose the list into the first clause and the rest.
    combine_bodies(RestClauses, FirstBody, Body).  % Combine the bodies of all the clauses.

% Helper predicate to combine the bodies of a list of clauses.
% The base case is when there are no more clauses to combine; the combined body is the current body.
combine_bodies([], Body, Body).
combine_bodies([(Head :- CurrentBody)|RestClauses], PrevBody, Body) :-
    % Combine the current body with the previous body using a conjunction (,).
    combine_two_bodies(PrevBody, CurrentBody, CombinedBody),
    % Recursively combine the rest of the bodies.
    combine_bodies(RestClauses, CombinedBody, Body).

% Predicate to combine two bodies.
% Handles the combination of different Prolog constructs like conjunctions, disjunctions, etc.
combine_two_bodies((A, B), (C, D), (A, B, C, D)) :- !.  % Combine conjunctions.
combine_two_bodies((A; B), (C; D), (A; B; C; D)) :- !.  % Combine disjunctions.
combine_two_bodies(A, B, (A, B)).  % Combine simple terms using conjunction.
*/

% if there are no more bodies, the accumulated Combined is the final CombinedBodies.
combine_bodies([], _, Combined, Combined).
% combine the transformed body with the accumulated bodies.
combine_bodies([(Head:-Body)|T], NewHead, Acc, CombinedBodies) :-
    transform(Head, NewHead, Body, TransformedBody),
    % Create a disjunction between the accumulated bodies and the transformed body.
    NewAcc = (Acc;TransformedBody),
    combine_bodies(T, NewHead, NewAcc, CombinedBodies).

% combine_code/3 combines Guard and Body to produce either Guard, Body, or a conjunction of both, depending on the values of Guard and Body.
combine_code(Guard, Body, Guard) :- Body==true, !.
combine_code(Guard, Body, Body) :- Guard==true, !.
combine_code(Guard, Body, (Guard, Body)).

% create_unifier/3 creates a unification code that unifies OneHead with NewHead.
% If OneHead and NewHead are structurally equal, then they are unified and the unification Guard is 'true'.
% Otherwise, the unification code is 'metta_unify(OneHead,NewHead)'.

create_unifier(OneHead,NewHead,Guard):- OneHead=@=NewHead,OneHead=NewHead,!,Guard=true.
create_unifier(OneHead,NewHead,Guard):- compound(OneHead),
  compound_name_list(OneHead,_,Args1),
  compound_name_list(NewHead,_,Args2),
  create_unifier_goals(Args1,Args2,Guard),!.
create_unifier(OneHead,NewHead,u(OneHead,NewHead)).

create_unifier_goals([V1],[V2],u(V1,V2)):-!.
create_unifier_goals([V1|Args1],[V2|Args2],RightGuard):-!,
  create_unifier_goals(Args1,Args2,Guard),
  combine_code(u(V1,V2),Guard,RightGuard).
create_unifier_goals([],[],true).


% transform/4 combines unification code with Body to produce NewBody according to the transformations required by NewHead.
% It uses create_unifier/3 to generate the unification code between OneHead and NewHead.
transform(OneHead, NewHead, Body, NewBody):- create_unifier(OneHead,NewHead,Guard),
   combine_code(Guard,Body,NewBody).




% ===============================
%  Compile in memory buffer
% ===============================

add_assertion(NewAssertion) :-
     expand_to_hb(NewAssertion,H,_),
     functor(H,F,A), functor(HH,F,A),
     assert(NewAssertion),
    % Get the current clauses of my_predicate/1
    findall(HH:-B,clause(HH,B),Prev),
    % Create a temporary file and add the new assertion along with existing clauses
    abolish(F/A),
    create_and_consult_temp_file(F/A, Prev).

% Predicate to create a temporary file and write the tabled predicate
create_and_consult_temp_file(PredName/Arity, PredClauses) :-
    % Generate a unique temporary memory buffer
    tmp_file_stream(text, TempFileName, TempFileStream),
    % Write the tabled predicate to the temporary file
    format(TempFileStream, ':- dynamic((~q)/~w).~n', [PredName, Arity]),
    %if_t( \+ option_value('tabling',false),
    if_t(option_value('tabling',true),format(TempFileStream,':- ~q.~n',[table(PredName/Arity)])),
    maplist(write_clause(TempFileStream), PredClauses),
    % Close the temporary file
    close(TempFileStream),
    % Consult the temporary file
    consult(TempFileName),
    % Delete the temporary file after consulting
    delete_file(TempFileName),
    assert(metta_compiled_predicate(PredName,Arity)).

% Helper predicate to write a clause to the file
write_clause(Stream, Clause) :-
    subst_vars(Clause,Can),
    write_canonical(Stream, Can),
    write(Stream, '.'),
    nl(Stream).


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
show_cvts(Term):- compound(Term),Term=(_=_),!, ppc(orig,Term),Term = FunctForm,
  functs_to_preds(_RetResult,FunctForm,Prolog), ppc(preds,Prolog),
  preds_to_functs(Prolog,NFunctForm), ppc(functs,NFunctForm).
show_cvts(Term):- ppc(orig,Term),
  preds_to_functs(Term,FunctForm), ppc(functs,FunctForm),
  functs_to_preds(_RetResult,FunctForm,Prolog), ppc(preds,Prolog).

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

% The predicate with_indents/2 modifies the src_indents option value during the execution of a goal.
% The first argument is the desired value for src_indents,
% and the second argument is the Goal to be executed with the given src_indents setting.
with_indents(TF, Goal) :-
    % Set the value of the `src_indents` option to TF and then execute the Goal
    with_option(src_indents, TF, Goal).

% The predicate allow_concepts/0 checks whether the use of concepts is allowed.
% It does this by checking the value of the concepts option and ensuring it is not false.
allow_concepts :-
    % Check if the option `concepts` is not set to false
    option_else(concepts, TF, true),
    \+ TF == false.

% The predicate with_concepts/2 enables or disables the use of concepts during the execution of a given goal.
% The first argument is a Boolean indicating whether to enable (true) or disable (false) concepts.
% The second argument is the Goal to be executed with the given concepts setting.
with_concepts(TF, Goal) :-
    % Set the value of the `concepts` option to TF and then execute the Goal
    with_option(concepts, TF, Goal).


% Various 'write_src' and 'write_src0' rules are handling the writing of the source,
% dealing with different types of values, whether they are lists, atoms, numbers, strings, compounds, or symbols.
write_src(V):- notrace(write_src0(V)).
write_src0(V):- allow_concepts,!,with_concepts(false,write_src1(V)),flush_output.
write_src0(V):- is_list(V),!,pp_sexi(V).
write_src0(V):- write_src1(V),!.

% Handling the final write when the value is a variable or a '$VAR' structure.
is_final_write(V):- var(V), !, format('$~p',[V]).
is_final_write('$VAR'(S)):- !, write('$'),write(S).

% Handling more cases for 'write_src1', when the value is a number, a string, a symbol, or a compound.
write_src1(V) :- is_final_write(V),!.
write_src1((Head:-Body)) :- !, print_metta_clause0(Head,Body).
write_src1(''):- !, writeq('').
write_src1(V):- number(V),!, writeq(V).
write_src1(V):- string(V),!, writeq(V).

% Continuing with 'write_src1', 'write_mobj', and related rules,
% handling different cases based on the value’s type and structure, and performing the appropriate writing action.
write_src1(V):- symbol(V),needs_quoted_in_metta(V,_),!, symbol_string(V,S),writeq(S).
write_src1(V):- symbol(V),!,write(V).
write_src1(V):- compound(V), \+ is_list(V),!,write_mobj(V).
write_src1(V):- pp_sex(V),!.

write_mobj(V) :- is_final_write(V),!.
write_mobj(V):- ( \+ compound(V) ; is_list(V)),!, write_src0(V).

write_mobj(V):- compound_name_list(V,F,Args),write_mobj(F,Args),!.
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



