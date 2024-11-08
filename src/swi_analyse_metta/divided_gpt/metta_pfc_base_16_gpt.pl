/* Predicate: is_simple_lhs/1
   Checks if the given term is a "simple" left-hand side term.
   A left-hand side term is considered simple if it does not involve certain "active" terms. */

%% is_simple_lhs(+Lhs) is semidet
%  This predicate breaks down complex left-hand sides like disjunctions (;) into individual components
%  and checks if each part is simple by calling is_simple_lhs/1 recursively.
%  @param Lhs Left-hand side of the rule.
%  @example 
%    ?- is_simple_lhs((a;b)).
%    true.
is_simple_lhs((Lhs1;Lhs2)) :- 
    !, % Green cut: once this clause succeeds, Prolog will not attempt other clauses.
    is_simple_lhs(Lhs1), % Check if Lhs1 is simple
    is_simple_lhs(Lhs2). % Check if Lhs2 is simple

%% is_simple_lhs(+ActN) is semidet
%  This clause fails if ActN is an "active" LHS term. 
%  @param ActN The action being checked.
is_simple_lhs(ActN) :- 
    is_active_lhs(ActN), % Check if ActN is an active left-hand side
    !, % Cut to avoid further clauses if ActN is active
    fail. % Fail since an active term makes this not a simple LHS.

%% is_simple_lhs(+Lhs) is semidet
%  If the term is a conjunction (/), it is automatically not simple.
is_simple_lhs((Lhs1/Lhs2)) :- 
    !, % Green cut
    fail, % Fail since conjunctions make the term non-simple
    is_simple_lhs(Lhs1), % This code will never execute due to the fail above
    is_simple_lhs(Lhs2).

%% is_simple_lhs(_) is semidet
%  This clause accepts any other terms as simple.
is_simple_lhs(_). 

/* Predicate: is_active_lhs/1
   Determines if a given term is considered "active".
   Active terms are certain Prolog control constructs or specific terms that affect execution. */

%% is_active_lhs(+ActN) is semidet
%  Fail if the term is a variable.
is_active_lhs(ActN) :- 
    var(ActN), % Check if ActN is a variable
    !, % Cut to avoid further clauses if ActN is a variable
    fail. % Variables are not considered active.

%% is_active_lhs(!) is semidet
%  The cut (!) operator is considered active.
is_active_lhs(!).

%% is_active_lhs(cut_c) is semidet
%  cut_c is a special term treated as active.
is_active_lhs(cut_c).

%% is_active_lhs(actn(_Act)) is semidet
%  An action term actn(_) is considered active.
is_active_lhs(actn(_Act)).

%% is_active_lhs('{}'(_Act)) is semidet
%  The '{}' term is considered active.
is_active_lhs('{}'(_Act)).

%% is_active_lhs((Lhs1/Lhs2)) is semidet
%  Active if either part of a conjunction (/) is active.
is_active_lhs((Lhs1/Lhs2)) :- 
    !, % Green cut
    is_active_lhs(Lhs1); % Check if Lhs1 is active
    is_active_lhs(Lhs2). % Check if Lhs2 is active

%% is_active_lhs((Lhs1,Lhs2)) is semidet
%  Active if either part of a conjunction (,) is active.
is_active_lhs((Lhs1,Lhs2)) :- 
    !, % Green cut
    is_active_lhs(Lhs1); % Check if Lhs1 is active
    is_active_lhs(Lhs2). % Check if Lhs2 is active

%% is_active_lhs((Lhs1;Lhs2)) is semidet
%  Active if either part of a disjunction (;) is active.
is_active_lhs((Lhs1;Lhs2)) :- 
    !, % Green cut
    is_active_lhs(Lhs1); % Check if Lhs1 is active
    is_active_lhs(Lhs2). % Check if Lhs2 is active

/* Predicate: add_lhs_cond/3
   Adds a condition to the left-hand side of a rule. */

%% add_lhs_cond(+Lhs1, +Lhs2, -Lhs1Cond) is det
%  If Lhs1 is a conjunction (/), append Lhs2 as an additional condition.
%  @example
%    ?- add_lhs_cond(a/b,c,R).
%    R = a/(b,c).
add_lhs_cond(Lhs1/Cond, Lhs2, Lhs1/(Cond,Lhs2)) :- 
    !. % Green cut: once this succeeds, no need to try the next clause.

%% add_lhs_cond(+Lhs1, +Lhs2, -Lhs1Lhs2) is det
%  If Lhs1 is not a conjunction, create a new conjunction.
add_lhs_cond(Lhs1, Lhs2, Lhs1/Lhs2).

/* Predicate: buildRhs/2
   Constructs the right-hand side (RHS) of a rule from a conjunction. */

%% buildRhs(+Conjunction, -Rhs) is det
%  Handles the base case where the input is a variable.
%  @param Conjunction The conjunction to process.
%  @param Rhs The constructed RHS as a list.
buildRhs(X, [X]) :- 
    var(X), % Base case: if X is a variable, wrap it in a list.
    !.

%% buildRhs(+Conjunction, -Rhs) is det
%  Recursively builds the RHS when the input is a conjunction (A,B).
buildRhs((A, B), [A2|Rest]) :- 
    !, % Green cut
    pfcCompileRhsTerm(A, A2), % Compile the first term A into A2
    buildRhs(B, Rest). % Recursively process the rest of the conjunction

%% buildRhs(+Term, -Rhs) is det
%  Compiles a single term into the RHS.
buildRhs(X, [X2]) :- 
    pfcCompileRhsTerm(X, X2). % Compile the term X into X2

/* Predicate: pfcCompileRhsTerm/2
   Compiles a right-hand side term, potentially turning P/C into (P :- C). */

%% pfcCompileRhsTerm(+Term, -CompiledTerm) is det
%  If the input is a conditional (P/C), turn it into (P :- C).
pfcCompileRhsTerm((P/C), ((P:-C))) :- 
    !.

%% pfcCompileRhsTerm(+P, -P) is det
%  Otherwise, leave the term unchanged.
pfcCompileRhsTerm(P, P).

/* previously: The following section has been retained, though some of it 
   seems unused or redundant. It is here for historical purposes. */


   % %  pfc_unnegate(N,P) is true if N is a negated term and P is the term
% %  with the negation operator stripped.

pfc_unnegate(P,_):- var(P),!,fail.
pfc_unnegate((~P),P):-  \+ tilded_negation.
pfc_unnegate((-P),P).
pfc_unnegate((\+(P)),P).

pfcNegatedLiteral(P) :-
  callable(P),
  pfc_unnegate(P,Q),
  pfcPositiveLiteral(Q).

pfcLiteral(X) :- pfcNegatedLiteral(X).
pfcLiteral(X) :- pfcPositiveLiteral(X).

pfcPositiveLiteral(X) :-
  callable(X),
  functor(X,F,_),
  \+ pfcConnective(F).

pfcConnective(';').
pfcConnective(',').
pfcConnective('/').
pfcConnective('|').
pfcConnective(('==>')).
pfcConnective(('<-')).
pfcConnective('<==>').

pfcConnective('-').
pfcConnective('~'):- \+ tilded_negation.
pfcConnective(( \+ )).

is_implicitly_prolog(Callable):- \+ callable(Callable),!, fail.
is_implicitly_prolog(_ is _).

processRule(Lhs,Rhs,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Rhs,Rhs2),
  current_why_U(USER), % @TODO REVIEW _U
  pfcForEach(pfc_nf(Lhs,Lhs2),
          buildRule(Lhs2,rhs(Rhs2),(ParentRuleCopy,USER))).

buildRule(Lhs,Rhs,Support) :-
  buildTrigger(Lhs,Rhs,Trigger),
  fcEvalLHS(Trigger,Support).

buildTrigger([],Consequent,Consequent).

buildTrigger([Test|Triggers],Consequent,(Test *-> X)) :- is_implicitly_prolog(Test),
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([V|Triggers],Consequent,'$pt$'(V,X)) :-
  var(V),
  !,
  buildTrigger(Triggers,Consequent,X).


buildTrigger([(T1/Test)|Triggers],Consequent,'$nt$'(T2,Test2,X)) :-
  pfc_unnegate(T1,T2),
  !,
  buildNtTest(T2,Test,Test2),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([(T1)|Triggers],Consequent,'$nt$'(T2,Test,X)) :-
  pfc_unnegate(T1,T2),
  !,
  buildNtTest(T2,true,Test),
  buildTrigger(Triggers,Consequent,X).

buildTrigger([{Test}|Triggers],Consequent,(Test *-> X)) :-
  !,
  buildTrigger(Triggers,Consequent,X).

buildTrigger([T/Test|Triggers],Consequent,'$pt$'(T,X)) :-

