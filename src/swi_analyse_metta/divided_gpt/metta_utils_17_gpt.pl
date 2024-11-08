/* 
  p1_call/2 recursively handles logical expressions like conjunctions (and), disjunctions (or), and negations (not).
  Each predicate ensures that the first argument is callable, and then proceeds to evaluate it.
*/

% Attempt to handle disjunctions in the form (P1;Q1).
% @param P1 First part of the disjunction.
% @param Q1 Second part of the disjunction.
% @param E Environment for the call.
p1_call((P1;Q1),E):- 
    must_be(callable,P1),  % Ensure P1 is callable.
    !,  % Use cut to avoid backtracking.
    (p1_call(P1,E);p1_call(Q1,E)).  % Call P1, and if it fails, call Q1.

% Attempt to handle conjunctions in the form (P1,Q1).
% @param P1 First part of the conjunction.
% @param Q1 Second part of the conjunction.
% @param E Environment for the call.
p1_call((P1,Q1),E):- 
    must_be(callable,P1),  % Ensure P1 is callable.
    !,  % Use cut to avoid backtracking.
    (p1_call(P1,E),p1_call(Q1,E)).  % Call both P1 and Q1.

% Handle disjunctions in the form or(P1,Q1).
% @param P1 First part of the disjunction.
% @param Q1 Second part of the disjunction.
% @param E Environment for the call.
p1_call(or(P1,Q1),E):- 
    must_be(callable,P1),  % Ensure P1 is callable.
    !, 
    (p1_call(P1,E);p1_call(Q1,E)).  % Same as above but using the or/2 functor.

% Handle conjunctions in the form and(P1,Q1).
% @param P1 First part of the conjunction.
% @param Q1 Second part of the conjunction.
% @param E Environment for the call.
p1_call(and(P1,Q1),E):- 
    must_be(callable(P1),  % Ensure P1 is callable.
    !, 
    (p1_call(P1,E),p1_call(Q1,E)).  % Same as conjunction above but using the and/2 functor.

% Handle double negation not(not(P1)).
% @param P1 Predicate to negate twice.
% @param E Environment for the call.
p1_call(not(not(P1)),E):- 
    !, 
    \+ \+ p1_call(P1,E).  % Double negation reduces to calling P1.

% Handle negation not(P1).
% @param P1 Predicate to negate.
% @param E Environment for the call.
p1_call(not(P1),E):- 
    !, 
    not(p1_call(P1,E)).  % Negate the result of p1_call.

% Call once(P1) to enforce the predicate only succeeds once.
% @param P1 Predicate to call once.
% @param E Environment for the call.
p1_call(once(P1),E):- 
    !, 
    once(p1_call(P1,E)).

% Ignore failures from P1, allowing success regardless.
% @param P1 Predicate to ignore failures from.
% @param E Environment for the call.
p1_call(ignore(P1),E):- 
    !, 
    ignore(p1_call(P1,E)).

% Check if P1 can succeed without leaving choice points.
% @param P1 Predicate to check.
% @param E Environment for the call.
p1_call(chk(P1),E):- 
    !, 
    \+ \+ (p1_call(P1,E)).  % Check if P1 succeeds without leaving choice points.

% Handle negation \+ (P1).
% @param P1 Predicate to negate.
% @param E Environment for the call.
p1_call( \+ (P1),E):- 
    !, 
    \+ p1_call(P1,E).

% Default case: call P1 in environment E.
% @param P1 Predicate to call.
% @param E Environment for the call.
p1_call(P1,E):- 
    !, 
    call(P1,E).

/* 
  chk/2 is a convenience predicate that wraps around a call to ensure it succeeds without leaving choice points. 
  It provides a way to check success without affecting the logic of the program.
*/
% @param X Predicate to check.
% @param E Environment for the call.
chk(X,E):- 
    \+ \+ call(X,E).  % Check if X succeeds without leaving choice points.

/* 
  p2_call_p2/4 manages sequential execution of two predicates, P2a and P2b.
  The result of the first call is passed as input to the second.
*/
% @param P2a First predicate to call.
% @param P2b Second predicate to call.
% @param A Input to the first call.
% @param B Output from the second call.
p2_call_p2(P2a,P2b,A,B):- 
    p2_call(P2a,A,M),  % Call P2a with input A, result M.
    p2_call(P2b,M,B).  % Call P2b with input M, result B.

/* 
  p2_call/3 processes complex terms like lists, conjunctions, and disjunctions.
  It can handle custom functors like type/2, and also directly call predicates.
*/

% Base case: when P2 is an empty list, A and B are the same.
% @param P2 The empty list.
% @param A Input term.
% @param B Output term.
p2_call(P2,A,B):- 
    P2==[],  % Check if P2 is an empty list.
    !, 
    A=B.  % A and B are equal in this case.

% Special case: call p1_call within p2_call.
% @param P1 Predicate to call via p1_call.
% @param E Input.
% @param O Output.
p2_call(p1_call(P1),E,O):- 
    !, 
    p1_call(P1,E),  % Call P1.
    E=O.  % Set output equal to input.

% Recursive case: handle single-element lists.
% @param P2 A single element.
% @param Grid Input.
% @param GridN Output.
p2_call([P2],Grid,GridN):- 
    !, 
    p2_call(P2, Grid,GridN).  % Process the single element.

% Recursive case: handle multi-element lists.
% @param P2 Head of the list.
% @param P2L Tail of the list.
% @param Grid Input.
% @param GridN Output.
p2_call([P2|P2L],Grid,GridN):- 
    !, 
    p2_call(P2, Grid,GridM),  % Process head.
    p2_call(P2L,GridM,GridN).  % Process tail.

/* The rest of the code follows similar patterns with more specialized cases. */


p2_call(ignore(P2),A,B):- p2_call(P2,A,B)*->true;A=B.
p2_call(type(Type,P2),A,B):- into_type(Type,A,AA),p2_call(P2,AA,B).
p2_call(or(P2,Q2),A,B):- nop(must_be(callable,P2)),!, (p2_call(P2,A,B);p2_call(Q2,A,B)).
p2_call(and(P2,Q2),A,B):- nop(must_be(callable,P2)),!, (p2_call(P2,A,AB),p2_call(Q2,AB,B)).
p2_call(P2,A,B):- must_be(callable,P2), call(P2,A,B).


p1_or(P1A,P1B,X):- p1_call(P1A,X)->true;p1_call(P1B,X).
p1_and(P1A,P1B,X):- p1_call(P1A,X),p1_call(P1B,X).
p1_not(P1,E):- \+ p1_call(P1,E).
p1_ignore(P1,E):- ignore(p1_call(P1,E)).
p1_arg(N,P1,E):- tc_arg(N,E,Arg),p1_call(P1,Arg).
p1_subterm(P1,E):- sub_term(Arg,E),p1_call(P1,Arg).

:- meta_predicate my_partition(-, ?, ?, ?).
my_partition(_,[],[],[]):-!.
my_partition(P1,[H|L],[H|I],E):- \+ \+ p1_call(P1,H),!,
  my_partition(P1,L,I,E).
my_partition(P1,[H|L],I,[H|E]):-
   my_partition(P1,L,I,E),!.
my_partition(P1,H,I,HE):- arcST,ibreak,
  my_partition(P1,[H],I,HE).


mapgroup(P2,G1,L2):- into_list(G1,L1),!, with_my_group(L1,maplist(P2,L1,L2)).
mapgroup(P1,G1):- into_list(G1,L1), !, with_my_group(L1,maplist(P1,L1)).

selected_group(Grp):- nb_current('$outer_group',Grp),!.
selected_group([]).

sub_cmpd(_, LF) :- \+ compound(LF), !, fail.
sub_cmpd(X, X).
sub_cmpd(X, Term) :-
    (   is_list(Term)
    ->  member(E, Term),
        sub_cmpd(X, E)
    ;   tc_arg(_, Term, Arg),
        sub_cmpd(X, Arg)
    ).



%with_my_group([O|Grp],Goal):- compound(O),O=obj(_),!, locally(nb_setval('$outer_group',[O|Grp]),Goal).
with_my_group(_,Goal):- call(Goal).

into_mlist(L,L).
my_maplist(P4,G1,L2,L3,L4):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P4,L1,L2,L3,L4)).
my_maplist(P3,G1,L2,L3):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P3,L1,L2,L3)).
my_maplist(P2,G1,L2):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P2,L1,L2)).
my_maplist(P1,G1):- into_mlist(G1,L1), !, with_my_group(L1,maplist(P1,L1)).


my_include(P1,L,I):- include(p1_call(P1),L,I).
%my_include(P1,[H|L],O):- (p2_call(p1_call(P1),H,HH)*->(my_include(P1,L,I),O=[HH|I]);my_include(P1,L,O)).

