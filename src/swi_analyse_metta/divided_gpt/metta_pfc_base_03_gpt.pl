'$exported_op'(_,_,_):- fail.
*/


:- multifile '$pldoc'/4.       % Declares '$pldoc'/4 as a multifile predicate, meaning it can be spread across different files.
:- dynamic '$pldoc'/4.         % Declares '$pldoc'/4 as dynamic, so it can be modified at runtime.
:- discontiguous '$pldoc'/4.   % Declares '$pldoc'/4 as discontiguous, allowing non-continuous clauses in code.
'$pldoc'(_,_,_,_):- fail.      % Always fails, serving as a fallback when no documentation exists.


/** <module> Autoload Definitions for Prolog

Defines the predicates related to autoloading in Prolog. Similar to PLDoc, these are defined as multifile and dynamic to
support flexibility and runtime modification.

*/

:- multifile '$autoload'/3.    % Declares '$autoload'/3 as a multifile predicate for autoloading predicates.
:- discontiguous '$autoload'/3. % Declares '$autoload'/3 as discontiguous.
:- dynamic '$autoload'/3.      % Declares '$autoload'/3 as dynamic.
'$autoload'(_,_,_):- fail.     % Always fails as a fallback for undefined autoloading cases.


/* previously: The following was skipped to avoid unnecessary flag settings. 
   Setting `retry_undefined` to `kb_shared` and `pfc_ready` to true is commented out because 
   these may not be relevant for all environments. 
*/

% :- set_prolog_flag(retry_undefined, kb_shared).  % Commented out, possibly because it isn't used in all contexts.
% :- set_prolog_flag(pfc_ready, true).             % Same here, related to some PFC setup.

:- set_prolog_flag(expect_pfc_file,unknown). 
:- endif.

% Sets a Prolog flag to handle unknown PFC files.

% Directive to import `day_of_the_week/2` and `day_of_the_year/2` from `date` module in IFProlog.
:- ifprolog:import(date:day_of_the_week/2).
:- ifprolog:import(date:day_of_the_year/2).

tilded_negation.

/** <predicate> bagof_or_nil/3

Attempts to find all solutions for `T` in `G` using `bagof/3`, but defaults to an empty list if no solutions exist.

@param T Template for the terms to be collected.
@param G Goal to be satisfied.
@param L List of solutions (or empty list).

@example
?- bagof_or_nil(X, member(X, [1, 2, 3]), L).
L = [1, 2, 3].
*/

bagof_or_nil(T,G,L):- bagof(T,G,L)*->true;L=[].  % Uses `bagof/3`, if it fails, L is set to an empty list.

/** <predicate> setof_or_nil/3

Same as `bagof_or_nil/3`, but uses `setof/3` to collect a sorted, non-duplicate list of solutions.

@param T Template for the terms to be collected.
@param G Goal to be satisfied.
@param L List of unique solutions (or empty list).

@example
?- setof_or_nil(X, member(X, [3, 1, 2, 2]), L).
L = [1, 2, 3].
*/

setof_or_nil(T,G,L):- setof(T,G,L)*->true;L=[].  % Similar to `bagof_or_nil`, but uses `setof/3` to return a sorted list.


/** <predicate> call_u/1

Calls a goal `G` using `pfcCallSystem/1`, likely tied to some PFC (Prolog Forward Chaining) system.

@param G Goal to be called.

*/

call_u(G):- pfcCallSystem(G).  % Calls the goal `G` through `pfcCallSystem`.


/** <predicate> clause_u/2

Retrieves a clause (head and body) using the `clause/2` built-in predicate.

@param H Head of the clause.
@param B Body of the clause.

*/

clause_u(H,B):- clause(H,B).  % Wrapper for the built-in `clause/2` predicate.


/** <predicate> mpred_ain/1

Asserts a predicate `P` into the database using `arc_assert/1`. The `arc_assert/1` ensures consistency
with any current justifications (`current_why_UU/1`).

@param P Predicate to be asserted.

*/

mpred_ain(P):- arc_assert(P).  % Asserts the predicate `P` using `arc_assert/1`.

% Special case where `P:-True` is treated differently, asserting only `P` when `True == true`.
arc_assert(P:-True):- True==true,!,arc_assert(P).
arc_assert(P):-  % fbugio(arc_assert(P)),
  must_ex(current_why_UU(UU)),nop(fbugio(pfcAdd(P, UU))),!,
(P, UU),asserta_if_new(P).


/** <predicate> pfc_retract/1
Retracts a predicate `P` from the database using `pfcRetract/1`.
@param P Predicate to be retracted.
*/

pfc_retract(P):- fbugio(pfc_retract(P)),pfcRetract(P).  % Logs and retracts `P` using `pfcRetract/1`.


/** <predicate> pfc_retractall/1

Retracts all instances of predicate `P` using `pfcRetractAll/1`.

@param P Predicate to be retracted.

*/

pfc_retractall(P):- fbugio(pfc_retractall(P)),pfcRetractAll(P).  % Logs and retracts all instances of `P`.


% Declare a dynamic predicate for negation as failure.
:- dynamic((~)/1.
~(_):- fail.  % This negation predicate always fails, providing a simple negation behavior.


/** <predicate> add/1

Adds a fact or rule `X` to the database using `pfcAdd/1`.

@param X Fact or rule to be added.

*/

add(X):- pfcAdd(X).  % Adds `X` to the database using `pfcAdd/1`.


/** <predicate> mpred_test/1

Tests a goal `X` using the `call_u/1` system call. It provides detailed reasoning (`mpred_test_why/1`)
if the goal is not satisfied.

@param X Goal to be tested.

*/

mpred_test(call_u(X)):- nonvar(X),!,pfcCallSystem(X),pfcWhy(X).  % If `X` is not a variable, calls and logs the reason.
mpred_test(\+ call_u(X)):- nonvar(X),!, (call_u(X) -> (fbugio(warn(failed(mpred_test(\+ call_u(X))))),mpred_test_why(X)); mpred_test_why(~(X))).
mpred_test(X):- (mpred_test_why(X) *-> true ; mpred_test_why(~(X))).  % Tests the goal or its negation.


% Thread-local declarations for storing child and dependency relationships.
:- thread_local t_l:shown_child/1.
:- thread_local t_l:shown_dep/2.


/** <predicate> pfc_info/1

Provides detailed information about a predicate `X` by testing it and showing related children and dependencies.

@param X Predicate to retrieve information for.

*/

pfc_info(X):- mpred_info(X).  % Calls `mpred_info/1` to get detailed information about `X`.


/** <predicate> mpred_info/1

Provides detailed information and dependencies for a predicate `X`.

@param X Predicate to retrieve information for.

*/

mpred_info(X):-
 retractall(t_l:shown_child(_)),  % Clear previous child records.
 retractall(t_l:shown_dep(_,_)),  % Clear previous dependency records.
 ignore((
  forall(mpred_test_why(X),true),  % Test the predicate and log results.
  forall(mpred_child_info(X),true))).

% Retrieves information about the children of `P`.
mpred_child_info(P):-
  retractall(t_l:shown_child(_)),
  show_child_info(P),!,
  printLine.

% Shows the child information for predicate `P`.
show_child_info(P):-
  pfcChildren(P,L),
  show_child_info(P,L),!.

% Avoids showing duplicate child information.
show_child_info(P,_):- t_l:shown_child(Q),P=@=Q,!.
show_child_info(P,_):- asserta(t_l:shown_child(P)),fail.
show_child_info(_,[]):-!.
show_child_info(P,L):- list_to_set(L,S),
  format("~N~nChildren for ",[]),
  ansi_format([fg(green)],'~@',[pp(P)]),  % Print the parent predicate in green.
  format(" :~n",[]),
  forall((member(D,S), \+ t_l:shown_dep(P,D)),(asserta(t_l:shown_dep(P,D)),ansi_format([fg(yellow)],'~N ~@. ~n',[pp(D)]))),  % Print the child predicates in yellow.
  my_maplist(show_child_info,S).  % Recursively show child info for all children.


/** <predicate> mpred_why/1

Explains why a predicate `X` holds by providing a detailed test and reasoning.

@param X Predicate to explain.

*/

mpred_why(X):- mpred_test_why(X).  % Calls `mpred_test_why/1` for detailed reasoning.


/** <predicate> mpred_test_why/1

Tests and explains why a goal `X` holds using the forward-chaining system.

@param X Goal to be tested.

*/

mpred_test_why(X):-
  pfcCallSystem(X)*->pfcTF1(X);(pfcTF1(X),!,fail).  % Tests the goal `X` using `pfcCallSystem/1`.


% Predicate definitions for Prolog terms related to positive literals and atoms.
mpred_literal(X):- pfcLiteral(X).
mpred_positive_literal(X):- pfcPositiveLiteral(X).
pfcAtom(X):- pfcLiteral(X).


/** <predicate> rem/1

Removes a predicate `X` using `pfcWithdraw/1`.

@param X Predicate to be removed.

*/

rem(X):- pfcWithdraw(X).  % Removes `X` using `pfcWithdraw/1`.


/** <predicate> rem2/1

Removes a predicate `X` using `pfcRemove/1`.

@param X Predicate to be removed.

*/

rem2(X):- pfcRemove(X).  % Removes `X` using `pfcRemove/1`.


/** <predicate> remove/1

Completely removes a predicate `X` using `pfcBlast/1`.

@param X Predicate to be removed.

*/

remove(X):- pfcBlast(X).  % Completely removes `X` from the system.


/* previously: Code for creating thread pools was commented out, possibly to avoid unnecessary overhead in
   systems where `mpred_ain_in_thread` is not required.
*/

% :- mpred_ain_in_thread.  % This was skipped, possibly to avoid complex thread management during asserts.
% :- current_thread_pool(ain_pool)->true;thread_pool_create(ain_pool,20,[]).  % Same here, for thread pool creation.


/** <predicate> create_pool/1

Creates a thread pool named `ain_pool` with 50 threads, detached, for parallel operations.

@param Pool Name of the pool to be created.

*/

:- multifile thread_pool:create_pool/1.
:- dynamic thread_pool:create_pool/1.
thread_pool:create_pool(ain_pool) :-
    thread_pool_create(ain_pool, 50, [detached(true)] ).  % Creates a thread pool for parallel processing.


/** <module> HTTP Server Support

Imports HTTP and thread pool libraries for handling multi-threaded HTTP requests in Prolog.

*/

:- use_module(library(http/thread_httpd)).  % Imports the HTTP server module for handling HTTP requests.
:- use_module(library(thread_pool)).  % Imports thread pool management library.


/** <predicate> is_ain_pool_empty/0

Checks if the `ain_pool` is empty by querying the running threads in the pool.

*/

is_ain_pool_empty:- thread_pool_property(ain_pool,running(N)),!,N==0.  % Checks if the pool has no running threads.