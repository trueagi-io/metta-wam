/*
 * @brief Set Prolog flag to ensure that PFC (Prolog Forward Chaining) 
 *        is shared across all modules, specifically setting it to the 'user' module.
 * @details The set_prolog_flag/2 predicate changes runtime flags. 
 *          Here, the flag 'pfc_shared_module' ensures the PFC features are 
 *          available for the 'user' module, which allows using PFC without 
 *          additional imports.
 * @example
 *      ?- set_prolog_flag(pfc_shared_module, user).
 *      true.
 */
:- set_prolog_flag(pfc_shared_module, user).

/* previously: % :- if( \+ current_predicate(set_fileAssertMt/1)). 
 * This line was skipped because it checks whether 'set_fileAssertMt/1' is already defined.
 * If it were not, the code within the 'if' block would execute. 
 * However, this might not be needed anymore because 'set_fileAssertMt/1' 
 * could have been predefined elsewhere, or it's being dynamically handled.
 */


%:- set_prolog_flag(pfc_shared_module,baseKB).
% This line sets a Prolog flag to use the 'baseKB' as a shared module for PFC (Prolog Forward Chaining).
% It is commented out, likely for debugging purposes or because it's set elsewhere in the code.

% Predicate: must_ex/1
% Executes the goal X and catches any exceptions. If the goal succeeds, it does nothing further.
% If it fails, it traces the error and retries the goal X with debugging enabled.
% @param X The goal to be executed.
% @example must_ex(my_goal).
must_ex(X) :-
    % Try to execute X and catch any exceptions (E) if they occur.
    catch(X, E, rtrace(E)) 
    % If X succeeds, succeed silently.
    *-> true ;
    % If X fails, log the failure and trace the execution of X.
    (dmsg(failed(must_ex(X))), rtrace(X)).

% Predicate: quietly_ex/1
% Calls the goal X silently without any additional output or side effects.
% @param X The goal to be executed.
% @example quietly_ex(my_goal).
quietly_ex(X) :- call(X).

% Predicate: control_arg_types/2
% This predicate was intended to control and check argument types, but it is currently disabled.
% The 'fail' predicate causes this version to always fail. It can be re-enabled after defining into_type/3.
% @param A Input argument
% @param B Output argument
% @TODO Re-enable this once into_type/3 is defined to avoid failure.
control_arg_types(A, B) :- 
    fail, 
    once(control_arg_types1(20, [], A, B)), 
    A \== B, 
    !.

%:- listing(control_arg_types/3).
% This directive lists the predicates and clauses related to control_arg_types when executed. It is commented out for now.

% Predicate: control_arg_types1/4
% Recursive function that processes and compares types for arguments.
% @param Max The maximum depth of recursion allowed.
% @param Pre A list of previously processed arguments.
% @param A The first argument to check.
% @param B The second argument to check.
control_arg_types1(Max, _, A, B) :-
    % If Max depth is exceeded, unify A with B.
    Max < 1, !, A = B.
control_arg_types1(_, _, A, B) :-
    % If A is not a compound term, unify A with B.
    \+ compound(A), !, A = B.
control_arg_types1(_, _, A, B) :-
    % If A is a cons (non-list structure), unify A with B.
    iz_conz(A), \+ is_list(A), !, A = B.
control_arg_types1(_, _, A, B) :-
    % Use the current_predicate check_args/2 if it exists, to check arguments.
    (current_predicate(check_args/2) -> check_args(A, B) -> A \=@= B), !.
% The following line was disabled since it�s unnecessary when A is a list.
%control_arg_types1(Max, Pre, A, B) :- is_list(A), !, maplist(control_arg_types1(Max, Pre), A, B).

control_arg_types1(Max, Pre, A, B) :-
    % Process compound arguments by breaking them into their components.
    Max0 is Max - 1,
    compound_name_arguments(A, F, AA),
    length(AA, N),
    do_control_arg_types1(Max0, F/N, 1, Pre, AA, BB),
    compound_name_arguments(B, F, BB).

% Predicate: do_control_arg_types1/6
% Helper function that processes arguments recursively and performs type checking.
% @param Max Maximum depth of recursion.
% @param FofN Functor and arity.
% @param ArgN Argument number.
% @param Pre List of previous arguments.
% @param AA List of arguments for term A.
% @param BB List of arguments for term B.
do_control_arg_types1(_Max, _FofN, _ArgNp1, _Pre, [], []) :- !.
do_control_arg_types1(Max, FofN, ArgN, Pre, [A | AA], [B | BB]) :-
    % Process each argument recursively.
    do_control_1arg_type(Max, FofN, ArgN, Pre, A, B),
    ArgNp1 is ArgN + 1,
    do_control_arg_types1(Max, FofN, ArgNp1, Pre, AA, BB).

% Predicate: do_control_1arg_type/6
% Helper function that processes a single argument and performs type checking.
% @param Max Maximum depth of recursion.
% @param F Functor name.
% @param N Argument number.
% @param Pre List of previous arguments.
% @param A The first argument.
% @param B The second argument.
do_control_1arg_type(_Max, _FN, _N, _Pre, A, B) :- var(A), !, B = A.
do_control_1arg_type(_Max, F/_, N, _Pre, A, B) :-
    % Call into_type/3 based on the functor and argument position.
    arg_n_isa(F, N, ISA), into_type(ISA, A, B), !.
do_control_1arg_type(Max, FofN, _, Pre, A, B) :-
    Max0 is Max - 1,
    control_arg_types1(Max0, [FofN | Pre], A, B).

% Predicate: arg_n_isa/3
% Finds the type (ISA) for a particular argument based on its functor and argument position.
% @param F Functor name.
% @param N Argument position.
% @param ISA The type (ISA) of the argument.
% This was disabled with fail but now works by clause_b.
arg_n_isa(F, N, ISA) :- clause_b(argIsa(F, N, ISA)).

% Predicate: save_pfc_state/0
% Saves the current state of PFC-related predicates to a file.
save_pfc_state :-
    % tell(pfcState),  % This would direct output to a file, but it's commented out.
    forall(
        (pfcStateTerm(F/A), current_predicate(F/A)),
        listing(F/A)
    ),
    % told.  % Closes the output stream (also commented out).
    !.

% Predicate: pfcDoAll/1
% Calls the goal for all results, ensuring each succeeds without failing.
% @param Goal The goal to execute for each result.
pfcDoAll(Goal) :- forall(call(Goal), true).

% Predicate: pfcStateTerm/1
% Identifies which predicates represent PFC-related state.
% @param F/A The functor and arity of the predicate.
pfcStateTerm(F/A) :- pfcDatabaseTerm(F/A).
pfcStateTerm(F/A) :-
    % List of functors and arities related to PFC state.
    member(
        (F/A), [
            fcUndoMethod/2,
            fcAction/2,
            fcTmsMode/1,
            pfcQueue/1,
            pfcCurrentDb/1,
            pfcHaltSignal/1,
            pfcDebugging/0,
            pfcSelect/1,
            pfcSearch/1
        ]
    ).

% This directive is executed conditionally if the 'xref' flag or module context is active.
:- if((current_prolog_flag(xref, true) ; ('$current_source_module'(SM), 'context_module'(M), '$current_typein_module'(CM), current_prolog_flag(pfc_shared_module, BaseKB), asserta(BaseKB:'wusing_pfc'(M, CM, SM, pfc_rt))))).
:- endif.

% The following section is executed if the 'xref' flag is true (cross-referencing).
:- if(current_prolog_flag(xref, true)).
% This line defines the 'pfc_rt' module (commented out).
%:- module(pfc_rt, []).
:- endif.

% This section handles file-specific contexts for loading/unloading files.
:- if((prolog_load_context(source, File), prolog_load_context(file, File))).
% This unloads the current file if it's being loaded again.
%:- prolog_load_context(file, File), unload_file(File).
:- use_module(library(logicmoo_utils)).  % This imports utilities from the logicmoo_utils library.
:- endif.

% This section is executed if the 'xref' flag is not true (normal mode).
:- if( \+  current_prolog_flag(xref,true)).
:- current_prolog_flag(pfc_shared_module,BaseKB),
   must_ex(retract(BaseKB:'wusing_pfc'(M,CM,SM,pfc_rt))),
   nop(fbugio(BaseKB:'chusing_pfc'(M,CM,SM,pfc_rt))),
   (M==SM ->
     (nop(maybe_ensure_abox(SM)),nop((M:ain(genlMt(SM,BaseKB)))));
     nop(fbugio(BaseKB:'lusing_pfc'(M,CM,SM,pfc_rt)))),
   assert(BaseKB:'$using_pfc'(M,CM,SM,pfc_rt)),
   asserta(SM:'$does_use_pfc_mod'(M,CM,SM,pfc_rt)).
   %backtrace(200).

/*
:- multifile '$exported_op'/3.
:- dynamic '$exported_op'/3.
:- discontiguous '$exported_op'/3.
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
/* 
   @predicate is_ain_pool_empty
   @desc This predicate checks if the `ain_pool` is empty. Currently, the body is missing.
   @example 
     ?- is_ain_pool_empty.
     true.
*/
is_ain_pool_empty.

/* 
   @predicate show_ain_pool
   @desc This predicate prints all properties of the `ain_pool` thread pool using `thread_pool_property/2`.
   @example 
     ?- show_ain_pool.
     show_ain_pool(pool_size(4)).
*/
show_ain_pool:- 
  /* Calls `forall/2` to iterate over every property of `ain_pool` and display it using `fmt/1`. */
  forall(thread_pool_property(ain_pool, PP), fmt(show_ain_pool(PP))).

/* 
   @predicate await_ain_pool
   @desc This predicate waits until the `ain_pool` is empty, using a combination of `repeat`, `sleep`, and `is_ain_pool_empty`.
   @example 
     ?- await_ain_pool.
     true.
*/
await_ain_pool :- 
  /* If the pool is empty, succeed, otherwise loop indefinitely, checking every 0.005 seconds. */
  is_ain_pool_empty -> true ; 
  (repeat, sleep(0.005), is_ain_pool_empty).

/* 
   @predicate ain_in_thread/1
   @param MAIN The task to be processed in the thread.
   @desc Strips the module from the input term and adds the resulting AIN to the thread pool using `call_in_thread`.
   @example 
     ?- ain_in_thread(foo:bar).
*/
ain_in_thread(MAIN) :- 
  /* Removes the module wrapper from the term */
  strip_module(MAIN, M, AIN), 
  /* Calls the module-specific predicate `pfcAdd` in a thread. */
  call_in_thread(M:pfcAdd(AIN)).

/* 
   @predicate call_in_thread/1
   @param MG The goal to be called in a thread.
   @desc Strips the module, serializes the goal, and runs it in a thread. 
   `copy_term/3`, `numbervars/3`, and `term_to_atom/2` are used to uniquely name the thread.
   @example 
     ?- call_in_thread(foo:bar).
*/
call_in_thread(MG) :- 
  /* Strips the module from the goal */
  strip_module(MG, M, G), 
  /* Non-traceable block to copy the goal and assign numbers to variables for serialization */
  notrace((
    copy_term(M:G, GG, _), 
    numbervars(GG, 0, _, [attvar(skip), singletons(true)]), 
    term_to_atom(GG, TN))),
  /* Initiates thread with unique name `TN` */
  call_in_thread(TN, M, G),
  /* Pretty-prints the call for debugging */
  dmsg_pretty(call_in_thread(TN, M, G)).

/* 
   @predicate call_in_thread/3
   @param TN The unique thread name.
   @param M The module the goal belongs to.
   @param G The goal to be executed.
   @desc Checks if the thread is already running; if not, creates a new one.
   @example 
     ?- call_in_thread(foo:bar).
*/
call_in_thread(TN, M, G) :- 
  /* Checks if a thread with alias `TN` exists */
  thread_property(_, alias(TN)), !, 
  /* If it exists, report it as already queued */
  dmsg_pretty(already_queued(M, G)).
call_in_thread(TN, M, G) :- 
  /* Calls `current_why/1` to get context for debugging purposes */
  must_ex(current_why(Why)), 
  /* Creates a new thread in `ain_pool` */
  thread_create_in_pool(ain_pool, call_in_thread_code(M, G, Why, TN), _Id, [alias(TN)]).

/* 
   @predicate call_in_thread_code/4
   @param M The module.
   @param G The goal.
   @param Why Context for debugging.
   @param TN The unique thread name.
   @desc Executes the goal in a thread and logs success, failure, or exceptions.
*/
call_in_thread_code(M, G, Why, TN) :- 
  /* Wraps the goal in the current `Why` context for debugging */
  with_only_current_why(Why, 
    /* Attempts to execute the goal and log outcomes (success, failure, or exception) */
    catch((
      M:G -> nop(dmsg_pretty(succeeded(exit, TN))) ; 
      dmsg_pretty(failed(exit, TN))
    ), 
    E, dmsg_pretty(error(E --> TN)))).

%   File: pfc
%   Author: Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: Consult system file for ensuring rules are properly loaded and managed.

/* 
   @predicate pfcVersion/1
   @param 3.0 The version of PFC system.
   @desc Declares the current version of the PFC system.
*/
pfcVersion(3.0).

/* previously: pfcFile('pfcsyntax') etc.
   These were directives to load PFC system files. Commented out as loading is now handled differently. 
   If needed in the future, uncomment for manual loading.
*/

/* previously: pfcLoad
   This predicate was responsible for loading all PFC system files but has been skipped. 
   This might be obsolete in systems where modules and files are automatically handled.
*/

/* previously: pfcFcompile
   This was used to compile PFC files but is commented out, likely due to modern systems favoring dynamic loading over pre-compilation.
*/

/* previously: call_in_thread(fbugio(call_in_thread))
   This was an example invocation of `call_in_thread` for debugging, but commented out to prevent unnecessary execution during runtime.
*/

/* 
   The following are operator definitions and syntactic sugar for PFC terms.
*/

/* 
   Declares the negation operator `~` at precedence 500.
*/
:- op(500, fx, '~').

/* 
   Declares logical implication operators for PFC rules at precedence 1050.
*/
:- op(1050, xfx, ('==>')).
:- op(1050, xfx, '<==>').
:- op(1050, xfx, ('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).

:- dynamic(pfctmp:knows_will_table_as/2).

/* previously: The following operator definitions were used in different parts of the PFC system.
   They are preserved for potential legacy support but are commented out due to redundancy in modern usage.
*/

/* 
   @predicate will_table_as/2
   @param Stuff The term to be tabled.
   @param As The tabling mode.
   @desc This predicate declares that `Stuff` should be treated as a table with mode `As`.
   @example 
     ?- will_table_as(foo, dynamic).
*/
will_table_as(Stuff, As) :- 
  /* Checks if the tabling is already known */
  pfctmp:knows_will_table_as(Stuff, As), !.
will_table_as(Stuff, As) :- 
  /* Asserts the new tabling rule and reacts to it */
  assert(pfctmp:knows_will_table_as(Stuff, As)),
  must_ex(react_tabling(Stuff, As)), !, fail.

/* 
   @predicate react_tabling/2
   @desc Ensures the term is dynamic once it is marked for tabling.
*/
react_tabling(Stuff, _) :- 
  /* Declares the term `Stuff` as dynamic */
  dynamic(Stuff).

:- dynamic(lmconf:is_treated_like_pfc_file/1).
:- dynamic(lmconf:is_pfc_module/1).

/* 
   Conditional checks if the current file/module should be treated like a PFC file based on naming conventions or module declarations.
*/
if_pfc_indicated :- 
  /* Checks the source file extension for `.pfc` */
  source_location(F, _), (sub_string(F, _, _, _, '.pfc') -> true ; lmconf:is_treated_like_pfc_file(F)), !.
if_pfc_indicated :- 
  /* Checks if the module is a PFC module */
  prolog_load_context(module, M), lmconf:is_pfc_module(M), !.

/* 
   @predicate skip_pfc_term_expansion/1
   @desc Skips certain terms from PFC expansion, like variables or file markers.
*/
skip_pfc_term_expansion(Var) :- 
  /* Skips if the input is a variable */
  var(Var), !.
skip_pfc_term_expansion(begin_of_file). 
skip_pfc_term_expansion(end_of_file).

/* 
   Directive to export the `pfc_term_expansion/2` predicate, making it available for other modules.
*/
:- export(pfc_term_expansion/2).
/** <module> Pfc Core

This module defines core predicates for Pfc (Prolog forward chaining). It includes 
term expansion for Pfc rules, dynamic predicates, and initialization of global assertions.

@tbd Ensure better documentation for predicates as they are defined and used.
*/

% Import the `pfc_term_expansion/2` predicate from the system module.
/* previously: system:import(pfc_term_expansion/2). */
:- system:import(pfc_term_expansion/2).

/** pfc_term_expansion(+I, -O)
    Perform term expansion for Pfc rules.

    This predicate defines how certain Prolog terms, such as table declarations, 
    ==> rules, and <==> rules, are expanded to incorporate Pfc-specific functionality.

    @param I The input term to be expanded.
    @param O The output term after expansion.

    @example
    ===
    ?- pfc_term_expansion((P ==> Q), Output).
    Output = (:- pfcAdd((P ==> Q))).
    ===
*/

% Predicate to skip Pfc term expansion if certain conditions are met.
/* Explanation: If we need to skip Pfc term expansion, the predicate succeeds and returns I as O. */
pfc_term_expansion(I, O) :- skip_pfc_term_expansion(I), !, I = O.

% Handle the expansion of table declarations when 'Stuff' is defined with a specific 'Type'.
/* Explanation: If 'Stuff' is a table with a specific type (like incremental), this rule performs the expansion. */
pfc_term_expansion((:- table Stuff as Type), [:- pfcAdd(tabled_as(Stuff, Type)), (:- table Stuff as Type)]) :- 
    nonvar(Stuff), !, 
    if_pfc_indicated, 
    \+ will_table_as(Stuff, Type).

% Handle the expansion of table declarations when 'Stuff' is defined without a specific type.
/* Explanation: If 'Stuff' is a table without specifying a type, it defaults to 'incremental' and the term is expanded accordingly. */
pfc_term_expansion((:- table Stuff), [:- pfcAdd(tabled_as(Stuff, incremental)), (:- table Stuff as incremental)]) :- 
    if_pfc_indicated, 
    \+ will_table_as(Stuff, incremental).

% Skip expansion for any other term that starts with :-.
/* Explanation: This clause prevents expansion for other directives or terms that we don't want to modify. */
pfc_term_expansion((:- _), _) :- !, fail.

% Handle the expansion of rules in the form P ==> Q.
/* Explanation: This expands forward chaining rules of the form 'P ==> Q' into Pfc assertions. */
pfc_term_expansion((P ==> Q), (:- pfcAdd((P ==> Q)))).

% Dead code for an alternative term expansion for speed optimization.
/* previously: A speed-up attempt for term expansion of the form 'P ==> Q'. */
% term_expansion((P ==> Q), (:- pfcAdd(('<-'(Q, P))))).  % speed-up attempt

% Handle the expansion of backward chaining rules in the form '<-'(P, Q).
/* Explanation: This expands backward chaining rules into Pfc assertions. */
pfc_term_expansion(('<-'(P, Q)), (:- pfcAdd(('<-'(P, Q))))).

% Handle the expansion of equivalence rules in the form P <==> Q.
/* Explanation: Expands bi-directional equivalence rules into Pfc assertions. */
pfc_term_expansion((P <==> Q), (:- pfcAdd((P <==> Q)))).

% Handle the expansion of named rules in the form RuleName :::: Rule.
/* Explanation: Expands rules with explicit names into Pfc assertions. */
pfc_term_expansion((RuleName :::: Rule), (:- pfcAdd((RuleName :::: Rule)))).

% Handle the expansion of rules in the form ==>P.
/* Explanation: Expands standalone forward chaining rules into Pfc assertions. */
pfc_term_expansion((==> P), (:- pfcAdd(P))).

% End the term expansion when the end_of_file marker is reached.
/* Explanation: When the end_of_file marker is encountered, the input term is not changed. */
pfc_term_expansion(I, I) :- I == end_of_file, !.

% General case: any other term is expanded by wrapping it with pfcAdd.
/* Explanation: Any remaining terms are wrapped with pfcAdd to add them to the Pfc knowledge base. */
pfc_term_expansion(P, (:- pfcAdd(P))) :- if_pfc_indicated.


% Dead code for controlling the use of Pfc term expansion.
/* previously: Attempted to control when Pfc term expansion should be used, 
   but skipped due to current Prolog flags and source file contexts. */
% use_pfc_term_expansion :- current_prolog_flag(pfc_term_expansion, false), !, fail.
% maybe switch to prolog_load_context(file,...)?
% use_pfc_term_expansion :- source_location(File, _), atom_concat(_, '.pfc.pl', File).


/** term_subst(+Subst, +P, -O)
    Substitute terms within a compound structure based on a substitution list.

    @param Subst The substitution list of pairs to apply.
    @param P The input term to be transformed.
    @param O The output term after substitution.

    @example
    ===
    ?- term_subst([(not)-(~)], not(X), O).
    O = ~(X).
    ===
*/

% Substitute terms based on clause structure.
/* Explanation: The substitution is performed based on the clause structure, matching the Subst list. */
term_subst(P, O) :- term_subst(clause, P, O), !.

% Base case: if the term is not compound, no substitution is needed.
/* Explanation: Non-compound terms are simply returned as-is. */
term_subst(_, P, O) :- \+ compound(P), !, O = P.

% Handle specific substitutions for tilded negation and other operators.
/* Explanation: Special case for negation and logical operators, performing targeted substitutions. */
term_subst(tilded_negation, P, O) :- !, 
    term_subst([(not) - (~), (=>) - (==>), (<=>) - (<==>), (<=) - (<-)], P, O).

% General case: recursively substitute within a compound term.
/* Explanation: Compound terms are recursively broken down, and substitutions are applied to each part. */
term_subst(Subst, P, O) :-
    compound_name_arguments(P, F, Args),
    my_maplist(term_subst(Subst), Args, ArgsL),
    termf_subst(Subst, F, F2),
    compound_name_arguments(O, F2, ArgsL).

/** termf_subst(+Subst, +F, -F2)
    Substitute the functor F with F2 if a substitution exists in the Subst list.

    @param Subst The substitution list of functor pairs.
    @param F The original functor.
    @param F2 The new functor after substitution.

    @example
    ===
    ?- termf_subst([(foo)-(bar)], foo, F2).
    F2 = bar.
    ===
*/

% Substitute the functor F based on the Subst list.
/* Explanation: The functor is replaced with its corresponding value in the substitution list. */
termf_subst(Subst, F, F2) :- member(F - F2, Subst) -> true; F = F2.


% Load necessary list operations from the library.
/* Explanation: The 'lists' library is used for list manipulations in the Pfc code. */
:- use_module(library(lists)).


/* previously: Uncommented core clauses for various Pfc dynamic predicates. These are 
   preserved here as placeholders for future use. */
% ==>(_).
% ==>(G):- arc_assert(G).

% Dynamic predicate declarations for core Pfc predicates.
/* Explanation: Declares several Pfc-related dynamic predicates for forward chaining, truth maintenance, and undo operations. */
:- dynamic '$pt$'/2.
:- dynamic '$nt$'/3.
:- dynamic '$bt$'/2.
:- dynamic fcUndoMethod/2.
:- dynamic fcAction/2.
:- dynamic fcTmsMode/1.
:- dynamic pfcQueue/1.
:- dynamic pfcCurrentDb/1.
:- dynamic pfcHaltSignal/1.
:- dynamic pfcDebugging/0.
:- dynamic pfcSelect/1.
:- dynamic pfcSearch/1.

% Thread-local predicate for Pfc search.
/* Explanation: The thread-local directive ensures that pfcSearchTL is isolated across different threads. */
:- thread_local(t_l:pfcSearchTL/1).

% Dynamic predicate for Pfc support table facts.
/* Explanation: '$spft$' holds facts in the support table for forward-chaining reasoning. */
:- dynamic '$spft$'/3.


/** pfcSetVal(+Stuff)
    Set the value of a Pfc fact, ensuring it is asserted uniquely in the database.

    @param Stuff The fact to be asserted.

    @example
    ===
    ?- pfcSetVal(foo(bar)).
    ===
*/

% Ensure that a fact is uniquely asserted in the database.
/* Explanation: This predicate retracts any existing fact that matches 'Stuff', and then asserts it anew. */
pfcSetVal(Stuff) :-
    duplicate_term(Stuff, DStuff),
    functor(DStuff, _, N),
    setarg(N, DStuff, _),
    retractall(DStuff),
    assert(Stuff).


% %  pfcDefault/1 initialized a global assertion.
% %   pfcDefault(P,Q) - if there is any fact unifying with P, then do
% %   nothing, else assert Q.
/* 
  PLDoc header for pfcDefault/2 predicate
  pfcDefault(+GeneralTerm, +Default)
  Ensures that if a clause for the GeneralTerm is present, it remains unchanged.
  Otherwise, it asserts the Default value for the term.
  @param GeneralTerm - The term to check in the database.
  @param Default - The default term to assert if GeneralTerm is not found.
*/
pfcDefault(GeneralTerm,Default) :-
  /* Checks if the GeneralTerm clause exists and is true. If so, do nothing. */
  clause(GeneralTerm,true) -> true 
  /* Otherwise, assert the Default term into the database. */
  ; assert(Default).

/* 
  File directive for setting default mode for forward chaining system.
  It checks if there is a set mode, if not, defaults to 'cycles' mode.
  PLDoc header for the directive below.
*/
:- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).

/* previously: Set the truth maintenance system (TMS) mode to 'cycles'. */

/* 
  File directive for setting default search strategy for PFC (Prolog Forward Chaining).
  PLDoc header for this directive.
*/
:- pfcDefault(pfcSearch(_), pfcSearch(direct)).

/* previously: Default search strategy for forward chaining is 'direct'. */

/* 
  PLDoc header for pfcAdd/1 predicate
  pfcAdd(+P)
  Adds the fact P to the database with support.
  @param P - The fact to be added.
  @example pfcAdd(fact1).
*/
pfcAdd(P) :- 
  /* Retrieves the current "why" context, used for forward reasoning. */
  must_ex(current_why_UU(UU)),
  /* Adds the fact with its support (the reason for it being added). */
  pfcAdd(P, UU).

/* previously: pfcAdd predicate with unused commented code about 'with_current_why'. */

/* previously: Alternate form of pfcAdd, specifically for cases where P is of the form ==>P */

/* 
  PLDoc header for pfcAdd/2 predicate
  pfcAdd(+P, +S)
  Adds the fact P to the database with support S, triggers forward reasoning, 
  and logs warnings if the add fails.
  @param P - The fact to be added.
  @param S - The support for this fact.
  @example pfcAdd(fact1, some_support).
*/
pfcAdd(P,S) :-
  /* Posts the fact P with support S to the database. */
  pfcPost(P,S),
  /* Runs the forward reasoning process (triggers new inferences). */
  pfcRun, !.

/* previously: Catch-all to handle failed adds, with warnings. */
pfcAdd(P,S) :- 
  /* Issues a warning if adding P with support S fails. */
  pfcWarn("pfcAdd(~p,~p) failed", [P,S]).

/* 
  PLDoc header for pfcPost/2 predicate
  pfcPost(+Ps, +S)
  Attempts to post a set of facts to the database.
  @param Ps - The list of facts to be posted.
  @param S - The support for these facts.
  @example pfcPost([fact1, fact2], some_support).
*/
pfcPost(List,S):- 
  /* Calls pfcPost_rev with reversed order to process the facts. */
  pfcPost_rev(S, List).

/* 
  PLDoc header for pfcPost_rev/2 predicate
  pfcPost_rev(+S, +Term)
  Recursively posts facts from a list.
  @param S - The support for the facts.
  @param Term - A list or singleton fact to be processed.
  @example pfcPost_rev(support, [fact1, fact2]).
*/
pfcPost_rev(S,Term) :-
  /* If Term is a list, map over the list and post each fact recursively. */
  is_list(Term)
  /* Maps over each element in the list. */
  -> my_maplist(pfcPost_rev(S), Term)
  /* Otherwise, post the singleton fact. */
  ; pfcPost1(Term, S).

/* 
  PLDoc header for pfcPost1/2 predicate
  pfcPost1(+P, +S)
  Posts a fact and adds it to the forward chaining queue.
  @param P - The fact to be posted.
  @param S - The support for the fact.
*/
pfcPost1(Fact,S) :- 
  /* Ensures that the argument types are correct and fixed before posting. */
  control_arg_types(Fact,Fixed),
  /* Recursively calls itself with the fixed arguments. */
  !, pfcPost1(Fixed, S).

/* previously: Error handling during fact posting using Prolog's occurs_check flag. */

pfcPost1(P,S):- 
  /* Temporarily sets occurs_check flag to true and catches any errors. */
  locally(set_prolog_flag(occurs_check, true),
    catch(pfcPost11(P,S), E, 
      (notrace, wdmsg(P => E), trace))).

/* previously: Handles assertion and uniqueness checking during fact posting. */

pfcPost11(P,S) :-
  /* Adds support for the fact. */
  must_ex(pfcAddSupport(P,S)),
  /* If the fact is unique, it proceeds with further posting; otherwise, issues a warning. */
  (pfcUnique(post, P)
  /* Posts the fact to the database. */
  -> pfcPost2(P,S) 
  /* Issues a warning if the fact is not unique. */
  ; nop(pfcWarn(not_pfcUnique(post, P)))).


pfcPost2(P,S):-
  must_ex(once(\+ \+ is_asserted_exact(P);assert(P))),
  must_ex(pfcTraceAdd(P,S)),
  !,
  must_ex(pfcEnqueue(P,S)),
  !.

is_asserted_exact(MH,B):-
  strip_module(MH,M,H),
  is_asserted_exact(M,H,B).
is_asserted_exact(MHB):-
  strip_module(MHB,M,HB),
  expand_to_hb(HB,H,B),
  is_asserted_exact(M,H,B).
is_asserted_exact(M,H,B):-
    M=MM,
    (MM:clause(M:H,B,Ref)*->true; M:clause(MM:H,B,Ref)),
    %clause_ref_module(Ref),
    clause_property(Ref,module(MM)),
  %module_checks_out
   is_asserted_exact(MM,H,B,Ref).
is_asserted_exact(_,H,B,Ref):-
    clause(CH,CB,Ref),strip_m(CH,HH),HH=@=H,strip_m(CB,BB),cl(HH,BB)=@=cl(H,B).



%pfcPost1(_,_).
%pfcPost1(P,S) :-
 %pfcWarn("pfcPost1: ~p\n (support: ~p) failed",[P,S]).

% %   pfcAddDbToHead(+P,-NewP) is semidet.
% talkes a fact P or a conditioned fact
% (P:-C) and adds the Db context.
%

pfcAddDbToHead(P,NewP) :-
  pfcCallSystem(pfcCurrentDb(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   true      -> NewP = (P :- Db)).

:- dynamic(pfcCurrentDb/1).
pfcCurrentDb(true).

% %  pfcUnique(X) is det.
%
% is true if there is no assertion X in the prolog db.
%

pfcUnique(Type,(Head:-Tail)) :- !,pfcUnique(Type,Head,Tail).


/**
 *  pfcUnique(+Type, +P) is semidet.
 *
 *  Ensures the uniqueness of an assertion of a given Type (e.g., post, pre).
 *  This is used to prevent duplication of assertions, checking if a fact has already been asserted.
 *  Delegates to a helper predicate that includes a default value for the third argument.
 *
 *  @param Type Type of assertion (e.g., post, pre).
 *  @param P Assertion term to check for uniqueness.
 */
pfcUnique(Type, P) :- pfcUnique(Type, P, true).

/* previously: pfcUnique(post,Head,Tail):- !, \+ is_clause_asserted(Head,Tail).
   This code was used to prevent asserting a clause that already exists but was commented out due to performance improvements in the current implementation.
*/

/**
 *  pfcUnique(+_, +Head, +Tail) is semidet.
 *
 *  Checks if the exact assertion of Head and Tail is already present.
 *  This prevents the assertion of duplicate facts or rules.
 *
 *  @param _ Placeholder for unused Type argument.
 *  @param Head Head of the rule to check.
 *  @param Tail Body of the rule to check.
 */
pfcUnique(_, Head, Tail) :- \+ is_asserted_exact(Head, Tail), !.

/*
   previously: This block attempted to verify if an assertion is already made by examining 
   the internal structure of clauses. It was commented out due to its inefficiency and complexity.
   pfcUnique(_,H,B):- \+ is_asserted(H,B),!.
   pfcUnique(_,H,B):- \+ (
       clause(H, B, Ref),
       clause(HH, BB, Ref),
       strip_m(HH, HHH),
       HHH=@=H,
       strip_m(BB, BBB),
       BBB=@=B).
*/

/**
 *  pfcSetSearch(+Mode) is det.
 *
 *  Sets the current search mode for forward-chaining inferences.
 *
 *  @param Mode The search mode (e.g., direct, depth, breadth, etc.).
 */
pfcSetSearch(Mode) :- pfcSetVal(pfcSearch(Mode)).

/**
 *  pfcGetSearch(-Mode) is det.
 *
 *  Retrieves the current search mode for forward-chaining inferences.
 *
 *  @param Mode Variable to unify with the current search mode.
 */
pfcGetSearch(Mode) :- (t_l:pfcSearchTL(ModeT) -> true ; pfcSearch(ModeT)) -> Mode = ModeT.

/**
 *  pfcEnqueue(+P, +S) is det.
 *
 *  Adds a fact or rule to the processing queue based on the current search mode.
 *
 *  @param P The fact or rule to enqueue.
 *  @param S Optional source or context of the rule.
 */
pfcEnqueue(P, S) :- pfcGetSearch(Mode), !,
    pfcEnqueue(Mode, P, S).

pfcEnqueue(P, S) :- pfcWarn("No pfcSearch mode"),
    pfcEnqueue(direct, P, S).

/**
 *  pfcEnqueue(+Mode, +P, +S) is det.
 *
 *  Enqueues a fact or rule depending on the search mode specified.
 *
 *  @param Mode The search mode (direct, thread, depth, breadth).
 *  @param P Fact or rule to be enqueued.
 *  @param S Optional source/context.
 */
pfcEnqueue(Mode, P, S) :-
    Mode = direct  -> pfcFwd(P) ;
    Mode = thread  -> pfcThreadFwd(P, S) ;
    Mode = depth   -> pfcAsserta(pfcQueue(P), S) ;
    Mode = breadth -> pfcAssert(pfcQueue(P), S) ;
    true           -> pfcWarn("Unrecognized pfcSearch mode: ~p", Mode), pfcEnqueue(direct, P, S).

/**
 *  pfcRemoveOldVersion(+Rule) is det.
 *
 *  Removes any previous version of the rule if it exists.
 *  This helps to prevent conflicts or redundancy in the database.
 *
 *  @param Rule The rule that may have older versions to remove.
 */
pfcRemoveOldVersion((Identifier::::Body)) :-
  % Warn if Identifier is a variable, as this is unexpected.
  (var(Identifier)
  ->
  pfcWarn("Variable used as a rule name in ~p :::: ~p",
          [Identifier, Body]);
  pfcRemoveOldVersion0(Identifier::::Body)).

/**
 *  pfcRemoveOldVersion0(+Rule) is det.
 *
 *  Helper predicate that removes the old version of a rule.
 *
 *  @param Rule The rule for which older versions may be removed.
 */
pfcRemoveOldVersion0((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody), _),
  \+(Body = OldBody),
  pfcWithdraw((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion0(_).

/**
 *  with_fc_mode(+Mode, :Goal) is semidet.
 *
 *  Temporarily changes the forward-chaining propagation mode while running the Goal.
 *
 *  @param Mode The forward-chaining mode to use (e.g., direct, thread).
 *  @param Goal The goal to execute under the specified mode.
 */
with_fc_mode(Mode, Goal) :- locally(t_l:pfcSearchTL(Mode), Goal).

/**
 *  pfcThreadFwd(+S, +P) is det.
 *
 *  Forwards a fact/rule P in thread mode, encapsulating the reasoning within the specified source/context S.
 *
 *  @param S Source/context for the forward propagation.
 *  @param P Fact/rule to be forwarded.
 */
pfcThreadFwd(S, P) :-
    with_only_current_why(S,
        % Optionally, keep 'thread' mode active
        call_in_thread(with_fc_mode(thread, (pfcFwd(P))))).

/*
   previously: There were multiple implementations for in_fc_call/1,
   commented out as the code evolved to streamline the selection of the forward-chaining mode.
   These older versions were removed for simplicity.
   in_fc_call(Goal):- with_fc_mode(thread, Goal).
   in_fc_call(Goal):- with_fc_mode(direct, Goal).
*/

/**
 *  pfcRun is det.
 *
 *  Computes the deductive closure of the current database.
 *  The process depends on the search mode, either direct, depth, or breadth.
 */
pfcRun :-
  (\+ pfcGetSearch(direct)),
  pfcStep,
  pfcRun.
pfcRun.

/**
 *  pfcStep is det.
 *
 *  Processes one fact/rule from the forward-chaining queue.
 *  Stops if a halt signal is encountered or if the queue is empty.
 */
pfcStep :-
  % If a halt signal is present, remove it and stop processing.
  pfcRetract(pfcHaltSignal(Msg)),
  pfcTraceMsg(removing(pfcHaltSignal(Msg))),
  !,
  fail.

pfcStep :-
  % Draw conclusions from the next fact in the queue.
  get_next_fact(P),
  pfcdo(pfcFwd(P)),
  !.

/**
 *  get_next_fact(-P) is det.
 *
 *  Retrieves the next fact from the queue for forward-chaining reasoning.
 *
 *  @param P The fact to process.
 */
get_next_fact(P) :-
  select_next_fact(P),
  remove_selection(P).

/**
 *  remove_selection(+P) is det.
 *
 *  Removes a selected fact from the queue after it has been processed.
 *
 *  @param P The fact to remove from the queue.
 */
remove_selection(P) :-
  pfcRetract(pfcQueue(P)),

pfcRemoveSupportsQuietly(pfcQueue(P)),
% Ensures no further backtracking once the fact is processed.
!.

/* remove_selection/1 handles cases where a fact is selected but is not on the PFC queue. */
% PLDoc header
% @param P - The fact that is being checked for removal.
% If the fact P is not found in the PFC queue, it prints a warning message.
remove_selection(P) :-
  brake(pfcPrintf("pfc:get_next_fact - selected fact not on Queue: ~p",
               [P])).

/* select_next_fact/1 is used to identify the next fact to process in the forward-chaining mechanism. */
% PLDoc header
% @param P - The fact to be selected.
% First attempts to use a user-defined predicate to select the next fact.
select_next_fact(P) :-
  pfcSelect(P),
  % If a user-defined selection succeeds, it stops here.
  !.
% If user-defined selection fails, it falls back to the default fact selection mechanism.
select_next_fact(P) :-
  defaultpfcSelect(P),
  % Ensures no backtracking after selection.
  !.

/* defaultpfcSelect/1 is the fallback mechanism to select a fact from the PFC queue. */
% PLDoc header
% @param P - The fact that is selected from the front of the queue.
% Selects a fact from the PFC queue using a system-level call.
defaultpfcSelect(P) :- 
  pfcCallSystem(pfcQueue(P)),
  % Ensures no further backtracking once the selection is done.
  !.

/* pfcHalt/0 stops the forward chaining mechanism and halts the process. */
% PLDoc header
% Halts the forward chaining mechanism due to an unspecified reason.
pfcHalt :-  
  pfcHalt("unknown_reason", []).

/* pfcHalt/1 halts the system with a specific message format. */
% PLDoc header
% @param Format - The format of the halt message.
pfcHalt(Format) :-  
  pfcHalt(Format, []).

/* pfcHalt/2 stops the process with a formatted message and arguments. */
% PLDoc header
% @param Format - The format of the message.
% @param Args - Additional arguments for the formatted message.
pfcHalt(Format, Args) :-
  % Create a formatted halt message.
  format(string(Msg), Format, Args),
  % If a halt signal is already set, log a warning.
  (pfcHaltSignal(Msg) ->
       pfcWarn("pfcHalt finds pfcHaltSignal(~w) already set", [Msg])
     % Otherwise, assert a new halt signal.
     ; assert(pfcHaltSignal(Msg))).

% %
% %  Predicates for manipulating triggers.

% PLDoc header for pfcAddTrigger/2
% @param Trigger - The trigger structure.
% @param Support - The support structure.
% Add a positive trigger ('$pt$') with its body and support.
pfcAddTrigger('$pt$'(Trigger, Body), Support) :-
  % Add a positive trigger and trace the message.
  !,
  pfcTraceMsg('      Adding positive trigger(+) ~p~n', ['$pt$'(Trigger, Body)]),
  % Assert the positive trigger with support.
  pfcAssert('$pt$'(Trigger, Body), Support),
  % Create a copy of the term for further evaluation.
  copy_term('$pt$'(Trigger, Body), Tcopy),
  % Evaluate the trigger.
  pfc_call(Trigger),
  % Evaluate the body of the trigger with the current why explanation.
  with_current_why(Trigger, fcEvalLHS(Body, (Trigger, Tcopy))),
  % Fail to force backtracking to evaluate all triggers.
  fail.

/* Adding negative triggers with body and support */
% PLDoc header for pfcAddTrigger/3 for negative triggers.
% @param Trigger - The trigger structure.
% @param Test - The test condition for the negative trigger.
% @param Body - The body of the negative trigger.
% @param Support - The support structure.
pfcAddTrigger('$nt$'(Trigger, Test, Body), Support) :-
  !,
  % Trace the message for adding a negative trigger.
  pfcTraceMsg('      Adding negative trigger(-): ~p~n       test: ~p~n       body: ~p~n', [Trigger, Test, Body]),
  % Create a copy of the trigger for further evaluation.
  copy_term(Trigger, TriggerCopy),
  % Assert the negative trigger with support.
  pfcAssert('$nt$'(TriggerCopy, Test, Body), Support),
  % If the test condition fails, evaluate the body.
  \+ pfc_call(Test),
  % Evaluate the body with the current why explanation.
  with_current_why(\+ pfc_call(Test), fcEvalLHS(Body, ((\+Trigger), '$nt$'(TriggerCopy, Test, Body)))).

/* Adding backward triggers */
% PLDoc header for pfcAddTrigger/3 for backward triggers.
% @param Trigger - The trigger structure.
% @param Body - The body of the backward trigger.
% @param Support - The support structure.
pfcAddTrigger('$bt$'(Trigger, Body), Support) :-
  !,
  % Assert the backward trigger with support.
  pfcAssert('$bt$'(Trigger, Body), Support),
  % Combine backward and positive triggers for evaluation.
  pfcBtPtCombine(Trigger, Body, Support).

/* Handling unrecognized triggers */
% PLDoc header for pfcAddTrigger/2 when the trigger type is not recognized.
% @param X - The unrecognized trigger.
% @param _Support - The support structure (ignored).
pfcAddTrigger(X, _Support) :-
  % Log a warning for unrecognized trigger.
  pfcWarn("Unrecognized trigger(?) to pfcAddTrigger: ~p", [X]).

/* pfcBtPtCombine/3 combines backward and positive triggers for evaluation. */
% PLDoc header
% @param Head - The head of the trigger.
% @param Body - The body of the trigger.
% @param Support - The support structure.
pfcBtPtCombine(Head, Body, Support) :-
  % Find any positive triggers ('$pt$') with unifying heads and add the instantiated '$bt$' body.
  pfcGetTriggerQuick('$pt$'(Head, _PtBody)),
  % Evaluate the left-hand side of the body.
  fcEvalLHS(Body, Support),
  % Fail to ensure backtracking.
  fail.
pfcBtPtCombine(_, _, _) :- 
  % Succeed if no further backtracking is required.
  !.

/* pfcGetTriggerQuick/1 fetches triggers quickly using clauses or pfc_call. */
% PLDoc header
% @param Trigger - The trigger to fetch.
pfcGetTriggerQuick(Trigger) :-  
  % Attempt to fetch the clause directly, or fall back to pfc_call.
  clause(Trigger, true) *-> true ; pfc_call(Trigger).

/* pfcCallSystem/1 invokes system-level PFC calls. */
% PLDoc header
% @param Trigger - The trigger to call.
pfcCallSystem(Trigger) :-  
  pfc_call(Trigger).

% %
% %  Predicates for manipulating action traces.

% PLDoc header for pfcAddActionTrace/2
% @param Action - The action being traced.
% @param Support - The support structure for the action.
% Add an action trace along with its support.
pfcAddActionTrace(Action, Support) :-
  % Adds the action trace with support.
  pfcAddSupport(pfcAction(Action), Support).

% PLDoc header for pfcRemActionTrace/1
% @param pfcAction(A) - The action to remove from the action trace.
pfcRemActionTrace(pfcAction(A)) :-
  % Find the undo method for the action and call it to remove the action trace.
  fcUndoMethod(A, UndoMethod),
  pfcCallSystem(UndoMethod),
  % Succeed after removing the trace.
  !.

% %
% %  Predicates to remove PFC facts, triggers, action traces, and queue items from the database.

% PLDoc header for pfcRetract/1
% @param X - The fact, trigger, or action trace to retract.
% Retract an arbitrary item (fact, trigger, etc.) from the database.
pfcRetract(X) :-
  % Determine the type of the item (fact, trigger, action, etc.).
  pfcType(X, Type),
  % Retract the item based on its type.
  pfcRetractType(Type, X),
  % Succeed after retracting.
  !.

/* pfcRetractType/2 retracts facts specifically. */
% PLDoc header for pfcRetractType/2 for facts.
% @param Type - The type of item (fact, rule, etc.).
% @param X - The fact to be retracted.
pfcRetractType(fact(_), X) :-
  % Add the database head for the fact and retract it.
  pfcAddDbToHead(X, X2) -> retract(X2) ; retract(X).

pfcRetractType(rule(_),X) :-
  % %  db
  pfcAddDbToHead(X,X2) ->  retract(X2) ; retract(X).


% pfcRetractType/2
% @desc This predicate handles retracting different types of database entries, specifically triggers and actions.
% It first tries to retract X and if successful, calls unFc/1 on it. If X can't be found, it issues a warning.
% @param trigger(Pos) - A trigger identified by Pos.
% @param action - An action to be removed.
% @example pfcRetractType(trigger(pos1), fact_to_retract).
pfcRetractType(trigger(Pos),X) :-
    % Try to retract X. If successful, call unFc/1.
    retract(X)
    -> unFc(X)
    % If the retraction fails, issue a warning.
    ; pfcWarn("Trigger(~p) not found to retract: ~p",[Pos,X]).

% pfcRetractType/2 (special case for action)
% @desc Removes an action trace from the database.
% @param action - The action to be removed.
pfcRetractType(action,X) :-
    % Remove the action trace associated with X.
    pfcRemActionTrace(X).

/* previously: General rule retraction was more flexible, but now this specializes for actions and triggers. */

% pfcAddType1/1
% @desc Adds an item X to the database after determining its type.
% It first checks the type of X, then adds it to the database accordingly.
% @param X - The item to add to the database (could be a fact, rule, trigger, or action).
% @example pfcAddType1(fact(some_fact)).
pfcAddType1(X) :-
    % Determine the type of X.
    pfcType(X,Type),
    % Add the entry to the database (or retract and re-add if necessary).
    pfcAddDbToHead(X,X2),
    % Call the appropriate predicate based on the type.
    pfcAddType(Type,X2).

% pfcAddType/2
% @desc Adds facts, rules, or triggers based on their type to ensure uniqueness.
% @param fact(Type) - A fact of some specific type.
% @param rule(Type) - A rule of some specific type.
% @param trigger(Pos) - A trigger at a specific position.
pfcAddType(fact(Type),X) :-
    % Ensure the fact is unique before asserting it into the database.
    pfcUnique(fact(Type),X),
    % Add the fact to the database.
    assert(X),!.

pfcAddType(rule(Type),X) :-
    % Ensure the rule is unique before asserting it.
    pfcUnique(rule(Type),X),
    % Add the rule to the database.
    assert(X),!.

pfcAddType(trigger(Pos),X) :-
    % If the trigger is unique, add it to the database.
    pfcUnique(trigger(Pos),X)
    -> assert(X)
    % Otherwise, warn that the trigger is not unique but still add it.
    ; (pfcWarn(not_pfcUnique(X)),assert(X)).

% pfcAddType/2 (special case for actions)
% @desc Actions are added without uniqueness checks.
% @param action - The action to be added.
pfcAddType(action,_Action) :-
    % Simply succeed; no further action needed for action types.
    !.

/* previously: Actions were checked for uniqueness, but it was determined unnecessary for performance reasons. */

% pfcWithdraw/1
% @desc Withdraws support for a fact or a list of facts. If a list is provided, it recursively withdraws support for each element.
% @param P - The fact or list of facts to withdraw support for.
% @example pfcWithdraw([fact1, fact2]).
pfcWithdraw(P) :-
    % If P is a list, recursively withdraw support for each element.
    is_list(P),!,
    my_maplist(pfcWithdraw,P).

pfcWithdraw(P) :-
    % Try to match the support mechanism.
    matches_why_UU(UU),
    % Withdraw support for P, using UU as context.
    pfcWithdraw(P,UU).

% pfcWithdraw/2
% @desc Removes support for a fact P, checks if it's still supported, and retracts it if not.
% @param P - The fact to withdraw support for.
% @param S - The specific support term being removed.
pfcWithdraw(P,S) :-
    % Get the support relationship between P and S.
    pfcGetSupport(P,S),
    % Ensure the support term is materialized.
    matterialize_support_term(S,Sup),
    % Trace message for debugging support withdrawal.
    pfcTraceMsg('    Withdrawing direct support: ~p   \n   From: ~p~n',[Sup,P]),
    % Attempt to remove one unit of support.
    (pfcRemOneSupportOrQuietlyFail(P,S)
        % If successful, trace the success.
        -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
        % If not, issue a warning.
        ; pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not pfcRemOneSupport: ~p",[Sup,P])),
    % Check if P is now unsupported and remove it if so.
    removeIfUnsupported(P).

pfcWithdraw(P,S) :-
    % If no matching support was found, trace the failure.
    matterialize_support_term(S,Sup),
    pfcTraceMsg('    No support matching: ~p   \n   For: ~p~n',[Sup,P]),!,
    % Remove P if unsupported.
    removeIfUnsupported(P).

% pfcRetractAll/1
% @desc Withdraws both direct and indirect support for a fact (or list of facts).
% @param P - The fact or list of facts to fully retract support for.
% @example pfcRetractAll([fact1, fact2]).
pfcRetractAll(P) :-
    % If P is a list, iterate over all elements and retract support for each.
    is_list(P),!,
    my_maplist(pfcRetractAll,P).

pfcRetractAll(P) :-
    % Try to match the support mechanism.
    matches_why_UU(UU),
    % Fully retract all support for P using UU as context.
    pfcRetractAll(P,UU).

% pfcRetractAll/2
% @desc Removes all support for a fact and checks if it can still be supported.
% If not, it retracts the fact from the database.
% @param Fact - The fact for which to retract support.
% @param S - The specific support term being removed.
pfcRetractAll(Fact,S) :-
    % Control the argument types and ensure they are fixed for the retraction.
    control_arg_types(Fact,Fixed),!,
    % Perform the full retraction with fixed arguments.
    pfcRetractAll(Fixed,S).

pfcRetractAll(P,S) :-
    % Withdraw support for P. If successful, fail to stop further retractions.
    \+ \+ pfcWithdraw(P,S),
    fail.

pfcRetractAll(P,S) :-
  pfcGetSupport(P,(P2,_)),
  pfcType(P2,fact(_)),
  pfcSupportedBy(P2,S,_How),
   pfcRetractAll(P2),
    \+ fcSupported(P),!,
    fcUndo(P).
pfcRetractAll(P,S) :-
  pfcGetSupport( P,(_,T)),
    pfcGetSupport(T,(P2,_)),
    pfcSupportedBy(P2,S,_How),
    pfcType(P2,fact(_)),
   pfcRetractAll(P2),
    \+ fcSupported(P),!,
    fcUndo(P).
pfcRetractAll(P,S) :-
  fcSupported(P),
  pfcGetSupport(P,(P2,_)),
  pfcSupportedBy(P2,S,_How),
  pfcType(P2,rule(_)),
   pfcRetractAll(P2),
    \+ fcSupported(P),
    fcUndo(P),!.

pfcRetractAll(P,_S0) :-
  /* removeIfUnsupported(P): If fact P is unsupported, remove it. */
  removeIfUnsupported(P),
  /* The fail predicate forces backtracking to ensure all instances are removed. */
  fail.

/**
 * This clause succeeds once all instances of P have been removed.
 */
pfcRetractAll(_,_).

/**
 * pfcSupportedBy(+P, +S, -How) 
 * @param P The fact being checked.
 * @param S The support fact.
 * @param How The fact that provides the support.
 * This predicate determines how fact P is supported by another fact S, considering both forwards and backwards links.
 */
pfcSupportedBy(P,S,How) :-
   /* Get the support for P in terms of facts F and T. */
   pfcGetSupport(P,(F,T)),
   /* Check if fact F supports P. If not, check if T supports P. */
   (pfcSupportedBy(F,S,_) -> How = F;
    pfcSupportedBy(T,S,How)).

/**
 * pfcSupportedBy(+P, +S, -How) 
 * If P is equal to S, the support is directly S.
 */
pfcSupportedBy(P,S,How) :- P = S, How = S.

/**
 * pfcRetractAll_v2(+P, +S0) 
 * @param P The fact to retract.
 * @param S0 The original support state.
 * A more complex version of pfcRetractAll, this version also logs support removals and handles 
 * dependencies between facts more thoroughly.
 */
pfcRetractAll_v2(P,S0) :-
  /* Double negation ensures P is handled without committing to side effects too early. */
  \+ \+ pfcWithdraw(P,S0),
  /* Get the support and potential trigger for removal. */
  pfcGetSupport(P,(S,RemoveIfTrigger)),
  /* previously: pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])) */
  /* Generate a materialized support term for logging purposes. */
  matterialize_support_term((S,RemoveIfTrigger),Sup),
  /* Log the removal of support with a message. */
  pfcTraceMsg('    Removing support: ~p   \n   From: ~p~n',[Sup,P]),
  /* Attempt to remove one instance of support, logging success or failure. */
  (pfcRemOneSupportOrQuietlyFail(P,(S,RemoveIfTrigger))
     -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
     ; (pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not yet pfcRetractAll_v2: ~p",
                [Sup,P]))),
  /* Recursively retract all support for S. */
  pfcRetractAll_v2(S, S0),
  /* Force failure to continue backtracking and processing. */
  fail.

/**
 * When no more instances of P exist, remove it if unsupported.
 */
pfcRetractAll_v2(P,_) :- removeIfUnsupported(P).

/**
 * pfcRemove(+Fact) 
 * @param Fact The fact to remove.
 * User-facing predicate for retracting support for a fact P. It behaves like pfcRetractAll but may apply additional 
 * "forceful" measures like pfcBlast to ensure retraction.
 */
pfcRemove(Fact) :-
  /* Validate and fix the control argument types. */
  control_arg_types(Fact,Fixed), !, pfcRemove(Fixed).

pfcRemove(P) :-
  /* Retract all instances of P. */
  pfcRetractAll(P),
  /* If P still exists, apply pfcBlast to force retraction. */
  pfc_call(P) -> pfcBlast(P) ; true.

/**
 * pfcBlast(+F) 
 * @param F The fact to retract forcefully.
 * Retract fact F and remove any dependent facts, ensuring complete removal from the database.
 */
pfcBlast(F) :-
  /* Remove any remaining supports for F. */
  pfcRemoveSupports(F),
  /* Undo F by performing any necessary retraction or action cleanup. */
  fcUndo(F).

/**
 * pfcRemoveSupports(+F) 
 * @param F The fact to remove supports for.
 * This predicate removes all remaining supports for a fact F, issuing warnings as necessary.
 */
pfcRemoveSupports(F) :-
  /* Attempt to remove one support at a time, logging warnings if supports still exist. */
  pfcRemOneSupport(F,S),
  pfcWarn("~p was still supported by ~p (but no longer)",[F,S]),
  /* Fail to trigger backtracking and handle all supports. */
  fail.

pfcRemoveSupports(_).

/**
 * pfcRemoveSupportsQuietly(+F) 
 * @param F The fact to remove supports for.
 * Similar to pfcRemoveSupports but operates quietly, without issuing warnings.
 */
pfcRemoveSupportsQuietly(F) :-
  pfcRemOneSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

/**
 * fcUndo(+X) 
 * @param X The action or fact to undo.
 * This predicate undoes the fact X, either by reversing an action or retracting a fact from the database.
 */
fcUndo(pfcAction(A)) :-
  /* previously: Find and execute a method to undo the action A. */
  /* Remove action trace and undo. */
  !, pfcRemActionTrace(pfcAction(A)).

fcUndo('$pt$'(/*Key,*/Head,Body)) :-
  /* Undo a positive trigger by retracting it from the database. */
  !,
  (retract('$pt$'(/*Key,*/Head,Body))
    -> unFc('$pt$'(Head,Body))
     ; pfcWarn("Trigger not found to retract: ~p",['$pt$'(Head,Body)])).

fcUndo('$nt$'(Head,Condition,Body)) :-
  /* Undo a negative trigger by retracting it from the database. */
  !,
  (retract('$nt$'(Head,Condition,Body))
    -> unFc('$nt$'(Head,Condition,Body))
     ; pfcWarn("Trigger not found to retract: ~p",['$nt$'(Head,Condition,Body)])).

fcUndo(Fact) :-
  /* Undo a general fact by retracting it and logging the removal. */
  retract(Fact),
  pfcTraceRem(Fact),
  unFc(Fact).

/**
 * unFc(+P) 
 * @param P The fact to "un-forward-chain" from.
 * This predicate undoes the forward chaining effects of a fact P, removing dependent relations and 
 * cleaning up associated triggers.
 */
unFc(F) :-
  /* Retract all dependent relations of F. */
  pfcRetractDependantRelations(F),
  /* Perform additional cleanup checks. */
  unFc1(F).

/**
 * unFc1(+P) 
 * @param P The fact to check for triggers.
 * This helper predicate checks triggers and ensures consistency after a fact has been removed.
 */
unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  
% % File Directive: This directive runs the PFC engine.
% It initiates the process of forward chaining rules.
pfcRun.

% % Predicate: pfcUnFcCheckTriggers/1
% This predicate checks triggers for a fact and attempts to 
% evaluate the associated action if the condition fails.
%
% @param F The fact to check triggers for.
% @example pfcUnFcCheckTriggers(myFact).
pfcUnFcCheckTriggers(F) :-
  % Check if F is of the type 'fact'.
  pfcType(F,fact(_)),
  
  % Make a copy of the fact for safe evaluation.
  copy_term(F,Fcopy),
  
  % Find a trigger associated with the fact.
  pfcCallSystem('$nt$'(Fcopy, Condition, Action)),
  
  % If the condition fails, evaluate the action.
  (\+ pfcCallSystem(Condition)),
  
  % Evaluate the action and provide a context for negation.
  fcEvalLHS(Action, ((\+ F), '$nt$'(F, Condition, Action))),
  
  % Force backtracking to continue checking other triggers.
  fail.

% Base case for pfcUnFcCheckTriggers/1 when no more triggers are left.
pfcUnFcCheckTriggers(_).

% % Predicate: pfcRetractDependantRelations/1
% This predicate removes any dependent relations for a given fact.
%
% @param Fact The fact whose dependent relations are retracted.
% @example pfcRetractDependantRelations(myFact).
pfcRetractDependantRelations(Fact) :-
  % Check the type of the fact.
  pfcType(Fact, Type),
  
  % If the fact is a trigger, remove its support.
  (Type = trigger(_Pos) -> pfcRemOneSupport(P, (_, Fact))
  
  % Otherwise, remove support for a regular fact.
  ; pfcRemOneSupportOrQuietlyFail(P, (Fact, _))),
  
  % Remove the fact if it is no longer supported.
  removeIfUnsupported(P),
  
  % Force backtracking to continue retracting other relations.
  fail.

% Base case for pfcRetractDependantRelations/1 when no more relations are left.
pfcRetractDependantRelations(_).

% % Predicate: removeIfUnsupported/1
% This predicate checks if a given fact is unsupported and removes 
% it if it is not.
%
% @param P The fact to check for support.
% @example removeIfUnsupported(myFact).
removeIfUnsupported(P) :-
   % If P is supported, print a trace message; otherwise, undo it.
   fcSupported(P) -> pfcTraceMsg(fcSupported(P)) ; fcUndo(P).

% % Predicate: fcSupported/1
% This predicate succeeds if the given fact is supported based on the 
% current TMS (Truth Maintenance System) mode.
%
% @param P The fact to check for support.
% @example fcSupported(myFact).
fcSupported(P) :-
  % Ensure the TMS mode is available.
  must_ex(fcTmsMode(Mode)),
  
  % Check if the fact is supported based on the mode.
  supported(Mode, P).

% The supported/2 predicate defines how support is determined
% based on the TMS mode.
supported(local, P) :- !, pfcGetSupport(P, _).
supported(cycles, P) :- !, wellFounded(P).
supported(_, _P) :- true.

% % Predicate: wellFounded/1
% A fact is well-founded if it is supported by the user or by a set of facts 
% and rules, all of which are well-founded.
%
% @param Fact The fact to check for well-foundedness.
% @example wellFounded(myFact).
wellFounded(Fact) :-
  % Start the check with an empty list of descendants.
  wf(Fact, []).

% % Predicate: wf/2
% The main recursive predicate to check if a fact is well-founded.
%
% @param F The fact to check.
% @param Descendants The list of facts that have already been checked.
wf(F, _) :-
  % If the fact is an axiom or an assumption, it is well-founded.
  (axiom(F); assumption(F)),
  !.

wf(F, Descendants) :-
  % Ensure there is no cyclic dependency.
  (\+ memberchk(F, Descendants)),
  
  % Find the facts that support this fact.
  supports(F, Supporters),
  
  % Ensure all the supporters are well-founded.
  wflist(Supporters, [F | Descendants]),
  !.

% % Predicate: wflist/2
% Applies wf/2 to each fact in a list to ensure all are well-founded.
%
% @param L The list of facts to check.
% @param Descendants The current list of descendants.
wflist([], _).
wflist([X | Rest], L) :-
  wf(X, L),
  wflist(Rest, L).

% % Predicate: supports/2
% Finds a justification for a fact by returning the list of supporters.
% Typically, one of the supporters is a rule.
%
% @param F The fact to find supporters for.
% @param ListOfSupporters The list of supporters.
% @example supports(myFact, Supporters).
supports(F, [Fact | MoreFacts]) :-
  % Get the support for the fact and trigger.
  pfcGetSupport(F, (Fact, Trigger)),
  
  % Find the supporters of the trigger.
  triggerSupports(Trigger, MoreFacts).

% % Predicate: triggerSupports/2
% Finds the supporters for a trigger. 
% If the trigger is axiomatic, there are no further supporters.
%
% @param Trigger The trigger to check.
% @param AllSupport The list of supporting facts.
triggerSupports(U, []) :- 
  axiomatic_supporter(U), !.

% triggerSupports/2 attempts to find the supporters of a trigger,
% first using triggerSupports1/2 and falling back to triggerSupports2/2.
triggerSupports(Trigger, AllSupport) :-
  % Attempt to find the supporters with triggerSupports1/2.
  triggerSupports1(Trigger, AllSupport) *-> true ; triggerSupports2(Trigger, AllSupport).

% % Predicate: triggerSupports1/2
% Finds the first level of support for a trigger.
%
% @param Trigger The trigger to check.
% @param AllSupport The list of supporting facts.
triggerSupports1(Trigger, AllSupport) :-
  % Get the support for the trigger.
  pfcGetSupport(Trigger, (Fact, AnotherTrigger)),
  
  % Recursively find supporters for the other triggers.
  (triggerSupports(AnotherTrigger, MoreFacts) *-> true ; MoreFacts = [AnotherTrigger]),
  
  % Combine the facts into a support list.
  [Fact | MoreFacts] = AllSupport.

% % Predicate: triggerSupports2/2
% This is the second method to find support for a trigger, but it is currently disabled.
% The reason for skipping this code is unclear, but the predicate fails immediately.
% This is likely an optimization or a fallback that is no longer needed.
% It remains here for historical purposes.
triggerSupports2(Trigger, AllSupport) :- fail,
  pfcGetSupport(Trigger, (Fact, AnotherTrigger)),
  (triggerSupports(AnotherTrigger, MoreFacts) *-> true ; MoreFacts = [AnotherTrigger]),
  [Fact | MoreFacts] = AllSupport.

% % Predicate: axiomatic_supporter/1
% Defines the types of axiomatic supporters.
%
% @param Var The potential axiomatic supporter.
axiomatic_supporter(Var) :- is_ftVar(Var), !, fail.
axiomatic_supporter(is_ftVar(_)).
axiomatic_supporter(clause_u(_)).
axiomatic_supporter(user(_)).
axiomatic_supporter(U) :- is_file_ref(U), !.
axiomatic_supporter(ax) :- !.

% % Predicate: is_file_ref/1
% Checks if a term is a file reference.
%
% @param A The term to check.
is_file_ref(A) :- compound(A), A = mfl4(_VarNameZ, _, _, _).

% A special case of triggerSupports where the variable is a free term variable (is_ftVar).
triggerSupports(_, Var, [is_ftVar(Var)]) :- is_ftVar(Var), !.
/**
 * @predicate triggerSupports/3
 * Triggers the support of a fact if an axiomatic supporter exists or recursively finds
 * support through other triggers.
 * @param FactIn - The initial fact.
 * @param Trigger - The trigger to check for support.
 * @param OUT - The list of supported facts.
 * @example
 * ?- triggerSupports(someFact, someTrigger, Result).
 * Result = [list_of_facts].
 */
triggerSupports(_, U, []) :- 
    % If U is an axiomatic supporter, terminate the search immediately.
    axiomatic_supporter(U), !.
triggerSupports(FactIn, Trigger, OUT) :-
    % Try to find the support for the Trigger using pfcGetSupport.
    % If a support is found, recursively search for more support.
    pfcGetSupport(Trigger, (Fact, AnotherTrigger)) *->
    ( 
        % Recursively find more support facts.
        triggerSupports(Fact, AnotherTrigger, MoreFacts),
        % Collect the facts into the output list.
        OUT = [Fact | MoreFacts]
    );
    % If no support found with pfcGetSupport, proceed with another method.
    triggerSupports1(FactIn, Trigger, OUT).

/**
 * @predicate triggerSupports1/3
 * Triggers the support for a fact, using the `may_cheat` flag if available.
 * @param FactIn - The initial fact (unused here).
 * @param X - The trigger.
 * @param OUT - The output list containing the trigger.
 */
triggerSupports1(_, X, [X]) :- 
    % If may_cheat flag is true, cheat and return the trigger as the support.
    may_cheat.

/**
 * @predicate may_cheat/0
 * A helper predicate used to allow "cheating" in the support checking.
 */
may_cheat :- true_flag.


% File directive:
% Set occurs_check flag to true when running forward chaining

/**
 * @predicate pfcFwd/1
 * Forward chains from a fact or a list of facts.
 * @param Fact - The fact or list of facts to forward chain from.
 */
pfcFwd(Fact) :- 
    % Fix the control argument types for Fact.
    control_arg_types(Fact, Fixed), 
    !,
    % Forward chain with the fixed argument.
    pfcFwd(Fixed).
pfcFwd(Fact) :-
    % Temporarily set the Prolog flag `occurs_check` to true during forward chaining.
    locally(set_prolog_flag(occurs_check, true), 
    % Forward chain with the original fact.
    pfcFwd0(Fact)).

/**
 * @predicate pfcFwd0/1
 * A helper predicate for pfcFwd/1.
 * If the input is a list, recursively forward chain for each item; otherwise, forward chain for a single fact.
 * @param Fact - A fact or list of facts.
 */
pfcFwd0(Fact) :- 
    % If the input is a list, apply forward chaining to each fact in the list.
    is_list(List) -> 
    my_maplist(pfcFwd0, List); 
    % If not a list, forward chain a single fact.
    pfcFwd1(Fact).

/**
 * @predicate pfcFwd1/1
 * Forward chains for a single fact, checking positive and negative triggers.
 * @param Fact - The fact to forward chain from.
 */
pfcFwd1(Fact) :-
    % Check if the fact triggers a forward chain rule.
    (fc_rule_check(Fact) *-> true; true),
    % Create a copy of the fact for further processing.
    copy_term(Fact, F),
    % Check positive triggers for forward chaining.
    ignore(fcpt(Fact, F)),
    % Check negative triggers for forward chaining.
    ignore(fcnt(Fact, F)).


/**
 * @predicate fc_rule_check/1
 * Special built-in forward chaining for rules.
 * Processes rule patterns such as P==>Q or P<==>Q.
 * @param P - The rule to check.
 */
fc_rule_check((Name::::P==>Q)) :-
    !,
    % Process the rule for forward chaining.
    processRule(P, Q, (Name::::P==>Q)).
fc_rule_check((Name::::P<==>Q)) :-
    !,
    % Process both directions of the bi-directional rule.
    processRule(P, Q, ((Name::::P<==>Q))),
    processRule(Q, P, ((Name::::P<==>Q))).
fc_rule_check((P==>Q)) :-
    !,
    % Process a standard forward chaining rule.
    processRule(P, Q, (P==>Q)).
fc_rule_check((P<==>Q)) :-
    !,
    % Process a bi-directional rule.
    processRule(P, Q, (P<==>Q)),
    processRule(Q, P, (P<==>Q)).
fc_rule_check(('<-'(P, Q))) :-
    !,
    % Define a backward chaining rule.
    pfcDefineBcRule(P, Q, ('<-'(P, Q))).
fc_rule_check(_).


/**
 * @predicate fcpt/2
 * Checks for positive triggers during forward chaining.
 * @param Fact - The fact to be checked.
 * @param F - A copy of the fact.
 */
fcpt(Fact, F) :-
    % Get a quick trigger for the fact.
    pfcGetTriggerQuick('$pt$'(F, Body)),
    % Trace the positive trigger.
    pfcTraceMsg('\n Found positive trigger(+):\n    ~p~n       body: ~p~n', [F, Body]),
    % Get the support for the positive trigger.
    pfcGetSupport('$pt$'(F, Body), Support),
    % Evaluate the left-hand side of the body of the trigger.
    with_current_why(Support, with_current_why(Fact, fcEvalLHS(Body, (Fact, '$pt$'(F, Body))))),
    % Fail to allow backtracking and finding more triggers.
    fail.

/* previously:  commented out variant of fcpt that handles 'presently' predicates, 
   not used due to specific scenario requirements */
% fcpt(Fact, F) :-
%   pfcGetTriggerQuick('$pt$'(presently(F), Body)),
%   fcEvalLHS(Body, (presently(Fact), '$pt$'(presently(F), Body))),
%   fail.

% A fallback to succeed if no more positive triggers are found.
fcpt(_, _).

/**
 * @predicate fcnt/2
 * Checks for negative triggers during forward chaining.
 * @param Fact - The fact to be checked.
 * @param F - A copy of the fact.
 */
fcnt(_Fact, F) :-
    % Check if there is a negative trigger.
    pfc_spft(X, _, '$nt$'(F, Condition, Body)),
    % Call the system to evaluate the condition.
    pfcCallSystem(Condition),
    % Remove the negative trigger support.
    pfcRem_S(X, (_, '$nt$'(F, Condition, Body))),
    % Fail to allow backtracking and finding more negative triggers.
    fail.
fcnt(_, _).

/**
 * @predicate pfcRem_S/2
 * Removes support for a fact and retracts it if no longer supported.
 * @param P - The fact.
 * @param S - The support to be removed.
 */
pfcRem_S(P, S) :-
    % Trace message indicating support removal.
    pfcTraceMsg('    Removing support: ~p from ~p~n', [S, P]),
    % Attempt to remove one support and check if fact is unsupported.
    pfcRemOneSupport(P, S) 
        -> removeIfUnsupported(P)
        ; % If support cannot be removed, issue a warning.
          pfcWarn("pfcRem_S/2 Could not find support ~p to remove from fact ~p", [S, P]).


/**
 * @predicate pfcDefineBcRule/3
 * Defines a backward chaining rule and adds the corresponding '$bt$' triggers.
 * @param Head - The head of the rule.
 * @param Body - The body of the rule.
 * @param ParentRule - The parent rule to which the backward chaining rule belongs.
 */
pfcDefineBcRule(Head, _Body, ParentRule) :-
    % Check if the head of the rule is not an atomic literal.
    (\+ pfcLiteral(Head)),
    % Warn if the backward chaining rule is malformed.
    pfcWarn("Malformed backward chaining rule.  ~p not atomic literal.", [Head]),
    pfcError("caused by rule: ~p", [ParentRule]),
    % Fail if rule is malformed.
    !, fail.

pfcDefineBcRule(Head, Body, ParentRule) :-
    % Add the backward chaining rule to the system (details omitted).
    %...
/** <module> PFC Evaluation and Backward Chaining Module
    This module handles PFC (Prolog Forward Chaining) evaluation
    for both the LHS (Left Hand Side) and RHS (Right Hand Side) of rules, 
    including backward chaining and triggers.
*/

/* Copy the parent rule for further modifications */
% @param ParentRule The original rule.
% @param ParentRuleCopy A copy of the parent rule.
copy_term(ParentRule, ParentRuleCopy),

/* Build the right-hand side (RHS) of a rule */
% @param Head The head of the rule.
% @param Rhs The right-hand side of the rule constructed from Head.
buildRhs(Head, Rhs),

/* Get the current user for tracking purposes */
% @param USER The current user of the system.
% This line uses a custom predicate current_why_U to fetch the user. 
% @TODO: Review why '_U' is used here and confirm its purpose.
current_why_U(USER), 

/* Iterate over each LHS in the PFC rule and create a trigger */
% pfcForEach is used to find all matching facts (using lazy_findall)
% for the given LHS, and a trigger is constructed for each one.
pfcForEach(pfc_nf(Body, Lhs),
    (buildTrigger(Lhs, rhs(Rhs), Trigger),
    /* Add the trigger and its associated backward chaining */
    pfcAdd('$bt$'(Head, Trigger), (ParentRuleCopy, USER)))).

/** get_bc_clause(+Head, -Clause)
    Retrieve the backward chaining clause for the given head.
    @param Head The head of the clause to retrieve.
    @param Clause The corresponding clause (HeadC :- BodyC).
*/
get_bc_clause(Head, (HeadC :- BodyC)) :- get_bc_clause(Head, HeadC, BodyC).

/** get_bc_clause(+HeadIn, -HeadC, -Body)
    Recursive form of get_bc_clause to handle negation.
    @param HeadIn The input head, potentially negated (~Head).
    @param HeadC The resulting (possibly negated) head.
    @param Body The body of the backward chaining clause.
*/
get_bc_clause(HeadIn, ~HeadC, Body) :- 
    compound(HeadIn), HeadIn = ~Head, !,
    /* Establish the body for the negated head */
    Body = (awc, 
           (nonvar(HeadC) -> (HeadC = Head, !) ; (HeadC = Head)), 
           pfc_bc_and_with_pfc(~Head)).

get_bc_clause(Head, Head, Body) :-  
    /* Handle the case where Head is not negated */
    Body = (awc, !, pfc_bc_and_with_pfc(Head)).

/* Thread initialization directive */
% This directive initializes a named variable to hold current PFC choices.
% It runs once at thread startup.
:- thread_initialization(nb_setval('$pfc_current_choice', [])).

/** push_current_choice/0
    Push the current Prolog choice point onto the stack.
    This handles cases where PFC cuts are supported.
*/
push_current_choice :- current_prolog_flag(pfc_support_cut, false), !.
push_current_choice :- prolog_current_choice(CP), push_current_choice(CP), !.
push_current_choice(CP) :- 
    nb_current('$pfc_current_choice', Was) -> 
    b_setval('$pfc_current_choice', [CP | Was]) ; 
    b_setval('$pfc_current_choice', [CP]).

/** cut_c/0
    Perform a Prolog cut to the most recent choice point, 
    provided by PFC.
*/
cut_c :- current_prolog_flag(pfc_support_cut, false), !.
cut_c :- must_ex(nb_current('$pfc_current_choice', [CP | _WAS])), prolog_cut_to(CP).

/** fcEvalLHS(+LHS, +Support)
    Evaluate the Left-Hand Side (LHS) of a rule.
    Depending on the form of LHS, different actions are triggered.
    @param LHS The LHS of the rule to evaluate.
    @param Support The supporting context for this evaluation.
*/
fcEvalLHS((Test -> Body), Support) :-
    /* If-then construct: evaluate Test, then proceed with Body */
    !,
    pfcDoAll(pfcCallSystem(Test) -> fcEvalLHS(Body, Support)),
    !.

fcEvalLHS((Test *-> Body), Support) :-
    /* Soft-cut construct: Test, then Body if Test succeeds */
    !,
    pfcDoAll(pfcCallSystem(Test) *-> fcEvalLHS(Body, Support)).

fcEvalLHS(rhs(X), Support) :-
    /* Directly evaluate the RHS */
    !,
    pfcDoAll(pfc_eval_rhs(X, Support)),
    !.

fcEvalLHS(X, Support) :-
    /* If X is a trigger, add it */
    pfcType(X, trigger(_Pos)),
    !,
    pfcAddTrigger(X, Support),
    !.

/* Dead code: commented out snippet of code for snip triggers */
/* previously: 
fcEvalLHS(snip(X), Support) :-
    snip(Support),
    fcEvalLHS(X, Support).
*/

fcEvalLHS(X, _) :-
    /* Unrecognized LHS */
    pfcWarn("Unrecognized item found in trigger body, namely ~p.", [X]).

/** pfc_eval_rhs(+RHS, +Support)
    Evaluate the Right-Hand Side (RHS) of a rule.
    @param RHS The RHS to be evaluated, may consist of multiple parts.
    @param Support The supporting context for this evaluation.
*/
pfc_eval_rhs([], _) :- !.
pfc_eval_rhs([Head | Tail], Support) :-
    /* Recursively evaluate each part of the RHS */
    pfc_eval_rhs1(Head, Support),
    pfc_eval_rhs(Tail, Support).

/** pfc_eval_rhs1(+Fact, +Support)
    Helper to evaluate individual RHS facts or actions.
    @param Fact The fact or action to evaluate.
    @param Support The supporting context.
*/
pfc_eval_rhs1(Fact, S) :- 
    control_arg_types(Fact, Fixed), 
    !, 
    pfc_eval_rhs1(Fixed, S).

pfc_eval_rhs1({Action}, Support) :-
    /* Evaluate a Prolog action */
    !,
    fcEvalAction(Action, Support).

pfc_eval_rhs1(P, _Support) :-
    /* Remove a negated literal */
    pfcNegatedLiteral(P),
    !,
    pfcWithdraw(P).

pfc_eval_rhs1([X | Xrest], Support) :-
    /* Embedded sublist */
    !,
    pfc_eval_rhs([X | Xrest], Support).

pfc_eval_rhs1(Assertion, Support) :-
    /* Assert a fact */
    once_writeq_nl(pfcRHS(Assertion)),
    (must_ex(pfcPost1(Assertion, Support)) *-> true ; 
    pfcWarn("Malformed RHS of a rule: ~p", [Assertion])).

/** fcEvalAction(+Action, +Support)
    Evaluate an action found on the RHS of a rule.
    @param Action The action to evaluate.
    @param Support The supporting context.
*/
fcEvalAction(Action, Support) :-
    pfcCallSystem(Action),
    (undoable(Action)
    -> pfcAddActionTrace(Action, Support)
    ; true).

/** trigger_trigger(+Trigger, +Body, +Support)
    Handle triggers in PFC rules.
    @param Trigger The trigger condition.
    @param Body The body to evaluate if the trigger fires.
    @param Support The supporting context.
*/
trigger_trigger(Trigger, Body, _Support) :-
    trigger_trigger1(Trigger, Body).
trigger_trigger(_, _, _).

/* Dead code: previously used for "presently" triggers */
/* previously: 
trigger_trigger1(presently(Trigger), Body) :-
    !,
    copy_term(Trigger, TriggerCopy),
    pfc_call(Trigger).
*/
/**
 * trigger_trigger1/2
 * 
 * @param Trigger Term that triggers the body evaluation
 * @param Body The body to evaluate when the trigger is met
 * 
 * This predicate copies the Trigger term, then attempts to evaluate it.
 * It also associates the Trigger with its original form through `'$pt$'`.
 * The `fail/0` predicate ensures backtracking, so the process can be repeated if needed.
 *
 * @example
 * ?- trigger_trigger1(my_trigger, my_body).
 */
trigger_trigger1(Trigger, Body) :-
  % Copy the Trigger term into TriggerCopy to preserve the original.
  copy_term(Trigger, TriggerCopy),
  
  % Call the pfc_call/1 predicate to attempt proving Trigger.
  pfc_call(Trigger),
  
  % Associate Trigger with its copy and call fcEvalLHS.
  with_current_why(Trigger, fcEvalLHS(Body, (Trigger, '$pt$'(TriggerCopy, Body)))),
  
  % Force backtracking with fail/0.
  fail.

/**
 * pfc_call/1
 * 
 * @param P Term to evaluate.
 * 
 * This predicate handles various cases for P. It attempts to prove facts, handle logic operators,
 * and invoke system predicates. This ensures compatibility with forward chaining logic.
 * 
 * @example
 * ?- pfc_call(my_predicate).
 */
% Check if P is a variable; if so, call pfcFact to attempt proving it as a fact.
pfc_call(P) :- var(P), !, pfcFact(P).

% Ensure P is callable; otherwise, throw an error.
pfc_call(P) :- \+ callable(P), throw(pfc_call(P)).

% Handle the cut operator (!).
pfc_call((!)) :- !, cut_c.

% Handle the truth case.
pfc_call(true) :- !.

% Handle conditional logic with "if-then-else".
pfc_call((A -> B; C)) :- !, pfc_call(A) -> pfc_call(B); pfc_call(C).

% Handle a form of conditional logic using soft cut.
pfc_call((A *-> B; C)) :- !, pfc_call(A) *-> pfc_call(B); pfc_call(C).

% Handle if-then without the else case.
pfc_call((A -> B)) :- !, pfc_call(A) -> pfc_call(B).

% Handle soft cut without the else case.
pfc_call((A *-> B)) :- !, pfc_call(A) *-> pfc_call(B).

% Handle conjunction (A, B).
pfc_call((A, B)) :- !, pfc_call(A), pfc_call(B).

% Handle disjunction (A; B).
pfc_call((A; B)) :- !, pfc_call(A); pfc_call(B).

% Handle negation (\+ A).
pfc_call(\+ (A)) :- !, \+ pfc_call(A).

% Handle arithmetic expressions.
pfc_call((A is B)) :- !, A is B.

% Handle clause lookup.
pfc_call(clause(A, B)) :- !, clause(A, B).
pfc_call(clause(A, B, Ref)) :- !, clause(A, B, Ref).

% Handle the triggering of backward chaining rules (via '$bt$').
pfc_call(P) :-
  % Attempt to trigger backward chaining (bc) rules for P.
  '$bt$'(P, Trigger),
  
  % Get support for the rule that was triggered.
  pfcGetSupport('$bt$'(P, Trigger), S),
  
  % Evaluate the left-hand side of the rule using the trigger and support.
  fcEvalLHS(Trigger, S),
  
  % Fail to backtrack.
  fail.

% Check if P is a system predicate; if so, call it directly.
pfc_call(P) :- predicate_property(P, imported_from(system)), !, call(P).
pfc_call(P) :- predicate_property(P, built_in), !, call(P).

% Handle dynamic predicates by calling them directly.
pfc_call(P) :- \+ predicate_property(P, _), functor(P, F, A), dynamic(F/A), !, call(P).

% Call predicates that have no clauses, directly.
pfc_call(P) :- \+ predicate_property(P, number_of_clauses(_)), !, call(P).

% Handle the current choice point and retrieve clauses to evaluate.
pfc_call(P) :-
  setup_call_cleanup(
    % Save the current choice point for backtracking.
    nb_current('$pfc_current_choice', Was),
    
    % Capture the current prolog choice point and call clauses for P.
    (prolog_current_choice(CP), push_current_choice(CP), clause(P, Condition), pfc_call(Condition)),
    
    % Restore the saved choice point after evaluation.
    nb_setval('$pfc_current_choice', Was)).

/* previously: This version of pfc_call was skipped because the new version is more efficient
 * and handles dynamic predicates better. It was replaced with a more streamlined version above.
 *
 * pfc_call(P) :-
 *   clause(P,true)*-> true ; (clause(P,Condition), Condition\==true,
 *      pfc_call(Condition)).
 */

/**
 * undoable/1
 * 
 * @param A Action to check for undoability.
 * 
 * This predicate succeeds if there is a defined method for undoing action A.
 */
undoable(A) :- fcUndoMethod(A, _).

/**
 * pfc_cache_bc/1
 * 
 * @param P Term to cache backward chaining results for.
 * 
 * This predicate ensures backward chaining rules for P are evaluated and cached.
 * It iterates through all backward chaining triggers and their supports.
 */
pfc_cache_bc(P) :-
  % Iterate over all backward chaining triggers for P.
  forall('$bt$'(P, Trigger),
         % For each trigger, evaluate and cache the left-hand side of the rule.
         forall(pfcGetSupport('$bt$'(P, Trigger), S),
                fcEvalLHS(Trigger, S))).

/**
 * pfc_nf/2
 * 
 * @param In The left-hand side of the rule to normalize.
 * @param Out The normalized form of the rule.
 * 
 * This predicate converts the left-hand side of a PFC rule into a normal form.
 * It may backtrack to produce additional clauses as needed.
 */
pfc_nf(LHS, List) :-
  % Convert the LHS to its first normal form.
  pfc_nf1(LHS, List2),
  
  % Handle negations to produce the final normalized list.
  pfc_nf_negations(List2, List).

/**
 * pfc_nf1/2
 * 
 * @param In The left-hand side of the rule to normalize.
 * @param Out The intermediate normalized form.
 * 
 * This predicate handles specific forms of LHS expressions in PFC rules.
 * It may backtrack into additional clauses.
 */

% Handle variables by returning them as-is.
pfc_nf1(P, [P]) :- var(P), !.

% The next rule handles ... (upward compatibility reasons).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf1(+P/Cond, -NF) is det.
% %
% % This predicate normalizes a literal `P` with a condition `Cond`.
% % It handles different types of literals such as negated literals,
% % conjunctions, disjunctions, and atomic literals.
% %
% % @example 
% % ?- pfc_nf1((a,b), NF).
% % NF = [a, b].
% %
% % The first clause checks if the literal `P` is negated and converts it 
% % into a normalized form.
% %
pfc_nf1(P/Cond, [( \+P )/Cond]) :- 
    pfcNegatedLiteral(P), % Check if P is a negated literal.
    !. % Cut to prevent backtracking if this clause succeeds.

% % The second clause handles cases where `P` is a non-negated literal 
% % and keeps it in its original form.
pfc_nf1(P/Cond, [P/Cond]) :-  
    pfcLiteral(P),  % Check if P is a literal (non-negated).
    !.

% % The next clause handles the negation of a term by calling pfc_unnegate/2
% % to get the non-negated version and recursively normalizing that.
pfc_nf1(NegTerm, NF) :- 
    pfc_unnegate(NegTerm, Term), % Unnegate NegTerm to obtain Term.
    !, 
    pfc_nf1_negation(Term, NF). % Normalize the negation of the term.

% % Clause for handling disjunction (P ; Q). It recursively normalizes both
% % parts and returns the normalized form of either part.
pfc_nf1((P;Q), NF) :- 
    !, 
    (pfc_nf1(P, NF) ; pfc_nf1(Q, NF)).

% % Clause for handling conjunction (P, Q). Both parts of the conjunction 
% % are normalized separately and then combined using append/3.
pfc_nf1((P, Q), NF) :- 
    !, 
    pfc_nf1(P, NF1), % Normalize first part.
    pfc_nf1(Q, NF2), % Normalize second part.
    append(NF1, NF2, NF). % Combine both normalized parts.

% % This clause handles random atomic literals that are not negated or combined
% % with other terms. It simply returns the literal in a list.
pfc_nf1(P, [P]) :- 
    pfcLiteral(P), % Check if P is a literal.
    !.

% % If none of the above clauses succeed, the predicate issues a warning 
% % indicating that the term cannot be normalized, but still accepts it.
pfc_nf1(Term, [Term]) :- 
    pfcWarn("pfc_nf doesn''t know how to normalize ~p (accepting though)", [Term]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf1_negation(+P, -NF) is det.
% %
% % This predicate normalizes the negation of a term `P`. It handles negation
% % for conjunctions, disjunctions, and atomic terms.
% %
pfc_nf1_negation((P/Cond), [( \+(P) )/Cond]) :- 
    !. % Handle negation of a conditional term.

% % This clause handles the negation of a disjunction (P ; Q). 
% % It recursively negates both parts and appends the results.
pfc_nf1_negation((P;Q), NF) :- 
    !, 
    pfc_nf1_negation(P, NFp), % Negate first part.
    pfc_nf1_negation(Q, NFq), % Negate second part.
    append(NFp, NFq, NF).

% % This clause handles negation of a conjunction (P, Q). 
% % It was noted to be incorrect, and the second part is skipped to prevent issues.
% % The alternative is to normalize both parts and combine the results.
pfc_nf1_negation((P, Q), NF) :- 
    /* previously: this code is not correct! twf. */ 
    !, 
    pfc_nf1_negation(P, NF) % Attempt to negate the first part.
    ; 
    (pfc_nf1(P, Pnf), % Normalize the first part.
     pfc_nf1_negation(Q, Qnf), % Negate the second part.
     append(Pnf, Qnf, NF)). % Combine results.

% % Default clause for negating a single term P.
pfc_nf1_negation(P, [\+P]).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf_negations(+List2, -List) is det.
% %
% % This predicate normalizes all negations in a list. It converts negations
% % from the form ~{...} to {\+...}.
% % It is unclear if this is still needed.
% % 
% % @example
% % ?- pfc_nf_negations([a, ~(b)], NF).
% % NF = [a, {\+ b}].
% %
pfc_nf_negations(X, X) :- 
    !. % If X is already normalized, return it as is.

% % Base case for recursion. An empty list is normalized to an empty list.
pfc_nf_negations([], []).

% % Recursive case for lists. The head of the list is normalized, and the
% % normalization continues for the tail.
pfc_nf_negations([H1|T1], [H2|T2]) :- 
    pfc_nf_negation(H1, H2), % Normalize the head.
    pfc_nf_negations(T1, T2). % Recursively normalize the tail.

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % pfc_nf_negation(+Form, -NegForm) is det.
% %
% % This predicate handles various forms of negations. It checks for
% % specific patterns of negation (e.g., tilded negations) and converts them 
% % into the standard form {\+ X}.
% %
pfc_nf_negation(Form, {\+ X}) :- 
    nonvar(Form), 
    Form = (~({X})), % Handle tilded negation.
    !.

pfc_nf_negation(Form, {\+ X}) :- 
    tilded_negation, % Check for a tilded negation.
    nonvar(Form), 
    Form = (-({X})), % Handle negation using -.
    !.

pfc_nf_negation(Form, {\+ X}) :- 
    tilded_negation, 
    nonvar(Form), 
    Form = ( \+ ({X})), % Handle negation using \+.
    !.

% % Default case: if no specific negation form is found, return the form unchanged.
pfc_nf_negation(X, X).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % constrain_meta(+Lhs, ?Guard) is semidet.
% %
% % This predicate generates a constraint guard for a given LHS (Left-Hand Side)
% % of a rule. It handles positive facts, negations, and various forms of chaining.
% % The flag `constrain_meta` can be disabled to turn off this feature.
% %
% % To disable:
% % ?- set_prolog_flag(constrain_meta,false).
% %
constrain_meta(_, _) :- 
    current_prolog_flag(constrain_meta, false), % Check if the flag is set to false.
    !, 
    fail.

% % Fact case: If LHS is a fact, constrain it as a positive fact.
constrain_meta(P, mpred_positive_fact(P)) :- 
    is_ftVar(P), % Check if P is a first-order term.
    !.

% % Negation chaining: Handle negations in the LHS by recursively constraining the term.
constrain_meta(~P, CP) :- 
    !, 
    constrain_meta(P, CP).

constrain_meta(\+P, CP) :- 
    !, 
    constrain_meta(P, CP).

% % Forward chaining: Constrain the RHS (Right-Hand Side) of forward chaining rules.
constrain_meta((_ ==> Q), nonvar(Q)) :- 
    !, 
    is_ftVar(Q).

% % Equivalence chaining: Ensure that either side of the equivalence is non-variable.
constrain_meta((P <==> Q), (nonvar(Q); nonvar(P))) :- 
    (is_ftVar(Q); is_ftVar(P)), 
    !.

% % Backward chaining: Constrain the RHS of backward chaining rules.
constrain_meta((Q <- _), mpred_literal(Q)) :- 
    is_ftVar(Q), 
    !.

constrain_meta((Q <- _), CQ) :- 
    !, 
    constrain_meta(Q, CQ).

% % Conditional chaining: Handle LHS of conditional rules.
constrain_meta((Q :- _), mpred_literal(Q)) :- 
    is_ftVar(Q), 
    !.

constrain_meta((Q :- _), CQ) :- 
    !, 
    constrain_meta(Q, CQ).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % is_simple_lhs(+Lhs) is semidet.
% %
% % This predicate checks if the given LHS is simple enough to be used in 
% % specific contexts, such as certain rule constraints. Complex terms are
% % filtered out by failing.
% %


 is_simple_lhs(ActN):- is_ftVar(ActN),!,fail.
 is_simple_lhs( \+ _ ):-!,fail.
 is_simple_lhs( ~ _ ):-!,fail.
 is_simple_lhs( _  / _ ):-!,fail.
 is_simple_lhs((Lhs1,Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
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


%! buildTrigger(+Triggers, +Consequent, -X) is det.
% This predicate processes a list of triggers, and constructs a 
% Prolog term that represents a conjunction of triggers.
% The '$pt$' functor is used to represent the processed trigger.
% 
% The first argument is a list of triggers, the second argument is the 
% consequent (the result of the rule firing), and the third argument is the
% output, which is the processed trigger list.
% 
% @example
% ?- buildTrigger([trigger1, trigger2], consequent, X).
% X = '$pt$'(trigger1, '$pt$'(trigger2, consequent)).

!,
buildTest(Test, Test2),  % Call buildTest to process Test and get Test2.
buildTrigger([{Test2}|Triggers], Consequent, X).  % Recursively build triggers.

% buildTrigger([snip|Triggers], Consequent, snip(X)) :-
%   !,
%   buildTrigger(Triggers, Consequent, X).
%
% This piece of code was commented out, potentially because it's
% an alternative form that deals with 'snip' trigger formats, which 
% was not needed in the current context.

%! buildTrigger(+T, +Consequent, -X) is det.
% This predicate constructs a trigger by recursively conjoining
% elements using the '$pt$' functor.
% 
% @example
% ?- buildTrigger([t1, t2], consequent, X).
% X = '$pt$'(t1, '$pt$'(t2, consequent)).

buildTrigger([T|Triggers], Consequent, '$pt$'(T, X)) :-
  !,
  buildTrigger(Triggers, Consequent, X).  % Recursively build triggers.

% buildNtTest(+Trigger, +Testin, -Testout) is det.
% This predicate builds a test for negative triggers ('$nt$'/3).
% It ensures that no matching facts are in the database and any
% additional test specified in the rule is included.
% 
% @example
% ?- buildNtTest(trigger, test_in, Test_out).
% Test_out = (pfc_call(trigger), test_in).
%
buildNtTest(T, Testin, Testout) :-
  buildTest(Testin, Testmid),  % Process Testin to get Testmid.
  pfcConjoin((pfc_call(T)), Testmid, Testout).  % Conjoin the tests.

% buildTest(+TestIn, -TestOut) is det.
% This predicate simplifies curly bracketed terms.
% If the input is wrapped in curly brackets, it removes them.
% Otherwise, it leaves the term unchanged.
%
% @example
% ?- buildTest({a}, X).
% X = a.
%
% @example
% ?- buildTest(a, X).
% X = a.

buildTest({Test}, Test) :- !.  % If Test is wrapped in curly brackets, remove them.
buildTest(Test, Test).  % Otherwise, leave Test unchanged.

% pfcType(+Value, -Type) is semidet.
% This predicate identifies the type of a PFC (Prolog Forward Chaining)
% object. It distinguishes between facts, rules, and triggers.
% 
% @example
% ?- pfcType(==>(a,b), Type).
% Type = rule(fwd).

pfcType(Var, Type):- 
  var(Var), !,  % If the input is a variable, it's assumed to be a fact.
  Type = fact(_FT).

pfcType(_:X, Type):- 
  !, pfcType(X, Type).  % If the input is module-prefixed, recurse into the term.

pfcType(~_, Type):- 
  !, Type = fact(_FT).  % If the term starts with '~', it's a fact.

pfcType(('==>'(_,_)), Type):- 
  !, Type = rule(fwd).  % If the term is a forward chaining rule, mark it as such.

pfcType('==>'(X), Type):- 
  !, pfcType(X, Type), 
  pfcWarn(pfcType('==>'(X), Type)).  % Handle warnings for forward chaining rules.

pfcType(('<==>'(_,_)), Type):- 
  !, Type = rule(<==>).  % Bidirectional rule (equivalence).

pfcType(('<-'(_,_)), Type):- 
  !, Type = rule(bwc).  % Backward chaining rule.

pfcType((':-'(_,_)), Type):- 
  !, Type = rule(cwc).  % Classical Prolog rule.

pfcType('$pt$'(_,_,_), Type):- 
  !, Type = trigger(+).  % Positive trigger with three arguments.

pfcType('$pt$'(_, _), Type):- 
  !, Type = trigger(+).  % Positive trigger with two arguments.

pfcType('$nt$'(_, _, _), Type):- 
  !, Type = trigger(-).  % Negative trigger.

pfcType('$bt$'(_, _), Type):- 
  !, Type = trigger(?).  % Bidirectional trigger.

pfcType(pfcAction(_), Type):- 
  !, Type = action.  % Action term.

pfcType(('::::'(_,X)), Type):- 
  !, pfcType(X, Type).  % Handle ':'-prefixed terms.

pfcType(_, fact(_FT)):- 
  % If none of the above patterns match, assume it's a fact.
  !.

% pfcAssert(+P, +Support) is det.
% This predicate asserts a fact or rule P if it doesn't already exist,
% and adds the provided support information.
% 
% @example
% ?- pfcAssert(fact, support).
% true.
%
pfcAssert(P, Support) :-
  (pfc_clause(P) ; assert(P)),  % Assert P if it's not already in the database.
  !,
  pfcAddSupport(P, Support).  % Add support information for P.

% pfcAsserta(+P, +Support) is det.
% Like pfcAssert/2, but uses asserta/1 to add P to the front of the database.
pfcAsserta(P, Support) :-
  (pfc_clause(P) ; asserta(P)),
  !,
  pfcAddSupport(P, Support).

% pfcAssertz(+P, +Support) is det.
% Like pfcAssert/2, but uses assertz/1 to add P to the end of the database.
pfcAssertz(P, Support) :-
  (pfc_clause(P) ; assertz(P)),
  !,
  pfcAddSupport(P, Support).

% pfc_clause(+Head) is semidet.
% This predicate checks if a clause with a given head exists in the database.
% It ensures the clause is exactly the same by using variant/2.
% This helps avoid duplicate clauses with the same meaning but different variables.
%
pfc_clause((Head :- Body)) :- 
  !,
  copy_term(Head, Head_copy),
  copy_term(Body, Body_copy),
  clause(Head, Body),
  variant(Head, Head_copy),
  variant(Body, Body_copy).

pfc_clause(Head) :- 
  % Find unit clauses that are exactly identical to Head.
  copy_term(Head, Head_copy),
  clause(Head_copy, true),
  variant(Head, Head_copy).

% pfcForEach(+Binder, +Body) is det.
% This predicate iterates over Binder and applies Body for each result.
% After failing once, it succeeds (because of fail/0).
% 
% @example
% ?- pfcForEach((X=1; X=2), writeln(X)).
% 1
% 2
% true.

pfcForEach(Binder, Body) :- 
  Binder, pfcdo(Body), fail.  % Execute Body for each solution to Binder.
pfcForEach(_, _).  % End recursion when no more solutions exist.

% pfcdo(+X) is det.
% Executes X once and always succeeds. This ensures predicates 
% like pfcForEach don't fail prematurely.
pfcdo(X) :- 
  X, !.  % Execute X and succeed if it succeeds.
pfcdo(_).  % If X fails, succeed anyway.

% pfcUnion(+L1, +L2, -L3) is det.
% This predicate creates a union of two sets, L1 and L2, where sets
% are represented as simple lists.
% 
% @example
% ?- pfcUnion([a,b,c], [b,d], L3).
% L3 = [a, c, d].
%
pfcUnion([], L, L).  % The union of an empty set and L is L.
pfcUnion([Head|Tail], L, Tail2) :-
  memberchk(Head, L),  % If Head is already in L, skip it.
  !,
  pfcUnion(Tail, L, Tail2).  % Recursively process the tail.
pfcUnion([Head|Tail], L, [Head|Tail2]) :-
  pfcUnion(Tail, L, Tail2).  % Add Head to the result and process the tail.

% pfcConjoin(+Conjunct1, +Conjunct2, -Conjunction) is det.
% This predicate conjoins two terms, producing a simplified conjunction.
% It's used to build composite tests from individual conditions.
%  File   : pfcdb.pl
%  Author : Tim Finin, finin@prc.unisys.com
%  Author : Dave Matuszek, dave@prc.unisys.com
%  Author : Dan Corpron
%  Updated: 10/11/87, ...
%  Purpose: This file defines predicates to manipulate a PFC (Prolog Forward Chaining) database 
%           by saving, restoring, resetting the database, and handling database terms.

% The directive for loading or processing the file is assumed to be handled elsewhere.
% No file directives here explicitly.

% Predicate: pfcConjoin/3
% Combines two terms into a conjunction.
% The first two clauses handle cases where one of the terms is 'true' to avoid redundant conjunctions.
% If neither is 'true', the terms are combined into a conjunction (C1, C2).
%
% @example
% ?- pfcConjoin(true, X, Result).   % Result = X.
% ?- pfcConjoin(X, true, Result).   % Result = X.
% ?- pfcConjoin(a, b, Result).      % Result = (a, b).
pfcConjoin(true, X, X) :- !.  % If the first argument is true, the result is simply X.
pfcConjoin(X, true, X) :- !.  % If the second argument is true, the result is X.
pfcConjoin(C1, C2, (C1,C2)).  % Combine both arguments into a conjunction.

% Predicate: pfcDatabaseTerm/1
% Describes what kinds of terms are considered part of the PFC database.
% These terms are part of forward chaining logic in PFC and shouldn't appear in an empty database.
%
% @example
% ?- pfcDatabaseTerm('==>'/2).  % True if the term is part of the PFC database.
pfcDatabaseTerm('$spft$'/3).  % A special PFC term for forward chaining.
pfcDatabaseTerm('$pt$'/2).    % Another PFC term (predicates).
pfcDatabaseTerm('$bt$'/2).    % Base term (backward chaining triggers).
pfcDatabaseTerm('$nt$'/3).    % Negation trigger term.
pfcDatabaseTerm('==>'/2).     % Forward implication.
pfcDatabaseTerm('<==>'/2).    % Bidirectional implication.
pfcDatabaseTerm('<-'/2).      % Reverse implication.
pfcDatabaseTerm(pfcQueue/1).  % The queue used for managing PFC operations.

% Predicate: pfcReset/0
% Resets the PFC database by retracting all forward chaining rules and justifications.
% It checks whether any PFC-related items remain in the database, issuing a warning if so.
% 
% @example
% ?- pfcReset.  % Cleans the PFC database by retracting all relevant terms.
pfcReset :-
  pfc_spft(P, F, Trigger),  % Iterate through all forward chaining rules in the form of $spft$.
  pfcRetractOrWarn(P),      % Attempt to retract the rule's predicate P.
  pfcRetractOrWarn('$spft$'(P, F, Trigger)),  % Attempt to retract the $spft$ term itself.
  fail.  % Fail to force backtracking and ensure all relevant terms are retracted.
pfcReset :-  % After all retracts, verify the database is empty.
  (pfcDatabaseItem(T) ->  % Check if there are any remaining database items.
   (pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n", [T]), fail)  % Warn if there are.
    ; true).  % Succeed if no items remain.

% Predicate: pfcDatabaseItem/1
% True if there is still some PFC-related data (crud) in the database.
% It checks if any terms match the patterns defined by pfcDatabaseTerm/1.
%
% @example
% ?- pfcDatabaseItem(Term).  % Term will match any PFC-related database item.
pfcDatabaseItem(Term:-Body) :-
  pfcDatabaseTerm(P/A),  % Check if the term's functor matches a PFC database term.
  functor(Term, P, A),   % Extract the functor and arity of the term.
  clause(Term, Body).    % Check if the term exists in the database with a body.

% Predicate: pfcRetractOrWarn/1
% Retracts a term if it exists in the database, otherwise issues a warning.
% 
% @example
% ?- pfcRetractOrWarn(myFact).  % Retract 'myFact' if it exists, warn otherwise.
pfcRetractOrWarn(X) :-  retract(X), !.  % Retract the term if possible.
pfcRetractOrWarn(X) :-
  pfcWarn("Couldn't retract ~p.", [X]),  % Issue a warning if retract fails.
  nop((dumpST, pfcWarn("Couldn't retract ~p.", [X]))), !.  % Further logging (no operation in nop).

% Predicate: pfcRetractOrQuietlyFail/1
% Similar to pfcRetractOrWarn/1, but fails quietly instead of issuing a warning when retract fails.
% 
% @example
% ?- pfcRetractOrQuietlyFail(myFact).  % Retract 'myFact' if possible, fail otherwise without warning.
pfcRetractOrQuietlyFail(X) :-  retract(X), !.  % Retract the term if possible.
pfcRetractOrQuietlyFail(X) :-
  nop((pfcTraceMsg("Trace: Couldn't retract ~p.", [X]),  % Log the failure quietly.
      nop((dumpST, pfcWarn("Couldn't retract ~p.", [X]))))),  % Further logging.
  !, fail.  % Fail after attempting to log the failure quietly.

%   Previously commented out code and dead code for future reference:
%   It seems some logging or debugging-related code has been omitted or replaced by nop operations.
%   They may be useful for debugging purposes in specific situations, thus preserved as references.
%   /* previously:  pfcReset_with_logging ...  */
