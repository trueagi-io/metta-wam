/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/

%*********************************************************************************************
% PROGRAM FUNCTION: provides forward chaining support for logical rules, triggers, and fact
% management with features like dependency tracking and truth maintenance.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- if( \+ current_predicate(set_fileAssertMt/1)).

%!  set_prolog_flag(+Flag, +Value) is det.
%
%   Sets the given Prolog flag to the specified value.
%
%   This predicate allows modifying runtime flags that control various
%   aspects of the Prolog environment. In this case, it sets the `pfc_shared_module`
%   flag to `user`, affecting how modules are shared or accessed.
%
:- set_prolog_flag(pfc_shared_module,user).
%:- set_prolog_flag(pfc_shared_module,baseKB).

%!  must_ex(+Goal) is det.
%
%   Executes the given Goal, handling exceptions and retrying with tracing if needed.
%   If the Goal throws an exception, it catches and traces it. On failure, it logs
%   the failure and retries with `rtrace/1`.
%
%   @arg Goal The Prolog goal to be executed.
%
%   @example
%     % Execute a goal that might fail or raise an exception.
%     ?- must_ex(member(X, [1, 2, 3])).
%     X = 1 ;
%     X = 2 ;
%     X = 3.
%
must_ex(X) :-
    % Attempt to execute the Goal with exception handling.
    catch(X, E, rtrace(E)) *-> true
    ;  % If the Goal fails, log the failure and retry with tracing.
    (dmsg(failed(must_ex(X))), rtrace(X)).

%!  quietly_ex(+Goal) is det.
%
%   Calls the given Goal without logging or tracing.
%
%   @arg Goal The Prolog goal to be executed.
%
%   @example
%     % Silently execute a goal.
%     ?- quietly_ex(member(X, [1, 2, 3])).
%     X = 1 ;
%     X = 2 ;
%     X = 3.
%
quietly_ex(X) :-
    % Simply call the Goal without any logging or tracing.
    call(X).

% @TODO undisable when we have defined into_type/3 to not fail

%!  control_arg_types(+A, +B) is nondet.
%
%   @arg A The first argument to be controlled.
%   @arg B The second argument to be controlled.
%
%   @example
%     % Example usage of `control_arg_types/2`.
%     ?- control_arg_types(foo, bar).
%     false.  % As the logic starts with `fail/0`, this will always be false.
%
control_arg_types(A, B) :-
    % Immediately fail to ensure control only through the subsequent logic.
    fail,
    % Execute control_arg_types1/4 once with specific parameters.
    once(control_arg_types1(20, [], A, B)),
    % Ensure A and B are not identical.
    A \== B,
    % Use a cut to prevent further backtracking.
    !.

%:- listing(control_arg_types/3).

%!  control_arg_types1(+Max, +Pre, +A, -B) is det.
%
%   Controls the transformation or matching of arguments A and B based on a
%   maximum recursion depth. If the depth reaches zero or certain conditions
%   are met, it unifies A with B.
%
%   @arg Max The maximum recursion depth allowed.
%   @arg Pre A list representing the predicate stack for tracking context.
%   @arg A   The input term to be controlled.
%   @arg B   The output term after control logic is applied.
%
%   @example
%     % Unify two simple terms when max depth is reached.
%     ?- control_arg_types1(0, [], foo, X).
%     X = foo.
%
%     % Control a structure with nested functors.
%     ?- control_arg_types1(2, [], f(a, b), X).
%     X = f(a, b).
%
%     % Handle non-compound terms.
%     ?- control_arg_types1(2, [], 42, X).
%     X = 42.
%
control_arg_types1(Max, _, A, B):-
    % If the maximum depth is reached, unify A with B.
    Max < 1, !, A = B.
control_arg_types1(_Max, _, A, B):-
    % If A is not a compound term, unify A with B.
    \+ compound(A), !, A = B.
control_arg_types1(_Max, _, A, B):-
    % If A is a compound term and not a list, unify A with B.
    iz_conz(A), \+ is_list(A), !, A = B.
control_arg_types1(_Max, _, A, B):-
    % If `check_args/2` is available, use it to compare A and B.
    (current_predicate(check_args/2) -> check_args(A, B) -> A \=@= B), !.
% control_arg_types1(Max, Pre, A, B):-
%     % If A is a list, apply control_arg_types1/4 to each element.
%     is_list(A), !, maplist(control_arg_types1(Max, Pre), A, B).
control_arg_types1(Max, Pre, A, B):-
    % Decrement the maximum depth.
    Max0 is Max - 1,
    % Decompose A into functor and arguments.
    compound_name_arguments(A, F, AA),
    % Get the number of arguments.
    length(AA, N),
    % Control each argument.
    do_control_arg_types1(Max0, F/N, 1, Pre, AA, BB),
    % Reconstruct B with controlled arguments.
    compound_name_arguments(B, F, BB).

%!  do_control_arg_types1(+Max, +FofN, +ArgN, +Pre, +AA, -BB) is det.
%
%   Controls each argument in the given list of terms AA to produce BB.
%
%   @arg Max   The maximum recursion depth.
%   @arg FofN  The functor and arity of the current term.
%   @arg ArgN  The current argument number.
%   @arg Pre   A list representing the predicate stack.
%   @arg AA    The input list of arguments.
%   @arg BB    The output list of controlled arguments.
%
%   @example
%     % Control the arguments of a functor.
%     ?- do_control_arg_types1(2, f/2, 1, [], [a, b], X).
%     X = [a, b].
%
%     % Handle empty lists gracefully.
%     ?- do_control_arg_types1(2, f/2, 1, [], [], X).
%     X = [].
%
do_control_arg_types1(_Max, _FofN, _ArgNp1, _Pre, [], []):- !.
do_control_arg_types1(Max, FofN, ArgN, Pre, [A | AA], [B | BB]):-
    % Control a single argument.
    do_control_1arg_type(Max, FofN, ArgN, Pre, A, B),
    % Move to the next argument.
    ArgNp1 is ArgN + 1,
    do_control_arg_types1(Max, FofN, ArgNp1, Pre, AA, BB).

%!  do_control_1arg_type(+Max, +FofN, +N, +Pre, +A, -B) is det.
%
%   Controls a single argument A to produce B.
%
%   @arg Max   The maximum recursion depth.
%   @arg FofN  The functor and arity of the current term.
%   @arg N     The current argument number.
%   @arg Pre   A list representing the predicate stack.
%   @arg A     The input argument.
%   @arg B     The output argument.
%
%   @example
%     % Unify a variable argument.
%     ?- do_control_1arg_type(2, f/2, 1, [], X, Y).
%     X = Y.
%
%     % Apply a type constraint using `arg_n_isa/3`.
%     ?- assert(arg_n_isa(f, 1, integer)),
%        do_control_1arg_type(2, f/2, 1, [], 5, X).
%     X = 5.
%
%     % Recursively control a nested argument.
%     ?- do_control_1arg_type(1, f/1, 1, [], f(g(a)), X).
%     X = f(g(a)).
%
do_control_1arg_type(_Max, _FN, _N, _Pre, A, B):-
    % If A is a variable, unify B with A.
    var(A), !, B = A.
do_control_1arg_type(_Max, F / _, N, _Pre, A, B):-
    % If a type constraint exists, apply it.
    arg_n_isa(F, N, ISA), into_type(ISA, A, B), !.
do_control_1arg_type(Max, FofN, _, Pre, A, B):-
    % Recursively control the argument.
    Max0 is Max - 1,
    control_arg_types1(Max0, [FofN | Pre], A, B).

%!  arg_n_isa(+F, +N, -ISA) is nondet.
%
%   Retrieves the type constraint ISA for the N-th argument of a functor F.
%   If no such constraint is found, the predicate fails.
%
%   @arg F   The functor for which the argument type is being queried.
%   @arg N   The argument number (1-based index).
%   @arg ISA The type constraint for the N-th argument.
%
%   @example
%     % Query the type constraint for the 2nd argument of `example/3`.
%     ?- arg_n_isa(example, 2, ISA).
%
%   @see clause_b/1 for querying clause existence.
%
% arg_n_isa(_F, _N, _ISA) :- fail.
arg_n_isa(F, N, ISA) :-
    % Retrieve the type constraint for the N-th argument using clause_b/1.
    clause_b(argIsa(F, N, ISA)).

%!  save_pfc_state is det.
%
%   Saves the state of all PFC-related predicates by listing them.
%   The state terms include both dynamic and predefined PFC terms.
%
%   @see pfcStateTerm/1 for terms included in the state.
%
save_pfc_state :-
    %tell(pfcState),
    % List all predicates matching PFC state terms.
    forall(
        (pfcStateTerm(F/A), current_predicate(F/A)),
        listing(F/A)
    ),
    % Prevent further backtracking.
    %told.
    !.

%!  pfcDoAll(:Goal) is det.
%
%   Executes the given Goal for all possible solutions, ensuring success for each.
%
%   @arg Goal The goal to be executed.
%
%   @example
%     % Execute a goal for all solutions.
%     ?- pfcDoAll(member(X, [1, 2, 3])).
%
pfcDoAll(Goal) :-
    % Ensure that Goal succeeds for all possible solutions.
    forall(call(Goal), true).

%!  pfcStateTerm(?Term) is nondet.
%
%   Defines terms related to the PFC (Prolog Forward Chaining) system state.
%   This predicate succeeds for any valid PFC state term, whether dynamically
%   defined or listed among predefined state terms.
%
%   @arg Term The functor/arity pair representing a PFC state term.
%
%   @example
%     % Check if `fcAction/2` is a valid PFC state term.
%     ?- pfcStateTerm(fcAction/2).
%     true.
%
%     % Attempt to match a non-existent state term.
%     ?- pfcStateTerm(nonexistent/1).
%     false.
%
pfcStateTerm(F/A) :-
    % Check if F/A is a term stored in the PFC database.
    pfcDatabaseTerm(F/A).
pfcStateTerm(F/A) :-
    % Check if F/A matches one of the predefined PFC state terms.
    member((F/A), [
        fcUndoMethod/2,
        fcAction/2,
        fcTmsMode/1,
        pfcQueue/1,
        pfcCurrentDb/1,
        pfcHaltSignal/1,
        pfcDebugging/0,
        pfcSelect/1,
        pfcSearch/1
    ]).

%
%   The following code handles the dynamic management of PFC (Prolog Forward Chaining)
%   state by asserting or retracting facts depending on the loading context
%   and whether cross-referencing is enabled. It interacts with modules and
%   system predicates to track and manage PFC usage.
%

:- if((current_prolog_flag(xref, true) ;
   ('$current_source_module'(SM), 'context_module'(M), '$current_typein_module'(CM),
    current_prolog_flag(pfc_shared_module, BaseKB),
    asserta(BaseKB:'wusing_pfc'(M, CM, SM, pfc_rt))))).
% If cross-referencing is enabled or certain modules are active,
% assert that PFC is being used with the current modules.
:- endif.

:- if(current_prolog_flag(xref, true)).
% If cross-referencing is enabled, optionally define an empty module.
% :- module(pfc_rt, []).
:- endif.

:- if((prolog_load_context(source, File), prolog_load_context(file, File))).
% If the file being loaded matches the source file, import necessary utilities.
:- use_module(library(logicmoo_utils)).
% Optionally, unload the file after loading.
% :- prolog_load_context(file, File), unload_file(File).
:- endif.

% :- pfc_lib:use_module(pfc_lib).
% Optionally include the PFC library for further functionality.

:- if(\+ current_prolog_flag(xref, true)).
% If cross-referencing is not enabled, perform PFC state updates.
:- current_prolog_flag(pfc_shared_module, BaseKB),
   must_ex(retract(BaseKB:'wusing_pfc'(M, CM, SM, pfc_rt))),
   % Log the retraction of PFC usage.
   nop(fbugio(BaseKB:'chusing_pfc'(M, CM, SM, pfc_rt))),
   (M == SM ->
     % Ensure the ABox for the shared module and assert its relation to BaseKB.
     (nop(maybe_ensure_abox(SM)),
      nop((M:ain(genlMt(SM, BaseKB)))));
     % Otherwise, log the PFC usage for tracking purposes.
     nop(fbugio(BaseKB:'lusing_pfc'(M, CM, SM, pfc_rt)))
   ),
   % Assert that the current modules are using PFC.
   assert(BaseKB:'$using_pfc'(M, CM, SM, pfc_rt)),
   asserta(SM:'$does_use_pfc_mod'(M, CM, SM, pfc_rt)).
   % Optionally enable a backtrace with a depth of 200.
   % backtrace(200).

/*
:- multifile '$exported_op'/3.
:- dynamic '$exported_op'/3.
:- discontiguous '$exported_op'/3.
'$exported_op'(_,_,_):- fail.
*/

% multifile     = Declare predicate can be defined across multiple files
% dynamic       = Make the predicate runtime modifyable
% discontiguous = Declare that clauses of the predicate need not be contiguous

:- multifile '$pldoc'/4.
:- dynamic '$pldoc'/4.
:- discontiguous '$pldoc'/4.
'$pldoc'(_,_,_,_):- fail.

:- multifile '$autoload'/3.
:- discontiguous '$autoload'/3.
:- dynamic '$autoload'/3.
'$autoload'(_,_,_):- fail.

%
%   This section sets important Prolog flags and conditionally imports
%   predicates depending on the environment or platform. The configuration
%   includes enabling useful system libraries and managing how Prolog
%   interacts with PFC (Prolog Forward Chaining).

% Load the `make` library to allow dynamic reloading and recompiling of code.
:- system:use_module(library(make)).

% Optional flag settings for PFC behavior, currently commented out:
% :- set_prolog_flag(retry_undefined, kb_shared).
%     % Retry undefined predicates in the shared knowledge base.
% :- set_prolog_flag(pfc_ready, true).
%     % Set the flag to signal that PFC is ready for use.

% Set the `expect_pfc_file` flag to `unknown`. This controls whether Prolog
% expects to load PFC files during execution.
:- set_prolog_flag(expect_pfc_file, unknown).
:- endif.

% Import predicates from the `ifprolog` library, if available. These predicates
% are related to date handling and are platform-specific.
:- ifprolog:import(date:day_of_the_week/2).
:- ifprolog:import(date:day_of_the_year/2).

tilded_negation.

%!  bagof_or_nil(+Template, :Goal, -List) is det.
%
%   Executes `bagof/3` and returns the solutions as a list. If no solutions
%   are found, it returns an empty list instead of failing.
%
%   @arg Template The term to collect.
%   @arg Goal     The goal to solve.
%   @arg List     The list of collected solutions, or an empty list if none.
%
%   @example
%     % Collect all elements of a list.
%     ?- bagof_or_nil(X, member(X, [1,2,3]), L).
%     L = [1, 2, 3].
%
%     % If no solutions are found, return an empty list.
%     ?- bagof_or_nil(X, member(X, []), L).
%     L = [].
%
bagof_or_nil(T, G, L) :-
    % Try to collect solutions using `bagof/3`. If it fails, return an empty list.
    bagof(T, G, L) *-> true ; L = [].

%!  setof_or_nil(+Template, :Goal, -List) is det.
%
%   Executes `setof/3` and returns the solutions as a sorted list. If no
%   solutions are found, it returns an empty list instead of failing.
%
%   @arg Template The term to collect.
%   @arg Goal     The goal to solve.
%   @arg List     The sorted list of solutions, or an empty list if none.
%
%   @example
%     % Collect unique and sorted elements.
%     ?- setof_or_nil(X, member(X, [3,2,1,2]), L).
%     L = [1, 2, 3].
%
%     % If no solutions are found, return an empty list.
%     ?- setof_or_nil(X, member(X, []), L).
%     L = [].
%
setof_or_nil(T, G, L) :-
    % Try to collect sorted solutions using `setof/3`. If it fails, return an empty list.
    setof(T, G, L) *-> true ; L = [].

%!  call_u(:Goal) is det.
%
%   Calls the given goal using `pfcCallSystem/1`.
%
%   @arg Goal The goal to be executed.
%
%   @example
%     % Call a simple goal that succeeds.
%     ?- call_u(member(X, [a, b, c])).
%     X = a ;
%     X = b ;
%     X = c.
%
%     % Call a goal that fails.
%     ?- call_u(member(x, [a, b, c])).
%     false.
%
call_u(G) :- pfcCallSystem(G).

%!  clause_u(+Head, -Body) is nondet.
%
%   Retrieves a clause with the given head and body.
%
%   This predicate is a wrapper around the built-in `clause/2` predicate.
%   It succeeds if the clause with the specified head and body exists.
%
%   @arg Head The head of the clause.
%   @arg Body The body of the clause.
%
%   @example
%     % Define a sample fact and a rule.
%     ?- assertz(foo(a)).
%     ?- assertz((foo(X) :- bar(X))).
%
%     % Retrieve a fact with the specified head.
%     ?- clause_u(foo(a), Body).
%     Body = true.
%
%     % Retrieve a rule with the specified head and body.
%     ?- clause_u(foo(X), Body).
%     Body = bar(X).
%
clause_u(H, B) :- clause(H, B).

%!  mpred_ain(+Predicate) is det.
%
%   Asserts a predicate into the PFC system, handling additional logic
%   for assertions with bodies.
%
%   @arg Predicate The predicate to be asserted.
%
%   @example
%     % Assert a fact into the PFC system.
%     ?- mpred_ain(foo(a)).
%
%     % Assert a rule into the PFC system.
%     ?- mpred_ain((foo(X) :- bar(X))).
%
mpred_ain(P) :- arc_assert(P).

%!  arc_assert(+Clause) is det.
%
%   Asserts a clause or fact into the Prolog system. If the given clause has a
%   body of `true`, only the head is asserted. The predicate also ensures that
%   additional metadata is attached to the assertion using `current_why_UU/1`.
%
%   This predicate is used for managing the insertion of facts or rules in
%   systems that track provenance (the reason or context behind each assertion).
%
%   @arg Clause The clause or fact to be asserted. It can be a head or a head-body
%        combination (e.g., `Head :- Body`).
%
%   @example
%     % Assert a fact with provenance tracking.
%     ?- arc_assert(foo(a)).
%
%     % Assert a rule with a true body, reducing it to a fact.
%     ?- arc_assert((foo(a) :- true)).
%
%     % Assert a rule with a meaningful body.
%     ?- arc_assert((foo(X) :- bar(X))).
%
arc_assert(P :- True) :-
    % If the body is `true`, only the head is asserted.
    True == true, !, arc_assert(P).
arc_assert(P) :-
    % Ensure that `current_why_UU/1` provides a reason for the assertion.
    must_ex(current_why_UU(UU)),
    % Optionally log the assertion (disabled with `nop`).
    nop(fbugio(pfcAdd(P, UU))), !,
    % Assert the predicate along with its provenance information.
    (P, UU),
    % Use `asserta_if_new/1` to ensure the assertion is unique.
    asserta_if_new(P).

%!  pfc_retract(+Predicate) is det.
%
%   Retracts a predicate from the PFC system with logging.
%
%   @arg Predicate The predicate to retract.
%
%   @example
%     % Assert and then retract a fact.
%     ?- mpred_ain(foo(a)).
%     ?- pfc_retract(foo(a)).
%
%     % Assert and then retract a rule.
%     ?- mpred_ain((foo(X) :- bar(X))).
%     ?- pfc_retract(foo(X)).
%
pfc_retract(P) :- fbugio(pfc_retract(P)), pfcRetract(P).

%!  pfc_retractall(+Predicate) is det.
%
%   Retracts all instances of a predicate from the PFC system with logging.
%
%   @arg Predicate The predicate to retract.
%
%   @example
%     % Assert multiple instances and retract all of them.
%     ?- mpred_ain(foo(a)).
%     ?- mpred_ain(foo(b)).
%     ?- pfc_retractall(foo(_)).
%
%     % Verify that all instances have been retracted.
%     ?- clause_u(foo(_), _).
%     false.
%
pfc_retractall(P) :- fbugio(pfc_retractall(P)), pfcRetractAll(P).

%!  ~(+Term) is det.
%
%   A dynamic negation operator. Always fails when called.
%
:- dynamic((~)/1).
~(_) :- fail.

%!  add(+X) is det.
%
%   Adds a term to the PFC system using `pfcAdd/1`.
%
%   @arg X The term to add.
%
%   @example
%     % Add a fact to the PFC system.
%     ?- add(foo(a)).
%
%     % Add a rule to the PFC system.
%     ?- add((foo(X) :- bar(X))).
%
add(X) :- pfcAdd(X).

%!  mpred_test(:Goal) is det.
%
%   Tests a given goal within the PFC system and logs the result. Handles both
%   positive and negated tests, ensuring proper logging and explanation of failures.
%
%   @arg Goal The goal to test.
%
%   @example
%     % Test a goal that succeeds.
%     ?- mpred_test(call_u(member(X, [1, 2, 3]))).
%     X = 1 ;
%     X = 2 ;
%     X = 3.
%
%     % Test a goal that fails and triggers the negated test logic.
%     ?- mpred_test(\+ call_u(member(4, [1, 2, 3]))).
%     true.
%
mpred_test(call_u(X)) :-
    % If X is not a variable, execute the goal using pfcCallSystem/1.
    nonvar(X), !,
    pfcCallSystem(X),  % Call the system version of the goal.
    pfcWhy(X).         % Log the reasoning for the goal success.
mpred_test(\+ call_u(X)) :-
    % If X is not a variable, test the negated goal.
    nonvar(X), !,
    (call_u(X) ->
        % If the goal unexpectedly succeeds, log a warning and explain why.
        (fbugio(warn(failed(mpred_test(\+ call_u(X))))),
         mpred_test_why(X))  % Log the reason for the success.
    ;
        % If the negated goal holds, log the explanation for its success.
        mpred_test_why(~(X))
    ).
mpred_test(X) :-
    % Test the goal. If it fails, try testing its negation.
    (mpred_test_why(X) *-> true ; mpred_test_why(~(X))).

% Declare `shown_child/1` and `shown_dep/2` as thread-local predicates.
% These predicates are used to store intermediate state during the processing
% of PFC information to avoid redundant operations within a thread.
:- thread_local t_l:shown_child/1.
:- thread_local t_l:shown_dep/2.

%!  pfc_info(+X) is det.
%
%   Collects and displays information about the given predicate or term `X`.
%   It uses `mpred_test_why/1` and `mpred_child_info/1` to collect reasoning
%   and dependency information about `X`.
%
%   @arg X The predicate or term for which information is collected.
%
%   @example
%     % Collect and display information about a fact.
%     ?- pfc_info(foo(a)).
%
%     % Collect and display information about a rule.
%     ?- pfc_info((foo(X) :- bar(X))).
%
pfc_info(X) :-
    % Delegate to `mpred_info/1`.
    mpred_info(X).

%!  mpred_info(+X) is det.
%
%   Collects and displays all reasoning and child dependency information
%   related to the given term or goal `X`. This predicate ensures that stale
%   information is cleared before gathering fresh data, and it uses thread-local
%   state to avoid redundant processing.
%
%   @arg X The term or goal for which information is collected.
%
%   @example
%     % Collect and display information about the term `likes(john, pizza)`.
%     ?- mpred_info(likes(john, pizza)).
%
mpred_info(X) :-
    % Clear previous state to ensure no stale information is shown.
    retractall(t_l:shown_child(_)),
    retractall(t_l:shown_dep(_, _)),
    % Ignore failures while collecting reasoning and child information.
    ignore((
        % Collect and process all reasoning information for `X`.
        forall(mpred_test_why(X), true),
        % Collect and process all child information for `X`.
        forall(mpred_child_info(X), true)
    )).

%!  mpred_child_info(+P) is det.
%
%   Collects and displays child information for a given predicate or term `P`.
%
%   @arg P The predicate or term for which child information is collected.
%
%   @example
%     % Display child information for a term.
%     ?- mpred_child_info(foo(a)).
%
mpred_child_info(P) :-
    % Clear previous child state.
    retractall(t_l:shown_child(_)),
    % Display the child information for `P`.
    show_child_info(P), !,
    % Print a separator line.
    printLine.

%!  show_child_info(+P) is det.
%
%   Gathers and displays the children of predicate `P`.
%
%   @arg P The predicate for which child information is gathered.
%
show_child_info(P) :-
    % Retrieve all children of `P` into a list `L`.
    pfcChildren(P, L),
    % Display child information for `P` using the collected list `L`.
    show_child_info(P, L), !.
% Skip if this child has already been shown.
show_child_info(P, _) :-
    t_l:shown_child(Q), P =@= Q, !.
% Mark the child as shown to prevent redundant displays.
show_child_info(P, _) :-
    asserta(t_l:shown_child(P)), fail.
% Stop if there are no more children to display.
show_child_info(_, []) :- !.
% Display the children of `P` as a formatted set.
show_child_info(P, L) :-
    % Convert the list to a set to remove duplicates.
    list_to_set(L, S),
    % Print the header with the parent term.
    format("~N~nChildren for ", []),
    ansi_format([fg(green)], '~@', [pp(P)]),
    format(" :~n", []),
    % Print each child in yellow, and track dependencies to avoid repeats.
    forall(
        (member(D, S), \+ t_l:shown_dep(P, D)),
        (
            asserta(t_l:shown_dep(P, D)),
            ansi_format([fg(yellow)], '~N ~@. ~n', [pp(D)])
        )
    ),
    % Recursively show child information for all children.
    my_maplist(show_child_info, S).

%!  mpred_why(+X) is det.
%
%   Displays reasoning information for the given goal or term `X`.
%
%   @arg X The goal or term for which reasoning information is displayed.
%
%   @example
%     % Display reasoning information for a fact.
%     ?- mpred_why(foo(a)).
%
%     % Display reasoning information for a goal.
%     ?- mpred_why(likes(john, pizza)).
%
mpred_why(X) :-
    mpred_test_why(X).

%!  mpred_test_why(+X) is det.
%
%   Tests and displays the reasoning or truth value of the goal or term `X`.
%
%   @arg X The goal or term to test and display reasoning for.
%
%   @example
%     % Test and display reasoning for a goal.
%     ?- mpred_test_why(member(X, [1, 2, 3])).
%     X = 1 ;
%     X = 2 ;
%     X = 3.
%
mpred_test_why(X) :-
    % Call the goal using `pfcCallSystem/1`. If it succeeds, apply `pfcTF1/1`.
    pfcCallSystem(X) *-> pfcTF1(X)
    % If the goal fails, still apply `pfcTF1/1` but fail afterwards.
    ; (pfcTF1(X), !, fail).

%!  mpred_literal(+X) is nondet.
%
%   Checks if X is a literal in the PFC (Prolog Forward Chaining) system.
%
%   This predicate wraps the `pfcLiteral/1` predicate to determine if X
%   qualifies as a literal.
%
%   @arg X The term to be checked as a literal.
%
%   @example
%     ?- mpred_literal(literal_example).
%     true.
%
mpred_literal(X) :-
    pfcLiteral(X).

%!  mpred_positive_literal(+X) is nondet.
%
%   Checks if X is a positive literal in the PFC system.
%
%   This predicate succeeds if the given term X represents a positive literal,
%   using the underlying `pfcPositiveLiteral/1`.
%
%   @arg X The term to be checked as a positive literal.
%
%   @example
%     ?- mpred_positive_literal(positive_example).
%     true.
%
mpred_positive_literal(X) :-
    pfcPositiveLiteral(X).

%!  pfcAtom(+X) is nondet.
%
%   Determines whether X is an atom in the PFC system.
%
%   This predicate behaves similarly to `mpred_literal/1` since all literals
%   are treated as atoms within the PFC context.
%
%   @arg X The term to be checked as an atom.
%
%   @example
%     ?- pfcAtom(atom_example).
%     true.
%
pfcAtom(X) :-
    pfcLiteral(X).

%!  rem(+X) is det.
%
%   Withdraws a literal X from the PFC knowledge base.
%
%   This predicate removes the given literal X by invoking the
%   `pfcWithdraw/1` operation.
%
%   @arg X The literal to be withdrawn from the knowledge base.
%
%   @example
%     ?- rem(withdraw_example).
%     true.
%
rem(X) :-
    pfcWithdraw(X).

%!  rem2(+X) is det.
%
%   Removes a literal X using a secondary removal mechanism.
%
%   This predicate is an alternative to `rem/1`, utilizing the
%   `pfcRemove/1` operation to delete the literal X.
%
%   @arg X The literal to be removed.
%
%   @example
%     ?- rem2(secondary_remove_example).
%     true.
%
rem2(X) :-
    pfcRemove(X).

%!  remove(+X) is det.
%
%   Blasts (forcibly removes) a literal X from the PFC knowledge base.
%
%   This predicate performs a more aggressive removal using the
%   `pfcBlast/1` operation, which ensures that the literal X
%   is completely deleted.
%
%   @arg X The literal to be forcibly removed.
%
%   @example
%     ?- remove(blast_example).
%     true.
%
remove(X) :-
    pfcBlast(X).

% :- mpred_ain_in_thread.
% :- current_thread_pool(ain_pool)->true;thread_pool_create(ain_pool,20,[]).

% create_pool/1 can be defined across multiple files or modules and modified at runtime.
:- multifile thread_pool:create_pool/1.
:- dynamic thread_pool:create_pool/1.

%!  thread_pool:create_pool(+Pool) is det.
%
%   Creates a thread pool named `ain_pool` with 50 detached threads.
%   Detached threads automatically reclaim their resources upon termination,
%   making them suitable for tasks that do not require the parent to wait or
%   explicitly manage the thread's life cycle.
%
%   @arg Pool The pool name to be created, here `ain_pool`.
%
%   @example
%     % Create a pool named 'ain_pool' with 50 detached threads.
%     ?- thread_pool:create_pool(ain_pool).
%
thread_pool:create_pool(ain_pool) :-
    % Create the thread pool with 50 threads and detached mode enabled.
    thread_pool_create(ain_pool, 50, [detached(true)]).

% Imports the HTTP server library to handle HTTP requests in a multithreaded environment.
:- use_module(library(http/thread_httpd)).
% Provides predicates to manage and create thread pools for concurrent task execution.
:- use_module(library(thread_pool)).

%!  is_ain_pool_empty is nondet.
%
%   Checks if the `ain_pool` thread pool is currently empty.
%   The predicate succeeds if no threads are running in the pool.
%
%   @example
%     ?- is_ain_pool_empty.
%     true.
%
is_ain_pool_empty :-
    % Query the number of running threads in `ain_pool`.
    thread_pool_property(ain_pool, running(N)),
    % Succeed if there are no running threads.
    !, N == 0.
% Default case: Assume the pool is not empty.
is_ain_pool_empty.

%!  show_ain_pool is det.
%
%   Displays the properties of the `ain_pool` thread pool by printing
%   each property using the `fmt/1` predicate.
%
%   @example
%     ?- show_ain_pool.
%     % Displays the properties of the `ain_pool`.
%
show_ain_pool :-
    % Iterate over all properties of `ain_pool` and print them.
    forall(thread_pool_property(ain_pool, PP), fmt(show_ain_pool(PP))).

%!  await_ain_pool is det.
%
%   Waits until the `ain_pool` is empty. If the pool is not empty,
%   it repeatedly checks every 5 milliseconds until it becomes empty.
%
%   @example
%     ?- await_ain_pool.
%     true.
%
await_ain_pool :-
    % If the pool is empty, succeed immediately.
    is_ain_pool_empty -> true ;
    % Otherwise, repeat until the pool becomes empty.
    (repeat, sleep(0.005), is_ain_pool_empty).

%!  ain_in_thread(+Goal) is det.
%
%   Runs a given goal `AIN` within the context of the `ain_pool`.
%   The goal is extracted from its module and scheduled for execution.
%
%   @arg Goal The goal to execute in a thread within the `ain_pool`.
%
%   @example
%     ?- ain_in_thread(my_module:my_goal).
%     true.
%
ain_in_thread(MAIN) :-
    % Extract the module and goal from the input.
    strip_module(MAIN, M, AIN),
    % Schedule the goal for execution in the thread pool.
    call_in_thread(M:pfcAdd(AIN)).

%!  call_in_thread(+Goal) is det.
%
%   Schedules a goal `G` for execution within the `ain_pool` thread pool.
%   It creates a copy of the goal, assigns variable numbers, and converts
%   it to an atom representation for tracking.
%
%   @arg Goal The goal to be executed in the thread pool.
%
%   @example
%     ?- call_in_thread(my_goal).
%     true.
%
call_in_thread(MG) :-
    % Extract the module and goal from the input.
    strip_module(MG, M, G),
    % Create a copy of the goal and convert it to an atom for tracking.
    notrace((
        copy_term(M:G, GG, _),
        numbervars(GG, 0, _, [attvar(skip), singletons(true)]),
        term_to_atom(GG, TN)
    )),
    % Schedule the converted goal for execution.
    call_in_thread(TN, M, G),
    % Log the goal for tracking purposes.
    dmsg_pretty(call_in_thread(TN, M, G)).

%!  call_in_thread(+ThreadName, +Module, +Goal) is det.
%
%   Creates a new thread in the `ain_pool` with the given alias `TN`.
%   If a thread with the same alias is already running, it logs the event.
%   Otherwise, a new thread is created to execute the goal.
%
%   @arg ThreadName The alias of the thread.
%   @arg Module The module where the goal is defined.
%   @arg Goal The goal to be executed within the thread.
%
%   @example
%     ?- call_in_thread('goal_thread', my_module, some_goal).
%     true.
%
call_in_thread(TN, M, G) :-
    % Check if a thread with the same alias is already running.
    thread_property(_, alias(TN)),
    % If found, log the event and prevent duplicate scheduling.
    !, dmsg_pretty(already_queued(M, G)).
% If no thread with the same alias exists, create a new one.
call_in_thread(TN, M, G) :-
    % Retrieve the reason for the current operation.
    must_ex(current_why(Why)),
    % Create a new thread in the `ain_pool` with the specified alias.
    thread_create_in_pool(
        ain_pool,
        call_in_thread_code(M, G, Why, TN),
        _Id,
        [alias(TN)]
    ).

%!  call_in_thread_code(+Module, +Goal, +Why, +ThreadName) is det.
%
%   Executes the given goal `G` within a thread. It catches and logs
%   any exceptions that occur during execution, and logs whether the
%   goal succeeds or fails.
%
%   @arg Module The module where the goal resides.
%   @arg Goal The goal to be executed within the thread.
%   @arg Why The reason/context for executing this goal.
%   @arg ThreadName The alias of the thread executing the goal.
%
%   @example
%     ?- call_in_thread_code(my_module, some_goal, reason, 'goal_thread').
%     true.
%
call_in_thread_code(M, G, Why, TN) :-
    % Set the current context using the given reason.
    with_only_current_why(
        Why,
        % Attempt to execute the goal and log the outcome.
        catch(
            % If the goal succeeds, log the success.
            ( M:G -> nop(dmsg_pretty(suceeded(exit, TN)))
            ; % If the goal fails, log the failure.
              dmsg_pretty(failed(exit, TN))
            ),
            % Catch and log any exceptions that occur.
            E,
            dmsg_pretty(error(E --> TN))
        )
    ).
%:- call_in_thread(fbugio(call_in_thread)).

% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

%   File   : pfc
%   Author : Tim Finin, finin@umbc.edu
%   Updated: 10/11/87, ...
%   Purpose: consult system file for ensure

% declares the current version which helps with compatibility and version tracking.
pfcVersion(3.0).

/*
pfcFile('pfcsyntax').   % operator declarations.
pfcFile('pfccore'). % core of Pfc.
pfcFile('pfcsupport').  % support maintenance
pfcFile('pfcdb').   % predicates to manipulate database.
pfcFile('pfcdebug').    % debugging aids (e.g. tracing).
pfcFile('pfcjust'). % predicates to manipulate justifications.
pfcFile('pfcwhy').  % interactive exploration of justifications.

pfcLoad :- pfcFile(F), ensure_loaded(F), fail.
pfcLoad.
*/

%pfcFcompile :- pfcFile(F), compile(F), fail.
%pfcFcompile.

%:- pfcLoad.

%   File   : pfccompile.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%   Purpose: compile system file for Pfc
/*
:- compile(pfcsyntax).
:- compile(pfccore).
:- compile(pfcdb).
:- compile(pfcjust).
:- compile(pfcwhy).
:- compile(pfcdebug).
*/

%   File   : pfcsyntax.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Purpose: syntactic sugar for Pfc - operator definitions and term expansions.

:- op(500, fx, '~').           % Declares '~' as a prefix operator with precedence 500.
:- op(1050, xfx, ('==>')).       % Declares '==>' as an infix operator with precedence 1050.
:- op(1050, xfx, '<==>').      % Declares '<==>' as an infix operator with precedence 1050.
:- op(1050, xfx, ('<-')).        % Declares '<-' as an infix operator with precedence 1050.
:- op(1100, fx, ('==>')).        % Declares '==>' as a prefix operator with precedence 1100.
:- op(1150, xfx, ('::::')).      % Declares '::::' as an infix operator with precedence 1150.

% declare that pfctmp:knows_will_table_as/2 can be modified at runtime.
:- dynamic(pfctmp:knows_will_table_as/2).

%!  will_table_as(+Stuff, +As) is nondet.
%
%   Determines whether a specific `Stuff` is known to be tabled as `As`.
%   If not, it asserts the knowledge and triggers the tabling reaction.
%
%   @arg Stuff The predicate or term to be tabled.
%   @arg As    The way in which the `Stuff` is tabled (i.e., the tabling behavior).
%
%   @example
%     % Check or assert that some_predicate is tabled as memoization.
%     ?- will_table_as(some_predicate, memoization).
%     true.
%
will_table_as(Stuff, As) :-
    % If the relationship is already known, succeed.
    pfctmp:knows_will_table_as(Stuff, As),
    !.
will_table_as(Stuff, As) :-
    % Assert the knowledge that `Stuff` will be tabled as `As`.
    assert(pfctmp:knows_will_table_as(Stuff, As)),
    % Ensure the tabling behavior is reacted to.
    must_ex(react_tabling(Stuff, As)),
    % Force backtracking to prevent further solutions.
    !, fail.

%!  react_tabling(+Stuff, +As) is det.
%
%   Reacts to the tabling operation by marking the `Stuff` predicate as dynamic.
%   This is needed for predicates that are altered or generated at runtime.
%
%   @arg Stuff The predicate or term to be marked as dynamic.
%   @arg As    The tabling behavior (not used in this clause).
%
%   @example
%     % React to the tabling of some_predicate.
%     ?- react_tabling(some_predicate, _).
%     true.
%
react_tabling(Stuff, _) :-
    % Declare the predicate as dynamic so it can be modified at runtime.
    dynamic(Stuff).

% declare predicates that can be modified at runtime.
:- dynamic(lmconf:is_treated_like_pfc_file/1).
:- dynamic(lmconf:is_pfc_module/1).

%!  if_pfc_indicated is nondet.
%
%   Determines if the current source file or module is treated as a PFC (Prolog Forward Chaining) source.
%   It checks if the file has a `.pfc` extension or is marked to behave like a PFC file.
%
%   @example
%     % If the file or module is indicated as PFC, the predicate succeeds.
%     ?- if_pfc_indicated.
%     true.
%
if_pfc_indicated :-
    % Check if the source file has a `.pfc` extension or is treated as a PFC file.
    source_location(F, _),
    (sub_string(F, _, _, _, '.pfc') -> true ; lmconf:is_treated_like_pfc_file(F)),
    !.
if_pfc_indicated :-
    % Alternatively, check if the current module is marked as a PFC module.
    prolog_load_context(module, M),
    lmconf:is_pfc_module(M),
    !.

%!  skip_pfc_term_expansion(+Term) is nondet.
%
%   Determines if the given term should be excluded from PFC term expansion.
%   It skips terms like variables or special markers such as `begin_of_file` or `end_of_file`.
%
%   @arg Term The term to check for exclusion from term expansion.
%
%   @example
%     ?- skip_pfc_term_expansion(begin_of_file).
%     true.
%     ?- skip_pfc_term_expansion(foo).
%     false.
%
skip_pfc_term_expansion(Var) :-
    % Skip expansion if the term is a variable.
    var(Var),
    !.
skip_pfc_term_expansion(begin_of_file).  % Skip the `begin_of_file` marker.
skip_pfc_term_expansion(end_of_file).    % Skip the `end_of_file` marker.

% Export `pfc_term_expansion/2` to make it available for use by other modules.
:- export(pfc_term_expansion/2).
% Import `pfc_term_expansion/2` into the `system` namespace to override or extend built-in behavior.
:- system:import(pfc_term_expansion/2).

%!  pfc_term_expansion(+Input, -Output) is det.
%
%   Expands PFC-specific terms into Prolog directives or rules.
%   This handles constructs such as table declarations, `==>`, `<==>`, `<-`, and named rules.
%
%   @arg Input  The original term to be expanded.
%   @arg Output The expanded term or directive.
%
%   @example
%     % Expand a table declaration into PFC-aware directives.
%     ?- pfc_term_expansion((:- table foo as memo), Expanded).
%     Expanded = [:- pfcAdd(tabled_as(foo, memo)), (:- table foo as memo)].
%
%     % Expand a rule using the `==>` operator.
%     ?- pfc_term_expansion((a ==> b), Expanded).
%     Expanded = (:- pfcAdd((a ==> b))).
%
pfc_term_expansion(I, O) :-
    % If the term is to be skipped, unify the input and output.
    skip_pfc_term_expansion(I),
    !, I = O.
pfc_term_expansion((:- table Stuff as Type),
                   [:- pfcAdd(tabled_as(Stuff, Type)),
                    (:- table Stuff as Type)]) :-
    % Handle table declarations with specific types.
    nonvar(Stuff),
    !,
    if_pfc_indicated,
    \+ will_table_as(Stuff, Type).
pfc_term_expansion((:- table Stuff),
                   [:- pfcAdd(tabled_as(Stuff, incremental)),
                    (:- table Stuff as incremental)]) :-
    % Handle table declarations with the default `incremental` type.
    if_pfc_indicated,
    \+ will_table_as(Stuff, incremental).
pfc_term_expansion((:- _), _) :-
    % Ignore directives that are not explicitly handled.
    !, fail.
pfc_term_expansion((P ==> Q), (:- pfcAdd((P ==> Q)))).
% term_expansion((P ==> Q), (:- pfcAdd(('<-'(Q, P))))) :-  % Speed-up attempt (commented out).
pfc_term_expansion(('<-'(P, Q)), (:- pfcAdd(('<-'(P, Q))))).
pfc_term_expansion((P <==> Q), (:- pfcAdd((P <==> Q)))).
pfc_term_expansion((RuleName :::: Rule), (:- pfcAdd((RuleName :::: Rule)))).
pfc_term_expansion((==> P), (:- pfcAdd(P))).
pfc_term_expansion(I, I) :-
    % Preserve the `end_of_file` marker unchanged.
    I == end_of_file,
    !.
pfc_term_expansion(P, (:- pfcAdd(P))) :-
    % For all other terms, if PFC is indicated, wrap the term in `pfcAdd/1`.
    if_pfc_indicated.

%use_pfc_term_expansion:- current_prolog_flag(pfc_term_expansion,false),!,fail.
% maybe switch to prolog_load_context(file,...)?
%use_pfc_term_expansion:- source_location(File,_), atom_concat(_,'.pfc.pl',File).

%!  term_subst(+SubstType, -Output) is det.
%
%   Performs a predefined substitution on a term based on the given substitution type.
%   It handles different substitution types such as `clause` or `tilded_negation`.
%
%   @arg SubstType The substitution type to be applied (e.g., `clause` or `tilded_negation`).
%   @arg Output    The resulting term after substitution.
%
%   @example
%     % Apply the `clause` substitution type.
%     ?- term_subst(clause, Result).
%     Result = clause.
%
term_subst(P, O) :-
    % Shortcut for the `clause` substitution type.
    term_subst(clause, P, O),
    !.

%!  term_subst(+SubstType, +Input, -Output) is det.
%
%   Substitutes terms recursively based on the specified substitution type.
%   Handles both non-compound and compound terms, applying substitutions to functors and arguments.
%
%   @arg SubstType The substitution type (e.g., `clause`, `tilded_negation`).
%   @arg Input     The input term to be transformed.
%   @arg Output    The transformed term after applying substitutions.
%
%   @example
%     % Apply the `tilded_negation` substitution to replace logical operators.
%     ?- term_subst(tilded_negation, (not(a) => b), Result).
%     Result = (~a ==> b).
%
%     % Apply a generic substitution to a simple term.
%     ?- term_subst(_, foo, Result).
%     Result = foo.
%
term_subst(_, P, O) :-
    % If the input is not a compound term, return it unchanged.
    \+ compound(P),
    !,
    O = P.
term_subst(tilded_negation, P, O) :-
    % Handle substitution specific to negation and logical operators.
    !,
    term_subst(
        [(not) - (~),
         (=>)  - (==>),
         (<=>) - (<==>),
         (<=)  - (<-)],
        P, O
    ).
term_subst(Subst, P, O) :-
    % Decompose the compound term into functor and arguments.
    compound_name_arguments(P, F, Args),
    % Recursively apply substitutions to all arguments.
    my_maplist(term_subst(Subst), Args, ArgsL),
    % Substitute the functor if needed.
    termf_subst(Subst, F, F2),
    % Rebuild the compound term with the new functor and transformed arguments.
    compound_name_arguments(O, F2, ArgsL).

%!  termf_subst(+Subst, +Functor, -NewFunctor) is det.
%
%   Substitutes a functor based on a list of substitutions.
%   If no matching substitution is found, the original functor remains unchanged.
%
%   @arg Subst      The list of substitutions in the form `OldFunctor - NewFunctor`.
%   @arg Functor    The original functor to be substituted.
%   @arg NewFunctor The resulting functor after substitution.
%
%   @example
%     % Substitute the functor `not` with `~`.
%     ?- termf_subst([(not) - (~)], not, Result).
%     Result = ~.
%
%     % Use a functor without a matching substitution.
%     ?- termf_subst([(not) - (~)], foo, Result).
%     Result = foo.
%
termf_subst(Subst, F, F2) :-
    % Check if a substitution exists for the functor.
    member(F - F2, Subst) -> true ;
    % If no substitution is found, keep the original functor.
    F = F2.

%   File   : pfccore.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated: 10/11/87, ...
%            4/2/91 by R. McEntire: added calls to valid_dbref as a
%                                   workaround for the Quintus 3.1
%                                   bug in the recorded database.
%   Purpose: core Pfc predicates.

:- use_module(library(lists)).

%==>(_).

% ==>(G):- arc_assert(G).

% 'dynamic' declares that a predicate's clauses can be modified at runtime,
% allowing new facts or rules to be added or existing ones to be removed during execution.

%:- multifile ('<-')/2.
%:- dynamic ('<-')/2.
%:- discontiguous(('<-')/2).
%'<-'(_,_).

%:- multifile ('==>')/2.
%:- dynamic ('==>')/2.
%:- discontiguous(('==>')/2).
%'==>'(_,_).

%:- multifile ('==>')/2.
%:- dynamic ('::::')/2.
%:- dynamic '<==>'/2.
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

:- thread_local(t_l:pfcSearchTL/1).

:- dynamic '$spft$'/3.

%!  pfcSetVal(+Stuff) is det.
%
%   Sets a global value by asserting the provided term after ensuring no
%   existing matching term is present. It duplicates the input term, unifies
%   the last argument with a variable, and then asserts it to the knowledge base.
%
%   This predicate ensures that only one version of a term with the same structure
%   is present at a time, retracting any matching terms before the new assertion.
%
%   @arg Stuff The term to be asserted as a global value.
%
%   @example
%     % Set a global value by asserting it.
%     ?- pfcSetVal(my_global_term(1)).
%
%     % Example of asserting another term, overriding any matching one.
%     ?- pfcSetVal(config_param(42)).
%
%   @see retractall/1, assert/1
%
% Duplicate the term to manipulate without side effects.
pfcSetVal(Stuff) :-
   duplicate_term(Stuff, DStuff),
   % Retrieve the arity (N) of the term.
   functor(DStuff, _, N),
   % Make the last argument unbound.
   setarg(N, DStuff, _),
   % Remove any existing matching terms.
   retractall(DStuff),
   % Assert the new term.
   assert(Stuff).

%!  pfcDefault(+GeneralTerm, +Default) is det.
%
%   Ensures that a default value is asserted only if no existing fact unifies
%   with the provided general term. This is useful for initializing global
%   assertions with fallback values.
%
%   @arg GeneralTerm The term to be checked for existence.
%   @arg Default     The term to be asserted if `GeneralTerm` is absent.
%
%   @example
%     % Ensure a default mode is set if none exists.
%     ?- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).
%
%     % Set a default search strategy if not already defined.
%     ?- pfcDefault(pfcSearch(_), pfcSearch(direct)).
%
% Check if the general term exists; if so, do nothing.
pfcDefault(GeneralTerm, Default) :-
   clause(GeneralTerm, true) -> true
   % Otherwise, assert the default term.
   ; assert(Default).

%   fcTmsMode/1 is det.
%
%   Controls the Truth Maintenance System (TMS) algorithm by setting its mode.
%   The mode can be one of the following:
%     - `none`: Disables the TMS algorithm.
%     - `local`: Uses a local version of the TMS.
%     - `cycles`: Uses a cycle-based TMS algorithm.
%
%   @example
%     % Set the default TMS mode to 'cycles' if it isn't defined.
%     ?- fcTmsMode(cycles).
%
% Ensure the default TMS mode is 'cycles' if not defined.
:- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).

%   pfcSearch/1 is det.
%
%   Specifies the search strategy used by the PFC (Prolog Forward Chaining)
%   system. The strategy can be one of:
%     - `direct`: Executes search directly without expanding paths.
%     - `depth`: Uses depth-first search.
%     - `breadth`: Uses breadth-first search.
%
%   @example
%     % Set the default search strategy to 'direct' if not defined.
%     ?- pfcSearch(direct).
%
% Ensure the default search strategy is 'direct' if not defined.
:- pfcDefault(pfcSearch(_), pfcSearch(direct)).

%

%!  pfcAdd(+P) is det.
%
%   Adds a new clause or fact `P` to the database. This version retrieves the
%   current context (support) using `current_why_UU/1` and passes it to
%   `pfcAdd/2` for further processing.
%
%   @arg P  The clause or fact to be added to the database.
%
%   @example
%     % Add a fact using the current context.
%     ?- pfcAdd(my_fact).
%
pfcAdd(P) :-
    % Retrieve the current support context.
    must_ex(current_why_UU(UU)),
    % Add the clause or fact with the retrieved context as support.
    pfcAdd(P, UU).

% % Alternative version using with_current_why (commented out):
% % pfcAdd(P) :- must_ex(current_why_UU(UU)),
% %              with_current_why(pfcAdd(P), pfcAdd(P, UU)).

%!  pfcAdd(+P, +S) is det.
%
%   Adds a clause or fact `P` to the database with the given support `S`.
%   If the term is in the form `(==>P)`, it strips the implication head and adds
%   only the body `P` with the specified support. After adding the clause,
%   forward reasoning is initiated.
%
%   @arg P  The clause or fact to be added.
%   @arg S  The support context or reason for adding the clause.
%
%   @example
%     % Add a fact with explicit support.
%     ?- pfcAdd(my_fact, some_reason).
%
%     % Add an implication, storing only the body.
%     ?- pfcAdd((==>my_fact), some_reason).
%
pfcAdd((==>P), S) :-
    % If the term is an implication (==>P), add only the body `P` with support `S`.
    !, pfcAdd(P, S).
pfcAdd(P, S) :-
    % Add the clause with the given support and initiate forward reasoning.
    pfcPost(P, S),pfcRun, !.
% % Fallback for debugging purposes (commented out):
% % pfcAdd(_, _).
pfcAdd(P, S) :-
    % Log a warning if the addition of the clause fails.
    pfcWarn("pfcAdd(~p, ~p) failed", [P, S]).

%!  pfcPost(+Ps, +S) is det.
%
%   Attempts to add a fact or a set of facts into the database. For each fact
%   or singleton, `pfcPost1/2` is called to handle the addition and queue processing.
%
%   @arg Ps  A single fact or a list of facts to be posted.
%   @arg S   The supporting context or reason for posting the facts.
%
%   @example
%     % Post a fact or a set of facts with context.
%     ?- pfcPost([fact1, fact2], some_reason).
%
% Post facts by reversing the list and handling each one.
pfcPost(List, S) :-
   pfcPost_rev(S, List).
% Handle a list of terms or a singleton by recursively posting each element.
pfcPost_rev(S, Term) :-
   is_list(Term)
   -> my_maplist(pfcPost_rev(S), Term)
   ; pfcPost1(Term, S).

%!  pfcPost1(+P, +S) is det.
%
%   Tries to add a fact to the database and, if successful, queues it for
%   subsequent forward chaining. This predicate always succeeds, even if
%   the addition encounters issues.
%
%   @arg P  The fact to be added to the database.
%   @arg S  The supporting context or reason for the fact.
%
%   @example
%     % Post a single fact with support.
%     ?- pfcPost1(my_fact, some_reason).
%
% Check and fix argument types before posting.
pfcPost1(Fact, S) :-
   control_arg_types(Fact, Fixed),
   !, pfcPost1(Fixed, S).
% Handle exceptions during posting and enforce occurs check.
pfcPost1(P, S) :-
   locally(set_prolog_flag(occurs_check, true),
      catch(pfcPost11(P, S), E, (notrace, write_src_uo(P => E), trace))).

%!  pfcPost11(+P, +S) is det.
%
%   Adds support for the given fact and handles uniqueness checks before posting
%   the fact to the database. This predicate ensures that the fact is properly
%   supported, and if uniqueness is required, it only posts unique facts.
%
%   If the fact is already present and marked as unique, it will skip posting and
%   log a warning. This predicate ensures that the proper forward reasoning setup
%   is completed for each addition.
%
%   @arg P  The fact to be added to the database.
%   @arg S  The supporting context or reason for the fact.
%
%   @example
%     % Add a unique fact with support, if not already present.
%     ?- pfcPost11(my_fact, reason).
%
%   @see pfcAddSupport/2, pfcPost2/2, pfcUnique/2, nop/1.
%
% Add support and handle uniqueness checks before posting.
pfcPost11(P, S) :-
   % %  db pfcAddDbToHead(P, P2),  % Prepare the term for the database head (commented out).
   % pfcRemoveOldVersion(P),       % Remove any old version of the term (commented out).
   must_ex(pfcAddSupport(P, S)),   % Ensure the fact is added with proper support.
   (pfcUnique(post, P)             % Check if the fact is unique.
   -> pfcPost2(P, S)               % If unique, proceed with posting.
   ; nop(pfcWarn(not_pfcUnique(post, P)))).  % Otherwise, log a warning.

%!  pfcPost2(+P, +S) is det.
%
%   Posts the given fact `P` to the database and ensures it is enqueued for
%   forward reasoning. If the fact `P` is not already asserted, it will be
%   asserted, and all relevant tracing and enqueuing steps will be performed.
%   This predicate ensures that the reasoning process proceeds smoothly by
%   tracing and queuing the fact appropriately.
%
%   @arg P  The fact to be added or checked in the database.
%   @arg S  The supporting context or reason for adding the fact.
%
%   @example
%     % Post a new fact and ensure it is traced and enqueued for reasoning.
%     ?- pfcPost2(my_fact, reason).
%
%   @see is_asserted_exact/1, pfcTraceAdd/2, pfcEnqueue/2.
%
% Check if the fact is already asserted; if not, assert it.
pfcPost2(P, S) :-
   must_ex(once(\+ \+ is_asserted_exact(P); assert(P))),
   % Trace the addition of the fact with the given support.
   must_ex(pfcTraceAdd(P, S)),
   !,  % Commit the tracing step.
   % Enqueue the fact for further processing in forward reasoning.
   must_ex(pfcEnqueue(P, S)),
   !.  % Commit the enqueuing step.

%!  is_asserted_exact(+MH, -B) is nondet.
%
%   Checks if a given clause with the module-qualified head `MH` and body `B`
%   is exactly asserted in the database. This ensures that the clause exists
%   with matching head and body, considering the originating module.
%
%   @arg MH  The module-qualified head of the clause (e.g., `module:head`).
%   @arg B   The body of the clause to be checked.
%
%   @example
%     % Check if a clause with a specific head exists and get its body.
%     ?- is_asserted_exact(user:my_clause(X), Body).
%
%   @see strip_module/3, clause/3.
%
is_asserted_exact(MH, B) :-
   % Extract the module and head from the term.
   strip_module(MH, M, H),
   is_asserted_exact(M, H, B).

%!  is_asserted_exact(+MHB) is nondet.
%
%   Checks if a given module-qualified head-body term `MHB` is exactly
%   asserted in the database. This version expands the input term into
%   its head and body components before performing the assertion check.
%
%   @arg MHB  A module-qualified term in the form `module:(head-body)`.
%
%   @example
%     % Check if a module-qualified head-body term is asserted.
%     ?- is_asserted_exact(user:(my_clause :- true)).
%
%   @see strip_module/3, expand_to_hb/3.
%
is_asserted_exact(MHB) :-
   % Extract the module and head-body term.
   strip_module(MHB, M, HB),
   % Expand to separate head and body components.
   expand_to_hb(HB, H, B),
   is_asserted_exact(M, H, B).

%!  is_asserted_exact(+M, +H, -B) is nondet.
%
%   Checks if a clause with the specified module `M`, head `H`, and body `B`
%   is exactly asserted in the database. This predicate performs a detailed
%   check considering the clause's module properties.
%
%   @arg M  The module where the clause is asserted.
%   @arg H  The head of the clause.
%   @arg B  The body of the clause.
%
%   @example
%     % Verify if a clause is asserted with a specific head and body.
%     ?- is_asserted_exact(user, my_clause(X), Body).
%
%   @see clause/3, clause_property/2.
%
is_asserted_exact(M, H, B) :-
   M = MM,  % Ensure module names match.
   % Search for clause, trying both module forms.
   (MM:clause(M:H, B, Ref) *-> true ; M:clause(MM:H, B, Ref)),
   % Verify that the clause's module matches.
   clause_property(Ref, module(MM)),
   % Continue with full clause check.
   is_asserted_exact(MM, H, B, Ref).

%!  is_asserted_exact(+M, +H, +B, +Ref) is nondet.
%
%   Verifies the exact match of a clause's content by comparing the heads
%   and bodies using the clause reference `Ref`. This ensures that the
%   retrieved clause corresponds precisely to the given head and body.
%
%   @arg M    The module where the clause is asserted.
%   @arg H    The head of the clause.
%   @arg B    The body of the clause.
%   @arg Ref  The reference to the clause in the database.
%
%   @example
%     % Check the exact match of a clause using a reference.
%     ?- is_asserted_exact(user, my_clause(X), true, Ref).
%
%   @see clause/3, strip_m/2, =@=/2.
%
is_asserted_exact(_, H, B, Ref) :-
   % Retrieve the clause by reference.
   clause(CH, CB, Ref),
   % Strip the module from the head.
   strip_m(CH, HH),
   % Ensure the heads are exactly equal.
   HH =@= H,
   % Strip the module from the body.
   strip_m(CB, BB),
   % Ensure head-body pairs are exactly equal.
   cl(HH, BB) =@= cl(H, B).


%pfcPost1(_,_).
%pfcPost1(P,S) :-
 %pfcWarn("pfcPost1: ~p\n (support: ~p) failed",[P,S]).

% %   pfcAddDbToHead(+P,-NewP) is nondet.
% talkes a fact P or a conditioned fact
% (P:-C) and adds the Db context.
%

%!  pfcAddDbToHead(+P, -NewP) is det.
%
%   Modifies the given clause `P` by adding the current PFC database context to
%   its head. If the current database (`Db`) is set to `true`, the clause remains
%   unchanged. Otherwise, the database context is prepended to the body of the clause.
%
%   This ensures that all clauses interact with the appropriate database context.
%
%   @arg P     The original clause to be modified.
%   @arg NewP  The new clause with the database context added, if applicable.
%
%   @example
%     % Add database context to a clause with a head and body.
%     ?- pfcAddDbToHead((my_head :- my_body), NewP).
%     NewP = (my_head :- (true, my_body)).
%
%   @see pfcCallSystem/1.
%
pfcAddDbToHead(P, NewP) :-
   % Retrieve the current PFC database context.
   pfcCallSystem(pfcCurrentDb(Db)),
   % If the database context is `true`, keep the clause unchanged.
   (Db = true        -> NewP = P;
    % If the clause has a head and body, prepend the database context to the body.
    P = (Head :- Body) -> NewP = (Head :- (Db, Body));
    % Otherwise, create a new clause with the database context as the body.
    true -> NewP = (P :- Db)).

%  allows dynamic changes to the database context, which can be queried or modified
:- dynamic(pfcCurrentDb/1).

% Set the default database context to `true`.
pfcCurrentDb(true).

%!  pfcUnique(+Type, +P) is det.
%
%   True if the given fact `P` is not asserted in the database. This predicate
%   treats a fact as a head-body pair with the body being `true`, ensuring its
%   uniqueness.
%
%   @arg Type   The type of uniqueness check (e.g., `post`).
%   @arg P      The fact to be checked for uniqueness.
%
%   @example
%     % Ensure that a fact is unique before asserting it.
%     ?- pfcUnique(post, my_fact).
%
%   @see pfcUnique/3, is_asserted_exact/2.
%
pfcUnique(Type,(Head:-Tail)) :- !,pfcUnique(Type,Head,Tail).
pfcUnique(Type, P) :- pfcUnique(Type,P,true).

%!  pfcUnique(+Type, +Head, +Tail) is det.
%
%   True if the given clause with the specified `Head` and `Tail` is not asserted
%   exactly in the database. This predicate ensures that a head-body pair is unique
%   before being added, preventing duplicate assertions.
%
%   @arg Type   The type of uniqueness check (e.g., `post`).
%   @arg Head   The head of the clause to be checked.
%   @arg Tail   The body of the clause to be checked.
%
%   @example
%     % Ensure that a clause with a specific head and body is unique before asserting it.
%     ?- pfcUnique(post, my_fact, true).
%
%   @see is_asserted_exact/2.
%
% Check if the clause with the given head and tail is not asserted exactly.
/*
% Alternative implementation (commented out) that checks if the clause is not asserted.
pfcUnique(post, Head, Tail) :-
   !, \+ is_clause_asserted(Head, Tail).
*/
pfcUnique(_, Head, Tail) :-
   \+ is_asserted_exact(Head, Tail), !.
/*
pfcUnique(_,H,B):- \+ is_asserted(H,B),!.
pfcUnique(_,H,B):- \+ (
    clause(H, B, Ref),
    clause(HH, BB, Ref),
    strip_m(HH, HHH),
    HHH=@=H,
    strip_m(BB, BBB),
    BBB=@=B).
*/

%!  pfcSetSearch(+Mode) is det.
%
%   Sets the current search mode for enqueuing clauses or facts. The search
%   mode is stored as a global value using `pfcSetVal/1`.
%
%   @arg Mode  The search mode to set (e.g., `direct`, `thread`, `depth`, `breadth`).
%
%   @example
%     % Set the search mode to `breadth`.
%     ?- pfcSetSearch(breadth).
%
pfcSetSearch(Mode) :-
   % Store the search mode as a global value.
   pfcSetVal(pfcSearch(Mode)).

%!  pfcGetSearch(-Mode) is det.
%
%   Retrieves the current search mode. If a thread-local search mode is set,
%   it takes precedence; otherwise, the global search mode is used.
%
%   @arg Mode  The current search mode in use (e.g., `direct`, `thread`).
%
%   @example
%     % Get the current search mode.
%     ?- pfcGetSearch(Mode).
%
pfcGetSearch(Mode) :-
   % Check for a thread-local search mode first; if absent, use the global mode.
   (t_l:pfcSearchTL(ModeT) -> true ; pfcSearch(ModeT)) -> Mode = ModeT.

%!  pfcEnqueue(+P, +S) is det.
%
%   Enqueues the clause or fact `P` with support `S` according to the current
%   search mode settings. The search mode determines how forward reasoning is
%   processed (e.g., using direct execution, threads, depth-first, or breadth-first queues).
%   If no valid search mode is found, a warning is issued, and the `direct` mode is used.
%
%   @arg P  The clause or fact to be enqueued.
%   @arg S  The supporting context or reason for enqueuing the clause or fact.
%
%   @example
%     % Enqueue a fact with the current search mode.
%     ?- pfcEnqueue(my_fact, some_reason).
%
%   @see pfcGetSearch/1, pfcEnqueue/3, pfcWarn/2.
%
pfcEnqueue(P, S) :-
   % Retrieve the current search mode and use it for enqueuing.
   pfcGetSearch(Mode), !,
   pfcEnqueue(Mode, P, S).
% If no valid search mode is set, log a warning and fall back to `direct` mode.
pfcEnqueue(P, S) :-
   pfcWarn("No pfcSearch mode"),
   pfcEnqueue(direct, P, S).

%!  pfcEnqueue(+Mode, +P, +S) is det.
%
%   Enqueues the clause or fact `P` with support `S` according to the specified search mode.
%   The available modes determine how the enqueuing and forward reasoning are processed:
%     - `direct`: Perform immediate forward reasoning.
%     - `thread`: Use threaded forward reasoning.
%     - `depth`: Add to the queue with `asserta/2` for depth-first processing.
%     - `breadth`: Add to the queue with `assert/2` for breadth-first processing.
%     - If an unrecognized mode is used, a warning is issued, and the fallback mode is `direct`.
%
%   @arg Mode  The search mode to be used (e.g., `direct`, `thread`, `depth`, `breadth`).
%   @arg P     The clause or fact to be enqueued.
%   @arg S     The supporting context or reason for enqueuing.
%
%   @example
%     % Enqueue a fact using `depth` mode.
%     ?- pfcEnqueue(depth, my_fact, some_reason).
%
%   @see pfcWarn/2, pfcFwd/1, pfcThreadFwd/2.
%
pfcEnqueue(Mode, P, S) :-
   % Handle `direct` mode with immediate forward reasoning.
   Mode = direct  -> pfcFwd(P) ;
   % Handle `thread` mode with threaded forward reasoning.
   Mode = thread  -> pfcThreadFwd(P, S) ;
   % Handle `depth` mode by adding to the queue with `asserta/2`.
   Mode = depth   -> pfcAsserta(pfcQueue(P), S) ;
   % Handle `breadth` mode by adding to the queue with `assert/2`.
   Mode = breadth -> pfcAssert(pfcQueue(P), S) ;
   % For unrecognized modes, log a warning and fall back to `direct` mode.
   true           -> pfcWarn("Unrecognized pfcSearch mode: ~p", Mode),
                     pfcEnqueue(direct, P, S).

%!  pfcRemoveOldVersion(+Rule) is det.
%
%   Removes an old version of a rule if it exists in the form
%   `Identifier ::: Rule`. If a rule with the same `Identifier` but a different
%   body is found, the old rule is withdrawn from the database.
%
%   @arg Rule  The rule to be checked and possibly removed.
%
%   @example
%     % Remove an old version of a rule with the same identifier.
%     ?- pfcRemoveOldVersion(my_identifier ::: some_body).
%
%   @see pfcWithdraw/1.
%
pfcRemoveOldVersion((Identifier :::: Body)) :-
   % This should never happen: warn if the identifier is a variable.
   (var(Identifier)
   -> pfcWarn("variable used as an rule name in ~p :::: ~p",
              [Identifier, Body])
   ;  % Proceed to remove the old version of the rule.
      pfcRemoveOldVersion0(Identifier :::: Body)).
% Helper predicate to remove the old version of a rule with a specific identifier.
pfcRemoveOldVersion0((Identifier :::: Body)) :-
   % Ensure the identifier is not a variable.
   nonvar(Identifier),
   % Find the existing clause with the same identifier.
   clause((Identifier :::: OldBody), _),
   % If the bodies are different, withdraw the old rule.
   \+ (Body = OldBody),
   pfcWithdraw((Identifier :::: OldBody)),
   !.
% If no matching rule is found, succeed without action.
pfcRemoveOldVersion0(_).

%!  with_fc_mode(+Mode, :Goal) is nondet.
%
%   Temporarily switches to the specified forward-chaining propagation mode
%   while executing the given `Goal`. The original mode is restored after the
%   goal completes.
%
%   @arg Mode  The forward-chaining mode to use (e.g., `thread`).
%   @arg Goal  The goal to execute while in the specified mode.
%
%   @example
%     % Execute a goal in `thread` mode temporarily.
%     ?- with_fc_mode(thread, some_goal).
%
%   @see locally/2.
%
with_fc_mode(Mode, Goal) :-
   % Temporarily set the search mode while running the goal.
   locally(t_l:pfcSearchTL(Mode), Goal).

%!  pfcThreadFwd(+S, +P) is det.
%
%   Performs forward reasoning for the clause `P` in a separate thread,
%   using the given support `S`. The thread executes the forward chaining
%   logic within the `thread` mode.
%
%   @arg S  The supporting context or reason for the clause.
%   @arg P  The clause or fact to be processed in the thread.
%
%   @example
%     % Execute forward reasoning for a fact in a new thread.
%     ?- pfcThreadFwd(support_reason, my_fact).
%
%   @see with_fc_mode/2, call_in_thread/1.
%
pfcThreadFwd(S, P) :-
   % Run the forward reasoning in a new thread with `thread` mode.
   with_only_current_why(S,
      call_in_thread(with_fc_mode(thread, (pfcFwd(P))))).

% in_fc_call(Goal):- with_fc_mode( thread, Goal).
%in_fc_call(Goal):- with_fc_mode( direct, Goal).
% in_fc_call(Goal):- !, pfcCallSystem(Goal).

%

%!  pfcRun is det.
%
%   Computes the deductive closure of the current database. The method used
%   depends on the search mode:
%     - `direct`: Forward chaining (`fc`) has already computed the closure.
%     - `depth` or `breadth`: Uses the `pfcQueue` mechanism to process queued facts.
%
%   @example
%     % Compute the deductive closure using the current search mode.
%     ?- pfcRun.
%
% Recursively run forward reasoning unless using `direct` mode.
pfcRun :-
   (\+ pfcGetSearch(direct)),  % Ensure we're not in `direct` mode.
   pfcStep,                    % Perform one reasoning step.
   pfcRun.                     % Continue running.
pfcRun.

%!  pfcStep is det.
%
%   Removes one entry from the `pfcQueue` and reasons from it. If a halt signal
%   is found, it stops the reasoning process. Otherwise, it processes the next
%   fact from the queue using `pfcFwd/1`.
%
%   @example
%     % Perform one reasoning step by processing the next fact from the queue.
%     ?- pfcStep.
%
pfcStep :-
   % Check for a halt signal and stop if found.
   pfcRetract(pfcHaltSignal(Msg)),
   pfcTraceMsg(removing(pfcHaltSignal(Msg))),
   !, fail.
pfcStep :-
   % Draw conclusions from the next fact in the queue.
   get_next_fact(P),
   pfcdo(pfcFwd(P)),
   !.

%!  get_next_fact(-P) is nondet.
%
%   Retrieves the next fact to be processed from the `pfcQueue` and removes it.
%   This predicate fails if the queue is empty.
%
%   @arg P  The next fact to be processed.
%
%   @example
%     % Get the next fact from the queue.
%     ?- get_next_fact(P).
%
get_next_fact(P) :-
   % Select and remove the next fact from the queue.
   select_next_fact(P),
   remove_selection(P).

%!  remove_selection(+P) is det.
%
%   Removes the selected fact `P` from the `pfcQueue` and its supports. If the
%   fact is not found on the queue, it triggers a debugging message.
%
%   @arg P  The fact to be removed from the queue.
%
%   @example
%     % Remove a fact from the queue and its supports.
%     ?- remove_selection(my_fact).
%
remove_selection(P) :-
   % Remove the fact from the queue.
   pfcRetract(pfcQueue(P)),
   % Quietly remove any supports associated with the fact.
   pfcRemoveSupportsQuietly(pfcQueue(P)),
   !.
remove_selection(P) :-
   % Log a debugging message if the fact was not found on the queue.
   brake(pfcPrintf("pfc:get_next_fact - selected fact not on Queue: ~p", [P])).

%!  select_next_fact(-P) is nondet.
%
%   Identifies the next fact to reason from. This predicate first tries the
%   user-defined predicate `pfcSelect/1`. If that fails, it falls back to the
%   default selection mechanism provided by `defaultpfcSelect/1`.
%
%   @arg P  The next fact to be selected for reasoning.
%
%   @example
%     % Select the next fact to reason from.
%     ?- select_next_fact(P).
%
select_next_fact(P) :-
   % Try the user-defined predicate first.
   pfcSelect(P),
   !.
select_next_fact(P) :-
   % If the user-defined predicate fails, use the default selection mechanism.
   defaultpfcSelect(P),
   !.

%!  defaultpfcSelect(-P) is nondet.
%
%   The default selection predicate, which selects the item at the front of
%   the `pfcQueue`. This serves as a fallback when no user-defined selection
%   predicate is provided.
%
%   @arg P  The next fact at the front of the queue.
%
%   @example
%     % Select the next fact from the queue.
%     ?- defaultpfcSelect(P).
%
defaultpfcSelect(P) :-
   % Select the next fact from the front of the queue.
   pfcCallSystem(pfcQueue(P)),
   !.

%!  pfcHalt is det.
%
%   Stops the forward chaining process by setting a halt signal with an
%   unknown reason.
%
%   @example
%     % Halt the forward chaining process.
%     ?- pfcHalt.
%
pfcHalt :-
   % Use the halt mechanism with an unknown reason.
   pfcHalt("unknown_reason", []).

%!  pfcHalt(+Format) is det.
%
%   Halts the forward chaining process with a formatted message, using the
%   provided format string and an empty list of arguments.
%
%   @arg Format  The format string for the halt message.
%
%   @example
%     % Halt the forward chaining with a specific message.
%     ?- pfcHalt("Halt due to system maintenance").
%
pfcHalt(Format) :-
   % Use the formatted halt mechanism with no additional arguments.
   pfcHalt(Format, []).

%!  pfcHalt(+Format, +Args) is det.
%
%   Halts the forward chaining process with a formatted message, using the
%   provided format string and arguments. If the halt signal with the same
%   message is already set, a warning is issued.
%
%   @arg Format  The format string for the halt message.
%   @arg Args    The arguments to format the halt message.
%
%   @example
%     % Halt the forward chaining with a formatted message.
%     ?- pfcHalt("Halt due to reason: ~w", [maintenance]).
%
pfcHalt(Format, Args) :-
   % Format the halt message.
   format(string(Msg), Format, Args),
   % Check if the halt signal is already set.
   (pfcHaltSignal(Msg)
   -> % Warn if the halt signal is already set.
      pfcWarn("pfcHalt finds pfcHaltSignal(~w) already set", [Msg])
   ;  % Otherwise, assert the halt signal.
      assert(pfcHaltSignal(Msg))).

% %
% %
% %  predicates for manipulating triggers
% %

%!  pfcAddTrigger(+Trigger, +Support) is det.
%
%   Adds a trigger (positive, negative, or backward) to the system,
%   asserting it with the provided support and evaluating it if applicable.
%   Unrecognized triggers result in a warning message.
%
%   @arg Trigger  The trigger to be added, which can be a positive (`$pt$`),
%                 negative (`$nt$`), or backward (`$bt$`) trigger.
%   @arg Support  The support context for asserting the trigger.
%
%   @example
%     % Add a positive trigger.
%     ?- pfcAddTrigger('$pt$'(my_trigger, some_body), support_context).
%
pfcAddTrigger('$pt$'(Trigger, Body), Support) :-
   % Add a positive trigger.
   !,
   pfcTraceMsg('      Adding positive trigger(+) ~p~n', ['$pt$'(Trigger, Body)]),
   pfcAssert('$pt$'(Trigger, Body), Support),
   copy_term('$pt$'(Trigger, Body), Tcopy),
   pfc_call(Trigger),
   with_current_why(Trigger, fcEvalLHS(Body, (Trigger, Tcopy))),
   fail.
pfcAddTrigger('$nt$'(Trigger, Test, Body), Support) :-
   % Add a negative trigger.
   !,
   pfcTraceMsg('      Adding negative trigger(-): ~p~n       test: ~p~n       body: ~p~n',
               [Trigger, Test, Body]),
   copy_term(Trigger, TriggerCopy),
   pfcAssert('$nt$'(TriggerCopy, Test, Body), Support),
   \+ pfc_call(Test),
   with_current_why(\+ pfc_call(Test), fcEvalLHS(Body, ((\+Trigger), '$nt$'(TriggerCopy, Test, Body)))).
pfcAddTrigger('$bt$'(Trigger, Body), Support) :-
   % Add a backward trigger.
   !,
   pfcAssert('$bt$'(Trigger, Body), Support),
   pfcBtPtCombine(Trigger, Body, Support).
pfcAddTrigger(X, _Support) :-
   % Warn about unrecognized triggers.
   pfcWarn("Unrecognized trigger(?) to pfcAddtrigger: ~p", [X]).

%!  pfcBtPtCombine(+Head, +Body, +Support) is det.
%
%   Combines a backward trigger (`$bt$`) with any positive triggers
%   (`$pt$`) that have unifying heads. For each unifying positive trigger,
%   the instantiated body of the backward trigger is evaluated.
%
%   @arg Head     The head of the backward trigger.
%   @arg Body     The body of the backward trigger.
%   @arg Support  The support context for the combination.
%
%   @example
%     % Combine a backward trigger with matching positive triggers.
%     ?- pfcBtPtCombine(my_head, my_body, support_context).
%
pfcBtPtCombine(Head, Body, Support) :-
   % Find any positive triggers with unifying heads and evaluate the body.
   pfcGetTriggerQuick('$pt$'(Head, _PtBody)),
   fcEvalLHS(Body, Support),
   fail.
pfcBtPtCombine(_, _, _) :- !.

%!  pfcGetTriggerQuick(+Trigger) is nondet.
%
%   Quickly checks if a trigger exists in the system. It succeeds if the trigger
%   is defined as a clause or can be executed with `pfc_call/1`.
%
%   @arg Trigger  The trigger to check.
%
%   @example
%     % Check if a positive trigger exists.
%     ?- pfcGetTriggerQuick('$pt$'(my_head, _)).
%
pfcGetTriggerQuick(Trigger) :-
   % Check if the trigger is defined as a clause or can be executed.
   clause(Trigger, true) *-> true ; pfc_call(Trigger).

%!  pfcCallSystem(+Trigger) is det.
%
%   Calls the system with the provided trigger, using `pfc_call/1`.
%
%   @arg Trigger  The trigger to call.
%
%   @example
%     % Call a trigger in the system.
%     ?- pfcCallSystem(my_trigger).
%
pfcCallSystem(Trigger) :-
   % Execute the trigger with `pfc_call/1`.
   pfc_call(Trigger).

% %
% %
% %  predicates for manipulating action traces.
% %

%!  pfcAddActionTrace(+Action, +Support) is det.
%
%   Adds an action trace along with its support context. The action is stored
%   as `pfcAction/1` to keep track of actions taken during forward reasoning.
%
%   @arg Action   The action to be traced.
%   @arg Support  The supporting context for the action.
%
%   @example
%     % Add a trace for an action with support.
%     ?- pfcAddActionTrace(my_action, some_support).
%
pfcAddActionTrace(Action, Support) :-
   % Add an action trace and its support.
   pfcAddSupport(pfcAction(Action), Support).

%!  pfcRemActionTrace(+PfcAction) is det.
%
%   Removes an action trace by executing the corresponding undo method. If an
%   undo method is defined for the action, it is called using the system's
%   trigger mechanism.
%
%   @arg pfcAction(A)  The action trace to be removed.
%
%   @example
%     % Remove a previously added action trace.
%     ?- pfcRemActionTrace(pfcAction(my_action)).
%
pfcRemActionTrace(pfcAction(A)) :-
   % Retrieve and execute the undo method for the action.
   fcUndoMethod(A, UndoMethod),
   pfcCallSystem(UndoMethod),
   !.

% %
% %  predicates to remove pfc facts, triggers, action traces, and queue items
% %  from the database.
% %

%!  pfcRetract(+X) is det.
%
%   Retracts an arbitrary fact, rule, trigger, or action from the database.
%   It first determines the type of the entity and then uses the appropriate
%   retraction mechanism based on the type.
%
%   @arg X  The entity to be retracted (fact, rule, trigger, or action).
%
%   @example
%     % Retract a fact from the database.
%     ?- pfcRetract(my_fact).
%
pfcRetract(X) :-
   % Determine the type of the entity to be retracted.
   pfcType(X, Type),
   % Use the appropriate retraction mechanism based on the type.
   pfcRetractType(Type, X),
   !.

%!  pfcRetractType(+Type, +X) is det.
%
%   Retracts an entity from the database based on its type. The predicate handles
%   facts, rules, triggers, and actions differently, ensuring the correct version
%   (with or without database context) is retracted. For actions, it removes the
%   associated action trace. If a trigger is not found, a warning is issued.
%
%   @arg Type  The type of the element to be retracted (e.g., `fact(_)`, `rule(_)`, `trigger(Pos)`, `action`).
%   @arg X     The specific entity to be retracted.
%
%   @example
%     % Retract a fact with or without database context.
%     ?- pfcRetractType(fact(_), my_fact).
%
%     % Retract a rule with database context if present.
%     ?- pfcRetractType(rule(_), my_rule).
%
%     % Retract a trigger and clean up forward chaining.
%     ?- pfcRetractType(trigger(+), my_trigger).
%
%     % Retract an action trace.
%     ?- pfcRetractType(action, pfcAction(my_action)).
%
pfcRetractType(fact(_), X) :-
    % Handle facts, with or without a database context.
    pfcAddDbToHead(X, X2) -> retract(X2) ; retract(X).
pfcRetractType(rule(_), X) :-
    % Handle rules, with or without a database context.
    pfcAddDbToHead(X, X2) -> retract(X2) ; retract(X).
pfcRetractType(trigger(Pos), X) :-
    % Attempt to retract the trigger.
    retract(X)
    -> % If successful, perform additional cleanup.
       unFc(X)
    ;  % If not found, issue a warning.
       pfcWarn("Trigger(~p) not found to retract: ~p", [Pos, X]).
pfcRetractType(action, X) :-
    % Remove the action trace using the appropriate helper.
    pfcRemActionTrace(X).

%!  pfcAddType1(+X) is det.
%
%   Adds the item `X` to the appropriate database based on its type.
%   This predicate determines the type of the item, prepares it by adding
%   the database context if needed, and delegates to the corresponding
%   addition predicate.
%
%   @arg X  The item to be added to the database (fact, rule, trigger, or action).
%
%   @example
%     % Add a fact to the appropriate database.
%     ?- pfcAddType1(my_fact).
%
pfcAddType1(X) :-
   % Determine the type of the item.
   pfcType(X, Type),
   % Prepare the item by adding the database context.
   pfcAddDbToHead(X, X2),
   % Call the appropriate predicate based on the type.
   pfcAddType(Type, X2).

%!  pfcAddType(+FactType, +X) is det.
%
%   Adds a fact to the database. If the fact is unique, it is asserted;
%   otherwise, it is skipped.
%
%   @arg factType  The specific type of the fact.
%   @arg X         The fact to be added.
%
%   @example
%     % Add a unique fact to the database.
%     ?- pfcAddType(fact(my_type), my_fact).
%
pfcAddType(fact(Type), X) :-
   % Check for uniqueness and assert the fact.
   pfcUnique(fact(Type), X),
   assert(X), !.
pfcAddType(rule(Type), X) :-
   % Check for uniqueness and assert the rule.
   pfcUnique(rule(Type), X),
   assert(X), !.
pfcAddType(trigger(Pos), X) :-
   % Check for uniqueness and assert the trigger. Issue a warning if not unique.
   pfcUnique(trigger(Pos), X)
   -> assert(X)
   ;  (pfcWarn(not_pfcUnique(X)), assert(X)).
pfcAddType(action, _Action) :-
   % otherwise succeed
   !.

%!  pfcWithdraw(+P) is det.
%
%   Withdraws any direct support for the given fact, rule, or trigger `P`.
%   If `P` is a list, it iterates through the list and withdraws each element's support.
%
%   @arg P  The fact, rule, trigger, or list of such elements to withdraw support from.
%
%   @example
%     % Withdraw support from a single fact.
%     ?- pfcWithdraw(my_fact).
%
%     % Withdraw support from a list of facts.
%     ?- pfcWithdraw([fact1, fact2]).
%
pfcWithdraw(P) :-
   % If `P` is a list, iterate and withdraw support for each element.
   is_list(P),
   !, my_maplist(pfcWithdraw, P).

pfcWithdraw(P) :-
   % Withdraw support with the current "why" context.
   matches_why_UU(UU),
   pfcWithdraw(P, UU).

%!  pfcWithdraw(+P, +S) is det.
%
%   Removes the support `S` from the given fact, rule, or trigger `P`.
%   If the entity is no longer supported after the removal, it is retracted
%   from the database, and any related support relationships are removed.
%
%   @arg P  The fact, rule, or trigger to withdraw support from.
%   @arg S  The specific support to be removed.
%
%   @example
%     % Withdraw a specific support from a fact.
%     ?- pfcWithdraw(my_fact, support_reason).
%
pfcWithdraw(P, S) :-
   % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  pfcGetSupport(P,S),
  matterialize_support_term(S,Sup),
  pfcTraceMsg('    Withdrawing direct support: ~p   \n   From: ~p~n',[Sup,P]),
   (pfcRemOneSupportOrQuietlyFail(P,S)
      -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
      ; pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not pfcRemOneSupport: ~p",
                 [Sup,P])),
   removeIfUnsupported(P).
pfcWithdraw(P, S) :-
   % Handle the case where no matching support is found.
   matterialize_support_term(S, Sup),
   pfcTraceMsg('    No support matching: ~p\n   For: ~p~n', [Sup, P]),
   !,
   % Check if the entity should be removed due to lack of support.
   removeIfUnsupported(P).

%!  pfcRetractAll(+P) is det.
%
%   Withdraws both "direct" and "indirect" support for the given fact, rule,
%   or trigger `P`. If `P` is a list, it iterates through the list and
%   withdraws support for each element.
%
%   @arg P  The fact, rule, trigger, or list of such elements to withdraw support from.
%
%   @example
%     % Withdraw all support from a single fact.
%     ?- pfcRetractAll(my_fact).
%
%     % Withdraw all support from a list of facts.
%     ?- pfcRetractAll([fact1, fact2]).
%
pfcRetractAll(P) :-
   % If `P` is a list, iterate and withdraw all support for each element.
   is_list(P),
   !, my_maplist(pfcRetractAll, P).
pfcRetractAll(P) :-
   % Withdraw both direct and indirect support with the current "why" context.
   matches_why_UU(UU),
   pfcRetractAll(P, UU).

%!  pfcRetractAll(+P, +S) is det.
%
%   Removes the support `S` from the given fact, rule, or trigger `P` and checks
%   whether `P` is still supported. If `P` is no longer supported, it is retracted
%   from the database, and all related support relationships are removed.
%
%   @arg P  The fact, rule, or trigger to remove support from.
%   @arg S  The specific support to be removed.
%
%   @example
%     % Withdraw a specific support and check if the entity should be removed.
%     ?- pfcRetractAll(my_fact, some_support).
%
pfcRetractAll(Fact, S) :-
   % Normalize the arguments for proper handling.
   control_arg_types(Fact, Fixed),
   !,
   pfcRetractAll(Fixed, S).

pfcRetractAll(P, S) :-
   % Withdraw support and fail to backtrack for further operations.
   \+ \+ pfcWithdraw(P, S),
   fail.

pfcRetractAll(P, S) :-
   % If `P` is supported by another fact, recursively remove that support.
   pfcGetSupport(P, (P2, _)),
   pfcType(P2, fact(_)),
   pfcSupportedBy(P2, S, _How),
   pfcRetractAll(P2),
   \+ fcSupported(P),
   !,
   fcUndo(P).

pfcRetractAll(P, S) :-
   % Handle cases where `P` is indirectly supported by another trigger.
   pfcGetSupport(P, (_, T)),
   pfcGetSupport(T, (P2, _)),
   pfcSupportedBy(P2, S, _How),
   pfcType(P2, fact(_)),
   pfcRetractAll(P2),
   \+ fcSupported(P),
   !,
   fcUndo(P).

pfcRetractAll(P, S) :-
   % If `P` is supported by a rule, recursively remove the rule's support.
   fcSupported(P),
   pfcGetSupport(P, (P2, _)),
   pfcSupportedBy(P2, S, _How),
   pfcType(P2, rule(_)),
   pfcRetractAll(P2),
   \+ fcSupported(P),
   fcUndo(P),
   !.

pfcRetractAll(P, _S0) :-
   % Remove `P` if it is no longer supported.
   removeIfUnsupported(P),
   fail.

pfcRetractAll(_, _).

%!  pfcSupportedBy(+P, +S, -How) is nondet.
%
%   Determines how the entity `P` is supported by the support `S`. It checks
%   both the forward and backward support relationships.
%
%   @arg P    The entity being checked.
%   @arg S    The supporting context.
%   @arg How  The method or relationship through which the support is provided.
%
pfcSupportedBy(P, S, How) :-
   % Check if the support relationship matches either forward or backward links.
   pfcGetSupport(P, (F, T)),
   (pfcSupportedBy(F, S, _) -> How = F ; pfcSupportedBy(T, S, How)).
pfcSupportedBy(P, S, How) :-
   % If the entity is directly supported by `S`.
   P = S,
   How = S.

%!  pfcRetractAll_v2(+P, +S0) is det.
%
%   A version of `pfcRetractAll/2` that removes the support `S0` from the
%   given entity `P` and traces the process. It recursively removes the
%   support and any related triggers.
%
%   @arg P   The entity from which the support is being removed.
%   @arg S0  The original support to be removed.
%
pfcRetractAll_v2(P, S0) :-
   % Withdraw the given support and trace the process.
   \+ \+ pfcWithdraw(P,S0),
  pfcGetSupport(P,(S,RemoveIfTrigger)),
  % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  matterialize_support_term((S,RemoveIfTrigger),Sup),
  pfcTraceMsg('    Removing support: ~p   \n   From: ~p~n',[Sup,P]),
  (pfcRemOneSupportOrQuietlyFail(P,(S,RemoveIfTrigger))
     -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
     ; (pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not yet pfcRetractAll_v2: ~p",
                [Sup,P]))),
   % Recursively remove the remaining support.
   pfcRetractAll_v2(S, S0),
   fail.
pfcRetractAll_v2(P, _) :-
   % Remove the entity if it is no longer supported.
   removeIfUnsupported(P).

%!  pfcRemove(+P) is det.
%
%   The user interface for removing support for the given fact, rule, or trigger `P`.
%   It behaves like `pfcRetractAll/1` by withdrawing all user support, but if the
%   entity remains in the database after removing the support, it is forcefully
%   removed using `pfcBlast/1`.
%
%   @arg P  The fact, rule, or trigger to remove from the database.
%
%   @example
%     % Remove a fact and ensure it is forcefully retracted if still present.
%     ?- pfcRemove(my_fact).
%
pfcRemove(Fact) :-
   % Normalize the argument type.
   control_arg_types(Fact, Fixed),
   !,
   pfcRemove(Fixed).
pfcRemove(P) :-
   % Withdraw all support for the entity.
   pfcRetractAll(P),pfc_call(P) -> pfcBlast(P) ; true.

%!  pfcBlast(+F) is det.
%
%   Forcefully retracts the given fact, rule, or trigger `F` from the database
%   and removes any dependent facts. This ensures that the entity is completely
%   removed along with its related support relationships.
%
%   @arg F  The fact, rule, or trigger to be forcefully removed.
%
%   @example
%     % Forcefully remove a fact from the database.
%     ?- pfcBlast(my_fact).
%
pfcBlast(F) :-
   % Remove all supports associated with the entity.
   pfcRemoveSupports(F),
   % Undo the entity and its effects.
   fcUndo(F).

%!  pfcRemoveSupports(+F) is det.
%
%   Removes any remaining supports for the given fact, rule, or trigger `F`.
%   If any support is found, it logs a warning indicating which support was removed.
%   This predicate ensures that all support relationships are cleaned up.
%
%   @arg F  The fact, rule, or trigger whose supports are to be removed.
%
%   @example
%     % Remove all supports for a fact with warnings.
%     ?- pfcRemoveSupports(my_fact).
%
pfcRemoveSupports(F) :-
   % Attempt to remove one support at a time, logging a warning for each.
   pfcRemOneSupport(F, S),
   pfcWarn("~p was still supported by ~p (but no longer)", [F, S]),
   fail.
pfcRemoveSupports(_).

%!  pfcRemoveSupportsQuietly(+F) is det.
%
%   Removes any remaining supports for the given fact, rule, or trigger `F`
%   without logging any warnings. This version ensures that support relationships
%   are cleaned up quietly.
%
%   @arg F  The fact, rule, or trigger whose supports are to be removed.
%
%   @example
%     % Quietly remove all supports for a fact.
%     ?- pfcRemoveSupportsQuietly(my_fact).
%
pfcRemoveSupportsQuietly(F) :-
   % Attempt to remove one support at a time without logging warnings.
   pfcRemOneSupport(F, _),
   fail.
pfcRemoveSupportsQuietly(_).

% fcUndo(X) undoes X.

%!  fcUndo(+P) is det.
%
%   Undoes the given fact, rule, or trigger `P` by removing it from the
%   database and handling any associated cleanup. Specific undo actions
%   are executed depending on the type of the entity.
%
%   @arg P  The entity to be undone (e.g., `pfcAction/1`, positive or negative trigger).
%
%   @example
%     % Undo a specific action.
%     ?- fcUndo(pfcAction(my_action)).
%
fcUndo(pfcAction(A)) :-
   % Undo an action by removing its trace.
   !,
   pfcRemActionTrace(pfcAction(A)).

fcUndo('$pt$'(Head, Body)) :-
   % Undo a positive trigger (+).
   !,
   (retract('$pt$'(Head, Body))
   -> unFc('$pt$'(Head, Body))
   ;  pfcWarn("Trigger not found to retract: ~p", ['$pt$'(Head, Body)])).

fcUndo('$nt$'(Head, Condition, Body)) :-
   % Undo a negative trigger (-).
   !,
   (retract('$nt$'(Head, Condition, Body))
   -> unFc('$nt$'(Head, Condition, Body))
   ;  pfcWarn("Trigger not found to retract: ~p", ['$nt$'(Head, Condition, Body)])).

fcUndo(Fact) :-
   % Undo a general fact, printing a trace if relevant.
   retract(Fact),
   pfcTraceRem(Fact),
   unFc(Fact).

%!  unFc(+F) is det.
%
%   "Un-forward-chains" from the given fact `F`, meaning that `F` has been
%   removed from the database, so all dependent relationships are also removed.
%   It ensures that related entities are properly cleaned up.
%
%   @arg F  The fact that has just been removed.
%
%   @example
%     % Undo forward chaining for a removed fact.
%     ?- unFc(my_fact).
%
unFc(F) :-
   % Retract all dependent relations associated with the fact.
   pfcRetractDependantRelations(F),
   % Continue with further cleanup.
   unFc1(F).

%!  unFc1(+F) is det.
%
%   Performs additional cleanup after a fact has been removed. It checks
%   if any triggers need to be undone for the removed fact, and may restart
%   forward chaining if necessary.
%
%   @arg F  The fact that has been removed and needs further processing.
%
%   @example
%     % Perform post-removal cleanup for a fact.
%     ?- unFc1(my_fact).
%
unFc1(F) :-
   % Check if any triggers need to be undone for the given fact.
   pfcUnFcCheckTriggers(F),
   % Optionally run forward chaining again (if appropriate).
   pfcRun.

%!  pfcUnFcCheckTriggers(+F) is det.
%
%   Checks for negative triggers (`$nt$`) associated with the given fact `F`
%   and evaluates their conditions. If the conditions no longer hold, the
%   associated actions are executed.
%
%   @arg F  The fact to check for triggers.
%
%   @example
%     % Check for triggers associated with a fact.
%     ?- pfcUnFcCheckTriggers(my_fact).
%
pfcUnFcCheckTriggers(F) :-
   % Ensure the entity is a fact before processing triggers.
   pfcType(F, fact(_)),
   copy_term(F, Fcopy),
   % Look for any negative triggers associated with the fact.
   pfcCallSystem('$nt$'(Fcopy, Condition, Action)),
   % If the condition no longer holds, evaluate the associated action.
   (\+ pfcCallSystem(Condition)),
   fcEvalLHS(Action, ((\+F), '$nt$'(F, Condition, Action))),
   fail.
pfcUnFcCheckTriggers(_).

%!  pfcRetractDependantRelations(+Fact) is det.
%
%   Removes any dependent relationships associated with the given fact or
%   trigger. If the dependent entity is no longer supported, it is also removed.
%
%   @arg Fact  The fact or trigger whose dependent relationships are to be removed.
%
%   @example
%     % Remove all dependent relations for a fact.
%     ?- pfcRetractDependantRelations(my_fact).
%
pfcRetractDependantRelations(Fact) :-
   % Determine the type of the entity.
   pfcType(Fact, Type),
   % If it is a trigger, handle it accordingly.
   (Type = trigger(_Pos) -> pfcRemOneSupport(P, (_, Fact))
   ; pfcRemOneSupportOrQuietlyFail(P, (Fact, _))),
   % Remove the dependent entity if it is unsupported.
   removeIfUnsupported(P),
   fail.
pfcRetractDependantRelations(_).

%!  removeIfUnsupported(+P) is det.
%
%   Checks whether the given fact, rule, or trigger `P` is supported.
%   If it is not supported, it is removed from the database using `fcUndo/1`.
%
%   @arg P  The entity to be checked and possibly removed.
%
%   @example
%     % Remove a fact if it is no longer supported.
%     ?- removeIfUnsupported(my_fact).
%
removeIfUnsupported(P) :-
   % If supported, trace the message; otherwise, undo it.
   fcSupported(P)
   -> pfcTraceMsg(fcSupported(P))
   ;  fcUndo(P).

%!  fcSupported(+P) is nondet.
%
%   Succeeds if the given fact, rule, or trigger `P` is "supported". The
%   meaning of "supported" depends on the TMS (Truth Maintenance System)
%   mode currently in use.
%
%   @arg P  The entity to be checked for support.
%
%   @example
%     % Check if a fact is supported.
%     ?- fcSupported(my_fact).
%
fcSupported(P) :-
   % Retrieve the current TMS mode and check if `P` is supported.
   must_ex(fcTmsMode(Mode)),
   supported(Mode, P).

%!  supported(+Mode, +P) is nondet.
%
%   Determines whether the given entity `P` is supported based on the
%   current TMS mode. Different modes have different criteria for support:
%     - `local`: Supported if it has a support relationship.
%     - `cycles`: Supported if it is well-founded.
%     - Other modes: Always considered supported.
%
%   @arg Mode  The current TMS mode.
%   @arg P     The entity to check for support.
%
supported(local, P) :-
   % In local mode, check if there is a support relationship.
   !, pfcGetSupport(P, _).
supported(cycles, P) :-
   % In cycles mode, check if the entity is well-founded.
   !, wellFounded(P).
supported(_, _P) :-
   % In other modes, assume everything is supported.
   true.

%!  wellFounded(+Fact) is nondet.
%
%   Determines whether the given `Fact` is well-founded. A fact is considered
%   well-founded if it is supported by the user (axiom) or assumption, or if
%   it is supported by a set of facts and rules, all of which are also well-founded.
%
%   @arg Fact  The fact to check for well-foundedness.
%
%   @example
%     % Check if a fact is well-founded.
%     ?- wellFounded(my_fact).
%
wellFounded(Fact) :-
   % Start the well-foundedness check with an empty list of descendants.
   wf(Fact, []).

%!  wf(+F, +Descendants) is nondet.
%
%   Recursively checks whether the given fact `F` is well-founded. It ensures
%   that the fact is not part of a dependency loop and that all its supporters
%   are well-founded.
%
%   @arg F            The fact to check.
%   @arg Descendants  A list of descendants to detect loops.
%
wf(F, _) :-
   % If the fact is an axiom or assumption, it is well-founded.
   (axiom(F) ; assumption(F)),
   !.
wf(F, Descendants) :-
   % Ensure there is no dependency loop.
   (\+ memberchk(F, Descendants)),
   % Find a justification for the fact.
   supports(F, Supporters),
   % Check that all supporters are well-founded.
   wflist(Supporters, [F | Descendants]),
   !.

%!  wflist(+List, +Descendants) is det.
%
%   Recursively checks whether all elements in the given list are well-founded.
%
%   @arg List         A list of facts or rules to check.
%   @arg Descendants  A list of ancestors to detect loops.
%
%   @example
%     % Check if all facts in a list are well-founded.
%     ?- wflist([fact1, fact2], []).
%
wflist([], _).
wflist([X | Rest], L) :-
   % Check if the current element is well-founded.
   wf(X, L),
   % Continue with the rest of the list.
   wflist(Rest, L).

% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [user].

%!  supports(+F, -ListOfSupporters) is nondet.
%
%   Determines a list of supporters (`ListOfSupporters`) for a given fact `F`.
%   The list represents one justification for `F`, consisting of facts and
%   potentially a rule that together deduce `F`. User-defined facts typically
%   have the support `[user]`.
%
%   @arg F                 The fact for which supporters are determined.
%   @arg ListOfSupporters  A list of facts and rules supporting the given fact.
%
%   @example
%     % Get the supporters for a fact.
%     ?- supports(my_fact, Supporters).
%
supports(F, [Fact | MoreFacts]) :-
   % Retrieve the fact and the associated trigger.
   pfcGetSupport(F, (Fact, Trigger)),
   % Determine the additional supports from the trigger.
   triggerSupports(Trigger, MoreFacts).

%!  triggerSupports(+Trigger, -AllSupport) is nondet.
%
%   Retrieves the list of all supporters for the given trigger. If the trigger
%   has no additional supporters, an axiomatic check is performed.
%
%   @arg Trigger     The trigger to find supporters for.
%   @arg AllSupport  The list of all supporters for the trigger.
%
triggerSupports(U, []) :-
   % Handle axiomatic supporters.
   axiomatic_supporter(U),
   !.

triggerSupports(Trigger, AllSupport) :-
   % Attempt to retrieve supporters using two strategies.
    triggerSupports1(Trigger, AllSupport) *-> true ;
    triggerSupports2(Trigger, AllSupport).

%!  triggerSupports1(+Trigger, -AllSupport) is nondet.
%
%   Retrieves supporters for the given trigger. This method retrieves the fact
%   and a secondary trigger, recursively determining all supporters.
%
%   @arg Trigger     The trigger to find supporters for.
%   @arg AllSupport  The list of all supporters for the trigger.
%
triggerSupports1(Trigger, AllSupport) :-
   % Retrieve support details for the trigger.
   pfcGetSupport(Trigger, (Fact, AnotherTrigger)),
   % Recursively find supporters or use the secondary trigger.
   (triggerSupports(AnotherTrigger, MoreFacts) *-> true ; MoreFacts = [AnotherTrigger]),
   % Construct the complete list of supporters.
   [Fact | MoreFacts] = AllSupport.

%!  triggerSupports2(+Trigger, -AllSupport) is nondet.
%
%   Alternative strategy for retrieving supporters. This version is currently
%   disabled with a `fail` directive.
%
%   @arg Trigger     The trigger to find supporters for.
%   @arg AllSupport  The list of all supporters for the trigger.
%
triggerSupports2(Trigger, AllSupport) :-
   fail,  % This strategy is disabled.
   pfcGetSupport(Trigger, (Fact, AnotherTrigger)),
   (triggerSupports(AnotherTrigger, MoreFacts) *-> true ; MoreFacts = [AnotherTrigger]),
   [Fact | MoreFacts] = AllSupport.

%!  axiomatic_supporter(+U) is nondet.
%
%   Checks if the given entity `U` is an axiomatic supporter, meaning it provides
%   fundamental or assumed support (e.g., user input or assumptions).
%
%   @arg U  The entity to check for axiomatic support.
%
axiomatic_supporter(Var) :-
   % Fail if the entity is a free variable.
   is_ftVar(Var),
   !,
   fail.
axiomatic_supporter(is_ftVar(_)).
axiomatic_supporter(clause_u(_)).
axiomatic_supporter(user(_)).
axiomatic_supporter(U) :-
   % Check if the entity is a file reference.
   is_file_ref(U),
   !.
axiomatic_supporter(ax) :- !.

%!  is_file_ref(+A) is nondet.
%
%   Checks if the given term `A` is a file reference.
%
%   @arg A  The term to check for being a file reference.
%
is_file_ref(A) :-
   % Check if the term matches the expected file reference structure.
   compound(A),
   A = mfl4(_VarNameZ, _, _, _).

%!  triggerSupports(+FactIn, +Trigger, -OUT) is det.
%
%   Retrieves supporters for a given trigger. If the trigger has additional
%   supports, they are added to the output list `OUT`. If not, fallback strategies
%   are used.
%
%   @arg FactIn   The input fact to which the trigger is related.
%   @arg Trigger  The trigger to find supporters for.
%   @arg OUT      The output list of supporters.
%
triggerSupports(_, Var, [is_ftVar(Var)]) :-
   % Handle free variables.
   is_ftVar(Var),
   !.
triggerSupports(_, U, []) :-
   % Handle axiomatic supporters.
   axiomatic_supporter(U),
   !.
triggerSupports(FactIn, Trigger, OUT) :-
   % Attempt to retrieve supporters for the trigger.
   pfcGetSupport(Trigger, (Fact, AnotherTrigger)) *->
      (triggerSupports(Fact, AnotherTrigger, MoreFacts), OUT = [Fact | MoreFacts])
   ;  triggerSupports1(FactIn, Trigger, OUT).

%!  triggerSupports1(+FactIn, +X, -OUT) is det.
%
%   Fallback strategy to retrieve supporters. If all other methods fail, it may
%   use this strategy depending on the system's configuration.
%
%   @arg FactIn  The input fact related to the trigger.
%   @arg X       The trigger or fact being checked.
%   @arg OUT     The output list of supporters.
%
triggerSupports1(_, X, [X]) :-
   % Use this strategy only if permitted by system configuration.
   may_cheat.

%!  may_cheat is nondet.
%
%   Determines if fallback strategies are allowed by checking a system flag.
%
may_cheat :-
   % Check if the system is configured to allow fallback behavior.
   true_flag.

% %
% %
%!  pfcFwd(+X) is det.
%
%   Forward-chains from a given fact or a list of facts `X`. This predicate
%   performs forward reasoning by processing positive and negative triggers
%   associated with the facts. If the input is a list, it iterates over the
%   elements, applying forward chaining to each.
%
%   @arg X  The fact or list of facts to forward chain from.
%
%   @example
%     % Forward chain from a single fact.
%     ?- pfcFwd(my_fact).
%
%     % Forward chain from a list of facts.
%     ?- pfcFwd([fact1, fact2]).
%
pfcFwd(Fact) :-
   % Normalize the argument types.
   control_arg_types(Fact, Fixed),
   !,
   pfcFwd(Fixed).

pfcFwd(Fact) :-
   % Set `occurs_check` flag locally and perform forward chaining.
   locally(set_prolog_flag(occurs_check, true), pfcFwd0(Fact)).

pfcFwd0(Fact) :-
   % If input is a list, apply forward chaining to each element.
   is_list(List)
   -> my_maplist(pfcFwd0, List)
   ;  pfcFwd1(Fact).

%!  pfcFwd1(+Fact) is det.
%
%   Performs forward chaining for a single fact. It checks if the fact is
%   subject to special rule handling and processes positive and negative triggers.
%
%   @arg Fact  The fact to forward chain from.
%
pfcFwd1(Fact) :-
   (fc_rule_check(Fact) *-> true ; true),
   % Make a copy of the fact for further processing.
   copy_term(Fact, F),
   % Check and process positive triggers.
   ignore(fcpt(Fact, F)),
   % Check and process negative triggers.
   ignore(fcnt(Fact, F)).

%!  fc_rule_check(+P) is nondet.
%
%   Performs special built-in forward chaining if the input `P` is a rule.
%   It processes unidirectional (`==>`), bidirectional (`<==>`) and backward (`<-`) rules.
%
%   @arg P  The rule to process.
%
fc_rule_check((Name::::P ==> Q)) :-
   % Process unidirectional rule with a name.
   !, processRule(P, Q, (Name::::P ==> Q)).
fc_rule_check((Name::::P <==> Q)) :-
   % Process bidirectional rule with a name.
   !,
   processRule(P, Q, ((Name::::P <==> Q))),
   processRule(Q, P, ((Name::::P <==> Q))).
fc_rule_check((P ==> Q)) :-
   % Process unidirectional rule.
   !, processRule(P, Q, (P ==> Q)).
fc_rule_check((P <==> Q)) :-
   % Process bidirectional rule.
   !,
   processRule(P, Q, (P <==> Q)),
   processRule(Q, P, (P <==> Q)).
fc_rule_check(('<-'(P, Q))) :-
   % Process backward chaining rule.
   !, pfcDefineBcRule(P, Q, ('<-'(P, Q))).
fc_rule_check(_).

%!  fcpt(+Fact, +F) is det.
%
%   Processes positive triggers associated with the given fact. It searches for
%   positive triggers and evaluates their bodies if found.
%
%   @arg Fact  The original fact.
%   @arg F     A copy of the original fact for further processing.
%
fcpt(Fact, F) :-
   % Retrieve positive triggers associated with the fact.
   pfcGetTriggerQuick('$pt$'(F, Body)),
   pfcTraceMsg('\n Found positive trigger(+):\n    ~p~n       body: ~p~n', [F, Body]),
   % Get the support for the trigger and evaluate the body.
   pfcGetSupport('$pt$'(F, Body), Support),
   with_current_why(Support, with_current_why(Fact, fcEvalLHS(Body, (Fact, '$pt$'(F, Body))))),
   fail.
% fcpt(Fact, F) :-
%   pfcGetTriggerQuick('$pt$'(presently(F), Body)),
%   fcEvalLHS(Body, (presently(Fact), '$pt$'(presently(F), Body))),
%   fail.
fcpt(_, _).

%!  fcnt(+Fact, +F) is det.
%
%   Processes negative triggers associated with the given fact. If the condition
%   of a negative trigger is satisfied, the trigger is removed.
%
%   @arg Fact  The original fact.
%   @arg F     A copy of the original fact for further processing.
%
fcnt(_Fact, F) :-
   % Retrieve and process negative triggers.
   pfc_spft(X, _, '$nt$'(F, Condition, Body)),
   pfcCallSystem(Condition),
   pfcRem_S(X, (_, '$nt$'(F, Condition, Body))),
   fail.
fcnt(_, _).

%!  pfcRem_S(+P, +S) is det.
%
%   Removes the support `S` from the given fact, rule, or trigger `P`. After
%   removing the support, it checks whether `P` is still supported. If `P` is
%   no longer supported, it is retracted from the database, and any related
%   support relationships are also removed.
%
%   @arg P  The entity (fact, rule, or trigger) from which support is removed.
%   @arg S  The specific support to be removed.
%
%   @example
%     % Remove support from a fact and check if it should be retracted.
%     ?- pfcRem_S(my_fact, some_support).
%
pfcRem_S(P, S) :-
   % Trace the removal of support for debugging purposes.
   pfcTraceMsg('    Removing support: ~p from ~p~n', [S, P]),
   % Attempt to remove the support.
   pfcRemOneSupport(P, S)
   -> % If successful, check if the entity should be removed.
      removeIfUnsupported(P)
   ;  % If the support was not found, issue a warning.
      pfcWarn("pfcRem_S/2 Could not find support ~p to remove from fact ~p",
              [S, P]).

%!  pfcDefineBcRule(+Head, +Body, +ParentRule) is det.
%
%   Defines a backward-chaining rule and adds the corresponding `$bt$` triggers
%   to the database. If the `Head` is not an atomic literal, a warning and error
%   are issued, and the rule definition fails.
%
%   @arg Head        The head of the backward-chaining rule.
%   @arg Body        The body of the backward-chaining rule.
%   @arg ParentRule  The parent rule from which this rule is derived.
%
%   @example
%     % Define a backward-chaining rule.
%     ?- pfcDefineBcRule(my_head, my_body, (my_head :- my_body)).
%
pfcDefineBcRule(Head, _Body, ParentRule) :-
   % Ensure the head of the rule is an atomic literal.
   (\+ pfcLiteral(Head)),
   pfcWarn("Malformed backward chaining rule. ~p not atomic literal.", [Head]),
   pfcError("caused by rule: ~p", [ParentRule]),
   !,
   fail.
pfcDefineBcRule(Head, Body, ParentRule) :-
   % Copy the parent rule for safe manipulation.
   copy_term(ParentRule, ParentRuleCopy),
   % Build the right-hand side (RHS) of the rule.
   buildRhs(Head, Rhs),
   % Retrieve the current user context.
   current_why_U(USER),
   % For each normalized form of the body, build and add the trigger.
   pfcForEach(pfc_nf(Body, Lhs),
              (buildTrigger(Lhs, rhs(Rhs), Trigger),
               pfcAdd('$bt$'(Head, Trigger), (ParentRuleCopy, USER)))).

%!  get_bc_clause(+Head, -Clause) is det.
%
%   Retrieves the corresponding backward-chaining clause for the given `Head`.
%   The clause consists of a head and a body, handling cases where the head
%   is negated (`~Head`).
%
%   @arg Head    The head of the clause.
%   @arg Clause  The full clause, including both head and body.
%
get_bc_clause(Head, (HeadC :- BodyC)) :-
   % Retrieve the head and body components of the backward clause.
   get_bc_clause(Head, HeadC, BodyC).

%!  get_bc_clause(+HeadIn, -HeadC, -Body) is det.
%
%   Retrieves the head and body components of a backward-chaining clause,
%   handling negated heads (`~Head`) if necessary.
%
%   @arg HeadIn  The input head, possibly negated.
%   @arg HeadC   The canonical head of the clause.
%   @arg Body    The body of the clause.
%
get_bc_clause(HeadIn, ~HeadC, Body) :-
   % Handle negated head case.
   compound(HeadIn),
   HeadIn = ~Head,
   !,
   Body = (awc,
           (nonvar(HeadC) -> (HeadC = Head, !) ; (HeadC = Head)),
           pfc_bc_and_with_pfc(~Head)).
get_bc_clause(Head, Head, Body) :-
   % Handle non-negated head case.
   Body = (awc, !, pfc_bc_and_with_pfc(Head)).

:- thread_initialization(nb_setval('$pfc_current_choice',[])).

%!  push_current_choice is det.
%
%   Pushes the current Prolog choice point onto a stack for backtracking
%   control. This operation is skipped if the `pfc_support_cut` flag is disabled.
%
push_current_choice :-
   % Skip if the `pfc_support_cut` flag is disabled.
   current_prolog_flag(pfc_support_cut, false),
   !.
push_current_choice :-
   % Retrieve the current choice point and push it onto the stack.
   prolog_current_choice(CP),
   push_current_choice(CP),
   !.
push_current_choice(CP) :-
   % Update the stack with the new choice point.
   nb_current('$pfc_current_choice', Was)
   -> b_setval('$pfc_current_choice', [CP | Was])
   ;  b_setval('$pfc_current_choice', [CP]).

%!  cut_c is det.
%
%   Cuts to the most recent saved choice point, restoring the state before
%   that choice point was created. This operation is skipped if the
%   `pfc_support_cut` flag is disabled.
%
%   @example
%     % Perform a cut to the most recent choice point.
%     ?- cut_c.
%
cut_c :-
   % Skip if the `pfc_support_cut` flag is disabled.
   current_prolog_flag(pfc_support_cut, false),
   !.
cut_c :-
   % Retrieve the most recent saved choice point and cut to it.
   must_ex(nb_current('$pfc_current_choice', [CP |_WAS])),
   prolog_cut_to(CP).

%!  fcEvalLHS(+LHS, +Support) is det.
%
%   Evaluates the left-hand side (LHS) of a rule. Depending on the structure
%   of the LHS, it may process conditional tests, triggers, or RHS elements.
%   If the LHS contains an unrecognized item, a warning is issued.
%
%   @arg LHS      The left-hand side expression to be evaluated.
%   @arg Support  The supporting context for the evaluation.
%
%   @example
%     % Evaluate a conditional LHS with support.
%     ?- fcEvalLHS((some_condition -> some_action), support_reason).
%
fcEvalLHS((Test -> Body), Support) :-
   % Handle conditional tests on the LHS.
   !,
   pfcDoAll(pfcCallSystem(Test) -> (fcEvalLHS(Body, Support))),
   !.
fcEvalLHS((Test *-> Body), Support) :-
   % Handle soft conditional tests on the LHS.
   !,
   pfcDoAll(pfcCallSystem(Test) *-> (fcEvalLHS(Body, Support))).
fcEvalLHS(rhs(X), Support) :-
   % Evaluate the RHS as part of the LHS.
   !,
   pfcDoAll(pfc_eval_rhs(X, Support)),
   !.
fcEvalLHS(X, Support) :-
   % Handle triggers as part of the LHS.
   pfcType(X, trigger(_Pos)),
   !,
   pfcAddTrigger(X, Support),
   !.
% fcEvalLHS(snip(X), Support) :-
%    snip(Support),
%    fcEvalLHS(X, Support).
fcEvalLHS(X, _) :-
   % Issue a warning for unrecognized items on the LHS.
   pfcWarn("Unrecognized item found in trigger body, namely ~p.", [X]).

%!  pfc_eval_rhs(+RHS, +Support) is det.
%
%   Evaluates the right-hand side (RHS) of a rule. The RHS may contain multiple
%   elements, and this predicate ensures each is processed appropriately.
%
%   @arg RHS      The right-hand side elements to be evaluated.
%   @arg Support  The supporting context for the evaluation.
%
%   @example
%     % Evaluate a list of RHS elements with support.
%     ?- pfc_eval_rhs([action1, action2], support_reason).
%
pfc_eval_rhs([], _) :-
   % Base case: empty RHS.
   !.
pfc_eval_rhs([Head | Tail], Support) :-
   % Evaluate each element on the RHS.
   pfc_eval_rhs1(Head, Support),
   pfc_eval_rhs(Tail, Support).

%!  pfc_eval_rhs1(+Element, +Support) is det.
%
%   Evaluates a single element on the RHS. Depending on the structure, it may
%   be an action, a negated literal, a sublist, or an assertion.
%
%   @arg Element  The RHS element to be evaluated.
%   @arg Support  The supporting context for the evaluation.
%
pfc_eval_rhs1(Fact, S) :-
   % Normalize argument types before further processing.
   control_arg_types(Fact, Fixed),
   !,
   pfc_eval_rhs1(Fixed, S).
pfc_eval_rhs1({Action}, Support) :-
   % Handle evaluable Prolog code wrapped in `{}`.
   !,
   fcEvalAction(Action, Support).
pfc_eval_rhs1(P, _Support) :-
   % Handle negated literals on the RHS.
   pfcNegatedLiteral(P),
   !,
   pfcWithdraw(P).
pfc_eval_rhs1([X | Xrest], Support) :-
   % Handle embedded sublists on the RHS.
   !,
   pfc_eval_rhs([X | Xrest], Support).
pfc_eval_rhs1(Assertion, Support) :-
   % Handle assertions to be added to the database.
   once_writeq_nl(pfcRHS(Assertion)),
   (must_ex(pfcPost1(Assertion, Support)) *-> true
   ;  pfcWarn("Malformed rhs of a rule: ~p", [Assertion])).

%!  fcEvalAction(+Action, +Support) is det.
%
%   Evaluates an action found on the RHS of a rule. If the action is
%   `undoable`, it adds an action trace for future reference. If not,
%   it simply executes the action.
%
%   @arg Action   The action to be evaluated.
%   @arg Support  The supporting context for the action.
%
%   @example
%     % Evaluate an action with support tracking.
%     ?- fcEvalAction(my_action, support_context).
%
fcEvalAction(Action, Support) :-
   % Execute the action using the system's call mechanism.
   pfcCallSystem(Action),
   % If the action is undoable, add an action trace.
   (undoable(Action)
   -> pfcAddActionTrace(Action, Support)
   ;  true).

%!  trigger_trigger(+Trigger, +Body, +Support) is det.
%
%   Evaluates a trigger by calling the trigger condition and then evaluating
%   the associated body. If the trigger condition holds, it proceeds to evaluate
%   the body using `fcEvalLHS/2`.
%
%   @arg Trigger  The trigger condition to evaluate.
%   @arg Body     The body to execute if the trigger condition holds.
%   @arg Support  The supporting context for the trigger.
%
%   @example
%     % Evaluate a trigger and its body.
%     ?- trigger_trigger(my_trigger, my_body, support_reason).
%
trigger_trigger(Trigger, Body, _Support) :-
   % Process the trigger if the condition holds.
   trigger_trigger1(Trigger, Body).
trigger_trigger(_, _, _).

% trigger_trigger1(presently(Trigger), Body) :-
%    % Evaluate triggers with a `presently/1` wrapper.
%    !,
%    copy_term(Trigger, TriggerCopy),
%    pfc_call(Trigger),
%    fcEvalLHS(Body, (presently(Trigger), '$pt$'(presently(TriggerCopy), Body))),
%    fail.

%!  trigger_trigger1(+Trigger, +Body) is det.
%
%   Copies the trigger term, evaluates it, and if successful, executes the
%   associated body. This predicate ensures that triggers are evaluated with
%   the correct context using `fcEvalLHS/2`.
%
%   @arg Trigger  The trigger condition to evaluate.
%   @arg Body     The body to execute if the trigger condition holds.
%
%   @example
%     % Evaluate a trigger condition and its body.
%     ?- trigger_trigger1(my_trigger, my_body).
%
trigger_trigger1(Trigger, Body) :-
   % Make a copy of the trigger for safe evaluation.
   copy_term(Trigger, TriggerCopy),
   % Execute the trigger condition.
   pfc_call(Trigger),
   % Evaluate the associated body with the current context.
   with_current_why(Trigger, fcEvalLHS(Body, (Trigger, '$pt$'(TriggerCopy, Body)))),
   fail.

%!  pfc_call(+F) is nondet.
%
%   `pfc_call/1` succeeds if the given term `F` is a fact available for forward
%   chaining. This predicate also has the side effect of catching unsupported
%   facts and assigning them support from a default source (God). It handles
%   various forms of Prolog terms such as conjunctions, disjunctions, negations,
%   and arithmetic evaluations.
%
%   @arg F  The fact or term to be evaluated.
%
%   @example
%     % Call a Prolog fact or evaluate an expression.
%     ?- pfc_call(my_fact).
%
%pfc_call(F) :- var(F), !, pfc_call(F).
pfc_call(P) :-
   % Handle cases where the input is a variable.
   var(P),!, pfcFact(P).
pfc_call(P) :-
   % Ensure the input is callable; throw an error if not.
   \+ callable(P), throw(pfc_call(P)).
pfc_call((!)) :-
   % Handle cuts by invoking `cut_c`.
   !, cut_c.
pfc_call(true) :-
   % Handle the `true/0` predicate.
   !.
pfc_call((A -> B; C)) :-
   % Handle conditional disjunctions with `->`.
   !, pfc_call(A) -> pfc_call(B) ; pfc_call(C).
pfc_call((A *-> B; C)) :-
   % Handle soft conditional disjunctions with `*->`.
   !, pfc_call(A) *-> pfc_call(B) ; pfc_call(C).
pfc_call((A -> B)) :-
   % Handle conditional conjunctions with `->`.
   !, pfc_call(A) -> pfc_call(B).
pfc_call((A *-> B)) :-
   % Handle soft conditional conjunctions with `*->`.
   !, pfc_call(A) *-> pfc_call(B).
pfc_call((A, B)) :-
   % Handle conjunctions.
   !, pfc_call(A), pfc_call(B).
pfc_call((A; B)) :-
   % Handle disjunctions.
   !, pfc_call(A) ; pfc_call(B).
pfc_call(\+ (A)) :-
   % Handle negation.
   !, \+ pfc_call(A).
pfc_call((A is B)) :-
   % Handle arithmetic evaluations.
   !, A is B.
pfc_call(clause(A, B)) :-
   % Handle clause/2.
   !, clause(A, B).
pfc_call(clause(A, B, Ref)) :-
   % Handle clause/3.
   !, clause(A, B, Ref).
% we really need to check for system predicates as well.
% this is probably not advisable due to extreme inefficiency.
%pfc_call(P) :- var(P), !, pfcFact(P).
% Handle backward-chaining rules.
pfc_call(P) :-
   '$bt$'(P, Trigger),
   pfcGetSupport('$bt$'(P, Trigger), S),
   % Evaluate the trigger with its support.
   fcEvalLHS(Trigger, S),
   fail.
% Handle system predicates.
pfc_call(P) :-
   predicate_property(P, imported_from(system)), !, call(P).
% Handle built-in predicates.
pfc_call(P) :-
   predicate_property(P, built_in), !, call(P).
% Handle dynamic predicates.
pfc_call(P) :-
   \+ predicate_property(P, _), functor(P, F, A), dynamic(F / A), !, call(P).
% Handle predicates with no clauses.
pfc_call(P) :-
   \+ predicate_property(P, number_of_clauses(_)), !, call(P).
% Handle general cases with backtracking and choice points.
pfc_call(P) :-
   setup_call_cleanup(
      nb_current('$pfc_current_choice', Was),
      (prolog_current_choice(CP),
       push_current_choice(CP),
       clause(P, Condition),
       pfc_call(Condition)),
      nb_setval('$pfc_current_choice', Was)).
/*
pfc_call(P) :-
  clause(P, true) *-> true ;
  (clause(P, Condition), Condition \== true, pfc_call(Condition)).
*/

%!  undoable(+A) is nondet.
%
%   Determines if an action `A` is undoable by checking if there is a method
%   available for undoing it.
%
%   @arg A  The action to check for undoability.
%
%   @example
%     % Check if an action is undoable.
%     ?- undoable(my_action).
%
undoable(A) :-
   fcUndoMethod(A, _).

%!  pfc_cache_bc(+P) is det.
%
%   Triggers any backward-chaining (BC) rules for the given fact `P`. For each
%   matching trigger, the rule is evaluated with its corresponding support.
%
%   @arg P  The fact to trigger backward-chaining rules for.
%
%   @example
%     % Trigger backward-chaining rules for a fact.
%     ?- pfc_cache_bc(my_fact).
%
pfc_cache_bc(P) :-
   % Iterate over all backward-chaining rules and evaluate their triggers.
   forall('$bt$'(P, Trigger),
      forall(pfcGetSupport('$bt$'(P, Trigger), S),
         fcEvalLHS(Trigger, S))).

% %
% %  Defining Forward-Chaining Rules
% %

%!  pfc_nf(+In, -Out) is nondet.
%
%   Maps the left-hand side (LHS) of a PFC rule to a normalized form (Out).
%   This predicate also applies optimizations. Backtracking may produce
%   additional normalized forms.
%
%   @arg In   The LHS of the rule to be normalized.
%   @arg Out  The normalized form of the LHS.
%
%   @example
%     % Normalize a conjunction.
%     ?- pfc_nf((a, b), NF).
%
pfc_nf(LHS, List) :-
   % Normalize the LHS and handle negations.
   pfc_nf1(LHS, List2),
   pfc_nf_negations(List2, List).

%!  pfc_nf1(+In, -Out) is nondet.
%
%   Converts the LHS of a PFC rule to a normalized form. This predicate
%   supports variables, literals, negations, conjunctions, and disjunctions.
%
%   @arg In   The LHS of the rule to normalize.
%   @arg Out  The normalized form of the LHS.
%
pfc_nf1(P, [P]) :-
   % Handle variables directly.
   var(P),
   !.

% These two rules provide upward compatibility and will be removed
% once the P/Condition form is no longer in use.

pfc_nf1(P / Cond, [(\+P) / Cond]) :-
   % Handle negated literals.
   pfcNegatedLiteral(P),
   !.

pfc_nf1(P / Cond, [P / Cond]) :-
   % Handle positive literals.
   pfcLiteral(P),
   !.

pfc_nf1(NegTerm, NF) :-
   % Handle negated forms by un-negating them.
   pfc_unnegate(NegTerm, Term),
   !,
   pfc_nf1_negation(Term, NF).

pfc_nf1((P ; Q), NF) :-
   % Handle disjunctions.
   !,
   (pfc_nf1(P, NF) ; pfc_nf1(Q, NF)).

pfc_nf1((P, Q), NF) :-
   % Handle conjunctions.
   !,
   pfc_nf1(P, NF1),
   pfc_nf1(Q, NF2),
   append(NF1, NF2, NF).

pfc_nf1(P, [P]) :-
   % Handle individual literals.
   pfcLiteral(P),
   !.

/* % % % Should we catch remaining cases as errors? */
pfc_nf1(Term, [Term]) :-
   % Issue a warning if the term is not recognized.
   pfcWarn("pfc_nf doesn''t know how to normalize ~p (accepting though)", [Term]).

%!  pfc_nf1_negation(+P, -NF) is det.
%
%   Produces the normalized form `NF` for a negated term `P`.
%
%   @arg P   The negated term.
%   @arg NF  The normalized form of the negated term.
%
pfc_nf1_negation((P / Cond), [(\+(P)) / Cond]) :-
   !.
pfc_nf1_negation((P ; Q), NF) :-
   % Handle negated disjunctions.
   !,
   pfc_nf1_negation(P, NFp),
   pfc_nf1_negation(Q, NFq),
   append(NFp, NFq, NF).
pfc_nf1_negation((P, Q), NF) :-
   % This code may not be correct (marked by "twf").
   !,
   pfc_nf1_negation(P, NF) ;
   (pfc_nf1(P, Pnf), pfc_nf1_negation(Q, Qnf), append(Pnf, Qnf, NF)).
pfc_nf1_negation(P, [\+P]).

%!  pfc_nf_negations(+List2, -List) is det.
%
%   Processes a list of terms, converting tilde-based negations to
%   Prolog-style negations using `\+`.
%
%   @arg List2  The input list with original negations.
%   @arg List   The output list with converted negations.
%
pfc_nf_negations(X, X) :-
   % If the input and output are the same, do nothing.
   !.
pfc_nf_negations([], []).
pfc_nf_negations([H1 | T1], [H2 | T2]) :-
   % Process each term recursively.
   pfc_nf_negation(H1, H2),pfc_nf_negations(T1, T2).

%!  pfc_nf_negation(+Form, -Normalized) is det.
%
%   Converts various negation forms to Prolog-style negations.
%
%   @arg Form       The original negated form.
%   @arg Normalized The normalized negated form.
%
pfc_nf_negation(Form, {\+ X}) :-
   % Handle tilde-based negations.
   nonvar(Form),
   Form = (~({X})),
   !.
pfc_nf_negation(Form, {\+ X}) :-
   % Handle negations with `-`.
   tilded_negation,
   nonvar(Form),
   Form = (-({X})),
   !.
pfc_nf_negation(Form, {\+ X}) :-
   % Handle explicit Prolog-style negations with `\+`.
   tilded_negation,
   nonvar(Form),
   Form = (\+ ({X})),
   !.
pfc_nf_negation(X, X).

%!  constrain_meta(+Lhs, ?Guard) is nondet.
%
%   Generates a guard condition based on the provided logical structure.
%   Guards are constraints or checks to ensure that certain properties hold
%   for the logical terms in Prolog. This predicate applies rules to various
%   logical constructs to generate corresponding guards.
%
%   If the `constrain_meta` flag is set to `false`, this predicate fails immediately,
%   effectively disabling the guard creation logic.
%
%   You can disable this feature by setting the following Prolog flag:
%
%   @example
%     ?- set_prolog_flag(constrain_meta, false).
%
%   @arg Lhs   The left-hand side of the logical construct (e.g., a term or rule).
%   @arg Guard The guard that corresponds to the logical construct in `Lhs`.
%              This is unified with a generated constraint or check.
%
constrain_meta(_, _) :-
    % Check if the constrain_meta flag is set to false.
    % If so, immediately fail and prevent guard creation.
    current_prolog_flag(constrain_meta, false), !, fail.
% FACT case: Generate a positive fact guard.
constrain_meta(P, mpred_positive_fact(P)) :-
    % If P is a free term variable, generate a positive fact guard.
    is_ftVar(P), !.
% NEG chaining: Handle negated expressions (~P or \+ P).
constrain_meta(~P, CP) :-
    % Recursively apply the guard logic on the inner term P.
    !, constrain_meta(P, CP).
constrain_meta(\+P, CP) :-
    % Recursively apply the guard logic on the inner term P.
    !, constrain_meta(P, CP).
% FWD chaining: Handle forward chaining rules (_ ==> Q).
constrain_meta((_ ==> Q), nonvar(Q)) :-
    % If Q is a free term variable, generate a nonvar check for Q.
    !, is_ftVar(Q).
% EQV chaining: Handle equivalence rules (P <==> Q).
constrain_meta((P <==> Q), (nonvar(Q); nonvar(P))) :-
    % If either P or Q is a free term variable, generate nonvar checks.
    (is_ftVar(Q); is_ftVar(P)), !.
% BWD chaining: Handle backward chaining rules (Q <- _).
constrain_meta((Q <- _), mpred_literal(Q)) :-
    % If Q is a free term variable, generate a literal check for Q.
    is_ftVar(Q), !.
constrain_meta((Q <- _), CQ) :-
    % Recursively apply the guard logic on Q.
    !, constrain_meta(Q, CQ).
% CWC chaining: Handle conditional rules (Q :- _).
constrain_meta((Q :- _), mpred_literal(Q)) :-
    % If Q is a free term variable, generate a literal check for Q.
    is_ftVar(Q), !.
constrain_meta((Q :- _), CQ) :-
    % Recursively apply the guard logic on Q.
    !, constrain_meta(Q, CQ).

%!  is_simple_lhs(+ActN) is nondet.
%
%   Checks whether the given term is a "simple" left-hand side (LHS).
%   A "simple" LHS is defined as one that is not negated, divided, or
%   a conjunction or disjunction of complex terms. This predicate applies
%   several checks to ensure the simplicity of the LHS structure.
%
%   @arg ActN The term to be checked. It can be any valid Prolog term.
%
%   The predicate fails if the term matches any of the complex or negated
%   patterns, and succeeds if none of those conditions hold.
%
%   Below are the cases where the predicate will explicitly fail:
%     1. The term is a free variable.
%     2. The term is negated (\+ or ~).
%     3. The term contains division or complex logical structures.
%
is_simple_lhs(ActN) :-
    % If the term is a free variable, fail immediately.
    is_ftVar(ActN), !, fail.
% Fail if the LHS is explicitly negated.
is_simple_lhs(\+ _) :- !, fail.
is_simple_lhs(~ _)  :- !, fail.
% Fail if the LHS contains a division structure (/) or complex parts.
is_simple_lhs(_ / _) :- !, fail.
% Handle conjunctions (Lhs1, Lhs2): Both parts must be simple LHSs.
is_simple_lhs((Lhs1, Lhs2)) :-
    % Recursively check both parts of the conjunction.
    !, is_simple_lhs(Lhs1),
    is_simple_lhs(Lhs2).
% Handle disjunctions (Lhs1; Lhs2): Both parts must be simple LHSs.
is_simple_lhs((Lhs1; Lhs2)) :-
    % Recursively check both parts of the disjunction.
    !, is_simple_lhs(Lhs1),
    is_simple_lhs(Lhs2).
is_simple_lhs(ActN) :-
    % Fail if the term is an "active" LHS (as defined by is_active_lhs/1).
    is_active_lhs(ActN), !, fail.
% Handle complex division structures (Lhs1 / Lhs2).
is_simple_lhs((Lhs1 / Lhs2)) :-
    % Fail immediately but ensure both sides are checked for simplicity.
    !, fail,
    is_simple_lhs(Lhs1),
    is_simple_lhs(Lhs2).
% Succeed if the term passes all checks and is considered a "simple" LHS.
is_simple_lhs(_).

%!  is_active_lhs(+ActN) is nondet.
%
%   Checks whether the given term represents an "active" left-hand side (LHS).
%   An "active" LHS includes specific terms that are recognized as control constructs,
%   such as cuts or certain wrapped terms. This predicate identifies such terms
%   through pattern matching and recursive checks.
%
%   @arg ActN The term to be checked. It can be any valid Prolog term.
%
%   The predicate fails if the input term is a variable. For compound terms
%   (e.g., conjunctions, disjunctions, and divisions), it recursively checks
%   the parts of the term to determine if any component is active.
%
is_active_lhs(ActN) :-
    % Fail immediately if the term is a free variable.
    var(ActN), !, fail.
% Recognize the cut operator (!) as an active LHS.
is_active_lhs(!).
% Recognize the `cut_c` atom as an active LHS.
is_active_lhs(cut_c).
% Recognize terms of the form `actn/1` as active.
is_active_lhs(actn(_Act)).
% Recognize terms wrapped in `{}/1` as active.
is_active_lhs('{}'(_Act)).
% Handle division structures (Lhs1 / Lhs2) by recursively checking both sides.
is_active_lhs((Lhs1 / Lhs2)) :-
    % Succeed if either LHS1 or LHS2 is active.
    !, is_active_lhs(Lhs1); is_active_lhs(Lhs2).
% Handle conjunctions (Lhs1, Lhs2) by recursively checking both sides.
is_active_lhs((Lhs1, Lhs2)) :-
    % Succeed if either LHS1 or LHS2 is active.
    !, is_active_lhs(Lhs1); is_active_lhs(Lhs2).
% Handle disjunctions (Lhs1; Lhs2) by recursively checking both sides.
is_active_lhs((Lhs1; Lhs2)) :-
    % Succeed if either LHS1 or LHS2 is active.
    !, is_active_lhs(Lhs1); is_active_lhs(Lhs2).

%!  add_lhs_cond(+Lhs1, +Lhs2, -Result) is det.
%
%   Combines two left-hand side (LHS) terms with an additional condition.
%   If the first term is already a division (Lhs1 / Cond), the second LHS
%   is added to the condition part. Otherwise, a new division is formed
%   by combining both terms.
%
%   @arg Lhs1   The first LHS term, which may already include a condition.
%   @arg Lhs2   The second LHS term to be added as a condition.
%   @arg Result The resulting LHS term with the new condition applied.
%
%   @examples
%     % Combine two LHS terms directly:
%     ?- add_lhs_cond(foo, bar, Result).
%     Result = foo/bar.
%
%     % Add a new condition to an existing condition:
%     ?- add_lhs_cond(foo/old_cond, new_cond, Result).
%     Result = foo/(old_cond, new_cond).
%
add_lhs_cond(Lhs1 / Cond, Lhs2, Lhs1 / (Cond, Lhs2)) :- !.
add_lhs_cond(Lhs1, Lhs2, Lhs1 / Lhs2).

%!  buildRhs(+Conjunction, -Rhs) is det.
%
%   Constructs a right-hand side (RHS) list from a given conjunction of terms.
%   This predicate recursively processes a conjunction `(A, B)` by converting
%   each part into a compiled RHS term using `pfcCompileRhsTerm/2` and collecting
%   the results in a list. If the input is a single term, it is wrapped in a list.
%
%   @arg Conjunction The input term, which can be a conjunction `(A, B)` or a
%                    single term. If it is a conjunction, each part will be processed
%                    and added to the result list.
%   @arg Rhs         The output list containing the compiled RHS terms.
%
%   @examples
%     % Process a single term:
%     ?- buildRhs(foo, Rhs).
%     Rhs = [foo].
%
%     % Process a conjunction:
%     ?- buildRhs((foo, bar), Rhs).
%     Rhs = [foo, bar].
%
buildRhs(X, [X]) :-
    % If the input is a variable, wrap it in a list and stop.
    var(X),
    !.
buildRhs((A, B), [A2 | Rest]) :-
    % If the input is a conjunction, process the first term (A).
    % Compile A into A2, and recursively build the RHS for the rest.
    !,
    pfcCompileRhsTerm(A, A2),
    buildRhs(B, Rest).
buildRhs(X, [X2]) :-
    % If the input is a single non-variable term, compile it and wrap it in a list.
    pfcCompileRhsTerm(X, X2).

%!  pfcCompileRhsTerm(+Term, -CompiledTerm) is det.
%
%   Compiles a term for use on the right-hand side (RHS) of a rule.
%   If the term has the form `P / C`, it is rewritten as `(P :- C)`.
%   Otherwise, the term is left unchanged.
%
%   @arg Term          The input term to be compiled. It can be a term of the form
%                      `P / C` or any other valid Prolog term.
%   @arg CompiledTerm  The output term, which is either the rewritten `(P :- C)`
%                      or the original term if no rewriting is needed.
%
%   @examples
%     % Compile a term with a condition:
%     ?- pfcCompileRhsTerm(foo / bar, Compiled).
%     Compiled = (foo :- bar).
%
%     % Compile a simple term:
%     ?- pfcCompileRhsTerm(foo, Compiled).
%     Compiled = foo.
%
pfcCompileRhsTerm((P / C), ((P :- C))) :-
    % Rewrite `P / C` as `(P :- C)` to form a valid Prolog rule.
    !.
pfcCompileRhsTerm(P, P).
    % Leave the term unchanged if it is not of the form `P / C`.

%!  pfc_unnegate(+N, -P) is nondet.
%
%   True if `N` is a negated term, and `P` is the corresponding term
%   with the negation operator removed. This predicate handles multiple
%   types of negation, including `~`, `-`, and `\+/1`.
%
%   @arg N The potentially negated term.
%   @arg P The unnegated form of the term.
%
%   @example
%     ?- pfc_unnegate(~foo, Result).
%     Result = foo.
%
pfc_unnegate(P, _) :-
    % Fail if the term is a variable.
    var(P), !, fail.

pfc_unnegate((~P), P) :-
    % Handle negation using `~` if `tilded_negation` is disabled.
    \+ tilded_negation.

pfc_unnegate((-P), P).
    % Handle negation using `-`.

pfc_unnegate((\+(P)), P).
    % Handle negation using Prolog's `\+/1` operator.

%!  pfcNegatedLiteral(+P) is nondet.
%
%   True if `P` is a negated literal. This predicate uses `pfc_unnegate/2`
%   to determine if the literal is negated and then checks if the inner term
%   is a positive literal using `pfcPositiveLiteral/1`.
%
%   @arg P The literal to check.
%
%   @example
%     ?- pfcNegatedLiteral(\+foo).
%     true.
%
pfcNegatedLiteral(P) :-
    % Ensure that P is a callable term.
    callable(P),
    % Unnegate the term and check if the result is a positive literal.
    pfc_unnegate(P, Q),
    pfcPositiveLiteral(Q).

%!  pfcLiteral(+X) is nondet.
%
%   True if `X` is either a negated or positive literal.
%
%   @arg X The literal to check.
%
%   @example
%     ?- pfcLiteral(foo).
%     true.
%
pfcLiteral(X) :-
    % Check if the term is a negated literal.
    pfcNegatedLiteral(X).
pfcLiteral(X) :-
    % Check if the term is a positive literal.
    pfcPositiveLiteral(X).

%!  pfcPositiveLiteral(+X) is nondet.
%
%   True if `X` is a positive literal. A positive literal is a callable term
%   that does not use any of the predefined connectives.
%
%   @arg X The literal to check.
%
%   @example
%     ?- pfcPositiveLiteral(bar).
%     true.
%
pfcPositiveLiteral(X) :-
    % Ensure that X is a callable term.
    callable(X),
    % Extract the functor from X.
    functor(X, F, _),
    % Fail if the functor is a known connective.
    \+ pfcConnective(F).

%!  pfcConnective(+F) is nondet.
%
%   True if `F` is a known connective used in logical terms. Connectives
%   are operators or symbols used to combine terms.
%
%   @arg F The functor to check.
%
%   @example
%     ?- pfcConnective(';').
%     true.
%
pfcConnective(';').      % Disjunction
pfcConnective(',').      % Conjunction
pfcConnective('/').      % Division
pfcConnective('|').      % Alternative
pfcConnective(('==>')).  % Forward implication
pfcConnective(('<-')).   % Backward implication
pfcConnective('<==>').   % Bi-conditional
pfcConnective('-').      % Negation with `-`

pfcConnective('~') :-
    % Use `~` as a connective only if `tilded_negation` is disabled.
    \+ tilded_negation.

pfcConnective((\+)).     % Prolog's negation operator

%!  is_implicitly_prolog(+Callable) is nondet.
%
%   True if `Callable` is a valid Prolog construct.
%
%   @arg Callable A term to be checked if it is a valid Prolog construct.
%
%   @example
%     ?- is_implicitly_prolog(X is 2).
%     true.
%
is_implicitly_prolog(Callable) :-
    % Fail if the term is not callable.
    \+ callable(Callable), !, fail.

is_implicitly_prolog(_ is _).
    % Handle Prolog's `is/2` arithmetic evaluation.

%!  processRule(+Lhs, +Rhs, +ParentRule) is det.
%
%   Processes a rule by building the RHS and constructing new rules.
%   This predicate creates a copy of the `ParentRule`, compiles the RHS,
%   and processes each normalized form of the LHS.
%
%   @arg Lhs        The left-hand side (LHS) of the rule.
%   @arg Rhs        The right-hand side (RHS) of the rule.
%   @arg ParentRule The original rule being processed.
%
%   @example
%     ?- processRule(foo, bar, my_rule).
%
processRule(Lhs, Rhs, ParentRule) :-
    % Create a copy of the parent rule for later use.
    copy_term(ParentRule, ParentRuleCopy),
    % Build the RHS by compiling the terms into a list.
    buildRhs(Rhs, Rhs2),
    % Retrieve the current user context (USER).
    current_why_U(USER),  % @TODO: Review _U usage.
    % Process each normalized form of the LHS.
    pfcForEach(
        pfc_nf(Lhs, Lhs2),
        buildRule(Lhs2, rhs(Rhs2), (ParentRuleCopy, USER))
    ).

%!  buildRule(+Lhs, +Rhs, +Support) is det.
%
%   Constructs a rule by building a trigger from the LHS and RHS, and
%   evaluating the trigger using `fcEvalLHS/2`.
%
%   @arg Lhs     The left-hand side of the rule.
%   @arg Rhs     The right-hand side of the rule.
%   @arg Support The supporting information for the rule, typically including
%                the original rule and user context.
%
%   @example
%     ?- buildRule(foo, bar, some_support).
%
buildRule(Lhs, Rhs, Support) :-
    % Build a trigger from the LHS and RHS.
    buildTrigger(Lhs, Rhs, Trigger),
    % Evaluate the trigger with the given support.
    fcEvalLHS(Trigger, Support).


%!  buildTrigger(+Triggers, +Consequent, -Result) is det.
%
%   Constructs a trigger from a list of conditions (triggers) and a consequent.
%   This predicate recursively processes the triggers and assembles the resulting
%   structure based on the nature of each trigger.
%
%   If a trigger is a test or involves Prolog constructs, it handles them
%   accordingly. Special treatment is given to negated triggers and variables.
%
%   @arg Triggers   The list of conditions (triggers) to process.
%   @arg Consequent The consequent to be associated with the triggers.
%   @arg Result     The resulting trigger structure.
%
%   @example
%     ?- buildTrigger([X > 0, foo], bar, Result).
%     Result = (X > 0 *-> '$pt$(foo, bar)').
%
buildTrigger([], Consequent, Consequent).
    % If no triggers are left, the result is simply the consequent.
buildTrigger([Test | Triggers], Consequent, (Test *-> X)) :-
    % If the current trigger is a Prolog construct, process it using `*->/2`.
    is_implicitly_prolog(Test),
    !,
    buildTrigger(Triggers, Consequent, X).
buildTrigger([V | Triggers], Consequent, '$pt$'(V, X)) :-
    % Handle variables as triggers by wrapping them in a `$pt$/2` structure.
    var(V),
    !,
    buildTrigger(Triggers, Consequent, X).
buildTrigger([(T1 / Test) | Triggers], Consequent, '$nt$'(T2, Test2, X)) :-
    % Handle negated triggers with a test. Use `pfc_unnegate/2` to remove negation.
    pfc_unnegate(T1, T2),
    !,
    buildNtTest(T2, Test, Test2),
    buildTrigger(Triggers, Consequent, X).
buildTrigger([(T1) | Triggers], Consequent, '$nt$'(T2, Test, X)) :-
    % Handle simple negated triggers without a test.
    pfc_unnegate(T1, T2),
    !,
    buildNtTest(T2, true, Test),
    buildTrigger(Triggers, Consequent, X).
buildTrigger([{Test} | Triggers], Consequent, (Test *-> X)) :-
    % Handle triggers wrapped in curly braces using `*->/2`.
    !,
    buildTrigger(Triggers, Consequent, X).
buildTrigger([T / Test | Triggers], Consequent, '$pt$'(T, X)) :-
    % Handle a trigger with a test.
    !,
    buildTest(Test, Test2),
    buildTrigger([{Test2} | Triggers], Consequent, X).
% buildTrigger([snip | Triggers], Consequent, snip(X)) :-
%   !,
%   buildTrigger(Triggers, Consequent, X).
buildTrigger([T | Triggers], Consequent, '$pt$'(T, X)) :-
    % Handle general triggers by wrapping them in a `$pt$/2` structure.
    !,
    buildTrigger(Triggers, Consequent, X).

%!  buildNtTest(+Trigger, +TestIn, -TestOut) is det.
%
%   Constructs the test used in a negative trigger ('$nt$'/3). The resulting
%   test is a conjunction of the check that no matching facts exist in the
%   database and any additional test specified in the rule associated with
%   the negated term.
%
%   This predicate ensures that negated conditions are correctly evaluated
%   by combining the `pfc_call/1` on the trigger with the additional test.
%
%   @arg Trigger  The negated trigger term.
%   @arg TestIn   The initial test term (if provided).
%   @arg TestOut  The resulting combined test.
%
%   @example
%     ?- buildNtTest(foo, X > 0, TestOut).
%     TestOut = (pfc_call(foo), X > 0).
%
buildNtTest(T, Testin, Testout) :-
    % Build the intermediate test from the input test.
    buildTest(Testin, Testmid),
    % Conjoin the call to `pfc_call/1` on the trigger with the intermediate test.
    pfcConjoin((pfc_call(T)), Testmid, Testout).

%!  buildTest(+InputTest, -OutputTest) is det.
%
%   Strips away any curly brackets from the given test term. If the input
%   is wrapped in curly brackets, it removes them and returns the inner term.
%
%   @arg InputTest   The original test term, possibly wrapped in curly brackets.
%   @arg OutputTest  The unwrapped test term.
%
%   @example
%     ?- buildTest({X > 0}, Test).
%     Test = (X > 0).
%
%     ?- buildTest(Y > 1, Test).
%     Test = (Y > 1).
%
buildTest({Test}, Test) :-
    % If the test is wrapped in curly brackets, remove them.
    !.
buildTest(Test, Test).
    % If the test is not wrapped, return it unchanged.


%!  pfcType(+Value, ?Type) is nondet.
%
%   Determines the PFC (Prolog Forward Chaining) database type for a given term.
%   This predicate performs simple typing for PFC objects, identifying whether
%   the term is a fact, rule, trigger, or action. It recursively inspects nested
%   terms and applies specific type rules.
%
%   @arg Value  The PFC object to type-check. This can be a fact, rule, trigger,
%               or action.
%   @arg Type   The resulting type of the given value. It is unified with a term
%               indicating the type, such as `fact/1`, `rule/1`, or `trigger/1`.
%
%   @example
%     ?- pfcType(foo, Type).
%     Type = fact(_FT).
%
%     ?- pfcType('==>'(a, b), Type).
%     Type = rule(fwd).
%
pfcType(Var, Type) :-
    % If the input is a variable, type it as a `fact/1`.
    var(Var), !, Type = fact(_FT).
pfcType(_:X, Type) :-
    % Handle module-qualified terms by stripping the module prefix.
    !, pfcType(X, Type).
pfcType(~_, Type) :-
    % Handle negated facts, typing them as `fact/1`.
    !, Type = fact(_FT).
pfcType(('==>'(_, _)), Type) :-
    % Identify forward rules using `==>/2`.
    !, Type = rule(fwd).
pfcType('==>'(X), Type) :-
    % Handle improperly formatted forward rules and warn.
    !, pfcType(X, Type),pfcWarn(pfcType('==>'(X), Type)).
pfcType(('<==>'(_, _)), Type) :-
    % Identify bi-conditional rules using `<==>/2`.
    !, Type = rule(<==>).
pfcType(('<-'(_, _)), Type) :-
    % Identify backward rules using `<-/2`.
    !, Type = rule(bwc).
pfcType((':-'(_, _)), Type) :-
    % Identify conditional rules using `:-/2`.
    !, Type = rule(cwc).
pfcType('$pt$'(_, _, _), Type) :-
    % Identify positive triggers using `$pt$/3`.
    !, Type = trigger(+).
pfcType('$pt$'(_, _), Type) :-
    % Identify positive triggers using `$pt$/2`.
    !, Type = trigger(+).
pfcType('$nt$'(_, _, _), Type) :-
    % Identify negative triggers using `$nt$/3`.
    !, Type = trigger(-).
pfcType('$bt$'(_, _), Type) :-
    % Identify neutral triggers using `$bt$/2`.
    !, Type = trigger(?).
pfcType(pfcAction(_), Type) :-
    % Identify actions using `pfcAction/1`.
    !, Type = action.
pfcType((('::::'(_, X))), Type) :-
    % Recursively process terms prefixed with `::::`.
    !,pfcType(X, Type).
pfcType(_, fact(_FT)) :-
    % If none of the above cases apply, type the term as a FACT!
    % This assumes the term is a fact by default.
    !.

%!  pfcAssert(+P, +Support) is det.
%
%   Asserts a clause or fact `P` into the database with associated support.
%   If the clause or fact already exists, it is not re-asserted, but the
%   support information is added.
%
%   @arg P        The clause or fact to assert.
%   @arg Support  The supporting information to associate with the clause or fact.
%
%   @example
%     ?- pfcAssert(foo, support_info).
%
pfcAssert(P, Support) :-
    % If the clause already exists, skip assertion; otherwise, assert it.
    (pfc_clause(P) ; assert(P)),
    !,
    % Add support information to the asserted clause or fact.
    pfcAddSupport(P, Support).

%!  pfcAsserta(+P, +Support) is det.
%
%   Asserts a clause or fact `P` to the beginning of the database with support.
%   If it already exists, it is not re-asserted, but the support information is added.
%
%   @arg P        The clause or fact to assert.
%   @arg Support  The supporting information to associate with the clause or fact.
%
%   @example
%     ?- pfcAsserta(foo, support_info).
%
pfcAsserta(P, Support) :-
    (pfc_clause(P) ; asserta(P)),!,pfcAddSupport(P, Support).

%!  pfcAssertz(+P, +Support) is det.
%
%   Asserts a clause or fact `P` to the end of the database with support.
%   If it already exists, it is not re-asserted, but the support information is added.
%
%   @arg P        The clause or fact to assert.
%   @arg Support  The supporting information to associate with the clause or fact.
%
%   @example
%     ?- pfcAssertz(foo, support_info).
%
pfcAssertz(P, Support) :-
    (pfc_clause(P) ; assertz(P)),!,pfcAddSupport(P, Support).

%!  pfc_clause(+Clause) is nondet.
%
%   Checks if the given clause already exists in the database. It ensures the clause
%   matches exactly by comparing terms after copying them.
%
%   @arg Clause  The clause to check for existence in the database.
%
%   @example
%     ?- pfc_clause((foo :- bar)).
%     true.
%
pfc_clause((Head :- Body)) :-
    !,
    % Copy the Head and Body to prevent unwanted unification.
    copy_term(Head, Head_copy),
    copy_term(Body, Body_copy),
    % Check if a matching clause exists in the database.
    clause(Head, Body),
    % Ensure the copied terms are variants of the original.
    variant(Head, Head_copy),
    variant(Body, Body_copy).
pfc_clause(Head) :-
    % Handle unit clauses without a body.
    copy_term(Head, Head_copy),
    clause(Head_copy, true),
    variant(Head, Head_copy).

%!  pfcForEach(+Binder, +Body) is det.
%
%   Executes the `Body` for each solution of the `Binder`. If no solutions exist,
%   it succeeds without side effects.
%
%   @arg Binder  A Prolog goal that generates solutions.
%   @arg Body    The body to execute for each solution.
%
%   @example
%     ?- pfcForEach(member(X, [1,2,3]), writeln(X)).
%     1
%     2
%     3
%
pfcForEach(Binder, Body) :-
    % Execute the body for each solution of the binder.
    Binder,
    pfcdo(Body),
    fail.
pfcForEach(_, _).

%!  pfcdo(+Goal) is det.
%
%   Executes a goal once and always succeeds, even if the goal fails.
%
%   @arg Goal  The goal to execute.
%
%   @example
%     ?- pfcdo(writeln('Hello, World!')).
%     Hello, World!
%     true.
%
pfcdo(X) :-
    % Execute the goal. If it succeeds, cut to avoid backtracking.
    X, !.
pfcdo(_).
    % If the goal fails, succeed without side effects.

%!  pfcUnion(+List1, +List2, -Union) is det.
%
%   Computes the union of two sets represented as lists. Duplicates are removed
%   by checking membership before adding elements.
%
%   @arg List1  The first input list.
%   @arg List2  The second input list.
%   @arg Union  The resulting union of the two input lists.
%
%   @example
%     ?- pfcUnion([a, b], [b, c], Union).
%     Union = [a, b, c].
%
pfcUnion([], L, L).
    % If the first list is empty, the union is the second list.
pfcUnion([Head | Tail], L, Tail2) :-
    % If the head of the first list is already in the second list, skip it.
    memberchk(Head, L),!,pfcUnion(Tail, L, Tail2).
pfcUnion([Head | Tail], L, [Head | Tail2]) :-
    % Otherwise, add the head to the result and continue processing.
    pfcUnion(Tail, L, Tail2).

%!  pfcConjoin(+Conjunct1, +Conjunct2, -Conjunction) is det.
%
%   Simplifies the conjunction of two expressions. If either conjunct
%   is `true`, the result is the other conjunct. Otherwise, the result
%   is the conjunction of both expressions.
%
%   @arg Conjunct1    The first expression to conjoin.
%   @arg Conjunct2    The second expression to conjoin.
%   @arg Conjunction  The resulting simplified conjunction of the two expressions.
%
%   @example
%     % Simplify conjunction with true:
%     ?- pfcConjoin(true, X > 0, Result).
%     Result = (X > 0).
%
%     % Conjoin two non-trivial expressions:
%     ?- pfcConjoin(X > 0, Y < 10, Result).
%     Result = ((X > 0), (Y < 10)).
%
pfcConjoin(true, X, X) :-
    % If the first conjunct is `true`, return the second conjunct.
    !.
pfcConjoin(X, true, X) :-
    % If the second conjunct is `true`, return the first conjunct.
    !.
pfcConjoin(C1, C2, (C1, C2)).
    % If both conjuncts are non-trivial, return their conjunction.


%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
% %     restore, reset, etc.0

%!  pfcDatabaseTerm(+PA) is nondet.
%
%   True if `PA` is a term that PFC adds to the database and should not
%   be present in an empty PFC database. This predicate identifies terms
%   that are specific to the PFC system, such as rules, triggers, and queues.
%
%   @arg P/A  A functor/arity pair representing a term in the PFC database.
%
%   @example
%     ?- pfcDatabaseTerm('$pt$'/2).
%     true.
%
pfcDatabaseTerm('$spft$'/3).
pfcDatabaseTerm('$pt$'/2).
pfcDatabaseTerm('$bt$'/2).
pfcDatabaseTerm('$nt$'/3).
pfcDatabaseTerm('==>'/2).
pfcDatabaseTerm('<==>'/2).
pfcDatabaseTerm('<-'/2).
pfcDatabaseTerm(pfcQueue/1).

%!  pfcReset is det.
%
%   Removes all forward chaining rules and justifications from the database.
%   If the database is not empty after attempting to reset, an error is raised.
%
%   @example
%     ?- pfcReset.
%
pfcReset :-
    % Iterate over all PFC facts and justifications to remove them.
    pfc_spft(P, F, Trigger),
    pfcRetractOrWarn(P),
    pfcRetractOrWarn('$spft$'(P, F, Trigger)),
    fail.
pfcReset :-
    % If the database is not empty, raise an error.
    (pfcDatabaseItem(T) *->
        (pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n", [T]), fail)
    ;
        true).

%!  pfcDatabaseItem(:TermBody) is nondet.
%
%   True if the given `Term :- Body` is a "PFC-specific" item in the database.
%   These items correspond to terms that PFC adds, which should not be present
%   in an empty database.
%
%   @arg Term  The term representing a PFC database entry.
%   @arg Body  The body of the clause associated with the term.
%
%   @example
%     ?- pfcDatabaseItem(Term :- Body).
%
pfcDatabaseItem(Term :- Body) :-
    % Check if the term matches a known PFC database term.
    pfcDatabaseTerm(P/A),
    functor(Term, P, A),
    % Ensure the clause exists in the database.
    clause(Term, Body).

%!  pfcRetractOrWarn(+X) is det.
%
%   Retracts the given clause or fact `X` from the database. If the retraction
%   fails, a warning message is generated.
%
%   @arg X  The clause or fact to retract.
%
%   @example
%     ?- pfcRetractOrWarn(foo).
%
pfcRetractOrWarn(X) :-
    % Attempt to retract the clause or fact.
    retract(X),
    !.
pfcRetractOrWarn(X) :-
    % If retraction fails, issue a warning.
    pfcWarn("Couldn't retract ~p.", [X]),
    nop((dumpST, pfcWarn("Couldn't retract ~p.", [X]))),
    !.

%!  pfcRetractOrQuietlyFail(+X) is nondet.
%
%   Attempts to retract the given clause or fact `X`. If retraction fails,
%   the operation quietly fails without raising an error or warning.
%
%   @arg X  The clause or fact to retract.
%
%   @example
%     ?- pfcRetractOrQuietlyFail(foo).
%
pfcRetractOrQuietlyFail(X) :-
    % Attempt to retract the clause or fact.
    retract(X),
    !.
pfcRetractOrQuietlyFail(X) :-
    % If retraction fails, log a trace message and quietly fail.
    nop((pfcTraceMsg("Trace: Couldn't retract ~p.", [X]),
        nop((dumpST, pfcWarn("Couldn't retract ~p.", [X]))))),
    !,
    fail.

