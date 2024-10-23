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
% PROGRAM FUNCTION: provides support for logical rules, triggers, and fact management with 
% features like dependency tracking and truth maintenance.
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

%!  control_arg_types(+A, +B) is semidet.
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


%!  arg_n_isa(+F, +N, -ISA) is semidet.
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

%!  clause_u(+Head, -Body) is semidet.
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

%!  mpred_literal(+X) is semidet.
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

%!  mpred_positive_literal(+X) is semidet.
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

%!  pfcAtom(+X) is semidet.
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
%   Detached threads automatically reclaim their resources upon termination.
%
%   @arg Pool The pool name to be created, here `ain_pool`.
%
thread_pool:create_pool(ain_pool) :-
    % Create the thread pool with 50 threads and detached mode enabled.
    thread_pool_create(ain_pool, 50, [detached(true)]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(thread_pool)).

%!  is_ain_pool_empty is semidet.
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
            ( M:G -> nop(dmsg_pretty(succeeded(exit, TN)))
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
:- op(1050, xfx, '==>').       % Declares '==>' as an infix operator with precedence 1050.
:- op(1050, xfx, '<==>').      % Declares '<==>' as an infix operator with precedence 1050.
:- op(1050, xfx, '<-').        % Declares '<-' as an infix operator with precedence 1050.
:- op(1100, fx, '==>').        % Declares '==>' as a prefix operator with precedence 1100.
:- op(1150, xfx, '::::').      % Declares '::::' as an infix operator with precedence 1150.


% declare that pfctmp:knows_will_table_as/2 can be modified at runtime.
:- dynamic(pfctmp:knows_will_table_as/2).

%!  will_table_as(+Stuff, +As) is semidet.
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

%!  if_pfc_indicated is semidet.
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

%!  skip_pfc_term_expansion(+Term) is semidet.
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

% % % initialization of global assertons

pfcSetVal(Stuff):-
   duplicate_term(Stuff,DStuff),
   functor(DStuff,_,N),
   setarg(N,DStuff,_),
   retractall(DStuff),
   assert(Stuff).

% %  pfcDefault/1 initialized a global assertion.
% %   pfcDefault(P,Q) - if there is any fact unifying with P, then do
% %   nothing, else assert Q.

pfcDefault(GeneralTerm,Default) :-
  clause(GeneralTerm,true) -> true ; assert(Default).

% %  fcTmsMode is one of {none,local,cycles} and controles the tms alg.
:- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).

% Pfc Search strategy. pfcSearch(X) where X is one of {direct,depth,breadth}
:- pfcDefault(pfcSearch(_), pfcSearch(direct)).


% 

% %  pfcAdd/2 and pfcPost/2 are the main ways to assert new clauses into the
% %  database and have forward reasoning done.

% %  pfcAdd(P,S) asserts P into the dataBase with support from S.

pfcAdd(P) :- must_ex(current_why_UU(UU)),
  pfcAdd(P, UU).

%pfcAdd(P) :- must_ex(current_why_UU(UU)),%with_current_why(pfcAdd(P), pfcAdd(P, UU)).

pfcAdd((==>P),S) :- !, pfcAdd(P,S).

pfcAdd(P,S) :-
  pfcPost(P,S),
  pfcRun,!.

%pfcAdd(_,_).
pfcAdd(P,S) :- pfcWarn("pfcAdd(~p,~p) failed",[P,S]).


% pfcPost(+Ps,+S) tries to add a fact or set of fact to the database.  For
% each fact (or the singelton) pfcPost1 is called. It always succeeds.

pfcPost(List,S):- pfcPost_rev(S,List).

pfcPost_rev(S,Term) :-
  is_list(Term)
  -> my_maplist(pfcPost_rev(S),Term)
  ; pfcPost1(Term,S).


% pfcPost1(+P,+S) tries to add a fact to the database, and, if it succeeded,
% adds an entry to the pfc queue for subsequent forward chaining.
% It always succeeds.

pfcPost1(Fact,S) :- control_arg_types(Fact,Fixed),!,pfcPost1(Fixed,S).


pfcPost1(P,S):-
  locally(set_prolog_flag(occurs_check, true),
    catch(pfcPost11(P,S),E,(notrace,wdmsg(P => E),trace))).

pfcPost11(P,S) :-
  % %  db pfcAddDbToHead(P,P2),
  % pfcRemoveOldVersion(P),
  must_ex(pfcAddSupport(P,S)),
  (pfcUnique(post, P)-> pfcPost2(P,S) ; nop(pfcWarn(not_pfcUnique(post, P)))).

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
pfcUnique(Type, P) :- pfcUnique(Type,P,true).

%pfcUnique(post,Head,Tail):- !, \+ is_clause_asserted(Head,Tail).
pfcUnique(_,Head,Tail):- \+ is_asserted_exact(Head,Tail),!.
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


% %  pfcEnqueue(P,Q) is det.
%
% Enqueu according to settings
%
pfcSetSearch(Mode):- pfcSetVal(pfcSearch(Mode)).

pfcGetSearch(Mode):- (t_l:pfcSearchTL(ModeT)->true;pfcSearch(ModeT))->Mode=ModeT.

pfcEnqueue(P,S) :- pfcGetSearch(Mode),!,
   pfcEnqueue(Mode,P,S).
pfcEnqueue(P,S) :- pfcWarn("No pfcSearch mode"),
   pfcEnqueue(direct,P,S).

pfcEnqueue(Mode,P,S):-
    Mode=direct  -> pfcFwd(P) ;
    Mode=thread  -> pfcThreadFwd(P,S) ;
    Mode=depth   -> pfcAsserta(pfcQueue(P),S) ;
    Mode=breadth -> pfcAssert(pfcQueue(P),S) ;
    true         -> pfcWarn("Unrecognized pfcSearch mode: ~p", Mode),pfcEnqueue(direct,P,S).



% %  pfcRemoveOldVersion(+Rule) is det.
%
% if there is a rule of the form Identifier ::: Rule then delete it.

pfcRemoveOldVersion((Identifier::::Body)) :-
  % this should never happen.
  (var(Identifier)
  ->
  pfcWarn("variable used as an  rule name in ~p :::: ~p",
          [Identifier,Body]);
  pfcRemoveOldVersion0(Identifier::::Body)).


pfcRemoveOldVersion0((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody),_),
  \+(Body=OldBody),
  pfcWithdraw((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion0(_).


% %  with_fc_mode(+Mode,:Goal) is semidet.
%
% Temporariliy changes to forward chaining propagation mode while running the Goal
%
with_fc_mode(Mode,Goal):- locally(t_l:pfcSearchTL(Mode),Goal).


pfcThreadFwd(S,P):-
      with_only_current_why(S,
       % maybe keep `thread` mode?
        call_in_thread(with_fc_mode(thread, (pfcFwd(P))))).

% in_fc_call(Goal):- with_fc_mode( thread, Goal).
%in_fc_call(Goal):- with_fc_mode( direct, Goal).
% in_fc_call(Goal):- !, pfcCallSystem(Goal).




% 

% pfcRun compute the deductive closure of the current database.
% How this is done depends on the searching mode:
%    direct -  fc has already done the job.
%    depth or breadth - use the pfcQueue mechanism.

pfcRun :-
  (\+ pfcGetSearch(direct)),
  pfcStep,
  pfcRun.
pfcRun.


% pfcStep removes one entry from the pfcQueue and reasons from it.


pfcStep :-
  % if pfcHaltSignal(Msg) is true, reset it and fail, thereby stopping inferencing.
  pfcRetract(pfcHaltSignal(Msg)),
  pfcTraceMsg(removing(pfcHaltSignal(Msg))),
  !,
  fail.

pfcStep :-
  % draw immediate conclusions from the next fact to be considered.
  % fails iff the queue is empty.
  get_next_fact(P),
  pfcdo(pfcFwd(P)),
  !.

get_next_fact(P) :-
  %identifies the nect fact to fc from and removes it from the queue.
  select_next_fact(P),
  remove_selection(P).

remove_selection(P) :-
  pfcRetract(pfcQueue(P)),
  pfcRemoveSupportsQuietly(pfcQueue(P)),
  !.
remove_selection(P) :-
  brake(pfcPrintf("pfc:get_next_fact - selected fact not on Queue: ~p",
               [P])).


% select_next_fact(P) identifies the next fact to reason from.
% It tries the user defined predicate first and, failing that,
%  the default mechanism.

select_next_fact(P) :-
  pfcSelect(P),
  !.
select_next_fact(P) :-
  defaultpfcSelect(P),
  !.

% the default selection predicate takes the item at the froint of the queue.
defaultpfcSelect(P) :- pfcCallSystem(pfcQueue(P)),!.

% pfcHalt stops the forward chaining.
pfcHalt :-  pfcHalt("unknown_reason",[]).

pfcHalt(Format) :- pfcHalt(Format,[]).

pfcHalt(Format,Args) :-
  format(string(Msg),Format,Args),
  (pfcHaltSignal(Msg) ->
       pfcWarn("pfcHalt finds pfcHaltSignal(~w) already set",[Msg])
     ; assert(pfcHaltSignal(Msg))).


% % 
% %
% %  predicates for manipulating triggers
% %

pfcAddTrigger('$pt$'(Trigger,Body),Support) :-
  !,
  pfcTraceMsg('      Adding positive trigger(+) ~p~n',
        ['$pt$'(Trigger,Body)]),
  pfcAssert('$pt$'(Trigger,Body),Support),
  copy_term('$pt$'(Trigger,Body),Tcopy),
  pfc_call(Trigger),
  with_current_why(Trigger,fcEvalLHS(Body,(Trigger,Tcopy))),
  fail.


pfcAddTrigger('$nt$'(Trigger,Test,Body),Support) :-
  !,
  pfcTraceMsg('      Adding negative trigger(-): ~p~n       test: ~p~n       body: ~p~n',
        [Trigger,Test,Body]),
  copy_term(Trigger,TriggerCopy),
  pfcAssert('$nt$'(TriggerCopy,Test,Body),Support),
  \+ pfc_call(Test),
  with_current_why(\+ pfc_call(Test), fcEvalLHS(Body,((\+Trigger),'$nt$'(TriggerCopy,Test,Body)))).

pfcAddTrigger('$bt$'(Trigger,Body),Support) :-
  !,
  pfcAssert('$bt$'(Trigger,Body),Support),
  pfcBtPtCombine(Trigger,Body,Support).

pfcAddTrigger(X,_Support) :-
  pfcWarn("Unrecognized trigger(?) to pfcAddtrigger: ~p",[X]).


pfcBtPtCombine(Head,Body,Support) :-
  % %  a backward trigger(?) ('$bt$') was just added with head and Body and support Support
  % %  find any '$pt$'(s) with unifying heads and add the instantied '$bt$' body.
  pfcGetTriggerQuick('$pt$'(Head,_PtBody)),
  fcEvalLHS(Body,Support),
  fail.
pfcBtPtCombine(_,_,_) :- !.

pfcGetTriggerQuick(Trigger) :-  clause(Trigger,true)*->true;pfc_call(Trigger).
pfcCallSystem(Trigger) :-  pfc_call(Trigger).

% % 
% %
% %  predicates for manipulating action traces.
% %

pfcAddActionTrace(Action,Support) :-
  % adds an action trace and it''s support.
  pfcAddSupport(pfcAction(Action),Support).

pfcRemActionTrace(pfcAction(A)) :-
  fcUndoMethod(A,UndoMethod),
  pfcCallSystem(UndoMethod),
  !.


% % 
% %  predicates to remove pfc facts, triggers, action traces, and queue items
% %  from the database.
% %

pfcRetract(X) :-
  % %  retract an arbitrary thing.
  pfcType(X,Type),
  pfcRetractType(Type,X),
  !.

pfcRetractType(fact(_),X) :-
  % %  db
  pfcAddDbToHead(X,X2)-> retract(X2) ; retract(X).

pfcRetractType(rule(_),X) :-
  % %  db
  pfcAddDbToHead(X,X2) ->  retract(X2) ; retract(X).

pfcRetractType(trigger(Pos),X) :-
  retract(X)
    -> unFc(X)
     ; pfcWarn("Trigger(~p) not found to retract: ~p",[Pos,X]).

pfcRetractType(action,X) :- pfcRemActionTrace(X).


% %  pfcAddType1(X) adds item X to some database

pfcAddType1(X) :-
  % what type of X do we have?
  pfcType(X,Type),
  pfcAddDbToHead(X,X2),
  % call the appropriate predicate.
  pfcAddType(Type,X2).

pfcAddType(fact(Type),X) :-
  pfcUnique(fact(Type),X),
  assert(X),!.
pfcAddType(rule(Type),X) :-
  pfcUnique(rule(Type),X),
  assert(X),!.
pfcAddType(trigger(Pos),X) :-
  pfcUnique(trigger(Pos),X) -> assert(X) ;
   (pfcWarn(not_pfcUnique(X)),assert(X)).

pfcAddType(action,_Action) :- !.




% pfcWithdraw/1  withdraws any "direct" support for P.
% If a list, iterates down the list
pfcWithdraw(P) :- is_list(P),!,my_maplist(pfcWithdraw,P).
pfcWithdraw(P) :- matches_why_UU(UU), pfcWithdraw(P,UU).
% %  pfcWithdraw(P,S) removes support S from P and checks to see if P is still supported.
% %  If it is not, then the fact is retractred from the database and any support
% %  relationships it participated in removed.
pfcWithdraw(P,S) :-
  % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  pfcGetSupport(P,S),
  matterialize_support_term(S,Sup),
  pfcTraceMsg('    Withdrawing direct support: ~p   \n   From: ~p~n',[Sup,P]),
   (pfcRemOneSupportOrQuietlyFail(P,S)
      -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
      ; pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not pfcRemOneSupport: ~p",
                 [Sup,P])),
   removeIfUnsupported(P).

pfcWithdraw(P,S) :-
  matterialize_support_term(S,Sup),
  pfcTraceMsg('    No support matching: ~p   \n   For: ~p~n',[Sup,P]),!,
  removeIfUnsupported(P).

% pfcRetractAll/1  withdraws any "direct" and "indirect" support for P.
% If a list, iterates down the list
pfcRetractAll(P) :- is_list(P),!,my_maplist(pfcRetractAll,P).
pfcRetractAll(P) :- matches_why_UU(UU), pfcRetractAll(P,UU).

% %  pfcRetractAll(P,S) removes support S from P and checks to see if P is still supported.
% %  If it is not, then the fact is retreactred from the database and any support
% %  relationships it participated in removed.

pfcRetractAll(Fact,S) :- control_arg_types(Fact,Fixed),!,pfcRetractAll(Fixed,S).
pfcRetractAll(P,S) :-
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
  removeIfUnsupported(P),
  fail.
pfcRetractAll(_,_).


pfcSupportedBy(P,S,How):-
   pfcGetSupport(P,(F,T)),
   (pfcSupportedBy(F,S,_)->How=F;
   pfcSupportedBy(T,S,How)).

pfcSupportedBy(P,S,How):-P=S,How=S.

pfcRetractAll_v2(P,S0) :-
  \+ \+ pfcWithdraw(P,S0),
  pfcGetSupport(P,(S,RemoveIfTrigger)),
  % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  matterialize_support_term((S,RemoveIfTrigger),Sup),
  pfcTraceMsg('    Removing support: ~p   \n   From: ~p~n',[Sup,P]),
  (pfcRemOneSupportOrQuietlyFail(P,(S,RemoveIfTrigger))
     -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
     ; (pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not yet pfcRetractAll_v2: ~p",
                [Sup,P]))),
  pfcRetractAll_v2(S, S0),
  fail.

pfcRetractAll_v2(P,_):- removeIfUnsupported(P).

% pfcRemove/1 is the user''s interface - it withdraws user support for P.
%
% pfcRemove is like pfcRetractAll, but if P is still in the DB after removing the
% user's support, it is retracted by more forceful means (e.g. pfcBlast).
%
pfcRemove(Fact) :- control_arg_types(Fact,Fixed),!,pfcRemove(Fixed).
pfcRemove(P) :-
  pfcRetractAll(P),
  pfc_call(P)
     -> pfcBlast(P)
      ; true.


% %  pfcBlast(+F) is det
%
% retracts fact F from the DB and removes any dependent facts
%

pfcBlast(F) :-
  pfcRemoveSupports(F),
  fcUndo(F).


% removes any remaining supports for fact F, complaining as it goes.

pfcRemoveSupports(F) :-
  pfcRemOneSupport(F,S),
  pfcWarn("~p was still supported by ~p (but no longer)",[F,S]),
  fail.
pfcRemoveSupports(_).

pfcRemoveSupportsQuietly(F) :-
  pfcRemOneSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

% fcUndo(X) undoes X.


fcUndo(pfcAction(A)) :-
  % undo an action by finding a method and successfully executing it.
  !,
  pfcRemActionTrace(pfcAction(A)).

fcUndo('$pt$'(/*Key,*/Head,Body)) :-
  % undo a positive trigger(+).
  %
  !,
  (retract('$pt$'(/*Key,*/Head,Body))
    -> unFc('$pt$'(Head,Body))
     ; pfcWarn("Trigger not found to retract: ~p",['$pt$'(Head,Body)])).

fcUndo('$nt$'(Head,Condition,Body)) :-
  % undo a negative trigger(-).
  !,
  (retract('$nt$'(Head,Condition,Body))
    -> unFc('$nt$'(Head,Condition,Body))
     ; pfcWarn("Trigger not found to retract: ~p",['$nt$'(Head,Condition,Body)])).

fcUndo(Fact) :-
  % undo a random fact, printing out the trace, if relevant.
  retract(Fact),
  pfcTraceRem(Fact),
  unFc(Fact).


% %  unFc(P) is det.
%
% unFc(P) "un-forward-chains" from fact f.  That is, fact F has just
% been removed from the database, so remove all dependant relations it
% participates in and check the things that they support to see if they
% should stayu in the database or should also be removed.


unFc(F) :-
  pfcRetractDependantRelations(F),
  unFc1(F).

unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  pfcRun.


pfcUnFcCheckTriggers(F) :-
  pfcType(F,fact(_)),
  copy_term(F,Fcopy),
  pfcCallSystem('$nt$'(Fcopy,Condition,Action)),
  (\+ pfcCallSystem(Condition)),
  fcEvalLHS(Action,((\+F),'$nt$'(F,Condition,Action))),
  fail.
pfcUnFcCheckTriggers(_).

pfcRetractDependantRelations(Fact) :-
  pfcType(Fact,Type),
  (Type=trigger(_Pos) -> pfcRemOneSupport(P,(_,Fact))
                ; pfcRemOneSupportOrQuietlyFail(P,(Fact,_))),
  removeIfUnsupported(P),
  fail.
pfcRetractDependantRelations(_).



% %  removeIfUnsupported(+P) checks to see if P is supported and removes
% %  it from the DB if it is not.

removeIfUnsupported(P) :-
   fcSupported(P) -> pfcTraceMsg(fcSupported(P)) ;  fcUndo(P).


% %  fcSupported(+P) succeeds if P is "supported". What this means
% %  depends on the TMS mode selected.

fcSupported(P) :-
  must_ex(fcTmsMode(Mode)),
  supported(Mode,P).

supported(local,P) :- !, pfcGetSupport(P,_).
supported(cycles,P) :-  !, wellFounded(P).
supported(_,_P) :- true.


% % 
% %  a fact is well founded if it is supported by the user
% %  or by a set of facts and a rules, all of which are well founded.
% %

wellFounded(Fact) :- wf(Fact,[]).

wf(F,_) :-
  % supported by user (axiom) or an "absent" fact (assumption).
  (axiom(F) ; assumption(F)),
  !.

wf(F,Descendants) :-
  % first make sure we aren't in a loop.
  (\+ memberchk(F,Descendants)),
  % find a justification.
  supports(F,Supporters),
  % all of whose members are well founded.
  wflist(Supporters,[F|Descendants]),
  !.

% %  wflist(L) simply maps wf over the list.

wflist([],_).
wflist([X|Rest],L) :-
  wf(X,L),
  wflist(Rest,L).



% supports(+F,-ListofSupporters) where ListOfSupports is a list of the
% supports for one justification for fact F -- i.e. a list of facts which,
% together allow one to deduce F.  One of the facts will typically be a rule.
% The supports for a user-defined fact are: [user].

supports(F,[Fact|MoreFacts]) :-
  pfcGetSupport(F,(Fact,Trigger)),
  triggerSupports(Trigger,MoreFacts).

triggerSupports(U,[]) :- axiomatic_supporter(U),!.

triggerSupports(Trigger,AllSupport):-
  triggerSupports1(Trigger,AllSupport)*->true;triggerSupports2(Trigger,AllSupport).

triggerSupports1(Trigger,AllSupport) :-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  (triggerSupports(AnotherTrigger,MoreFacts)*->true;MoreFacts=[AnotherTrigger]),
  [Fact|MoreFacts] = AllSupport.

triggerSupports2(Trigger,AllSupport) :- fail,
  pfcGetSupport(Trigger,(Fact,AnotherTrigger)),
  (triggerSupports(AnotherTrigger,MoreFacts)*->true;MoreFacts=[AnotherTrigger]),
  [Fact|MoreFacts] = AllSupport.

axiomatic_supporter(Var):-is_ftVar(Var),!,fail.
axiomatic_supporter(is_ftVar(_)).
axiomatic_supporter(clause_u(_)).
axiomatic_supporter(user(_)).
axiomatic_supporter(U):- is_file_ref(U),!.
axiomatic_supporter(ax):-!.

is_file_ref(A):-compound(A),A=mfl4(_VarNameZ,_,_,_).

triggerSupports(_,Var,[is_ftVar(Var)]):-is_ftVar(Var),!.
triggerSupports(_,U,[]):- axiomatic_supporter(U),!.
triggerSupports(FactIn,Trigger,OUT):-
  pfcGetSupport(Trigger,(Fact,AnotherTrigger))*->
  (triggerSupports(Fact,AnotherTrigger,MoreFacts),OUT=[Fact|MoreFacts]);
  triggerSupports1(FactIn,Trigger,OUT).

triggerSupports1(_,X,[X]):- may_cheat.
may_cheat:- true_flag.



% % 
% %
% %  pfcFwd(X) forward chains from a fact or a list of facts X.
% %
pfcFwd(Fact) :- control_arg_types(Fact,Fixed),!,pfcFwd(Fixed).
pfcFwd(Fact):- locally(set_prolog_flag(occurs_check,true), pfcFwd0(Fact)).
pfcFwd0(Fact) :- is_list(List)->my_maplist(pfcFwd0,List);pfcFwd1(Fact).

% fc1(+P) forward chains for a single fact.


pfcFwd1(Fact) :-
  (fc_rule_check(Fact)*->true;true),
  copy_term(Fact,F),
  % check positive triggers
  ignore(fcpt(Fact,F)),
  % check negative triggers
  ignore(fcnt(Fact,F)).


% %
% %  fc_rule_check(P) does some special, built in forward chaining if P is
% %  a rule.
% %

fc_rule_check((Name::::P==>Q)) :-
  !,
  processRule(P,Q,(Name::::P==>Q)).
fc_rule_check((Name::::P<==>Q)) :-
  !,
  processRule(P,Q,((Name::::P<==>Q))),
  processRule(Q,P,((Name::::P<==>Q))).



fc_rule_check((P==>Q)) :-
  !,
  processRule(P,Q,(P==>Q)).
fc_rule_check((P<==>Q)) :-
  !,
  processRule(P,Q,(P<==>Q)),
  processRule(Q,P,(P<==>Q)).

fc_rule_check(('<-'(P,Q))) :-
  !,
  pfcDefineBcRule(P,Q,('<-'(P,Q))).

fc_rule_check(_).


fcpt(Fact,F) :-
  pfcGetTriggerQuick('$pt$'(F,Body)),
  pfcTraceMsg('\n Found positive trigger(+):\n    ~p~n       body: ~p~n',
        [F,Body]),
  pfcGetSupport('$pt$'(F,Body),Support), %fbugio(pfcGetSupport('$pt$'(F,Body),Support)),
  with_current_why(Support,with_current_why(Fact,fcEvalLHS(Body,(Fact,'$pt$'(F,Body))))),
  fail.

%fcpt(Fact,F) :-
%  pfcGetTriggerQuick('$pt$'(presently(F),Body)),
%  fcEvalLHS(Body,(presently(Fact),'$pt$'(presently(F),Body))),
%  fail.

fcpt(_,_).

fcnt(_Fact,F) :-
  pfc_spft(X,_,'$nt$'(F,Condition,Body)),
  pfcCallSystem(Condition),
  pfcRem_S(X,(_,'$nt$'(F,Condition,Body))),
  fail.
fcnt(_,_).


% %  pfcRem_S(P,S) removes support S from P and checks to see if P is still supported.
% %  If it is not, then the fact is retreactred from the database and any support
% %  relationships it participated in removed.
pfcRem_S(P,S) :-
  % pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])),
  pfcTraceMsg('    Removing support: ~p from ~p~n',[S,P]),
  pfcRemOneSupport(P,S)
     -> removeIfUnsupported(P)
      ; pfcWarn("pfcRem_S/2 Could not find support ~p to remove from fact ~p",
                [S,P]).



% %  pfcDefineBcRule(+Head,+Body,+ParentRule)
%
% defines a backward
% chaining rule and adds the corresponding '$bt$' triggers to the database.
%

pfcDefineBcRule(Head,_Body,ParentRule) :-
  (\+ pfcLiteral(Head)),
  pfcWarn("Malformed backward chaining rule.  ~p not atomic literal.",[Head]),
  pfcError("caused by rule: ~p",[ParentRule]),
  !,
  fail.

pfcDefineBcRule(Head,Body,ParentRule) :-
  copy_term(ParentRule,ParentRuleCopy),
  buildRhs(Head,Rhs),
  current_why_U(USER), % @TODO REVIEW _U
  pfcForEach(pfc_nf(Body,Lhs),
          (buildTrigger(Lhs,rhs(Rhs),Trigger),
           pfcAdd('$bt$'(Head,Trigger),(ParentRuleCopy,USER)))).
get_bc_clause(Head,(HeadC:- BodyC)):- get_bc_clause(Head,HeadC,BodyC).

get_bc_clause(HeadIn, ~HeadC, Body):- compound(HeadIn), HeadIn = ~Head,!,
     Body = ( awc,
            ( nonvar(HeadC)-> (HeadC = Head,!) ; (HeadC = Head)),
              pfc_bc_and_with_pfc(~Head)).
get_bc_clause(Head, Head, Body):-  % % :- is_ftNonvar(Head).
     Body = ( awc, !, pfc_bc_and_with_pfc(Head)).

:- thread_initialization(nb_setval('$pfc_current_choice',[])).

push_current_choice:- current_prolog_flag(pfc_support_cut,false),!.
push_current_choice:- prolog_current_choice(CP),push_current_choice(CP),!.
push_current_choice(CP):- nb_current('$pfc_current_choice',Was)->b_setval('$pfc_current_choice',[CP|Was]);b_setval('$pfc_current_choice',[CP]).

cut_c:- current_prolog_flag(pfc_support_cut,false),!.
cut_c:- must_ex(nb_current('$pfc_current_choice',[CP|_WAS])),prolog_cut_to(CP).


% % 
% %
% %  eval something on the LHS of a rule.
% %


fcEvalLHS((Test->Body),Support) :-
  !,
  pfcDoAll(pfcCallSystem(Test) -> (fcEvalLHS(Body,Support))),
  !.

fcEvalLHS((Test*->Body),Support) :-
  !,
  pfcDoAll(pfcCallSystem(Test) *-> (fcEvalLHS(Body,Support))).

fcEvalLHS(rhs(X),Support) :-
  !,
  pfcDoAll(pfc_eval_rhs(X,Support)),
  !.

fcEvalLHS(X,Support) :-
  pfcType(X,trigger(_Pos)),
  !,
  pfcAddTrigger(X,Support),
  !.

%fcEvalLHS(snip(X),Support) :-
%  snip(Support),
%  fcEvalLHS(X,Support).

fcEvalLHS(X,_) :-
  pfcWarn("Unrecognized item found in trigger body, namely ~p.",[X]).


% %
% %  eval something on the RHS of a rule.
% %

pfc_eval_rhs([],_) :- !.
pfc_eval_rhs([Head|Tail],Support) :-
  pfc_eval_rhs1(Head,Support),
  pfc_eval_rhs(Tail,Support).


pfc_eval_rhs1(Fact,S) :- control_arg_types(Fact,Fixed),!,pfc_eval_rhs1(Fixed,S).

pfc_eval_rhs1({Action},Support) :-
 % evaluable Prolog code.
 !,
 fcEvalAction(Action,Support).

pfc_eval_rhs1(P,_Support) :-
 % predicate to remove.
 pfcNegatedLiteral(P),
 !,
 pfcWithdraw(P).

pfc_eval_rhs1([X|Xrest],Support) :-
 % embedded sublist.
 !,
 pfc_eval_rhs([X|Xrest],Support).

pfc_eval_rhs1(Assertion,Support) :-
 % an assertion to be added.
  once_writeq_nl(pfcRHS(Assertion)),
 (must_ex(pfcPost1(Assertion,Support))*->true ;
   pfcWarn("Malformed rhs of a rule: ~p",[Assertion])).


% %
% %  evaluate an action found on the rhs of a rule.
% %

fcEvalAction(Action,Support) :-
  pfcCallSystem(Action),
  (undoable(Action)
     -> pfcAddActionTrace(Action,Support)
      ; true).


% %
% %
% %

trigger_trigger(Trigger,Body,_Support) :-
 trigger_trigger1(Trigger,Body).
trigger_trigger(_,_,_).


%trigger_trigger1(presently(Trigger),Body) :-
%  !,
%  copy_term(Trigger,TriggerCopy),
%  pfc_call(Trigger),
%  fcEvalLHS(Body,(presently(Trigger),'$pt$'(presently(TriggerCopy),Body))),
%  fail.

trigger_trigger1(Trigger,Body) :-
  copy_term(Trigger,TriggerCopy),
  pfc_call(Trigger),
  with_current_why(Trigger,fcEvalLHS(Body,(Trigger,'$pt$'(TriggerCopy,Body)))),
  fail.


% %  pfc_call(F) is nondet.
%
% pfc_call(F) is true iff F is a fact available for forward chaining.
% Note that this has the side effect of catching unsupported facts and
% assigning them support from God.
%

%pfc_call(F) :- var(F), !, pfc_call(F).
pfc_call(P) :- var(P), !, pfcFact(P).
pfc_call(P) :- \+ callable(P), throw(pfc_call(P)).
pfc_call((!)) :-!,cut_c.
pfc_call(true):-!.
pfc_call((A->B;C)) :-!, pfc_call(A)->pfc_call(B);pfc_call(C).
pfc_call((A*->B;C)) :-!, pfc_call(A)*->pfc_call(B);pfc_call(C).
pfc_call((A->B)) :-!, pfc_call(A)->pfc_call(B).
pfc_call((A*->B)) :-!, pfc_call(A)*->pfc_call(B).
pfc_call((A,B)) :-!, pfc_call(A),pfc_call(B).
pfc_call((A;B)) :-!, pfc_call(A);pfc_call(B).
pfc_call(\+ (A)) :-!, \+ pfc_call(A).
pfc_call((A is B)) :-!, A is B.
pfc_call(clause(A,B)) :-!, clause(A,B).
pfc_call(clause(A,B,Ref)) :-!, clause(A,B,Ref).
% we really need to check for system predicates as well.
% this is probably not advisable due to extreme inefficiency.
pfc_call(P) :-
  % trigger(?) any bc rules.
  '$bt$'(P,Trigger),
  pfcGetSupport('$bt$'(P,Trigger),S),
  % @TODO REVIEW _U
  fcEvalLHS(Trigger,S),
  fail.
%pfc_call(P) :- var(P), !, pfcFact(P).
pfc_call(P) :- predicate_property(P,imported_from(system)), !, call(P).
pfc_call(P) :- predicate_property(P,built_in), !, call(P).
pfc_call(P) :- \+ predicate_property(P,_), functor(P,F,A), dynamic(F/A), !, call(P).
pfc_call(P) :- \+ predicate_property(P,number_of_clauses(_)), !, call(P).
pfc_call(P) :-
  setup_call_cleanup(
    nb_current('$pfc_current_choice',Was),
    (prolog_current_choice(CP), push_current_choice(CP), clause(P,Condition), pfc_call(Condition)),
    nb_setval('$pfc_current_choice',Was)).

/*
pfc_call(P) :-
  clause(P,true)*-> true ; (clause(P,Condition), Condition\==true,
     pfc_call(Condition)).
*/

% an action is undoable if there exists a method for undoing it.
undoable(A) :- fcUndoMethod(A,_).

pfc_cache_bc(P) :-
  % trigger(?) any bc rules.
  forall('$bt$'(P,Trigger),
  forall(pfcGetSupport('$bt$'(P,Trigger),S),
  % @TODO REVIEW _U
  fcEvalLHS(Trigger,S))).


% % 
% %
% %  defining fc rules
% %

% %  pfc_nf(+In,-Out) maps the LHR of a pfc rule In to one normal form
% %  Out.  It also does certain optimizations.  Backtracking into this
% %  predicate will produce additional clauses.


pfc_nf(LHS,List) :-
  pfc_nf1(LHS,List2),
  pfc_nf_negations(List2,List).


% %  pfc_nf1(+In,-Out) maps the LHR of a pfc rule In to one normal form
% %  Out.  Backtracking into this predicate will produce additional clauses.

% handle a variable.

pfc_nf1(P,[P]) :- var(P), !.

% these next two rules are here for upward compatibility and will go
% away eventually when the P/Condition form is no longer used anywhere.

pfc_nf1(P/Cond,[( \+P )/Cond]) :- pfcNegatedLiteral(P), !.

pfc_nf1(P/Cond,[P/Cond]) :-  pfcLiteral(P), !.

% %  handle a negated form

pfc_nf1(NegTerm,NF) :-
  pfc_unnegate(NegTerm,Term),
  !,
  pfc_nf1_negation(Term,NF).

% %  disjunction.

pfc_nf1((P;Q),NF) :-
  !,
  (pfc_nf1(P,NF) ;   pfc_nf1(Q,NF)).


% %  conjunction.

pfc_nf1((P,Q),NF) :-
  !,
  pfc_nf1(P,NF1),
  pfc_nf1(Q,NF2),
  append(NF1,NF2,NF).

% %  handle a random atom.

pfc_nf1(P,[P]) :-
  pfcLiteral(P),
  !.

/*% % % shouln't we have something to catch the rest as errors?*/
pfc_nf1(Term,[Term]) :-
  pfcWarn("pfc_nf doesn''t know how to normalize ~p (accepting though)",[Term]).


% %  pfc_nf1_negation(P,NF) is true if NF is the normal form of \+P.
pfc_nf1_negation((P/Cond),[(\+(P))/Cond]) :- !.

pfc_nf1_negation((P;Q),NF) :-
  !,
  pfc_nf1_negation(P,NFp),
  pfc_nf1_negation(Q,NFq),
  append(NFp,NFq,NF).

pfc_nf1_negation((P,Q),NF) :-
  % this code is not correct! twf.
  !,
  pfc_nf1_negation(P,NF)
  ;
  (pfc_nf1(P,Pnf),
   pfc_nf1_negation(Q,Qnf),
   append(Pnf,Qnf,NF)).

pfc_nf1_negation(P,[\+P]).


% %  pfc_nf_negations(List2,List) sweeps through List2 to produce List,
% %  changing ~{...} to {\+...}
% % % ? is this still needed? twf 3/16/90

pfc_nf_negations(X,X) :- !.  % I think not! twf 3/27/90

pfc_nf_negations([],[]).

pfc_nf_negations([H1|T1],[H2|T2]) :-
  pfc_nf_negation(H1,H2),
  pfc_nf_negations(T1,T2).

% Maybe \+ tilded_negation ?

pfc_nf_negation(Form,{\+ X}) :-
  nonvar(Form),
  Form=(~({X})),
  !.
pfc_nf_negation(Form,{\+ X}) :- tilded_negation,
  nonvar(Form),
  Form=(-({X})),
  !.
pfc_nf_negation(Form,{\+ X}) :- tilded_negation,
  nonvar(Form),
  Form=( \+ ({X})),
  !.
pfc_nf_negation(X,X).



     % %  constrain_meta(+Lhs, ?Guard) is semidet.
     %
     % Creates a somewhat sane Guard.
     %
     % To turn this feature off...
     % ?- set_prolog_flag(constrain_meta,false).
     %
     %
     constrain_meta(_,_):- current_prolog_flag(constrain_meta,false),!,fail.
     % FACT
     constrain_meta(P,mpred_positive_fact(P)):- is_ftVar(P),!.
     % NEG chaining
     constrain_meta(~ P, CP):- !,  constrain_meta(P,CP).
     constrain_meta(\+ P, CP):- !,  constrain_meta(P,CP).
     % FWD chaining
     constrain_meta((_==>Q),nonvar(Q)):- !, is_ftVar(Q).
     % EQV chaining
     constrain_meta((P<==>Q),(nonvar(Q);nonvar(P))):- (is_ftVar(Q);is_ftVar(P)),!.
     % BWD chaining
     constrain_meta((Q <- _),mpred_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q <- _),CQ):- !, constrain_meta(Q,CQ).
     % CWC chaining
     constrain_meta((Q :- _),mpred_literal(Q)):- is_ftVar(Q),!.
     constrain_meta((Q :- _),CQ):- !, constrain_meta(Q,CQ).





     is_simple_lhs(ActN):- is_ftVar(ActN),!,fail.
     is_simple_lhs( \+ _ ):-!,fail.
     is_simple_lhs( ~ _ ):-!,fail.
     is_simple_lhs( _  / _ ):-!,fail.
     is_simple_lhs((Lhs1,Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs((Lhs1;Lhs2)):- !,is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(ActN):- is_active_lhs(ActN),!,fail.
     is_simple_lhs((Lhs1/Lhs2)):- !,fail, is_simple_lhs(Lhs1),is_simple_lhs(Lhs2).
     is_simple_lhs(_).


     is_active_lhs(ActN):- var(ActN),!,fail.
     is_active_lhs(!).
     is_active_lhs(cut_c).
     is_active_lhs(actn(_Act)).
     is_active_lhs('{}'(_Act)).
     is_active_lhs((Lhs1/Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1,Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).
     is_active_lhs((Lhs1;Lhs2)):- !,is_active_lhs(Lhs1);is_active_lhs(Lhs2).


     add_lhs_cond(Lhs1/Cond,Lhs2,Lhs1/(Cond,Lhs2)):-!.
     add_lhs_cond(Lhs1,Lhs2,Lhs1/Lhs2).



% %
% %  buildRhs(+Conjunction,-Rhs)
% %

buildRhs(X,[X]) :-
  var(X),
  !.

buildRhs((A,B),[A2|Rest]) :-
  !,
  pfcCompileRhsTerm(A,A2),
  buildRhs(B,Rest).

buildRhs(X,[X2]) :-
   pfcCompileRhsTerm(X,X2).

pfcCompileRhsTerm((P/C),((P:-C))) :- !.

pfcCompileRhsTerm(P,P).


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
  !,
  buildTest(Test,Test2),
  buildTrigger([{Test2}|Triggers],Consequent,X).


%buildTrigger([snip|Triggers],Consequent,snip(X)) :-
%  !,
%  buildTrigger(Triggers,Consequent,X).

buildTrigger([T|Triggers],Consequent,'$pt$'(T,X)) :-
  !,
  buildTrigger(Triggers,Consequent,X).

% %
% %  buildNtTest(+,+,-).
% %
% %  builds the test used in a negative trigger(-) ('$nt$'/3).  This test is a
% %  conjunction of the check than no matching facts are in the db and any
% %  additional test specified in the rule attached to this ~ term.
% %
     %  tilded_negation.
buildNtTest(T,Testin,Testout) :-
  buildTest(Testin,Testmid),
  pfcConjoin((pfc_call(T)),Testmid,Testout).


% this just strips away any currly brackets.

buildTest({Test},Test) :- !.
buildTest(Test,Test).

% % 


% %  pfcType(+VALUE1, ?Type) is semidet.
%
% PFC Database Type.
%
%  simple typeing for Pfc objects
%


pfcType(Var,Type):- var(Var),!, Type=fact(_FT).
pfcType(_:X,Type):- !, pfcType(X,Type).
pfcType(~_,Type):- !, Type=fact(_FT).
pfcType(('==>'(_,_)),Type):- !, Type=rule(fwd).
pfcType( '==>'(X),Type):- !, pfcType(X,Type), pfcWarn(pfcType( '==>'(X), Type)).
pfcType(('<==>'(_,_)),Type):- !, Type=rule(<==>).
pfcType(('<-'(_,_)),Type):- !, Type=rule(bwc).
pfcType((':-'(_,_)),Type):- !, Type=rule(cwc).
pfcType('$pt$'(_,_,_),Type):- !, Type=trigger(+).
pfcType('$pt$'(_,_),Type):- !, Type=trigger(+).
pfcType('$nt$'(_,_,_),Type):- !,  Type=trigger(-).
pfcType('$bt$'(_,_),Type):- !,  Type=trigger(?).
pfcType(pfcAction(_),Type):- !, Type=action.
pfcType((('::::'(_,X))),Type):- !, pfcType(X,Type).
pfcType(_,fact(_FT)):-
  %  if it''s not one of the above, it must_ex be a fact!
  !.

pfcAssert(P,Support) :-
  (pfc_clause(P) ; assert(P)),
  !,
  pfcAddSupport(P,Support).

pfcAsserta(P,Support) :-
  (pfc_clause(P) ; asserta(P)),
  !,
  pfcAddSupport(P,Support).

pfcAssertz(P,Support) :-
  (pfc_clause(P) ; assertz(P)),
  !,
  pfcAddSupport(P,Support).

pfc_clause((Head :- Body)) :-
  !,
  copy_term(Head,Head_copy),
  copy_term(Body,Body_copy),
  clause(Head,Body),
  variant(Head,Head_copy),
  variant(Body,Body_copy).

pfc_clause(Head) :-
  % find a unit clause identical to Head by finding one which unifies,
  % and then checking to see if it is identical
  copy_term(Head,Head_copy),
  clause(Head_copy,true),
  variant(Head,Head_copy).

pfcForEach(Binder,Body) :- Binder,pfcdo(Body),fail.
pfcForEach(_,_).

% pfcdo(X) executes X once and always succeeds.
pfcdo(X) :- X,!.
pfcdo(_).


% %  pfcUnion(L1,L2,L3) - true if set L3 is the result of appending sets
% %  L1 and L2 where sets are represented as simple lists.

pfcUnion([],L,L).
pfcUnion([Head|Tail],L,Tail2) :-
  memberchk(Head,L),
  !,
  pfcUnion(Tail,L,Tail2).
pfcUnion([Head|Tail],L,[Head|Tail2]) :-
  pfcUnion(Tail,L,Tail2).


% %  pfcConjoin(+Conjunct1,+Conjunct2,?Conjunction).
% %  arg3 is a simplified expression representing the conjunction of
% %  args 1 and 2.

pfcConjoin(true,X,X) :- !.
pfcConjoin(X,true,X) :- !.
pfcConjoin(C1,C2,(C1,C2)).


%   File   : pfcdb.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Author :  Dan Corpron
%   Updated: 10/11/87, ...
%   Purpose: predicates to manipulate a pfc database (e.g. save,
% %     restore, reset, etc.0

% pfcDatabaseTerm(P/A) is true iff P/A is something that pfc adds to
% the database and should not be present in an empty pfc database

pfcDatabaseTerm('$spft$'/3).
pfcDatabaseTerm('$pt$'/2).
pfcDatabaseTerm('$bt$'/2).
pfcDatabaseTerm('$nt$'/3).
pfcDatabaseTerm('==>'/2).
pfcDatabaseTerm('<==>'/2).
pfcDatabaseTerm('<-'/2).
pfcDatabaseTerm(pfcQueue/1).

% removes all forward chaining rules and justifications from db.

pfcReset :-
  pfc_spft(P,F,Trigger),
  pfcRetractOrWarn(P),
  pfcRetractOrWarn('$spft$'(P,F,Trigger)),
  fail.
pfcReset :-
  (pfcDatabaseItem(T)*->
   (pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n",[T]),fail)
    ; true).


% true if there is some pfc crud still in the database.
pfcDatabaseItem(Term:-Body) :-
  pfcDatabaseTerm(P/A),
  functor(Term,P,A),
  clause(Term,Body).

pfcRetractOrWarn(X) :-  retract(X), !.
pfcRetractOrWarn(X) :-
  pfcWarn("Couldn't retract ~p.",[X]),nop((dumpST,pfcWarn("Couldn't retract ~p.",[X]))),!.

pfcRetractOrQuietlyFail(X) :-  retract(X), !.
pfcRetractOrQuietlyFail(X) :-
  nop((pfcTraceMsg("Trace: Couldn't retract ~p.",[X]),nop((dumpST,pfcWarn("Couldn't retract ~p.",[X]))))),
  !,fail.


