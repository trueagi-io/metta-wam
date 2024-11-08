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
 */

% Uncommented out lines define the agent action queue, with multifile and dynamic predicates
% These declarations allow predicates to be defined in multiple files and modified dynamically at runtime.
% :- multifile(baseKB:agent_action_queue/3).
% :- dynamic(baseKB:agent_action_queue/3).

% Set garbage collection flag to true. This allows Prolog to automatically reclaim unused memory during execution.
:- set_prolog_flag(gc, true).

% Declare a thread-local predicate that can be modified per thread, used to disable PX (predicate expansion).
:- thread_local(t_l:disable_px/0).
% Ensure that no thread has PX disabled at startup by removing any existing clauses for the predicate.
:- retractall(t_l:disable_px).

% Ensure that the predicate `t_l:disable_px` is not active in any thread.
:- must(\+ t_l:disable_px).

% Define custom operators with specified precedence and associativity.
% These operators enable symbolic syntax in Prolog, useful for logical expressions in MeTTa.
:- op(500, fx, '~').        % Unary negation operator (~P for NOT P)
:- op(1050, xfx, '=>').     % Implication operator (A => B for A implies B)
:- op(1050, xfx, '<==>').   % Equivalence operator (A <==> B for A if and only if B)
:- op(1050, xfx, '<-').     % Reverse implication (A <- B for A is implied by B)
:- op(1100, fx, '==>').     % Unary forward chaining rule operator (==> A)
:- op(1150, xfx, '::::').   % Custom operator used for specific MeTTa logic

% Temporarily elevate access level to system to allow modification of certain system settings.
:- current_prolog_flag(access_level, Was),
   set_prolog_flag(access_level, system),
   % Define additional operators for logical constructs and arithmetic operations.
   op(1190, xfx, '::::'),
   op(1180, xfx, '==>'),
   op(1170, xfx, '<==>'),
   op(1160, xfx, '<-'),
   op(1150, xfx, '=>'),
   op(1140, xfx, '<='),
   op(1130, xfx, '<=>'),
   op(600, yfx, '&'),       % AND operator
   op(600, yfx, 'v'),       % OR operator
   op(350, xfx, 'xor'),     % XOR operator
   op(300, fx, '~'),        % Negation
   op(300, fx, '-'),        % Unary minus (negative numbers)
   op(1199, fx, '==>'),     % Forward chaining rule
   % Reset access level to its previous state.
   set_prolog_flag(access_level, Was).

% Style checks, dialect settings, and macro expansion control (currently commented out).
% :- style_check(-discontiguous). % Disables warnings for non-contiguous predicate definitions.
% :- enable_mpred_expansion.      % Enables certain predicate expansions specific to the logicmoo framework.
% :- expects_dialect(pfc).        % Set the expected Prolog dialect to PFC (Prolog Forward Chaining).

/*
 * Dynamic predicate declarations. These predicates represent sessions and agents
 * and allow Prolog to track and manage interactions between them.
 */
% :- dynamic lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2, telnet_fmt_shown/3, agent_action_queue/3).

% Uncommenting the next line would set the source module to `baseKB`, a knowledge base module.
% :- nop('$set_source_module'(baseKB)).

% Set flags to optimize runtime safety and debugging rather than speed.
:- set_prolog_flag(runtime_speed, 0).     % Disable speed optimizations.
:- set_prolog_flag(runtime_safety, 2).    % Maximize safety checks.
:- set_prolog_flag(runtime_debug, 2).     % Maximize debug information.
:- set_prolog_flag(unsafe_speedups, false). % Disable potentially unsafe optimizations.
:- set_prolog_flag(expect_pfc_file, always). % Expect PFC file loading.

% Disable the PFC term expansion during certain phases.
:- set_prolog_flag(pfc_term_expansion, false).

% Predicate to handle function parameter and return type information.
% It separates the return type from the list of parameters.
params_and_return_type([->|TypeList], Len, Params, Ret):-
   append(Params, [Ret], TypeList),  % Splits the list into Params and Ret (return type).
   length(Params, Len).              % Returns the number of parameters.

% Merges two function predicates by comparing their arguments and ensuring they match.
merge_fp(_, _, N) :- N < 1.          % Base case: stop when N is less than 1.
merge_fp(T1, T2, N) :-
  N > 0,
  arg(N, T1, X),                      % Get the N-th argument of T1.
  arg(N, T2, X),                      % Ensure the N-th argument of T2 matches.
  N1 is N - 1,                        % Decrease N and recursively merge the remaining arguments.
  merge_fp(T1, T2, N1).

% Re-enable PFC term expansion after processing is complete.
:- set_prolog_flag(pfc_term_expansion, true).

% Define a "functional-predicate" as a Prolog term.
% This code generates two function terms (P1 and P2) with the same functor (Name and Arity),
% and binds their final arguments.
'functional-predicate'(Name, Arity) ==>
  {functor(P1, Name, Arity),          % Create a functor term P1.
   functor(P2, Name, Arity),          % Create another functor term P2.
   arg(Arity, P1, PV1),               % Get the last argument of P1.