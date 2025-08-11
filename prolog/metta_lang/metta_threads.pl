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

%*********************************************************************************************
% PROGRAM FUNCTION: provides asynchronous and lazy evaluation capabilities, allowing goals to
% be executed in background threads or deferred until needed, optimizing performance and resource use.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% use_module is equivalent to ensure_loaded/1, except that it raises an error if the files are not module files.
:- use_module(library(predicate_options)).
:- use_module(library(record)).
% Ensure that the `metta_interp` library is loaded.
% This loads all the predicates called from this file
:- ensure_loaded(metta_interp).

% Define options for async/3 with a default policy.
% The `opts` record includes a `policy` field, which can be `ephemeral`
% or `lazy`, with `ephemeral` set as the default.
:- record(opts( policy:oneof([ephemeral,lazy])=ephemeral
              )).

% Declare option specifications for the predicates spawn/2 and async/3.
:- predicate_options(spawn/2,2,[pass_to(async/3,3)]).
:- predicate_options(async/3,3, [ policy(+oneof([ephemeral,lazy]))
                                ]).

% Define meta-predicate declarations to allow certain predicates to
% accept goals (i.e., other predicates) as arguments.
% - spawn/0: A meta-predicate to call a goal in a background thread.
% - async/2 and async/3: Allow goal execution asynchronously with optional tokens.
% - async_policy/4: Specifies policies for executing asynchronous goals.
:- meta_predicate
    spawn(0),
    async_eval(0,('-')),
    async_token(0,('-')),
    async_token(0,('-'),('+')),
    async_policy('+',0,'-','+').

:- multifile(same_meta_predicate/1).
:- dynamic(same_meta_predicate/1).

% Declare a thread-local variable to track asynchronous tokens that
% still need to be awaited. Each thread has its own instance of this variable.
:- thread_local
    spawn_token_needs_await/1.

%!  spawn(:Goal) is det.
%
%   Executes a given `Goal` asynchronously in a background thread, using default options.
%
%   This predicate is a shorthand version of `spawn/2`, which runs the specified `Goal`
%   in a separate thread, allowing other tasks to proceed concurrently. By default, it
%   uses the `ephemeral` policy, which immediately starts execution of the `Goal` in the
%   background without waiting for a subsequent call to `await/1`.
%
%   This version is useful for simple cases where no specific options are required, and
%   default asynchronous behavior is desired.
%
%   @arg Goal The goal to be executed asynchronously. This should be any valid Prolog
%             goal that may succeed or fail independently of the main thread.
%
%   @example
%     % Execute a goal in a background thread without specifying options.
%     ?- spawn(sleep(2)).  % Will run in the background and finish after 2 seconds.
%
%     % If Goal unifies variables, they can be accessed after the async task completes.
%     ?- spawn(member(X, [a, b, c])), writeln(X).
%
%   @see spawn/2 for an option to specify additional settings like execution policies.
%
spawn(Goal) :-
    spawn(Goal, []).

%! spawn(:Goal, +Options) is det.
%
%   Seek solutions to `Goal` in a background thread. Solutions are
%   communicated to the calling thread by unifying free variables in
%   `Goal`. If `Goal` has no free variables, `async/3` must be used instead.
%   Options specified in `Options` are passed directly to `async/3`.
%
%   This predicate is designed to enable concurrent processing by allowing
%   `Goal` to run in parallel with the calling thread. Free variables in
%   `Goal` will be unified with solutions from the background thread as they
%   become available, allowing the calling thread to continue processing
%   asynchronously.
%
%   @arg Goal     The goal to be executed asynchronously. It may contain free
%                 variables, which are unified with solutions generated by `Goal`.
%   @arg Options  Options to control the execution policy, passed to `async/3`.
%
%   For example, the following code runs in about 1 second because both
%   `sleep/1` calls happen in parallel. When `foo/0` unifies `L`, it blocks
%   until `silly/1` has finished.
%
%       silly(L) :-
%           sleep(1),
%           L = [a,b].
%       foo :-
%           spawn(silly(L)),
%           sleep(1),
%           L = [A,B],  % blocks, if necessary
%           writeln(A-B).
%
%   If `Goal` produces multiple solutions, they can be iterated by
%   backtracking over the unification (e.g., `L = [A, B]` in the example above).
%   In case `Goal` fails or raises an exception, this outcome is observed
%   by the calling thread at the unification point.
%
%   @see async/3 for more fine-grained control when no free variables are present.
%
spawn(Goal, Options) :-
    term_variables(Goal, Vars),          % Collect free variables in Goal
    async_token(Goal, Token, Options),         % Start async execution with options
    Id is random(1<<63),                 % Generate a unique ID for this task
    assert(spawn_token_needs_await(Id)), % Register task as needing await
    make_opts(Options, Opts),            % Process options into Opts format
    maplist(spawn_freeze(Id, Token, Opts), Vars).  % Freeze variables until needed

%!  spawn_freeze(+Id, +Token, +Opts, +Var) is det.
%
%   Delays execution of `spawn_thaw/3` until `Var` is bound.
%
%   This predicate sets up `Var` with a `freeze/2` wrapper so that `spawn_thaw/3`
%   is executed only when `Var` becomes bound. This mechanism is used to defer
%   the processing of asynchronous tasks until needed.
%
%   @arg Id Unique identifier for the spawned task.
%   @arg Token Token associated with the spawned task, used for tracking.
%   @arg Opts Options record, containing the policy for async processing.
%   @arg Var The variable whose binding triggers the execution of `spawn_thaw/3`.
%
spawn_freeze(Id, Token, Opts, Var) :-
    freeze(Var, spawn_thaw(Id, Token, Opts)).

%! spawn_thaw(+Id, +Token, +Opts) is det.
%
%   Handles the thawing process when a frozen variable becomes bound,
%   proceeding with asynchronous task completion based on the options.
%
%   If the spawned task identified by `Id` still requires awaiting, `await/1` is
%   called on `Token` to block until the task is done. If the `lazy` policy is set
%   in `Opts`, `await/1` is called again to defer execution. If `await/1` has
%   already been called, it skips re-execution, indicated by a debug message.
%
%   @arg Id Unique identifier for the spawned task.
%   @arg Token Token associated with the spawned task.
%   @arg Opts Options record, specifying the task execution policy.
%
%   @example
%     % Example showing how spawn_thaw/3 coordinates task completion
%     % with the lazy policy.
%
%     % Suppose a goal like `compute_results/1` is spawned with `lazy`:
%     ?- spawn(compute_results(X), [policy(lazy)]).
%
%     % When `X` is later needed, `spawn_thaw/3` will handle the awaiting process.
%     % This ensures `compute_results/1` only executes when required by the calling
%     % thread.
%
spawn_thaw(Id, Token, Opts) :-
    ( retract(spawn_token_needs_await(Id)) ->
        % Await task completion if still pending
        debug(spawn, "Await on ~d", [Id]),
        await(Token)
    ; opts_policy(Opts, lazy) ->
        % Await again if the `lazy` policy is active
        debug(spawn, "Awaiting again on ~d", [Id]),
        await(Token)
    ; % Otherwise, task has already been awaited
        debug(spawn, "Already did await on ~d", [Id]),
        true
    ).

%!  lazy(+Goal) is det.
%
%   Postpone execution of `Goal` until it is actually needed. This predicate
%   behaves the same as `spawn/1` but uses the `lazy` policy, meaning that
%   `Goal` will only execute if required by the calling context.
%
%   Using `lazy/1` can be beneficial when `Goal` is resource-intensive or
%   time-consuming, especially if the result is only necessary in specific
%   code paths. This approach avoids duplicating complicated logic within
%   conditional branches and can serve as an alternative to defining an
%   additional, separate predicate.
%
%   @arg Goal The goal whose execution is deferred until needed.
%
%   @example
%     % Example of using lazy/1 in a conditional context:
%
%     foo(Xs) :-
%         % Defers the execution of `i_am_slow/3` until absolutely needed.
%         lazy(i_am_slow(a, B, [c(C), d(d), e(etc)])),
%
%         % Execution of `i_am_slow/3` depends on the following conditions:
%         ( day_of_week(tuesday) ->
%             append(B, C, Xs)
%         ; phase_of_moon(full) ->
%             append(C, B, Xs)
%         ; true ->
%             % `i_am_slow/3` is never executed in this code path
%             Xs = [hi]
%         ).
%
%   This example shows how `lazy/1` defers the execution of `i_am_slow/3`.
%   If neither `day_of_week(tuesday)` nor `phase_of_moon(full)` holds true,
%   the `lazy` policy prevents `i_am_slow/3` from executing at all, avoiding
%   unnecessary computation.
%
lazy(Goal) :-
    spawn(Goal, [policy(lazy)]).


%!  async_token(+Goal, -Token) is det.
%
%   Executes `Goal` asynchronously in a background thread, returning a `Token`
%   to track its status. This predicate is a shorthand for `async/3`, using
%   default options for execution.
%
%   The `Token` can be used with `await/1` to synchronize with the completion
%   of `Goal`. This allows the main thread to proceed with other tasks without
%   blocking, only retrieving results when necessary.
%
%   @arg Goal   The goal to be executed asynchronously. This can be any Prolog
%               goal, including those with free variables that unify upon completion.
%   @arg Token  A token that represents the asynchronous task, which can later
%               be used with `await/1` to wait for or query the result.
%
%   @example
%     % Run a goal asynchronously, retrieve its status using Token.
%     ?- async_token(some_heavy_task(X), Token),
%        % Other tasks can proceed here
%        await(Token),  % Wait for `some_heavy_task/1` to complete
%        writeln(X).
%
%   @see async/3 for specifying execution options.
%
async_token(Goal, Token) :-
    async_token(Goal, Token, []).

async_eval_token(Eval, Result, Token) :-
    async_token(eval_args(Eval,Result), Token, []).

async_eval(Eval, Result):-
   async_eval_token(Eval, Ref, Token),
   token_ref_result(Token, Ref, Result).

token_ref_result(Token, Ref, Result):-
   freeze(Result, (await(Token), Ref=Result)).

materialize(_):- !.
/*
materialize(Var):- attvar(Var),!,(frozen(Var,Body)->call(Body);true).
materialize(Val):- term_attvars(Val,List),!,maplist(materialize,List).
*/

%!  async_token(+Goal, -Token, +Options) is det.
%
%   Execute `Goal` in a background thread, using `Options` to control execution policy.
%   Returns a `Token` that can be used with `await/1` to synchronize with the task
%   completion. Free variables in `Goal` are unified with solutions found by the background
%   computation, allowing the calling thread to obtain results asynchronously.
%
%   Solutions to `Goal` are copied between threads, along with `Goal` itself, allowing
%   independent execution. Take care if large terms are involved, as copying may have
%   performance implications.
%
%   @arg Goal    The goal to be executed asynchronously. This can include free variables
%                that will unify upon the task’s completion.
%   @arg Token   A token representing the asynchronous task, which can later be passed
%                to `await/1` to retrieve results or wait for task completion.
%   @arg Options Execution options controlling task behavior.
%
%   Options are as follows:
%
%     * policy(Policy)
%       Specifies when and how `Goal` should execute. The `Policy` can be:
%       - `ephemeral` (default): Starts a new thread to execute `Goal` immediately.
%       - `lazy`: Defers execution of `Goal` until `await/1` is called, avoiding
%         creation of a background thread until the result is needed.
%
%   @example
%     % Example of executing a goal asynchronously with default (ephemeral) policy:
%     ?- async_token(some_computation(Result), Token, [policy(ephemeral)]),
%        % Do other work here
%        await(Token),  % Blocks until some_computation/1 completes
%        writeln(Result).
%
%     % Using the lazy policy to defer execution until needed:
%     ?- async_token(another_computation(Output), Token, [policy(lazy)]),
%        % other code executes without blocking
%        await(Token),  % Triggers execution of another_computation/1 only now
%        writeln(Output).
%
%   @see spawn/2 for a higher-level predicate without explicit token handling.
%
async_token(Goal, Token, Options) :-
    make_opts(Options, Opts),             % Prepare options in internal format
    opts_policy(Opts, Policy),            % Extract the execution policy
    async_policy(Policy, Goal, Token, Opts).  % Dispatch based on policy

%!  async_policy(+Policy, +Goal, -Token, +Opts) is det.
%
%   Determines the execution strategy for `Goal` based on `Policy`. For
%   `ephemeral`, `Goal` is executed in a new worker thread that seeks solutions.
%   For `lazy`, execution is deferred until `await/1` is invoked.
%
%   @arg Policy  Specifies whether the execution is `ephemeral` (immediate) or `lazy` (deferred).
%   @arg Goal    The goal to be executed. This goal may contain free variables to be unified upon task completion.
%   @arg Token   A token representing the task, used to manage or await the result.
%   @arg Opts    Additional options for the execution policy (currently unused).
%
async_policy(ephemeral, Goal, Token, _Opts) :-
    % Collects free variables in Goal for unification upon completion.
    term_variables(Goal, Vars),
    % Creates a message queue with a maximum size of 1 to hold results.
    message_queue_create(SolutionsQ, [max_size(1)]),
    % The token for `ephemeral` includes the variable list and solution queue.
    Token = ephemeral_token(Vars, SolutionsQ),
    % Define the work as the goal with its associated variables and queue,
    % and create a detached thread to handle it.
    Work = work(Goal, Vars, SolutionsQ),
    thread_create(ephemeral_worker(Work), _, [detached(true)]).
async_policy(lazy, Goal, Token, _Opts) :-
    % For lazy evaluation, create a token that encapsulates the goal itself.
    Token = lazy_thunk(Goal).

%!  ephemeral_worker(+Work) is det.
%
%   Executes the `Goal` in `Work` within an ephemeral (temporary) worker thread.
%   This worker sends each solution, exceptions, or a final result to `SolutionsQ`.
%
%   If the goal has multiple solutions, each one is sent to `SolutionsQ` in sequence.
%   Upon completion, a `final/1` message is sent with the last solution, or `none`
%   if no solutions were found. Any exceptions during execution are caught and
%   sent as `exception/1`.
%
%   @arg Work  Encapsulates the goal, its free variables (`Vars`), and the solution queue (`SolutionsQ`).
%
ephemeral_worker(work(Goal, Vars, SolutionsQ)) :-
    debug(spawn, "Seeking solutions to: ~q", [Goal]),
    (
        % Execute `Goal` within `call_cleanup`, which ensures `Done=true` when complete.
        catch(call_cleanup(Goal, Done = true), E, true) *->
        (
            nonvar(E) ->
            % If an exception occurs, send it to `SolutionsQ`.
            debug(spawn, "Caught exception: ~q", [E]),
            thread_send_message(SolutionsQ, exception(E))
        ;
            var(Done) ->
            % If not done, send the current solution and continue to look for more.
            debug(spawn, "Sending solution: ~q", [Vars]),
            thread_send_message(SolutionsQ, solution(Vars)),
            fail  % triggers backtracking for additional solutions
        ;
            Done = true ->
            % When all solutions are found, send the final solution.
            debug(spawn, "Final solution: ~q", [Vars]),
            thread_send_message(SolutionsQ, final(Vars))
        )
    ;
        % No solutions found; signal completion with `none`.
        debug(spawn, "Found no solutions", []),
        thread_send_message(SolutionsQ, none)
    ).

%!  await(+Token) is det.
%
%   Waits for solutions from an asynchronous call made with `async/3`. `Token`
%   is an opaque identifier provided by `async/3` that uniquely represents the
%   background computation.
%
%   await/1 strives to have the same determinism as the original Goal
%   passed to async/3. If that goal fails, await/1 fails. If that goal
%   throws an exception, so does await/1. If that goal produces many
%   solutions, so does await/1 on backtracking.
%
%   - If the goal succeeds with multiple solutions, `await/1` will yield each solution
%     on backtracking.
%   - If the goal fails, `await/1` also fails.
%   - If the goal raises an exception, `await/1` propagates the exception.
%
%   The `Token` indicates whether the computation is `ephemeral` (running in a background
%   thread) or `lazy` (only executed when `await/1` is called).
%
%   @arg Token  An opaque token representing the asynchronous computation, either
%               `ephemeral_token/2` for background-threaded tasks or `lazy_thunk/1`
%               for deferred execution.
%
%   @example
%     % Run a goal asynchronously with async/3 and wait for its result using await/1:
%
%     % Define a sample goal with a delay to simulate computation.
%     simulated_task(Result) :-
%         sleep(2),                    % Simulate a time-consuming task
%         Result = "Task Complete".
%
%     % Asynchronously run the simulated task.
%     ?- async_token(simulated_task(Result), Token, [policy(ephemeral)]),
%        writeln("Doing other work while task runs..."),
%        await(Token),                % Block until task completes
%        writeln(Result).             % Output the result upon completion
%
%     % Expected output:
%     % Doing other work while task runs...
%     % Task Complete
%
await(ephemeral_token(Vars, SolutionsQ)) :-
    % Repeatedly check for messages in the solution queue until the final solution.
    repeat,
    thread_get_message(SolutionsQ, Solution),
    ( Solution = solution(Vars) ->
        true   % Return a solution, leaving choice point for backtracking
    ; Solution = final(Vars) ->
        !,     % Last solution found; cut to prevent backtracking
        true
    ; Solution = none ->
        !,     % No solutions available; cut and fail
        fail
    ; Solution = exception(E) ->
        throw(E)  % Propagate any exception thrown by the async goal
    ; % what? Catch unexpected message types
        throw(unexpected_await_solution(Solution))
    ).
await(lazy_thunk(Goal)) :-
    % For lazy evaluation, execute the goal directly when `await/1` is called.
    call(Goal).





%!  ccml_nth:attr_unify_hook(+Nth, +Var) is det.
%
%   Hook for attribute unification, used in the context of concurrent computations.
%
%   @arg Nth The index or position in a list.
%   @arg Var The variable to which the attribute is attached.
%
ccml_nth:attr_unify_hook(_Nth, _Var).

%!  metta_hyperpose_v0(:P2, +InList, -OutList) is det.
%
%   Concurrently applies a predicate P2 to each element of InList, producing OutList.
%   This version leverages threading to maximize CPU utilization.
%
%   @arg P2 The predicate to apply to each element.
%   @arg InList The input list of elements.
%   @arg OutList The output list with results of applying P2.
%




metta_hyperpose_v0(P2, InList, OutList) :- !,
    metta_concurrent_maplist(P2, InList, OutList).

metta_hyperpose_v0(P2, InList, OutList) :-
    % Get the number of available CPU cores.
    current_prolog_flag(cpu_count, Count),
    % Determine the length of the input list.
    length(InList, Len),
    % Ensure the output list has the same length as the input list.
    length(OutList, Len),
    % Calculate the number of processes to use.
    max_min(Count, Len, _, Procs),
    % Iterate over the input list.
    findall(thread(Goal, OutputVar),
            (nth1(N, InList, InputVar),
            % Create a goal for each input.
            Goal = call(P2, InputVar, OutputVar),
            % Attach an attribute to the output variable.
            put_attr(OutputVar, ccml_nth, N)),
            % Collect goals with their associated output variables.
            GoalsWithOutputs),
    % Separate the goals and outputs.
    separate_goals_and_outputs(GoalsWithOutputs, Goals, OutList),
    % Execute the goals concurrently using the specified number of processes.
    concurrent(Procs, Goals, []).

%!  separate_goals_and_outputs(+GoalsWithOutputs, -Goals, -Outputs) is det.
%
%   Separates goals from their corresponding output variables in a list of threaded goals.
%
%   @arg GoalsWithOutputs A list of threaded goals with output variables.
%   @arg Goals The list of goals extracted.
%   @arg Outputs The list of output variables extracted.
%
separate_goals_and_outputs([], [], []).  % Base case: empty list yields empty outputs.
separate_goals_and_outputs([thread(Goal, OutputVar) | GoalsWithOutputs],
    % Recursively process the remaining goals and outputs.
    [Goal | Goals],
    [OutputVar | Outputs]) :-
    % Separate the goals and output variables.
    separate_goals_and_outputs(GoalsWithOutputs, Goals, Outputs).

% Previously included directive to use the concurrent library.
% :- use_module(library(concurrent)).

%!  metta_concurrent_maplist(:P2, +InList, -OutList) is det.
%
%   Concurrently applies P2 to each element of InList to produce OutList.
%   If InList has two or more elements, threading is used to parallelize the operation.
%
%   @arg P2 The predicate to apply to each element.
%   @arg InList The input list of elements.
%   @arg OutList The output list with results of applying P2.
%



choose_worker_count(List1, WorkerCount) :-
    length(List1, Len), !,
    current_prolog_flag(cpu_count, CPUs),
    ( CPUs < 2 -> Count = 8 ;  Count is CPUs * 4 ),
    WorkerCount is min(Count, Len).


ml_goal_2(Goal, Elem1,Elem2, call(Goal, Elem1, Elem2)).


metta_concurrent_maplist(Goal, List1, List2) :- List1 = [_,_|_],
    thread:same_length(List1, List2),
    choose_worker_count(List1,WorkerCount), !,
    maplist(ml_goal_2(Goal), List1, List2, Goals),
    concurrent(WorkerCount, Goals, []).

metta_concurrent_maplist(P2, InList, OutList) :-
    % Check if InList has two or more elements.
    InList = [_,_|_],
    !,  % Cut to ensure threading is used.
    % Setup concurrent processing with cleanup.
    setup_call_cleanup(
             % Assert results after concurrent computation.
             concurrent_assert_result(P2, InList, Tag),
             % Gather the results in order.
             gather_results_in_order(Tag, InList, OutList),
             % Cleanup the results after processing.
             cleanup_results(Tag)).

metta_concurrent_maplist(P2, InList, OutList) :- maplist(P2, InList, OutList).


metta_concurrent_maplist(Goal, List1, List2, List3, List4) :-
    same_length_4(List1, List2, List3, List4),
    choose_worker_count(List1, WorkerCount),
    !, maplist(ml_goal_4(Goal), List1, List2, List3, List4, Goals),
    concurrent(WorkerCount, Goals, []).
metta_concurrent_maplist(M:Goal, List1, List2, List3, List4) :- maplist(once_in_module_4(M, Goal), List1, List2, List3, List4).
same_length_4([], [], [], []).
same_length_4([_|T1], [_|T2], [_|T3], [_|T4]) :- same_length_4(T1, T2, T3, T4).
ml_goal_4(Goal, Elem1, Elem2, Elem3, Elem4, call(Goal, Elem1, Elem2, Elem3, Elem4)).
once_in_module_4(M, Goal, Arg1, Arg2, Arg3, Arg4) :- call(M:Goal, Arg1, Arg2, Arg3, Arg4), !.


metta_concurrent_maplist(Goal, E1, E2, E3, E4, E5) :-
    same_length_5(E1, E2, E3, E4, E5),
    choose_worker_count(E1, WorkerCount),
    !, maplist(ml_goal_5(Goal), E1, E2, E3, E4, E5, Goals),
    concurrent(WorkerCount, Goals, []).
metta_concurrent_maplist(M:Goal, E1, E2, E3, E4, E5) :- maplist(once_in_module_5(M, Goal), E1, E2, E3, E4, E5).
same_length_5([], [], [], [], []).
same_length_5([_|E1], [_|E2], [_|E3], [_|E4], [_|E5]) :- same_length_5(E1, E2, E3, E4, E5).
ml_goal_5(Goal, E1, E2, E3, E4, E5, call(Goal, E1, E2, E3, E4, E5)).
once_in_module_5(M, Goal, E1, E2, E3, E4, E5) :- call(M:Goal, E1, E2, E3, E4, E5), !.

metta_concurrent_maplist(Goal, E1, E2, E3, E4, E5, E6) :-
    same_length_6(E1, E2, E3, E4, E5, E6),
    choose_worker_count(E1, WorkerCount),
    !, maplist(ml_goal_6(Goal), E1, E2, E3, E4, E5, E6, Goals),
    concurrent(WorkerCount, Goals, []).
metta_concurrent_maplist(M:Goal, E1, E2, E3, E4, E5, E6) :- maplist(once_in_module_6(M, Goal), E1, E2, E3, E4, E5, E6).
same_length_6([], [], [], [], [], []).
same_length_6([_|E1], [_|E2], [_|E3], [_|E4], [_|E5], [_|E6]) :- same_length_6(E1, E2, E3, E4, E5, E6).
ml_goal_6(Goal, E1, E2, E3, E4, E5, E6, call(Goal, E1, E2, E3, E4, E5, E6)).
once_in_module_6(M, Goal, E1, E2, E3, E4, E5, E6) :- call(M:Goal, E1, E2, E3, E4, E5, E6), !.


/*
:- meta_predicate maplist(5, ?, ?, ?, ?, ?).

maplist(Goal, List1, List2, List3, List4, List5) :-
    maplist_(List1, List2, List3, List4, List5, Goal).

maplist_([], [], [], [], [], _).
maplist_([X1|Xs1], [X2|Xs2], [X3|Xs3], [X4|Xs4], [X5|Xs5], Goal) :-
    call(Goal, X1, X2, X3, X4, X5),
    maplist_(Xs1, Xs2, Xs3, Xs4, Xs5, Goal).

:- meta_predicate maplist(6, ?, ?, ?, ?, ?, ?).

maplist(Goal, List1, List2, List3, List4, List5, List6) :-
    maplist_(List1, List2, List3, List4, List5, List6, Goal).

maplist_([], [], [], [], [], [], _).
maplist_([X1|Xs1], [X2|Xs2], [X3|Xs3], [X4|Xs4], [X5|Xs5], [X6|Xs6], Goal) :-
    call(Goal, X1, X2, X3, X4, X5, X6),
    maplist_(Xs1, Xs2, Xs3, Xs4, Xs5, Xs6, Goal).

:- meta_predicate maplist(7, ?, ?, ?, ?, ?, ?, ?).

maplist(Goal, List1, List2, List3, List4, List5, List6, List7) :-
    maplist_(List1, List2, List3, List4, List5, List6, List7, Goal).

maplist_([], [], [], [], [], [], [], _).
maplist_([X1|Xs1], [X2|Xs2], [X3|Xs3], [X4|Xs4], [X5|Xs5], [X6|Xs6], [X7|Xs7], Goal) :-
    call(Goal, X1, X2, X3, X4, X5, X6, X7),
    maplist_(Xs1, Xs2, Xs3, Xs4, Xs5, Xs6, Xs7, Goal).
*/


%!  metta_hyperpose(:Eq, :RetType, +Depth, +MSpace, +InList, -Res) is det.
%
%   Concurrently applies a function, leveraging threading to optimize processing,
%   but falls back to a single-threaded approach for small inputs.
%
%   @arg Eq The equation to evaluate.
%   @arg RetType The type of the returned result.
%   @arg Depth The depth of the evaluation.
%   @arg MSpace The execution environment.
%   @arg InList The input list.
%   @arg Res The result.
%
metta_hyperpose(Eq, RetType, Depth, MSpace, InList, Res) :-
 \+ option_value(threading,false),!,
 %with_metta_ctx(Eq, RetType, Depth, MSpace, ['hyperpose'|InList], metta_hyperpose_v0(eval, InList, Res)).
  with_metta_ctx(Eq, RetType, Depth, MSpace, ['hyperpose'|InList], metta_hyperpose_v0(eval_args(Eq, RetType, Depth, MSpace), InList, Res)).

metta_hyperpose(Eq, RetType, Depth, MSpace, InList, Res) :-
    % This part of the code is currently skipped with fail.
    % fail,
    \+ option_value(threading,false),
    % Check if InList has two or more elements.
    InList = [_,_|_],!,
    !,  % Cut to ensure threading is used.
    % Setup concurrent processing with cleanup.
    setup_call_cleanup(
             % Assert results after concurrent computation.
             concurrent_assert_result(eval_args(Eq, RetType, Depth, MSpace), InList, Tag),
             % Gather each result in order.
             each_result_in_order(Tag, InList, Res),
             % Cleanup the results after processing.
             cleanup_results(Tag)).

metta_hyperpose(Eq, RetType, Depth, MSpace, ArgL, Res) :-
    % Evaluate the equation using single-threaded approach.
    eval_args(Eq, RetType, Depth, MSpace, ['superpose', ArgL], Res).

%!  concurrent_assert_result(:P2, +InList, -Tag) is det.
%
%   Concurrently applies P2 to each element of InList and tags the result with a unique identifier.
%
%   @arg P2 The predicate to apply.
%   @arg InList The input list.
%   @arg Tag A unique identifier for the result set.
%
concurrent_assert_result(P2, InList, Tag) :-
    % Get the number of available CPU cores.
    current_prolog_flag(cpu_count, Count),
    % Determine the length of the input list.
    length(InList, Len),
    % Calculate the number of processes to use.
    max_min(Count, Len, _, Procs),
    % Generate a unique identifier (tag) for the result set.
    gensym(counter, Tag),
    % Apply P2 concurrently to each element of InList.
    concurrent_forall( nth1(Index, InList, InputVar),
                       % Assert the result after computation.
                       assert_result_after_computation(P2, Tag, Index, InputVar),
                       [threads(Procs)]).


:- dynamic(result/4).

% Asserts the output of applying P2 to Input.
assert_result_after_computation(P2, Tag, Index, Input) :-
    % Catch any errors that occur during computation.
    catch(
      % If P2 succeeds, assert the result.
      (call(P2, Input, Output) *-> assert(result(Tag, Index, Input, Output))
      % If P2 fails, assert that it failed.
      ; assert(result(Tag, Index, Input, failed(Tag)))),
      % On error, assert the error result.
      E, (assert(result(Tag, Index, Input, error(E))))).

%!  gather_results_in_order(+Tag, +InList, -OrderedResults) is det.
%
%   Gathers results in order, matching them with the corresponding inputs.
%
%   @arg Tag The unique identifier for the result set.
%   @arg InList The input list of elements.
%   @arg OrderedResults The output list of results in the same order as InList.
%
gather_results_in_order(Tag, InList, OrderedResults) :-
    % Start the gathering process with an initial index of 0.
    gather_results_in_order(Tag, InList, 0, OrderedResults).

%!  use_result(+IInput, +RResult, +Input, -Result) is det.
%
%   Helper predicate to unify inputs and results, handling errors and failures appropriately.
%   This predicate attempts to unify `Input` with `IInput`, handling different cases based on the value of `RResult`.
%
%   @arg IInput The input term to be unified with `Input`.
%   @arg RResult The result of a previous computation, which could be a variable, an error, or a failure indicator.
%   @arg Input The original input term.
%   @arg Result The resulting term after unification or error handling.
%
%   @example Unify inputs directly when `RResult` is a variable:
%       ?- use_result(X, Y, A, B).
%
%   @example Handle error case:
%       ?- use_result(X, error('SomeError'), A, B).
%
use_result(IInput, RResult, Input, Result) :-
    % If RResult is a variable, unify directly.
    var(RResult),
    !,
    IInput = Input,
    Result = RResult.

use_result(IInput, error(E), Input, _) :-
    % On error, unify input and throw the exception.
    ignore(IInput = Input),
    !,
    throw(E).

use_result(IInput, failed(_), Input, _) :-
    % On failure, unify input and fail.
    ignore(IInput = Input),
    !,
    fail.

use_result(IInput, RResult, Input, Result) :-
    % Default case: unify input and result.
    IInput = Input,
    Result = RResult.

%!  gather_results_in_order(+Tag, +Inputs, +Index, -OrderedResults) is det.
%
%   Collects results in the same order as the corresponding inputs, handling delays in result availability.
%   This predicate processes a list of inputs, attempting to retrieve and order the results based on their availability.
%
%   @arg Tag The unique identifier associated with the set of results.
%   @arg Inputs The list of inputs for which results are being gathered.
%   @arg Index The current index in the list of inputs.
%   @arg OrderedResults The output list of results, ordered to match the input list.
%
%   @example Gather results in order:
%       ?- gather_results_in_order(Tag, [Input1, Input2], 0, Results).
%

% Base case: empty input list produces an empty result list.
gather_results_in_order(_, [], _, []).
gather_results_in_order(Tag, [Input | RestInputs], Index, [Result | OrderedResults]) :-
     % Check if the result is available.
     (result(Tag, Index, IInput, RResult)
       % If so, use the result.
       *->  (
              use_result(IInput, RResult, Input, Result),
              % Increment the index and continue gathering.
              NextIndex is Index + 1,
              gather_results_in_order(Tag, RestInputs, NextIndex, OrderedResults)
            )
      ; % If not, wait for 75 milliseconds before retrying.
        (
            sleep(0.075),
            gather_results_in_order(Tag, [Input | RestInputs], Index, [Result | OrderedResults])
        )
     ).

%!  each_result_in_order(+Tag, +InList, -OrderedResults) is nondet.
%
%   Retrieves each result in order, matching them with the corresponding inputs.
%
%   @arg Tag The unique identifier for the result set.
%   @arg InList The input list of elements.
%   @arg OrderedResults The output list of results in the same order as InList.
%
each_result_in_order(Tag, InList, OrderedResults) :-
    % Start the process with an initial index of 0.
    each_result_in_order(Tag, InList, 0, OrderedResults).

% Base case: empty input list fails to produce a result.
each_result_in_order(_, [], _, _) :- !, fail.

each_result_in_order(Tag, [Input | RestInputs], Index, Result) :-
     % Check if the result is available.
     (result(Tag, Index, IInput, RResult)
       % If so, use the result.
       *->  (use_result(IInput, RResult, Input, Result);
             % Increment the index and continue gathering.
             (NextIndex is Index + 1,
              each_result_in_order(Tag, RestInputs, NextIndex, Result)))
      ; % If not, wait for 75 milliseconds before retrying.
        (sleep(0.075),
         each_result_in_order(Tag, [Input | RestInputs], Index, Result))).

%!  cleanup_results(+Tag) is det.
%
%   Cleanup predicate to remove asserted results from the database.
%
%   @arg Tag The unique identifier for the result set to be cleaned up.
%
cleanup_results(Tag) :-
    % Retract all results associated with the given Tag.
    retractall(result(Tag, _, _, _)).


mc_unit(repl_x):- repl_x.


can_open_console_x:- getenv('DISPLAY', _),
    absolute_file_name(path(xterm), _XTerm, [access(execute)]).

repl_x :-
    repl_x(_).

repl_x(Title) :- can_open_console_x, !,
    thread_self(Me),
    thread_create(thread_run_repl_x(Me, Title),
                  _Id,
                  [detached(true)]),
    thread_get_message(Msg),
    (   Msg=title(Title0)
    ->  Title=Title0
    ;   Msg=throw(Error)
    ->  throw(Error)
    ;   Msg=false
    ->  fail
    ).
repl_x(Title) :-
    print_message(error, cannot_attach_console_x(Title)),
    fail.


thread_has_console_x :- current_prolog_flag(break_level, _), !.
thread_has_console_x :- thread_self(Id), thread_has_console_x(Id), !.
thread_has_console_x(main) :- !.
thread_has_console_x(Id) :- thread_util:has_console(Id, _, _, _).

console_title_x(Thread, Title) :-
    current_prolog_flag(console_menu_version, qt), !, human_x_id(Thread, Id),
    format(atom(Title), 'Thread ~w', [Id]).
console_title_x(Thread, Title) :-
    current_prolog_flag(system_thread_id, SysId), human_x_id(Thread, Id), format(atom(Title),
    'MeTTa Thread ~w (~d) X-REPL', [Id, SysId]).

human_x_id(Thread, Id) :- atom(Thread), !, Id=Thread.
human_x_id(Thread, Id) :- thread_property(Th, alias(Id)), (Th==Thread; Thread==Id), !.
human_x_id(Thread, Id) :- thread_property(Th, id(Id)), (Th==Thread; Thread==Id), !.
human_x_id(Thread, Id) :- format(atom(Id),'~q',[Thread]), !.


attach_console_x :- attach_console_x(_).

attach_console_x(_) :- thread_has_console_x, !.
attach_console_x(Title) :- can_open_console_x, !,
    thread_self(Id),
    (   var(Title)
    ->  console_title_x(Id, Title)
    ;   true
    ),
    thread_util:open_console(Title, In, Out, Err),
    assert(thread_util:has_console(Id, In, Out, Err)),
    set_stream(In, alias(user_input)),
    set_stream(Out, alias(user_output)),
    set_stream(Err, alias(user_error)),
    set_stream(In, alias(current_input)),
    set_stream(Out, alias(current_output)),
    thread_util:enable_line_editing(In, Out, Err),
    thread_at_exit(detach_console_x(Id)).
attach_console_x(Title) :-
    print_message(error, cannot_attach_console(Title)),
    fail.

detach_console_x(Id) :-
    (   retract(thread_util:has_console(Id, In, Out, Err))
    ->  thread_util:disable_line_editing(In, Out, Err),
        close(In, [force(true)]),
        close(Out, [force(true)]),
        close(Err, [force(true)])
    ;   true
    ).


thread_run_repl_x :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    attach_console_x(_Title),
    print_message(banner, thread_welcome),
    repl.

thread_run_repl_x(Creator, Title) :-
    set_prolog_flag(query_debug_settings, debug(false, false)),
    Error=error(Formal, _),
    (   catch(attach_console_x(Title), Error, true)
    ->  (   var(Formal)
        ->  thread_send_message(Creator, title(Title)),
            print_message(banner, thread_welcome),
            repl
        ;   thread_send_message(Creator, throw(Error))
        )
    ;   thread_send_message(Creator, false)
    ).


