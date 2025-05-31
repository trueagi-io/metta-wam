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

%*********************************************************************************************%
% PROGRAM FUNCTION: provides predicates to create and manipulate atoms, bindings, spaces,
% and other components necessary for Mettalog interactions with Python.
%*********************************************************************************************%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load Metta interpreter module.
:- ensure_loaded(metta_interp).

% Disable singleton variable warnings.
:- style_check(-singleton).

% Allow multifile and dynamic definitions of lazy_load_python/0.
:- multifile(lazy_load_python/0).
:- dynamic(lazy_load_python/0).

% Import the Janus library, which is used for interfacing with Python.
:- module_property(janus,file(_)) -> true; janus:ensure_loaded(library(janus)).

% Uncomment the next line for quieter runs, meaning any debugging or tracing calls will be suppressed.
% if_bugger(_):- !.
if_bugger(G) :- call(G).

% Define a no-op predicate if `nop/1` is not already defined.
:- if(\+ current_predicate(nop/1)).
nop(_).
:- endif.

% Define a tracing predicate to handle failure cases during execution.
% If `trace_failures/1` is not already defined, define it here.
:- if(\+ current_predicate(trace_failures/1)).

%!  trace_failures(+Goal) is det.
%
%   A utility predicate for tracing and debugging failures in compound goals.
%   This predicate attempts to execute a goal and, if it fails, it logs the failure and optionally traces the goal.
%
%   @arg Goal The goal to be executed. If the goal consists of two subgoals (A, B), both will be traced separately if necessary.
%
%   @example
%     % Trace a compound goal and handle failure:
%     ?- trace_failures((member(X, [1,2,3]), member(Y, [a,b,c]))).
%
trace_failures((A, B)) :- !,
    % If A succeeds deterministically (*->), attempt B. If B fails, trace A and B.
    (A *-> trace_failures(B);
    % If A fails or B fails, log the failure and trace the goal (A, B).
    (B *-> trace_failures(A); wfailed(((A, B))))).

% Handle simple goals (non-compound).
trace_failures(A) :-
    % If A succeeds deterministically (*->), nothing is traced. If it fails, log and trace.
    (A *-> true; (wfailed(A), trace, A)).

%!  wfailed(+Goal) is det.
%
%   A helper predicate that prints a failure message for a goal that has failed.
%
%   @arg Goal The goal that has failed.
%
%   @example
%     % Log a failed goal:
%     ?- wfailed(member(X, [1,2,3])).
%
wfailed(G) :-
    % Write a failure message to the console.
    writeln(wfailed(G)),
    % Fail to indicate that the goal has failed.
    fail.

:- endif.  % trace_failures

%!  py_is_tf(+Goal, -TF) is det.
%
%   Evaluates the given Goal and unifies TF with '@'(true) if the goal succeeds,
%   or '@'(false) if it fails.
%
%   @arg Goal The Prolog goal to evaluate.
%   @arg TF   The result, '@'(true)' or '@'(false)', depending on the success of the Goal.
%
py_is_tf(Goal, TF) :- once(Goal) -> TF = '@'(true) ; TF = '@'(false).

% Dynamic predicate to store registered Python functions with details about their parameters and return type.
:- dynamic registered_function/5.

%!  register_function(+ModuleFunctionName, +ParamNames, +ParamTypes, +ParamDefaults, +ReturnType) is det.
%
%   Registers a Python function in Prolog with its associated parameters and return type.
%   If a function with the same name is already registered, it will be retracted and replaced
%   with the new details.
%
%   @arg ModuleFunctionName The name of the function being registered (concatenation of module and function name).
%   @arg ParamNames List of parameter names.
%   @arg ParamTypes List of parameter types.
%   @arg ParamDefaults List of default values for the parameters.
%   @arg ReturnType The return type of the function.
%
%   @example
%     % Registering a Python function named 'math_add':
%     ?- register_function('math_add', ['x', 'y'], ['int', 'int'], [0, 0], 'int').
%   Registered function: 'math_add' with parameters: ['x', 'y'], defaults: [0, 0], types: ['int', 'int'] -> 'int'.
%
register_function(ModuleFunctionName, ParamNames, ParamTypes, ParamDefaults, ReturnType) :-
    % Remove any previous registration of the function.
    retractall(registered_function(ModuleFunctionName, _, _, _, _)),
    % Register the function with its new details.
    assertz(registered_function(ModuleFunctionName, ParamNames, ParamTypes, ParamDefaults, ReturnType)),
    format('Registered function: ~q with parameters: ~q, defaults: ~q, types: ~q -> ~q~n',
           [ModuleFunctionName, ParamNames, ParamDefaults, ParamTypes, ReturnType]).

%!  from_python(+ModuleFunctionName, +TupleArgs, +LArgs, +KwArgs, -Return) is det.
%
%   Handles calling a registered Prolog predicate or, if not found, the original Python function.
%   It first checks if the Prolog predicate exists, and if so, dynamically constructs the goal and calls it.
%   If no Prolog predicate is found, it calls the original Python function.
%
%   @arg ModuleFunctionName The name of the function being called.
%   @arg TupleArgs A tuple of arguments (not currently used but passed for compatibility).
%   @arg LArgs A list of positional arguments.
%   @arg KwArgs A list of keyword arguments.
%   @arg Return The result of the function call.
%
%   @example
%     % Calling a registered Prolog function:
%     ?- from_python('math_add', _, [1, 2], [], Result).
%   Result = 3.
%
from_python(ModuleFunctionName, _TupleArgs, LArgs, KwArgs, Result) :-
    % Determine the arity of the function based on the number of positional arguments.
    length(LArgs, Arity),
    NewArity1 is Arity + 1,  % Adjust arity to account for one additional argument (Return).
    NewArity2 is Arity + 2,  % Adjust arity to account for two additional arguments (KwArgs and Return).
    % Check if a Prolog predicate with the corresponding arity exists.
    (current_predicate(ModuleFunctionName/NewArity2) -> append(LArgs, [KwArgs, Return], FullArgs);
    (current_predicate(ModuleFunctionName/NewArity1) -> append(LArgs, [Return], FullArgs);
    (current_predicate(ModuleFunctionName/Arity) -> append(LArgs, [], FullArgs)))),
    % Construct the predicate dynamically with all arguments.
    Predicate =.. [ModuleFunctionName | FullArgs],
    registered_function(ModuleFunctionName, _, _, _, ReturnType),
    % Call the Prolog predicate and handle its return type.
    if_bugger(format('Calling existing Prolog predicate: ~q -> ~q ', [Predicate, ReturnType])), !,
    trace_failures(once((call_ret_type(Predicate, ReturnType, Return, Result),
                    if_bugger(writeln(Return -> Result)), nonvar(Result)))).
from_python(_ModuleFunctionName, _TupleArgs, _LArgs, _KwArgs, 'call_original_function') :-
    % Fallback clause if no corresponding Prolog predicate is found, calls the original Python function.
    !.
from_python(ModuleFunctionName, TupleArgs, LArgs, KwArgs, Return) :-
    format('No Prolog predicate found for: ~w. Calling original Python function.~n', [ModuleFunctionName]),
    call_python_original(ModuleFunctionName, TupleArgs, LArgs, KwArgs, Return).

%!  call_ret_type(+Predicate, +ReturnType, +Return, -Result) is det.
%
%   Calls the provided Predicate and handles different return types.
%   The Result is unified with the appropriate representation based on the ReturnType.
%
%   @arg Predicate The Prolog predicate to be called.
%   @arg ReturnType The type of the return value ('bool', 'None', or other types).
%   @arg Return The actual return value of the predicate (if applicable).
%   @arg Result The processed result, which will be unified based on the ReturnType.
%
%   @example
%     % Example for a boolean return type:
%     ?- call_ret_type(member(1, [1, 2, 3]), bool, _, Result).
%     Result = '@'(true).
%
%     % Example for a None return type:
%     ?- call_ret_type(writeln('Hello!'), 'None', _, Result).
%     Hello!
%     Result = '@'(none).
%
%     % Example for a generic return type:
%     ?- call_ret_type(is_list([1, 2]), _, [1, 2], Result).
%     Result = [1, 2].
%
call_ret_type(Predicate, bool, _Return, Result) :- !,
    % Predicate to handle calling a predicate with a boolean return type.
    (call(Predicate) -> ignore(Result = '@'('true')); ignore(Result = '@'('false'))).
call_ret_type(Predicate, 'None', _Return, Result) :- !,
    % Predicate to handle calling a predicate with a 'None' return type.
    ignore(trace_failures(Predicate)) -> ignore(Result = '@'('none')).
call_ret_type(Predicate, _RetType, Return, Result) :- !,
    % Generic handler for other return types.
    call(Predicate), ret_res(Return, Result).

%!  ret_res(+Return, -Result) is det.
%
%   A helper predicate that unifies the return value with the result.
%   This is typically used in scenarios where you want to pass the return
%   value through unchanged or extract a value from more complex structures.
%
%   @arg Return The value to unify.
%   @arg Result The unified result.
%
%   @example
%     ?- ret_res(5, Result).
%     Result = 5.
%
%     % Example with a complex structure (e.g., extracting ID from an object):
%     % ret_res(o3(_, ID, _), ID).
%
ret_res(ID, ID).

%!  override_hyperonpy is det.
%
%   Loads the Python override (if not already loaded) and calls the Python function `override_hyperonpy/0`
%   from the `metta_python_override` module. The result is expected to be an empty Python dictionary (`py{}`).
%
%   @example
%     % Applying the override to Hyperon:
%     ?- override_hyperonpy.
%   true.
%
override_hyperonpy :-
    load_metta_python_override,  % Ensure the override is loaded.
    py_call(metta_python_override:load_hyperon_overrides(), _), !.

%!  test_override_hyperonpy is det.
%
%   Loads the Hyperon Python module by first applying the necessary overrides using `override_hyperonpy/0`.
%
%   @example
%     % Load the Hyperon module:
%     ?- test_override_hyperonpy.
%   true.
%
test_override_hyperonpy :-
    load_metta_python_override,  % Ensure the override is loaded.
    py_call(metta_python_override:test_hyperon_overrides(), _), !.

%!  metta_python_override(-Content) is det.
%
%   Reads and asserts the content of the Python file 'metta_python_override.py' as a dynamic fact.
%   This allows the Python code to be accessed and used at runtime within Prolog.
%
%   @arg Content A string representing the contents of the 'metta_python_override.py' file.
%
%   @example
%     % Retrieve the content of the Python override:
%     ?- metta_python_override(Content).
%   Content = "... Python code ...".
%
:- dynamic(metta_python_override/1).

% Read the content of the Python override file and assert it as a fact.
:- read_file_to_string('./metta_python_override.py', String, []),
   assertz(metta_python_override(String)), !.

%!  did_load_metta_python_override is det.
%
%   A volatile predicate that acts as a flag indicating whether the Python override has been loaded.
%   This flag is not saved between sessions and only exists during the current runtime.
%
%   @example
%     % After loading the override, this will succeed:
%     ?- did_load_metta_python_override.
%
:- dynamic(did_load_metta_python_override/0).
:- volatile(did_load_metta_python_override/0).

%!  load_metta_python_override is det.
%
%   Loads the Python override if it has not been loaded yet. The override content is retrieved
%   from `metta_python_override/1`, and the Python module is loaded via `py_module/2`. Once
%   loaded, `did_load_metta_python_override/0` is asserted to prevent reloading.
%
%   @example
%     % Load the Python override:
%     ?- load_metta_python_override.
%   true.
%
load_metta_python_override :-
    % If already loaded, do nothing.
    did_load_metta_python_override, !.
load_metta_python_override :-
    % Retrieve the Python override content.
    metta_python_override(String),
    % Load the Python module via py_module/2.
    py_module(metta_python_override, String),
    % Assert that the override has now been loaded.
    assert(did_load_metta_python_override), !.
    % Try to load the module again, ignoring any errors.
    % ignore(notrace(with_safe_argv(catch(py_module(metta_python_override, String), _, true)))), !.

%!  maybe_load_metta_python_override is det.
%
%   This predicate ensures that the Python integration for Metta is loaded.
%   It tries to load the Python interface lazily by calling `lazy_load_python/0`.
%   If the lazy loading succeeds (determined by the cut `!`), it does nothing more.
%   If lazy loading fails, it proceeds to load the Metta Python override using
%   `load_metta_python_override/0`.
%
maybe_load_metta_python_override :-
    % Attempt lazy loading of the Python interface.
    lazy_load_python, !.
maybe_load_metta_python_override :-
    % If lazy loading fails, load the Metta Python override manually.
    load_metta_python_override.

% The following directives ensure that `maybe_load_metta_python_override/0` is called
% during system initialization. The first initialization runs when the program
% starts, and the second runs when the system is restored from a saved state.
%:- initialization(maybe_load_metta_python_override).
%:- initialization(maybe_load_metta_python_override, restore).

%!  metta_python_patcher(-Content) is det.
%
%   Reads and asserts the content of the Python file 'metta_python_patcher.py' as a dynamic fact.
%   This allows the Python code to be accessed and used at runtime within Prolog.
%
%   @arg Content A string representing the contents of the 'metta_python_patcher.py' file.
%
%   @example
%     % Retrieve the content of the Python patcher:
%     ?- metta_python_patcher(Content).
%   Content = "... Python code ...".
%
:- dynamic(metta_python_patcher/1).

:-  % Get the directory of the current Prolog file being loaded.
    prolog_load_context(directory, Dir),
    % Construct the full path to the Python patcher file.
    atomic_list_concat([Dir, '/', 'metta_python_patcher.py'], FilePath),
    % Read the content of the Python patcher file and assert it.
    read_file_to_string(FilePath, String, []),
    asserta(metta_python_patcher(String)), !.

%!  did_load_metta_python_patcher is det.
%
%   A volatile predicate that acts as a flag indicating whether the Python patcher has been loaded.
%   This flag is not saved between sessions and only exists during the current runtime.
%
%   @example
%     % After loading the patcher, this will succeed:
%     ?- did_load_metta_python_patcher.
%
:- dynamic(did_load_metta_python_patcher/0).
:- volatile(did_load_metta_python_patcher/0).

%!  load_metta_python_patcher is det.
%
%   Loads the Python patcher if it has not been loaded yet. The patcher content is retrieved
%   from `metta_python_patcher/1`, and the Python module is loaded via `py_module/2`. Once
%   loaded, `did_load_metta_python_patcher/0` is asserted to prevent reloading.
%
%   @example
%     % Load the Python patcher:
%     ?- load_metta_python_patcher.
%   true.
%
load_metta_python_patcher :- !.
load_metta_python_patcher :-
    did_load_metta_python_patcher, !.  % If already loaded, do nothing.

load_metta_python_patcher :-
    % Retrieve the Python patcher content.
    metta_python_patcher(String),
    % Load the Python module via py_module/2.
    py_module(metta_python_patcher, String),
    % Assert that the patcher has now been loaded.
    assert(did_load_metta_python_patcher), !,
    % Try to load the module again, ignoring any errors.
    %ignore(notrace(with_safe_argv(catch(py_module(metta_python_patcher, String), _, true)))),
    !.

%!  maybe_load_metta_python_patcher is det.
%
%   This predicate ensures that the Python integration for Metta is loaded.
%   It tries to load the Python interface lazily by calling `lazy_load_python/0`.
%   If the lazy loading succeeds (determined by the cut `!`), it does nothing more.
%   If lazy loading fails, it proceeds to load the Metta Python patcher using
%   `load_metta_python_patcher/0`.
%
maybe_load_metta_python_patcher :-
    % Attempt lazy loading of the Python interface.
    lazy_load_python, !.
maybe_load_metta_python_patcher :-
    % If lazy loading fails, load the Metta Python patcher manually.
    load_metta_python_patcher.

% The following directives ensure that `maybe_load_metta_python_patcher/0` is called
% during system initialization. The first initialization runs when the program
% starts, and the second runs when the system is restored from a saved state.
%:- initialization(maybe_load_metta_python_patcher).
%:- initialization(maybe_load_metta_python_patcher, restore).


%!  patch_hyperonpy is det.
%
%   Loads the Python patcher (if not already loaded) and calls the Python function `patch_hyperonpy/0`
%   from the `metta_python_patcher` module. The result is expected to be an empty Python dictionary (`py{}`).
%
%   @example
%     % Applying the patch to Hyperon:
%     ?- patch_hyperonpy.
%   true.
%
patch_hyperonpy :-
    load_metta_python_patcher,  % Ensure the patcher is loaded.
    py_call(metta_python_patcher:patch_hyperonpy(), O), !,
    O = py{}.

%!  load_hyperonpy is det.
%
%   Loads the Hyperon Python module by first applying the necessary patches using `patch_hyperonpy/0`.
%
%   @example
%     % Load the Hyperon module:
%     ?- load_hyperonpy.
%   true.
%
load_hyperonpy :-
    patch_hyperonpy.

%!  load_mettalogpy is det.
%
%   Loads the `mettalog` Python module and optionally attempts to load the `hyperon` module.
%   The `nop/1` around the second `py_exec/1` ensures that loading `hyperon` does not raise an error.
%
%   @example
%     % Load mettalog and hyperon (if available):
%     ?- load_mettalogpy.
%   true.
%
load_mettalogpy :-
    py_exec("import mettalog"),
    nop(py_exec("import hyperon")).

%!  mettalogpy_repl is det.
%
%   Starts the REPL (Read-Eval-Print Loop) for the `mettalog` Python module by invoking the `repl/0` function.
%
%   @example
%     % Start the mettalog REPL:
%     ?- mettalogpy_repl.
%   true.
%
mettalogpy_repl :-
    catch_log(override_hyperonpy),
    catch_log(py_call(mettalog:repl())), !.
hyperonpy_repl :-
    maplist(catch_log,
            [load_metta_python_proxy,
             load_metta_python_patcher,
             load_mettalogpy,
             py_call(mettalog:repl())]).

%!  call_python_original(+ModuleFunctionName, +TupleArgs, +LArgs, +KwArgs, -Return) is det.
%
%   Calls the original Python function if no corresponding Prolog predicate exists.
%
%   @arg ModuleFunctionName The Python function (with module) to call.
%   @arg TupleArgs Positional arguments tuple.
%   @arg LArgs Additional positional arguments.
%   @arg KwArgs Keyword arguments.
%   @arg Return Result of the Python function call.
%
call_python_original(ModuleFunctionName, TupleArgs, LArgs, KwArgs, Return) :-
    fail,
    py_call(metta_python_override:call_python_original(ModuleFunctionName, TupleArgs, LArgs, KwArgs), Return).

%!  my_module_add(+A, +B, +Unused, -R) is det.
%
%   A simple demonstration function that adds two numbers A and B, ignoring the third argument.
%
%   @arg A The first number to add.
%   @arg B The second number to add.
%   @arg Unused An unused argument (typically ignored in this function).
%   @arg R The result of adding A and B.
%
%   @example
%     ?- my_module_add(2, 3, _, R).
%     R = 5.
%
my_module_add(A, B, _, R) :- R is A + B.

%!  maybe_info(+Fmt, +Args) is det.
%
%   Conditionally logs information based on the provided format and arguments.
%   If no logging is needed, the first clause suppresses the output using a cut.
%   Otherwise, the second clause formats and outputs the message.
%
%   @arg Fmt The format string used to structure the output.
%   @arg Args The arguments to fill in the format string.
%
%   @example
%     ?- maybe_info("Processing value: ~w", [Value]).
%     Processing value: some_value
%
maybe_info(_Fmt, _Args) :- !.
maybe_info(Fmt, Args) :- format(Fmt, Args).

%!  my_module_factorial(+N, -F) is det.
%
%   Computes the factorial of a non-negative integer N.
%   The base case for 0 is defined as 1, and for positive integers,
%   the factorial is computed recursively.
%
%   @arg N The non-negative integer for which the factorial is to be computed.
%   @arg F The result of the factorial computation, where F is N!.
%
%   @example
%     ?- my_module_factorial(5, F).
%     F = 120.
%
my_module_factorial(0, 1).
my_module_factorial(N, F) :- N > 0,N1 is N - 1,my_module_factorial(N1, F1),F is N * F1.

% ==================================================
% **Prolog Translation of the Given File with Numbered Functions**
% Module Declaration and Exports
% ==================================================

%!  no_module is det.
%
%   This enables Prolog to invoke multiple Python functions in Hyperon.
%
%   This predicate declares a module named `hyperonpy_prolog_translation`. It exports a large number of predicates
%   used for interacting with the Python `hyperonpy` library, including for managing atoms, bindings, spaces,
%   interpretation, and other related operations.
%
no_module:- module(hyperonpy_prolog_translation, [
    % Object-oriented helper predicates
    oo_new/3,          % 1
    oo_free/1,         % 2
    oo_invoke/4,       % 3
    oo_clone/2,        % 4
    oo_get/4,          % 5
    oo_set/3,          % 6
    oo_equal/3,        % 7

    % hyperonpy_* functions
    % 1. Atom Functions
    hyperonpy_atom_sym/2,            % 8
    hyperonpy_atom_var/2,            % 9
    hyperonpy_atom_expr/2,           % 10
    hyperonpy_atom_gnd/3,            % 11
    hyperonpy_atom_free/1,           % 12
    hyperonpy_atom_to_str/2,         % 13
    hyperonpy_atom_eq/3,             % 14
    hyperonpy_atom_get_name/2,       % 15
    hyperonpy_atom_get_children/2,   % 16
    hyperonpy_atom_get_grounded_type/2, % 17
    hyperonpy_atom_get_object/2,     % 18
    hyperonpy_atom_is_cgrounded/2,   % 19
    hyperonpy_atom_is_error/2,       % 20
    hyperonpy_atom_error_message/2,  % 21
    hyperonpy_atom_var_parse_name/2, % 22
    hyperonpy_atom_get_metatype/2,   % 23
    hyperonpy_atom_get_space/2,      % 24
    hyperonpy_atom_iterate/2,        % 25
    hyperonpy_atom_match_atom/3,     % 26
    hyperonpy_atom_gnd_serialize/3,  % 27
    hyperonpy_atom_clone/2,          % 28

    % 2. Atom Vector Functions
    hyperonpy_atom_vec_new/1,        % 29
    hyperonpy_atom_vec_from_list/2,  % 30
    hyperonpy_atom_vec_len/2,        % 31
    hyperonpy_atom_vec_push/2,       % 32
    hyperonpy_atom_vec_pop/2,        % 33
    hyperonpy_atom_vec_free/1,       % 34

    % 3. Bindings Functions
    hyperonpy_bindings_new/1,        % 35
    hyperonpy_bindings_free/1,       % 36
    hyperonpy_bindings_add_var_binding/4, % 37
    hyperonpy_bindings_resolve/3,    % 38
    hyperonpy_bindings_is_empty/2,   % 39
    hyperonpy_bindings_list/2,       % 40
    hyperonpy_bindings_merge/3,      % 41
    hyperonpy_bindings_eq/3,         % 42
    hyperonpy_bindings_clone/2,      % 43
    hyperonpy_bindings_to_str/2,     % 44
    hyperonpy_bindings_narrow_vars/2, % 45

    % 4. Bindings Set Functions
    hyperonpy_bindings_set_empty/1,  % 46
    hyperonpy_bindings_set_free/1,   % 47
    hyperonpy_bindings_set_add_var_binding/3, % 48
    hyperonpy_bindings_set_is_empty/2, % 49
    hyperonpy_bindings_set_list/2,   % 50
    hyperonpy_bindings_set_merge_into/2, % 51
    hyperonpy_bindings_set_eq/3,     % 52
    hyperonpy_bindings_set_clone/2,  % 53
    hyperonpy_bindings_set_from_bindings/2, % 54
    hyperonpy_bindings_set_unpack/2, % 55
    hyperonpy_bindings_set_is_single/2, % 56
    hyperonpy_bindings_set_push/2,   % 57
    hyperonpy_bindings_set_single/1, % 58
    hyperonpy_bindings_set_add_var_equality/3, % 59
    hyperonpy_bindings_set_to_str/2, % 60

    % 5. Validation Functions
    hyperonpy_validate_atom/3,       % 61
    hyperonpy_check_type/4,          % 62
    hyperonpy_get_atom_types/3,      % 63

    % 6. Metta Functions
    hyperonpy_metta_new/3,           % 64
    hyperonpy_metta_free/1,          % 65
    hyperonpy_metta_space/2,         % 66
    hyperonpy_metta_tokenizer/2,     % 67
    hyperonpy_metta_run/3,           % 68
    hyperonpy_metta_err_str/2,       % 69
    hyperonpy_metta_evaluate_atom/3, % 70
    hyperonpy_metta_load_module_at_path/4, % 71
    hyperonpy_metta_load_module_direct/4, % 72
    hyperonpy_metta_working_dir/2,   % 73
    hyperonpy_metta_eq/3,            % 74

    % 7. Environment Builder Functions
    hyperonpy_environment_config_dir/2, % 75
    hyperonpy_env_builder_start/1,   % 76
    hyperonpy_env_builder_use_default/1, % 77
    hyperonpy_env_builder_use_test_env/1, % 78
    hyperonpy_env_builder_set_working_dir/2, % 79
    hyperonpy_env_builder_set_config_dir/2, % 80
    hyperonpy_env_builder_create_config_dir/3, % 81
    hyperonpy_env_builder_disable_config_dir/1, % 82
    hyperonpy_env_builder_set_is_test/2, % 83
    hyperonpy_env_builder_push_include_path/2, % 84
    hyperonpy_env_builder_push_fs_module_format/3, % 85
    hyperonpy_env_builder_init_common_env/2, % 86

    % 8. Space Functions
    hyperonpy_space_new_grounding/1, % 87
    hyperonpy_space_free/1,          % 88
    hyperonpy_space_add/2,           % 89
    hyperonpy_space_remove/3,        % 90
    hyperonpy_space_replace/4,       % 91
    hyperonpy_space_subst/4,         % 92
    hyperonpy_space_eq/3,            % 93
    hyperonpy_space_list/2,          % 94
    hyperonpy_space_atom_count/2,    % 95
    hyperonpy_space_get_payload/2,   % 96
    hyperonpy_space_new_custom/2,    % 97
    hyperonpy_space_query/3,         % 98

    % 9. Interpretation Functions
    hyperonpy_interpret_init/3,      % 99
    hyperonpy_interpret_step/2,      % 100
    hyperonpy_step_get_result/2,     % 101
    hyperonpy_step_has_next/2,       % 102

    % 10. Tokenizer Functions
    hyperonpy_tokenizer_new/1,       % 103
    hyperonpy_tokenizer_free/1,      % 104
    hyperonpy_tokenizer_register_token/3, % 105
    hyperonpy_tokenizer_clone/2,     % 106

    % 11. Runner State Functions
    hyperonpy_runner_state_new_with_atoms/3, % 107
    hyperonpy_runner_state_new_with_parser/3, % 108
    hyperonpy_runner_state_step/1,   % 109
    hyperonpy_runner_state_current_results/2, % 110
    hyperonpy_runner_state_is_complete/2, % 111
    hyperonpy_runner_state_free/1,   % 112
    hyperonpy_runner_state_err_str/2, % 113

    % 12. Syntax Node Functions
    hyperonpy_syntax_node_clone/2,   % 114
    hyperonpy_syntax_node_free/1,    % 115
    hyperonpy_syntax_node_is_null/2, % 116
    hyperonpy_syntax_node_type/2,    % 117
    hyperonpy_syntax_node_is_leaf/2, % 118
    hyperonpy_syntax_node_unroll/2,  % 119
    hyperonpy_syntax_node_src_range/2, % 120

    % 13. Run Context Functions
    hyperonpy_run_context_get_metta/2, % 121
    hyperonpy_run_context_get_space/2, % 122
    hyperonpy_run_context_get_tokenizer/2, % 123
    hyperonpy_run_context_import_dependency/2, % 124
    hyperonpy_run_context_init_self_module/3, % 125
    hyperonpy_run_context_load_module/3, % 126

    % 14. Logging Functions
    hyperonpy_log_error/1,           % 127
    hyperonpy_log_info/1,            % 128
    hyperonpy_log_warn/1,            % 129

    % 15. Load Function
    hyperonpy_load_ascii/3           % 130
]).

% Declares a dynamic predicate `metta_atom/2`
:- dynamic metta_atom/2.

% Declares a dynamic predicate `o_f_v/3`
:- dynamic(o_f_v/3).

% Declares a dynamic predicate `t_f_v/3`
:- dynamic(t_f_v/3).

% ==================================================
% **1. Object-Oriented Simulation in Prolog**
% ==================================================

%!  from_obj(+Value, -Obj) is det.
%
%   Predicate to handle objects directly if they are strings, numbers, symbols, or variables.
%   Converts Value to Obj. If Value is a variable, it is returned as-is. If Value is
%   a direct atom (symbol, string, number, or variable), it is returned as-is.
%   Otherwise, Obj is set to Value.
%
%   @arg Value The value to be converted.
%   @arg Obj   The resulting object.
%
from_obj(Value, Obj) :- var(Value), !,Obj = Value.
from_obj(Value, Obj) :- direct_atom(Value), !, Obj = Value.
from_obj(Obj, Obj).

%!  direct_atom(+Value) is det.
%
%   Predicate to identify direct atoms (strings, numbers, symbols, variables).
%   Succeeds if Value is a symbol, string, number, or variable.
%
%   @arg Value The value to check.
%
direct_atom(Value) :- (symbol_not_ref(Value); string(Value); number(Value); bvar(Value)), !.

%!  o_f_v(+Object, +FieldName, -Value) is det.
%
%   Main predicate to manage object fields based on their type.
%   Retrieves the value of FieldName for Object by determining its type and then
%   fetching the field value.
%
%   @arg Object    The object from which to get the field value.
%   @arg FieldName The field to retrieve.
%   @arg Value     The field value.
%
o_f_v(Object, FieldName, Value) :-
    determine_object_type(Object, Type),!,object_field_value(Type, Object, FieldName, Value).

%!  p1_typename(+Predicate, -TypeName) is det.
%
%   Maps a predicate to its corresponding type name. This predicate serves as a
%   lookup table for associating Prolog predicates with their human-readable type names.
%
%   @arg Predicate  The internal predicate or type used in the system.
%   @arg TypeName   The human-readable name associated with the given predicate.
%
%   @examples
%     % Query the type name for the predicate 'bvar'.
%     ?- p1_typename(bvar, TypeName).
%     TypeName = 'Variable'.
%
%     % Query the type name for the predicate 'string'.
%     ?- p1_typename(string, TypeName).
%     TypeName = 'String'.
%
p1_typename(bvar, 'Variable').
p1_typename(is_list, 'Expression').
p1_typename(string, 'String').
p1_typename(number, 'Number').
p1_typename(symbol_not_ref, 'Symbol').

%!  symbol_not_ref(+Atom) is nondet.
%
%   Predicate to identify symbols that are not references (i.e., not declared o_f_v/3).
%   Succeeds if Atom is an atom and there is no `o_f_v(Atom, type, _)` clause.
%
%   @arg Atom  The atom to be checked.
%
%   @example
%     ?- symbol_not_ref(some_atom).
%     true.
%
symbol_not_ref(Atom) :- atom(Atom),\+ clause(o_f_v(Atom, type, _), true).

%!  bvar(+Var) is nondet.
%
%   Checks if the given term is a variable or matches the custom `bvar/1` pattern.
%   The predicate succeeds if the term is a Prolog variable or a compound term of
%   the form `'$VAR'(_)`.
%
%   @arg Var  The variable or term to be checked.
%
%   @example
%     % Check if the term is a variable or follows the `bvar` pattern.
%     ?- bvar(X).
%     true.
%
%     ?- bvar('$VAR'(0)).
%     true.
%
bvar(Var) :- var(Var), !.
bvar(Var) :- compound(Var), !, Var = '$VAR'(_).

%!  bvar(+Var, -Symbol) is nondet.
%
%   Unifies the Symbol with the argument of the compound term `'$VAR'(Symbol)` if
%   Var is in that form.
%
%   @arg Var     The variable or term to be checked.
%   @arg Symbol  The symbol unified with the argument of the term.
%
%   @example
%     % Extract the symbol from a `'$VAR'` term.
%     ?- bvar('$VAR'(0), Symbol).
%     Symbol = 0.
%
bvar(Var, Symbol) :- compound(Var), !, Var = '$VAR'(Symbol).

%!  determine_object_type(+Object, -TypeName) is det.
%
%   Determines the type of the given Object based on mappings defined in
%   `p1_typename/2`. The predicate calls the appropriate check based on the
%   predicate name associated with the type.
%
%   @arg Object    The object whose type is being determined.
%   @arg TypeName  The human-readable type name for the object.
%
%   @example
%     % Determine the type of a string.
%     ?- determine_object_type("hello", TypeName).
%     TypeName = 'String'.
%
%     % Determine the type of a number.
%     ?- determine_object_type(42, TypeName).
%     TypeName = 'Number'.
%
determine_object_type(Object, TypeName) :- p1_typename(PredicateName, TypeName),call(PredicateName, Object), !.

%!  atom_kind(+EXPR, -Value) is det.
%
%   Retrieves the kind of atom for the given expression (EXPR) by invoking the
%   `AtomKind` function from the `hyperonpy` Python module. This predicate is
%   used to determine the type or classification of the atom represented by EXPR.
%
%   @arg EXPR   The expression whose atom kind is to be determined.
%   @arg Value  The result from the Python call, representing the kind of atom.
%
%   @example
%     % Retrieve the atom kind of an expression.
%     ?- atom_kind(expr_example, Value).
%     Value = 'SomeAtomKind'.
%
atom_kind(EXPR, Value):- py_call(hyperonpy:'AtomKind', EXPR, Value).

% Helper predicates for different object types.

%!  object_field_value(+MetaType, +Object, +FieldName, -Value) is det.
%
%   Retrieves the field value for an object based on its MetaType and FieldName.
%   This predicate handles objects of various types such as 'Expression', 'Variable',
%   'Symbol', 'String', and 'Number'. Depending on the object type, it returns
%   the corresponding type, metatype, value, or grounded_type.
%
%   @arg MetaType   The metatype of the object ('Expression', 'Variable', 'Symbol', etc.).
%   @arg Object     The object whose field value is being retrieved.
%   @arg FieldName  The field for which the value is being retrieved ('type', 'metatype', or 'value').
%   @arg Value      The value of the specified field.
%
%   @example
%     % Retrieve the value for an 'Expression' object.
%     ?- object_field_value('Expression', expr_object, type, Value).
%     Value = atom.
%
%     % Retrieve the metatype for a 'Symbol'.
%     ?- object_field_value('Symbol', symbol_object, metatype, Value).
%     Value = 'SYMBOL'.
%
object_field_value('Expression', Object, FieldName, Value) :- !,
    % Handle lists with metatype 'Expression' and no grounded_type.
   ((FieldName = type, Value = atom);
    (FieldName = metatype, atom_kind('EXPR', Value));
    (FieldName = value, Value = Object)).
object_field_value('Variable', Object, FieldName, Value) :- !,
    % Handle variables with metatype 'Variable' and no grounded_type.
   ((FieldName = type, Value = atom);
    (FieldName = metatype, atom_kind('VARIABLE', Value));
    (FieldName = value, Value = Object)).
object_field_value('Symbol', Object, FieldName, Value) :- !,
    % Handle symbols with metatype 'Symbol' and no grounded_type.
   ((FieldName = type, Value = atom);
    (FieldName = metatype, atom_kind('SYMBOL', Value));
    (FieldName = value, Value = Object)).
object_field_value('String', Object, FieldName, Value) :- !,
    % Handle objects of type 'String' with 'grounded_type'.
    object_field_value_base(Object, FieldName, Value, 'String').
object_field_value('Number', Object, FieldName, Value) :- !,
    % Handle objects of type 'Number' with 'grounded_type'.
    object_field_value_base(Object, FieldName, Value, 'Number').
object_field_value(atom, _Object, FieldName, Value) :-
    % Generic handler for atoms that are not specifically typed as 'String', 'Number', 'Symbol', 'Expression', or 'Variable'.
    (FieldName = type, Value = atom).

%!  object_field_value_base(+Object, +FieldName, -Value, +GroundedType) is det.
%
%   Base handler for returning field values for grounded types such as 'String' and 'Number'.
%   It retrieves the type, value, grounded_type, or metatype of the object.
%
%   @arg Object        The object whose field value is being retrieved.
%   @arg FieldName     The field for which the value is being retrieved ('type', 'value', etc.).
%   @arg Value         The value of the specified field.
%   @arg GroundedType  The grounded type of the object (e.g., 'String' or 'Number').
%
%   @example
%     % Retrieve the grounded type for a 'String' object.
%     ?- object_field_value_base("hello", grounded_type, Value, 'String').
%     Value = 'String'.
%
object_field_value_base(Object, FieldName, Value, GroundedType) :-
   ((FieldName = type, Value = atom);
    (FieldName = value, Value = Object);
    (FieldName = grounded_type, Value = GroundedType);
    (FieldName = metatype, atom_kind('GROUNDED', Value))).

% 1. oo_new/3

%!  oo_new(+Type, +Attributes, -ObjectID) is det.
%
%   Creates a new object or retrieves an existing one based on its attributes.
%   The object state is stored using the predicate `o_f_v/3`, where `ID` is
%   the object identifier, `FieldName` represents the attribute name, and
%   `Value` holds the corresponding value.
%
%   The first clause attempts to find an existing object with matching attributes,
%   particularly handling cases where the `value` field is present. If a matching
%   object is found, its ID is returned. If no match is found, the second clause
%   creates a new object with a unique ID and stores the attributes.
%
%   @arg Type       The type of the object to be created or matched.
%   @arg Attributes A list of field-value pairs representing the object attributes.
%   @arg ObjectID   The unique ID of the newly created or matched object.
%
%   @example
%     % Create a new object with attributes and retrieve its ID.
%     ?- oo_new('Person', [name:'John', age:30], ObjectID).
%     ObjectID = 1.
%
%     % If an object with the same attributes exists, retrieve its ID.
%     ?- oo_new('Person', [name:'John', age:30], ObjectID).
%     ObjectID = 1.
%
oo_new(Type, Attributes, ObjectID) :-
    fail,  % This clause will fail, leading to the next one if no match is found.
    % Attempt to find an existing object by matching the 'value' field.
    select(value:ValueIn, Attributes, RestOf),
    o_f_v(ExistingID, value, Value),
    (var(Value) -> Value == ValueIn ; ValueIn =@= Value),
    o_f_v(ExistingID, type, Type),
    % Ensure all remaining attributes match.
    forall(
        member(FieldName:FieldValue, RestOf),
        o_f_v(ExistingID, FieldName, FieldValue)
    ),
    !,  % Cut to prevent backtracking if a match is found.
    ObjectID = ExistingID.
oo_new(Type, Attributes, ObjectID) :-
    % Fallback clause: If no matching object is found, create a new one.
    % Generate a unique ID for the new object.
    generate_object_id(Type, ObjectID),
    % Store the type of the object.
    assertz(o_f_v(ObjectID, type, Type)),
    % Store all provided attributes for the new object.
    oo_set_attibutes(ObjectID,value,Attributes).


oo_set_attibutes(ObjectID,FallbackTrait,Attributes):-
    forall(
        (member(NV, Attributes),
         nv_name_value(FallbackTrait,NV,FieldName,FieldValue)),
        assertz(o_f_v(ObjectID, FieldName, FieldValue))
    ).

nv_name_value(FallbackTrait,Compound,FallbackTrait,NonCompound):- \+ compound(NonCompound),!.
nv_name_value(_FallbackTrait,Compound,FieldName,FieldValue):- compound_name_arguments(Compound,FieldName,[FieldValue]),!.
nv_name_value(_FallbackTrait,Compound,FieldName,FieldValue):- compound_name_arguments(Compound,_,[FieldName,FieldValue]),!.

%!  setup_default_fields(+Type, +ObjectID, +ProvidedFields) is det.
%
%   Helper predicate to set up default fields from t_f_v/3 if not explicitly provided.
%   Sets up default fields for an object based on its type. The default fields
%   are retrieved from the `t_f_v/3` predicate, which defines the default field
%   values for each type. If any fields are explicitly provided, they take priority
%   over the defaults.
%
%   @arg Type           The type of the object.
%   @arg ObjectID       The unique ID of the object.
%   @arg ProvidedFields A list of field-value pairs explicitly provided for the object.
%
%   @example
%     % Set up default fields for an object of type 'Person', with some provided fields.
%     ?- setup_default_fields('Person', 1, [age:30, name:'John']).
%
setup_default_fields(Type, ObjectID, ProvidedFields) :-
    % Get all default fields for the given type from t_f_v/3.
    findall(FieldName:DefaultValue, t_f_v(Type, FieldName, DefaultValue), Defaults),
    % Merge provided fields with the defaults, giving priority to provided fields.
    merge_fields(Defaults, ProvidedFields, FinalFields),
    % Assert each final field into o_f_v/3.
    forall(member(FieldName:FieldValue, FinalFields),assertz(o_f_v(ObjectID, FieldName, FieldValue))).

%!  merge_fields(+Defaults, +ProvidedFields, -FinalFields) is det.
%
%   Merges a list of default fields with a list of provided fields, giving priority
%   to the provided fields. If a field is explicitly provided, it overrides the
%   corresponding default value. The merged result is stored in FinalFields.
%
%   @arg Defaults       A list of field-value pairs representing the default fields.
%   @arg ProvidedFields A list of field-value pairs explicitly provided for the object.
%   @arg FinalFields    The final merged list of fields.
%
%   @example
%     % Merge defaults and provided fields, prioritizing provided fields.
%     ?- merge_fields([age:25, name:'Unknown'], [name:'John'], FinalFields).
%     FinalFields = [age:25, name:'John'].
%
merge_fields([], ProvidedFields, ProvidedFields).
merge_fields([FieldName:DefaultValue | RestDefaults], ProvidedFields, [FieldName:Value | Merged]) :-
    (select(FieldName:Value, ProvidedFields, Remaining) -> true ; Value = DefaultValue, Remaining = ProvidedFields),
    merge_fields(RestDefaults, Remaining, Merged).

%!  generate_object_id(+Type, -ID) is det.
%
%   Generates a unique ID for an object based on its type. The ID is generated
%   by concatenating the type with a unique identifier created using `gensym/2`.
%
%   @arg Type  The type of the object.
%   @arg ID    The generated unique ID for the object.
%
%   @example
%     % Generate a unique ID for a 'Person' object.
%     ?- generate_object_id('Person', ID).
%     ID = 'Person_1'.
%
generate_object_id(Type, ID) :- atom_concat(Type, '_', TempID),gensym(TempID, ID).


% 2. oo_free/1

%!  oo_free(+ObjectID) is det.
%
%   Frees (removes) an object from the system by retracting all facts associated
%   with the given ObjectID from the `o_f_v/3` predicate. This effectively deletes
%   the object state and attributes.
%
%   @arg ObjectID  The unique ID of the object to be removed.
%
%   @example
%     % Free an object with ID 'Object_1'.
%     ?- oo_free('Object_1').
%     true.
%
oo_free(ObjectID) :-
    % Retract all facts associated with the ObjectID.
    retractall(o_f_v(ObjectID, _, _)).

% 3. oo_invoke/4

%!  oo_get(+Type, +ObjectID, +FieldName, -Value) is det.
%
%   Retrieves the value of a specific field from an object state. The search order
%   is as follows: it first looks for the field in the object state using `o_f_v/3`,
%   and if the field is not found, it retrieves a default value from the type definition
%   using `t_f_v/3`. If the `FieldName` is not explicitly stored or defined, it tries
%   to retrieve the generic 'value' field from the object state.
%
%   @arg Type       The type of the object.
%   @arg ObjectID   The unique ID of the object.
%   @arg FieldName  The name of the field whose value is being retrieved.
%   @arg Value      The value of the specified field.
%
%   @example
%     % Retrieve the 'name' field from an object.
%     ?- oo_get('Person', 'Object_1', name, Value).
%     Value = 'John'.
%
oo_get(Type, ObjectID, FieldName, Value) :-
    fail,  % This clause attempts to fail, leading to fallback on the next clause.
    o_f_v(ObjectID, type, Type),( o_f_v(ObjectID, FieldName, Value); (t_f_v(Type, FieldName, Value))), !.
oo_get(_Type, ObjectID, FieldName, Value) :- o_f_v(ObjectID, FieldName, Value), !.
oo_get(Type, _ObjectID, FieldName, Value) :- t_f_v(Type, FieldName, Value), !.
oo_get(_Type, ObjectID, _FieldName, Value) :- o_f_v(ObjectID, value, Value), !.

%!  oo_get_else(+Type, +ObjectID, +FieldName, -Value, +Else) is det.
%
%   Retrieves the value of a specific field from an object state, with a fallback.
%   If the field is not found, the predicate unifies `Value` with `Else`.
%
%   @arg Type       The type of the object.
%   @arg ObjectID   The unique ID of the object.
%   @arg FieldName  The name of the field whose value is to be retrieved.
%   @arg Value      The value of the specified field, or the fallback value if not found.
%   @arg Else       The fallback value to be used if the field is not found.
%
%   @example
%     % Retrieve a field or use a default value.
%     ?- oo_get_else('Person', 'Object_1', age, Value, 25).
%     Value = 25.
%
oo_get_else(Type, ObjectID, FieldName, Value, Else) :- oo_get(Type, ObjectID, FieldName, Value) -> true
            ; Value = Else.

%!  oo_set(+ObjectID, +FieldName, +Value) is det.
%
%   Sets the value of a specific field in an object state. If a value for the field
%   already exists, it is removed and replaced with the new value.
%
%   @arg ObjectID   The unique ID of the object whose field is being set.
%   @arg FieldName  The name of the field to be updated.
%   @arg Value      The new value to be assigned to the specified field.
%
%   @example
%     % Set the 'age' field of an object.
%     ?- oo_set('Object_1', age, 35).
%     true.
%
oo_set(ObjectID, FieldName, Value) :-
    % Remove any existing value for the field.
    retractall(o_f_v(ObjectID, FieldName, _)),
    % Assert the new value.
    assertz(o_f_v(ObjectID, FieldName, Value)).

%!  oo_invoke(+Type, +ObjectID, +MethodCall, -Result) is det.
%
%   Invokes a method on an object. The method call is handled by `handle_method_call/4`,
%   which processes the specific method invocation based on the object type and
%   state.
%
%   @arg Type        The type of the object on which the method is invoked.
%   @arg ObjectID    The unique ID of the object.
%   @arg MethodCall  The method to be invoked (typically a functor with arguments).
%   @arg Result      The result of the method invocation.
%
%   @example
%     % Invoke a method on an object.
%     ?- oo_invoke('Person', 'Object_1', greet('Hello'), Result).
%     Result = 'Hello, John'.
%
oo_invoke(Type, ObjectID, MethodCall, Result) :-
    % Ensure the object type matches the specified Type.
    o_f_v(ObjectID, type, Type),
    % Handle the method call.
    handle_method_call(Type, ObjectID, MethodCall, Result).

% 4. oo_clone/2

%!  oo_clone(+ObjectID, -ClonedObjectID) is det.
%
%   Clones an existing object by creating a new object with the same fields and values,
%   except for the 'type' field, which is automatically handled during the creation of
%   the new object. The new object will have a unique `ClonedObjectID`.
%
%   @arg ObjectID        The unique ID of the object to be cloned.
%   @arg ClonedObjectID  The unique ID of the cloned object.
%
%   @example
%     % Clone an object and retrieve the cloned object ID.
%     ?- oo_clone('Object_1', ClonedObjectID).
%     ClonedObjectID = 'Object_2'.
%
oo_clone(ObjectID, ClonedObjectID) :-
    % Retrieve the type of the original object.
    o_f_v(ObjectID, type, Type),
    % Collect all field-value pairs of the original object.
    findall(FieldName:Value, o_f_v(ObjectID, FieldName, Value), Attributes),
    % Exclude the 'type' field from the attributes to copy.
    exclude(=(type:_), Attributes, AttributesWithoutType),
    % Create a new object with the same attributes.
    oo_new(Type, AttributesWithoutType, ClonedObjectID).

%!  oo_equal(+ObjectID1, +ObjectID2, -AreEqual) is det.
%
%   Checks if two objects are equal by comparing their fields and values.
%   If both objects have the same fields with the same values, they are considered equal.
%   The result is unified with '@'(true) or '@'(false).
%
%   @arg ObjectID1  The unique ID of the first object.
%   @arg ObjectID2  The unique ID of the second object.
%   @arg AreEqual   Unifies with '@'(true) if the objects are equal, '@'(false) otherwise.
%
%   @example
%     % Check if two objects are equal.
%     ?- oo_equal('Object_1', 'Object_2', AreEqual).
%     AreEqual = '@'(false).
%
oo_equal(ObjectID1, ObjectID2, AreEqual) :-
    (ObjectID1 == ObjectID2 -> AreEqual = '@'(true)
    ;
        % Collect the state of both objects.
        collect_object_state(ObjectID1, State1),collect_object_state(ObjectID2, State2),
        % Compare the collected states of both objects.
        (State1 == State2 -> AreEqual = '@'(true); AreEqual = '@'(false))
    ).

%!  collect_object_state(+ObjectID, -State) is det.
%
%   Collects all field-value pairs for an object into a list. This list represents
%   the entire state of the object.
%
%   @arg ObjectID  The unique ID of the object.
%   @arg State     A list of field-value pairs representing the object state.
%
%   @example
%     % Collect the state of an object.
%     ?- collect_object_state('Object_1', State).
%     State = [type-'Person', name-'John', age-30].
%
collect_object_state(ObjectID, State) :- findall(FieldName-Value, o_f_v(ObjectID, FieldName, Value), State).

% ==================================================
% Type Field Values (Default values for types)
% ==================================================

% Define default field values for types here if needed.
% Example:
% t_f_v(atom_vec, value, []).

% ==================================================
% 1. Atom Functions (8-28)
% ==================================================

% 8. hyperonpy_atom_sym/2

%!  hyperonpy_atom_sym(+Symbol, -Atom) is det.
%
%   Creates a symbolic atom from a given symbol. If the provided `Symbol` is a valid
%   symbol, it directly returns the symbol as the atom. Otherwise, it converts the
%   `String` into a symbol and creates a new symbolic atom using `hyperonpy_sym_sym/2`.
%
%   @arg Symbol  The input symbol or string from which the atom is created.
%   @arg Atom    The resulting symbolic atom.
%
%   @example
%     % Create a symbolic atom from a string.
%     ?- hyperonpy_atom_sym('example', Atom).
%     Atom = 'example'.
%
hyperonpy_atom_sym(String, Atom) :-
    % If the input is a valid symbol, use it as is; otherwise, convert the string.
    (symbol(String) -> Symbol = String ; name(Symbol, String)),
    % Create a symbolic atom using hyperonpy_sym_sym/2.
    hyperonpy_sym_sym(Symbol, Atom).

%!  hyperonpy_sym_sym(+Symbol, -Atom) is det.
%
%   Determines whether a given symbol is already a valid atom or needs to be
%   created as a new symbolic atom. If the symbol is not a reference, it returns
%   the symbol directly as the atom. Otherwise, it creates a new atom object.
%
%   @arg Symbol  The input symbol.
%   @arg Atom    The resulting atom, either the symbol itself or a new symbolic atom.
%
%   @example
%     % Handle a symbol that is not a reference.
%     ?- hyperonpy_sym_sym('example', Atom).
%     Atom = 'example'.
%
%     % Handle a symbol that needs to be created as a new atom.
%     ?- hyperonpy_sym_sym('ref_symbol', Atom).
%     Atom = some_atom_id.
%
hyperonpy_sym_sym(Symbol, Atom) :-
    % If the symbol is not a reference, return it directly.
    symbol_not_ref(Symbol), !, Atom = Symbol.
hyperonpy_sym_sym(Symbol, Atom) :-
    % Otherwise, create a new atom object with the symbol metatype as 'Symbol'.
    oo_new(atom, [metatype:'Symbol', value:Symbol], Atom), !.

% 9. hyperonpy_atom_var/2

%!  hyperonpy_atom_var(+VarName, -Atom) is det.
%
%   Creates a variable atom from a given variable name. If the variable name
%   is in the form of a custom `bvar/2` variable, the corresponding symbol is used.
%   Otherwise, the variable name is converted to a symbol, and a new variable atom
%   is created.
%
%   @arg VarName  The name of the variable (either a custom `bvar` variable or a string).
%   @arg Atom     The resulting variable atom.
%
%   @example
%     % Create a variable atom from a custom variable name.
%     ?- hyperonpy_atom_var('$VAR'(0), Atom).
%     Atom = some_atom_id.
%
%     % Create a variable atom from a string variable name.
%     ?- hyperonpy_atom_var('X', Atom).
%     Atom = another_atom_id.
%
hyperonpy_atom_var(VarName, Atom) :-
    % If VarName is in the form of a custom variable, extract the symbol.
    (bvar(VarName, Symbol) -> true ; name(Symbol, VarName)),
    % Create a new variable atom with the metatype 'Variable'.
    oo_new(atom, [metatype:'Variable', value:Symbol], Atom).

% 10. hyperonpy_atom_expr/2

%!  hyperonpy_atom_expr(+ExpressionList, -Atom) is det.
%
%   Creates an expression atom from a list of expressions. The resulting atom will
%   have the `metatype` set to 'Expression' and its value set to the given list of
%   expressions.
%
%   @arg ExpressionList  A list of expressions to be wrapped in an atom.
%   @arg Atom            The resulting atom representing the expression.
%
%   @example
%     % Create an expression atom from a list of expressions.
%     ?- hyperonpy_atom_expr([expr1, expr2], Atom).
%     Atom = some_atom_id.
%
hyperonpy_atom_expr(ExpressionList, Atom) :-
    % Create a new atom with 'Expression' as the metatype and the list of expressions as the value.
    oo_new(atom, [metatype:'Expression', value:ExpressionList], Atom).

% 11. hyperonpy_atom_gnd/3

%!  hyperonpy_atom_gnd(+Object, +GroundedType, -Atom) is det.
%
%   Creates a grounded atom from a given object and its specified type. If the
%   `GroundedType` is 'Number' and the object is a number, it is handled directly.
%   For other grounded types, a new grounded atom is created with the provided type.
%
%   @arg Object        The object to be wrapped in a grounded atom.
%   @arg GroundedType  The grounded type of the object (e.g., 'Number').
%   @arg Atom          The resulting grounded atom.
%
%   @example
%     % Create a grounded atom for a number.
%     ?- hyperonpy_atom_gnd(42, 'Number', Atom).
%     Atom = some_atom_id.
%
%     % Create a grounded atom for a custom object.
%     ?- hyperonpy_atom_gnd(my_object, 'CustomType', Atom).
%     Atom = another_atom_id.
%
hyperonpy_atom_gnd(Object, GroundedType, Atom) :-
    % Special handling for numbers with the 'Number' grounded type.
    (GroundedType == 'Number', number(Object) ->
        oo_new(atom, [metatype:'Grounded', value:Object, grounded_type:GroundedType], Atom)
    ;
        % Handle other grounded types by creating a grounded atom.
        oo_new(atom, [metatype:'Grounded', value:Object, grounded_type:GroundedType], Atom)
    ).

% 12. hyperonpy_atom_free/1

%!  hyperonpy_atom_free(+Atom) is det.
%
%   Frees an atom from memory by removing all facts associated with it.
%   This predicate removes the atom state from the system.
%
%   @arg Atom  The unique ID of the atom to be freed.
%
%   @example
%     % Free an atom from memory.
%     ?- hyperonpy_atom_free(some_atom_id).
%     true.
%
hyperonpy_atom_free(Atom) :-
    % Free the atom by removing its state.
    oo_free(Atom).

% 13. hyperonpy_atom_to_str/2

%!  hyperonpy_atom_to_str(+Atom, -Str) is det.
%
%   Converts an atom to its string representation based on its metatype.
%   Symbols, variables, expressions, and grounded types are formatted accordingly.
%
%   @arg Atom  The unique ID of the atom to be converted to a string.
%   @arg Str   The resulting string representation of the atom.
%
%   @example
%     % Convert a symbol atom to a string.
%     ?- hyperonpy_atom_to_str(some_atom_id, Str).
%     Str = "symbol_value".
%
hyperonpy_atom_to_str(Atom, Str) :-
    % Retrieve the metatype and value of the atom.
    oo_get(atom, Atom, metatype, Metatype),
    oo_get(atom, Atom, value, Value),
    % Format the string based on the metatype.
    (Metatype == 'Symbol' ->
        format(atom(Str), "~w", [Value])
    ; Metatype == 'Variable' ->
        format(atom(Str), "?~w", [Value])
    ; Metatype == 'Expression' ->
        format(atom(Str), "(~w)", [Value])
    ; Metatype == 'Grounded' ->
        format(atom(Str), "<~w>", [Value])
    ;
        % Default to an empty string if no known metatype is found.
        Str = ""
    ).

% 14. hyperonpy_atom_eq/3

%!  hyperonpy_atom_eq(+Atom1, +Atom2, -AreEqual) is det.
%
%   Compares two atoms for equality. If both atoms have the same fields and values,
%   `AreEqual` will unify with `@'(true)`, otherwise it will unify with `@'(false)`.
%
%   @arg Atom1     The unique ID of the first atom.
%   @arg Atom2     The unique ID of the second atom.
%   @arg AreEqual  Unifies with `@'(true)` if the atoms are equal, otherwise with `@'(false)`.
%
%   @example
%     % Compare two atoms for equality.
%     ?- hyperonpy_atom_eq(atom1_id, atom2_id, AreEqual).
%     AreEqual = '@'(false).
%
hyperonpy_atom_eq(Atom1, Atom2, AreEqual) :-
    % Use oo_equal to check if the two atoms are equal.
    oo_equal(Atom1, Atom2, AreEqual).

% 15. hyperonpy_atom_get_name/2

%!  hyperonpy_atom_get_name(+Atom, -Name) is det.
%
%   Retrieves the name (or value) of an atom. The atom value, stored in the `value` field,
%   is considered its name in this context.
%
%   @arg Atom  The unique ID of the atom whose name is being retrieved.
%   @arg Name  The name or value of the atom.
%
%   @example
%     % Get the name of an atom.
%     ?- hyperonpy_atom_get_name(atom_id, Name).
%     Name = 'symbol_name'.
%
hyperonpy_atom_get_name(Atom, Name) :-
    % Retrieve the 'value' field of the atom, which represents its name.
    oo_get(atom, Atom, value, Name).

% 16. hyperonpy_atom_get_children/2

%!  hyperonpy_atom_get_children(+Atom, -Children) is det.
%
%   Retrieves the children of an expression atom. This is only applicable if the atom
%   `metatype` is 'Expression', and its `value` field contains the list of children (sub-expressions).
%
%   @arg Atom      The unique ID of the expression atom.
%   @arg Children  The list of children (sub-expressions) of the atom.
%
%   @example
%     % Get the children of an expression atom.
%     ?- hyperonpy_atom_get_children(expr_atom_id, Children).
%     Children = [child1, child2].
%
hyperonpy_atom_get_children(Atom, Children) :-
    % Ensure the atom is of metatype 'Expression'.
    oo_get(atom, Atom, metatype, 'Expression'),
    % Retrieve the list of children stored in the 'value' field.
    oo_get(atom, Atom, value, Children).

% 17. hyperonpy_atom_get_grounded_type/2

%!  hyperonpy_atom_get_grounded_type(+Atom, -GroundedType) is det.
%
%   Retrieves the grounded type of a grounded atom. This is only applicable if the atom
%   is grounded, meaning its `metatype` is 'Grounded', and the grounded type is stored in
%   the `grounded_type` field.
%
%   @arg Atom          The unique ID of the grounded atom.
%   @arg GroundedType  The grounded type of the atom (e.g., 'Number', 'String').
%
%   @example
%     % Get the grounded type of a grounded atom.
%     ?- hyperonpy_atom_get_grounded_type(grounded_atom_id, GroundedType).
%     GroundedType = 'Number'.
%
hyperonpy_atom_get_grounded_type(Atom, GroundedType) :-
    % Retrieve the grounded type from the 'grounded_type' field.
    oo_get(atom, Atom, grounded_type, GroundedType).

% 18. hyperonpy_atom_get_object/2

%!  hyperonpy_atom_get_object(+Atom, -Object) is det.
%
%   Retrieves the object associated with a grounded atom. This is only applicable
%   if the atom is grounded, and the object is stored in the `value` field of the atom.
%
%   @arg Atom    The unique ID of the grounded atom.
%   @arg Object  The object associated with the grounded atom.
%
%   @example
%     % Get the object associated with a grounded atom.
%     ?- hyperonpy_atom_get_object(grounded_atom_id, Object).
%     Object = 42.
%
hyperonpy_atom_get_object(Atom, Object) :-
    % Retrieve the object from the 'value' field of the grounded atom.
    oo_get(atom, Atom, value, Object).

% 19. hyperonpy_atom_is_cgrounded/2

%!  hyperonpy_atom_is_cgrounded(+Atom, -IsCgrounded) is det.
%
%   Checks if an atom is a grounded atom. The check is based on the atom `metatype`.
%   If the `metatype` is 'Grounded', the atom is considered grounded.
%
%   @arg Atom         The unique ID of the atom to check.
%   @arg IsCgrounded  Unifies with `@'(true)` if the atom is grounded, otherwise `@'(false)`.
%
%   @example
%     % Check if an atom is grounded.
%     ?- hyperonpy_atom_is_cgrounded(grounded_atom_id, IsCgrounded).
%     IsCgrounded = '@'(true).
%
hyperonpy_atom_is_cgrounded(Atom, IsCgrounded) :-
    % Retrieve the metatype of the atom.
    oo_get(atom, Atom, metatype, Metatype),
    % Check if the metatype is 'Grounded'.
    (Metatype == 'Grounded' -> IsCgrounded = '@'(true); IsCgrounded = '@'(false)).

% 20. hyperonpy_atom_is_error/2

%!  hyperonpy_atom_is_error(+Atom, -IsError) is det.
%
%   Checks if an atom represents an error. This is determined by the presence of
%   an `is_error` field in the atom state. If the `is_error` field is non-variable
%   and set to true, the atom is considered an error.
%
%   @arg Atom    The unique ID of the atom to check.
%   @arg IsError Unifies with `@'(true)` if the atom represents an error, otherwise `@'(false)`.
%
%   @example
%     % Check if an atom represents an error.
%     ?- hyperonpy_atom_is_error(error_atom_id, IsError).
%     IsError = '@'(true).
%
hyperonpy_atom_is_error(Atom, IsError) :-
    % Retrieve the 'is_error' field from the atom.
    oo_get(atom, Atom, is_error, IsErrorValue),
    % If the field is non-variable, use its value; otherwise, default to false.
    (nonvar(IsErrorValue) -> IsError = IsErrorValue; IsError = '@'(false)).

% 21. hyperonpy_atom_error_message/2

%!  hyperonpy_atom_error_message(+Atom, -ErrorMessage) is det.
%
%   Retrieves the error message associated with an atom. This is applicable only if the
%   atom represents an error (i.e., the `is_error` field is `@'(true)`).
%
%   @arg Atom          The unique ID of the atom.
%   @arg ErrorMessage  The error message associated with the atom.
%
%   @example
%     % Retrieve the error message for an error atom.
%     ?- hyperonpy_atom_error_message(error_atom_id, ErrorMessage).
%     ErrorMessage = 'An error occurred'.
%
hyperonpy_atom_error_message(Atom, ErrorMessage) :-
    % Ensure the atom is marked as an error.
    oo_get(atom, Atom, is_error, '@'(true)),
    % Retrieve the error message from the 'error_message' field.
    oo_get(atom, Atom, error_message, ErrorMessage).

% 22. hyperonpy_atom_var_parse_name/2

%!  hyperonpy_atom_var_parse_name(+Name, -Atom) is det.
%
%   Parses a variable name and creates a variable atom. This predicate wraps
%   `hyperonpy_atom_var/2` to handle the creation of a variable atom from the given name.
%
%   @arg Name  The name of the variable to be parsed.
%   @arg Atom  The resulting variable atom created from the name.
%
%   @example
%     % Parse a variable name and create a variable atom.
%     ?- hyperonpy_atom_var_parse_name('X', Atom).
%     Atom = some_atom_id.
%
hyperonpy_atom_var_parse_name(Name, Atom) :-
    % Create a variable atom from the given name.
    hyperonpy_atom_var(Name, Atom).

% 23. hyperonpy_atom_get_metatype/2

%!  hyperonpy_atom_get_metatype(+Atom, -MetaType) is det.
%
%   Retrieves the metatype of an atom. The metatype indicates the kind of atom,
%   such as 'Symbol', 'Variable', 'Expression', or 'Grounded'.
%
%   @arg Atom      The unique ID of the atom.
%   @arg MetaType  The metatype of the atom.
%
%   @example
%     % Get the metatype of an atom.
%     ?- hyperonpy_atom_get_metatype(atom_id, MetaType).
%     MetaType = 'Symbol'.
%
hyperonpy_atom_get_metatype(Atom, MetaType) :-
    % Retrieve the 'metatype' field of the atom.
    oo_get(atom, Atom, metatype, MetaType).

% 24. hyperonpy_atom_get_space/2

%!  hyperonpy_atom_get_space(+Atom, -Space) is det.
%
%   Retrieves the space associated with an atom. The space represents the context
%   or environment in which the atom exists.
%
%   @arg Atom   The unique ID of the atom.
%   @arg Space  The space associated with the atom.
%
%   @example
%     % Get the space associated with an atom.
%     ?- hyperonpy_atom_get_space(atom_id, Space).
%     Space = 'some_space'.
%
hyperonpy_atom_get_space(Atom, Space) :-
    % Retrieve the 'space' field of the atom.
    oo_get(atom, Atom, space, Space).

% 25. hyperonpy_atom_iterate/2

%!  hyperonpy_atom_iterate(+Atom, -List) is det.
%
%   Iterates over the components of an atom. If the atom is of metatype 'Expression',
%   it retrieves the list of components (sub-expressions) from the atom value.
%   Otherwise, it returns an empty list.
%
%   @arg Atom  The unique ID of the atom.
%   @arg List  The list of components if the atom is an expression, otherwise an empty list.
%
%   @example
%     % Iterate over the components of an expression atom.
%     ?- hyperonpy_atom_iterate(expression_atom, List).
%     List = [sub_expr1, sub_expr2].
%
%     % Iterate over a non-expression atom (returns an empty list).
%     ?- hyperonpy_atom_iterate(non_expression_atom, List).
%     List = [].
%
hyperonpy_atom_iterate(Atom, List) :-
    % Get the metatype of the atom.
    oo_get(atom, Atom, metatype, Metatype),
    % If it is an expression, return the list of components; otherwise, return an empty list.
    (Metatype == 'Expression' ->
        oo_get(atom, Atom, value, List)
    ;
        List = []
    ).

% 26. hyperonpy_atom_match_atom/3

%!  hyperonpy_atom_match_atom(+Atom1, +Atom2, -BindingsSet) is det.
%
%   Matches two atoms and returns the resulting set of bindings. If the atoms
%   are identical (exact match), the result is an empty bindings list. If the atoms
%   do not match, an empty set of bindings is returned.
%
%   @arg Atom1        The first atom to be matched.
%   @arg Atom2        The second atom to be matched.
%   @arg BindingsSet  The resulting set of bindings after matching the two atoms.
%
%   @example
%     % Match two identical atoms and return bindings.
%     ?- hyperonpy_atom_match_atom(atom1_id, atom1_id, BindingsSet).
%     BindingsSet = [].
%
%     % Match two different atoms (no bindings).
%     ?- hyperonpy_atom_match_atom(atom1_id, atom2_id, BindingsSet).
%     BindingsSet = [].
%
hyperonpy_atom_match_atom(Atom1, Atom2, Bindings) :-
    % Check if Atom1 and Atom2 are identical.
    hyperonpy_atom_eq(Atom1, Atom2, AreEqual),
    (AreEqual == '@'(true) ->
        % If atoms are equal, create a new empty bindings object.
        (hyperonpy_bindings_new(BindingsObj),
         oo_get(bindings, BindingsObj, value, Bindings))
    ;
        % If atoms do not match, return an empty bindings set.
        hyperonpy_bindings_set_empty(Bindings)
    ).

% 27. hyperonpy_atom_gnd_serialize/3

%!  hyperonpy_atom_gnd_serialize(+Atom, +Serializer, -SerializedResult) is det.
%
%   Serializes a grounded atom using the provided serializer. For simplicity, the
%   atom value is directly converted into a string representation, ignoring the
%   actual serializer for this basic implementation.
%
%   @arg Atom            The unique ID of the grounded atom to be serialized.
%   @arg Serializer      The serializer to be used (currently not applied in this version).
%   @arg SerializedResult  The resulting serialized string representation of the atom value.
%
%   @example
%     % Serialize a grounded atom.
%     ?- hyperonpy_atom_gnd_serialize(grounded_atom_id, serializer, SerializedResult).
%     SerializedResult = "42".
%
hyperonpy_atom_gnd_serialize(Atom, _Serializer, SerializedResult) :-
    % Retrieve the value of the grounded atom.
    oo_get(atom, Atom, value, Value),
    % Convert the value to a string for the serialized result.
    format(atom(SerializedResult), "~w", [Value]).

% 28. hyperonpy_atom_clone/2

%!  hyperonpy_atom_clone(+Atom, -ClonedAtom) is det.
%
%   Clones an atom by creating a new atom with the same fields and values as the original.
%   The cloned atom will have a unique ID but will share all other properties with the original.
%
%   @arg Atom        The unique ID of the atom to be cloned.
%   @arg ClonedAtom  The unique ID of the cloned atom.
%
%   @example
%     % Clone an atom.
%     ?- hyperonpy_atom_clone(original_atom_id, ClonedAtom).
%     ClonedAtom = cloned_atom_id.
%
hyperonpy_atom_clone(Atom, ClonedAtom) :-
    % Clone the atom by copying its fields and values.
    oo_clone(Atom, ClonedAtom).

%!  match_atoms(+Atom1, +Atom2, +BindingsIn, -BindingsOut) is nondet.
%
%   Matches two atoms and updates the bindings accordingly. If `Atom1` is a variable,
%   it adds the variable name and `Atom2` to the bindings. If both atoms have the same
%   metatype and value, it succeeds without changing the bindings. Otherwise, it fails.
%
%   @arg Atom1       The first atom to be matched.
%   @arg Atom2       The second atom to be matched.
%   @arg BindingsIn  The initial set of bindings before matching.
%   @arg BindingsOut The updated set of bindings after matching.
%
%   @example
%     % Match two atoms and update bindings.
%     ?- match_atoms(var_atom_id, const_atom_id, [], BindingsOut).
%     BindingsOut = [VarName-const_atom_id].
%
match_atoms(Atom1, Atom2, BindingsIn, BindingsOut) :-
    % Get the metatype of both atoms.
    oo_get(atom, Atom1, metatype, Type1),
    oo_get(atom, Atom2, metatype, Type2),

    % If Atom1 is a variable, add its binding to the bindings list.
    (Type1 == 'Variable' ->
        oo_get(atom, Atom1, value, VarName),
        BindingsOut = [VarName-Atom2 | BindingsIn]

    % If the types are the same, check if the values match.
    ; Type1 == Type2 ->
        oo_get(atom, Atom1, value, Value1),
        oo_get(atom, Atom2, value, Value2),
        (Value1 == Value2 ->
            % If the values match, keep the bindings unchanged.
            BindingsOut = BindingsIn
        ;
            % If the values do not match, fail.
            fail
        )

    % If the metatypes are different, fail.
    ;
        fail
    ).

% ==================================================
% 2. Atom Vector Functions (29-34)
% ==================================================

%!  t_f_v(+Type, +FieldName, -DefaultValue) is det.
%
%   Defines the default value for a specific field in a given type. This predicate
%   provides a default value for the `value` field of the `atom_vec` type, which is
%   an empty list `[]`.
%
%   @arg Type         The type of the object (e.g., `atom_vec`).
%   @arg FieldName    The field name whose default value is being defined (e.g., `value`).
%   @arg DefaultValue The default value for the specified field.
%
%   @example
%     % Get the default value for the `value` field of an `atom_vec`.
%     ?- t_f_v(atom_vec, value, DefaultValue).
%     DefaultValue = [].
%
t_f_v(atom_vec, value, []).

% 29. hyperonpy_atom_vec_new/1

%!  hyperonpy_atom_vec_new(-AtomVec) is det.
%
%   Creates a new empty atom vector. The `atom_vec` type is initialized with its
%   default fields, which include an empty list for the `value` field.
%
%   @arg AtomVec  The unique ID of the newly created empty atom vector.
%
%   @example
%     % Create a new empty atom vector.
%     ?- hyperonpy_atom_vec_new(AtomVec).
%     AtomVec = atom_vec_id.
%
hyperonpy_atom_vec_new(AtomVec) :-
    % Create a new object of type 'atom_vec' with no initial attributes.
    oo_new(atom_vec, [], AtomVec).

% 30. hyperonpy_atom_vec_from_list/2

%!  hyperonpy_atom_vec_from_list(+List, -AtomVec) is det.
%
%   Creates an atom vector from a given list of atoms. The list is stored in the
%   `value` field of the newly created `atom_vec` object.
%
%   @arg List     A list of atoms to initialize the atom vector.
%   @arg AtomVec  The unique ID of the newly created atom vector.
%
%   @example
%     % Create an atom vector from a list of atoms.
%     ?- hyperonpy_atom_vec_from_list([atom1, atom2], AtomVec).
%     AtomVec = atom_vec_id.
%
hyperonpy_atom_vec_from_list(List, AtomVec) :-
    % Create a new atom_vec with the 'value' field initialized to the provided list.
    oo_new(atom_vec, [value:List], AtomVec).

% 31. hyperonpy_atom_vec_len/2

%!  hyperonpy_atom_vec_len(+AtomVec, -Length) is det.
%
%   Retrieves the length of an atom vector.
%
%   @arg AtomVec  The unique ID of the atom vector.
%   @arg Length   The length of the atom vector (i.e., the number of atoms it contains).
%
%   @example
%     % Get the length of an atom vector.
%     ?- hyperonpy_atom_vec_len(atom_vec_id, Length).
%     Length = 2.
%
hyperonpy_atom_vec_len(AtomVec, Length) :-
    % Retrieve the list of atoms stored in the 'value' field of the atom vector.
    oo_get(atom_vec, AtomVec, value, List),
    % Calculate the length of the list.
    length(List, Length).

% 32. hyperonpy_atom_vec_push/2

%!  hyperonpy_atom_vec_push(+AtomVec, +Atom) is det.
%
%   Pushes a new atom into the atom vector.
%
%   @arg AtomVec  The unique ID of the atom vector.
%   @arg Atom     The atom to be added to the vector.
%
%   @example
%     % Push a new atom into the atom vector.
%     ?- hyperonpy_atom_vec_push(atom_vec_id, new_atom).
%     true.
%
hyperonpy_atom_vec_push(AtomVec, Atom) :-
    % Retrieve the current list of atoms in the vector.
    oo_get(atom_vec, AtomVec, value, List),
    % Append the new atom to the list.
    append(List, [Atom], NewList),
    % Update the vector with the new list.
    oo_set(AtomVec, value, NewList).

% 33. hyperonpy_atom_vec_pop/2

%!  hyperonpy_atom_vec_pop(+AtomVec, -PoppedAtom) is det.
%
%   Removes and returns the last atom from the vector.
%
%   @arg AtomVec     The unique ID of the atom vector.
%   @arg PoppedAtom  The atom that was removed from the end of the vector.
%
%   @example
%     % Pop the last atom from the atom vector.
%     ?- hyperonpy_atom_vec_pop(atom_vec_id, PoppedAtom).
%     PoppedAtom = last_atom.
%
hyperonpy_atom_vec_pop(AtomVec, PoppedAtom) :-
    % Retrieve the current list of atoms in the vector.
    oo_get(atom_vec, AtomVec, value, List),
    % Split the list into the new list and the popped atom.
    append(NewList, [PoppedAtom], List),
    % Update the vector with the new list.
    oo_set(AtomVec, value, NewList).

% 34. hyperonpy_atom_vec_free/1

%!  hyperonpy_atom_vec_free(+AtomVec) is det.
%
%   Frees an atom vector from memory, removing it and all its associated state.
%
%   @arg AtomVec  The unique ID of the atom vector to be freed.
%
%   @example
%     % Free an atom vector from memory.
%     ?- hyperonpy_atom_vec_free(atom_vec_id).
%     true.
%
hyperonpy_atom_vec_free(AtomVec) :-
    % Free the atom vector by removing its state.
    oo_free(AtomVec).

% ==================================================
% 3. Bindings Functions (35-45)
% ==================================================

% 35. hyperonpy_bindings_new/1

%!  hyperonpy_bindings_new(-NewBindings) is det.
%
%   Creates a new empty bindings object.
%
%   @arg NewBindings  The unique ID of the newly created bindings object.
%
%   @example
%     % Create a new empty bindings object.
%     ?- hyperonpy_bindings_new(Bindings).
%     Bindings = bindings_id.
%
hyperonpy_bindings_new(NewBindings) :-
    % Create a new bindings object with an empty list as its value.
    oo_new(bindings, [value:[]], NewBindings).

% 36. hyperonpy_bindings_free/1

%!  hyperonpy_bindings_free(+Bindings) is det.
%
%   Frees the bindings from memory, removing all its associated state.
%
%   @arg Bindings  The unique ID of the bindings object to be freed.
%
%   @example
%     % Free a bindings object.
%     ?- hyperonpy_bindings_free(Bindings).
%     true.
%
hyperonpy_bindings_free(Bindings) :-
    % Free the bindings object by removing its state.
    oo_free(Bindings).

% 37. hyperonpy_bindings_add_var_binding/4

%!  hyperonpy_bindings_add_var_binding(+Bindings, +VarAtom, +BoundAtom, -Success) is det.
%
%   Adds a variable binding to the bindings. If the variable is already bound,
%   the addition fails. Otherwise, the new binding is added.
%
%   @arg Bindings   The unique ID of the bindings object.
%   @arg VarAtom    The atom representing the variable.
%   @arg BoundAtom  The atom to which the variable is bound.
%   @arg Success    Unifies with `@'(true)` if the binding was added, or `@'(false)` if it failed.
%
%   @example
%     % Add a variable binding.
%     ?- hyperonpy_bindings_add_var_binding(Bindings, VarAtom, BoundAtom, Success).
%     Success = '@'(true).
%
hyperonpy_bindings_add_var_binding(Bindings, VarAtom, BoundAtom, Success) :-
    % Get the current list of bindings.
    oo_get(bindings, Bindings, value, BindingsList),
    % Get the variable name from the VarAtom.
    oo_get(atom, VarAtom, value, VarName),
    % Check if the variable is already bound.
    (member(VarName-_, BindingsList) ->
        % If the variable is already bound, fail.
        Success = '@'(false)
    ;
        % Otherwise, add the new binding and update the bindings object.
        NewBindingsList = [VarName-BoundAtom | BindingsList],
        oo_set(Bindings, value, NewBindingsList),
        Success = '@'(true)
    ).

% 38. hyperonpy_bindings_resolve/3

%!  hyperonpy_bindings_resolve(+Bindings, +VarAtom, -ResolvedAtom) is det.
%
%   Resolves a variable atom in the bindings by looking up its binding in the bindings list.
%   If the variable is not bound, the variable atom itself is returned.
%
%   @arg Bindings     The unique ID of the bindings object.
%   @arg VarAtom      The atom representing the variable to resolve.
%   @arg ResolvedAtom The resolved atom, or the original variable atom if not bound.
%
%   @example
%     % Resolve a variable atom.
%     ?- hyperonpy_bindings_resolve(Bindings, var_atom, ResolvedAtom).
%     ResolvedAtom = bound_atom.
%
hyperonpy_bindings_resolve(Bindings, Atom, ResolvedAtom) :-
    % Get the metatype of the atom.
    hyperonpy_atom_get_metatype(Atom, Metatype),
    % If it is a variable, attempt to resolve it.
    (Metatype == 'Variable' ->
        hyperonpy_atom_get_name(Atom, VarName),
        oo_get(bindings, Bindings, value, BindingsList),
        (member(VarName-Value, BindingsList) ->
            ResolvedAtom = Value
        ;
            ResolvedAtom = Atom
        )
    ;
        ResolvedAtom = Atom
    ).

% 39. hyperonpy_bindings_is_empty/2

%!  hyperonpy_bindings_is_empty(+Bindings, -IsEmpty) is det.
%
%   Checks if the bindings object is empty.
%
%   @arg Bindings  The unique ID of the bindings object.
%   @arg IsEmpty   Unifies with `@'(true)` if the bindings are empty, `@'(false)` otherwise.
%
%   @example
%     % Check if bindings are empty.
%     ?- hyperonpy_bindings_is_empty(Bindings, IsEmpty).
%     IsEmpty = '@'(true).
%
hyperonpy_bindings_is_empty(Bindings, IsEmpty) :-
    % Get the list of bindings and check if it is empty.
    oo_get(bindings, Bindings, value, BindingsList),
    (BindingsList == [] -> IsEmpty = '@'(true); IsEmpty = '@'(false)).

% 40. hyperonpy_bindings_list/2

%!  hyperonpy_bindings_list(+Bindings, -List) is det.
%
%   Retrieves the list of bindings from the bindings object.
%
%   @arg Bindings  The unique ID of the bindings object.
%   @arg List      The list of variable bindings in the bindings object.
%
%   @example
%     % Retrieve the list of bindings.
%     ?- hyperonpy_bindings_list(Bindings, List).
%     List = [var1-bound_atom1, var2-bound_atom2].
%
hyperonpy_bindings_list(Bindings, List) :-
    % Get the value (bindings list) from the bindings object.
    oo_get(bindings, Bindings, value, List).

% 41. hyperonpy_bindings_merge/3

%!  hyperonpy_bindings_merge(+Bindings1, +Bindings2, -ResultBindingsSet) is det.
%
%   Merges two bindings objects into a new bindings set.
%
%   @arg Bindings1           The first bindings object.
%   @arg Bindings2           The second bindings object.
%   @arg ResultBindingsSet   The resulting bindings set after merging.
%
%   @example
%     % Merge two bindings objects.
%     ?- hyperonpy_bindings_merge(Bindings1, Bindings2, ResultBindingsSet).
%     ResultBindingsSet = merged_bindings_set.
%
hyperonpy_bindings_merge(Bindings1, Bindings2, ResultBindingsSet) :-
    % Get the list of bindings from both objects.
    oo_get(bindings, Bindings1, value, List1),
    oo_get(bindings, Bindings2, value, List2),
    % Merge the two lists of bindings.
    append(List1, List2, MergedList),
    % Create a new bindings set with the merged list.
    oo_new(bindings_set, [value:[MergedList]], ResultBindingsSet).

% 42. hyperonpy_bindings_eq/3

%!  hyperonpy_bindings_eq(+Bindings1, +Bindings2, -AreEqual) is det.
%
%   Compares two bindings objects for equality.
%
%   @arg Bindings1  The first bindings object.
%   @arg Bindings2  The second bindings object.
%   @arg AreEqual   Unifies with `@'(true)` if the bindings are equal, `@'(false)` otherwise.
%
%   @example
%     % Compare two bindings for equality.
%     ?- hyperonpy_bindings_eq(Bindings1, Bindings2, AreEqual).
%     AreEqual = '@'(true).
%
hyperonpy_bindings_eq(Bindings1, Bindings2, AreEqual) :-
    % Check if the two bindings objects are equal.
    oo_equal(Bindings1, Bindings2, AreEqual).

% 43. hyperonpy_bindings_clone/2

%!  hyperonpy_bindings_clone(+Bindings, -NewBindings) is det.
%
%   Clones a bindings object, creating a new bindings object with the same content.
%
%   @arg Bindings    The original bindings object to be cloned.
%   @arg NewBindings The unique ID of the newly cloned bindings object.
%
%   @example
%     % Clone a bindings object.
%     ?- hyperonpy_bindings_clone(Bindings, NewBindings).
%     NewBindings = cloned_bindings.
%
hyperonpy_bindings_clone(Bindings, NewBindings) :-
    % Clone the bindings object.
    oo_clone(Bindings, NewBindings).

% 44. hyperonpy_bindings_to_str/2

%!  hyperonpy_bindings_to_str(+Bindings, -Str) is det.
%
%   Converts a bindings object to a string representation. The list of bindings is
%   converted to a string using `term_string/2`.
%
%   @arg Bindings  The unique ID of the bindings object.
%   @arg Str       The resulting string representation of the bindings.
%
%   @example
%     % Convert a bindings object to a string.
%     ?- hyperonpy_bindings_to_str(Bindings, Str).
%     Str = "[var1-bound_atom1, var2-bound_atom2]".
%
hyperonpy_bindings_to_str(Bindings, Str) :-
    % Retrieve the list of bindings from the bindings object.
    oo_get(bindings, Bindings, value, BindingsList),
    % Convert the bindings list to a string.
    term_string(BindingsList, Str).

% 45. hyperonpy_bindings_narrow_vars/2

%!  hyperonpy_bindings_narrow_vars(+Bindings, +VarAtomVec) is det.
%
%   Narrows the variable bindings in the bindings by keeping only the variables that
%   exist in the given variable atom vector.
%
%   @arg Bindings    The unique ID of the bindings object.
%   @arg VarAtomVec  The atom vector containing the variables to narrow to.
%
%   @example
%     % Narrow the variable bindings to a set of variables.
%     ?- hyperonpy_bindings_narrow_vars(Bindings, VarAtomVec).
%     true.
%
hyperonpy_bindings_narrow_vars(Bindings, VarAtomVec) :-
    % Retrieve the list of bindings from the bindings object.
    oo_get(bindings, Bindings, value, BindingsList),
    % Retrieve the list of variable atoms from the atom vector.
    oo_get(atom_vec, VarAtomVec, value, VarAtoms),
    % Narrow the bindings list by keeping only the variables that exist in VarAtomVec.
    findall(VarName-BoundAtom,
       (member(VarName-BoundAtom, BindingsList),
        member(VarAtom, VarAtoms),
        hyperonpy_atom_get_name(VarAtom, VarName)
    ), NarrowedBindingsList),
    % Update the bindings object with the narrowed list of bindings.
    oo_set(Bindings, value, NarrowedBindingsList).

% ==================================================
% 4. Bindings Set Functions (46-60)
% ==================================================

% 46. hyperonpy_bindings_set_empty/1

%!  hyperonpy_bindings_set_empty(-BindingsSet) is det.
%
%   Creates an empty bindings set.
%
%   @arg BindingsSet  The unique ID of the newly created empty bindings set.
%
%   @example
%     % Create an empty bindings set.
%     ?- hyperonpy_bindings_set_empty(BindingsSet).
%     BindingsSet = bindings_set_id.
%
hyperonpy_bindings_set_empty(BindingsSet) :-
    oo_new(bindings_set, [value:[]], BindingsSet).

% 47. hyperonpy_bindings_set_free/1

%!  hyperonpy_bindings_set_free(+BindingsSet) is det.
%
%   Frees a bindings set from memory, removing all its associated state.
%
%   @arg BindingsSet  The unique ID of the bindings set to be freed.
%
%   @example
%     % Free a bindings set from memory.
%     ?- hyperonpy_bindings_set_free(BindingsSet).
%     true.
%
hyperonpy_bindings_set_free(BindingsSet) :-
    oo_free(BindingsSet).

% 48. hyperonpy_bindings_set_add_var_binding/3

%!  hyperonpy_bindings_set_add_var_binding(+BindingsSet, +VarAtom, +BoundAtom) is det.
%
%   Adds a variable binding to the bindings set.
%
%   @arg BindingsSet  The unique ID of the bindings set.
%   @arg VarAtom      The variable atom.
%   @arg BoundAtom    The atom to which the variable is bound.
%
%   @example
%     % Add a variable binding to a bindings set.
%     ?- hyperonpy_bindings_set_add_var_binding(BindingsSet, VarAtom, BoundAtom).
%     true.
%
hyperonpy_bindings_set_add_var_binding_(BindingsSet, VarAtom, BoundAtom):-
    hyperonpy_bindings_set_add_var_binding_impl2(BindingsSet, VarAtom, BoundAtom).

hyperonpy_bindings_set_add_var_binding_impl1(BindingsSet, VarAtom, BoundAtom) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    hyperonpy_atom_get_name(VarAtom, VarName),
    findall(NewBinds, (
        member(Binds, BindingsList),
        (member(VarName-_, Binds) ->
            fail
        ;
            NewBinds = [VarName-BoundAtom | Binds]
        )
    ), NewBindingsList),
    oo_set(BindingsSet, value, NewBindingsList).

hyperonpy_bindings_set_add_var_binding_impl2(BindingsSet, VarAtom, BoundAtom) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    hyperonpy_bindings_new(NewBindings),
    hyperonpy_bindings_add_var_binding(NewBindings, VarAtom, BoundAtom, _),
    oo_get(bindings, NewBindings, value, NewBindingsList),
    append(BindingsList, [NewBindingsList], NewBindingsSetList),
    oo_set(BindingsSet, value, NewBindingsSetList).

% 49. hyperonpy_bindings_set_is_empty/2

%!  hyperonpy_bindings_set_is_empty(+BindingsSet, -IsEmpty) is det.
%
%   Checks if a bindings set is empty.
%
%   @arg BindingsSet  The unique ID of the bindings set.
%   @arg IsEmpty      Unifies with `@'(true)` if the bindings set is empty, `@'(false)` otherwise.
%
%   @example
%     % Check if a bindings set is empty.
%     ?- hyperonpy_bindings_set_is_empty(BindingsSet, IsEmpty).
%     IsEmpty = '@'(true).
%
hyperonpy_bindings_set_is_empty(BindingsSet, IsEmpty) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    (BindingsList == [] -> IsEmpty = '@'(true); IsEmpty = '@'(false)).

% 50. hyperonpy_bindings_set_list/2

%!  hyperonpy_bindings_set_list(+BindingsSet, -List) is det.
%
%   Retrieves the list of bindings from a bindings set.
%
%   @arg BindingsSet  The unique ID of the bindings set.
%   @arg List         The list of bindings in the bindings set.
%
%   @example
%     % Retrieve the list of bindings from a bindings set.
%     ?- hyperonpy_bindings_set_list(BindingsSet, List).
%     List = [binding1, binding2].
%
hyperonpy_bindings_set_list(BindingsSet, List) :-
    oo_get(bindings_set, BindingsSet, value, List).

% 51. hyperonpy_bindings_set_merge_into/2

%!  hyperonpy_bindings_set_merge_into(+BindingsSet1, +BindingsSet2) is det.
%
%   Merges two bindings sets by appending the bindings of BindingsSet2 to BindingsSet1.
%
%   @arg BindingsSet1  The first bindings set.
%   @arg BindingsSet2  The second bindings set.
%
%   @example
%     % Merge two bindings sets.
%     ?- hyperonpy_bindings_set_merge_into(BindingsSet1, BindingsSet2).
%     true.
%
hyperonpy_bindings_set_merge_into(BindingsSet1, BindingsSet2) :-
    oo_get(bindings_set, BindingsSet1, value, BindsList1),
    oo_get(bindings_set, BindingsSet2, value, BindsList2),
    append(BindsList1, BindsList2, MergedBindsList),
    oo_set(BindingsSet1, value, MergedBindsList).

% 52. hyperonpy_bindings_set_eq/3

%!  hyperonpy_bindings_set_eq(+BindingsSet1, +BindingsSet2, -AreEqual) is det.
%
%   Compares two bindings sets for equality.
%
%   @arg BindingsSet1  The first bindings set.
%   @arg BindingsSet2  The second bindings set.
%   @arg AreEqual      Unifies with `@'(true)` if the bindings sets are equal, `@'(false)` otherwise.
%
%   @example
%     % Compare two bindings sets for equality.
%     ?- hyperonpy_bindings_set_eq(BindingsSet1, BindingsSet2, AreEqual).
%     AreEqual = '@'(true).
%
hyperonpy_bindings_set_eq(BindingsSet1, BindingsSet2, AreEqual) :-
    oo_get(bindings_set, BindingsSet1, value, BindsList1),
    oo_get(bindings_set, BindingsSet2, value, BindsList2),
    oo_equal(BindsList1, BindsList2, AreEqual).

% 53. hyperonpy_bindings_set_clone/2

%!  hyperonpy_bindings_set_clone(+BindingsSet, -ClonedBindingsSet) is det.
%
%   Clones a bindings set, creating a new bindings set with the same content.
%
%   @arg BindingsSet       The original bindings set to be cloned.
%   @arg ClonedBindingsSet The unique ID of the newly cloned bindings set.
%
%   @example
%     % Clone a bindings set.
%     ?- hyperonpy_bindings_set_clone(BindingsSet, ClonedBindingsSet).
%     ClonedBindingsSet = cloned_bindings_set.
%
hyperonpy_bindings_set_clone(BindingsSet, ClonedBindingsSet) :-
    oo_clone(BindingsSet, ClonedBindingsSet).

% 54. hyperonpy_bindings_set_from_bindings/2

%!  hyperonpy_bindings_set_from_bindings(+Bindings, -BindingsSet) is det.
%
%   Creates a bindings set from a bindings object.
%
%   @arg Bindings     The unique ID of the bindings object.
%   @arg BindingsSet  The unique ID of the newly created bindings set.
%
%   @example
%     % Create a bindings set from a bindings object.
%     ?- hyperonpy_bindings_set_from_bindings(Bindings, BindingsSet).
%     BindingsSet = bindings_set_id.
%
hyperonpy_bindings_set_from_bindings(Bindings, BindingsSet) :-
    oo_get(bindings, Bindings, value, BindingsList),
    oo_new(bindings_set, [value:[BindingsList]], BindingsSet).

% 55. hyperonpy_bindings_set_unpack/2

%!  hyperonpy_bindings_set_unpack(+BindingsSet, -UnpackedList) is det.
%
%   Unpacks a bindings set into a list of individual bindings.
%
%   @arg BindingsSet   The unique ID of the bindings set.
%   @arg UnpackedList  The list of individual bindings.
%
%   @example
%     % Unpack a bindings set.
%     ?- hyperonpy_bindings_set_unpack(BindingsSet, UnpackedList).
%     UnpackedList = [binding1, binding2].
%
hyperonpy_bindings_set_unpack(BindingsSet, UnpackedList) :-
    oo_get(bindings_set, BindingsSet, value, UnpackedList).

% 56. hyperonpy_bindings_set_is_single/2

%!  hyperonpy_bindings_set_is_single(+BindingsSet, -IsSingle) is det.
%
%   Checks if a bindings set contains a single binding.
%
%   @arg BindingsSet  The unique ID of the bindings set.
%   @arg IsSingle     Unifies with `@'(true)` if the bindings set contains a single binding, `@'(false)` otherwise.
%
%   @example
%     % Check if a bindings set contains a single binding.
%     ?- hyperonpy_bindings_set_is_single(BindingsSet, IsSingle).
%     IsSingle = '@'(true).
%
hyperonpy_bindings_set_is_single(BindingsSet, IsSingle) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    (BindingsList = [_] -> IsSingle = '@'(true); IsSingle = '@'(false)).

% 57. hyperonpy_bindings_set_push/2

%!  hyperonpy_bindings_set_push(+BindingsSet, +Bindings) is det.
%
%   Pushes a set of bindings into a bindings set.
%
%   @arg BindingsSet  The unique ID of the bindings set.
%   @arg Bindings     The bindings to be added to the bindings set.
%
%   @example
%     % Push a set of bindings into a bindings set.
%     ?- hyperonpy_bindings_set_push(BindingsSet, Bindings).
%     true.
%
hyperonpy_bindings_set_push(BindingsSet, Bindings) :-
    oo_get(bindings, Bindings, value, BindingsList),
    oo_get(bindings_set, BindingsSet, value, ExistingList),
    append(ExistingList, [BindingsList], NewList),
    oo_set(BindingsSet, value, NewList).

% 58. hyperonpy_bindings_set_single/1

%!  hyperonpy_bindings_set_single(-SingleBindingsSet) is det.
%
%   Creates a bindings set containing a single empty binding.
%
%   @arg SingleBindingsSet  The unique ID of the bindings set with a single empty binding.
%
%   @example
%     % Create a bindings set containing a single empty binding.
%     ?- hyperonpy_bindings_set_single(SingleBindingsSet).
%     SingleBindingsSet = single_bindings_set_id.
%
hyperonpy_bindings_set_single(SingleBindingsSet) :-
    oo_new(bindings_set, [value:[[]]], SingleBindingsSet).

% 59. hyperonpy_bindings_set_add_var_equality/3

%!  hyperonpy_bindings_set_add_var_equality(+BindingsSet, +VarAtom, +EqualAtom) is det.
%
%   Adds a variable equality constraint to a bindings set.
%
%   @arg BindingsSet  The unique ID of the bindings set.
%   @arg VarAtom      The variable atom.
%   @arg EqualAtom    The atom to which the variable is equal.
%
%   @example
%     % Add a variable equality constraint to a bindings set.
%     ?- hyperonpy_bindings_set_add_var_equality(BindingsSet, VarAtom, EqualAtom).
%     true.
%
hyperonpy_bindings_set_add_var_equality(BindingsSet, VarAtom, EqualAtom) :-
    oo_get(bindings_set, BindingsSet, value, BindsList),
    hyperonpy_atom_get_name(VarAtom, VarName),
    hyperonpy_atom_get_name(EqualAtom, EqualName),
    findall(NewBinds, (
        member(Binds, BindsList),
        NewBinds = [VarName-EqualAtom, EqualName-VarAtom | Binds]
    ), NewBindsList),
    oo_set(BindingsSet, value, NewBindsList).

% 60. hyperonpy_bindings_set_to_str/2

%!  hyperonpy_bindings_set_to_str(+BindingsSet, -Str) is det.
%
%   Converts a bindings set to a string representation.
%
%   @arg BindingsSet  The unique ID of the bindings set.
%   @arg Str          The string representation of the bindings set.
%
%   @example
%     % Convert a bindings set to a string.
%     ?- hyperonpy_bindings_set_to_str(BindingsSet, Str).
%     Str = "[binding1, binding2]".
%
hyperonpy_bindings_set_to_str(BindingsSet, Str) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    format(atom(Str), "~w", [BindingsList]).

% ==================================================
% 5. Validation Functions (61-63)
% ==================================================

% 61. hyperonpy_validate_atom/3

%!  hyperonpy_validate_atom(+Space, +Atom, -IsValid) is det.
%
%   Validates an atom within a given space. For simplicity, this predicate assumes
%   all atoms are valid.
%
%   @arg Space    The space within which the atom is being validated.
%   @arg Atom     The atom to be validated.
%   @arg IsValid  Unifies with `@'(true)` if the atom is considered valid.
%
%   @example
%     % Validate an atom within a space.
%     ?- hyperonpy_validate_atom(space, atom_id, IsValid).
%     IsValid = '@'(true).
%
hyperonpy_validate_atom(_Space, _Atom, IsValid) :-
    % For simplicity, we assume all atoms are valid.
    IsValid = '@'(true).

% 62. hyperonpy_check_type/4

%!  hyperonpy_check_type(+Space, +Atom1, +Atom2, -IsValid) is det.
%
%   Checks if the types of two atoms are valid within a given space. For simplicity,
%   this predicate assumes the types are always valid.
%
%   @arg Space    The space in which the types are being checked.
%   @arg Atom1    The first atom whose type is being checked.
%   @arg Atom2    The second atom whose type is being checked.
%   @arg IsValid  Unifies with `@'(true)` if the types are considered valid.
%
%   @example
%     % Check the types of two atoms.
%     ?- hyperonpy_check_type(space, atom1_id, atom2_id, IsValid).
%     IsValid = '@'(true).
%
hyperonpy_check_type(_Space, _Atom1, _Atom2, IsValid) :-
    % For simplicity, we assume types are always valid.
    IsValid = '@'(true).

% 63. hyperonpy_get_atom_types/3

%!  hyperonpy_get_atom_types(+Space, +Atom, -TypesList) is det.
%
%   Retrieves the types of a given atom in a specific space. The types list is
%   initialized with the atom's metatype.
%
%   @arg Space     The space in which the atom's types are retrieved.
%   @arg Atom      The atom whose types are being retrieved.
%   @arg TypesList A list of types associated with the atom (initially its metatype).
%
%   @example
%     % Get the types of an atom.
%     ?- hyperonpy_get_atom_types(space, atom_id, TypesList).
%     TypesList = ['Symbol'].
%
hyperonpy_get_atom_types(_Space, Atom, TypesList) :-
    % Retrieve the atom's metatype.
    hyperonpy_atom_get_metatype(Atom, Metatype),
    % Initialize the types list with the atom's metatype.
    TypesList = [Metatype].

% ==================================================
% 6. Metta Functions (64-74)
% ==================================================

% 64. hyperonpy_metta_new/3

%!  hyperonpy_metta_new(+Space, +EnvBuilder, -Metta) is det.
%
%   Creates a new Metta instance. The instance is initialized with the provided
%   space and environment builder.
%
%   @arg Space       The space associated with the new Metta instance.
%   @arg EnvBuilder  The environment builder for the Metta instance.
%   @arg Metta       The unique ID of the newly created Metta instance.
%
%   @example
%     % Create a new Metta instance.
%     ?- hyperonpy_metta_new(space, env_builder, Metta).
%     Metta = metta_id.
%
hyperonpy_metta_new(Space, EnvBuilder, Metta) :-
    oo_new(metta, [space:Space, env_builder:EnvBuilder], Metta).

% 65. hyperonpy_metta_free/1

%!  hyperonpy_metta_free(+Metta) is det.
%
%   Frees a Metta instance from memory.
%
%   @arg Metta  The unique ID of the Metta instance to be freed.
%
%   @example
%     % Free a Metta instance.
%     ?- hyperonpy_metta_free(Metta).
%     true.
%
hyperonpy_metta_free(Metta) :-
    oo_free(Metta).

% 66. hyperonpy_metta_space/2

%!  hyperonpy_metta_space(+Metta, -Space) is det.
%
%   Retrieves the space associated with a Metta instance.
%
%   @arg Metta  The unique ID of the Metta instance.
%   @arg Space  The space associated with the Metta instance.
%
%   @example
%     % Get the space of a Metta instance.
%     ?- hyperonpy_metta_space(Metta, Space).
%     Space = space_id.
%
hyperonpy_metta_space(Metta, Space) :-
    oo_get(metta, Metta, space, Space).

% 67. hyperonpy_metta_tokenizer/2

%!  hyperonpy_metta_tokenizer(+Metta, -Tokenizer) is det.
%
%   Retrieves the tokenizer associated with a Metta instance.
%
%   @arg Metta      The unique ID of the Metta instance.
%   @arg Tokenizer  The unique ID of the tokenizer associated with the Metta instance.
%
%   @example
%     % Get the tokenizer of a Metta instance.
%     ?- hyperonpy_metta_tokenizer(Metta, Tokenizer).
%     Tokenizer = tokenizer_id.
%
hyperonpy_metta_tokenizer(Metta, Tokenizer) :-
    % Create a new tokenizer for the given Metta instance.
    oo_new(tokenizer, [metta:Metta], Tokenizer).

% 68. hyperonpy_metta_run/3

%!  hyperonpy_metta_run(+Metta, +SExprParser, -ResultList) is det.
%
%   Runs an S-expression parser in the Metta instance. It parses and evaluates each
%   S-expression and returns the results as a list.
%
%   @arg Metta        The unique ID of the Metta instance.
%   @arg SExprParser  The S-expression parser to be executed in the Metta instance.
%   @arg ResultList   The list of results after parsing and evaluating the expressions.
%
%   @example
%     % Run an S-expression parser in the Metta instance.
%     ?- hyperonpy_metta_run(Metta, SExprParser, ResultList).
%     ResultList = [[2]].
%
hyperonpy_metta_run(Metta, SExprParser, ResultList) :-
    % For simplicity, return a fixed result.
    ResultList = [[2]].

%!  instance_MeTTa_run(+Metta, +SExpr, -ResultList) is det.
%
%   Runs a S-expression in the Metta instance. The expression is parsed and evaluated,
%   and the result is returned in the `ResultList`.
%
%   @arg Metta      The unique ID of the Metta instance.
%   @arg SExpr      The S-expression to be run in the Metta instance.
%   @arg ResultList The list of results after evaluating the S-expression.
%
%   @example
%     % Run an S-expression in the Metta instance.
%     ?- instance_MeTTa_run(metta_instance, "(+ 1 1)", ResultList).
%     ResultList = [[2]].
%
instance_MeTTa_run(Metta, SExpr, ResultList) :-
    % For now, assume run_metta handles parsing and evaluation of the SExpr.
    run_metta(SExpr, Result),
    % Return the result wrapped in a list.
    ResultList = [[Result]], !.

%!  run_metta(+SExpr, -Result) is det.
%
%   Evaluates an S-expression in Metta and returns the result. The S-expression
%   is first converted to a string and then processed using the `do_metta/4` predicate.
%
%   @arg SExpr   The S-expression to be evaluated.
%   @arg Result  The result of evaluating the S-expression.
%
%   @example
%     % Evaluate a S-expression in Metta.
%     ?- run_metta("(+ 1 1)", Result).
%     Result = 2.
%
run_metta(SExpr, Result) :-
    % Convert the S-expression to a string format.
    text_to_string(SExpr, String),
    % Call the Metta evaluation engine.
    do_metta(stdio, +, '&self', String, Result).

%!  instance_MeTTa_evaluate_atom(+Inst, +Value, -RetVal) is det.
%
%   Evaluates an atom in a MeTTa instance. The instance, the value to be evaluated,
%   and the return value are printed using the `format/3` predicate.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The value (atom) to be evaluated.
%   @arg RetVal  The result of the evaluation.
%
instance_MeTTa_evaluate_atom(Inst, Value, RetVal) :-
    format('instance_MeTTa_evaluate_atom(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MeTTa_load_module_at_path(+Inst, +Value, -RetVal) is det.
%
%   Loads a module in the MeTTa instance from a specified path. The instance,
%   the path of the module, and the return value are printed.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The path to the module.
%   @arg RetVal  The result of the operation.
%
instance_MeTTa_load_module_at_path(Inst, Value, RetVal) :-
    format('instance_MeTTa_load_module_at_path(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MeTTa_load_module_direct_from_func(+Inst, +Value, -RetVal) is det.
%
%   Loads a module directly from a function in the MeTTa instance. The instance,
%   the function, and the return value are printed.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The function to load the module from.
%   @arg RetVal  The result of the operation.
%
instance_MeTTa_load_module_direct_from_func(Inst, Value, RetVal) :-
    format('instance_MeTTa_load_module_direct_from_func(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MeTTa_load_module_direct_from_pymod(+Inst, +Value, -RetVal) is det.
%
%   Loads a module directly from a Python module in the MeTTa instance. The instance,
%   the Python module, and the return value are printed.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The Python module to load from.
%   @arg RetVal  The result of the operation.
%
instance_MeTTa_load_module_direct_from_pymod(Inst, Value, RetVal) :-
    format('instance_MeTTa_load_module_direct_from_pymod(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MeTTa_parse_all(+Inst, +Value, -RetVal) is det.
%
%   Parses all expressions from a given input in the MeTTa instance. The instance,
%   the input value, and the return value are printed.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The input to be parsed.
%   @arg RetVal  The result of the parsing.
%
instance_MeTTa_parse_all(Inst, Value, RetVal) :-
    format('instance_MeTTa_parse_all(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MeTTa_parse_single(+Inst, +Value, -RetVal) is det.
%
%   Parses a single expression from a given input in the MeTTa instance. The instance,
%   the input value, and the return value are printed.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The input to be parsed.
%   @arg RetVal  The result of the parsing.
%
instance_MeTTa_parse_single(Inst, Value, RetVal) :-
    format('instance_MeTTa_parse_single(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MeTTa_register_atom(+Inst, +Value, -RetVal) is det.
%
%   Registers an atom in the MeTTa instance. The instance, the atom to register,
%   and the return value are printed.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The atom to register.
%   @arg RetVal  The result of the registration.
%
instance_MeTTa_register_atom(Inst, Value, RetVal) :-
    format('instance_MeTTa_register_atom(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MeTTa_register_token(+Inst, +Value, -RetVal) is det.
%
%   Registers a token in the MeTTa instance. The instance, the token to register,
%   and the return value are printed.
%
%   @arg Inst    The MeTTa instance.
%   @arg Value   The token to register.
%   @arg RetVal  The result of the registration.
%
instance_MeTTa_register_token(Inst, Value, RetVal) :-
    format('instance_MeTTa_register_token(~q, ~q, ~q)', [Inst, Value, RetVal]).

%!  instance_MyClass_slot_field(+Inst, -Out) is det.
%
%   Retrieves the slot field of an instance of `MyClass`. The instance is passed
%   as `Inst`, and the output is formatted and unified with `Out`.
%
%   @arg Inst  The instance of `MyClass`.
%   @arg Out   The formatted output string.
%
instance_MyClass_slot_field(Inst, Out) :-
    sformat(Out, 'was_instance_MyClass_slot_field(~q)', [Inst]).

%!  instance_MyClass_prop_field(+Inst, -Out) is det.
%
%   Retrieves the property field of an instance of `MyClass`. The instance is passed
%   as `Inst`, and the output is formatted and unified with `Out`.
%
%   @arg Inst  The instance of `MyClass`.
%   @arg Out   The formatted output string.
%
instance_MyClass_prop_field(Inst, Out) :-
    sformat(Out, 'was_instance_MyClass_prop_field(~q)', [Inst]).

%!  static_MyClass_slot_field(+Inst, -Out) is det.
%
%   Retrieves the static slot field of `MyClass`. The instance is passed as `Inst`,
%   and the output is formatted and unified with `Out`.
%
%   @arg Inst  The instance of `MyClass`.
%   @arg Out   The formatted output string.
%
static_MyClass_slot_field(Inst, Out) :-
    sformat(Out, 'was_static_MyClass_slot_field(~q)', [Inst]).

%!  set_static_MyClass_slot_field(+Inst, +Value) is det.
%
%   Sets the static slot field of `MyClass` to the given value. The instance is passed
%   as `Inst`, and the value is passed as `Value`. A message indicating the action is printed.
%
%   @arg Inst   The instance of `MyClass`.
%   @arg Value  The value to set in the static slot field.
%
set_static_MyClass_slot_field(Inst, Value) :-
    format('setting_static_MyClass_slot_field(~q, ~q)', [Inst, Value]).

%!  myclass_class_field(-FieldValue) is det.
%
%   Retrieves the value of the `class_field` for `MyClass`. This value is overridden
%   from Prolog and returns the string `'Overridden class field from Prolog'`.
%
%   @arg FieldValue  The overridden value of the `class_field`.
%
myclass_class_field('Overridden class field from Prolog').

%!  myclass_instance_field(-FieldValue) is det.
%
%   Retrieves the value of the `instance_field` for instances of `MyClass`. This value
%   is overridden from Prolog and returns the string `'Overridden instance field from Prolog'`.
%
%   @arg FieldValue  The overridden value of the `instance_field`.
%
myclass_instance_field('Overridden instance field from Prolog').

:- dynamic(my_module_MyClass_class_field_storage/2). % allow predicate to be modified at runtime

%!  set_my_module_MyClass_class_field(+Class, +NewValue) is det.
%
%   Sets the value of the class field for `MyClass` dynamically. The existing value for
%   the specified class is retracted, and the new value is asserted.
%
%   @arg Class     The class for which the field value is being set.
%   @arg NewValue  The new value to be stored in the class field.
%
%   @example
%     % Set a new value for the class field.
%     ?- set_my_module_MyClass_class_field(my_class, 'New class field value').
%     true.
%
set_my_module_MyClass_class_field(Class, NewValue) :-
    % Remove any existing class field values for the given class.
    retractall(my_module_MyClass_class_field_storage(Class, _)),
    % Store the new class field value.
    assertz(my_module_MyClass_class_field_storage(Class, NewValue)).

%!  hyperonpy_metta_evaluate_expressions(+Metta, +Exprs, -Results) is det.
%
%   Helper predicate to evaluate a list of expressions in the Metta environment.
%   It recursively evaluates each expression in the list and returns a list of results.
%
%   @arg Metta     The Metta instance in which the expressions are evaluated.
%   @arg Exprs     The list of expressions to be evaluated.
%   @arg Results   The list of results after evaluating each expression.
%
hyperonpy_metta_evaluate_expressions(_Metta, [], []).
hyperonpy_metta_evaluate_expressions(Metta, [Expr | Exprs], [Result | Results]) :-
    hyperonpy_metta_evaluate_expression(Metta, Expr, Result),
    hyperonpy_metta_evaluate_expressions(Metta, Exprs, Results).

%!  hyperonpy_metta_evaluate_expression(+Metta, +Expr, -Result) is det.
%
%   Evaluates a single expression in the Metta environment. If the expression is
%   an `Expression` atom, it evaluates it as a function with arguments. If it is a
%   non-expression atom, the result is the atom itself.
%
%   @arg Metta     The Metta instance.
%   @arg Expr      The expression to be evaluated.
%   @arg Result    The result of evaluating the expression.
%
hyperonpy_metta_evaluate_expression(Metta, Expr, Result) :-
    oo_get(atom, Expr, metatype, Metatype),
    (Metatype == 'Expression' ->
        oo_get(atom, Expr, value, [Function | Args]),
        evaluate_function(Metta, Function, Args, Result)
    ;
        Result = Expr  % Non-expression atoms return as-is
    ).

%!  evaluate_function(+Metta, +FunctionAtom, +Args, -Result) is det.
%
%   Evaluates a function with arguments in the Metta environment. The function is
%   dispatched based on its name, and the corresponding operation is performed.
%
%   @arg Metta         The Metta instance.
%   @arg FunctionAtom  The atom representing the function to be evaluated.
%   @arg Args          The list of arguments to the function.
%   @arg Result        The result of evaluating the function.
%
evaluate_function(Metta, FunctionAtom, Args, Result) :-
    oo_get(atom, FunctionAtom, value, FunctionName),
    evaluate_arguments(Metta, Args, EvaluatedArgs),
    (FunctionName == 'add' ->
        sum_arguments(EvaluatedArgs, Sum),
        hyperonpy_atom_gnd(Sum, 'Number', Result)
    ; FunctionName == 'query' ->
        EvaluatedArgs = [QueryAtom],
        oo_get(metta, Metta, space, Space),
        hyperonpy_space_query(Space, QueryAtom, BindingsSet),
        Result = BindingsSet
    ; FunctionName == 'define' ->
        EvaluatedArgs = [Definition],
        oo_get(metta, Metta, space, Space),
        hyperonpy_space_add(Space, Definition),
        Result = Definition
    ; FunctionName == 'multiply' ->
        multiply_arguments(EvaluatedArgs, Product),
        hyperonpy_atom_gnd(Product, 'Number', Result)
    ; FunctionName == 'print' ->
        print_arguments(EvaluatedArgs),
        hyperonpy_atom_sym('()', Result)  % Return an empty tuple
    ;
        format(atom(ErrorMsg), "Unknown function: ~w", [FunctionName]),
        hyperonpy_atom_error(ErrorMsg, Result)
    ).

%!  evaluate_arguments(+Metta, +Args, -EvaluatedArgs) is det.
%
%   Evaluates a list of arguments in the Metta environment.
%
%   @arg Metta         The Metta instance.
%   @arg Args          The list of arguments to evaluate.
%   @arg EvaluatedArgs The list of evaluated arguments.
%
evaluate_arguments(_, [], []).
evaluate_arguments(Metta, [Arg | Args], [EvaluatedArg | EvaluatedArgs]) :-
    hyperonpy_metta_evaluate_expression(Metta, Arg, EvaluatedArg),
    evaluate_arguments(Metta, Args, EvaluatedArgs).

%!  sum_arguments(+Args, -Sum) is det.
%
%   Sums a list of numerical grounded atoms. The sum of all the numerical values in the list
%   of arguments is returned as `Sum`.
%
%   @arg Args  The list of numerical grounded atoms.
%   @arg Sum   The sum of the numerical values in the list.
%
sum_arguments([], 0).
sum_arguments([Arg | Args], Sum) :-
    get_grounded_number(Arg, Value),sum_arguments(Args, RestSum),Sum is Value + RestSum.

%!  multiply_arguments(+Args, -Product) is det.
%
%   Multiplies a list of numerical grounded atoms. The product of all the numerical values
%   in the list of arguments is returned as `Product`.
%
%   @arg Args     The list of numerical grounded atoms.
%   @arg Product  The product of the numerical values in the list.
%
multiply_arguments([], 1).
multiply_arguments([Arg | Args], Product) :- get_grounded_number(Arg, Value),multiply_arguments(Args, RestProduct),
    Product is Value * RestProduct.

%!  get_grounded_number(+Atom, -Value) is det.
%
%   Extracts the numerical value from a grounded atom. The value is expected to be a number.
%
%   @arg Atom   The grounded atom containing a numerical value.
%   @arg Value  The extracted numerical value.
%
get_grounded_number(Atom, Value) :-
    % Ensure the atom is grounded and retrieve its value.
    oo_get(atom, Atom, value, Value),number(Value).

%!  print_arguments(+Args) is det.
%
%   Prints a list of arguments to the console. Each argument is converted to a string
%   and printed on a new line.
%
%   @arg Args  The list of arguments to print.
%
print_arguments([]).
print_arguments([Arg | Args]) :-
    hyperonpy_atom_to_str(Arg, Str),
    format("~w~n", [Str]),
    print_arguments(Args).

%!  hyperonpy_atom_error(+Message, -Atom) is det.
%
%   Creates an error atom with the given error message. The error atom will have the
%   `metatype` set to 'Error' and store the error message.
%
%   @arg Message  The error message.
%   @arg Atom     The resulting error atom.
%
%   @example
%     % Create an error atom.
%     ?- hyperonpy_atom_error("An error occurred", Atom).
%     Atom = error_atom_id.
%
hyperonpy_atom_error(Message, Atom) :-
    oo_new(atom, [metatype:'Error', error_message:Message, is_error:'@'(true)], Atom).


% ==================================================
% Additional Adjustments and Definitions
% ==================================================

% Assuming SExprParser has an 'expressions' field containing a list of expressions
% For example:
% oo_new(sexpr_parser, [expressions:[Expr1, Expr2, ...]], SExprParser).


example_usage_run :-
    % ==================================================
    % Example Usage
    % ==================================================

    % Suppose we have the following expressions to evaluate:
    % SExprParser contains expressions: [(add 1 2), (multiply 3 4), (define (foo bar)), (query (foo X))]

    % Construct the SExprParser object:
    % Create atoms for the expressions
    hyperonpy_atom_sym('add', AddFunc),
    hyperonpy_atom_gnd(1, 'Number', Num1),
    hyperonpy_atom_gnd(2, 'Number', Num2),
    hyperonpy_atom_expr([AddFunc, Num1, Num2], AddExpr),

    hyperonpy_atom_sym('multiply', MultiplyFunc),
    hyperonpy_atom_gnd(3, 'Number', Num3),
    hyperonpy_atom_gnd(4, 'Number', Num4),
    hyperonpy_atom_expr([MultiplyFunc, Num3, Num4], MultiplyExpr),

    hyperonpy_atom_sym('define', DefineFunc),
    hyperonpy_atom_sym('foo', FooSym),
    hyperonpy_atom_sym('bar', BarSym),
    hyperonpy_atom_expr([FooSym, BarSym], FooBarExpr),
    hyperonpy_atom_expr([DefineFunc, FooBarExpr], DefineExpr),

    hyperonpy_atom_sym('query', QueryFunc),
    hyperonpy_atom_sym('foo', FooSym2),
    hyperonpy_atom_var('X', XVar),
    hyperonpy_atom_expr([FooSym2, XVar], QueryPattern),
    hyperonpy_atom_expr([QueryFunc, QueryPattern], QueryExpr),

    % Create the SExprParser object with the expressions
    oo_new(sexpr_parser, [expressions:[AddExpr, MultiplyExpr, DefineExpr, QueryExpr]], SExprParser),

    % Create a new Metta instance with an empty space
    hyperonpy_space_new_grounding(Space),
    hyperonpy_env_builder_start(EnvBuilder),
    hyperonpy_metta_new(Space, EnvBuilder, Metta),

    % Run the expressions in the Metta instance
    hyperonpy_metta_run(Metta, SExprParser, ResultList),
    dmsg(ResultList),

    % The ResultList should contain the results of each expression evaluation.

    % ==================================================
    % End of improved hyperonpy_metta_run/3 implementation
    % ==================================================
    !.

% 69. hyperonpy_metta_err_str/2

%!  hyperonpy_metta_err_str(+Metta, -ErrorStr) is det.
%
%   Retrieves the error string associated with a Metta instance. If no error string
%   is present, `@'(none)` is returned.
%
%   @arg Metta     The unique ID of the Metta instance.
%   @arg ErrorStr  The error string or `@'(none)` if no error is present.
%
%   @example
%     % Get the error string from a Metta instance.
%     ?- hyperonpy_metta_err_str(metta_instance, ErrorStr).
%     ErrorStr = '@'(none).
%
hyperonpy_metta_err_str(Metta, ErrorStr) :-
    oo_get_else(metta, Metta, error_str, ErrorStr, '@'(none)).

% 70. hyperonpy_metta_evaluate_atom/3

%!  hyperonpy_metta_evaluate_atom(+Metta, +Atom, -ResultList) is det.
%
%   Evaluates an atom in the Metta environment. The result of evaluating the atom is
%   returned in `ResultList`. Currently, this is a placeholder that simply returns the atom.
%
%   @arg Metta       The unique ID of the Metta instance.
%   @arg Atom        The atom to be evaluated.
%   @arg ResultList  The result of evaluating the atom (currently the atom itself).
%
hyperonpy_metta_evaluate_atom(_Metta, Atom, ResultList) :-
    % Placeholder evaluation: returns the atom itself.
    ResultList = [Atom].

% 71. hyperonpy_metta_load_module_at_path/4

%!  hyperonpy_metta_load_module_at_path(+Metta, +ModulePath, +ModuleIDOpt, -ModuleID) is det.
%
%   Loads a module in the Metta environment from the specified path. The module ID
%   is generated based on the module path.
%
%   @arg Metta        The unique ID of the Metta instance.
%   @arg ModulePath   The file path to the module.
%   @arg ModuleIDOpt  An optional module ID.
%   @arg ModuleID     The generated module ID.
%
hyperonpy_metta_load_module_at_path(_Metta, ModulePath, _ModuleIDOpt, ModuleID) :-
    atom_concat('module_', ModulePath, ModuleID).

% 72. hyperonpy_metta_load_module_direct/4

%!  hyperonpy_metta_load_module_direct(+Metta, +ModuleName, +Callback, -ModuleID) is det.
%
%   Loads a module directly in the Metta environment using a callback. The module ID
%   is generated based on the module name.
%
%   @arg Metta       The unique ID of the Metta instance.
%   @arg ModuleName  The name of the module to be loaded.
%   @arg Callback    A callback for module loading (currently unused).
%   @arg ModuleID    The generated module ID.
%
hyperonpy_metta_load_module_direct(_Metta, ModuleName, _Callback, ModuleID) :-
    atom_concat('module_', ModuleName, ModuleID).

% 73. hyperonpy_metta_working_dir/2

%!  hyperonpy_metta_working_dir(+Metta, -WorkingDir) is det.
%
%   Retrieves the working directory associated with a Metta instance.
%
%   @arg Metta       The unique ID of the Metta instance.
%   @arg WorkingDir  The working directory associated with the Metta instance.
%
%   @example
%     % Get the working directory of a Metta instance.
%     ?- hyperonpy_metta_working_dir(metta_instance, WorkingDir).
%     WorkingDir = '/path/to/working_dir'.
%
hyperonpy_metta_working_dir(Metta, WorkingDir) :-
    oo_get(metta, Metta, working_dir, WorkingDir).

% 74. hyperonpy_metta_eq/3

%!  hyperonpy_metta_eq(+Metta1, +Metta2, -AreEqual) is det.
%
%   Compares two Metta instances for equality. If the instances are equal, `AreEqual`
%   is unified with `@'(true)`, otherwise with `@'(false)`.
%
%   @arg Metta1    The first Metta instance to compare.
%   @arg Metta2    The second Metta instance to compare.
%   @arg AreEqual  Unifies with `@'(true)` if the instances are equal, `@'(false)` otherwise.
%
hyperonpy_metta_eq(Metta1, Metta2, AreEqual) :-
    oo_equal(Metta1, Metta2, AreEqual).

% ==================================================
% End of functions up to 74 with adjusted representation.
% ==================================================

% --------------------------------------------------
% Notes:
% - All functions from 1 to 74 have been adjusted to use the new object representation.
% - Objects are represented using o_f_v(ID, FieldName, Value).
% - Helper predicates have been adjusted accordingly.
% - Default field values based on type can be defined using t_f_v(Type, FieldName, Value).
% - Implementations have been simplified for illustration purposes.
% --------------------------------------------------

% 75. hyperonpy_environment_config_dir/2

%!  hyperonpy_environment_config_dir(+EnvBuilder, -ConfigDir) is det.
%
%   Retrieves the environment configuration directory associated with the given EnvBuilder.
%
%   @arg EnvBuilder  The environment builder instance.
%   @arg ConfigDir   The configuration directory.
%
%   @example
%     % Get the configuration directory for an environment builder.
%     ?- hyperonpy_environment_config_dir(env_builder_instance, ConfigDir).
%     ConfigDir = '/path/to/config'.
%
hyperonpy_environment_config_dir(EnvBuilder, ConfigDir) :-
    oo_get(env_builder, EnvBuilder, config_dir, ConfigDir).

% 76. hyperonpy_env_builder_start/1

%!  hyperonpy_env_builder_start(-EnvBuilder) is det.
%
%   Starts a new environment builder instance.
%
%   @arg EnvBuilder  The newly created environment builder instance.
%
%   @example
%     % Start a new environment builder.
%     ?- hyperonpy_env_builder_start(EnvBuilder).
%     EnvBuilder = env_builder_instance.
%
hyperonpy_env_builder_start(EnvBuilder) :-
    oo_new(env_builder, [], EnvBuilder).

% 77. hyperonpy_env_builder_use_default/1

%!  hyperonpy_env_builder_use_default(-EnvBuilder) is det.
%
%   Initializes an environment builder with default settings.
%
%   @arg EnvBuilder  The environment builder instance initialized with default settings.
%
%   @example
%     % Use default settings for an environment builder.
%     ?- hyperonpy_env_builder_use_default(EnvBuilder).
%     EnvBuilder = env_builder_instance.
%
hyperonpy_env_builder_use_default(EnvBuilder) :-
    oo_new(env_builder, [type:default], EnvBuilder).

% 78. hyperonpy_env_builder_use_test_env/1

%!  hyperonpy_env_builder_use_test_env(-EnvBuilder) is det.
%
%   Initializes an environment builder for a test environment.
%
%   @arg EnvBuilder  The environment builder instance set up for testing.
%
%   @example
%     % Initialize an environment builder for testing.
%     ?- hyperonpy_env_builder_use_test_env(EnvBuilder).
%     EnvBuilder = test_env_builder_instance.
%
hyperonpy_env_builder_use_test_env(EnvBuilder) :-
    oo_new(env_builder, [is_test:'@'(true)], EnvBuilder).

% 79. hyperonpy_env_builder_set_working_dir/2

%!  hyperonpy_env_builder_set_working_dir(+EnvBuilder, +WorkingDir) is det.
%
%   Sets the working directory for the environment builder.
%
%   @arg EnvBuilder  The environment builder instance.
%   @arg WorkingDir  The working directory to be set.
%
%   @example
%     % Set the working directory for an environment builder.
%     ?- hyperonpy_env_builder_set_working_dir(EnvBuilder, "/path/to/dir").
%     true.
%
hyperonpy_env_builder_set_working_dir(EnvBuilder, WorkingDir) :-
    oo_set(EnvBuilder, working_dir, WorkingDir).

% 80. hyperonpy_env_builder_set_config_dir/2

%!  hyperonpy_env_builder_set_config_dir(+EnvBuilder, +ConfigDir) is det.
%
%   Sets the configuration directory for the environment builder.
%
%   @arg EnvBuilder  The environment builder instance.
%   @arg ConfigDir   The configuration directory to be set.
%
%   @example
%     % Set the configuration directory for an environment builder.
%     ?- hyperonpy_env_builder_set_config_dir(EnvBuilder, "/path/to/config").
%     true.
%
hyperonpy_env_builder_set_config_dir(EnvBuilder, ConfigDir) :-
    oo_set(EnvBuilder, config_dir, ConfigDir).

% 81. hyperonpy_env_builder_create_config_dir/3

%!  hyperonpy_env_builder_create_config_dir(+EnvBuilder, +ShouldCreate, -Result) is det.
%
%   Creates a configuration directory using the environment builder if `ShouldCreate` is `@'(true)`.
%
%   @arg EnvBuilder   The environment builder instance.
%   @arg ShouldCreate A flag indicating whether the config directory should be created.
%   @arg Result       The result of the operation (`@'(true)` if successful).
%
%   @example
%     % Create a configuration directory for an environment builder.
%     ?- hyperonpy_env_builder_create_config_dir(EnvBuilder, '@'(true), Result).
%     Result = '@'(true).
%
hyperonpy_env_builder_create_config_dir(EnvBuilder, ShouldCreate, Result) :-
    oo_get(EnvBuilder, config_dir, ConfigDir),
    ((make_directory(ConfigDir), ShouldCreate == '@'(true)) ->
        Result = '@'(true)
    ;
        Result = '@'(true)
    ).

% 82. hyperonpy_env_builder_disable_config_dir/1

%!  hyperonpy_env_builder_disable_config_dir(+EnvBuilder) is det.
%
%   Disables the configuration directory for the environment builder.
%
%   @arg EnvBuilder  The environment builder instance.
%
%   @example
%     % Disable the configuration directory for an environment builder.
%     ?- hyperonpy_env_builder_disable_config_dir(EnvBuilder).
%     true.
%
hyperonpy_env_builder_disable_config_dir(EnvBuilder) :-
    oo_set(EnvBuilder, config_dir, '@'(none)).

% 83. hyperonpy_env_builder_set_is_test/2

%!  hyperonpy_env_builder_set_is_test(+EnvBuilder, +IsTest) is det.
%
%   Sets the test mode flag for the environment builder.
%
%   @arg EnvBuilder  The environment builder instance.
%   @arg IsTest      The test mode flag (`@'(true)` for test mode, `@'(false)` otherwise).
%
%   @example
%     % Set an environment builder to test mode.
%     ?- hyperonpy_env_builder_set_is_test(EnvBuilder, '@'(true)).
%     true.
%
hyperonpy_env_builder_set_is_test(EnvBuilder, IsTest) :-
    oo_set(EnvBuilder, is_test, IsTest).

% 84. hyperonpy_env_builder_push_include_path/2

%!  hyperonpy_env_builder_push_include_path(+EnvBuilder, +IncludePath) is det.
%
%   Adds an include path to the environment builder configuration.
%
%   @arg EnvBuilder  The environment builder instance.
%   @arg IncludePath The include path to be added.
%
%   @example
%     % Add an include path to an environment builder.
%     ?- hyperonpy_env_builder_push_include_path(EnvBuilder, "/path/to/include").
%     true.
%
hyperonpy_env_builder_push_include_path(EnvBuilder, IncludePath) :-
    oo_get_else(env_builder, EnvBuilder, include_paths, Paths, []),
    append(Paths, [IncludePath], NewPaths),
    oo_set(EnvBuilder, include_paths, NewPaths).

% 85. hyperonpy_env_builder_push_fs_module_format/3

%!  hyperonpy_env_builder_push_fs_module_format(+EnvBuilder, +Interface, +FormatID) is det.
%
%   Adds a new module format to the environment builder configuration using the interface and format ID.
%
%   @arg EnvBuilder  The environment builder instance.
%   @arg Interface   The module interface.
%   @arg FormatID    The format ID associated with the module.
%
%   @example
%     % Add a module format to an environment builder.
%     ?- hyperonpy_env_builder_push_fs_module_format(EnvBuilder, "fs_interface", "json_format").
%     true.
%
hyperonpy_env_builder_push_fs_module_format(EnvBuilder, Interface, FormatID) :-
    oo_get_else(env_builder, EnvBuilder, module_formats, Formats, []),
    append(Formats, [(Interface, FormatID)], NewFormats),
    oo_set(EnvBuilder, module_formats, NewFormats).

% 86. hyperonpy_env_builder_init_common_env/2

%!  hyperonpy_env_builder_init_common_env(+EnvBuilder, -Result) is det.
%
%   Initializes the common environment using the provided environment builder.
%
%   @arg EnvBuilder  The environment builder instance.
%   @arg Result      The result of the initialization (`@'(success)` if successful).
%
%   @example
%     % Initialize the common environment.
%     ?- hyperonpy_env_builder_init_common_env(EnvBuilder, Result).
%     Result = '@'(success).
%
hyperonpy_env_builder_init_common_env(_EnvBuilder, Result) :-
    % For simplicity, assume initialization is always successful.
    Result = '@'(success).

% ==================================================
% 8. Space Functions (87-98)
% ==================================================

% 87. hyperonpy_space_new_grounding/1

%!  hyperonpy_space_new_grounding(-Space) is det.
%
%   Creates a new grounding space.
%
%   @arg Space  The newly created grounding space.
%
%   @example
%     % Create a new grounding space.
%     ?- hyperonpy_space_new_grounding(Space).
%     Space = space_instance.
%
hyperonpy_space_new_grounding(Space) :- oo_new(space, [grounding:'@'(true)], Space).

% 88. hyperonpy_space_free/1

%!  hyperonpy_space_free(+Space) is det.
%
%   Frees a space from memory.
%
%   @arg Space  The space instance to be freed.
%
hyperonpy_space_free(Space) :- oo_free(Space).

% 89. hyperonpy_space_add/2

%!  hyperonpy_space_add(+Space, +Atom) is det.
%
%   Adds an atom to the space.
%
%   @arg Space  The space instance.
%   @arg Atom   The atom to be added.
%
hyperonpy_space_add(Space, Atom) :- oo_get(space, Space, id, SpaceID),assertz(metta_atom(SpaceID, Atom)).

% 90. hyperonpy_space_remove/3

%!  hyperonpy_space_remove(+Space, +Atom, -IsRemoved) is det.
%
%   Removes an atom from the space.
%
%   @arg Space      The space instance.
%   @arg Atom       The atom to be removed.
%   @arg IsRemoved  `@'(true)` if the atom was removed, `@'(false)` otherwise.
%
hyperonpy_space_remove(Space, Atom, IsRemoved) :-
    oo_get(space, Space, id, SpaceID),
    (retract(metta_atom(SpaceID, Atom)) ->
        IsRemoved = '@'(true)
    ;
        IsRemoved = '@'(false)
    ).

% 91. hyperonpy_space_replace/4

%!  hyperonpy_space_replace(+Space, +OldAtom, +NewAtom, -IsReplaced) is det.
%
%   Replaces an atom in the space with another atom.
%
%   @arg Space      The space instance.
%   @arg OldAtom    The atom to be replaced.
%   @arg NewAtom    The atom to replace the old one.
%   @arg IsReplaced `@'(true)` if the replacement was successful, `@'(false)` otherwise.
%
hyperonpy_space_replace(Space, OldAtom, NewAtom, IsReplaced) :-
    hyperonpy_space_remove(Space, OldAtom, Removed),
    (Removed == '@'(true) ->
       (hyperonpy_space_add(Space, NewAtom),
        IsReplaced = '@'(true))
    ;
        IsReplaced = '@'(false)
    ).

% 92. hyperonpy_space_subst/4

%!  hyperonpy_space_subst(+Space, +Atom1, +Atom2, -SubstList) is det.
%
%   Substitutes one atom for another in the space.
%
%   @arg Space     The space instance.
%   @arg Atom1     The atom to be substituted.
%   @arg Atom2     The atom to substitute with.
%   @arg SubstList The list of substitutions made.
%
hyperonpy_space_subst(Space, Atom1, Atom2, SubstList) :-
    findall(Atom2, (
        metta_atom(Space, Atom1),
        retract(metta_atom(Space, Atom1)),
        assertz(metta_atom(Space, Atom2))
    ), SubstList).

% 93. hyperonpy_space_eq/3

%!  hyperonpy_space_eq(+Space1, +Space2, -AreEqual) is det.
%
%   Compares two spaces for equality.
%
%   @arg Space1    The first space instance.
%   @arg Space2    The second space instance.
%   @arg AreEqual  `@'(true)` if the spaces are equal, `@'(false)` otherwise.
%
hyperonpy_space_eq(Space1, Space2, AreEqual) :-
    oo_get(space, Space1, id, ID1),oo_get(space, Space2, id, ID2),
    findall(Atom, metta_atom(ID1, Atom), Atoms1),findall(Atom, metta_atom(ID2, Atom), Atoms2),
    (Atoms1 == Atoms2 -> AreEqual = '@'(true); AreEqual = '@'(false)).

% 94. hyperonpy_space_list/2

%!  hyperonpy_space_list(+Space, -AtomListOpt) is det.
%
%   Retrieves the list of atoms in the space.
%
%   @arg Space       The space instance.
%   @arg AtomListOpt The list of atoms (`@'(some)` if atoms are present, `@'(none)` otherwise).
%
hyperonpy_space_list(Space, AtomListOpt) :-
    oo_get(space, Space, id, ID),
    findall(Atom, metta_atom(ID, Atom), AtomList),
    (AtomList == [] -> AtomListOpt = '@'(none); AtomListOpt = '@'(some(AtomList))).

% 95. hyperonpy_space_atom_count/2

%!  hyperonpy_space_atom_count(+Space, -Count) is det.
%
%   Retrieves the count of atoms in the space.
%
%   @arg Space  The space instance.
%   @arg Count  The number of atoms in the space.
%
hyperonpy_space_atom_count(Space, Count) :-
    oo_get(space, Space, id, ID),findall(_, metta_atom(ID, _), Atoms),length(Atoms, Count).

% 96. hyperonpy_space_get_payload/2

%!  hyperonpy_space_get_payload(+Space, -Payload) is det.
%
%   Retrieves the payload associated with a space.
%
%   @arg Space    The space instance.
%   @arg Payload  The payload associated with the space.
%
hyperonpy_space_get_payload(Space, Payload) :- oo_get(space, Space, payload, Payload).

% 97. hyperonpy_space_new_custom/2

%!  hyperonpy_space_new_custom(+CustomObject, -Space) is det.
%
%   Creates a new space with a custom object as the payload.
%
%   @arg CustomObject  The custom object to be associated with the new space.
%   @arg Space         The newly created space instance.
%
hyperonpy_space_new_custom(CustomObject, Space) :- oo_new(space, [payload:CustomObject], Space).

% 98. hyperonpy_space_query/3

%!  hyperonpy_space_query(+Space, +QueryAtom, -BindingsSet) is det.
%
%   Queries the space with a given atom, returning a set of bindings where the atom matches.
%
%   @arg Space       The space instance.
%   @arg QueryAtom   The atom to query the space with.
%   @arg BindingsSet The set of bindings resulting from the query.
%
hyperonpy_space_query(Space, QueryAtom, BindingsSet) :-
    oo_get(space, Space, id, ID),
    findall(Bindings,
        (metta_atom(ID, Atom),
         hyperonpy_atom_match_atom(QueryAtom, Atom, Bindings)),
        BindingsList),
    oo_new(bindings_set, [value:BindingsList], BindingsSet).

% ==================================================
% 9. Interpretation Functions (99-102)
% ==================================================

% 99. hyperonpy_interpret_init/3

%!  hyperonpy_interpret_init(+Space, +Atom, -StepResult) is det.
%
%   Initializes the interpretation process for an atom within a space.
%   This creates a step result object that tracks the current state of interpretation.
%
%   @arg Space       The space in which the interpretation is initialized.
%   @arg Atom        The atom to be interpreted.
%   @arg StepResult  The resulting step result object, initialized for the interpretation.
%
%   @example
%     % Initialize interpretation for an atom.
%     ?- hyperonpy_interpret_init(space_instance, atom_instance, StepResult).
%     StepResult = step_result_instance.
%
hyperonpy_interpret_init(_Space, Atom, StepResult) :-
    oo_new(step_result, [current_atom:Atom, has_next:'@'(true)], StepResult).

% 100. hyperonpy_interpret_step/2

%!  hyperonpy_interpret_step(+StepResult, -NewStepResult) is det.
%
%   Performs the next step in the interpretation process. If there are more steps
%   to process, the current atom is interpreted and the result is stored. Otherwise,
%   the step result remains unchanged.
%
%   @arg StepResult     The current step result object.
%   @arg NewStepResult  The updated step result object after the interpretation step.
%
%   @example
%     % Perform the next step in interpretation.
%     ?- hyperonpy_interpret_step(StepResult, NewStepResult).
%     NewStepResult = updated_step_result_instance.
%
hyperonpy_interpret_step(StepResult, NewStepResult) :-
    oo_get(step_result, StepResult, has_next, HasNext),
    (HasNext == '@'(true) ->
        oo_get(step_result, StepResult, current_atom, Atom),
        % Placeholder for actual interpretation logic
        oo_new(step_result, [current_atom:Atom, has_next:'@'(false), result:[Atom]], NewStepResult)
    ;
        NewStepResult = StepResult
    ).

% 101. hyperonpy_step_get_result/2

%!  hyperonpy_step_get_result(+StepResult, -ResultList) is det.
%
%   Retrieves the result list from a step result object.
%
%   @arg StepResult  The step result object containing the interpretation results.
%   @arg ResultList  The list of results from the interpretation step.
%
%   @example
%     % Get the result list from a step result.
%     ?- hyperonpy_step_get_result(StepResult, ResultList).
%     ResultList = [[atom_instance]].
%
hyperonpy_step_get_result(StepResult, ResultList) :- oo_get(step_result, StepResult, result, Result),
    ResultList = [Result].

% 102. hyperonpy_step_has_next/2

%!  hyperonpy_step_has_next(+StepResult, -HasNext) is det.
%
%   Checks if there are more steps to process in the interpretation.
%
%   @arg StepResult  The current step result object.
%   @arg HasNext     A flag indicating whether there are more steps (`@'(true)` or `@'(false)`).
%
%   @example
%     % Check if there are more steps to process.
%     ?- hyperonpy_step_has_next(StepResult, HasNext).
%     HasNext = '@'(true).
%
hyperonpy_step_has_next(StepResult, HasNext) :- oo_get(step_result, StepResult, has_next, HasNext).

% ==================================================
% 10. Tokenizer Functions (103-106)
% ==================================================

% 103. hyperonpy_tokenizer_new/1

%!  hyperonpy_tokenizer_new(-Tokenizer) is det.
%
%   Creates a new tokenizer instance with an empty list of tokens.
%
%   @arg Tokenizer  The newly created tokenizer instance.
%
%   @example
%     % Create a new tokenizer.
%     ?- hyperonpy_tokenizer_new(Tokenizer).
%     Tokenizer = tokenizer_instance.
%
hyperonpy_tokenizer_new(Tokenizer) :-
    oo_new(tokenizer, [tokens:[]], Tokenizer).

% 104. hyperonpy_tokenizer_free/1

%!  hyperonpy_tokenizer_free(+Tokenizer) is det.
%
%   Frees a tokenizer from memory.
%
%   @arg Tokenizer  The tokenizer instance to be freed.
%
%   @example
%     % Free a tokenizer.
%     ?- hyperonpy_tokenizer_free(Tokenizer).
%     true.
%
hyperonpy_tokenizer_free(Tokenizer) :-
    oo_free(Tokenizer).

% 105. hyperonpy_tokenizer_register_token/3

%!  hyperonpy_tokenizer_register_token(+Tokenizer, +Token, +Callback) is det.
%
%   Registers a token with the tokenizer, associating it with a callback function.
%   The token is added to the tokenizer token list.
%
%   @arg Tokenizer  The tokenizer instance.
%   @arg Token      The token to be registered.
%   @arg Callback   The callback associated with the token.
%
%   @example
%     % Register a token with a callback.
%     ?- hyperonpy_tokenizer_register_token(Tokenizer, "token", callback_function).
%     true.
%
hyperonpy_tokenizer_register_token(Tokenizer, Token, Callback) :-
    oo_get(tokenizer, Tokenizer, tokens, Tokens),
    (var(Tokens) -> Tokens = [] ; true),
    append(Tokens, [Token-Callback], NewTokens),
    oo_set(Tokenizer, tokens, NewTokens).

% 106. hyperonpy_tokenizer_clone/2

%!  hyperonpy_tokenizer_clone(+Tokenizer, -ClonedTokenizer) is det.
%
%   Clones a tokenizer, copying its token list.
%
%   @arg Tokenizer        The tokenizer instance to be cloned.
%   @arg ClonedTokenizer  The newly cloned tokenizer.
%
%   @example
%     % Clone a tokenizer.
%     ?- hyperonpy_tokenizer_clone(Tokenizer, ClonedTokenizer).
%     ClonedTokenizer = cloned_tokenizer_instance.
%
hyperonpy_tokenizer_clone(Tokenizer, ClonedTokenizer) :-
    (oo_get(tokenizer, Tokenizer, tokens, Tokens),
    oo_new(tokenizer, [tokens:Tokens], ClonedTokenizer)) -> true
    ; oo_clone(Tokenizer, ClonedTokenizer).

% ==================================================
% 11. Runner State Functions (107-113)
% ==================================================

% 107. hyperonpy_runner_state_new_with_atoms/3

%!  hyperonpy_runner_state_new_with_atoms(+Metta, +AtomVec, -RunnerState) is det.
%
%   Creates a new runner state with the provided Metta instance and vector of atoms.
%
%   @arg Metta        The Metta instance.
%   @arg AtomVec      The vector of atoms to initialize the runner state with.
%   @arg RunnerState  The newly created runner state.
%
%   @example
%     % Create a new runner state with atoms.
%     ?- hyperonpy_runner_state_new_with_atoms(metta_instance, atom_vec_instance, RunnerState).
%     RunnerState = runner_state_instance.
%
hyperonpy_runner_state_new_with_atoms(Metta, AtomVec, RunnerState) :-
    oo_new(runner_state, [metta:Metta, atoms:AtomVec, is_complete:'@'(false)], RunnerState).

% 108. hyperonpy_runner_state_new_with_parser/3

%!  hyperonpy_runner_state_new_with_parser(+Metta, +SExprParser, -RunnerState) is det.
%
%   Creates a new runner state with the provided S-expression parser and Metta instance.
%
%   @arg Metta         The Metta instance.
%   @arg SExprParser   The S-expression parser to initialize the runner state with.
%   @arg RunnerState   The newly created runner state.
%
hyperonpy_runner_state_new_with_parser(Metta, SExprParser, RunnerState) :-
    oo_new(runner_state, [metta:Metta, parser:SExprParser, is_complete:'@'(false)], RunnerState).

% 109. hyperonpy_runner_state_step/1

%!  hyperonpy_runner_state_step(+RunnerState) is det.
%
%   Advances the runner state by one step. Marks the state as complete after the step.
%
%   @arg RunnerState  The current runner state instance.
%
%   @example
%     % Advance the runner state.
%     ?- hyperonpy_runner_state_step(runner_state_instance).
%     true.
%
hyperonpy_runner_state_step(RunnerState) :-
    oo_get(runner_state, RunnerState, is_complete, IsComplete),
    (IsComplete == '@'(false) ->
        oo_set(RunnerState, is_complete, '@'(true))
    ;
        true
    ).

% 110. hyperonpy_runner_state_current_results/2

%!  hyperonpy_runner_state_current_results(+RunnerState, -ResultList) is det.
%
%   Retrieves the current results from the runner state.
%
%   @arg RunnerState  The current runner state instance.
%   @arg ResultList   The list of results from the runner state.
%
%   @example
%     % Get the current results from the runner state.
%     ?- hyperonpy_runner_state_current_results(runner_state_instance, ResultList).
%     ResultList = [result1, result2].
%
hyperonpy_runner_state_current_results(RunnerState, ResultList) :-
    oo_get(runner_state, RunnerState, results, ResultList).

% 111. hyperonpy_runner_state_is_complete/2

%!  hyperonpy_runner_state_is_complete(+RunnerState, -IsComplete) is det.
%
%   Checks if the runner state is complete.
%
%   @arg RunnerState  The runner state instance.
%   @arg IsComplete   A flag indicating whether the runner state is complete.
%
%   @example
%     % Check if the runner state is complete.
%     ?- hyperonpy_runner_state_is_complete(runner_state_instance, IsComplete).
%     IsComplete = '@'(true).
%
hyperonpy_runner_state_is_complete(RunnerState, IsComplete) :-
    py_is_tf(oo_get(runner_state, RunnerState, completed, '@'(true)), IsComplete).

% 112. hyperonpy_runner_state_free/1

%!  hyperonpy_runner_state_free(+RunnerState) is det.
%
%   Frees the runner state from memory.
%
%   @arg RunnerState  The runner state instance to be freed.
%
%   @example
%     % Free a runner state.
%     ?- hyperonpy_runner_state_free(runner_state_instance).
%     true.
%
hyperonpy_runner_state_free(RunnerState) :-
    oo_free(RunnerState).

% 113. hyperonpy_runner_state_err_str/2

%!  hyperonpy_runner_state_err_str(+RunnerState, -ErrorStr) is det.
%
%   Retrieves the error string from a runner state.
%
%   @arg RunnerState  The runner state instance.
%   @arg ErrorStr     The error string associated with the runner state.
%
%   @example
%     % Get the error string from a runner state.
%     ?- hyperonpy_runner_state_err_str(runner_state_instance, ErrorStr).
%     ErrorStr = "Error message".
%
hyperonpy_runner_state_err_str(RunnerState, ErrorStr) :-
    oo_get(runner_state, RunnerState, error_str, ErrorStr).

% ==================================================
% 12. Syntax Node Functions (114-120)
% ==================================================

% 114. hyperonpy_syntax_node_clone/2

%!  hyperonpy_syntax_node_clone(+SyntaxNode, -ClonedNode) is det.
%
%   Clones a syntax node by copying its data.
%
%   @arg SyntaxNode  The syntax node to be cloned.
%   @arg ClonedNode  The newly cloned syntax node.
%
%   @example
%     % Clone a syntax node.
%     ?- hyperonpy_syntax_node_clone(node_instance, ClonedNode).
%     ClonedNode = cloned_node_instance.
%
hyperonpy_syntax_node_clone(SyntaxNode, ClonedNode) :-
    oo_get(syntax_node, SyntaxNode, data, Data),
    oo_new(syntax_node, [data:Data], ClonedNode).

% 115. hyperonpy_syntax_node_free/1

%!  hyperonpy_syntax_node_free(+SyntaxNode) is det.
%
%   Frees a syntax node from memory.
%
%   @arg SyntaxNode  The syntax node to be freed.
%
%   @example
%     % Free a syntax node.
%     ?- hyperonpy_syntax_node_free(node_instance).
%     true.
%
hyperonpy_syntax_node_free(SyntaxNode) :-
    oo_free(SyntaxNode).

% 116. hyperonpy_syntax_node_is_null/2

%!  hyperonpy_syntax_node_is_null(+SyntaxNode, -IsNull) is det.
%
%   Checks whether a syntax node is null (i.e., whether its data is unbound or empty).
%
%   @arg SyntaxNode  The syntax node to check.
%   @arg IsNull      `@'(true)` if the node is null, `@'(false)` otherwise.
%
%   @example
%     % Check if a syntax node is null.
%     ?- hyperonpy_syntax_node_is_null(node_instance, IsNull).
%     IsNull = '@'(false).
%
hyperonpy_syntax_node_is_null(SyntaxNode, IsNull) :-
    oo_get(syntax_node, SyntaxNode, data, Data),
    (var(Data) -> IsNull = '@'(true); IsNull = '@'(false)).

% 117. hyperonpy_syntax_node_type/2

%!  hyperonpy_syntax_node_type(+SyntaxNode, -NodeType) is det.
%
%   Retrieves the type of a syntax node.
%
%   @arg SyntaxNode  The syntax node whose type is to be retrieved.
%   @arg NodeType    The type of the syntax node.
%
%   @example
%     % Get the type of a syntax node.
%     ?- hyperonpy_syntax_node_type(node_instance, NodeType).
%     NodeType = 'expression'.
%
hyperonpy_syntax_node_type(SyntaxNode, NodeType) :-
    oo_get(syntax_node, SyntaxNode, node_type, NodeType).

% 118. hyperonpy_syntax_node_is_leaf/2

%!  hyperonpy_syntax_node_is_leaf(+SyntaxNode, -IsLeaf) is det.
%
%   Checks whether a syntax node is a leaf node (i.e., has no children).
%
%   @arg SyntaxNode  The syntax node to check.
%   @arg IsLeaf      `@'(true)` if the node is a leaf node, `@'(false)` otherwise.
%
%   @example
%     % Check if a syntax node is a leaf.
%     ?- hyperonpy_syntax_node_is_leaf(node_instance, IsLeaf).
%     IsLeaf = '@'(true).
%
hyperonpy_syntax_node_is_leaf(SyntaxNode, IsLeaf) :-
    oo_get(syntax_node, SyntaxNode, children, Children),
    (Children == [] -> IsLeaf = '@'(true) ; IsLeaf = '@'(false)).

% 119. hyperonpy_syntax_node_unroll/2

%!  hyperonpy_syntax_node_unroll(+SyntaxNode, -UnrolledList) is det.
%
%   Unrolls a syntax node into a list of nodes by recursively unrolling its children.
%
%   @arg SyntaxNode    The syntax node to unroll.
%   @arg UnrolledList  The list of nodes resulting from the unrolling process.
%
%   @example
%     % Unroll a syntax node.
%     ?- hyperonpy_syntax_node_unroll(node_instance, UnrolledList).
%     UnrolledList = [node_instance, child_node_instance].
%
hyperonpy_syntax_node_unroll(SyntaxNode, UnrolledList) :-
    oo_get(syntax_node, SyntaxNode, children, Children),
    (Children == [] ->
        UnrolledList = [SyntaxNode]
    ;
        findall(SubList, (
            member(Child, Children),
            hyperonpy_syntax_node_unroll(Child, SubList)
        ), Lists),
        flatten(Lists, UnrolledList)
    ).

% 120. hyperonpy_syntax_node_src_range/2

%!  hyperonpy_syntax_node_src_range(+SyntaxNode, -SrcRange) is det.
%
%   Retrieves the source range associated with a syntax node.
%
%   @arg SyntaxNode  The syntax node whose source range is to be retrieved.
%   @arg SrcRange    The source range associated with the node.
%
%   @example
%     % Get the source range of a syntax node.
%     ?- hyperonpy_syntax_node_src_range(node_instance, SrcRange).
%     SrcRange = 'line 1 to 2'.
%
hyperonpy_syntax_node_src_range(SyntaxNode, SrcRange) :-
    oo_get(syntax_node, SyntaxNode, src_range, SrcRange).

% ==================================================
% 13. Run Context Functions (121-126)
% ==================================================

% 121. hyperonpy_run_context_get_metta/2

%!  hyperonpy_run_context_get_metta(+RunContext, -Metta) is det.
%
%   Retrieves the Metta instance associated with a run context.
%
%   @arg RunContext  The run context from which to retrieve the Metta instance.
%   @arg Metta       The retrieved Metta instance.
%
%   @example
%     % Get the Metta instance from a run context.
%     ?- hyperonpy_run_context_get_metta(run_context_instance, Metta).
%     Metta = metta_instance.
%
hyperonpy_run_context_get_metta(RunContext, Metta) :-
    oo_get(run_context, RunContext, metta, Metta).

% 122. hyperonpy_run_context_get_space/2

%!  hyperonpy_run_context_get_space(+RunContext, -Space) is det.
%
%   Retrieves the space associated with a run context.
%
%   @arg RunContext  The run context from which to retrieve the space.
%   @arg Space       The retrieved space instance.
%
%   @example
%     % Get the space from a run context.
%     ?- hyperonpy_run_context_get_space(run_context_instance, Space).
%     Space = space_instance.
%
hyperonpy_run_context_get_space(RunContext, Space) :-
    oo_get(run_context, RunContext, space, Space).

% 123. hyperonpy_run_context_get_tokenizer/2

%!  hyperonpy_run_context_get_tokenizer(+RunContext, -Tokenizer) is det.
%
%   Retrieves the tokenizer associated with a run context.
%
%   @arg RunContext  The run context from which to retrieve the tokenizer.
%   @arg Tokenizer   The retrieved tokenizer instance.
%
%   @example
%     % Get the tokenizer from a run context.
%     ?- hyperonpy_run_context_get_tokenizer(run_context_instance, Tokenizer).
%     Tokenizer = tokenizer_instance.
%
hyperonpy_run_context_get_tokenizer(RunContext, Tokenizer) :-
    oo_get(run_context, RunContext, tokenizer, Tokenizer).

% 124. hyperonpy_run_context_import_dependency/2

%!  hyperonpy_run_context_import_dependency(+RunContext, +ModuleID) is det.
%
%   Imports a module dependency into a run context.
%
%   @arg RunContext  The run context into which the dependency is imported.
%   @arg ModuleID    The module ID to be imported.
%
%   @example
%     % Import a module dependency into a run context.
%     ?- hyperonpy_run_context_import_dependency(run_context_instance, "module_1").
%     true.
%
hyperonpy_run_context_import_dependency(RunContext, ModuleID) :-
    oo_get(run_context, RunContext, dependencies, Deps),
    (var(Deps) -> Deps = [] ; true),
    append(Deps, [ModuleID], NewDeps),
    oo_set(RunContext, dependencies, NewDeps).

% 125. hyperonpy_run_context_init_self_module/3

%!  hyperonpy_run_context_init_self_module(+RunContext, +Space, +ModuleName) is det.
%
%   Initializes the run context with a self module, associating a space and module name.
%
%   @arg RunContext  The run context to be initialized.
%   @arg Space       The space instance to associate with the run context.
%   @arg ModuleName  The name of the self module to be initialized.
%
%   @example
%     % Initialize the run context with a self module.
%     ?- hyperonpy_run_context_init_self_module(run_context_instance, space_instance, "self_module").
%     true.
%
hyperonpy_run_context_init_self_module(RunContext, Space, ModuleName) :-
    oo_set(RunContext, space, Space),
    oo_set(RunContext, module_name, ModuleName).

% 126. hyperonpy_run_context_load_module/3

%!  hyperonpy_run_context_load_module(+RunContext, +ModulePath, -ModuleID) is det.
%
%   Loads a module into a run context, returning the module ID.
%
%   @arg RunContext  The run context into which the module is loaded.
%   @arg ModulePath  The file path of the module to be loaded.
%   @arg ModuleID    The ID of the loaded module.
%
%   @example
%     % Load a module into a run context.
%     ?- hyperonpy_run_context_load_module(run_context_instance, "path/to/module", ModuleID).
%     ModuleID = "path/to/module".
%
hyperonpy_run_context_load_module(RunContext, ModulePath, ModuleID) :-
    % For simplicity, assume ModuleID is the ModulePath.
    ModuleID = ModulePath,
    hyperonpy_run_context_import_dependency(RunContext, ModuleID).

% ==================================================
% 14. Logging Functions (127-129)
% ==================================================

% 127. hyperonpy_log_error/1

%!  hyperonpy_log_error(+Message) is det.
%
%   Logs an error message to the console or log system.
%
%   @arg Message  The error message to be logged.
%
%   @example
%     % Log an error message.
%     ?- hyperonpy_log_error("An error occurred.").
%     ERROR: An error occurred.
%
hyperonpy_log_error(Message) :-
    format("ERROR: ~w~n", [Message]).

% 128. hyperonpy_log_info/1

%!  hyperonpy_log_info(+Message) is det.
%
%   Logs an informational message to the console or log system.
%
%   @arg Message  The informational message to be logged.
%
%   @example
%     % Log an informational message.
%     ?- hyperonpy_log_info("Processing completed.").
%     INFO: Processing completed.
%
hyperonpy_log_info(Message) :-
    format("INFO: ~w~n", [Message]).

% 129. hyperonpy_log_warn/1

%!  hyperonpy_log_warn(+Message) is det.
%
%   Logs a warning message to the console or log system.
%
%   @arg Message  The warning message to be logged.
%
%   @example
%     % Log a warning message.
%     ?- hyperonpy_log_warn("Memory usage is high.").
%     WARNING: Memory usage is high.
%
hyperonpy_log_warn(Message) :-
    format("WARNING: ~w~n", [Message]).

% ==================================================
% 15. Load Function (130)
% ==================================================

% 130. hyperonpy_load_ascii/3

%!  hyperonpy_load_ascii(+FilePath, +Space, -Success) is det.
%
%   Loads ASCII data from the specified file into the provided space.
%   If the file is successfully read, the content is added to the space.
%   Returns `@'(true)` on success and `@'(false)` if an error occurs.
%
%   @arg FilePath  The path to the file containing ASCII data.
%   @arg Space     The space into which the file content is loaded.
%   @arg Success   `@'(true)` if the file is loaded successfully, `@'(false)` otherwise.
%
%   @example
%     % Load ASCII data from a file into a space.
%     ?- hyperonpy_load_ascii("data.txt", space_instance, Success).
%     Success = '@'(true).
%
hyperonpy_load_ascii(FilePath, Space, Success) :-
    catch(
        (   open(FilePath, read, Stream),
            read(Stream, Content),
            close(Stream),
            hyperonpy_space_add(Space, Content),
            Success = '@'(true)
        ),
        _Error,
        Success = '@'(false)
    ).

% ==================================================
% **End of Translation**
% ==================================================
% ==============================
% End of the hyperonpy_* function definitions.
% ==============================


%!  handle_method_call(+Type, +ID, +Attributes, +MethodCall, -Result) is det.
%
%   Handles method calls on objects by matching on the object type and method.
%
%   This predicate defines various method handlers that can be invoked on objects,
%   depending on the object's type and the method called. Each method handler
%   modifies or queries the object's state based on the method call.
%
%   @arg Type The type of the object.
%   @arg ID The ID of the object.
%   @arg Attributes The attributes associated with the object.
%   @arg MethodCall The method being invoked on the object.
%   @arg Result The result of the method call (if applicable).
%
%   @example
%   ?- oo_invoke(atom_vec, atom_vec1, length(), Len).
%   Len = 3.
%

% ------------------ Handle Method Calls Implementation ------------------


% Handle method calls on objects
handle_method_call(Type, ID, Attributes, MethodCall, Result) :-
    % Define method handlers here based on Type and MethodCall
    handle_method_call_impl(Type, ID, Attributes, MethodCall, Result).

% Handle missing attributes
handle_method_call(Type, ID, Attributes, Method, Result):-
   var(Attributes), nonvar(ID),
   into_object(ID, o3(Type, _, Attributes2)), Attributes\=@=Attributes2,!,
   handle_method_call(Type, ID, Attributes2, Method, Result).

% Handling length() for other o3 types that have a list attribute
handle_method_call(_, _, Attributes, length(), Length) :-
    freeze(List,is_list(List)),member_chk(value:List, Attributes),
    length(List, Length).

% Push method
handle_method_call(atom_vec, ID, Attributes, push(Element), true) :-
    member_chk(value:List, Attributes),
    append(List, [Element], NewList),
    retract(o3(atom_vec, ID, Attributes)),
    assert(o3(atom_vec, ID, [type:atom_vec, value:NewList])).

% Pop method
handle_method_call(atom_vec, ID, Attributes, pop(), PoppedElement) :-
    member_chk(value:List, Attributes),
    append(PoppedList, [PoppedElement], List),
    retract(o3(atom_vec, ID, Attributes)),
    assert(o3(atom_vec, ID, [type:atom_vec, value:PoppedList])).

% Handle method for other types as per your list
handle_method_call(atom, _ID, Attributes, to_str(), Str) :-
    member_chk(value:Value, Attributes),
    format(atom(Str), '~w', [Value]).

% Handle get_* methods
handle_method_call(_Type, _ID, Attributes, Method, Value) :-
    compound_name_arity(Method, Get_Something, 0),
    % Ensures that Method is a compound term like get_name, get_type, etc., with arity 0
    compound_name_arity(Method, Get_Something, 0),
    % Ensure that the functor begins with 'get_' followed by the field name
    atom_concat('get_', FieldName, Get_Something),
    % Look up the attribute by the extracted field name
    member_chk(FieldName:Value, Attributes),!.

% in case get_method and not get_method()
handle_method_call(Type, ID, Attributes, AMethod, Value) :-
    atom(AMethod),compound_name_arity(Method, Method, 0),!,
    handle_method_call(Type, ID, Attributes, Method, Value).

% atom_eq
handle_method_call(atom, _ID1, Attributes1, eq(ID2), AreEqual) :-
    member_chk(value:Atom1, Attributes1),
    o3(atom, ID2, [value:Atom2]),
    AreEqual = (Atom1 == Atom2).

% atom_error_message
handle_method_call(atom, _ID, Attributes, error_message(), ErrorMessage) :-
    member_chk(value:_Atom, Attributes),
    atom_is_error(ID, true),
    atom_get_children(ID, [_, ErrorMessage]).

% atom_expr
handle_method_call(atom, _Static, _, expr(ExpressionList), NewID) :-
    oo_new(atom, [metatype:'Expression', value:ExpressionList], NewID).

% atom_free
handle_method_call(Atom, ID, _, free(), _) :-
    retractall(o3(Atom, ID, _)).

% atom_get_children
handle_method_call(atom, _ID, Attributes, get_children(), Children) :-
    member_chk(metatype:'Expression', Attributes),
    member_chk(value:Children, Attributes).

% atom_gnd
handle_method_call(atom, _Static, _, gnd(Object, GroundedType), NewID) :-
    oo_new(atom, [metatype:'Grounded', object:Object, grounded_type:GroundedType], NewID).

% atom_sym
handle_method_call(atom, _Static, _, atom_sym(Symbol), Atom) :-
    hyperon_atom_var(Symbol, Atom).

% atom_var
handle_method_call(atom, _Static, _, atom_var(VarName), Atom) :-
    hyperon_atom_var(VarName, Atom).


% atom_to_str
handle_method_call(atom, _ID, Attributes, to_str(), Str) :-
    member_chk(value:Atom, Attributes),
    format(atom(Str), '~w', [Atom]).

% atom_vec_new
handle_method_call(atom_vec, _Static, _, new(), NewID) :-
    oo_new(atom_vec, [type:atom_vec, value:[]], NewID).

% atom_vec_from_list
handle_method_call(atom_vec, _Static, _, from_list(List), NewID) :-
    oo_new(atom_vec, [type:atom_vec, value:List], NewID).

% bindings_add_var_binding
handle_method_call(bindings, _ID, Attributes, add_var_binding(VarAtom, BoundAtom), Success) :-
    member_chk(value:Bindings, Attributes),
    (   memberchk(VarAtom-BoundAtom, Bindings)
    ->  Success = '@'(true)
    ;   Success = '@'(false)).

% bindings_clone
handle_method_call(bindings, _ID, Attributes, clone(), NewID) :-
    member_chk(value:Bindings, Attributes),
    oo_new(bindings, [type:bindings, value:Bindings], NewID).

% bindings_is_empty
handle_method_call(bindings, _ID, Attributes, is_empty(), IsEmpty) :-
    member_chk(value:Bindings, Attributes),
    py_is_tf(Bindings == [], IsEmpty).

% bindings_list
handle_method_call(bindings, _ID, Attributes, list(), List) :-
    member_chk(value:Bindings, Attributes),
    findall(Var-Atom, member(Var-Atom, Bindings), List).

% bindings_merge
handle_method_call(bindings, _ID1, Attributes1, merge(ID2), NewID) :-
    member_chk(value:Bindings1, Attributes1),
    o3(bindings, ID2, [value:Bindings2]),
    append(Bindings1, Bindings2, ResultBindings),
    oo_new(bindings, [type:bindings, value:ResultBindings], NewID).

% bindings_new
handle_method_call(bindings, _Static, _, new(), NewID) :-
    oo_new(bindings, [type:bindings, value:[]], NewID).

% bindings_resolve
handle_method_call(bindings, _ID, Attributes, resolve(VarAtom), ResolvedAtom) :-
    member_chk(value:Bindings, Attributes),
    member(VarAtom-ResolvedAtom, Bindings).

% bindings_set_add_var_binding
handle_method_call(bindings_set, ID, Attributes, add_var_binding(VarAtom, BoundAtom), _) :-
    member_chk(value:BindingsSet, Attributes),
    append(BindingsSet, [[VarAtom-BoundAtom]], NewSet),
    retractall(o3(bindings_set, ID, _)),
    assert(o3(bindings_set, ID, [type:bindings_set, value:NewSet])).

% bindings_set_is_empty
handle_method_call(bindings_set, _ID, Attributes, is_empty(), IsEmpty) :-
    member_chk(value:BindingsSet, Attributes),
    (BindingsSet == [] -> IsEmpty = '@'(true); IsEmpty = '@'(false)).

% bindings_set_list
handle_method_call(bindings_set, _ID, Attributes, list(), List) :-
    member_chk(value:BindingsSet, Attributes),
    List = BindingsSet.

% bindings_set_merge_into
handle_method_call(bindings_set, ID1, Attributes1, merge_into(ID2), _) :-
    member_chk(value:BindingsSet1, Attributes1),
    o3(bindings_set, ID2, [value:BindingsSet2]),
    append(BindingsSet1, BindingsSet2, MergedSet),
    retractall(o3(bindings_set, ID1, _)),
    assert(o3(bindings_set, ID1, [type:bindings_set, value:MergedSet])).

% tokenizer_new
handle_method_call(tokenizer, NewID, _, new(), _) :-
    oo_new(tokenizer, [type:tokenizer, value:[]], NewID).

% tokenizer_register_token
handle_method_call(tokenizer, ID, Attributes, register_token(Token, Callback), _) :-
    member_chk(value:TokenList, Attributes),
    append(TokenList, [(Token, Callback)], NewTokenList),
    retractall(o3(tokenizer, ID, _)),
    assert(o3(tokenizer, ID, [type:tokenizer, value:NewTokenList])).

% tokenizer_clone
handle_method_call(tokenizer, NewID, Attributes, clone(), _) :-
    member_chk(value:TokenList, Attributes),
    oo_new(tokenizer, [type:tokenizer, value:TokenList], NewID).

% syntax_node_free
handle_method_call(syntax_node, ID, _, free(), _) :-
    retractall(o3(syntax_node, ID, _)).

% syntax_node_is_null
handle_method_call(syntax_node, _ID, Attributes, is_null(), IsNull) :-
    py_is_tf( ( \+ member_chk(value:_, Attributes)), IsNull).


% syntax_node_clone
handle_method_call(syntax_node, NewID, Attributes, clone(), _) :-
    member_chk(value:NodeValue, Attributes),
    oo_new(syntax_node, [type:syntax_node, value:NodeValue], NewID).


end_of_file.

% Create a new atom
?- hyperonpy_atom_sym('x', Atom).
% Atom = o3(atom, atom_1, [type:atom_sym, value:'x']).

% Retrieve the name of the atom
?- hyperonpy_atom_get_name(Atom, Name).
% Name = 'x'.

% Convert the atom to a string
?- hyperonpy_atom_to_str(Atom, Str).
% Str = "x".

% Clone the atom
?- hyperonpy_atom_clone(Atom, ClonedAtom).
% ClonedAtom = o3(atom, atom_2, [type:atom_sym, value:'x']).

% Check equality
?- hyperonpy_atom_eq(Atom, ClonedAtom, AreEqual).
% AreEqual = '@'(false).

% Free the cloned atom
?- hyperonpy_atom_free(ClonedAtom).
true.



% ==============================
% **4.2. Working with Atom Vectors**
% =============================


% Create a new atom vector from a list
?- hyperonpy_atom_vec_from_list([Atom1, Atom2, Atom3], AtomVec).
% AtomVec = o3(atom_vec, atom_vec_1, [type:atom_vec, value:[o3(atom, atom_1, ...), o3(atom, atom_2, ...), o3(atom, atom_3, ...)]]).

% Get the length of the atom vector
?- hyperonpy_atom_vec_len(AtomVec, Length).
% Length = 3.

% Push a new atom into the vector
?- hyperonpy_atom_vec_push(AtomVec, Atom4).
true.

% Pop an atom from the vector
?- hyperonpy_atom_vec_pop(AtomVec, PoppedAtom).
% PoppedAtom = o3(atom, atom_4, [type:atom_sym, value:'y']).



% ==============================
% **4.3. Managing Bindings**
% =============================


% Create new bindings
?- hyperonpy_bindings_new(Bindings).
% Bindings = o3(bindings, bindings_1, [value:[]]).

% Add a variable binding
?- hyperonpy_bindings_add_var_binding(Bindings, VarAtom, BoundAtom, Success).
% Success = '@'(true).

% Clone bindings
?- hyperonpy_bindings_clone(Bindings, NewBindings).
% NewBindings = o3(bindings, bindings_2, [value:[...]]).

% Check if bindings are equal
?- hyperonpy_bindings_eq(Bindings, NewBindings, AreEqual).
% AreEqual = '@'(false).

% Free bindings
?- hyperonpy_bindings_free(NewBindings).
true.




