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
% PROGRAM FUNCTION: provides debugging utilities, environment management, and option handling
% specifically tailored for SWI-Prolog.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%   These configurations help ensure a controlled and silent environment
%
% Prevents messages about predicates being autoloaded.
:- set_prolog_flag(verbose_autoload, false).
% Suppresses general verbose output during program execution.
:- set_prolog_flag(verbose, silent).
% Suppresses messages during the loading of files.
:- set_prolog_flag(verbose_load, silent).
% Fails any attempt to export operators from the user module.
:- assert((user:'$exported_op'(_,_,_):- fail)).
% Removes the existing export predicate from the system module.
:- abolish((system:'$exported_op'/3)).
% Prevents further operator exports from the system module by always failing.
:- assert((system:'$exported_op'(_,_,_):- fail)).

%!  fbug(+Term) is det.
%
%   A versatile debugging predicate that handles various types of terms,
%   printing relevant debugging information to the `user_error` stream.
%   It supports named-value pairs, compound terms, and arbitrary terms.
%
%   @arg Term A Prolog term to be debugged.
%   @example
%     % Debugging a named-value pair:
%     ?- fbug(x = 42).
%     /* x :- 42. */
%
%     % Debugging a compound term:
%     ?- fbug(test(foo, bar)).
%     /* test :- test(foo, bar). */
%
fbug(_) :-
    % if compatibility mode is enabled.
    is_compatio, !.
fbug(_) :-
    \+ is_debugging(fbug),!.
fbug(P) :-
    % Write output to `user_error` if `write_src/1` exists.
    format("~N"), current_predicate(write_src/1),
    with_output_to(user_error, in_cmt(write_src(P))), !.
fbug(N = V) :-
    % Debug a named-value pair.
    nonvar(N), !, fbdebug1(N :- V).
fbug(V) :-
    % Debug a compound term by extracting the functor.
    compound(V), functor(V, F, _A), !, fbdebug1(F :- V).
fbug(V) :-
    % Default case for arbitrary terms.
    fbdebug1(debug :- V).

%!  fbdebug1(+Message) is det.
%
%   Outputs a debugging message to `user_error` with error handling.
%   If `portray_clause/3` fails, it falls back to formatted output.
%
%   @arg Message A structured Prolog term representing a debugging message.
%
fbdebug1(Message) :-
    % Flush output buffers (ISO standard).
    flush_output(user_output),
    flush_output(user_error),
    % Attempt to print the message, falling back to formatted output on failure.
    catch(portray_clause(user_error, Message, []), _,
          catch_ignore(format(user_error, "~n/* ~q. */~n", [Message]))),
    %format(user_error, "~n/* ~p. */~n", [Message]),  % Preserved commented-out code.
    flush_output(user_error).

%!  swi_only(:Goal) is nondet.
%
%   Executes the given goal only if running on SWI-Prolog. If the current
%   environment is Scryer-Prolog, the predicate fails immediately.
%
%   @arg Goal The goal to be executed, provided the environment is SWI-Prolog.
%
swi_only(_) :-
    % Fail if running on Scryer-Prolog.
    is_scryer, !, fail.
swi_only(G) :-
    % Execute the goal in SWI-Prolog.
    call(G).

%!  is_scryer is nondet.
%
%   Succeeds if the current Prolog environment is Scryer-Prolog.
%
is_scryer :-
    % Check if the 'libswipl' flag is not set, indicating Scryer-Prolog.
    \+ current_prolog_flag(libswipl, _).

% Creates the `max_per_file` flag with an initial value of `inf`.
:- create_prolog_flag(max_per_file, inf, [keep(true), access(read_write), type(term)]).
% Creates the `max_disk_cache` flag with an initial value of `inf`.
:- create_prolog_flag(max_disk_cache, inf, [keep(true), access(read_write), type(term)]).
% Creates the `samples_per_million` flag with an initial value of `inf`.
:- create_prolog_flag(samples_per_million, inf, [keep(true), access(read_write), type(term)]).

%!  with_cwd(+Directory, :Goal) is det.
%
%   Executes the given Goal within the specified working directory.
%   If the directory is `'.'`, it uses the current working directory.
%   If the directory is a variable, it binds to the current directory.
%   If the directory does not exist, it throws an exception.
%   Uses `setup_call_cleanup/3` to ensure the working directory is
%   restored after the Goal completes.
%
%   @arg Directory The directory in which to execute the goal. If `'.'`,
%        the current directory is used.
%   @arg Goal The goal to execute within the directory context.
%   @example
%     % Execute a goal in a valid directory (e.g., /tmp):
%     ?- with_cwd('/tmp', writeln('Hello from /tmp')).
%     Hello from /tmp
%
with_cwd(Dir, Goal) :-
    % Execute the goal in the current directory if Dir is '.'.
    Dir == '.',!,setup_call_cleanup(working_directory(X, X), Goal, working_directory(_, X)).
with_cwd(Dir, Goal) :-
    % Bind Dir to the current directory if it is a variable.
    var(Dir),X = Dir,!,setup_call_cleanup(working_directory(X, X), Goal, working_directory(_, X)).
with_cwd(Dir, Goal) :-
    % Throw an exception if the directory does not exist.
    \+ exists_directory(Dir),!,throw(with_cwd(Dir, Goal)), !.
with_cwd(Dir, Goal) :-
    % Execute the goal in the given directory, restoring the original afterward.
    setup_call_cleanup(working_directory(X, Dir), Goal, working_directory(_, X)).

%!  with_option(+Options, :Goal) is det.
%
%   Executes the given Goal with specified temporary options. Options can be
%   provided as a list, `Name=Value` pairs, or compound terms. After the goal
%   completes, the original options are restored.
%
%   @arg Options A list of options or a single option to apply temporarily.
%        Options can be provided as `Name=Value` pairs or compound terms.
%   @arg Goal The goal to execute with the specified options.
%   @example
%     % Execute a goal with a temporary flag value:
%     ?- with_option(samples_per_million=100, writeln('Option applied')).
%     Option applied
%
with_option([], G) :-
    % Call the goal when no options are provided.
    !, call(G).
with_option([H|T], G) :-
    % Apply multiple options recursively.
    !, with_option(H, with_option(T, G)).
with_option(N = V, G) :-
    % Apply a name-value option.
    !, with_option(N, V, G).
with_option(NV, G) :-
    % Decompose a compound option and apply it.
    compound(NV), NV =.. [N, V],!, with_option(N, V, G).
with_option(N, G) :-
    % Apply an option with a default value of `true`.
    with_option(N, true, G).
with_option(N, V, G) :-
    % Set the option value temporarily and restore it after the goal completes.
    (was_option_value(N, W) -> true ; W = []),
    setup_call_cleanup(set_option_value(N, V), G, set_option_value(N, W)).

%!  was_option_value(+Name, -Value) is nondet.
%
%   Retrieves the current value of an option, if it exists, using Prolog flags,
%   load context, or non-backtrackable variables.
%
%   @arg Name The name of the option to retrieve.
%   @arg Value The current value of the option, if it exists.
%   @example
%     % Retrieve the value of the `max_per_file` option.
%     ?- was_option_value(max_per_file, Value).
%     Value = inf.
%
was_option_value(N, V) :-
    % Check if the option is set as a Prolog flag.
    current_prolog_flag(N, VV), !, V = VV.
was_option_value(N, V) :-
    % Check if the option is available in the load context.
    prolog_load_context(N, VV), !, V = VV.
was_option_value(N, V) :-
    % Check if the option is stored as a non-backtrackable variable.
    nb_current(N, VV), VV \== [], !, V = VV.

%!  option_else(+Name, -Value, +Else) is det.
%
%   Retrieves the value of an option if it exists; otherwise, assigns the
%   fallback value `Else`. This predicate ensures that a meaningful default
%   value is used if the option is not available.
%
%   @arg Name The name of the option to retrieve.
%   @arg Value The value of the option, or the fallback value if not found.
%   @arg Else The fallback value to use if the option is not available.
%   @example
%     % Retrieve the value of an existing option or a fallback.
%     ?- option_else(samples_per_million, Value, 100).
%     Value = inf.
%
%     % Retrieve the value of a non-existent option with a fallback.
%     ?- option_else(non_existent_option, Value, 'default').
%     Value = 'default'.
%
option_else(N, V, Else) :-
    % Retrieve the value or assign the fallback using `option_else0/3`.
    notrace((option_else0(N, VV, Else), p2mE(VV, V))).

%!  option_else0(+Name, -Value, +Else) is det.
%
%   Helper predicate for `option_else/3`. It attempts to retrieve the value
%   of the specified option using `was_option_value/2`. If the option is not
%   available, it unifies the value with the given fallback.
%
%   @arg Name The name of the option to retrieve.
%   @arg Value The value of the option if found, otherwise the fallback value.
%   @arg Else The fallback value to use if the option is not available.
%   @example
%     % Use the fallback when the option is unavailable.
%     ?- option_else0(non_existent, Value, 'fallback').
%     Value = 'fallback'.
%
option_else0(N, V, _Else) :-
    % Get the option value if available.
    was_option_value(N, VV), !, VV = V.
option_else0(_N, V, Else) :-
    % Use the fallback value if the option is unavailable.
    !, V = Else.

%!  option_value(+Name, -Value) is nondet.
%
%   Retrieves the current value of an option, with special handling for boolean
%   values (`true` or `false`).
%
%   @arg Name The name of the option to retrieve.
%   @arg Value The value of the option.
%   @example
%     % Retrieve the value of the `samples_per_million` option.
%     ?- option_value(samples_per_million, Value).
%     Value = inf.
%
option_value(N, V) :-
    % Retrieve the option value if V is unbound.
    var(V), !,was_option_value(N, VV),once((p2mE(VV, V2), p2mE(V, V1))), V1 = V2.
option_value(N, V) :-
    % Handle boolean value `true`.
    V == true, option_value0(N, 'True'), !.
option_value(N, V) :-
    % Handle boolean value `false`.
    V == false, option_value0(N, 'False'), !.
option_value(N, V) :-
    % Retrieve the option value without tracing.
    notrace(option_value0(N, V)).

%!  option_value0(+Name, -Value) is nondet.
%
%   Retrieves the raw option value or returns an empty list if unavailable.
%
%   @arg Name The name of the option to retrieve.
%   @arg Value The value of the option or an empty list if not found.
%   @example
%     % Retrieve the raw value of an option or an empty list.
%     ?- option_value0(max_disk_cache, Value).
%     Value = inf.
%
option_value0(N, V) :-
    % Retrieve and convert the option value.
    was_option_value(N, VV),once((p2mE(VV, V2), p2mE(V, V1))), V1 = V2.
option_value0(_N, []).

%!  p2mE(+Input, -Output) is det.
%
%   Converts boolean and other values into specific internal representations.
%   This predicate ensures that `true` and `false` are mapped to string values
%   `'True'` and `'False'`, respectively, and leaves other values unchanged.
%
%   @arg Input The input value to convert.
%   @arg Output The converted value.
%   @example
%     % Convert true to 'True'.
%     ?- p2mE(true, Result).
%     Result = 'True'.
%
p2mE(NA, NA) :-
    % If the input is not an atom, leave it unchanged.
    \+ atom(NA), !.
p2mE(false, 'False').  % Convert false to 'False'.
p2mE(true, 'True').    % Convert true to 'True'.
p2mE(E, N):- atom(E), atom_number(E, NN),!,NN=N.
p2mE(E, E).            % Leave other values unchanged.

%!  set_option_value(+Name, +Value) is det.
%
%   Sets an option value using non-backtrackable storage and Prolog flags.
%   Handles internal conversions and ensures the flag is created or updated.
%
%   @arg Name The name of the option to set.
%   @arg Value The value to set for the option.
%   @example
%     % Set the value of the option 'my_option' to true.
%     ?- set_option_value(my_option, true).
%
set_option_value(N, V) :-
    % Call the internal predicate to set the option value.
    set_option_value0(N, V).

%!  set_option_value0(+Name, +Value) is det.
%
%   Internal helper for setting option values. Converts the value if needed,
%   stores it in non-backtrackable storage, creates a Prolog flag, and updates
%   the flag’s value, handling any errors that occur.
%
%   @arg Name The name of the option to set.
%   @arg Value The value to set for the option.
set_option_value0(N, V) :-
    % Convert the input value if necessary.
    p2mE(V, VV), !,
    % Store the value in non-backtrackable storage, handling errors.
    catch(nb_setval(N, VV), E, fbug(E)),
    % Prepare the value for Prolog flag creation.
    p2mE(PV, VV), !,
    % Create the Prolog flag, handling errors.
    catch(create_prolog_flag(N, PV, [keep(false), access(read_write), type(term)]), E2, fbug(E2)),
    % Set the value of the Prolog flag, handling errors.
    catch(set_prolog_flag(N, PV), E3, fbug(E3)), !.

%!  kaggle_arc is det.
%
%   Loads the Kaggle ARC module if the required directory exists.
%   If the directory is missing, the predicate succeeds without loading anything.
%
%   @example
%     % Load the Kaggle ARC module (if the directory exists):
%     ?- kaggle_arc.
%
kaggle_arc :-
    % Succeed if the Kaggle ARC directory does not exist.
    \+ exists_directory('/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/'), !.
%kaggle_arc :- !.
kaggle_arc :-
    % Load the Kaggle ARC module with specific options.
    with_option(argv, ['--libonly'],
        with_cwd('/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/',
            ensure_loaded(kaggle_arc))).

%:- ensure_loaded((read_obo2)).

%:- kaggle_arc.

%!  all_upper_symbol(+Symbol) is nondet.
%
%   Succeeds if the given Symbol is an uppercase atom.
%
%   @arg Symbol The atom to check.
%   @example
%     ?- all_upper_symbol('HELLO').
%     true.
%
all_upper_symbol(A) :- all_upper_atom(A).

%!  any_to_symbol(+Any, -Symbol) is det.
%
%   Converts any term to an atom.
%
%   @arg Any The input term to convert.
%   @arg Symbol The resulting atom.
%   @example
%     ?- any_to_symbol(42, A).
%     A = '42'.
%
any_to_symbol(A, B) :- any_to_atom(A, B).

%!  concat_symbol(+List, -Separator, -Result) is det.
%
%   Joins a list of atoms using a separator.
%
%   @arg List A list of atoms to join.
%   @arg Separator The separator between atoms.
%   @arg Result The concatenated atom.
%   @example
%     ?- concat_symbol(['hello', 'world'], ' ', R).
%     R = 'hello world'.
%
concat_symbol(A, B, C) :- concat_atom(A, B, C).

%!  downcase_symbol(+Symbol, -Lowercase) is det.
%
%   Converts an atom to lowercase.
%
%   @arg Symbol The input atom.
%   @arg Lowercase The lowercase atom.
%   @example
%     ?- downcase_symbol('HELLO', L).
%     L = 'hello'.
%
downcase_symbol(A, B) :- downcase_atom(A, B).

%!  non_empty_symbol(+Symbol) is nondet.
%
%   Succeeds if the given atom is non-empty.
%
%   @arg Symbol The atom to check.
%   @example
%     ?- non_empty_symbol('A').
%     true.
%
non_empty_symbol(A) :- non_empty_atom(A).

non_empty_atom(A):- \+ atom_codes(A,[]).

%!  string_to_symbol(+String, -Symbol) is det.
%
%   Converts a string to an atom.
%
%   @arg String The input string.
%   @arg Symbol The resulting atom.
%   @example
%     ?- string_to_symbol("hello", A).
%     A = 'hello'.
%
string_to_symbol(A, B) :- string_to_atom(A, B).

%!  sub_string_or_symbol(+Input, +Before, +Length, +After, -Substring) is nondet.
%
%   Extracts a substring from an atom or string.
%
%   @arg Input The input atom or string.
%   @arg Before The number of characters before the substring.
%   @arg Length The length of the substring.
%   @arg After The number of characters after the substring.
%   @arg Substring The resulting substring.
%   @example
%     ?- sub_string_or_symbol('Prolog', 0, 3, _, S).
%     S = 'Pro'.
%
sub_string_or_symbol(A, B, C, D, E) :- sub_string_or_atom(A, B, C, D, E).

%!  sub_symbol(+Atom, +Before, +Length, +After, -SubAtom) is nondet.
%
%   Extracts a sub-atom from an atom.
%
%   @arg Atom The input atom.
%   @arg Before The number of characters before the sub-atom.
%   @arg Length The length of the sub-atom.
%   @arg After The number of characters after the sub-atom.
%   @arg SubAtom The resulting sub-atom.
%   @example
%     ?- sub_symbol('Prolog', 0, 3, _, S).
%     S = 'Pro'.
%
sub_symbol(A, B, C, D, E) :- sub_atom(A, B, C, D, E).

%!  symbol(+Term) is nondet.
%
%   Succeeds if the given Term is an atom.
%
%   @arg Term The term to check.
%   @example
%     ?- symbol('hello').
%     true.
%
symbol(A) :- atom(A).

%!  symbol_chars(+Symbol, -Chars) is det.
%
%   Converts an atom to a list of characters.
%
%   @arg Symbol The input atom.
%   @arg Chars The resulting list of characters.
%   @example
%     ?- symbol_chars('hello', L).
%     L = ['h', 'e', 'l', 'l', 'o'].
%
symbol_chars(A, B) :- atom_chars(A, B).

%!  symbol_codes(+Symbol, -Codes) is det.
%
%   Converts an atom to a list of character codes.
%
%   @arg Symbol The input atom.
%   @arg Codes The resulting list of character codes.
%   @example
%     ?- symbol_codes('A', L).
%     L = [65].
%
symbol_codes(A, B) :- atom_codes(A, B).

%!  symbol_concat(+A, +B, -C) is det.
%
%   Concatenates two atoms.
%
%   @arg A The first atom.
%   @arg B The second atom.
%   @arg C The resulting concatenated atom.
%   @example
%     ?- symbol_concat('hello', 'world', Result).
%     Result = 'helloworld'.
%
symbol_concat(A, B, C) :- atom_concat(A, B, C).

%!  symbol_contains(+Symbol, +Substring) is nondet.
%
%   Succeeds if the Symbol contains the given Substring.
%
%   @arg Symbol The atom to search within.
%   @arg Substring The atom to search for.
%   @example
%     ?- symbol_contains('hello', 'ell').
%     true.
%
symbol_contains(A, B) :- atom_contains(A, B).

%!  symbol_length(+Symbol, -Length) is det.
%
%   Computes the length of an atom.
%
%   @arg Symbol The input atom.
%   @arg Length The length of the atom.
%   @example
%     ?- symbol_length('hello', L).
%     L = 5.
%
symbol_length(A, B) :- atom_length(A, B).

%!  symbol_number(+Symbol, -Number) is nondet.
%
%   Converts an atom to a number if possible.
%
%   @arg Symbol The input atom.
%   @arg Number The resulting number.
%   @example
%     ?- symbol_number('42', N).
%     N = 42.
%
symbol_number(A, B) :- atom_number(A, B).

%!  symbol_string(+Symbol, -String) is det.
%
%   Converts an atom to a string.
%
%   @arg Symbol The input atom.
%   @arg String The resulting string.
%   @example
%     ?- symbol_string('hello', S).
%     S = "hello".
%
symbol_string(A, B) :- atom_string(A, B).

%!  symbol_upper(+Symbol, -Upper) is det.
%
%   Converts an atom to uppercase.
%
%   @arg Symbol The input atom.
%   @arg Upper The uppercase version of the atom.
%   @example
%     ?- symbol_upper('hello', U).
%     U = 'HELLO'.
%
symbol_upper(A, B) :- upcase_atom(A, B).

%!  symbolic(+Term) is nondet.
%
%   Succeeds if the given Term is atomic (an atom or number).
%
%   @arg Term The term to check.
%   @example
%     ?- symbolic('hello').
%     true.
%
symbolic(A) :- atomic(A).

%!  symbolic_concat(+A, +B, -C) is det.
%
%   Concatenates two atomic terms.
%
%   @arg A The first atomic term.
%   @arg B The second atomic term.
%   @arg C The resulting concatenated atomic term.
%   @example
%     ?- symbolic_concat('hello', 'world', R).
%     R = 'helloworld'.
%
symbolic_concat(A, B, C) :- atomic_concat(A, B, C).

%!  symbolic_concat(+A, +B, +C, -Result) is det.
%
%   Concatenates three atomic terms.
%
%   @arg A The first atomic term.
%   @arg B The second atomic term.
%   @arg C The third atomic term.
%   @arg Result The resulting concatenated atomic term.
%   @example
%     ?- symbolic_concat('a', 'b', 'c', R).
%     R = 'abc'.
%
symbolic_concat(A, B, C, D) :- atomic_concat(A, B, C, D).

%!  symbolic_list_concat(+List, -Result) is det.
%
%   Concatenates a list of atomic terms into a single atomic term.
%
%   @arg List The list of atomic terms to concatenate.
%   @arg Result The resulting concatenated atomic term.
%   @example
%     ?- symbolic_list_concat(['a', 'b', 'c'], R).
%     R = 'abc'.
%
symbolic_list_concat(A, B) :- atomic_list_concat(A, B).

%!  symbolic_list_concat(+List, +Separator, -Result) is det.
%
%   Concatenates a list of atomic terms with a separator.
%
%   @arg List The list of atomic terms to concatenate.
%   @arg Separator The separator to use between terms.
%   @arg Result The resulting concatenated atomic term.
%   @example
%     ?- symbolic_list_concat(['a', 'b', 'c'], '-', R).
%     R = 'a-b-c'.
%
symbolic_list_concat(A, B, C) :- atomic_list_concat(A, B, C).

%!  symbolic_to_string(+Atomic, -String) is det.
%
%   Converts an atomic term to a string.
%
%   @arg Atomic The input atomic term.
%   @arg String The resulting string.
%   @example
%     ?- symbolic_to_string(123, S).
%     S = "123".
%
symbolic_to_string(A, B) :- atomic_to_string(A, B).

%!  symbolics_to_string(+List, -String) is det.
%
%   Converts a list of atomic terms to a string.
%
%   @arg List The list of atomic terms to convert.
%   @arg String The resulting string.
%   @example
%     ?- symbolics_to_string([1, 2, 3], S).
%     S = "123".
%
symbolics_to_string(A, B) :- atomics_to_string(A, B).

%!  symbolics_to_string(+List, +Separator, -String) is det.
%
%   Converts a list of atomic terms to a string with a separator.
%
%   @arg List The list of atomic terms.
%   @arg Separator The separator between terms.
%   @arg String The resulting string.
%   @example
%     ?- symbolics_to_string([1, 2, 3], '-', S).
%     S = "1-2-3".
%
symbolics_to_string(A, B, C) :- atomics_to_string(A, B, C).

%!  upcase_symbol(+Symbol, -Upper) is det.
%
%   Converts a symbol to uppercase.
%
%   @arg Symbol The input symbol.
%   @arg Upper The uppercase version of the symbol.
%   @example
%     ?- upcase_symbol('hello', U).
%     U = 'HELLO'.
%
upcase_symbol(A, B) :- upcase_atom(A, B).

% Asserts the directory of the FlyBase FTP data release if it exists.
:- prolog_load_context(directory, File),
   ignore((
     absolute_file_name('../../data/ftp.flybase.org/releases/current/', Dir,
                        [relative_to(File), file_type(directory), file_errors(fail)]),
     asserta(ftp_data(Dir))
   )).

% Asserts the current directory as the PySWIP directory.
:- prolog_load_context(file, File),
   absolute_file_name('./', Dir, [relative_to(File), file_type(directory)]),
   asserta(pyswip_dir(Dir)).

% Asserts the current directory as both a library directory and the PySWIP Metta directory.
:- prolog_load_context(directory, Dir),
   asserta(user:library_directory(Dir)),
   asserta(pyswip_metta_dir(Dir)).

% Loads the metta_python library.
metta_python :- ensure_loaded(library(metta_python)).

% Conditionally execute if the predicate must_det_ll/1 is not defined (this block does nothing if the condition fails).
:- if((fail, \+ current_predicate(must_det_ll/1))).

% Calls the given Goal and throws an exception if Goal fails.
% Usage: must_det_ll(+Goal).

%!  must_det_ll(:Goal) is det.
%
%   Executes the given Goal deterministically within the `user` module.
%   If the Goal fails or is a variable, it throws an exception. This
%   ensures the Goal behaves as expected and prevents nondeterminism
%   from leaking into the rest of the program.
%
%   @arg Goal The goal to execute deterministically.
%   @example
%     % Execute a simple goal deterministically.
%     ?- must_det_ll(writeln('Hello')).
%     Hello
%
must_det_ll(M:Goal) :- !, must_det_ll(M, Goal).
must_det_ll(Goal) :- must_det_ll(user, Goal).

%!  must_det_ll(+Module, :Goal) is det.
%
%   Executes the given Goal deterministically within the specified Module.
%   Handles complex logical constructs such as conjunctions, disjunctions,
%   and conditionals. If the Goal fails or is a variable, it throws an
%   exception. The predicate ensures deterministic behavior by throwing
%   exceptions when the Goal fails.
%
%   @arg Module The module within which the Goal is executed.
%   @arg Goal The goal to execute deterministically.
%   @example
%     % Execute a goal within the `user` module.
%     ?- must_det_ll(user, writeln('Hello from user module')).
%     Hello from user module
%
must_det_ll(_M, Goal) :-
    % Throw an error if Goal is a variable.
    var(Goal), !, throw(var_must_det_ll(Goal)), !.
must_det_ll(M, Goal) :-
    % Strip the module from Goal if M is a variable.
    var(M), !, strip_module(Goal, M, NewGoal), !, must_det_ll(M, NewGoal).
must_det_ll(M, (GoalA, GoalB)) :-
    % Execute conjunction (GoalA, GoalB) deterministically.
    !, must_det_ll(M, GoalA), must_det_ll(M, GoalB).
must_det_ll(M, (GoalA -> GoalB ; GoalC)) :-
    % Execute conditional (GoalA -> GoalB ; GoalC).
    !, (call_ll(M, GoalA) -> must_det_ll(M, GoalB) ; must_det_ll(M, GoalC)).
must_det_ll(M, (GoalA *-> GoalB ; GoalC)) :-
    % Execute soft cut (*->) with alternative GoalC.
    !, (call_ll(M, GoalA) *-> must_det_ll(M, GoalB) ; must_det_ll(M, GoalC)).
must_det_ll(M, (GoalA -> GoalB)) :-
    % Execute conditional (GoalA -> GoalB).
    !, (call_ll(M, GoalA) -> must_det_ll(M, GoalB)).
must_det_ll(_, M:Goal) :-
    % If the goal has a module prefix, handle it.
    !, must_det_ll(M, Goal).
must_det_ll(M, Goal) :-
    % Call the goal, and if it fails, throw an exception.
    M:call(Goal) -> true ; throw(failed(Goal)).

%!  call_ll(+Module, :Goal) is det.
%
%   Safely executes the given Goal within the specified Module.
%   If the Goal is a variable, it throws an exception. If the Module
%   is a variable, it strips the module from the Goal and re-executes
%   the call. This ensures proper handling of goals with or without
%   module prefixes.
%
%   @arg Module The module within which the Goal is executed.
%   @arg Goal The goal to execute.
%   @example
%     % Call a goal within the `user` module.
%     ?- call_ll(user, writeln('Hello from user module')).
%     Hello from user module
%
call_ll(_M, Goal) :-
    % Throws an error if Goal is a variable.
    var(Goal), !, throw(var_call_ll(Goal)), !.
call_ll(M, Goal) :-
    % Strips the module from Goal if M is a variable and re-executes.
    var(M), !, strip_module(Goal, M, NewGoal), !, call_ll(M, NewGoal).
call_ll(M, Goal) :-
    % Calls the Goal within the specified Module.
    M:call(Goal).

:- endif.

% Defines if_t/2 if it does not already exist; executes Then if If succeeds, otherwise does nothing.
:- if(\+ current_predicate(if_t/2)).
if_t(If, Then) :- call(If) -> call(Then) ; true.
:- endif.

% Defines atom_contains/2 if it does not already exist; succeeds if SubAtom is contained within Atom1.
:- if(\+ current_predicate(atom_contains/2)).
atom_contains(Atom1, SubAtom) :- sub_atom(Atom1, _Before, _, _After, SubAtom).
:- endif.

% Defines nop/1 if it does not already exist; a no-operation predicate that always succeeds.
:- if(\+ current_predicate(nop/1)).
nop(_).
:- endif.

% Defines catch_ignore/1 if it does not already exist; catches exceptions and logs them if they occur.
:- if(\+ current_predicate(catch_ignore/1)).
catch_ignore(G) :- ignore(catch(G, E, catch_i((nl, writeq(causes(G, E)), nl)))).
:- endif.

% Defines catch_i/1 if it does not already exist; catches exceptions and always succeeds.
:- if(\+ current_predicate(catch_i/1)).
catch_i(G) :- ignore(catch(G, _, true)).
:- endif.

% Defines add_history1/1 if it does not already exist; a placeholder that always succeeds.
:- if(\+ current_predicate(add_history1/1)).
add_history1(_).
:- endif.

% Defines add_history/1 if it does not already exist; a placeholder that always succeeds.
:- if(\+ current_predicate(add_history/1)).
add_history(_).
:- endif.


