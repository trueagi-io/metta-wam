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
% PROGRAM FUNCTION: Interpret MeTTa code within the SWI-Prolog environment, enabling logical
% inference, runtime evaluation, and cross-language integration with Python and Rust.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set the encoding for the Prolog system to UTF-8 to ensure proper handling of characters.
% The previously commented out line was for iso_latin_1 encoding.
% UTF-8 is more universal and can handle a wider range of characters.
:- encoding(utf8).

o_quietly(G):- call(G).
% o_quietly(G):- quietly(G).

o_woc(G):- call(G).
% o_woc(G):- woc(G).

:- dynamic('$metta_setup':on_init_metta/1).
on_metta_setup(Goal):-
   assertz('$metta_setup':on_init_metta(Goal)).
do_metta_setup:- forall('$metta_setup':on_init_metta(Goal),
                        ignore(catch(Goal, Err, format(user_error, '; Goal: ~q   Caused: ~q', [Goal, Err])))).


% Set the 'RUST_BACKTRACE' environment variable to 'full'.
% This likely enables detailed error backtraces when using Rust-based components.
% Rust will now output full stack traces when errors occur, which aids in debugging.
:- on_metta_setup(setenv('RUST_BACKTRACE', full)).

% Set the Prolog flag for encoding to UTF-8 (overrides other default encodings).
% This ensures that the Prolog interpreter treats all input/output as UTF-8 encoded.
:- set_prolog_flag(encoding, utf8).
:- initialization(set_prolog_flag(encoding, utf8)).

% Set a global non-backtrackable variable 'cmt_override' with a specific string pattern.
% This could be used for customizing the way comments or other formatting behaviors are handled.
:- on_metta_setup(nb_setval(cmt_override, lse('; ', ' !(" ', ' ") '))).

% Set the flag to make the source search relative to the working directory.
% This helps locate Prolog files from the current working directory.
:- initialization(set_prolog_flag(source_search_working_directory, true)).

% Enable backtracing, which allows tracking the sequence of goals that led to an error.
% This is useful for debugging as it shows the call stack at the time of an error.
:- initialization(set_prolog_flag(backtrace, true)).

% Set the maximum depth for the backtrace to 100, meaning up to 100 frames of the call stack will be shown.
:- initialization(set_prolog_flag(backtrace_depth, 100)).

% Set the maximum goal depth for backtraces, limiting the display of deeply nested goal calls.
:- initialization(set_prolog_flag(backtrace_goal_dept, 100)).

% Enable showing line numbers in the backtrace, which can help pinpoint where in the source code an error occurred.
:- initialization(set_prolog_flag(backtrace_show_lines, true)).

% Configure the flag to customize how Prolog writes out attributes (such as variables and terms).
% Using 'portray' ensures that the output is human-readable and properly formatted.
:- initialization(set_prolog_flag(write_attributes, portray)).

% Enable debugging on errors.
% When an error occurs, this setting will automatically start the Prolog debugger, providing detailed information about the error.
:- initialization(set_prolog_flag(debug_on_error, true)).

% !(set-prolog-flag debug-on-error True)

% Load additional Prolog support functions from the 'swi_support' file.
% This could include helper predicates or extensions for SWI-Prolog.
:- ensure_loaded(swi_support).

% Load the Prolog documentation library (pldoc).
% This library provides tools for generating and interacting with Prolog documentation.
:- ensure_loaded(library(pldoc)).

/*
% Set the encoding of the `current_input` stream to UTF-8.
% This ensures that any input read from `current_input` (which is typically `user_input`) is interpreted as UTF-8.
:- initialization(set_stream(current_input, encoding(utf8)).

% Set the encoding for the `user_input` stream to UTF-8.
% This makes sure that all input read from `user_input` is correctly handled as UTF-8 encoded text.
:- initialization(set_stream(user_input, encoding(utf8)).

% Treat `user_output` as a terminal (TTY).
% This ensures that output to the terminal behaves properly, recognizing that it's interacting with a terminal (e.g., for handling special characters).
:- initialization(set_stream(user_output, tty(true)).

% Set the encoding for the `user_output` stream to UTF-8.
% This ensures that all output sent to `user_output` is encoded in UTF-8, allowing the display of Unicode characters.
:- initialization(set_stream(user_output, encoding(utf8)).

% Treat `user_error` as a terminal (TTY).
% This ensures that error messages are handled as terminal output, allowing for proper interaction when the user sees error messages.
:- initialization(set_stream(user_error, tty(true)).

% Set the encoding for the `user_error` stream to UTF-8.
% This ensures that error messages and other output sent to `user_error` are encoded in UTF-8, preventing issues with special characters in error messages.
:- initialization(set_stream(user_error, encoding(utf8)).

% Flush any pending output to ensure that anything waiting to be written to output is immediately written.
% Useful to make sure output is synchronized and nothing is left in the buffer.
:- flush_output.
*/

%:- initialization(set_prolog_flag(debug_on_interrupt,true).
%:- initialization(set_prolog_flag(compile_meta_arguments,control).

%   Load required Prolog packs and set up their paths dynamically.
%
%   - Ensures consistent library resolution regardless of the system or environment.
%   - Avoids issues arising from hard-coded paths.
%   - Facilitates maintainability and portability of the codebase.
%   - Supports dynamic pack management during runtime without requiring manual adjustments.
%
attach_mettalog_packs:- (prolog_load_context(directory, Value); Value='.'),
   % Resolve the absolute path to the '../../libraries/' directory.
   absolute_file_name('../../libraries/', Dir, [relative_to(Value)]),
   % Build paths for specific libraries/packs.
   atom_concat(Dir, 'predicate_streams', PS),
   atom_concat(Dir, 'logicmoo_utils', LU),
   % Attach the base library directory with specified options.
   attach_packs(Dir, [duplicate(replace), search(first)]),
      % Attach the `predicate_streams` and `logicmoo_utils` packs.
   pack_attach(PS, [duplicate(replace), search(first)]),
   pack_attach(LU, [duplicate(replace), search(first)]).

:- attach_mettalog_packs.
%:- initialization(attach_mettalog_packs).

%   :- attach_packs.
%:- ensure_loaded(metta_interp).

%!  is_win64 is nondet.
%
%   Succeeds if the current Prolog system is running on a 64-bit Windows operating system.
%
%   This predicate checks the Prolog flag `windows` to determine if the current operating
%   system is Windows. It does not specifically check whether it is a 32-bit or 64-bit version,
%   but typically Prolog systems set this flag on any Windows environment.
%
%   @example Check if the system is running on Windows:
%     ?- is_win64.
%     true.
%
is_win64 :-
    % Check if the 'windows' Prolog flag is set.
    current_prolog_flag(windows, _).

%!  is_win64_ui is nondet.
%
%   Succeeds if the current Prolog system is running on a 64-bit Windows operating system
%   and has a graphical user interface (GUI) enabled.
%
%   This predicate extends `is_win64/0` by also checking if the `hwnd` Prolog flag is set,
%   which typically indicates the presence of a GUI (e.g., SWI-Prolog's graphical tools).
%
%   @see is_win64/0
%   @see current_prolog_flag/2
%
%   @example Check if the system is running on Windows with a GUI:
%     ?- is_win64_ui.
%     true.
%
is_win64_ui :-
    % First, verify if running on Windows.
    is_win64,
    % Then, verify if the 'hwnd' Prolog flag is set, indicating GUI availability.
    current_prolog_flag(hwnd, _).

%   This predicate is used as a guard condition in stream-related operations
%   to prevent unintended changes to Prolog's standard input, output, or error streams.
%   It ensures that existing stream configurations remain intact, avoiding conflicts
%   during operations that might otherwise alter stream properties.
dont_change_streams:- true.

%!  lazy_load_python is det.
%
%   This predicate represents a placeholder or a stub for lazily loading the Python
%   integration. Currently, it does not contain any implementation logic.
%   This would attempt to load Python-related resources or interfaces
%   when needed, avoiding unnecessary overhead if Python is not required.
%
%   The implementation should be added to perform the actual lazy loading of
%   the Python environment or integration.
%
:- dynamic(lazy_load_python/0).
lazy_load_python.

% 'dynamic' enables runtime modification
:- dynamic(user:is_metta_src_dir/1).
% Ensure the `user:is_metta_src_dir/1` predicate is updated with the current directory.
% This retrieves the current loading directory, clears any previous assertions,
% and asserts the new directory dynamically.
save_metta_src_dir:- prolog_load_context(directory,Dir),
  retractall(user:is_metta_src_dir(_)),
  asserta(user:is_metta_src_dir(Dir)).
:- save_metta_src_dir.

%!  metta_root_dir(-Dir) is det.
%
%   Determines the root directory of the Mettalog project.
%
%   This predicate attempts to determine the root directory by first using the
%   `user:is_metta_src_dir/1` dynamic predicate. If that is unavailable or fails,
%   it falls back to the `METTA_DIR` environment variable.
%
%   @arg Dir The absolute path to the Mettalog root directory.
%
%   @example Determine the Mettalog root directory:
%     ?- metta_root_dir(Dir).
%     Dir = '/path/to/metta/root'.
%
%   @see is_metta_src_dir/1
%   @see getenv/2
%   @see absolute_file_name/3
metta_root_dir(Dir) :-
    % Use METTALOG_DIR environment variable if set.
    getenv('METTALOG_DIR', Dir), !.
metta_root_dir(Dir) :-
    % Attempt to resolve the root directory relative to the source directory.
    is_metta_src_dir(Value),
    absolute_file_name('../../', Dir, [relative_to(Value)]).
metta_root_dir(Dir) :-
    % Fallback to using the METTA_DIR environment variable.
    getenv('METTA_DIR', Dir), !.

%!  metta_library_dir(-Dir) is det.
%
%   Determines the library directory of the Mettalog project.
%
%   This predicate resolves the library directory relative to the Mettalog root
%   directory using `metta_root_dir/1`.
%
%   @arg Dir The absolute path to the Mettalog library directory.
%
%   @example Determine the Mettalog library directory:
%     ?- metta_library_dir(Dir).
%     Dir = '/path/to/metta/root/libraries/'.
%
%   @see metta_root_dir/1
%   @see absolute_file_name/3
metta_library_dir(Dir) :-
    % Resolve the library directory relative to the root directory.
    metta_root_dir(Value),
    absolute_file_name('./libraries/', Dir, [relative_to(Value)]).

%!  metta_dir(-Dir) is det.
%
%   Determines the directory path for the Mettalog system.
%
%   This predicate tries multiple approaches to resolve the directory associated
%   with Mettalog in a specific order. It prioritizes specific subdirectories, then
%   falls back to source, library, and root directories.
%
%   The resolution strategy is as follows:
%   1. It attempts to resolve the directory as a subdirectory of the Mettalog library
%      path, specifically './loaders/genome/'.
%   2. Falls back to the directory specified by `is_metta_src_dir/1`.
%   3. Further falls back to the Mettalog library directory.
%   4. Finally, it falls back to the Mettalog root directory.
%
%   @arg Dir The absolute directory path to the Mettalog system.
%
%   @example Find the Mettalog directory:
%     ?- metta_dir(Dir).
%     Dir = '/path/to/metta/loaders/genome/'.
%
%   @see metta_library_dir/1
%   @see is_metta_src_dir/1
%   @see metta_root_dir/1
metta_dir(Dir) :-
    % Attempt to resolve using the Mettalog library directory and './loaders/genome/'.
    metta_library_dir(Value),
    absolute_file_name('./loaders/genome/', Dir, [relative_to(Value)]).
    % Fallback to the source directory if the above resolution fails.
metta_dir(Dir) :-
    is_metta_src_dir(Dir).
    % Fallback to the Mettalog library directory if previous attempts fail.
metta_dir(Dir) :-
    metta_library_dir(Dir).
    % Final fallback to the Mettalog root directory.
metta_dir(Dir) :-
    metta_root_dir(Dir).
% metta_dir(Dir) :- is_metta_src_dir(Value), absolute_file_name('../flybase/', Dir, [relative_to(Value)]).

% The Prolog 'dynamic' enables runtime modification of predicate.
:- dynamic user:file_search_path/2.
%
%   The `user:file_search_path/2` predicate allows associating logical aliases
%   with specific directories, making it easier to reference files without
%   hardcoding their paths. In this case:
%
%   - `library` is mapped to the Mettalog directory, enabling searches for
%     library files relative to the Mettalog root.
%   - `mettalog` is also mapped to the Mettalog directory, likely to handle
%     specific files related to the MettalogLog transpiler/interpreter.
:- multifile user:file_search_path/2.
user:file_search_path(library,Dir):- metta_dir(Dir).
user:file_search_path(mettalog,Dir):- metta_dir(Dir).

%
%   This directive conditionally loads the `logicmoo_utils` library if the system
%   is detected as Windows 64-bit. If the platform is not Windows 64-bit, the
%   directive does nothing.
%
:- is_win64 -> ensure_loaded(library(logicmoo_utils)) ; true.

%   :- initialization(attach_packs).

%   'nodebug' disables specific debugging flags for various Mettalog-related modules.
%
%   These directives ensure that certain debugging categories (`metta(eval)`,
%   `metta(exec)`, `metta(load)`, and `metta(prolog)`) are disabled by default.
%   This reduces console output noise and improves runtime performance in
%   non-debugging environments.
%
:- nodebug(metta(errors)).
:- nodebug(metta(eval)).
:- nodebug(metta(exec)).
:- nodebug(metta(load)).
:- nodebug(metta(prolog)).
:- nodebug(metta(argtypes)).
:- nodebug(metta(main)).
:- nodebug(metta(todo)).


/*
:- initialization(nodebug(metta(eval))).
:- initialization(nodebug(metta(exec))).
:- initialization(nodebug(metta(load))).
:- initialization(nodebug(metta(prolog))).
*/
% Each of these `nodebug/1` directives suppresses debugging output for the corresponding category.

%
%   This section declares several dynamic and multifile predicates used in the
%   Mettalog system. These predicates support runtime updates, extensibility, and
%   modular design. They cover various aspects of Mettalog functionality.
%
%   These predicates are marked as `dynamic` to allow runtime modifications
%   and as `multifile` to enable contributions from multiple modules.
%
:- dynamic(function_arity/3).
:- dynamic(predicate_arity/3).

:-multifile(user:metta_file/3).
:-dynamic(user:metta_file/3).

:- multifile(reset_cache/0).

    :-multifile(metta_type/3).
    :-dynamic(metta_type/3).

    :-multifile(metta_defn/3).
    :-dynamic(metta_defn/3).

:-multifile(user:asserted_metta_pred/2).
:-dynamic(user:asserted_metta_pred/2).
:-multifile(user:loaded_into_kb/2).
:-dynamic(user:loaded_into_kb/2).
:- dynamic(user:is_metta_dir/1).
:- multifile(metta_compiled_predicate/3).
:- dynamic(metta_compiled_predicate/3).

%!  once_writeq_nl(+P) is det.
%
%   Ensures that the given term `P` is printed exactly once in the current session.
%
%   This predicate prevents duplicate printing of the same term using a global
%   non-backtrackable variable (`$once_writeq_ln`). It utilizes `numbervars/4`
%   to standardize variable names for comparison and `ansi_format/3` to provide
%   colored output.
%
%   @arg P The term to be printed. It will be printed once per invocation with
%          `ansi_format` using cyan foreground text.
%
%   @example Print a term only once:
%     ?- once_writeq_nl(foo(bar)).
%     foo(bar).
%

once_writeq_nl(_) :-
    % If the predicate `pfcTraceExecution` is not defined, succeed immediately.
    \+ clause(pfcTraceExecution, true), !.
once_writeq_nl(P) :-
    % If `$once_writeq_ln` is already set to the current term `P`, succeed silently.
    nb_current('$once_writeq_ln', W),
    W=@=P,!.
once_writeq_nl(P):- once_writeq_nl_now(cyan, P), nb_setval('$once_writeq_ln', P),!.


%!  pfcAdd_Now(+P) is det.
%
%   Adds a clause or fact `P` to the database using `pfcAdd/1` or `assert/1`.
%
%   This predicate checks whether the predicate `pfcAdd/1` exists in the database.
%   If it exists, it calls `pfcAdd/1` after printing the term using `once_writeq_nl`.
%   If not, it defaults to using `assert/1` after printing the term.
%
%   @arg P The clause or fact to be added to the database.
%
%   @example Add a fact to the database:
%     ?- pfcAdd_Now(foo(bar)).
%     foo(bar).
%
%   @see once_writeq_nl/1
%

% TODO: Uncomment the following line if the `pfcAdd` predicate is stable and
%       does not interfere with the curried chainer logic.
% pfcAdd_Now(P):- pfcAdd(P),!.


pfcAdd_Now(Cl):-
   once( \+ nb_current(allow_dupes,t)
     ; sub_var_safely('&corelib',Cl )),
    clause_asserted(Cl),!.

pfcAdd_Now(P) :-
    % If `pfcAdd/1` is defined, print the term using `once_writeq_nl` and call `pfcAdd/1`.
    current_predicate(pfcAdd/1),!,
    once_writeq_nl(pfcAdd(P)),
    must_det_lls(pfcAdd(P)).
pfcAdd_Now(P) :-
    % If `pfcAdd/1` is not defined, print the term using `once_writeq_nl` and assert it.
    once_writeq_nl(assssssssssssssert(P)),
    must_det_lls(assert(P)).
%:- endif.

%!  system:copy_term_g(+I, -O) is det.
%
%   Optimized version of `copy_term/2` for ground terms.
%
%   If `I` is ground, it unifies `I` directly with `O`. Otherwise, it behaves
%   like `copy_term/2`, creating a fresh copy of `I`.
%
%   @arg I The input term (ground or non-ground).
%   @arg O The output term, a copy of `I`.
%
system:copy_term_g(I, O) :-
    % Directly unify if the input is ground.
    ground(I),!,I = O.
system:copy_term_g(I, O) :-
    % Otherwise, use `copy_term/2`.
    copy_term(I, O).

:- ensure_loaded(metta_debug).

%!  is_metta_flag(+What) is nondet.
%
%   Checks if a specific flag `What` is enabled in the current configuration.
%
%   This predicate uses `is_flag0/1` to verify the status of the given flag,
%   while suppressing tracing for performance reasons.
%
%   @arg What The name of the flag to check.
%
is_metta_flag(What) :-
    % Check the flag without enabling tracing.
    notrace(is_flag0(What)).

true_flag.
false_flag :- fail.

%!  is_tRuE(+TF) is det.
%
%   Checks if the given term `TF` represents a logical "true" value.
%
%   @arg TF A term expected to be either `'True'` or `'true'`.
%
is_tRuE(TF) :-
    % Match 'True' exactly.
    TF == 'True',!.
is_tRuE(TF) :-
    % Match 'true' exactly.
    TF == 'true',!.

%!  is_fAlSe(+TF) is det.
%
%   Checks if the given term `TF` represents a logical "false" value.
%
%   @arg TF A term expected to be either `'False'` or `'false'`.
%
is_fAlSe(TF) :-
    % Match 'False' exactly.
    TF == 'False',!.
is_fAlSe(TF) :-
    % Match 'false' exactly.
    TF == 'false',!.

%!  is_flag0(+What) is nondet.
%
%   Checks if a flag `What` is logically true in the current environment.
%
%   @arg What The flag to check.
%

is_flag(What):- notrace(is_flag0(What)).

is_flag0(What) :-
    % Check if the flag exists as a non-backtrackable global variable and is true.
    nb_current(What, TF),is_tRuE(TF),!.
is_flag0(What) :-
    % Check if the flag exists as a non-backtrackable global variable and is false.
    nb_current(What, TF),is_fAlSe(TF),!,fail.
is_flag0(What) :-
    % Check if the flag exists as a Prolog configuration flag and is true.
    current_prolog_flag(What, TF),is_tRuE(TF),!.
is_flag0(What) :-
    % Check if the flag exists as a Prolog configuration flag and is false.
    current_prolog_flag(What, TF),is_fAlSe(TF),!,fail.
is_flag0(What) :-
    % Build flag strings for parsing command-line arguments.
    symbol_concat('--', What, FWhat),
    symbol_concat(FWhat, '=true', FWhatTrue),
    symbol_concat('--no-', What, NoWhat),
    symbol_concat(FWhat, '=false', FWhatFalse),
    is_flag0(What, [FWhat, FWhatTrue], [NoWhat, FWhatFalse]).

%!  is_flag0(+What, +FWhatTrue, +FWhatFalse) is nondet.
%
%   Checks command-line arguments (`os_argv`) to determine the status of a flag.
%
%   @arg What       The flag being checked.
%   @arg FWhatTrue  A list of patterns representing a "true" status for the flag.
%   @arg FWhatFalse A list of patterns representing a "false" status for the flag.
%
is_flag0(What, _FWhatTrue, FWhatFalse) :-
    % Check if the flag is explicitly set to false in command-line arguments.
    current_prolog_flag(os_argv, ArgV),member(FWhat, FWhatFalse),member(FWhat, ArgV),!,
    % notrace(catch(set_prolog_flag(What, false), _, true)),
    set_option_value(What, 'False'),!,fail.
is_flag0(What, FWhatTrue, _FWhatFalse) :-
    % Check if the flag is explicitly set to true in command-line arguments.
    current_prolog_flag(os_argv, ArgV),member(FWhat, FWhatTrue),member(FWhat, ArgV),!,
    % notrace(catch(set_prolog_flag(What, true), _, true)),
    set_option_value(What, 'True'),!.
is_flag0(What, _FWhatTrue, _FWhatFalse) :-
    % Parse flags with specific key-value pair syntax in command-line arguments.
    current_prolog_flag(os_argv, ArgV),symbolic_list_concat(['--', What, '='], Starts),
    member(FWhat, ArgV),symbol_concat(Starts, Rest, FWhat),set_option_value_interp(What, Rest),!.

%!  is_compiling is nondet.
%
%   Succeeds if the program is currently in a compilation phase.
%
%   This predicate checks the Prolog runtime arguments (`os_argv`) to determine if
%   the system is performing specific compilation tasks, such as `qcompile_mettalog`
%   or `qsave_program`.
%
is_compiling :-
    current_prolog_flag(os_argv, ArgV),member(E, ArgV),
    % Check if compilation-specific arguments are present.
    (E == qcompile_mettalog; E == qsave_program),!.

%!  is_compiled is nondet.
%
%   Succeeds if the program has been compiled into an executable.
%
%   This predicate verifies whether Prolog is running a precompiled executable by
%   checking for the `-x` flag in `os_argv` or ensuring `swipl` is not present in the
%   argument list.
%
is_compiled :-
    current_prolog_flag(os_argv, ArgV),
    % Check if the `-x` flag is present, indicating an executable was started.
    member('-x', ArgV),!.
is_compiled :-
    current_prolog_flag(os_argv, ArgV),
    % If 'swipl' is absent from the arguments, assume it is a compiled binary.
    \+ member('swipl', ArgV),!.

%!  is_converting is nondet.
%
%   Succeeds if the 'convert' flag is set using is_metta_flag/1.
%
%   @see is_metta_flag/1
%
%   @example
%     ?- is_converting.
%     true.
is_converting :- is_metta_flag('convert').

%!  is_compat is nondet.
%
%   Succeeds if the 'compat' flag is set using is_metta_flag/1.
%
%   @see is_metta_flag/1
%
%   @example
%     ?- is_compat.
%     true.
is_compat :- is_metta_flag('compat').

%!  is_mettalog is nondet.
%
%   Succeeds if the 'log' flag is set using is_metta_flag/1.
%
%   @see is_metta_flag/1
%
%   @example
%     ?- is_mettalog.
%     true.

% is_mettalog :- is_win64,!.
is_mettalog :- is_metta_flag('log').

%!  is_devel is nondet.
%
%   Succeeds if the 'devel' flag is set using is_metta_flag/1.
%
%   @see is_metta_flag/1
%
%   @example
%     ?- is_devel.
%     true.
is_devel :- is_metta_flag('devel').

%!  is_synthing_unit_tests is nondet.
%
%   Wrapper around is_synthing_unit_tests0/0, executed without tracing.
%
%   @see is_synthing_unit_tests0/0
%
%   @example
%     ?- is_synthing_unit_tests.
%     true.
is_synthing_unit_tests :- notrace(is_synthing_unit_tests0).

%!  is_synthing_unit_tests0 is nondet.
%
%   Succeeds if is_testing/0 is true.
%
%   @see is_testing/0
%
%   @example
%     ?- is_synthing_unit_tests0.
%     true.
is_synthing_unit_tests0 :- is_testing.
% is_synthing_unit_tests0 :- is_html.
% is_synthing_unit_tests0 :- is_compatio,!,fail.

%!  is_testing is nondet.
%
%   Succeeds if the 'test' flag is set using is_metta_flag/1.
%
%   @see is_metta_flag/1
%
%   @example
%     ?- is_testing.
%     true.
is_testing :- o_quietly(is_metta_flag('test')).

%!  is_html is nondet.
%
%   Succeeds if the 'html' flag is set using is_metta_flag/1.
%
%   @see is_metta_flag/1
%
%   @example
%     ?- is_html.
%     true.
is_html :- is_metta_flag('html').

% If the file is not already loaded, this is equivalent to consult/1. Otherwise, if the file defines a module,
% import all public predicates. Finally, if the file is already loaded, is not a module file, and the context
% module is not the global user module, ensure_loaded/1 will call consult/1.
:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_loader).

%   This directive ensures that debugging messages or tracing for
%   `'trace-on-eval'` are suppressed, reducing console output during evaluation.
:- nodebug(metta('trace-on-eval')).

%!  is_compatio is nondet.
%
%   Base predicate for determining compatibility conditions.
%
%   This predicate evaluates several conditions, each possibly
%   succeeding or failing based on system flags or runtime conditions.
%
%   @example
%     ?- is_compatio.
%     true.
is_compatio :- notrace(fis_compatio0).

%fis_compatio0 :- is_win64,!,fail.
fis_compatio0 :- is_testing, !, fail.
fis_compatio0 :- is_flag0('compatio'), !.
fis_compatio0 :- is_mettalog, !, fail.
%is_compatio0 :- is_html,!,fail.
fis_compatio0 :- !.

%!  keep_output is nondet.
%
%   Determines if output should be preserved based on several conditions.
%
%   This predicate evaluates multiple conditions in sequence to decide
%   whether output streams should remain unchanged or suppressed.
%
%   @example
%     ?- keep_output.
%     true.
keep_output :- !.
keep_output :- dont_change_streams, !.
keep_output :- is_win64, !.
keep_output :- is_mettalog, !.
keep_output :- is_testing, !.
% fail condition
keep_output :- is_compatio, !, fail.

%
%   The `volatile/1` directive indicates that the predicate should not
%   be saved in a saved state of the program (e.g., when using `qsave_program/2`).
%   It ensures that `original_user_output/1` is only meaningful during runtime
%   and is not preserved across sessions.
%
%   @example
%     ?- volatile(original_user_output/1).
%     true.
:- volatile(original_user_output/1).

% This directive allows the predicate `original_user_output/1` to be modified during runtime.
:- dynamic(original_user_output/1).

%!  original_user_output(-X) is nondet.
%
%   Retrieves the stream associated with the standard output (file descriptor 1).
%
%   This predicate succeeds if `X` unifies with a stream that corresponds to the
%   standard output stream, as determined by the stream property `file_no(1)`.
%
%   @arg X The stream associated with standard output.
%
%   @example
%     ?- original_user_output(Stream).
%     Stream = <stream>.
original_user_output(X) :- stream_property(X, file_no(1)).

%!  original_user_error(-X) is nondet.
%
%   Retrieves the stream associated with the standard error (file descriptor 2).
%
%   This predicate succeeds if `X` unifies with a stream that corresponds to the
%   standard error stream, as determined by the stream property `file_no(2)`.
%
%   @arg X The stream associated with standard error.
%
%   @example
%     ?- original_user_error(Stream).
%     Stream = <stream>.
original_user_error(X) :- stream_property(X, file_no(2)).

% Ensure that the original output stream is set if not already defined.
save_original_user_output:- original_user_output(_)->true;(current_output(Out),asserta(original_user_output(Out))).

:- initialization(save_original_user_output).

%!  unnullify_output is det.
%
%   Restores the output stream to its original state.
%
%   If the current output stream matches the original user output, the predicate
%   succeeds immediately. Otherwise, it restores the output stream to the value
%   stored in `original_user_output/1`.
%
%   @example
%     ?- unnullify_output.
%     true.
unnullify_output :-
    current_output(MFS),
    original_user_output(OUT),
    MFS == OUT,
    !.
unnullify_output :-
    original_user_output(MFS),
    set_prolog_IO(user_input, MFS, user_error).

%!  null_output(-MFS) is det.
%
%   Sets the output stream to a memory file stream.
%
%   If `dont_change_streams/0` succeeds, the original user output is preserved.
%   Otherwise, a new memory file is created and used as the output stream.
%
%   @arg MFS The memory file stream set as the output.
%
%   @example
%     ?- null_output(Stream).
%     Stream = <memory_file_stream>.
null_output(MFS) :- dont_change_streams, !, original_user_output(MFS), !.
null_output(MFS) :- use_module(library(memfile)),new_memory_file(MF),open_memory_file(MF, append, MFS).

% Ensure `null_user_output/1` is not preserved in saved states (e.g., with qsave_program/2).
:- volatile(null_user_output/1).

% Allow `null_user_output/1` to be modified dynamically at runtime.
:- dynamic(null_user_output/1).

% Initialize `null_user_output/1` with a memory file stream if it is not already defined.
:- initialization(((null_user_output(_) -> true ; (null_output(MFS), asserta(null_user_output(MFS)))))).

%!  nullify_output is det.
%
%   Redirects the output stream to a memory file.
%
%   If `keep_output/0` or `dont_change_streams/0` succeed, the predicate does nothing.
%   Otherwise, it calls `nullify_output_really/0` to set up the memory file stream.
%
%   @example
%     ?- nullify_output.
%     true.
nullify_output :- keep_output, !.
nullify_output :- dont_change_streams, !.
nullify_output :- nullify_output_really.

%!  nullify_output_really is det.
%
%   Forces the output stream to be redirected to a memory file.
%
%   If the current output matches `null_user_output/1`, the predicate succeeds.
%   Otherwise, it switches to the memory file stream defined in `null_user_output/1`.
%
%   @example
%     ?- nullify_output_really.
%     true.
nullify_output_really :- current_output(MFS), null_user_output(OUT), MFS == OUT, !.
nullify_output_really :- null_user_output(MFS), set_prolog_IO(user_input, MFS, MFS).

%!  set_output_stream is det.
%
%   Configures the output stream based on current conditions.
%
%   If `dont_change_streams/0` is true, no changes are made.
%   Otherwise, the stream is either nullified or restored, depending on `keep_output/0`.
%
%   @example
%     ?- set_output_stream.
%     true.
set_output_stream :- dont_change_streams, !.
set_output_stream :- \+ keep_output -> nullify_output; unnullify_output.

% Initialize the output stream configuration at startup.
:- initialization(set_output_stream).
% :- nullify_output.

%!  switch_to_mettalog is det.
%
%   Switches the system configuration to the `mettalog` mode.
%
%   This predicate adjusts several runtime options specific to the
%   `mettalog` mode, including output stream configuration and option values.
%   It ensures the system behaves according to the `mettalog` runtime settings.
%
%   @example
%     ?- switch_to_mettalog.
%     true.
switch_to_mettalog :-
    unnullify_output,
    set_option_value('compatio', false),
    set_option_value('compat', false),
    set_option_value('load', show),
    set_option_value('load', verbose),
    set_option_value('log', true),
    %set_option_value('test', true),
    forall(mettalog_option_value_def(Name, DefaultValue), set_option_value(Name, DefaultValue)),
    set_output_stream.

%!  switch_to_mettarust is det.
%
%   Switches the system configuration to the `mettarust` mode.
%
%   This predicate adjusts several runtime options specific to the
%   `mettarust` mode, including output stream configuration and option values.
%   It ensures the system behaves according to the `mettarust` runtime settings.
%
%   @example
%     ?- switch_to_mettarust.
%     true.
switch_to_mettarust :-
    nullify_output,
    set_option_value('compatio', true),
    set_option_value('compat', true),
    set_option_value('log', false),
    set_option_value('test', false),
    forall(rust_option_value_def(Name, DefaultValue), set_option_value(Name, DefaultValue)),
    set_output_stream.

%!  show_os_argv is det.
%
%   Displays the operating system arguments (`os_argv`) used during the execution of the Prolog program.
%
%   This predicate prints the list of command-line arguments passed to the Prolog interpreter.
%   If compatibility mode (`is_compatio/0`) is enabled, it does nothing.
%
%   @example Display the Prolog command-line arguments:
%     ?- show_os_argv.
%     ; libswipl: ['swipl', '-g', 'main', '--', 'arg1', 'arg2'].
%
show_os_argv :-
    % If compatibility mode is enabled, do nothing and succeed silently.
    is_compatio, !.
show_os_argv :-
    % Retrieve and print the command-line arguments using the 'os_argv' Prolog flag.
    current_prolog_flag(os_argv, ArgV),
    if_verbose(main,(write('; libswipl: '), writeln(ArgV))).

%!  is_pyswip is det.
%
%   Succeeds if the current Prolog interpreter was invoked via PySwip.
%
%   This predicate checks the `os_argv` Prolog flag to determine if the argument list
%   contains an entry starting with `'./'`, which is a common indicator that the program
%   was launched via a PySwip wrapper script or similar integration.
%
%   @example Check if Prolog is running via PySwip:
%     ?- is_pyswip.
%     true.
%
is_pyswip :-
    % Retrieve the operating system arguments.
    current_prolog_flag(os_argv, ArgV),
    % Check if any argument starts with './'.
    member('./', ArgV).

% 'multifile' allows a predicate's clauses to be defined across multiple files or modules, while dynamic
% enables runtime modification (asserting or retracting) of a predicate's clauses.
:- multifile(is_metta_data_functor/1).
:- dynamic(is_metta_data_functor/1).
:- multifile(is_nb_space/1).
:- dynamic(is_nb_space/1).

%:- '$set_source_module'('user').

%
%   Provides predicates for extended file operations, such as copying, moving,
%   deleting files, and manipulating file paths.
%
:- use_module(library(filesex)).

%
%   Offers predicates for interacting with the operating system, such as
%   executing commands, retrieving environment variables, and system-level tasks.
%
:- use_module(library(system)).

%
%   Provides predicates for executing shell commands from Prolog, allowing
%   integration with external programs and system utilities.
%
%   Example usage:
%     ?- shell('ls -l').
%     (Lists directory contents).
:- use_module(library(shell)).

%:- use_module(library(tabling)).

%!  use_top_self is nondet.
%
%   Succeeds if the 'top-self' option is enabled.
%
%   This predicate checks the runtime option `'top-self'` using `fast_option_value/2`.
%   If the option is set to `true`, it indicates that the system should treat `&self`
%   as `&top` in certain contexts.
%
%   Example usage:
%     ?- use_top_self.
%     true.
use_top_self :-
    % Check if the 'top-self' option is set to true.
    fast_option_value('top-self', true).

%!  top_self(-Self) is det.
%
%   Determines the current self-reference context.
%
%   If `use_top_self/0` succeeds, `Self` is unified with `&top`, indicating
%   a global context. Otherwise, it defaults to `&self`.
%
%   @arg Self The current self-reference context, either `&top` or `&self`.
%
%   Example usage:
%     ?- top_self(Self).
%     Self = '&top'.
%
%     ?- set_option_value('top-self', false), top_self(Self).
%     Self = '&self'.
top_self('&top') :-
    % If 'top-self' is enabled, return '&top'.
    use_top_self, !.
top_self('&self').
%:- top_self(Self), nb_setval(self_space, '&self'),

%!  current_self(-Self) is det.
%
%   Retrieves the current self-reference context.
%
%   Retrieves the current self-reference context. If the non-backtrackable
%   global variable `self_space` is set, non-empty, and not `&self`, unifies
%   `Self` with its value. Otherwise, falls back to `top_self/1`.
%
%   @arg Self The current self-reference context, typically `&self` or `&top`.
%
current_self(Self) :-
    % Check if 'self_space' is set and not empty or '&self'.
    ((  nb_current(self_space, Self),
        Self \== [],
        assertion(Self \== '&self'))
    ->  true
    ;   % If not, fall back to 'top_self'.
        top_self(Self)
    ).

%
%   Sets the initial value of the REPL mode.
%
%   The non-backtrackable global variable `repl_mode` is initialized with `'+'`,
%   indicating a default REPL mode behavior.
:- initialization(nb_setval(repl_mode, '+')).

% Define the option and call help documentation

%!  option_value_def(+Name, -DefaultValue) is nondet.
%
%   Retrieves the default value of an option.
%
%   This predicate queries the option definitions to fetch the default value
%   associated with a given option `Name`. The default value is extracted
%   from the `all_option_value_name_default_type_help/5` predicate.
%
%   @arg Name The name of the option whose default value is to be retrieved.
%   @arg DefaultValue The default value associated with the option.
%
%   Example usage:
%     ?- option_value_def('compat', Default).
%     Default = false.
option_value_def(Name, DefaultValue) :-
    % Fetch the default value for the given option.
    all_option_value_name_default_type_help(Name, DefaultValueR, _, _, _),
    DefaultValueR = DefaultValue.

%!  rust_option_value_def(+Name, -DefaultValue) is nondet.
%
%   Retrieves the default value of a Rust-specific option.
%
%   This predicate examines option definitions specifically related to Rust
%   compatibility mode. It fetches the MettaLog default value and ensures
%   it differs from the standard default value.
%
%   @arg Name The name of the Rust-specific option.
%   @arg DefaultValue The default value specific to Rust compatibility mode.
%
%   Example usage:
%     ?- rust_option_value_def('compat', Default).
%     Default = true.
rust_option_value_def(Name, DefaultValue) :-
    % Fetch the MettaLog default value and ensure it's different from the standard default.
    all_option_value_name_default_type_help(Name, MettaLogDV, [DefaultValue|_],_Cmt,_Topic),
    MettaLogDV \= DefaultValue.

%!  mettalog_option_value_def(+Name, -MettaLogDV) is nondet.
%
%   Retrieves the MettaLog-specific default value of an option.
%
%   This predicate fetches the MettaLog-specific default value for an option,
%   ensuring it differs from the general default value.
%
%   @arg Name The name of the option.
%   @arg MettaLogDV The MettaLog-specific default value.
%
%   Example usage:
%     ?- mettalog_option_value_def('compat', MettaLogDV).
%     MettaLogDV = true.
mettalog_option_value_def(Name, MettaLogDV) :-
    % Fetch the MettaLog-specific default value and ensure it's different from the general default.
    all_option_value_name_default_type_help(Name, MettaLogDV, [DefaultValue|_],_Cmt,_Topic),
    MettaLogDV \= DefaultValue.

% The discontiguous directive allows clauses of the same predicate to appear non-consecutively in a
% source file, enabling better organization of related code segments across different parts of the file.
:- discontiguous(option_value_name_default_type_help/5).
:- discontiguous(all_option_value_name_default_type_help/5).

%!  all_option_value_name_default_type_help(+Name, -DefaultValue, -Type, -Cmt, -Topic) is nondet.
%
%   Retrieves the details of an option, including its default value, type,
%   comment, and topic.
%
%   This predicate acts as a wrapper around `option_value_name_default_type_help/5`
%   to provide details about a specific configuration option. It ensures consistent
%   querying of option metadata from a centralized source.
%
%   @arg Name The name of the option.
%   @arg DefaultValue The default value of the option.
%   @arg Type A list of valid types or values for the option.
%   @arg Cmt A comment describing the option's purpose.
%   @arg Topic The category or topic to which the option belongs.
%
%   @example Retrieve details of an option:
%     ?- all_option_value_name_default_type_help('compat', Default, Type, Cmt, Topic).
%     Default = false,
%     Type = [true, false],
%     Cmt = "Enable all compatibility with MeTTa-Rust",
%     Topic = 'Compatibility and Modes'.
all_option_value_name_default_type_help(Name, DefaultValue, Type, Cmt, Topic) :-
    % Delegate to the core predicate for retrieving option details.
    option_value_name_default_type_help(Name, DefaultValue, Type, Cmt, Topic), nocut.

% Compatibility and Modes

%!  option_value_name_default_type_help(+Name, -DefaultValue, -Type, -Cmt, -Topic) is nondet.
%
%   Provides metadata about a specific configuration option.
%
%   This predicate defines various configuration options, their default values,
%   allowed types, descriptions, and the categories (topics) they belong to.
%   It is primarily used for querying and managing runtime options in the system.
%
%   @arg Name The name of the configuration option.
%   @arg DefaultValue The default value assigned to the option.
%   @arg Type A list of valid types or values for the option.
%   @arg Cmt A descriptive comment explaining the option's purpose.
%   @arg Topic The category or topic the option belongs to.
%
%   @examples
%     ?- option_value_name_default_type_help('compat', Default, Type, Cmt, Topic).
%     Default = false,
%     Type = [true, false],
%     Cmt = "Enable all compatibility with MeTTa-Rust",
%     Topic = 'Compatibility and Modes'.
%
%     ?- option_value_name_default_type_help('devel', Default, Type, Cmt, Topic).
%     Default = false,
%     Type = [false, true],
%     Cmt = "Developer mode",
%     Topic = 'Compatibility and Modes'.
option_value_name_default_type_help('compat', false, [true, false], "Enable all compatibility with MeTTa-Rust", 'Compatibility and Modes').
option_value_name_default_type_help('compatio', false, [true, false], "Enable IO compatibility with MeTTa-Rust", 'Compatibility and Modes').
option_value_name_default_type_help(src_indents,  false, [false,true], "Sets the indenting of list printing", 'Compatibility and Modes').
all_option_value_name_default_type_help('repl', auto, [false, true, auto], "Enter REPL mode (auto means true unless a file argument was supplied)", 'Execution and Control').
all_option_value_name_default_type_help('prolog', false, [false, true], "Enable or disable Prolog REPL mode", 'Compatibility and Modes').
option_value_name_default_type_help('devel', false, [false, true], "Developer mode", 'Compatibility and Modes').
all_option_value_name_default_type_help('exec', noskip, [noskip, skip, interp], "Controls execution during script loading: noskip or skip (don't-skip-include/binds) vs skip-all", 'Execution and Control').

default_depth(DEPTH):- default_max_depth(DEPTH), !.
%default_depth(DEPTH):- DEPTH = 200,!.
default_max_depth(DEPTH):- current_prolog_flag(max_tagged_integer,DEPTH).

% Resource Limits
option_value_name_default_type_help('stack-max', DEPTH, [MAXDEPTH,1000,500], "Maximum stack depth allowed during execution", 'Resource Limits'):- default_depth(DEPTH),default_max_depth(MAXDEPTH).
all_option_value_name_default_type_help('limit-result-count', inf, [inf,1,2,3,10], "Set the maximum number of results, infinite by default", 'Miscellaneous').
option_value_name_default_type_help('initial-result-count', 10, [inf,10,1], "For MeTTaLog log mode: print the first 10 answers without waiting for user", 'Miscellaneous').

% Miscellaneous
option_value_name_default_type_help('answer-format', 'show', ['rust', 'silent', 'detailed'], "Control how results are displayed", 'Output and Logging').
option_value_name_default_type_help('repeats', true, [true, false], "false to avoid repeated results", 'Miscellaneous').
option_value_name_default_type_help('time', true, [false, true], "Enable or disable timing for operations (in Rust compatibility mode, this is false)", 'Miscellaneous').
option_value_name_default_type_help('vn', true, [true, auto, false], "Enable or disable, (auto = enable but not if it breaks stuff) EXPERIMENTAL BUG-FIX where variable names are preserved (see https://github.com/trueagi-io/metta-wam/issues/221)", 'Miscellaneous').
option_value_name_default_type_help('top-self', true, [true, false, auto], "When set, stop pretending &self==&top", 'Miscellaneous').
option_value_name_default_type_help('devel', false, [false, true], "Set all developer flags", 'Miscellaneous').
option_value_name_default_type_help('old-empty', false, [false, true], "Enable Old Empty undoing EXPERIMENTAL BUG-FIX (see https://github.com/trueagi-io/metta-wam/issues/275)", 'Miscellaneous').

% Testing and Validation
option_value_name_default_type_help('synth-unit-tests', false, [false, true], "Synthesize unit tests", 'Testing and Validation').

% Optimization and Compilation
option_value_name_default_type_help('optimize', true, [true, false], "Enable or disable optimization", 'Optimization and Compilation').
option_value_name_default_type_help('transpiler', 'silent', ['silent', 'verbose'], "Sets the expected level of output from the transpiler", 'Output and Logging').
option_value_name_default_type_help('compile', 'false', ['false', 'true', 'full'], "Compilation option: 'true' is safe vs 'full' means to include unsafe as well", 'Optimization and Compilation').
option_value_name_default_type_help('tabling', auto, [auto, true, false], "When to use predicate tabling (memoization)", 'Optimization and Compilation').

% Output and Logging
option_value_name_default_type_help('log', unset, [false, unset, true], "Act like MeTTaLog more so than H-E (also does generate more logging)", 'Output and Logging').
all_option_value_name_default_type_help('html', false, [false, true], "Generate HTML output", 'Output and Logging').
all_option_value_name_default_type_help('python', true, [true, false], "Enable Python functions", 'Output and Logging').
option_value_name_default_type_help('output', './', ['./'], "Set the output directory", 'Output and Logging').
option_value_name_default_type_help('exeout', './Sav.BADNAME.MeTTaLog', [_], "Output executable location", 'Miscellaneous').
option_value_name_default_type_help('halt', false, [false, true], "Halts execution after the current operation", 'Miscellaneous').

% Debugging and Tracing
option_value_name_default_type_help('trace-length', 500, [inf], "Length of the trace buffer for debugging", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-overtime', 4.0, [inf], "Trace if execution time exceeds limit", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-overflow', 10_000, [inf,10_000_000], "Trace on stack overflow", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-eval', false, [false, true], "Trace during normal evaluation", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-load', silent, [silent, verbose], "Verbosity on file loading", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-exec', false, [silent, verbose], "Trace on execution during loading", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-errors', 'non-type', [false, 'non-type', true], "Trace on all or none or non-type errors", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-fail', false, [false, true], "Trace on failure", 'Debugging and Tracing').
option_value_name_default_type_help('trace-on-test', true, [silent, false, verbose], "Trace on success as well", 'Debugging and Tracing').
option_value_name_default_type_help('repl-on-error', true, [false, true], "Drop to REPL on error", 'Debugging and Tracing').
option_value_name_default_type_help('repl-on-fail',  false, [false, true], "Start REPL on failed unit test", 'Debugging and Tracing').
option_value_name_default_type_help('exit-on-fail',  false, [true, false], "Rust exits on first Assertion Error", 'Debugging and Tracing').

option_value_name_default_type_help('rrtrace',  false, [false, true], "Extreme Tracing", 'Debugging and Tracing').

%!  type_value(+Type, +Value) is det.
%
%   Defines possible values for various configuration types used in the system.
%   These values represent different modes and behaviors that can be toggled
%   or configured dynamically during runtime.
%
%   @arg Type The type of configuration or operational mode being defined.
%             Examples include `verbosity_mode`, `compile_mode`, `exec_mode`,
%             `fail_mode`, and `error_mode`.
%   @arg Value The specific value associated with the given type. Each type has
%              a predefined set of valid values.
%
% Verbosity values
type_value(verbosity_mode, 'silent').  % No output or only critical errors
type_value(verbosity_mode, 'error').   % Only errors are shown
type_value(verbosity_mode, 'warn').    % Errors and warnings are shown
type_value(verbosity_mode, 'info').    % General information (default level)
type_value(verbosity_mode, 'debug').   % Detailed debug output
type_value(verbosity_mode, 'trace').   % Extremely detailed output, execution trace

% Compile modes
type_value(compile_mode, 'false').  % Compilation is disabled
type_value(compile_mode, 'true').   % Basic compilation is enabled
type_value(compile_mode, 'auto').   % Automatically decide based on context
type_value(compile_mode, 'full').   % Full compilation is enabled

% Execution modes
type_value(exec_mode, 'noskip').   % Execution proceeds normally
type_value(exec_mode, 'skip').   % Execution is skipped

% Fail modes
type_value(fail_mode, 'repl').   % On failure, drop into REPL
type_value(fail_mode, 'exit').   % On failure, exit execution

% Error handling modes
type_value(error_mode, 'default').  % Default error handling mode
type_value(warning_mode, 'default'). % Default warning handling mode

% Dynamically show all available options with descriptions in the required format, grouped and halt

%!  show_help_options_no_halt is det.
%
%   Displays all available runtime options with their descriptions in a grouped format.
%   Options are categorized based on their groups, and each entry displays:
%     - Name: The option's name.
%     - Default Value: The default value for the option.
%     - Possible Values: A list of acceptable values for the option.
%     - Description: A brief explanation of the option's purpose.
%
%   The output groups options logically and formats them for clarity.
%
show_help_options_no_halt :-
    findall([Name, DefaultValue, Type, Help, Group],
            option_value_name_default_type_help(Name, DefaultValue, Type, Help, Group),
            Options),
    max_name_length(Options, MaxLen),
    format("  First value is the default; if a brown value is listed, it is the Rust compatibility default:\n\n"),
    group_options(Options, MaxLen),!.

%!  show_help_options is det.
%
%   Displays all available runtime options in a grouped format and halts execution.
%   This predicate is a wrapper around `show_help_options_no_halt/0` and ensures
%   the program stops after displaying the options.
%
show_help_options :-
    show_help_options_no_halt,
    halt.

%!  max_name_length(+Options, -MaxLen) is det.
%
%   Calculates the maximum length of option names from a list of options.
%
%   This predicate iterates over a list of options and determines the length
%   of each option's name. The maximum length is then unified with `MaxLen`.
%   This value is typically used for aligning descriptions when displaying
%   options in a formatted output.
%
%   @arg Options A list of options, where each option is represented as a list
%                with at least one element (`Name`) being an atom.
%   @arg MaxLen  The maximum length (in characters) of the option names in `Options`.
%
%   @examples
%     ?- max_name_length([['verbosity', _, _, _, _], ['compile_mode', _, _, _, _]], MaxLen).
%     MaxLen = 12.
%
max_name_length(Options, MaxLen) :-
    % Extract the length of each option name from the Options list.
    findall(Length,
            (member([Name, _, _, _, _], Options), % For each option, extract the name.
             atom_length(Name, Length)            % Get the length of the name.
            ), Lengths),
    % Find the maximum value in the list of lengths.
    max_list(Lengths, MaxLen).

%!  group_options(+Options, +MaxLen) is det.
%
%   Groups runtime options by category and prints them in an organized format.
%
%   This predicate collects option categories (groups) from the options list,
%   removes duplicates, and then passes each group to `print_groups/3` for display.
%
%   @arg Options A list of options, where each option includes a group category.
%   @arg MaxLen  The maximum length of option names for formatting alignment.
%
group_options(Options, MaxLen) :-
    % Extract all groups from the options list
    findall(Group, member([_, _, _, _, Group], Options), Groups),
    % Remove duplicate groups
    list_to_set(Groups, SortedGroups),
    % Print each group
    print_groups(SortedGroups, Options, MaxLen).

%!  print_groups(+Groups, +Options, +MaxLen) is det.
%
%   Print options by group with clarification for defaults and Rust compatibility.
%
%   This predicate iterates over each group, displays the group name, and calls
%   `print_group_options/3` to display the group's options.
%
%   @arg Groups  A list of unique option groups.
%   @arg Options A list of all options.
%   @arg MaxLen  The maximum length of option names for formatting alignment.
%
print_groups([], _, _). % Base case: no groups left to print.
print_groups([Group | RestGroups], Options, MaxLen) :-
    % Print group header
    format("   ~w:\n", [Group]),
    % Print options within the current group
    print_group_options(Group, Options, MaxLen),
    % Add spacing after group
    format("\n"),
    % Recursively print the next group
    print_groups(RestGroups, Options, MaxLen).

%!  print_group_options(+Group, +Options, +MaxLen) is det.
%
%   Prints all options belonging to a specific group, aligned by the longest option name.
%   Special formatting is applied to highlight default values and Rust-specific values.
%
%   @arg Group   The category of options to print.
%   @arg Options A list of options, each represented by `[Name, DefaultValue, Type, Help, Group]`.
%   @arg MaxLen  The maximum length of option names for consistent alignment.
%
print_group_options(_, [], _).
print_group_options(Group, [[Name, DefaultValue, Type, Help, Group] | Rest], MaxLen) :-
    % Remove duplicates from the list of values
    list_to_set(Type, UniqueValues),
    list_to_set([DefaultValue|Type], [_,_|UniqueValues2]),
    % Define the column where the comment should start
    CommentColumn is 60, % Adjust this number to set the comment column position
    ( (UniqueValues = [DefaultValue | RestOfValues])
    ->  % Print default first, then other values, omit empty lists
        (format_value_list(RestOfValues, CleanRest),
         ( (CleanRest \= '')
         ->  format("     --~w~*t=<\033[1;37m~w\033[0m|~w> \033[~dG ~w\n", [Name, MaxLen, DefaultValue, CleanRest, CommentColumn, Help])
         ;   format("     --~w~*t=<\033[1;37m~w\033[0m> \033[~dG ~w\n", [Name, MaxLen, DefaultValue, CommentColumn, Help])
         ))
    ;   % Case 2: If the default value is not first, list default first and mark the first value as Rust-specific
        (UniqueValues = [RustSpecificValue | _RestOfValues],
         DefaultValue \= RustSpecificValue)
    ->  % Print default first, mark the Rust value in brown, then other values, omit empty lists
        (format_value_list(UniqueValues2, CleanRest),
         ( (CleanRest \= '')
         ->  format("     --~w~*t=<\033[1;37m~w\033[0m|\033[38;5;94m~w\033[0m|~w> \033[~dG ~w\n", [Name, MaxLen, DefaultValue, RustSpecificValue, CleanRest, CommentColumn, Help])
         ;   format("     --~w~*t=<\033[1;37m~w\033[0m|\033[38;5;94m~w\033[0m> \033[~dG ~w\n", [Name, MaxLen, DefaultValue, RustSpecificValue, CommentColumn, Help])
         ))
    ),!,
    print_group_options(Group, Rest, MaxLen).
% Skip options that don't match the current group and continue processing the rest.
print_group_options(Group, [_ | Rest], MaxLen) :-
    print_group_options(Group, Rest, MaxLen).

%!  format_value_list(+Values, -Formatted) is det.
%
%   Converts a list of values into a formatted string, separating entries with a pipe (`|`) character.
%
%   This predicate handles lists of values, removing square brackets and ensuring a clean string format.
%
%   @arg Values    A list of atomic values to format.
%   @arg Formatted The resulting string where values are concatenated and separated by `|`.
%
%   @examples
%     ?- format_value_list([a, b, c], Formatted).
%     Formatted = "a|b|c".
%
%     ?- format_value_list([a], Formatted).
%     Formatted = "a".
%
format_value_list([], ''). % Base case: Empty list results in an empty string.
% Single-element list, return the element directly.
format_value_list([H], H) :- !.
format_value_list([H|T], Formatted) :-
    % Recursively format the tail of the list
    format_value_list(T, Rest),
    % Concatenate head and rest with a pipe `|`
    format(atom(Formatted), "~w|~w", [H, Rest]).

%!  fbugio(+TF, +P) is det.
%
%   Outputs debugging information based on a truth-functional flag.
%
%   @arg TF A truth-functional flag
%   @arg P  The message or term to be passed to `fbug/1` for debugging.
%

%fbugio(TF,P):-!, ignore(( TF,!,write_src_uo(fbug(P)))). % Previously used for different debugging output.
%fbugio(_,_):- is_compatio,!. % Bypasses debugging in compatibility mode.
fbugio(TF,P):-
    !, ignore((TF,!,fbug(P))).
% Assumes TF is true by default.
fbugio(IO):-
    fbugio(true, IO).

%!  different_from(+N, +V) is nondet.
%
%   Succeeds if the value associated with `N` is different from `V`.
%
%   This predicate checks whether the value tied to `N` (through `option_value_def/2`
%   or `nb_current/2`) differs from `V`. It is primarily used to determine if a
%   configuration option or global variable has a non-matching value.
%
%   @arg N The name of the option or variable to check.
%   @arg V The value to compare against.
%
%   @examples
%     ?- different_from('verbosity', 'debug').
%     true.
%
%     ?- different_from('verbosity', 'info').
%     false.
%
different_from(N,V) :-
    % Check if N matches V via option_value_def/2, fail if it matches.
    \+ \+ option_value_def(N,V), !, fail.
different_from(N,V) :-
    % Check if N matches V via nb_current/2, fail if it matches.
    \+ \+ nb_current(N,V), !, fail.
different_from(_,_). % Default case: succeeds if no match was found.

%!  set_option_value_interp(+N, +V) is det.
%
%   Sets the value of an option or multiple options, handling comma-separated lists.
%
%   If `N` contains multiple comma-separated option names, it splits them
%   and applies `set_option_value_interp/2` to each. Otherwise, it directly sets
%   the value for `N` and triggers any necessary callbacks via `on_set_value/3`.
%
%   @arg N The option name or a comma-separated list of option names.
%   @arg V The value to set for the option(s).
%
%   @examples
%     ?- set_option_value_interp('verbosity', 'debug').
%     true.
%
%     ?- set_option_value_interp('verbosity,logging', 'info').
%     true.
%
set_option_value_interp(N,V):-
    % If N is a comma-separated list, split and set each option individually.
    symbol(N), symbolic_list_concat(List,',',N),
    List \= [_], % Ensure it's not a single-element list.
    !,forall(member(E,List), set_option_value_interp(E,V)).
set_option_value_interp(N,V):-
    % Directly set the option value and trigger any callbacks.
    % Note can be used for debugging purposes (commented out).
    %(different_from(N,V)->Note=true;Note=false),
    Note = true,
    %fbugio(Note,set_option_value(N,V)), % Uncomment for debugging.
    ignore(set_option_value(N,V)), % Set the value for the option.
    ignore(forall(on_set_value(Note,N,V), true)). % Trigger callbacks if any.

%!  on_set_value(+Note, +N, +V) is det.
%
%   Handles callbacks and side effects when an option value is set.
%
%   Depending on the option name (`N`) and value (`V`), this predicate triggers
%   specific behaviors such as switching modes, enabling debugging, or setting
%   flags. It also supports string-based logical values ('True', 'False').
%
%   @arg Note A debugging or informational flag, typically set to `true` or `false`.
%   @arg N    The name of the option being set.
%   @arg V    The value assigned to the option.
%
%   @examples
%     ?- on_set_value(true, 'log', true).
%     % Switches to mettalog mode.
%
%     ?- on_set_value(true, 'trace-on-load', true).
%     % Enables trace-on-load debugging.
%

% Map string logical values ('True', 'False') to their atom equivalents.

on_set_value(Note,N,'True'):- nocut,
    on_set_value(Note,N,true).    % true
on_set_value(Note,N,'False'):- nocut,
    on_set_value(Note,N,false).   % false
on_set_value(_Note,abolish_trace,true):- nocut, ignore(abolish_trace),!.
on_set_value(_Note,log,true):-
    % Switch to mettalog mode if 'log' is set to true.
    switch_to_mettalog,!.
on_set_value(_Note,compatio,true):-
    % Switch to mettarust mode if 'compatio' is set to true.
    switch_to_mettarust,!.
on_set_value(Note,N,V):-
    % Handle trace-specific options by extracting the trace flag from the option name.
    symbol(N),
    % Extract trace-specific flag.
    symbol_concat('trace-on-',F,N),nocut,
     % Debugging output.
    if_trace(main,not_compatio(fbugio(Note,set_debug(F,V)))),
    % Enable or disable trace based on value.
    set_debug(F,V).
on_set_value(Note,N,V):-
    % General debugging setting for other options.
    symbol(N),
    % Check if the value is debug-like.
    is_debug_like(V,TF),
    % Debugging output.
    if_trace(main,not_compatio(fbugio(Note,set_debug(N,TF)))),
    % Enable or disable debug mode based on value.
    set_debug(N,TF).

%!  is_debug_like(+Value, -Flag) is det.
%
%   Determines whether a given value represents a debugging-related flag.
%
%   This predicate maps symbolic values commonly used for debugging (`trace`,
%   `debug`, `silent`, etc.) to their corresponding Boolean representations.
%
%   @arg Value The symbolic value representing a debugging state.
%   @arg Flag  The Boolean flag (`true` or `false`) representing the debugging state.
%
%   @examples
%     ?- is_debug_like(trace, Flag).
%     Flag = true.
%
%     ?- is_debug_like(silent, Flag).
%     Flag = false.
%

% Previously included case for explicit 'false' mapping (commented out).
%is_debug_like(false, false).
% Trace mode is considered a debugging state.
is_debug_like(trace, true).
% 'notrace' disables debugging.
is_debug_like(notrace, false).
% Debug mode is explicitly enabled.
is_debug_like(debug, true).
% 'nodebug' disables debugging.
is_debug_like(nodebug, false).
% 'silent' indicates no debugging output.
is_debug_like(silent, false).
is_debug_like(hide, false).
is_debug_like(quiet, false).
is_debug_like(verbose, true).


%!  'is-symbol'(+X) is nondet.
%
%   Checks if `X` is a valid symbol.
%
%   This predicate succeeds if `X` is recognized as a symbol.
%   It acts as a wrapper around the built-in `symbol/1` predicate.
%
%   @arg X The term to check.
%
%   @examples
%     ?- 'is-symbol'(hello).
%     true.
%
%     ?- 'is-symbol'(123).
%     false.
%
'is-symbol'(X):-
    % Check if X is a symbol.
    symbol(X).

%:- (is_mettalog->switch_to_mettalog;switch_to_mettarust).

%!  set_is_unit_test(+TF) is det.
%
%   Configures runtime settings for unit testing mode.
%
%   This predicate sets various runtime options based on the value of `TF`.
%   When `TF` is `false`, testing-related settings are disabled, and default
%   values are restored. When `TF` is `true`, unit testing settings are applied.
%
%   @arg TF A Boolean value (`true` or `false`) indicating whether unit testing mode should be enabled.
%
%   @examples
%     ?- set_is_unit_test(true).
%     % Enables unit testing mode with specific options.
%
%     ?- set_is_unit_test(false).
%     % Disables unit testing mode and restores defaults.
%

% Disable unit testing and reset runtime options to defaults.
set_is_unit_test(false):-
    % Reset all options to their default values.
    reset_default_flags,
    % Explicitly disable trace and test-related settings.
    set_option_value_interp('trace-on-test', false),
    set_option_value_interp('trace-on-fail', false),
    set_option_value_interp('load', silent),
    set_option_value_interp('test', false),
    !.
% Enable unit testing with specific runtime configurations.
set_is_unit_test(TF):-
    abolish_trace,
    % Reset all options to their default values.
    reset_default_flags,
    % Disable specific trace settings during unit testing.
    set_option_value_interp('trace-on-test', false),
    set_option_value_interp('trace-on-fail', false),
    % Enable specific load and test options.
    set_option_value_interp('load', show),
    set_option_value_interp('test', TF),
    %set_option_value_interp('trace-on-load',TF),
/*  if_t(TF,set_option_value_interp('exec',debug)),
  if_t(TF,set_option_value_interp('eval',debug)),
  set_option_value_interp('trace-on-exec',TF),
  set_option_value_interp('trace-on-eval',TF),*/
 % if_t( \+ TF , set_prolog_flag(debug_on_interrupt,true)),
 % TODO: what is this cutting here?
  !.

:- meta_predicate fake_notrace(0).

%!  fake_notrace(:Goal) is det.
%
%   Executes the given Goal with modified tracing behavior.
%
%   @arg Goal The goal to execute.
fake_notrace(G) :-
    % If tracing is active, suppress tracing using `real_notrace/1`.
    tracing, !, real_notrace(G).
fake_notrace(G) :-
    % If tracing is inactive, execute the goal without tracing using `notrace/1`.
    !, notrace(G).
fake_notrace(G) :-
    !, once(G).
% `o_quietly/1` allows breaking in and inspection (real `no_trace/1` does not)
fake_notrace(G) :-
    o_quietly(G), !.

:- meta_predicate real_notrace(0).

%!  real_notrace(:Goal) is det.
%
%   Executes the given Goal while completely suppressing tracing. This ensures
%   that the execution of Goal is not interrupted by trace events or debugging.
%
%   @arg Goal The goal to execute.
%
real_notrace(Goal) :- !, notrace(Goal).
real_notrace(Goal) :-
    % Temporarily disable tracing and execute the goal.
    setup_call_cleanup(
        % Save current tracing state and skip level.
        '$notrace'(Flags, SkipLevel),
        % Execute the goal once.
        once(Goal),
        % Restore the original tracing state and skip level.
        '$restore_trace'(Flags, SkipLevel)
    ).

:- volatile(is_answer_output_stream/2).
:- dynamic(is_answer_output_stream/2).

%!  answer_output(-Stream) is det.
%
%   Retrieves or creates a stream for writing answers. If an answer output stream
%   is already open, it is reused. Otherwise, a new memory file is created, and a
%   stream is opened for writing to it.
%
%   @arg Stream The stream for writing answers.
%

%answer_output(Stream):- is_testing,original_user_output(Stream),!.
%answer_output(Stream):- !,original_user_output(Stream),!. % yes, the cut is on purpose
answer_output(Stream) :-
    is_answer_output_stream(_, Stream), !.  % Use existing stream if already open
answer_output(Stream) :-
    new_memory_file(MemFile),                              % Create a new memory file
    open_memory_file(MemFile, write, Stream, [encoding(utf8)]),  % Open it as a stream
    asserta(is_answer_output_stream(MemFile, Stream)).  % Store memory file and stream reference

%!  write_answer_output is det.
%
%   Handles the output of answers stored in a memory file. This predicate performs
%   the following steps:
%   1. Retrieves and removes the reference to the current memory file and its stream.
%   2. Closes the stream safely.
%   3. Reads the contents of the memory file as a string.
%   4. Writes the string to the standard output.
%   5. Frees the memory file to release resources.
%
write_answer_output :-
    retract(is_answer_output_stream(MemFile, Stream)), !,  % Retrieve and remove memory file reference
    ignore(catch_ignore(close(Stream))),                     % Close the stream
    memory_file_to_string(MemFile, String),               % Read contents from the memory file
    write(String),                                        % Write the contents to output
    catch_ignore(free_memory_file(MemFile)).              % Free the memory file
write_answer_output.

:- at_halt(write_answer_output).  % Ensure cleanup at halt

%!  with_answer_output(:Goal, -String) is det.
%
%   Executes the specified Goal while capturing its output into a string.
%   This predicate creates a temporary memory file and stream for capturing
%   output, executes the Goal, retrieves the output as a string, and cleans up
%   all resources after execution.
%
%   @arg Goal   The Prolog goal to execute.
%   @arg String The string capturing the output of the Goal.
%
with_answer_output(Goal, S) :-
    new_memory_file(MemFile),                         % Create a new memory file
    open_memory_file(MemFile, write, Stream, [encoding(utf8)]),  % Open it as a stream
    asserta(is_answer_output_stream(MemFile, Stream), Ref),
    setup_call_cleanup(
        true,
        (call(Goal)),                                  % Execute the Goal
        (
            close(Stream),                             % Close the stream after Goal
            memory_file_to_string(MemFile, S),         % Retrieve content as a string
            free_memory_file(MemFile),                 % Free the memory file
            erase(Ref)                                 % Retract the temporary predicate
        )
    ).

%!  null_io(:Goal) is det.
%
%   Executes the specified Goal while suppressing its output. The output
%   is redirected to a null output stream.
%
%   @arg Goal The Prolog goal to execute with output suppressed.
%
null_io(G) :-
    % Redirect output to a null stream and execute the Goal.
    null_user_output(Out), !,
    with_output_to(Out, G).

%!  user_io(:Goal) is det.
%
%   Executes the specified Goal while directing its output to the user.
%   This predicate avoids tracing overhead and supports MettaLog runtime.
%
%   @arg Goal The Prolog goal to execute with user-directed output.
%
user_io(G) :-
    % Execute the goal using the user_io_0/1 helper.
    notrace(user_io_0(G)).

%!  user_io_0(:Goal) is det.
%
%   A helper predicate for `user_io/1` that directs output based on the
%   runtime environment. In MettaLog runtime mode, output is directed to
%   `original_user_error/1`; otherwise, it is directed to `original_user_output/1`.
%
%   @arg Goal The Prolog goal to execute with appropriate output redirection.
%
user_io_0(G) :-
    nb_current('$dont_redirect_output', true), !,
    call(G).
user_io_0(G) :-
    % If in MettaLog runtime mode, output to the error stream.
    current_prolog_flag(mettalog_rt, true), !,
    original_user_error(Out),
    ttyflush, !,
    with_output_to(Out, G),
    flush_output(Out),
    ttyflush.
user_io_0(G) :-
    % Otherwise, output to the original user output stream.
    original_user_output(Out),
    ttyflush, !,
    with_output_to(Out, G),
    flush_output(Out),
    ttyflush.

%!  user_err(:Goal) is det.
%
%   Executes the specified Goal while directing its output to the error stream.
%
%   @arg Goal The Prolog goal to execute with error-directed output.
%
user_err(G) :-
    % Redirect output to the original error stream and execute the Goal.
    original_user_error(Out), !,
    with_output_to(Out, G).

%!  with_output_to_s(+Stream, :Goal) is det.
%
%   Temporarily redirects the current output to the specified stream while
%   executing the given Goal. Restores the previous output settings afterward.
%
%   @arg Stream The stream to which output should be redirected.
%   @arg Goal   The Prolog goal to execute with redirected output.
%
with_output_to_s(Out, G) :-
    % Save the current output stream.
    current_output(COut),
    % Temporarily redirect output and execute the Goal.
    redo_call_cleanup(
        set_prolog_IO(user_input, Out, user_error),
        G,
        % Restore the original output stream after execution.
        set_prolog_IO(user_input, COut, user_error)
    ).

%!  not_compatio(:Goal) is det.
%
%   Executes the specified Goal only if the current context is not in compatibility mode.
%   This behavior depends on the `is_mettalog`, `is_testing`, and `is_compatio` conditions.
%
%   @arg Goal The Prolog goal to execute when compatibility mode is off.
%
not_compatio(G) :-
    % If already in a `not_compatio` context, execute the Goal directly.
    nb_current(in_not_compatio, true), !,
    call(G).
not_compatio(G) :-
    % Otherwise, determine whether compatibility mode is off and execute the Goal.
    if_t(
        once(is_mettalog ; is_testing ; (\+ is_compatio)),
        user_err(
            locally(b_setval(in_not_compatio, true), G)
        )
    ).

:- thread_initialization(nb_setval(in_not_compatio,[])).

%!  extra_answer_padding(+Arg) is det.
%
%   Placeholder predicate for padding answers. Currently, this predicate
%   performs no operation.
%
%   @arg Arg Placeholder argument, unused in this implementation.
%
extra_answer_padding(_).

%!  in_answer_io(+G) is det.
%
%   Main predicate for executing a goal while capturing its output and handling it appropriately.
%   This predicate first checks if the answer output is suspended via `nb_current/2`.
%   If output is not suspended, it captures the output based on the streams involved.
%
%   @arg G The goal to be executed.
in_answer_io(G):- notrace((in_answer_io_0(G))).
in_answer_io_0(_):- nb_current(suspend_answers,true),!.
in_answer_io_0(G) :-
    % Get the answer_output stream
    answer_output(AnswerOut),
    % Get the current output stream
    current_output(CurrentOut),
    % Get the standard output stream via file_no(1)
    get_stdout_stream(StdOutStream),
    % If the output is already visible to the user, execute G directly
   ((   AnswerOut == CurrentOut ;   AnswerOut == StdOutStream )
    ->  call(G)
    ; ( % Otherwise, capture and process the output
        % Determine the encoding
        stream_property(CurrentOut, encoding(CurrentEncoding0)),
        (   CurrentEncoding0 == text
        ->  stream_property(AnswerOut, encoding(CurrentEncoding))
        ;   CurrentEncoding = CurrentEncoding0
        ),
        % Start capturing output per solution
        capture_output_per_solution(G, CurrentOut, AnswerOut, StdOutStream, CurrentEncoding))).

%!  get_stdout_stream(-StdOutStream) is det.
%
%   Retrieves the standard output stream. This predicate identifies the stream
%   associated with file descriptor 1 (stdout) by using `current_stream/3` and
%   checking its properties.
%
%   @arg StdOutStream The variable that unifies with the standard output stream.
%
get_stdout_stream(StdOutStream) :-
    % Find a stream that is open for writing and matches file descriptor 1.
    current_stream(_, write, StdOutStream),
    stream_property(StdOutStream, file_no(1)), !.

%!  capture_output_per_solution(+G, +CurrentOut, +AnswerOut, +StdOutStream, +CurrentEncoding) is det.
%
%   Captures and processes the output for each solution of a nondeterministic goal.
%   A memory file is used to temporarily store the output for each solution, which
%   is then finalized and handled appropriately.
%
%   @arg G               The goal whose output is being captured.
%   @arg CurrentOut      The current output stream.
%   @arg AnswerOut       The answer output stream.
%   @arg StdOutStream    The standard output stream.
%   @arg CurrentEncoding The encoding used for capturing and writing output.
%
capture_output_per_solution(G, CurrentOut, AnswerOut, StdOutStream, CurrentEncoding) :-
    % Prepare initial memory file and write stream
    State = state(_, _),
    set_output_to_memfile(State, CurrentEncoding),
    % Use setup_call_catcher_cleanup to handle execution and cleanup
    setup_call_catcher_cleanup(
        true,
        (
           (call(G),
            % Check determinism after G succeeds
            deterministic(Det)),
            % Process the captured output
            process_and_finalize_output(State, CurrentOut, AnswerOut, StdOutStream, CurrentEncoding),
            % If there are more solutions, prepare for the next one
            (   Det == false
            ->  % Prepare a new memory file and write stream for the next solution
                set_output_to_memfile(State,CurrentEncoding)
            ;   % If deterministic, leave cleanup for process_and_finalize_output
                true
            )
        ),
        Catcher,
        (
            % Final cleanup
            process_and_finalize_output(State, CurrentOut, AnswerOut, StdOutStream, CurrentEncoding)
        )
    ),
    % Handle exceptions and failures
    handle_catcher(Catcher).

%!  set_output_to_memfile(+State, +CurrentEncoding) is det.
%
%   Creates a new memory file and write stream for capturing output and updates the State.
%   This predicate also sets the output stream to the new memory file's write stream.
%
%   @arg State The state holding the memory file and write stream.
%   @arg CurrentEncoding The encoding to use for the memory file.
set_output_to_memfile(State,CurrentEncoding):-
    % Create a new memory file.
    new_memory_file(NewMemFile),
    % Open the memory file for writing with the specified encoding.
    open_memory_file(NewMemFile, write, NewWriteStream, [encoding(CurrentEncoding)]),
    % Update the state with the new memory file and write stream (non-backtrackable).
    nb_setarg(1, State, NewMemFile),
    nb_setarg(2, State, NewWriteStream),
    % Redirect output to the new write stream.
    set_output(NewWriteStream).

%!  process_and_finalize_output(+State, +CurrentOut, +AnswerOut, +StdOutStream, +CurrentEncoding) is det.
%
%   Finalizes the captured output, closing streams and writing content to the necessary output streams.
%   This also handles freeing up memory resources and transcodes content if necessary.
%
%   @arg State The current state holding the memory file and write stream.
%   @arg CurrentOut The original output stream to after_load /**/ after processing.
%   @arg AnswerOut The stream to write the captured output to.
%   @arg StdOutStream The standard output stream.
%   @arg CurrentEncoding The encoding used for reading and writing the output.
process_and_finalize_output(State, CurrentOut, AnswerOut, StdOutStream, CurrentEncoding) :-
    % Retrieve the memory file and write stream from the state.
    arg(1, State, MemFile),
    arg(2, State, WriteStream),
    % Close the write stream to flush the output
    (nonvar(WriteStream) -> (close(WriteStream),nb_setarg(2, State, _)) ; true),
    % Reset the output stream to its original state
    set_output(CurrentOut),
    % Read the captured content from the memory file
    (nonvar(MemFile) ->
      (nb_setarg(1, State, _),
       open_memory_file(MemFile, read, ReadStream, [encoding(CurrentEncoding)]),
       read_string(ReadStream, _, Content),
       close(ReadStream),
       % Free the memory file
       free_memory_file(MemFile),
       % Write the content to the streams, handling encoding differences
       write_to_stream(AnswerOut, Content, CurrentEncoding),
       (   AnswerOut \== StdOutStream ->  nop(write_to_stream(user_error, Content, CurrentEncoding)) ;   true )) ; true).


%!  handle_catcher(+Catcher) is det.
%
%   Handles the `setup_call_catcher_cleanup/4` catcher to determine how to proceed after execution.
%
%   @arg Catcher The result of the call (either success, failure, or exception).
handle_catcher(Var) :-
    % If the catcher is unbound, the call succeeded.
    var(Var), !.
handle_catcher(exit).  % Success, do nothing.
handle_catcher(fail) :- fail.  % Failure, propagate it.
handle_catcher(exception(Exception)) :- throw(Exception).  % Exception, re-throw it.

%!  write_to_stream(+Stream, +Content, +ContentEncoding) is det.
%
%   Writes the given content to the specified stream, handling encoding differences between the content and the stream.
%
%   @arg Stream The stream to write to.
%   @arg Content The content to be written.
%   @arg ContentEncoding The encoding of the content.
write_to_stream(Stream, Content, ContentEncoding) :-
  % Retrieve the encoding of the destination stream.
  stream_property(Stream, encoding(StreamEncoding)),
  transcode_content(Content, ContentEncoding, StreamEncoding, TranscodedContent),
  with_output_to(Stream, write(TranscodedContent)).

%!  transcode_content(+Content, +FromEncoding, +ToEncoding, -TranscodedContent) is det.
%
%   Transcodes content from one encoding to another by writing it to a temporary memory file.
%
%   @arg Content The original content.
%   @arg FromEncoding The encoding of the original content.
%   @arg ToEncoding The target encoding.
%   @arg TranscodedContent The resulting content in the target encoding.
transcode_content(Content, SameEncoding, SameEncoding, Content) :- !.
transcode_content(Content, FromEncoding, ToEncoding, TranscodedContent) :-
    % Write the content to a temporary memory file with the original encoding.
    new_memory_file(TempMemFile),
    open_memory_file(TempMemFile, write, TempWriteStream, [encoding(FromEncoding)]),
    write(TempWriteStream, Content),
    close(TempWriteStream),
    % Read the content from the memory file with the target encoding.
    open_memory_file(TempMemFile, read, TempReadStream, [encoding(ToEncoding), encoding_errors(replace)]),
    read_string(TempReadStream, _, TranscodedContent),
    close(TempReadStream),
    % Free the temporary memory file.
    free_memory_file(TempMemFile).

%if_compatio(G):- if_t(is_compatio,user_io(G)).
% if_compat_io(G):- if_compatio(G).

%!  not_compat_io(Goal) is nondet.
%
%   Executes the given Goal if the current environment or configuration
%   is not in compatibility mode. This predicate relies on `not_compatio/1`
%   to determine whether the compatibility condition is not satisfied.
%
%   @arg Goal The Prolog goal to execute conditionally.
%
%   @example
%     % Example usage to check for non-compatibility mode and run a task:
%     ?- not_compat_io(writeln('Non-compatible environment active.')).
%
not_compat_io(G) :- not_compatio(G).

%!  non_compat_io(Goal) is nondet.
%
%   Alias for `not_compat_io/1`. This predicate is provided for semantic clarity
%   or readability in specific contexts where a slightly different naming convention
%   improves understanding.
%
%   @arg Goal The Prolog goal to execute conditionally.
%
%   @example
%     % Example usage to run a goal in non-compatible mode:
%     ?- non_compat_io(writeln('Non-compatible I/O behavior enabled.')).
%
non_compat_io(G) :- not_compatio(G).

%!  trace_on_pass is det.
%
%   Hardcoded to `false`. This predicate is intended to control whether
%   execution tracing occurs on successful goals. Since it always fails,
%   no tracing will happen on success.
%
%   @example
%     % Always fails, as it is hardcoded:
%     ?- trace_on_pass.
%     false.
%
trace_on_pass :- false.

%!  trace_on_fail is nondet.
%
%   Succeeds if the runtime option `trace-on-fail` is set to `true`.
%   This predicate checks the current state of the `trace-on-fail` option
%   to determine whether tracing should be active for failing goals.
%
%   @see option_value/2
%
%   @example
%     % Check if tracing on failure is enabled:
%     ?- set_option_value('trace-on-fail', true).
%     ?- trace_on_fail.
%     true.
%
trace_on_fail :- option_value('trace-on-fail', true).

%!  trace_on_overflow is nondet.
%
%   Succeeds if the runtime option `trace-on-overflow` is set to `true`.
%   This option enables tracing when stack or memory overflows occur.
%
%   @see option_value/2
%
%   @example
%     % Enable and check trace-on-overflow:
%     ?- set_option_value('trace-on-overflow', true).
%     ?- trace_on_overflow.
%     true.
%
trace_on_overflow :- option_value('trace-on-overflow', true).

%!  doing_repl is nondet.
%
%   Succeeds if the runtime option `doing_repl` is set to `true`.
%   This predicate determines whether the program is running in a
%   Read-Eval-Print Loop (REPL) mode.
%
%   @see option_value/2
%
%   @example
%     % Check if the REPL mode is active:
%     ?- set_option_value('doing_repl', true).
%     ?- doing_repl.
%     true.
%
doing_repl :- option_value('doing_repl', true).

%!  if_repl(:Goal) is det.
%
%   Executes the provided Goal if `doing_repl/0` succeeds. Otherwise,
%   it does nothing. This is used for conditionally executing goals
%   depending on whether the REPL mode is active.
%
%   @arg Goal The Prolog goal to execute in REPL mode.
%
%   @example
%     % Execute a goal only in REPL mode:
%     ?- set_option_value('doing_repl', true).
%     ?- if_repl(writeln('This is REPL mode.')).
%     This is REPL mode.
%
if_repl(Goal) :- doing_repl -> call(Goal) ; true.

%!  any_floats(+List) is nondet.
%
%   Succeeds if any element of the provided list is a floating-point number.
%   It scans the list and succeeds as soon as a float is found.
%
%   @arg List The list to check for floating-point numbers.
%
%   @example
%     % A list containing floats:
%     ?- any_floats([1, 2.5, 3]).
%     true.
%
%     % A list with no floats:
%     ?- any_floats([1, 2, 3]).
%     false.
%
any_floats(S) :- member(E, S), float(E), !.

%!  show_options_values is det.
%
%   Outputs all current non-hidden runtime options and their values.
%   Options that are symbolic atoms starting with `$` are excluded.
%   The results are printed in the format `pragma!(Name, Value)`.
%
%   @see nb_current/2
%
%   @example
%     % Display all visible runtime options and values:
%     ?- show_options_values.
%     pragma!(trace-on-fail, true)
%     pragma!(doing_repl, true)
%
show_options_values :-
    forall((nb_current(N, V), \+ ((symbol(N), symbol_concat('$', _, N)))),
           write_src_nl(['pragma!', N, V])).

%!  interpreter_source_file(-File) is det.
%
%   Retrieves the current source file being loaded and stores it in the database.
%   This is useful for identifying the file from which the interpreter was invoked.
%   The source file is obtained using `prolog_load_context/2`.
%
%   @arg File The file being asserted as the source of the interpreter.
%
%   @example
%     % Check the source file:
%     ?- interpreter_source_file(File).
%     File = 'your_prolog_file.pl'.
%
:- ignore((prolog_load_context(source, File), assert(interpreter_source_file(File)))).




find_missing_cuts :-
    once(prolog_load_context(source, File);prolog_load_context(file, File)),!,
    find_missing_cuts(File).

find_missing_cuts(File) :-
    findall(Predicate, source_file(Predicate, File), Predicates),
    sort(Predicates, UniquePredicates),
    maplist(check_predicate_for_missing_cuts(File), UniquePredicates).



skip_checking(file_search_path,2).
skip_checking('$pldoc',4).
skip_checking('$mode',2).
skip_checking('reset_cache',0).
skip_checking(_,0).

check_predicate_for_missing_cuts(_File, P):- functor(P,F,A), skip_checking(F,A),!.
check_predicate_for_missing_cuts(File, Predicate) :-
    predicate_property(Predicate, number_of_clauses(TotalClauses)),
    predicate_property(Predicate, number_of_rules(RuleClauses)), RuleClauses>0,
    functor(Predicate, Functor, Arity),
    %format('Checking predicate ~w/~w:\n', [Functor, Arity]),
    forall(
        (nth_clause(Predicate, ClauseNumber, Ref),
         clause(Predicate, Body, Ref),
         clause_property(Ref, line_count(LineNumber))),
        check_clause_for_cut(Functor/Arity, Body, File, LineNumber, ClauseNumber, TotalClauses)
    ),!.
check_predicate_for_missing_cuts(_File, _Predicate).

% Do not warn about missing cuts on the last clause
check_clause_for_cut(_, _, _, _, ClauseNumber, TotalClauses) :-
    ClauseNumber =:= TotalClauses, !.

check_clause_for_cut(PredicateIndicator, Body, File, Line, ClauseNum, TotalClauses) :-
    ( deterministic_body(Body) ->
        true
    ;
        is_extreme_debug(( atomic_list_concat(L,'/',File),atomic_list_concat(L,'\\',WFile),
          format(' H:~w(~w): (clause ~w of ~w for ~w)\n',
               [WFile, Line, ClauseNum, TotalClauses, PredicateIndicator])))

    ).

% Simplistic deterministic check heuristic:
deterministic_body(Body) :- sub_var(!, Body),!.
deterministic_body(Body) :- sub_var(nocut, Body),!.
%deterministic_body(Body) :- sub_var(fail, Body),!.
deterministic_body(Body) :- Body == fail,!.
deterministic_body(Body) :- Body == true.
nocut.


% If the file is not already loaded, ensure_loaded is equivalent to consult/1. Otherwise, if the file defines a module,
% import all public predicates. Finally, if the file is already loaded, is not a module file,
% and the context module is not the global user module, ensure_loaded/1 will call consult/1.
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_proof).
%:- ensure_loaded(mettalog('metta_ontology.pfc.pl')).
:- ensure_loaded(metta_pfc_debug).
:- ensure_loaded(metta_pfc_base).
:- ensure_loaded(metta_pfc_support).
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_eval).
:- nb_setval(self_space, '&top').

:- initialization(nb_setval(self_space, '&top')).

%!  set_is_unit_test(+Flag) is det.
%
%   Sets the system's unit testing mode. When `false`, unit testing is disabled,
%   reverting to normal runtime behavior. If `true`, unit testing mode is enabled.
%   The predicate is typically used to control runtime settings for testing purposes.
%
%   @arg Flag A boolean value (`true` or `false`) indicating whether unit testing
%        mode should be enabled or disabled.
%
%   @example
%     % Disable unit testing mode:
%     ?- set_is_unit_test(false).
%
:- on_metta_setup(set_is_unit_test(false)).

%!  extract_prolog_arity(+TypesList, -PrologArity) is nondet.
%
%   Extracts the arity (number of arguments) of a Prolog predicate from a list of parameter types.
%   This is specifically designed for cases where the parameter types are prefixed by an arrow (`->`).
%   The predicate succeeds if the first element in the list is `->`.
%
%   @arg TypesList A list containing the `->` symbol followed by parameter types.
%   @arg PrologArity The resulting arity derived from the length of the parameter types list.
%
%   @example
%     % Extract arity from a list with a `->` symbol:
%     ?- extract_prolog_arity(['->', int, string], Arity).
%     Arity = 2.
%
extract_prolog_arity([Arrow | ParamTypes], PrologArity) :-
    Arrow == ('->'), !,
    len_or_unbound(ParamTypes, PrologArity).

%!  add_prolog_code(+KB, +Clause) is det.
%
%   Adds a Prolog clause to the knowledge base if it is not already present.
%   This predicate uses `assertz_if_new/1` to ensure that duplicate clauses are not added.
%   The knowledge base (`KB`) argument is currently unused but may be used for context in the future.
%
%   @arg KB Unused in the current implementation.
%   @arg Clause The Prolog clause to add to the knowledge base.
%
%   @example
%     % Add a Prolog clause if it doesn't already exist:
%     ?- add_prolog_code(my_kb, parent(john, mary)).
%
add_prolog_code(_KB, AssertZIfNew) :-
    fbug(add_prolog_code(AssertZIfNew)),
    assertz_if_new(AssertZIfNew).

%!  gen_interp_stubs(+KB, +Symbol, +Definition) is det.
%
%   This predicate is currently disabled.
%
%   When enabled, `gen_interp_stubs/3` dynamically generates Prolog clauses (stubs)
%   that wrap Metta definitions into Prolog predicates. These stubs allow evaluating
%   Metta function calls in a Prolog environment using `eval_H/2`.
%
%   The generated clauses are added to the specified knowledge base (`KB`) and
%   provide a mechanism to map function symbols to Prolog predicates.
%
%   @arg KB The knowledge base where the generated Prolog clauses would be added.
%   @arg Symbol The symbol representing the function or predicate to evaluate.
%   @arg Definition The definition or type signature for the function, usually a list.
%
%   @note
%     - This functionality is currently manually handled in `metta_compiler_lib.pl`.
%     - No operations are performed when this predicate is invoked in its current state.
%
%   @example
%     % If enabled, the following would generate a Prolog clause:
%     ?- gen_interp_stubs('&kb', 'my_func', ['->', int, int, int]).
%
%     % Resulting clause:
%     % mc_3__my_func(Arg1, Arg2, RetVal) :-
%     %     eval_H(['my_func', Arg1, Arg2], RetVal).
%

% This clause disables the functionality of the predicate.
% The actual implementation is currently done manually in `metta_compiler_lib.pl`.
gen_interp_stubs(_KB, _Symb, _Def) :- !.
% Intended functionality (currently disabled):
% Dynamically generate Prolog stubs for Metta definitions.
gen_interp_stubs(KB, Symb, Def) :-
    ignore((
        is_list(Def),  % Ensure the definition is in list format.
        must_det_ll((
            % Extract the arity of the Prolog predicate from the definition.
            extract_prolog_arity(Def, PrologArity),
            % Calculate the function arity by subtracting one for the return value.
            FunctionArity is PrologArity - 1,
            % Ensure the symbol is valid.
            symbol(Symb),
            % Construct the name of the trampoline predicate dynamically.
            atomic_list_concat(['mc_', FunctionArity, '__', Symb], Tramp),
            % Generate arguments for the Prolog clause.
            length(PrologArgs, PrologArity),
            append(MeTTaArgs, [RetVal], PrologArgs),
            % Construct the head of the Prolog clause.
            TrampH =.. [Tramp | PrologArgs],
            % Add the clause to the knowledge base.
            add_prolog_code(KB,
                (TrampH :- eval_H([Symb | MeTTaArgs], RetVal)))
        ))
    )).

% 'int_fa_format-args'(FormatArgs, Result):- eval_H(['format-args'|FormatArgs], Result).
% 'ext_fa_format-args'([EFormat, EArgs], Result):- int_format-args'(EFormat, EArgs, Result)
/*

'ext_format-args'(Shared,Format, Args, EResult):-
    pred_in('format-args',Shared,3),
      argn_in(1,Shared,Format,EFormat),
      argn_in(2,Shared,Args,EArgs),
      argn_in(3,Shared,EResult,Result),
    int_format-args'(Shared,EFormat, EArgs, Result),
      arg_out(1,Shared,EFormat,Format),
      arg_out(2,Shared,EArgs,Args),
      arg_out(3,Shared,Result,EResult).

 you are goign to create the clause based on the first 2 args

?-  gen_form_body('format-args',3, HrnClause).

HrnClause =
   ('ext_format-args'(Shared, Arg1, Arg2, EResult):-
    pred_in('format-args',Shared,3),
      argn_in(1,Shared,Arg1,EArg1),
      argn_in(2,Shared,Arg2,EArg2),
      argn_in(3,Shared,EResult,Result),
    'int_format-args'(Shared,EArg1, EArg2, Result),
      arg_out(1,Shared,EArg1,Arg1),
      arg_out(2,Shared,EArg2,Arg2),
      arg_out(3,Shared,Result,EResult)).

*/
%!  generate_head(+Shared, +Arity, +FormName, +Args, -Head) is det.
%
%   Generates the head of a Prolog clause for a given form. The head includes a unique
%   predicate name derived from the form name and its arity, along with shared context
%   and argument placeholders.
%
%   @arg Shared   The shared context passed to the generated clause.
%   @arg Arity    The arity (number of arguments) for the form.
%   @arg FormName The base name of the form being processed.
%   @arg Args     The list of arguments for the generated clause.
%   @arg Head     The resulting head of the clause.
%
%   @example
%     % Generate the head for a form named "example" with arity 3:
%     ?- generate_head(shared_context, 3, 'example', [Arg1, Arg2, Arg3], Head).
%     Head = ext_example3(shared_context, Arg1, Arg2, Arg3).
%
generate_head(Shared, Arity, FormName, Args, Head) :-
    atom_concat('ext_', FormName, ExtFormName),
    number_string(Arity, ArityStr),
    atom_concat(ExtFormName, ArityStr, FinalFormName), % Append arity to form name for uniqueness
    append([FinalFormName, Shared | Args], HeadArgs),
    Head =.. HeadArgs.

%!  generate_body(+Shared, +Arity, +FormName, +Args, +EArgs, -Body) is det.
%
%   Generates the body of a Prolog clause, handling argument transformation
%   and internal processing. Input arguments are reversed for internal evaluation.
%
%   @arg Shared   The shared context passed to the generated clause.
%   @arg Arity    The arity (number of arguments) for the form.
%   @arg FormName The base name of the form being processed.
%   @arg Args     The original list of arguments for the clause.
%   @arg EArgs    The evaluated arguments for the clause.
%   @arg Body     The resulting body of the clause.
%
%   @example
%     % Generate the body for a form with specific arguments:
%     ?- generate_body(shared_context, 3, 'example', [A1, A2, A3], [EA1, EA2, EA3], Body).
%     Body = (pred_in(example, shared_context, 3),
%             argn_in(1, shared_context, A1, EA1),
%             argn_in(2, shared_context, A2, EA2),
%             argn_in(3, shared_context, A3, EA3),
%             int_example3(shared_context, EA3, EA2, EA1),
%             arg_out(1, shared_context, EA1, A1),
%             arg_out(2, shared_context, EA2, A2),
%             arg_out(3, shared_context, EA3, A3)).
%
generate_body(Shared, Arity, FormName, Args, EArgs, Body) :-
    atom_concat('int_', FormName, IntFormName),
    number_string(Arity, ArityStr),
    atom_concat(IntFormName, ArityStr, FinalIntFormName), % Append arity to internal form name for uniqueness
    reverse(EArgs, ReversedEArgs), % Reverse the order of evaluated arguments for internal processing
    % Generate predicates for input handling
    findall(argn_in(Index, Shared, Arg, EArg),
            (nth1(Index, Args, Arg), nth1(Index, EArgs, EArg)), ArgIns),
    % Internal processing call with reversed arguments
    append([Shared | ReversedEArgs], IntArgs),
    InternalCall =.. [FinalIntFormName | IntArgs],
    % Generate predicates for output handling
    findall(arg_out(Index, Shared, EArg, Arg),
            (nth1(Index, EArgs, EArg), nth1(Index, Args, Arg)), ArgOuts),
    % Combine predicates
    PredIn = pred_in(FormName, Shared, Arity),
    append([PredIn | ArgIns], [InternalCall | ArgOuts], BodyParts),
    list_to_conjunction(BodyParts, Body).

%!  gen_form_body(+FormName, +Arity, -Clause) is det.
%
%   Generates a complete Prolog clause for a given form. The clause includes
%   a head (generated by `generate_head/5`) and a body (generated by `generate_body/6`).
%
%   @arg FormName The base name of the form being processed.
%   @arg Arity    The arity (number of arguments) for the form.
%   @arg Clause   The resulting Prolog clause.
%
%   @example
%     % Generate a clause for a form named "example" with arity 3:
%     ?- gen_form_body('example', 3, Clause).
%     Clause = (ext_example3(Shared, Arg1, Arg2, Arg3) :-
%                 (pred_in(example, Shared, 3),
%                  argn_in(1, Shared, Arg1, EArg1),
%                  argn_in(2, Shared, Arg2, EArg2),
%                  argn_in(3, Shared, Arg3, EArg3),
%                  int_example3(Shared, EArg3, EArg2, EArg1),
%                  arg_out(1, Shared, EArg1, Arg1),
%                  arg_out(2, Shared, EArg2, Arg2),
%                  arg_out(3, Shared, EArg3, Arg3))).
%
gen_form_body(FormName, Arity, Clause) :-
    % Create lists of variables for arguments and their evaluated forms.
    length(Args, Arity),   % Args are placeholders for input arguments.
    length(EArgs, Arity),  % EArgs are placeholders for evaluated arguments.
    % Generate the head of the clause using the form name and shared context.
    generate_head(Shared, Arity, FormName, Args, Head),
    % Generate the body of the clause, including argument transformations
    % and the call to the internal processing predicate.
    generate_body(Shared, Arity, FormName, Args, EArgs, Body),
    % Construct the full clause by combining the head and the body.
    Clause = (Head :- Body).

%!  format_atom(+Format, +N, -Atom) is det.
%
%   Creates a formatted atom based on the provided format string and number.
%   This is a utility predicate for constructing atoms dynamically.
%
%   @arg Format A format string (e.g., "prefix_%d").
%   @arg N      A number to include in the formatted string.
%   @arg Atom   The resulting formatted atom.
%
%   @example
%     % Format an atom with a number:
%     ?- format_atom("func_%d", 3, Atom).
%     Atom = 'func_3'.
%
format_atom(Format, N, Atom) :-
    format(atom(Atom), Format, [N]).


% 'int_format-args'(Shared,Format, Args, Result):-
%    .... actual impl ....

% ============================
% %%%% Missing Arithmetic Operations
% ============================

%!  '%'(+Dividend, +Divisor, -Remainder) is det.
%
%   Computes the remainder of dividing Dividend by Divisor using the modulo operation.
%   Use eval_H/2 to evaluate the modulo operation and unify the result with Remainder.
%
'%'(Dividend, Divisor, Remainder) :- eval_H(['mod', Dividend, Divisor], Remainder).

%!  mettalog_rt_args(-Args) is det.
%
%   Retrieves runtime arguments for MettaLog. If the `mettalog_rt_args` flag is set,
%   its value is used; otherwise, a default list is returned.
%
mettalog_rt_args(Args) :-
    % Check if the Prolog flag `mettalog_rt_args` is set, and use its value if so.
    current_prolog_flag(mettalog_rt_args, Args), !.
% Return a default argument list if the flag is not set.
mettalog_rt_args(['--repl=false']).

%!  metta_argv(-Args) is det.
%
%   Retrieves Metta command-line arguments based on different runtime conditions.
%
metta_argv(Args) :-
    % Use the `metta_argv` Prolog flag if it has been explicitly set.
    current_prolog_flag(metta_argv, Args), !.
metta_argv(Args) :-
    % If running in MettaLog runtime mode, retrieve runtime arguments.
    current_prolog_flag(mettalog_rt, true), !,
    mettalog_rt_args(Args).
metta_argv(Before) :-
    % Parse OS arguments to extract those following `--args`.
    current_prolog_flag(os_argv, OSArgv),
    append(_, ['--args' | AArgs], OSArgv),
    % Separate arguments before and after the double-dash `--`.
    before_arfer_dash_dash(AArgs, Before, _), !,
    % Process and store the extracted arguments as Metta arguments.
    set_metta_argv(Before).

%!  argv_metta(+Nth, -Value) is nondet.
%
%   Retrieves the Nth argument from the Metta arguments.
%
argv_metta(Nth, Value) :-
    % Fetch the full list of Metta arguments.
    metta_argv(Args),
    % Get the Nth argument using nth1/3 (1-based indexing).
    nth1(Nth, Args, Value).

%!  set_metta_argv(+Before) is det.
%
%   Sets the `metta_argv` flag with processed arguments from the input list.
%
set_metta_argv(Before) :-
    % Process each argument in the input list.
    maplist(read_argv, Before, Args),
    % Store the processed arguments in the `metta_argv` Prolog flag.
    set_prolog_flag(metta_argv, Args), !.

%!  read_argv(+RawArg, -ProcessedArg) is det.
%
%   Processes a single argument. If the argument is not symbolic, it is left unchanged.
%   Otherwise, it is converted into a Prolog term.
%
read_argv(AArg, Arg) :-
    % If the argument is not a symbol, use it as-is.
    \+ symbol(AArg), !,
    AArg = Arg.
read_argv(AArg, Arg) :-
    % Convert symbolic arguments to Prolog terms using `read_sexpr/2`.
    atom_string(AArg, S),
    read_sexpr(S, Arg), !.

cmdline_flag_option_value(What, Value):-
  symbolic_list_concat(['--', What,'='],FWhat),
  metta_cmd_args(Args),!,member(FWhatQValue,Args),
  symbol_concat(FWhat,QValue,FWhatQValue),
  unquote_value(QValue,Value),!.
cmdline_flag_option_value(What, Value):- option_value(What, Value),!.

unquote_value(Atom, New) :- concat_atom_safe(['',New,''], '"', Atom),!.
unquote_value(Atom, New) :- concat_atom_safe(['',New,''], "'", Atom),!.
unquote_value(Atom, New) :- Atom=New,!.



%!  metta_cmd_args(-Args) is det.
%
%   Retrieves command-line arguments passed to Metta, prioritizing different sources.
%
metta_cmd_args(Args) :-
    % Use MettaLog runtime arguments if `mettalog_rt` is active.
    current_prolog_flag(mettalog_rt, true), !,
    mettalog_rt_args(Args).
metta_cmd_args(Rest) :-
    % Use late options if the `late_metta_opts` flag is set.
    current_prolog_flag(late_metta_opts, Rest), !.
metta_cmd_args(Rest) :-
    % Extract arguments after `--` in the OS arguments.
    current_prolog_flag(os_argv, P),
    append(_, ['--' | Rest], P), !.
metta_cmd_args(Rest) :-
    current_prolog_flag(argv,P),append(_,['--'|Rest],P),!.
metta_cmd_args(Rest) :-
    % Fall back to using all arguments from the `argv` flag.
    current_prolog_flag(argv, Rest).

:- dynamic(has_run_cmd_args/0).  % Informs the interpreter that the definition of the predicate(s) may change during execution.
:- volatile(has_run_cmd_args/0). % Declare that the clauses of specified predicates should not be saved to the program.

%!  run_cmd_args_prescan is det.
%
%   Performs a prescan of the command-line arguments, ensuring this is done only once.
%
run_cmd_args_prescan :-
    % Skip the prescan if it has already been completed.
    has_run_cmd_args, !.
run_cmd_args_prescan :-
    % Mark that the prescan has been executed.
    assert(has_run_cmd_args),
    % Perform the prescan using `do_cmdline_load_metta/1`.
    do_cmdline_load_metta(prescan).

%!  run_cmd_args is det.
%
%   Processes the command-line arguments by executing both prescan and execution steps.
%
run_cmd_args :-
    % First, ensure the prescan step is complete.
    run_cmd_args_prescan,
    % Enable debugging on interrupt signals.
    set_prolog_flag(debug_on_interrupt, true),
    % Execute the remaining command-line processing.
    do_cmdline_load_metta(execute),
    maybe_do_repl(run_cmd_args).

%!  metta_make_hook is det.
%
%   This predicate ensures proper handling of reloading versus fresh starts
%   within the Metta environment.
%
%   @see metta_cmd_args/1
%   @see do_cmdline_load_metta/3
%
metta_make_hook :-
    % Reset the environment.
    loonit_reset,
    % Check if the `not_a_reload` option is set to true.
    option_value(not_a_reload, true), !.
metta_make_hook :-
    % Retrieve the remaining command-line arguments for Metta.
    metta_cmd_args(Rest),
    % Convert command-line arguments into reload options.
    into_reload_options(Rest, Reload),
    % Perform the reload operation with the processed options.
    do_cmdline_load_metta(reload, '&self', Reload).

:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

%!  prolog:make_hook(+Phase, +Args) is det.
%
%   A hook for integrating Metta into the Prolog `make/0` rebuild process.
%   This hook is triggered during the specified phase of the `make/0` operation.
%   Currently, it checks for the `after` phase and executes `metta_make_hook/0`.
%
%   @arg Phase The phase of the `make/0` process. This can be `after`, `before`, etc.
%   @arg Args  Additional arguments for the hook (currently unused).
%
%   @see metta_make_hook/0
%
prolog:make_hook(after, _Some) :-
    % Call `metta_make_hook/0` within a safe environment, ignoring its effects with `nop/1`.
    nop(metta_make_hook).

%!  into_reload_options(+Input, -Output) is det.
%
%   Passes the reload options unchanged from Input to Output.
%
%   @arg Input  The input reload options.
%   @arg Output The output reload options, identical to Input.
%
into_reload_options(Reload, Reload).

%!  is_cmd_option(+Opt, +Cmd, -Value) is nondet.
%
%   Checks if a given command-line argument matches a specific option and extracts its value.
%   If the option is found, Value is unified with the extracted or inferred value.
%
%   @arg Opt   The base name of the option (e.g., "verbose").
%   @arg Cmd   The full command-line argument (e.g., "-verbose=true").
%   @arg Value The extracted value (e.g., `true`, `false`, or a specific term).
%
%   @example
%     ?- is_cmd_option('verbose', '-verbose=true', Value).
%     Value = true.
%
is_cmd_option(Opt, M, TF) :-
    % Ensure both Opt (the option) and M (the command) are symbolic atoms.
    symbol(M),
    % Construct the flag by prefixing Opt with a hyphen.
    symbol_concat('-', Opt, Flag),
    % Check if the command contains the flag.
    atom_contains(M, Flag), !,
    % Extract the associated value for the option.
    get_flag_value(M, FV),
    % Unify the result with the extracted value.
    TF = FV.

%!  get_flag_value(+Cmd, -Value) is det.
%
%   Extracts the value associated with a command-line argument. If no explicit value
%   is provided, the value is inferred based on the presence or absence of `-no` in the argument.
%
%   @arg Cmd   The command-line argument (e.g., "-verbose=true").
%   @arg Value The extracted or inferred value (`true`, `false`, or another term).
%
%   @example
%     ?- get_flag_value('-verbose=true', Value).
%     Value = true.
%
get_flag_value(M, V) :-
    % If the command contains '=', extract the value after '='.
    symbolic_list_concat([_, V], '=', M), !.
get_flag_value(M, false) :-
    % If the command contains '-no', set the value to `false`.
    atom_contains(M, '-no'), !.
% Default to `true` if no explicit value is found.
get_flag_value(_, true).

% Ensure default options are set when not reloading.
% This block is ignored during reloading to prevent resetting options unnecessarily.
set_default_flags:- ignore(((
       % Check if the current context is not in reload mode.
       \+ prolog_load_context(reloading, true),
       % Set default option values for all defined options.
       nop((reset_default_flags))
))).

:- on_metta_setup(set_default_flags).

%!  process_python_option is nondet.
%
%   Processes the value of specific options and ensures the required modules are loaded.
%   If the `python` option is not set to `false`, it attempts to load the `metta_python` module
%   and initializes Python integration using `ensure_mettalog_py/0`.
%
%   This predicate currently fails unconditionally due to the initial `fail/0`.
%
%   @see option_value/2
%
%   @example
%     % This predicate always fails:
%     ?- process_python_option.
%     false.
%
%   @note The commented-out version avoids `fail/0` and skips loading `metta_python`.
%

%process_python_option :- option_value('python', false), !, skip(ensure_loaded(metta_python)).
    % If the `python` option is not explicitly set to `false`, load the `metta_python` module.
process_python_option :- process_python_option_now, !.
process_python_option_now :- option_value('python', false),!.
process_python_option_now :-
    %ensure_loaded(('/home/deb12user/metta-wam/prolog-NOW/metta_lang/metta_python')),
    % Initialize Python integration.
    setenv('METTALOG_VERBOSE','0'),
    real_notrace((ensure_mettalog_py)).


%!  process_late_opts is det.
%
%   Handles the processing of late-stage options by iterating through defined options and
%   applying corresponding actions.
%
%   1. Iterates through all option definitions and ensures they are processed.
%   2. If the `html` option is set to `true`, switches the system to unit testing mode.
%   3. Additional behaviors, including interaction with the REPL or halting, are present
%      in commented-out lines.
%
%   @see process_python_option/0
%

process_late_opts:- forall(process_each_late_opt, true).

process_each_late_opt :- nocut,
    % Process all option definitions by calling `process_python_option/0`.
    forall(process_python_option, true).
process_each_late_opt :-
    % If the `html` option is set to `true`, enable unit testing mode.
    once(option_value('html', true)),
    set_is_unit_test(true).
%process_late_opts :- current_prolog_flag(os_argv, [_]), !, ignore(repl).
%process_late_opts :- halt(7).
% Ensure the predicate always succeeds, even if no other clause matches.
%process_late_opts.

%!  do_cmdline_load_metta(+Phase) is det.
%
%   Processes Metta command-line arguments and performs necessary actions based
%   on the specified Phase. This version assumes the context of the current module (`&self`).
%
%   @arg Phase The phase of the command-line processing (e.g., `prescan`, `execute`).
%
do_cmdline_load_metta(Phase) :-
    % Retrieve the remaining command-line arguments.
    metta_cmd_args(Rest), !,
    % Delegate processing to `do_cmdline_load_metta/3` with `&self` as the context.
    do_cmdline_load_metta(Phase, '&self', Rest).

%!  do_cmdline_load_metta(+Phase, +Self, +Rest) is det.
%
%   Processes command-line arguments for the specified Phase and context.
%   Handles late options, loads Metta modules, and processes command-line options.
%
%   @arg Phase The phase of command-line processing (e.g., `prescan`, `execute`).
%   @arg Self  The context or identifier for the current module.
%   @arg Rest  The remaining unprocessed command-line arguments.
%
%   @example
%     % Process command-line arguments for the execution phase:
%     ?- do_cmdline_load_metta(execute, '&self', ['--example=1', '--html=true']).
%

%do_cmdline_load_metta(Phase,_Slf,Rest):- select('--prolog',Rest,RRest),!,
%  set_option_value_interp('prolog',true),
%  set_prolog_flag(late_metta_opts,RRest).
do_cmdline_load_metta(Phase, Self, Rest) :-
    % Store the remaining arguments in the `late_metta_opts` Prolog flag.
    set_prolog_flag(late_metta_opts, Rest),
    % Process all defined options using `process_python_option/0`.
    forall(process_python_option, true), !,
    % Execute the Metta command-line loading logic for the specified Phase and context.
    cmdline_load_metta(Phase, Self, Rest), !,
    % Process any late options, such as enabling specific features.
    process_late_opts.

:- if( \+ current_predicate(load_metta_file/2)).

%!  load_metta_file(+Self, +Filemask) is det.
%
%   Determines the appropriate loading mechanism for a given file based on its extension.
%
%   This predicate checks if the provided `Filemask` matches a file with the `.metta` extension.
%   If so, it calls `load_metta/2` to handle the file. Otherwise, it defaults to `load_flybase/1`.
%
%   The `:- if/1` directive ensures that this predicate is only defined if `load_metta_file/2`
%   is not already defined elsewhere, preventing conflicts during reloading or loading from
%   other files.
%
%   @arg Self The context or "space" in which the file is to be loaded.
%   @arg Filemask The file path or pattern to be loaded. Files ending in `.metta` will be processed
%        differently from other files.
%
%   @example
%     % Load a `.metta` file:
%     ?- load_metta_file('&self', 'example.metta').
%     % Calls load_metta('&self', 'example.metta').
%
load_metta_file(Self, Filemask) :-
    % If the file has a `.metta` extension, call `load_metta/2`.
    symbol_concat(_, '.metta', Filemask), !,
    load_metta(Self, Filemask).
load_metta_file(_Slf, Filemask) :-
    % Otherwise, call `load_flybase/1` to handle the file.
    load_flybase(Filemask).

:- endif.

%!  catch_abort(+From, :Goal) is det.
%
%   Executes a Goal, handling any '$aborted' exceptions gracefully.
%   This is a utility predicate to ensure that if the Goal raises an `$aborted`
%   exception, it logs the error and avoids abrupt termination of the program.
%
%   The first clause is a shorthand for calling `catch_abort/3` with `From` and the
%   same `Goal` as both arguments for `TermV` and `Goal`.
%
%   @arg From  A term describing the origin of the Goal, typically used for debugging
%              or tracing the source of the operation.
%   @arg Goal  The Prolog goal to be executed.
%
%   @example
%     % Execute a goal and handle '$aborted' exceptions:
%     ?- catch_abort(my_source, writeln('Hello, World!')).
%     Hello, World!
%
%     % Handle an '$aborted' exception gracefully:
%     ?- catch_abort(my_source, abort).
%     % Logs: aborted(my_source, abort)
%
catch_abort(From, Goal) :-
    % Redirect to the three-argument version of `catch_abort`.
    catch_abort(From, Goal, Goal).

%!  catch_abort(+From, +TermV, :Goal) is det.
%
%   Executes a Goal, catching and handling any '$aborted' exceptions.
%   If a '$aborted' exception is raised, it logs the `From` and `TermV` using `fbug/1`.
%
%   @arg From   A term describing the origin of the Goal, used for logging.
%   @arg TermV  The term or context associated with the Goal, often the same as the Goal.
%   @arg Goal   The Prolog goal to be executed.
%
%   @example
%     % Catch an '$aborted' exception and log the details:
%     ?- catch_abort(my_source, some_context, abort).
%     % Logs: aborted(my_source, some_context)
%
catch_abort(From, TermV, Goal) :-
    % Use `catch/3` to handle exceptions raised by the Goal.
    catch(
        Goal,               % The goal to execute.
        '$aborted',         % Catch '$aborted' exceptions.
        % Log the exception using `fbug/1`.
        fbug(aborted(From, TermV))
    ).
% done

%!  before_arfer_dash_dash(+Rest, -Args, -NewRest) is det.
%
%   Splits a list of arguments at the first occurrence of `--`.
%   Arguments before `--` are unified with `Args`, and those after `--`
%   are unified with `NewRest`. If `--` is not found, the entire list is
%   treated as `Args`, and `NewRest` is unified with an empty list.
%
%   @arg Rest     The input list of arguments to process.
%   @arg Args     The arguments before the first occurrence of `--`.
%   @arg NewRest  The arguments after the first occurrence of `--`, or an empty list if no `--` exists.
%
%   @example
%     % Example with `--` present in the list:
%     ?- before_arfer_dash_dash([a, b, '--', c, d], Args, NewRest).
%     Args = [a, b],
%     NewRest = [c, d].
%
%     % Example without `--` in the list:
%     ?- before_arfer_dash_dash([a, b, c], Args, NewRest).
%     Args = [a, b, c],
%     NewRest = [].
%
before_arfer_dash_dash(Rest, Args, NewRest) :-
    % Attempt to split the list at `--`.
    append(Args, ['--' | NewRest], Rest) ->
        true; % If `--` is found, split the list accordingly.
        % If `--` is not found, assign the entire list to `Args` and set `NewRest` to an empty list.
        ([] = NewRest, Args = Rest).

%!  cmdline_load_metta(+Phase, +Self, +Args) is det.
%
%   Processes Metta command-line arguments based on the given Phase.
%   Handles various options and directives, including `--args`, `--repl`, `--log`,
%   file loading, and custom commands like `-g` and `-G`.
%
%   @arg Phase The phase of processing (e.g., `prescan`, `execute`).
%   @arg Self  The context or "space" in which to execute commands or load files.
%   @arg Args  A list of command-line arguments to process.
%
%   @example
%     % Example of processing `--args`:
%     ?- cmdline_load_metta(execute, '&self', ['--args', '--file=example.metta']).
%

maybe_do_repl(_Why) :- current_prolog_flag(mettalog_rt, true), !.
maybe_do_repl(_Why) :- flag(cmdline_load_file, X, X), X==0, once(repl).


% Base case: succeed when the argument list is empty.
cmdline_load_metta(_, _, Nil) :- Nil == [], !.
% Handle double-dash (`--`) by skipping it and continuing with the rest of the arguments.
cmdline_load_metta(Phase, Self, ['--' | Rest]) :- !,
    cmdline_load_metta(Phase, Self, Rest).
% Handle `--args` by extracting arguments before `--` and setting them for Metta.
cmdline_load_metta(Phase, Self, ['--args' | Rest]) :- !,
    before_arfer_dash_dash(Rest, Before, NewRest), !,
    set_metta_argv(Before), !,
    cmdline_load_metta(Phase, Self, NewRest).
% Handle `--repl` by entering the REPL during the execution phase.
cmdline_load_metta(Phase, Self, ['--repl' | Rest]) :- !,
    if_phase(Phase, execute, repl), !,
    cmdline_load_metta(Phase, Self, Rest).
% Handle `--log` by switching to MettaLog mode during the execution phase.
cmdline_load_metta(Phase, Self, ['--log' | Rest]) :- !,
    if_phase(Phase, execute, switch_to_mettalog), !,
    cmdline_load_metta(Phase, Self, Rest).
% Handle `-g` by executing the given Prolog term.
cmdline_load_metta(Phase, Self, ['-g', M | Rest]) :- !,
    if_phase(Phase, execute, catch_abort(['-g', M], ((
        read_term_from_atom(M, Term, []),
        ignore(call(Term)))
    ))), !,
    cmdline_load_metta(Phase, Self, Rest).
% Handle `-G` by evaluating a Metta expression within the current context.
cmdline_load_metta(Phase, Self, ['-G', Str | Rest]) :- !,
    current_self(Self),
    if_phase(Phase, execute, catch_abort(['-G', Str], ignore(call_sexpr('!', Self, Str, _S, _Out)))), !,
    cmdline_load_metta(Phase, Self, Rest).

% Handle file loading when the argument does not start with `-`.
cmdline_load_metta(Phase, Self, [Filemask | Rest]) :-
    symbol(Filemask),
    \+ symbol_concat('-', _, Filemask), !,
    if_phase(Phase, execute, cmdline_load_file(Self, Filemask)), !,
    cmdline_load_metta(Phase, Self, Rest).
% Handle command-line options by setting their corresponding values.
cmdline_load_metta(Phase, Self, [M | Rest]) :-
    m_opt(M, Opt),
    is_cmd_option(Opt, M, TF),
    set_option_value_interp(Opt, TF), !,
    cmdline_load_metta(Phase, Self, Rest).
% Handle unrecognized command-line options by logging a warning.
cmdline_load_metta(Phase, Self, [M | Rest]) :-
    format('~N'),
    fbug(unused_cmdline_option(Phase, M)), !,
    cmdline_load_metta(Phase, Self, Rest).


reset_default_flags:-
   forall(option_value_def(A,B), set_option_value_interp(A,B)),
   metta_cmd_args(Rest),
   forall(member(Flag,Rest),process_flag(Flag)).

process_flag(M) :- ignore((symbol(M),
    m_opt(M, Opt),
    is_cmd_option(Opt, M, TF),
    set_option_value_interp(Opt, TF))),!.



%!  install_ontology is det.
%
%   Installs the core ontology by ensuring core library types are loaded.
%   This predicate serves as a setup step for ontology-related operations,
%   preparing the necessary types required by the system.
%
%   @example
%     % Ensure the core library types are installed:
%     ?- install_ontology.
%
install_ontology :-
    ensure_corelib_types.

%!  load_ontology is det.
%
%   Loads the ontology unless the `compile` option is explicitly set to `false`.
%   If the `compile` option is `false`, the predicate succeeds immediately without
%   performing any operations.
%
%   @example
%     % Load ontology when `compile` option is true:
%     ?- load_ontology.
%
load_ontology :-
    % Skip loading if the `compile` option is set to `false`.
    option_value(compile, false), !.
load_ontology.

%!  cmdline_load_file(+Self, +Filemask) is det.
%
%   Loads a file in the given context (`Self`) based on the specified `Filemask`.
%   The predicate attempts to load the file using `load_metta_file/2` and handles
%   any exceptions gracefully using `catch_abort/2`. During execution, it ensures
%   compatibility checks and output flushing.
%
%   @arg Self      The context or "space" in which the file is to be loaded.
%   @arg Filemask  The file path or pattern to be loaded.
%
%   @example
%     % Load a Metta file in the current context:
%     ?- cmdline_load_file('&self', 'example.metta').
%

%cmdline_load_file(Self, Filemask):- is_converting, !,
cmdline_load_file(Self, Filemask) :-
    flag(cmdline_load_file,X,X+1),
    % Construct the source for file loading.
    Src = (user:load_metta_file(Self, Filemask)), !,
    % Use `catch_abort/2` to handle exceptions during file loading.
    catch_abort(Src,
        (
            must_det_ll((
                % Ensure compatibility checks and write debug output.
                not_compatio((nl, write('; '), write_src(Src), nl)),
                % Execute the file loading logic with error handling.
                catch_red(Src),
                !,
                % Flush output streams to ensure sync.
                flush_output
            ))
        )
    ),
    !.

%!  if_phase(+Current, +Phase, :Goal) is det.
%
%   Executes a Goal if the Current phase matches the specified Phase.
%   This predicate is useful for controlling execution flow during specific phases.
%
%   @arg Current The current execution phase.
%   @arg Phase   The target phase to match.
%   @arg Goal    The Prolog goal to execute if the phases match.
%
%   @example
%     % Execute a goal during the `execute` phase:
%     ?- if_phase(execute, execute, writeln('Executing...')).
%     Executing...
%
if_phase(Current, Phase, Goal) :-
    sub_var_safely(Current, Phase) -> call(Goal) ; true.

%!  set_tty_color_term(+TF) is det.
%
%   Configures terminal output to enable or disable color formatting and ensures UTF-8 encoding.
%
%   Updates the current output stream to reflect the `tty` mode based on the input flag.
%
%   @arg TF A boolean indicating whether to enable (`true`) or disable (`false`) color formatting.
%
%   @example
%     % Enable color formatting for the terminal output:
%     ?- set_tty_color_term(true).
%
set_tty_color_term(TF) :-
    current_output(X),
    set_stream(X, tty(TF)),
    set_stream(X, encoding(utf8)),
    set_stream(current_output, tty(TF)),
    set_stream(current_output, encoding(utf8)),
    set_prolog_flag(color_term, TF).

%!  m_opt(+M, -Opt) is det.
%
%   Extracts the option name from a given command-line argument.
%   This predicate handles prefixes like `--`, `--no-`, and `-`.
%
%   @arg M   The raw command-line option (e.g., `--verbose`, `-debug`).
%   @arg Opt The extracted option name (e.g., `verbose`, `debug`).
%
%   @example
%     % Extract the option name from a raw command-line argument:
%     ?- m_opt('--no-debug', Opt).
%     Opt = debug.
%
m_opt(M, Opt) :-
    m_opt0(M, Opt1),
    m_opt1(Opt1, Opt).

%!  m_opt1(+Opt1, -Opt) is det.
%
%   Processes options containing an `=` sign to extract the core option name.
%
%   @arg Opt1 The input option with or without `=` (e.g., `verbose=true`).
%   @arg Opt  The extracted option name (e.g., `verbose`).
%
%   @example
%     % Extract the core option from an option string:
%     ?- m_opt1('verbose=true', Opt).
%     Opt = verbose.
%
m_opt1(Opt1, Opt) :-
    symbolic_list_concat([Opt | _], '=', Opt1).

%!  m_opt0(+M, -Opt) is det.
%
%   Strips prefixes like `--no-`, `--`, or `-` from the given option.
%
%   @arg M   The raw command-line option (e.g., `--no-debug`).
%   @arg Opt The stripped option name (e.g., `debug`).
%
%   @example
%     % Remove prefixes from an option string:
%     ?- m_opt0('--no-debug', Opt).
%     Opt = debug.
%
m_opt0(M, Opt) :-
    symbol_concat('--no-', Opt, M), !.
m_opt0(M, Opt) :-
    symbol_concat('--', Opt, M), !.
m_opt0(M, Opt) :-
    symbol_concat('-', Opt, M), !.


%!  start_html_of(+Filename) is det.
%
%   Prepares the environment for HTML output generation.
%   This includes clearing associated states and resetting the specified output file.
%
%   If no output file is configured via `tee_file/1`, the predicate exits silently.
%   Otherwise, it performs the following:
%   - Resets internal states using `loonit_reset/0`.
%   - Deletes the contents of the output file.
%
%   @arg Filename The name of the file for which HTML output is being prepared.
%
%   @example
%     % Prepare an HTML output file:
%     ?- start_html_of('output.html').
%
start_html_of(_Filename) :-
    % Exit silently if no terminal output file is set.
    \+ tee_file(_TEE_FILE), !.
start_html_of(_Filename) :- !.
start_html_of(_Filename) :-
    must_det_ll((
        S = _,
        % Remove any existing type definitions for the given context.
        %retractall(metta_eq_def(Eq, S, _, _)),
        nop(retractall(metta_type(S, _, _))),
        % Remove any existing atom definitions for the given context.
        %retractall(get_metta_atom(Eq, S, _, _, _)),
        % Reset the internal state to a clean configuration.
        loonit_reset,
        % Retrieve the terminal output file path.
        tee_file(TEE_FILE),
        % Clear the contents of the terminal output file.
        sformat(S, 'cat /dev/null > "~w"', [TEE_FILE]),
        % Debugging output to indicate the action being performed.
        writeln(doing(S)),
        % Execute the shell command to clear the file.
        ignore(shell(S))
    )).

%!  save_html_of(+Filename) is det.
%
%   Finalizes and saves the generated HTML output. If the `tee_file/1` is not configured
%   or there are no results to save (`has_loonit_results/0` is false), the predicate exits silently.
%
%   If HTML generation is required, the following is performed:
%   - Generates a summary using `loonit_report/0`.
%   - Converts ANSI terminal output to HTML using `ansi2html`.
%
%   @arg Filename The name of the output file for saving the HTML content.
%
%   @example
%     % Save the generated HTML output:
%     ?- save_html_of('output.metta').
%
save_html_of(_Filename) :-
    % Exit silently if no terminal output file is set.
    \+ tee_file(_TEE_FILE), !.
save_html_of(_) :-
    % Exit if there are no results or HTML output is disabled.
    \+ has_loonit_results,
    \+ option_value('html', true).
save_html_of(_) :-
    % Generate the summary report if applicable.
    loonit_report, !,
    writeln('<br/> <a href="#" onclick="window.history.back(); return false;">Return to summaries</a><br/>').
save_html_of(_Filename) :- !.
save_html_of(Filename) :-
    must_det_ll((
        % Prepare the HTML output filename.
        file_name_extension(Base, _, Filename),
        file_name_extension(Base, 'metta.html', HtmlFilename),
        % Reset the internal state.
        loonit_reset,
        % Retrieve the terminal output file path.
        tee_file(TEE_FILE),
        % Write the summary report link.
        writeln('<br/> <a href="#" onclick="window.history.back(); return false;">Return to summaries</a><br/>'),
        % Convert ANSI terminal output to HTML.
        sformat(S, 'ansi2html -u < "~w" > "~w" ', [TEE_FILE, HtmlFilename]),
        % Debugging output to indicate the action being performed.
        writeln(doing(S)),
        % Execute the shell command to generate the HTML file.
        ignore(shell(S))
    )).

%!  tee_file(-TEE_FILE) is nondet.
%
%   Determines the path to the terminal output file (`TEE_FILE`).
%   The path is resolved as follows:
%   1. If the environment variable `TEE_FILE` is set, its value is used.
%   2. Otherwise, a default path is constructed using the Metta directory.
%
%   @arg TEE_FILE The resolved path to the terminal output file.
%
%   @example
%     % Retrieve the terminal output file path:
%     ?- tee_file(Path).
%
tee_file(TEE_FILE) :-
    getenv('TEE_FILE', TEE_FILE), !.
tee_file(TEE_FILE) :-
    metta_dir(Dir),
    directory_file_path(Dir, 'TEE.ansi', TEE_FILE), !.

%!  clear_spaces is det.
%
%   Clears all spaces by invoking `clear_space/1` for each relevant context.
%
%   @example
%     % Clear all spaces:
%     ?- clear_spaces.
%
clear_spaces :- clear_space(_).

%!  clear_space(+S) is det.
%
%   Clears all references and types associated with a given space (`S`).
%   This includes retracting asserted atoms and other type-related information.
%
%   @arg S The identifier of the space to clear.
%
%   @example
%     % Clear a specific space:
%     ?- clear_space(my_space).
%
clear_space(S) :-
    % Remove loaded knowledge base entries for the space.
    retractall(user:loaded_into_kb(S, _)),
    % Remove type definitions for the space.
    %retractall(metta_eq_def(_, S, _, _)),
    nop(retractall(metta_type(S, _, _))),
    % Remove asserted atoms for the space.
    retractall(metta_atom_asserted(S, _)).

%!  dcall(:G) is det.
%
%   A direct wrapper for `call/1`.
%   This predicate provides a shorthand for invoking any Prolog goal.
%
%   @arg G The Prolog goal to execute.
%
%   @example
%     % Execute a goal:
%     ?- dcall(writeln('Hello, World!')).
%     Hello, World!
%
dcall(G) :- call(G).

%!  lsm is det.
%
%   Lists metadata associated with all spaces.
%   This includes file references, dynamic types, and definitions for each space.
%
%   @example
%     % List metadata for all spaces:
%     ?- lsm.
%
lsm :- lsm(_).

%!  lsm(+S) is det.
%
%   Lists metadata associated with a specific space (`S`).
%   Displays file references, dynamic types, and definitions related to the space.
%
%   @arg S The identifier of the space for which metadata is listed.
%
%   @example
%     % List metadata for a specific space:
%     ?- lsm(my_space).
%
lsm(S) :-
    % List files associated with the space.
    listing(metta_file(S, _, _)),
    % List dynamic types for the space with color-coded messages.
    %listing(mdyn_type(S, _, _, _)),
    forall(mdyn_type(S, _, _, Src), color_g_mesg('#22a5ff', write_f_src(Src))),
    nl, nl, nl,
    % List definitions for the space with color-coded messages.
    forall(mdyn_defn(S, _, _, Src), color_g_mesg('#00ffa5', write_f_src(Src))),
    %listing(mdyn_defn(S, _, _, _)),
    !.

%!  write_f_src(+H, +B) is det.
%
%   Writes a formatted representation of a source term to the output.
%   This predicate handles two cases:
%   1. If the terms `H` and `B` are structurally identical (`=@=/2`), it writes the term `H`.
%   2. Otherwise, it writes a list containing `=` and the terms `H` and `B`.
%
%   @arg H The first term to format and write.
%   @arg B The second term to format and write.
%
%   @example
%     % Write a single term when `H` and `B` are identical:
%     ?- write_f_src(foo, foo).
%     foo
%
%     % Write a formatted list when `H` and `B` differ:
%     ?- write_f_src(foo, bar).
%     [=,foo,bar]
%
write_f_src(H, B) :-
    % If the terms `H` and `B` are structurally identical, write `H`.
    H=@=B, !,
    write_f_src(H).
write_f_src(H, B) :-
    % Otherwise, write a list containing `=` and the terms `H` and `B`.
    write_f_src(['=', H, B]).

%!  hb_f(+HB, -ST) is nondet.
%
%   Determines a sub-term (`ST`) of `HB` that satisfies specific conditions.
%   The sub-term must:
%   - Be a symbol.
%   - Not be the term `=` or `:`.
%
%   @arg HB The input term from which the sub-term is extracted.
%   @arg ST The sub-term of `HB` that meets the specified conditions.
%
%   @example
%     % Extract a valid sub-term:
%     ?- hb_f(foo(bar, baz), ST).
%     ST = bar.
%
hb_f(HB,ST):- sub_term_safely(ST,HB),(symbol(ST),ST\==(=),ST\==(:)),!.

%!  write_f_src(+HB) is det.
%
%   Writes the formatted representation of a higher-level construct (`HB`).
%   This includes managing the current definition state and invoking the `write_src/1` predicate.
%
%   Steps:
%   1. Extract a sub-term `ST` from `HB` using `hb_f/2`.
%   2. Check the `current_def` option and update it if it differs from `ST`.
%   3. Write the formatted source term using `write_src/1`.
%
%   @arg HB The higher-level construct to process and write.
%
%   @example
%     % Write the formatted source of a term:
%     ?- write_f_src(foo(bar)).
%
write_f_src(HB) :-
    % Extract a sub-term `ST` from `HB`.
    hb_f(HB, ST),
    % Retrieve the current definition state, defaulting to an empty list if not set.
    option_else(current_def, CST, []),
    !,
    % Update the `current_def` option if it differs from `ST`.
    (CST == ST -> true ; (nl, nl, nl, set_option_value_interp(current_def, ST))),
    % Write the source term.
    write_src(HB).

%!  debug_only(:G) is det.
%
%   Executes the goal `G` in debug mode, suppressing any warnings or errors.
%   This predicate ignores warnings and uses `notrace/1` to prevent tracing during execution.
%
%   @arg G The Prolog goal to execute in debug mode.
%
%   @example
%     % Run a goal in debug mode:
%     ?- debug_only(writeln('Debugging...')).
%     Debugging...
%
debug_only(G) :-
    notrace(ignore(catch_warn(G))).

%!  debug_only(+What, :G) is det.
%
%   Executes the goal `G` in debug mode, with a context identifier (`What`).
%   Similar to `debug_only/1`, but includes a `What` argument for contextual debugging.
%
%   @arg What A term describing the context of the debug operation.
%   @arg G    The Prolog goal to execute in debug mode.
%
%   @example
%     % Run a goal in debug mode with context:
%     ?- debug_only(some_context, writeln('Debugging...')).
%
debug_only(_What, G) :-
    ignore((fail, notrace(catch_warn(G)))).

%!  'True' is det.
%
%   A predicate that always succeeds. It is an alias for `true/0`.
%
%   @example
%     % Use the 'True' predicate:
%     ?- 'True'.
%     true.
%
'True' :-
    true.

%!  'False' is det.
%
%   A predicate that always fails. It is an alias for `fail/0`.
%
%   @example
%     % Use the 'False' predicate:
%     ?- 'False'.
%     false.
%
'False' :-
    fail.

%!  'mettalog::vspace-main' is det.
%
%   The main entry point for the MettaLog REPL.
%   This predicate initializes the REPL interface for user interaction.
%
%   @example
%     % Start the MettaLog REPL:
%     ?- 'mettalog::vspace-main'.
%
'mettalog::vspace-main' :-
    repl.

%!  into_underscores(+D, -U) is det.
%
%   Transforms a symbol by replacing hyphens (`-`) with underscores (`_`).
%   If the input is not a symbol, it recursively applies the transformation to its components.
%
%   @arg D The input term to transform.
%   @arg U The transformed term with hyphens replaced by underscores.
%
%   @example
%     % Transform a symbol:
%     ?- into_underscores('some-symbol', Result).
%     Result = 'some_symbol'.
%
into_underscores(D, U) :-
    symbol(D),
    !,
    symbolic_list_concat(L, '-', D),
    symbolic_list_concat(L, '_', U).
into_underscores(D, U) :-
    descend_and_transform(into_underscores, D, U),
    !.

%!  descend_and_transform(+P2, +Input, -Transformed) is det.
%
%   Recursively applies a transformation predicate to all components of a term.
%   Variables remain unchanged. Compound terms are decomposed, their components transformed,
%   and then reconstructed. Non-compound terms are processed using the provided transformation predicate.
%
%   @arg P2          The transformation predicate to apply.
%   @arg Input       The input term to transform.
%   @arg Transformed The transformed term.
%
%   @example
%     % Transform components of a compound term:
%     ?- descend_and_transform(into_underscores, foo('some-symbol', bar-baz), Result).
%     Result = foo('some_symbol', bar_baz).
descend_and_transform(P2, Input, Transformed) :-
    (   var(Input)
    ->  Transformed = Input  % Keep variables as they are
    ;   compound(Input)
    -> (compound_name_arguments(Input, Functor, Args),
        maplist(descend_and_transform(P2), Args, TransformedArgs),
        compound_name_arguments(Transformed, Functor, TransformedArgs))
    ;   (symbol(Input),call(P2,Input,Transformed))
    ->  true % Transform atoms using xform_atom/2
    ;   Transformed = Input  % Keep other non-compound terms as they are
    ).

/*
is_syspred(H,Len,Pred):- notrace(is_syspred0(H,Len,Pred)).
is_syspred0(H,_Ln,_Prd):- \+ symbol(H),!,fail.
is_syspred0(H,_Ln,_Prd):- upcase_atom(H,U),downcase_atom(H,U),!,fail.
is_syspred0(H,Len,Pred):- current_predicate(H/Len),!,Pred=H.
is_syspred0(H,Len,Pred):- symbol_concat(Mid,'!',H), H\==Mid, is_syspred0(Mid,Len,Pred),!.
is_syspred0(H,Len,Pred):- into_underscores(H,Mid), H\==Mid, is_syspred0(Mid,Len,Pred),!.

fn_append(List,X,Call):-
  fn_append1(List,X,ListX),
  into_fp(ListX,Call).





is_metta_data_functor(Eq,F):-
  current_self(Self),is_metta_data_functor(Eq,Self,F).

is_metta_data_functor(Eq,Other,H):-
  metta_type(Other,H,_),
  \+ get_metta_atom(Eq,Other,[H|_]),
  \+ metta_eq_def(Eq,Other,[H|_],_).
*/

%!  is_function(+F) is nondet.
%
%   Checks if `F` is a symbol, determining if it is a function.
%
%   @arg F The term to check.
%
%   @example
%     % Check if 'foo' is a function:
%     ?- is_function(foo).
%     true.
%
is_function(F) :-
    symbol(F).

%!  is_False(+X) is nondet.
%
%   Determines if `X` evaluates to `False`. This is done by:
%   1. Checking if `X` satisfies `is_False1/1`.
%   2. If not, evaluating `X` and checking if the result satisfies `is_False1/1`.
%
%   @arg X The term to evaluate for "false-ness."
%
%   @example
%     % Check if 'False' evaluates to false:
%     ?- is_False('False').
%     true.
%

%is_False(X):- X\=='True', (is_False1(X)-> true ; (eval_H(X,Y),is_False1(Y))).
is_False(Y):- (Y==0;Y=='False'),!.

%!  is_False1(+Y) is nondet.
%
%   A helper predicate for `is_False/1`.
%   Determines if `Y` is one of the following:
%   - `0`
%   - `[]`
%   - `'False'`
%
%   @arg Y The term to check for "false-ness."
%
is_False1(Y) :-
    (Y == 0 ; Y == [] ; Y == 'False').

%!  is_conz(+Self) is nondet.
%
%   Checks if `Self` is a compound term with a list-like structure.
%
%   @arg Self The term to check.
%
%   @example
%     % Check if a term is a cons:
%     ?- is_conz([a, b]).
%     true.
%
is_conz(Self) :-
    compound(Self),
    Self = [_|_].

%!  dont_x(+Term) is nondet.
%
%   Specifies terms that should not be expanded or executed.
%   In this case, prevents evaluation of terms involving a less-than comparison (`<`) within `eval_H`.
%
%   @arg Term The term to check against restricted forms.
%
%   @example
%     % Check if a term is restricted:
%     ?- dont_x(eval_H(1<2, _)).
%     true.
%

%dont_x(eval_H(Depth,Self,metta_if(A<B,L1,L2),R)).
dont_x(eval_H(_<_,_)).

%!  into_fp(+D, -CallAB) is det.
%
%   Transforms a term `D` into a functional program representation `CallAB`.
%   If `D` does not require transformation (checked using `dont_x/1`), it remains unchanged.
%
%   @arg D       The input term to transform.
%   @arg CallAB  The transformed functional program representation.
%
%   @example
%     % Transform a term into functional program representation:
%     ?- into_fp(my_term, Result).
%
into_fp(D, D) :-
    \+ \+ dont_x(D),
    !.
into_fp(ListX, CallAB) :-
    sub_term_safely(STerm, ListX),
    needs_expanded(STerm, Term),
    % copy_term_g(Term, CTerm),  % Original commented-out line
    =(Term, CTerm),
    substM(ListX, CTerm, Var, CallB),
    fn_append1(Term, Var, CallA),
    into_fp((CallA, CallB), CallAB), !.
into_fp(A, A).

%!  needs_expand(+Expand) is nondet.
%
%   Checks if the term `Expand` needs to be expanded.
%   This is determined if `Expand` is a compound term with a functor name starting with `metta_`.
%
%   @arg Expand The term to check.
%
needs_expand(Expand) :-
    compound(Expand),
    functor(Expand, F, N),
    N >= 1,
    symbol_concat(metta_, _, F).

%!  needs_expanded(+Term, -Expand) is nondet.
%
%   Determines if a sub-term of `Term` needs to be expanded.
%   The sub-term must satisfy the following conditions:
%   - It is compound.
%   - It is not a cons-like structure.
%   - It is not an ftVar.
%   - It satisfies `needs_expand/1`.
%
%   @arg Term    The input term containing potential sub-terms.
%   @arg Expand  The sub-term that requires expansion.
%
needs_expanded(eval_H(Term, _), Expand) :-
    !,
    sub_term_safely(Expand, Term),
    compound(Expand),
    Expand\=@=Term,
    compound(Expand),
    \+ is_conz(Expand),
    \+ is_ftVar(Expand),
    needs_expand(Expand).
needs_expanded([A|B], Expand) :-
    sub_term_safely(Expand, [A|B]),
    compound(Expand),
    \+ is_conz(Expand),
    \+ is_ftVar(Expand),
    needs_expand(Expand).

%!  fn_append1(+Term, +X, -Result) is det.
%
%   Appends `X` to a term `Term`, creating a functional program representation.
%
%   @arg Term   The input term.
%   @arg X      The value to append.
%   @arg Result The resulting functional program representation.
%
fn_append1(eval_H(Term, X), X, eval_H(Term, X)) :- !.
fn_append1(Term, X, eval_H(Term, X)).

%!  assert_preds(+Self, +Load, +Preds) is det.
%
%   Asserts predicates (`Preds`) into the current context (`Self`).
%   Handles individual clauses, lists of clauses, and expanded forms.
%
%   This predicate provides a mechanism for asserting clauses into the Prolog database,
%   with additional logic for dynamic and table declarations when `show_transpiler` or
%   tabling options are enabled.
%
%   @arg Self  The context or namespace for the predicates being asserted.
%   @arg Load  An indicator or flag for loading behavior (unused in some clauses).
%   @arg Preds The predicates or list of predicates to assert.
%
%   @example
%     % Assert a single predicate:
%     ?- assert_preds('&self', _Load, (example_pred :- true)).
%
%     % Assert a list of predicates:
%     ?- assert_preds('&self', _Load, [(example_pred1 :- true), (example_pred2 :- fail)]).
%

%assert_preds('&corelib',_Load, _Clause):-  !.
assert_preds(Self,Load,List):- is_list(List),!,maplist(assert_preds(Self,Load),List).
%assert_preds(_Self,_Load,Clause):- assertz(Clause),!.
%assert_preds(_Self,_Load,_Preds):- \+ show_transpiler,!.
assert_preds(Self,Load,Preds):-
  expand_to_hb(Preds,H,_B),
  functor(H,F,A), %trace,
  if_t((false,show_transpiler),
    color_g_mesg_ok('#005288',(
   ignore((
      % \+ predicate_property(H,defined),
      %if_t(is_transpiling,catch_i(dynamic(F,A))),
      if_t( \+ predicate_property(H,defined),
           not_compatio(format('  :- ~q.~n',[dynamic(F/A)]))),
      if_t(option_value('tabling','True'),
           not_compatio(format('  :- ~q.~n',[table(F/A)]))))),
      not_compatio(format('~N~n  ~@',[portray_clause(Preds)]))))),

  %if_t(is_transpiling, if_t( \+ predicate_property(H, static), add_assertion(Self,Preds))),
  % allow errors and warning rather than silently doing nothing as the clause above would have done
  if_t(is_transpiling, add_assertion(Self,Preds)),
  nop(metta_anew1(Load,Preds)).

%!  load_hook(+Load, -Hooked) is det.
%
%   Processes the loading of a file or resource by invoking custom hooks.
%   This predicate attempts to apply all defined hooks for the given `Load`
%   argument. If no hooks are successfully applied, it ensures the operation
%   continues gracefully.
%
%   @arg Load   The item being loaded (e.g., file or module).
%   @arg Hooked The result of applying the load hooks.
%
%   @example
%     % Process a load operation with hooks:
%     ?- load_hook(my_file, Hooked).

%load_hook(_Load,_Hooked):- !.
load_hook(Load,Hooked):-
   ignore(( \+ ((forall(load_hook0(Load,Hooked),true))))),!.

%!  rtrace_on_error(:Goal) is det.
%
%   Executes the given Goal and handles errors using tracing.
%   If the Goal raises an exception, it writes debugging information and
%   re-executes the Goal under the Prolog tracer (`rtrace`).
%
%   @arg Goal The Prolog goal to execute.
%
%   @example
%     % Execute a goal and trace errors:
%     ?- rtrace_on_error(writeln('Hello, World!')).
%
rtrace_on_error(G):- !, call(G).
%rtrace_on_error(G):- catch(G,_,fail).
rtrace_on_error(G):-
  catch_err(G,E,
   (%notrace,
    write_src_uo(E=G),
    %catch(rtrace(G),E,throw(E)),
    catch(rtrace(G),E,throw(give_up(E=G))),
    throw(E))).

%!  rtrace_on_failure(:Goal) is det.
%
%   Executes the given Goal and handles failures using tracing.
%   If the Goal fails, it writes debugging information, enables the Prolog tracer,
%   and re-executes the Goal.
%
%   @arg Goal The Prolog goal to execute.
%
%   @example
%     % Execute a goal and trace failures:
%     ?- rtrace_on_failure(writeln('This will not fail.')).
%
rtrace_on_failure(G):- tracing,!,call(G).
rtrace_on_failure(G):-
  catch_err((G*->true;(write_src_uo(rtrace_on_failure(G)),
                       ignore(rtrace(G)),
                       write_src_uo(rtrace_on_failure(G)),
                       !,fail)),E,
   (%notrace,
    write_src_uo(E=G),
    %catch(rtrace(G),E,throw(E)),
    catch(rtrace(G),E,throw(give_up(E=G))),
    throw(E))).

%!  rtrace_on_failure_and_break(:Goal) is det.
%
%   Executes the given Goal and handles failures using tracing and debugging.
%   If the Goal fails, it writes debugging information, enables the Prolog tracer,
%   re-executes the Goal, and allows the user to inspect the failure using `break/0`.
%
%   @arg Goal The Prolog goal to execute.
%
%   @example
%     % Execute a goal and trace failures, breaking on failure:
%     ?- rtrace_on_failure_and_break(writeln('This may fail.')).
%
rtrace_on_failure_and_break(G):-
    % If tracing is already active, execute the Goal directly.
    tracing, !, call(G).
rtrace_on_failure_and_break(G):-
    % Catch any exceptions during the execution of the Goal.
    catch_err((G *-> true ; (
                       % Log the failure and enable tracing.
                       write_src(rtrace_on_failure(G)),
                       ignore(rtrace(G)),
                       % Log the failure again and enter debugging mode.
                       write_src(rtrace_on_failure(G)),
                       !, break, fail)), E,
       (% notrace
        % Write debugging information about the error.
        write_src_uo(E = G),
        % Re-execute the Goal with tracing enabled.
        % Previously commented-out version:
        % catch(rtrace(G), E, throw(E)),
        catch(rtrace(G), E, throw(give_up(E = G))),
        % Re-throw the original exception.
        throw(E)
       )).

%!  assertion_hb(+Clause, -Self, -Eq, -H, -B) is det.
%
%   Processes various forms of clauses to extract or transform their components.
%   Handles specific patterns like `metta_eq_def/4`, `metta_defn/3`, and others,
%   mapping them into a unified structure of `Self`, `Eq`, `H`, and `B`.
%
%   @arg Clause The input clause to process.
%   @arg Self   The "space" or context in which the clause is defined.
%   @arg Eq     The equality or relationship operator (e.g., `=` or `:-`).
%   @arg H      The head of the clause.
%   @arg B      The body of the clause.
%
%   @example
%     % Process a `metta_eq_def/4` clause:
%     ?- assertion_hb(metta_eq_def('foo', '&self', head, body), Self, Eq, H, B).
%     Self = '&self',
%     Eq = 'foo',
%     H = head,
%     B = body.
%
assertion_hb(metta_eq_def(Eq, Self, H, B), Self, Eq, H, B) :- !.
assertion_hb(metta_defn(Self, H, B), Self, '=', H, B) :- !.
assertion_hb(metta_defn(Eq, Self, H, B), Self, Eq, H, B) :-
    % Ensure the equality operator is valid.
    assertion_neck_cl(Eq), !.
assertion_hb(X, Self, Eq, H, B) :-
    % Attempt to transform `X` into a recognized form and recurse.
    maybe_xform(X, Y), !,
    assertion_hb(Y, Self, Eq, H, B).
assertion_hb(metta_atom_asserted(Self, [Eq, H, B]), Self, Eq, H, B) :-
    % Handle clauses asserted as `metta_atom_asserted`.
    !,
    assertion_neck_cl(Eq),
    !.

%!  assertion_neck_cl(+Eq) is nondet.
%
%   Verifies if the provided equality operator (`Eq`) is valid.
%   Accepts symbolic equality operators such as `=` and `:-`.
%
%   @arg Eq The equality operator to check.
%
%   @example
%     % Check a valid equality operator:
%     ?- assertion_neck_cl('=').
%     true.
%
%     % Fail for invalid operators:
%     ?- assertion_neck_cl(foo).
%     false.
%
assertion_neck_cl(Eq) :-
    % Fail if `Eq` is not a symbol.
    \+ symbol(Eq), !, fail.
assertion_neck_cl('=').
assertion_neck_cl(':-').

%!  load_hook0(+Load, +Assertion) is det.
%
%   Processes a given `Assertion` within the context of a `Load` operation.
%   Transforms the `Assertion` into its components (`Self`, `Eq`, `H`, `B`) using
%   `assertion_hb/5` and then passes these components to `load_hook1/5` for further handling.
%
%   @arg Load       The loading operation context (e.g., file or resource being loaded).
%   @arg Assertion  The assertion to process and handle.
%
%   @example
%     % Example usage:
%     ?- load_hook0(my_load, metta_eq_def('=', self, head, body)).
%
%   @see assertion_hb/5, load_hook1/5
%

%   % load_hook0(_, _) :- \+ show_transpiler, !. % \+ is_transpiling, !.
load_hook0(Load, Assertion) :-
    % Extract components of the assertion using `assertion_hb/5`.
    once(assertion_hb(Assertion, Self, Eq, H, B)),
    % Pass the components to `load_hook1/5` for further processing.
    load_hook1(Load, Self, Eq, H, B).

%!  load_hook1(+Load, +Self, +Eq, +H, +B) is det.
%
%   Handles the processing of assertions (`Eq`, `H`, `B`) within a specific
%   context (`Self`) during a `Load` operation. Converts function-like terms
%   to predicates and asserts them into the knowledge base if the system is ready.
%
%   @arg Load The loading operation context (e.g., file or resource being loaded).
%   @arg Self The "space" or context in which the assertion applies.
%   @arg Eq   The equality or relationship operator (e.g., `=` or `:-`).
%   @arg H    The head of the assertion.
%   @arg B    The body of the assertion.
%
%   @example
%     % Example usage:
%     ?- load_hook1(my_load, '&corelib', '=', head, body).

% load_hook1(_Load, '&corelib', _Eq, _H, _B) :- !.
load_hook1(_, _, _, _, _) :-
    % Skip processing if the `metta_interp` flag is not set to `ready`.
    \+ current_prolog_flag(metta_interp, ready), !.
load_hook1(Load, Self, Eq, H, B) :-
    % Ensure the Metta compiler is ready for use.
    use_metta_compiler,
    % Convert functions to predicates.
    functs_to_preds([Eq, H, B], Preds),
    % Assert the converted predicates into the knowledge base.
    assert_preds(Self, Load, Preds), !.

% old compiler hook
/*
load_hook0(Load,Assertion):-
     assertion_hb(Assertion,Self, Eq, H,B),
     rtrace_on_error(compile_for_assert_eq(Eq, H, B, Preds)),!,
     rtrace_on_error(assert_preds(Self,Load,Preds)).
load_hook0(_,_):- \+ current_prolog_flag(metta_interp,ready),!.
*/
/*
load_hook0(Load,get_metta_atom(Eq,Self,H)):- B = 'True',
       H\=[':'|_], functs_to_preds([=,H,B],Preds),
       assert_preds(Self,Load,Preds).
*/

%!  is_transpiling is nondet.
%
%   Succeeds if the system is in a state where the Metta compiler is being used.
%   This is determined by the availability or readiness of the Metta compiler.
%
%   @example
%     % Check if the system is transpiling:
%     ?- is_transpiling.
%     true.
%
is_transpiling :-
    % Check if the Metta compiler is in use.
    use_metta_compiler.

%!  use_metta_compiler is nondet.
%
%   Succeeds if the `compile` option is set to `full`, indicating that the
%   Metta compiler is fully enabled and operational.
%
%   @example
%     % Verify if the Metta compiler is enabled:
%     ?- use_metta_compiler.
%     true.
%
use_metta_compiler :-
    % Check if the `compile` option is set to `full`.
    option_value('compile', 'full').

%!  preview_compiler is nondet.
%
%   Succeeds if the Metta compiler is enabled or if the `compile` option is set to `true`.
%   This predicate provides a lightweight check to determine the availability of the compiler.
%
%   @example
%     % Preview the compiler status:
%     ?- preview_compiler.
%     true.
%
% preview_compiler :- use_metta_compiler, !.
preview_compiler :-
    % Use the compiler or check if the `compile` option is `true`.
    notrace(use_metta_compiler; option_value('compile', 'true')).

%!  show_transpiler is nondet.
%
%   Succeeds if the transpiler should display output, based on the `code` option.
%   The transpiler is shown if the `code` option is not set to `silent` or if
%   the compiler is in preview mode.
%
%   @example
%     % Check if the transpiler should display output:
%     ?- show_transpiler.
%     true.
%
show_transpiler :-
    % Check if the `code` option has a value other than `silent`.
    option_value('code', Something),
    Something \== silent, !.
show_transpiler :-
    % Fallback to preview mode.
    preview_compiler.

%!  option_switch_pred(-F) is nondet.
%
%   Identifies predicates that act as option switches, based on their naming conventions and source location.
%   A predicate qualifies as an option switch if:
%   - It is defined in the current source file.
%   - Its name starts with one of the prefixes: `is_`, `show_`, or `trace_on_`.
%   - It is not `show_help_options`.
%
%   @arg F The name of a predicate that acts as an option switch.
%
option_switch_pred(F) :-
    % Check if the predicate exists and is defined in the current source file.
    current_predicate(F/0),
    interpreter_source_file(File),
    source_file(F, File),
    % Ensure the predicate name matches one of the prefixes.
    \+ \+ (member(Prefix, ['is_', 'show_', 'trace_on_']), symbol_concat(Prefix, _, F)),
    \+  symbol_concat(_, '0', F), \+  symbol_concat(_, '_', F),
    % Exclude `show_help_options` from the results.
    F \== show_help_options.

%!  do_show_option_switches is det.
%
%   Displays the status (enabled or disabled) of all identified option switch predicates.
%   For each option switch predicate, it prints:
%   - `yes(F)` if the predicate succeeds when called.
%   - `not(F)` if the predicate fails when called.
%
%   @example
%     % Display the status of all option switch predicates:
%     ?- do_show_option_switches.
%     yes(is_debug).
%     not(trace_on_error).
%
do_show_option_switches :-
    % Iterate over all distinct option switch predicates.
    forall(distinct(option_switch_pred(F)),
           % Call the predicate and print its status.
           (call(F) -> writeln(yes(F)) ; writeln(not(F)))).

%!  do_show_options_values is det.
%
%   Displays the current values of non-hidden runtime options, followed by the
%   statuses of all option switches and debugging topics.
%   Hidden options (whose names start with `$`) are excluded from the output.
%
%   @example
%     % Display all current option values and statuses:
%     ?- do_show_options_values.
%     pragma!(option_name, option_value)
%     yes(is_debug)
%     @debug_topic=trace
%
do_show_options_values :-
    % Display all non-hidden option values.
    forall((nb_current(N, V), \+ ((symbol(N), symbol_concat('$', _, N)))),
           write_src_nl(['pragma!', N, V])),
    % Display the status of option switches.
    do_show_option_switches,
    % Display debugging topics.
    display_metta_debug_topics.

%!  display_metta_debug_topics is det.
%
%   Displays all active Metta debugging topics in the format `@topic=trace`.
%   Iterates over all unique debugging topics and prints them to the error stream.
%
%   @example
%     % Display all active debugging topics:
%     ?- display_metta_debug_topics.
%     @debug_topic=trace
%
display_metta_debug_topics :-
    writeln(user_error,' Tracing...'),
    % Iterate over all distinct debugging topics.
    distinct(Sub=TF, debugging(metta(Sub), TF)),
    % Print each debugging topic to the error stream.
    once(tf_to_trace(TF,Tracing)),
    format(user_error, '~N  @~w~n', [Sub=Tracing]),
    fail.
% Ensure the predicate always succeeds, even if no debugging topics are active.
display_metta_debug_topics :- !.
tf_to_trace(true,trace).
tf_to_trace(false,silent).
tf_to_trace(X,X).


% Dynamic and Multifile Declaration: Ensures that predicates can be modified at runtime and extended across
% multiple files.
:- dynamic(metta_atom_asserted/2).
:- multifile(metta_atom_asserted/2).
:- dynamic(metta_atom_deduced/2).
:- multifile(metta_atom_deduced/2).
:- dynamic(metta_atom_in_file/2).
:- multifile(metta_atom_in_file/2).
:- dynamic(metta_atom_asserted_last/2).
:- multifile(metta_atom_asserted_last/2).

%get_metta_atom(Eq,KB, [F|List]):- KB='&flybase',fb_pred(F, Len), length(List,Len),apply(F,List).

%!  maybe_into_top_self(+WSelf, -Self) is nondet.
%
%   Conditionally maps `WSelf` to the current `Self` context if `use_top_self` is enabled.
%   The mapping is deferred if `WSelf` or `Self` is unbound, using `freeze/2`.
%
%   @arg WSelf The working self-reference (`&self` or variable).
%   @arg Self  The resolved self-reference.
%
maybe_into_top_self(_, _) :-
    % Fail if `use_top_self` is not enabled.
    \+ use_top_self, !, fail.
maybe_into_top_self(WSelf, Self) :-
    % Handle unbound `WSelf` with deferred resolution.
    var(WSelf), !, \+ attvar(WSelf), !, freeze(Self, from_top_self(Self, WSelf)).
maybe_into_top_self(WSelf, Self) :-
    % Map `&self` to the current context.
    WSelf = '&self',
    current_self(Self),
    Self \== WSelf, !.

%!  into_top_self(+WSelf, -Self) is det.
%
%   Resolves `WSelf` to `Self` using `maybe_into_top_self/2`. If mapping fails, `Self` defaults to `WSelf`.
%
%   @arg WSelf The working self-reference.
%   @arg Self  The resolved self-reference.
%
into_top_self(WSelf, Self) :-
    % Attempt conditional mapping.
    maybe_into_top_self(WSelf, Self), !.
into_top_self(Self, Self).

%!  from_top_self(+Self, -WSelf) is det.
%
%   Maps `Self` back to its working form (`WSelf`). Handles unbound `Self` using `freeze/2`.
%
%   @arg Self  The resolved self-reference.
%   @arg WSelf The working self-reference.
%
from_top_self(Self, WSelf) :-
    % Handle unbound `Self` with deferred resolution.
    var(Self), !, \+ attvar(Self), !, freeze(Self, from_top_self(Self, WSelf)).
%from_top_self(Self, WSelf):- var(Self), trace, !, freeze(Self, from_top_self(Self, WSelf)).
from_top_self(Self, WSelf) :-
    % Map from top self-reference.
    top_self(CSelf), CSelf == Self, WSelf = '&self', !.
from_top_self(Self, WSelf) :-
    % Map from current self-reference.
    current_self(CSelf), CSelf == Self, WSelf = '&self', !.
from_top_self(Self, Self).

%!  get_metta_atom_from(+KB, -Atom) is nondet.
%
%   Retrieves an atom associated with the specified knowledge base (`KB`).
%
%   @arg KB   The knowledge base context.
%   @arg Atom The atom associated with the knowledge base.
%
get_metta_atom_from(KB, Atom) :-
  o_quietly(metta_atom0(no_inherit,KB, Atom)).


%!  metta_atom(-Atom) is nondet.
%
%   Retrieves an atom associated with the current knowledge base (`current_self/1`).
%
%   @arg Atom The atom associated with the current knowledge base.
%
metta_atom(Atom) :-
    notrace(current_self(KB)),
    metta_atom(KB, Atom).

%!  metta_atom_added(+X, -Y) is nondet.
%
%   Determines if an atom has been added to the specified space (`X`).
%   Checks for assertions, file associations, deductions, or recent assertions.
%
%   @arg X The context or space.
%   @arg Y The atom to check.
%
metta_atom_added(X, Y) :- nocut,
    % Check if the atom was explicitly asserted.
    metta_atom_asserted(X, Y).
metta_atom_added(X, Y) :- nocut,
    % Check if the atom is associated with a file.
    metta_atom_in_file(X, Y).
metta_atom_added(X, Y) :- nocut,
    % Check if the atom was deduced and not explicitly asserted.
    metta_atom_deduced(X, Y),
    \+ clause(metta_atom_asserted(X, Y), true).
metta_atom_added(X, Y) :- nocut,
    % Check if the atom was recently asserted.
    metta_atom_asserted_last(X, Y).

%!  metta_atom(+Space, -Atom) is nondet.
%
%   Retrieves atoms associated with a given space or knowledge base (`Space`).
%   Handles various conditions such as typed lists, predicates from `&flybase`,
%   and atoms added or inherited from other spaces.
%
%   @arg Space The context or knowledge base.
%   @arg Atom  The atom associated with the given space.
%

metta_atom(KB, Atom):-
  o_quietly(metta_atom0(inherit([KB]),KB, Atom)).

metta_atom_fast(KB, Atom):-
  o_quietly(metta_atom0(no_inherit,KB, Atom)).


/*
metta_atom0(Inherit,KB, Fact) :-
   transform_about(Fact, Rule, Cond), Cond=='True',!,
   fact_store(KB, Rule, Fact, Cond).
*/

% metta_atom([Superpose,ListOf], Atom) :-   Superpose == 'superpose',    is_list(ListOf), !,      member(KB, ListOf),    get_metta_atom_from(KB, Atom).
metta_atom0(_Inherit,Space, Atom) :- typed_list(Space, _, L), !, member(Atom, L).
% metta_atom(KB, Atom) :- KB == '&corelib', !, metta_atom_corelib(Atom).
% metta_atom(X, Y) :- use_top_self, maybe_resolve_space_dag(X, XX), !, in_dag(XX, XXX), XXX \== X, metta_atom(XXX, Y).

%metta_atom0(Inherit,X, Y) :- var(X), use_top_self, current_self(TopSelf),  metta_atom0(Inherit,TopSelf, Y), X = '&self'.
metta_atom0(Inherit,X, Y) :- maybe_into_top_self(X, TopSelf), !, metta_atom0(Inherit,TopSelf, Y).

metta_atom0(_Inherit,KB, Atom) :- metta_atom_added(KB, Atom).




metta_atom0(_Inherit,KB, _Atom) :- \+atom(KB), !, fail.
metta_atom0(_Inherit,KB, [F, A | List]) :-
    KB == '&flybase', !, fb_pred_nr(F, Len), current_predicate(F/Len),
    length([A | List], Len), apply(F, [A | List]).
% metta_atom(KB, Atom) :- KB == '&corelib', !, metta_atom_asserted('&self', Atom).
% metta_atom(KB, Atom) :- KB \== '&corelib', using_all_spaces, !, metta_atom('&corelib', Atom).
%metta_atom(KB, Atom) :- KB \== '&corelib', !, metta_atom('&corelib', Atom).
metta_atom0(_Inherit,KB, Atom) :- clause(metta_atomspace(KB,Atom),Body),!, call(Body).

metta_atom0(inherit(Except),KB, Atom) :- inherit_into(KB,KB2), \+ member(KB2,Except), metta_atom0(inherit([KB2|Except]),KB2, Atom).


inherit_into(KB,'&corelib') :- KB=='&top', \+ should_not_inherit_from_corelib(KB).

%  should_inhert_from(KB, Atom).
% metta_atom(KB, Atom) :- metta_atom_asserted_last(KB, Atom).

'same-index'(X,Y):-
  transform_about(X, t(Inst,Type,Pred, Super), Cond), \+ \+ (nonvar(Pred);nonvar(Super)),
  transform_about(Y, t(Inst,Type,Pred, Super), Cond), !.
'same-index'(X,Y):- copy_term(X,Y).

% Direct type association case
transform_about([Colon, Pred, Super],             t(inst,type,Pred, Super), 'True') :-  Colon == ':', !.
% Type association inside an equality assertion
transform_about([Eq, [Colon, Pred, Super], Cond], t(inst,type,Pred, Super), Cond) :-  Eq == '=', Colon == ':', !.
% Subtype relationship
transform_about([Smile, Pred, Super],             t(type,type,Pred, Super), 'True') :-  Smile == ':>', !.
% Subtype relationship inside an equality assertion
transform_about([Eq, [Smile, Pred, Super], Cond], t(type,type,Pred, Super), Cond) :-  Eq == '=',  Smile == ':>',!.

% Proven fact with arguments inside an equality assertion
transform_about([Eq, [Pred | Args], Cond],        t(pred,head,Pred, Args), Cond) :-  Eq == '=', !.
% General proven fact
transform_about([Pred | Args],                    t(pred,fact,Pred, Args), true):- !.
transform_about(PredArgs,                         t(pred,fact,Pred, Args), true):- compound(PredArgs),!, PredArgs=..[Pred | Args],!.
transform_about(Pred,                             t(pred,fact,Pred,_Args), true).

add_indexed_fact(OBO):- arg(1,OBO,KB), arg(2,OBO,Fact), add_fact(KB, Fact),!.

add_fact(KB, Fact):-
   must_det_lls(transform_about(Fact, Rule, Cond)),
   assertz(fact_store(KB, Rule, Fact, Cond)).

query_fact(KB, Fact, Cond) :-
   transform_about(Fact, Rule, Cond),
   fact_store(KB, Rule, Fact, Cond).

query_type_of(KB, Pred, Type, Cond) :-
   fact_store(KB, inst_type(Pred, Type), _, Cond).
query_super_type(KB, Pred, Type, Cond) :-
   fact_store(KB, type_type(Pred, Type), _, Cond).

/*
% Commented-out inheritance exclusions for core libraries.
% Uncomment or modify as needed to apply specific rules for inheritance exclusion.
%should_not_inherit_from_corelib('&self').
%should_not_inherit_from_corelib('&top').
*/
should_not_inherit_from_corelib('&corelib').
should_not_inherit_from_corelib('&stdlib').

%!  should_inherit_atom_from_corelib(+Atom) is nondet.
%
%   Determines whether a specific atom (`Atom`) should be inherited from the `&corelib` space.
%   The decision is based on the current configuration (e.g., `using_all_spaces`) and the structure of the atom.
%
%   @arg Atom The atom to check for inheritance from `&corelib`.
%
%   @example
%     % Check inheritance when all spaces are used:
%     ?- using_all_spaces, should_inherit_atom_from_corelib(my_atom).
%     true.
%
%     % Check inheritance for a structured atom:
%     ?- should_inherit_atom_from_corelib([=, [my_functor, arg1], body]).
%
should_inherit_atom_from_corelib(_) :-
    % Automatically allow inheritance if all spaces are used.
    using_all_spaces, !.
% Default case: inheritance is disallowed unless explicitly permitted.
%should_inherit_atom_from_corelib(Top) :- metta_atom_asserted_last(Top, '&corelib').
% Check if the operator `H` is permitted for inheritance and `A` is nonvar.
should_inherit_atom_from_corelib([H, A | _]) :- nonvar(H), should_inherit_op_from_corelib(H), !, nonvar(A).
% should_inherit_atom_from_corelib([H | _]) :-
%     % Uncomment to allow inheritance for `@doc` headers.
%     H == '@doc', !.
should_inherit_atom_from_corelib([H, A | T]) :-
    % Additional rule for inheritance based on specific conditions.
    fail, % Disabled; uncomment or modify as needed.
    H == '=', write_src_uo(try([H, A | T])), !,
    A = [F | _], nonvar(F), F \== ':', is_list(A),
    % Ensure the functor `F` is not already asserted in `&self`.
    \+ metta_atom_asserted('&self', [:, F | _]),
    % Optionally check if the functor exists in `&corelib`.
    % Uncomment the following line to enforce this check.
    % \+ \+ metta_atom_asserted('&corelib', [=, [F | _] | _]),
    write_src_uo([H, A | T]).

%!  is_code_inheritor(+KB) is nondet.
%
%   Determines whether the current execution context (`KB`) can inherit from a code base.
%   This is true if the current self (`current_self/1`) matches the provided `KB`.
%
%   @arg KB The knowledge base to check for code inheritance.
%
%   @example
%     % Check if the current context is a code inheritor:
%     ?- is_code_inheritor('&corelib').
%     true.
%
is_code_inheritor(KB) :- % Check if the current self matches `KB`.
    current_self(KB).

%!  should_inherit_op_from_corelib(+Op) is nondet.
%
%   Determines whether a specific operator (`Op`) should be inherited from `&corelib`.
%   By default, operators like `:` and `@doc` are considered inheritable.
%
%   @arg Op The operator to check for inheritance from `&corelib`.
%
%   @example
%     % Check if an operator should be inherited:
%     ?- should_inherit_op_from_corelib(':').
%     true.
%
%   Commented-out rules allow customization for other operators.
%

% should_inherit_op_from_corelib('=').  % Uncomment to include the '=' operator.
should_inherit_op_from_corelib(':').
should_inherit_op_from_corelib('@doc').
% should_inherit_op_from_corelib(_).    % Uncomment to allow all operators.

%!  metta_atom_asserted_last(+KB, -Atom) is nondet.
%
%   Retrieves the last asserted atom associated with a specific knowledge base (`KB`).
%   Handles top-level contexts and predefined relationships like `&corelib` and `&stdlib`.
%
%   @arg KB   The knowledge base or context for which the atom is being checked.
%   @arg Atom The last asserted atom associated with the knowledge base.
%
%   @example
%     % Retrieve the last asserted atom for a specific knowledge base:
%     ?- metta_atom_asserted_last('&flybase', Atom).
%     Atom = '&corelib'.


metta_atom_asserted_last(_,_) :- !, fail.
%metta_atom_asserted('&self','&corelib').
%metta_atom_asserted('&self','&stdlib').
metta_atom_asserted_last(Top, '&corelib') :-
    % Assert `&corelib` for the top-level context.
    top_self(Top).
metta_atom_asserted_last(Top, '&stdlib') :-
    % Assert `&stdlib` for the top-level context.
    top_self(Top).
metta_atom_asserted_last('&stdlib', '&corelib').
metta_atom_asserted_last('&flybase', '&corelib').
metta_atom_asserted_last('&flybase', '&stdlib').
metta_atom_asserted_last('&catalog', '&corelib').
metta_atom_asserted_last('&catalog', '&stdlib').

%!  maybe_resolve_space_dag(+Var, +XX) is det.
%
%   Resolves the context or space for a variable `Var` based on the given list `[XX]`.
%   If `Var` is uninstantiated and not an attributed variable, it sets up a freeze
%   on `[XX]`, which triggers the `space_to_ctx/2` predicate when `[XX]` is resolved.
%
%   @arg Var The variable or context to resolve.
%   @arg XX The list containing a potential context to bind `Var` to.
%
maybe_resolve_space_dag(Var, [XX]) :-
    var(Var),                  % Check if `Var` is uninstantiated.
    !,                        % Cut to ensure this clause is used for variables.
    \+ attvar(Var),           % Ensure `Var` is not an attributed variable.
    freeze(XX, space_to_ctx(XX, Var)).  % Set up a freeze to resolve context when `XX` is instantiated.

maybe_resolve_space_dag('&self', [Self]) :-
    current_self(Self).       % If `Var` is '&self', unify `Self` with the current self.

%!  in_dag(+X, -XX) is nondet.
%
%   Checks if `XX` is part of `X`. Handles both list and atomic inputs.
%   If `X` is a list, succeeds if `XX` is a member of the list.
%   If `X` is not a list, succeeds if `X` equals `XX`.
%
%   @arg X The input which may be a list or atomic value.
%   @arg XX The value to check for membership or equality.
%
in_dag(X, XX) :-
    is_list(X),               % Check if `X` is a list.
    !,                        % Cut to avoid backtracking to the next clause.
    member(XX, X).            % Check if `XX` is a member of the list.
in_dag(X, X).                 % If `X` is not a list, succeed if `X` equals `XX`.

%!  space_to_ctx(+Top, -Var) is det.
%
%   Resolves the context for a given `Top` and unifies it with `Var`.
%   If `Top` is the current self context, `Var` is set to '&self'.
%   Otherwise, attempts to map `Top` to a context using 'mod-space'/2.
%
%   @arg Top The input to resolve.
%   @arg Var The resolved context or space.
%
space_to_ctx(Top, Var) :-
    nonvar(Top),              % Ensure `Top` is instantiated.
    current_self(Top),        % Check if `Top` is the current self context.
    !,                        % Cut to ensure this clause is used in this case.
    Var = '&self'.            % Unify `Var` with '&self'.
space_to_ctx(Top, Var) :-
    'mod-space'(Top, Var),    % Attempt to map `Top` to a context using 'mod-space'/2.
    !.
space_to_ctx(Var, Var).       % If no mapping is possible, unify `Var` with itself.

%!  'mod-space'(+Top, -Var) is det.
%
%   Maps specific values of `Top` to corresponding context identifiers.
%   Used by `space_to_ctx/2` to resolve contexts.
%
'mod-space'(top, '&top').
'mod-space'(catalog, '&catalog').
'mod-space'(corelib, '&corelib').
'mod-space'(stdlib, '&stdlib').
'mod-space'(Top, '&self') :- current_self(Top).  % Fallback to '&self' if `Top` matches the current self.

%!  not_metta_atom_corelib(+A, +N) is det.
%
%   Succeeds if `A` is not '&corelib' but `N` is a metta atom associated with '&corelib'.
%   Ensures that the given `A` is distinct from '&corelib' while still linking to a metta
%   atom from the corelib context.
%
%   @arg A The input to verify is not '&corelib'.
%   @arg N The metta atom to check against '&corelib'.
%
not_metta_atom_corelib(A, N) :-
    A \== '&corelib',         % Ensure `A` is not '&corelib'.
    metta_atom('&corelib', N). % Check if `N` is a metta atom in the '&corelib' context.

%metta_atom_asserted_fallback( KB,Atom):- metta_atom_stdlib(KB,Atom)

%metta_atom(KB,[F,A|List]):- metta_atom(KB,F,A,List), F \== '=',!.

%!  is_metta_space(+Space) is nondet.
%
%   Checks if the given `Space` is a valid Metta space.
%   This predicate uses a double negation (`\+ \+`) to enforce a deterministic result.
%   It succeeds if `is_space_type/2` succeeds for the given `Space` with any test.
%
%   The double negation ensures that `is_metta_space/1` behaves deterministically:
%   - It succeeds if `is_space_type/2` succeeds.
%   - It fails if `is_space_type/2` fails.
%
%   @arg Space The space to check for being a valid Metta space.
%
%   @example
%     % Check if a given space is a valid Metta space:
%     ?- is_metta_space('&corelib').
%     true.
%
%     ?- is_metta_space('&unknown_space').
%     false.
%
is_metta_space(Space) :-  nonvar(Space),
    \+ \+ is_space_type(Space, _Test).  % Enforce deterministic behavior using double negation.

%!  metta_eq_def(+Eq, +KB, +H, +B) is det.
%
%   Defines equality (`=`) within a specific knowledge base (`KB`) for given head (`H`)
%   and body (`B`). The predicate first ensures `Eq` is unified with the equality operator (`=`).
%   Then, it attempts to define the equality in the `KB` using `metta_atom/2`.
%   If this fails, it falls back to checking that the equality is not tied to the corelib context
%   via `not_metta_atom_corelib/2`.
%
%   @arg Eq The equality operator, typically '='.
%   @arg KB The knowledge base in which the equality is defined.
%   @arg H  The head of the equality definition.
%   @arg B  The body of the equality definition.
%
%   @example
%     % Define equality in a KB:
%     ?- metta_eq_def('=', 'my_kb', 'head', 'body').
%     true.
%

% metta_eq_def(Eq,KB,H,B):- ignore(Eq = '='),if_or_else(metta_atom(KB,[Eq,H,B]), metta_atom_corelib(KB,[Eq,H,B])).
% metta_eq_def(Eq,KB,H,B):-  ignore(Eq = '='),metta_atom(KB,[Eq,H,B]).
metta_eq_def(Eq, KB, H, B) :-
   ignore(Eq = '='),
  metta_atom0(inherit([KB]),KB,[EQ, H, B]),
  EQ == Eq.

% Original commented-out code, retained as-is for potential future use:
% metta_defn(KB,Head,Body):- metta_eq_def(_Eq,KB,Head,Body).
% metta_defn(KB,H,B):- if_or_else(metta_atom(KB,['=',H,B]),not_metta_atom_corelib(KB,['=',H,B])).

%!  metta_defn(+KB, +H, +B) is det.
%
%   Defines a predicate or function within the given knowledge base (`KB`).
%   The definition uses the equality operator (`=`) to associate `H` (head)
%   with `B` (body).
%
%   @arg KB The knowledge base in which the definition is made.
%   @arg H  The head of the definition.
%   @arg B  The body of the definition.
metta_defn(KB, H, B) :-
    % Use `=` to define the relation in the given knowledge base.
    metta_eq_def('=', KB, H, B).

%!  metta_type(+KB, +H, +B) is det.
%
%   Associates a type (`B`) with a head (`H`) in the given knowledge base (`KB`).
%   This uses the type operator (`:`) for the association.
%
%   @arg KB The knowledge base in which the type association is defined.
%   @arg H  The head being associated with a type.
%   @arg B  The type being associated with the head.

% metta_type(KB,H,B):- if_or_else(metta_atom(KB,[':',H,B]),not_metta_atom_corelib(KB,[':',H,B])).
metta_type(KB, H, B) :-
    % Use `:` to associate the head with a type in the given knowledge base.
    metta_eq_def(':', KB, H, B).
% metta_type(S,H,B):- S == '&corelib', metta_atom_stdlib_types([':',H,B]).

% typed_list(Cmpd,Type,List):-  compound(Cmpd), Cmpd\=[_|_], compound_name_arguments(Cmpd,Type,[List|_]),is_list(List).

%metta_atom_corelib(KB,Atom):- KB\='&corelib',!,metta_atom('&corelib',Atom).

%!  maybe_xform(+Original, -Transformed) is nondet.
%
%   Transforms Metta constructs into a normalized or alternative representation.
%   This predicate handles various forms of Metta terms, converting them into
%   their respective representations or normal forms for further processing.
%
%   @arg Original   The original Metta term to be transformed.
%   @arg Transformed The transformed or normalized version of the term.

% maybe_xform(metta_atom(KB,[F,A|List]),metta_atom(KB,F,A,List)):- is_list(List),!.
% Transform `metta_eq_def` into a `metta_atom` representation.
maybe_xform(metta_eq_def(Eq, KB, Head, Body), metta_atom(KB, [Eq, Head, Body])).
% Transform `metta_defn` into a `metta_atom` representation using `=` as the operator.
maybe_xform(metta_defn(KB, Head, Body), metta_atom(KB, ['=', Head, Body])).
% Transform `metta_type` into a `metta_atom` representation using `:` as the operator.
maybe_xform(metta_type(KB, Head, Body), metta_atom(KB, [':', Head, Body])).
% Transform `metta_atom` into its asserted form.
maybe_xform(metta_atom(KB, HeadBody), metta_atom_asserted(KB, HeadBody)).
% Transform `metta_atom_in_file` into its asserted form.
maybe_xform(metta_atom_in_file(KB, HB), metta_atom_asserted(KB, HB)).
% Transform `metta_atom_deduced` into its asserted form.
maybe_xform(metta_atom_deduced(KB, HB), metta_atom_asserted(KB, HB)).
% Transform `metta_atom_asserted_last` into its asserted form.
maybe_xform(metta_atom_asserted_last(KB, HB), metta_atom_asserted(KB, HB)).
% Transform a workspace knowledge base (`WKB`) into a top-level knowledge base (`KB`).
maybe_xform(metta_atom_asserted(WKB, HB), metta_atom_asserted(KB, HB)) :-
    % Convert `WKB` into `KB` using `maybe_into_top_self/2`.
    maybe_into_top_self(WKB, KB),
    !.
% Catch-all clause: fail if no transformation applies.
maybe_xform(_OBO, _XForm) :-
    !, fail.

%!  metta_anew1(+Load, +OBO) is det.
%
%   Handles different operations (`Load`, `unload`, `unload_all`) on Metta objects (`OBO`).
%   Depending on the mode or type of the input, it applies the appropriate transformation
%   and performs actions like adding, removing, or processing atoms in the knowledge base.
%
%   @arg Load The operation mode (e.g., `load`, `unload`, `unload_all`).
%   @arg OBO  The object being processed (e.g., `metta_atom(Space, Atom)`).

% If `Load` is unbound, start tracing to diagnose the issue.
metta_anew1(Load, _OBO) :-
    var(Load), % Check if `Load` is uninstantiated.
    trace,     % Enable tracing for debugging.
    !.
% Resolve the mode for `Ch` using `metta_interp_mode/2`, then recurse with the resolved mode.
metta_anew1(Ch, OBO) :-
    metta_interp_mode(Ch, Mode), % Determine the mode for `Ch`.
    Ch\=@=Mode, !,
    metta_anew1(Mode, OBO).      % Recurse with the resolved mode.
% Attempt to transform `OBO` using `maybe_xform/2`, then recurse with the transformed form.
metta_anew1(Load, OBO) :-
    maybe_xform(OBO, XForm),     % Transform `OBO` if possible.
    OBO \=@= XForm, !,
    metta_anew1(Load, XForm).    % Recurse with the transformed form.
% Handle `load` operation for `metta_atom`.
metta_anew1(load, OBO) :-
    OBO = metta_atom(Space, Atom), % Match the structure of `OBO`.
    !,
    'add-atom'(Space, Atom), !.       % Add the atom to the specified space.
% Handle `unload` operation for `metta_atom`.
metta_anew1(unload, OBO) :-
    OBO = metta_atom(Space, Atom), % Match the structure of `OBO`.
    !,
    'remove-atom'(Space, Atom).    % Remove the atom from the specified space.
% Handle `unload_all` for all `metta_atom` objects.
metta_anew1(unload_all, OBO) :-
    OBO = forall(metta_atom(Space, Atom), ignore('remove-atom'(Space, Atom))). % Remove all atoms.

% Default `load` operation with hooks and PFC integration and error handling.
metta_anew1(load, OBO) :-  !,
    must_det_lls((
        load_hook(load, OBO),         % Execute the load hook.
        subst_vars(OBO, Cl),          % Substitute variables in `OBO`.
        % display(obo(OBO)), display(cl(Cl)),
        add_indexed_fact(Cl),
        pfcAdd_Now(Cl)  % Add the clause and show errors if any.
    )),!.
% Handle `unload` by erasing matching clauses.
metta_anew1(unload, OBO) :-
    subst_vars(OBO, Cl),          % Substitute variables in `OBO`.
    load_hook(unload, OBO),       % Execute the unload hook.
    expand_to_hb(Cl, Head, Body), % Expand to head and body.
    predicate_property(Head, number_of_clauses(_)), % Check if the predicate has clauses.
    ignore((
        clause(Head, Body, Ref),   % Find a clause matching the head and body.
        clause(Head2, Body2, Ref), % Retrieve the clause again for validation.
        (Head + Body) =@= (Head2 + Body2), % Check if the clauses are equivalent.
        erase(Ref),               % Erase the clause.
        if_trace(atomspace,pp_m(unload(Cl)))          % Log the unload operation.
    )), !.
% Handle `unload_all` by retracting all matching clauses.
metta_anew1(unload_all, OBO) :-
    !,
    must_det_ll((
        load_hook(unload_all, OBO),  % Execute the unload_all hook.
        subst_vars(OBO, Cl),         % Substitute variables in `OBO`.
        if_trace(atomspace,once_writeq_nl_now(yellow, retractall(Cl))), % Log and retract all matching clauses.
        retractall(Cl)      %to_metta(Cl).
    )).
% Alternative `unload_all` operation with detailed clause handling.
metta_anew1(unload_all, OBO) :-
    subst_vars(OBO, Cl),           % Substitute variables in `OBO`.
    load_hook(unload_all, OBO),    % Execute the unload_all hook.
    expand_to_hb(Cl, Head, Body),  % Expand to head and body.
    predicate_property(Head, number_of_clauses(_)), % Check if the predicate has clauses.
    forall(
        (clause(Head, Body, Ref), clause(Head2, Body2, Ref)), % Iterate over all matching clauses.
        must_det_ll((
            ((Head + Body) =@= (Head2 + Body2)) -> % Check if the clauses are equivalent.
                (erase(Ref), nop(pp_m(unload_all(Ref, Cl)))) % Erase and log equivalent clauses.
            ;
                (pp_m(unload_all_diff(Cl, (Head + Body) \=@= (Head2 + Body2)))) % Log differences.
        ))
    ).

/*
metta_anew2(Load,_OBO):- var(Load),trace,!.
metta_anew2(Load,OBO):- maybe_xform(OBO,XForm),!,metta_anew2(Load,XForm).
metta_anew2(Ch,OBO):-  metta_interp_mode(Ch,Mode), !, metta_anew2(Mode,OBO).
metta_anew2(load,OBO):- must_det_ll((load_hook(load,OBO),subst_vars_not_last(OBO,Cl),assertz_if_new(Cl))). %to_metta(Cl).
metta_anew2(unload,OBO):- subst_vars_not_last(OBO,Cl),load_hook(unload,OBO),
  expand_to_hb(Cl,Head,Body),
  predicate_property(Head,number_of_clauses(_)),
  ignore((clause(Head,Body,Ref),clause(Head2,Body2,Ref),(Head+Body)=@=(Head2+Body2),erase(Ref),pp_m(Cl))).
metta_anew2(unload_all,OBO):- subst_vars_not_last(OBO,Cl),load_hook(unload_all,OBO),
  expand_to_hb(Cl,Head,Body),
  predicate_property(Head,number_of_clauses(_)),
  forall((clause(Head,Body,Ref),clause(Head2,Body2,Ref),(Head+Body)=@=(Head2+Body2),erase(Ref),pp_m(Cl)),true).
*/

%!  metta_anew(+Load, +Src, +OBO) is det.
%
%   Processes operations (`Load`) on Metta objects (`OBO`) with source information (`Src`).
%   This predicate delegates tasks to `metta_anew1/2` after handling transformations,
%   modes, and logging or output behavior.
%
%   @arg Load The operation mode (e.g., `load`, `unload`, etc.).
%   @arg Src  The source context or description for the operation.
%   @arg OBO  The object being processed.
metta_anew(Load, Src, OBO) :- % Transform `OBO` if possible and retry with the transformed version.
    maybe_xform(OBO, XForm),  % Attempt to transform `OBO`.
    !,
    metta_anew(Load, Src, XForm).  % Recur with the transformed object.
% Resolve the mode for `Ch` using `metta_interp_mode/2`, then retry with the resolved mode.
metta_anew(Ch, Src, OBO) :-
    metta_interp_mode(Ch, Mode),  % Determine the mode for `Ch`.
    !,
    metta_anew(Mode, Src, OBO).   % Recur with the resolved mode.
% If silent loading is enabled, process the object without additional output.
metta_anew(Load, _Src, OBO) :-
    silent_loading,  % Check if silent loading is active.
    !,
    metta_anew1(Load, OBO).  % Directly delegate to `metta_anew1/2`.
% Default handling with output and logging behavior.
metta_anew(Load, Src, OBO) :-
    % Handle non-compatible I/O operations.
    not_compat_io((
        % Output information about the source if in Metta language.
        output_language(metta, (
            if_trace((atomspace;loading), color_g_mesg('#ffa500', ((
                format('~N '),  % Newline for separation.
                  % format('~N'),
                nop(copy_term(Src,OSrc,Names)),
                materialize_vns(Src,SrcVns),
                write_src(SrcVns),  % Display source information.
                format('~N '),  % Newline for separation.
                nop((ignore(Src=OSrc),
                writeq(Load = Names),  % Display the operation and object.
                format('~N ')))  % Newline for separation.
            ))))
        )),
        % Output information about the operation and object.
        output_language(prolog, (
            if_trace((atomspace;loading), color_g_mesg('#4f4f0f', (((
                write('; Action: '),  % Indicate the action being performed.
                copy_term(OBO,OBOS,VarNames),
                materialize_vns(OBO,OBOVns),
                writeq(Load = OBOVns),  % Display the operation and object.
                %nl  % Newline for clarity.
                format('~N '),  % Newline for separation.
                nop((ignore(OBO=OBOS),
                writeq(cr = VarNames),  % Display the operation and object.
                format('~N ')))  % Newline for separation.
            )))
        )))),
        true  % Ensure successful execution of all output steps.
    )),
    % Perform the main operation using `metta_anew1/2`.
    metta_anew1(Load, OBO),
    % Add a final newline for output separation.
    not_compat_io((format('~N'))).

%!  subst_vars_not_last(+A, -B) is det.
%
%   Substitutes variables in `A` to produce `B`, while ensuring that the last
%   argument of `B` matches the corresponding last argument of `A`.
%   This predicate uses `subst_vars/2` for the substitution process and ensures
%   that the last argument remains unchanged by reassigning it using `nb_setarg/3`.
%
%   @arg A The input term with potential variables to substitute.
%   @arg B The output term with variables substituted and the last argument restored.
subst_vars_not_last(A, B) :-
    % Get the arity (number of arguments) of the term `A`.
    functor(A, _F, N),
    % Retrieve the last argument (`E`) of the term `A`.
    arg(N, A, E),
    % Perform the variable substitution on the term `A` to create `B`.
    subst_vars(A, B),
    % Restore the last argument of `B` to match the last argument of `A`.
    nb_setarg(N, B, E),
    % Ensure determinism by cutting any alternative solutions.
    !.

%!  con_write(+W) is det.
%
%   Writes the term `W` to the current output stream if silent loading is not active.
%   Ensures compatibility with I/O operations.
%
%   @arg W The term to write.
con_write(W) :-
    % Check if silent loading is active; skip writing if true.
    check_silent_loading,
    % Perform the write operation if compatible with I/O settings.
    not_compat_io((write(W))).

%!  con_writeq(+W) is det.
%
%   Writes the term `W` to the current output stream using quoted format,
%   if silent loading is not active. Ensures compatibility with I/O operations.
%
%   @arg W The term to write in quoted format.
con_writeq(W) :-
    % Check if silent loading is active; skip writing if true.
    check_silent_loading,
    % Perform the write operation in quoted format if compatible with I/O settings.
    not_compat_io((writeq(W))).

%!  writeqln(+Q) is det.
%
%   Writes the term `Q` in quoted format followed by a newline.
%   Ensures compatibility with I/O operations and silent loading settings.
%
%   @arg Q The term to write in quoted format with a newline.
writeqln(Q) :-
    % Check if silent loading is active; skip writing if true.
    check_silent_loading,
    % Write a space, the term `Q` in quoted format, and a newline.
    not_compat_io((write(' '), con_writeq(Q), connl)).

%!  into_space(+Self, +Myself, -SelfO) is det.
%
%   Resolves the space identifier (`SelfO`) based on `Self` and `Myself`.
%   If `Myself` is `&self`, `SelfO` is set to `Self`.
%   Otherwise, `SelfO` is set to `Myself`.
%
%   @arg Self   The current space identifier.
%   @arg Myself The candidate space identifier.
%   @arg SelfO  The resolved space identifier.
into_space(Self, '&self', Self) :- !.
into_space(_, Other, Other) :- !.
into_space(Self, Myself, SelfO) :-
    % Use a default depth of 30 when calling the arity-4 version.
    into_space(30, Self, Myself, SelfO).

%!  into_space(+Depth, +Self, +Myself, -SelfO) is det.
%
%   Resolves the space identifier (`SelfO`) based on depth, `Self`, and `Myself`.
%   Handles specific cases for `&self` and `None`.
%
%   @arg Depth  The current depth for evaluation.
%   @arg Self   The current space identifier.
%   @arg Myself The candidate space identifier.
%   @arg SelfO  The resolved space identifier.
into_space(_Dpth, Self, Myself, Self) :-
    % If `Myself` is `&self`, resolve to `Self`.
    Myself == '&self',
    !.
into_space(_Dpth, Self, None, Self) :-
    % If `Myself` is `None`, resolve to `Self`.
    'None' == None,
    !.
into_space(Depth, Self, Other, Result) :-
    % Evaluate the result using the `eval_H/4` predicate.
    eval_H(Depth, Self, Other, Result).

%!  into_name(+Self, +Other, -Other) is det.
%
%   Resolves a name based on the input `Other`.
%   If no specific handling is needed, the name remains unchanged.
%
%   @arg Self  The current self identifier (unused).
%   @arg Other The input name or identifier.
%   @arg Other The resolved name, same as the input.
into_name(_, Other, Other).

%eval_f_args(Depth,Self,F,ARGS,[F|EARGS]):- maplist(eval_H(Depth,Self),ARGS,EARGS).

%!  combine_result(+TF, +R2, -Result) is det.
%
%   Combines two results (`TF` and `R2`) into a single result.
%   If `TF` is an empty list, the result is `R2`. Otherwise, the result is `TF`.
%
%   @arg TF     The first result to combine.
%   @arg R2     The second result to combine.
%   @arg Result The combined result.
combine_result(TF, R2, R2) :-
    % If `TF` is an empty list, unify `Result` with `R2`.
    TF == [],
    !.
combine_result(TF, _, TF) :-
    % Otherwise, unify `Result` with `TF`.
    !.

%!  do_metta1_e(+Self, +LoadExec, +Term) is det.
%
%   Processes Metta expressions or terms based on their structure.
%   Supports special handling for executable terms, equality expressions, and generic terms.
%
%   @arg Self     The current self context (unused here).
%   @arg LoadExec The execution context or mode (used for specific terms).
%   @arg Term     The term to process.
do_metta1_e(_Self, _, exec(Exec)) :-
    % If the term is an executable expression, write the execution representation.
    !,write_exec(Exec),!.
do_metta1_e(_Self, _, [=, A, B]) :-
    % If the term is an equality expression, write it in structured format.
    !,
    with_concepts(false, (
        % Begin writing the equality expression.
        con_write('(= '),
        % Write the left-hand side (`A`) with no indentation.
        with_indents(false, write_src(A)),
        % Add a newline if `B` is a list.
        (is_list(B) -> connl ; true),
        % Write the right-hand side (`B`) with indentation.
        con_write(' '),
        with_indents(true, write_src(B)),
        % Close the equality expression.
        con_write(')')
    )),
    connl.
do_metta1_e(_Self, _LoadExec, Term) :-
    % For other terms, simply write the term and append a newline.
    write_src(Term),
    connl.

%!  write_exec(+Exec) is det.
%
%   Writes the representation of an executable term (`Exec`).
%   Ensures tracing is disabled during the operation.
%
%   @arg Exec The executable term to write.
write_exec(Exec) :-
    % Call the helper predicate `write_exec0/1` without tracing.
    real_notrace(write_exec0(Exec)).

% Original commented-out code retained for potential future use:
% write_exec0(Exec):- symbol(Exec),!,write_exec0([Exec]).

%!  write_exec0(+Exec) is det.
%
%   Writes a structured representation of the executable term (`Exec`).
%   Updates the execution source value and logs the formatted output.
%
%   @arg Exec The executable term to write.
write_exec0(Exec) :-
    % Generate the structured representation of the execution term.
    wots(S, write_src(exec(Exec))),
    % Update the execution source value for tracking.
    nb_setval(exec_src, Exec),
    % Output the structured representation with formatting.
    not_compatio((
        format('~N'),  % Insert a newline.
        output_language(metta, ignore((
            notrace((color_g_mesg('#0D6328', writeln(S))))))))).

%!(let* (( ($a $b) (collapse (get-atoms &self)))) ((bind! &stdlib $a) (bind! &corelib $b)))

%!  asserted_do_metta(+Space, +Ch, +Src) is det.
%
%   Executes a Metta command (`Ch`) within the given space (`Space`) using the source (`Src`).
%   Determines the interpretation mode for `Ch` and delegates processing accordingly.
%
%   @arg Space The space in which the command is executed.
%   @arg Ch    The command or operation to interpret and execute.
%   @arg Src   The source input or term associated with the command.
asserted_do_metta(Space, Ch, Src) :-
    % Resolve the interpretation mode for `Ch` and recurse with the resolved mode.
    metta_interp_mode(Ch, Mode),
    !,
    asserted_do_metta(Space, Mode, Src).
asserted_do_metta(Space, Load, Src) :-
    % If the mode is `exec`, handle execution using the `do_metta_exec/4` predicate.
    Load == exec,
    !,
    do_metta_exec(python, Space, Src, _Out).
asserted_do_metta(Space, Load, Src) :-
    % Delegate to `asserted_do_metta2/4` for other modes.
    asserted_do_metta2(Space, Load, Src, Src).

%!  asserted_do_metta2(+Space, +Ch, +Info, +Src) is det.
%
%   Handles Metta commands with additional information (`Info`) and source input (`Src`).
%   Resolves the interpretation mode for the command (`Ch`) and delegates accordingly.
%
%   @arg Space The space in which the command is executed.
%   @arg Ch    The command or operation to interpret and execute.
%   @arg Info  Additional context or information associated with the command.
%   @arg Src   The source input or term associated with the command.
asserted_do_metta2(Space, Ch, Info, Src) :-
    % Resolve the interpretation mode for `Ch` and recurse with the resolved mode.
    nonvar(Ch),
    metta_interp_mode(Ch, Mode),
    !,
    asserted_do_metta2(Space, Mode, Info, Src).
/*
asserted_do_metta2(Self,Load,[TypeOp,Fn,Type], Src):- TypeOp == ':',  \+ is_list(Type),!,
 must_det_ll((
  color_g_mesg_ok('#ffa501',metta_anew(Load,Src,metta_atom(Self,[':',Fn,Type]))))),!.

asserted_do_metta2(Self,Load,[TypeOp,Fn,TypeDecL], Src):- TypeOp == ':',!,
 must_det_ll((
  decl_length(TypeDecL,Len),LenM1 is Len - 1, last_element(TypeDecL,LE),
  color_g_mesg_ok('#ffa502',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecL]))),
  metta_anew1(Load,metta_arity(Self,Fn,LenM1)),
  arg_types(TypeDecL,[],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),!,
  metta_anew1(Load,metta_last(Self,Fn,LE)))).
*/
/*
asserted_do_metta2(Self,Load,[TypeOp,Fn,TypeDecL,RetType], Src):- TypeOp == ':',!,
 must_det_ll((
  decl_length(TypeDecL,Len),
  append(TypeDecL,[RetType],TypeDecLRet),
  color_g_mesg_ok('#ffa503',metta_anew(Load,Src,metta_atom(Self,[':',Fn,TypeDecLRet]))),
  metta_anew1(Load,metta_arity(Self,Fn,Len)),
  arg_types(TypeDecL,[RetType],EachArg),
  metta_anew1(Load,metta_params(Self,Fn,EachArg)),
  metta_anew1(Load,metta_return(Self,Fn,RetType)))),!.
*/
/*do_metta(File,Self,Load,PredDecl, Src):-fail,
   metta_anew(Load,Src,metta_atom(Self,PredDecl)),
   ignore((PredDecl=['=',Head,Body], metta_anew(Load,Src,metta_eq_def(Eq,Self,Head,Body)))),
   ignore((Body == 'True',!,do_metta(File,Self,Load,Head))),
   nop((fn_append(Head,X,Head), fn_append(PredDecl,X,Body),
   metta_anew((Head:- Body)))),!.*/
/*
asserted_do_metta2(Self,Load,[EQ,Head,Result], Src):- EQ=='=', !,
 color_g_mesg_ok('#ffa504',must_det_ll((
    discover_head(Self,Load,Head),
    metta_anew(Load,Src,metta_eq_def(EQ,Self,Head,Result)),
    discover_body(Self,Load,Result)))).
*/
asserted_do_metta2(Self, Load, PredDecl, Src) :-
    % Discover the head of the predicate declaration (commented-out code retained).
    % ignore(discover_head(Self, Load, PredDecl)),
    % Execute the command and indicate success with a color-coded message.
    color_g_mesg_ok('#ffa505', metta_anew(Load, Src, metta_atom(Self, PredDecl))).

%!  never_compile(+X) is det.
%
%   Determines whether a given term (`X`) should never be compiled.
%   This is based on the execution mode (interpreted) or specific conditions.
%
%   @arg X The term to check.
never_compile(_):- option_value('exec',interp),!.
%never_compile(X):- always_exec(X).
toplevel_interp_only([S|_]):-  symbol(S), toplevel_interp_only_symbol(S).
toplevel_interp_only_symbol('import!').
toplevel_interp_only_symbol('extend-py!').
toplevel_interp_only_symbol('include').
toplevel_interp_only_symbol('include!').
toplevel_interp_only_symbol(H):- symbol_concat('add-atom',_,H),!.

%!  always_exec(+W) is nondet.
%
%   Determines if a given term (`W`) should always be executed rather than compiled.
%   Handles various conditions to evaluate whether execution is appropriate.
%
%   @arg W The term to evaluate.
always_exec(W):- var(W),!,fail.
always_exec([H|_]):- always_exec_symbol(H),!.
always_exec(Comp):- compound(Comp),compound_name_arity(Comp,Name,N),symbol_concat('eval',_,Name),Nm1 is N-1, arg(Nm1,Comp,TA),!,always_exec(TA).
always_exec([H|_]):- always_exec_symbol(H),!.
always_exec(List):- \+ is_list(List),!,fail.
always_exec([Var|_]):- \+ symbol(Var),!,fail.
always_exec(['extend-py!'|_]):- !, fail.
always_exec(['assertEqualToResult'|_]):-!,fail.
always_exec(['assertEqual'|_]):-!,fail.
always_exec(_):-!,fail. % everything else

%!  always_exec_symbol(+Sym) is nondet.
%
%   Determines if a given symbol (`Sym`) should always be executed.
%   Checks for specific symbol patterns, such as those with a `!` suffix.
%
%   @arg Sym The symbol to evaluate.
always_exec_symbol(Sym):- \+ symbol(Sym),!,fail.
always_exec_symbol(H):- always_exec_symbol_except(H),!.
always_exec_symbol(H):- symbol_concat(_,'!',H),!. %pragma!/print!/transfer!/bind!/include! etc
always_exec_symbol(H):- symbol_concat('add-atom',_,H),!.
always_exec_symbol(H):- symbol_concat('remove-atom',_,H),!.
always_exec_symbol(H):- symbol_concat('subst-',_,H),!.

always_exec_symbol_except('die!').

%!  file_hides_results(+List) is nondet.
%
%   Checks if the given list indicates that results should be hidden.
%   Specifically, looks for the presence of the `pragma!` keyword as the first element.
%
%   @arg List The list to check.
file_hides_results([W|_]) :-
    % Succeed if the first element of the list is `pragma!`.
    W == 'pragma!'.

%!  if_t(+A, +B, +C) is det.
%
%   Executes conditional logic while tracing. Calls itself recursively with modified arguments.
%
%   @arg A The condition to check.
%   @arg B The true branch to execute if `A` succeeds.
%   @arg C The alternative branch to execute if `A` fails.
if_t(A, B, C) :-
    % Enable tracing for debugging and execute conditional logic.
    trace,
    if_t((A, B), C).

%!  check_answers_for(+TermV, +Ans) is nondet.
%
%   Validates if answers (`Ans`) for a given term (`TermV`) can be checked.
%   Various conditions are applied to ensure that certain cases (e.g., suspended answers,
%   bad types, or invalid terms) are rejected.
%
%   @arg TermV The term being checked.
%   @arg Ans   The answer to validate.
check_answers_for(_, _) :-
    % Fail if answers are suspended (tracked by `suspend_answers` flag).
    nb_current(suspend_answers, true),
    !,
    fail.
check_answers_for(TermV, Ans) :-
    % Fail if `TermV` is a string, or if `Ans` or `TermV` is uninstantiated.
    (string(TermV); var(Ans); var(TermV)),
    !,
    fail.
check_answers_for(TermV, _) :-
    % Fail if `TermV` contains "assert" (used to identify test-like structures).
    sformat(S, '~q', [TermV]),
    atom_contains(S, "[assert"),
    !,
    fail.
check_answers_for(_, Ans) :-
    % Fail if the answer contains the variable `BadType`.
    contains_var('BadType', Ans),
    !,
    fail.
check_answers_for(TermV, _) :-
    % Fail if `TermV` involves an assertion with executable content.
    inside_assert(TermV, BaseEval),
    always_exec(BaseEval),
    !,
    fail.
% check_answers_for([TermV],Ans):- !, check_answers_for(TermV,Ans).
% check_answers_for(TermV,[Ans]):- !, check_answers_for(TermV,Ans).
% Default case: succeed without further checks.
check_answers_for(_, _).

    /*
got_exec_result2(Val,Nth,Ans):- is_list(Ans), exclude(==(','),Ans,Ans2), Ans\==Ans2,!,
  got_exec_result2(Val,Nth,Ans2).
got_exec_result2(Val,Nth,Ans):-
 must_det_ll((
  Nth100 is Nth+100,
  get_test_name(Nth100,TestName),
  nb_current(exec_src,Exec),
  if_t( ( \+ is_unit_test_exec(Exec)),
  ((equal_enough(Val,Ans)
     -> write_pass_fail_result_now(TestName,exec,Exec,'PASS',Ans,Val)
      ; write_pass_fail_result_now(TestName,exec,Exec,'FAIL',Ans,Val)))))).

write_pass_fail_result_now(TestName,exec,Exec,PASS_FAIL,Ans,Val):-
   (PASS_FAIL=='PASS'->flag(loonit_success, X, X+1);flag(loonit_failure, X, X+1)),
   (PASS_FAIL=='PASS'->Color=cyan;Color=red),
   color_g_mesg(Color,write_pass_fail_result_c(TestName,exec,Exec,PASS_FAIL,Ans,Val)),!,nl,
   nl,writeln('--------------------------------------------------------------------------'),!.

write_pass_fail_result_c(TestName,exec,Exec,PASS_FAIL,Ans,Val):-
  nl,write_mobj(exec,[(['assertEqualToResult',Exec,Ans])]),
  nl,write_src('!'(['assertEqual',Val,Ans])),
  write_pass_fail_result(TestName,exec,Exec,PASS_FAIL,Ans,Val).
*/

%!  is_unit_test_exec(+Exec) is nondet.
%
%   Checks whether the given executable term (`Exec`) appears to be part of a unit test.
%   This predicate examines the string representation of the term for specific patterns
%   that indicate a test-related operation (e.g., `assert` or Metta assertions `!'`).
%
%   @arg Exec The executable term to be analyzed.
%
%   @example
%     ?- is_unit_test_exec(assert(something)).
%     true.
%
%     ?- is_unit_test_exec(!('assert-something')).
%     true.
%
is_unit_test_exec(Exec) :-
    % Format the term into a string and check for the presence of the word 'assert'.
    sformat(S, '~w', [Exec]),
    sub_atom(S, _, _, _, 'assert').
is_unit_test_exec(Exec) :-
    % Format the term into a quoted string and check for the presence of Metta-style assertion ('!').
    sformat(S, '~q', [Exec]),
    sub_atom(S, _, _, _, "!',").

%!  make_empty(-Empty) is det.
%
%   Produces an empty result. This is used as a placeholder or default value
%   in scenarios where an "empty" or "null" equivalent is required.
%
%   @arg Empty The output term representing emptiness. Defaults to `'Empty'`.
%
%   @example
%     ?- make_empty(X).
%     X = Empty.
%
make_empty(Empty) :-
    % Default value for "empty" is the atom 'Empty'.
    'Empty' = Empty.

%!  make_empty(+_Input, -Empty) is det.
%
%   Produces an empty result, ignoring the input.
%
%   @arg _Input An ignored input.
%   @arg Empty  The output term representing emptiness.
%
make_empty(_, Empty) :-
    make_empty(Empty).

%!  make_empty(+_RetType, +_Input, -Empty) is det.
%
%   Produces an empty result, ignoring the input and return type.
%
%   @arg _RetType An ignored return type.
%   @arg _Input   An ignored input.
%   @arg Empty    The output term representing emptiness.
%
make_empty(_RetType, _, Empty) :-
    make_empty(Empty).

%!  make_nop(-Nop) is det.
%
%   Produces a "no-operation" result. This is typically used as a placeholder
%   when no meaningful operation is required.
%
%   @arg Nop The output term representing no-operation. Defaults to an empty list (`[]`).
%
%   @example
%     ?- make_nop(X).
%     X = [].
%
make_nop(Nop) :-
    % Default "no-operation" value is an empty list.
    [] = Nop.

%!  make_nop(+_Input, -Nop) is det.
%
%   Produces a "no-operation" result, ignoring the input.
%
%   @arg _Input An ignored input.
%   @arg Nop    The output term representing no-operation.
%
make_nop(_, Nop) :-
    make_nop(Nop).

%!  make_nop(+_RetType, +_Input, -Nop) is det.
%
%   Produces a "no-operation" result, ignoring the input and return type.
%
%   @arg _RetType An ignored return type.
%   @arg _Input   An ignored input.
%   @arg Nop      The output term representing no-operation.
%
make_nop(_RetType, _, Nop) :-
    make_nop(Nop).

%!  convert_tax(+How, +Self, +Tax, -Expr, -NewHow) is nondet.
%
%   Converts a tax expression into a normalized form. Depending on the interpretation mode,
%   it may process the `Tax` string differently. The result is a parsed representation of
%   the `Tax` as `Expr`, and the transformation method is unified with `NewHow`.
%
%   @arg How    The current transformation method.
%   @arg Self   The current context or space.
%   @arg Tax    The input tax string to be converted.
%   @arg Expr   The parsed expression result.
%   @arg NewHow The updated transformation method.
%
%   @example
%     % Example usage with a direct read of a Metta tax expression:
%     ?- convert_tax(current_mode, '&self', 'tax_expr', Expr, NewHow).
%
convert_tax(_How, Self, Tax, Expr, NewHow) :-
    % Retrieve the current Metta interpretation mode and channel.
    metta_interp_mode(Ch, Mode),
    % Check if the `Tax` string starts with the interpretation channel.
    string_concat(Ch, TaxM, Tax),
    !,
    % Normalize the tax string by removing extraneous spaces.
    normalize_space(string(NewTax), TaxM),
    % Recurse with the updated mode and normalized tax string.
    convert_tax(Mode, Self, NewTax, Expr, NewHow).
convert_tax(How, _Self, Tax, Expr, How) :-
    % Default case: read the `Tax` string as a Metta expression.
    % parse_sexpr_metta(Tax,Expr).
    read_metta(Tax, Expr).

%:- if( \+ current_predicate(notrace/1) ).
%  notrace(G):- once(G).
%:- endif.

%!  metta_interp_mode(+Symbol, -Mode) is det.
%
%   Maps specific symbols to interpretation modes in the Metta language.
%   Each symbol represents a distinct action, such as loading, unloading, or executing code.
%
%   @arg Symbol The symbol representing the interpretation mode.
%   @arg Mode   The corresponding interpretation mode.
%
%   @example
%     % Retrieve the mode for the '+' symbol:
%     ?- metta_interp_mode('+', Mode).
%     Mode = load.
%
metta_interp_mode('+', load).         % '+' symbol indicates "load" mode.
metta_interp_mode('-', unload).       % '-' symbol indicates "unload" mode.
metta_interp_mode('--', unload_all).  % '--' symbol indicates "unload all" mode.
metta_interp_mode('!', exec).         % '!' symbol indicates "execute" mode.
metta_interp_mode('?', call).         % '?' symbol indicates "call" mode.
metta_interp_mode('^', load_like_file). % '^' symbol indicates "load-like-file" mode.

%!  call_sexpr(+How, +Self, +Tax, -S, -Out) is det.
%
%   Processes a symbolic or string Tax expression using the given interpretation method (`How`)
%   and context (`Self`). The Tax expression is first normalized and converted into a parsed
%   Metta expression (`Expr`), and the resulting mode (`NewHow`) is used to execute the operation.
%   Finally, the output (`Out`) is obtained from the execution.
%
%   @arg How   The current interpretation method.
%   @arg Self  The context or "space" in which the operation is executed.
%   @arg Tax   The input tax expression (symbolic or string) to be processed.
%   @arg S     Reserved or ignored output variable.
%   @arg Out   The result of executing the processed expression.
%
%   @example
%     % Execute a Metta expression with 'exec' mode:
%     ?- call_sexpr('!', '&self', 'assert(a)', S, Out).
%
call_sexpr(How, Self, Tax, _S, Out) :-
    % Ensure the Tax expression is either a symbol or a string.
    (symbol(Tax); string(Tax)),
    % Normalize the Tax expression by removing extraneous spaces.
    normalize_space(string(TaxM), Tax),
    % Convert the normalized Tax expression into a parsed Metta expression (Expr),
    % and determine the updated mode (NewHow).
    convert_tax(How, Self, TaxM, Expr, NewHow),
    !,
    % Display the execution process and execute the Metta operation.
    show_call(do_metta(python, NewHow, Self, Expr, Out)).

%!  do_metta(+File, +Load, +Self, +In, -Out) is det.
%
%   Main handler for processing Metta expressions (`In`). Depending on the `Load` mode
%   and the type of the input, different operations are executed. The result of processing
%   is unified with `Out`.
%
%   @arg File The source of the input (e.g., a file or other context).
%   @arg Load The interpretation mode (e.g., `exec`, `call`, `unload`).
%   @arg Self The context or "space" in which the operation is executed.
%   @arg In   The input expression to process.
%   @arg Out  The output resulting from processing the input expression.

/*
do_metta(File,Load,Self,Cmt,Out):-
  fail,
  if_trace(do_metta, fbug(do_metta(File,Load,Self,Cmt,Out))),fail.
*/
do_metta(_File, _Load, _Self, In, Out) :-
    % If the input is a variable, unify it directly with the output.
    var(In), !, In = Out.
do_metta(_From, _Mode, _Self, end_of_file, 'Empty') :- !.
    % When reaching the end of a file, return `'Empty'`.
    % halt(7), writeln('\n\n% To restart, use: ?- repl.').
do_metta(_File, Load, _Self, Cmt, Out) :-
    % If the mode is not `exec` and the input is an empty list, produce an empty output.
    Load \== exec, Cmt == [], !, ignore(Out = []).
do_metta(From, Load, Self, '$COMMENT'(Expr, _, _), Out) :- !,
    % Handle Metta comments with the `$COMMENT` wrapper.
    do_metta(From, comment(Load), Self, Expr, Out).
do_metta(From, Load, Self, '$STRING'(Expr), Out) :- !,
    % Handle Metta strings with the `$STRING` wrapper.
    do_metta(From, comment(Load), Self, Expr, Out).
do_metta(From, comment(Load), Self, [Expr], Out) :- !,
    % Handle single-element lists as comments.
    do_metta(From, comment(Load), Self, Expr, Out).
do_metta(From, comment(Load), Self, Cmt, Out) :-
    % Write the comment and handle specific cases of MettaLog comments.
    if_trace(loading;load,write_comment(Cmt)), !,
    ignore((symbolic(Cmt),
            symbolic_list_concat([_, Src], 'MeTTaLog only: ', Cmt),
            !,
            atom_string(Src, SrcCode),
            do_metta(mettalog_only(From), Load, Self, SrcCode, Out))),
    ignore((symbolic(Cmt),
            symbolic_list_concat([_, Src], 'MeTTaLog: ', Cmt),
            !,
            atom_string(Src, SrcCode),
            do_metta(mettalog_only(From), Load, Self, SrcCode, Out))),!.
do_metta(From, How, Self, Src, Out) :-
    % Process string inputs by normalizing and converting to Metta expressions.
    string(Src), !,
    must_det_ll((normalize_space(string(TaxM), Src),
                 convert_tax(How, Self, TaxM, Expr, NewHow))),
    do_metta(From, NewHow, Self, Expr, Out).
do_metta(From, _, Self, exec(Expr), Out) :- !,
    % Directly execute Metta expressions wrapped in `exec`.
    do_metta(From, exec, Self, Expr, Out).
% Prolog CALL
do_metta(From, _, Self, call(Expr), Out) :- !,
    % Handle explicit Prolog calls wrapped in `call`.
    do_metta(From, call, Self, Expr, Out).
do_metta(From, _, Self, ':-'(Expr), Out) :- !,
    % Handle `:-` as a Prolog call.
    do_metta(From, call, Self, Expr, Out).
do_metta(From, call, Self, TermV, FOut) :- !,
    % Handle Prolog `call` terms by preparing and executing the call.
    if_t(into_simple_op(call, TermV, OP), pfcAdd_Now('next-operation'(OP))),
    call_for_term_variables(TermV, Term, NamedVarsList, X),
    must_be(nonvar, Term),
    copy_term(NamedVarsList, Was),
   Output = X,
    user:u_do_metta_exec(From, Self, call(TermV), Term, X, NamedVarsList, Was, Output, FOut).
% Non Exec
do_metta(_File, Load, Self, Src, Out) :-
    % Handle non-executable inputs for modes other than `exec`.
    Load \== exec, !,
    if_t(into_simple_op(Load, Src, OP), pfcAdd_Now('next-operation'(OP))),
    dont_give_up(as_tf(asserted_do_metta(Self, Load, Src), Out)).
% Doing Exec
do_metta(file(Filename), exec, Self, TermV, Out) :-
    % Handle executable terms when processing files.
   must_det_ll((inc_exec_num(Filename),
                 get_exec_num(Filename, Nth),
                 Nth > 0)),
    ((
     is_synthing_unit_tests,
     file_answers(Filename, Nth, Ans),
     \+ is_transpiling,
        check_answers_for(TermV, Ans))), !,
    if_t(into_simple_op(exec, TermV, OP), pfcAdd_Now('next-operation'(OP))),
     must_det_ll((
      ensure_increments((color_g_mesg_ok('#ffa509',
       (writeln(';; In file as:  '),
                           color_g_mesg([bold, fg('#FFEE58')], write_src(exec(TermV))),
                           write(';; To unit test case:'))), !,
                          call(do_metta_exec(file(Filename), Self,
                                             ['assertEqualToResult', TermV, Ans], Out)))))).
%   Handles the direct execution of Metta terms (`TermV`) in the `exec` mode.
do_metta(From, exec, Self, TermV, Out) :- !,
    % Simplify the term into an operation (if possible) and register it.
    if_t(into_simple_op(exec, TermV, OP), pfcAdd_Now('next-operation'(OP))),
    % Attempt to execute the term, preventing failure propagation.
    dont_give_up(do_metta_exec(From, Self, TermV, Out)).

%!  do_metta_exec(+From, +Self, +TermV, -FOut) is det.
%
%   Executes a Metta term (`TermV`) in the specified context (`Self`) and source (`From`).
%   This predicate:
%   1. Optionally outputs the execution trace if the source is a file.
%   2. Converts the term into a callable Prolog term using `into_metta_callable/5`.
%   3. Executes the term with `user:u_do_metta_exec/8`.
%   4. Catches any errors raised during execution and logs them as "gave up."
%
%   @arg From   The source of the execution (e.g., a file or runtime context).
%   @arg Self   The context or "space" in which the term is executed.
%   @arg TermV  The Metta term to be executed.
%   @arg FOut   The result of the execution.
%
%   @example
%     % Execute a term with a file source:
%     ?- do_metta_exec(file('example.metta'), '&self', 'assert(foo)', Out).
%     Out = ResultOfExecution.
%
do_metta_exec(From, Self, TermV, FOut) :-

    % Debugging output for initial state.
    % format("########################X0 ~w ~w ~w\n", [Self, TermV, FOut]),
 (catch(((
        % Show execution trace if the source is a file.
        if_t(From = file(_), output_language(metta, write_exec(TermV))),
        Output = X,
        % Convert the term into a callable Prolog term.
        notrace(into_metta_callable(Self, TermV, Term, X, NamedVarsList, Was)), !,
        % Debugging output for intermediate state.
        % format("########################X1 ~w ~w ~w ~w\n", [Term, X, NamedVarsList, Output]),
        % Perform the execution using the user-defined handler.
        user:u_do_metta_exec(From, Self, TermV, Term, X, NamedVarsList, Was, Output, FOut))),
                give_up(Why), pp_m(red, gave_up(Why)))).
        % Catch errors during execution and log them.
    % Debugging output for final state.
    % format("########################X2 ~w ~w ~w\n", [Self, TermV, FOut]).

%!  a_e(+Assertion) is nondet.
%
%   Defines valid assertion operation names in Metta. These operations are typically used
%   in unit testing to compare expected and actual values.
%
%   @arg Assertion The assertion operation name as a string.
%
%   @example
%     % Check if an operation is a valid assertion:
%     ?- a_e('assertEqual').
%     true.
%
a_e('assertEqual').
a_e('assertNotEqual').
a_e('assertEqualToResult').
a_e('assertNotEqualToResult').

%!  o_s(+OpArgs, -Simplified) is det.
%
%   Simplifies a list of operation arguments (`OpArgs`) to a single term.
%   This predicate:
%   1. Recognizes assertion operations (e.g., `assertEqual`) as the primary operation.
%   2. Returns the last argument as the simplified result if no assertion is present.
%
%   @arg OpArgs     The list of operation arguments to simplify.
%   @arg Simplified The resulting simplified term.
%
%   @example
%     % Simplify operation arguments with an assertion:
%     ?- o_s(['assertEqual', foo, bar], Simplified).
%     Simplified = bar.
%
%     % Simplify operation arguments without an assertion:
%     ?- o_s([foo, bar], Simplified).
%     Simplified = bar.
%
o_s([AE | O], S) :-
    % If the first element is a valid assertion and there are other arguments,
    % simplify the remaining list to the last argument.
    nonvar(AE), a_e(AE), nonvar(O), !, o_s(O, S).
o_s([O | _], S) :-
    % If the first element is non-variable and not an assertion, simplify directly.
    nonvar(O), !, o_s(O, S).
o_s(S, S). % Base case: the list itself is the simplified term.

%!  into_simple_op(+Load, +Args, -Op) is nondet.
%
%   Converts a list of arguments (`Args`) into a simplified operation (`Op`).
%   The operation includes:
%   - The `Load` mode.
%   - The primary operation (`Op`).
%   - A simplified version of the remaining arguments.
%
%   @arg Load The current mode of execution (e.g., `exec`).
%   @arg Args The list of arguments to simplify.
%   @arg Op   The resulting operation term.
%
%   @example
%     % Simplify arguments into a structured operation:
%     ?- into_simple_op(exec, ['assertEqual', foo, bar], Op).
%     Op = op(exec, 'assertEqual', bar).
%
into_simple_op(Load, [Op | O], op(Load, Op, S)) :-
    % Simplify the remaining arguments and construct the operation term.
    o_s(O, S), !.

%! call_for_term_variables(+Term, +X, -Result, -NamedVarsList, +TF) is det.
%   Handles the term `Term` and determines the term variable list and final result.
%   This version handles the case when the term has no variables and converts it to a truth-functional form.
%
%   @arg Term The input term to be analyzed.
%   @arg X The list of variables found within the term. It can be empty or contain one variable.
%   @arg Result The final result, either as the original term or transformed into a truth-functional form.
%   @arg NamedVarsList The list of named variables associated with the term.
%   @arg TF The truth-functional form when the term has no variables.
%
%   @example
%     % Example with no variables:
%     ?- call_for_term_variables(foo, Result, Vars, TF).
%     Result = as_tf(foo, TF),
%     Vars = [].
%
call_for_term_variables(TermV, catch_red(show_failure(TermR)), NewNamedVarsList, X) :-
    % Substitute variables in the term, producing a processed term and an initial named variable list.
    subst_vars(TermV, Term, NamedVarsList),
    % Debugging output: Show substitutions and variable analysis.
    wwdmsg(subst_vars(TermV, Term, NamedVarsList)),
    % Retrieve all variables in the term.
    term_variables(Term, AllVars),
    % Debugging output for all variables.
    % get_global_varnames(VNs), append(NamedVarsList, VNs, All), nb_setval('$variable_names', All),
    % wdmsg(term_variables(Term, AllVars) = All),
    % Identify singletons (unused variables) in the term.
    term_singletons(Term, Singletons),
    % Identify don't-care variables in the term.
    term_dont_cares(Term, DontCares),
    % Debugging output for singletons and don't-cares.
    wwdmsg((term_singletons(Term, Singletons), term_dont_cares(Term, DontCares))),
    % Filter out variables that are in the singleton list from all variables.
    include(not_in_eq(Singletons), AllVars, NonSingletons),
    wwdmsg([dc = DontCares, sv = Singletons, ns = NonSingletons]), !,
    % Further refine variables by removing those in the don't-care list.
    include(not_in_eq(DontCares), NonSingletons, CNonSingletons),
    include(not_in_eq(DontCares), Singletons, CSingletons),
    wwdmsg([dc = DontCares, csv = CSingletons, cns = CNonSingletons]), !,
    % Map the variables to named representations.
    maplist(maplist(into_named_vars),
            [DontCares, CSingletons, CNonSingletons],
            [DontCaresN, CSingletonsN, CNonSingletonsN]),
    wwdmsg([dc_nv = DontCaresN, sv_nv = CSingletonsN, ns_nv = CNonSingletonsN]),
    % Delegate to `call_for_term_variables5/8` for further processing.
    call_for_term_variables5(
        Term, DontCaresN, CNonSingletonsN, CSingletonsN,
        TermR, NamedVarsList, NewNamedVarsList, X), !,
    % Debugging output for final processed result.
    wwdmsg(call_for_term_variables5(
        orig = Term, all = DontCaresN, singles = CSingletonsN,
        shared = CNonSingletonsN, call = TermR,
        nvl = NamedVarsList, nvlo = NewNamedVarsList, output = X)).

%!  wwdmsg(+Message) is det.
%
%   A placeholder debugging predicate. It can be replaced with custom
%   debugging or logging implementations as needed. Currently, it does nothing.
%
%   @arg Message The debugging message or data to output.
%
wwdmsg(_).

%!  call_for_term_variables5(+Term, +DontCares, +NonSingletons, +Singletons, -CallTerm, -VarList, -UpdatedVarList, -Return) is det.
%
%   Handles the processing of a term (`Term`) to generate a callable term representation
%   (`CallTerm`) when the term is ground (contains no variables). It also builds or updates
%   a variable list for the term's context.
%

% If the term is ground, return the as_tf form.
%call_for_term_variables5(Term,_,_,_,as_tf(Term,Ret),VL,['$RetVal'=Ret|VL],[==,['call!',Term],Ret]) :- ground(Term), !.
% If the term is ground, create a call_nth with the term.
call_for_term_variables5(Term,_,_,_,call_nth(Term,Count),VL,['Count'=Count|VL],Ret) :- Ret=Term.

%!  into_metta_callable(+Self, +InputTerm, -OutputTerm, -Result, -NamedVarsList, -CopiedVars) is nondet.
%
%   Processes an input term (`InputTerm`) into a callable representation (`OutputTerm`)
%   within the Metta framework. This predicate handles:
%   - Substitution of variables within the term.
%   - Conversion of terms for execution, including runtime calls.
%   - Transpilation if enabled.
%
%   @arg Self           The context or space for processing the term.
%   @arg InputTerm      The input term to process (e.g., `call/1`, raw expressions).
%   @arg OutputTerm     The resulting term, ready for execution.
%   @arg Result         The result variable for execution.
%   @arg NamedVarsList  A list of variable mappings within the term.
%   @arg CopiedVars     A copy of the named variables for safe processing.
%
into_metta_callable(_Self,CALL,Term,X,NamedVarsList,Was):- fail,
   % wdmsg(mc(CALL)),
    CALL= call(TermV),
    \+ never_compile(TermV),
     must_det_ll((((
     term_variables(TermV,Res),
    % ignore(Res = '$VAR'('ExecRes')),
     RealRes = Res,
     TermV=ExecGoal,
     %format("~w ~w\n",[Res,ExecGoal]),
     subst_vars(Res+ExecGoal,Res+Term,NamedVarsList),
     copy_term_g(NamedVarsList,Was),
     term_variables(Term,Vars),


     Call = do_metta_runtime(Res, ExecGoal),
     output_language(prolog, notrace((color_g_mesg('#114411', print_pl_source(:- Call  ))))),
     %nl,writeq(Term),nl,
     ((\+ \+
     ((
     %numbervars(v(TermV,Term,NamedVarsList,Vars),999,_,[attvar(skip)]),
     %nb_current(variable_names,NamedVarsList),
     %nl,print(subst_vars(Term,NamedVarsList,Vars)),
     nop(nl))))),
     nop(maplist(verbose_unify,Vars)),
     %NamedVarsList=[_=RealRealRes|_],
     %var(RealRes),
     X = RealRes)))),!.


into_metta_callable(_Self,TermV,Term,X,NamedVarsList,Was):-
  \+ option_value('exec',interp),
 % never_compile(TermV),
 \+ toplevel_interp_only(TermV),
 is_transpiling, !,
  must_det_ll((((

 % ignore(Res = '$VAR'('ExecRes')),
  RealRes = Res,
  compile_for_exec(Res,TermV,ExecGoal),!,
  %format("~w ~w\n",[Res,ExecGoal]),
  subst_vars(Res+ExecGoal,Res+Term,NamedVarsList),
  copy_term_g(NamedVarsList,Was),
  term_variables(Term,Vars),


  Call = do_metta_runtime(Res, ExecGoal),
  output_language(prolog, notrace((color_g_mesg('#114411', print_pl_source(:- Call  ))))),
  %nl,writeq(Term),nl,
  ((\+ \+
  ((
  %numbervars(v(TermV,Term,NamedVarsList,Vars),999,_,[attvar(skip)]),
  %nb_current(variable_names,NamedVarsList),
  %nl,print(subst_vars(Term,NamedVarsList,Vars)),
  nop(nl))))),
  nop(maplist(verbose_unify,Vars)),
  %NamedVarsList=[_=RealRealRes|_],
  %var(RealRes),
  X = RealRes)))),!.


into_metta_callable(Self,TermV,CALL,X,NamedVarsList,Was):-!,
 default_depth(DEPTH),
 option_else('stack-max',StackMax,DEPTH),
 CALL = eval_H(StackMax,Self,Term,X),
 notrace(( must_det_ll((
 if_t(show_transpiler,write_compiled_exec(TermV,_Goal)),
  subst_vars(TermV,Term,NamedVarsList),
  copy_term_g(NamedVarsList,Was)
  %term_variables(Term,Vars),
  %nl,writeq(Term),nl,
  %skip((\+ \+
  %((numbervars(v(TermV,Term,NamedVarsList,Vars),999,_,[attvar(bind)]),  %nb_current(variable_names,NamedVarsList),
  %nl,print(subst_vars(TermV,Term,NamedVarsList,Vars)),nl)))),
  %nop(maplist(verbose_unify,Vars)))))),!.
  )))),!.



%!  eval_S(+Form) is det.
%
%   Evaluates a given form (`Form`) in the context of a specific `Self` if the current
%   context matches the form is executed in `exec` mode
%   using the `do_metta/5` predicate.
%
%   @arg Self  The context or "space" in which the evaluation should occur.
%   @arg Form  The form or term to be evaluated.
%
eval_S(Self, Form) :-
    % Ensure the `Form` is instantiated (not a variable).
    nonvar(Form),
    % Check if the current self matches the provided `Self`.
    current_self(Self),
    % Execute the form in `exec` mode.
    do_metta(true, exec, Self, Form, _Out).


% Read the form in `+` mode.
eval_string(String):- user_io((eval_string(String, _Out))).
eval_string(String, Out):-
    current_self(Self),
    read_metta(String, Metta),
    do_metta(true, +, Self, Metta, Out).


%!  eval_H(+Term, -Result) is det.
%
%   Handles the evaluation of a term (`Term`) and captures the result (`Result`).
%   This version wraps the evaluation with `catch_metta_return/2` for error handling.
%
%   @arg Term    The term to evaluate.
%   @arg Result  The result of the evaluation.
%
%   @example
%     % Evaluate a term with error handling:
%     ?- eval_H(some_term, Result).
%
eval_H(Term, X) :-
    % Wrap the evaluation in `catch_metta_return/2` to handle any errors.
    catch_metta_return(eval_args(Term, X), X).

%!  eval_H(+StackMax, +Self, +Term, -Result) is det.
%
%   Evaluates a term (`Term`) in the context of a specific `Self`, with a limit on stack size.
%   If the `compile` option is set to `save`, it skips evaluation and directly returns the term.
%   Otherwise, it delegates to `eval_args/6` for evaluation.
%
%   @arg StackMax  The maximum allowable stack size for the evaluation.
%   @arg Self      The context or "space" for evaluation.
%   @arg Term      The term to be evaluated.
%   @arg Result    The result of the evaluation.
%
%   @example
%     % Evaluate a term with a stack size limit:
%     ?- eval_H(100, '&self', some_term, Result).
%
eval_H(_StackMax, _Self, Term, Term) :-
    % If the `compile` option is set to `save`, return the term unchanged.
    fast_option_value(compile, save), !.
eval_H(StackMax, Self, Term, X) :-
    % Otherwise, perform evaluation with error handling, passing the stack limit.
    catch_metta_return(eval_args('=', _, StackMax, Self, Term, X), X).
/*
eval_H(StackMax,Self,Term,X).

eval_H(StackMax,Self,Term,X):-
  Time = 90.0,
 ((always_exec(Term)) ->
    if_or_else(t1('=',_,StackMax,Self,Term,X),
                      (t2('=',_,StackMax,Self,Term,X)));
    call_max_time(t1('=',_,StackMax,Self,Term,X),   Time,
                              (t2('=',_,StackMax,Self,Term,X)))).

eval_H(Term,X):-
    current_self(Self), StackMax = 100,
    if_or_else((t1('=',_,StackMax,Self,Term,X),X\==Term),(t2('=',_,StackMax,Self,Term,X),nop(X\==Term))).


t1('=',_,StackMax,Self,Term,X):- eval_args('=',_,StackMax,Self,Term,X).
t2('=',_,StackMax,Self,Term,X):- fail, subst_args('=',_,StackMax,Self,Term,X).
*/

%eval_H(Term,X):- if_or_else((subst_args(Term,X),X\==Term),(eval_args(Term,Y),Y\==Term)).

%!  print_goals(+TermV) is det.
%
%   Outputs the source representation of a term (`TermV`).
%
%   @arg TermV The term to be printed.
%
%   @example
%     % Print a term:
%     ?- print_goals(foo(bar)).
%     foo(bar).
%
print_goals(TermV) :-
    % Write the source representation of the term.
    write_src(TermV).

%!  if_or_else(+IfTrue1, +OrElse2) is det.
%
%   Executes the first goal (`IfTrue1`). If it succeeds, the predicate succeeds.
%   If the first goal fails, executes the alternative goal (`OrElse2`).
%
%   @arg IfTrue1 The first goal to try.
%   @arg OrElse2 The alternative goal to execute if `IfTrue1` fails.
%
%   @example
%     % Example usage of if_or_else/2:
%     ?- if_or_else(true, writeln('Else executed')).
%     true.
%
%     ?- if_or_else(fail, writeln('Else executed')).
%     Else executed
%     true.
%
if_or_else(IfTrue1, OrElse2) :-
    % Execute `IfTrue1`; if it succeeds, succeed. Otherwise, execute `OrElse2`.
    call(IfTrue1) *-> true ; call(OrElse2).

%!  if_or_else(+IfTrue1, +OrElse2, +OrElse3) is det.
%
%   Executes the first goal (`IfTrue1`). If it succeeds, the predicate succeeds.
%   Otherwise, tries `OrElse2`. If `OrElse2` fails, executes `OrElse3`.
%
%   @arg IfTrue1 The first goal to try.
%   @arg OrElse2 The second goal to try if `IfTrue1` fails.
%   @arg OrElse3 The third goal to try if both `IfTrue1` and `OrElse2` fail.
%
if_or_else(IfTrue1, OrElse2, OrElse3) :-
    % Chain the calls, trying each alternative in sequence.
    if_or_else(IfTrue1, if_or_else(OrElse2, OrElse3)).

%!  if_or_else(+IfTrue1, +OrElse2, +OrElse3, +OrElse4) is det.
%
%   Extends `if_or_else/3` to include a fourth alternative (`OrElse4`).
%
if_or_else(IfTrue1, OrElse2, OrElse3, OrElse4) :-
    % Chain the calls, trying each alternative in sequence.
    if_or_else(IfTrue1, if_or_else(OrElse2, OrElse3, OrElse4)).

%!  if_or_else(+IfTrue1, +OrElse2, +OrElse3, +OrElse4, +OrElse5) is det.
%
%   Extends `if_or_else/4` to include a fifth alternative (`OrElse5`).
%
if_or_else(IfTrue1, OrElse2, OrElse3, OrElse4, OrElse5) :-
    % Chain the calls, trying each alternative in sequence.
    if_or_else(IfTrue1, if_or_else(OrElse2, OrElse3, OrElse4, OrElse5)).

%!  interacting is nondet.
%
%   Determines if the system is in an interactive mode.
%   This predicate succeeds if:
%   - Tracing is enabled.
%   - Debug mode is active.
%   - The `interactive` option is set to `true`.
%   - The `prolog` option is set to `true`.
%
interacting :-
    % Check if tracing is enabled.
    tracing, !.
interacting :-
    % Check if debugging is enabled.
    current_prolog_flag(debug, true), !.
interacting :-
    % Check if the `interactive` option is enabled.
    option_value(interactive, true), !.
interacting :-
    % Check if the `prolog` option is enabled.
    option_value(prolog, true), !.

%!  call_max_time(+Goal, +MaxTime, +Else) is det.
%
%   Executes a goal (`Goal`) with a maximum time limit (`MaxTime`).
%   If the goal fails or the time limit is exceeded, executes the alternative goal (`Else`).
%
%   @arg Goal     The goal to execute.
%   @arg MaxTime  The maximum time allowed for `Goal`.
%   @arg Else     The alternative goal to execute if `Goal` fails or times out.
%
%   @example
%     % Run a goal with a time limit:
%     ?- call_max_time(true, 5, writeln('Timeout or failure occurred')).
%
call_max_time(Goal, _MaxTime, Else) :-
    % If the system is in interactive mode, skip the time limit and execute directly.
    interacting, !, if_or_else(Goal, Else).
call_max_time(Goal, _MaxTime, Else) :-
    % Fallback to executing the goal directly if no time limit is set.
    !, if_or_else(Goal, Else).
call_max_time(Goal, MaxTime, Else) :-
    % Use `call_with_time_limit/2` to enforce the time limit, handling exceptions for timeouts.
    catch(if_or_else(call_with_time_limit(MaxTime, Goal), Else), time_limit_exceeded, Else).

%!  catch_err(+Goal, +Exception, +Handler) is det.
%
%   Executes a goal (`Goal`) and handles exceptions using a handler (`Handler`).
%   If the exception satisfies `always_rethrow/1`, it is rethrown instead of being handled.
%
%   @arg Goal      The goal to execute.
%   @arg Exception The exception to catch.
%   @arg Handler   The handler to execute if an exception occurs.
%
catch_err(G, E, C) :-
    catch(G, E, (always_rethrow(E) -> (throw(E)) ; C)).

%!  dont_give_up(+Goal) is det.
%
%   Attempts to execute a goal (`Goal`), catching `give_up/1` exceptions.
%   Logs the exception using `write_src_uo/1` if caught.
%
%   @arg Goal The goal to execute.
%
dont_give_up(G) :-
    catch(G, give_up(E), write_src_uo(dont_give_up(E))).

%!  not_in_eq(+List, +Element) is nondet.
%
%   Succeeds if an element (`Element`) is in a list (`List`) and is equal
%   to an existing element using the `==` operator.
%
%   @arg List     The list to search.
%   @arg Element  The element to compare.
%
%   @example
%     % Check for exact equality in a list:
%     ?- not_in_eq([a, b, c], b).
%     true.
%
not_in_eq(List, Element) :-
    % Iterate over the list and check for equality using `==`.
    member(V, List), V == Element.

:- ensure_loaded(metta_repl).

% Each of these `nodebug/1` directives suppresses debugging output for the corresponding category.
:- nodebug(metta(eval)).
:- nodebug(metta(exec)).
:- nodebug(metta(load)).
:- nodebug(metta(prolog)).

%
% Below code measures the execution time of a Prolog goal and displays the duration in seconds,
% milliseconds, or microseconds, depending on the execution time.
%
% Args:
%   - Goal: The Prolog goal to be executed and timed.
%
% The predicate uses the `statistics/2` predicate to measure the CPU time before
% and after executing the provided goal. It calculates the elapsed time in seconds
% and converts it to milliseconds and microseconds. The output is formatted to
% provide clear timing information:
%
% - If the execution takes more than 2 seconds, it displays the time in seconds.
% - If the execution takes between 1 millisecond and 2 seconds, it displays the time
%   in milliseconds.
% - If the execution takes less than 1 millisecond, it displays the time in microseconds.
%
% Example usage:
%   ?- time_eval(my_goal(X)).
%
%   ?- time_eval(sleep(0.95)).
%
% Output examples:
%   ; Evaluation took 2.34 seconds.
%   ; Evaluation took 123.45 ms.
%   ; Evaluation took 0.012 ms. (12.33 microseconds)
%

%!  time_eval(+Goal) is det.
%
%   Measures the execution time of a Prolog goal (`Goal`) and displays the duration
%   in seconds, milliseconds, or microseconds, depending on the elapsed time.
%   Uses CPU time for measurement.
%
%   @arg Goal The Prolog goal to be executed and timed.
%
%   @example
%     % Measure and display the time for a simple goal:
%     ?- time_eval(my_goal(X)).
%
%     % Measure the time for a goal with a delay:
%     ?- time_eval(sleep(0.95)).
%
time_eval(Goal) :-
    time_eval('Evaluation', Goal).

%!  time_eval(+What, +Goal) is det.
%
%   Extended version of `time_eval/1` allowing a custom description (`What`) to be
%   displayed in the timing output.
%
%   @arg What A description of the evaluated task.
%   @arg Goal The Prolog goal to be executed and timed.
%
time_eval(What, Goal) :-
    timed_call(Goal, Seconds),
    give_time(What, Seconds).

%!  ctime_eval(+Goal) is det.
%
%   Similar to `time_eval/1`, but explicitly uses CPU time for measuring the
%   execution time of the goal (`Goal`).
%
ctime_eval(Goal) :-
    ctime_eval('Evaluation', Goal).

%!  ctime_eval(+What, +Goal) is det.
%
%   Allows a custom description (`What`) for CPU time-based evaluation.
%
ctime_eval(What, Goal) :-
    ctimed_call(Goal, Seconds),
    give_time(What, Seconds).

%!  wtime_eval(+Goal) is det.
%
%   Measures the wall-clock (real) time for the execution of a goal (`Goal`).
%   Suitable for tasks involving delays or external interactions.
%
wtime_eval(Goal) :-
    wtime_eval('Evaluation', Goal).

%!  wtime_eval(+What, +Goal) is det.
%
%   Allows a custom description (`What`) for wall-clock time-based evaluation.
%
wtime_eval(What, Goal) :-
    wtimed_call(Goal, Seconds),
    give_time(What, Seconds).

%!  give_time(+What, +Seconds) is det.
%
%   Formats and displays the elapsed time for a task (`What`) based on the duration
%   in seconds (`Seconds`). Adjusts the output units to seconds, milliseconds, or
%   microseconds based on the value of `Seconds`.
%
%   @arg What    A description of the evaluated task.
%   @arg Seconds The elapsed time in seconds.
%
%   Output rules:
%   - More than 2 seconds: Displays in seconds.
%   - Between 1 ms and 2 seconds: Displays in milliseconds.
%   - Less than 1 ms: Displays in microseconds.
%

%give_time(_What,_Seconds):- is_compatio,!.
give_time(What, Seconds) :-
    Milliseconds is Seconds * 1_000,
    (Seconds > 2
        -> format('~N; ~w took ~2f seconds.~n~n', [What, Seconds])
        ; (Milliseconds >= 1
            -> format('~N; ~w took ~3f secs. (~2f milliseconds) ~n~n', [What, Seconds, Milliseconds])
            ; (Micro is Milliseconds * 1_000,
              format('~N; ~w took ~6f secs. (~2f microseconds) ~n~n', [What, Seconds, Micro])))).

%!  timed_call(+Goal, -Seconds) is det.
%
%   Measures the CPU time taken to execute a Prolog goal (`Goal`).
%   Delegates to `ctimed_call/2` for actual measurement.
%
%   @arg Goal    The goal to be executed and timed.
%   @arg Seconds The elapsed time in seconds.
%
timed_call(Goal, Seconds) :-
    ctimed_call(Goal, Seconds).

%!  ctimed_call(+Goal, -Seconds) is det.
%
%   Measures the CPU time taken to execute a Prolog goal (`Goal`) using
%   `statistics/2`.
%
%   @arg Goal    The goal to be executed and timed.
%   @arg Seconds The elapsed CPU time in seconds.
%
ctimed_call(Goal, Seconds) :-
    statistics(cputime, Start),
    % Use `rtrace` for debugging if applicable.
    ( \+ rtrace_this(Goal) -> rtrace_on_error(Goal) ; rtrace(Goal) ),
    statistics(cputime, End),
    Seconds is End - Start.

%!  wtimed_call(+Goal, -Seconds) is det.
%
%   Measures the wall-clock (real) time taken to execute a Prolog goal (`Goal`)
%   using `statistics/2`.
%
%   @arg Goal    The goal to be executed and timed.
%   @arg Seconds The elapsed wall-clock time in seconds.
%
wtimed_call(Goal, Seconds) :-
    statistics(walltime, [Start, _]),
    % Use `rtrace` for debugging if applicable.
    ( \+ rtrace_this(Goal) -> rtrace_on_error(Goal) ; rtrace(Goal) ),
    statistics(walltime, [End, _]),
    Seconds is (End - Start) / 1000.

%!  rtrace_this(+Call) is nondet.
%
%   Determines whether a given call (`Call`) should be traced using `rtrace/1`.
%   The predicate checks various conditions to decide if tracing should be enabled:
%   - Specific forms of `Call` are explicitly excluded from tracing.
%   - Tracing is enabled based on runtime options or debugging flags.
%
%   @arg Call The goal or term to be checked for tracing.
%
%   @example
%     % Enable tracing for general terms:
%     ?- option_value(rtrace, true), rtrace_this(my_goal(foo)).
%     true.
%
%     % Exclude certain pragmas or imports:
%     ?- rtrace_this(['pragma!', foo]).
%     false.
%
%     % Check nested compound terms:
%     ?- rtrace_this(eval_H(_, _, foo(bar), _)).
%     true.
%
rtrace_this(eval_H(_, _, P, _)) :-
    % If the third argument (`P`) is compound, recursively check its structure.
    compound(P), !, rtrace_this(P).
rtrace_this([P|_]) :-
    % Exclude the pragma directive `'pragma!'`.
    P == 'pragma!', !, fail.
rtrace_this([P|_]) :-
    % Exclude the import directive `'import!'`.
    P == 'import!', !, fail.
rtrace_this([P|_]) :-
    % Allow tracing for the directive `'rtrace!'`.
    P == 'rtrace!', !.
rtrace_this(_Call) :-
    % Enable tracing if the `rtrace` option is set to `true`.
    option_value(rtrace, true), !.
rtrace_this(_Call) :-
    % Enable tracing if the `rtrace` debugging flag is active.
    is_debugging(rtrace), !.

%:- nb_setval(cmt_override,lse('; ',' !(" ',' ") ')).

:- ignore(abolish(fbug/1)).  % Removes any existing definition of `fbug/1` to ensure a clean slate.

%!  fbug(+Info) is det.
%
%   Handles debug logging for the system. It determines whether logging should occur
%   based on compatibility checks and runtime options. If logging is enabled,
%   it outputs the provided debug information (`Info`) in a specific format.
%
%   @arg Info The debug information to be logged.
%
fbug(_) :-
    % If compatibility mode (`is_compatio`) is active, do nothing.
    is_compatio, !.
fbug(_) :- % If the 'log' option is not set to 'true', do nothing.
    option_value('log', 'false'), !.
fbug(Info) :-
    % Otherwise, log the debug information using `write_src/1` with formatting.
    real_notrace(in_cmt(color_g_mesg('#2f2f2f', write_src(Info)))).

%!  example0(+Input) is det.
%
%   An example predicate that always fails, regardless of the input.
%
example0(_) :- fail.

%!  example1(+Input) is nondet.
%
%   Succeeds only for the input `a`. Fails for any other input.
%
example1(a).
example1(_) :- fail.

%!  example2(+Input) is nondet.
%
%   Succeeds for `a` and `b`. Fails for any other input.
%
example2(a).
example2(b).
example2(_) :- fail.

%!  example3(+Input) is nondet.
%
%   Succeeds for `a`, `b`, and `c`. Fails for any other input.
%
example3(a).
example3(b).
example3(c).
example3(_) :- fail.

% eval_H(100, '&self', ['change-state!', '&var', [+, 1, ['get-state', '&var']]], OUT)

%!  dcall(+Goal) is nondet.
%
%   Executes a given goal (`Goal`) and determines if it is deterministic.
%   If deterministic, it applies a cut (`!`) to prevent backtracking.
%
%   @arg Goal The goal to execute.
%
%dcall(X):- (call(X),deterministic(YN)),trace,((YN==true)->!;true).

%!  chkdet_call(+Goal) is det.
%
%   A variant of `call/1` that executes a goal deterministically.
%
%   @arg Goal The goal to execute.
%
chkdet_call(XX) :- !, call(XX).

%!  chkdet_call0(+Goal) is det.
%
%   Similar to `chkdet_call/1`, executes a goal deterministically.
%
%   @arg Goal The goal to execute.
%
chkdet_call0(XX) :- !, call(XX).

%!  dcall0000000000(+Goal) is nondet.
%
%   Executes a goal (`XX`) and manages deterministic and nondeterministic cases.
%   Uses `call_nth/5` to execute the goal and track its solution number (`Nth`) and determinism (`Det`).
%
%   @arg Goal The goal to be executed.
%
dcall0000000000(XX) :-
    % Create a structure to track the solution (`USol`).
   USol = sol(dead),
    % Create a copy of the goal (`XX`) to avoid unintended modifications.
    copy_term_g(XX, X),
    % Execute the goal and retrieve its solution number, determinism, and previous state.
    call_nth(USol, X, Nth, Det, Prev),
    % Optionally log the call (commented out in the original code).
    % fbug(call_nth(USol, X, Nth, Det, Prev)),
    % Unify `XX` with the previous result (`Prev`).
    XX = Prev,
    % Handle deterministic and nondeterministic cases.
    (Det == yes ->
        % If deterministic, commit the result and unify `XX` with the result or copy.
        (!, (XX = Prev ; XX = X))
    ;
        % Handle nondeterministic cases based on the solution number (`Nth`).
        (((var(Nth) ->
            % If `Nth` is unbound, commit the result if `Prev` is not `dead`.
            (!, Prev \== dead)
        ;
      true),
        % If `Nth` equals 1, commit the result; otherwise, allow backtracking.
        (Nth == 1 -> ! ; true)))).

%!  call_nth(+USol, +Goal, -Nth, -Det, -Prev) is nondet.
%
%   Executes a goal (`XX`) and tracks its solution number (`Nth`) and determinism (`Det`).
%   Updates the `USol` structure with the latest solution (`Prev`).
%
%   @arg USol  A structure to hold the previous solution.
%   @arg Goal  The goal to be executed.
%   @arg Nth   The solution number of the goal.
%   @arg Det   Indicates if the goal is deterministic (`yes` or `no`).
%   @arg Prev  The previous solution for the goal.
%
call_nth(USol, XX, Nth, Det, Prev) :-
  repeat,
    (
        % Execute the goal, track its solution number and determinism, and update `USol`.
        (call_nth(XX, Nth), deterministic(Det), arg(1, USol, Prev)) *->
            % Update the solution in `USol`.
            (nb_setarg(1, USol, XX))
        ;
            % Commit the previous solution if no new ones are available.
            (!, arg(1, USol, Prev))
    ).

%!  catch_red(+Term) is det.
%
%   Executes a goal (`Term`) and catches any exceptions, logging them with `pp_m_m_red/2`.
%
%   @arg Term The term to be executed.
%
catch_red(Term) :-
    catch_err(Term, E, pp_m_m_red(red, in(Term, E))).

% catch_red(Term):- call(Term).

%!  pp_m_m_red(+Color, +Term) is det.
%
%   Handles exceptions and logs them in a formatted manner, skipping specific cases.
%
%   @arg Color The color used for formatting the log.
%   @arg Term  The exception or term to be logged.
%
pp_m_m_red(_, T) :-
    % Skip logging for specific error terms.
    T =@= in(not_compat_io(maybe_halt(7)), unwind(halt(7))), !.
pp_m_m_red(_, T) :- compound(T),
    % Skip logging for specific error terms.
    T = in(not_compat_io(maybe_halt(Seven)), unwind(halt(Seven))), number(Seven),!.
pp_m_m_red(C, T) :-
    % Log the term with the given color.
    pp_m(C, T).

%!  s2p(+Input, -Output) is det.
%
%   Converts a symbolic expression (`Input`) to a Prolog term (`Output`).
%   Uses `sexpr_s2p/2` for the actual conversion.
%
%   @arg Input  The input symbolic expression.
%   @arg Output The converted Prolog term.
%
s2p(I, O) :- sexpr_s2p(I, O), !.

%!  discover_head(+Self, +Load, +Head) is det.
%
%   Discovers the head of a predicate and processes it within a specific context (`Self`).
%   This includes analyzing the argument types and creating a new Metta head.
%
%   @arg Self The context in which the predicate is being loaded.
%   @arg Load The load operation to apply.
%   @arg Head The predicate head to be analyzed.
%
discover_head(Self, Load, Head) :-
    ignore((
        [Fn | PredDecl] = Head,
        nop((
            arg_types(PredDecl, [], EachArg),
            metta_anew1(Load, metta_head(Self, Fn, EachArg))
        ))
    )).

%!  discover_body(+Self, +Load, +Body) is det.
%
%   Discovers the body of a predicate and processes it within a specific context (`Self`).
%   This includes analyzing the argument types and creating a new Metta body.
%
%   @arg Self The context in which the predicate is being loaded.
%   @arg Load The load operation to apply.
%   @arg Body The predicate body to be analyzed.
%
discover_body(Self, Load, Body) :-
    nop((
        [Fn | PredDecl] = Body,
        arg_types(PredDecl, [], EachArg),
        metta_anew1(Load, metta_body(Self, Fn, EachArg))
    )).

%!  decl_length(+TypeDeclaration, -Length) is det.
%
%   Computes the length of a type declaration (`TypeDeclaration`).
%   For lists, it calculates the actual length. For other types, it defaults to 1.
%
%   @arg TypeDeclaration The type declaration to analyze.
%   @arg Length           The computed length of the declaration.
%
decl_length(TypeDecL, Len) :-
    is_list(TypeDecL), !,
    length(TypeDecL, Len).
decl_length(_TypeDecL, 1).

%!  arg_types(+ArgList, +Rest, -FinalArgList) is det.
%
%   Processes an argument list (`ArgList`) by removing `->` separators and appending
%   the rest of the arguments (`Rest`) to produce a final argument list (`FinalArgList`).
%
%   @arg ArgList       The initial list of arguments.
%   @arg Rest          Additional arguments to append.
%   @arg FinalArgList  The resulting argument list.
%
arg_types([Ar | L], R, LR) :-
    Ar == '->', !,
    arg_types(L, R, LR).
arg_types([[Ar | L]], R, LR) :-
    Ar == '->', !,
    arg_types(L, R, LR).
arg_types(L, R, LR) :-
    append(L, R, LR).

%:- ensure_loaded('../../examples/factorial').
%:- ensure_loaded('../../examples/fibonacci').

%!  extreme_tracing is nondet.
%
%   Determines whether extreme tracing should be enabled. This predicate checks
%   the value of the runtime option `rrtrace`. If the `rrtrace` option is not set
%   to `false`, it succeeds, enabling extreme tracing behavior.
%
%   This allows conditional activation of detailed tracing based on configuration.
%
%   @example
%     % Enable extreme tracing when `rrtrace` is not explicitly set to `false`:
%     ?- extreme_tracing.
%     true.
%
%     % Disable extreme tracing when `rrtrace` is set to `false`:
%     ?- fast_option_value(rrtrace, false), extreme_tracing.
%     false.
%
extreme_tracing :-
    % Check if the `rrtrace` option is not set to `false`. If true, enable tracing.
    \+ fast_option_value(rrtrace, false), !.

%print_preds_to_functs:-preds_to_functs_src(factorial_tail_basic)

%!  ggtrace(+Goal) is nondet.
%
%   Executes a goal (`Goal`) with conditional tracing. If `extreme_tracing` is
%   enabled, the goal is executed using `rtrace/1` for detailed debugging. If not,
%   the predicate either fails silently or executes the goal without tracing.
%
%   @arg Goal The goal to be executed with or without tracing.
%
%   @example
%     % Execute with `rtrace` if extreme tracing is enabled:
%     ?- ggtrace(my_goal(X)).
%
%     % Fails if extreme tracing is not enabled:
%     ?- ggtrace(my_failing_goal).
%
ggtrace(G) :-
    % If extreme tracing is enabled, use `rtrace/1` for the goal.
    extreme_tracing, !, rtrace(G).
ggtrace(G) :-
    % If extreme tracing is disabled, fail silently.
    !, fail, call(G).

% ggtrace(G):- call(G).  % Original fallback clause (commented out).

%!  ggtrace0(+Goal) is det.
%
%   Executes a goal (`Goal`) with enhanced tracing options. Configures tracing behavior
%   using `leash/1`, `visible/1`, and other debugging settings. Ensures that tracing
%   starts before executing the goal and stops after its completion.
%
%   @arg Goal The goal to be executed with detailed tracing.
%
%   @example
%     % Execute a goal with enhanced tracing:
%     ?- ggtrace0(my_goal(X)).
%
ggtrace0(G) :-
    % Enable general tracing using `ggtrace/1`.
    ggtrace,
    % Suppress all Prolog debugger prompts.
    leash(-all),
    % Make all ports invisible by default.
  visible(-all),
    % Customize visibility for specific ports (e.g., call and exception).
    % debug,
    % visible(+redo),  % Uncomment to make redo ports visible.
  visible(+call),
  visible(+exception),
    % Optionally allow leash on exceptions.
    maybe_leash(+exception),
    % Setup tracing for the goal, ensuring cleanup after execution.
    setup_call_cleanup(trace, G, notrace).

:- dynamic(began_loon/1).

%!  loon is det.
%
%   The main entry point for the `loon` predicate. By default, it invokes
%   `loon(typein)` to handle user input or interactive behavior.
%
%   @example
%     % Start the loon system in its default mode:
%     ?- loon.
%
loon :-
    % Invoke the `loon/1` predicate with the default argument `typein`.
    loon(typein).

%!  catch_red_ignore(+Goal) is det.
%
%   Executes a goal (`G`) and suppresses any errors raised during execution.
%   It uses `catch_red/1` to catch errors and ignores them, always succeeding.
%
%   @arg Goal The goal to be executed.
%
%   @example
%     % Execute a goal, ignoring errors:
%     ?- catch_red_ignore(my_goal(X)).
%
catch_red_ignore(G) :-
    % Execute `catch_red/1` and always succeed, even if it fails.
    if_or_else(catch_red(G), true).

%
%   Exports the `loon/1` predicate, making it accessible to other modules.
%
:- export(loon/1).

%
%   Declares the `loon/1` predicate as public, ensuring it is visible and callable
%   even if this file is loaded into a restricted environment or module.
%
:- public(loon/1).

%!  loon(+Why) is det.
%
%   Handles the initialization or continuation of the `loon` process based on the
%   context (`Why`). The predicate considers various conditions, such as whether
%   compilation or reloading is occurring, and acts accordingly.
%
%   @arg Why A description or reason for invoking `loon`.
%
%   @example
%     % Start loon with a specific reason:
%     ?- loon(toplevel).
%

% loon(Why):- began_loon(Why),!,fbugio(begun_loon(Why)).
loon(Why) :-
    % If in compilation mode, log the event and succeed.
    is_compiling, !,
    if_trace(main,not_compatio(fbug(compiling_loon(Why)))), !.
% loon( _Y):- current_prolog_flag(os_argv,ArgV),member('-s',ArgV),!.
% Why\==toplevel,Why\==default, Why\==program,!
loon(Why) :-
    % If the program is already compiled and not in the `toplevel` phase,
    % log the event and succeed.
    is_compiled, Why \== toplevel, !,
    if_trace(main,not_compatio(fbugio(compiled_loon(Why)))), !.
loon(Why) :-
    % If `loon` has already begun for any reason, log the event and skip further processing.
    began_loon(_), !,
    if_trace(main,not_compatio(fbugio(skip_loon(Why)))).
loon(Why) :-
    % Otherwise, log the beginning of `loon`, record it, and start `do_loon`.
    if_trace(main,not_compatio(fbugio(began_loon(Why)))),
    assert(began_loon(Why)),
  do_loon.

%!  do_loon is det.
%
%   Performs the core initialization and execution tasks for `loon`. This includes:
%   - Setting up the environment (e.g., readline and output stream).
%   - Ensuring compatibility and loading essential components.
%   - Running commands and handling results.
%   - Safely ignoring errors for specific initialization steps.
%

% Avoid reloading context interference.
do_loon :- prolog_load_context(reloading, true),!.
% Execute a sequence of initialization tasks, ignoring errors where needed.
do_loon :-
   % install_readline_editline,
   % nts1,
   % install_ontology,
   metta_final, !, % saves statistics for comparison
   % ensure_corelib_types,
   % set_output_stream,
   % test_alarm,!,
   run_cmd_args,!,
   write_answer_output,!,
   not_compat_io(maybe_halt(7)),!.



do_loon_prev :-
 ignore((

        \+ prolog_load_context(reloading, true),
        maplist(catch_red_ignore, [
            % Uncomment the following lines if needed during compilation:
            % if_t(is_compiled, ensure_mettalog_py),
          install_readline_editline,
            % nts1,
            % install_ontology,
   metta_final,
   % ensure_corelib_types,
   set_output_stream,
            if_t(is_compiled, update_changed_files),
   test_alarm,
   run_cmd_args,
   write_answer_output,
            not_compat_io(maybe_halt(7))
        ])
    )), !.

%!  need_interaction is nondet.
%
%   Determines if user interaction is required. Interaction is needed if:
%   - The `had_interaction` option is not set to `true`.
%   - The system is not currently converting, compiling, or using `pyswip`.
%   - The `prolog` and `repl` options are both set to `false`.
%   - There are no active Metta files loaded.
%
%   @example
%     % Check if interaction is needed:
%     ?- need_interaction.
%
need_interaction :-
    % Check if the `had_interaction` option is not set to true.
    \+ option_value('had_interaction', true),
    % Ensure the system is not converting, compiling, or using `pyswip`.
    \+ is_converting, \+ is_compiling, \+ is_pyswip, !,
    % Check if both `prolog` and `repl` options are false.
    option_value('prolog', false),
    option_value('repl', false),
    % Ensure no Metta files are currently loaded.
    \+ metta_file(_Self, _Filename, _Directory).

%!  pre_halt1 is nondet.
%
%   Performs pre-halt tasks. This predicate checks specific conditions to decide
%   whether halting should proceed or additional steps are required:
%   - If compiling, halting is skipped.
%   - Otherwise, generates a `loonit_report` and fails to prevent halting.
%
%   @example
%     % Perform pre-halt checks:
%     ?- pre_halt1.
%
pre_halt1 :- % Skip halting if the system is compiling.
    is_compiling, !, fail.
pre_halt1 :- % Generate a `loonit_report` and fail.
    loonit_report, fail.

%!  pre_halt2 is nondet.
%
%   Performs extended pre-halt tasks. Depending on the system state, it may:
%   - Skip halting if compiling.
%   - Start the Prolog interpreter or REPL if the corresponding options are enabled.
%   - Initiate interaction if needed.
%
%   @example
%     % Perform pre-halt tasks and handle interaction:
%     ?- pre_halt2.
%
pre_halt2 :-
    % Skip halting if the system is compiling.
    is_compiling, !, fail.
pre_halt2 :-
    % If the `prolog` option is true, start Prolog and retry `pre_halt2`.
    option_value('prolog', true), !,
    set_option_value('prolog', started),
    call_cleanup(prolog, pre_halt2).
pre_halt2 :-
    % If the `repl` option is true, start the REPL and retry `pre_halt2`.
    option_value('repl', true), !,
    set_option_value('repl', started),
    call_cleanup(repl, pre_halt2).
pre_halt2 :-
    % If interaction is needed, set `had_interaction` to true and start the REPL.
    need_interaction,
    set_option_value('had_interaction', true),
    call_cleanup(repl, pre_halt2).

%loon:- time(loon_metta('./examples/compat/test_scripts/*.metta')),fail.
%loon:- repl, (option_value('halt',false)->true;halt(7)).
%maybe_halt(Seven):- option_value('prolog',true),!,call_cleanup(prolog,(set_option_value_interp('prolog',false),maybe_halt(Seven))).
%maybe_halt(Seven):- option_value('repl',true),!,call_cleanup(repl,(set_option_value_interp('repl',false),maybe_halt(Seven))).
%maybe_halt(Seven):- option_value('repl',true),!,halt(Seven).

%!  maybe_halt(+ExitCode) is det.
%
%   Handles system halting logic with pre-halt checks and conditional behavior
%   based on runtime options and flags. The predicate evaluates multiple conditions
%   before deciding whether to halt the system, run the REPL, or perform additional tasks.
%
%   @arg ExitCode The exit code to use if the system halts.
%
%   @example
%     % Attempt to halt the system with exit code 7:
%     ?- maybe_halt(7).
%
maybe_halt(_) :-
    % Perform preliminary halting checks (`pre_halt1`) and fail.
    once(pre_halt1), fail.
maybe_halt(Seven) :-
    % If the REPL is disabled (`repl = false`), halt with the specified exit code.
    option_value('repl', false), !, halt(Seven).
maybe_halt(Seven) :-
    % If halting is explicitly enabled (`halt = true`), halt with the specified exit code.
    option_value('halt', true), !, halt(Seven).
maybe_halt(_) :-
    % Perform extended halting checks (`pre_halt2`) and fail.
    once(pre_halt2), fail.
maybe_halt(Seven) :-
    % Log the halting attempt and fail.
    if_trace(main,not_compatio(fbugio(maybe_halt(Seven)))), fail.
maybe_halt(_) :-
    % If the Prolog runtime flag `mettalog_rt` is true, prevent halting.
    current_prolog_flag(mettalog_rt, true), !.
maybe_halt(H) :-
    % If no other conditions apply, halt with the specified exit code.
    halt(H).

%
%   Runs initialization steps when the program is loaded. These include setting
%   the `cmt_override` variable and restoring the system state.
%
:- on_metta_setup(nb_setval(cmt_override, lse('; ', ' !(" ', ' ") '))).

% needs_repl:- \+ is_converting, \+ is_pyswip, \+ is_compiling, \+ has_file_arg.

%  libswipl: ['./','-q',--home=/usr/local/lib/swipl]

%
%   Displays the operating system arguments (`os_argv`) during initialization.
%
:- on_metta_setup(show_os_argv).

%!  ensure_mettalog_system_compilable is det.
%
%   Ensures that the MettaLog system is in a compilable state. This involves
%   loading necessary modules and initializing the environment.
%
%   @example
%     % Prepare the system for compilation:
%     ?- ensure_mettalog_system_compilable.
%
ensure_mettalog_system_compilable:-
    %ensure_loaded(library(metta_python)),
    ensure_mettalog_system.

%!  ensure_mettalog_system is det.
%
%   Sets up the MettaLog system by loading essential modules and performing
%   initialization tasks. This includes:
%   - Resetting dynamic predicates.
%   - Loading standard Prolog libraries.
%   - Enabling autoloading and recompilation of code.
%
%   @example
%     % Prepare the MettaLog system:
%     ?- ensure_mettalog_system.
%
ensure_mettalog_system:-
 must_det_lls((
    abolish(began_loon/1),
    dynamic(began_loon/1),
    system:use_module(library(quasi_quotations)),
    system:use_module(library(hashtable)),
    system:use_module(library(gensym)),
    system:use_module(library(sort)),
    system:use_module(library(writef)),
    system:use_module(library(rbtrees)),
    system:use_module(library(dicts)),
    system:use_module(library(shell)),
    use_module(library(date)),
    system:use_module(library(edinburgh)),
  %  system:use_module(library(lists)),
    system:use_module(library(statistics)),
    system:use_module(library(nb_set)),
    system:use_module(library(assoc)),
    system:use_module(library(pairs)),
    if_t(exists_source(library(swi_ide)),user:use_module(library(swi_ide))),
    user:use_module(library(prolog_profile)),
    %metta_python,
    %ensure_loaded('./src/main/flybase_convert'),
    %ensure_loaded('./src/main/flybase_main'),
    %ensure_loaded(library(flybase_convert)),
    %ensure_loaded(library(flybase_main)),
    %autoload_all,
    %make,
    autoload_all,
    %pack_install(predicate_streams, [upgrade(true),global(true)]),
    %pack_install(logicmoo_utils, [upgrade(true),global(true)]),
    %pack_install(dictoo, [upgrade(true),global(true)]),
    true)),!.

%!  file_save_name(+File, -SaveName) is nondet.
%
%   Determines the save name for a given file (`File`). The save name is derived
%   based on specific prefixes (`Sav.` or `Bin.`) or by processing the file's base name.
%
%   @arg File     The input file to process.
%   @arg SaveName The resulting save name.
%
file_save_name(E, _) :-
    % Fail if `E` is not a symbol.
    \+ symbol(E), !, fail.
file_save_name(E, Name) :-
    % If the base name of `E` differs from `E`, recursively derive the save name.
    file_base_name(E, BN), BN \== E, !,
    file_save_name(BN, Name).
file_save_name(E, E) :-
    % Accept names prefixed with `Sav.`.
    symbol_concat('Sav.', _, E), !.
file_save_name(E, E) :-
    % Accept names prefixed with `Bin.`.
    symbol_concat('Bin.', _, E), !.

%!  before_underscore(+Symbol, -Prefix) is nondet.
%
%   Extracts the part of a symbol (`Symbol`) before the first underscore (`_`).
%
%   @arg Symbol The input symbol to process.
%   @arg Prefix The part of the symbol before the underscore.
%
before_underscore(E, N) :-
    symbolic_list_concat([N | _], '_', E), !.

%!  save_name(-Name) is det.
%
%   Determines the current save name based on the operating system arguments.
%   The first matching file name derived by `file_save_name/2` is returned.
%
%   @arg Name The resulting save name.
%
save_name(Name) :-
    current_prolog_flag(os_argv, ArgV),
    member(E, ArgV),
    file_save_name(E, Name), !.

%!  next_save_name(-Name) is det.
%
%   Generates the next save name for the program. The name is derived from the
%   current save name or a predefined option (`exeout`). It ensures that the
%   generated name does not conflict with an existing file.
%
%   @arg Name The resulting save name.
%
next_save_name(SavMeTTaLog) :-
    % Use the `exeout` option if it is valid and sufficiently long.
    option_value(exeout, SavMeTTaLog),
    symbolic(SavMeTTaLog),
    atom_length(SavMeTTaLog, Len),
    Len > 1, !.
next_save_name(Name) :-
    % Derive the name from the current save name and append a unique suffix.
    save_name(E),
    before_underscore(E, N),
    symbol_concat(N, '_', Stem),
    gensym(Stem, Name),
  \+ exists_file(Name),
    Name \== E, !.
next_save_name('Sav.MeTTaLog').

%!  qcompile_mettalog is det.
%
%   Compiles the MettaLog system into a standalone executable program.
%   The program name is derived from the `exeout` option. If an error occurs,
%   it is caught and displayed.
%
qcompile_mettalog :-
 must_det_lls((
    % Ensure the system is initialized.
    ensure_mettalog_system,
    % Retrieve the target executable name from the `exeout` option.
    cmdline_flag_option_value(exeout, Name),
    % Attempt to save the program as an executable.
    qsave_program(Name), nonvar(Name),
    % Exit the program with success status.
    true)),
    inform_compiler_success,
    halt(0).

inform_compiler_success:- ignore((catch((getenv('METTALOG_COMPILE_SUCCESS',STAMP),tell(STAMP),told),_,true))).

%!  qsave_program is det.
%
%   Saves the current Prolog program to a file as a non-standalone executable.
%   The save name is generated using `next_save_name/1`.
%

qsave_program:-
 must_det_lls((
    % Ensure the system is initialized.
    ensure_mettalog_system,
    % Generate the next save name.
    next_save_name(Name), nonvar(Name),
    % Attempt to save the program as an executable.
    qsave_program(Name),
    % Exit the program with success status.
    true)),halt(0).

qsave_program(Name) :-
    % Attempt to save the program.
    if_verbose(main,write_src_nl(start(qsave_program(Name)))),
    catch_err(qsave_program(Name, [
        class(development),
        autoload(true),
        goal(loon(goal)),
        toplevel(loon(toplevel)),
        stand_alone(false)
    ]), E, writeln(E)), !,
    if_verbose(main,write_src_nl(done(qsave_program(Name)))).

:- ensure_loaded(library(flybase_main)).
:- ensure_loaded(metta_server).


:- initialization(update_changed_files).


%!  nts is det.
%
%   Main entry point for initializing or running `nts1`.
%
% nts :- nts1.

%!  nts1 is det.
%
%   Configures or redefines specific system predicates. The first clause
%   disables further redefinition by cutting execution early. If redefinition
%   is allowed, it handles modifications to `system:notrace/1` to customize its behavior.
%

%nts1 :- !. % Disable redefinition by cutting execution.
%nts1 :- is_flag(notrace),!.
nts1 :-
    % Redefine the system predicate `system:notrace/1` to customize its behavior.
    redefine_system_predicate(system:notrace/1),
  %listing(system:notrace/1),
    % Remove the existing definition of `system:notrace/1`.
  abolish(system:notrace/1),
    % Declare `system:notrace/1` as a dynamic predicate, allowing runtime modifications.
  dynamic(system:notrace/1),
    % Define the meta-predicate behavior for `system:notrace/1`.
  meta_predicate(system:notrace(0)),
    % Define the new behavior for `system:notrace/1`.
    % The redefined version executes the goal (`G`) with `once/1` and succeeds deterministically.
    asserta(( system:notrace(G) :- (!, once(G) ))).
nts1 :-
    % Ensure that further redefinitions of `nts1` are not allowed after the first.
    !.

%:-nts1.
:- initialization(nts1).
%!  nts0 is det.
%
%   Configures or redefines the `system:notrace/0` predicate.
%   The redefined version writes debug information to `write_src_uo/1` when called.
%
nts0 :-
    % Redefine the system predicate `system:notrace/0`.
    redefine_system_predicate(system:notrace/0),
    % Remove the existing definition of `system:notrace/0`.
  abolish(system:notrace/0),
    % Define the new behavior for `system:notrace/0`.
    % The redefined version writes debug output for tracing.
    asserta((
        system:notrace :-
            write_src_uo(notrace)
    )).

% Optionally initialize `nts0`.
% :- nts0.

%!  override_portray is det.
%
%   Overrides the default `portray/1` behavior in the user module. Existing
%   clauses for `user:portray/1` are moved to `user:portray_prev/1` for backup.
%   A new `user:portray/1` implementation is then asserted, which delegates
%   portrayal to `metta_portray/1`.
%
%   @example
%     % Override the `portray/1` predicate:
%     ?- override_portray.
%
override_portray :-
    % For each existing clause of `user:portray/1`:
    forall(
        clause(user:portray(List), Where:Body, Cl),
        (
            % Backup the existing clause as `user:portray_prev/1`.
            assert(user:portray_prev(List) :- Where:Body),
            % Erase the original clause.
            erase(Cl)
        )
    ),
    % Add a new `user:portray/1` clause that delegates to `metta_portray/1`.
    asserta((user:portray(List) :- metta_portray(List))).

%!  metta_message_hook(+A, +B, +C) is nondet.
%
%   Handles error messages by invoking `fbug/1` for logging when `B == error`.
%   Fails to indicate that the message should be handled by the default mechanism.
%
%   @arg A The message identifier.
%   @arg B The message kind (e.g., `error`).
%   @arg C Additional message context.
%
metta_message_hook(A, B, C) :-
    user:(
        % If the message kind is `error`, log it and fail.
        B == error,
          fbug(metta_message_hook(A, B, C)),
          fail
      ).

%!  override_message_hook is det.
%
%   Overrides the `message_hook/3` predicate in the user module. Existing clauses
%   for `user:message_hook/3` are moved to a backup version. A new implementation
%   is added to delegate message handling to `metta_message_hook/3`.
%
%   @example
%     % Override the `message_hook/3` predicate:
%     ?- override_message_hook.
%
override_message_hook :-
    % For each existing clause of `user:message_hook/3`:
      forall(
        clause(user:message_hook(A, B, C), Where:Body, Cl),
        (
            % Backup the existing clause as a new version.
            assert(user:message_hook(A, B, C) :- Where:Body),
            % Erase the original clause.
            erase(Cl)
        )
    ),
    % Add a new `user:message_hook/3` clause that delegates to `metta_message_hook/3`.
    asserta((user:message_hook(A, B, C) :- metta_message_hook(A, B, C))).

%!  fix_message_hook is det.
%
%   Fixes the `message_hook/3` predicate by removing specific clauses where the
%   message kind is `error`. The identified clause is erased after logging it.
%
%   @example
%     % Fix the `message_hook/3` predicate:
%     ?- fix_message_hook.
%
fix_message_hook :-
    % Identify clauses of `message_hook/3` where `B == error`.
    clause(
        message_hook(A, B, C),
        user:(
            B == error,
            fbug(user:message_hook(A, B, C)),
            fail
        ),
        Cl
    ),
    % Erase the identified clause.
    erase(Cl).

:- on_metta_setup(unnullify_output).

%:- ensure_loaded(metta_python).

%:- ensure_loaded('../../library/genome/flybase_loader').

:- ensure_loaded(metta_python).
:- ensure_loaded(metta_corelib).
%:- ensure_loaded(metta_help).

%:- initialization( enter_comment).

%!  stack_times_16 is det.
%
%   Increases the Prolog stack limit to 16 times its current value. This is useful
%   in scenarios requiring significantly more stack space, such as deep recursion
%   or handling large datasets.
%
%   @example
%     % Increase the stack limit by 16 times:
%     ?- stack_times_16.
%
stack_times_16 :-
    % Retrieve the current stack limit.
    current_prolog_flag(stack_limit, X),
    % Calculate the new stack limit as 16 times the current value.
    X_16 is X * 16,
    % Set the new stack limit.
    set_prolog_flag(stack_limit, X_16).

:- initialization(stack_times_16,after_load /**/).
:- initialization(use_corelib_file,after_load /**/).
:- initialization(use_metta_ontology,after_load /**/).

%!  immediate_ignore is det.
%
%   Executes a sequence of initialization steps, ignoring any errors or failures
%   that occur during execution. The predicate includes several key tasks for
%   setting up the environment and running finalization steps.
%
%   @example
%     % Perform immediate initialization while ignoring errors:
%     ?- immediate_ignore.
%
immediate_ignore:- ignore(((
   %write_src_uo(init_prog),
   use_corelib_file,
   (is_testing -> UNIT_TEST=true; UNIT_TEST=false),
   set_is_unit_test(UNIT_TEST),
   %trace,
   \+ prolog_load_context(reloading,true),
   initialization(loon(after_load /**/),after_load /**/),
   % should fail (if tested from here https://swi-prolog.discourse.group/t/call-with-time-limit-2-not-enforcing-time-limit-as-expected/7755)
   %test_alarm,
   % nts1,
   metta_final,
   true))).

%!  use_metta_ontology is det.
%
%   Loads the `metta_ontology` library to initialize the ontology used in the MettaLog system.
%   This ensures that all necessary predicates and rules defined in the ontology are available
%   for use. The library is expected to contain Prolog facts and rules for the system.
%
%   @example
%     % Load the Metta ontology:
%     ?- use_metta_ontology.
%
use_metta_ontology:- ensure_loaded(library('metta_ontology.pfc.pl')).
% use_metta_ontology:- load_pfc_file('metta_ontology.pl.pfc').
%:- use_metta_ontology.
%:- initialization(use_metta_ontology).
%:- initialization(loon(program),program).
%:- initialization(loon(default)).

%!  flush_metta_output is det.
%
%   Ensures that any pending output is flushed to the terminal, ensuring smooth
%   runtime interactions. This predicate is typically used to clear the output buffer
%   after writing results or messages.
%
%   @example
%     % Flush pending output to the terminal:
%     ?- flush_metta_output.
%
flush_metta_output :-
    % Write any pending answers to the output and flush the terminal buffer.
    write_answer_output,
    ttyflush.

%!  metta_runtime_write_answers(+List) is det.
%
%   Writes a list of answers in `hyperon-experimental` format to the `user_error` stream.
%   The output is formatted as a JSON-like list, providing an easy way to analyze
%   results in real-time.
%
%   @arg List A list of answers to be written.
%
%   @example
%     % Write answers to the user_error stream:
%     ?- metta_runtime_write_answers([answer1, answer2]).
%
%     Output:
%     [answer1, answer2]
%
metta_runtime_write_answers(List) :-
    % Write the list of answers to `user_error` in JSON-like format.
    with_output_to(user_error, (
        write('['), write_answers_aux(List), write(']')
    )).

%!  write_answers_aux(+List) is det.
%
%   Helper predicate for `metta_runtime_write_answers/1` that formats and writes
%   each element of the list to the `user_error` stream. Elements are separated by commas.
%
%   @arg List A list of answers to be written.
%
%   @example
%     % Format and write a list of answers:
%     ?- write_answers_aux([answer1, answer2]).
%
%     Output:
%     answer1, answer2
%
write_answers_aux([]) :-
    % Stop when the list is empty.
    !.
write_answers_aux([H|T]) :-
    % Write the current element (`H`) to `user_error`.
    with_output_to(user_error, (
        write_src_woi(H),
        % If there are more elements, write a comma and continue.
        (T == [] -> true ; write(', '), write_answers_aux(T))
    )).

%!  file_desc(-Message) is det.
%
%   Dynamically describes the current file or an actively reading file. This predicate
%   provides context for runtime sessions by indicating which file is being loaded or
%   processed.
%
%   @arg Message A string describing the file context.
%
%   @example
%     % Get the description of the current file:
%     ?- file_desc(Message).
%     Message = "File(current_file_name.pl)".
%
file_desc(Message) :-
    % Get the current file from the Prolog load context.
    prolog_load_context(file, CurrentFile),
    (
        % Check if there is an open stream in read mode that is not at the end.
        stream_property(Stream, mode(read)),
        stream_property(Stream, file_name(File)),
        \+ at_end_of_stream(Stream),
        File \= CurrentFile,
        !,
        % If an active file is being read, use its name.
        sformat(Message, 'File(~w)', [File])
    ;
        % Otherwise, use the current file.
        sformat(Message, 'File(~w)', [CurrentFile])
    ).

:- dynamic(runtime_session/4).

%!  begin_metta_runtime is det.
%
%   Starts a Metta runtime session by recording the wall clock and CPU start times.
%   The runtime session details are stored dynamically and a message is written
%   to `user_error` indicating the session start.
%
%   @example
%     % Start a runtime session:
%     ?- begin_metta_runtime.
%
begin_metta_runtime :-
    % Get a description of the current file or active file context.
    file_desc(Description),
    % Record the current wall clock and CPU times.
    current_times(WallStart, CPUStart),
    % Store the runtime session details dynamically.
    asserta(runtime_session(start, WallStart, CPUStart, Description)),
    % Write the start message to `user_error`.
    with_output_to(user_error, format('~w started.~n', [Description])).

%!  end_metta_runtime is det.
%
%   Ends a Metta runtime session by calculating and printing the elapsed wall
%   clock and CPU times. If no session start information is found, an error
%   message is written to `user_error`.
%
%   @example
%     % End a runtime session:
%     ?- end_metta_runtime.
%
end_metta_runtime :-
    % Get a description of the current file or active file context.
    file_desc(Description),
    % Attempt to retract the stored runtime session details.
    (   retract(runtime_session(start, WallStart, CPUStart, Description))
    ->  % If successful, calculate and print the elapsed times.
        calculate_elapsed_time(WallStart, CPUStart, WallElapsedTime, CPUElapsedTime),
        print_elapsed_time(WallElapsedTime, CPUElapsedTime, Description)
    ;   % If no session start information is found, log an error message.
        with_output_to(user_error,
            format('Error: No runtime session start information found for "~w".~n', [Description]))
    ).

%!  current_times(-WallStart, -CPUStart) is det.
%
%   Retrieves the current wall clock and CPU start times.
%
%   @arg WallStart The current wall clock time.
%   @arg CPUStart  The current CPU time.
%
current_times(WallStart, CPUStart) :-
    % Get the current wall clock time.
    get_time(WallStart),
    % Get the current CPU time.
    statistics(cputime, CPUStart).

%!  calculate_elapsed_time(+WallStart, +CPUStart, -WallElapsedTime, -CPUElapsedTime) is det.
%
%   Calculates the elapsed wall clock and CPU times by comparing the current
%   times with the recorded start times.
%
%   @arg WallStart        The wall clock start time.
%   @arg CPUStart         The CPU start time.
%   @arg WallElapsedTime  The calculated elapsed wall clock time.
%   @arg CPUElapsedTime   The calculated elapsed CPU time.
%
calculate_elapsed_time(WallStart, CPUStart, WallElapsedTime, CPUElapsedTime) :-
    % Get the current wall clock and CPU times.
    current_times(WallEnd, CPUEnd),
    % Calculate the elapsed wall clock time.
    WallElapsedTime is WallEnd - WallStart,
    % Calculate the elapsed CPU time.
    CPUElapsedTime is CPUEnd - CPUStart.

%!  print_elapsed_time(+WallElapsedTime, +CPUElapsedTime, +Description) is det.
%
%   Prints the elapsed wall clock and CPU times with a description to `user_error`.
%
%   @arg WallElapsedTime The elapsed wall clock time.
%   @arg CPUElapsedTime  The elapsed CPU time.
%   @arg Description     The description of the runtime session.
%
%   @example
%     % Print the elapsed times:
%     ?- print_elapsed_time(5.123, 4.567, "File(metta_example.pl)").
%
print_elapsed_time(WallElapsedTime, CPUElapsedTime, Description) :-
    with_output_to(user_error,
        format('~N          % Walltime: ~9f seconds, CPUtime: ~9f seconds for ~w~n',
               [WallElapsedTime, CPUElapsedTime, Description])).

%!  do_metta_runtime(+Var, +Call) is det.
%
%   Executes a Prolog query (`Call`) with performance logging, time measurement,
%   and result handling. The results are output to `user_error`, including
%   execution details, elapsed times, and formatted results.
%
%   @arg Var  The variable used for collecting results.
%   @arg Call The Prolog query or goal to be executed.
%
%   @example
%     % Execute a runtime query:
%     ?- do_metta_runtime(Result, my_goal(X)).
%
do_metta_runtime(_Var, _Call) :-
    % Skip execution if the `compile` option is set to `save`.
    fast_option_value(compile, save), !.
do_metta_runtime(Var, Eval) :-
    % If `Eval` is a list, compile it into an executable query (`Call`) and execute.
    is_list(Eval),
    compile_for_exec(Var, Eval, Call), !,
    do_metta_runtime(Var, Call).
do_metta_runtime(Var, Goal) :-
    % Handle cases where `Var` is a flexible type variable (ftVar) by substituting
    % it into the `Goal`, producing a new query to execute.
    nonvar(Var),
    is_ftVar(Var),
    subst(Goal, Var, NewVar, Call), !,
    do_metta_runtime(NewVar, Call).
do_metta_runtime(Var, Call) :-
    % Extract the functor name from the goal (`Call`) to create a description.
    functor(Call, Func, _),
    atom_concat('Testing ', Func, Description),
    % Record the start times (wall clock and CPU time).
    current_times(WallStart, CPUStart),
    % Execute the query and collect results, outputting progress to `user_error`.
    with_output_to(user_error, findall_or_skip(Var, Call, List)),
    % Calculate elapsed times (wall clock and CPU).
    calculate_elapsed_time(WallStart, CPUStart, WallElapsedTime, CPUElapsedTime),
    % Output the collected results in a formatted manner.
    with_output_to(user_error, metta_runtime_write_answers(List)),
    % Print the elapsed times for the query execution.
    print_elapsed_time(WallElapsedTime, CPUElapsedTime, Description),
    % Flush any pending output to ensure smooth runtime interactions.
    flush_metta_output.

%!  findall_or_skip(+Var, +Call, -List) is det.
%
%   Executes a `findall/3` query unless the `exec` option is set to `skip`.
%   If skipping is enabled, logs the skipped query execution to `user_error`.
%
%   @arg Var  The variable to collect results into.
%   @arg Call The query or goal to be executed.
%   @arg List The list of results collected by `findall/3`.
%
findall_or_skip(Var, Call, []) :-
    % If the `exec` option is set to `skip`, log the skipped execution and return an empty list.
    fast_option_value(exec, skip), !,
    once_writeq_nl_now(red, (skipping :- time(findall(Var, Call, _List)))).
findall_or_skip(Var, Call, List) :-
    % Execute the query using `findall/3` to collect results into `List`.
    findall(Var, Call, List).

:- initialization(set_prolog_flag(metta_interp,ready)).
%:- ensure_loaded(metta_runtime).
%:- initialization(set_prolog_flag(gc,false).

%:- initialization(trace, now).
:- use_module(library(clpr)). % Import the CLP(R) library
%:- initialization(loon_main, main).
:- initialization(loon(main), main).

%:- ensure_loaded(mettalog('metta_ontology.pfc.pl')).


%!  complex_relationship3_ex(+Likelihood1, +Likelihood2, +Likelihood3) is nondet.
%
%   Define a predicate to relate the likelihoods of three events.
%
%   Describes a set of constraints between three likelihood variables:
%   - `Likelihood1` is 30% of `Likelihood2`.
%   - `Likelihood2` is 50% of `Likelihood3`.
%   - `Likelihood3` must be between 0 and 1 (exclusive).
%
%   @arg Likelihood1 The first likelihood variable.
%   @arg Likelihood2 The second likelihood variable.
%   @arg Likelihood3 The third likelihood variable, which bounds the relationships.
%
complex_relationship3_ex(Likelihood1, Likelihood2, Likelihood3) :-
    { Likelihood1 = 0.3 * Likelihood2 },
    { Likelihood2 = 0.5 * Likelihood3 },
    { Likelihood3 < 1.0 },
    { Likelihood3 > 0.0 }.

% Example query to find the likelihoods that satisfy the constraints
%?- complex_relationship(L1, L2, L3).

:- find_missing_cuts.


:- thread_initialization(do_metta_setup).

