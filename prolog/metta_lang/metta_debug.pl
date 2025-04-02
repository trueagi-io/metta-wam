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
% PROGRAM FUNCTION: Provides predicates useful for debugging, such as tracing logical derivations
% and interactively exploring justifications for derived facts.
%*********************************************************************************************

% When the the `metta_interp` library is loaded, it makes sure the rest of the files are loaded in
% the correct order independent of which file is loaded first and the needed predicates and ops are defined.
:- ensure_loaded(metta_interp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   The `is_cached_call/3` predicate is used to store information about calls that
%   have been executed before, along with their results and the time they were cached.
%   The predicate is declared `dynamic` to allow modification during runtime.
:- dynamic(is_cached_call/3).

%!  cached_call(+ForSeconds, :Call) is nondet.
%
%   Attempts to use cached results for `Call`, or executes `Call` if no valid cache is present.
%
%   This predicate checks if a cached result for the given `Call` exists and is still valid based on
%   the expiration time `ForSeconds`. If a valid cached result is found, it is used; otherwise, the
%   `Call` is executed and its result is cached for future use.
%
%   @arg ForSeconds The number of seconds for which the cached result remains valid. If the cached
%        result is older than this, the `Call` is executed again.
%   @arg Call The goal to be executed or retrieved from the cache. If the result is not cached or
%        the cache has expired, the `Call` is executed and cached.
%
%   @example
%   % Execute a goal and cache the result for 60 seconds:
%   ?- cached_call(60, member(X, [1, 2, 3])).
%
cached_call(ForSeconds, Call) :-
    get_time(CurrentTime), % Get the current time for cache validation.
    copy_term(Call, CallCopied), % Create a copy of the Call for consistent comparison.
    numbervars(CallCopied, 0, _, [attvar(bind)]), % Ensure variables in Call are standardized.
    NewerThan is CurrentTime - ForSeconds, % Calculate the cutoff time for cache validity.
    (
        % Check if a valid cache entry exists.
        is_cached_call(CallCopied, CachedTime, Result),
        NewerThan > CachedTime
    ->
        true % Use cached result if valid.
    ;
        % Otherwise, execute Call and update cache.
        (retractall(is_cached_call(CallCopied, _, _)), % Remove any existing cache for Call.
        call_ndet(Call, IsLast), % Execute the Call, expecting it to be nondeterministic.
        nop(assertion(IsLast)), % Assert that the last call succeeded, for debugging purposes.
        assertz(is_cached_call(CallCopied, CurrentTime, Result)) % Cache the new result.
        )
    ),
    Call = Result. % Return the result.

%!  debugging_metta(+G) is nondet.
%
%   Debugging utility for metta-related goals.
%
%   This predicate checks whether debugging for the `eval` predicate is enabled. If debugging is
%   active, it attempts to execute the goal `G` within a `notrace` block, which prevents tracing
%   of the execution. If debugging is not active, the predicate succeeds without executing `G`.
%
%   @arg G The goal related to metta that will be conditionally executed based on the debugging status.
%
%   @example
%   % Run a goal with debugging enabled:
%   ?- debugging_metta(my_goal).
%
debugging_metta(G) :- notrace((is_debugging((eval)) -> ignore(G); true)).

%   Ensures that debugging for `metta(eval)` is disabled.
:- nodebug(metta(eval)). % Ensure no debugging for metta(eval).

%!  depth_to_use(+InDepth, -UseThis) is det.
%
%   Determine a depth value to use, based on a modulo operation.
%
%   This predicate calculates a depth value to use based on the input depth `InDepth`.
%   The depth to use is computed by taking the absolute value of `InDepth` and performing
%   a modulo 50 operation. If the modulo operation fails or `InDepth` is invalid, it defaults
%   to a depth of 5.
%
%   @arg InDepth The input depth, which can be any integer.
%   @arg UseThis The actual depth to use, calculated as `InDepth mod 50`. If no valid
%        result is computed, it defaults to 5.
%
%   @example
%   % Calculate a depth based on modulo 50:
%   ?- depth_to_use(123, Depth).
%   Depth = 23.
%
%   % If the input depth is negative:
%   ?- depth_to_use(-75, Depth).
%   Depth = 25.
%
depth_to_use(InDepth, UseThis) :-
    Depth is abs(InDepth), % Ensure the depth is non-negative.
    UseThis is Depth mod 50, % Calculate modulo 50.
    !. % Cut to prevent backtracking.
depth_to_use(_InDepth, 5). % Default to depth 5 if other cases fail.

%!  w_indent(+Depth, :Goal) is det.
%
%   Executes a goal with indentation based on the specified depth.
%
%   This predicate performs a goal `Goal` with output indentation determined by the given `Depth`.
%   The actual indentation is computed using `depth_to_use/2`, which ensures the depth is adjusted
%   by a modulo operation. It then executes the `Goal` and manages indentation before and after
%   the execution.
%
%   @arg Depth The depth value used to calculate the amount of indentation.
%   @arg Goal The goal to be executed with indentation applied before and after.
%
%   @example
%   % Execute a goal with indentation based on a depth of 10:
%   ?- w_indent(10, writeln('Indented text')).
%
w_indent(Depth, Goal) :-
    must_det_ll((
        depth_to_use(Depth, UseThis), % Determine the depth to use.
        format('~N'), % Start a new line.
        setup_call_cleanup(i_this(UseThis), Goal, format('~N')) % Execute the goal with indentation.
    )).

%!  i_this(+UseThis) is det.
%
%   Helper predicate to create indentation based on depth.
%
%   This predicate generates indentation by writing spaces for the specified depth `UseThis`.
%   It writes two spaces for each unit of depth, followed by a delimiter (`;;`). Any errors
%   during the indentation process are safely caught and ignored.
%
%   @arg UseThis The number of indentation units (each unit results in two spaces).
%
%   @example
%   % Create indentation for a depth of 5:
%   ?- i_this(5).
%   '          ;;'  % 10 spaces followed by ';;'.
%
i_this(UseThis) :-
    ignore(catch(forall(between(1, UseThis, _), write('  ')), _, true)), % Write indentation spaces.
    write(';;'). % End with a delimiter

%!  indentq2(+Depth, +Term) is det.
%
%   Print a term with indentation based on the specified depth.
%
%   This predicate prints the given `Term` with indentation determined by the `Depth`.
%   It uses the `w_indent/2` predicate to handle the indentation. If indentation fails
%   or is unnecessary, it falls back to printing the term without indentation.
%
%   @arg Depth The depth used to determine the amount of indentation.
%   @arg Term  The term to print.
%
%   @example
%   % Print a term with indentation for a depth of 3:
%   ?- indentq2(3, some_term(foo, bar)).
%   '    some_term(foo, bar)'  % Indented by 3 units.
%
indentq2(Depth, Term) :-
    w_indent(Depth, format('~q', [Term])), % Print the term with indentation.
    !.
indentq2(_Depth, Term) :-
    format('~q', [Term]). % Fallback printing without indentation.

%!  print_padded(+TraceLen, +DR, +AR) is det.
%
%   Print a padded line with extra formatting, if certain conditions are met.
%
%   This predicate prints a formatted line based on the values of `TraceLen`, `DR`, and `AR`, with padding and
%   additional formatting. If `is_fast_mode/0` is enabled, the printing is skipped. The padding is computed
%   using modulo operations on the `DR` value, and extra formatting is applied. The `AR` component is printed
%   after the padding.
%
%   @arg TraceLen The first component used for padding, expected to be an integer.
%   @arg DR The second component used for padding, expected to be an integer.
%   @arg AR The component to print after padding.
%
%   @example
%   % Print padded values:
%   ?- print_padded(5, 10, 'Result').
%   '          |   |   |    Result'
%
print_padded(_DR, _EX, _AR) :-
    is_fast_mode, !. % Skip printing in fast mode.
print_padded(TraceLen, DR, AR) :-
    integer(TraceLen), integer(DR), TraceLen > 0, DR > 0,
    nb_current('$print_padded', print_padded(TraceLen, DR, _)), % Check if padding is active.
    !,
    format("~|          |", []), % Print the initial padding.
    DRA is abs(round(DR) mod 24), % Calculate padding size.
    forall(between(2, DRA, _), write('   |')), % Write additional padding.
    write('    '), write(' '), write(AR). % Write the AR value.
print_padded(TraceLen, DR, AR) :-
    format("~|~` t~d~5+:~d~5+|", [TraceLen, DR]), % Print padded TraceLen and DR values.
    nb_setval('$print_padded', print_padded(TraceLen, DR, AR)), % Set the current padding.
    DRA is abs(round(DR) mod 24), % Calculate padding size.
    forall(between(1, DRA, _), write('   |')), % Write additional padding.
    write('-'), write(AR). % Write the AR value.

%!  indentq_d(+Depth, +Prefix4, +Message) is det.
%
%   Print a message with depth-based indentation and a specified prefix.
%
%   This predicate prints a message `Message` with indentation determined by `Depth`.
%   It includes a prefix `Prefix4` and adjusts the indentation and format based on
%   internal flags and modulo operations. If `is_fast_mode/0` is active, the printing is skipped.
%
%   @arg Depth   The depth used to calculate indentation.
%   @arg Prefix4 A string prefix to include before the message.
%   @arg Message The message to print with indentation and prefix.
%
%   @example
%   % Print a message with indentation and prefix:
%   ?- indentq_d(10, 'INFO:', 'Processing started').
%   'INFO:       Processing started'  % The message with depth-based indentation.
%
%   % Skip printing in fast mode:
%   ?- is_fast_mode, indentq_d(10, 'INFO:', 'Processing started').
%   % No output is produced.
%
indentq_d(_DR, _EX, _AR) :-
    is_fast_mode, !. % Skip printing in fast mode.
indentq_d(Depth, Prefix4, Message) :-
    flag(eval_num, EX0, EX0), % Get the current evaluation number.
    TraceLen is EX0 mod 500, % Compute TraceLen using modulo 500.
    DR is 99 - (Depth mod 100), % Compute DR using depth and modulo 100.
    indentq(DR, TraceLen, Prefix4, Message). % Call indentq with the formatted values.

%!  indentq(+DR, +TraceLen, +AR, +Term) is det.
%
%   Print a term with depth-based and TraceLen-based indentation.
%
%   This predicate prints a `Term` with indentation based on the values of `DR` (depth)
%   and `TraceLen` (a component used for formatting). The `AR` component is included in the
%   formatting as well. Special cases are handled for return values, list elements, and
%   structured terms. If `is_fast_mode/0` is enabled, the predicate skips printing.
%
%   @arg DR   The depth used to determine the indentation.
%   @arg TraceLen   The TraceLen component for additional formatting control.
%   @arg AR   The AR component for formatting or additional text.
%   @arg Term The term to print, which could be a return value, a list element, or a structured term.
%
%   @example
%   % Print a structured term with indentation:
%   ?- indentq(10, 5, 'INFO:', ste('start', 'processing', 'end')).
%   'INFO: start processing end'  % Printed with depth-based indentation.
%
%   % Skip printing in fast mode:
%   ?- is_fast_mode, indentq(10, 5, 'INFO:', 'processing').
%   % No output is produced.
%
indentq(_DR, _EX, _AR, _Term) :-
    is_fast_mode, !. % Skip printing in fast mode.
indentq(DR, TraceLen, AR, retval(Term)) :-
    nonvar(Term), !,
    indentq(DR, TraceLen, AR, Term). % Handle return values specially.
indentq(DR, TraceLen, AR, [E, Term]) :-
    E == e, !,
    indentq(DR, TraceLen, AR, Term). % Special case for list elements.
%indentq(_DR,_EX,_AR,_Term):- flag(trace_output_len,X,X+1), XX is (X mod 1000), XX>100,!.
indentq(DR, TraceLen, AR, ste(S, Term, E)) :- !,
    indentq(DR, TraceLen, AR, S, Term, E). % Special case for structured terms.
indentq(DR, TraceLen, AR, Term) :-
    indentq(DR, TraceLen, AR, '', Term, ''). % Default case with empty prefix/suffix.

%!  indentq(+DR, +TraceLen, +AR, +S, +Term, +E) is det.
%
%   Print a term with depth-based indentation, including start and end strings.
%
%   This predicate prints a `Term` with indentation based on the depth `DR` and formatting components
%   `TraceLen` and `AR`. The `S` argument provides a start string to print before the term, and `E` provides
%   an end string to print after the term. The predicate formats the term, converts any newlines to
%   spaces, and then prints the formatted string. The output is managed within a `setup_call_cleanup/3`
%   block to ensure clean execution.
%
%   @arg DR   The depth used to determine the indentation.
%   @arg TraceLen   The TraceLen component for additional formatting control.
%   @arg AR   The AR component for formatting or additional text.
%   @arg S    A string to print before the term.
%   @arg Term The term to print.
%   @arg E    A string to print after the term.
%
%   @example
%   % Print a term with depth-based indentation and custom start/end strings:
%   ?- indentq(10, 5, 'INFO:', 'Start:', 'processing', 'End.').
%   'INFO: Start: processing End.'  % Printed with depth-based indentation.
%
indentq(DR, TraceLen, AR, S, Term, E) :-
    setup_call_cleanup(
        notrace(format('~N;')), % Start with a newline and suppress trace.
        (
            wots(Str, indentq0(DR, TraceLen, AR, S, Term, E)), % Format the term.
            newlines_to_spaces(Str, SStr), % Convert newlines to spaces.
            write(SStr) % Write the formatted string.
        ),
        notrace(format('~N')) % End with a newline and suppress trace.
    ).

%!  newlines_to_spaces(+Str, -SStr) is det.
%
%   Convert newlines in a string to spaces.
%
%   This predicate takes an input string `Str` that may contain newlines and converts
%   all newlines to spaces. It first splits the string at newline characters (`\n`),
%   and then joins the resulting parts with spaces, producing the output string `SStr`.
%
%   @arg Str  The input string that contains newlines.
%   @arg SStr The output string with newlines replaced by spaces.
%
%   @example
%   % Convert newlines in a string to spaces:
%   ?- newlines_to_spaces("Hello\nWorld\n", Result).
%   Result = "Hello World ".
%
newlines_to_spaces(Str, SStr) :-
    atomics_to_string(L, '\n', Str), % Split the string by newlines.
    atomics_to_string(L, ' ', SStr). % Join the parts with spaces.

%!  indentq0(+DR, +TraceLen, +AR, +S, +Term, +E) is det.
%
%   Print a term with padding and depth-based indentation.
%
%   This predicate prints a `Term` with depth-based indentation determined by `DR` and
%   includes padding and formatting based on `TraceLen` and `AR`. The `S` argument specifies
%   a start string to print before the term, and `E` specifies an end string to print
%   after the term. The term is printed using `write_src/1`, and the indentation is
%   controlled by `with_indents/2`.
%
%   @arg DR   The depth used to determine the indentation.
%   @arg TraceLen   The TraceLen component for formatting and padding control.
%   @arg AR   The AR component for additional formatting or padding.
%   @arg S    A string to print before the term.
%   @arg Term The term to print with padding and indentation.
%   @arg E    A string to print after the term.
%
%   @example
%   % Print a term with indentation, padding, and start/end strings:
%   ?- indentq0(5, 10, 'INFO:', 'Start:', some_term(foo, bar), 'End.').
%   'INFO: Start: some_term(foo, bar) End.'  % Printed with depth-based indentation.
%
indentq0(DR, TraceLen, AR, S, Term, E) :-
    as_trace((
        print_padded(TraceLen, DR, AR), % Print the padded line.
        format(S, []), % Print the start string.
        with_indents(false, write_src(Term)), % Print the term.
        format(E, []) % Print the end string.
    )).

%!  reset_eval_num is det.
%
%   Reset evaluation-related flags.
%
%   This predicate resets the `eval_num` and `trace_output_len` flags to zero. These flags are
%   typically used for tracking evaluation state and trace output length during program execution.
%
%   @example
%   % Reset evaluation-related flags:
%   ?- reset_eval_num.
%
reset_eval_num :-
    flag(eval_num, _, 0), % Reset eval_num flag.
    flag(trace_output_len, _, 0). % Reset trace_output_len flag.

%!  reset_only_eval_num is det.
%
%   Reset only the `eval_num` flag.
%
%   This predicate resets the `eval_num` flag to zero, which is used for tracking evaluation state.
%   It does not affect any other flags, such as `trace_output_len`.
%
%   @example
%   % Reset only the eval_num flag:
%   ?- reset_only_eval_num.
%
reset_only_eval_num :-
    flag(eval_num, _, 0). % Reset eval_num flag.

%!  is_fast_mode is nondet.
%
%   Check if the system is in fast mode.
%
%   This predicate succeeds if the system is in "fast mode", a state where certain operations,
%   such as debugging, are bypassed. It currently fails by default but can be modified to
%   enable fast mode checks based on specific conditions.
%
%   @example
%   % Check if the system is in fast mode:
%   ?- is_fast_mode.
%   false.
%
is_fast_mode :- is_nodebug,!.
is_fast_mode :- fail, \+ is_debugging(eval), !.


%!  ignore_trace_once(:Goal) is nondet.
%
%   Ignore trace for a single execution of a goal.
%
%   This predicate executes a given `Goal` while suppressing tracing for a single execution.
%   If any errors occur during the execution of `Goal`, they are caught and the predicate fails silently.
%
%   @arg Goal The goal to execute without tracing.
%
%   @example
%   % Execute a goal without tracing:
%   ?- ignore_trace_once(my_goal).
%
ignore_trace_once(Goal) :-
    ignore(notrace(catch(ignore(Goal), _, fail))), !.

%!  as_trace(:Goal) is nondet.
%
%   Execute a goal while suppressing trace output.
%
%   This predicate executes the given `Goal` while ensuring that trace output is suppressed.
%   It utilizes `ignore_trace_once/1` to suppress tracing for the duration of the goal execution.
%
%   @arg Goal The goal to execute without trace output.
%
%   @example
%   % Execute a goal with trace output suppressed:
%   ?- as_trace(my_goal).
%
as_trace(Goal) :- ignore_trace_once(\+ with_no_screen_wrap(color_g_mesg('#2f2f2f', Goal))).

%!  with_no_screen_wrap(:Goal) is nondet.
%
%   Execute a goal without screen wrapping.
%
%   This predicate runs the given `Goal` while ensuring that screen wrapping is disabled.
%   If the first clause succeeds, it simply calls the `Goal`. Otherwise, it disables screen
%   wrapping by setting the terminal columns using `with_no_wrap/2`.
%
%   @arg Goal The goal to execute without screen wrapping.
%
%   @example
%   % Execute a goal without screen wrapping:
%   ?- with_no_screen_wrap(my_goal).
%
with_no_screen_wrap(Goal) :- !, call(Goal).
with_no_screen_wrap(Goal) :- with_no_wrap(6000, Goal).

%!  with_no_wrap(+Cols, :Goal) is nondet.
%
%   Execute a goal with a specific number of columns, without wrapping.
%
%   This predicate sets the terminal to use a specific number of columns (`Cols`)
%   and disables line wrapping for the duration of the execution of the `Goal`.
%   After the `Goal` completes, the original terminal settings are restored.
%
%   @arg Cols The number of columns to set for the terminal.
%   @arg Goal The goal to execute without line wrapping.
%
%   @example
%   % Execute a goal with 80 columns and no line wrapping:
%   ?- with_no_wrap(80, my_goal).
%
with_no_wrap(Cols, Goal) :-
    % Setup: Save current terminal settings and disable line wrapping
    setup_call_cleanup(
        begin_no_wrap(Cols, OriginalCols, OriginalRows), % Begin no-wrap mode.
        Goal, % Execute the goal.
        end_no_wrap(OriginalCols, OriginalRows) % Restore original settings.
    ).

%!  begin_no_wrap(+Cols, -OriginalCols, -OriginalRows) is det.
%
%   Begin no-wrap mode by setting terminal size.
%
%   This predicate saves the current terminal size (columns and rows), then sets
%   the terminal to use the specified number of columns (`Cols`). It also disables
%   line wrapping in the terminal.
%
%   @arg Cols The desired number of columns for the terminal.
%   @arg OriginalCols The original number of columns before modification.
%   @arg OriginalRows The original number of rows before modification.
%
%   @example
%   % Start no-wrap mode with 100 columns:
%   ?- begin_no_wrap(100, OrigCols, OrigRows).
%
begin_no_wrap(Cols, OriginalCols, OriginalRows) :-
    cached_call(30.0, get_current_terminal_settings(OriginalCols, OriginalRows)), % Get current terminal settings.
    set_terminal_size(Cols, OriginalRows), % Set the new terminal size.
    format('~s', ["\e[?7l"]). % Disable line wrapping.

%!  end_no_wrap(+OriginalCols, +OriginalRows) is det.
%
%   End no-wrap mode by restoring terminal size.
%
%   This predicate restores the terminal size to its original number of columns
%   (`OriginalCols`) and rows (`OriginalRows`), and re-enables line wrapping.
%
%   @arg OriginalCols The original number of columns before no-wrap mode.
%   @arg OriginalRows The original number of rows before no-wrap mode.
%
%   @example
%   % End no-wrap mode and restore terminal settings:
%   ?- end_no_wrap(OrigCols, OrigRows).
%
end_no_wrap(OriginalCols, OriginalRows) :-
    set_terminal_size(OriginalCols, OriginalRows), % Restore original terminal size.
    format('~s', ["\e[?7h"]). % Re-enable line wrapping.

%!  get_current_terminal_settings(-Cols, -Rows) is det.
%
%   Get the current terminal size.
%
%   This predicate retrieves the current terminal dimensions by executing the `stty size`
%   command. It reads the number of columns (`Cols`) and rows (`Rows`) from the output.
%   The dimensions are returned as integers.
%
%   @arg Cols The number of columns in the terminal.
%   @arg Rows The number of rows in the terminal.
%
%   @example
%   % Get the current terminal size:
%   ?- get_current_terminal_settings(Cols, Rows).
%   Cols = 80,
%   Rows = 24.
%
get_current_terminal_settings(Cols, Rows) :-
    % Use 'stty size' to get the current dimensions of the terminal
    process_create(path(stty), ['size'], [stdout(pipe(Stream))]), % Execute stty size command.
    read_line_to_string(Stream, SizeStr), % Read the output.
    close(Stream), % Close the stream.
    split_string(SizeStr, " ", "", [RowsStr, ColsStr]), % Split the string into rows and columns.
    number_string(Rows, RowsStr), % Convert rows to number.
    number_string(Cols, ColsStr), % Convert columns to number.
    !.
get_current_terminal_settings(_, _).

%!  set_terminal_size(+Cols, +Rows) is det.
%
%   Set the terminal size (conceptual, may not work in all terminals).
%
%   This predicate conceptually sets the terminal size to the specified number of
%   columns (`Cols`) and rows (`Rows`). It uses an escape sequence to attempt resizing
%   the terminal. However, the resizing may not work in all environments or terminals.
%
%   @arg Cols The number of columns to set for the terminal.
%   @arg Rows The number of rows to set for the terminal.
%
%   @example
%   % Set the terminal size to 80 columns and 24 rows:
%   ?- set_terminal_size(80, 24).
%
set_terminal_size(Cols, Rows) :-
    % Conceptual; actual resizing may not work in all terminals
    if_t(integer(Cols),
         if_t(integer(Rows), format('~s~w;~w~s', ["\e[8;", Rows, Cols, "t"]))).

%!  with_debug(+Flag, :Goal) is nondet.
%
%   Execute a goal with debugging enabled based on a flag.
%
%   This predicate executes a `Goal` with debugging enabled if the given `Flag` is set.
%   If the `Flag` is active, it immediately calls the `Goal`. Otherwise, it temporarily
%   enables debugging for the duration of the `Goal` execution and disables it afterward.
%
%   @arg Flag The debugging flag to check or activate.
%   @arg Goal The goal to execute with debugging, if applicable.
%
%   @example
%   % Execute a goal with debugging enabled for 'eval':
%   ?- with_debug(eval, my_goal).
%
with_debug(Flag, Goal) :-
    is_debugging(Flag),
    !,
    call(Goal).
with_debug(Flag, Goal) :-
    reset_only_eval_num,
    setup_call_cleanup(set_debug(Flag, true), call(Goal), set_debug(Flag, false)).

%!  is_nodebug is nondet.
%
%   Checks if the 'nodebug' option is set to true.
%   This predicate succeeds if the option `nodebug` is set to true,
%   otherwise it fails.
%
%   @example
%     ?- is_nodebug.
%     true.
%

% Check if the option 'nodebug' is explicitly set to false.  (ideally very rare - code has to relaly know about this)
is_nodebug :- option_value(nodebug, false), !, fail.
% By default spawned threads would need nodebug=false
is_nodebug :- thread_self(Self), Self \== main, Self \== 0.
is_nodebug :-
    % Check if the option 'nodebug' is set to true.
    option_value(nodebug, true).

%!  with_no_debug(+Goal) is det.
%
%   Executes the given Goal with debug-related options set appropriately.
%   This predicate runs Goal under different debugging settings depending
%   on whether the 'nodebug' option is active.
%
%   @arg Goal The goal to execute. It can be any Prolog predicate.
%
%   If 'nodebug' is set to true, it immediately calls the goal without any
%   additional changes to options. Otherwise, it wraps the Goal execution
%   inside several `with_option/3` predicates that modify various debugging
%   and evaluation options.
%
%   @examples
%     % Call a goal with no debugging if 'nodebug' is set.
%     ?- with_no_debug(true).
%     true.
%
%     % Example where debug options are set and the goal is executed.
%     ?- with_no_debug(member(X, [1, 2, 3])).
%     X = 1 ;
%     X = 2 ;
%     X = 3.
%
with_no_debug(Goal) :-  is_nodebug, !, % If 'nodebug' is true, call the goal without any further option adjustments.
    call(Goal).
with_no_debug(Goal) :-
    % Otherwise, call the goal while modifying several debugging and execution options.
    with_option(nodebug, true,
        with_option(time, false,
            with_option(debug, false,
                with_option(e, silent,
                    with_option(eval, true,
                        with_option(exec, noskip, call(Goal))))))).

%!  flag_to_var(+Flag, -Var) is det.
%
%   Convert a debugging flag to a variable name.
%
%   This predicate converts a given debugging `Flag` into a variable name, specifically
%   by prepending the string 'trace-on-' to the flag name. If the `Flag` is already
%   in the form of `metta(Flag)`, the inner flag is extracted and converted. If no
%   conversion is needed, the `Flag` is unified directly with `Var`.
%
%   @arg Flag The debugging flag to convert.
%   @arg Var  The resulting variable name or unchanged flag.
%
%   @example
%   % Convert a debugging flag to a variable name:
%   ?- flag_to_var(eval, Var).
%   Var = 'trace-on-eval'.
%
%   % Handle metta flag conversion:
%   ?- flag_to_var(metta(eval), Var).
%   Var = 'trace-on-eval'.
%
flag_to_var(Flag, Var) :- atom(Flag),\+ atom_concat('trace-on-', _, Flag),!,atom_concat('trace-on-', Flag, Var).
flag_to_var(metta(Flag), Var) :- !, nonvar(Flag),flag_to_var(Flag, Var).
flag_to_var(Flag,Var):-Flag=Var.

%!  set_debug(+Flag, +TF) is det.
%
%   Set debugging on or off based on a flag.
%
%   This predicate enables or disables debugging based on the given `Flag` and the boolean
%   value `TF`. If `TF` is `'True'`, debugging is enabled; if `TF` is `'False'`, debugging
%   is disabled. It handles special cases where the `Flag` is in the form of `metta(Flag)`,
%   as well as direct boolean values.
%
%   @arg Flag The debugging flag to set.
%   @arg TF   Boolean value indicating whether debugging should be enabled (`true`) or disabled (`false`).
%
%   @example
%   % Enable debugging for the 'eval' flag:
%   ?- set_debug(eval, true).
%
set_debug(metta(Flag), TF) :- nonvar(Flag), !, set_debug(Flag, TF).
%set_debug(Flag,Val):- \+ atom(Flag), flag_to_var(Flag,Var), atom(Var),!,set_debug(Var,Val).
set_debug(Flag, Val) :- atom(Flag), atom_concat('trace-on-', Var, Flag),!,set_debug(Var,Val).
set_debug(Flag, Var) :- debugging(metta(Flag), Var),!.
set_debug(Flag, TF) :- TF == 'False', !, set_debug(Flag, false).
set_debug(Flag, TF) :- TF == 'silent', !, set_debug(Flag, false).
set_debug(Flag, TF) :- atomic(TF), symbol_concat('h',_,TF),!, set_debug(Flag, false). % hide, hid, hidden, ..
set_debug(Flag, TF) :- atomic(TF), symbol_concat('n',_,TF),!, set_debug(Flag, false). % notrace, no, ..
set_debug(Flag, TF) :- atomic(TF), symbol_concat(_,'f',TF),!, set_debug(Flag, false). % traceoff, off, ..
set_debug(Flag, false) :- !, nodebug(metta(Flag)),!. %, flag_to_var(Flag, Var), set_fast_option_value(Var, false).
set_debug(Flag, _) :- !, debug(metta(Flag)),!. %, flag_to_var(Flag, Var), set_fast_option_value(Var, true).


%!  if_trace(+Flag, :Goal) is nondet.
%
%   Conditionally execute a goal if tracing is enabled for the given flag.
%
%   This predicate executes the provided `Goal` only if tracing is enabled for the given `Flag`.
%   It first checks if debugging (tracing) is enabled for the `Flag`, and if so, the `Goal` is executed.
%   If an error occurs during execution, it is caught and reported using `fbug/1`.
%
%   @arg Flag The flag indicating if tracing is enabled.
%   @arg Goal The goal to execute if tracing is enabled.
%
%   @example
%   % Execute a goal if tracing is enabled for 'eval':
%   ?- if_trace(eval, writeln('Tracing is enabled')).
%
if_trace(Flag, Goal) :- notrace(real_notrace((catch_err(ignore((is_debugging(Flag), Goal)),E,fbug(E --> if_trace(Flag, Goal)))))).

if_tracemsg(Flag, Message):- if_trace(Flag, wdmsg(Message)).

rtrace_when(Why,Goal):- is_debugging(Why)->rtrace(Goal);call(Goal).
show_failure_when(Why,Goal):- \+ is_debugging(Why), !, call(Goal).
show_failure_when(Why, Goal):- if_or_else(Goal, (once(show_failing(Why,Goal)),fail)).
show_failing(Why,Goal):- notrace, ignore(nortrace),
                      debugm1(Why, show_failed(Why, Goal)),
                      nop(if_t(is_debugging(failures),trace)),!,fail.

%show_failure_when(_Why,Goal):- call(Goal)*->true;(trace,fail).
check_trace(Topic):- (is_debugging(Topic)-> (notrace,ignore(nortrace),writeln(user_error,check_trace(Topic)),maybe_trace) ; true).

trace_if_debug(AE,_LenX):- if_t(is_debugging(AE),maybe_trace),!.
maybe_trace(Why):- if_t(is_debugging(Why),maybe_trace),!.
maybe_trace:- is_extreme_debug(trace).

%is_extreme_debug(G):- (( fail, gethostname(X),X=='HOSTAGE')), !, call(G).
is_extreme_debug(_).

sub_var_safely(Var,Source):-
  woc(sub_var(Var,Source)).

sub_term_safely(Sub,Source):- acyclic_term(Source),!,sub_term(Sub,Source).

%abolish_trace:- \+ is_flag(abolish_trace),!.
abolish_trace:-
  redefine_system_predicate(system:trace/0),
  abolish(system:trace/0),
  assert(( (system:trace) :- (format(user_error,'~nTRACE_CALLED~n',[]), break, bt))).

woc(Goal):- current_prolog_flag(occurs_check,true),!,call(Goal).
woc(Goal):- redo_call_cleanup(set_prolog_flag(occurs_check,true),Goal,set_prolog_flag(occurs_check,false)).
% woc(Goal):- locally(set_prolog_flag(occurs_check,true),Goal).
wocf(Goal):-
  locally(set_prolog_flag(occurs_check,false), Goal).

print_locally_tested_flag:- current_prolog_flag(locally_tested_flag,X),writeln(locally_tested_flag=X).
test_locally_setting_flags:-
  forall((locally(set_prolog_flag(locally_tested_flag,1),
     ((member(X,[1,2,3]),print_locally_tested_flag))),
        writeln(X),print_locally_tested_flag),nl).
:- thread_initialization(set_prolog_flag(locally_tested_flag,0)).

%:- initialization(set_prolog_flag(occurs_check,error)).
%:- initialization(set_prolog_flag(occurs_check,true)).
:- initialization(set_prolog_flag(occurs_check,false)).
:- thread_initialization(set_prolog_flag(occurs_check,false)).
%:- initialization(set_prolog_flag(gc,false)).

%!  is_showing(+Flag) is nondet.
%
%   Check if showing is enabled for a flag.
%
%   This predicate checks if "showing" is enabled for the given `Flag`. It succeeds if the
%   flag's value is set to `'show'` or if verbose mode is active. It fails if the flag is set
%   to `'silent'`. The flag's value is retrieved using `fast_option_value/2`.
%
%   @arg Flag The flag to check for the showing state.
%
%   @example
%   % Check if showing is enabled for 'eval':
%   ?- is_showing(eval).
%   true.
%
is_showing(_) :- is_nodebug, !, fail.
is_showing(Flag) :- fast_option_value(Flag, 'silent'), !, fail.
is_showing(Flag) :- is_verbose(Flag), !.
is_showing(Flag) :- fast_option_value(Flag, 'show'), !.



%!  if_show(+Flag, :Goal) is nondet.
%
%   Conditionally execute a goal if showing is enabled for the given flag.
%
%   This predicate executes the provided `Goal` if showing is enabled for the given `Flag`.
%   It checks whether "showing" is active for the `Flag` using `is_showing/1` and, if so,
%   runs the `Goal`. If an error occurs, it is caught and reported using `fbug/1`.
%
%   @arg Flag The flag indicating if showing is enabled.
%   @arg Goal The goal to execute if showing is enabled.
%
%   @example
%   % Execute a goal if showing is enabled for 'eval':
%   ?- if_show(eval, writeln('Showing is enabled')).
%
if_show(Flag, Goal) :- real_notrace((catch_err(ignore((is_showing(Flag), Goal)),E,fbug(E --> if_show(Flag, Goal))))).

%!  fast_option_value(+N, -V) is nondet.
%
%   Get the value of a fast option.
%
%   This predicate retrieves the value `V` of a fast option identified by `N`.
%   It uses `current_prolog_flag/2` to obtain the value of the option.
%
%   @arg N The name of the option.
%   @arg V The value of the option.
%
%   @example
%   % Get the value of the 'verbose' flag:
%   ?- fast_option_value('verbose', Value).
%   Value = true.
%
fast_option_value(N, V) :- atom(N), current_prolog_flag(N, V).

%!  is_verbose(+Flag) is nondet.
%
%   Check if verbose mode is enabled for a flag.
%
%   This predicate checks whether verbose mode is enabled for the given `Flag`.
%   It succeeds if the flag's value is `'verbose'` or if debugging is enabled for
%   the flag. It fails if the flag's value is `'silent'`.
%
%   @arg Flag The flag to check for verbose mode.
%
%   @example
%   % Check if verbose mode is enabled for 'eval':
%   ?- is_verbose(eval).
%   true.
%
is_verbose(_) :- is_nodebug, !, fail.
is_verbose(Flag) :- fast_option_value(Flag, 'silent'), !, fail.
is_verbose(Flag) :- fast_option_value(Flag, 'verbose'), !.
is_verbose(Flag) :- is_debugging(Flag), !.

%!  if_verbose(+Flag, :Goal) is nondet.
%
%   Conditionally execute a goal if verbose mode is enabled for the given flag.
%
%   This predicate executes the provided `Goal` if verbose mode is enabled for the given `Flag`.
%   It checks whether verbose mode is active using `is_verbose/1`. If an error occurs during the
%   execution of `Goal`, it is caught and reported using `fbug/1`.
%
%   @arg Flag The flag indicating if verbose mode is enabled.
%   @arg Goal The goal to execute if verbose mode is enabled.
%
%   @example
%   % Execute a goal if verbose mode is enabled for 'eval':
%   ?- if_verbose(eval, writeln('Verbose mode is enabled')).
%
if_verbose(Flag, Goal) :-
    real_notrace((catch_err(ignore((is_verbose(Flag), Goal)), E,
                            fbug(E --> if_verbose(Flag, Goal))))).

%!  maybe_efbug(+SS, :G) is nondet.
%
%   Execute a goal and potentially report it as an efbug.
%
%   @arg SS A string describing the potential error or issue to report.
%   @arg G  The goal to execute.
%
%maybe_efbug(SS,G):- efbug(SS,G)*-> if_trace(eval,fbug(SS=G)) ; fail.
maybe_efbug(_, G) :- call(G).

%!  efbug(+_, :G) is nondet.
%
%   Execute a goal while suppressing trace errors.
%
%   This predicate attempts to execute the given goal `G`, ignoring any trace or debugging-related errors.
%   The first argument is ignored (`_`), as it is not used in the execution of the goal. The primary
%   purpose of this predicate is to provide a mechanism for running `G` while ensuring that errors related
%   to tracing or debugging are suppressed.
%
%   @arg _ An ignored parameter.
%   @arg G The goal to execute.
%
%   @example
%   % Execute a goal and suppress any trace-related errors:
%   ?- efbug(_, writeln('Executing safely')).
%   Executing safely
%
%efbug(P1,G):- call(P1,G).
efbug(_, G) :- call(G).

%!  is_debugging_always(+_Flag) is nondet.
%
%   Always return true for debugging, used as a placeholder.
%
%   This predicate always succeeds, regardless of the input `Flag`. It is typically used as
%   a placeholder where debugging is always assumed to be enabled. The `Flag` is ignored
%   in the evaluation.
%
%   @arg _Flag An ignored parameter representing a debugging flag.
%
%   @example
%   % Always return true for debugging:
%   ?- is_debugging_always(some_flag).
%   true.
%
is_debugging_always(_) :- is_nodebug, !, fail.
is_debugging_always(_Flag) :- !.


%!  is_debugging(+Flag) is nondet.
%
%   Check if debugging is enabled for a flag.
%
%   This predicate checks whether debugging is currently enabled for the given `Flag`.
%   It succeeds if debugging is active for the specified flag. The actual implementation
%   of this check will depend on how debugging is tracked within the system.
%
%   @arg Flag The flag to check for debugging status.
%
%   @example
%   % Check if debugging is enabled for 'eval':
%   ?- is_debugging(eval).
%   true.
%
%is_debugging(Flag):- !, fail.
is_debugging(Flag) :- var(Flag), !, fail.
is_debugging(_) :- is_nodebug, !, fail.
is_debugging((A; B)) :- !, (is_debugging(A); is_debugging(B)).
is_debugging((A, B)) :- !, (is_debugging(A), is_debugging(B)).
is_debugging(not(Flag)) :- !, \+ is_debugging(Flag).
is_debugging(Flag) :- Flag == false, !, fail.
is_debugging(Flag) :- Flag == true, !.
%is_debugging(e):- is_testing, \+ fast_option_value(compile,'full'),!.
%is_debugging(e):- is_testing,!.
%is_debugging(eval):- is_testing,!.
%is_debugging(_):-!,fail.
is_debugging(Flag) :- fast_option_value(Flag, 'debug'), !.
is_debugging(Flag) :- fast_option_value(Flag, 'trace'), !.
is_debugging(Flag) :- debugging(metta(Flag), TF), !, TF == true.
is_debugging(Flag) :- debugging(Flag, TF), !, TF == true.
%is_debugging(Flag):- debugging(Flag,TF),!,TF==true.
%is_debugging(Flag):- once(flag_to_var(Flag,Var)),
%  (fast_option_value(Var,true)->true;(Flag\==Var -> is_debugging(Var))).

% overflow = trace
% overflow = fail
% overflow = continue
% overflow = debug

%!  trace_eval(:P4, +ReasonsToTrace, +D1, +Self, +X, +Y) is det.
%
%   Perform trace evaluation of a goal, managing trace output and depth.
%
%   This predicate performs a trace evaluation on the given goal `P4`, managing depth and trace output
%   according to the trace length and trace depth options. It increments evaluation flags and handles
%   tracing based on the current depth and trace settings. The evaluation process outputs trace messages
%   for both entering and exiting the goal, while managing repeated evaluations and specific trace conditions.
%
%   @arg P4   The goal or predicate to evaluate.
%   @arg ReasonsToTrace  The trace name/type, used for managing trace output and ensuring proper subterm handling.
%   @arg D1   The current depth of the evaluation.
%   @arg Self A self-referential term passed during evaluation.
%   @arg X    The input term for the evaluation.
%   @arg Y    The output term resulting from the evaluation.
%
%   @example
%   % Perform a trace evaluation on a goal:
%   ?- trace_eval(my_predicate, trace_type, 1, self, input, output).
%

trace_eval(P4, _, D1, Self, X, Y) :- is_nodebug, !, call(P4, D1, Self, X, Y).


trace_eval(P4, ReasonsToTrace, D1, Self, X, Y) :- !,


    must_det_ll((
        notrace((
            flag(eval_num, EX0, EX0 + 1),     % Increment eval_num flag.
            TraceLen is EX0 mod 500,               % Calculate TraceLen modulo 500.
            DR is 99 - (D1 mod 100),         % Calculate DR based on depth.
            PrintRet = _,                    % Initialize PrintRet.
            option_else('trace-length', MaxTraceLen, 500), % Get trace-length option.
            %option_else('trace-depth', MaxTraceDepth, 30),   % Get trace-depth option.
            !
        )),

        TraceTooLong = _,

        quietly((
            if_t((nop(stop_rtrace), TraceLen > MaxTraceLen), (
                set_debug(eval, false),
                MaxP1 is MaxTraceLen + 1,
                nop(format('; Switched off tracing. For a longer trace: !(pragma! trace-length ~w)', [MaxP1])),
                TraceTooLong = 1,
                nop((start_rtrace, rtrace))
            ))
        ))

    )),

    ((sub_term_safely(Why, ReasonsToTrace), ReasonsToTrace \= Why) -> true ; ReasonsToTrace = Why), % Ensure proper Why handling.

    if_t(D1<0, (set_debug(deval,true))),

    (\+ \+ if_trace((eval; ReasonsToTrace), (
        PrintRet = 1,
        if_t( TraceTooLong \== 1, indentq(DR, TraceLen, '-->', [Why, X]))
    ))),

    Ret = retval(fail),
    !,

    Display = ignore((
        \+ \+ (
           flag(eval_num, EX1, EX1 + 1),
           PrintRet == 1,
           TraceTooLong \== 1,
           ((Ret \=@= retval(fail), nonvar(Y))
                -> indentq(DR, EX1, '<--', [Why, Y])
                ; indentq(DR, EX1, '<--', [Why, Ret])
            )
        )
    )),

    call_cleanup(
        (call(P4, D1, Self, X, Y)
            *-> (setarg(1, Ret, Y), one_shot(Display))
            ;  (fail, trace, call(P4, D1, Self, X, Y))
        ),
        one_shot(Display)),

    Ret \=@= retval(fail).


trace_eval(P4, ReasonsToTrace, D1, Self, X, Y) :-
    must_det_ll((
        notrace((
            flag(eval_num, EX0, EX0 + 1), % Increment eval_num flag.
            TraceLen is EX0 mod 500, % Calculate TraceLen modulo 500.
            DR is 99 - (D1 mod 100), % Calculate DR based on depth.
            PrintRet = _, % Initialize PrintRet.
            option_else('trace-length', MaxTraceLen, 500), % Get trace-length option.
            option_else('trace-depth', MaxTraceDepth, 30) % Get trace-depth option.
        )),
        quietly((if_t((nop(stop_rtrace), TraceLen > MaxTraceLen), (set_debug(eval, false), MaxP1 is MaxTraceLen + 1,
         %set_debug(overflow,false),
            nop(format('; Switched off tracing. For a longer trace: !(pragma! trace-length ~w)', [MaxP1])),
            nop((start_rtrace, rtrace)))))),
        nop(notrace(no_repeats_var(NoRepeats))))),

        ((sub_term_safely(Why, ReasonsToTrace), ReasonsToTrace \= Why) -> true ; ReasonsToTrace = Why), % Ensure proper subterm handling.
   %if_t(DR<MaxTraceDepth, )
        ( \+ \+ if_trace((eval; ReasonsToTrace), (PrintRet = 1,
            indentq(DR, TraceLen, '-->', [Why, X]))) ),

        Ret = retval(fail), !,

        (Display = call(((( \+ \+ (flag(eval_num, EX1, EX1 + 1),
                ((Ret \=@= retval(fail), nonvar(Y))
                -> indentq(DR, EX1, '<--', [Why, Y])
                ; indentq(DR, EX1, '<--', [Why, Ret])))))))),

        call_cleanup((
            (call(P4, D1, Self, X, Y)
                 *-> (setarg(1, Ret, Y),one_shot(Display))
                 ; (fail, trace, (call(P4, D1, Self, X, Y)))),

     ignore((notrace(( \+ (Y \= NoRepeats), setarg(1, Ret, Y)))))),
    % cleanup
        ignore((PrintRet == 1 -> (one_shot(Display)) ;
       (notrace(ignore((( % Y\=@=X,
         if_t(DR<MaxTraceDepth,if_trace((eval;Why),one_shot(Display))))))))))),
        Ret \=@= retval(fail).

%  (Ret\=@=retval(fail)->true;(fail,trace,(call(P4,D1,Self,X,Y)),fail)).
one_shot(Display):- ignore(once(Display)),setarg(1,Display,true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* Output Language Subsectioning

    This module provides functionality for conditional output formatting based on the current logging language.
    It allows for dynamic execution and formatting of goals (`Goal`) according to a specified information type (`InfoType`),
    adapting the output to match the desired language or format.

    The module is particularly useful for applications that need to generate outputs in multiple languages or formats,
    such as generating documentation, logs, or code in different programming languages or markup languages.

    ### Key Predicate:

    - **output_language/2**:
      - Usage: `output_language(+InfoType, :Goal) is det.`
      - Description: Outputs the specified `Goal` in the language defined by `InfoType`,
        formatting the output according to the current log file type.
      - See the predicate documentation for more details.

    ### Dependencies:

    The module relies on several auxiliary predicates and settings that should be defined elsewhere in your codebase:

    - `log_file_type/1`: Retrieves the current log file type or language.
    - `enter_markdown/1`: Enters markdown mode for a specific language.
    - `leave_markdown/1`: Exits markdown mode for a specific language.
    - `enter_comment/0`: Enters comment mode for output.
    - `leave_comment/0`: Exits the current comment context.
    - `into_blocktype/2`: Processes a goal within a specific block type.
    - `in_file_output/1`: Redirects output operations within its scope.
    - `must_det_ll/1`: Ensures that enclosed operations are deterministic and properly handled.

    ### Example Usage:

    ```prolog
    % Output the goal in the current file language
    ?- output_language(prolog, format('~q.',[prolog:-code])).
    ```
*/

:- dynamic(enabled_use_markdown/0).      % Flag indicating if markdown output is enabled
:- dynamic(enabled_use_comments/0).      % Flag indicating if comment output is enabled
:- dynamic(enabled_output_lang/1).       % Tracks the enabled output languages

%! enabled_use_markdown is semidet.
%  Succeeds if markdown output is enabled based on the 'markdown' option.
enabled_use_markdown :- fast_option_value(markdown, true), !.

%! enabled_use_comments is semidet.
%  Succeeds if comment output is enabled; OFTEN relies on markdown being enabled.
enabled_use_comments :- enabled_use_markdown.

%! in_file_output(:Goal) is det.
%  Executes the provided Goal; can be modified for output redirection.
%  when we are wrtting out a markdown or conversion file we like to be inside
%   this body in case there is some other sort of redirrection we work arround
%    in most cases this should turn off ansi color printing
%     EXCEPT when we test log output (because we use ansi2html on _that_ output)
in_file_output(Goal) :-
  format('~N'),call(Goal),format('~N').

%! into_blocktype(+InfoType, :Goal) is det.
%  Enters markdown mode for InfoType and executes Goal.
%  Alternative implementations are commented out and may be used if needed.
%  into_blocktype(InfoType, Goal) :-
%      log_file_type(markdown), !,
%      setup_call_cleanup(
%          format('~N```~w~n', [InfoType]),
%          Goal,
%          format('~N```~n', [])
%      ).
into_blocktype(InfoType, Goal) :- enter_markdown(InfoType), !, call(Goal).

%! output_language(+InfoType, :Goal) is det.
%  Outputs Goal in the language defined by InfoType, formatting based on current log file type.
%
%  If `InfoType` matches the current log file type (`Lang`), it enters markdown mode,
%  leaves comment mode if necessary, and executes `Goal`.
%  Otherwise, it enters comment mode and processes `Goal` within the specified block type.

%output_language( InfoType, Goal ) :- log_file_type(markdown), !, into_blocktype(InfoType,Goal).
%output_language( comment, Goal ) :- log_file_type(markdown), !, call(Goal).
%output_language( comment, Goal ) :- log_file_type(prolog), !, format('~N:- q.~n', [output_language( comment, Goal)]).
%output_language( comment, Goal ) :- log_file_type(metta), !, in_cmt(Goal).
output_language( InfoType, Goal ) :- notrace((output_language_impl( InfoType, Goal ))).

output_language_impl( InfoType,_Goal ) :- atom(InfoType), flag(InfoType,X,X+1), X>1000,!. % on ly priont out the first 1000
output_language_impl( InfoType, Goal ) :- log_file_type(Lang), !, % (Lang==prolog; Lang==metta),!,
  in_file_output(((InfoType == Lang -> (must_det_ll((enter_markdown(Lang),leave_comment)),call(Goal)) ; (must_det_ll(enter_comment),into_blocktype(InfoType,Goal))))).
output_language_impl(_InfoType,_Goal ) :-!.
%! log_file_type(-Type) is det.
%  Determines the current log file type or language based on options.
%
%  The log file type can be `'prolog'`, `'markdown'`, or `'metta'`.
%  It checks various options to decide the type and defaults to `'prolog'` if none match.
log_file_type(X) :- nonvar(X), !, log_file_type(Is), !, Is = X.            % If X is instantiated, check if it matches current log file type
log_file_type(prolog) :- fast_option_value(compile, save), !.              % If 'compile' option is 'save', type is 'prolog'
log_file_type(markdown) :- fast_option_value(markdown, true), !.           % If 'markdown' option is true, type is 'markdown'
log_file_type(metta) :- \+ fast_option_value(compile, save), !.            % If 'compile' option is not 'save', type is 'metta'
log_file_type(prolog).                                                     % Default type is 'prolog'

:- dynamic(inside_comment/0).            % Tracks if currently inside a comment

%! leave_comment is det.
%  Exits comment mode if inside a comment.
%
%  If comments are not enabled (`enabled_use_comments` fails), it does nothing.
%  If currently inside a comment, it outputs the comment closing syntax and retracts the flag.
leave_comment :- \+ enabled_use_comments, !.                                % If comments are not enabled, do nothing
leave_comment :- inside_comment, !, format('~N*/~n~n'), retract(inside_comment). % If inside comment, output '*/', retract flag
leave_comment.                                                              % If not inside comment, do nothing

%! enter_comment is det.
%  Enters comment mode for output.
%
%  If comments are not enabled, it does nothing.
%  If not already inside a comment, it outputs the comment opening syntax and sets the flag.
enter_comment :- \+ enabled_use_comments, !.                                % If comments are not enabled, do nothing
enter_comment :- inside_comment, !.                                         % If already inside comment, do nothing
enter_comment :- format('~N~n/*~n'), assert(inside_comment).                % Output '/*', set inside_comment flag

:- enter_comment.                        % Start by entering comment mode at the beginning

:- dynamic(inside_markdown/1).           % Tracks the current markdown language mode

%! leave_markdown(+Lang) is det.
%  Exits markdown mode for Lang if inside it.
%
%  If markdown is not enabled, or not inside any markdown mode, it does nothing.
%  If inside markdown mode for `Lang`, it outputs the markdown code block closing syntax and retracts the flag.
leave_markdown(_) :- \+ enabled_use_markdown, !.                            % If markdown not enabled, do nothing
leave_markdown(_) :- \+ inside_markdown(_), !.                              % If not inside any markdown, do nothing
leave_markdown(Lang) :- inside_markdown(Lang), !, format('~N```~n'), retract(inside_markdown(Lang)). % If inside Lang markdown, output '```', retract flag
leave_markdown(_Lang) :- !.                                                 % Do nothing otherwise

%! enter_markdown(+Lang) is det.
%  Enters markdown mode for Lang.
%
%  If markdown is not enabled, it does nothing.
%  If already inside markdown mode for `Lang`, it does nothing.
%  If inside markdown mode for a different language, it leaves that mode first.
%  It outputs the markdown code block opening syntax with the language specifier and sets the flag.
enter_markdown(_) :- \+ enabled_use_markdown, !.                            % If markdown not enabled, do nothing
enter_markdown(Lang) :- inside_markdown(Lang), !.                           % If already inside Lang markdown, do nothing
enter_markdown(Lang) :- inside_markdown(Other), !, leave_markdown(Other), !, enter_markdown(Lang). % If inside other markdown, leave it, enter Lang markdown
enter_markdown(Lang) :- log_file_type(Us), Us = Lang, inside_comment, !,             % If current log file type is Lang and inside comment
    format('~N```~w~n', [Lang]), asserta(inside_markdown(Lang)), leave_comment.      % Output '```Lang', set flag, leave comment
enter_markdown(Lang) :- format('~N```~w~n', [Lang]), asserta(inside_markdown(Lang)). % Output '```Lang', set flag

%! pick_quote(+String, -Quote) is det.
%  Selects a quote character not present in String.
%
%  It tries `"`, `'`, and `` ` `` in order and selects the first one not found in `String`.
pick_quote(String, '"') :- \+ string_contains(String, '"'), !.              % Use '"' if not in String
pick_quote(String, '\'') :- \+ string_contains(String, '\''), !.            % Use '\'' if not in String
pick_quote(String, '`') :- \+ string_contains(String, '`'), !.              % Use '`' if not in String

:- at_halt(in_file_output(leave_markdown(_))).  % Ensure markdown mode is exited at halt
:- at_halt(in_file_output(leave_comment)).      % Ensure comment mode is exited at halt

pcp:- print_last_choicepoint_upwards.
print_last_choicepoint_upwards :-
    prolog_current_choice(ChI0),         % Choice in print_last_choicepoint_info/0
    prolog_choice_attribute(ChI0, parent, ChI1), !,
    print_last_choicepoint_upwards(ChI1).
print_last_choicepoint_upwards.

print_last_choicepoint_upwards(ChI0):-
  once(print_last_choicepoint_info(ChI0, [])),
  prolog_choice_attribute(ChI0, parent, ChI1), !,
  print_last_choicepoint_upwards(ChI1).
print_last_choicepoint_upwards(_).

print_last_choicepoint_info(ChI1, Options) :-
    real_choice_info(ChI1, ChI),
    prolog_choice_attribute(ChI, frame, F),
    prolog_frame_attribute(F, goal, Goal),
    %Goal \= '$execute_goal2'(_,_,_),     % Toplevel REPL choicepoint
    %!,
    Goal \='$c_call_prolog',!,
    % Goal \='$runtoplevel', !,
    option(message_level(Level), Options, warning),
    get_prolog_backtrace(2, [_|Stack], [frame(F)]),
    (   predicate_property(Goal, foreign)
    ->  print_message(Level, choicepoint(foreign(Goal), Stack))
    ;   prolog_frame_attribute(F, clause, Clause),
        (   prolog_choice_attribute(ChI, pc, PC)
        ->  Ctx = jump(PC)
        ;   prolog_choice_attribute(ChI, clause, Next)
        ->  Ctx = clause(Next)
        ),
        print_message(Level, choicepoint(clause(Goal, Clause, Ctx), Stack))
    ).
print_last_choicepoint_info(_, _).

real_choice_info(Ch0, Ch) :-
    prolog_choice_attribute(Ch0, type, Type),
    nop(((dummy_type_info(Type), !))),
    prolog_choice_attribute(Ch0, parent, Ch1),
    real_choice_info(Ch1, Ch).
real_choice_info(Ch, Ch).

dummy_type_info(debug).
dummy_type_info(none).

:- multifile(prolog:message//1).
prolog:message(choicepoint_info(Choice, Stack)) -->
    user:choice_info(Choice),
    [ nl, 'Called from', nl ],
    prolog:message(Stack).

user:choice_info(foreign(Goal)) -->
    prolog_stack:success_goal(Goal, 'a foreign choice_info point').
user:choice_info(clause(Goal, ClauseRef, clause(Next))) -->
    prolog_stack:success_goal(Goal, 'a choice_info point in alternate clause'),
    [ nl ],
    [ '  ' ], prolog_stack:clause_descr(ClauseRef), [': clause succeeded', nl],
    [ '  ' ], prolog_stack:clause_descr(Next),      [': next candidate clause' ].
user:choice_info(clause(Goal, ClauseRef, jump(PC))) -->
    { prolog_stack:clause_where(false, ClauseRef, PC, Where,
                   [subgoal_positions(true)])
    },
    prolog_stack:success_goal(Goal, 'an in-clause choice_info point'),
    [ nl, '  ' ],
    prolog_stack:where_no_goal(Where).


