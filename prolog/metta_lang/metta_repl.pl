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
% PROGRAM FUNCTION:  Implements a REPL (Read-Eval-Print Loop) for the Mettalog interpreter, providing
% interactive execution, debugging, and command handling capabilities.
%*********************************************************************************************

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Directive to save history when the program halts.
:- at_halt(save_history).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% You can debug mettalog using the repl:
%   1) enter "mettalog" on your command line
%   2) enter Prolog mode by entering "prolog." This gives you the '?-' swipl prompt.
% This method gives you a handy means of debugging.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! history_file_location(-Filename) is det.
%   Determines the location of the REPL history file.
%   On Linux, the history is stored in '~/.config/metta/repl_history.txt.'
%   @arg Filename will be the expanded path to the history file.
%
%   @example Retrieve the history file location:
%     ?- history_file_location(Filename).
%     Filename = '/home/user/.config/metta/repl_history.txt'.
%
history_file_location(Filename) :-
    % Expands the relative path to an absolute path.
    expand_file_name('~/.config/metta/repl_history.txt', [Filename]).
    % For Linux, Windows paths might be different.

%! check_directory_exists(+Dir) is det.
%   Ensures that a directory exists. If not, it will create it.
%   @arg Dir is the directory path to check.
%
%   @example Ensure a directory exists:
%     ?- check_directory_exists('/home/user/.config/metta').
%     true.
%
check_directory_exists('').
    % Base case for an empty string (root of directory tree).
check_directory_exists('/').
    % Base case for the root directory.
check_directory_exists('.').
    % Base case for the current directory.
check_directory_exists('~').
    % Base case for the home directory.
check_directory_exists('..').
    % Base case for the parent directory.
check_directory_exists(Dir) :-
    % Get the parent directory of Dir.
    file_directory_name(Dir, Parent),
    % Recursively check and create parent directories if needed.
    check_directory_exists(Parent),
    % If the directory does not exist, create it.
    (exists_directory(Dir) -> true ; make_directory(Dir)).

%! check_file_exists_for_append(+HistoryFile) is det.
%   Ensures that the history file exists and can be appended to.
%   If the file does not exist, it will create the file and its directory.
%   @arg HistoryFile is the path to the file to be checked or created.
%
%   @example Ensure a history file exists:
%     ?- check_file_exists_for_append('/home/user/.config/metta/repl_history.txt').
%     true.
%
check_file_exists_for_append(HistoryFile) :-
    % Check if the file exists and is accessible for appending.
    exists_file(HistoryFile),
    access_file(HistoryFile, append),
    !.
check_file_exists_for_append(HistoryFile) :-
    % If the file does not exist, ensure the directory exists.
    file_directory_name(HistoryFile, Dir),
    check_directory_exists(Dir),
    % Create the file by opening it in write mode.
    open(HistoryFile, write, Stream, [create([read, write])]),
    !,
    % Close the stream after creating the file.
    close(Stream).
check_file_exists_for_append(HistoryFile) :-
    % If the file cannot be created, print an error message and halt the program.
    write("Error opening history file: "),
    writeln(HistoryFile),!.
    %halt(1).

%! save_history is det.
%   Saves the current input history to a file if input is from a terminal (tty).
%   Uses el_write_history/2 to write the history.
%
%   @example Save the current history:
%     ?- save_history.
%     true.
%

% Dummy to avoid errors on windows.
save_history:- is_win64, !.
save_history:- is_docker, !.
save_history :-
    % Get the current input stream.
    current_input(Input),
    % Check if the input is from a terminal.
    (stream_property(Input, tty(true)) ->
        % If so, get the history file location and save the history.
        (history_file_location(HistoryFile),
        el_write_history(Input, HistoryFile))
    ;
        % Otherwise, do nothing.
        true).

%! load_and_trim_history is det.
%   Loads and trims the REPL history if needed, and installs readline support.
%
%   @example Load and trim the history:
%     ?- load_and_trim_history.
%     true.
%
load_and_trim_history :-
    % Disable tracing for the following operations.
    notrace((
        % Get the current input stream.
        current_input(In),
        % Try installing readline, ignoring any errors.
        ignore(install_readline(In))
    )).

% Commented-out code related to REPL that is not currently in use.
% Previously: repl:- option_value('repl',prolog),!,prolog.

% Previously: :- ensure_loaded(metta_toplevel). % Disabled for now, might be used in the future.

% Previously: :- discontiguous do_metta_exec/3.
%   This directive is commented out, indicating that discontiguous handling for the predicate `do_metta_exec/3` is not needed at the moment.

% Previously: repl:- setup_call_cleanup(flag(repl_level,Was,Was+1),repl0,
%            (flag(repl_level,_,Was),(Was==0 -> maybe_halt(7) ; true))).
%   This version of the REPL setup using flags is commented out, probably replaced by simpler or alternative REPL logic.

%! repl is det.
%   Starts the REPL (Read-Eval-Print Loop) using `catch/3` to handle end-of-input gracefully.
%   This ensures the REPL terminates without error when the end of input is reached.
%
%   @example Start the REPL:
%     ?- repl.
%     metta>
%
repl :-
    flag(need_prompt,_,1),
    % Catch any end_of_input exception and terminate the REPL gracefully.
    catch(repl2, end_of_input, true).

%! repl1 is det.
%   A higher-level REPL function that sets some options before starting the REPL process.
%   It uses `with_option/3` to set internal flags and then invokes `repl2/0`.
%
%   @example Start the REPL with internal options:
%     ?- repl1.
%     metta>
%
repl1 :-
    % Set the option 'doing_repl' to true.
    with_option('doing_repl', true,
    % Set the 'repl' option to true and then start repl2.
        with_option(repl, true, repl2)).

%! repl2 is nondet.
%   The main loop of the REPL, responsible for managing history, garbage collection, and catching any errors.
%   It continually prompts the user until an error occurs or input is exhausted.
%
%   @example Start the REPL loop:
%     ?- repl2.
%     metta>
%
repl2 :-
    % Load the REPL history and clean it up if necessary.
    ignore(catch(load_and_trim_history,_,true)),

    % Begin an infinite loop using repeat to keep REPL active.
    repeat,
    % Reset internal caches for better performance.
    notrace((reset_caches,
    % Force garbage collection to free memory.
    garbage_collect)),
    % Execute repl3 and catch any errors that occur during execution.
    ignore(catch((ignore(catch(once(repl3), restart_reading, true))),
    % If an error occurs, print the reason and continue the loop.
    give_up(Why), pp_m(red, gave_up(Why)))),
    % Fail at the end to ensure the repeat loop continues indefinitely.
    fail.

%! write_metta_prompt is det.
%   Writes the REPL prompt for the user, including the current mode and self-reference.
%   It uses the `flush_output/1` to ensure all output is displayed immediately.
%
%   @example Display the REPL prompt:
%     ?- write_metta_prompt.
%     metta>
%
write_metta_prompt :-
    % Ensure any pending output is flushed to the terminal.
    flush_output(current_output),
    % Write the initial prompt text "metta".
    format('~Nmetta', []),
    % Display the current REPL mode (e.g., normal, query).
    current_read_mode(repl, Mode), write(Mode),
    % Display the current self reference, unless it is '&self'.
    current_self(Self), top_self(Top),
    ((Self == '&self' ; Self == Top) -> true ; write(Self)),
    % Write the final '>' as the prompt and flush the output again.
    write('>'), flush_output(current_output).

%! repl3 is det.
%   Prepares the REPL prompt and handles the user input in a safe way.
%   It manages the prompt display and ensures the terminal is properly set up.
%
%   @example Set up the REPL prompt and call repl4:
%     ?- repl3.
%     metta>
%
repl3 :-
    % Set up cleanup for the terminal prompt and execute repl4.
    notrace(prompt(Was, Was)),
    setup_call_cleanup(
        % Set the terminal prompt without tracing.
        notrace(set_metta_prompt),
        % Flush the terminal and call repl4 to handle input.
        setup_call_cleanup(ttyflush, repl4, ttyflush),
        % After execution, restore the previous terminal prompt.
        notrace(prompt(_, Was))).

% Create the prompt by writing it to an atom `P`.
set_metta_prompt:-
    with_output_to(atom(P), write_metta_prompt),
    %prompt1(P),
    prompt(_, P).



%! repl4 is det.
%   Executes the REPL logic by reading the input, processing expressions, and handling directives or commands.
%   The loop is managed through exceptions (e.g., restarting or ending input).
%
%   @example Execute the main REPL logic:
%     ?- repl4.
%     metta>
%
repl4 :-
    % Reset the evaluation number to ensure expressions are counted properly.
    notrace((reset_eval_num,
    % Write the result of the previous evaluation (if any) to the output.
    write_answer_output,
    % The following command to reset terminal settings is commented out for now.
    % ignore(shell('stty sane ; stty echo')),
    % Read the next expression from the REPL input.
    catch(repl_read(Expr),stream_error(_,E),(writeln(E),throw(restart_reading))),
    % Check if the input is either `end_of_file` or empty on Windows; if so, throw `end_of_input`.
    (if_t((Expr == end_of_file; (is_win64, Expr == '')), notrace(throw(end_of_input)))),
    % Flush the terminal input/output to make sure the REPL is responsive.
    ttyflush,
    % Check for any directives embedded in the expression and process them.
    (ignore(check_has_directive(Expr))),
    % Get the current self reference and reading mode for the REPL.
    current_self(Self), current_read_mode(repl, Mode))),
    % Output the read expression for debugging purposes, if applicable.
    %nop(writeqln(repl_read(Expr))),!,
    % Evaluate the expression using the `do_metta/5` predicate.
    ignore(once((do_metta(repl_true, Mode, Self, Expr, O)))),!,
    % Optionally write the result of the evaluation to the source.
    notrace((nop((write_src(O), nl)),
    % Throw `restart_reading` to restart the REPL input process after execution.
    nop(notrace(throw(restart_reading))))),!.

cls:- shell(clear).

:- dynamic(metta_trace_restore/1).
store_metta_trace:- ignore((\+ metta_trace_restore(_), get_trace_reset(W),assert(metta_trace_restore(W)))),notrace.
restore_metta_trace:- notrace,ignore((retract(metta_trace_restore(W)),call(W))).

%!  check_has_directive(+V) is nondet.
%
%   Processes a given input `V` to determine if it contains a recognized directive
%   and executes the associated logic. This predicate handles several types of directives,
%   such as switching modes, assigning values, or invoking debugging utilities.
%
%   This predicate fails if the input `V` is a variable or if no matching directive is found.
%
%   Directives can take several forms:
%   - Simple commands like `'log.'` or `'rust.'` that switch modes.
%   - Assignments of the form `call(N=V)`.
%   - Special debugging and REPL controls using `@` or other characters.
%
%   @arg V The input term to be checked and processed. This can be a variable,
%          an atom, or a more complex term like an assignment.
%
%   @example
%     % Example of switching to mettalog mode:
%     ?- check_has_directive('log.').
%     % This switches the system to the mettalog mode.
%
%     % Example of switching to mettarust mode:
%     ?- check_has_directive('rust.').
%     % This switches the system to the mettarust mode.
%
check_has_directive(V) :- var(V), !, fail.
check_has_directive(V) :- notrace(check_has_directive1(V)),!,notrace(throw(restart_reading)).
% No directive found.
check_has_directive(_).

% Directive to switch to mettalog.
check_has_directive1('@log') :- switch_to_mettalog, !, write_src_uo(switch_to_mettalog).
% Directive to switch to mettarust.
check_has_directive1('@rust') :- switch_to_mettarust, !, write_src_uo(switch_to_mettarust).
% Displays options when '@' is input and restarts reading.
check_has_directive1('@') :- do_show_options_values, !.
% Checks if the symbol contains a '.' (common for directives).
check_has_directive1(Atom) :- symbol(Atom), symbol_concat(_, '.', Atom), !.
% Assign a value to a directive, e.g., call(N=V).
check_has_directive1(call(N=V)) :- nonvar(N), !, set_directive(N, V).
% Handle directive in the form [@Name, Value].
check_has_directive1([AtEq, Value]) :- symbol(AtEq), symbol_concat('@', Name, AtEq), set_directive(Name, Value).
% Enable rtrace debugging and restart reading.
% check_has_directive(call(Rtrace)) :- rtrace == Rtrace, !. % rtrace, notrace(throw(restart_reading)).
% Handle expressions in the form of N=V.
check_has_directive1(NEV) :- symbol(NEV), symbolic_list_concat([N, V], '=', NEV), set_directive(N, V).
% Handle mode changes in the REPL.
check_has_directive1(ModeChar) :- symbol(ModeChar), metta_interp_mode(ModeChar, _Mode), !, set_directive(repl_mode, ModeChar).
% Process expressions like @NEV=Value.
check_has_directive1(AtEq) :- symbol(AtEq), symbol_concat('@', NEV, AtEq), option_value(NEV, Foo), fbug(NEV = Foo), !.

%!  set_directive(+N, +V) is det.
%
%   Assigns the value `V` to the directive `N`. Handles special cases such as
%   REPL mode changes differently from general directives.
%
%   @arg N The name of the directive to set. It can be a general option or
%          a specific control like `mode`.
%   @arg V The value to assign to the directive.
%
%   @example
%     % Set the REPL mode to 'debug':
%     ?- set_directive('mode', 'debug').
%
%     % Assign a general option value:
%     ?- set_directive('timeout', 100).
%
set_directive(N, V) :- symbol(N), symbol_concat('@', NN, N), !, set_directive(NN, V).
% Special case for setting the REPL mode.
set_directive(N, V) :- N == 'mode', !, set_directive(repl_mode, V).
% Set a general directive using `set_option_value_interp/2`.
set_directive(N, V) :- nonvar(N), show_call(set_option_value_interp(N, V)), !, notrace(throw(restart_reading)).

%!  read_pending_white_codes(+In) is det.
%   Reads the pending codes (whitespace characters) from the input stream `In`.
%   Specifically, it looks for the newline character (ASCII 10).
%   This predicate ensures that the REPL input stream is properly cleaned up.
%
%   @arg In The input stream from which to read pending whitespace codes.
%
%   @example
%     % Clean up the input stream by reading pending newlines:
%     ?- read_pending_white_codes(user_input).
%
read_pending_white_codes(In) :-
    % Read pending codes from the input stream, only considering ASCII 10 (newline).
    read_pending_codes(In, [10], []),
    % Succeed after reading.
    !.
% If the input stream is not provided, do nothing.
read_pending_white_codes(_).


prolog:message(unbalanced_parens) -->
    ['Found unbalanced parentheses!'-[]].


%!  next_expr(+ExprI, -Expr) is det.
%
%   Processes the given expression and returns the next expression to be used.
%   If `ExprI` is `end_of_file`, it attempts to retrieve a buffered comment or
%   defaults to an empty string. Otherwise, it directly unifies `ExprI` with `Expr`.
%
%   @arg ExprI The input expression, which may be `end_of_file`.
%   @arg Expr  The resulting expression to be used in further processing.
%
%   @example
%     % If ExprI is `end_of_file`, it tries to get a buffered comment or returns "".
%     ?- next_expr(end_of_file, Expr).
%     Expr = "".
%
next_expr(ExprI, Expr) :-
    % If the input expression is `end_of_file`, handle it with a cut.
    ExprI == end_of_file, !,
    % Retrieve a buffered comment or default to an empty string.
    (comment_buffer(Expr); (Expr = "")).
% If ExprI is not `end_of_file`, unify it directly with Expr.
next_expr(ExprI, Expr) :- ExprI = Expr.

%!  repl_read(+In, -Expr) is det.
%
%   Reads an expression from the given input stream, processes it with
%   `next_expr/2`, and returns the result.
%
%   @arg In   The input stream from which the expression is read.
%   @arg Expr The resulting expression after reading and processing.
%
%   @example
%     % Open a file and read an expression from it.
%     ?- open('input.txt', read, In), repl_read(In, Expr).
%     Expr = some_expression.
%
repl_read(In, Expr) :-
    % Read the next expression from the input stream.
    repl_read_next(In, ExprI),
    % Process it to determine the final expression.
    next_expr(ExprI, Expr),!.

%!  repl_read(-Expr) is det.
%
%   Reads an expression without a specific input stream, processes it with
%   `next_expr/2`, and returns the result.
%
%   @arg Expr The resulting expression after reading and processing.
%
%   @example
%     % Read an expression from the default input source.
%     ?- repl_read(Expr).
%     Expr = some_expression.
%
repl_read(Expr) :-
    % Read the next expression.
    repl_read_next(ExprI),
    % Process it to determine the final expression.
    next_expr(ExprI, Expr).

% maybe Write any stored comments to the output?
%!  comment_buffer(-Comment) is nondet.
%
%   Retrieves and removes a comment from the metta file comment buffer.
%   It retracts a `metta_file_comment/5` fact and unifies its `Comment` field
%   with the output argument.
%
%   @arg Comment The comment retrieved from the buffer.
%
%   @example
%     % Assume a comment was previously stored in the buffer.
%     ?- comment_buffer(Comment).
%     Comment = 'This is a comment'.
%
comment_buffer(Comment) :-
    % Retract a comment from the buffer and unify it with the output argument.
    retract(metta_file_comment(_Line, _Col, _CharPos, Comment, _Pos)).

%!  repl_read_next(+NewAccumulated, -Expr) is det.
%
%   Reads the next expression by interpreting the accumulated input. It handles
%   special cases (such as symbols `'!'` and `'+'`), manages syntax errors,
%   balances parentheses, and normalizes spaces in input. If an error occurs,
%   the reading process may be restarted.
%
%   @arg NewAccumulated The accumulated input to be processed.
%   @arg Expr           The resulting expression, or a specific symbol or mode indicator.
%
%   @example
%     % Read a valid metta expression from input.
%     ?- repl_read_next("write(hello)", Expr).
%     Expr = call(write(hello)).
%
repl_read_next(In, Expr) :-
    % If `In` is the current input stream, read with `repl_read/1`.
    current_input(In0), In == In0, !, repl_read(Expr).

repl_read_next(NewAccumulated, Expr) :-
    % Concatenate the input with '.' and try to interpret it as an atom.
    symbol_concat(_Atom,'.',NewAccumulated),
    % Attempt to read the term from the atom, handle errors and retry if necessary.
    open_string(NewAccumulated,Stream),
    catch_err((read_prolog_syntax_unsafe(Stream, Term), Expr = call(Term)), E,
       (((fail, write('Syntax error: '), writeq(E), nl, repl_read_next(Expr))))), !.

% Previously commented: repl_read_next(Str, Expr):- ((clause(t_l:s_reader_info(Expr),_,Ref),erase(Ref))).
% Handle special case for '!' symbol.
repl_read_next("!", '!') :- !.
% Handle special case for '+' symbol.
repl_read_next("+", '+') :- !.
% Convert a string to an atom and check for a valid interpreter mode.
repl_read_next(Str, Atom) :- atom_string(Atom, Str), metta_interp_mode(Atom, _), !.

% Handle input starting with '@'.
repl_read_next(Str, Expr) :- symbol_concat('@', _, Str), !, atom_string(Expr, Str).

% Previously commented: repl_read_next(Str, 'add-atom'('&self',Expr)):- symbol_concat('+',W,Str),!,repl_read_next(W,Expr).
% Previously commented: repl_read_next(NewAccumulated, exec(Expr)):- string_concat("!", Renew, NewAccumulated), !, repl_read_next(Renew, Expr).

% Read and process the input if parentheses are balanced, then add it to the history.
repl_read_next(NewAccumulated, Expr) :-
    % Normalize the string
    normalize_space(string(Renew), NewAccumulated),
    % Convert the accumulated string to a list of characters.
    string_chars(Renew, Chars),
    % Ensure there is some content in the input.
    length(Chars, Len), Len > 0,
    % Parse the metta expression from the accumulated input.

    % for debugging
    % notrace(catch(read_sexpr(NewAccumulated, Expr),SE,true)),
    % for non-debugging
    notrace(catch(parse_sexpr(NewAccumulated, Expr),SE,true)),
    check_unbalanced_parens(SE),!,
    add_history_src(Expr).

% Read the next line of input, accumulate it, and continue processing.
repl_read_next(Accumulated, Expr) :-
    if_t(flag(need_prompt,1,0),(format('~N'),set_metta_prompt)),
        % On windows we need to output the prompt again
    (is_win64-> (ttyflush,prompt(P, P),write(P), ttyflush) ; true),
    % Read a line from the current input stream.
    read_line_to_string(current_input, Line),
    % switch prompts after the first line is read
    repl_read_next(Accumulated, Expr, Line).

repl_read_next(Accumulated, Expr, Line):-
    normalize_space(string(NAccumulated), Accumulated), NAccumulated = "",
    normalize_space(string(NLine), Line), NLine = "", !,
    repl_read_next(Accumulated, Expr).

repl_read_next(Accumulated, Expr, Line):-
    (Line==end_of_file->notrace(throw(end_of_input));true),
    % Concatenate the accumulated input with the new line using a space between them.
    symbolics_to_string([Accumulated, "\n", Line], NewAccumulated), !,
    % Continue reading and processing the new accumulated input.
    format(atom(T),'| ~t',[]),prompt(_,T),
    repl_read_next(NewAccumulated, Expr).



% if stream error is not recoverable restart_reading
check_unbalanced_parens(SE):- var(SE),!. % no error
check_unbalanced_parens(SE):- \+ compound(SE),!,notrace(throw(SE)). % rethrow no compounds
check_unbalanced_parens(stream_error(_, syntax_error(unexpected_char(_Char), _Reason))):- print_message(warning,unbalanced_parens), notrace(throw(restart_reading)).
check_unbalanced_parens(SE):- SE=stream_error(_,_), !,
    %print_message(error,SE),
    %writeln(continue_reading),
    fail.
check_unbalanced_parens(SE):-  writeln(restart_reading), ignore((print_message(error,SE))), writeln(restart_reading), notrace(throw(restart_reading)).
%check_unbalanced_parens(SE):- trace, SE = stream_error(_,syntax_error(unexpected_end_of_file,_)),!.
%check_unbalanced_parens(SE):- wdmsg(SE), print_message(warning,unbalanced_parens), notrace(throw(restart_reading)).
% Retrieve stored reader info and erase it.
repl_read_next(O2) :- clause(t_l:s_reader_info(O2), _, Ref), erase(Ref).

% Repeat the REPL reading process until input is fully processed.
repl_read_next(Expr) :-
    repeat,
    % Remove any pending buffer codes.
    remove_pending_buffer_codes(_, Was),
    % Convert buffer contents to a string.
    text_to_string(Was, Str),
    % Read the expression from the input.
    repl_read_next(Str, Expr),
    % Stop the repeat loop if there are no more pending codes.
    ((peek_pending_codes(_, Peek), Peek == []) -> ! ; true).

%!  add_history_string(+Str) is det.
%
%   Adds a string to the REPL history if the input is coming from a terminal (TTY).
%   This helps maintain a history of inputs, which can be useful for interactive
%   sessions.
%
%   If the input stream is not from a terminal, the predicate simply succeeds without
%   taking any action.
%
%   @arg Str The string to be added to the REPL history.
%
%   @example Adding a string to history:
%     ?- add_history_string("example query").
%     true.
%
add_history_string(Str) :-
    % Check if the current input stream is from a terminal (tty).
    current_input(Input),
    % If the input is from a terminal, add Str to the history using el_add_history/2.
    (((stream_property(Input, tty(true)))) ->
        ((notrace(ignore(catch(el_add_history(Input,Str),_,true)))))
    ;
        % Otherwise, do nothing.
        true), !.

%! add_history_src(+Exec) is det.
%   Adds the source code to the input history if the execution is non-empty.
%   @arg Exec is the executed code to be added to the history.
%
%   @example Add executed code to history:
%     ?- add_history_src([write('Hello'), nl]).
%     true.
%
%   @example No effect with empty code:
%     ?- add_history_src([]).
%     true.
%
add_history_src(Exec) :-
    % Check if Exec is not empty, and if so, write it to the string H and add it to the history.
    notrace(ignore((Exec \= [], with_output_to(string(H), with_indents(false, write_src(Exec))), add_history_string(H)))).

%!  add_history_pl(+Exec) is det.
%
%   Adds evaluated terms to the REPL history unless they are variables or special cases.
%
%   @arg Exec The evaluated term to be added to the history.
%
%   @example Add a regular term to history:
%     ?- add_history_pl(write('Hello')).
%     true.
%
%   @example Skip variables:
%     ?- add_history_pl(_).
%     true.
%
add_history_pl(Exec) :-
    % If Exec is a variable, do nothing.
    var(Exec), !.

% Recursive case to handle adding nested evaluated terms to history.
add_history_pl(eval(_,catch_red(PL),_)) :-
    % If the term is in catch_red, process the nested PL term.
    !, add_history_pl(PL).

% If the evaluated term is a failure, recursively handle the nested term.
add_history_pl(show_failure(PL)) :-
    % Process failure cases by adding the nested PL term.
    !, add_history_pl(PL).

% If the term is in as_tf form, recursively handle the nested term.
add_history_pl(as_tf(PL,_OUT)) :-
    % Process as_tf terms by adding the nested PL term.
    !, add_history_pl(PL).

% General case for adding non-variable, non-special terms to the history.
add_history_pl(Exec) :-
    % Write the executed term to the string H, then add it to the history.
    notrace(ignore((Exec \= [], with_output_to(string(H), with_indents(false, (writeq(Exec), writeln('.')))), add_history_string(H)))).

% Directive to set a global variable for variable names.
:- nb_setval(variable_names, []).


%!  is_interactive(+From) is nondet.
%
%   Checks if the input source is interactive, such as the REPL or a terminal.
%   This predicate delegates the check to an internal helper `is_interactive0/1`.
%
%   @arg From The source to check, typically an input stream or context.
%
%   @example Check if the source is interactive:
%     ?- is_interactive(user_input).
%     true.
%
%   @example Handling non-interactive sources:
%     ?- open('file.pl', read, In), is_interactive(In), close(In).
%     false.
%
% Delegate to the internal helper predicate.

is_interactive(From) :- notrace(is_interactive0(From)).

%!  is_interactive0(+From) is nondet.
%
%   Internal helper to determine if the given input source is interactive.
%   This predicate checks various cases, including symbolic streams, explicit flags,
%   and properties of streams to decide whether the input is interactive (e.g., REPL).
%
%   @arg From The source to evaluate, which could be a symbolic name, stream, or flag.
%
%   @example Check if a symbolic source is interactive:
%     ?- is_interactive0(repl_true).
%     true.
%
is_interactive0(From) :- compound(From), From = file(_), !, fail.
is_interactive0(From) :-
    % Check if the source is repl_true, meaning it is interactive.
    From == repl_true, !.
is_interactive0(From) :-
    % If the source is false, it is not interactive.
    From == false, !, fail.
is_interactive0(From) :-
    % Check if the source is symbolic and a stream that does not have a filename.
    symbolic(From), is_stream(From), !, \+ stream_property(From, filename(_)).
is_interactive0(From) :-
    % If the source is true, it is interactive.
    From = true, !.

% ==================================================
% Predicate to check and process assertions within terms.
% ==================================================

%!  inside_assert(+Var, -Result) is det.
%
%   Processes and identifies terms that involve assertions, extracting relevant information.
%   This predicate recursively navigates through various term structures to determine if
%   the term contains an assertion or related construct.
%
%   @arg Var    The input term or variable to be analyzed.
%   @arg Result The processed result, potentially modified based on the term structure.
%
%   @example
%     % Process a term containing an assertion.
%     ?- inside_assert(assert(foo), Result).
%     Result = assert(foo).
%
inside_assert(Var,Var) :-
    % If the variable is not a compound term, leave it unchanged.
    \+ compound(Var), !.
inside_assert([H,IA,_],IA) :-
    % Check if the term is symbolic and starts with 'assert'.
    symbol(H), symbol_concat('assert', _, H), !.
inside_assert(Conz,Conz) :-
    % Check if the term is considered a 'conz' (constant).
    is_conz(Conz), !.
inside_assert(exec(I),O) :-
    % If the term is exec, recursively process the inside assertion.
    !, inside_assert(I,O).
inside_assert(Eval,O) :-
    % If the term is an evaluation, extract the relevant part and process it.
    functor(Eval, eval_H, A, _), A1 is A - 1, arg(A1, Eval, I), !, inside_assert(I, O).
inside_assert(call(I),O) :-
    % Handle call terms by processing the inside assertion.
    !, inside_assert(I, O).
inside_assert(?-(I),O) :-
    % Handle query terms by processing the inside assertion.
    !, inside_assert(I, O).
inside_assert(:-(I),O) :-
    % Handle clause terms by processing the inside assertion.
    !, inside_assert(I, O).
    % Default case where the variable is unchanged.
inside_assert(Var,Var).

% ==================================================
% Predicate to retrieve the current reading mode (REPL or file).
% ==================================================

%!  current_read_mode(+Source, -Mode) is det.
%
%   Retrieves the current reading mode based on the source, which can either be
%   the REPL or a file. It checks the relevant settings and options to determine
%   the mode, defaulting to `'+'` if no specific mode is set.
%
%   @arg Source The source of the input, either `repl` or `file`.
%   @arg Mode   The mode retrieved, or `'+'` if no specific mode is set.
%
%   @example
%     % Retrieve the REPL mode, defaulting to '+' if unset.
%     ?- current_read_mode(repl, Mode).
%     Mode = '+'.
%
current_read_mode(repl,Mode) :-
    % Retrieve the REPL mode from the options if set, otherwise default to '+'.
    ((option_value(repl_mode, Mode), Mode \== []) -> true; Mode = '+'), !.
current_read_mode(file,Mode) :-
    % Retrieve the file mode from nb_current if set, otherwise default to '+'.
    ((nb_current(file_mode, Mode), Mode \== []) -> true; Mode = '+').

%! eval(+Form) is det.
%   Evaluates a Form and ensures all conditions in the form hold true.
%   Handles the case where the form is wrapped in `all/1`.
%   @arg Form is the form to be evaluated.
%
%   @example
%     % Evaluate a form wrapped in `all/1`.
%     ?- eval(all(write(hello))).
%     hello
%     true.
%
eval(all(Form)) :-
    % Check that Form is instantiated (nonvar) and evaluate it as long as it is true.
    nonvar(Form), !, forall(eval(Form),true).
% Evaluate a form by calling do_metta/5 with the current Self context and display the output.
eval(Form) :-
    % Get the current self-reference.
    current_self(Self),
    % Execute the form and generate the output using do_metta/5.
    do_metta(true, exec, Self, Form, Out),
    % Write the result to the source.
    output_language(metta_answers, write_src(Out)).

%! eval(+Form, -Out) is det.
%   Evaluates a form and returns the output.
%   @arg Form is the input form to evaluate.
%   @arg Out is the output after evaluation.
%
%   @example
%     % Evaluate a form and retrieve the output.
%     ?- eval(write(hello), Out).
%     Out = some_output.
%
eval(Form, Out) :-
    % Get the current self-reference.
    current_self(Self),
    % Call eval/3 to evaluate the form and generate the output.
    eval(Self, Form, Out).

%! eval(+Self, +Form, -Out) is det.
%   Internal evaluation helper that evaluates a form with a timeout.
%   @arg Self is the current self-reference.
%   @arg Form is the input form to evaluate.
%   @arg Out is the output after evaluation.
%
%   @example
%     % Evaluate a form with the current self-reference and retrieve the output.
%     ?- current_self(Self), eval(Self, write(hello), Out).
%     Out = some_output.
%
eval(Self, Form, Out) :-
    % Use eval_H with a depth of default_depth(DEPTH), to evaluate the form.
    default_depth(DEPTH),
    eval_H(DEPTH, Self, Form, Out).

%! eval_I(+Self, +Form, -OOut) is det.
%   Evaluates a form and transforms the output using xform_out/2.
%   @arg Self is the current self-reference.
%   @arg Form is the form to evaluate.
%   @arg OOut is the transformed output.
%
%   @example
%     % Evaluate a form and transform the output.
%     ?- current_self(Self), eval_I(Self, write(hello), OOut).
%     OOut = some_output.
%
eval_I(Self, Form, OOut) :-
    % Evaluate the form with a depth using eval_H.
    default_depth(DEPTH),
    eval_H(DEPTH, Self, Form, Out),
    % Enable trace for debugging purposes.
    %trace,
    % Transform the output.
    xform_out(Out, OOut).

%! xform_out(+Out, -OOut) is det.
%   Transforms the output by checking if it is a return value.
%   @arg Out is the initial output.
%   @arg OOut is the transformed output.
%
%   @example
%     % Transform a returned value.
%     ?- xform_out(return_value, OOut).
%     OOut = return_value.
%
%     % Handle a non-return value.
%     ?- xform_out(some_output, OOut).
%     OOut = 'Empty'.
%
xform_out(Out, OOut) :-
    % If the output is a returned value, pass it through unchanged.
    is_returned(Out), !, OOut = Out.
% If the output is not a return value, set OOut to 'Empty'.
xform_out(_Out, 'Empty').

%! name_vars(+Equality) is det.
%   Assigns names to variables in the given term.
%   @arg P is the term containing variables to be named.
name_vars(Equality) :-
    % Ignore failures when naming variables.
    ignore(name_vars0(Equality)).

%! name_vars0(+Equality) is det.
%   Helper predicate that assigns names to variables if necessary.
%   @arg Equality is a term containing variables.
%
%   @example
%     % Assign names to variables in an equality expression.
%     ?- name_vars(X=Y).
%     true.
%
name_vars0(X=Y) :-
    % If X and Y are identical, do nothing.
    X == Y, !.
% If X is a '$VAR', set the name.
name_vars0(X='$VAR'(X)).

%! reset_cache is det.
% Resets internal caches.
%   Placeholder for cache resetting logic.
reset_cache.

%! reset_caches is det.
%   Resets all caches by evaluating each clause that defines reset_cache.
reset_caches :-
    % For each clause of reset_cache, run the body in rtrace mode to handle errors.
    forall(clause(reset_cache, Body), forall(rtrace_on_error(Body), true)).

%! u_do_metta_exec(+From, +Self, +TermV, +Term, +X, +NamedVarsList, +Was, -Output, -FOut) is det.
%   Executes a metta command (maybe interactively), handling potential errors and caching.
%   Resets caches and evaluates the execution command, catching any errors that occur.
%
%   @arg From is the source of the interaction (e.g., REPL, file).
%   @arg Self is the current context or environment.
%   @arg TermV is the evaluated version of the term.
%   @arg Term is the term to be executed.
%   @arg X is a variable placeholder.
%   @arg NamedVarsList is the list of variable names used in the execution.
%   @arg Was is the previous state before execution.
%   @arg Output is the output generated from the execution.
%   @arg FOut is the final output, after additional processing.
%
%   @example
%       ?- u_do_metta_exec(repl, self, TermV, my_term, X, NamedVarsList, Was, Output, FOut).
%       Output = ..., FOut = ...
u_do_metta_exec(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut) :-
    % Reset internal caches before executing the command.
    woc(reset_caches),
    % Attempt to execute the command interactively, catching any errors.
    (catch(u_do_metta_exec00(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),
          Error,
          % If an error occurs, log it along with the source and the term.
          write_src(error(Error,From,TermV)))).

each_pair_list(A-B,A,B).

%! u_do_metta_exec00(+From, +Self, +TermV, +Term, +X, +NamedVarsList, +Was, -Output, -FOut) is det.
%   A helper function that handles the core logic of the interactive metta execution, catching potential aborts.
%   This is the next layer in the call stack after u_do_metta_exec/9.
%
%   @arg From is the source of the interaction.
%   @arg Self is the current context or environment.
%   @arg TermV is the evaluated version of the term.
%   @arg Term is the term to be executed.
%   @arg X is a variable placeholder.
%   @arg NamedVarsList is the list of variable names used in the execution.
%   @arg Was is the previous state before execution.
%   @arg Output is the output generated from the execution.
%   @arg FOut is the final output, after additional processing.

u_do_metta_exec00(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut):-
   catch(u_do_metta_exec000(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),'$aborted', fbug(aborted(From,TermV))).

u_do_metta_exec000(FromLSP,Self,TermV,Term,X,NamedVarsList,Was,OutputL,FOutL):- ground(FromLSP), FromLSP = file(lsp(From)), nonvar(From), !,
   woc(findall(Output-FOut,u_do_metta_exec01(repl_true,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),List)),!,
   woc(maplist(each_pair_list,List,OutputL,FOutL)).

u_do_metta_exec000(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut) :-
    % Attempt the actual execution and catch any '$aborted' exceptions.
    (catch(u_do_metta_exec01(From,Self,TermV,Term,X,NamedVarsList,Was,Output,FOut),
          % Handle the '$aborted' exception by logging it.
          '$aborted', fbug(aborted(From,TermV)))).

%!  u_do_metta_exec01(+From, +Self, +_TermV, +Term, -X, +NamedVarsList, +Was, -VOutput, +FOut) is det.
%
%   Executes a term in a controlled interactive environment, handling history, skipping, and timing of results.
%   This predicate manages evaluation in an interactive session, possibly skipping certain executions based on file source and other conditions.
%
%   @arg From is the source of the input (file, repl, etc.).
%   @arg Self is the context for evaluation.
%   @arg _TermV is the evaluated term (unused here).
%   @arg Term is the term to be evaluated.
%   @arg X is the output/result of the evaluation.
%   @arg NamedVarsList is a list of variable bindings.
%   @arg Was is a flag indicating whether interaction occurred.
%   @arg VOutput is the variable holding the formatted output.
%   @arg FOut is the final output to be printed.
%
%   @example
%       ?- u_do_metta_exec01(file("example"), self, _, term(likes), Result, NamedVarsList, Was, Output, Final).
%       Result = likes(X,Y),
%       Output = "Execution Time: 1.5s",
%       Final = 'Completed Successfully'.
%
%   @see reset_eval_num/0 for resetting evaluation counters, notrace/1 to suppress trace during execution, and lazy_findall/3 for lazy evaluation.

:- discontiguous u_do_metta_exec01/9.

% Handles interactive execution of mettalog commands, but skips execution if From is a file and results are hidden.
u_do_metta_exec01(file(_), Self, _TermV, Term, X, _NamedVarsList, _Was, _Output, _FOut) :-
    notrace(file_hides_results(Term)), !, % Checks if the term should hide results when sourced from a file
    eval_args(Self, Term, X). % Evaluate arguments and return the result

u_do_metta_exec01(From,Self,TermV,Term,X,NamedVarsList,Was,VOutput,FOut):-
    notrace((flag(result_num,_,0), % Reset result number flag
     reset_eval_num, % Reset evaluation counters for a fresh start
     inside_assert(Term,BaseEval))), % Convert the current term into a base evaluation
    (notrace(skip_do_metta_exec(From,Self,TermV,BaseEval,Term,X,NamedVarsList,Was,VOutput,FOut))-> true;
     u_do_metta_exec02(From,Self,TermV,BaseEval,Term,X,NamedVarsList,Was,VOutput,FOut)).

% --exec=skip
skip_do_metta_exec(From,Self,TermV,BaseEval,_Term,X,NamedVarsList,_Was,_VOutput,_FOut):-
    option_value('exec',skip), From = file(_Filename), \+ use_metta_compiler,
    \+ always_exec(BaseEval),  \+ always_exec(TermV),
    color_g_mesg('#da70d6', (write('; SKIPPING: '), write_src_woi(TermV))),
    default_depth(DEPTH),
    prolog_only(if_t((TermV\=@=BaseEval),color_g_mesg('#da70d6', (write('\n% Thus: '), writeq(eval_H(DEPTH,Self,BaseEval,X)),writeln('.'))))),
    \+ \+ maybe_add_history(Self, BaseEval, NamedVarsList).

maybe_add_history(Self, BaseEval, NamedVarsList) :-
    % Prepare evaluation for the base term
    PL=eval(Self,BaseEval,X),
    user:maplist(name_vars, NamedVarsList),
    user:name_vars('OUT' = X),
    if_t(\+ option_value(doing_repl,true),
        if_t(\+ option_value(repl,true),
              if_t(option_value(prolog,true), add_history_pl(PL)))),
    if_t(option_value(repl,true), add_history_src(exec(BaseEval))),

    % Debug output in interactive mode, showing evaluated terms and results
   prolog_only((color_g_mesg('#da70d6', (write('% DEBUG:   '), writeq(PL), writeln('.'))))).


u_do_metta_exec02(From,Self,TermV,BaseEval,Term,_X,NamedVarsList,Was,VOutput,FOut):- !,
    notrace((
     if_t(is_interactive(From), \+ \+ maybe_add_history(Self, BaseEval, NamedVarsList)),
     % Was --exec=skip but this is the type of directive we'd do anyways
     if_t((From = file(_), option_value('exec',skip)), \+ \+ color_g_mesg('#da7036', (write('\n; Always-Exec: '), write_src_woi(TermV),nl,
        write_w_attvars(Term)))),

    % Initialize the result variable, with FOut to hold the final output
    Result = res(FOut),

    % If compatible, determine the evaluation mode (either 'leap' or 'each')
    (is_compatio -> option_else(answer,Leap,leap) ; option_else(answer,Leap, each)),

     % Set options for maximum and initial result counts, infinite results if needed
     option_else('limit-result-count',MaxResults,inf),
     option_else('initial-result-count',InitialResults,10),

     % Control variable initialized with max result count and leap control
     Control = contrl(InitialResults,MaxResults,Leap),

     GgGgGgGgGgG = (
         % Execute Term and capture the result
         ((  (Term,deterministic(Complete)),  % record if top-level metta evaluation is completed
             % Transform output for display and store it in the result
             notrace((xform_out(VOutput,Output), nb_setarg(1,Result,Output)))))),


    % Placeholder for a previous result, starting with 'Empty'
    Prev = prev_result('Empty'),

    % Print formatted answer output
    in_answer_io(format('~N[')))),!,

   % Interactive looping with possible timing and stepping control
   (
    forall_interactive(
    From, WasInteractive,Complete, %may_rtrace
      timed_call((GgGgGgGgGgG),Seconds),


         (((

         %(Complete==true->!;true),

         ((print_result_output(WasInteractive,Complete,ResNum,Prev,NamedVarsList,Control,Result,Seconds,Was,Output,Stepping))),

        (ResNum >= MaxResults -> ! ; true),


        Cut = _,
        Next = _,
        once((((Stepping==true) ->
         ( repeat, % Q
          old_not_compatio(format("~npress ';' for more solutions ")),get_single_char_key(C),
          old_not_compatio((writeq(key=C),nl)),
         (C=='b' -> (once(repl),fail) ;
         (C=='B' -> (once(prolog),fail) ;
         (C=='a' -> (notrace(abort),fail) ;
         (C=='e' -> (notrace(halt(5)),fail) ;
         (C=='m' -> (make,fail) ;
         (C=='c' -> (trace,Next=true) ;
         (C=='C' -> (once(cls),fail) ;
         (C==' ' -> (trace,Next=true) ;
         (C=='t' -> (nop(set_debug(eval,true)),rtrace,Next=true) ;
         (C=='T' -> (set_debug(eval,true),Next=true);
         (C=='?' -> (print_debug_help,fail)) ;
         (C=='*' -> (print_last_choicepoint_upwards,fail)) ;

         (C==';' -> Next=true ;
         (C==esc('[A',[27,91,65]) -> (Cut=true,Next=false) ;
         (C==esc('[B',[27,91,66]) -> (nb_setarg(3, Control, leap),Cut=false,Next=true) ;
         (C=='L' -> nb_setarg(2, Control, ResNum) ;
         (C=='l' -> (nb_setarg(3, Control, leap),Next=true) ;
         (((C=='\n');(C=='\r')) -> (Cut=false,nb_setarg(3, Control, leap),Next=true);
         (C=='g' -> write_src(exec(TermV));
         (C=='G' -> (bt, Next=false);
         (C=='s' -> (Cut=true,Next=false);
         (true -> (write('Unknown Char'),fail))))))))))))))))))))), % should consume 'b's paren
         (nonvar(Next);nonvar(Cut))) ; true))),

            ((Complete==true;Cut==true) ->! ; true),
            (nonvar(Next)->Next==true; true),
            ((flag(result_num,ResNum,ResNum),ResNum >= MaxResults) -> (!,fail) ; true)
           /*(Complete\==true, \+ WasInteractive, Control = contrl(_,_,leap)) -> true ;

                          )),
                     not_compatio(extra_answer_padding(format('~N~n')))
            )*/

            )))
                    ) *-> % Each forall_interactive
                        (((flag(result_num,ResNum,ResNum),ResNum >= MaxResults) -> ! ; true),ignore(Result = res(FOut)),ignore(Output = (FOut)))
                    ; % Last forall_interactive
                       (flag(result_num,ResNum,ResNum),(ResNum==0-> (old_not_compatio(format('~N;; no-results ;; ~n')),!,true);true))

   ),

   in_answer_io((write(']\n'),if_t(\+is_mettalog, nop(nl)))),
   flag(need_prompt,_,1),
   ignore(Result = res(FOut)).

print_result_output(WasInteractive,Complete,ResNum,Prev,NamedVarsList,Control,Result,Seconds,Was,Output,Stepping):-
         set_option_value(interactive,WasInteractive),
         Control = contrl(InitialResultLeash,Max,DoLeap),
    assertion(InitialResultLeash==inf;number(InitialResultLeash)),
    assertion(Max==inf;number(Max)),
         nb_setarg(1,Result,Output),
         current_input(CI), read_pending_codes(CI,_,[]),
         flag(result_num,R,R+1),
         flag(result_num,ResNum,ResNum),
         reset_eval_num,
         %not_compatio(format('~N')), maybe more space between answers?

       user_io((
         in_answer_io(if_t((Prev\=@=prev_result('Empty')),write(', '))),
            nb_setarg(1,Prev,Output))),


       output_language(answers,(if_t(ResNum=<Max,
           (
           (((ResNum==1,Complete==true)->(not_compatio(format('~N~nDeterministic: ',  [])), !);          %or Nondet
           /* previously: handle deterministic result output */
           (Complete==true -> (not_compatio(format('~N~nR(~w): ',[ResNum])),! );
            not_compatio(format('~N~nN(~w): ',[ResNum]))))),
         not_compatio(ignore(((
              if_t( \+ symbolic(Output), not_compatio(nop(nl))),
              %if_t(ResNum==1,in_answer_io(format('~N['))),
              % user_io
              (with_indents(is_mettalog,
                color_g_mesg_ok(yellow,
                 \+ \+
                 (maybe_name_vars(NamedVarsList),
                  old_not_compatio(write_bsrc(Output)),
                  true)))) )) )))))),

         in_answer_io(write_asrc((Output))),


         ((Complete \== true, WasInteractive, DoLeap \== leap,
                  InitialResultLeash =< ResNum, ResNum < Max) -> Stepping = true ; Stepping = false),

         %if_debugging(time,with_output_to(user_error,give_time('Execution',Seconds))),
           if_t((Stepping==true;Complete==true),if_trace(time,color_g_mesg_ok(yellow,(user_io(give_time('Execution',Seconds)))))),

           color_g_mesg(green,
              ignore((NamedVarsList \=@= Was ->(not_compatio((
                   reverse(NamedVarsList,NamedVarsListR),
                   maplist(print_var,NamedVarsListR), nop(nl)))) ; true))).




%old_not_compatio(_G):- \+ is_testing, !.
old_not_compatio(G):- call(G),ttyflush.

%! maybe_assign(+N_V) is det.
%
%   Attempts to assign variable V to the variable name N, if V is unbound.
%
%   @arg N=V is the variable assignment term.
%
%   @example
%     % Attempt to assign a variable name.
%     ?- maybe_assign(x=Var).
%     Var = '$VAR'(x).
%
maybe_assign(N=V):- ignore(V='$VAR'(N)).

% Disable the debug mode for the 'metta(time)' predicate.
:- nodebug(metta(time)).

%! mqd is det.
%
%   A query executor that retrieves terms from a knowledge base using 'query-info', computes variable intersections,
%   and evaluates the query Q against the term T in the context of a flybase.
%
%   @example
%     % Execute the query matching process.
%     ?- mqd.
%     Entity1
%     Entity2
%     ...
%     true.
%
mqd :-
    % Iterate over all metta_atom/3 calls that match the 'query-info' term.
    forall(metta_atom(_KB, ['query-info', E, T, Q]),
        (writeln(E),    % Print the entity E.
        term_variables(T, TVs),   % Get the variables of the term T.
        term_variables(Q, QVs),   % Get the variables of the query Q.
        intersection(TVs, QVs, _, _, SVs),  % Compute the intersection of the variables of T and Q.
        notrace(eval(['match', '&flybase', Q, T], SVs)))).   % Evaluate the matching terms.

%! get_single_char_key(-O) is det.
%
%   Reads a single character from input and transforms it into an atom.
%   Handles escape sequences for special keys.
%
%   @arg O is the output character, transformed into an atom.
%
%   @example
%     % Read a single character and transform it into an atom.
%     ?- get_single_char_key(O).
%     O = a.
%
get_single_char_key(O):-
    % Get the single character input.
    get_single_char(C),
    % Recursively read characters until a valid key is obtained.
    get_single_char_key(C, O).

%! get_single_char_key(+C, -A) is det.
%
%   Handles special cases such as escape sequences for the arrow keys.
%
%   @arg C is the character received.
%   @arg A is the resulting atom.
%
%   @example
%     % Handle escape sequence input.
%     ?- get_single_char_key(27, esc(A, [27|Codes])).
%     A = '\e[A', Codes = [91, 65].
%
get_single_char_key(27, esc(A,[27|O])):-
    !,
    % Read pending escape sequences and convert them to a name.
    current_input(Input),
    read_pending_codes(Input, O, []),
    name(A, O).
% Convert the character code C into an atom A.
get_single_char_key(C, A):- name(A, [C]).

%! forall_interactive(+From, +WasInteractive, +Complete, :Goal, :After) is det.
%
%   Executes a goal in interactive or non-interactive contexts based on the source (From).
%   It handles stepping, completion, and quiet execution of post-goal actions.
%
%   @arg From is the source of the execution (e.g., file, prolog, REPL).
%   @arg WasInteractive is a flag indicating whether the execution was interactive.
%   @arg Complete indicates whether the goal reached a final result.
%   @arg Goal is the main goal to be executed interactively.
%   @arg After is the action to perform after the goal is executed.
forall_interactive(file(_), false, Complete, Goal, After) :- !,
    % Execute the goal.
    %format("%%%%%%%%%%%%%%%%%%%%%%%%%0 ~w\n",[Goal]),
    Goal,
    % If the goal is complete, execute 'After', otherwise skip it.
    (Complete == true -> (!, call(After), !) ; ( \+ quietly(After))).
forall_interactive(prolog, false, Complete, Goal, After) :- !,
    % Execute the goal.
    %format("%%%%%%%%%%%%%%%%%%%%%%%%%1 ~w\n",[Goal]),
    Goal,
    % If the goal is complete, succeed, otherwise continue.
    (Complete == true -> ! ; true),
    % Execute 'After' quietly (without trace output).
    quietly(After).
forall_interactive(From, WasInteractive, Complete, Goal, After) :-
    % Check if the source (From) is interactive.
    (is_interactive(From) -> WasInteractive = true ; WasInteractive = false),
    !,
    % Execute the goal.
    Goal,
    % If the goal is complete, quietly execute 'After', otherwise negate 'After'.
    (Complete == true -> (!, quietly(After), !) ; ( \+ quietly(After))).

%!  print_var(+Name, +Var) is det.
%
%   Prints a variable name and its value.
%
%   @arg Name is the name of the variable.
%   @arg Var is the value of the variable.
%
%   @example Example of printing a variable:
%       ?- print_var(X, 42).
%       X = 42.
print_var(Name=Var) :- print_var(Name,Var).

%!  write_var(+V) is det.
%
%   Writes a variable, handling special cases like unbound or '$VAR'.
%
%   @arg V is the variable to be written.
%
%   @example
%     % Write an unbound variable.
%     ?- write_var(X).
%     _G123
%
write_var(V):- var(V), !, write_dvar(V),!.  % Write the unbound variable using a helper predicate.
write_var('$VAR'(S)):- !, write_dvar(S),!.  % Handle the Prolog internal variable representation.
write_var(V):- write_dvar(V),!.  % Default case: write the variable.

%!  print_var(+Name, +Var) is det.
%
%   Prints a variable assignment in the format `Name = Var`.
%
%   @arg Name The name of the variable.
%   @arg Var  The value of the variable.
%
%   @example
%     % Print a variable assignment.
%     ?- print_var(Name, 42).
%     Name = 42
%
print_var(Name,Var):-
    % Print the variable name.
    write_var(Name),
    % Print the equality sign.
    write(' = '),
    % Print the source of the variable value.
    write_bsrc(Var),
    % Print a newline after the variable.
    nl.

%!  write_asrc(+Var) is det.
%
%   Writes a variable, skipping if it is 'Empty' and compatible with the environment.
%
%   @arg Var is the variable to be written.
%
%   @example
%     % Write a variable unless it is 'Empty' in a compatible environment.
%     ?- write_asrc(X).
%
write_asrc(Var):- Var=='Empty',is_compatio,!.  % Skip writing if the variable is 'Empty' in a compatible mode.
write_asrc(Var):- write_bsrc(Var),!.  % Otherwise, write the variable.

%!  write_bsrc(+Var) is det.
%
%   Writes the value of a variable, handling ground terms and variables with goals.
%
%   @arg Var is the variable to be written.
%
%   @example
%     % Write the value of a ground term.
%     ?- write_bsrc(42).
%     42
%
write_bsrc(Var):- Var=='Empty',!,write(Var).  % Special case: write 'Empty' directly.
write_bsrc(Var):- ground(Var),!,write_bsrc1(Var).  % If the variable is ground, write it directly.
write_bsrc(Var):- copy_term(Var,Copy,Goals),Var=Copy,
  exclude(excluded_hidden_goal,Goals,UnhiddenGoals),
  write_bsrc_goal(Var,UnhiddenGoals).  % For non-ground terms, handle goals.

excluded_hidden_goal(name_variable(_,_)).

%!  write_bsrc_goal(+Var, +Goals) is det.
%
%   Writes a variable along with its associated goals, if any.
%
%   @arg Var   The variable to be written.
%   @arg Goals A list of goals associated with the variable.
%
%   @example
%     % Write a variable with goals.
%     ?- write_bsrc_goal(Var, [goal1, goal2]).
%     Var { goal1 goal2 }
%
write_bsrc_goal(Var,[]):- write_src(Var).  % Write the variable if no goals are present.
write_bsrc_goal(Var,[G|Goals]):-
    % Write the variable.
    write_bsrc1(Var),
    % Write the opening brace for goals.
    write(' { '),
    % Write the first goal.
    write_bsrc1(G),
    % Write the remaining goals, separated by spaces.
    maplist(write_src_space, Goals),
    % Write the closing brace and newline.
    writeln(' } ').

%!  write_bsrc1(+Var) is det.
%
%   Writes the value of a variable (often not indenting it)
%
%   @arg Var is the variable to be written.
%
%   @example
%     % Write a list of lists.
%     ?- write_bsrc1([[1, 2], [3, 4]]).
%     [[1,2],[3,4]]
%
write_bsrc1(Var):- is_list(Var), member(E, Var), is_list(E), !, write_src(Var).
write_bsrc1(Var):- write_src_woi(Var).

%!  write_src_space(+Goal) is det.
%
%   Writes a goal with a preceding space.
%
%   @arg Goal is the goal to be written.
%
%   @example
%     % Write a goal with a space before it.
%     ?- write_src_space(goal1).
%      goal1
%
write_src_space(Goal):-
    % Write a space before the goal.
    write(' '),
    % Write the goal.
    write_bsrc1(Goal).

%!  get_term_variables(+Term, -DontCaresN, -CSingletonsN, -CNonSingletonsN) is det.
%
%   Collects variables from a Prolog term, identifying do-not-care variables, singletons, and non-singletons.
%   It then maps these variables into named variable lists.
%
%   @arg Term is the Prolog term whose variables are being analyzed.
%   @arg DontCaresN is the list of do-not-care variables (those represented by underscores).
%   @arg CSingletonsN is the list of singleton variables (those that appear only once).
%   @arg CNonSingletonsN is the list of non-singleton variables (those that appear more than once).
%
%   @example Analyze the variables in a term:
%       ?- get_term_variables(foo(X, _Y, X), DontCares, Singletons, NonSingletons).
%       DontCares = [_Y], Singletons = [X], NonSingletons = [].
get_term_variables(Term, DontCaresN, CSingletonsN, CNonSingletonsN) :-
    % Extract all variables from the term.
    term_variables(Term, AllVars),
    % Get the global variable names.
    get_global_varnames(VNs),
    % Log the extracted variables and global variable names.
    writeqln(term_variables(Term, AllVars)=VNs),
    % Identify singleton variables in the term.
    term_singletons(Term, Singletons),
    % Identify do-not-care variables in the term.
    term_dont_cares(Term, DontCares),
    % Filter out singletons from the set of all variables.
    include(not_in_eq(Singletons), AllVars, NonSingletons),
    % Remove do-not-care variables from the non-singleton set.
    include(not_in_eq(DontCares), NonSingletons, CNonSingletons),
    % Remove do-not-care variables from the singleton set.
    include(not_in_eq(DontCares), Singletons, CSingletons),
    % Map the do-not-care, singleton, and non-singleton variables into named variable lists.
    maplist(into_named_vars, [DontCares, CSingletons, CNonSingletons],
                             [DontCaresN, CSingletonsN, CNonSingletonsN]),
    % Log the final result.
    writeqln([DontCaresN, CSingletonsN, CNonSingletonsN]).


%!  term_dont_cares(+Term, -DontCares) is det.
%
%   Finds the do-not-care variables (those represented by underscores) in a term.
%
%   @arg Term is the term to analyze.
%   @arg DontCares is the list of do-not-care variables in the term.
%
%   @example
%     % Find do-not-care variables in a term.
%     ?- term_dont_cares(f(_, X, _), DontCares).
%     DontCares = [_G123, _G124].
%
term_dont_cares(Term, DontCares) :-
    % Extract all variables from the term.
    term_variables(Term, AllVars),
    % Get global variable names.
    get_global_varnames(VNs),
    % Find variables that have sub-variables in the term.
    include(has_sub_var(AllVars), VNs, HVNs),
    % Filter out underscore variables (do-not-cares).
    include(underscore_vars, HVNs, DontCareNs),
    % Extract the actual variable values from the named variables.
    maplist(arg(2), DontCareNs, DontCares).

%!  into_named_vars(+Vars, -L) is det.
%
%   Converts a list of variables into a list of named variables.
%
%   @arg Vars is the input list of variables or a term containing variables.
%   @arg L is the resulting list of named variables.
%
%   @example
%   ?- into_named_vars([X,Y,Z], L).
%   L = ['X'=X, 'Y'=Y, 'Z'=Z].
into_named_vars(Vars,L):-
    % If Vars is a list, process each variable individually.
    is_list(Vars), !,
    % Map each variable to its named version using name_for_var_vn/2.
    maplist(name_for_var_vn,Vars,L).
into_named_vars(Vars,L):-
    % If Vars is a term, extract the variables using term_variables/2.
    term_variables(Vars,VVs),!,
    % Convert extracted variables into named variables.
    into_named_vars(VVs,L).


%!  has_sub_var(+AllVars, +Equality) is nondet.
%
%   Succeeds if V is a sub-variable of any of the variables in AllVars.
%
%   @arg AllVars is the list of variables to search in.
%   @arg Equality is the variable to check as a sub-variable.
%
%   @example
%     % Check if a variable is a sub-variable of another.
%     ?- has_sub_var([X, Y], X=Z).
%     true.
%
has_sub_var(AllVars,_=V):-
    % Check if V is a sub-variable of any variable in AllVars.
    sub_var_safely(V,AllVars).


%!  underscore_vars(+Var) is nondet.
%
%   Succeeds if the variable or name represents a do-not-care variable (underscore).
%
%   @arg Var is the variable or name to check.
%
%   @example
%     % Check if a variable is a do-not-care variable.
%     ?- underscore_vars('_G123').
%     true.
%
underscore_vars(V):-
    % If V is a variable, retrieve its name and check if it is an underscore variable.
    var(V),!,
    name_for_var(V,N),!,
    underscore_vars(N).
underscore_vars(N=_):-
    % If N is a symbolic value, further check if it starts with an underscore.
    !, symbolic(N),!,
    underscore_vars(N).
underscore_vars(N):-
    % If N is a symbolic value, check if it starts with an underscore.
    symbolic(N),!,
    symbol_concat('_',_,N).


%!  get_global_varnames(-VNs) is det.
%
%   Retrieves the global list of variable names.
%
%   @arg VNs is the list of variable names in the current context.
%
%   @example
%     % Retrieve the global variable names.
%     ?- get_global_varnames(VNs).
%     VNs = [X, Y].
%
get_global_varnames(VNs):-
    % If there are variable names in nb_current, use them.
    nb_current('variable_names',VNs),VNs\==[],!.
get_global_varnames(VNs):-
    % Otherwise, retrieve variable names from the current Prolog context.
    prolog_load_context(variable_names,VNs),!.


%!  maybe_set_var_names(+List) is det.
%
%   Conditionally sets the variable names if the list is not empty.
%
%   @arg List is the list of variable names.
%
%   @example
%     % Set a list of variable names if it is non-empty.
%     ?- maybe_set_var_names([X, Y]).
%     true.
%
maybe_set_var_names(List):-
    % If the list is empty, do nothing.
    List==[],!.
maybe_set_var_names(List):-
    % If the list is non-empty, set the list of variable names.
    is_list(List),!,
    nb_linkval('$variable_names',List),
    nb_linkval(variable_names,List).
maybe_set_var_names(_).


%!  name_for_var_vn(+V, -EqualityPair) is det.
%
%   Maps a variable V to a named variable pair N=V.
%
%   @arg V is the input variable.
%   @arg EqualityPair is the resulting named variable pair.
%
%   @example
%     % Map a variable to its named pair.
%     ?- X = some_value, name_for_var_vn(X, Pair).
%     Pair = N = some_value.
%
name_for_var_vn(V,N=V):-
    % Retrieve the name for the variable V.
    name_for_var(V,N).


%!  name_for_var(+V, -N) is det.
%
%   Retrieves the name for a variable V based on the current variable names.
%
%   @arg V is the variable whose name is being retrieved.
%   @arg N is the name corresponding to V.
%
%   @example
%     % Retrieve the name for a variable from global names.
%     ?- nb_linkval(variable_names, ['X'=X]), name_for_var(X, N).
%     N = 'X'.
%
name_for_var(V,N):-
    % If V is a variable, check the global variable names.
    var(V),!,
    get_global_varnames(VNs),
    member(N=VV,VNs),
    VV==V,!.
name_for_var(N=_,N):- !.
name_for_var(V,N):-
    % Convert the variable V to an atom representing its name.
    term_to_atom(V,N),!.

%!  really_trace is nondet.
%
%   Activates tracing if 'exec' or 'eval' tracing options are enabled, or if debugging is enabled for exec or eval.
%   Used as a helper to conditionally invoke tracing logic.
%
%   @example Example usage:
%       ?- really_trace.
%       Trace mode will be activated if conditions match.
really_trace :-
    % Check if 'exec' or 'eval' tracing options are set, or if debugging is active.
    once(option_value('exec', rtrace); option_value('eval', rtrace); is_debugging((exec)); is_debugging((eval))).

% Ensures that tracing is enabled before running the goal.
may_rtrace(Goal):-
    % If tracing is required, call really_rtrace/1.
    really_trace, !, really_rtrace(Goal).
% Otherwise, enable tracing temporarily for the goal and execute it.
may_rtrace(Goal):-
    Goal *-> true; ( \+ tracing, trace, really_rtrace(Goal)).

% Actual tracing logic that checks for transpiling and invokes the goal with tracing.
really_rtrace(Goal):-
    % If transpiling, call rtrace/1 on the goal.
    is_transpiling, !, rtrace(call(Goal)).
% Otherwise, run the goal with debug contexts for exec and eval.
really_rtrace(Goal):-
    with_debug((e), with_debug((exec), Goal)).


%!  rtrace_on_existence_error(:G) is nondet.
%
%   Attempts to execute the goal G, but if an existence error is encountered, it switches to tracing and retries G.
%
%   @arg G is the goal to execute.
%
%   @example
%     % Demonstrate retrying a goal on existence error with tracing.
%     ?- rtrace_on_existence_error(nonexistent_predicate).
%     ERROR: existence_error ...
%
rtrace_on_existence_error(G):-
    % Catch any existence errors, log them, and retry G with tracing enabled.
    !, catch_err(G, E, (fbug(E = G), \+ tracing, trace, rtrace(G))).


%!  prolog_only(:Goal) is nondet.
%
%   Runs the goal if tracing is enabled for Prolog operations.
%
%   @arg Goal is the Prolog goal to execute.
%
%   @example
%     % Run a goal only if Prolog tracing is enabled.
%     ?- prolog_only(write('Tracing enabled for Prolog')).
%     Tracing enabled for Prolog
%
prolog_only(Goal):-
    % If Prolog tracing is enabled, run the goal.
    if_trace(prolog, Goal).


%!  write_compiled_exec(+Exec, +Goal) is det.
%
%   Compiles the goal for execution and prints the compiled result.
%
%   @arg Exec is the compiled execution result.
%   @arg Goal is the goal being compiled and executed.
%
%   @example
%     % Compile and print the result of a goal.
%     ?- write_compiled_exec(Result, my_goal).
%     #114411: answer2(Result) :- my_goal
%
write_compiled_exec(Exec, Goal):-
    % Compile the goal for execution and store the result in Res.
    compile_for_exec(Res, Exec, Goal),
    % Print the compiled goal with formatting.
    Call = do_metta_runtime(Res, Goal),
    output_language(prolog, notrace((color_g_mesg('#114411', print_pl_source(:- Call))))),
    nop(call(Call)).


%!  verbose_unify(+Term) is det.
%
%   Activates verbose unification mode for variables in the given term. If no
%   specific trace context is provided, it defaults to 'trace'.
%
%   @arg Term The term whose variables will be traced.
%
%   @example
%     % Enable verbose unification on a term.
%     ?- verbose_unify(X + Y).
%     true.
%
verbose_unify(Term) :-
    % Default to 'trace' context for verbose unification.
    verbose_unify(trace, Term).

%!  verbose_unify(+What, +Term) is det.
%
%   Activates verbose unification mode for variables in the term with the specified context.
%
%   @arg What The trace context for verbose unification.
%   @arg Term The term whose variables will be traced.
%
%   @example
%     % Enable verbose unification with a custom context.
%     ?- verbose_unify(my_trace, X + Y).
%     true.
%
verbose_unify(What, Term) :-
    % Extract variables from the term and apply `verbose_unify0` to each.
    term_variables(Term, Vars),
    maplist(verbose_unify0(What), Vars),
    !.

%!  verbose_unify0(+What, +Var) is det.
%
%   Applies verbose unification to individual variables by assigning the `verbose_unify` attribute.
%
%   @arg What The trace context for verbose unification.
%   @arg Var  The variable to which the attribute will be assigned.
%
%   @example
%     % Assign the 'verbose_unify' attribute to a variable.
%     ?- verbose_unify0(trace, X).
%     true.
%
verbose_unify0(What, Var) :-
    % Set the 'verbose_unify' attribute for the variable.
    put_attr(Var, verbose_unify, What).

% Attribute unification hook for verbose_unify, logs when variables are unified.
verbose_unify:attr_unify_hook(Attr, Value) :-
    % Log the unification process for the attribute and value.
    format('~N~q~n', [verbose_unify:attr_unify_hook(Attr, Value)]),
    % Perform verbose unification.
    vu(Attr, Value).

%! vu(+Attr, +Value) is det.
%
%   Handles different verbose unification cases based on the attributes provided.
%
%   @arg Attr is the attribute that determines the unification behavior.
%   @arg Value is the value to unify.
%
%   @example Example usage:
%
%       ?- vu(trace, _).
%       % Starts the trace.
%
%       ?- vu(fail, _).
%       % Fails the unification.
%
vu(_Attr, Value):-
    % Skip if the value is a frozen variable (ftVar).
    is_ftVar(Value),
    !.
vu(fail, _Value):-
    % Fail the unification if the attribute is 'fail'.
    !, fail.
vu(true, _Value):-
    % Succeed silently if the attribute is 'true'.
    !.
vu(trace, _Value):-
    % Enable tracing if the attribute is 'trace'.
    trace.

%!  toplevel_goal(+Goal) is det.
%
%   Entry point for executing a goal with variable tracing disabled.
%
%   @arg Goal is the goal to be executed.
%
%   @example
%       ?- toplevel_goal(likes(A,B)).
%       Solution: A = alice, B = bob.
%

% Entry point for a goal execution, tracing is turned off by default.
toplevel_goal(Goal) :-
    % Extract variables from the goal.
    term_variables(Goal,Vars),
    % Pass the goal and its variables to the interactive loop.
    interact(Vars, Goal, trace_off).


%!  trace_goal(+Goal) is det.
%!  trace_goal(+Goal, +Tracing) is det.
%
%   Entry point for executing a goal with tracing enabled or disabled.
%
%   @arg Goal is the goal to be executed.
%   @arg Tracing is either trace_on or trace_off to control tracing behavior.
%
%   @example
%       ?- trace_goal(likes(A,B)).
%       Entering goal: likes(A,B)
%       Solution: A = alice, B = bob.
%

% Entry point for executing a goal with tracing enabled by default.
trace_goal(Goal) :-
    % By default, tracing is enabled.
    trace_goal(Goal, trace_on).

% Execute a goal with optional tracing.
trace_goal(Goal, Tracing) :-
    % If tracing is on, print the goal being entered.
    (Tracing == trace_on -> writeln('Entering goal:'), writeln(Goal) ; true),
    % Extract variables from the goal.
    term_variables(Goal, Variables),
    % Call the goal.
    (call(Goal) ->
        % If the goal succeeds, print the result and interact with the user if tracing is on.
        (Tracing == trace_on -> writeln('Goal succeeded with:'), writeln(Variables) ; true),
        interact(Variables, Goal, Tracing)
    ;   % If the goal fails, log the failure if tracing is on.
        (Tracing == trace_on -> writeln('Goal failed.') ; true),
        false
    ).


%!  interact(+Variables, +Goal, +Tracing) is det.
%
%   Handles interaction with the user, allowing them to request the next solution or execute commands.
%
%   @arg Variables is the list of variables bound by the goal.
%   @arg Goal is the goal currently being executed.
%   @arg Tracing is the trace state (on or off).
%
%   @example
%       ?- interact([A, B], likes(A, B), trace_on).
%       Solution: A = alice, B = bob.
%       [;next]?
%

% This predicate handles user interaction and command processing during execution.
interact(Variables, Goal, Tracing) :-
    % Call the goal and print the result.
    call(Goal), write('Solution: '), write_src(Variables),
    % Prompt the user to continue or stop.
    write(' [;next]?'),
    % Get a single character input from the user.
    get_single_char(Code),
    % If the input is a valid command, process it.
    (command(Code, Command) ->
        handle_command(Command, Variables, Goal, Tracing)
    ;   % If the input is unknown, print an error and continue interaction.
        writeln('Unknown command.'), interact(Variables, Goal, Tracing) % Handle unknown commands.
    ).

%!  install_readline_editline is det.
%
%   Installs readline or editline support for the current input stream, enabling advanced input handling.
%
%   @example Example usage:
%
%       ?- install_readline_editline.
%
%   This installs readline/editline support, allowing for line editing and history during input.
:- dynamic(is_installed_readline_editline/1).
:- volatile(is_installed_readline_editline/1).

install_readline_editline :-  is_win64,!.
install_readline_editline :-  is_docker,!.
install_readline_editline :-
    % Get the current input stream.
    current_input(Input),
    % Install readline support for the current input.
    install_readline(Input),
    !.


%!  el_wrap_metta(+Input) is det.
%
%   Wraps the input stream in editline (or readline) for use with mettalog, if it is a TTY (terminal).
%
%   @arg Input is the input stream to be wrapped.
%
%   @example Example usage:
%
%       ?- el_wrap_metta(user_input).
%
%   This wraps the user input stream, enabling line editing and history for interactive inputs.
%
%   Note: This setup is specific to the mettalog environment and does not use the default SWI-Prolog completions.
%
%   @see editline:el_wrap/4 for more details on wrapping input streams in editline.
el_wrap_metta(Input) :-
    % If the input is already wrapped, do nothing.
    el_wrapped(Input),
    !.
el_wrap_metta(Input) :-
    % Check if the input is from a terminal (tty).
    stream_property(Input, tty(true)),
    !,
    % Wrap the input stream using editline with specific configurations for swipl.
    editline:el_wrap(swipl, Input, user_output, user_error),
    % Add custom commands specific to mettalog environment.
    add_metta_commands(Input),
    % Ensure that editline is properly set up for the input stream.
    forall(editline:el_setup(Input), true).
el_wrap_metta(_NoTTY) :-
    % Do nothing for non-tty clients (e.g., SWISH or HTTP/REST-based input).
    true.

%!  add_metta_commands(+Input) is det.
%
%   Sets up command bindings in the terminal for metta operations, adding useful key commands for editing.
%   The predicate uses editline predicates to bind input commands for electric matching, history search, etc.
%
%   @arg Input is the terminal input stream for which commands are being bound.
%
%   This function ensures that useful key bindings, like electric matching for parentheses and history searching, are added to the current input.
%   File completions are commented out for potential future use.
%
/* previously: It would be nice to include file name completion here, but it was skipped for atom completion */
add_metta_commands(Input) :-
    % TODO: File name completion would be useful, but it is currently skipped for Prolog atom completion.
    % Bind a function for atom and file completion. Commented out.
    editline:el_addfn(Input,complete,'Complete atoms and files',editline:complete),
    % Bind a function to list completions. Also commented out.
    editline:el_addfn(Input,show_completions,'List completions',editline:show_completions),
    % Bind the electric function to highlight matching brackets during input.
    editline:el_addfn(Input, electric, 'Indicate matching bracket', editline:electric),
    % Bind the incremental search function to allow searching through input history.
    editline:el_addfn(Input, isearch_history, 'Incremental search in history', editline:isearch_history),
    % Previously bound commands for tab completion and listing completions, commented out for now.
    editline:el_bind(Input,["^I",complete]),
    editline:el_bind(Input,["^[?",show_completions]),
    % Bind the "^R" key to initiate an incremental search through history.
    editline:el_bind(Input, ["^R", isearch_history]),
    % Enable the electric mode for the current input.
    editline:bind_electric(Input),
    % Source additional commands from the input (if any are defined).
    editline:el_source(Input, _).

%!  install_readline(+Input) is det.
%
%   Installs readline functionality for the input stream, providing useful editing commands and history.
%   This predicate configures the Prolog input stream to support terminal history and command completion using the editline library.
%
%   @arg Input is the input stream for which readline features should be installed.
%
%   This version skips installation for non-tty(true) clients (e.g., SWISH or HTTP clients).
%
%   @example Example of usage:
%
%       ?- install_readline(user_input).
%
is_docker :- exists_file('/.dockerenv'),!.

install_readline(_Input):- is_docker,!. % too chancy
install_readline(_Input):- is_win64,!. % already installed
install_readline(Input):-
    % Check if readline is already installed for this Input.
    is_installed_readline_editline(Input), !.
% previously: Compatibility check for non-standard environments. Skipped for now.
%install_readline(_):- is_compatio, !.
install_readline(Input):-
    % Check if Input is a terminal (tty).
    stream_property(Input, tty(true)),
    % Assert that readline is now installed for this Input.
    assert(is_installed_readline_editline(Input)),
    % Install editline functionality (main logic for readline).
    install_readline_editline1,
    % Use the editline library (previously: readline library is commented out).
    %use_module(library(readline)),
    use_module(library(editline)),
    % Catch potential errors when loading history (currently ignored).
    %nop(catch(load_history,_,true)),
    % Unwrap the Prolog input wrapper, so that the custom readline features can be used.
    ignore(el_unwrap(Input)),
    % Wrap the input with the Metta readline handler.
    ignore(el_wrap_metta(Input)),
    load_metta_history_from_txt_file('~/.config/metta/history.txt'),
    % Load command history from a file, if it exists.
    history_file_location(HistoryFile),
    % Check if the history file exists, ensuring we can append to it.
    check_file_exists_for_append(HistoryFile),
    % Load the previous history from the file into the readline session.
    el_read_history(Input, HistoryFile),
    % previously: Adding specific history commands for convenience, now skipped.
    %add_history_string("!(load-flybase-full)"),
    %add_history_string("!(pfb3)"),
    %add_history_string("!(obo-alt-id $X BS:00063)"),
    %add_history_string("!(and (total-rows $T TR$) (unique-values $T2 $Col $TR))"),
    !.

% Clause to handle non-tty(true) clients, like SWISH or HTTP server requests.
install_readline(_NoTTY). % For non-tty(true) clients over SWISH/Http/Rest server


load_metta_history_from_txt_file(File):-
   exists_file(File),!,
   open(File, read, In, [encoding(utf8)]),
   repeat,
   read_line_to_string(In, Line),
   (Line==end_of_file -> ! ;
     (add_history_file_string(Line),fail)).
load_metta_history_from_txt_file(File):-
   expand_file_name(File,List),maplist(load_metta_history_from_txt_file,List).

skip_over_history_txt(Line):- text_to_string(Line,Str),Str\==Line,!,skip_over_history_txt(Line).
skip_over_history_txt(Line):- atom_concat('#V',_,Line),!.
skip_over_history_txt(Line):- atom_concat('_H',_,Line),!.
add_history_file_string(Line):- text_to_string(Line,Str),Str\==Line,!,add_history_file_string(Str).
add_history_file_string(Line):- skip_over_history_txt(Line),!.
add_history_file_string(Line):-
  string_replace(Line,'\\n','\n',String),
  string_replace(String,'\\t','\t',Str),
  add_history_string(Str), !.



%!  install_readline_editline1 is det.
%
%   Ensures readline is properly installed using the editline library, but only runs once.
%   This setup prevents redundant installations and simplifies configuration for terminal input.
%
%   @example Example of usage:
%
%       ?- install_readline_editline1.
%
%   This predicate will execute once to install readline, preventing multiple installations.
%
:- dynamic setup_done/0.
:- volatile setup_done/0.

% If setup_done is already asserted, skip the rest of the predicate.
install_readline_editline1 :-
   setup_done, % Check if setup has already been done.
   !. % Cut to prevent further execution if setup_done is true.

% If setup_done is not true, assert it and continue with the installation process.
install_readline_editline1 :-
   asserta(setup_done). % Assert that setup is now complete.

% previously: Various other initialization tasks were included here, but they have been commented out as overkill.
%   '$toplevel':(
%    '$clean_history', % Clear command history.
%    apple_setup_app, % Initialize Apple-specific application setup.
%    '$run_initialization', % Run system initialization tasks.
%    '$load_system_init_file', % Load system initialization files.
%    set_toplevel, % Set the top-level predicate.
%    '$set_file_search_paths', % Define search paths for files.
%    init_debug_flags, % Initialize debugging flags.
%    start_pldoc, % Start the Prolog documentation server (pldoc).
%    opt_attach_packs, % Attach optional packs (libraries).
%    load_init_file, % Load the user initialization file.
%    catch(setup_backtrace, E1, print_message(warning, E1)), % Setup backtrace handling, catching errors.
%    %catch(setup_readline,  E2, print_message(warning, E2)), % Setup readline, previously caught and skipped.
%    %catch(setup_history,   E3, print_message(warning, E3)), % Setup history management, previously skipped.
%    catch(setup_colors, E4, print_message(warning, E4))), % Setup color scheme, catching any errors.
%   install_readline(Input). % Main installation of readline for Input stream.


%!  print_help is det.
%
%   Prints the help message for available debugger commands.
%
%   This message includes a list of all recognized commands that a user can issue in the debugger.
%
%   @example To view the help message:
%
%       ?- print_help.
%
print_debug_help :-
    % Print each available debugger command with its description.
    writeln('Debugger commands:'),
    writeln('(;)  next             - Retry with next solution.'),
    writeln('(g)  goal             - Show the current goal.'),
    %writeln('(u)  up               - Finish this goal without interruption.'),
    writeln('(s)  skip             - Skip to the next solution.'),
    writeln('(c)  creep or <space> - Proceed step by step.'),
    writeln('(l)  leap             - Leap over (the debugging).'),
    %writeln('(f)  fail             - Force the current goal to fail.'),
    %writeln('(B)  back             - Go back to the previous step.'),
    writeln('(t)  trace            - Toggle tracing on or off.'),
    writeln('(e)  exit             - Exit the debugger.'),
    writeln('(a)  abort            - Abort the current operation.'),
    writeln('(b)  break            - Break to a new sub-REPL.'),
    writeln('(?)  help             - Display this help message.'),
    %writeln('(A)  alternatives     - Show alternative solutions.'),
    writeln('(m)  make             - Recompile/Update the current running code.'),
    %writeln('(C)  compile          - Compile a fresh executable (based on the running state).'),
    %writeln('(E)  error msg        - Show the latest error messages.'),
    %writeln('(r)  retry            - Retry the previous command.'),
    %writeln('(I)  info             - Show information about the current state.'),
    !.

:- find_missing_cuts.

