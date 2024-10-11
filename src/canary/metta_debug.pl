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

:- dynamic(is_cached_call/3).

%% cached_call(+ForSeconds, :Call) is nondet.
% Attempts to use cached results for Call, or executes Call if no valid cache is present.
% ForSeconds - Expire after so many seconds
% Call - The Goal that is cached
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


%% debugging_metta(+G) is nondet.
% Debugging utility for metta-related goals.
debugging_metta(G) :- notrace((is_debugging((eval)) -> ignore(G); true)).


:- nodebug(metta(eval)). % Ensure no debugging for metta(eval).

%% depth_to_use(+InDepth, -UseThis) is det.
% Determine a depth value to use, based on a modulo operation.
% InDepth - The input depth.
% UseThis - The depth to actually use, calculated by modulo 50.
depth_to_use(InDepth, UseThis) :-
    Depth is abs(InDepth), % Ensure the depth is non-negative.
    UseThis is Depth mod 50, % Calculate modulo 50.
    !. % Cut to prevent backtracking.
depth_to_use(_InDepth, 5). % Default to depth 5 if other cases fail.


%% w_indent(+Depth, :Goal) is det.
% Execute a goal with indentation based on depth.
% Depth - The depth to determine indentation.
% Goal - The goal to execute with indentation.
w_indent(Depth, Goal) :-
    must_det_ll((
        depth_to_use(Depth, UseThis), % Determine the depth to use.
        format('~N'), % Start a new line.
        setup_call_cleanup(i_this(UseThis), Goal, format('~N')) % Execute the goal with indentation.
    )).

%% i_this(+UseThis) is det.
% Helper predicate to create indentation based on depth.
i_this(UseThis) :-
    ignore(catch(forall(between(1, UseThis, _), write('  ')), _, true)), % Write indentation spaces.
    write(';;'). % End with a delimiter.

%% indentq2(+Depth, +Term) is det.
% Print a term with indentation based on depth.
% Depth - The depth for indentation.
% Term - The term to print.
indentq2(Depth, Term) :-
    w_indent(Depth, format('~q', [Term])), % Print the term with indentation.
    !.
indentq2(_Depth, Term) :-
    format('~q', [Term]). % Fallback printing without indentation.

%% print_padded(+EX, +DR, +AR) is det.
% Print a padded line with extra formatting, if certain conditions are met.
% EX - The EX component for padding.
% DR - The DR component for padding.
% AR - The AR component to print.
print_padded(_DR, _EX, _AR) :- is_fast_mode, !. % Skip printing in fast mode.
print_padded(EX, DR, AR) :-
    integer(EX), integer(DR), EX > 0, DR > 0,
    nb_current('$print_padded', print_padded(EX, DR, _)), % Check if padding is active.
    !,
    format("~|          |", []), % Print the initial padding.
    DRA is abs(round(DR) mod 24), % Calculate padding size.
    forall(between(2, DRA, _), write('   |')), % Write additional padding.
    write('    '), write(' '), write(AR). % Write the AR value.
print_padded(EX, DR, AR) :-
    format("~|~` t~d~5+:~d~5+|", [EX, DR]), % Print padded EX and DR values.
    nb_setval('$print_padded', print_padded(EX, DR, AR)), % Set the current padding.
    DRA is abs(round(DR) mod 24), % Calculate padding size.
    forall(between(1, DRA, _), write('   |')), % Write additional padding.
    write('-'), write(AR). % Write the AR value.

%% indentq_d(+Depth, +Prefix4, +Message) is det.
% Print a message with depth-based indentation and prefix.
% Depth - The depth for indentation.
% Prefix4 - The prefix to include.
% Message - The message to print.
indentq_d(_DR, _EX, _AR) :- is_fast_mode, !. % Skip printing in fast mode.
indentq_d(Depth, Prefix4, Message) :-
    flag(eval_num, EX0, EX0),
    EX is EX0 mod 500,
    DR is 99 - (Depth mod 100),
    indentq(DR, EX, Prefix4, Message). % Call indentq with the formatted values.

%% indentq(+DR, +EX, +AR, +Term) is det.
% Print a term with depth and EX-based indentation.
% DR - Depth for indentation.
% EX - EX component for formatting.
% AR - AR component for formatting.
% Term - The term to print.
indentq(_DR, _EX, _AR, _Term) :- is_fast_mode, !. % Skip printing in fast mode.
indentq(DR, EX, AR, retval(Term)) :-
    nonvar(Term), !,
    indentq(DR, EX, AR, Term). % Handle return values specially.
indentq(DR, EX, AR, [E, Term]) :-
    E == e, !,
    indentq(DR, EX, AR, Term). % Special case for list elements.
%indentq(_DR,_EX,_AR,_Term):- flag(trace_output_len,X,X+1), XX is (X mod 1000), XX>100,!.
indentq(DR, EX, AR, ste(S, Term, E)) :- !,
    indentq(DR, EX, AR, S, Term, E). % Special case for structured terms.
indentq(DR, EX, AR, Term) :-
    indentq(DR, EX, AR, '', Term, ''). % Default case with empty prefix/suffix.

%% indentq(+DR, +EX, +AR, +S, +Term, +E) is det.
% Print a term with depth-based indentation, including start and end strings.
% DR - Depth for indentation.
% EX - EX component for formatting.
% AR - AR component for formatting.
% S - Start string.
% Term - The term to print.
% E - End string.
indentq(DR, EX, AR, S, Term, E) :-
    setup_call_cleanup(
        notrace(format('~N;')),
        (
            wots(Str, indentq0(DR, EX, AR, S, Term, E)), % Format the term.
            newlines_to_spaces(Str, SStr), % Convert newlines to spaces.
            write(SStr) % Write the formatted string.
        ),
        notrace(format('~N')) % End with a newline.
    ).

%% newlines_to_spaces(+Str, -SStr) is det.
% Convert newlines in a string to spaces.
% Str - Input string with newlines.
% SStr - Output string with spaces.
newlines_to_spaces(Str, SStr) :-
    atomics_to_string(L, '\n', Str), % Split the string by newlines.
    atomics_to_string(L, ' ', SStr). % Join the parts with spaces.

%% indentq0(+DR, +EX, +AR, +S, +Term, +E) is det.
% Print a term with padding and depth-based indentation.
% DR - Depth for indentation.
% EX - EX component for formatting.
% AR - AR component for formatting.
% S - Start string.
% Term - The term to print.
% E - End string.
indentq0(DR, EX, AR, S, Term, E) :-
    as_trace((
        print_padded(EX, DR, AR), % Print the padded line.
        format(S, []), % Print the start string.
        with_indents(false, write_src(Term)), % Print the term.
        format(E, []) % Print the end string.
    )).

%% reset_eval_num is det.
% Reset evaluation-related flags.
reset_eval_num :-
    flag(eval_num, _, 0), % Reset eval_num flag.
    flag(trace_output_len, _, 0). % Reset trace_output_len flag.

%% reset_only_eval_num is det.
% Reset only the eval_num flag.
reset_only_eval_num :-
    flag(eval_num, _, 0). % Reset eval_num flag.

%% is_fast_mode is semidet.
% Check if the system is in fast mode.
is_fast_mode :- fail, \+ is_debugging(eval), !.

%% ignore_trace_once(:Goal) is nondet.
% Ignore trace for a single execution of a goal.
% Goal - The goal to execute.
ignore_trace_once(Goal) :- ignore(notrace(catch(ignore(Goal), _, fail))), !.
%ignore_trace_once(Goal):- must_det_ll(Goal).

%% as_trace(:Goal) is nondet.
% Execute a goal while suppressing trace output.
% Goal - The goal to execute.
as_trace(Goal) :-
    ignore_trace_once(\+ with_no_screen_wrap(color_g_mesg('#2f2f2f', Goal))).

%% with_no_screen_wrap(:Goal) is nondet.
% Execute a goal without screen wrapping.
% Goal - The goal to execute.
with_no_screen_wrap(Goal) :- !, call(Goal).
with_no_screen_wrap(Goal) :- with_no_wrap(6000, Goal).

%% with_no_wrap(+Cols, :Goal) is nondet.
% Execute a goal with a specific number of columns, without wrapping.
% Cols - Number of columns to use.
% Goal - The goal to execute.
with_no_wrap(Cols, Goal) :-
    % Setup: Save current terminal settings and disable line wrapping
    setup_call_cleanup(
        begin_no_wrap(Cols, OriginalCols, OriginalRows), % Begin no-wrap mode.
        Goal, % Execute the goal.
        end_no_wrap(OriginalCols, OriginalRows) % Restore original settings.
    ).

%% begin_no_wrap(+Cols, -OriginalCols, -OriginalRows) is det.
% Begin no-wrap mode by setting terminal size.
% Cols - Desired number of columns.
% OriginalCols - Original number of columns.
% OriginalRows - Original number of rows.
begin_no_wrap(Cols, OriginalCols, OriginalRows) :-
    cached_call(30.0, get_current_terminal_settings(OriginalCols, OriginalRows)), % Get current terminal settings.
    set_terminal_size(Cols, OriginalRows), % Set the new terminal size.
    format('~s', ["\e[?7l"]). % Disable line wrapping.

%% end_no_wrap(+OriginalCols, +OriginalRows) is det.
% End no-wrap mode by restoring terminal size.
% OriginalCols - Original number of columns.
% OriginalRows - Original number of rows.
end_no_wrap(OriginalCols, OriginalRows) :-
    set_terminal_size(OriginalCols, OriginalRows), % Restore original terminal size.
    format('~s', ["\e[?7h"]). % Re-enable line wrapping.

%% get_current_terminal_settings(-Cols, -Rows) is det.
% Get the current terminal size.
% Cols - Number of columns.
% Rows - Number of rows.
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

%% set_terminal_size(+Cols, +Rows) is det.
% Set the terminal size (conceptual, may not work in all terminals).
% Cols - Number of columns.
% Rows - Number of rows.
set_terminal_size(Cols, Rows) :-
    % Conceptual; actual resizing may not work in all terminals
    if_t(integer(Cols),
         if_t(integer(Rows), format('~s~w;~w~s', ["\e[8;", Rows, Cols, "t"]))).

%% with_debug(+Flag, :Goal) is nondet.
% Execute a goal with debugging enabled based on a flag.
% Flag - Debugging flag.
% Goal - The goal to execute.
with_debug(Flag, Goal) :-
    is_debugging(Flag),
    !,
    call(Goal).
with_debug(Flag, Goal) :-
    reset_only_eval_num,
    setup_call_cleanup(set_debug(Flag, true), call(Goal), set_debug(Flag, false)).


%!  is_nodebug is semidet.
%
%   Checks if the 'nodebug' option is set to true.
%   This predicate succeeds if the option `nodebug` is set to true, 
%   otherwise it fails.
%
%   @example
%     ?- is_nodebug.
%     true.
%
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
            with_option(debug, silent,
                with_option(e, silent,
                    with_option(eval, true,
                        with_option(exec, noskip, call(Goal))))))).



%% flag_to_var(+Flag, -Var) is det.
% Convert a debugging flag to a variable name.
% Flag - The debugging flag.
% Var - The resulting variable name.
flag_to_var(Flag, Var) :- atom(Flag), \+ atom_concat('trace-on-', _, Flag), !, atom_concat('trace-on-', Flag, Var).
flag_to_var(metta(Flag), Var) :- !, nonvar(Flag), flag_to_var(Flag, Var).
flag_to_var(Flag, Var) :- Flag = Var.

%% set_debug(+Flag, +TF) is det.
% Set debugging on or off based on a flag.
% Flag - The debugging flag.
% TF - Boolean flag for true/false.
set_debug(metta(Flag), TF) :- nonvar(Flag), !, set_debug(Flag, TF).
%set_debug(Flag,Val):- \+ atom(Flag), flag_to_var(Flag,Var), atom(Var),!,set_debug(Var,Val).


set_debug(Flag, TF) :- TF == 'True', !, set_debug(Flag, true).
set_debug(Flag, TF) :- TF == 'False', !, set_debug(Flag, false).
set_debug(Flag, true) :- !, debug(metta(Flag)). %, flag_to_var(Flag, Var), set_fast_option_value(Var, true).
set_debug(Flag, false) :- nodebug(metta(Flag)). %, flag_to_var(Flag, Var), set_fast_option_value(Var, false).

%% if_trace(+Flag, :Goal) is nondet.
% Conditionally execute a goal if tracing is enabled for the flag.
% Flag - The tracing flag.
% Goal - The goal to execute.
if_trace(Flag, Goal) :-
    notrace(real_notrace((catch_err(ignore((is_debugging(Flag), Goal)), E,
        fbug(E --> if_trace(Flag, Goal)))))).


%% is_showing(+Flag) is semidet.
% Check if showing is enabled for a flag.
% Flag - The flag to check.
is_showing(_) :- is_nodebug, !, fail.
is_showing(Flag) :- fast_option_value(Flag, 'silent'), !, fail.
is_showing(Flag) :- is_verbose(Flag), !.
is_showing(Flag) :- fast_option_value(Flag, 'show'), !.

%% if_show(+Flag, :Goal) is nondet.
% Conditionally execute a goal if showing is enabled for the flag.
% Flag - The showing flag.
% Goal - The goal to execute.
if_show(Flag, Goal) :-
    real_notrace((catch_err(ignore((is_showing(Flag), Goal)), E,
                        fbug(E --> if_show(Flag, Goal))))).


%% fast_option_value(+N, -V) is semidet.
% Get the value of a fast option.
% N - Option name.
% V - Option value.
fast_option_value(N, V) :- atom(N), current_prolog_flag(N, V).

%% is_verbose(+Flag) is semidet.
% Check if verbose mode is enabled for a flag.
% Flag - The flag to check.
is_verbose(_) :- is_nodebug, !, fail.
is_verbose(Flag) :- fast_option_value(Flag, 'silent'), !, fail.
is_verbose(Flag) :- fast_option_value(Flag, 'verbose'), !.
is_verbose(Flag) :- is_debugging(Flag), !.

%% if_verbose(+Flag, :Goal) is nondet.
% Conditionally execute a goal if verbose mode is enabled for the flag.
% Flag - The verbose flag.
% Goal - The goal to execute.
if_verbose(Flag, Goal) :-
    real_notrace((catch_err(ignore((is_verbose(Flag), Goal)), E,
                            fbug(E --> if_verbose(Flag, Goal))))).

%% maybe_efbug(+SS, :G) is nondet.
% Execute a goal and potentially report it as an efbug.
% SS - The string to report.
% G - The goal to execute.

%maybe_efbug(SS,G):- efbug(SS,G)*-> if_trace(eval,fbug(SS=G)) ; fail.
maybe_efbug(_, G) :- call(G).

%% efbug(+_, :G) is nondet.
% Execute a goal, suppressing trace errors.
% _ - Ignored parameter.
% G - The goal to execute.

%efbug(P1,G):- call(P1,G).
efbug(_, G) :- call(G).

%% is_debugging_always(+_Flag) is semidet.
% Always return true for debugging, used as a placeholder.
is_debugging_always(_) :- is_nodebug, !, fail.
is_debugging_always(_Flag) :- !.

%% is_debugging(+Flag) is semidet.
% Check if debugging is enabled for a flag.
% Flag - The flag to check.

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
%is_debugging(Flag):- debugging(Flag,TF),!,TF==true.
%is_debugging(Flag):- once(flag_to_var(Flag,Var)),
%  (fast_option_value(Var,true)->true;(Flag\==Var -> is_debugging(Var))).

% overflow = trace
% overflow = fail
% overflow = continue
% overflow = debug

%% trace_eval(:P4, +TNT, +D1, +Self, +X, +Y) is det.
% Perform trace evaluation of a goal, managing trace output and depth.
% P4 - The predicate to call.
% TNT - The trace name/type.
% D1 - The current depth.
% Self - The self-referential term.
% X - Input term.
% Y - Output term.
trace_eval(P4, TNT, D1, Self, X, Y) :-
    must_det_ll((
        notrace((
            flag(eval_num, EX0, EX0 + 1), % Increment eval_num flag.
            EX is EX0 mod 500, % Calculate EX modulo 500.
            DR is 99 - (D1 mod 100), % Calculate DR based on depth.
            PrintRet = _, % Initialize PrintRet.
            option_else('trace-length', Max, 500), % Get trace-length option.
            option_else('trace-depth', DMax, 30) % Get trace-depth option.
        )),
        quietly((if_t((nop(stop_rtrace), EX > Max), (set_debug(eval, false), MaxP1 is Max + 1,
         %set_debug(overflow,false),
            nop(format('; Switched off tracing. For a longer trace: !(pragma! trace-length ~w)', [MaxP1])),
            nop((start_rtrace, rtrace)))))),
        nop(notrace(no_repeats_var(NoRepeats))))),

        ((sub_term(TN, TNT), TNT \= TN) -> true ; TNT = TN), % Ensure proper subterm handling.
   %if_t(DR<DMax, )
        ( \+ \+ if_trace((eval; TNT), (PrintRet = 1,
            indentq(DR, EX, '-->', [TN, X]))) ),

        Ret = retval(fail), !,

        (Display = ( \+ \+ (flag(eval_num, EX1, EX1 + 1),
                ((Ret \=@= retval(fail), nonvar(Y))
                -> indentq(DR, EX1, '<--', [TN, Y])
                ; indentq(DR, EX1, '<--', [TN, Ret]))))),

        call_cleanup((
            (call(P4, D1, Self, X, Y) *-> nb_setarg(1, Ret, Y);
            (fail, trace, (call(P4, D1, Self, X, Y)))),
            ignore((notrace(( \+ (Y \= NoRepeats), nb_setarg(1, Ret, Y)))))),
    % cleanup
        ignore((PrintRet == 1 -> ignore(Display) ;
       (notrace(ignore((( % Y\=@=X,
         if_t(DR<DMax,if_trace((eval;TN),ignore(Display))))))))))),
        Ret \=@= retval(fail).

%  (Ret\=@=retval(fail)->true;(fail,trace,(call(P4,D1,Self,X,Y)),fail)).



:- set_prolog_flag(expect_pfc_file, unknown).

% =======================================================
/*
%
%= predicates to examine the state of pfc
% interactively exploring Pfc justifications.
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
% =======================================================
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/mpred/pfc_list_triggers.pl
:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
pfc_listing_module :- nop(module(pfc_listing,
          [ draw_line/0,
            loop_check_just/1,
            pinfo/1,
            pp_items/2,
            pp_item/2,
            pp_filtered/1,
            pp_facts/2,
            pp_facts/1,
            pp_facts/0,
            pfc_list_triggers_types/1,
            pfc_list_triggers_nlc/1,
            pfc_list_triggers_1/1,
            pfc_list_triggers_0/1,
            pfc_list_triggers/1,
            pfc_contains_term/2,
            pfc_classify_facts/4,
            lqu/0,
            get_clause_vars_for_print/2,
            %pfcWhyBrouse/2,
            %pfcWhy1/1,
            %pfcWhy/1,
            %pfcWhy/0,
            pp_rules/0,
            pfcPrintSupports/0,
            pfcPrintTriggers/0,
            print_db_items/1,
            print_db_items/2,
            print_db_items/3,
            print_db_items/4,
            print_db_items_and_neg/3,
            show_pred_info/1,
            show_pred_info_0/1,
            pfc_listing_file/0
          ])).

%:- include('pfc_header.pi').

:- endif.

% Operator declarations
:- op(500, fx, '~').
:- op(1050, xfx, ('==>')).
:- op(1050, xfx, '<==>').
    :- op(1050,xfx,('<-')).
    :- op(1100,fx,('==>')).
:- op(1150, xfx, ('::::')).

% :- use_module(logicmoo(util/logicmoo_util_preddefs)).

:- multifile((
              user:portray/1,
              user:prolog_list_goal/1,
              user:prolog_predicate_name/2,
              user:prolog_clause_name/2)).

:- dynamic user:portray/1.

%:- dynamic(whybuffer/2).

%% lqu is semidet.
% Lists all clauses of the predicate que/2.
lqu :- listing(que/2).

:- ensure_loaded(metta_pfc_base).

%   File   : pfcdebug.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author : Dave Matuszek, dave@prc.unisys.com
%   Author : Douglas R. Miles, dmiles@teknowledge.com
%   Updated:
%   Purpose: provides predicates for examining the database and debugging
%   for Pfc.

:- dynamic pfcTraced/1.
:- dynamic pfcSpied/2.
:- dynamic pfcTraceExecution/0.
:- dynamic pfcWarnings/1.

:- pfcDefault(pfcWarnings(_), pfcWarnings(true)).

%% pfcQueue is semidet.
% Lists all clauses of the predicate pfcQueue/1.
pfcQueue :- listing(pfcQueue/1).

%% pfcPrintDB is semidet.
% Prints the entire Pfc database, including facts, rules, triggers, and supports.
pfcPrintDB :-
  pfcPrintFacts,
  pfcPrintRules,
  pfcPrintTriggers,
  pfcPrintSupports,!.

%% printLine is semidet.
% Draws a line in the console output for formatting purposes.
printLine :- ansi_format([underline], "~N=========================================~n", []).

%% pfcPrintFacts is semidet.
% Prints all facts in the Pfc database.
pfcPrintFacts :- pfcPrintFacts(_, true).

%% pfcPrintFacts(+Pattern) is semidet.
% Prints all facts in the Pfc database that match a given pattern.
% Pattern - The pattern to match facts against.
pfcPrintFacts(Pattern) :- pfcPrintFacts(Pattern, true).

%% pfcPrintFacts(+Pattern, +Condition) is semidet.
% Prints all facts in the Pfc database that match a given pattern and condition.
% Pattern - The pattern to match facts against.
% Condition - The condition to filter facts.
pfcPrintFacts(P, C) :-
  pfcFacts(P, C, L),
  pfcClassifyFacts(L, User, Pfc, _Rule),
  printLine,
  pfcPrintf("User added facts:~n", []),
  pfcPrintitems(User),
  printLine,
  pfcPrintf("MettaLog-Pfc added facts:~n", []),
  pfcPrintitems(Pfc),
  printLine,!.

%% pfcPrintitems(+List) is det.
% Prints a list of items. Note that this predicate clobbers its arguments, so beware.
% List - The list of items to print.
pfcPrintitems([]).
pfcPrintitems([H|T]) :-
  % numbervars(H,0,_),
  %format('~N ~p.',[H]),
  \+ \+ ( pretty_numbervars(H, H1), format(" ", []), portray_clause_w_vars(H1)),
  pfcPrintitems(T).

%% pfcClassifyFacts(+Facts, -UserFacts, -PfcFacts, -RuleFacts) is det.
% Classifies a list of facts into user-added facts, Pfc-added facts, and rule facts.
% Facts - The list of facts to classify.
% UserFacts - The list of user-added facts.
% PfcFacts - The list of Pfc-added facts.
% RuleFacts - The list of rule facts.
pfcClassifyFacts([], [], [], []).

pfcClassifyFacts([H|T], User, Pfc, [H|Rule]) :-
  pfcType(H, rule),
  !,
  pfcClassifyFacts(T, User, Pfc, Rule).

pfcClassifyFacts([H|T], [H|User], Pfc, Rule) :-
  matches_why_UU(UU),
  pfcGetSupport(H, UU),
  !,
  pfcClassifyFacts(T, User, Pfc, Rule).

pfcClassifyFacts([H|T], User, [H|Pfc], Rule) :-
  pfcClassifyFacts(T, User, Pfc, Rule).

%% pfcPrintRules is semidet.
% Prints all rules in the Pfc database.
pfcPrintRules :-
  printLine,
  pfcPrintf("Rules:...~n", []),
  bagof_or_nil((P==>Q), clause((P==>Q), true), R1),
  pfcPrintitems(R1),
  bagof_or_nil((P<==>Q), clause((P<==>Q), true), R2),
  pfcPrintitems(R2),
  bagof_or_nil((P<-Q), clause((P<-Q), true), R3),
  pfcPrintitems(R3),
  printLine.

%% pfcGetTrigger(-Trigger) is nondet.
% Retrieves a trigger from the Pfc database.
% Trigger - The retrieved trigger.
pfcGetTrigger(Trigger) :- pfc_call(Trigger).

%% pfcPrintTriggers is semidet.
% Pretty prints all triggers in the Pfc database.
pfcPrintTriggers :-
  print_db_items("Positive triggers", '$pt$'(_, _)),
  print_db_items("Negative triggers", '$nt$'(_, _, _)),
  print_db_items("Goal triggers", '$bt$'(_, _)).

pp_triggers :- pfcPrintTriggers.

%% pfcPrintSupports is semidet.
% Pretty prints all supports in the Pfc database.
pfcPrintSupports :-
  % temporary hack.
  draw_line,
  fmt("Supports ...~n", []),
  setof_or_nil((P =< S), (pfcGetSupport(P, S), \+ pp_filtered(P)), L),
  pp_items('Support', L),
  draw_line,!.
pp_supports :- pfcPrintSupports.

%% pp_filtered(+Predicate) is semidet.
% Checks if a predicate should be filtered out from pretty-printing.
% Predicate - The predicate to check.
pp_filtered(P) :- var(P),!,fail.
pp_filtered(_:P) :- !, pp_filtered(P).
pp_filtered(P) :- safe_functor(P, F, A), F\==(/),!, pp_filtered(F/A).
pp_filtered(F/_) :- F==pfc_prop.

%% pfcFact(+Predicate) is semidet.
% Checks if a fact was asserted into the database via pfcAdd.
% Predicate - The fact to check.
pfcFact(P) :- pfcFact(P, true).

%% pfcFact(+Predicate, +Condition) is semidet.
% Checks if a fact was asserted into the database via pfcAdd and a condition is satisfied.
% Predicate - The fact to check.
% Condition - The condition to check.
% Example:   pfcFact(X,pfcUserFact(X))
pfcFact(F, C) :-
  filter_to_pattern_call(F, P, Call),
  pfcFact1(P, C),
  pfcCallSystem(Call).

%% pfcFact1(+Predicate, +Condition) is semidet.
% Helper predicate for pfcFact/2.
% Predicate - The fact to check.
% Condition - The condition to check.
pfcFact1(P, C) :-
  pfcGetSupport(P, _),
  pfcType(P, fact(_)),
  pfcCallSystem(C).

%% pfcFacts(-ListofPfcFacts) is det.
% Returns a list of facts added to the Pfc database.
% ListofPfcFacts - The list of facts.
pfcFacts(L) :- pfcFacts(_, true, L).

%% pfcFacts(+Pattern, -ListofPfcFacts) is det.
% Returns a list of facts added to the Pfc database that match a given pattern.
% Pattern - The pattern to match facts against.
% ListofPfcFacts - The list of facts.
pfcFacts(P, L) :- pfcFacts(P, true, L).

%% pfcFacts(+Pattern, +Condition, -ListofPfcFacts) is det.
% Returns a list of facts added to the Pfc database that match a given pattern and condition.
% Pattern - The pattern to match facts against.
% Condition - The condition to filter facts.
% ListofPfcFacts - The list of facts.
pfcFacts(P, C, L) :- setof_or_nil(P, pfcFact(P, C), L).

%% brake(+Predicate) is det.
% Calls a system predicate and breaks execution.
% Predicate - The predicate to call.
brake(X) :- pfcCallSystem(X), ibreak.

%% pfcTraceAdd(+Predicate) is det.
% Adds a predicate to the Pfc trace.
% Predicate - The predicate to trace.
pfcTraceAdd(P) :-
  % this is here for upward compat. - should go away eventually.
  pfcTraceAdd(P, (o, o)).

%% pfcTraceAdd(+Trigger, +Support) is det.
% Adds a trigger and its support to the Pfc trace.
% Trigger - The trigger to trace.
% Support - The support of the trigger.
pfcTraceAdd('$pt$'(_, _), _) :- !. % Never trace positive triggers.
pfcTraceAdd('$nt$'(_, _), _) :- !. % Never trace negative triggers.

pfcTraceAdd(P, S) :-
  pfcTraceAddPrint(P, S),
  pfcTraceBreak(P, S).

%% pfcTraceAddPrint(+Predicate, +Support) is det.
% Prints a predicate being added to the Pfc trace.
% Predicate - The predicate to print.
% Support - The support of the predicate.
pfcTraceAddPrint(P, S) :-
  pfcIsTraced(P),
  !,
  \+ \+ (pretty_numbervars(P, Pcopy),
      % numbervars(Pcopy,0,_),
      matches_why_UU(UU),
      (S=UU
        -> pfcPrintf("Adding (u) ~@", [fmt_cl(Pcopy)])
        ; pfcPrintf("Adding ~@", [fmt_cl(Pcopy)]))).

pfcTraceAddPrint(_, _).

%% pfcTraceBreak(+Predicate, +Support) is det.
% Breaks execution if a predicate is spied in the Pfc trace.
% Predicate - The predicate to check.
% Support - The support of the predicate.
pfcTraceBreak(P, _S) :-
  pfcSpied(P, +) ->
   (pretty_numbervars(P, Pcopy),
    % numbervars(Pcopy,0,_),
    pfcPrintf("Breaking on pfcAdd(~p)", [Pcopy]),
    ibreak)
   ; true.

%% pfcTraceRem(+Trigger) is det.
% Removes a trigger from the Pfc trace.
% Trigger - The trigger to remove.
pfcTraceRem('$pt$'(_, _)) :- !. % Never trace positive triggers.
pfcTraceRem('$nt$'(_, _)) :- !. % Never trace negative triggers.

pfcTraceRem(P) :-
  (pfcIsTraced(P)
     -> pfcPrintf("Removing: ~p.", [P])
      ; true),
  (pfcSpied(P, -)
   -> (pfcPrintf("Breaking on pfcRem(~p)", [P]),
       ibreak)
   ; true).

%% pfcIsTraced(+Predicate) is semidet.
% Checks if a predicate is being traced.
% Predicate - The predicate to check.
pfcIsTraced(P) :- pfcIsNotTraced(P),!,fail.
pfcIsTraced(P) :- compound_eles(1, P, Arg), pfcTraced(Arg).

%% pfcIsNotTraced(+Predicate) is semidet.
% Checks if a predicate is not being traced.
% Predicate - The predicate to check.
pfcIsNotTraced(P) :- compound_eles(1, P, Arg), pfcIgnored(Arg).

:- dynamic(pfcIgnored/1).

%% compound_eles(+Level, +Compound, -Element) is det.
% Extracts elements from a compound term.
% Level - The level of extraction.
% Compound - The compound term.
% Element - The extracted element.
compound_eles(Lvl, P, Arg) :- var(P),!, get_attr(P, A, AV), compound_eles(Lvl, attvar(A, AV), Arg).
compound_eles(Lvl, P, Arg) :- (\+ compound(P); Lvl<1),!, Arg=P.
compound_eles(Lvl, P, Arg) :- LvlM1 is Lvl-1, compound_eles(P, E), compound_eles(LvlM1, E, Arg).

compound_eles(P, E) :- is_list(P),!, member(E, P).
compound_eles(P, E) :- compound(P), compound_name_arguments(P, F, Args),!, member(E, [F|Args]).

%% mpred_trace_exec is det.
% Enables tracing and watching in Pfc.
mpred_trace_exec :- pfcWatch, pfcTrace.

%% mpred_notrace_exec is det.
% Disables tracing and watching in Pfc.
mpred_notrace_exec :- pfcNoTrace, pfcNoWatch.
%% pfcTrace is det.
% Enables tracing in Pfc.
pfcTrace :- pfcTrace(_).

%% pfcTrace(+Form) is det.
% Enables tracing for a specific form in Pfc.
% Form - The form to trace.
pfcTrace(Form) :-
  assert(pfcTraced(Form)).

%% pfcTrace(+Form, +Condition) is det.
% Enables tracing for a specific form under a given condition in Pfc.
% Form - The form to trace.
% Condition - The condition under which to trace the form.
pfcTrace(Form, Condition) :-
  assert((pfcTraced(Form) :- Condition)).

%% pfcSpy(+Form) is det.
% Adds a form to the Pfc spy list.
% Form - The form to spy on.
pfcSpy(Form) :- pfcSpy(Form, [+,-], true).

%% pfcSpy(+Form, +Modes) is det.
% Adds a form to the Pfc spy list with specific modes.
% Form - The form to spy on.
% Modes - The modes to use for spying.
pfcSpy(Form, Modes) :- pfcSpy(Form, Modes, true).

%% pfcSpy(+Form, +Modes, +Condition) is det.
% Adds a form to the Pfc spy list with specific modes and a condition.
% Form - The form to spy on.
% Modes - The modes to use for spying.
% Condition - The condition under which to spy the form.
pfcSpy(Form, [H|T], Condition) :-
  !,
  pfcSpy1(Form, H, Condition),
  pfcSpy(Form, T, Condition).

pfcSpy(Form, Mode, Condition) :-
  pfcSpy1(Form, Mode, Condition).

%% pfcSpy1(+Form, +Mode, +Condition) is det.
% Helper predicate for pfcSpy/3.
% Form - The form to spy on.
% Mode - The mode to use for spying.
% Condition - The condition under which to spy the form.
pfcSpy1(Form, Mode, Condition) :-
  assert((pfcSpied(Form, Mode) :- Condition)).

%% pfcNospy is det.
% Removes all forms from the Pfc spy list.
pfcNospy :- pfcNospy(_,_,_).

%% pfcNospy(+Form) is det.
% Removes a specific form from the Pfc spy list.
% Form - The form to remove.
pfcNospy(Form) :- pfcNospy(Form,_,_).

%% pfcNospy(+Form, +Mode, +Condition) is det.
% Removes a specific form from the Pfc spy list with a given mode and condition.
% Form - The form to remove.
% Mode - The mode to remove.
% Condition - The condition to remove.
pfcNospy(Form, Mode, Condition) :-
  clause(pfcSpied(Form, Mode), Condition, Ref),
  erase(Ref),
  fail.

pfcNospy(_,_,_).

%% pfcNoTrace is det.
% Disables tracing in Pfc.
pfcNoTrace :- pfcUntrace.

%% pfcUntrace is det.
% Untraces all forms in Pfc.
pfcUntrace :- pfcUntrace(_).

%% pfcUntrace(+Form) is det.
% Untraces a specific form in Pfc.
% Form - The form to untrace.
pfcUntrace(Form) :- retractall(pfcTraced(Form)).

%% pfcTraceMsg(+Message) is det.
% Traces a message in Pfc.
% Message - The message to trace.
pfcTraceMsg(Msg) :- pfcTraceMsg('~p', [Msg]).

%% pfcTraceMsg(+Message, +Arguments) is det.
% Traces a message with arguments in Pfc.
% Message - The message to trace.
% Arguments - The arguments for the message.
pfcTraceMsg(Msg, Args) :-
    pfcTraceExecution,
    !,
    pfcPrintf(user_output, Msg, Args).
pfcTraceMsg(Msg, Args) :-
    member(P, Args), pfcIsTraced(P),
    !,
    pfcPrintf(user_output, Msg, Args).
pfcTraceMsg(_, _).

%% pfcPrintf(+Message, +Arguments) is det.
% Prints a formatted message in Pfc.
% Message - The message to print.
% Arguments - The arguments for the message.
pfcPrintf(Msg, Args) :-
  pfcPrintf(user_output, Msg, Args).

%% pfcPrintf(+Where, +Message, +Arguments) is det.
% Prints a formatted message to a specific location in Pfc.
% Where - The location to print the message.
% Message - The message to print.
% Arguments - The arguments for the message.
pfcPrintf(Where, Msg, Args) :-
  format(Where, '~N', []),
  with_output_to(Where,
    color_g_mesg_ok(blue, format(Msg, Args))).

%% pfcWatch is det.
% Enables execution tracing in Pfc.
pfcWatch :- clause(pfcTraceExecution, true),!.
pfcWatch :- assert(pfcTraceExecution).

%% pfcNoWatch is det.
% Disables execution tracing in Pfc.
pfcNoWatch :- retractall(pfcTraceExecution).

%% pfcError(+Message) is det.
% Prints an error message in Pfc.
% Message - The error message to print.
pfcError(Msg) :- pfcError(Msg, []).

%% pfcError(+Message, +Arguments) is det.
% Prints an error message with arguments in Pfc.
% Message - The error message to print.
% Arguments - The arguments for the message.
pfcError(Msg, Args) :-
  format("~N~nERROR/Pfc: ", []),
  format(Msg, Args).

% %
% %  These control whether or not warnings are printed at all.
% %    pfcWarn.
% %    nopfcWarn.
% %
% %  These print a warning message if the flag pfcWarnings is set.
% %    pfcWarn(+Message)
% %    pfcWarn(+Message,+ListOfArguments)
% %






%% pfcWarn is det.
% Enables warning messages in Pfc.
pfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

%% nopfcWarn is det.
% Disables warning messages in Pfc.
nopfcWarn :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(false)).

%% pfcWarn(+Message) is det.
% Prints a warning message in Pfc.
% Message - The warning message to print.
pfcWarn(Msg) :- pfcWarn('~p', [Msg]).

%% pfcWarn(+Message, +Arguments) is det.
% Prints a warning message with arguments in Pfc.
% Message - The warning message to print.
% Arguments - The arguments for the message.
pfcWarn(Msg, Args) :-
  pfcWarnings(true),
  !,
  ansi_format([underline, fg(red)], "~N==============WARNING/Pfc================~n", []),
  ansi_format([fg(yellow)], Msg, Args),
  printLine.
pfcWarn(_, _).

%% pfcWarnings is det.
% Enables warning messages in Pfc.
% sets flag to cause pfc warning messages to print.
pfcWarnings :-
  retractall(pfcWarnings(_)),
  assert(pfcWarnings(true)).

%% pfcNoWarnings is det.
% Disables warning messages in Pfc.
% sets flag to cause pfc warning messages not to print.
pfcNoWarnings :-
  retractall(pfcWarnings(_)).

%% pp_facts is semidet.
% Pretty prints all facts in the Pfc database.
pp_facts :- pp_facts(_, true).

%% pp_facts(+Pattern) is semidet.
% Pretty prints facts in the Pfc database that match a given pattern.
% Pattern - The pattern to match facts against.
pp_facts(Pattern) :- pp_facts(Pattern, true).

%% pp_facts(+Pattern, +Condition) is semidet.
% Pretty prints facts in the Pfc database that match a given pattern and condition.
% Pattern - The pattern to match facts against.
% Condition - The condition to filter facts.
pp_facts(P, C) :-
  pfcFacts(P, C, L),
  pfc_classify_facts(L, User, Pfc, _Rule),
  draw_line,
  fmt("User added facts:", []),
  pp_items(user, User),
  draw_line,
  draw_line,
  fmt("MettaLog-Pfc added facts:", []),
  pp_items(system, Pfc),
  draw_line.

%% pp_deds is semidet.
% Pretty prints all deduced facts in the Pfc database.
pp_deds :- pp_deds(_, true).

%% pp_deds(+Pattern) is semidet.
% Pretty prints deduced facts in the Pfc database that match a given pattern.
% Pattern - The pattern to match facts against.
pp_deds(Pattern) :- pp_deds(Pattern, true).

%% pp_deds(+Pattern, +Condition) is semidet.
% Pretty prints deduced facts in the Pfc database that match a given pattern and condition.
% Pattern - The pattern to match facts against.
% Condition - The condition to filter facts.
pp_deds(P, C) :-
  pfcFacts(P, C, L),
  pfc_classify_facts(L, _User, Pfc, _Rule),
  draw_line,
  fmt("MettaLog-Pfc added facts:", []),
  pp_items(system, Pfc),
  draw_line.

%% show_deds_w(+Pattern) is semidet.
% Shows deduced facts that match a given pattern.
% Pattern - The pattern to match deduced facts against.
show_deds_w(F) :- pp_deds(F).

%% show_info(+Pattern) is semidet.
% Shows information about facts that match a given pattern.
% Pattern - The pattern to match facts against.
show_info(F) :-
    pfcFacts(_, true, L),
    include(sub_functor(F), L, FL),
    pfc_classify_facts(FL, User, Pfc, _Rule),
    draw_line,
    fmt("User added facts with ~q:", [F]),
    pp_items(user, User),
    draw_line,
    draw_line,
    fmt("MettaLog-Pfc added facts with ~q:", [F]),
    pp_items(system, Pfc),
    draw_line.

%% maybe_filter_to_pattern_call(+Pattern, +Predicate, -Condition) is det.
% Converts a pattern and predicate to a condition for filtering.
% Pattern - The pattern to filter.
% Predicate - The predicate to filter.
% Condition - The resulting condition.
maybe_filter_to_pattern_call(F, _, true) :- var(F), !, fail.
maybe_filter_to_pattern_call(F, P, true) :- atom(F), !, (P = F ; freeze(P, (P \== F, sub_functor(F, P)))).
maybe_filter_to_pattern_call(F, P, true) :- \+ compound(F), !, P = _ ; freeze(P, (P \== F, sub_functor(F, P))).
maybe_filter_to_pattern_call(F/A, P, true) :- !, freeze(P, (P \== F, sub_functor(F/A, P))).
%maybe_filter_to_pattern_call(F,P,true):-P=F.

%% filter_to_pattern_call(+Pattern, +Predicate, -Condition) is det.
% Converts a pattern and predicate to a condition for filtering, with alternative handling.
% Pattern - The pattern to filter.
% Predicate - The predicate to filter.
% Condition - The resulting condition.
filter_to_pattern_call(F, P, Call) :-
   maybe_filter_to_pattern_call(F, P, Call) *-> true; alt_filter_to_pattern_call(F, P, Call).

%% alt_filter_to_pattern_call(+Pattern, +Predicate, -Condition) is det.
% Alternative handling for filter_to_pattern_call/3.
% Pattern - The pattern to filter.
% Predicate - The predicate to filter.
% Condition - The resulting condition.
alt_filter_to_pattern_call(P, P, true).

%% sub_functor(+Functor, +Term) is semidet.
% Checks if a term contains a specific functor.
% Functor - The functor to check for.
% Term - The term to check.
sub_functor(F-UnF, Term) :- !, sub_functor(F, Term), \+ sub_functor(UnF, Term).
sub_functor(F, Term) :- var(F), !, sub_var(F, Term), !.
sub_functor(F/A, Term) :- !, sub_term(E, Term), compound(E), compound_name_arity(E, F, A).
sub_functor(F, Term) :- sub_term(E, Term), E =@= F, !.
sub_functor(F, Term) :- sub_term(E, Term), compound(E), compound_name_arity(E, FF, AA), (AA == F ; FF == F).

%% pp_items(+Type, +Items) is semidet.
% Pretty prints a list of items.
% Type - The type of items.
% Items - The list of items to print.
pp_items(_Type, []) :- !.
pp_items(Type, [H|T]) :-
  ignore(pp_item(Type, H)), !,
  pp_items(Type, T).
pp_items(Type, H) :- ignore(pp_item(Type, H)).

:- thread_local t_l:print_mode/1.

%% pp_item(+Mode, +Item) is semidet.
% Pretty prints a single item.
% Mode - The mode for printing.
% Item - The item to print.
pp_item(_M, H) :- pp_filtered(H), !.
pp_item(MM, (H :- B)) :- B == true, pp_item(MM, H).
pp_item(MM, H) :- flag(show_asserions_offered, X, X+1), find_and_call(get_print_mode(html)), (\+ \+ if_defined(pp_item_html(MM, H))), !.

pp_item(MM, '$spft$'(W0, U, ax)) :- W = (_KB:W0), !, pp_item(MM, U:W).
pp_item(MM, '$spft$'(W0, F, U)) :- W = (_KB:W0), atom(U), !, fmt('~N%~n', []), pp_item(MM, U:W), fmt('rule: ~p~n~n', [F]), !.
pp_item(MM, '$spft$'(W0, F, U)) :- W = (_KB:W0), !, fmt('~w~nd:       ~p~nformat:    ~p~n', [MM, W, F]), pp_item(MM, U).
pp_item(MM, '$nt$'(Trigger0, Test, Body)) :- Trigger = (_KB:Trigger0), !, fmt('~w n-trigger(-): ~p~ntest: ~p~nbody: ~p~n', [MM, Trigger, Test, Body]).
pp_item(MM, '$pt$'(F0, Body)) :- F = (_KB:F0), !, fmt('~w p-trigger(+):~n', [MM]), pp_item('', (F:-Body)).
pp_item(MM, '$bt$'(F0, Body)) :- F = (_KB:F0), !, fmt('~w b-trigger(?):~n', [MM]), pp_item('', (F:-Body)).

pp_item(MM, U:W) :- !, format(string(S), '~w  ~w:', [MM, U]), !, pp_item(S, W).
pp_item(MM, H) :- \+ \+ (get_clause_vars_for_print(H, HH), fmt("~w ~p~N", [MM, HH])).
%% get_clause_vars_for_print(+Clause, -ClauseWithVars) is det.
% Prepares a clause for printing by handling variables.
% Clause - The clause to prepare.
% ClauseWithVars - The clause with variables prepared for printing.
get_clause_vars_for_print(HB, HB) :- ground(HB), !.
get_clause_vars_for_print(I, I) :- is_listing_hidden(skipVarnames), fail.
get_clause_vars_for_print(H0, MHB) :- get_clause_vars_copy(H0, MHB), H0 \=@= MHB, !.
get_clause_vars_for_print(HB, HB) :- numbervars(HB, 0, _, [singletons(true), attvars(skip)]), !.

%% pfc_classify_facts(+Facts, -UserFacts, -PfcFacts, -Rules) is det.
% Classifies facts into user facts, Pfc deductions, and rules.
% Facts - The facts to classify.
% UserFacts - The User Added facts.
% PfcFacts - The System Added facts.
% Rules - Classified as rules
pfc_classify_facts([],[],[],[]).

pfc_classify_facts([H|T],User,Pfc,[H|Rule]) :-
  pfcType(H,rule),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],[H|User],Pfc,Rule) :-
  pfcGetSupport(H,(mfl4(_VarNameZ,_,_,_),ax)),
  !,
  pfc_classify_facts(T,User,Pfc,Rule).

pfc_classify_facts([H|T],User,[H|Pfc],Rule) :-
  pfc_classify_facts(T,User,Pfc,Rule).


%=

% %  print_db_items( ?T, ?I) is semidet.
%
% Print Database Items.
% T - The title or label for the items being printed.
% I - The items or goals to be printed.
%
print_db_items(T, I):-
    draw_line, % Draw a separator line before printing.
    fmt("~N~w ...~n",[T]), % Print the title.
    print_db_items(I), % Print the database items.
    draw_line, % Draw a separator line after printing.
    !.

%=

%%  print_db_items( ?I) is semidet.
%
% Print Database Items.
% I - The predicate or item to be printed.
%
print_db_items(F/A):- 
    number(A),!, % Check if A is a number, ensuring F/A is a valid functor/arity pair.
    safe_functor(P,F,A),!, % Safely create a functor from F and A.
    print_db_items(P). % Print the functor.
print_db_items(H):- 
    bagof(H,clause(H,true),R1), % Collect all clauses matching H into a list R1.
    pp_items((':'),R1), % Pretty print the collected items.
    R1\==[],!. % Succeed if the list is non-empty.
print_db_items(H):- 
    \+ current_predicate(_,H),!. % Succeed if H is not a current predicate.
print_db_items(H):- 
    catch( ('$find_predicate'(H,_),call_u(listing(H))),_,true),!, % Try to list the predicate, catching any errors.
    nl,nl. % Print two newlines after listing.

%=

% %  pp_rules is semidet.
%
% Pretty Print Rules.
% This predicate prints different types of rules and facts in the database.
%
pp_rules :-
   print_db_items("Forward Rules",(_ ==> _)), % Print forward rules.
   print_db_items("Bidirectional Rules",(_ <==> _)), % Print bidirectional rules.
   print_db_items("Implication Rules",=>(_ , _)), % Print implication rules.
   print_db_items("Bi-conditional Rules",<=>(_ , _)), % Print bi-conditional rules.
   print_db_items("Backchaining Rules",(_ <- _)), % Print backchaining rules.
   print_db_items("Positive Facts",(==>(_))), % Print positive facts.
   print_db_items("Negative Facts",(~(_))). % Print negative facts.

%=

% %  draw_line is semidet.
%
% Draw Line.
% This predicate draws a line separator in the console output.
%
draw_line:- 
    \+ thread_self_main,!. % Do nothing if not in the main thread.
draw_line:- printLine,!. % Attempt to use printLine to draw a line.
draw_line:- 
    (t_l:print_mode(H)->true;H=unknown), % Get the current print mode or set to unknown.
    fmt("~N% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %~n",[]), % Draw the line using format.
    H=H.

:- meta_predicate loop_check_just(0).

%=

% %  loop_check_just( :GoalG) is semidet.
%
% Loop Check Justification.
% GoalG - The goal to check for loops.
%
loop_check_just(G):- 
    loop_check(G,ignore(arg(1,G,[]))). % Perform loop check, ignoring goals with an empty first argument.

%=

% %  show_pred_info( ?F) is semidet.
%
% Show Predicate Info.
% PI - The predicate indicator (F/A) for which information is to be shown.
%
show_pred_info(PI):-
   ((
       pi_to_head_l(PI,Head), % Convert predicate indicator to head.
       % doall(show_call(why,call_u(isa(Head,_)))),
        safe_functor(Head,F,_), % Extract the functor from the head.
        doall(show_call(why,call_u(isa(F,_)))), % Show all instances where F is a certain type.
       ((current_predicate(_,M:Head), (\+ predicate_property(M:Head,imported_from(_))))
          -> show_pred_info_0(M:Head); % Show predicate info if not imported.
             wdmsg_pretty(cannot_show_pred_info(Head))))),!. % Display a message if unable to show info.

%=

% %  show_pred_info_0( ?Head) is semidet.
%
% Show Predicate Info Primary Helper.
% Head - The head of the predicate for which information is to be shown.
%
show_pred_info_0(Head):-
        doall(show_call(why,predicate_property(Head,_))), % Show all properties of the predicate.
        (has_cl(Head)->doall((show_call(why,clause(Head,_))));quietly((listing(Head)))),!. % List the predicate clauses or show the listing.

% ===================================================
% Pretty Print Formula
% ===================================================

%=

% %  print_db_items( ?Title, ?Mask, ?What) is semidet.
%
% Print Database Items.
% Title - The title to be printed.
% Mask - The mask or pattern to match.
% What - The items to print.
%
print_db_items(Title,Mask,What):-
    print_db_items(Title,Mask,Mask,What). % Print items with the given title, mask, and what parameters.

%=


%% print_db_items(+Title, +Mask, +Show, +What) is semidet.
% Prints database items based on a mask, show predicate, and a condition.
% Title - The title for the items.
% Mask - The mask to filter items.
% Show - The show predicate for the items.
% What - The condition to filter items.
print_db_items(Title, Mask, Show, What0) :-
     get_pi(Mask, H), get_pi(What0, What),
     format(atom(Showing), '~p for ~p...', [Title, What]),
     statistics(cputime, Now), Max is Now + 2, !,
     gripe_time(1.0,
         doall((once(statistics(cputime, NewNow)), NewNow < Max, clause_or_call(H, B),
             quietly(pfc_contains_term(What, (H:-B))),
             flag(print_db_items, LI, LI+1),
             ignore(quietly(pp_item(Showing, Show)))))),
     ignore(pp_item(Showing, done)),!.

%% pfc_contains_term(+Term, +Inside) is semidet.
% Checks if a term contains another term.
% Term - The term to check.
% Inside - The term to look for inside the term.
pfc_contains_term(What, _) :- is_ftVar(What), !.
pfc_contains_term(What, Inside) :- compound(What), !, (\+ \+ ((copy_term_nat(Inside, Inside0), snumbervars(Inside0), occurs:contains_term(What, Inside0)))), !.
pfc_contains_term(What, Inside) :- (\+ \+ once((subst(Inside, What, foundZadooksy, Diff), Diff \=@= Inside ))), !.

%% hook_pfc_listing(+What) is semidet.
% Hook for Pfc listing.
% What - The condition to filter items.
:- current_prolog_flag(pfc_shared_module, BaseKB),
 assert_if_new((BaseKB:hook_pfc_listing(What) :- on_x_debug(pfc_list_triggers(What)))).

:- thread_local t_l:pfc_list_triggers_disabled/0.
% listing(L):-locally(t_l:pfc_list_triggers_disabled,listing(L)).


%% pfc_list_triggers(+What) is semidet.
% Lists triggers in the Pfc database.
% What - The condition to filter triggers.
pfc_list_triggers(_) :- t_l:pfc_list_triggers_disabled, !.
pfc_list_triggers(What) :- loop_check(pfc_list_triggers_nlc(What)).

%% pfc_list_triggers_nlc(+What) is semidet.
% Lists triggers in the Pfc database (no loop check).
% What - The condition to filter triggers.
:- meta_predicate(pfc_list_triggers_nlc(?)).
pfc_list_triggers_nlc(MM:What) :- atom(MM), !, MM:pfc_list_triggers(What).
pfc_list_triggers_nlc(What) :- loop_check(pfc_list_triggers_0(What), true).

%% pfc_list_triggers_0(+What) is semidet.
% Lists triggers in the Pfc database (primary helper).
% What - The condition to filter triggers.
pfc_list_triggers_0(What) :- get_pi(What, PI), PI \=@= What, pfc_list_triggers(PI).
pfc_list_triggers_0(What) :- nonvar(What), What = ~(Then), !, \+ \+ pfc_list_triggers_1(Then), \+ \+ pfc_list_triggers_1(What).
pfc_list_triggers_0(What) :- \+ \+ pfc_list_triggers_1(~(What)), \+ \+ pfc_list_triggers_1(What).

%% pfc_list_triggers_types(-TriggerType) is semidet.
% Lists trigger types in the Pfc database.
% TriggerType - The trigger type to list.
pfc_list_triggers_types('Triggers').
pfc_list_triggers_types('Instances').
pfc_list_triggers_types('Subclasses').
pfc_list_triggers_types('ArgTypes').
pfc_list_triggers_types('Arity').
pfc_list_triggers_types('Forward').
pfc_list_triggers_types('Bidirectional').
pfc_list_triggers_types('Backchaining').
pfc_list_triggers_types('Negative').
pfc_list_triggers_types('Sources').
pfc_list_triggers_types('Supports').
pfc_list_triggers_types('Edits').

%% print_db_items_and_neg(+Title, +Fact, +What) is semidet.
% Prints database items and their negations.
% Title - The title for the items.
% Fact - The fact to check.
% What - The condition to filter items.
print_db_items_and_neg(Title, Fact, What) :- print_db_items(Title, Fact, What).
print_db_items_and_neg(Title, Fact, What) :- print_db_items(Title, ~(Fact), What).

%% pfc_list_triggers_1(+What) is semidet.
% Lists triggers in the Pfc database (secondary helper).
% What - The condition to filter triggers.
pfc_list_triggers_1(What) :- var(What), !.
pfc_list_triggers_1(~(What)) :- var(What), !.
pfc_list_triggers_1(~(_What)) :- !.
pfc_list_triggers_1(What) :-
   print_db_items('Supports User', spft_precanonical(P, mfl4(VarNameZ, _, _, _), ax), '$spft$'(P, mfl4(VarNameZ, _, _, _), ax), What),
   print_db_items('Forward Facts', (nesc(F)), F, What),
   print_db_items('Forward Rules', (_==>_), What),
 ignore((What\= ~(_), safe_functor(What, IWhat, _),
   print_db_items_and_neg('Instance Of', isa(IWhat, _), IWhat),
   print_db_items_and_neg('Instances: ', isa(_, IWhat), IWhat),
   print_db_items_and_neg('Subclass Of', genls(IWhat, _), IWhat),
   print_db_items_and_neg('Subclasses: ', genls(_, IWhat), IWhat))),
   forall(suggest_m(M), print_db_items('PFC Watches', pfc_prop(M, _, _, _), What)),
   print_db_items('Triggers Negative', '$nt$'(_, _, _, _), What),
   print_db_items('Triggers Goal', '$bt$'(_, _, _), What),
   print_db_items('Triggers Positive', '$pt$'(_, _, _), What),
   print_db_items('Bidirectional Rules', (_<==>_), What),
   dif(A, B), print_db_items('Supports Deduced', spft_precanonical(P, A, B), '$spft$'(P, A, B), What),
   dif(G, ax), print_db_items('Supports Nonuser', spft_precanonical(P, G, G), '$spft$'(P, G, G), What),
   print_db_items('Backchaining Rules', (_<-_), What),
   % print_db_items('Edits',is_disabled_clause(_),What),
   print_db_items('Edits', is_edited_clause(_, _, _), What),
   print_db_items('Instances', isa(_, _), What),
   print_db_items('Subclasses', genls(_, _), What),
   print_db_items('Negative Facts', ~(_), What),

   print_db_items('ArgTypes', argGenls(_, _, _), What),
   print_db_items('ArgTypes', argIsa(_, _, _), What),
   print_db_items('ArgTypes', argQuotedIsa(_, _, _), What),
   print_db_items('ArgTypes', meta_argtypes(_), What),
   print_db_items('ArgTypes', predicate_property(G, meta_predicate(G)), What),
   print_db_items('ArgTypes', resultGenls(_, _), What),
   print_db_items('ArgTypes', resultIsa(_, _), What),
   print_db_items('Arity', arity(_, _), What),
   print_db_items('Arity', current_predicate(_), What),
   print_db_items('MetaFacts Predicate', predicate_property(_, _), What),
   print_db_items('Sources', module_property(_, _), What),
   print_db_items('Sources', predicateConventionMt(_, _), What),
   print_db_items('Sources', source_file(_, _), What),
   print_db_items('Sources', _:man_index(_, _, _, _, _), What),
   print_db_items('Sources', _:'$pldoc'(_, _, _, _), What),
   print_db_items('Sources', _:'$pred_option'(_, _, _, _), What),
   print_db_items('Sources',_:'$mode'(_,_),What),
   !.

%% pinfo(+Functor_Arity) is semidet.
% Shows predicate information for a specific functor and arity.
% F - Functor of the predicate.
% A - Arity of the predicate.
pinfo(F/A) :-
    listing(F/A), % List the definition of the predicate.
    safe_functor(P,F,A), % Create a functor from F/A.
    findall(Prop, predicate_property(P,Prop), List), % Collect all properties of the predicate.
    wdmsg_pretty(pinfo(F/A) == List), % Display the properties in a formatted way.
    !.



%% pp_DB is semidet.
% Pretty print all facts, rules, triggers, and supports in the default module.

%pp_DB:- defaultAssertMt(M),clause_b(mtHybrid(M)),!,pp_DB(M).
%pp_DB:- forall(clause_b(mtHybrid(M)),pp_DB(M)).
pp_DB :- prolog_load_context(module, M), pp_DB(M).

%% with_exact_kb(+Module, +Goal) is det.
% Executes a goal within the context of a specific module.
% Module - The module context.
% Goal - The goal to execute.
with_exact_kb(M, G) :- M:call(G).

%% pp_DB(+Module) is semidet.
% Pretty prints the Pfc database for a specific module.
% Module - The module context.
pp_DB(M) :-
 with_exact_kb(M,
 M:must_det_l((
  pp_db_facts,
  pp_db_rules,
  pp_db_triggers,
  pp_db_supports))).

%% pp_db_facts is semidet.
% Pretty prints all facts in the current module's Pfc database.
pp_db_facts :- context_module(M), pp_db_facts(M).

%% pp_db_rules is semidet.
% Pretty prints all rules in the current module's Pfc database.
pp_db_rules :- context_module(M), pp_db_rules(M).

%% pp_db_triggers is semidet.
% Pretty prints all triggers in the current module's Pfc database.
pp_db_triggers :- context_module(M), pp_db_triggers(M).

%% pp_db_supports is semidet.
% Pretty prints all supports in the current module's Pfc database.
pp_db_supports :- context_module(M), pp_db_supports(M).

:- system:import(pp_DB/0).
:- system:export(pp_DB/0).

%% pp_db_facts(+Module) is semidet.
% Pretty prints all facts in a specific module's Pfc database.
% Module - The module context.
pp_db_facts(MM) :- ignore(pp_db_facts(MM, _, true)).

%% pp_db_facts(+Module, +Pattern) is semidet.
% Pretty prints facts in a specific module's Pfc database that match a given pattern.
% Module - The module context.
% Pattern - The pattern to match facts against.
pp_db_facts(MM, Pattern) :- pp_db_facts(MM, Pattern, true).

%% pp_db_facts(+Module, +Pattern, +Condition) is semidet.
% Pretty prints facts in a specific module's Pfc database that match a given pattern and condition.
% Module - The module context.
% Pattern - The pattern to match facts against.
% Condition - The condition to filter facts.
pp_db_facts(MM, P, C) :-
  pfc_facts_in_kb(MM, P, C, L),
  pfc_classifyFacts(L, User, Pfc, _ZRule),
  length(User, UserSize), length(Pfc, PfcSize),
  format("~N~nUser added facts in [~w]: ~w", [MM, UserSize]),
  pp_db_items(User),
  format("~N~nSystem added facts in [~w]: ~w", [MM, PfcSize]),
  pp_db_items(Pfc).

%% pp_db_items(+Items) is det.
% Pretty prints a list of database items.
% Items - The list of items to print.

pp_db_items(Var):-var(Var),!,format("~N  ~p",[Var]).
pp_db_items([]) :- !.
pp_db_items([H|T]) :- !,
  % numbervars(H,0,_),
  format("~N  ~p", [H]),
  nonvar(T), pp_db_items(T).

pp_db_items((P >= FT)) :- is_hidden_pft(P, FT), !.

pp_db_items(Var) :-
  format("~N  ~p", [Var]).

%% is_hidden_pft(+Predicate, +FactType) is semidet.
% Checks if a fact type should be hidden.
% Predicate - The predicate to check.
% FactType - The fact type to check.
is_hidden_pft(_,(mfl4(_VarNameZ, BaseKB, _, _), ax)) :- current_prolog_flag(pfc_shared_module, BaseKB), !.
is_hidden_pft(_,(why_marked(_), ax)).

%% pp_mask(+Type, +Module, +Mask) is semidet.
% Prints masked items in a module's Pfc database.
% Type - The type of items.
% Module - The module context.
% Mask - The mask to filter items.
pp_mask(Type, MM, Mask) :-
  bagof_or_nil(Mask, lookup_kb(MM, Mask), Nts),
  list_to_set_variant(Nts, NtsSet), !,
  pp_mask_list(Type, MM, NtsSet).

%% pp_mask_list(+Type, +Module, +List) is semidet.
% Pretty prints a list of masked items.
% Type - The type of items.
% Module - The module context.
% List - The list of masked items.
pp_mask_list(Type, MM, []) :- !,
  format("~N~nNo ~ws in [~w]...~n", [Type, MM]).
pp_mask_list(Type, MM, NtsSet) :- length(NtsSet, Size), !,
  format("~N~n~ws (~w) in [~w]...~n", [Type, Size, MM]),
  pp_db_items(NtsSet).

%% pfc_classifyFacts(+Facts, -UserFacts, -PfcFacts, -Rules) is det.
% Classifies facts into user facts, Pfc facts, and rule facts.
% Facts - The facts to classify.
% UserFacts - The classified Output list of user-added facts.
% PfcFacts - The classified Output list of system-added facts.
% Rules - The classified Output list of rules.
pfc_classifyFacts([], [], [], []).

pfc_classifyFacts([H|T], User, Pfc, [H|Rule]) :-
    pfcType(H, rule(_)), !,
  pfc_classifyFacts(T, User, Pfc, Rule).

pfc_classifyFacts([H|T], [H|User], Pfc, Rule) :-
  % get_source_uu(UU),
    get_first_user_reason(H, _UU), !,
  pfc_classifyFacts(T, User, Pfc, Rule).

pfc_classifyFacts([H|T], User, [H|Pfc], Rule) :-
  pfc_classifyFacts(T, User, Pfc, Rule).

%% pp_db_rules(+Module) is det.
% Pretty print all types of rules in a specified module.
% Module - The module to operate within.
pp_db_rules(MM) :-
   pp_mask("Forward Rule", MM, ==>(_,_)),
   pp_mask("Bidirectional Rule", MM, <==>(_,_)),
   pp_mask("Backchaining Rule", MM, <-(_, _)),
   pp_mask("Implication Rule", MM, =>(_, _)),
   pp_mask("Bi-conditional Rule", MM, <=>(_, _)),
   pp_mask("Negative Fact",MM,(~(_))),
%pp_mask("Material-implRule",MM,<=(_,_)),
%pp_mask("PrologRule",MM,:-(_,_)),
!.

%% pp_db_triggers(+Module) is det.
% Pretty prints all triggers in a specific module's Pfc database.
% Module - The module to operate within.
pp_db_triggers(MM) :-
 pp_mask("Positive trigger(+)", MM, '$pt$'(_, _)),
 pp_mask("Negative trigger(-)", MM, '$nt$'(_, _, _)),
 pp_mask("Goal trigger(?)", MM, '$bt$'(_, _)), !.

%% pp_db_supports(+Module) is semidet.
% Pretty prints all supports in a specific module's Pfc database.
% Module - The module context.
pp_db_supports(MM) :-
  % temporary hack.
  format("~N~nSupports in [~w]...~n", [MM]),
  with_exact_kb(MM, bagof_or_nil((P >= S), pfcGetSupport(P, S), L)),
  list_to_set_variant(L, LS),
  pp_db_items(LS), !.

%% list_to_set_variant(+List, -Unique) is det.
% Convert a list to a set, removing variants.
% List - The input list.
% Unique - The output set.
list_to_set_variant(List, Unique) :-
    list_unique_1(List, [], Unique), !.

%% list_unique_1(+List, +So_far, -Unique) is det.
% Helper predicate for list_to_set_variant/2.
% List - The input list.
% So_far - Accumulator of unique items.
% Unique - The output set.
list_unique_1([], _, []).
list_unique_1([X|Xs], So_far, Us) :-
    memberchk_variant(X, So_far), !,
    list_unique_1(Xs, So_far, Us).
list_unique_1([X|Xs], So_far, [X|Us]) :-
    list_unique_1(Xs, [X|So_far], Us).

%% memberchk_variant(+Val, +List) is semidet.
%   Deterministic check of membership using =@= rather than
%   unification.

memberchk_variant(X, [Y|Ys]) :-
    (X =@= Y -> true ; memberchk_variant(X, Ys)).

%% lookup_kb(+MM, -MHB) is nondet.
% Lookup a clause in the knowledge base module.
% MM - The module to operate within.
% MHB - The head-body clause found.
lookup_kb(MM, MHB) :-
 strip_module(MHB,M,HB),
     expand_to_hb(HB, H, B),
      (MM:clause(M:H, B, Ref) *-> true; M:clause(MM:H, B, Ref)),
      %clause_ref_module(Ref),
      clause_property(Ref, module(MM)).

%% has_cl(+Head) is semidet.
% Checks if a clause exists for a specific head.
% Head - The head to check.
has_cl(H) :- predicate_property(H, number_of_clauses(_)).

%%  clause_or_call( +H, ?B) is semidet.
% Determine if a predicate can be called directly or needs to match a clause.

% PFC2.0 clause_or_call(M:H,B):-is_ftVar(M),!,no_repeats(M:F/A,(f_to_mfa(H,M,F,A))),M:clause_or_call(H,B).
% PFC2.0 clause_or_call(isa(I,C),true):-!,call_u(isa_asserted(I,C)).
% PFC2.0 clause_or_call(genls(I,C),true):-!,on_x_log_throw(call_u(genls(I,C))).
clause_or_call(H, B) :- clause(src_edit(_Before, H), B).
clause_or_call(H, B) :- 
    predicate_property(H, number_of_clauses(C)), 
    predicate_property(H, number_of_rules(R)), 
    ((R*2 < C) -> (clause(H, B) *-> ! ; fail) ; clause(H, B)).

% PFC2.0 clause_or_call(H,true):- call_u(should_call_for_facts(H)),no_repeats(on_x_log_throw(H)).

  /*



% as opposed to simply using clause(H,true).

% %  should_call_for_facts( +H) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(H):- get_functor(H,F,A),call_u(should_call_for_facts(H,F,A)).

% %  should_call_for_facts( +VALUE1, ?F, ?VALUE3) is semidet.
%
% Should Call For Facts.
%
should_call_for_facts(_,F,_):- a(prologSideEffects,F),!,fail.
should_call_for_facts(H,_,_):- modulize_head(H,HH), \+ predicate_property(HH,number_of_clauses(_)),!.
should_call_for_facts(_,F,A):- clause_b(pfc_prop(_M,F,A,pfcRHS)),!,fail.
should_call_for_facts(_,F,A):- clause_b(pfc_prop(_M,F,A,pfcMustFC)),!,fail.
should_call_for_facts(_,F,_):- a(prologDynamic,F),!.
should_call_for_facts(_,F,_):- \+ a(pfcControlled,F),!.

       */

%% no_side_effects(+Predicate) is semidet.
% Checks if a predicate has no side effects.
% Predicate - The predicate to check.
no_side_effects(P) :- (\+ is_side_effect_disabled -> true; (get_functor(P, F, _), a(prologSideEffects, F))).

%% pfc_facts_in_kb(+Module, +Pattern, +Condition, -Facts) is det.
% Retrieves facts from a specific module's knowledge base.
% Module - The module context.
% Pattern - The pattern to match facts against.
% Condition - The condition to filter facts.
% Facts - The retrieved facts.
pfc_facts_in_kb(MM, P, C, L) :- with_exact_kb(MM, setof_or_nil(P, pfcFact(P, C), L)).

%% lookup_spft(+Predicate, -Fact, -Type) is nondet.
% Looks up a support fact type for a specific predicate.
% Predicate - The predicate to look up.
% Fact - The support fact.
% Type - The support type.
lookup_spft(P, F, T) :- pfcGetSupport(P, (F, T)).
% why_dmsg(Why,Msg):- with_current_why(Why,dmsg_pretty(Msg)).

%% u_to_uu(+U, -UU) is det.
% Converts a user fact or support to a user fact type (U to UU).
% U - The user fact or support.
% UU - The resulting user fact type.
u_to_uu(U, (U, ax)) :- var(U), !.
u_to_uu(U, U) :- nonvar(U), U = (_, _), !.
u_to_uu([U|More], UU) :- list_to_conjuncts([U|More], C), !, u_to_uu(C, UU).
u_to_uu(U, (U, ax)) :- !.

%% get_source_uu(-UU) is det.
% Retrieves the source reference for the current context.
% UU - The retrieved source reference.
% (Current file or User)
:- module_transparent((get_source_uu)/1).
get_source_uu(UU) :- must_ex((get_source_ref1(U), u_to_uu(U, UU))), !.
%% get_source_ref1(-U) is det.
% Retrieves the source reference for the current context (helper predicate).
% U - The retrieved source reference.
get_source_ref1(U) :- quietly_ex((current_why(U), nonvar(U))); ground(U), !.
get_source_ref1(U) :- quietly_ex((get_source_mfl(U))), !.

%% get_why_uu(-UU) is det.
% Retrieves the current "why" reference as a user fact type (UU).
% UU - The retrieved user fact type.
:- module_transparent((get_why_uu)/1).
get_why_uu(UU) :- findall(U, current_why(U), Whys), Whys \== [], !, u_to_uu(Whys, UU).
get_why_uu(UU) :- get_source_uu(UU), !.

%% get_startup_uu(-UU) is det.
% Retrieves the startup "why" reference as a user fact type (UU).
% UU - The retrieved user fact type.
get_startup_uu(UU) :-
  prolog_load_context(module, CM),
  u_to_uu((isRuntime, mfl4(VarNameZ, CM, user_input, _)), UU), varnames_load_context(VarNameZ).

%% is_user_reason(+UserFact) is semidet.
% Checks if a user fact is a valid user reason.
% UserFact - The user fact to check.
is_user_reason((_, U)) :- atomic(U).
only_is_user_reason((U1, U2)) :- freeze(U2, is_user_reason((U1, U2))).

%% is_user_fact(+Predicate) is semidet.
% Checks if a predicate is a user-added fact.
% Predicate - The predicate to check.
is_user_fact(P) :- get_first_user_reason(P, UU), is_user_reason(UU).

%% get_first_real_user_reason(+Predicate, -UU) is semidet.
% Retrieves the first real user reason for a predicate.
% Predicate - The predicate to check.
% UU - The retrieved user reason.
get_first_real_user_reason(P, UU) :- nonvar(P), UU = (F, T),
  quietly_ex(((((lookup_spft(P, F, T))), is_user_reason(UU)) *-> true;
    ((((lookup_spft(P, F, T))), \+ is_user_reason(UU)) *-> (!, fail) ; fail))).

%% get_first_user_reason(+Predicate, -UU) is semidet.
% Retrieves the first user reason for a predicate.
% Predicate - The predicate to check.
% UU - The retrieved user reason.
get_first_user_reason(P, (F, T)) :-
  UU = (F, T),
  ((((lookup_spft(P, F, T))), is_user_reason(UU)) *-> true;
    ((((lookup_spft(P, F, T))), \+ is_user_reason(UU)) *-> (!, fail) ;
       (clause_asserted(P), get_source_uu(UU), is_user_reason(UU)))), !.
get_first_user_reason(_, UU) :- get_why_uu(UU), is_user_reason(UU), !.
get_first_user_reason(_, UU) :- get_why_uu(UU), !.
get_first_user_reason(P, UU) :- must_ex(ignore((get_first_user_reason0(P, UU)))), !.
%get_first_user_reason(_,UU):- get_source_uu(UU),\+is_user_reason(UU). % ignore(get_source_uu(UU)).



%% get_first_user_reason0(+Predicate, -UU) is semidet.
% Helper predicate for get_first_user_reason/2.
% Predicate - The predicate to check.
% UU - The retrieved user reason.
get_first_user_reason0(_, (M, ax)) :- get_source_mfl(M).

%:- export(pfc_at_box:defaultAssertMt/1).
%:- system:import(defaultAssertMt/1).
%:- pfc_lib:import(pfc_at_box:defaultAssertMt/1).

%% get_source_mfl(-MFL) is det.
% Retrieves the source reference for the current module/file location.
% MFL - The retrieved source reference.
:- module_transparent((get_source_mfl)/1).
get_source_mfl(M):- current_why(M), nonvar(M) , M =mfl4(_VarNameZ,_,_,_).
get_source_mfl(mfl4(VarNameZ, M, F, L)) :- defaultAssertMt(M), current_source_location(F, L), varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ, M, F, L)) :- defaultAssertMt(M), current_source_file(F:L), varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ, M, F, _L)) :- defaultAssertMt(M), current_source_file(F), varnames_load_context(VarNameZ).
get_source_mfl(mfl4(VarNameZ, M, _F, _L)) :- defaultAssertMt(M), varnames_load_context(VarNameZ).
%get_source_mfl(M):-(defaultAssertMt(M)->true;(atom(M)->(module_property(M,class(_)),!);(var(M),module_property(M,class(_))))).
get_source_mfl(M):-fail,dtrace,
((defaultAssertMt(M)->!;
(atom(M)->(module_property(M,class(_)),!);
pfcError(no_source_ref(M))))).

is_source_ref1(_).

defaultAssertMt(M):-prolog_load_context(module,M).



%% pfc_pp_db_justifications(+Predicate, +Justifications) is det.
% Pretty prints the justifications for a predicate.
% Predicate - The predicate to print justifications for.
% Justifications - The justifications to print.
pfc_pp_db_justifications(P, Js) :-
 show_current_source_location,
 must_ex(quietly_ex((format("~NJustifications for ~p:", [P]),
  pfc_pp_db_justification1('', Js, 1)))).

%% pfc_pp_db_justification1(+Prefix, +Justifications, +N) is det.
% Helper predicate for pfc_pp_db_justifications/2.
% Prefix - The prefix for printing.
% Justifications - The justifications to print.
% N - The current justification number.
pfc_pp_db_justification1(_, [], _).
pfc_pp_db_justification1(Prefix, [J|Js], N) :-
  % show one justification and recurse.
  nl,
  pfc_pp_db_justifications2(Prefix, J, N, 1),
  %reset_shown_justs,
  N2 is N+1,
  pfc_pp_db_justification1(Prefix, Js, N2).

%% pfc_pp_db_justifications2(+Prefix, +Justification, +JustNo, +StepNo) is det.
% Helper predicate for pfc_pp_db_justification1/3.
% Prefix - The prefix for printing.
% Justification - The justification to print.
% JustNo - The current justification number.
% StepNo - The current step number.
pfc_pp_db_justifications2(_, [], _, _).
pfc_pp_db_justifications2(Prefix, [C|Rest], JustNo, StepNo) :-
(nb_hasval('$last_printed',C)-> dmsg_pretty(chasVal(C)) ;
 ((StepNo==1->fmt('~N~n',[]);true),
  backward_compatibility:sformat(LP,' ~w.~p.~p',[Prefix,JustNo,StepNo]),
  nb_pushval('$last_printed',LP),
  format("~N  ~w ~p",[LP,C]),
  ignore(loop_check(pfcWhy_sub_sub(C))),
  StepNext is 1+StepNo,
  pfc_pp_db_justifications2(Prefix,Rest,JustNo,StepNext))).


%% pfcWhy_sub_sub(+Predicate) is det.
% Sub-function for pfcWhy to handle sub-subjustifications.
% Predicate - The predicate to check.
pfcWhy_sub_sub(P) :-
  justifications(P, Js),
  clear_proofs,
  % retractall_u(t_l:whybuffer(_,_)),
  (nb_hasval('$last_printed', P) -> dmsg_pretty(hasVal(P)) ;
   ((
  assertz(t_l:whybuffer(P, Js)),
   nb_getval('$last_printed', LP),
   ((pfc_pp_db_justification1(LP, Js, 1), fmt('~N~n', [])))))).

%   File   : pfcwhy.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Updated:
%   Purpose: predicates for interactively exploring Pfc justifications.

% ***** predicates for browsing justifications *****

:- use_module(library(lists)).

:- dynamic(t_l:whybuffer/2).

%% pfcWhy is semidet.
% Interactively explores Pfc justifications.
pfcWhy :-
  t_l:whybuffer(P, _),
  pfcWhy(P).

%% pfcTF(+Predicate) is semidet.
% Prints the truth value of a predicate.
% Predicate - The predicate to check.
pfcTF(P) :- pfc_call(P) *-> foreach(pfcTF1(P), true); pfcTF1(P).

%% pfcTF1(+Predicate) is semidet.
% Helper predicate for pfcTF/1.
% Predicate - The predicate to check.
pfcTF1(P) :-
   ansi_format([underline], "~N=========================================", []),
   (ignore(pfcWhy(P))), ignore(pfcWhy(~P)),
   printLine.

%% pfcWhy(+N) is semidet.
%% pfcWhy(+Predicate) is semidet.
% Interactively explores the Nth justification for a predicate.
% N - The justification number.
% Predicate - The predicate to explore.
pfcWhy(N) :-
  number(N), !,
  t_l:whybuffer(P, Js),
  pfcWhyCommand(N, P, Js).
pfcWhy(P) :-
  justifications(P, Js),
  retractall(t_l:whybuffer(_,_)),
  assert(t_l:whybuffer(P, Js)),
  pfcWhyBrouse(P, Js).

%% pfcWhy1(+Predicate) is semidet.
% Interactively explores the first justification for a predicate.
% Predicate - The predicate to explore.
pfcWhy1(P) :-
  justifications(P, Js),
  pfcWhyBrouse(P, Js).

%% pfcWhy2(+Predicate, +N) is semidet.
% Interactively explores the Nth justification for a predicate.
% Predicate - The predicate to explore.
% N - The justification number.
pfcWhy2(P, N) :-
  justifications(P, Js), pfcShowJustification1(Js, N).

%% pfcWhyBrouse(+Predicate, +Justifications) is semidet.
% Interactively explores justifications for a predicate.
% Predicate - The predicate to explore.
% Justifications - The justifications to explore.
pfcWhyBrouse(P, Js) :-
  % rtrace(pfc_pp_db_justifications(P,Js)),
  pfcShowJustifications(P, Js),
  nop((pfcAsk(' >> ', Answer),
  pfcWhyCommand(Answer, P, Js))).

%% pfcWhyCommand(+Command, +Predicate, +Justifications) is semidet.
% Executes a command during Pfc justification exploration.
% Command - The command to execute.
% Predicate - The predicate being explored.
% Justifications - The justifications being explored.
pfcWhyCommand(q, _, _) :- !. % Quit.
pfcWhyCommand(h, _, _) :- !, % Help.
  format("~nJustification Browser Commands:
 q   quit.
 N   focus on Nth justification.
 N.M browse step M of the Nth justification
 u   up a level~n", []).

pfcWhyCommand(N, _P, Js) :- float(N), !,
  pfcSelectJustificationNode(Js, N, Node),
  pfcWhy1(Node).

pfcWhyCommand(u, _, _) :- !. % Up a level.

pfcCommand(N, _, _) :- integer(N), !,
  pfcPrintf("~p is a yet unimplemented command.", [N]),
  fail.

pfcCommand(X, _, _) :- pfcPrintf("~p is an unrecognized command, enter h. for help.", [X]),
 fail.

%% pfcShowJustifications(+Predicate, +Justifications) is semidet.
% Pretty prints justifications for a predicate.
% Predicate - The predicate to print justifications for.
% Justifications - The justifications to print.
pfcShowJustifications(P, Js) :-
  show_current_source_location,
  reset_shown_justs,
  %color_line(yellow,1),
  format("~N~nJustifications for ", []),
  ansi_format([fg(green)], '~@', [pp(P)]),
  format(" :~n", []),
  pfcShowJustification1(Js, 1),!,
  printLine.

%% pfcShowJustification1(+Justifications, +N) is semidet.
% Pretty prints the Nth justification in a list.
% Justifications - The list of justifications.
% N - The justification number.
pfcShowJustification1([J|Js], N) :- !,
  % show one justification and recurse.
  %reset_shown_justs,
  pfcShowSingleJustStep(N, J),!,
  N2 is N+1,
  pfcShowJustification1(Js, N2).

pfcShowJustification1(J, N) :-
  %reset_shown_justs, % nl,
  pfcShowSingleJustStep(N, J),!.

%% pfcShowSingleJustStep(+JustNo, +Justification) is semidet.
% Pretty prints a single step in a justification.
% JustNo - The justification number.
% Justification - The justification step.
pfcShowSingleJustStep(N, J) :- 
  pfcShowSingleJust(N, step(1), J),!.
pfcShowSingleJustStep(N, J) :- 
  pp(pfcShowSingleJustStep(N, J)),!.

%% incrStep(+StepNo, -Step) is det.
% Increments the step number.
% StepNo - The current step number.
% Step - The incremented step number.
incrStep(StepNo, Step) :- compound(StepNo), arg(1, StepNo, Step), X is Step+1, nb_setarg(1, StepNo, X).

%% pfcShowSingleJust(+JustNo, +StepNo, +Justification) is semidet.
% Pretty prints a single justification step.
% JustNo - The justification number.
% StepNo - The step number.
% Justification - The justification step.
pfcShowSingleJust(JustNo, StepNo, C) :- is_ftVar(C), !, incrStep(StepNo, Step),
  ansi_format([fg(cyan)], "~N    ~w.~w ~w ", [JustNo, Step, C]), !, maybe_more_c(C).
pfcShowSingleJust(_JustNo,_StepNo,[]):-!.
pfcShowSingleJust(JustNo, StepNo, (P, T)) :- !,
  pfcShowSingleJust(JustNo, StepNo, P),
  pfcShowSingleJust(JustNo, StepNo, T).
pfcShowSingleJust(JustNo, StepNo, (P, F, T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P),
  pfcShowSingleJust(JustNo, StepNo, F),
  pfcShowSingleJust1(JustNo, StepNo, T).
pfcShowSingleJust(JustNo, StepNo, (P *-> T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P), format('      *-> ', []),
  pfcShowSingleJust1(JustNo, StepNo, T).

pfcShowSingleJust(JustNo, StepNo, (P :- T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P), format(':- ~p.', [T]).

pfcShowSingleJust(JustNo, StepNo, (P : - T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, P), format('      :- ', []),
  pfcShowSingleJust(JustNo, StepNo, T).

pfcShowSingleJust(JustNo, StepNo, (P :- T)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, call(T)),
  pfcShowSingleJust1(JustNo, StepNo, P).

pfcShowSingleJust(JustNo, StepNo, [P|T]) :- !,
  pfcShowSingleJust(JustNo, StepNo, P),
  pfcShowSingleJust(JustNo, StepNo, T).

pfcShowSingleJust(JustNo, StepNo, '$pt$'(P, Body)) :- !,
  pfcShowSingleJust1(JustNo, StepNo, '$pt$'(P)),
  pfcShowSingleJust(JustNo, StepNo, Body).

pfcShowSingleJust(JustNo, StepNo, C) :- 
  pfcShowSingleJust1(JustNo, StepNo, C).

%% fmt_cl(+Clause) is det.
% Formats and writes a clause to the output.
% Clause - The clause to format.
fmt_cl(P) :- \+ \+ (numbervars(P, 666, _, [attvars(skip), singletons(true)]), write_src(P)), !.
fmt_cl(P) :- \+ \+ (pretty_numbervars(P, PP), numbervars(PP, 126, _, [attvar(skip), singletons(true)]),
   write_term(PP, [portray(true), portray_goal(fmt_cl)])), write('.').
fmt_cl(S,_):- term_is_ansi(S), !, write_keeping_ansi(S).
fmt_cl(G,_):- is_grid(G),write('"'),user:print_grid(G),write('"'),!.
% fmt_cl(P,_):- catch(arc_portray(P),_,fail),!.
fmt_cl(P,_):- is_list(P),catch(p_p_t_no_nl(P),_,fail),!.
%ptg(PP,Opts):- is_list(PP),select(portray_goal(ptg),Opts,Never),write_term(PP,Never).



%% unwrap_litr(+Clause, -UnwrappedClause) is det.
% Unwraps a literal clause.
% Clause - The clause to unwrap.
% UnwrappedClause - The unwrapped clause.
unwrap_litr(C, CCC+VS) :- copy_term(C, CC, VS),
  numbervars(CC+VS, 0, _),
  unwrap_litr0(CC, CCC), !.
unwrap_litr0(call(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0('$pt$'(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0(body(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0(head(C), CC) :- unwrap_litr0(C, CC).
unwrap_litr0(C, C).

:- thread_local t_l:shown_why/1.

%% pfcShowSingleJust1(+JustNo, +StepNo, +Clause) is det.
% Pretty prints a single clause in a justification.
% JustNo - The justification number.
% StepNo - The step number.
% Clause - The clause to print.
pfcShowSingleJust1(JustNo, _, MFL) :- is_mfl(MFL), JustNo \== 1, !.
pfcShowSingleJust1(JustNo, StepNo, C) :- unwrap_litr(C, CC), !, pfcShowSingleJust4(JustNo, StepNo, C, CC).

%% pfcShowSingleJust4(+JustNo, +StepNo, +Clause, +UnwrappedClause) is det.
% Helper predicate for pfcShowSingleJust1/3.
% JustNo - The justification number.
% StepNo - The step number.
% Clause - The clause to print.
% UnwrappedClause - The unwrapped clause to print.
pfcShowSingleJust4(_, _, _, CC) :- t_l:shown_why(C), C =@= CC, !.
pfcShowSingleJust4(_, _, _, MFL) :- is_mfl(MFL), !.
pfcShowSingleJust4(JustNo, StepNo, C, CC) :- assert(t_l:shown_why(CC)), !,
   incrStep(StepNo, Step),
   ansi_format([fg(cyan)], "~N    ~w.~w ~@ ", [JustNo, Step, user:fmt_cl(C)]),
   %write('<'),
   pfcShowSingleJust_C(C),!,%write('>'),
   format('~N'),
   ignore((maybe_more_c(C))),
   assert(t_l:shown_why(C)),
   format('~N'), !.

%% is_mfl(+Term) is semidet.
% Checks if a term is an mfl (module/file/line) reference.
% Term - The term to check.
is_mfl(MFL) :- compound(MFL), MFL = mfl4(_, _, _, _).

%% maybe_more_c(+Term) is det.
% Triggers exploration of more clauses if needed.
% Term - The term to check.
maybe_more_c(MFL) :- is_mfl(MFL), !.
maybe_more_c(_) :- t_l:shown_why(no_recurse).
maybe_more_c(C) :- t_l:shown_why(more(C)), !.
maybe_more_c(C) :- t_l:shown_why((C)), !.
maybe_more_c(C) :- assert(t_l:shown_why(more(C))), assert(t_l:shown_why((C))),
 locally(t_l:shown_why(no_recurse),
  locally(t_l:shown_why((C)), locally(t_l:shown_why(more(C)),
   ignore(catch(pfcWhy2(C, 1.1), E, fbugio(E)))))), !.

%% pfcShowSingleJust_C(+Clause) is det.
% Helper predicate for pfcShowSingleJust1/3.
% Clause - The clause to print.
pfcShowSingleJust_C(C) :- is_file_ref(C), !.
pfcShowSingleJust_C(C) :- find_mfl(C, MFL), assert(t_l:shown_why(MFL)), !, pfcShowSingleJust_MFL(MFL).
pfcShowSingleJust_C(_) :- ansi_format([hfg(black)], " % [no_mfl] ", []), !.

%% short_filename(+File, -ShortFilename) is det.
% Extracts a short filename from a full file path.
% File - The full file path.
% ShortFilename - The extracted short filename.
short_filename(F, FN) :- symbolic_list_concat([_, FN], '/pack/', F), !.
short_filename(F, FN) :- symbolic_list_concat([_, FN], swipl, F), !.
short_filename(F, FN) :- F = FN, !.

%% pfcShowSingleJust_MFL(+MFL) is det.
% Helper predicate for pfcShowSingleJust_C/1.
% MFL - The mfl (module/file/line) reference to print.
pfcShowSingleJust_MFL(MFL) :- MFL = mfl4(VarNameZ, _M, F, L), atom(F), short_filename(F, FN), !, varnames_load_context(VarNameZ),
   ansi_format([hfg(black)], " % [~w:~w] ", [FN, L]).

pfcShowSingleJust_MFL(MFL) :- MFL = mfl4(V, M, F, L), my_maplist(var, [V, M, F, L]), !.
pfcShowSingleJust_MFL(MFL) :- ansi_format([hfg(black)], " % [~w] ", [MFL]), !.

%% pfcAsk(+Message, -Answer) is det.
% Asks the user for input during Pfc justification exploration.
% Message - The message to display.
% Answer - The user's input.
pfcAsk(Msg, Ans) :-
  format("~n~w", [Msg]),
  read(Ans).

%% pfcSelectJustificationNode(+Justifications, +Index, -Node) is det.
% Selects a specific node in a justification based on an index.
% Justifications - The list of justifications.
% Index - The index to select.
% Node - The selected node.
pfcSelectJustificationNode(Js, Index, Step) :-
  JustNo is integer(Index),
  nth1(JustNo, Js, Justification),
  StepNo is 1 + integer(Index*10 - JustNo*10),
  nth1(StepNo, Justification, Step).
