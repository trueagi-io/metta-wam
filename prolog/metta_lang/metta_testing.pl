/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
%:- encoding(iso_latin_1).

%*********************************************************************************************
% PROGRAM FUNCTION: provides a framework for executing, tracking, and logging Prolog
% unit tests with structured reporting and ANSI-formatted output for readability.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(occurs)).

% Ensure that the `metta_interp` library is loaded.
% This loads all the predicates called from this file.
:- ensure_loaded(metta_interp).
:- ensure_loaded(metta_utils).

% Reset loonit counters

%!  loonit_reset is det.
%
%   Resets all counters related to testing outcomes. Specifically, it clears
%   any previous test results by resetting both the success and failure flags
%   to zero.
%
%   This predicate is useful for initializing or reinitializing test metrics
%   before starting a new set of tests.
loonit_reset :-
    % Ensures any pending output is written before resetting.
    flush_output,
    % Calls a report function.
    loonit_report,
    % Flushes output again after reporting.
    flush_output,
    % Resets the failure counter to zero.
    flag(loonit_failure, _, 0),
    % Resets the success counter to zero.
    flag(loonit_success, _, 0).

%!  has_loonit_results is nondet.
%
%   Succeeds if there are any test results (either successes or failures).
%
%   This predicate is used to check if tests have been run by examining the
%   cumulative count of results. If the count (FS) is greater than 1, it
%   indicates results exist.
has_loonit_results :-
    % Retrieves the current test count.
    loonit_number(FS),
    % Succeeds if there are results (count > 1).
    FS > 1.

%!  loonit_number(-FS) is det.
%
%   Retrieves the total number of tests executed so far, including both
%   successes and failures. The result (FS) unifies with this count.
%
%   This predicate first tries to retrieve the test number flag. If no test
%   number flag exists or has a zero value, it calculates the count based on
%   success and failure flags, adding one as a base count.
%
%   @arg FS The unified test count.
loonit_number(FS) :-
    % Tries to read the test number flag.
    flag(loonit_test_number, FS, FS),
    % If the flag exists and is non-zero, succeeds.
    FS > 0,!.
loonit_number(FS) :-
    % Retrieves the success count.
    flag(loonit_success, Successes, Successes),
    % Retrieves the failure count.
    flag(loonit_failure, Failures, Failures),
    % Calculates total count with a base of 1.
    FS is Successes + Failures + 1.

%!  string_replace(+Original, +Search, +Replace, -Replaced) is det.
%
%   Replaces all occurrences of `Search` in `Original` with `Replace`,
%   producing `Replaced`. This predicate breaks `Original` into segments
%   split by `Search`, then concatenates these segments with `Replace`
%   between each.
%
%   @arg Original The original string to search within.
%   @arg Search   The substring to search for and replace.
%   @arg Replace  The substring to insert in place of `Search`.
%   @arg Replaced The resulting string after all replacements.
%
%   @example
%     % Replace all occurrences of "foo" with "bar" in the string:
%     ?- string_replace("foo_test_foo", "foo", "bar", Replaced).
%     Replaced = "bar_test_bar".
%
string_replace(Original, Search, Replace, Replaced) :-
    symbolic_list_concat(Split, Search, Original),
    symbolic_list_concat(Split, Replace, Replaced),!.

%!  get_test_name(+Number, -TestName) is det.
%
%   Generates a test name by appending a formatted `Number` to the base
%   path or filename currently loaded. If `loading_file` is defined
%   with a non-empty path, it is used as the base; otherwise, a default
%   path ('SOME/UNIT-TEST') is used.
%
%   @arg Number   The test number to include in the name.
%   @arg TestName The generated test name, combining the base path with
%                 the test number in a standardized format.
%
%   @example
%     % Generate a test name for test number 5:
%     ?- get_test_name(5, TestName).
%     TestName = 'SOME/UNIT-TEST.05'.
%
get_test_name(Number, TestName) :-
   ((nb_current(loading_file, FilePath), FilePath \== []) -> true ; FilePath = 'SOME/UNIT-TEST'),
   make_test_name(FilePath, Number, TestName).

%!  ensure_basename(+FilePath0, -FilePath) is det.
%
%   Ensures that FilePath is a base name without directory components.
%   If FilePath0 already lacks directory components, it is returned as-is.
%   Otherwise, it is converted to an absolute path.
%
%   @arg FilePath0 Initial file path, potentially with directories.
%   @arg FilePath  Resulting base name or absolute path.
%
%   @example
%     % Convert an absolute file path with directories:
%     ?- ensure_basename('/home/user/test.pl', FilePath).
%     FilePath = '/home/user/test.pl'.
%
ensure_basename(FilePath, FilePath) :-
    % Succeeds if FilePath is already a simple file name without directory components.
    \+ directory_file_path(('.'), _, FilePath), !.
ensure_basename(FilePath0, FilePath) :-
    % Converts FilePath0 to an absolute path, stored in FilePath.
    absolute_file_name(FilePath0, FilePath), !.
ensure_basename(FilePath, FilePath).

%!  make_test_name(+FilePath0, +Number, -TestName) is det.
%
%   Generates a standardized test name based on a given file path and a test number.
%   The test name combines the uppercase parent directory and base file name,
%   with underscores replaced by hyphens, followed by a zero-padded test number.
%
%   @arg FilePath0 Initial file path used to create the test name.
%   @arg Number     Numeric identifier for the test, formatted as two digits.
%   @arg TestName   Resulting standardized test name.
%
%   @example
%     % Generate a test name from a file path and test number:
%     ?- make_test_name('/home/user/test_file.pl', 5, TestName).
%     TestName = 'HOME.TEST-FILE.05'.
make_test_name(FilePath0, Number, TestName) :-
    % Normalize the file path to a base name.
    ensure_basename(FilePath0, FilePath),
    % Extracts the base file name from the path.
    file_base_name(FilePath, FileName),
    % Retrieves the parent directory of the file.
    directory_file_path(ParentDir, FileName, FilePath),
    % Extracts the base name of the parent directory.
    file_base_name(ParentDir, ParentDirBase),
    % Removes the file extension from the base file name.
    file_name_extension(Base, _, FileName),
    % Converts the parent directory base name to uppercase.
    string_upper(ParentDirBase, UpperParentDirBase),
    % Converts the base file name to uppercase.
    string_upper(Base, UpperBase),
    % Replaces underscores with hyphens in the base name.
    string_replace(UpperBase, "_MW", "", NOMW),
    string_replace(NOMW, "_", "-", NoUnderscore0),
    % Replaces underscores with hyphens in the parent directory name.
    string_replace(UpperParentDirBase, "_", "-", NoUnderscoreParent0),
    % Formats the test number as a zero-padded two-digit string.
    wots(NS, format('~`0t~d~2|', [Number])),
    compiled_or_interp(CorI),
    sformat(NoUnderscore,'~w~w',[NoUnderscore0,CorI]),
    sformat(NoUnderscoreParent,'~w~w',[NoUnderscoreParent0,CorI]),
    % Combines parent directory, file base, and test number to form the test name.
    format(string(TestName), "~w.~w.~w", [NoUnderscoreParent, NoUnderscore, NS]).

%!  color_g_mesg(+Color, :Goal) is det.
%
%   Executes the given Goal with color formatting, if conditions allow.
%   This predicate evaluates whether to display a message in color based on
%   system settings (e.g., compatibility or silent loading modes). If conditions
%   permit, it applies the specified color to the Goal's output.
%
%   @arg Color The color to apply if messages are displayed.
%   @arg Goal  The goal to execute and display, formatted with the specified color.
%
%   @example
%     % Display a message in cyan if not in silent mode:
%     ?- color_g_mesg(cyan, writeln('Test message')).

% color_g_mesg(_, _) :- is_compatio, !.
% color_g_mesg(_, _) :- silent_loading, !.
color_g_mesg(C, G) :-
    % Checks if output is allowed, skipping if silent mode is active.
    notrace((
        % Determines if silent loading mode should suppress output.
        nop(check_silent_loading),
        % Executes the color message output if permissible.
        color_g_mesg_ok(C, G)
    )).

%!  color_g_mesg_ok(+Color, :Goal) is det.
%
%   Internal helper to apply color formatting conditionally and execute Goal.
%   This predicate first checks compatibility mode and, if active, simply calls
%   the Goal without formatting. If compatibility mode is inactive, it formats
%   the output in the specified color using `our_ansi_format/3`.
%
%   @arg Color The color in which the Goal output is formatted.
%   @arg Goal  The goal to execute with or without color formatting.
%
%   @example
%     % Execute a message with color formatting if allowed:
%     ?- color_g_mesg_ok(red, writeln('Important message')).
color_g_mesg_ok(_, G) :-
    % In compatibility mode, simply execute the Goal without formatting.
    is_compatio, !, call(G).
color_g_mesg_ok(C, G) :-
    % Quietly executes Goal and formats output, suppressing if empty.
    quietly((
        % Writes the output of Goal to S, if any.
        wots(S, must_det_ll(user:G)),
        % Applies color formatting if there is non-empty output in S.
        (S == "" -> true ; our_ansi_format(C, '~w~n', [S]))
    )), !.

%!  our_ansi_format(+Color, +Format, +Args) is det.
%
%   Formats and outputs text in the specified color if Color is an atom,
%   otherwise uses `ansi_format/3` directly. This predicate allows for conditional
%   colorized output, with fallback formatting if color settings are not provided.
%
%   @arg Color  The color to apply to the text. If non-atomic, falls back to default formatting.
%   @arg Format The format string.
%   @arg Args   The arguments for the format string.
%
%   @example
%     % Apply color formatting directly:
%     ?- our_ansi_format(green, 'Success: ~w', ['All tests passed']).
%
our_ansi_format(C, Fmt, Args):-
% This better be a list of ansi_format/3 attributes because we're not
% checking that. Those can be compound fg, bg etc terms, or single atoms
% denoting not font style, e.g. bold (but not colour!).
    is_list(C),
    !,
    ansi_format(C,Fmt,Args).
% ansi_format/3 accepts as its first argument a single compound term
% denoting a colour attribute, as well as a list thereof. The following
% clause deals with single, arity-1 compounds. Acceptable attribute
% terms are found in the SWI-Prolog documentation.
our_ansi_format(CT, Fmt, Args):-
    compound(CT),
    CT =.. [Attr,_C],
    memberchk(Attr,[fg,bg,hfg,hbg,fg8,bg8]),
    !,
    ansi_format(CT,Fmt,Args).
% The Attribute term may be an arity-3 compound with arguments for R, G
% and B values.
our_ansi_format(CT, Fmt, Args):-
    compound(CT),
    CT =.. [Attr,_R,_G,_B],
    memberchk(Attr,[fg,bg]),
    !,
    ansi_format(CT,Fmt,Args).
% If the colour term is a single atom, then it's probably our shortcut
% for "use this colour in the forergound".
our_ansi_format(C, Fmt, Args):-
    atom(C),
    ansi_format([fg(C)],Fmt,Args).


%!  print_current_test is det.
%
%   Prints the current test's name in an HTML heading format. The test name is
%   retrieved based on the current `loonit_test_number`.
%
%   @example
%     % Display the current test in an HTML format:
%     ?- print_current_test.
print_current_test :-
    % Retrieves the current test number.
    loonit_number(Number),
    % Generates the test name based on the current number.
    get_test_name(Number, TestName),
    % Prints the test name in an HTML heading format.
    format('~N~n;<h3 id="~w">;; ~w</h3>~n', [TestName, TestName]).

%!  ensure_increments(:Goal) is det.
%
%   Executes a Goal and increments the failure counter if it does not alter the
%   pass/fail counts. This predicate is useful for tracking test failures during
%   a Goal's execution.
%
%   @arg Goal The goal whose outcome is tracked for increments.
%
%   @example
%     % Track a goal and increment failure count if it fails:
%     ?- ensure_increments(writeln('Running test goal...')).
ensure_increments(Goal) :-
    % Sets up initial conditions and executes Goal, adjusting counters afterward.
    setup_call_cleanup(
        % Retrieves initial test result counts.
        get_pass_fail(_, _, TotalStart),
        % Executes the given Goal.
        Goal,
        % After execution, increments failure counter if pass/fail counts remain unchanged.
        ((get_pass_fail(_, _, TotalEnd),
         if_t(TotalEnd == TotalStart,
              flag(loonit_failure, Failures, Failures + 1))))).

%!  get_pass_fail(-Successes, -Failures, -Total) is det.
%
%   Retrieves the current counts of successful and failed tests.
%
%   @arg Successes Unified with the current success count.
%   @arg Failures  Unified with the current failure count.
%   @arg Total     Unified with the sum of Successes and Failures.
%
%   @example
%     % Query the current pass and fail counts:
%     ?- get_pass_fail(S, F, T).
%     S = 10, F = 2, T = 12.
get_pass_fail(Successes, Failures, Total) :-
    % Retrieves the success count.
    flag(loonit_success, Successes, Successes),
    % Retrieves the failure count.
    flag(loonit_failure, Failures, Failures),
    % Sums successes and failures to get the total count.
    !,
    Total is Successes + Failures.

%!  loonit_asserts(+Src, +Precondition, +Goal) is det.
%
%   Ensures that assertions for a Goal are properly tracked and increments the
%   counters based on the result. It calls `loonit_asserts0/3` as the core logic.
%
%   @arg Src         The source identifier for the assertion.
%   @arg Precondition The precondition to be met before running the Goal.
%   @arg Goal        The goal to assert.
%
%   @example
%     % Assert a goal with a precondition and track the result:
%     ?- loonit_asserts('test_source', true, writeln('Testing...')).
loonit_asserts(S, Pre, G) :-
    % Wraps `loonit_asserts0/3` with increment tracking.
    ensure_increments(loonit_asserts0(S, Pre, G)).

%!  loonit_asserts0(+Src, +Precondition, +Goal) is det.
%
%   Asserts a Goal if its preconditions are met and increments the test counter.
%   Displays the current test in a structured format and tracks the Goal’s execution.
%
%   @arg Src         The source identifier for the assertion.
%   @arg Precondition The precondition to be met before running the Goal.
%   @arg Goal        The goal to assert.
%
%   @example
%     % Perform assertion with tracking, assuming precondition holds:
%     ?- loonit_asserts0('source', true, writeln('Executing goal')).
loonit_asserts0(S, Pre, G) :-
    % Increments the test number.
    flag(loonit_test_number, X, X + 1),
    % Copies the precondition term to avoid modifications.
    copy_term(Pre, Pro),
    % Displays the current test name.
    print_current_test,
    % Evaluates the precondition once.
    once(Pre), !,
    % Sets the execution source if available.
    ((nb_current(exec_src, Exec), Exec \== []) -> true ; S = Exec),
    % Writes the execution source for logging.
    write_src(exec(Exec)), nl, nl,
    % wots(S, ((((nb_current(exec_src, WS), WS \== []) -> writeln(WS); write_src(exec(TestSrc)))))),
    % Evaluates the main assertion goal.
    once(loonit_asserts1(Exec, Pro, G)).

%!  give_pass_credit(+TestSrc, +Precondition, +Goal) is det.
%
%   Marks a test as passed and updates the success counter if conditions are met.
%   This predicate either fails if assertions do not hold or logs the test as passed,
%   updating the `loonit_success` counter.
%
%   @arg TestSrc     The source identifier of the test.
%   @arg Precondition A placeholder for the precondition, not actively used here.
%   @arg Goal        The goal representing the test to be evaluated.
%
%   @example
%     % Increment success counter and log a test as passed:
%     ?- give_pass_credit('test_source', true, writeln('Goal succeeded')).
give_pass_credit(TestSrc, _Pre, _G) :-
    % This branch fails immediately, used if assertions don't hold.
    fail,
    % Retrieves base evaluation from within assertion context.
    inside_assert(TestSrc, BaseEval),
    % Executes the base evaluation.
    always_exec(BaseEval), !.
give_pass_credit(TestSrc, _Pre, G) :-
    % Logs the test as passed with 'PASS' status.
    must_det_lls((
    if_t(is_testing,ignore(write_pass_fail(TestSrc, 'PASS', G))),
    % Increments the success counter.
    flag(loonit_success, X, X + 1),
    % Displays a success message in cyan color.
    if_t((is_testing;true),color_g_mesg(cyan, ignore(write_src_wi(loonit_success(G))))))), !.

%!  write_pass_fail(+TestDetails, +Status, +Goal) is det.
%
%   Logs the test's result, including the test name, status, and key arguments
%   of the goal. This predicate structures test details and formats them for output.
%
%   @arg TestDetails A list containing information about the test source, category, and type.
%   @arg Status      A string representing the result status (e.g., 'PASS' or 'FAIL').
%   @arg Goal        The goal being evaluated, with selected arguments logged.
%
%   @example
%     % Log the details of a test with a passing status:
%     ?- write_pass_fail(['source', 'category', 'type'], 'PASS', some_goal(arg1, arg2)).
write_pass_fail([P, C| _], PASS_FAIL, G) :- !,
    % Ensures deterministic logging of the test result.
    must_det_ll((
        % Retrieves the current test number.
        loonit_number(Number),
        % Generates the test name.
        get_test_name(Number, TestName),
        % Extracts arguments from the goal for structured output.
        arg(1, G, G1),
        arg(2, G, G2),
        % Logs the formatted test name, source, category, status, and arguments.
        write_pass_fail(TestName, P, C, PASS_FAIL, G1, G2))).

write_pass_fail(LIST, PASS_FAIL, G) :- is_list(LIST), PCC = [P, _, _], member(PCC,LIST), nonvar(P), !, write_pass_fail(PCC, PASS_FAIL, G).
write_pass_fail(LIST, PASS_FAIL, G) :- is_list(LIST), PCC = [P|_], member(PCC,LIST), nonvar(P), !, write_pass_fail(PCC, PASS_FAIL, G).

%!  write_pass_fail(+TestName, +Source, +Category, +Status, +GoalArg1, +GoalArg2) is det.
%
%   Formats and appends test results to a shared log file, including metadata like
%   source, category, status, and duration. It generates a detailed, linkable log entry
%   for each test, facilitating comprehensive result tracking and HTML output per test.
%
%   @arg TestName  The name of the test being logged.
%   @arg Source    The test source identifier.
%   @arg Category  The category identifier for the test.
%   @arg Status    The result status (e.g., 'PASS' or 'FAIL').
%   @arg GoalArg1  The first argument of the goal being logged.
%   @arg GoalArg2  The second argument of the goal being logged.
%
%   @example
%     % Write formatted test results to the log file:
%     ?- write_pass_fail('Test1', 'source', 'category', 'PASS', arg1, arg2).
write_pass_fail(TestName, P, C, PASS_FAIL, G1, G2) :-
    % Check if the current file path is loaded; if not, set a default.
    ignore(((
        (nb_current(loading_file, FilePath), FilePath \== []) -> true ; FilePath = 'SOME/UNIT-TEST.metta'),
        % Concatenate for the file base.
        symbolic_list_concat([_, R], 'tests/', FilePath),
        file_name_extension(Base, _, R))),
        % Optional format output for HTML log entry.
        nop(format('<h3 id="~w">;; ~w</h3>', [TestName, TestName])),
        compiled_or_interp_html(CompOrInterp),
        % Log test details deterministically.
        must_det_ll((
            (tee_file(TEE_FILE) -> true ; 'TEE.ansi' = TEE_FILE),
            ((
                % Retrieve or create HTML file name.
                once(getenv('HTML_FILE', HTML_OUT0) ; sformat(HTML_OUT0, '~w.metta.html', [Base])),
                sformat(HTML_OUT,'~w~w',[HTML_OUT0,CompOrInterp]),
                % Compute and store a per-test HTML output.
                compute_html_out_per_test(HTML_OUT, TEE_FILE, TestName, HTML_OUT_PerTest),
                % Measure and format the duration of the last call.
                get_last_call_duration(Duration),
                DurationX1000 is Duration * 1000,
                % Write the detailed formatted log entry.
                % Optional shared units for organized logging.
             ignore((   shared_units(UNITS),
             catch(setup_call_cleanup(
                open(UNITS, append, Stream, [encoding(utf8)]),
                format(Stream,'| ~w | ~w |[~w](https://logicmoo.github.io/metta-testsuite/~w#~w) | ~@ | ~@ | ~@ | ~w | ~w |~n',
                    [TestName,PASS_FAIL,TestName,HTML_OUT,TestName,
                    trim_gstring_bar_I(write_src_woi([P,C]),600),
                    trim_gstring_bar_I(write_src_woi(G1),600),
                    trim_gstring_bar_I(write_src_woi(G2),600),
                    DurationX1000,
                    HTML_OUT_PerTest]),
                % Close the log stream
                close(Stream)),_,true))))))).


compiled_or_interp('-COMP'):- option_value('compile', 'full'),!.
compiled_or_interp('').
compiled_or_interp_html('-COMP.html'):- option_value('compile', 'full'),!.
compiled_or_interp_html('').

% Needs not to be absolute and not relative to CWD (since tests like all .metta files change their local CWD at least while "loading")

%!  output_directory(-OutputDir) is det.
%
%   Retrieves the output directory from environment variables. First, it checks
%   for the `METTALOG_OUTPUT` variable; if unset, it falls back to `OUTPUT_DIR`.
%
%   @arg OutputDir The directory path for output logs.
%
%   @example
%     % Query the output directory for logging:
%     ?- output_directory(Dir).
%     Dir = '/path/to/output'.
output_directory(OUTPUT_DIR) :- getenv('METTALOG_OUTPUT', OUTPUT_DIR), !.
output_directory(OUTPUT_DIR) :- getenv('OUTPUT_DIR', OUTPUT_DIR), !.

%!  shared_units(-UnitsFile) is det.
%
%   Retrieves the file path for shared units logging. This checks for the
%   `SHARED_UNITS` environment variable; if unset, it defaults to a standard
%   file within the output directory or `/tmp/SHARED.UNITS`.
%
%   @arg UnitsFile The path to the shared units file.
%
%   @example
%     % Get the shared units file path:
%     ?- shared_units(Units).
%     Units = '/path/to/SHARED.UNITS'.
shared_units(UNITS):- shared_units0(UNITS), exists_file(UNITS),!.

shared_units0(UNITS) :-
    % Needs not to be relative to CWD
    getenv('SHARED_UNITS', UNITS).
shared_units0(UNITS) :-
    metta_root_dir(ROOT_DIR),
    getenv('SHARED_UNITS', VAR_UNITS),
    absolute_file_name(VAR_UNITS, UNITS, [relative_to(ROOT_DIR)]).
shared_units0(UNITS) :-
    output_directory(OUTPUT_DIR),
    absolute_file_name('SHARED.UNITS', UNITS, [relative_to(OUTPUT_DIR)]).
shared_units0(UNITS) :-
    UNITS = '/tmp/SHARED.UNITS'.


% currently in a shared file per TestCase class..
%   but we might make each test dump its stuff to its own HTML file for easier spotting why test failed

%!  compute_html_out_per_test(+HTML_OUT, +TeeFile, +TestName, -HTML_OUT_PerTest) is det.
%
%   Sets the HTML output path for a specific test based on the given HTML output directory.
%
%   @arg HTML_OUT         The main HTML output directory path.
%   @arg TeeFile          A file used for tee-style output (not actively used).
%   @arg TestName         The name of the test for which output is being generated.
%   @arg HTML_OUT_PerTest The computed output path for the individual test's HTML.
%
%   @example
%     % Compute the HTML output for an individual test:
%     ?- compute_html_out_per_test('/output/main.html', _, 'Test1', HTMLPerTest).
%     HTMLPerTest = '/output/main.html'.
compute_html_out_per_test(HTML_OUT, _TEE_FILE, _TestName, HTML_OUT_PerTest) :-
    HTML_OUT = HTML_OUT_PerTest.

%!  record_call_duration(:Goal) is det.
%
%   Executes the given Goal and records the execution duration in milliseconds
%   in a global variable. This duration is updated regardless of whether the
%   Goal succeeds or fails.
%
%   @arg Goal The goal whose execution duration is to be measured.
%
%   @example
%     % Record the duration of a sample goal:
%     ?- record_call_duration((writeln('Sample goal'), sleep(1))).
record_call_duration(Goal) :-
    nb_setval('$last_call_duration', 120),
    statistics(cputime, Start),  % Get the start CPU time
    (   call(Goal)               % Call the Goal
    *-> EndResult = true         % If Goal succeeds, proceed
    ;   EndResult = false        % If Goal fails, record it but proceed
    ),
    statistics(cputime, End),    % Get the end CPU time
    Duration is End - Start,     % Calculate the CPU duration
    nb_setval('$last_call_duration', Duration),  % Set the global variable non-backtrackably
    EndResult.                   % Preserve the result of the Goal

%!  get_last_call_duration(-Duration) is det.
%
%   Retrieves the duration of the last executed goal, which is stored in a global
%   variable by `record_call_duration/1`.
%
%   @arg Duration The duration of the last goal in milliseconds.
%
%   @example
%     % Retrieve the duration of the last recorded call:
%     ?- get_last_call_duration(Dur).
%     Dur = 1.234.
get_last_call_duration(Duration) :-
    nb_getval('$last_call_duration', Duration), !.

%!  trim_gstring_bar_I(:Goal, +MaxLen) is det.
%
%   Executes a goal to generate a string, replaces certain characters, and trims
%   the result to a maximum length. This is primarily used for logging formatted
%   results within a limited display space.
%
%   @arg Goal   The goal that produces the string output.
%   @arg MaxLen The maximum allowable length for the output.
%
%   @example
%     % Trim and modify the output of a goal for display:
%     ?- trim_gstring_bar_I(writeln('Example|output\nNewline'), 10).
trim_gstring_bar_I(Goal, MaxLen) :-
    wots(String0, Goal),
    string_replace(String0, '|', 'I', String1),
    string_replace(String1, '\n', '\\n', String),
    atom_length(String, Len),
    (   Len =< MaxLen
    ->  Trimmed = String
    ;   (sub_string(String, LeftOver, MaxLen, 0, SubStr),
        format(string(Trimmed), '~w...(~w)', [SubStr, LeftOver]))
    ),
    write(Trimmed).

%! tst_cwdl(+Goal, +MaxDepth) is det.
%
%  Call Goal with a depth limit of MaxDepth. If the depth limit is exceeded,
%  an exception `over_test_resource_limit(depth_limit, MaxDepth, 1)` is thrown.
%
%  @arg Goal The Prolog goal to be called.
%  @arg MaxDepth The maximum depth allowed for the call.
%
%  @throws over_test_resource_limit(depth_limit, MaxDepth, 1)
%  If the depth limit is exceeded.
%
%  @example
%    % Successful call within depth limit
%    ?- tst_cwdl(member(X, [1,2,3]), 3).
%    X = 1 ;
%    X = 2 ;
%    X = 3.
%
%  @example
%    % Call exceeding depth limit
%    ?- tst_cwdl(member(X, [1,2,3]), 0).
%    ERROR: Unhandled exception: over_test_resource_limit(depth_limit, 0, 1)
%
tst_cwdl(Goal, _MaxDepth) :- !, call(Goal).
tst_cwdl(Goal, MaxDepth) :-
    call_with_depth_limit(Goal, MaxDepth, Result),
    cwdl_handle_result(Result, MaxDepth).

%!  cwdl_handle_result(+Result, +MaxDepth) is det.
%
%   Processes the result of a depth-limited call. If the result indicates that
%   the depth limit was exceeded, it throws an exception with details of the
%   maximum allowed depth. For other results, it succeeds without action.
%
%   @arg Result   The outcome of the depth-limited call (e.g., `depth_limit_exceeded`).
%   @arg MaxDepth The maximum allowable depth for the call.
%
%   @example
%     % Handle a depth limit exceeded result:
%     ?- cwdl_handle_result(depth_limit_exceeded, 5).
%     ERROR: over_test_resource_limit(depth_limit, 5, 1).
%
%     % Handle a successful result without exceeding the limit:
%     ?- cwdl_handle_result(success, 5).
%     true.
cwdl_handle_result(depth_limit_exceeded, MaxDepth) :-
    % If depth limit is exceeded, throw an exception with max depth details.
    !,
    throw(over_test_resource_limit(depth_limit, MaxDepth, 1)).
cwdl_handle_result(_, _).

%! tst_cwil(+Goal, +MaxInference) is det.
%
%  Call Goal with a inference limit of MaxInference. If the inference limit is exceeded,
%  an exception `over_test_resource_limit(inference_limit, MaxInference, 1)` is thrown.
%
%  @arg Goal The Prolog goal to be called.
%  @arg MaxInference The maximum inference allowed for the call.
%
%  @throws over_test_resource_limit(inference_limit, MaxInference, 1)
%  If the inference limit is exceeded.
%
%  @example
%    % Successful call within inference limit
%    ?- tst_cwil(member(X, [1,2,3]), 3).
%    X = 1 ;
%    X = 2 ;
%    X = 3.
%
%  @example
%    % Call exceeding inference limit
%    ?- tst_cwil(member(X, [1,2,3]), 0).
%    ERROR: Unhandled exception: over_test_resource_limit(inference_limit, 0, 1)
%
tst_cwil(Goal, _MaxInference) :- !, call(Goal).
tst_cwil(Goal, MaxInference) :-
    call_with_inference_limit(Goal, MaxInference, Result),
    cwil_handle_result(Result, MaxInference).

%!  cwil_handle_result(+Result, +MaxInference) is det.
%
%   Processes the result of an inference-limited call. If the inference limit
%   is exceeded, it throws an exception with details of the maximum inferences allowed.
%
%   @arg Result       The result from `call_with_inference_limit/3`, indicating success or limit exceeded.
%   @arg MaxInference The maximum inference count allowed for the call.
%
%   @example
%     % Handle an inference limit exceeded result:
%     ?- cwil_handle_result(inference_limit_exceeded, 5).
%     ERROR: Unhandled exception: over_test_resource_limit(inference_limit, 5, 1).
%
%     % Handle a result within inference limits:
%     ?- cwil_handle_result(success, 5).
%     true.
cwil_handle_result(inference_limit_exceeded, MaxInference) :-
    % If inference limit is exceeded, throw an exception with max inference details.
    !,
    throw(over_test_resource_limit(inference_limit, MaxInference, 1)).
cwil_handle_result(_, _).

%! tst_cwtl(+Goal, +TimeLimit) is det.
%
%  Call Goal with a time limit of TimeLimit seconds. If the time limit is exceeded,
%  an exception `over_test_resource_limit(time_limit, TimeLimit, exceeded)` is thrown.
%
%  @arg Goal The Prolog goal to be called.
%  @arg TimeLimit The maximum time allowed for the call in seconds.
%
%  @throws over_test_resource_limit(time_limit, TimeLimit, exceeded)
%  If the time limit is exceeded.
%
%  @example
%    % Successful call within time limit
%    ?- tst_cwtl((sleep(1), writeln('Completed')), 2).
%    Completed
%    true.
%
%  @example
%    % Call exceeding time limit
%    ?- tst_cwtl((sleep(2), writeln('Completed')), 1).
%    ERROR: Unhandled exception: over_test_resource_limit(time_limit, 1, exceeded)
%
tst_cwtl(Goal, TimeLimit) :-
    catch(call_with_time_limit(TimeLimit,Goal),time_limit_exceeded,throw(over_test_resource_limit(time_limit, TimeLimit, exceeded))).

%!  test_alarm is det.
%
%   Tests for alarm-triggered time limits. Sets a time limit of 0.5 seconds for
%   a goal. If the limit is exceeded, the exception is caught, and a "passed"
%   message is displayed; otherwise, it displays "failed."
%
%   @example
%     % Run the alarm test:
%     ?- test_alarm.
%     % Output depends on whether the time limit is exceeded.
test_alarm :-
    !.
test_alarm :-
    % Set time limit and attempt goal within 0.5 seconds.
    (catch(
        % Run goal with time limit; fail if it exceeds limit.
        (call_with_time_limit(0.5,
            (forall(between(1, 15, _), sleep(0.1)),
            write_src_uo(failed_test_alarm)))),
        % If limit is exceeded, catch exception and display "passed."
        time_limit_exceeded,
        write_src_uo(passed_test_alarm)
    )).

%!  loonit_divisor(-TestNumber) is det.
%
%   Retrieves the current test number, defaulting to 1 if no tests have run.
%   This ensures a minimum divisor of 1.
%
%   @arg TestNumber The divisor, based on the current test number or 1 if unset.
%
%   @example
%     % Get the test divisor:
%     ?- loonit_divisor(Divisor).
%     Divisor = 1.
loonit_divisor(TestNumber) :-
    % Get current test number or default to 1 if unset.
    loonit_number(TN),
    (TN > 0 -> TestNumber = TN ; TestNumber = 1),
    !.

%! compute_available_time(-ActualTimeout) is det.
%
%  Computes the actual timeout based on the available timeout and test number,
%  ensuring it is at least 4 seconds.
%
%  @arg ActualTimeout The computed timeout value, ensuring a minimum of 4 seconds.
%
%  @example
%    % Assuming `loonit_number/1` returns 3 and available timeout is 120 seconds:
%    ?- compute_available_time(ActualTimeout).
%    ActualTimeout = 38.0.
%
%  @example
%    % With a smaller available timeout:
%    ?- option_else(timeout, "20", _), compute_available_time(ActualTimeout).
%    ActualTimeout = 4.0.
%
compute_available_time(ActualTimeout) :-
    option_else(timeout, Was, fake), Was == fake,!,ActualTimeout=3600.
compute_available_time(ActualTimeout) :-
    loonit_divisor(TestNumber),
    option_else(timeout, AvailableTimeoutStr, "120"),
    into_number(AvailableTimeoutStr, AvailableTimeout),
    ComputedTimeout is (AvailableTimeout / TestNumber) - 2,
    max_min(4, ComputedTimeout, ActualTimeout, _), !.

%! tst_call_limited(+Goal) is det.
%
%  Calls the Goal with a depth limit of 1000 and a time limit calculated based on
%  the available timeout divided by the test number, ensuring the timeout is at least 4 seconds.
%
%  @arg Goal The Prolog goal to be called.
%
%  @throws over_test_resource_limit(time_limit, ActualTimeout, exceeded)
%  if the time limit is exceeded.
%  @throws over_test_resource_limit(depth_limit, 1000, 1)
%  if the depth limit is exceeded.
%
%  @example
%    % Run a goal within the calculated limits:
%    ?- tst_call_limited(member(X, [1,2,3])).
%    X = 1 ;
%    X = 2 ;
%    X = 3.
%
%  @example
%    % Run a goal that exceeds the calculated time limit:
%    ?- tst_call_limited(sleep(20)).
%    red: Exception: over_test_resource_limit(sleep(20), time_limit, 4, exceeded)
%    false.
%
tst_call_limited(Goal) :-
    % notrace(write_src_uo(tst_call_limited(Goal))),
    compute_available_time(ActualTimeout),
    catch(
        % Apply the time limit, depth limit, inference limit
        tst_cwtl(tst_cwil(tst_cwdl(Goal, 300), 1_000_000_000), ActualTimeout),
        Exception,
        (ansi_format([fg(red)],'~n~n~q~n~n',[failing(Goal, Exception)]), !, fail)
    ).

%!  loonit_asserts1(+TestSrc, +Precondition, +Goal) is det.
%
%   Executes the Goal with a given precondition and records its duration.
%   If the Goal succeeds, it gives pass credit; if it fails, it logs the failure.
%
%   @arg TestSrc     The source identifier for the assertion.
%   @arg Precondition The precondition to be met before running the Goal.
%   @arg Goal        The goal to be asserted.
%
%   @example
%     % Run an assertion and handle pass/fail logging:
%     ?- loonit_asserts1('source', true, writeln('Goal executed successfully')).
loonit_asserts1(TestSrc, Pre, G) :-
    % Run precondition and record duration of Goal execution.
    _ = nop(Pre),
    record_call_duration(G),
    % Log as passed if Goal succeeds.
    must_det_lls(give_pass_credit(TestSrc, Pre, G)),
    !.
/*
loonit_asserts1(TestSrc,Pre,G) :-  fail,
    sub_var_safely('BadType',TestSrc), \+ check_type,!,
    write('\n!check_type (not considering this a failure)\n'),
    color_g_mesg('#D8BFD8',write_src_wi(loonit_failureR(G))),!,
    ignore(((
       option_value('on-fail','trace'),
       setup_call_cleanup(debug(metta(eval)),call((Pre,G)),nodebug(metta(eval)))))).
*/
loonit_asserts1(TestSrc, Pre, G) :-
    % Handle failed Goal by logging, flagging failure, and optionally tracing.
    must_det_ll((
        color_g_mesg(red, write_src_wi(loonit_failureR(G))),
        write_pass_fail(TestSrc, 'FAIL', G),
        %display(G),
        flag(loonit_failure, X, X + 1),
        % Optional trace or REPL on failure based on settings.
        if_t(option_value('on-fail', 'repl'), repl),
        if_t(option_value('on-fail', 'trace'),
            setup_call_cleanup(debug(metta(eval)), call((Pre, G)), nodebug(metta(eval)))
        )
    )),
    devel_trace(loonit_failureR,G).
    % (thread_self(main)->trace;sleep(0.3)

devel_trace(_,Why):- ignore(( fail, gethostname(X),X=='HOSTAGE',!,
  copy_term(Why,Cp,Goals),Cp=Why, trace, Goals=_,Cp=_, call(Why))).

% Generate loonit report with colorized output
:- dynamic(gave_loonit_report/0).

%!  loonit_report is det.
%
%   Generates a colorized report of test successes and failures, ensuring the
%   report is only generated once. If there are no successes or any failures,
%   optionally launches a REPL based on settings.
%
%   @example
%     % Generate a loonit report:
%     ?- loonit_report.
loonit_report :-
    % Skip if report already generated in this session.
    gave_loonit_report,
    !.
loonit_report :-
    % Mark the report as generated.
    assert(gave_loonit_report),
    % Retrieve current counts of successes and failures.
    flag(loonit_success, Successes, Successes),
    flag(loonit_failure, Failures, Failures),
    % Display the report based on these counts.
    loonit_report(Successes, Failures),
    % If no successes or any failures, open REPL if settings permit.
    if_t((Successes == 0 ; Failures > 0),
         if_t(option_value(repl, failures) ; option_value(frepl, true), repl)).

% Ensure loonit report runs at program halt.
:- at_halt(loonit_report).

%!  loonit_report(+Successes, +Failures) is det.
%
%   Outputs a formatted report of test results with colorized success and failure counts.
%
%   @arg Successes The count of successful tests.
%   @arg Failures  The count of failed tests.
%
%   @example
%     % Display the report with specified counts:
%     ?- loonit_report(5, 2).
loonit_report(0, 0) :-
    !. % Skip report if no successes or failures.
loonit_report(Successes, Failures) :-
    % Display report header in bold.
    ansi_format([bold], 'LoonIt Report~n', []),
    format('------------~n'),
    % Display success count in green.
    ansi_format([fg(green)], 'Successes: ~w~n', [Successes]),
    % Display failure count in red if any; otherwise, green.
    ((integer(Failures), Failures > 0)
     -> ansi_format([fg(red)], 'Failures: ~w~n', [Failures])
     ; ansi_format([fg(green)], 'Failures: ~w~n', [Failures])).

%!  loon_metta(+File) is det.
%
%   Resets test counters, loads a specified file, and generates a status report.
%   This is useful for reinitializing test metrics before loading files.
%
%   @arg File The file to load and evaluate.
%
%   @example
%     % Load a file and generate a status report:
%     ?- loon_metta('test_file.pl').
loon_metta(File) :-
    % Save current success and failure counts, then reset counters.
    flag(loonit_success, WasSuccesses, 0),
    flag(loonit_failure, WasFailures, 0),
    % Load the specified file.
    load_metta(File),
    % Generate report after loading.
    loonit_report,
    % Restore previous success and failure counts.
    flag(loonit_success, _, WasSuccesses),
    flag(loonit_failure, _, WasFailures),
    !.

% dynamic means that the predicate can be modified at runtime.
:- dynamic(file_answers/3).
:- dynamic(file_exec_num/2).

%!  set_exec_num(+SFileName, +Val) is det.
%
%   Updates or asserts the execution number for the specified file. If an execution
%   number already exists for the file, it is replaced with the new value; otherwise,
%   the value is asserted as a new entry.
%
%   @arg SFileName The source file name, which is converted to an absolute path.
%   @arg Val       The execution number to set for the file.
%
%   @example
%     % Set the execution number for a file:
%     ?- set_exec_num('test_file.pl', 3).
set_exec_num(SFileName, Val) :-
    % Convert to absolute file path.
    absolute_file_name(SFileName, FileName),
    % If an entry exists for FileName, retract it; otherwise, do nothing.
    (   retract(file_exec_num(FileName, _))
    ->  true
    ;   true
    ),
    % Assert the new execution number for FileName.
    asserta(file_exec_num(FileName, Val)).

%!  get_exec_num(-Val) is det.
%
%   Retrieves the execution number for the current file. If no execution number
%   is set for the current file, it returns 0.
%
%   @arg Val The current execution number for the file or 0 if not set.
%
%   @example
%     % Retrieve the execution number for the current file, defaulting to 0:
%     ?- get_exec_num(Val).
%     Val = 3.
get_exec_num(Val) :-
    % Get the absolute path of the current file.
    current_exec_file_abs(FileName),
    % Retrieve the execution number for FileName, stopping after one result.
    file_exec_num(FileName, Val),
    !.

%!  get_exec_num(+FileName, -Val) is det.
%
%   Retrieves the execution number for the specified file. If none exists, it returns 0.
%
%   @arg FileName The file name for which to retrieve the execution number.
%   @arg Val      The current execution number or 0 if not set.
%
%   @example
%     % Retrieve the execution number for a file, defaulting to 0 if not set:
%     ?- get_exec_num('test_file.pl', Val).
%     Val = 3.
get_exec_num(FileName, Val) :-
    % If an entry exists for FileName, retrieve its value; otherwise, return 0.
    (   file_exec_num(FileName, CurrentVal)
    ->  Val = CurrentVal
    ;   Val = 0
    ).

%!  current_exec_file_abs(-FileName) is det.
%
%   Retrieves the absolute path of the currently executing file.
%
%   @arg FileName The absolute file path of the current execution file.
%
%   @example
%     % Get the absolute path of the current execution file:
%     ?- current_exec_file_abs(FileName).
current_exec_file_abs(FileName) :-
    % Obtain the file name of the current execution file and convert it to absolute path.
    current_exec_file(SFileName),
    absolute_file_name(SFileName, FileName),
    !.

%!  get_expected_result(-Ans) is det.
%
%   Retrieves the expected result (answer) for the current file and execution number,
%   if it is available.
%
%   @arg Ans The expected answer for the current file execution.
%
%   @example
%     % Retrieve the expected answer for the current file and execution:
%     ?- get_expected_result(Ans).
get_expected_result(Ans) :-
    ignore((
        % Get absolute file name, execution number, and retrieve answer.
        current_exec_file_abs(FileName),
        file_exec_num(FileName, Nth),
        file_answers(FileName, Nth, Ans)
    )),
    !.

%!  got_exec_result(+Val) is det.
%
%   Records the actual result (`Val`) of the current execution and compares it with
%   the expected result. If the result matches, it logs a pass; otherwise, it logs a fail.
%
%   @arg Val The actual result produced during the current execution.
%
%   @example
%     % Record and evaluate an execution result:
%     ?- got_exec_result(Result).
got_exec_result(Val) :-
    ignore((
        % Get file name, execution number, expected answer, and evaluate result.
        current_exec_file_abs(FileName),
        file_exec_num(FileName, Nth),
        file_answers(FileName, Nth, Ans),
        got_exec_result(Val, Ans)
    )).

%!  got_exec_result(+Val, +Ans) is det.
%
%   Compares the actual result (`Val`) with the expected result (`Ans`). If the results
%   match, it logs a pass; otherwise, it logs a fail.
%
%   @arg Val The actual result produced.
%   @arg Ans The expected result.
%
%   @example
%     % Compare an actual result with the expected answer:
%     ?- got_exec_result(actual_val, expected_ans).
got_exec_result(Val, Ans) :-
    must_det_ll((
        % Get file name, execution number, and test name for logging.
        current_exec_file_abs(FileName),
        file_exec_num(FileName, Nth),
        Nth100 is Nth + 100,
        get_test_name(Nth100, TestName),
        % Retrieve execution context and compare actual vs. expected result.
        nb_current(exec_src, Exec),
        (equal_enough_for_test(Val, Ans)
         -> write_pass_fail_result(TestName, exec, Exec, 'PASS', Ans, Val)
          ; write_pass_fail_result(TestName, exec, Exec, 'FAIL', Ans, Val)
        )
    )).

%!  write_pass_fail_result(+TestName, +Exec, +ExecContext, +PassFail, +Ans, +Val) is det.
%
%   Logs the result of a test, specifying whether it passed or failed.
%
%   @arg TestName     The name of the test.
%   @arg Exec         Execution identifier (e.g., 'exec').
%   @arg ExecContext  The context of the execution.
%   @arg PassFail     Result status ('PASS' or 'FAIL').
%   @arg Ans          The expected result.
%   @arg Val          The actual result produced.
%
%   @example
%     % Log the result of a test as pass or fail:
%     ?- write_pass_fail_result('Test1', exec, context, 'PASS', expected_ans, actual_val).
write_pass_fail_result(TestName, exec, Exec, PASS_FAIL, Ans, Val) :-
    % Output and log the result as a pass or fail.
    nl, writeq(write_pass_fail_result(TestName, exec, Exec, PASS_FAIL, Ans, Val)), nl,
    write_pass_fail(TestName, exec, Exec, PASS_FAIL, Ans, Val).

%!  current_exec_file(-FileName) is det.
%
%   Retrieves the current execution file name if set.
%
%   @arg FileName The current file name in execution.
%
%   @example
%     % Get the current file in execution:
%     ?- current_exec_file(FileName).
current_exec_file(FileName) :-
    nb_current(loading_file, FileName).

%!  inc_exec_num(+FileName) is det.
%
%   Increments the execution number for the given file. If no entry exists for
%   the file, it initializes the execution number to 1.
%
%   @arg FileName The name of the file for which to increment the execution number.
%
%   @example
%     % Increment the execution number for the current file:
%     ?- inc_exec_num('test_file.pl').
inc_exec_num :-
    % Get the absolute path of the current execution file.
    current_exec_file_abs(FileName),
    !,
    inc_exec_num(FileName).
inc_exec_num(FileName) :-
    % If an entry exists, increment its value; otherwise, set it to 1.
    (   retract(file_exec_num(FileName, CurrentVal))
    ->  NewVal is CurrentVal + 1
    ;   NewVal = 1
    ),
    % Assert the updated execution number.
    asserta(file_exec_num(FileName, NewVal)).

%!  load_answer_file(+File) is det.
%
%   Loads the answer file specified by `File`, handling path resolution and
%   initialization of execution tracking. If the file does not exist or is not
%   specified as an absolute path, it attempts to resolve it.
%
%   @arg File The path to the answer file to load.
%
%   @example
%     % Load an answer file with automatic path resolution:
%     ?- load_answer_file('answers_file.ans').
load_answer_file(Base) :-
    calc_answer_file(Base,File),
    load_answer_file_now(File),
    !.



calc_answer_file(_Base,AnsFile):- getenv(hyperon_results,AnsFile),exists_file(AnsFile),!.
calc_answer_file(RelFile,AnsFile):- nonvar(RelFile),
    (   \+ atom(RelFile); \+ is_absolute_file_name(RelFile); \+ exists_file(RelFile)),
    % Resolve to an absolute file path if necessary.
    absolute_file_name(RelFile, MidFile), RelFile\=@=MidFile,
    exists_file(MidFile),
    calc_answer_file(MidFile,AnsFile),!.
calc_answer_file(AnsFile,AnsFile):- \+ atom_concat(_, metta, AnsFile),!.
% Finds a file using expand_file_name for wildcard matching.
calc_answer_file(MeTTaFile,AnsFile):-  atom_concat(MeTTaFile, '.?*', Pattern),
        expand_file_name(Pattern, Matches), member(AnsFile,Matches), ok_ansfile_name(AnsFile),  !. % Select the first match
calc_answer_file(Base,AnsFile):- ensure_extension(Base, answers, AnsFile),!.

ok_ansfile_name(AnsFile):- \+ symbol(AnsFile),!,fail.
ok_ansfile_name(AnsFile):- skip_ans_ext(Htm),symbol_concat(_,Htm,AnsFile),!,fail.
ok_ansfile_name(_).
skip_ans_ext(Htm):- arg(_, v('tmp', 'bak', 'html', '~', 'sav', 'ansi', 'pl', 'metta',
             'py', 'txt', 'md', 'tee', 'o', 'dll', 'so', 'exe', 'sh', 'text', 'rc',
            'mettalogrc', 'bat', 'c', 'java', 'datalog', 'in', 'out', 'xml', 'obo'), Htm).


%!  load_answer_file_now(+File) is det.
%
%   Processes and loads the specified answer file, initializing or updating
%   execution tracking for the file.
%
%   @arg File The file path of the answer file to load.
%
%   @example
%     % Begin loading an answer file, initializing execution tracking:
%     ?- load_answer_file_now('/path/to/answers_file.ans').

load_answer_file_now(Base) :-
    calc_answer_file(Base, AnsFile),
    ignore((
        % Ensure correct file extension for result storage files.
        file_name_extension(StoredAs, _, AnsFile),
        % Initialize execution count and start loading.
        set_exec_num(StoredAs, 1),
        fbug(load_answer_file(AnsFile, StoredAs)),
        load_answer_file(AnsFile, StoredAs)
    )).

%!  load_answer_file(+AnsFile, +StoredAs) is det.
%
%   Loads answers from `AnsFile` into the system under the identifier `StoredAs`.
%   If answers are already loaded or the file does not exist, it skips loading.
%
%   @arg AnsFile  The path of the answer file to load.
%   @arg StoredAs The identifier under which the answers are stored.
%
%   @example
%     % Load answers from a file into the system under an identifier:
%     ?- load_answer_file('answers_file.ans', 'stored_as').
load_answer_file(AnsFile, StoredAs) :-
    % If answers are already loaded or file is absent, skip loading.
    (   file_answers(StoredAs, _, _)
    ->  true
    ;   (   \+ exists_file(AnsFile)
        ->  true
        ;   % Open file and load answers from stream.
            (setup_call_cleanup(
                open(AnsFile, read, Stream, [encoding(utf8)]),
                (load_answer_stream(1, StoredAs, Stream)),
                close(Stream))
            )
        )
    ),
    % Initialize execution number after loading.
    set_exec_num(StoredAs, 1),
    !.

% This allows Prolog to print debug information related to the metta(answers) topic
:- debug(metta(answers)).

%!  load_answer_stream(+Nth, +StoredAs, +Stream) is det.
%
%   Reads and loads answers from a given stream, associating each answer with the
%   identifier `StoredAs` and an index `Nth`. If the end of the stream is reached,
%   it lists all loaded answers for debugging if `answers` tracing is enabled.
%
%   @arg Nth      The index of the current answer being read.
%   @arg StoredAs The identifier under which the answers are stored.
%   @arg Stream   The input stream from which answers are read.
%
%   @example
%     % Load answers from a stream and associate them with an identifier:
%     ?- open('answers.txt', read, Stream), load_answer_stream(1, 'stored_as', Stream).
load_answer_stream(_Nth, StoredAs, Stream) :-
    % Stop if end of the stream is reached, optionally listing loaded answers.
    at_end_of_stream(Stream),
    !,
    if_trace((answers), prolog_only(listing(file_answers(StoredAs, _, _)))).
load_answer_stream(Nth, StoredAs, Stream) :-
    % Read a line from the stream and process it recursively.
    read_line_to_string(Stream, String),
    load_answer_stream(Nth, StoredAs, String, Stream).

%!  load_answer_stream(+Nth, +StoredAs, +String, +Stream) is det.
%
%   Processes a string read from the stream, parsing it as an answer, storing
%   it with `StoredAs`, and continuing to the next line. If parsing is successful,
%   it stores the answer using `pfcAdd_Now/1` with an incremented index.
%
%   @arg Nth      The index of the current answer being processed.
%   @arg StoredAs The identifier under which the answer is stored.
%   @arg String   The string representation of the answer.
%   @arg Stream   The input stream for reading additional lines if needed.
%
%   @example
%     % Process an answer string from the stream and store it:
%     ?- load_answer_stream(1, 'stored_as', "[Answer]", Stream).

/*
load_answer_stream(Nth, StoredAs, String, Stream) :- fail,
    atom_chars(String, Chars),
    count_brackets(Chars, 0, 0, Balance),
    (   Balance =< 0
    ->  StoredAs = String
    ;   read_line_to_string(Stream, NextString),
        string_concat(String, "\n", StringWithNewLine),
        string_concat(StringWithNewLine, NextString, CombinedString),
        load_answer_stream(Nth, StoredAs, CombinedString, Stream)
    ).
*/
load_answer_stream(Nth, StoredAs, String, Stream) :- % string_concat("[", _, String), !
    % Debugging statement to show the answer being processed.
    fbug(Nth = String),
    % Parse answer string into a Prolog term.
    parse_answer_string(String, Metta),
    !,
    % Store the parsed answer.
    pfcAdd_Now(file_answers(StoredAs, Nth, Metta)),
    % Skip if the answer contains a comma.
    skip(must_det_ll(\+ sub_var_safely(',', Metta))),
    % Increment index and continue processing next line.
    Nth2 is Nth + 1,
    load_answer_stream(Nth2, StoredAs, Stream).
load_answer_stream(Nth, StoredAs, _, Stream) :-
    % Fall back to reading the next line if no answer is processed.
    load_answer_stream(Nth, StoredAs, Stream).
/*
count_brackets([], Open, Close, Balance) :- !,
    Balance is Open - Close.
count_brackets([Char|Rest], Open, Close, Balance) :-
    ((((   Char == '['
    ->  NewOpen is Open + 1
       ;   (Char == ']'
    ->  NewClose is Close + 1
      ;   (NewOpen = Open,
          NewClose = Close)))))),
      count_brackets(Rest, NewOpen, NewClose, Balance).
*/

%!  parse_answer_string(+String, -Metta) is nondet.
%
%   Parses a given String and converts it into a Prolog term `Metta`.
%   This predicate handles various formats of input strings, performing
%   specific parsing based on the format. If the String matches certain
%   error patterns, the predicate will fail.
%
%   @arg String The input string that represents some answer or assertion result.
%   @arg Metta  The output variable where the parsed result will be unified, if parsing is successful.
%
%   @example Parsing an empty list:
%       ?- parse_answer_string("[]", Result).
%       Result = [].
%
%   @example Handling an error assertion:
%       ?- parse_answer_string("[(Error (assert ...))]", Result).
%       false.
%

% Parse an empty list, unifying with an empty list if the string is "[]".
parse_answer_string("[]", []) :- !.
% parse_answer_string(String, Metta) :- string_concat("(", _, String), !, parse_sexpr_metta(String, Metta), !.
% Fail if the string starts with an assertion error pattern.
parse_answer_string(String, _Metta) :- string_concat("[(Error (assert", _, String), !, fail.
% Fail if the string begins with "Expected: [" and contains an expected inner pattern.
parse_answer_string(String, _Metta) :- string_concat("Expected: [", Mid, String), string_concat(_Expected_Inner, "]", Mid), !, fail.
% Parse a `Got` response by extracting the inner content from "Got: [ ... ]".
parse_answer_string(String, Metta) :- string_concat("Got: [", Mid, String), string_concat(Got_Inner, "]", Mid), !, parse_answer_inner(Got_Inner, Metta).
% Parse generic bracketed content by extracting the inner part from "[ ... ]".
parse_answer_string(String, Metta) :- string_concat("[", Mid, String), string_concat(Inner0, "]", Mid), !, parse_answer_inner(Inner0, Metta).

%!  parse_answer_inner(+Inner0, -Metta) is det.
%
%   Converts the content of `Inner0` into a Prolog term `Metta` by replacing specific patterns,
%   parsing the modified string, and conditionally skipping processing if certain variables are detected.
%
%   @arg Inner0 The input string to parse.
%   @arg Metta  The resulting parsed Prolog term.
%
%   @example
%       ?- parse_answer_inner("some,content", Result).
%       Result = parsed_term.
%

parse_answer_inner(Inner0, Metta) :-
    must_det_ll((
        % Replace specific character patterns in Inner0 to create Inner.
        replace_in_string([', '=' , '], Inner0, Inner),
        % Parse modified string Inner into Metta.
        parse_answer_str(Inner, Metta),
        % Skip processing if Metta meets the specified condition.
        skip((\+ sub_var_safely(',', rc(Metta))))
    )).

%!  parse_answer_str(+Inner0, -Metta) is det.
%
%   Parses the content of `Inner0` into a structured Prolog term `Metta` by handling various formats.
%   Depending on the format, it may apply transformations, handle comma removal, and check for
%   certain variable conditions.
%
%   @arg Inner0 The input string to parse.
%   @arg Metta  The resulting parsed Prolog term.
%
%   @example
%       ?- parse_answer_str("some content", Result).
%       Result = parsed_term.
%

% Parse a string with specific formatting, building the term as a list starting with C.
parse_answer_str(Inner, [C|Metta]) :-
    atomics_to_string(["(", Inner, ")"], Str),parse_sexpr_metta(Str, CMettaC), CMettaC = [C|MettaC],
    % Remove commas from MettaC to create Metta, if conditions are met.
    ((remove_m_commas(MettaC, Metta),\+ sub_var_safely(',', rc(Metta)))).
% Handle concatenated symbols in Inner0 by converting them into a list and parsing each element.
parse_answer_str(Inner0, Metta) :- symbolic_list_concat(InnerL, ' , ', Inner0),
    maplist(atom_string, InnerL, Inner),maplist(parse_sexpr_metta, Inner, Metta),
    skip((must_det_ll(( \+ sub_var_safely(',', rc2(Metta)))))), !.
% Apply replacements in Inner0 and parse as a single expression.
parse_answer_str(Inner0, Metta) :-
    ((replace_in_string([' , '=' '], Inner0, Inner),atomics_to_string(["(", Inner, ")"], Str), !,
    parse_sexpr_metta(Str, Metta), !,skip((must_det_ll(\+ sub_var_safely(',', rc3(Metta))))),
    skip((\+ sub_var_safely(',', rc(Metta)))))).
%parse_answer_string(String,Metta):- String=Metta,!,fail.

parse_sexpr_metta(Str,Metta):- notrace(catch(parse_sexpr_untyped(Str,Metta),_,fail)).

%!  remove_m_commas(+InputList, -OutputList) is det.
%
%   Removes specific elements (such as commas or "and") from `InputList`, creating `OutputList`.
%   If `InputList` does not contain any commas as variables, `OutputList` is identical to `InputList`.
%
%   @arg InputList  The list to process, potentially containing unwanted elements.
%   @arg OutputList The resulting list with specific elements removed.
%
%   @example
%       ?- remove_m_commas([and, item1, ',', item2, and, item3], Result).
%       Result = [item1, item2, item3].
%

% Return the list as-is if it contains no commas as variables.
remove_m_commas(Metta, Metta) :- \+ sub_var_safely(',', Metta), !.
% Remove 'and' from the beginning of the list and continue processing.
remove_m_commas([C, H | T], [H | TT]) :- C == 'and', !, remove_m_commas(T, TT).
% Remove ',' from the beginning of the list and continue processing.
remove_m_commas([C, H | T], [H | TT]) :- C == ',', !, remove_m_commas(T, TT).
% Process remaining elements recursively.
remove_m_commas([H | T], [H | TT]) :- !, remove_m_commas(T, TT).

%!  change_extension(+OriginalFileName, +NewExtension, -NewBaseName) is det.
%
%   Changes the file extension of `OriginalFileName` to `NewExtension`, producing `NewBaseName`.
%   This predicate extracts the base name without the original extension and appends the
%   specified `NewExtension` to create the new file name.
%
%   @arg OriginalFileName The original file path with an extension to be changed.
%   @arg NewExtension     The new extension to use for the file.
%   @arg NewBaseName      The resulting file name with the new extension.
%
%   @example
%       ?- change_extension('path/to/myfile.txt', 'pdf', NewFileName).
%       NewFileName = 'path/to/myfile.pdf'.
%
change_extension(OriginalFileName, NewExtension, NewBaseName) :-
    % Split the original file name to extract the base without its extension.
    file_name_extension(BaseWithoutExt, _, OriginalFileName),
    % Create a new file name by appending the new extension to the base.
    file_name_extension(BaseWithoutExt, NewExtension, NewBaseName), !.

%!  ensure_extension(+OriginalFileName, +Extension, -NewFileName) is det.
%
%   Ensures that `OriginalFileName` has the specified `Extension`. If it already has the extension,
%   `NewFileName` is identical to `OriginalFileName`. Otherwise, the `Extension` is appended.
%
%   @arg OriginalFileName The original file path, potentially with or without the desired extension.
%   @arg Extension        The required extension for the file.
%   @arg NewFileName      The resulting file name, with the ensured extension.
%
%   @example
%       ?- ensure_extension('path/to/myfile', 'txt', NewFileName).
%       NewFileName = 'path/to/myfile.txt'.
%
ensure_extension(OriginalFileName, Extension, NewFileName) :-
    % Extract the current extension of the file, if any.
    file_name_extension(_, CurrentExt, OriginalFileName),
    % If the current extension matches the desired one, keep the original file name.
    (   CurrentExt = Extension
    ->  NewFileName = OriginalFileName
    % Otherwise, append the new extension to create NewFileName.
    ;   atom_concat(OriginalFileName, '.', TempFileName),
        atom_concat(TempFileName, Extension, NewFileName)
    ).

% Example usage:
% ?- remove_specific_extension('path/to/myfile.txt', 'txt', NewFileName).
% NewFileName = 'path/to/myfile'.

%!  remove_specific_extension(+OriginalFileName, +Extension, -FileNameWithoutExtension) is det.
%
%   Removes a specific extension from `OriginalFileName` if it matches `Extension`.
%   If `OriginalFileName` does not have the specified `Extension`, it is returned unchanged.
%
%   @arg OriginalFileName         The original file path, possibly with an extension.
%   @arg Extension                The specific extension to remove.
%   @arg FileNameWithoutExtension The resulting file name without the specified extension.
%
%   @example
%       ?- remove_specific_extension('path/to/myfile.txt', 'pdf', NewFileName).
%       NewFileName = 'path/to/myfile.txt'.
%
remove_specific_extension(OriginalFileName, Extension, FileNameWithoutExtension) :-
    % Extract the extension of the file, if any.
    file_name_extension(FileNameWithoutExtension, Ext, OriginalFileName),
    % If the extracted extension matches the specified one, return the base name;
    % otherwise, retain the original file name.
    ( Ext = Extension -> true ; FileNameWithoutExtension = OriginalFileName ).

%!  quick_test is det.
%
%   Runs a quick test by executing each test case in `quick_test/1` and loading it
%   into the system via `load_metta_stream/2`. Each test case is opened as a string
%   stream and processed with a predefined entity identifier `&self`.
%
%   This predicate is intended for streamlined testing by iterating over all
%   available quick test cases.
%
%   @example
%       ?- quick_test.
%       % Runs all quick tests in quick_test/1 through load_metta_stream.
%
quick_test :-
    % For each test in quick_test/1, open the test as a stream and load it.
    forall(quick_test(Test),
           forall(open_string(Test, Stream),
                  load_metta_stream('&self', Stream))).

/*
 tests for term expander

*/
% :- debug(term_expansion).

% Enable conditional compilation if debugging for term expansion is active.
:- if((false, debugging(term_expansion))).
% Enable ARC-specific term expansions if the condition is met.
:- enable_arc_expansion.
% Suppress warnings about singleton variables in this section.
:- style_check(-singleton).
% Define various test cases for deterministic term expansion.
% Each `dte` clause represents a different expansion or assertion pattern.
% Set a local variable.
dte :- set(_X.local) = val.
% Set a global variable.
dte :- gset(_X.global) = gval.
% Assert that setting `_X.a` to `b` must succeed deterministically.
dte :- must_det_ll((set(_X.a) = b)).
% Use `must_det_ll` to ensure that `nb_setval/2` runs locally and call `dte` recursively
% with a modified term involving `X.tail`.
dte :- must_det_ll(locally(b_setval(e, X.locally), dte([foo | set(X.tail)]))).
% Check if `set(V.element)` is a member of `set(V.list)`.
dte :- member(set(V.element), set(V.list)).
% Define a specific expansion for `dte/1` with input `set(E.v)` when `set(E.that)` equals `v`.
dte(set(E.v)) :- set(E.that) = v.
% Restore the default singleton variable warnings.
:- style_check(+singleton).
% Disable ARC-specific term expansions after this section.
:- disable_arc_expansion.
% List all clauses of `dte/1` for inspection.
:- listing(dte).
% End the conditional compilation.
:- endif.



































%!  factorial_recursive(+N, -Result) is det.
%
%   Computes the factorial of N using a simple recursive approach.
%   This predicate multiplies N by the factorial of (N-1) until it reaches 0.
%
%   @arg N      The non-negative integer for which to calculate the factorial.
%   @arg Result The resulting factorial value of N.
%
%   @example
%       ?- factorial_recursive(5, Result).
%       Result = 120.
%
factorial_recursive(0, 1).
factorial_recursive(N, Result) :-
    N > 0,N1 is N - 1,factorial_recursive(N1, Result1),Result is N * Result1.

%!  factorial_tail_recursive(+N, -Result) is det.
%
%   Computes the factorial of N using a tail-recursive approach with an accumulator.
%   This method is optimized for large values of N due to tail-call optimization.
%
%   @arg N      The non-negative integer for which to calculate the factorial.
%   @arg Result The resulting factorial value of N.
%
%   @example
%       ?- factorial_tail_recursive(5, Result).
%       Result = 120.
%
factorial_tail_recursive(N, Result) :- factorial_tail_helper(N, 1, Result).

%!  factorial_tail_helper(+N, +Acc, -Result) is det.
%
%   Helper predicate for factorial_tail_recursive/2 that accumulates the result
%   in `Acc`, allowing the computation to proceed in a tail-recursive manner.
%
%   @arg N      The current value being processed in the factorial calculation.
%   @arg Acc    The accumulator holding the intermediate factorial result.
%   @arg Result The final factorial value.
%
factorial_tail_helper(0, Acc, Acc).
factorial_tail_helper(N, Acc, Result) :-
    N > 0,NewAcc is Acc * N,N1 is N - 1,factorial_tail_helper(N1, NewAcc, Result).

%!  factorial_accumulator(+N, -Result) is det.
%
%   Computes the factorial of N using an accumulator-based approach,
%   accumulating the result in a helper predicate.
%
%   @arg N      The non-negative integer for which to calculate the factorial.
%   @arg Result The resulting factorial value of N.
%
%   @example
%       ?- factorial_accumulator(5, Result).
%       Result = 120.
%
factorial_accumulator(N, Result) :- factorial_acc(N, 1, Result).

%!  factorial_acc(+N, +Acc, -Result) is det.
%
%   Helper predicate for factorial_accumulator/2 that uses an accumulator
%   to store intermediate results, iterating until N reaches 0.
%
%   @arg N      The current value being processed in the factorial calculation.
%   @arg Acc    The accumulator holding the intermediate factorial result.
%   @arg Result The final factorial value.
%
factorial_acc(0, Result, Result).
factorial_acc(N, Acc, Result) :- N > 0,NewAcc is Acc * N,N1 is N - 1,factorial_acc(N1, NewAcc, Result).


% You can test each one by querying, for example:
% ?- factorial_recursive(5, X



% The following code defines several test cases and example usages for manipulating spaces, or knowledge bases, using
% predicates such as `add-atom`, `remove-atom`, `replace-atom`, and `get-atoms`. These predicates
% simulate basic CRUD (Create, Read, Update, Delete) operations on a conceptual data structure, `Space`,
% with operations applied to atoms within the space.
%
% The code includes:
%
% 1. **Basic Operations (`example_usages`)**:
%    Demonstrates initialization, addition, replacement, and retrieval of atoms in a space. Each operation's result
%    is output to show changes in the space's state after each operation.
%
% 2. **Test Cases for Clearing and Modifying Spaces**:
%    - `test_clear_space`: Initializes a space, adds atoms, verifies the count and content, clears it, and confirms
%      that the atoms and count reset.
%    - `test_operations`: Tests sequential additions, removals, and replacements of atoms in a space.
%
% 3. **Multiple Operations in a Shared Space (`test_my_space`)**:
%    Initializes a space and performs various atomic operations in sequence to verify the changes at each step.
%    This includes ensuring atoms can be added, counted, replaced, removed, and that the original space name is retained.
%
% 4. **Isolated Space Manipulation**:
%    Additional tests on separate spaces, such as `&kb22` and `&kb2`, demonstrate similar operations, providing
%    results to confirm that each operation succeeds independently.
%
% 5. **Run All Test Cases (`run_tests`)**:
%    A convenience predicate `run_tests` that executes `test_clear_space` and `test_operations` to validate
%    all defined atomic operations.
%
% The code serves as a comprehensive suite for verifying that atom manipulation functions behave as expected across
% various spaces, with each test printing intermediate results to facilitate debugging and validation of functionality.

% Example-usage
example_usages :-
    fetch_or_create_space(newSpace,Space),  % Assuming fetch_or_create_space/1 is defined to initialize a space
    'add-atom'(Space, a),
    'add-atom'(Space, b),
    'add-atom'(Space, c),
    'match'(Space, a, Template),
    write('Matched template: '), writeln(Template),


    write('Initial space: '), writeln(Space),

    'add-atom'(Space, a),
    write('Space after adding "a": '), writeln(Space),

    'add-atom'(Space, b),
    write('Space after adding "b": '), writeln(Space),

    'replace-atom'(Space, a, c),
    write('Space after replacing "a" with "c": '), writeln(Space),

    'get-atoms'(Space, Atoms),
    write('Atoms in space: '), writeln(Atoms),

    'atom-count'(Space, Count),
    write('Number of atoms in space: '), writeln(Count).

% Test case for clearing a space
test_clear_space :-
    writeln('Test: Clearing a space'),
    init_space('&kb1'),
    'add-atom'('&kb1', a),
    'add-atom'('&kb1', b),
    writeln('Expected Count Before Clearing: 2'),
    'atom-count'('&kb1', CountBefore), writeln('Actual Count:'), writeln(CountBefore),
    writeln('Expected Atoms Before Clearing: [b, a]'),
    'get-atoms'('&kb1', AtomsBefore), writeln('Actual Atoms:'), writeln(AtomsBefore),
    'clear-atoms'('&kb1'),
    writeln('Expected Count After Clearing: 0'),
    'atom-count'('&kb1', CountAfter), writeln('Actual Count:'), writeln(CountAfter),
    writeln('Expected Atoms After Clearing: []'),
    'get-atoms'('&kb1', AtomsAfter), writeln('Actual Atoms:'), writeln(AtomsAfter).

% Test case for various operations on a space
test_operations :-
    writeln('Test: Various Operations on a Space'),
    init_space('&kb2'),
    'add-atom'('&kb2', a),
    'add-atom'('&kb2', b),
    writeln('Expected Count After Adding: 2'),
    'atom-count'('&kb2', Count1), writeln('Actual Count:'), writeln(Count1),
    writeln('Expected Atoms After Adding: [b, a]'),
    'get-atoms'('&kb2', Atoms1), writeln('Actual Atoms:'), writeln(Atoms1),
    'remove-atom'('&kb2', a),
    writeln('Expected Atoms After Removing a: [b]'),
    'get-atoms'('&kb2', Atoms2), writeln('Actual Atoms:'), writeln(Atoms2),
    'replace-atom'('&kb2', b, c),
    writeln('Expected Atoms After Replacing b with c: [c]'),
    'get-atoms'('&kb2', Atoms3), writeln('Actual Atoms:'), writeln(Atoms3).

% Run the test cases
run_tests :-
    writeln('Running test_clear_space:'),
    test_clear_space,
    writeln('---'),
    writeln('Running test_operations:'),
    test_operations.

% Test case for various operations on a space
test_my_space :-
    fetch_or_create_space('&KB', InstanceOfKB),
    'clear-atoms'('&KB'),
    'add-atom'(InstanceOfKB, a),
    'add-atom'(InstanceOfKB, b),
    'atom-count'(InstanceOfKB, Count1),
    writeln('Should print 2: ' : Count1),

    'get-atoms'(InstanceOfKB, Atoms1),
    writeln('Should print [b, a]: ' : Atoms1),

    'remove-atom'(InstanceOfKB, a),
    'get-atoms'(InstanceOfKB, Atoms2),
    writeln('Should print [b]: ' : Atoms2),

    'replace-atom'(InstanceOfKB, b, c),
    'get-atoms'(InstanceOfKB, Atoms3),
    writeln('Should print [c]: ' : Atoms3),

    space_original_name(InstanceOfKB, OriginalName),
    writeln('Should print &KB':OriginalName),

    fetch_or_create_space('&KB'),
    'add-atom'('&KB', x),
    'add-atom'('&KB', y),
    'atom-count'('&KB', Count2),
    writeln('Should print 3: ' : Count2),

    'get-atoms'('&KB', Atoms4),
    writeln('Should print [c, y, x]: ' : Atoms4),

    'remove-atom'('&KB', x),
    'get-atoms'('&KB', Atoms5),
    writeln('Should print [c,y]: ' : Atoms5),

    'replace-atom'('&KB', y, z),
    'get-atoms'(InstanceOfKB, Atoms6),
    writeln('Should print [c,z]: ' : Atoms6).

% Test the code
test_clr_my_kb22 :-
    fetch_or_create_space('&kb22'),
    'add-atom'('&kb22', a),
    'add-atom'('&kb22', b),
    'atom-count'('&kb22', Count1), writeln(Count1),
    'get-atoms'('&kb22', Atoms1), writeln(Atoms1),
    'clear-atoms'('&kb22'),
    'atom-count'('&kb22', Count2), writeln(Count2),
    'get-atoms'('&kb22', Atoms2), writeln(Atoms2).

  %a:- !, be(B), (iF(A,B)  -> tHEN(A) ).
  %a:- !, be(B), (iF(A,B) *-> tHEN(A) ; eLSE(B)  ).

% Test the code
test_my_kb2:-
   fetch_or_create_space('&kb1', InstanceOfKB),
   \+ \+ ('add-atom'('&kb1', a, Out), writeln(Out)),
   \+ \+ ('add-atom'('&kb1', b, Out), writeln(Out)),
   \+ \+ ('atom-count'('&kb1', Count), writeln(Count)),
   \+ \+ ('get-atoms'('&kb1', Atoms), writeln(Atoms)),
   \+ \+ ('remove-atom'(InstanceOfKB, a, Out), writeln(Out)),
   \+ \+ ('get-atoms'('&kb1', NewAtoms), writeln(NewAtoms)),
   \+ \+ ('replace-atom'('&kb1', b, c, Out), writeln(Out)),
   \+ \+ ('get-atoms'('&kb1', FinalAtoms), writeln(FinalAtoms)),
   \+ \+ (space_original_name(InstanceOfKB, OriginalName), writeln(OriginalName)),
   \+ \+ (fetch_or_create_space('&kb2',_)),  % Creating a new space with a different name
   \+ \+ ('add-atom'('&kb2', a, Out), writeln(Out)),
   \+ \+ ('add-atom'('&kb2', b, Out), writeln(Out)),
   \+ \+ ('atom-count'('&kb2', Count), writeln(Count)),
   \+ \+ ('get-atoms'('&kb2', Atoms), writeln(Atoms)),
   \+ \+ ('remove-atom'('&kb2', a, Out), writeln(Out)),
   \+ \+ ('get-atoms'('&kb2', NewAtoms), writeln(NewAtoms)),
   \+ \+ ('replace-atom'('&kb2', b, c, Out), writeln(Out)),
   \+ \+ ('get-atoms'('&kb2', FinalAtoms), writeln(FinalAtoms)).



% This code loads a collection of `.metta` files across various directories and contexts, adding each to the read history.
% Each file path is specified via `mf/1`, and `add_history1(load_metta(H))` loads the file while logging it for quick
% access and debugging.
% Comment `end_of_file` out once to get these files in your readline history
end_of_file. %
mf('./1-VSpaceTest.metta').
mf('./2-VSpaceTest.metta').
mf('./3-Learn-Rules.metta').
mf('./4-VSpaceTest.metta').
mf('./5-Learn-Flybase.metta').
mf('./6-Learn-Flybase-Full.metta').
mf('./8-VSpaceTest.metta').
mf('./autoexec.metta').
mf('./data/OBO-Metta/export/Alliance_of_Genome_Resources.metta').
mf('./data/OBO-Metta/export/biosapiens.metta').
mf('./data/OBO-Metta/export/chebi_fb_2023_04.metta').
mf('./data/OBO-Metta/export/DBVAR.metta').
mf('./data/OBO-Metta/export/doid.metta').
mf('./data/OBO-Metta/export/flybase_controlled_vocabulary.metta').
mf('./data/OBO-Metta/export/flybase_stock_vocabulary.metta').
mf('./data/OBO-Metta/export/fly_anatomy.metta').
mf('./data/OBO-Metta/export/fly_development.metta').
mf('./data/OBO-Metta/export/gene_group_FB2023_04.metta').
mf('./data/OBO-Metta/export/go-basic.metta').
mf('./data/OBO-Metta/export/image.metta').
mf('./data/OBO-Metta/export/psi-mi.metta').
mf('./data/OBO-Metta/export/slice.chebi.metta').
mf('./data/OBO-Metta/export/so-simple.metta').
mf('./data/OBO-Metta/export/so.metta').
mf('./data/OBO-Metta/export/SOFA.metta').
mf('./examples/compat/common/BelieveMe.metta').
mf('./examples/compat/common/EqualityType.metta').
mf('./examples/compat/common/EqualityTypeTest.metta').
mf('./examples/compat/common/formula/DeductionFormula.metta').
mf('./examples/compat/common/formula/DeductionFormulaTest.metta').
mf('./examples/compat/common/formula/ImplicationDirectIntroductionFormula.metta').
mf('./examples/compat/common/formula/ModusPonensFormula.metta').
mf('./examples/compat/common/In.metta').
mf('./examples/compat/common/InTest.metta').
mf('./examples/compat/common/List.metta').
mf('./examples/compat/common/ListTest.metta').
mf('./examples/compat/common/Maybe.metta').
mf('./examples/compat/common/MaybeTest.metta').
mf('./examples/compat/common/Num.metta').
mf('./examples/compat/common/NumTest.metta').
mf('./examples/compat/common/OrderedSet.metta').
mf('./examples/compat/common/OrderedSetTest.metta').
mf('./examples/compat/common/Record.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValue.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValueTest.metta').
mf('./examples/compat/common/truthvalue/MeasEq.metta').
mf('./examples/compat/common/truthvalue/TemporalTruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValueTest.metta').
mf('./examples/compat/dependent-types/DeductionDTL.metta').
mf('./examples/compat/dependent-types/DeductionDTLTest.metta').
mf('./examples/compat/dependent-types/DeductionImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTL.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ModusPonensDTL.metta').
mf('./examples/compat/dependent-types/ModusPonensDTLTest.metta').
mf('./examples/compat/entail/DeductionEntail.metta').
mf('./examples/compat/entail/DeductionEntailTest.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntail.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntailTest.metta').
mf('./examples/compat/equal/DeductionEqual.metta').
mf('./examples/compat/equal/DeductionEqualTest.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqual.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqualTest.metta').
mf('./examples/compat/match/DeductionImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/match/DeductionMatch.metta').
mf('./examples/compat/match/DeductionMatchTest.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatch.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/prob-dep-types/inf_order_probs.metta').
mf('./examples/compat/prob-dep-types/prob_dep_types.metta').
mf('./examples/compat/recursion-schemes/src/base.metta').
mf('./examples/compat/recursion-schemes/src/examples/benchmark.metta').
mf('./examples/compat/recursion-schemes/src/examples/expression.metta').
mf('./examples/compat/recursion-schemes/src/schemes.metta').
mf('./examples/compat/synthesis/experiments/non-determinism.metta').
mf('./examples/compat/synthesis/experiments/self-contained-synthesize.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-superpose.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-type-checking.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify.metta').
mf('./examples/compat/synthesis/experiments/unify-via-case.metta').
mf('./examples/compat/synthesis/experiments/unify-via-let.metta').
mf('./examples/compat/synthesis/Synthesize.metta').
mf('./examples/compat/synthesis/SynthesizeTest.metta').
mf('./examples/compat/synthesis/Unify.metta').
mf('./examples/compat/synthesis/UnifyTest.metta').
mf('./examples/compat/test_scripts/a1_symbols.metta').
mf('./examples/compat/test_scripts/a2_opencoggy.metta').
mf('./examples/compat/test_scripts/a3_twoside.metta').
mf('./examples/compat/test_scripts/b0_chaining_prelim.metta').
mf('./examples/compat/test_scripts/b1_equal_chain.metta').
mf('./examples/compat/test_scripts/b2_backchain.metta').
mf('./examples/compat/test_scripts/b3_direct.metta').
mf('./examples/compat/test_scripts/b4_nondeterm.metta').
mf('./examples/compat/test_scripts/b5_types_prelim.metta').
mf('./examples/compat/test_scripts/c1_grounded_basic.metta').
mf('./examples/compat/test_scripts/c2_spaces.metta').
mf('./examples/compat/test_scripts/c2_spaces_kb.metta').
mf('./examples/compat/test_scripts/c3_pln_stv.metta').
mf('./examples/compat/test_scripts/d1_gadt.metta').
mf('./examples/compat/test_scripts/d2_higherfunc.metta').
mf('./examples/compat/test_scripts/d3_deptypes.metta').
mf('./examples/compat/test_scripts/d4_type_prop.metta').
mf('./examples/compat/test_scripts/d5_auto_types.metta').
mf('./examples/compat/test_scripts/e1_kb_write.metta').
mf('./examples/compat/test_scripts/e2_states.metta').
mf('./examples/compat/test_scripts/e3_match_states.metta').
mf('./examples/compat/test_scripts/f1_imports.metta').
mf('./examples/compat/test_scripts/f1_moduleA.metta').
mf('./examples/compat/test_scripts/f1_moduleB.metta').
mf('./examples/compat/test_scripts/f1_moduleC.metta').
mf('./examples/compat/test_scripts/_e2_states_dia.metta').
mf('./examples/fibo.metta').
mf('./examples/fwgc.metta').
mf('./examples/httpclient.metta').
mf('./examples/NARS.metta').
mf('./examples/NARS_listing.metta').
mf('./examples/RUN_minnars.metta').
mf('./examples/RUN_tests0.metta').
mf('./examples/RUN_tests1.metta').
mf('./examples/RUN_tests2.metta').
mf('./examples/RUN_tests3.metta').
mf('./examples/send-more.metta').
mf('./examples/talk80.metta').
mf('./examples/VRUN_tests0.metta').
mf('./examples/VRUN_tests1.metta').
mf('./examples/VRUN_tests2.metta').
mf('./examples/VRUN_tests3.metta').
mf('./src/nm_test.metta').
mf('./src/r.metta').
mf('./src/test_nspace.metta').
:- forall(mf(H),add_history1(load_metta(H))).
%:- load_metta





end_of_file.



parsing(String, Expr) :- string(String),!,string_codes(String,Codes),phrase(expressions(Expr), Codes).
parsing(String, Expr) :- phrase(expressions(Expr), String).

expressions([E|Es]) -->
    ws, expression(E), ws,
    !, % single solution: longest input match
    expressions(Es).
expressions([]) --> [].

% ws --> ";",until_eol,
ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% A number N is represented as n(N), a symbol S as s(S).

expression(s(A))         --> symbol(Cs), { atom_codes(A, Cs) }.
expression(n(N))         --> number(Cs), { number_codes(N, Cs) }.
expression(List)         --> [L],{is_bracket_lr(L,R)},expressions(List), [R].
expression([s(quote),Q]) --> "'", expression(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

digit(D) --> [D], { code_type(D, digit) }.

symbol([A|As]) -->
    [A],
    { is_ok_symbolchar(A) },
    symbolr(As).

symbolr([A|As]) -->
    [A],
    { is_ok_symbolchar(A) ; code_type(A, alnum) },
    symbolr(As).
symbolr([]) --> [].

is_bracket_lr(L,R):- member(LR,["()","{}","[]","\"\""]), nth0(0,LR,L),nth0(1,LR,R).
is_ok_symbolchar(A):- \+ code_type(A, space), \+ code_type(A, white), \+ is_bracket_lr(A,_), \+ is_bracket_lr(_,A).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

codelist_to_forms_i(AsciiCodesList,FormsOut):-
    parsing(AsciiCodesList, Forms0),
    compile_all(Forms0, FormsOut),!.

run(Program, Values) :-
    parsing(Program, Forms0),
    empty_assoc(E),
    compile_all(Forms0, Forms),
    writeq(seeingFormas(Forms)),nl,
    phrase(eval_all(Forms, Values0), [E-E], _),
    maplist(unfunc, Values0, Values).

unfunc(s(S), S).
unfunc(t, t).
unfunc(n(N), N).
unfunc([], []).
unfunc([Q0|Qs0], [Q|Qs]) :- unfunc(Q0, Q), unfunc(Qs0, Qs).

fold([], _, V, n(V)).
fold([n(F)|Fs], Op, V0, V) :- E =.. [Op,V0,F], V1 is E, fold(Fs, Op, V1, V).

compile_all(Fs0, Fs) :- maplist(compile, Fs0, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    compile/2 marks (with 'user/1') calls of user-defined functions.
    This eliminates an otherwise defaulty representation of function
    calls and thus allows for first argument indexing in eval//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

compile(F0, F) :-
    (   F0 = n(_)   -> F = F0
    ;   F0 = s(t)   -> F = t
    ;   F0 = s(nil) -> F = []
    ;   F0 = s(_)   -> F = F0
    ;   F0 = [] -> F = []
    ;   F0 = [s(quote),Arg] -> F = [quote,Arg]
    ;   F0 = [s(setq),s(Var),Val0] -> compile(Val0, Val), F = [setq,Var,Val]
    ;   F0 = [s(Op)|Args0],
        memberchk(Op, [+,-,*,equal,if,>,<,=,progn,eval,list,car,cons,
                       cdr,while,not]) ->
        compile_all(Args0, Args),
        F = [Op|Args]
    ;   F0 = [s(defun),s(Name),Args0|Body0] ->
        compile_all(Body0, Body),
        maplist(arg(1), Args0, Args),
        F = [defun,Name,Args|Body]
    ;   F0 = [s(Op)|Args0] -> compile_all(Args0, Args), F = [user(Op)|Args]
    ).

eval_all([], [])         --> [].
eval_all([A|As], [B|Bs]) --> eval(A, B), eval_all(As, Bs).

eval(n(N), n(N))       --> [].
eval(t, t)             --> [].
eval([], [])           --> [].
eval(s(A), V), [Fs-Vs] --> [Fs-Vs], { get_assoc(A, Vs, V) }.
eval([L|Ls], Value)    --> eval(L, Ls, Value).

eval(quote, [Q], Q) --> [].
eval(+, As0, V)     --> eval_all(As0, As), { fold(As, +, 0, V) }.
eval(-, As0, V)     --> eval_all(As0, [n(V0)|Vs0]), { fold(Vs0, -, V0, V) }.
eval(*, As0, V)     --> eval_all(As0, Vs), { fold(Vs, *, 1, V) }.
eval(car, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [C|_] }.
eval(cdr, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [_|C] }.
eval(list, Ls0, Ls) --> eval_all(Ls0, Ls).
eval(not, [A], V)   --> eval(A, V0), goal_truth(V0=[], V).
eval(>, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1>V2, V).
eval(<, [A,B], V)   --> eval(>, [B,A], V).
eval(=, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1=:=V2, V).
eval(progn, Ps, V)  --> eval_all(Ps, Vs), { last(Vs, V) }.
eval(eval, [A], V)  --> eval(A, F0), { compile(F0, F1) }, eval(F1, V).
eval(equal, [A,B], V) --> eval(A, V1), eval(B, V2), goal_truth(V1=V2, V).
eval(cons, [A,B], [V0|V1])  --> eval(A, V0), eval(B, V1).
eval(while, [Cond|Bs], [])  -->
    (   eval(Cond, []) -> []
    ;   eval_all(Bs, _),
        eval(while, [Cond|Bs], _)
    ).
eval(defun, [F,As|Body], s(F)), [Fs-Vs0] -->
    [Fs0-Vs0],
    { put_assoc(F, Fs0, As-Body, Fs) }.
eval(user(F), As0, V), [Fs-Vs] -->
    eval_all(As0, As1),
    [Fs-Vs],
    { empty_assoc(E),
      get_assoc(F, Fs, As-Body),
      bind_arguments(As, As1, E, Bindings),
      phrase(eval_all(Body, Results), [Fs-Bindings], _),
      last(Results, V) }.
eval('bind!', [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(setq, [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(if, [Cond,Then|Else], Value) -->
    (   eval(Cond, []) -> eval_all(Else, Values), { last(Values, Value) }
    ;   eval(Then, Value)
    ).

:- meta_predicate goal_truth(0,*,//,//).
goal_truth(Goal, T) --> { Goal -> T = t ; T = [] }.

bind_arguments([], [], Bs, Bs).
bind_arguments([A|As], [V|Vs], Bs0, Bs) :-
    put_assoc(A, Bs0, V, Bs1),
    bind_arguments(As, Vs, Bs1, Bs).

run(S):-'format'('~n~s~n',[S]),run(S,V),writeq(V).

%if_script_file_time(X):-if_startup_script(time(X)).
if_script_file_time(_):-!.
%if_script_file_time(X):- nop(time(X)).

% Append:
    :- if_script_file_time(run("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].


% Fibonacci, naive version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].


% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].


% Fibonacci, iterative version:
    :- if_script_file_time(run("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 30,794 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 12831368 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].



% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 350)")).

    %@ % 44,595 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 14526532 Lips)
    %@ V = [fib, fib1, 6254449428820551641549772190170184190608177514674331726439961915653414425].


% Higher-order programming and eval:
    :- if_script_file_time(run("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].

%:- ensure_loaded(metta_reader).



#[test]
fn test_case_operation() {
    let metta = new_metta_rust();
    let result = metta.run(&mut SExprParser::new("
    "));

    let expected = metta.run(&mut SExprParser::new("
        ! OK
        ! 7
        ! (superpose (OK-3 OK-4))
        ! (superpose (3 4 5))
        ! (superpose ())
    "));
    assert_eq!(result, expected);

    let metta = new_metta_rust();
    let result = metta.run(&mut SExprParser::new("
        (Rel-P A B)
        (Rel-Q A C)

        ; cases can be used for deconstruction
        !(case (match &self ($rel A $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))))

        ; %void% can be used to capture empty results
        !(case (match &self ($rel B $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))
            (%void% no-match)))

        ; a functional example
        (= (maybe-inc $x)
            (case $x
            (((Just $v) (Just (+ 1 $v)))
                (Nothing Nothing)))
        )
        !(maybe-inc Nothing)
        !(maybe-inc (Just 2))
    "));
    let expected = metta.run(&mut SExprParser::new("
        ! (superpose ((Q C) (P B)))
        ! no-match
        ! Nothing
        ! (Just 3)
    "));
    assert_eq_metta_results!(result, expected);
}



use hyperon::metta::text::*;
use hyperon::metta::runner::new_metta_rust;

#[test]
fn test_reduce_higher_order() {
    let program = "
        ; Curried plus
        (: plus (-> Number (-> Number Number)))
        (= ((plus $x) $y) (+ $x $y))
        ; Define inc as partial evaluation of plus
        (: inc (-> (-> Number Number)))
        (= (inc) (plus 1))

        !(assertEqualToResult ((inc) 2) (3))
    ";
    let metta = new_metta_rust();

    let result = metta.run(&mut SExprParser::new(program));

    assert_eq!(result, Ok(vec![vec![]]));
}



use hyperon::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_custom_match_with_space() {
    let mut main_space = GroundingSpace::new();
    let mut inserted_space = GroundingSpace::new();
    inserted_space.add(expr!("implies" ("B" x) ("C" x)));
    inserted_space.add(expr!("implies" ("A" x) ("B" x)));
    inserted_space.add(expr!("A" "Sam"));
    main_space.add(Atom::gnd(inserted_space));
    let result = main_space.query(&expr!("," ("implies" ("B" x) z) ("implies" ("A" x) y) ("A" x)));
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].resolve(&VariableAtom::new("y")), Some(expr!("B" "Sam")));
    assert_eq!(result[0].resolve(&VariableAtom::new("z")), Some(expr!("C" "Sam")));
}



use hyperon::*;
use hyperon::common::*;
use hyperon::metta::interpreter::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_types_in_metta() {
    let mut space = GroundingSpace::new();
    space.add(expr!("=" ("check" (":" n "Int")) ({IS_INT} n)));
    space.add(expr!("=" ("check" (":" n "Nat")) ({AND} ("check" (":" n "Int")) ({GT} n {0}))));
    space.add(expr!("=" ("if" {true} then else) then));
    space.add(expr!("=" ("if" {false} then else) else));
    space.add(expr!(":" "if" ("->" "bool" "Atom" "Atom" "Atom")));
    space.add(expr!("=" ("fac" n) ("if" ("check" (":" n "Nat")) ("if" ({EQ} n {1}) {1} ({MUL} n ("fac" ({SUB} n {1})))) ({ERR}))));

    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Nat"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Nat"))), Ok(vec![expr!({false})]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(3)} "Nat")) "ok" "nok")), Ok(vec![expr!("ok")]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(-3)} "Nat")) "ok" "nok")), Ok(vec![expr!("nok")]));
    assert_eq!(interpret(&space, &expr!("fac" {1})), Ok(vec![expr!({1})]));
    assert_eq!(interpret(&space, &expr!("fac" {3})), Ok(vec![expr!({6})]));
}


    #[test]
    fn test_match_expression_with_variables() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" b c))),
        bind_set![{a: expr!("A"), b: expr!("B"), c: expr!("C") }]);
    }

    #[test]
    fn test_match_different_value_for_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" a c))), BindingsSet::empty());
    }

    #[test]
    fn test_match_query_variable_has_priority() {
        let mut space = GroundingSpace::new();
        space.add(expr!("equals" x x));

        let result = space.query(&expr!("equals" y z));
        assert_eq!(result, bind_set![{ y: expr!(z) }]);
    }

    #[test]
    fn test_match_query_variable_via_data_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!(x x));
        assert_eq!(space.query(&expr!(y (z))), bind_set![{y: expr!((z))}]);
    }

    #[test]
    fn test_match_if_then_with_x() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("if" "True" then) then));
        assert_eq!(space.query(&expr!("=" ("if" "True" "42") X)),
        bind_set![{X: expr!("42")}]);
    }

    #[test]
    fn test_match_combined_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!("posesses" "Sam" "baloon"));
        space.add(expr!("likes" "Sam" ("blue" "stuff")));
        space.add(expr!("has-color" "baloon" "blue"));

        let result = space.query(&expr!("," ("posesses" "Sam" object)
        ("likes" "Sam" (color "stuff"))
        ("has-color" object color)));
        assert_eq!(result, bind_set![{object: expr!("baloon"), color: expr!("blue")}]);
    }


