/* <not-a-module> Metta File Processor

This module provides utilities to process `.metta` files by reading S-expressions and comments from an input stream
and writing them as structured facts to an output stream, including the position information of each element within the file.

The module is designed to work with multifile predicates to store the metadata and content of processed files
in a modular manner. It facilitates the extraction of S-expressions, lists, and comments from the input file
while tracking their line, column, and character positions.

The main components of the module are:
  - `process_expressions/3`: Reads and processes the contents of a file (S-expressions and comments) and writes the processed information, including file positions, to an output stream.
  - `make_DL/4`: Generates and asserts facts for each processed item based on its type (list or non-list) and its position in the file.

### Multifile Declarations:
This module makes use of the following multifile predicates, which can be extended across different modules:
  - `afn_stem_filename/3`: Associates the absolute file name, file name stem, and the original file name.
  - `metta_file_buffer/5`: Stores processed S-expression data along with its file position and source file.
  - `metta_file_comment/5`: Stores processed comment data along with its file position and source file.

### Example Use Case:
1. Processing a `.metta` file:
   ```prolog
   process_expressions('example.metta', InStream, OutStream).
   ```
   This will read the S-expressions and comments from `example.metta`, process them, and output structured facts to `OutStream`.

2. Assertions made by `make_DL/4` will be based on the *
type of items being processed, allowing for dynamic and flexible *
handling of lists and individual items.s * *
*/



%!  parse_sexpr_untyped(+Input, -Output) is det.
%
%   Parses an untyped S-expression from the input.
%
%   This predicate acts as a wrapper around `parse_sexpr/2`, which performs
%   the actual parsing of S-expressions.
%
%   @arg Input  The input from which the S-expression is parsed.
%   @arg Output The parsed S-expression.
parse_sexpr_untyped(I, O) :-
    % Call the helper predicate to parse the S-expression.
    parse_sexpr(I, O).

%!  parse_sexpr(+Input, -Output) is det.
%
%   Parses an S-expression from the input.
%
%   This predicate reads the S-expression using `read_sexpr/2`.
%
%   @arg Input  The input from which the S-expression is read.
%   @arg Output The parsed S-expression.
parse_sexpr(I, O) :-
    % Use read_sexpr/2 to parse the input into an S-expression.
    read_sexpr(I, O).

:- export(extract_lvars/3).

%=

%!  extract_lvars(?A, ?B, ?After) is det.
%
%   Extract L-vars (logical variables).
%
%   This predicate extracts logical variables by processing the initial list
%   of variables (if any), removing incomplete variables, and copying them
%   into the output.
%
%   @arg A      The first term from which to extract logical variables.
%   @arg B      The list of variables to process.
%   @arg After  The resulting list after processing.
%
extract_lvars(A,B,After):-
    % Retrieve the variable list, if available, otherwise initialize to an empty list.
     (get_varname_list(Before)->true;Before=[]),
    % Remove incomplete variables from the list.
     remove_incompletes(Before,CBefore),!,
    % Copy the logical variables from the input.
     copy_lvars(A,CBefore,B,After),!.


%!  copy_lvars(:TermVAR, ?Vars, :TermNV, ?NVars) is det.
%
%   Copy logical variables.
%
%   This predicate copies logical variables from the first term, passing
%   through a list of variables, and outputs the copied result.
%
%   @arg TermVAR The term with logical variables to copy.
%   @arg Vars    The list of current variables.
%   @arg TermNV  The output term after copying logical variables.
%   @arg NVars   The updated list of variables after copying.
%

% Removed as it was commented out in the original code.
% copy_lvars( VAR,Vars,VAR,Vars):- var(VAR),!.

copy_lvars(Term, Vars, Out, VarsO) :-
    % If the term is an empty list, directly return it and pass along Vars.
    Term == [], !,
    must_det_ll((Out = Term, VarsO = Vars)).
copy_lvars(VAR, Vars, Out, VarsO) :-
    % If the term is a variable, return it unchanged and pass along Vars.
    var(VAR), !,
    must_det_ll((Out = VAR, VarsO = Vars)).
copy_lvars([H|T], Vars, [NH|NT], VarsO) :-
    % If the term is a list, recursively process the head and tail.
    !, copy_lvars(H, Vars, NH, SVars), !,
    copy_lvars(T, SVars, NT, VarsO).
copy_lvars('?'(Inner), Vars, Out, VarsO) :-
    % If the term is a '?'-prefixed variable, process the inner term and handle accordingly.
    !, copy_lvars(Inner, Vars, NInner, VarsO),
    must_det_ll((symbol(NInner) -> atom_concat_or_rtrace('?', NInner, Out) ; Out = '?'(NInner))), !.
copy_lvars(VAR, Vars, Out, VarsO) :-
    % If the term is a logical variable, register it and return the new variable list.
    svar(VAR, Name) -> must_det_ll(symbol(Name)), !,
    must_det_ll(register_var(Name = Out, Vars, VarsO)).
copy_lvars(VAR, Vars, Out, VarsO) :-
    % If the term is atomic (non-compound), return it unchanged.
    \+ compound(VAR), !,
    must_det_ll((Out = VAR, VarsO = Vars)).
copy_lvars(Term,Vars,NTerm,VarsO):-
    % If the term is compound, decompose it and process the arguments.
    Term =.. [F | Args],
    (svar(F,_)-> copy_lvars( [F|Args],Vars,NTerm,VarsO);
        % Reconstruct the term after copying the arguments.
       (copy_lvars(Args,Vars,NArgs,VarsO), NTerm=..[F|NArgs])),!.



%=

%!  svar(?Var, ?NameU) is det.
%
%   Checks if this is a KIF (Knowledge Interchange Format) variable and converts it to a name suitable for Prolog.
%
%   @arg Var   The variable to check.
%   @arg NameU The name of the variable after conversion.
%
svar(SVAR, UP) :-
    % Handle the case where the name is already bound.
    nonvar(UP), !, trace_or_throw(nonvar_svar(SVAR, UP)).
svar(Var, Name) :-
    % If the variable is unbound, fix its name.
    var(Var), !, must_det_ll(svar_fixvarname(Var, Name)).
svar('$VAR'(Var), Name) :-
    % Handle Prolog internal variables.
    number(Var), Var > -1, !,
    must_det_ll(format(atom(Name), '~w', ['$VAR'(Var)])), !.
svar('$VAR'(Name), VarName) :-
    % Process '$VAR' variables.
    !, must_det_ll(svar_fixvarname(Name, VarName)).
svar('?'(Name), NameU) :-
    % Handle variables prefixed by '?'.
    svar_fixvarname(Name, NameU), !.
svar(_, _) :-
    % Fail if not in a valid KIF context.
    \+ kif_ok, !, fail.
svar(VAR, Name) :-
    % If the variable starts with '?', fix its name.
    symbol(VAR), atom_concat_or_rtrace('?', A, VAR), non_empty_atom(A),
    svar_fixvarname(VAR, Name), !.
svar([], _) :-
    % Fail if the variable is an empty list.
    !, fail.
svar('#'(Name), NameU) :-
    % Handle variables prefixed by '#'.
    !, svar(Name, NameU), !.
svar('@'(Name), NameU) :-
    % Handle variables prefixed by '@'.
    svar_fixvarname(Name, NameU), !.
svar(VAR, Name) :-
    % If the variable starts with '@', fix its name.
    symbol(VAR), atom_concat_or_rtrace('@', A, VAR), non_empty_atom(A),
    svar_fixvarname(VAR, Name), !.

:- export(svar_fixvarname/2).

%=

%!  svar_fixvarname(?SVARIN, ?UP) is det.
%
%   Fix the variable name.
%
%   @arg SVARIN The input variable name.
%   @arg UP     The fixed variable name.
%
svar_fixvarname(SVAR, UP) :-
    % If the name is already bound, throw an error.
    nonvar(UP), !, trace_or_throw(nonvar_svar_fixvarname(SVAR, UP)).
svar_fixvarname(SVAR, UP) :-
    % Fix the name if it follows certain conventions.
    svar_fixname(SVAR, UP), !.
svar_fixvarname(SVAR, UP) :-
    % If fixing fails, throw an error.
    fail, trace_or_throw(svar_fixname(SVAR, UP)).

%!  svar_fixname(?Var, ?NameO) is det.
%
%   Fix the name of the variable if needed.
%
%   @arg Var    The variable to fix.
%   @arg NameO  The output variable name.
%
svar_fixname(Var, NameO) :-
    % If the variable is unbound, get its name.
    var(Var), !,
    variable_name_or_ref(Var, Name), sanity(nonvar(Name)), !,
    svar_fixvarname(Name, NameO).
svar_fixname('$VAR'(Name), UP) :-
    % Process Prolog internal '$VAR' variables.
    !, svar_fixvarname(Name, UP).
svar_fixname('@'(Name), UP) :-
    % Handle variables prefixed by '@'.
    !, svar_fixvarname(Name, UP).
svar_fixname('?'(Name), UP) :-
    % Handle variables prefixed by '?'.
    !, svar_fixvarname(Name, UP).
svar_fixname('block'(Name), UP) :-
    % Handle 'block' variables.
    !, svar_fixvarname(Name, UP).
svar_fixname(SVAR, SVARO) :-
    % If the name is already valid, return it as is.
    ok_var_name(SVAR), !, SVARO = SVAR.
svar_fixname('??', '_') :-
    % Special case for '??'.
    !.
svar_fixname(QA, AU) :-
    % Handle variables starting with '??'.
    atom_concat_or_rtrace('??', A, QA), non_empty_atom(A), !,
    svar_fixvarname(A, AO), atom_concat_or_rtrace('_', AO, AU).
svar_fixname(QA, AO) :-
    % Handle variables starting with '?'.
    atom_concat_or_rtrace('?', A, QA), non_empty_atom(A), !,
    svar_fixvarname(A, AO).
svar_fixname(QA, AO) :-
    % Handle variables starting with '@'.
    atom_concat_or_rtrace('@', A, QA), non_empty_atom(A), !,
    svar_fixvarname(A, AO).
svar_fixname(NameU, NameU) :-
    % Handle variables starting with '_', followed by numbers.
    atom_concat_or_rtrace('_', Name, NameU),
    non_empty_atom(Name), atom_number(Name, _), !.
svar_fixname(NameU, NameUO) :-
    % Handle variables starting with '_', followed by a non-number.
    atom_concat_or_rtrace('_', Name, NameU), non_empty_atom(Name),
 \+ atom_number(Name,_),!,svar_fixvarname(Name,NameO),atom_concat_or_rtrace('_',NameO,NameUO).
svar_fixname(I,O):-
    % Perform final adjustments on the variable name by replacing special characters.
 notrace((
  notrace(catch(fix_varcase(I,M0),_,fail)),
  atom_subst(M0,'@','_AT_',M1),
  atom_subst(M1,'?','_Q_',M2),
  atom_subst(M2,':','_C_',M3),
  atom_subst(M3,'-','_',O),
  ok_var_name(O))),!.

%=

%!  fix_varcase(?I, ?O) is det.
%
%   Fix the case of a variable name.
%
%   @arg I  The input variable name.
%   @arg O  The output variable name after case adjustment.
%
fix_varcase(Word, Word) :-
    % If the word starts with '_', leave it unchanged.
    atom_concat_or_rtrace('_', _, Word), !.
fix_varcase(Word, WordC) :-
    % Convert the first letter to uppercase.
    !, atom_codes(Word, [F | R]), to_upper(F, U), atom_codes(WordC, [U | R]).
fix_varcase(Word, Word) :-
    % If the word is already uppercase, leave it unchanged.
    upcase_atom(Word, UC), UC = Word, !.
fix_varcase(Word, WordC) :-
    % Convert the first letter to uppercase if the word is lowercase.
    downcase_atom(Word, UC), UC = Word, !,
    atom_codes(Word, [F | R]), to_upper(F, U), atom_codes(WordC, [U | R]).
fix_varcase(Word, Word).  % Handle mixed-case words.

:- export(ok_varname_or_int/1).

%!  ok_varname_or_int(?Name) is det.
%
%   Checks if a name is a valid variable name or an integer.
%
%   @arg Name The name to check.
%
ok_varname_or_int(Name) :-
    % Check if the name is a valid atom.
    symbol(Name), !, ok_var_name(Name).
ok_varname_or_int(Name) :-
    % Check if the name is a number.
    number(Name).

%!  ok_var_name(?Name) is det.
%
%   Checks if the name is a valid variable name.
%
%   @arg Name The name to validate.
%
quietly_sreader(G):- notrace(G).
ok_var_name(Name):-
    % Ensure the name follows valid Prolog variable naming rules.
 notrace((
  quietly_sreader(( symbol(Name),atom_codes(Name,[C|_List]),char_type(C,prolog_var_start),
      notrace(catch(read_term_from_atom(Name,Term,[variable_names(Vs)]),_,fail)),
      !,var(Term),Vs=[RName=RVAR],!,RVAR==Term,RName==Name)))).

%:- export(ok_codes_in_varname/1).
%ok_codes_in_varname([]).
%ok_codes_in_varname([C|List]):-!,ok_in_varname(C),ok_codes_in_varname(List).

%:- export(ok_in_varname/1).
%ok_in_varname(C):-sym_char(C),\+member(C,`!@#$%^&*?()`).

%% atom_upper( ?A, ?U) is det.
%
% Atom Upper.
%
atom_upper(A,U):-string_upper(A,S),quietly_sreader(((atom_string(U,S)))).


%!  io_to_err(+Goal) is det.
%
%   Redirects the output of the given Goal to the user_error stream.
%   This is used to log information to the error output.
%
%   @arg Goal The goal whose output is redirected to user_error.
%
%   @example Redirect the output of a simple print goal:
%       ?- io_to_err(write('Error message')).
%
io_to_err(Goal):-
    with_output_to(user_error, Goal).

%!  log_progress(+Fmt, +Args) is det.
%
%   Logs progress messages to the terminal with bold black text.
%   The message is formatted according to the given format and arguments.
%
%   @arg Fmt  The format string used by ansi_format/3.
%   @arg Args The arguments to be used in the format string.
log_progress(Fmt, Args):-
    ttyflush,
    io_to_err(ansi_format([bold, hfg(black)], Fmt, Args)),
    ttyflush.

%!  log_error(+Fmt, +Args) is det.
%
%   Logs error messages to the terminal with bold red text.
%   The message is formatted according to the given format and arguments.
%
%   @arg Fmt  The format string used by ansi_format/3.
%   @arg Args The arguments to be used in the format string.
%
%   @example Log an error message:
%       ?- log_error('Failed to open file: ~w', ['missing.txt']).
%
log_error(Fmt, Args):-
    ttyflush,
    io_to_err(ansi_format([bold, hfg(red)], Fmt, Args)),
    ttyflush.


% Define the dynamic predicate to store comments and their positions.
:- dynamic(metta_file_comment/5).

%! main_init is det.
%
% Main entry point for the program. It initializes by fetching command line arguments
% and passing them to handle_arguments/1 for further processing.
main_init :-
    current_prolog_flag(os_argv, [_|OsArgV]),  % Retrieve command line arguments.
    ignore(handle_arguments(OsArgV)),           % Pass arguments to the handler.
    sleep(3),!.
    %halt.  % Exit Prolog.

%! handle_files_option(+Flag:atom, +OsArgV:list(atom), :P1) is det.
%
% Proccess content from the input files.
% @arg Flag The stream where output should be sent (stdout or file).
% @arg OsArgV List of input file names.
% @arg P1 Apply P1 to the List of existing input file names in OsArgV that are found after Flag
handle_files_option(Flag,OsArgV,P1):-
    append(_,[Flag|Rest],OsArgV),!,
    forall((member(InputFile, Rest), exists_file(InputFile)), call(P1,InputFile)).

%! handle_arguments(+OsArgV:list(atom)) is det.
%
% Processes the command line arguments to determine the input and output handling.
% It also checks if the '.buffer~' file needs to be regenerated by comparing modification times.
% @arg OsArgV List of command line arguments.
handle_arguments(OsArgV) :-
    handle_files_option('--stdout',OsArgV,show_input_files(current_output)),!.
handle_arguments(OsArgV) :-
    handle_files_option('--regen',OsArgV,gen_tmp_file(true)),!.
handle_arguments(OsArgV) :-
    handle_files_option('--gen',OsArgV,gen_tmp_file(false)),!.

%! show_input_files(+Output:stream, +InputFile:atom) is det.
%
% Outputs the contents of the input file to the specified output stream.
% @arg Output The stream where output should be sent.
% @arg InputFile The name of the input file.
show_input_files(Output, InputFile) :-
    check_input_file(InputFile),  % Check if the input file exists and is readable.
    setup_call_cleanup(
        open(InputFile, read, InStream),              % Open the input file for reading.
        process_expressions(InputFile,InStream, Output),        % Process expressions and write to the output stream.
        close(InStream)                               % Close the input stream upon completion.
    ).

%! handle_input_file(+InputFile:atom) is det.
%
% Handles the regeneration of buffer files for the specified input file.
% @arg InputFile The name of the input file.
gen_tmp_file(Forced, InputFile) :-
    file_name_extension(InputFile, 'buffer~', OutputFile),  % Formulate the output file name.
    check_input_file(InputFile),  % Ensure the input file exists and is readable.
    (  (Forced ; needs_regeneration(InputFile, OutputFile)) ->  % Check if the buffer~ file needs regeneration.
        check_output_file(OutputFile),  % Ensure the output file is writable.
        check_file_size(InputFile, OutputFile),  % Ensure the output file is not empty and is at least 50% the expected size.
        setup_call_cleanup(
            open(OutputFile, write, OutStream),  % Open the output file for writing.
            show_input_files(OutStream, InputFile),  % Process expressions and write to the file.
            close(OutStream)  % Close the output stream upon completion.
        ),
        log_progress('Info: Regenerated: ~w~n', [OutputFile])
    ;   hide_op(log_progress('Info: No need to regenerate: ~w~n', [OutputFile]))
    ).

% skips code when used as a wrapper
hide_op(_).

%! check_input_file(+InputFile:atom) is det.
%
% Verifies that the input file exists and is readable. If not, throws an error.
% @arg InputFile The input file path.
check_input_file(InputFile) :-
    (   exists_file(InputFile) -> true
    ;   log_error('Error: Input file ~w does not exist.~n', [InputFile])
    ),
    (   access_file(InputFile, read) -> true
    ;   log_error('Error: Input file ~w is not readable.~n', [InputFile])
    ).

%! check_output_file(+OutputFile:atom) is det.
%
% Verifies that the output file is writable. If not, throws an error.
% @arg OutputFile The output file path.
check_output_file(OutputFile) :-
    (   exists_file(OutputFile) -> true
    ;   log_progress('Info: Output file ~w does not exist and will be created.~n', [OutputFile])
    ),
    (   access_file(OutputFile, write) -> true
    ;   log_error('Error: Output file ~w is not writable.~n', [OutputFile])
    ).

%! check_file_size(+InputFile:atom, +OutputFile:atom) is det.
%
% Ensures that the output file is at least 50% of the size of the input file.
% If the output file is smaller than this threshold, the file is considered too small.
% @arg InputFile The input file path.
% @arg OutputFile The output file path.
check_file_size(InputFile, OutputFile) :-
    size_file(InputFile, InputSize),  % Get the size of the input file.
    (   exists_file(OutputFile) ->
        size_file(OutputFile, OutputSize),
        MinSize is InputSize // 2,  % Calculate 50% of the input file size.
        (   OutputSize >= MinSize -> true
        ;   log_progress('Warning: Output file ~w is only ~@ bytes (too small), regenerating (>~@ bytes required).~n', [OutputFile, scaled_units(OutputSize), scaled_units(MinSize)])
        )
    ;   true  % Output file does not exist, so it will be created.
    ).

%! needs_regeneration(+InputFile:atom, +OutputFile:atom) is semidet.
%
% Checks if the '.buffer~' file needs to be regenerated by comparing modification times.
% If the input file is newer than the buffer file, returns true.
% @arg InputFile The input file path.
% @arg OutputFile The output buffer file path.
needs_regeneration(InputFile, OutputFile) :-
    exists_file(OutputFile),  % Check if the buffer~ file exists.
    time_file(InputFile, InputTime),  % Get the modification time of the input file.
    time_file(OutputFile, OutputTime),  % Get the modification time of the output file.
    InputTime > OutputTime.  % Regenerate if the input file is newer.

needs_regeneration(InputFile, _OutputFile) :-
    \+ exists_file(InputFile),  % If input file does not exist, fail.
    log_error('Error: Input file ~w does not exist.~n', [InputFile]),
    fail.

:- use_module(library(process)).
:- use_module(library(time)).
:- dynamic ok_to_stop/1.

%! count_lines_in_file(+FileName:atom, -LineCount:int) is det.
%
% Uses the Bash `wc -l` command to count the number of lines in the specified file.
% @arg FileName The name of the file to count lines in.
% @arg LineCount The number of lines in the file.
count_lines_in_file(FileName, LineCount) :-
    process_create(path(wc), ['-l', FileName], [stdout(pipe(Out))]),
    read_line_to_string(Out, Result),  % Read the output from the `wc -l` command
    close(Out),  % Close the stream
    split_string(Result, " ", "", [LineStr|_]),  % Extract the line count
    number_string(LineCount, LineStr).  % Convert the string to an integer


%! report_progress(+FileName:atom, +InStream:stream, +TotalLines:int, +StartTime:float) is det.
%
% Reports the progress of file processing by calculating the percentage of lines processed every 30 seconds.
% It also estimates the time remaining until completion based on the current processing speed.
% Stops when ok_to_stop(FileName, true) is asserted or when the stream is closed or at the end of the file.
% @arg FileName The input file being translated.
% @arg InStream The input stream being processed.
% @arg TotalLines The total number of lines in the file.
% @arg StartTime The time when the process started.
report_progress(FileName, InStream, TotalLines, StartTime) :-
    sleep(10),  % Initial delay before progress reporting starts
    TimeBetweenReports = 15,
    repeat,
    ( stop_reporting(FileName, InStream, TotalLines, StartTime)
      -> log_progress('~t - Stopping reporting on ~w progress.~n', [FileName])
      ; (once(report_progress_so_far(FileName, get_percent_done(InStream, TotalLines), StartTime, TimeLeft)),
         ((number(TimeLeft), HalfTimeLeft is TimeLeft / 2, HalfTimeLeft < TimeBetweenReports, n_max(HalfTimeLeft,2,MinTime)) -> sleep(MinTime) ; sleep(TimeBetweenReports)),  % Sleep for HalfTime seconds between progress reports
         fail)).  % And Continue reporting

n_max(N1,N2,N2):- N1<N2,!.
n_max(N1,_,N1).

%! stop_reporting(+FileName:atom, +InStream:stream, +TotalLines:int, +StartTime:floatf) is semidet.
%
% Determines whether the reporting process should stop, based on whether the stream has ended or errors are detected.
stop_reporting(FileName, InStream, _TotalLines, _StartTime):-
    (   ok_to_stop(FileName, true) ->  % Check if ok_to_stop has been asserted as true
        log_progress('Info: Processing progress: 100% ', [])
    ;   stream_property(InStream, error(true)) ->  % Check if there's a stream error
        log_error('Warning: Stream error.', [])
    ;   (stream_property(InStream, end_of_stream(At)), At \== not) ->  % Check if the stream has reached the end
        log_progress('Info: Stream ~w End-of-Stream.', [At])
    ;   \+ stream_property(InStream, position(_)) ->  % Check if the stream is closed
        log_error('Info: Stream closed.', [])
    ; fail ).  % Continue reporting if none of the above conditions are met



%! remaining_time(+PercentDone: float, +StartTime: float, -RemainingTime: integer) is det.
%
% Calculate the remaining time required to complete a task based on the percentage of the task already completed and the start time.
% This predicate calculates the elapsed time from the start time to the current time, then uses this along with the task completion percentage to compute the remaining time.
%
% @param PercentDone The percentage of the task that has been completed, expressed as a float (e.g., 50.0 for 50%).
% @param StartTime The start time of the task, expressed in epoch seconds.
% @param RemainingTime The computed remaining time to complete the task, also in seconds.
%
% This predicate assumes that PercentDone is a positive value greater than zero. If it is zero or negative, a default remaining time of 60 seconds is returned to avoid division by zero or other meaningless calculations.
%
remaining_time(PercentDone, StartTime, RemainingTime) :-
    PercentDone > 0,   % Ensure that PercentDone is greater than 0 to avoid division by zero
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,  % Calculate the time elapsed since the start
    TotalTime is ElapsedTime / (PercentDone / 100),  % Estimate total time based on current progress
    RemainingTime is TotalTime - ElapsedTime, !.  % Compute remaining time
remaining_time(_, _, 60).  % Return a default remaining time if PercentDone is not greater than 0


%! get_percent_done(+InStream: stream, +TotalLines: int, -Percent: float) is det.
%
% Calculates and logs the percentage of lines processed so far in a stream based on the total number of lines. This predicate not only calculates the percentage but also logs the progress directly.
%
% @param InStream The input stream from which lines are being read.
% @param TotalLines The total number of lines in the stream.
% @param Percent The percentage of lines processed thus far, calculated and used for logging.
%
get_percent_done(InStream, TotalLines, Percent):-
    stream_property(InStream, position(Position)),
    stream_position_data(line_count, Position, CurrentLine),  % Get the current line number being processed
    Percent is (CurrentLine / TotalLines) * 100,  % Calculate the percentage completed
    log_progress('Info: Processing progress:\t ~2f% (Now ~d of ~d lines) ', [Percent, CurrentLine, TotalLines]).


%! report_progress_so_far(+FileName: string, +CalcPercent: predicate, +StartTime: float, -RemainingTime: float) is det.
%
% Reports the progress and the estimated time remaining for processing a file, based on a percentage calculation predicate provided.
%
% @param FileName The name of the file being processed.
% @param CalcPercent A lambda that when called, computes the percentage of the task completed. It should have a signature like `calc_percent_done(-Percent: float)`.
% @param StartTime The start time of the file processing, typically captured using `get_time/1`.
% @param RemainingTime The computed remaining time to complete the task, also in seconds.
%
% This predicate assumes the `CalcPercent` predicate handles all necessary file stream interactions to determine the progress.
%
report_progress_so_far(FileName, CalcPercent, StartTime, RemainingTime):-
    call(CalcPercent, PercentDone),  % Call the provided predicate to calculate the percentage completed
    remaining_time(PercentDone, StartTime, RemainingTime),
    (   number(RemainingTime) ->
        format_time_remaining(RemainingTime, TimeLeft)  % Convert estimated time into a human-readable format
    ;   TimeLeft = 'N/A'  % If no lines have been processed, or an error occurred, time left is unknown
    ),
    % Log the progress and estimated time remaining
    log_progress('\tProcessing file ~w: ~2f% complete. \tEstimated time remaining: ~w', [FileName, PercentDone, TimeLeft]).


%! scaled_units(+Number:int) is det.
%
% Formats a large number into a human-readable form using T (terabytes), G (gigabytes),
% M (megabytes), or K (kilobytes), depending on the magnitude of the number.
% @arg Number The number to format.
scaled_units(Number) :-
    (   Number >= 1_020_000_000_000 ->  % Terabytes (T)
        ScaledNumber is Number / 1_000_000_000_000,
        decimal_units(ScaledNumber, 'T')
    ;   Number >= 1_020_000_000 ->  % Gigabytes (G)
        ScaledNumber is Number / 1_000_000_000,
        decimal_units(ScaledNumber, 'G')
    ;   Number >= 1_020_000 ->  % Megabytes (M)
        ScaledNumber is Number / 1_000_000,
        decimal_units(ScaledNumber, 'M')
    ;   Number >= 1_100 ->  % Kilobytes (K)
        ScaledNumber is Number / 1_000,
        decimal_units(ScaledNumber, 'K')
    ;   % If it's less than 1000, just print the number
        format('~d', [Number])
    ).

%! decimal_units(+ScaledNumber:float, +Unit:atom) is det.
%
% Formats the scaled number based on its magnitude
% - Two decimal places if less than 10.
% - One decimal place if between 10 and 100.
% - No decimal places if greater than or equal to 100.
decimal_units(ScaledNumber, Unit) :-
    (   ScaledNumber < 10 ->
    format('~2f~w', [ScaledNumber, Unit])  % Two decimal places
    ;   ScaledNumber < 100 ->
    format('~1f~w', [ScaledNumber, Unit])  % One decimal places
    ;   % No decimal places
    format('~0f~w', [ScaledNumber, Unit])  % Zero decimal place
    ).

%! format_time_remaining(+Seconds:float, -FormattedTime:atom) is det.
%
% Formats the estimated time remaining (in seconds) into a human-readable format (HH:MM:SS).
% @arg Seconds The estimated time remaining in seconds.
% @arg FormattedTime The formatted time as an atom in the format HH:MM:SS.
format_time_remaining(Seconds, FormattedTime) :-
    Hours is floor(Seconds / 3600),
    Minutes is floor((Seconds - Hours * 3600) / 60),  % Convert seconds to integer minutes
    RemainingSeconds is floor(Seconds - Hours * 3600 - Minutes * 60),  % Remaining seconds after hours and minutes
    format(atom(FormattedTime), '~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+', [Hours, Minutes, RemainingSeconds]).


%! process_expressions(+FileName:atom, +InStream:stream, +OutStream:stream) is det.
%
% Reads and processes S-expressions and comments from the input stream, writing results with their position to the output stream.
% It also handles multifile declarations and tracks file-related metadata.
%
% @arg FileName The name of the input file being processed.
% @arg InStream The input stream from which to read the S-expressions and comments.
% @arg OutStream The output stream where the processed data and assertions are written.
%
% The process involves reading the stream for S-expressions, comments, and positions,
% then writing those as structured facts to the output. This includes handling multifile
% declarations (`afn_stem_filename/3`, `metta_file_buffer/5`, and `metta_file_comments/5`)
% for modular handling of facts across multiple files.
:- dynamic ok_to_stop/1.

process_expressions(FileName, InStream, OutStream) :-
    % Get total number of lines in the file
    count_lines_in_file(FileName, TotalLines),
    size_file(FileName, InputSize),
    log_progress('~NInfo: File ~w is ~@ bytes (~@ lines)~n', [FileName, scaled_units(InputSize), scaled_units(TotalLines)]),

    % Set the dynamic predicate ok_to_stop/1 to false initially
    assertz(ok_to_stop(FileName, false)),

    % Start a thread to report progress every 30 seconds
    get_time(StartTime),  % Record the start time
    thread_create(report_progress(FileName, InStream, TotalLines, StartTime), _, [detached(true)]),

    ignore(stream_property(InStream, file_name(Stem))),  % Get the file name of the stream.
    ignore(Stem = FileName),  % Assign the input file name if no stream file name.
    absolute_file_name(Stem, AFNStem),  % Get the absolute path of the file.

    % Declare multifile predicates for storing file-related facts.
    write_readably(OutStream, :- multifile(afn_stem_filename/3)),
    write_readably(OutStream, :- dynamic(metta_file_buffer/5)),
    write_readably(OutStream, :- multifile(metta_file_buffer/5)),

    % Record the absolute file name, file name stem, and the original file name.
    write_readably(OutStream, afn_stem_filename(AFNStem, Stem, FileName)),

    % Start reading and processing expressions from the input stream.
    repeat,
    read_position(InStream, _Line_It, _Col_It, _CharPos_Item, Position),  % Retrieve line, column, and character position.
    read_sexpr(InStream, Item),  % Read an S-expression or comment from the input stream.

    % Write any stored comments to the output.
    forall(retract(metta_file_comment(_Line, _Col, _CharPos, Comment, Pos)),
           write_readably(OutStream, metta_file_buffer(+, Comment, [], AFNStem, Pos))),

    % If end of file is reached, stop processing and update the ok_to_stop flag.
    (   Item = end_of_file ->
       (retractall(ok_to_stop(FileName, _)),  % Remove the previous value
        assertz(ok_to_stop(FileName, true)),  % Set ok_to_stop to true to signal the thread to stop
        !)
    ;   % Write the contents and make assertions for the processed item.
      (once((write_readably(OutStream, metta_file_buffer(+, Item, [], AFNStem, Position)),
        %make_DL(InStream, OutStream, AFNStem, Item),
        %flush_output(OutStream),  % Ensure the output is flushed.
       true)), fail)  % Continue processing the next item.
    ).

%! make_DL(+InStream:stream, +OutStream:stream, +FileName:atom, +Item:term) is det.
%
% Creates assertions based on the items read from the stream. If the item is a list, it is
% converted into a Prolog fact, otherwise, it is wrapped into a `do_file_item/5` assertion.
%
% @arg InStream The input stream from which to read position information.
% @arg OutStream The output stream where the assertions are written.
% @arg FileName The name of the file being processed, used as part of the assertion.
% @arg Item The S-expression or term being processed, which will be asserted or wrapped in an assertion.
%
% This predicate is responsible for converting items into assertions or facts:
% - If the item is a list, it is turned into a fact where the functor is derived from the `FileName`.
% - Otherwise, it generates a `do_file_item/5` fact which contains the item's position in the file.
make_DL(_InStream, OutStream, FileName, Item) :-
    is_list(Item), !,
    Assertion =.. [FileName | Item],  % Create a fact with the file name as the functor and list as arguments.
    write_readably(OutStream, Assertion), !.  % Write the fact to the output.

make_DL(InStream, OutStream, FileName, Item) :-
    read_position(InStream, Line, Col, CharPos, _),  % Retrieve the position of the item in the file.
    Assertion = (:- do_file_item(Item, Line, Col, CharPos, FileName)),  % Wrap the item in a `do_file_item/5` fact.
    write_readably(OutStream, Assertion), !.  % Write the assertion to the output.

%! write_readably(+OutStream:stream, +Item:term) is det.
%
% Writes a Prolog term to the output stream in a human-readable form.
% @arg OutStream Stream to which the term is written.
% @arg Item The term to be written.
write_readably(OutStream, Item) :-
    write_term(OutStream, Item, [quoted(true)]),
    writeln(OutStream, '.').  % Append a period and a newline.

%! read_sexpr(+Stream:stream, -Item) is det.
%
% Reads a single item (S-expression or comment) from the specified stream, handling different formats and encodings.
% Throws an error with stream position if the S-expression cannot be parsed.
% @arg Stream Stream from which to read.
% @arg Item The item read from the stream.
read_sexpr(I,O):- string(I), open_string(I,S),!,read_sexpr(S,O).
read_sexpr(I,O):- cont_sexpr(')', I, O).
%! cont_sexpr(+EndChar:atom, +Stream:stream, -Item) is det.
%
% Reads a single item (S-expression or comment) from the specified stream, handling different formats and encodings.
% Throws an error with stream position if the S-expression cannot be parsed.
% @arg EndChar Character that denotes the end of a symbol.
% @arg Stream Stream from which to read.
% @arg Item The item read from the stream.
cont_sexpr(EndChar, Stream, Item) :-
    skip_spaces(Stream),  % Ignore whitespace before reading the expression.
    get_char(Stream, Char),
    (   Char = '(' -> read_list(')', Stream, Item)  % If '(', read an S-expression list.
    ;   Char = '[' -> (read_list(']', Stream, It3m), Item = ['[...]',It3m])  % If '[', read an S-expression list.
    ;   Char = '{' -> (read_list('}', Stream, It3m), Item = ['{...}',It3m])  % If '{', read an S-expression list.
    ;   Char = '"' -> read_quoted_string(Stream, '"', Item)  % Read a quoted string.
    ;   Char = '!' -> (read_sexpr(Stream, Subr), Item = exec(Subr))  % Read called directive
    ;   Char = '\'' -> read_quoted_symbol(Stream, '\'', Item)  % Read a quoted symbol.
    ;   Char = '`' -> read_quoted_symbol(Stream, '`', Item)  % Read a backquoted symbol.
    ;   Char = end_of_file -> Item = end_of_file  % If EOF, set Item to 'end_of_file'.
    ; read_symbolic(EndChar, Stream, Char, Item)            % Otherwise, read a symbolic expression.
    ), !.

%! throw_stream_error(+Stream:stream, +Reason:term) is det.
%
% Throws an error including the current stream position for better debugging.
% @arg Stream The input stream.
% @arg Reason The reason for the error.
throw_stream_error(Stream, Reason) :-
    read_position(Stream, Line, Col, CharPos, _),
    throw(error(stream_error(Line:Col:CharPos, Reason))).

%! read_single_line_comment(+Stream:stream) is det.
%
% Reads a single-line comment from the stream and asserts it with the position.
% A comment starts with ';' and continues to the end of the line.
% @arg Stream The input stream from which to read.
read_single_line_comment(Stream) :-
    % read_char(Stream, ';'),  % Skip the ';' character.
    read_position(Stream, Line, Col, CharPos, Pos),
    read_line_to_string(Stream, Comment),
    assertz(metta_file_comment(Line, Col, CharPos, '$COMMENT'(Comment, Line, Col), Pos)).

%! read_position(+Stream:stream, -Line:integer, -Col:integer, -CharPos:integer) is det.
%
% Reads the current line, column, and character position from the input stream.
% @arg Stream Stream from which to read position.
% @arg Line The current line number.
% @arg Col The current column number.
% @arg CharPos The current character position in the stream.
% @arg Position The current $postion/3 Term of the stream.
read_position(Stream, Line, Col, CharPos, Position) :-
    stream_property(Stream, position(Position)),  % Get the current position from the stream.
    stream_position_data(line_count, Position, Line),  % Extract the line number.
    stream_position_data(line_position, Position, Col),  % Extract the column number.
    stream_position_data(char_count, Position, CharPos).  % Extract the character position.

%! skip_spaces(+Stream:stream) is det.
%
% Skips spaces, single-line comments (starting with `;`), and block comments (between `/*` and `*/`),
% including nested block comments. It continues to read until a non-space, non-comment character is encountered.
%
% @arg Stream The stream from which to read and skip spaces/comments.
skip_spaces(Stream) :-
    peek_char(Stream, Char),
    (   Char = ';' ->
        ( read_single_line_comment(Stream),  % If the character is ';', read a single-line comment.
          skip_spaces(Stream))  % After reading the comment, continue skipping spaces.
    ;   Char = '/' ->
        skip_block_comment(Stream)  % Check if this is the start of a block comment.
    ;   is_like_space(Char) ->
        ( get_char(Stream, _),  % Consume the space character.
          skip_spaces(Stream))  % Continue skipping spaces.
    ;   true  % Non-space, non-comment character found; stop skipping.
    ), !.

%! is_like_space(+Char:char) is semidet.
%
% Checks if a character is a space or similar (e.g., tabs, newlines).
%
% @arg Char The character to check.
is_like_space(Char):- char_type(Char,white),!.
is_like_space(Char):- char_type(Char,end_of_line),!.
is_like_space(Char):- char_type(Char,space),!.
is_like_space(Char):- char_type(Char,cntrl),!.

%! skip_block_comment(+Stream:stream) is det.
%
% Skips over a block comment (starting with `/*` and ending with `*/`), supporting nested block comments.
% The function captures the block comment along with its position and stores it in the database.
%
% @arg Stream The input stream from which to skip the block comment.
skip_block_comment(Stream) :-
    peek_string(Stream, 2, LookAhead),
    (   LookAhead = "/*" ->
        read_block_comment(Stream)  % If we see the block comment start, read and handle it.
    ;   true  % Otherwise, no block comment, continue processing.
    ).

%! read_block_comment(+Stream:stream) is det.
%
% Reads a block comment (including nested block comments) from the stream
% and asserts it with the starting position. A block comment starts with '/*' and
% continues until the closing '*/'.
%
% @arg Stream The input stream from which to read the block comment.
read_block_comment(Stream) :-
    read_position(Stream, Line, Col, CharPos, Pos),  % Capture the start position.
    get_string(Stream, 2, _),  % Skip the '/*' characters.
    read_nested_block_comment(Stream, 1, Chars),  % Read the block comment, supporting nested ones.
    string_chars(Comment, Chars),
    assertz(metta_file_comment(Line, Col, CharPos, '$COMMENT'(Comment, Line, Col), Pos)).

%! read_nested_block_comment(+Stream:stream, +Level:int, -Comment:list) is det.
%
% Reads a block comment (including nested block comments) and returns the comment as a list of characters.
% The comment starts with '/*' and continues until the closing '*/', supporting nesting.
%
% @arg Stream The stream from which to read.
% @arg Level  The current level of block comment nesting (initially 1 when called from `read_block_comment`).
% @arg Comment The list of characters read within the block comment.
read_nested_block_comment(Stream, Level, Comment) :-
    read_nested_block_comment(Stream, Level, [], Comment).

read_nested_block_comment(Stream, Level, Acc, Comment) :-
    peek_string(Stream, 2, LookAhead),
    (   LookAhead = "*/" ->
        (   get_string(Stream, 2, _),  % Consume the '*/'.
            NewLevel is Level - 1,  % Decrease the nesting level.
            (   NewLevel = 0 ->
                reverse(Acc, Comment)  % If outermost comment is closed, return the accumulated comment.
            ;   read_nested_block_comment(Stream, NewLevel, ['*', '/' | Acc], Comment)  % Continue, append '*/'.
            )
        )
    ;   LookAhead = "/*" ->
        (   get_string(Stream, 2, _),  % Consume the '/*'.
            NewLevel is Level + 1,  % Increase the nesting level.
            read_nested_block_comment(Stream, NewLevel, ['/', '*' | Acc], Comment)  % Continue, append '/*'.
        )
    ;   (   get_char(Stream, Char),  % Read any other character.
            read_nested_block_comment(Stream, Level, [Char | Acc], Comment)  % Accumulate the character and continue.
        )
    ).


%! read_list(+EndChar:atom, +Stream:stream, -List:list) is det.
%
% Reads a list from the stream until the closing parenthesis is encountered.
% It skips comments while reading the list but asserts them with their positions.
% Throws an error with stream position if the list cannot be parsed correctly.
% @arg Stream Stream from which to read.
% @arg List The list read from the stream.
% @arg EndChar Character that denotes the end of the list.
read_list(EndChar, Stream, List) :-
    skip_spaces(Stream),  % Skip any leading spaces before reading.
    peek_char(Stream, Char), !,
    ( Char = EndChar ->  % Closing parenthesis signals the end of the list.
        get_char(Stream, _),  % Consume the closing parenthesis.
        List = []
    ; Char = end_of_file ->  % Unexpected end of file inside the list.
        throw_stream_error(Stream, syntax_error(unexpected_end_of_file, "Unexpected end of file in list"))
    ; ( cont_sexpr(EndChar, Stream, Element),  % Read the next S-expression.
        read_list(EndChar, Stream, Rest),  % Continue reading the rest of the list.
        List = [Element | Rest])  % Add the element to the result list.
    ), !.

%! read_quoted_string(+Stream:stream, +EndChar:atom, -String:atom) is det.
%
% Reads a quoted string from the stream until the corresponding ending quote is found.
% Handles escape sequences within the string.
% Throws an error with stream position if the quoted string cannot be parsed.
% @arg Stream Stream from which to read.
% @arg EndChar Character that denotes the end of the quoted string.
% @arg String The string read from the stream.
read_quoted_string(Stream, EndChar, String) :-
    read_until_char(Stream, EndChar, Chars),  % Read characters until the ending quote.
    string_chars(String, Chars).  % Convert the list of characters to a string.

%! read_quoted_symbol(+Stream:stream, +EndChar:atom, -Symbol:atom) is det.
%
% Reads a quoted symbol from the stream, handling escapes and storing the result as a symbol.
% Throws an error with stream position if the quoted symbol cannot be parsed.
% @arg Stream Stream from which to read.
% @arg EndChar Character that closes the quoted symbol.
% @arg Symbol The symbol read from the stream.
read_quoted_symbol(Stream, EndChar, Symbol) :-
    read_until_char(Stream, EndChar, Chars),
    ((EndChar == '\'', Chars = [Char])
             -> Symbol='#\\'(Char); atom_chars(Symbol, Chars)).

%! read_until_char(+Stream:stream, +EndChar:atom, -Chars:list) is det.
%
% Reads characters from the stream until the specified end character is encountered.
% This function is used to help read quoted strings and symbols.
% Throws an error with stream position if the end character is not found.
% @arg Stream Stream from which to read.
% @arg EndChar Character that indicates the end of the reading.
% @arg Chars List of characters read until the end character.
read_until_char(Stream, EndChar, Chars) :-
    get_char(Stream, Char),
    (   Char = end_of_file -> throw_stream_error(Stream, unexpected_end_of_file(read_until_char(EndChar)))
    ;   Char = EndChar -> Chars = []
    ;   Char = '\\' -> get_char(Stream, NextChar),
                       read_until_char(Stream, EndChar, RestChars),
                       Chars = [NextChar | RestChars]
    ;   read_until_char(Stream, EndChar, RestChars),
        Chars = [Char | RestChars]
    ).

%! read_symbolic(+EndChar:atom, +Stream:stream, +FirstChar:atom, -Symbolic:atom) is det.
%
% Reads a symbolic expression starting with a specific character, possibly incorporating more complex syntaxes.
% Throws an error with stream position if the symbolic expression cannot be parsed.
% @arg EndChar Character that indicates the end of the reading unless escaped.
% @arg Stream Stream from which to read.
% @arg FirstChar The first character of the symbolic expression.
% @arg Symbolic The complete symbolic expression read.
read_symbolic(EndChar, Stream, FirstChar, Symbolic) :-
    read_symbolic_cont(EndChar, Stream, RestChars),
    classify_and_convert_charseq([FirstChar| RestChars], Symbolic), !.

%! classify_and_convert_charseq(+Chars:list, -Symbolic:term) is det.
%
% Classifies and converts a sequence of characters into a Prolog term,
% handling special cases like variables, numbers, and symbolic terms.
%
% @param Chars    The input list of characters.
% @param Symbolic The resultant Prolog term or symbol, which could be a variable,
%                 number, or an atom.
classify_and_convert_charseq(Chars, Symbolic) :-
    % First, classify and convert the character sequence using the helper predicate.
    classify_and_convert_charseq_(Chars, Symbol),

    % If the classified symbol is an integer, and the original characters contain a '.',
    % convert it to a floating point number.
    ( ( integer(Symbol), memberchk('.', Chars))
      -> Symbolic is Symbol * 1.0   % Convert to floating-point number.
       ; Symbolic = Symbol).        % Otherwise, keep the symbol as is.


%! classify_and_convert_charseq_(+Chars:list, -Symbolic:term) is det.
%
% Helper predicate that attempts to classify the character sequence.
% Handles special cases such as Prolog variables and numbers.
%
% @param Chars    The input list of characters.
% @param Symbolic The resultant Prolog term or symbol.

% Case 1: If the character sequence starts with '$', treat it as a variable.
classify_and_convert_charseq_(['$'| RestChars], '$VAR'(Symbolic)) :-
    !,
    atom_chars(Symbolic, ['_'|RestChars]).  % Convert the rest of the characters into a variable name.
% Case 2: Attempt to interpret the characters as a Prolog term using `read_from_chars/2`.
% This handles more complex syntaxes like numbers, dates, etc.
classify_and_convert_charseq_(Chars, Symbolic) :-
    notrace(catch(read_from_chars(Chars, Symbolic), _, fail)),  % Safely attempt to parse the characters.
    atomic(Symbolic),  % Ensure the result is atomic.
    !.
% Case 3: If no other case applies, convert the characters directly into an atom.
classify_and_convert_charseq_(Chars, Symbolic) :-
    atom_chars(Symbolic, Chars).  % Convert the character sequence into an atom.


%! read_symbolic_cont(+EndChar:atom, +Stream:stream, -Chars:list) is det.
%
% Continues reading symbolic characters from the stream until a delimiter is encountered.
% If a backslash is followed by a delimiter, the delimiter is added as a regular character.
% @arg EndChar Character that indicates the end of the reading unless escaped.
% @arg Stream Stream from which to read.
% @arg Chars List of characters read, forming part of a symbolic expression.
read_symbolic_cont(EndChar, Stream, Chars) :-
    peek_char(Stream, NextChar),
    (   is_delimiter(NextChar) -> Chars = []  % Stop when a delimiter is found.
    ;   EndChar == NextChar -> Chars = []  % Stop when an EndChar is found.
    ; ( get_char(Stream, NextChar),
        (   NextChar = '\\' ->  % If it's a backslash, read the next char.
          ( get_char(Stream, EscapedChar),
            read_symbolic_cont(EndChar, Stream, RestChars),
            Chars = [EscapedChar | RestChars] ) % Add the escaped char normally.
        ; ( read_symbolic_cont(EndChar, Stream, RestChars),
            Chars = [NextChar | RestChars] ) % Continue reading the symbolic characters.
        ))
    ), !.


%! is_delimiter(+Char:atom) is semidet.
%
% Determines if a character is a delimiter for reading symbolic expressions.
% @arg Char Character to check.
is_delimiter(Char) :-
    char_type(Char, space) ;  % Space is a delimiter.
    arg(_, v('(', ')', end_of_file), Char).  % Other delimiters include parentheses and end of file.

% Ensure the program runs upon initialization.
:- initialization(main_init, main).
