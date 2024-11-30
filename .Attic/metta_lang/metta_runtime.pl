
% File that is loaded from LSP server and transpiled MeTTa files so all the options such as not starting the REPL and things happen

% Predicate to log the files that are currently open for reading excluding this file
print_reading_file(Message) :-
    prolog_load_context(file, CurrentFile),
    stream_property(Stream,file_name(File)),
    \+ at_end_of_stream(Stream),
    File \= CurrentFile,!,
    format('~N%File: ~w ~w~n',[File, Message]).
print_reading_file(Message):-
    prolog_load_context(file, File),
    format('~N%File: ~w ~w~n',[File, Message]).

% Set the directory context for loading packages; if prolog_load_context fails, use '.' as default
:- (prolog_load_context(directory, Value); Value='.'),
   absolute_file_name('../packs/', Dir, [relative_to(Value)]),
   atom_concat(Dir, 'predicate_streams', PS),
   atom_concat(Dir, 'logicmoo_utils', LU),
   attach_packs(Dir, [duplicate(replace), search(first)]),
   pack_attach(PS, [duplicate(replace), search(first)]),
   pack_attach(LU, [duplicate(replace), search(first)]).

% Set Prolog flags for mettalog runtime and argument configuration
:- set_prolog_flag(mettalog_rt, true),
   set_prolog_flag(mettalog_rt_args, []),
   set_prolog_flag(metta_argv, []).

% Set Prolog flags for argument vector and OS argument vector, focused on SWI-Prolog
:- set_prolog_flag(argv, []),
   set_prolog_flag(os_argv, [swipl]).

% Ensure certain libraries are loaded and start the main application
% Uncomment the next line if 'metta_compiler_lib' needs to be loaded separately
% :- user:ensure_loaded(metta_compiler_lib).
:- user:ensure_loaded(metta_interp),!,
   % calls metta interp bootstrap
   user:loon.

% Flush any pending output to the terminal or log file
flush_answer_output :- write_answer_output, ttyflush.

% Write out answers in a hyperon-experimental format
metta_runtime_write_answers(List) :-
   write('['), write_answers_aux(List), write(']'), nl.

% Helper to write answers or finish if the list is empty
write_answers_aux(List) :- List == [], !.
write_answers_aux([H|List]) :- List == [], !, write_src_woi(H).
write_answers_aux([H|List]) :- write_src_woi(H), write(', '), write_answers_aux(List).

% Record starting time
record_start_time(WallStart, CPUStart) :-
    statistics(walltime, [WallStart|_]),
    statistics(cputime, CPUStart).

% Calculate elapsed time and print it with a descriptive message
print_elapsed_time(WallStart, CPUStart, Description) :-
    statistics(walltime, [WallEnd|_]),
    statistics(cputime, CPUEnd),
    WallTime is WallEnd - WallStart,
    CPUTime is CPUEnd - CPUStart,
    format('Elapsed time for ~w - Wall time: ~w ms, CPU time: ~w seconds~n', [Description, WallTime, CPUTime]).

% Execute a Prolog query and handle output, performance logging, and time measurements
do_metta_runtime(Var, Call) :-
    functor(Call, Func, _),
    atom_concat('Testing ', Func, Description),
    record_start_time(WallStart, CPUStart),
    with_output_to(user_error, findall(Var, Call, List)),
    with_output_to(user_error, metta_runtime_write_answers(List)),
    print_elapsed_time(WallStart, CPUStart, Description),
    flush_answer_output.

% Start of a runtime session for logging and debugging with time recording
begin_metta_runtime :-
    Description = 'Starting test session',
    flush_answer_output,
    record_start_time(_, _),
    print_elapsed_time(_, _, Description).

% End of a runtime session; used to output timing and debugging information
end_metta_runtime :-
    Description = 'Ending test session',
    print_elapsed_time(_, _, Description),
    flush_answer_output.

