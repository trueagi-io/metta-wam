% This file initializes and configures the runtime environment for MeTTaLog .. it is for non RELP user such as
% From the LSP server and transpiled MeTTa files.
% It sets the required directories, attaches necessary packages, configures Prolog flags for mettalog and
% arguments, and ensures that essential libraries are loaded. The environment is prepared for execution
% without starting the REPL, and includes detailed runtime logging and session tracking functionalities.

% Configure arguments specific to SWI-Prolog (this is so the LSP Server when it uses this library doesn't trick MeTTa into processing its args)
:- set_prolog_flag(argv, []),
   set_prolog_flag(os_argv, [swipl]).

% Set the directory context for loading packages; use the default directory if context retrieval fails
:- (prolog_load_context(directory, Value); Value = '.'),
   absolute_file_name('../packs/', Dir, [relative_to(Value)]),
   atom_concat(Dir, 'predicate_streams', PS),
   atom_concat(Dir, 'logicmoo_utils', LU),
   attach_packs(Dir, [duplicate(replace), search(first)]),
   pack_attach(PS, [duplicate(replace), search(first)]),
   pack_attach(LU, [duplicate(replace), search(first)]).

% Set Prolog flags for mettalog runtime and arguments configuration
:- set_prolog_flag(mettalog_rt, true),
   set_prolog_flag(mettalog_rt_args, []),
   set_prolog_flag(metta_argv, []).

% Ensure essential libraries are loaded and initialize the main application
% Uncomment the next line if 'metta_compiler_lib' needs to be loaded as well
% :- user:ensure_loaded(metta_compiler_lib),!,

:- user:ensure_loaded(metta_interp),!,
   % Start interpreter code
   user:loon.

% Flush any pending output to ensure smooth runtime interactions
flush_metta_output :-
    with_output_to(user_error, (write_answer_output, ttyflush)).

% Write out answers in hyperon-experimental format to user_error
metta_runtime_write_answers(List) :-
    with_output_to(user_error, (write('['), write_answers_aux(List), write(']'))).

% Helper predicate to manage answer formatting to user_error
write_answers_aux([]) :- !.
write_answers_aux([H|T]) :-
    with_output_to(user_error, (write_src_woi(H), (T == [] -> true ; write(', '), write_answers_aux(T)))).

% Dynamically describe the current file or an actively reading file, providing context for runtime sessions
file_desc(Message) :-
    prolog_load_context(file, CurrentFile),
    (   stream_property(Stream, mode(read)),
        stream_property(Stream, file_name(File)),
        \+ at_end_of_stream(Stream),
        File \= CurrentFile,
        !,
        sformat(Message, 'File(~w)', [File])
    ;   sformat(Message, 'File(~w)', [CurrentFile])
    ).

:- dynamic(runtime_session/4).

% Begin a runtime session with detailed time recording, output to user_error
begin_metta_runtime :-
    file_desc(Description),
    current_times(WallStart, CPUStart),
    asserta(runtime_session(start, WallStart, CPUStart, Description)),
    with_output_to(user_error, format('~w started.~n', [Description])).

% End a runtime session, calculate and print elapsed times, output to user_error
end_metta_runtime :-
    file_desc(Description),
    (   retract(runtime_session(start, WallStart, CPUStart, Description))
    ->  calculate_elapsed_time(WallStart, CPUStart, WallElapsedTime, CPUElapsedTime),
        print_elapsed_time(WallElapsedTime, CPUElapsedTime, Description)
    ;   with_output_to(user_error, format('Error: No runtime session start information found for "~w".~n', [Description]))
    ).

% Wall and CPU time
current_times(WallStart, CPUStart) :-
    get_time(WallStart),
    statistics(cputime, CPUStart).

% Calculate elapsed times
calculate_elapsed_time(WallStart, CPUStart, WallElapsedTime, CPUElapsedTime) :-
    current_times(WallEnd, CPUEnd),
    WallElapsedTime is WallEnd - WallStart,
    CPUElapsedTime is CPUEnd - CPUStart.

% Print the elapsed wall and CPU time with a description, output to user_error
print_elapsed_time(WallElapsedTime, CPUElapsedTime, Description) :-
    with_output_to(user_error,
        format('             % Walltime: ~9f seconds, CPUtime: ~9f seconds for ~w~n',
               [WallElapsedTime, CPUElapsedTime, Description])).

% Execute a Prolog query and handle output, performance logging, and time measurements to user_error
do_metta_runtime(Var, Call) :-
    functor(Call, Func, _),
    atom_concat('Testing ', Func, Description),
    current_times(WallStart, CPUStart),
    % Execute the query and collect results
    with_output_to(user_error, findall(Var, Call, List)),
    % Record stop time
    calculate_elapsed_time(WallStart, CPUStart, WallElapsedTime, CPUElapsedTime),
    % Show results
    with_output_to(user_error, metta_runtime_write_answers(List)),
    % Print elapsed time
    print_elapsed_time(WallElapsedTime, CPUElapsedTime, Description),
    flush_metta_output.

