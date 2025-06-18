

:- set_prolog_flag(source_search_working_directory, false).

%:- writeln('use_module(library(readline)).').
%:- writeln('use_module(library(editline)).').

:- use_module(library(readline)).
:- use_module(library(editline)).



maybe_load_editline:-
    history_file(Path),
    ( exists_source(library(editline)) ->
        use_module(library(editline)),
        %editline:read_history(Path),
        %at_halt(editline:write_history),
        format(user_error, 'editline loaded. History enabled at ~w~n', [Path])), !.

maybe_load_readline :-
    history_file(Path),
    ( exists_source(library(readline)) ->
        use_module(library(readline)),
        %readline:read_history(Path),
        %at_halt(readline:write_history),
        format(user_error, 'readline loaded. History enabled at ~w~n', [Path])), !.

maybe_load_history :-
    maybe_load_editline -> true ; maybe_load_readline -> true ;
    format(user_error, 'readline nor editline is available. History not enabled.~n', []).


history_file(Path) :-
    ( current_prolog_flag(windows, true) ->
        getenv('APPDATA', AppData),
        atomic_list_concat([AppData, '\\SWI-Prolog\\swipl_history.txt'], Path)
    ; current_prolog_flag(apple, true) ->
        getenv('HOME', Home),
        atomic_list_concat([Home, '/Library/Application Support/SWI-Prolog/swipl_history.txt'], Path)
    ; getenv('HOME', Home),  % Unix/Linux fallback
        atomic_list_concat([Home, '/.swipl_history'], Path)
    ).



:- dynamic(transpiler_depends_on/4).
:- volatile(transpiler_depends_on/4).
:- discontiguous(transpiler_depends_on/4).
:- multifile(transpiler_depends_on/4).

:- dynamic(transpiler_stored_eval/3).
:- volatile(transpiler_stored_eval/3).
:- discontiguous(transpiler_stored_eval/3).
:- multifile(transpiler_stored_eval/3).


:- dynamic(transpiler_predicate_store/7).
:- discontiguous(transpiler_predicate_store/7).
:- multifile(transpiler_predicate_store/7).

:- dynamic(transpiler_predicate_nary_store/9).
:- discontiguous(transpiler_predicate_nary_store/9).
:- multifile(transpiler_predicate_nary_store/9).

:- dynamic(transpiler_clause_store/9).
:- discontiguous(transpiler_clause_store/9).
:- multifile(transpiler_clause_store/9).

:- dynamic(arg_type_n/4).
:- volatile(arg_type_n/4).
:- discontiguous(arg_type_n/4).


:-dynamic(pred_uses_fallback/2).
:-dynamic(pred_uses_impl/2).

:- dynamic(metta_function_asserted/3).
:- multifile(metta_function_asserted/3).
:- dynamic(metta_other_asserted/2).
:- multifile(metta_other_asserted/2).
:- dynamic(metta_function_asserted/3).
:- multifile(metta_function_asserted/3).
:- dynamic(metta_atom_asserted/2).
:- multifile(metta_atom_asserted/2).
:- dynamic(metta_atom_deduced/2).
:- multifile(metta_atom_deduced/2).
:- dynamic(metta_atom_in_file/2).
:- multifile(metta_atom_in_file/2).
:- dynamic(metta_atom_asserted_last/2).
:- multifile(metta_atom_asserted_last/2).


%:- initialization(maybe_load_history, now).

    %:- ensure_loaded(library(metta_lang/metta_interp)).
:- ensure_loaded(library(metta_rt)). % avoids starting the REPL
:- setup_library_calls.
:- dynamic(top_call/0).
:- multifile(top_call/0).
top_call:- fail.


% :- initialization(transpiled_main, program).

transpiled_ideas:-
   writeln("
Examples:
?- repl.
?- rtrace(top_call2).
?- guidebugger.
?- set_option_value(e,trace).
?- set_option_value(compile, false),
   set_option_value(compile, full),
   set_option_value(compile, hybrid),
   "),
  listing(top_call).

transpiled_main :-
  predicate_property(transpiled_main,number_of_clauses(1)),
  transpiled_ideas, prolog, !.

