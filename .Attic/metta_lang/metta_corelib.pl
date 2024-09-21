
% Declare `metta_python_patcher/1` as dynamic so it can be modified at runtime.
:- dynamic(metta_python_patcher/1).
% Read the content of the file './metta_python_patcher.py' into the variable `String`.
% Then, assert the content of the file as a fact `metta_python_patcher/1`.
:- read_file_to_string('./metta_python_patcher.py', String, []),
   assertz(metta_python_patcher(String)),!.
% Declare `did_load_metta_python_patcher/0` as volatile, meaning it will not be saved to a saved state.
% This is useful when you don't want this predicate to persist across sessions or save states.
:- dynamic(did_load_metta_python_patcher/0).
:- volatile(did_load_metta_python_patcher/0).
% If `did_load_metta_python_patcher/0` is not already asserted, it asserts the fact to indicate that the patcher has been loaded.
% It retrieves the `metta_python_patcher/1` fact (which contains the content of the file).
% Then, it calls `py_module/2` with the module name and the Python code as arguments.
% The cut (`!`) ensures no backtracking occurs once this is executed.

load_metta_python_patcher :- did_load_metta_python_patcher, !.
 load_metta_python_patcher :-
    %assert(did_load_metta_python_patcher),
    metta_python_patcher(String),
    py_module(metta_python_patcher, String),
    assert(did_load_metta_python_patcher),!,
     ignore(notrace(with_safe_argv(catch(py_module(metta_python_patcher, String),_,true)))),!.
 % Ensure that `load_metta_python_patcher/0` is called when the program is initialized (on startup).
 % This will trigger the loading of the Python patcher module during initialization.

%:- initialization(load_metta_python_patcher).
%:- initialization(load_metta_python_patcher,restore).
patch_hyperonpy:-
  load_metta_python_patcher,
  py_call(metta_python_patcher:patch_hyperonpy(),O),!,O=py{}.

load_hyperonpy:- patch_hyperonpy.
load_mettalogpy:-
   py_exec("import mettalog"),
   nop(py_exec("import hyperon")).

mettalogpy_repl:- py_call(mettalog:repl()).

