%!  metta_python_patcher(-Content) is det.
%
%   A dynamic predicate that stores the content of the Python file 'metta_python_patcher.py'.
%   The content is read into a string and asserted as a fact for later use.
%   This allows the Python code to be loaded and used at runtime in Prolog.
%
%   @arg Content A string representing the contents of the 'metta_python_patcher.py' file.
%
%   @example
%   % Reading and asserting the Python patcher file content:
%   ?- metta_python_patcher(Content).
%   Content = "... Python code ...".
%
:- dynamic(metta_python_patcher/1).

% Read the content of the file './metta_python_patcher.py' into the variable `String`.
% Then, assert the content of the file as a fact `metta_python_patcher/1`.
:- read_file_to_string('./metta_python_patcher.py', String, []),
   assertz(metta_python_patcher(String)), !.

%!  did_load_metta_python_patcher is det.
%
%   A volatile predicate used as a flag to track whether the Python patcher has been loaded.
%   This predicate will not be saved across sessions and exists only for the current runtime.
%
%   @example
%   % After loading the patcher, this will succeed:
%   ?- did_load_metta_python_patcher.
%
:- dynamic(did_load_metta_python_patcher/0).
:- volatile(did_load_metta_python_patcher/0).

%!  load_metta_python_patcher is det.
%
%   Loads the Python patcher into the system by reading the Python code from `metta_python_patcher/1`.
%   It first checks if `did_load_metta_python_patcher/0` is already asserted, indicating the patcher
%   has been loaded previously. If not, it retrieves the content of the Python file and loads the module
%   via `py_module/2`. After successfully loading, it asserts `did_load_metta_python_patcher/0` to prevent
%   reloading.
%
%   @example
%   % Loading the Python patcher:
%   ?- load_metta_python_patcher.
%   true.
%
load_metta_python_patcher :- !. % disable the patcher 
load_metta_python_patcher :-
    did_load_metta_python_patcher, !.  % If already loaded, do nothing.

load_metta_python_patcher :-
    % Retrieve the content of the Python patcher file.
    metta_python_patcher(String),
    % Load the Python patcher module via py_module/2.
    py_module(metta_python_patcher, String),
    % Assert that the patcher has now been loaded.
    assert(did_load_metta_python_patcher), !,
    % Attempt to load the module again, ignoring errors.
    ignore(notrace(with_safe_argv(catch(py_module(metta_python_patcher, String), _, true)))), !.

% Ensure that `load_metta_python_patcher/0` is called when the program is initialized (on startup).
% This will trigger the loading of the Python patcher module during initialization.

%:- initialization(load_metta_python_patcher).
%:- initialization(load_metta_python_patcher, restore).

%!  patch_hyperonpy is det.
%
%   Loads the Python patcher (if not already loaded) and calls the Python function `patch_hyperonpy/0`
%   from the `metta_python_patcher` module. The result is expected to be an empty Python dictionary (`py{}`).
%
%   @example
%   % Applying the patch to Hyperon:
%   ?- patch_hyperonpy.
%   true.
%
patch_hyperonpy :-
    load_metta_python_patcher,  % Ensure the patcher is loaded.
    py_call(metta_python_patcher:patch_hyperonpy(), O), !,
    O = py{}.

%!  load_hyperonpy is det.
%
%   Loads the Hyperon Python module by first applying the necessary patches using `patch_hyperonpy/0`.
%
%   @example
%   % Load the Hyperon module:
%   ?- load_hyperonpy.
%   true.
%
load_hyperonpy :-
    patch_hyperonpy.

%!  load_mettalogpy is det.
%
%   Loads the `mettalog` Python module and optionally attempts to load the `hyperon` module.
%   The `nop/1` around the second `py_exec/1` ensures that loading `hyperon` does not raise an error.
%
%   @example
%   % Load mettalog and hyperon (if available):
%   ?- load_mettalogpy.
%   true.
%
load_mettalogpy :-
    py_exec("import mettalog"),
    nop(py_exec("import hyperon")).

%!  mettalogpy_repl is det.
%
%   Starts the REPL (Read-Eval-Print Loop) for the `mettalog` Python module by invoking the `repl/0` function.
%
%   @example
%   % Start the mettalog REPL:
%   ?- mettalogpy_repl.
%   true.
%
mettalogpy_repl :- 
    load_metta_python_proxy,
    load_metta_python_patcher,
    load_mettalogpy,
    py_call(mettalog:repl()).


%!  maybe_load_metta_python_patcher is det.
%
%   This predicate ensures that the Python integration for Metta is loaded.
%   It tries to load the Python interface lazily by calling `lazy_load_python/0`.
%   If the lazy loading succeeds (determined by the cut `!`), it does nothing more.
%   If lazy loading fails, it proceeds to load the Metta Python patcher using 
%   `load_metta_python_patcher/0`.
%
maybe_load_metta_python_patcher :- 
    % Attempt lazy loading of the Python interface.
    lazy_load_python, !.
maybe_load_metta_python_patcher :- 
    % If lazy loading fails, load the Metta Python patcher manually.
    load_metta_python_patcher.

% The following directives ensure that `maybe_load_metta_python_patcher/0` is called 
% during system initialization. The first initialization runs when the program 
% starts, and the second runs when the system is restored from a saved state.
:- initialization(maybe_load_metta_python_patcher).

:- initialization(maybe_load_metta_python_patcher, restore).

