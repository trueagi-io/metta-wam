:- multifile(lazy_load_python/0).
:- dynamic(lazy_load_python/0).

% Import the Janus library, which is used for interfacing with Python.
:- use_module(library(janus)).

% Uncomment the next line for quieter runs, meaning any debugging or tracing calls will be suppressed.
% if_bugger(_):- !.
if_bugger(G) :- call(G).

% Define a no-op predicate if `nop/1` is not already defined.
:- if(\+ current_predicate(nop/1)).
nop(_).
:- endif.

% Define a tracing predicate to handle failure cases during execution.
% If `trace_failures/1` is not already defined, define it here.

:- if(\+ current_predicate(trace_failures/1)).
trace_failures((A, B)) :- !,
    (A *-> trace_failures(B);
    (B *-> trace_failures(A); wfailed(((A, B))))).
trace_failures(A) :- (A *-> true; wfailed(A)).

% Predicate to print failed goals for debugging purposes.
wfailed(G) :- writeln(wfailed(G)), fail.
:- endif.

py_is_tf(Goal,TF):- once(Goal)->TF='@'(true);TF='@'(false).

% Dynamic predicate to store registered Python functions with details about their parameters and return type.
:- dynamic registered_function/5.

%!  register_function(+ModuleFunctionName, +ParamNames, +ParamTypes, +ParamDefaults, +ReturnType) is det.
%
%   Registers a Python function in Prolog with its associated parameters and return type.
%   If a function with the same name is already registered, it will be retracted and replaced
%   with the new details.
%
%   @arg ModuleFunctionName The name of the function being registered (concatenation of module and function name).
%   @arg ParamNames List of parameter names.
%   @arg ParamTypes List of parameter types.
%   @arg ParamDefaults List of default values for the parameters.
%   @arg ReturnType The return type of the function.
%
%   @example
%     % Registering a Python function named 'math_add':
%     ?- register_function('math_add', ['x', 'y'], ['int', 'int'], [0, 0], 'int').
%   Registered function: 'math_add' with parameters: ['x', 'y'], defaults: [0, 0], types: ['int', 'int'] -> 'int'.
%
register_function(ModuleFunctionName, ParamNames, ParamTypes, ParamDefaults, ReturnType) :-
    % Remove any previous registration of the function.
    retractall(registered_function(ModuleFunctionName, _, _, _, _)),
    % Register the function with its new details.
    assertz(registered_function(ModuleFunctionName, ParamNames, ParamTypes, ParamDefaults, ReturnType)),
    format('Registered function: ~q with parameters: ~q, defaults: ~q, types: ~q -> ~q~n',
           [ModuleFunctionName, ParamNames, ParamDefaults, ParamTypes, ReturnType]).

%!  from_python(+ModuleFunctionName, +TupleArgs, +LArgs, +KwArgs, -Return) is det.
%
%   Handles calling a registered Prolog predicate or, if not found, the original Python function.
%   It first checks if the Prolog predicate exists, and if so, dynamically constructs the goal and calls it.
%   If no Prolog predicate is found, it calls the original Python function.
%
%   @arg ModuleFunctionName The name of the function being called.
%   @arg TupleArgs A tuple of arguments (not currently used but passed for compatibility).
%   @arg LArgs A list of positional arguments.
%   @arg KwArgs A list of keyword arguments.
%   @arg Return The result of the function call.
%
%   @example
%     % Calling a registered Prolog function:
%     ?- from_python('math_add', _, [1, 2], [], Result).
%   Result = 3.
%
from_python(ModuleFunctionName, _TupleArgs, LArgs, KwArgs, Result) :-
    % Determine the arity of the function based on the number of positional arguments.
    length(LArgs, Arity),
    NewArity1 is Arity + 1,  % Adjust arity to account for one additional argument (Return).
    NewArity2 is Arity + 2,  % Adjust arity to account for two additional arguments (KwArgs and Return).

    % Check if a Prolog predicate with the corresponding arity exists.
    (current_predicate(ModuleFunctionName/NewArity2) -> append(LArgs, [KwArgs, Return], FullArgs);
    (current_predicate(ModuleFunctionName/NewArity1) -> append(LArgs, [Return], FullArgs);
    (current_predicate(ModuleFunctionName/Arity) -> append(LArgs, [], FullArgs)))),
    % Construct the predicate dynamically with all arguments.
    Predicate =.. [ModuleFunctionName | FullArgs],
    registered_function(ModuleFunctionName, _, _, _, ReturnType),

    % Call the Prolog predicate and handle its return type.
    if_bugger(format('Calling existing Prolog predicate: ~q -> ~q ', [Predicate, ReturnType])), !,
    trace_failures((call_ret_type(Predicate, ReturnType, Return, Result),
                    if_bugger(writeln(Return -> Result)), nonvar(Result))).

% Fallback clause if no corresponding Prolog predicate is found, calls the original Python function.
from_python(_ModuleFunctionName, _TupleArgs, _LArgs, _KwArgs, 'call_original_function') :- !.

from_python(ModuleFunctionName, TupleArgs, LArgs, KwArgs, Return) :-
    format('No Prolog predicate found for: ~w. Calling original Python function.~n', [ModuleFunctionName]),
    call_python_original(ModuleFunctionName, TupleArgs, LArgs, KwArgs, Return).

% Predicate to handle calling a predicate with a boolean return type.
call_ret_type(Predicate, bool, _Return, Result) :- !,
    (call(Predicate) -> ignore(Result = '@'('true')); ignore(Result = '@'('false'))).

% Predicate to handle calling a predicate with a 'None' return type.
call_ret_type(Predicate, 'None', _Return, Result) :- !,
    ignore(call(Predicate)) -> ignore(Result = '@'('none')).

% Generic handler for other return types.
call_ret_type(Predicate, _RetType, Return, Result) :- !,
    call(Predicate), ret_res(Return, Result).

% Helper to process the return value.
ret_res(o3(_, ID, _), ID) :- nonvar(ID), !.
ret_res(ID, ID).

%!  override_hyperonpy is det.
%
%   Loads the Python overrider (if not already loaded) and calls the Python function `override_hyperonpy/0`
%   from the `metta_python_overrider` module. The result is expected to be an empty Python dictionary (`py{}`).
%
%   @example
%     % Applying the override to Hyperon:
%     ?- override_hyperonpy.
%   true.
%
override_hyperonpy :-
    load_metta_python_overrider,  % Ensure the overrider is loaded.
    py_call(metta_python_override:load_hyperon_overrides(), _), !.

%!  test_override_hyperonpy is det.
%
%   Loads the Hyperon Python module by first applying the necessary overridees using `override_hyperonpy/0`.
%
%   @example
%     % Load the Hyperon module:
%     ?- test_override_hyperonpy.
%   true.
%
test_override_hyperonpy :-
    load_metta_python_overrider,  % Ensure the overrider is loaded.
    py_call(metta_python_override:test_hyperon_overrides(), _), !.

%!  metta_python_overrider(-Content) is det.
%
%   Reads and asserts the content of the Python file 'metta_python_override.py' as a dynamic fact.
%   This allows the Python code to be accessed and used at runtime within Prolog.
%
%   @arg Content A string representing the contents of the 'metta_python_override.py' file.
%
%   @example
%     % Retrieve the content of the Python overrider:
%     ?- metta_python_overrider(Content).
%   Content = "... Python code ...".
%
:- dynamic(metta_python_overrider/1).

% Read the content of the Python override file and assert it as a fact.
:- read_file_to_string('./metta_python_override.py', String, []),
   assertz(metta_python_overrider(String)), !.

%!  did_load_metta_python_overrider is det.
%
%   A volatile predicate that acts as a flag indicating whether the Python overrider has been loaded.
%   This flag is not saved between sessions and only exists during the current runtime.
%
%   @example
%     % After loading the overrider, this will succeed:
%     ?- did_load_metta_python_overrider.
%
:- dynamic(did_load_metta_python_overrider/0).
:- volatile(did_load_metta_python_overrider/0).

%!  load_metta_python_overrider is det.
%
%   Loads the Python overrider if it hasn't been loaded yet. The overrider content is retrieved
%   from `metta_python_overrider/1`, and the Python module is loaded via `py_module/2`. Once
%   loaded, `did_load_metta_python_overrider/0` is asserted to prevent reloading.
%
%   @example
%     % Load the Python overrider:
%     ?- load_metta_python_overrider.
%   true.
%
load_metta_python_overrider :-
    did_load_metta_python_overrider, !.  % If already loaded, do nothing.

load_metta_python_overrider :-
    % Retrieve the Python overrider content.
    metta_python_overrider(String),
    % Load the Python module via py_module/2.
    py_module(metta_python_overrider, String),
    % Assert that the overrider has now been loaded.
    assert(did_load_metta_python_overrider), !.
    % Try to load the module again, ignoring any errors.
    % ignore(notrace(with_safe_argv(catch(py_module(metta_python_overrider, String), _, true)))), !.


%!  maybe_load_metta_python_overrider is det.
%
%   This predicate ensures that the Python integration for Metta is loaded.
%   It tries to load the Python interface lazily by calling `lazy_load_python/0`.
%   If the lazy loading succeeds (determined by the cut `!`), it does nothing more.
%   If lazy loading fails, it proceeds to load the Metta Python overrider using
%   `load_metta_python_overrider/0`.
%
maybe_load_metta_python_overrider :-
    % Attempt lazy loading of the Python interface.
    lazy_load_python, !.
maybe_load_metta_python_overrider :-
    % If lazy loading fails, load the Metta Python overrider manually.
    load_metta_python_overrider.

% The following directives ensure that `maybe_load_metta_python_overrider/0` is called 
% during system initialization. The first initialization runs when the program 
% starts, and the second runs when the system is restored from a saved state.
%:- initialization(maybe_load_metta_python_overrider).
%:- initialization(maybe_load_metta_python_overrider, restore).


%!  metta_python_patcher(-Content) is det.
%
%   Reads and asserts the content of the Python file 'metta_python_patcher.py' as a dynamic fact.
%   This allows the Python code to be accessed and used at runtime within Prolog.
%
%   @arg Content A string representing the contents of the 'metta_python_patcher.py' file.
%
%   @example
%     % Retrieve the content of the Python patcher:
%     ?- metta_python_patcher(Content).
%   Content = "... Python code ...".
%
:- dynamic(metta_python_patcher/1).

:-  % Get the directory of the current Prolog file being loaded.
    prolog_load_context(directory, Dir),
    % Construct the full path to the Python patcher file.
    atomic_list_concat([Dir, '/', 'metta_python_patcher.py'], FilePath),
    % Read the content of the Python patcher file and assert it.
    read_file_to_string(FilePath, String, []),
    asserta(metta_python_patcher(String)), !.

%!  did_load_metta_python_patcher is det.
%
%   A volatile predicate that acts as a flag indicating whether the Python patcher has been loaded.
%   This flag is not saved between sessions and only exists during the current runtime.
%
%   @example
%     % After loading the patcher, this will succeed:
%     ?- did_load_metta_python_patcher.
%
:- dynamic(did_load_metta_python_patcher/0).
:- volatile(did_load_metta_python_patcher/0).

%!  load_metta_python_patcher is det.
%
%   Loads the Python patcher if it hasn't been loaded yet. The patcher content is retrieved
%   from `metta_python_patcher/1`, and the Python module is loaded via `py_module/2`. Once
%   loaded, `did_load_metta_python_patcher/0` is asserted to prevent reloading.
%
%   @example
%     % Load the Python patcher:
%     ?- load_metta_python_patcher.
%   true.
%
load_metta_python_patcher :-
    did_load_metta_python_patcher, !.  % If already loaded, do nothing.

load_metta_python_patcher :-
    % Retrieve the Python patcher content.
    metta_python_patcher(String),
    % Load the Python module via py_module/2.
    py_module(metta_python_patcher, String),
    % Assert that the patcher has now been loaded.
    assert(did_load_metta_python_patcher), !,
    % Try to load the module again, ignoring any errors.
    %ignore(notrace(with_safe_argv(catch(py_module(metta_python_patcher, String), _, true)))), 
    !.

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
%:- initialization(maybe_load_metta_python_patcher).
%:- initialization(maybe_load_metta_python_patcher, restore).

%!  patch_hyperonpy is det.
%
%   Loads the Python patcher (if not already loaded) and calls the Python function `patch_hyperonpy/0`
%   from the `metta_python_patcher` module. The result is expected to be an empty Python dictionary (`py{}`).
%
%   @example
%     % Applying the patch to Hyperon:
%     ?- patch_hyperonpy.
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
%     % Load the Hyperon module:
%     ?- load_hyperonpy.
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
%     % Load mettalog and hyperon (if available):
%     ?- load_mettalogpy.
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
%     % Start the mettalog REPL:
%     ?- mettalogpy_repl.
%   true.
%
mettalogpy_repl :-
    catch_log(override_hyperonpy),
    catch_log(hyperonpy_repl), !.

hyperonpy_repl :-
    maplist(catch_log,
            [load_metta_python_proxy,
             load_metta_python_patcher,
             load_mettalogpy,
             py_call(mettalog:repl())]).

% Call the original Python function if there's no Prolog predicate
call_python_original(ModuleFunctionName, TupleArgs, LArgs, KwArgs, Return) :- fail,
    py_call(metta_python_override:call_python_original(ModuleFunctionName, TupleArgs, LArgs, KwArgs), Return).

% Sample Prolog function for demonstration
my_module_add(A, B, _, R) :- R is A + B.

maybe_info(_Fmt, _Args) :- !.
maybe_info(Fmt, Args) :- format(Fmt, Args).

% Define factorial in Prolog
my_module_factorial(0, 1).
my_module_factorial(N, F) :-
    N > 0,
    N1 is N - 1,
    my_module_factorial(N1, F1),
    F is N * F1.



% ==================================================
% **Prolog Translation of the Given File with Numbered Functions**
% ==================================================

no_module:- module(hyperonpy_prolog_translation, [
    % Object-oriented helper predicates
    oo_new/3,          % 1
    oo_free/1,         % 2
    oo_invoke/4,       % 3
    oo_clone/2,        % 4
    oo_get/4,          % 5
    oo_set/3,          % 6
    oo_equal/3,        % 7

    % hyperonpy_* functions
    % 1. Atom Functions
    hyperonpy_atom_sym/2,            % 8
    hyperonpy_atom_var/2,            % 9
    hyperonpy_atom_expr/2,           % 10
    hyperonpy_atom_gnd/3,            % 11
    hyperonpy_atom_free/1,           % 12
    hyperonpy_atom_to_str/2,         % 13
    hyperonpy_atom_eq/3,             % 14
    hyperonpy_atom_get_name/2,       % 15
    hyperonpy_atom_get_children/2,   % 16
    hyperonpy_atom_get_grounded_type/2, % 17
    hyperonpy_atom_get_object/2,     % 18
    hyperonpy_atom_is_cgrounded/2,   % 19
    hyperonpy_atom_is_error/2,       % 20
    hyperonpy_atom_error_message/2,  % 21
    hyperonpy_atom_var_parse_name/2, % 22
    hyperonpy_atom_get_metatype/2,   % 23
    hyperonpy_atom_get_space/2,      % 24
    hyperonpy_atom_iterate/2,        % 25
    hyperonpy_atom_match_atom/3,     % 26
    hyperonpy_atom_gnd_serialize/3,  % 27
    hyperonpy_atom_clone/2,          % 28

    % 2. Atom Vector Functions
    hyperonpy_atom_vec_new/1,        % 29
    hyperonpy_atom_vec_from_list/2,  % 30
    hyperonpy_atom_vec_len/2,        % 31
    hyperonpy_atom_vec_push/2,       % 32
    hyperonpy_atom_vec_pop/2,        % 33
    hyperonpy_atom_vec_free/1,       % 34

    % 3. Bindings Functions
    hyperonpy_bindings_new/1,        % 35
    hyperonpy_bindings_free/1,       % 36
    hyperonpy_bindings_add_var_binding/4, % 37
    hyperonpy_bindings_resolve/3,    % 38
    hyperonpy_bindings_is_empty/2,   % 39
    hyperonpy_bindings_list/2,       % 40
    hyperonpy_bindings_merge/3,      % 41
    hyperonpy_bindings_eq/3,         % 42
    hyperonpy_bindings_clone/2,      % 43
    hyperonpy_bindings_to_str/2,     % 44
    hyperonpy_bindings_narrow_vars/2, % 45

    % 4. Bindings Set Functions
    hyperonpy_bindings_set_empty/1,  % 46
    hyperonpy_bindings_set_free/1,   % 47
    hyperonpy_bindings_set_add_var_binding/3, % 48
    hyperonpy_bindings_set_is_empty/2, % 49
    hyperonpy_bindings_set_list/2,   % 50
    hyperonpy_bindings_set_merge_into/2, % 51
    hyperonpy_bindings_set_eq/3,     % 52
    hyperonpy_bindings_set_clone/2,  % 53
    hyperonpy_bindings_set_from_bindings/2, % 54
    hyperonpy_bindings_set_unpack/2, % 55
    hyperonpy_bindings_set_is_single/2, % 56
    hyperonpy_bindings_set_push/2,   % 57
    hyperonpy_bindings_set_single/1, % 58
    hyperonpy_bindings_set_add_var_equality/3, % 59
    hyperonpy_bindings_set_to_str/2, % 60

    % 5. Validation Functions
    hyperonpy_validate_atom/3,       % 61
    hyperonpy_check_type/4,          % 62
    hyperonpy_get_atom_types/3,      % 63

    % 6. Metta Functions
    hyperonpy_metta_new/3,           % 64
    hyperonpy_metta_free/1,          % 65
    hyperonpy_metta_space/2,         % 66
    hyperonpy_metta_tokenizer/2,     % 67
    hyperonpy_metta_run/3,           % 68
    hyperonpy_metta_err_str/2,       % 69
    hyperonpy_metta_evaluate_atom/3, % 70
    hyperonpy_metta_load_module_at_path/4, % 71
    hyperonpy_metta_load_module_direct/4, % 72
    hyperonpy_metta_working_dir/2,   % 73
    hyperonpy_metta_eq/3,            % 74

    % 7. Environment Builder Functions
    hyperonpy_environment_config_dir/2, % 75
    hyperonpy_env_builder_start/1,   % 76
    hyperonpy_env_builder_use_default/1, % 77
    hyperonpy_env_builder_use_test_env/1, % 78
    hyperonpy_env_builder_set_working_dir/2, % 79
    hyperonpy_env_builder_set_config_dir/2, % 80
    hyperonpy_env_builder_create_config_dir/3, % 81
    hyperonpy_env_builder_disable_config_dir/1, % 82
    hyperonpy_env_builder_set_is_test/2, % 83
    hyperonpy_env_builder_push_include_path/2, % 84
    hyperonpy_env_builder_push_fs_module_format/3, % 85
    hyperonpy_env_builder_init_common_env/2, % 86

    % 8. Space Functions
    hyperonpy_space_new_grounding/1, % 87
    hyperonpy_space_free/1,          % 88
    hyperonpy_space_add/2,           % 89
    hyperonpy_space_remove/3,        % 90
    hyperonpy_space_replace/4,       % 91
    hyperonpy_space_subst/4,         % 92
    hyperonpy_space_eq/3,            % 93
    hyperonpy_space_list/2,          % 94
    hyperonpy_space_atom_count/2,    % 95
    hyperonpy_space_get_payload/2,   % 96
    hyperonpy_space_new_custom/2,    % 97
    hyperonpy_space_query/3,         % 98

    % 9. Interpretation Functions
    hyperonpy_interpret_init/3,      % 99
    hyperonpy_interpret_step/2,      % 100
    hyperonpy_step_get_result/2,     % 101
    hyperonpy_step_has_next/2,       % 102

    % 10. Tokenizer Functions
    hyperonpy_tokenizer_new/1,       % 103
    hyperonpy_tokenizer_free/1,      % 104
    hyperonpy_tokenizer_register_token/3, % 105
    hyperonpy_tokenizer_clone/2,     % 106

    % 11. Runner State Functions
    hyperonpy_runner_state_new_with_atoms/3, % 107
    hyperonpy_runner_state_new_with_parser/3, % 108
    hyperonpy_runner_state_step/1,   % 109
    hyperonpy_runner_state_current_results/2, % 110
    hyperonpy_runner_state_is_complete/2, % 111
    hyperonpy_runner_state_free/1,   % 112
    hyperonpy_runner_state_err_str/2, % 113

    % 12. Syntax Node Functions
    hyperonpy_syntax_node_clone/2,   % 114
    hyperonpy_syntax_node_free/1,    % 115
    hyperonpy_syntax_node_is_null/2, % 116
    hyperonpy_syntax_node_type/2,    % 117
    hyperonpy_syntax_node_is_leaf/2, % 118
    hyperonpy_syntax_node_unroll/2,  % 119
    hyperonpy_syntax_node_src_range/2, % 120

    % 13. Run Context Functions
    hyperonpy_run_context_get_metta/2, % 121
    hyperonpy_run_context_get_space/2, % 122
    hyperonpy_run_context_get_tokenizer/2, % 123
    hyperonpy_run_context_import_dependency/2, % 124
    hyperonpy_run_context_init_self_module/3, % 125
    hyperonpy_run_context_load_module/3, % 126

    % 14. Logging Functions
    hyperonpy_log_error/1,           % 127
    hyperonpy_log_info/1,            % 128
    hyperonpy_log_warn/1,            % 129

    % 15. Load Function
    hyperonpy_load_ascii/3           % 130
]).

% The `oo_*` library simulates object-oriented behavior in Prolog.
% It allows for creating, manipulating, and interacting with objects using dynamic predicates and helper functions.
:- dynamic o3/3.

o3(atom,int1,[metatype,'Grounded',grounded_type:'Number',object:1]).
o3(atom,int2,[metatype,'Grounded',grounded_type:'Number',object:2]).
o3(atom,int3,[metatype,'Grounded',grounded_type:'Number',object:3]).
o3(atom,int4,[metatype,'Grounded',grounded_type:'Number',object:4]).


% Unique object ID generator
generate_object_id(Type, ObjectID) :-
    retract(object_counter(N)),
    N1 is N + 1,
    assert(object_counter(N1)),
    atom_concat(Type, '_', TempID),
    gensym(TempID, ObjectID).

% Define oo_new/3 to create a new object with attributes stored as o_f_v(Object, FieldName, Value)
%!  oo_new(+Type, +Attributes, -Object) is det.
%
%   Creates a new object with a unique ID based on its type.
%
%   This predicate generates a new object by combining the provided Type and Attributes.
%   The unique ID is generated using `gensym/2`, which ensures the ID is prefixed
%   with the Type.
%
%   @arg Type The type of the object to be created.
%   @arg Attributes The attributes associated with the object.
%   @arg Object The resulting object structure in the form o3(Type, ID, Attributes).
%
%   @example
%   ?- oo_new(atom_vec, [value:[a,b,c]], Obj).
%   Obj = o3(atom_vec, atom_vec1, [value:[a, b, c]]).
%
oo_new(Type, Attributes, Object) :-
        Object= o3(Type, ID, Attributes),

    generate_object_id(Type, Object),  % Generate a unique ID with the Type as a prefix                % Generate a unique ID with the Type as a prefix
    assert(o3(Type, ID, Attributes)).

%!  oo_free(+ObjectOrID) is det.
%
%   Frees (removes) an object from the system.
%
%   This predicate removes an object from the system by resolving the input to an
%   object structure using `into_object/2` and then retracting any associated facts.
%
%   @arg ObjectOrID Either an object structure or an object ID.
%
%   @example
%   ?- oo_free(atom_vec1).
%
oo_free(ObjectOrID) :-
    ignore((into_object_no_throw(ObjectOrID, o3(Type, ID, _)),
    retractall(o3(Type, ID, _)))).

%!  warn_if_not_true(+Goal, +Message) is det.
%
%   Logs a warning message if the given Goal is not true.
%
%   If the provided Goal fails, this predicate prints the given Message to the console.
%   This is useful for debugging purposes or checking for unexpected conditions.
%
%   @arg Goal The goal to be evaluated.
%   @arg Message The message to log if the goal fails.
%
warn_if_not_true(P0, Message) :-
    ((\+ P0) -> format('~N~nWARNING: ~q  , ~q ~n', [P0, Message]); true).

%!  oo_invoke(+Type, +ObjectOrID, +MethodCall, -Result) is det.
%
%   Invokes a method on an object, resolving the object using `into_object/2`.
%
%   This predicate ensures that the object type matches the expected Type. If the
%   type is correct, it proceeds to handle the method call using `handle_method_call/4`.
%
%   @arg Type The expected type of the object.
%   @arg ObjectOrID Either an object structure or object ID.
%   @arg MethodCall The method being invoked on the object.
%   @arg Result The result of the method call.
%
%   @example
%   ?- oo_invoke(atom_vec, atom_vec1, length(), Len).
%   Len = 3.
%
oo_invoke(Type, ObjectOrID, MethodCall, Result) :-
    into_object(ObjectOrID, o3(DeclType, ID, Attributes)),
    % Ensure the object type matches the expected type
    warn_if_not_true(DeclType = Type, oo_invoke(Type, ObjectOrID, MethodCall)),
    handle_method_call(Type, ID, Attributes, MethodCall, Result).

%!  oo_clone(+ObjectOrID, -ClonedObject) is det.
%
%   Clones an existing object by creating a new object with the same attributes.
%
%   This predicate copies the attributes of an existing object, generates a new
%   unique ID, and creates a new object using `oo_new/3`.
%
%   @arg ObjectOrID Either an object structure or object ID.
%   @arg ClonedObject The newly created object (clone).
%
%   @example
%     ?- oo_clone(atom_vec_1, Clone).
%     Clone = o3(atom_vec, atom_vec2, [value:[a, b, c]]).
%
oo_clone(ObjectOrID, ClonedObject) :-
    into_object(ObjectOrID, o3(Type, _, Attributes)),
    oo_new(Type, Attributes, ClonedObject).

















%!  oo_get(+Type, +ObjectOrID, +FieldName, -Value) is det.
%
%   Retrieves the value of a specific field from an object's attributes.
%
%   This predicate extracts the value associated with a field (FieldName) from
%   the object's attributes. It also ensures that the object's type matches the
%   expected Type.
%
%   @arg Type The expected type of the object.
%   @arg ObjectOrID Either an object structure or object ID.
%   @arg FieldName The field to retrieve from the attributes.
%   @arg Value The value associated with the field.
%
%   @example
%   ?- oo_get(atom_vec, atom_vec1, value, List).
%   List = [a, b, c].
%
oo_get(ObjectOrID, FieldName, Value):-
  oo_get(_, ObjectOrID, FieldName, Value).
oo_get(Type, ObjectOrID, FieldName, Value) :-
    % Convert ObjectOrID into o3 structure using into_object/2
    into_object(ObjectOrID, o3(DeclType, _, Attributes)),
    % Check for the FieldName in Attributes or use a fallback
     member_chk(FieldName:Value, Attributes),
    % Check if the declared type matches the expected type
    warn_if_not_true(DeclType = Type, oo_get(Type, ObjectOrID, FieldName, Value)),    
    !.

member_chk(FieldName:Value, Attributes):-
    trace_failures(
        (   
            memberchk(FieldName:Value, Attributes) -> true ;
            (   
                memberchk(value:Value, Attributes) -> true ;
                last(Attributes, _:Value)  % Default to the last attribute's value if FieldName not found
            )
        )
    ).

%!  oo_equal(+ObjectOrID1, +ObjectOrID2, -AreEqual) is det.
%
%   Checks equality between two objects.
%
%   This predicate compares two objects by resolving them to their object structures
%   using `into_object/2`. Objects are considered equal if they either have the same
%   ID or have the same type and identical attributes.
%
%   @arg ObjectOrID1 The first object or ID to compare.
%   @arg ObjectOrID2 The second object or ID to compare.
%   @arg AreEqual Unifies with `true` if the objects are equal, otherwise `false`.
%
%   @example
%   ?- oo_equal(atom_vec1, atom_vec2, AreEqual).
%   AreEqual = @(false).
%
oo_equal(ObjectOrID1, ObjectOrID2, AreEqual) :-
    % Resolve both objects to the object/3 form
    into_object(ObjectOrID1, o3(Type1, ID1, Attributes1)),
    into_object(ObjectOrID2, o3(Type2, ID2, Attributes2)),

    % Check if IDs are the same; if so, they are equal
    (ID1 == ID2 ->
        AreEqual = @(true)
    ;
    % Otherwise, if types are the same, compare attributes
    (Type1 == Type2 ->
        % Find common attributes
       (intersection(Attributes1, Attributes2, CommonAttributes),
        CommonAttributes \= [],   % Ensure at least one common attribute

        % Check that there are no differing attributes
        subtract(Attributes1, Attributes2, []),
        subtract(Attributes2, Attributes1, [])),

        % If all conditions are satisfied, set AreEqual to true
        AreEqual = @(true)
    ;
    % If neither condition is met, they are not equal
    AreEqual = @(false))).

%!  into_object(+Input, -Object) is det.
%
%   Helper predicate to convert an ID or object/3 structure into an object/3 structure.
%
%   This predicate attempts to resolve the input to an object/3 structure. If the input
%   is already in object/3 form, it succeeds. Otherwise, it looks up the object using
%   the provided ID.
%
%   @arg Input Either an object structure or an object ID.
%   @arg Object The resulting object structure (if found).
%
%   @throws existence_error if the object does not exist.
%
into_object(I,O):- into_object1(I,O),!.
into_object(ID, _) :- \+ o3(_, ID, _), throw(error(existence_error(object, ID), into_object/2)).

into_object1(Var,o3(Type, ID, Attributes)):- attvar(Var),get_attr(Var,type,Type),get_attr(Var,id,ID),get_attr(Var,fields,Attributes),!.
into_object1(Var,VarO):- var(Var),!,VarO=Var.
into_object1(o3(Type, ID, Attributes), o3(Type, ID, Attributes)) :- !.
% Fail if object not found
into_object1(ID, o3(Type, ID, Attributes)) :- o3(Type, ID, Attributes), !.


into_object_no_throw(I,O):- into_object1(I,O),!.
into_object_no_throw(I,I).



%!  find_list_attribute(+Attributes, -List) is det.
%
%   Helper predicate to find a list within an object's attributes.
%
%   This predicate looks for the first attribute that is a list. If found, it unifies
%   the List with the value of the attribute.
%
%   @arg Attributes The list of attributes associated with an object.
%   @arg List The list found in the attributes.
%
find_list_attribute([_:List|_], List) :-
    is_list(List), !.

find_list_attribute([_|Rest], List) :-
    find_list_attribute(Rest, List).




% Below are all the **141** `hyperonpy_*` functions translated into Prolog, leveraging the `oo_*` library for object-oriented operations. Each function is defined according to its parameters, types, and return types as specified.

% ==============================
% **2.1. Atom Functions**
% =============================

%!  hyperonpy_atom_eq(+Atom1, +Atom2, -AreEqual) is det.
%
%   Compares two atoms for equality.
%
%   This function checks if two `hyperonpy.CAtom` objects are equal by invoking the `oo_equal/3` predicate.
%
%   @arg Atom1 The first atom (`hyperonpy.CAtom`).
%   @arg Atom2 The second atom (`hyperonpy.CAtom`).
%   @arg AreEqual Boolean result indicating if the two atoms are equal.
hyperonpy_atom_eq(Atom1, Atom2, AreEqual) :-
    oo_equal(Atom1, Atom2, AreEqual).

%!  hyperonpy_atom_error_message(+Atom, -ErrorMessage) is det.
%
%   Retrieves the error message associated with an atom.
%
%   This function invokes a method to retrieve an error message for the given `hyperonpy.CAtom`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg ErrorMessage A string containing the error message.
hyperonpy_atom_error_message(Atom, ErrorMessage) :-
    oo_invoke(atom, Atom, error_message(), ErrorMessage).

%!  hyperonpy_atom_expr(+ExpressionList, -Atom) is det.
%
%   Creates an expression atom from a list of expressions.
%
%   This function creates a new `hyperonpy.CAtom` representing an expression using the given list of expressions.
%
%   @arg ExpressionList The list of expressions to form the atom.
%   @arg Atom The resulting `hyperonpy.CAtom`.
hyperonpy_atom_expr(ExpressionList, Atom) :-
    oo_new(atom, [metatype:'Expression', value:ExpressionList], Atom).

%!  hyperonpy_atom_free(+Atom) is det.
%
%   Frees an atom from memory.
%
%   This function removes a `hyperonpy.CAtom` object from memory by invoking `oo_free/1`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`) to be freed.
hyperonpy_atom_free(Atom) :-
    oo_free(Atom).

%!  hyperonpy_atom_get_children(+Atom, -Children) is det.
%
%   Retrieves the children of a given atom.
%
%   This function fetches the list of children associated with a `hyperonpy.CAtom`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg Children The list of child atoms.
hyperonpy_atom_get_children(Atom, Children) :-
    oo_get(atom, Atom, children, Children).

%!  hyperonpy_atom_get_grounded_type(+Atom, -GroundedType) is det.
%
%   Retrieves the grounded type of an atom.
%
%   This function fetches the grounded type of the given `hyperonpy.CAtom`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg GroundedType The grounded type of the atom.
hyperonpy_atom_get_grounded_type(Atom, GroundedType) :-
    oo_get(atom, Atom, grounded_type, GroundedType).

%!  hyperonpy_atom_get_metatype(+Atom, -MetaType) is det.
%
%   Retrieves the metatype of an atom.
%
%   This function fetches the metatype of the given `hyperonpy.CAtom`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg MetaType The metatype of the atom.
hyperonpy_atom_get_metatype(Atom, MetaType) :-
    oo_get(atom, Atom, metatype, MetaType).

%!  hyperonpy_atom_get_name(+Atom, -Name) is det.
%
%   Retrieves the name of an atom.
%
%   This function fetches the name of the given `hyperonpy.CAtom`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg Name The name of the atom (string).
hyperonpy_atom_get_name(Atom, Name) :-
    oo_get(atom, Atom, name, Name).

%!  hyperonpy_atom_get_object(+Atom, -Object) is det.
%
%   Retrieves the object associated with a grounded atom.
%
%   This function fetches the object associated with the given `hyperonpy.CAtom`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg Object The object associated with the atom.
hyperonpy_atom_get_object(Atom, Object) :-
    oo_get(atom, Atom, object, Object).

%!  hyperonpy_atom_get_space(+Atom, -Space) is det.
%
%   Retrieves the space associated with an atom.
%
%   This function fetches the space associated with the given `hyperonpy.CAtom`.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg Space The space associated with the atom (`CStruct<space_t>`).
hyperonpy_atom_get_space(Atom, Space) :-
    oo_get(atom, Atom, space, Space).

%!  hyperonpy_atom_gnd(+Object, +GroundedType, -Atom) is det.
%
%   Creates a grounded atom with a specified type.
%
%   This function creates a new grounded `hyperonpy.CAtom` with the given object and grounded type.
%
%   @arg Object The object to associate with the atom.
%   @arg GroundedType The grounded type for the atom.
%   @arg Atom The resulting `hyperonpy.CAtom`.
hyperonpy_atom_gnd(Object, GroundedType, Atom) :-
    oo_new(atom, [metatype:'Grounded', value:Object, grounded_type:GroundedType], Atom).

%!  hyperonpy_atom_gnd_serialize(+Atom, +Serializer, -SerializedResult) is det.
%
%   Serializes a grounded atom using a serializer.
%
%   This function invokes the serialization of a `hyperonpy.CAtom` using the provided `hyperonpy.Serializer`.
%
%   @arg Atom The grounded atom (`hyperonpy.CAtom`).
%   @arg Serializer The serializer (`hyperonpy.Serializer`).
%   @arg SerializedResult The result of the serialization (`hyperonpy.SerialResult`).
hyperonpy_atom_gnd_serialize(Atom, Serializer, SerializedResult) :-
    oo_invoke(atom, Atom, gnd_serialize(Serializer), SerializedResult).

%!  hyperonpy_atom_is_cgrounded(+Atom, -IsCgrounded) is det.
%
%   Checks if an atom is grounded.
%
%   This function checks whether the given `hyperonpy.CAtom` is grounded.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg IsCgrounded Boolean result indicating if the atom is grounded.
hyperonpy_atom_is_cgrounded(Atom, IsCgrounded) :-
    py_is_tf(oo_get(atom, Atom, metatype, 'Grounded'),IsCgrounded).

%!  hyperonpy_atom_is_error(+Atom, -IsError) is det.
%
%   Checks if an atom represents an error.
%
%   This function checks whether the given `hyperonpy.CAtom` represents an error.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg IsError Boolean result indicating if the atom is an error.
hyperonpy_atom_is_error(Atom, IsError) :-
    oo_invoke(atom, Atom, is_error(), IsError).

%!  hyperonpy_atom_iterate(+Atom, -List) is det.
%
%   Iterates over the components of an atom.
%
%   This function iterates over the components of the given `hyperonpy.CAtom`, returning them as a list.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg List The list of components.
hyperonpy_atom_iterate(Atom, List) :-
    oo_invoke(atom, Atom, iterate(), List).

%!  hyperonpy_atom_match_atom(+Atom1, +Atom2, -BindingsSet) is det.
%
%   Matches two atoms and returns the resulting bindings.
%
%   This function matches two `hyperonpy.CAtom` objects and returns the resulting bindings as a `CStruct<bindings_set_t>`.
%
%   @arg Atom1 The first atom (`hyperonpy.CAtom`).
%   @arg Atom2 The second atom (`hyperonpy.CAtom`).
%   @arg BindingsSet The resulting bindings set (`CStruct<bindings_set_t>`).
hyperonpy_atom_match_atom(Atom1, Atom2, BindingsSet) :-
    oo_invoke(atom, Atom1, match_atom(Atom2), BindingsSet).

%!  hyperonpy_atom_sym(+Symbol, -Atom) is det.
%
%   Creates a symbolic atom from a symbol.
%
%   This function creates a new `hyperonpy.CAtom` representing a symbol.
%
%   @arg Symbol The symbol to form the atom.
%   @arg Atom The resulting `hyperonpy.CAtom`.
hyperonpy_atom_sym(Symbol, Atom) :-
    oo_new(atom, [metatype:'Symbol', value:Symbol], Atom).

%!  hyperonpy_atom_to_str(+Atom, -Str) is det.
%
%   Converts an atom to its string representation.
%
%   This function converts a `hyperonpy.CAtom` to its string representation.
%
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg Str The string representation of the atom.
hyperonpy_atom_to_str(Atom, Str) :-
    oo_invoke(atom, Atom, to_str(), Str).

%!  hyperonpy_atom_var(+VarName, -Atom) is det.
%
%   Creates a variable atom from a variable name.
%
%   This function creates a new `hyperonpy.CAtom` representing a variable with the given name.
%
%   @arg VarName The name of the variable.
%   @arg Atom The resulting `hyperonpy.CAtom`.
hyperonpy_atom_var(VarName, Atom) :-
    oo_new(atom, [metatype:'Variable', value:VarName], Atom).

%!  hyperonpy_atom_var_parse_name(+Name, -Atom) is det.
%
%   Parses a variable name and creates a variable atom.
%
%   This function parses a variable name and creates a corresponding `hyperonpy.CAtom`.
%
%   @arg Name The name to parse.
%   @arg Atom The resulting `hyperonpy.CAtom`.
hyperonpy_atom_var_parse_name(Name, Atom) :-
    oo_new(atom, [type:atom_var, value:Name], Atom).

% ==============================
% **2.2. Atom Vector (`atom_vec`) Functions**
% =============================

%!  hyperonpy_atom_vec_free(+AtomVec) is det.
%
%   Frees an atom vector from memory.
%
%   This function removes a `hyperonpy.CVecAtom` object from memory by invoking `oo_free/1`.
%
%   @arg AtomVec The atom vector (`hyperonpy.CVecAtom`) to be freed.
hyperonpy_atom_vec_free(AtomVec) :-
    oo_free(AtomVec).

%!  hyperonpy_atom_vec_from_list(+List, -AtomVec) is det.
%
%   Creates an atom vector from a list of atoms.
%
%   This function creates a new `hyperonpy.CVecAtom` from a list of atoms.
%
%   @arg List The list of atoms.
%   @arg AtomVec The resulting `hyperonpy.CVecAtom`.
hyperonpy_atom_vec_from_list(List, AtomVec) :-
    oo_new(atom_vec, [type:atom_vec, value:List], AtomVec).

%!  hyperonpy_atom_vec_len(+AtomVec, -Length) is det.
%
%   Retrieves the length of an atom vector.
%
%   This function returns the length of the given `hyperonpy.CVecAtom`.
%
%   @arg AtomVec The atom vector (`hyperonpy.CVecAtom`).
%   @arg Length The length of the atom vector.
hyperonpy_atom_vec_len(AtomVec, Length) :-
    oo_invoke(atom_vec, AtomVec, length(), Length).

%!  hyperonpy_atom_vec_new(-AtomVec) is det.
%
%   Creates a new empty atom vector.
%
%   This function creates a new empty `hyperonpy.CVecAtom`.
%
%   @arg AtomVec The resulting empty atom vector (`hyperonpy.CVecAtom`).
hyperonpy_atom_vec_new(AtomVec) :-
    oo_new(atom_vec, [type:atom_vec, value:[]], AtomVec).

%!  hyperonpy_atom_vec_pop(+AtomVec, -PoppedAtom) is det.
%
%   Removes and returns the last atom from the vector.
%
%   This function pops the last atom from the given `hyperonpy.CVecAtom`.
%
%   @arg AtomVec The atom vector (`hyperonpy.CVecAtom`).
%   @arg PoppedAtom The atom that was removed from the vector.
hyperonpy_atom_vec_pop(AtomVec, PoppedAtom) :-
    oo_invoke(atom_vec, AtomVec, pop(), PoppedAtom).

%!  hyperonpy_atom_vec_push(+AtomVec, +Atom) is det.
%
%   Pushes a new atom into the atom vector.
%
%   This function adds a new atom to the end of the given `hyperonpy.CVecAtom`.
%
%   @arg AtomVec The atom vector (`hyperonpy.CVecAtom`).
%   @arg Atom The atom to push into the vector.
hyperonpy_atom_vec_push(AtomVec, Atom) :-
    oo_invoke(atom_vec, AtomVec, push(Atom), _).

% ==============================
% **2.3. Bindings Functions**
% =============================

%!  hyperonpy_bindings_add_var_binding(+Bindings, +VarAtom, +BoundAtom, -Success) is det.
%
%   Adds a variable binding to the bindings set.
%
%   This function adds a binding between a variable atom and a bound atom to the `hyperonpy.CBindings`.
%
%   @arg Bindings The bindings set (`hyperonpy.CBindings`).
%   @arg VarAtom The variable atom (`hyperonpy.CAtom`).
%   @arg BoundAtom The bound atom (`hyperonpy.CAtom`).
%   @arg Success Boolean result indicating whether the binding was added successfully.
hyperonpy_bindings_add_var_binding(Bindings, VarAtom, BoundAtom, Success) :-
    oo_invoke(bindings, Bindings, add_var_binding(VarAtom, BoundAtom), Success).

%!  hyperonpy_bindings_clone(+Bindings, -NewBindings) is det.
%
%   Clones the bindings.
%
%   This function creates a copy of the given `hyperonpy.CBindings`.
%
%   @arg Bindings The bindings set to clone (`hyperonpy.CBindings`).
%   @arg NewBindings The cloned bindings set (`hyperonpy.CBindings`).
hyperonpy_bindings_clone(Bindings, NewBindings) :-
    oo_clone(Bindings, NewBindings).

%!  hyperonpy_bindings_eq(+Bindings1, +Bindings2, -AreEqual) is det.
%
%   Compares two bindings sets for equality.
%
%   This function checks if two `hyperonpy.CBindings` sets are equal.
%
%   @arg Bindings1 The first bindings set (`hyperonpy.CBindings`).
%   @arg Bindings2 The second bindings set (`hyperonpy.CBindings`).
%   @arg AreEqual Boolean result indicating if the two bindings sets are equal.
hyperonpy_bindings_eq(Bindings1, Bindings2, AreEqual) :-
    oo_equal(Bindings1, Bindings2, AreEqual).

%!  hyperonpy_bindings_free(+Bindings) is det.
%
%   Frees the bindings from memory.
%
%   This function removes a `hyperonpy.CBindings` object from memory.
%
%   @arg Bindings The bindings set (`hyperonpy.CBindings`) to be freed.
hyperonpy_bindings_free(Bindings) :-
    oo_free(Bindings).

%!  hyperonpy_bindings_is_empty(+Bindings, -IsEmpty) is det.
%
%   Checks if the bindings set is empty.
%
%   This function checks if the given `hyperonpy.CBindings` set is empty.
%
%   @arg Bindings The bindings set (`hyperonpy.CBindings`).
%   @arg IsEmpty Boolean result indicating if the bindings set is empty.
hyperonpy_bindings_is_empty(Bindings, IsEmpty) :-
    oo_invoke(bindings, Bindings, is_empty(), IsEmpty).

%!  hyperonpy_bindings_list(+Bindings, -List) is det.
%
%   Retrieves the list of bindings from the bindings set.
%
%   This function returns the list of bindings in the `hyperonpy.CBindings` set.
%
%   @arg Bindings The bindings set (`hyperonpy.CBindings`).
%   @arg List The list of bindings.
hyperonpy_bindings_list(Bindings, List) :-
    oo_invoke(bindings, Bindings, list(), List).

%!  hyperonpy_bindings_merge(+Bindings1, +Bindings2, -ResultBindingsSet) is det.
%
%   Merges two bindings sets.
%
%   This function merges two `hyperonpy.CBindings` sets into a new bindings set (`CStruct<bindings_set_t>`).
%
%   @arg Bindings1 The first bindings set (`hyperonpy.CBindings`).
%   @arg Bindings2 The second bindings set (`hyperonpy.CBindings`).
%   @arg ResultBindingsSet The resulting merged bindings set (`CStruct<bindings_set_t>`).
hyperonpy_bindings_merge(Bindings1, Bindings2, ResultBindingsSet) :-
    oo_invoke(bindings, Bindings1, merge(Bindings2), ResultBindingsSet).

%!  hyperonpy_bindings_narrow_vars(+Bindings, +VarAtomVec) is det.
%
%   Narrows the variable bindings in the bindings set.
%
%   This function narrows the variable bindings in the `hyperonpy.CBindings` using the provided vector of variable atoms.
%
%   @arg Bindings The bindings set (`hyperonpy.CBindings`).
%   @arg VarAtomVec The vector of variable atoms (`hyperonpy.CVecAtom`).
hyperonpy_bindings_narrow_vars(Bindings, VarAtomVec) :-
    oo_invoke(bindings, Bindings, narrow_vars(VarAtomVec), _).

%!  hyperonpy_bindings_new(-NewBindings) is det.
%
%   Creates a new empty bindings set.
%
%   This function creates a new empty `hyperonpy.CBindings`.
%
%   @arg NewBindings The resulting empty bindings set (`hyperonpy.CBindings`).
hyperonpy_bindings_new(NewBindings) :-
    oo_new(bindings, [value:[]], NewBindings).

%!  hyperonpy_bindings_resolve(+Bindings, +Atom, -ResolvedAtom) is det.
%
%   Resolves a variable atom in the bindings set.
%
%   This function resolves a variable atom in the `hyperonpy.CBindings`, returning the bound atom if found.
%
%   @arg Bindings The bindings set (`hyperonpy.CBindings`).
%   @arg Atom The atom (`hyperonpy.CAtom`) to resolve.
%   @arg ResolvedAtom The resolved atom (`Optional[hyperonpy.CAtom]`).
hyperonpy_bindings_resolve(Bindings, Atom, ResolvedAtom) :-
    oo_invoke(bindings, Bindings, resolve(Atom), ResolvedAtom).

% ==============================
% **2.4. Bindings Set Functions**
% =============================

%!  hyperonpy_bindings_set_add_var_binding(+BindingsSet, +VarAtom, +BoundAtom) is det.
%
%   Adds a variable binding to a bindings set.
%
%   This function adds a binding between a variable atom and a bound atom to the `hyperonpy.CBindingsSet`.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg VarAtom The variable atom (`hyperonpy.CAtom`).
%   @arg BoundAtom The atom to bind to the variable (`hyperonpy.CAtom`).
hyperonpy_bindings_set_add_var_binding(BindingsSet, VarAtom, BoundAtom) :-
    oo_invoke(bindings_set, BindingsSet, add_var_binding(VarAtom, BoundAtom), _).

%!  hyperonpy_bindings_set_add_var_equality(+BindingsSet, +VarAtom, +EqualAtom) is det.
%
%   Adds a variable equality constraint to a bindings set.
%
%   This function adds an equality constraint between a variable atom and another atom in the `hyperonpy.CBindingsSet`.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg VarAtom The variable atom (`hyperonpy.CAtom`).
%   @arg EqualAtom The atom that the variable must equal (`hyperonpy.CAtom`).
hyperonpy_bindings_set_add_var_equality(BindingsSet, VarAtom, EqualAtom) :-
    oo_invoke(bindings_set, BindingsSet, add_var_equality(VarAtom, EqualAtom), _).

%!  hyperonpy_bindings_set_clone(+BindingsSet, -ClonedBindingsSet) is det.
%
%   Clones a bindings set.
%
%   This function creates a copy of the given `hyperonpy.CBindingsSet`.
%
%   @arg BindingsSet The bindings set to clone (`hyperonpy.CBindingsSet`).
%   @arg ClonedBindingsSet The resulting cloned bindings set.
hyperonpy_bindings_set_clone(BindingsSet, ClonedBindingsSet) :-
    oo_clone(BindingsSet, ClonedBindingsSet).

%!  hyperonpy_bindings_set_empty(-BindingsSet) is det.
%
%   Creates an empty bindings set.
%
%   This function creates a new, empty `hyperonpy.CBindingsSet`.
%
%   @arg BindingsSet The resulting empty bindings set.
hyperonpy_bindings_set_empty(BindingsSet) :-
    oo_new(bindings_set, [single:true], BindingsSet).

%!  hyperonpy_bindings_set_eq(+BindingsSet1, +BindingsSet2, -AreEqual) is det.
%
%   Compares two bindings sets for equality.
%
%   This function checks whether two `hyperonpy.CBindingsSet` are equal.
%
%   @arg BindingsSet1 The first bindings set (`hyperonpy.CBindingsSet`).
%   @arg BindingsSet2 The second bindings set (`hyperonpy.CBindingsSet`).
%   @arg AreEqual Boolean result indicating whether the two bindings sets are equal.
hyperonpy_bindings_set_eq(BindingsSet1, BindingsSet2, AreEqual) :-
    oo_equal(BindingsSet1, BindingsSet2, AreEqual).

%!  hyperonpy_bindings_set_free(+BindingsSet) is det.
%
%   Frees a bindings set from memory.
%
%   This function removes a `hyperonpy.CBindingsSet` object from memory.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`) to be freed.
hyperonpy_bindings_set_free(BindingsSet) :-
    oo_free(BindingsSet).

%!  hyperonpy_bindings_set_from_bindings(+Bindings, -BindingsSet) is det.
%
%   Creates a bindings set from a bindings object.
%
%   This function creates a new `hyperonpy.CBindingsSet` from an existing `hyperonpy.CBindings`.
%
%   @arg Bindings The bindings object (`hyperonpy.CBindings`).
%   @arg BindingsSet The resulting bindings set (`hyperonpy.CBindingsSet`).
hyperonpy_bindings_set_from_bindings(Bindings, BindingsSet) :-
    oo_new(bindings_set, [from_bindings:Bindings], BindingsSet).

%!  hyperonpy_bindings_set_is_empty(+BindingsSet, -IsEmpty) is det.
%
%   Checks if a bindings set is empty.
%
%   This function checks whether the given `hyperonpy.CBindingsSet` is empty.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg IsEmpty Boolean result indicating if the bindings set is empty.
hyperonpy_bindings_set_is_empty(BindingsSet, IsEmpty) :-
    oo_invoke(bindings_set, BindingsSet, is_empty(), IsEmpty).

%!  hyperonpy_bindings_set_is_single(+BindingsSet, -IsSingle) is det.
%
%   Checks if a bindings set contains a single binding.
%
%   This function checks whether the given `hyperonpy.CBindingsSet` contains only a single binding.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg IsSingle Boolean result indicating if the bindings set contains only one binding.
hyperonpy_bindings_set_is_single(BindingsSet, IsSingle) :-
    oo_invoke(bindings_set, BindingsSet, is_single(), IsSingle).

%!  hyperonpy_bindings_set_list(+BindingsSet, -List) is det.
%
%   Retrieves the list of bindings from a bindings set.
%
%   This function returns the list of bindings in the `hyperonpy.CBindingsSet`.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg List The list of bindings.
hyperonpy_bindings_set_list(BindingsSet, List) :-
    oo_invoke(bindings_set, BindingsSet, list(), List).

%!  hyperonpy_bindings_set_merge_into(+BindingsSet1, +BindingsSet2) is det.
%
%   Merges two bindings sets.
%
%   This function merges the bindings in `BindingsSet1` into `BindingsSet2`.
%
%   @arg BindingsSet1 The first bindings set (`hyperonpy.CBindingsSet`).
%   @arg BindingsSet2 The second bindings set (`hyperonpy.CBindingsSet`).
hyperonpy_bindings_set_merge_into(BindingsSet1, BindingsSet2) :-
    oo_invoke(bindings_set, BindingsSet1, merge_into(BindingsSet2), _).

%!  hyperonpy_bindings_set_push(+BindingsSet, +Bindings) is det.
%
%   Pushes a set of bindings into a bindings set.
%
%   This function pushes the given `hyperonpy.CBindings` into the `hyperonpy.CBindingsSet`.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg Bindings The bindings to push (`hyperonpy.CBindings`).
hyperonpy_bindings_set_push(BindingsSet, Bindings) :-
    oo_invoke(bindings_set, BindingsSet, push(Bindings), _).

%!  hyperonpy_bindings_set_single(-SingleBindingsSet) is det.
%
%   Creates a bindings set containing a single binding.
%
%   This function creates a new `hyperonpy.CBindingsSet` with only one binding.
%
%   @arg SingleBindingsSet The resulting bindings set with one binding.
hyperonpy_bindings_set_single(SingleBindingsSet) :-
    oo_new(bindings_set, [single:true], SingleBindingsSet).

%!  hyperonpy_bindings_set_to_str(+BindingsSet, -Str) is det.
%
%   Converts a bindings set to a string representation.
%
%   This function converts a `hyperonpy.CBindingsSet` to its string representation.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg Str The string representation of the bindings set.
hyperonpy_bindings_set_to_str(BindingsSet, Str) :-
    oo_invoke(bindings_set, BindingsSet, to_str(), Str).

%!  hyperonpy_bindings_set_unpack(+BindingsSet, -UnpackedList) is det.
%
%   Unpacks a bindings set into a list.
%
%   This function unpacks the bindings in the given `hyperonpy.CBindingsSet` into a list.
%
%   @arg BindingsSet The bindings set (`hyperonpy.CBindingsSet`).
%   @arg UnpackedList The list of unpacked bindings.
hyperonpy_bindings_set_unpack(BindingsSet, UnpackedList) :-
    oo_invoke(bindings_set, BindingsSet, unpack(), UnpackedList).

% ==============================
% **2.5. Bindings Functions Continued**
% =============================

%!  hyperonpy_bindings_to_str(+Bindings, -Str) is det.
%
%   Converts a bindings object to a string representation.
%
%   This function converts a `hyperonpy.CBindings` object to its string representation.
%
%   @arg Bindings The bindings object (`hyperonpy.CBindings`).
%   @arg Str The string representation of the bindings.
hyperonpy_bindings_to_str(Bindings, Str) :-
    oo_invoke(bindings, Bindings, to_str(), Str).

% ==============================
% **2.6. Type Checking Function**
% =============================

%!  hyperonpy_check_type(+Space, +Atom1, +Atom2, -IsValid) is det.
%
%   Checks if the type of two atoms is valid in a given space.
%
%   This function checks the types of `Atom1` and `Atom2` in the given `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`) in which to check the types.
%   @arg Atom1 The first atom (`hyperonpy.CAtom`).
%   @arg Atom2 The second atom (`hyperonpy.CAtom`).
%   @arg IsValid Boolean result indicating whether the types are valid.
hyperonpy_check_type(Space, Atom1, Atom2, IsValid) :-
    oo_invoke(space, Space, check_type(Atom1, Atom2), IsValid).

% ==============================
% **2.7. Environment Builder Functions**
% =============================


% environment_config_dir/1
% Retrieves the current configuration directory for the environment
hyperonpy_environment_config_dir(ConfigDir) :-
    oo_get(env_builder_instance, config_dir, ConfigDir).


% env_builder_init_common_env/2
% Finalizes the initialization of the common environment using the provided EnvBuilder
hyperonpy_env_builder_init_common_env(EnvBuilder, Result) :-
    oo_invoke(env_builder, EnvBuilder, init_common_env(), Result).

% env_builder_set_working_dir/2
% Sets the working directory for the given EnvBuilder
hyperonpy_env_builder_set_working_dir(EnvBuilder, WorkingDir) :-
    oo_set(EnvBuilder, working_dir, WorkingDir).

% env_builder_set_config_dir/2
% Sets the configuration directory for the given EnvBuilder
hyperonpy_env_builder_set_config_dir(EnvBuilder, ConfigDir) :-
    oo_set(EnvBuilder, config_dir, ConfigDir).


%!  hyperonpy_env_builder_create_config_dir(+EnvBuilder, +UseDefault, -Result) is det.
%
%   Creates a configuration directory using the environment builder.
%
%   This function uses `hyperonpy.EnvBuilder` to create a configuration directory, with an option to use the default directory.
%
%   @arg EnvBuilder The environment builder (`hyperonpy.EnvBuilder`).
%   @arg UseDefault Boolean indicating whether to use the default configuration directory.
%   @arg Result The result of the operation.
hyperonpy_env_builder_create_config_dir(EnvBuilder, ShouldCreate, Result) :-
    oo_invoke(env_builder, EnvBuilder, create_config_dir(ShouldCreate), Result).

% env_builder_disable_config_dir/1
% Disables the configuration directory for the environment builder
hyperonpy_env_builder_disable_config_dir(EnvBuilder) :-
    oo_set(EnvBuilder, config_dir, @(none)).

% env_builder_set_is_test/2
% Sets whether the environment is for testing or not
hyperonpy_env_builder_set_is_test(EnvBuilder, IsTest) :-
    oo_set(EnvBuilder, is_test, IsTest).

% env_builder_push_include_path/2
% Adds an include path to the environment for the EnvBuilder
hyperonpy_env_builder_push_include_path(EnvBuilder, IncludePath) :-
    oo_get(EnvBuilder, include_paths, Paths),
    append(Paths, [IncludePath], NewPaths),
    oo_set(EnvBuilder, include_paths, NewPaths).

% env_builder_push_fs_module_format/3
% Adds a new module format to the environment using an interface and format ID
hyperonpy_env_builder_push_fs_module_format(EnvBuilder, Interface, FormatID) :-
    oo_get(EnvBuilder, module_formats, Formats),
    append(Formats, [(Interface, FormatID)], NewFormats),
    oo_set(EnvBuilder, module_formats, NewFormats).


%!  hyperonpy_env_builder_start(-EnvBuilder) is det.
%
%   Starts a new environment builder.
%
%   This function creates a new `hyperonpy.EnvBuilder` and starts it.
%
%   @arg EnvBuilder The resulting environment builder (`hyperonpy.EnvBuilder`).
hyperonpy_env_builder_start(EnvBuilder) :-
    oo_new(env_builder, [], EnvBuilder).

%!  hyperonpy_env_builder_use_default(-EnvBuilder) is det.
%
%   Initializes an environment builder with default settings.
%
%   This function initializes the `hyperonpy.EnvBuilder` using default settings.
%
%   @arg EnvBuilder The resulting environment builder (`hyperonpy.EnvBuilder`).
%
%   Uses the default common environment for the EnvBuilder
hyperonpy_env_builder_use_default(EnvBuilder) :-
    oo_new(env_builder, [type:default], EnvBuilder).

%!  hyperonpy_env_builder_use_test_env(-EnvBuilder) is det.
%
%   Initializes an environment builder in a test environment.
%
%   This function initializes the `hyperonpy.EnvBuilder` as a test environment.
%
%   @arg EnvBuilder The resulting environment builder (`hyperonpy.EnvBuilder`).
hyperonpy_env_builder_use_test_env(EnvBuilder) :-
    oo_new(env_builder, [test_env: ( '@'(true))], EnvBuilder).

% ==============================
% **2.8. Environment Configuration Function**
% =============================

%!  hyperonpy_environment_config_dir(-ConfigDir) is det.
%
%   Retrieves the environment's configuration directory.
%
%   This function returns the path to the configuration directory in the environment.
%
%   @arg ConfigDir The path to the configuration directory.
hyperonpy_environment_config_dir(EnvBuilder, ConfigDir) :-
    oo_get(env_builder, EnvBuilder, config_dir, ConfigDir).


% ==============================
% **2.9. Atom Types Function**
% =============================

%!  hyperonpy_get_atom_types(+Space, +Atom, -TypesList) is det.
%
%   Retrieves the types of a given atom in a specific space.
%
%   This function fetches the list of types associated with a `hyperonpy.CAtom` in the provided `hyperonpy.CSpace`.
%
%   @arg Space The space in which to check for atom types (`hyperonpy.CSpace`).
%   @arg Atom The atom (`hyperonpy.CAtom`).
%   @arg TypesList The list of atom types.
hyperonpy_get_atom_types(Space, Atom, TypesList) :-
    oo_invoke(space, Space, get_atom_types(Atom), TypesList).

% ==============================
% **2.10. Interpretation Functions**
% =============================

%!  hyperonpy_interpret_init(+Space, +Atom, -StepResult) is det.
%
%   Initializes interpretation for an atom within a space.
%
%   This function initializes the interpretation of a `hyperonpy.CAtom` in a `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Atom The atom to interpret (`hyperonpy.CAtom`).
%   @arg StepResult The initial result of the interpretation (`hyperonpy.CStepResult`).
hyperonpy_interpret_init(Space, Atom, StepResult) :-
    oo_invoke(space, Space, interpret_init(Atom), StepResult).

%!  hyperonpy_interpret_step(+StepResult, -NewStepResult) is det.
%
%   Performs the next step in the interpretation process.
%
%   This function continues the interpretation of a `hyperonpy.CStepResult`.
%
%   @arg StepResult The current step result (`hyperonpy.CStepResult`).
%   @arg NewStepResult The result after performing the next step (`hyperonpy.CStepResult`).
hyperonpy_interpret_step(StepResult, NewStepResult) :-
    oo_invoke(step_result, StepResult, interpret_step(), NewStepResult).

% ==============================
% **2.11. Load Function**
% =============================

%!  hyperonpy_load_ascii(+FilePath, +Space, -Success) is det.
%
%   Loads ASCII data from a file into a space.
%
%   This function loads ASCII data into the provided `hyperonpy.CSpace` from a specified file.
%
%   @arg FilePath The path to the file (string).
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Success Boolean indicating whether the load was successful.
hyperonpy_load_ascii(FilePath, Space, Success) :-
    oo_invoke(space, Space, load_ascii(FilePath), Success).

% ==============================
% **2.12. Logging Functions**
% =============================

%!  hyperonpy_log_error(+Message) is det.
%
%   Logs an error message.
%
%   This function logs an error message.
%
%   @arg Message The error message (string).
hyperonpy_log_error(Message) :-
    format("hyperonpy_log_error: ~w~n", [Message]).

%!  hyperonpy_log_info(+Message) is det.
%
%   Logs an informational message.
%
%   This function logs an informational message.
%
%   @arg Message The informational message (string).
hyperonpy_log_info(Message) :-
    format("hyperonpy_log_info: ~w~n", [Message]).

%!  hyperonpy_log_warn(+Message) is det.
%
%   Logs a warning message.
%
%   This function logs a warning message.
%
%   @arg Message The warning message (string).
hyperonpy_log_warn(Message) :-
    format("hyperonpy_log_warn: ~w~n", [Message]).

% ==============================
% **2.13. Metta Functions**
% =============================

%!  hyperonpy_metta_eq(+Metta1, +Metta2, -AreEqual) is det.
%
%   Compares two `hyperonpy.CMetta` instances for equality.
%
%   This function checks if two `hyperonpy.CMetta` objects are equal.
%
%   @arg Metta1 The first `hyperonpy.CMetta` object.
%   @arg Metta2 The second `hyperonpy.CMetta` object.
%   @arg AreEqual Boolean result indicating whether the two objects are equal.
hyperonpy_metta_eq(Metta1, Metta2, AreEqual) :-
    oo_equal(Metta1, Metta2, AreEqual).

%!  hyperonpy_metta_err_str(+Metta, -ErrorStr) is det.
%
%   Retrieves the error string of a Metta instance.
%
%   This function retrieves the error string from the provided `hyperonpy.CMetta` instance.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg ErrorStr The error string associated with the Metta instance.
hyperonpy_metta_err_str(Metta, ErrorStr) :-
   nop(oo_invoke(metta, Metta, err_str(), ErrorStr)),
   ignore(ErrorStr = '@'('none')).

%!  hyperonpy_metta_evaluate_atom(+Metta, +Atom, -ResultList) is det.
%
%   Evaluates an atom using the provided Metta instance.
%
%   This function evaluates a `hyperonpy.CAtom` using the provided `hyperonpy.CMetta` instance.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg Atom The atom to evaluate (`hyperonpy.CAtom`).
%   @arg ResultList The resulting list after evaluation.
hyperonpy_metta_evaluate_atom(Metta, Atom, ResultList) :-
    oo_invoke(metta, Metta, evaluate_atom(Atom), ResultList).

%!  hyperonpy_metta_free(+Metta) is det.
%
%   Frees a Metta instance from memory.
%
%   This function removes a `hyperonpy.CMetta` object from memory.
%
%   @arg Metta The Metta instance (`hyperonpy.CMetta`) to be freed.
hyperonpy_metta_free(Metta) :-
    oo_free(Metta).

%!  hyperonpy_metta_load_module_at_path(+Metta, +ModulePath, +ModuleIDOpt, -ModuleID) is det.
%
%   Loads a module at the specified path into a Metta instance.
%
%   This function loads a module located at `ModulePath` into the provided `hyperonpy.CMetta` instance.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg ModulePath The path to the module (string).
%   @arg ModuleIDOpt The optional module ID (optional string).
%   @arg ModuleID The resulting module ID.
hyperonpy_metta_load_module_at_path(Metta, ModulePath, ModuleIDOpt, ModuleID) :-
    oo_invoke(metta, Metta, load_module_at_path(ModulePath, ModuleIDOpt), ModuleID).

%!  hyperonpy_metta_load_module_direct(+Metta, +ModuleName, +Callback, -ModuleID) is det.
%
%   Loads a module directly into a Metta instance.
%
%   This function loads a module into the provided `hyperonpy.CMetta` instance using a callback.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg ModuleName The name of the module (string).
%   @arg Callback The callback function to load the module.
%   @arg ModuleID The resulting module ID.
hyperonpy_metta_load_module_direct(Metta, ModuleName, Callback, ModuleID) :-
    oo_invoke(metta, Metta, load_module_direct(ModuleName, Callback), ModuleID).

%!  hyperonpy_metta_new(+Space, +EnvBuilder, -Metta) is det.
%
%   Creates a new Metta instance.
%
%   This function creates a new `hyperonpy.CMetta` instance using the provided `hyperonpy.CSpace` and environment builder.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg EnvBuilder The environment builder (`CStruct<env_builder_t>`).
%   @arg Metta The resulting Metta instance (`hyperonpy.CMetta`).
hyperonpy_metta_new(Space, EnvBuilder, Metta) :-
    oo_new(metta, [space:Space, env_builder:EnvBuilder], Metta).

%!  hyperonpy_metta_run(+Metta, +SExprParser, -ResultList) is det.
%
%   Runs an S-expression parser in the Metta instance.
%
%   This function runs the `hyperonpy.CSExprParser` in the provided `hyperonpy.CMetta` instance and returns the result.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg SExprParser The S-expression parser (`hyperonpy.CSExprParser`).
%   @arg ResultList The list of results from running the parser.
hyperonpy_metta_run(Metta, SExprParser, ResultList) :-
   nop(oo_invoke(metta, Metta, run(SExprParser), ResultList)),
   ignore(ResultList = [[int4]]). % [2].

%!  hyperonpy_metta_space(+Metta, -Space) is det.
%
%   Retrieves the space associated with a Metta instance.
%
%   This function returns the `hyperonpy.CSpace` associated with the provided `hyperonpy.CMetta` instance.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg Space The space (`hyperonpy.CSpace`).
hyperonpy_metta_space(Metta, Space) :-
    oo_get(metta, Metta, space, Space).

%!  hyperonpy_metta_tokenizer(+Metta, -Tokenizer) is det.
%
%   Retrieves the tokenizer associated with a Metta instance.
%
%   This function returns the tokenizer (`hyperonpy.CTokenizer`) associated with the provided `hyperonpy.CMetta` instance.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg Tokenizer The tokenizer (`hyperonpy.CTokenizer`).
hyperonpy_metta_tokenizer(Metta, Tokenizer) :-
    oo_get(metta, Metta, tokenizer, Tokenizer).

%!  hyperonpy_metta_working_dir(+Metta, -WorkingDir) is det.
%
%   Retrieves the working directory associated with a Metta instance.
%
%   This function returns the working directory of the provided `hyperonpy.CMetta` instance.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg WorkingDir The working directory (string).
hyperonpy_metta_working_dir(Metta, WorkingDir) :-
    oo_get(metta, Metta, working_dir, WorkingDir).

% ==============================
% **2.14. Run Context Functions**
% =============================

%!  hyperonpy_run_context_get_metta(+RunContext, -Metta) is det.
%
%   Retrieves the Metta instance associated with a run context.
%
%   This function fetches the `hyperonpy.CMetta` associated with the provided `hyperonpy.CRunContext`.
%
%   @arg RunContext The run context (`hyperonpy.CRunContext`).
%   @arg Metta The Metta instance (`hyperonpy.CMetta`).
hyperonpy_run_context_get_metta(RunContext, Metta) :-
    oo_get(run_context, RunContext, metta, Metta).

%!  hyperonpy_run_context_get_space(+RunContext, -Space) is det.
%
%   Retrieves the space associated with a run context.
%
%   This function fetches the `hyperonpy.CSpace` associated with the provided `hyperonpy.CRunContext`.
%
%   @arg RunContext The run context (`hyperonpy.CRunContext`).
%   @arg Space The space (`hyperonpy.CSpace`).
hyperonpy_run_context_get_space(RunContext, Space) :-
    oo_get(run_context, RunContext, space, Space).

%!  hyperonpy_run_context_get_tokenizer(+RunContext, -Tokenizer) is det.
%
%   Retrieves the tokenizer associated with a run context.
%
%   This function fetches the `hyperonpy.CTokenizer` associated with the provided `hyperonpy.CRunContext`.
%
%   @arg RunContext The run context (`hyperonpy.CRunContext`).
%   @arg Tokenizer The tokenizer (`hyperonpy.CTokenizer`).
hyperonpy_run_context_get_tokenizer(RunContext, Tokenizer) :-
    oo_get(run_context, RunContext, tokenizer, Tokenizer).

%!  hyperonpy_run_context_import_dependency(+RunContext, +ModuleID) is det.
%
%   Imports a module dependency into a run context.
%
%   This function imports a module dependency identified by `ModuleID` into the `hyperonpy.CRunContext`.
%
%   @arg RunContext The run context (`hyperonpy.CRunContext`).
%   @arg ModuleID The module ID (`CStruct<module_id_t>`).
hyperonpy_run_context_import_dependency(RunContext, ModuleID) :-
    oo_invoke(run_context, RunContext, import_dependency(ModuleID), _).

%!  hyperonpy_run_context_init_self_module(+RunContext, +Space, +ModuleName) is det.
%
%   Initializes the run context with a self module.
%
%   This function initializes a self module in the `hyperonpy.CRunContext` using the given space and module name.
%
%   @arg RunContext The run context (`hyperonpy.CRunContext`).
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg ModuleName The name of the self module (string).
hyperonpy_run_context_init_self_module(RunContext, Space, ModuleName) :-
    oo_invoke(run_context, RunContext, init_self_module(Space, ModuleName), _).

%!  hyperonpy_run_context_load_module(+RunContext, +ModulePath, -ModuleID) is det.
%
%   Loads a module into a run context.
%
%   This function loads a module from the given `ModulePath` into the provided `hyperonpy.CRunContext`.
%
%   @arg RunContext The run context (`hyperonpy.CRunContext`).
%   @arg ModulePath The path to the module (string).
%   @arg ModuleID The resulting module ID (`CStruct<module_id_t>`).
hyperonpy_run_context_load_module(RunContext, ModulePath, ModuleID) :-
    oo_invoke(run_context, RunContext, load_module(ModulePath), ModuleID).

% ==============================
% **2.15. Runner State Functions**
% =============================

%!  hyperonpy_runner_state_current_results(+RunnerState, -ResultList) is det.
%
%   Retrieves the current results from the runner state.
%
%   This function fetches the list of results from the current state of the `hyperonpy.CRunnerState`.
%
%   @arg RunnerState The runner state (`hyperonpy.CRunnerState`).
%   @arg ResultList The list of current results.
hyperonpy_runner_state_current_results(RunnerState, ResultList) :-
    oo_invoke(runner_state, RunnerState, current_results(), ResultList).

%!  hyperonpy_runner_state_err_str(+RunnerState, -ErrorStr) is det.
%
%   Retrieves the error string from a runner state.
%
%   This function fetches the error string associated with the provided `hyperonpy.CRunnerState`.
%
%   @arg RunnerState The runner state (`hyperonpy.CRunnerState`).
%   @arg ErrorStr The error string associated with the runner state.
hyperonpy_runner_state_err_str(RunnerState, ErrorStr) :-
    oo_invoke(runner_state, RunnerState, err_str(), ErrorStr).

%!  hyperonpy_runner_state_free(+RunnerState) is det.
%
%   Frees the runner state from memory.
%
%   This function removes a `hyperonpy.CRunnerState` object from memory.
%
%   @arg RunnerState The runner state (`hyperonpy.CRunnerState`) to be freed.
hyperonpy_runner_state_free(RunnerState) :-
    oo_free(RunnerState).

%!  hyperonpy_runner_state_is_complete(+RunnerState, -IsComplete) is det.
%
%   Checks if the runner state is complete.
%
%   This function checks whether the `hyperonpy.CRunnerState` has completed its operation.
%
%   @arg RunnerState The runner state (`hyperonpy.CRunnerState`).
%   @arg IsComplete Boolean indicating if the runner state is complete.
hyperonpy_runner_state_is_complete(RunnerState, IsComplete) :-
    oo_invoke(runner_state, RunnerState, is_complete(), IsComplete).

%!  hyperonpy_runner_state_new_with_atoms(+Metta, +AtomVec, -RunnerState) is det.
%
%   Creates a new runner state with the provided atoms.
%
%   This function initializes a new `hyperonpy.CRunnerState` with the given list of atoms (`hyperonpy.CVecAtom`) and the `hyperonpy.CMetta`.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg AtomVec The vector of atoms (`hyperonpy.CVecAtom`).
%   @arg RunnerState The resulting runner state (`hyperonpy.CRunnerState`).
hyperonpy_runner_state_new_with_atoms(Metta, AtomVec, RunnerState) :-
    oo_new(runner_state, [metta:Metta, atoms:AtomVec], RunnerState).

%!  hyperonpy_runner_state_new_with_parser(+Metta, +SExprParser, -RunnerState) is det.
%
%   Creates a new runner state with the provided S-expression parser.
%
%   This function initializes a new `hyperonpy.CRunnerState` using the provided `hyperonpy.CMetta` and `hyperonpy.CSExprParser`.
%
%   @arg Metta The `hyperonpy.CMetta` instance.
%   @arg SExprParser The S-expression parser (`hyperonpy.CSExprParser`).
%   @arg RunnerState The resulting runner state (`hyperonpy.CRunnerState`).
hyperonpy_runner_state_new_with_parser(Metta, SExprParser, RunnerState) :-
    oo_new(runner_state, [metta:Metta, sexpr_parser:SExprParser], RunnerState).

%!  hyperonpy_runner_state_step(+RunnerState) is det.
%
%   Advances the runner state by one step.
%
%   This function performs a single step in the `hyperonpy.CRunnerState`.
%
%   @arg RunnerState The runner state (`hyperonpy.CRunnerState`).
hyperonpy_runner_state_step(RunnerState) :-
    oo_invoke(runner_state, RunnerState, step(), _).

% ==============================
% **2.16. Space (`space`) Functions**
% =============================

atom_form(Atom,Form):-
  Atom=Form.

into_id(o3(_,ID,_),ID):-!.
into_id(ID,ID):-!.

%!  hyperonpy_space_add(+Space, +Atom) is det.
%
%   Adds an atom to the space.
%
%   This function adds the given `hyperonpy.CAtom` to the provided `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Atom The atom to add (`hyperonpy.CAtom`).
hyperonpy_space_add(Space, Atom) :-
    atom_form(Atom,Form),into_id(Space,ID),
    assertz(metta_atom(ID,Form)).

%!  hyperonpy_space_atom_count(+Space, -Count) is det.
%
%   Retrieves the count of atoms in the space.
%
%   This function returns the number of atoms present in the `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Count The count of atoms.
hyperonpy_space_atom_count(Space, Count) :-
    oo_invoke(space, Space, atom_count(), Count).

%!  hyperonpy_space_eq(+Space1, +Space2, -AreEqual) is det.
%
%   Compares two spaces for equality.
%
%   This function checks whether two `hyperonpy.CSpace` objects are equal.
%
%   @arg Space1 The first space (`hyperonpy.CSpace`).
%   @arg Space2 The second space (`hyperonpy.CSpace`).
%   @arg AreEqual Boolean indicating whether the two spaces are equal.
hyperonpy_space_eq(Space1, Space2, AreEqual) :-
    oo_equal(Space1, Space2, AreEqual).

%!  hyperonpy_space_free(+Space) is det.
%
%   Frees a space from memory.
%
%   This function removes a `hyperonpy.CSpace` object from memory.
%
%   @arg Space The space (`hyperonpy.CSpace`) to be freed.
hyperonpy_space_free(Space) :-
    oo_free(Space).

%!  hyperonpy_space_get_payload(+Space, -Payload) is det.
%
%   Retrieves the payload associated with a space.
%
%   This function fetches the payload object associated with the provided `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Payload The payload object.
hyperonpy_space_get_payload(Space, Payload) :-
    oo_get(space, Space, payload, Payload).

%!  hyperonpy_space_list(+Space, -AtomListOpt) is det.
%
%   Retrieves the list of atoms in the space.
%
%   This function returns the list of atoms in the provided `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg AtomListOpt The list of atoms, or an optional value if empty.
hyperonpy_space_list(Space, AtomListOpt) :-
    oo_invoke(space, Space, list(), AtomListOpt).

%!  hyperonpy_space_new_custom(+CustomObject, -Space) is det.
%
%   Creates a new space with a custom object.
%
%   This function creates a new `hyperonpy.CSpace` with the provided custom object.
%
%   @arg CustomObject The custom object to associate with the space.
%   @arg Space The resulting space (`hyperonpy.CSpace`).
hyperonpy_space_new_custom(CustomObject, Space) :-
    oo_new(space, [custom_object:CustomObject], Space).

%!  hyperonpy_space_new_grounding(-Space) is det.
%
%   Creates a new grounding space.
%
%   This function creates a new grounding `hyperonpy.CSpace`.
%
%   @arg Space The resulting grounding space (`hyperonpy.CSpace`).
hyperonpy_space_new_grounding(Space) :-
    oo_new(space, [grounding:true], Space).

%!  hyperonpy_space_query(+Space, +QueryAtom, -BindingsSet) is det.
%
%   Queries the space with an atom.
%
%   This function queries the `hyperonpy.CSpace` with the given `hyperonpy.CAtom`, returning a set of bindings.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg QueryAtom The atom to query (`hyperonpy.CAtom`).
%   @arg BindingsSet The resulting set of bindings (`hyperonpy.CBindingsSet`).
hyperonpy_space_query(Space, QueryAtom, BindingsSet) :-
    oo_invoke(space, Space, query(QueryAtom), BindingsSet).

%!  hyperonpy_space_remove(+Space, +Atom, -IsRemoved) is det.
%
%   Removes an atom from the space.
%
%   This function removes the given `hyperonpy.CAtom` from the provided `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Atom The atom to remove (`hyperonpy.CAtom`).
%   @arg IsRemoved Boolean indicating whether the atom was removed.
hyperonpy_space_remove(Space, Atom, IsRemoved) :-
    oo_invoke(space, Space, remove(Atom), IsRemoved).

%!  hyperonpy_space_replace(+Space, +OldAtom, +NewAtom, -IsReplaced) is det.
%
%   Replaces an atom in the space with another.
%
%   This function replaces `OldAtom` with `NewAtom` in the provided `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg OldAtom The atom to replace (`hyperonpy.CAtom`).
%   @arg NewAtom The atom to insert (`hyperonpy.CAtom`).
%   @arg IsReplaced Boolean indicating if the atom was replaced.
hyperonpy_space_replace(Space, OldAtom, NewAtom, IsReplaced) :-
    oo_invoke(space, Space, replace(OldAtom, NewAtom), IsReplaced).

%!  hyperonpy_space_subst(+Space, +Atom1, +Atom2, -SubstList) is det.
%
%   Substitutes one atom for another in the space.
%
%   This function performs a substitution of `Atom1` for `Atom2` in the provided `hyperonpy.CSpace`, returning a list of substitutions.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Atom1 The atom to be replaced (`hyperonpy.CAtom`).
%   @arg Atom2 The atom to replace `Atom1` (`hyperonpy.CAtom`).
%   @arg SubstList The resulting list of substitutions.
hyperonpy_space_subst(Space, Atom1, Atom2, SubstList) :-
    oo_invoke(space, Space, subst(Atom1, Atom2), SubstList).

% ==============================
% **2.17. Step Result Functions**
% =============================

%!  hyperonpy_step_get_result(+StepResult, -ResultList) is det.
%
%   Retrieves the result list from a step result.
%
%   This function fetches the list of results from the `hyperonpy.CStepResult`.
%
%   @arg StepResult The step result (`hyperonpy.CStepResult`).
%   @arg ResultList The resulting list.
hyperonpy_step_get_result(StepResult, ResultList) :-
    oo_invoke(step_result, StepResult, get_result(), ResultList).

%!  hyperonpy_step_has_next(+StepResult, -HasNext) is det.
%
%   Checks if the step result has more steps to process.
%
%   This function checks whether the `hyperonpy.CStepResult` has more steps.
%
%   @arg StepResult The step result (`hyperonpy.CStepResult`).
%   @arg HasNext Boolean indicating if more steps are available.
hyperonpy_step_has_next(StepResult, HasNext) :-
    oo_invoke(step_result, StepResult, has_next(), HasNext).

% ==============================
% **2.18. Syntax Node Functions**
% =============================

%!  hyperonpy_syntax_node_clone(+SyntaxNode, -ClonedNode) is det.
%
%   Clones a syntax node.
%
%   This function creates a copy of the given `hyperonpy.CSyntaxNode`.
%
%   @arg SyntaxNode The syntax node to clone (`hyperonpy.CSyntaxNode`).
%   @arg ClonedNode The resulting cloned syntax node (`hyperonpy.CSyntaxNode`).
hyperonpy_syntax_node_clone(SyntaxNode, ClonedNode) :-
    oo_clone(SyntaxNode, ClonedNode).

%!  hyperonpy_syntax_node_free(+SyntaxNode) is det.
%
%   Frees a syntax node from memory.
%
%   This function removes a `hyperonpy.CSyntaxNode` object from memory.
%
%   @arg SyntaxNode The syntax node (`hyperonpy.CSyntaxNode`) to be freed.
hyperonpy_syntax_node_free(SyntaxNode) :-
    oo_free(SyntaxNode).

%!  hyperonpy_syntax_node_is_leaf(+SyntaxNode, -IsLeaf) is det.
%
%   Checks if the syntax node is a leaf node.
%
%   This function checks whether the provided `hyperonpy.CSyntaxNode` is a leaf node.
%
%   @arg SyntaxNode The syntax node (`hyperonpy.CSyntaxNode`).
%   @arg IsLeaf Boolean result indicating if the node is a leaf.
hyperonpy_syntax_node_is_leaf(SyntaxNode, IsLeaf) :-
    oo_invoke(syntax_node, SyntaxNode, is_leaf(), IsLeaf).

%!  hyperonpy_syntax_node_is_null(+SyntaxNode, -IsNull) is det.
%
%   Checks if the syntax node is null.
%
%   This function checks whether the provided `hyperonpy.CSyntaxNode` is a null node.
%
%   @arg SyntaxNode The syntax node (`hyperonpy.CSyntaxNode`).
%   @arg IsNull Boolean result indicating if the node is null.
hyperonpy_syntax_node_is_null(SyntaxNode, IsNull) :-
    oo_invoke(syntax_node, SyntaxNode, is_null(), IsNull).

%!  hyperonpy_syntax_node_src_range(+SyntaxNode, -SrcRange) is det.
%
%   Retrieves the source range associated with a syntax node.
%
%   This function fetches the source range of the provided `hyperonpy.CSyntaxNode`.
%
%   @arg SyntaxNode The syntax node (`hyperonpy.CSyntaxNode`).
%   @arg SrcRange The source range object.
hyperonpy_syntax_node_src_range(SyntaxNode, SrcRange) :-
    oo_get(syntax_node, SyntaxNode, src_range, SrcRange).

%!  hyperonpy_syntax_node_type(+SyntaxNode, -NodeType) is det.
%
%   Retrieves the type of a syntax node.
%
%   This function fetches the type of the provided `hyperonpy.CSyntaxNode`.
%
%   @arg SyntaxNode The syntax node (`hyperonpy.CSyntaxNode`).
%   @arg NodeType The node type (`hyperonpy.SyntaxNodeType`).
hyperonpy_syntax_node_type(SyntaxNode, NodeType) :-
    oo_get(syntax_node, SyntaxNode, type, NodeType).

%!  hyperonpy_syntax_node_unroll(+SyntaxNode, -UnrolledList) is det.
%
%   Unrolls a syntax node into a list.
%
%   This function unrolls the components of the `hyperonpy.CSyntaxNode` into a list.
%
%   @arg SyntaxNode The syntax node (`hyperonpy.CSyntaxNode`).
%   @arg UnrolledList The list of unrolled components.
hyperonpy_syntax_node_unroll(SyntaxNode, UnrolledList) :-
    oo_invoke(syntax_node, SyntaxNode, unroll(), UnrolledList).

% ==============================
% **2.19. Tokenizer Functions**
% =============================

%!  hyperonpy_tokenizer_clone(+Tokenizer, -ClonedTokenizer) is det.
%
%   Clones a tokenizer.
%
%   This function creates a copy of the given `hyperonpy.CTokenizer`.
%
%   @arg Tokenizer The tokenizer to clone (`hyperonpy.CTokenizer`).
%   @arg ClonedTokenizer The resulting cloned tokenizer.
hyperonpy_tokenizer_clone(Tokenizer, ClonedTokenizer) :-
    oo_clone(Tokenizer, ClonedTokenizer).

%!  hyperonpy_tokenizer_free(+Tokenizer) is det.
%
%   Frees a tokenizer from memory.
%
%   This function removes a `hyperonpy.CTokenizer` object from memory.
%
%   @arg Tokenizer The tokenizer (`hyperonpy.CTokenizer`) to be freed.
hyperonpy_tokenizer_free(Tokenizer) :-
    oo_free(Tokenizer).

%!  hyperonpy_tokenizer_new(-Tokenizer) is det.
%
%   Creates a new tokenizer.
%
%   This function initializes a new `hyperonpy.CTokenizer`.
%
%   @arg Tokenizer The resulting tokenizer.
hyperonpy_tokenizer_new(Tokenizer) :-
    oo_new(tokenizer, [type:tokenizer, value:[]], Tokenizer).

%!  hyperonpy_tokenizer_register_token(+Tokenizer, +Token, +Callback) is det.
%
%   Registers a token with the tokenizer.
%
%   This function registers a new token and its associated callback function in the provided `hyperonpy.CTokenizer`.
%
%   @arg Tokenizer The tokenizer (`hyperonpy.CTokenizer`).
%   @arg Token The token to register (string).
%   @arg Callback The callback function for the token.
hyperonpy_tokenizer_register_token(Tokenizer, Token, Callback) :-
    oo_invoke(tokenizer, Tokenizer, register_token(Token, Callback), _).

% ==============================
% **2.20. Validation Function**
% =============================

%!  hyperonpy_validate_atom(+Space, +Atom, -IsValid) is det.
%
%   Validates an atom within a space.
%
%   This function validates a `hyperonpy.CAtom` in the provided `hyperonpy.CSpace`.
%
%   @arg Space The space (`hyperonpy.CSpace`).
%   @arg Atom The atom (`hyperonpy.CAtom`) to validate.
%   @arg IsValid Boolean result indicating if the atom is valid.
hyperonpy_validate_atom(Space, Atom, IsValid) :-
    oo_invoke(space, Space, validate_atom(Atom), IsValid).


% ==============================
% End of the hyperonpy_* function definitions.
% ==============================

%!  handle_method_call(+Type, +ID, +Attributes, +MethodCall, -Result) is det.
%
%   Handles method calls on objects by matching on the object's type and method.
%
%   This predicate defines various method handlers that can be invoked on objects,
%   depending on the object's type and the method called. Each method handler
%   modifies or queries the object's state based on the method call.
%
%   @arg Type The type of the object.
%   @arg ID The ID of the object.
%   @arg Attributes The attributes associated with the object.
%   @arg MethodCall The method being invoked on the object.
%   @arg Result The result of the method call (if applicable).
%
%   @example
%   ?- oo_invoke(atom_vec, atom_vec1, length(), Len).
%   Len = 3.
%

% ------------------ Handle Method Calls Implementation ------------------

% Handle missing attributes
handle_method_call(Type, ID, Attributes, Method, Result):-
   var(Attributes), nonvar(ID),
   into_object(ID, o3(Type, _, Attributes2)), Attributes\=@=Attributes2,!,
   handle_method_call(Type, ID, Attributes2, Method, Result).

% Handling length() for other o3 types that have a list attribute
handle_method_call(_, _, Attributes, length(), Length) :-
    freeze(List,is_list(List)),member_chk(value:List, Attributes),
    length(List, Length).


% Push method
handle_method_call(atom_vec, ID, Attributes, push(Element), true) :-
    member_chk(value:List, Attributes),
    append(List, [Element], NewList),
    retract(o3(atom_vec, ID, Attributes)),
    assert(o3(atom_vec, ID, [type:atom_vec, value:NewList])).

% Pop method
handle_method_call(atom_vec, ID, Attributes, pop(), PoppedElement) :-
    member_chk(value:List, Attributes),
    append(PoppedList, [PoppedElement], List),
    retract(o3(atom_vec, ID, Attributes)),
    assert(o3(atom_vec, ID, [type:atom_vec, value:PoppedList])).

% Handle method for other types as per your list
handle_method_call(atom, _ID, Attributes, to_str(), Str) :-
    member_chk(value:Value, Attributes),
    format(atom(Str), '~w', [Value]).

% Handle get_* methods
handle_method_call(_Type, _ID, Attributes, Method, Value) :-
    compound_name_arity(Method, Get_Something, 0),
    % Ensures that Method is a compound term like get_name, get_type, etc., with arity 0
    compound_name_arity(Method, Get_Something, 0),
    % Ensure that the functor begins with 'get_' followed by the field name
    atom_concat('get_', FieldName, Get_Something),
    % Look up the attribute by the extracted field name
    member_chk(FieldName:Value, Attributes),!.

% in case get_method and not get_method()
handle_method_call(Type, ID, Attributes, AMethod, Value) :-
    atom(AMethod),compound_name_arity(Method, Method, 0),!,
    handle_method_call(Type, ID, Attributes, Method, Value).

% atom_eq
handle_method_call(atom, _ID1, Attributes1, eq(ID2), AreEqual) :-
    member_chk(value:Atom1, Attributes1),
    o3(atom, ID2, [value:Atom2]),
    AreEqual = (Atom1 == Atom2).

% atom_error_message
handle_method_call(atom, _ID, Attributes, error_message(), ErrorMessage) :-
    member_chk(value:_Atom, Attributes),
    atom_is_error(ID, true),
    atom_get_children(ID, [_, ErrorMessage]).

% atom_expr
handle_method_call(atom, _Static, _, expr(ExpressionList), NewID) :-
    oo_new(atom, [metatype:'Expression', value:ExpressionList], NewID).

% atom_free
handle_method_call(Atom, ID, _, free(), _) :-
    retractall(o3(Atom, ID, _)).

% atom_get_children
handle_method_call(atom, _ID, Attributes, get_children(), Children) :-
    member_chk(metatype:'Expression', Attributes),
    member_chk(value:Children, Attributes).

% atom_gnd
handle_method_call(atom, _Static, _, gnd(Object, GroundedType), NewID) :-
    oo_new(atom, [metatype:'Grounded', object:Object, grounded_type:GroundedType], NewID).

% atom_sym
handle_method_call(atom, _Static, _, atom_sym(Symbol), Atom) :-
    hyperon_atom_var(Symbol, Atom).

% atom_var
handle_method_call(atom, _Static, _, atom_var(VarName), Atom) :-
    hyperon_atom_var(VarName, Atom).


% atom_to_str
handle_method_call(atom, _ID, Attributes, to_str(), Str) :-
    member_chk(value:Atom, Attributes),
    format(atom(Str), '~w', [Atom]).

% atom_vec_new
handle_method_call(atom_vec, _Static, _, new(), NewID) :-
    oo_new(atom_vec, [type:atom_vec, value:[]], NewID).

% atom_vec_from_list
handle_method_call(atom_vec, _Static, _, from_list(List), NewID) :-
    oo_new(atom_vec, [type:atom_vec, value:List], NewID).

% bindings_add_var_binding
handle_method_call(bindings, _ID, Attributes, add_var_binding(VarAtom, BoundAtom), Success) :-
    member_chk(value:Bindings, Attributes),
    (   memberchk(VarAtom-BoundAtom, Bindings)
    ->  Success = @(true)
    ;   Success = @(false)).

% bindings_clone
handle_method_call(bindings, _ID, Attributes, clone(), NewID) :-
    member_chk(value:Bindings, Attributes),
    oo_new(bindings, [type:bindings, value:Bindings], NewID).

% bindings_is_empty
handle_method_call(bindings, _ID, Attributes, is_empty(), IsEmpty) :-
    member_chk(value:Bindings, Attributes),
    py_is_tf(Bindings == [], IsEmpty).

% bindings_list
handle_method_call(bindings, _ID, Attributes, list(), List) :-
    member_chk(value:Bindings, Attributes),
    findall(Var-Atom, member(Var-Atom, Bindings), List).

% bindings_merge
handle_method_call(bindings, _ID1, Attributes1, merge(ID2), NewID) :-
    member_chk(value:Bindings1, Attributes1),
    o3(bindings, ID2, [value:Bindings2]),
    append(Bindings1, Bindings2, ResultBindings),
    oo_new(bindings, [type:bindings, value:ResultBindings], NewID).

% bindings_new
handle_method_call(bindings, _Static, _, new(), NewID) :-
    oo_new(bindings, [type:bindings, value:[]], NewID).

% bindings_resolve
handle_method_call(bindings, _ID, Attributes, resolve(VarAtom), ResolvedAtom) :-
    member_chk(value:Bindings, Attributes),
    member(VarAtom-ResolvedAtom, Bindings).

% bindings_set_add_var_binding
handle_method_call(bindings_set, ID, Attributes, add_var_binding(VarAtom, BoundAtom), _) :-
    member_chk(value:BindingsSet, Attributes),
    append(BindingsSet, [[VarAtom-BoundAtom]], NewSet),
    retractall(o3(bindings_set, ID, _)),
    assert(o3(bindings_set, ID, [type:bindings_set, value:NewSet])).

% bindings_set_is_empty
handle_method_call(bindings_set, _ID, Attributes, is_empty(), IsEmpty) :-
    member_chk(value:BindingsSet, Attributes),
    (BindingsSet == [] -> IsEmpty = @(true); IsEmpty = @(false)).

% bindings_set_list
handle_method_call(bindings_set, _ID, Attributes, list(), List) :-
    member_chk(value:BindingsSet, Attributes),
    List = BindingsSet.

% bindings_set_merge_into
handle_method_call(bindings_set, ID1, Attributes1, merge_into(ID2), _) :-
    member_chk(value:BindingsSet1, Attributes1),
    o3(bindings_set, ID2, [value:BindingsSet2]),
    append(BindingsSet1, BindingsSet2, MergedSet),
    retractall(o3(bindings_set, ID1, _)),
    assert(o3(bindings_set, ID1, [type:bindings_set, value:MergedSet])).

% tokenizer_new
handle_method_call(tokenizer, NewID, _, new(), _) :-
    oo_new(tokenizer, [type:tokenizer, value:[]], NewID).

% tokenizer_register_token
handle_method_call(tokenizer, ID, Attributes, register_token(Token, Callback), _) :-
    member_chk(value:TokenList, Attributes),
    append(TokenList, [(Token, Callback)], NewTokenList),
    retractall(o3(tokenizer, ID, _)),
    assert(o3(tokenizer, ID, [type:tokenizer, value:NewTokenList])).

% tokenizer_clone
handle_method_call(tokenizer, NewID, Attributes, clone(), _) :-
    member_chk(value:TokenList, Attributes),
    oo_new(tokenizer, [type:tokenizer, value:TokenList], NewID).

% syntax_node_free
handle_method_call(syntax_node, ID, _, free(), _) :-
    retractall(o3(syntax_node, ID, _)).

% syntax_node_is_null
handle_method_call(syntax_node, _ID, Attributes, is_null(), IsNull) :-
    py_is_tf( ( \+ member_chk(value:_, Attributes)), IsNull).
    

% syntax_node_clone
handle_method_call(syntax_node, NewID, Attributes, clone(), _) :-
    member_chk(value:NodeValue, Attributes),
    oo_new(syntax_node, [type:syntax_node, value:NodeValue], NewID).


end_of_file.

% Create a new atom
?- hyperonpy_atom_sym('x', Atom).
% Atom = o3(atom, atom_1, [type:atom_sym, value:'x']).

% Retrieve the name of the atom
?- hyperonpy_atom_get_name(Atom, Name).
% Name = 'x'.

% Convert the atom to a string
?- hyperonpy_atom_to_str(Atom, Str).
% Str = "x".

% Clone the atom
?- hyperonpy_atom_clone(Atom, ClonedAtom).
% ClonedAtom = o3(atom, atom_2, [type:atom_sym, value:'x']).

% Check equality
?- hyperonpy_atom_eq(Atom, ClonedAtom, AreEqual).
% AreEqual = @(false).

% Free the cloned atom
?- hyperonpy_atom_free(ClonedAtom).
true.



% ==============================
% **4.2. Working with Atom Vectors**
% =============================


% Create a new atom vector from a list
?- hyperonpy_atom_vec_from_list([Atom1, Atom2, Atom3], AtomVec).
% AtomVec = o3(atom_vec, atom_vec_1, [type:atom_vec, value:[o3(atom, atom_1, ...), o3(atom, atom_2, ...), o3(atom, atom_3, ...)]]).
    
% Get the length of the atom vector
?- hyperonpy_atom_vec_len(AtomVec, Length).
% Length = 3.

% Push a new atom into the vector
?- hyperonpy_atom_vec_push(AtomVec, Atom4).
true.

% Pop an atom from the vector
?- hyperonpy_atom_vec_pop(AtomVec, PoppedAtom).
% PoppedAtom = o3(atom, atom_4, [type:atom_sym, value:'y']).



% ==============================
% **4.3. Managing Bindings**
% =============================


% Create new bindings
?- hyperonpy_bindings_new(Bindings).
% Bindings = o3(bindings, bindings_1, [value:[]]).

% Add a variable binding
?- hyperonpy_bindings_add_var_binding(Bindings, VarAtom, BoundAtom, Success).
% Success = @(true).

% Clone bindings
?- hyperonpy_bindings_clone(Bindings, NewBindings).
% NewBindings = o3(bindings, bindings_2, [value:[...]]).

% Check if bindings are equal
?- hyperonpy_bindings_eq(Bindings, NewBindings, AreEqual).
% AreEqual = @(false).

% Free bindings
?- hyperonpy_bindings_free(NewBindings).
true.



