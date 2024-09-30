:- ensure_loaded(metta_interp).
:- style_check(-singleton).

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
trace_failures(A) :- (A *-> true; (wfailed(A),trace,A)).

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
    trace_failures(once((call_ret_type(Predicate, ReturnType, Return, Result),
                    if_bugger(writeln(Return -> Result)), nonvar(Result)))).

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
    ignore(trace_failures(Predicate)) -> ignore(Result = '@'('none')).

% Generic handler for other return types.
call_ret_type(Predicate, _RetType, Return, Result) :- !,
    call(Predicate), ret_res(Return, Result).

% Helper to process the return value.
% ret_res(o3(_, ID, _), ID) :- nonvar(ID), !.
ret_res(ID, ID).


%!  override_hyperonpy is det.
%
%   Loads the Python override (if not already loaded) and calls the Python function `override_hyperonpy/0`
%   from the `metta_python_override` module. The result is expected to be an empty Python dictionary (`py{}`).
%
%   @example
%     % Applying the override to Hyperon:
%     ?- override_hyperonpy.
%   true.
%
override_hyperonpy :-
    load_metta_python_override,  % Ensure the override is loaded.
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
    load_metta_python_override,  % Ensure the override is loaded.
    py_call(metta_python_override:test_hyperon_overrides(), _), !.


%!  metta_python_override(-Content) is det.
%
%   Reads and asserts the content of the Python file 'metta_python_override.py' as a dynamic fact.
%   This allows the Python code to be accessed and used at runtime within Prolog.
%
%   @arg Content A string representing the contents of the 'metta_python_override.py' file.
%
%   @example
%     % Retrieve the content of the Python override:
%     ?- metta_python_override(Content).
%   Content = "... Python code ...".
%
:- dynamic(metta_python_override/1).

% Read the content of the Python override file and assert it as a fact.
:- read_file_to_string('./metta_python_override.py', String, []),
   assertz(metta_python_override(String)), !.


%!  did_load_metta_python_override is det.
%
%   A volatile predicate that acts as a flag indicating whether the Python override has been loaded.
%   This flag is not saved between sessions and only exists during the current runtime.
%
%   @example
%     % After loading the override, this will succeed:
%     ?- did_load_metta_python_override.
%
:- dynamic(did_load_metta_python_override/0).
:- volatile(did_load_metta_python_override/0).


%!  load_metta_python_override is det.
%
%   Loads the Python override if it hasn't been loaded yet. The override content is retrieved
%   from `metta_python_override/1`, and the Python module is loaded via `py_module/2`. Once
%   loaded, `did_load_metta_python_override/0` is asserted to prevent reloading.
%
%   @example
%     % Load the Python override:
%     ?- load_metta_python_override.
%   true.
%
load_metta_python_override :-
    did_load_metta_python_override, !.  % If already loaded, do nothing.

load_metta_python_override :-
    % Retrieve the Python override content.
    metta_python_override(String),
    % Load the Python module via py_module/2.
    py_module(metta_python_override, String),
    % Assert that the override has now been loaded.
    assert(did_load_metta_python_override), !.
    % Try to load the module again, ignoring any errors.
    % ignore(notrace(with_safe_argv(catch(py_module(metta_python_override, String), _, true)))), !.



%!  maybe_load_metta_python_override is det.
%
%   This predicate ensures that the Python integration for Metta is loaded.
%   It tries to load the Python interface lazily by calling `lazy_load_python/0`.
%   If the lazy loading succeeds (determined by the cut `!`), it does nothing more.
%   If lazy loading fails, it proceeds to load the Metta Python override using
%   `load_metta_python_override/0`.
%
maybe_load_metta_python_override :-
    % Attempt lazy loading of the Python interface.
    lazy_load_python, !.
maybe_load_metta_python_override :-
    % If lazy loading fails, load the Metta Python override manually.
    load_metta_python_override.

% The following directives ensure that `maybe_load_metta_python_override/0` is called 
% during system initialization. The first initialization runs when the program 
% starts, and the second runs when the system is restored from a saved state.
%:- initialization(maybe_load_metta_python_override).
%:- initialization(maybe_load_metta_python_override, restore).



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
load_metta_python_patcher :- !.
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
    catch_log(py_call(mettalog:repl())), !.

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
% Module Declaration and Exports
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

% Declare dynamic predicates
:- dynamic metta_atom/2.
:- dynamic(o_f_v/3).
:- dynamic(t_f_v/3).
% ==================================================
% **1. Object-Oriented Simulation in Prolog**
% ==================================================
% Predicate to handle objects directly if they are strings, numbers, symbols, or variables.
from_obj(Value, Obj) :- var(Value),!, Obj = Value.
from_obj(Value, Obj) :-
    direct_atom(Value), !,  % Only handle strings, numbers, symbols, and variables.
    Obj = Value.
from_obj(Obj, Obj).  % If not a direct atom, return the object directly.

% Predicate to identify direct atoms (strings, numbers, symbols, variables).
direct_atom(Value) :-
    (symbol_not_ref(Value); string(Value); number(Value); bvar(Value)), !.

% Main predicate to manage object fields based on their type.
o_f_v(Object, FieldName, Value) :-
    determine_object_type(Object, Type), !,  % Identify the object type.
    object_field_value(Type, Object, FieldName, Value).

% Mapping between predicates and their corresponding type names.
p1_typename(bvar, 'Variable').
p1_typename(is_list, 'Expression').
p1_typename(string, 'String').
p1_typename(number, 'Number').
p1_typename(symbol_not_ref, 'Symbol').


% Predicate to identify symbols that are not references (i.e., not declared o_f_v/3).
symbol_not_ref(Atom) :-
    atom(Atom),
    \+ clause(o_f_v(Atom, type, _), true).

% Predicate to identify variables using the custom bvar/1 predicate.
bvar(Var) :- var(Var),!.
bvar(Var) :- compound(Var),!,Var='$VAR'(_).
bvar(Var, Symbol) :- compound(Var),!,Var='$VAR'(Symbol).


% Predicate to determine the object type using p1_typename/2 mappings.
determine_object_type(Object, TypeName) :-
    p1_typename(PredicateName, TypeName),
    call(PredicateName, Object), !.


atom_kind(EXPR,Value):- 
 py_call(hyperonpy:'AtomKind',EXPR,Value).
% Helper predicates for different object types.

% Handle lists with metatype 'Expression' and no grounded_type.
object_field_value('Expression', Object, FieldName, Value) :- !,
   ((FieldName = type, Value = atom);
    (FieldName = metatype, atom_kind('EXPR',Value));
    (FieldName = value, Value = Object)).

% Handle variables with metatype 'Variable' and no grounded_type.
object_field_value('Variable', Object, FieldName, Value) :- !,
   ((FieldName = type, Value = atom);
    (FieldName = metatype, atom_kind('VARIABLE',Value));
    (FieldName = value, Value = Object)).

% Handle symbols with metatype 'Symbol' and no grounded_type.
object_field_value('Symbol', Object, FieldName, Value) :- !,
   ((FieldName = type, Value = atom);
    (FieldName = metatype, atom_kind('SYMBOL',Value));
    (FieldName = value, Value = Object)).

% Handle objects of type 'String' with 'grounded_type'.
object_field_value('String', Object, FieldName, Value) :- !,
    object_field_value_base(Object, FieldName, Value, 'String').

% Handle objects of type 'Number' with 'grounded_type'.
object_field_value('Number', Object, FieldName, Value) :- !,
    object_field_value_base(Object, FieldName, Value, 'Number').


% Generic handler for atoms that are not specifically typed as 'String', 'Number', 'Symbol', 'Expression', or 'Variable'.
object_field_value(atom, _Object, FieldName, Value) :-
    (FieldName = type, Value = atom).

% Base handler for returning the field values for grounded object types ('String', 'Number').
object_field_value_base(Object, FieldName, Value, GroundedType) :-
   ((FieldName = type, Value = atom);
    (FieldName = value, Value = Object);
    (FieldName = grounded_type, Value = GroundedType);
    (FieldName = metatype, atom_kind('GROUNDED',Value))).


% 1. oo_new/3

% oo_new(+Type, +Attributes, -ObjectID) is det.
%
% Creates a new object with a unique ID based on its type.
% Stores the object's state in o_f_v(ID, FieldName, Value).
%
% Adjusted oo_new/3 for handling numbers and strings as ObjectID.
oo_new(Type, Attributes, ObjectID) :- fail,  
    % Separate the 'value' field from the rest of the attributes.
    select(value:ValueIn, Attributes, RestOf),
    % General case for other types of values.
    o_f_v(ExistingID, value, Value),
    (var(Value) -> Value==ValueIn ; ValueIn =@= Value),
    o_f_v(ExistingID, type, Type),
    forall(
        member(FieldName:FieldValue, RestOf),
        o_f_v(ExistingID, FieldName, FieldValue)
    ),
    !,  % Cut to prevent backtracking once a match is found.
    ObjectID = ExistingID.
   

% Fallback clause: If no existing object is found, create a new one.
oo_new(Type, Attributes, ObjectID) :-
    % Generate a unique ID for the new object.
    generate_object_id(Type, ObjectID),
    % Assert the type and all provided attributes for the new object.
    assertz(o_f_v(ObjectID, type, Type)),
    forall(
        member(FieldName:FieldValue, Attributes),
        assertz(o_f_v(ObjectID, FieldName, FieldValue))
    ).

% Helper predicate to set up default fields from t_f_v/3 if not explicitly provided.
setup_default_fields(Type, ObjectID, ProvidedFields) :-
    % Get all default fields for the given type.
    findall(FieldName:DefaultValue, t_f_v(Type, FieldName, DefaultValue), Defaults),
    % Merge provided fields with defaults, giving priority to provided fields.
    merge_fields(Defaults, ProvidedFields, FinalFields),
    % Assert each field into o_f_v/3.
    forall(
        member(FieldName:FieldValue, FinalFields),
        assertz(o_f_v(ObjectID, FieldName, FieldValue))
    ).

% Merge provided fields with defaults, giving priority to provided fields.
merge_fields([], ProvidedFields, ProvidedFields).
merge_fields([FieldName:DefaultValue | RestDefaults], ProvidedFields, [FieldName:Value | Merged]) :-
    (select(FieldName:Value, ProvidedFields, Remaining) -> true ; Value = DefaultValue, Remaining = ProvidedFields),
    merge_fields(RestDefaults, Remaining, Merged).

% Generate a unique ID for each object based on its type.
generate_object_id(Type, ID) :-
    atom_concat(Type, '_', TempID),
    gensym(TempID, ID).


% 2. oo_free/1

%!  oo_free(+ObjectOrID) is det.
%
%   Frees (removes) an object from the system.
%
oo_free(ObjectID) :-
    retractall(o_f_v(ObjectID, _, _)).
% 3. oo_invoke/4


% oo_get(+Type, +ObjectID, +FieldName, -Value) is det.
%
% Retrieves the value of a specific field from an object's state.
% If the field value is not directly stored for the object, it looks up the Type.
%
oo_get(Type, ObjectID, FieldName, Value) :- fail,
    o_f_v(ObjectID, type, Type),
    ( o_f_v(ObjectID, FieldName, Value)
    ; ( t_f_v(Type, FieldName, Value) )
    ), !.

oo_get(_Type,  ObjectID,  FieldName, Value) :- o_f_v(ObjectID, FieldName, Value),!.
oo_get( Type, _ObjectID,  FieldName, Value) :- t_f_v(Type, FieldName, Value),!.
oo_get(_Type,  ObjectID, _FieldName, Value) :- o_f_v(ObjectID, value, Value),!.
oo_get_else( Type, ObjectID, FieldName, Value, Else):-
  oo_get( Type, ObjectID, FieldName, Value)->true;Value=Else.



% oo_set(+ObjectID, +FieldName, +Value) is det.
%
% Sets the value of a specific field in an object's state.
%
oo_set(ObjectID, FieldName, Value) :-
    retractall(o_f_v(ObjectID, FieldName, _)),
    assertz(o_f_v(ObjectID, FieldName, Value)).

% oo_invoke(+Type, +ObjectID, +MethodCall, -Result) is det.
%
% Invokes a method on an object.
%
oo_invoke(Type, ObjectID, MethodCall, Result) :-
    o_f_v(ObjectID, type, Type),
    handle_method_call(Type, ObjectID, MethodCall, Result).

% 4. oo_clone/2

% oo_clone(+ObjectID, -ClonedObjectID) is det.
%
% Clones an existing object by creating a new object with the same fields.
%
oo_clone(ObjectID, ClonedObjectID) :-
    o_f_v(ObjectID, type, Type),
    findall(FieldName:Value, o_f_v(ObjectID, FieldName, Value), Attributes),
    exclude(=(type:_), Attributes, AttributesWithoutType),
    oo_new(Type, AttributesWithoutType, ClonedObjectID).

% oo_equal(+ObjectID1, +ObjectID2, -AreEqual) is det.
%
%   Checks equality between two objects.
%
oo_equal(ObjectID1, ObjectID2, AreEqual) :-
    (ObjectID1 == ObjectID2 ->
        AreEqual = '@'(true)
    ;
        collect_object_state(ObjectID1, State1),
        collect_object_state(ObjectID2, State2),
        (State1 == State2 -> AreEqual = '@'(true); AreEqual = '@'(false))
    ).


% collect_object_state(+ObjectID, -State) is det.
%
% Collects all field-value pairs for an object into a list.
%
collect_object_state(ObjectID, State) :-
    findall(FieldName-Value, o_f_v(ObjectID, FieldName, Value), State).

% ==================================================
% Type Field Values (Default values for types)
% ==================================================

% Define default field values for types here if needed.
% Example:
% t_f_v(atom_vec, value, []).

% ==================================================
% 1. Atom Functions (8-28)
% ==================================================


% 8. hyperonpy_atom_sym/2

%!  hyperonpy_atom_sym(+Symbol, -Atom) is det.
%
%   Creates a symbolic atom from a symbol.
%
hyperonpy_atom_sym(String, Atom) :-
    (symbol(String)->Symbol=String;name(Symbol, String)),
    hyperonpy_sym_sym(Symbol, Atom).
    
hyperonpy_sym_sym(Symbol, Atom):- symbol_not_ref(Symbol),!, Atom=Symbol.
hyperonpy_sym_sym(Symbol, Atom):-    
    oo_new(atom, [metatype:'Symbol', value:Symbol], Atom),!.

% 9. hyperonpy_atom_var/2

%!  hyperonpy_atom_var(+VarName, -Atom) is det.
%
%   Creates a variable atom from a variable name.
%
hyperonpy_atom_var(VarName, Atom) :-
    (bvar(VarName,Symbol)-> true ; name(Symbol, VarName)),
    oo_new(atom, [metatype:'Variable', value:Symbol], Atom).

% 10. hyperonpy_atom_expr/2

%!  hyperonpy_atom_expr(+ExpressionList, -Atom) is det.
%
%   Creates an expression atom from a list of expressions.
%
hyperonpy_atom_expr(ExpressionList, Atom) :-
    oo_new(atom, [metatype:'Expression', value:ExpressionList], Atom).

% 11. hyperonpy_atom_gnd/3

%!  hyperonpy_atom_gnd(+Object, +GroundedType, -Atom) is det.
%
%   Creates a grounded atom with a specified type.
%
% Adjusted hyperonpy_atom_gnd/3 to handle numbers directly
hyperonpy_atom_gnd(Object, GroundedType, Atom) :-
    (GroundedType == 'Number', number(Object) ->
        oo_new(atom, [metatype:'Grounded', value:Object, grounded_type:GroundedType], Atom)
    ;
        % Handle other grounded types as needed
        oo_new(atom, [metatype:'Grounded', value:Object, grounded_type:GroundedType], Atom)
    ).

% 12. hyperonpy_atom_free/1

%!  hyperonpy_atom_free(+Atom) is det.
%
%   Frees an atom from memory.
%
hyperonpy_atom_free(Atom) :-
    oo_free(Atom).

% 13. hyperonpy_atom_to_str/2

%!  hyperonpy_atom_to_str(+Atom, -Str) is det.
%
%   Converts an atom to its string representation.
%
hyperonpy_atom_to_str(Atom, Str) :-
    oo_get(atom, Atom, metatype, Metatype),
    oo_get(atom, Atom, value, Value),
    (Metatype == 'Symbol' ->
        format(atom(Str), "~w", [Value])
    ; Metatype == 'Variable' ->
        format(atom(Str), "?~w", [Value])
    ; Metatype == 'Expression' ->
        format(atom(Str), "(~w)", [Value])
    ; Metatype == 'Grounded' ->
        format(atom(Str), "<~w>", [Value])
    ; Str = ""
    ).

% 14. hyperonpy_atom_eq/3

%!  hyperonpy_atom_eq(+Atom1, +Atom2, -AreEqual) is det.
%
%   Compares two atoms for equality.
%
hyperonpy_atom_eq(Atom1, Atom2, AreEqual) :-
    oo_equal(Atom1, Atom2, AreEqual).

% 15. hyperonpy_atom_get_name/2

%!  hyperonpy_atom_get_name(+Atom, -Name) is det.
%
%   Retrieves the name of an atom.
%
hyperonpy_atom_get_name(Atom, Name) :-
    oo_get(atom, Atom, value, Name).

% 16. hyperonpy_atom_get_children/2

%!  hyperonpy_atom_get_children(+Atom, -Children) is det.
%
%   Retrieves the children of an expression atom.
%
hyperonpy_atom_get_children(Atom, Children) :-
    oo_get(atom, Atom, metatype, 'Expression'),
    oo_get(atom, Atom, value, Children).

% 17. hyperonpy_atom_get_grounded_type/2

%!  hyperonpy_atom_get_grounded_type(+Atom, -GroundedType) is det.
%
%   Retrieves the grounded type of a grounded atom.
%
hyperonpy_atom_get_grounded_type(Atom, GroundedType) :-
    % oo_get(atom, Atom, metatype, 'Grounded'),
    oo_get(atom, Atom, grounded_type, GroundedType).

% 18. hyperonpy_atom_get_object/2

%!  hyperonpy_atom_get_object(+Atom, -Object) is det.
%
%   Retrieves the object associated with a grounded atom.
%
hyperonpy_atom_get_object(Atom, Object) :-
    % oo_get(atom, Atom, metatype, 'Grounded'),
    oo_get(atom, Atom, value, Object).

% 19. hyperonpy_atom_is_cgrounded/2

%!  hyperonpy_atom_is_cgrounded(+Atom, -IsCgrounded) is det.
%
%   Checks if an atom is a grounded atom.
%
hyperonpy_atom_is_cgrounded(Atom, IsCgrounded) :-
    oo_get(atom, Atom, metatype, Metatype),
    (Metatype == 'Grounded' -> IsCgrounded = '@'(true); IsCgrounded = '@'(false)).

% 20. hyperonpy_atom_is_error/2

%!  hyperonpy_atom_is_error(+Atom, -IsError) is det.
%
%   Checks if an atom represents an error.
%
hyperonpy_atom_is_error(Atom, IsError) :-
    oo_get(atom, Atom, is_error, IsErrorValue),
    (nonvar(IsErrorValue) -> IsError = IsErrorValue; IsError = '@'(false)).

% 21. hyperonpy_atom_error_message/2

%!  hyperonpy_atom_error_message(+Atom, -ErrorMessage) is det.
%
%   Retrieves the error message associated with an atom.
%
hyperonpy_atom_error_message(Atom, ErrorMessage) :-
    oo_get(atom, Atom, is_error, '@'(true)),
    oo_get(atom, Atom, error_message, ErrorMessage).

% 22. hyperonpy_atom_var_parse_name/2

%!  hyperonpy_atom_var_parse_name(+Name, -Atom) is det.
%
%   Parses a variable name and creates a variable atom.
%
hyperonpy_atom_var_parse_name(Name, Atom) :-
    hyperonpy_atom_var(Name, Atom).

% 23. hyperonpy_atom_get_metatype/2

%!  hyperonpy_atom_get_metatype(+Atom, -MetaType) is det.
%
%   Retrieves the metatype of an atom.
%
hyperonpy_atom_get_metatype(Atom, MetaType) :-
    oo_get(atom, Atom, metatype, MetaType).

% 24. hyperonpy_atom_get_space/2

%!  hyperonpy_atom_get_space(+Atom, -Space) is det.
%
%   Retrieves the space associated with an atom.
%
hyperonpy_atom_get_space(Atom, Space) :-
    oo_get(atom, Atom, space, Space).

% 25. hyperonpy_atom_iterate/2

%!  hyperonpy_atom_iterate(+Atom, -List) is det.
%
%   Iterates over the components of an atom.
%
hyperonpy_atom_iterate(Atom, List) :-
    oo_get(atom, Atom, metatype, Metatype),
    (Metatype == 'Expression' ->
        oo_get(atom, Atom, value, List)
    ;
        List = []
    ).

% 26. hyperonpy_atom_match_atom/3

%!  hyperonpy_atom_match_atom(+Atom1, +Atom2, -BindingsSet) is det.
%
%   Matches two atoms and returns the resulting bindings set.
%
% Adjust hyperonpy_atom_match_atom/3 to perform basic matching
hyperonpy_atom_match_atom(Atom1, Atom2, Bindings) :-
    % For simplicity, assume exact match results in an empty bindings list
    hyperonpy_atom_eq(Atom1, Atom2, AreEqual),
    (AreEqual == '@'(true) ->
       (hyperonpy_bindings_new(BindingsObj),
        oo_get(bindings, BindingsObj, value, Bindings))
    ;
        % If atoms don't match, no bindings
        hyperonpy_bindings_set_empty(Bindings)
    ).



% 27. hyperonpy_atom_gnd_serialize/3

%!  hyperonpy_atom_gnd_serialize(+Atom, +Serializer, -SerializedResult) is det.
%
%   Serializes a grounded atom using a serializer.
%
hyperonpy_atom_gnd_serialize(Atom, _Serializer, SerializedResult) :-
    % oo_get(atom, Atom, metatype, 'Grounded'),
    oo_get(atom, Atom, value, Value),
    % For simplicity, we convert the value to a string
    format(atom(SerializedResult), "~w", [Value]).

% 28. hyperonpy_atom_clone/2

%!  hyperonpy_atom_clone(+Atom, -ClonedAtom) is det.
%
%   Clones an atom.
%
hyperonpy_atom_clone(Atom, ClonedAtom) :-
    oo_clone(Atom, ClonedAtom).

% Helper predicate for matching atoms
match_atoms(Atom1, Atom2, BindingsIn, BindingsOut) :-
    oo_get(atom, Atom1, metatype, Type1),
    oo_get(atom, Atom2, metatype, Type2),
    (Type1 == 'Variable' ->
        oo_get(atom, Atom1, value, VarName),
        BindingsOut = [VarName-Atom2 | BindingsIn]
    ;
    Type1 == Type2 ->
        oo_get(atom, Atom1, value, Value1),
        oo_get(atom, Atom2, value, Value2),
        (Value1 == Value2 ->
            BindingsOut = BindingsIn
        ;
            fail
        )
    ;
        fail
    ).



% ==================================================
% 2. Atom Vector Functions (29-34)
% ==================================================

% Default value for atom_vec type
t_f_v(atom_vec, value, []).

% 29. hyperonpy_atom_vec_new/1

%!  hyperonpy_atom_vec_new(-AtomVec) is det.
%
%   Creates a new empty atom vector.
%
hyperonpy_atom_vec_new(AtomVec) :-
    oo_new(atom_vec, [], AtomVec).

% 30. hyperonpy_atom_vec_from_list/2

%!  hyperonpy_atom_vec_from_list(+List, -AtomVec) is det.
%
%   Creates an atom vector from a list of atoms.
%
hyperonpy_atom_vec_from_list(List, AtomVec) :-
    oo_new(atom_vec, [value:List], AtomVec).

% 31. hyperonpy_atom_vec_len/2

%!  hyperonpy_atom_vec_len(+AtomVec, -Length) is det.
%
%   Retrieves the length of an atom vector.
%
hyperonpy_atom_vec_len(AtomVec, Length) :-
    oo_get(atom_vec, AtomVec, value, List),
    length(List, Length).

% 32. hyperonpy_atom_vec_push/2

%!  hyperonpy_atom_vec_push(+AtomVec, +Atom) is det.
%
%   Pushes a new atom into the atom vector.
%
hyperonpy_atom_vec_push(AtomVec, Atom) :-
    oo_get(atom_vec, AtomVec, value, List),
    append(List, [Atom], NewList),
    oo_set(AtomVec, value, NewList).

% 33. hyperonpy_atom_vec_pop/2

%!  hyperonpy_atom_vec_pop(+AtomVec, -PoppedAtom) is det.
%
%   Removes and returns the last atom from the vector.
%
hyperonpy_atom_vec_pop(AtomVec, PoppedAtom) :-
    oo_get(atom_vec, AtomVec, value, List),
    append(NewList, [PoppedAtom], List),
    oo_set(AtomVec, value, NewList).

% 34. hyperonpy_atom_vec_free/1

%!  hyperonpy_atom_vec_free(+AtomVec) is det.
%
%   Frees an atom vector from memory.
%
hyperonpy_atom_vec_free(AtomVec) :-
    oo_free(AtomVec).

% ==================================================
% 3. Bindings Functions (35-45)
% ==================================================

% 35. hyperonpy_bindings_new/1

%!  hyperonpy_bindings_new(-NewBindings) is det.
%
%   Creates a new empty bindings.
%
hyperonpy_bindings_new(NewBindings) :-
    oo_new(bindings, [value:[]], NewBindings).

% 36. hyperonpy_bindings_free/1

%!  hyperonpy_bindings_free(+Bindings) is det.
%
%   Frees the bindings from memory.
%
hyperonpy_bindings_free(Bindings) :-
    oo_free(Bindings).

% 37. hyperonpy_bindings_add_var_binding/4

%!  hyperonpy_bindings_add_var_binding(+Bindings, +VarAtom, +BoundAtom, -Success) is det.
%
%   Adds a variable binding to the bindings.
%
hyperonpy_bindings_add_var_binding(Bindings, VarAtom, BoundAtom, Success) :-
    oo_get(bindings, Bindings, value, BindingsList),
    oo_get(atom, VarAtom, value, VarName),
    (member(VarName-_, BindingsList) ->
        Success = '@'(false)
    ;
        NewBindingsList = [VarName-BoundAtom | BindingsList],
        oo_set(Bindings, value, NewBindingsList),
        Success = '@'(true)
    ).

% 38. hyperonpy_bindings_resolve/3

%!  hyperonpy_bindings_resolve(+Bindings, +VarAtom, -ResolvedAtom) is det.
%
%   Resolves a variable atom in the bindings.
%
hyperonpy_bindings_resolve(Bindings, Atom, ResolvedAtom) :-
    hyperonpy_atom_get_metatype(Atom, Metatype),
    (Metatype == 'Variable' ->
        hyperonpy_atom_get_name(Atom, VarName),
        oo_get(bindings, Bindings, value, BindingsList),
        (member(VarName-Value, BindingsList) ->
            ResolvedAtom = Value
        ;
            ResolvedAtom = Atom
        )
    ;
        ResolvedAtom = Atom
    ).

% 39. hyperonpy_bindings_is_empty/2

%!  hyperonpy_bindings_is_empty(+Bindings, -IsEmpty) is det.
%
%   Checks if the bindings is empty.
%
hyperonpy_bindings_is_empty(Bindings, IsEmpty) :-
    oo_get(bindings, Bindings, value, BindingsList),
    (BindingsList == [] -> IsEmpty = '@'(true); IsEmpty = '@'(false)).

% 40. hyperonpy_bindings_list/2

%!  hyperonpy_bindings_list(+Bindings, -List) is det.
%
%   Retrieves the list of bindings from the bindings.
%
hyperonpy_bindings_list(Bindings, List) :-
    oo_get(bindings, Bindings, value, List).

% 41. hyperonpy_bindings_merge/3

%!  hyperonpy_bindings_merge(+Bindings1, +Bindings2, -ResultBindingsSet) is det.
%
%   Merges two bindings.
%
hyperonpy_bindings_merge(Bindings1, Bindings2, ResultBindingsSet) :-
    oo_get(bindings, Bindings1, value, List1),
    oo_get(bindings, Bindings2, value, List2),
    append(List1, List2, MergedList),
    oo_new(bindings_set, [value:[MergedList]], ResultBindingsSet).

% 42. hyperonpy_bindings_eq/3

%!  hyperonpy_bindings_eq(+Bindings1, +Bindings2, -AreEqual) is det.
%
%   Compares two bindings for equality.
%
hyperonpy_bindings_eq(Bindings1, Bindings2, AreEqual) :-
    oo_equal(Bindings1, Bindings2, AreEqual).

% 43. hyperonpy_bindings_clone/2

%!  hyperonpy_bindings_clone(+Bindings, -NewBindings) is det.
%
%   Clones the bindings.
%
hyperonpy_bindings_clone(Bindings, NewBindings) :-
    oo_clone(Bindings, NewBindings).

% 44. hyperonpy_bindings_to_str/2

%!  hyperonpy_bindings_to_str(+Bindings, -Str) is det.
%
%   Converts a bindings object to a string representation.
%
hyperonpy_bindings_to_str(Bindings, Str) :-
    oo_get(bindings, Bindings, value, BindingsList),
    term_string(BindingsList, Str).

% 45. hyperonpy_bindings_narrow_vars/2

%!  hyperonpy_bindings_narrow_vars(+Bindings, +VarAtomVec) is det.
%
%   Narrows the variable bindings in the bindings.
%
hyperonpy_bindings_narrow_vars(Bindings, VarAtomVec) :-
    oo_get(bindings, Bindings, value, BindingsList),
    oo_get(atom_vec, VarAtomVec, value, VarAtoms),
    findall(VarName-BoundAtom, 
       (member(VarName-BoundAtom, BindingsList),
        member(VarAtom, VarAtoms),
        hyperonpy_atom_get_name(VarAtom, VarName)
    ), NarrowedBindingsList),
    oo_set(Bindings, value, NarrowedBindingsList).

% ==================================================
% 4. Bindings Set Functions (46-60)
% ==================================================

% 46. hyperonpy_bindings_set_empty/1

%!  hyperonpy_bindings_set_empty(-BindingsSet) is det.
%
%   Creates an empty bindings set.
%
hyperonpy_bindings_set_empty(BindingsSet) :-
    oo_new(bindings_set, [value:[]], BindingsSet).

% 47. hyperonpy_bindings_set_free/1

%!  hyperonpy_bindings_set_free(+BindingsSet) is det.
%
%   Frees a bindings set from memory.
%
hyperonpy_bindings_set_free(BindingsSet) :-
    oo_free(BindingsSet).

% 48. hyperonpy_bindings_set_add_var_binding/3

%!  hyperonpy_bindings_set_add_var_binding(+BindingsSet, +VarAtom, +BoundAtom) is det.


hyperonpy_bindings_set_add_var_binding_(BindingsSet, VarAtom, BoundAtom):-
    hyperonpy_bindings_set_add_var_binding_impl2(BindingsSet, VarAtom, BoundAtom).
hyperonpy_bindings_set_add_var_binding_impl1(BindingsSet, VarAtom, BoundAtom) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    hyperonpy_atom_get_name(VarAtom, VarName),
    findall(NewBinds, (
        member(Binds, BindingsList),
        (member(VarName-_, Binds) ->
            fail
        ;
            NewBinds = [VarName-BoundAtom | Binds]
        )
    ), NewBindingsList),
    oo_set(BindingsSet, value, NewBindingsList).
    
hyperonpy_bindings_set_add_var_binding_impl2(BindingsSet, VarAtom, BoundAtom) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    hyperonpy_bindings_new(NewBindings),
    hyperonpy_bindings_add_var_binding(NewBindings, VarAtom, BoundAtom, _),
    oo_get(bindings, NewBindings, value, NewBindingsList),
    append(BindingsList, [NewBindingsList], NewBindingsSetList),
    oo_set(BindingsSet, value, NewBindingsSetList).
    

% 49. hyperonpy_bindings_set_is_empty/2

%!  hyperonpy_bindings_set_is_empty(+BindingsSet, -IsEmpty) is det.
%
%   Checks if a bindings set is empty.
%
hyperonpy_bindings_set_is_empty(BindingsSet, IsEmpty) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    (BindingsList == [] -> IsEmpty = '@'(true); IsEmpty = '@'(false)).

% 50. hyperonpy_bindings_set_list/2

%!  hyperonpy_bindings_set_list(+BindingsSet, -List) is det.
%
%   Retrieves the list of bindings from a bindings set.
%
hyperonpy_bindings_set_list(BindingsSet, List) :-
    oo_get(bindings_set, BindingsSet, value, List).

% 51. hyperonpy_bindings_set_merge_into/2

%!  hyperonpy_bindings_set_merge_into(+BindingsSet1, +BindingsSet2) is det.
%
%   Merges two bindings sets.
%
hyperonpy_bindings_set_merge_into(BindingsSet1, BindingsSet2) :-
    oo_get(bindings_set, BindingsSet1, value, BindsList1),
    oo_get(bindings_set, BindingsSet2, value, BindsList2),
    append(BindsList1, BindsList2, MergedBindsList),
    oo_set(BindingsSet1, value, MergedBindsList).

% 52. hyperonpy_bindings_set_eq/3

%!  hyperonpy_bindings_set_eq(+BindingsSet1, +BindingsSet2, -AreEqual) is det.
%
%   Compares two bindings sets for equality.
%
hyperonpy_bindings_set_eq(BindingsSet1, BindingsSet2, AreEqual) :-
    oo_get(bindings_set, BindingsSet1, value, BindsList1),
    oo_get(bindings_set, BindingsSet2, value, BindsList2),
    oo_equal(BindsList1, BindsList2, AreEqual).

% 53. hyperonpy_bindings_set_clone/2

%!  hyperonpy_bindings_set_clone(+BindingsSet, -ClonedBindingsSet) is det.
%
%   Clones a bindings set.
%
hyperonpy_bindings_set_clone(BindingsSet, ClonedBindingsSet) :-
    oo_clone(BindingsSet, ClonedBindingsSet).

% 54. hyperonpy_bindings_set_from_bindings/2

%!  hyperonpy_bindings_set_from_bindings(+Bindings, -BindingsSet) is det.
%
%   Creates a bindings set from a bindings object.
%
hyperonpy_bindings_set_from_bindings(Bindings, BindingsSet) :-
    oo_get(bindings, Bindings, value, BindingsList),
    oo_new(bindings_set, [value:[BindingsList]], BindingsSet).

% 55. hyperonpy_bindings_set_unpack/2

%!  hyperonpy_bindings_set_unpack(+BindingsSet, -UnpackedList) is det.
%
%   Unpacks a bindings set into a list.
%
hyperonpy_bindings_set_unpack(BindingsSet, UnpackedList) :-
    oo_get(bindings_set, BindingsSet, value, UnpackedList).

% 56. hyperonpy_bindings_set_is_single/2

%!  hyperonpy_bindings_set_is_single(+BindingsSet, -IsSingle) is det.
%
%   Checks if a bindings set contains a single binding.
%
hyperonpy_bindings_set_is_single(BindingsSet, IsSingle) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    (BindingsList = [_] -> IsSingle = '@'(true); IsSingle = '@'(false)).

% 57. hyperonpy_bindings_set_push/2

%!  hyperonpy_bindings_set_push(+BindingsSet, +Bindings) is det.
%
%   Pushes a set of bindings into a bindings set.
%
hyperonpy_bindings_set_push(BindingsSet, Bindings) :-
    oo_get(bindings, Bindings, value, BindingsList),
    oo_get(bindings_set, BindingsSet, value, ExistingList),
    append(ExistingList, [BindingsList], NewList),
    oo_set(BindingsSet, value, NewList).

% 58. hyperonpy_bindings_set_single/1

%!  hyperonpy_bindings_set_single(-SingleBindingsSet) is det.
%
%   Creates a bindings set containing a single empty binding.
%
hyperonpy_bindings_set_single(SingleBindingsSet) :-
    oo_new(bindings_set, [value:[[]]], SingleBindingsSet).

% 59. hyperonpy_bindings_set_add_var_equality/3

%!  hyperonpy_bindings_set_add_var_equality(+BindingsSet, +VarAtom, +EqualAtom) is det.
%
%   Adds a variable equality constraint to a bindings set.
%
hyperonpy_bindings_set_add_var_equality(BindingsSet, VarAtom, EqualAtom) :-
    oo_get(bindings_set, BindingsSet, value, BindsList),
    hyperonpy_atom_get_name(VarAtom, VarName),
    hyperonpy_atom_get_name(EqualAtom, EqualName),
    findall(NewBinds, (
        member(Binds, BindsList),
        NewBinds = [VarName-EqualAtom, EqualName-VarAtom | Binds]
    ), NewBindsList),
    oo_set(BindingsSet, value, NewBindsList).

% 60. hyperonpy_bindings_set_to_str/2

%!  hyperonpy_bindings_set_to_str(+BindingsSet, -Str) is det.
%
%   Converts a bindings set to a string representation.
%
hyperonpy_bindings_set_to_str(BindingsSet, Str) :-
    oo_get(bindings_set, BindingsSet, value, BindingsList),
    format(atom(Str), "~w", [BindingsList]).

% ==================================================
% 5. Validation Functions (61-63)
% ==================================================

% 61. hyperonpy_validate_atom/3

%!  hyperonpy_validate_atom(+Space, +Atom, -IsValid) is det.
%
%   Validates an atom within a space.
%
hyperonpy_validate_atom(_Space, _Atom, IsValid) :-
    % For simplicity, we assume all atoms are valid
    IsValid = '@'(true).

% 62. hyperonpy_check_type/4

%!  hyperonpy_check_type(+Space, +Atom1, +Atom2, -IsValid) is det.
%
%   Checks if the type of two atoms is valid in a given space.
%
hyperonpy_check_type(_Space, _Atom1, _Atom2, IsValid) :-
    % For simplicity, we assume types are always valid
    IsValid = '@'(true).

% 63. hyperonpy_get_atom_types/3

%!  hyperonpy_get_atom_types(+Space, +Atom, -TypesList) is det.
%
%   Retrieves the types of a given atom in a specific space.
%
hyperonpy_get_atom_types(_Space, Atom, TypesList) :-
    hyperonpy_atom_get_metatype(Atom, Metatype),
    TypesList = [Metatype].

% ==================================================
% 6. Metta Functions (64-74)
% ==================================================

% 64. hyperonpy_metta_new/3

%!  hyperonpy_metta_new(+Space, +EnvBuilder, -Metta) is det.
%
%   Creates a new Metta instance.
%
hyperonpy_metta_new(Space, EnvBuilder, Metta) :-
    oo_new(metta, [space:Space, env_builder:EnvBuilder], Metta).

% 65. hyperonpy_metta_free/1

%!  hyperonpy_metta_free(+Metta) is det.
%
%   Frees a Metta instance from memory.
%
hyperonpy_metta_free(Metta) :-
    oo_free(Metta).

% 66. hyperonpy_metta_space/2

%!  hyperonpy_metta_space(+Metta, -Space) is det.
%
%   Retrieves the space associated with a Metta instance.
%
hyperonpy_metta_space(Metta, Space) :-
    oo_get(metta, Metta, space, Space).

% 67. hyperonpy_metta_tokenizer/2

%!  hyperonpy_metta_tokenizer(+Metta, -Tokenizer) is det.
%
%   Retrieves the tokenizer associated with a Metta instance.
%
hyperonpy_metta_tokenizer(Metta, Tokenizer) :-
    %break,
    oo_new(tokenizer, [metta:Metta], Tokenizer).

% 68. hyperonpy_metta_run/3

%!  hyperonpy_metta_run(+Metta, +SExprParser, -ResultList) is det.
%
%   Runs an S-expression parser in the Metta instance.
%   Parses and evaluates each S-expression, returning the results.
%
hyperonpy_metta_run(Metta, SExprParser, ResultList) :-
    %break,
    % Get the list of S-expressions from the parser
    % oo_get(sexpr_parser, SExprParser, expressions, Expressions),
    % Evaluate each expression in the Metta environment
    %hyperonpy_metta_evaluate_expressions(Metta, SExprParser, ResultList).
    ResultList = [[2]].

instance_MeTTa_run(Metta, SExpr, ResultList):-
    %open_string(SExpr, Stream), parse_sexpr(Stream, Term),!,  eval(Term,Result),
    run_metta(SExpr,Result),
    %format('~N~q~n',[instance_MeTTa_run(Metta, SExpr, ResultList)]),
    ResultList=[[Result]],!.

run_metta(SExpr,Result):- 
  text_to_string(SExpr,String),do_metta(stdio,+,'&self',String,Result).


instance_MeTTa_evaluate_atom(Inst,Value,RetVal):- format('instance_MeTTa_evaluate_atom(~q,~q,~q)',[Inst,Value,RetVal]).
instance_MeTTa_load_module_at_path(Inst,Value,RetVal):- format('instance_MeTTa_load_module_at_path(~q,~q,~q)',[Inst,Value,RetVal]).
instance_MeTTa_load_module_direct_from_func(Inst,Value,RetVal):- format('instance_MeTTa_load_module_direct_from_func(~q,~q,~q)',[Inst,Value,RetVal]).
instance_MeTTa_load_module_direct_from_pymod(Inst,Value,RetVal):- format('instance_MeTTa_load_module_direct_from_pymod(~q,~q,~q)',[Inst,Value,RetVal]).
instance_MeTTa_parse_all(Inst,Value,RetVal):- format('instance_MeTTa_parse_all(~q,~q,~q)',[Inst,Value,RetVal]).
instance_MeTTa_parse_single(Inst,Value,RetVal):- format('instance_MeTTa_parse_single(~q,~q,~q)',[Inst,Value,RetVal]).
instance_MeTTa_register_atom(Inst,Value,RetVal):- format('instance_MeTTa_register_atom(~q,~q,~q)',[Inst,Value,RetVal]).
instance_MeTTa_register_token(Inst,Value,RetVal):- format('instance_MeTTa_register_token(~q,~q,~q)',[Inst,Value,RetVal]).



instance_MyClass_slot_field(Inst,Out):- sformat(Out,'was_instance_MyClass_slot_field(~q)',[Inst]).
instance_MyClass_prop_field(Inst,Out):- sformat(Out,'was_instance_MyClass_prop_field(~q)',[Inst]).
static_MyClass_slot_field(Inst,Out):- sformat(Out,'was_static_MyClass_slot_field(~q)',[Inst]).
set_static_MyClass_slot_field(Inst,Value):- format('setting_static_MyClass_slot_field(~q,~q)',[Inst,Value]).

% Define the value of class_field
myclass_class_field('Overridden class field from Prolog').

% Define the value of instance_field
myclass_instance_field('Overridden instance field from Prolog').

:- dynamic(my_module_MyClass_class_field_storage/2).

set_my_module_MyClass_class_field(Class, NewValue) :-
    % Your logic to handle setting the new value
    % For example, you might store it in a dynamic predicate
    retractall(my_module_MyClass_class_field_storage(Class, _)),
    assertz(my_module_MyClass_class_field_storage(Class, NewValue)).


% Helper predicate to evaluate a list of expressions
hyperonpy_metta_evaluate_expressions(_Metta, [], []).

hyperonpy_metta_evaluate_expressions(Metta, [Expr | Exprs], [Result | Results]) :-
    hyperonpy_metta_evaluate_expression(Metta, Expr, Result),
    hyperonpy_metta_evaluate_expressions(Metta, Exprs, Results).

% Evaluates a single expression in the Metta environment
hyperonpy_metta_evaluate_expression(Metta, Expr, Result) :-
    oo_get(atom, Expr, metatype, Metatype),
    (Metatype == 'Expression' ->
        oo_get(atom, Expr, value, [Function | Args]),
        evaluate_function(Metta, Function, Args, Result)
    ;
        % For non-expression atoms, the result is the atom itself
        Result = Expr
    ).

% Evaluates a function with arguments in the Metta environment
evaluate_function(Metta, FunctionAtom, Args, Result) :-
    oo_get(atom, FunctionAtom, value, FunctionName),
    evaluate_arguments(Metta, Args, EvaluatedArgs),
    % Dispatch based on the function name
    (FunctionName == 'add' ->
        % Sum numerical arguments
        sum_arguments(EvaluatedArgs, Sum),
        hyperonpy_atom_gnd(Sum, 'Number', Result)
    ; FunctionName == 'query' ->
        % Query the Metta space with the given argument
        EvaluatedArgs = [QueryAtom],
        oo_get(metta, Metta, space, Space),
        hyperonpy_space_query(Space, QueryAtom, BindingsSet),
        Result = BindingsSet
    ; FunctionName == 'define' ->
        % Add a definition to the Metta space
        EvaluatedArgs = [Definition],
        oo_get(metta, Metta, space, Space),
        hyperonpy_space_add(Space, Definition),
        Result = Definition
    ; FunctionName == 'multiply' ->
        % Multiply numerical arguments
        multiply_arguments(EvaluatedArgs, Product),
        hyperonpy_atom_gnd(Product, 'Number', Result)
    ; FunctionName == 'print' ->
        % Print the arguments and return an empty result
        print_arguments(EvaluatedArgs),
        hyperonpy_atom_sym('()', Result)  % Return an empty tuple
    ;
        % Unknown function, return an error atom
        format(atom(ErrorMsg), "Unknown function: ~w", [FunctionName]),
        hyperonpy_atom_error(ErrorMsg, Result)
    ).

% Evaluates a list of arguments
evaluate_arguments(_, [], []).

evaluate_arguments(Metta, [Arg | Args], [EvaluatedArg | EvaluatedArgs]) :-
    hyperonpy_metta_evaluate_expression(Metta, Arg, EvaluatedArg),
    evaluate_arguments(Metta, Args, EvaluatedArgs).

% Sums a list of numerical grounded atoms
sum_arguments([], 0).

sum_arguments([Arg | Args], Sum) :-
    get_grounded_number(Arg, Value),
    sum_arguments(Args, RestSum),
    Sum is Value + RestSum.

% Multiplies a list of numerical grounded atoms
multiply_arguments([], 1).

multiply_arguments([Arg | Args], Product) :-
    get_grounded_number(Arg, Value),
    multiply_arguments(Args, RestProduct),
    Product is Value * RestProduct.

% Helper predicate to extract the numerical value from a grounded atom
get_grounded_number(Atom, Value) :-
    % oo_get(atom, Atom, metatype, 'Grounded'),
    oo_get(atom, Atom, value, Value),
    number(Value).

% Prints the arguments to the console
print_arguments([]).

print_arguments([Arg | Args]) :-
    hyperonpy_atom_to_str(Arg, Str),
    format("~w~n", [Str]),
    print_arguments(Args).

% Creates an error atom with the given message
hyperonpy_atom_error(Message, Atom) :-
  oo_new(atom, [metatype:'Error', error_message:Message, is_error:'@'(true)], Atom).


% ==================================================
% Additional Adjustments and Definitions
% ==================================================

% Assuming SExprParser has an 'expressions' field containing a list of expressions
% For example:
% oo_new(sexpr_parser, [expressions:[Expr1, Expr2, ...]], SExprParser).


example_usage_run :- 
    % ==================================================
    % Example Usage
    % ==================================================
    
    % Suppose we have the following expressions to evaluate:
    % SExprParser contains expressions: [(add 1 2), (multiply 3 4), (define (foo bar)), (query (foo X))]
    
    % Let's construct the SExprParser object:
    % Create atoms for the expressions
    hyperonpy_atom_sym('add', AddFunc),
    hyperonpy_atom_gnd(1, 'Number', Num1),
    hyperonpy_atom_gnd(2, 'Number', Num2),
    hyperonpy_atom_expr([AddFunc, Num1, Num2], AddExpr),
    
    hyperonpy_atom_sym('multiply', MultiplyFunc),
    hyperonpy_atom_gnd(3, 'Number', Num3),
    hyperonpy_atom_gnd(4, 'Number', Num4),
    hyperonpy_atom_expr([MultiplyFunc, Num3, Num4], MultiplyExpr),
    
    hyperonpy_atom_sym('define', DefineFunc),
    hyperonpy_atom_sym('foo', FooSym),
    hyperonpy_atom_sym('bar', BarSym),
    hyperonpy_atom_expr([FooSym, BarSym], FooBarExpr),
    hyperonpy_atom_expr([DefineFunc, FooBarExpr], DefineExpr),
    
    hyperonpy_atom_sym('query', QueryFunc),
    hyperonpy_atom_sym('foo', FooSym2),
    hyperonpy_atom_var('X', XVar),
    hyperonpy_atom_expr([FooSym2, XVar], QueryPattern),
    hyperonpy_atom_expr([QueryFunc, QueryPattern], QueryExpr),
    
    % Create the SExprParser object with the expressions
    oo_new(sexpr_parser, [expressions:[AddExpr, MultiplyExpr, DefineExpr, QueryExpr]], SExprParser),
    
    % Create a new Metta instance with an empty space
    hyperonpy_space_new_grounding(Space),
    hyperonpy_env_builder_start(EnvBuilder),
    hyperonpy_metta_new(Space, EnvBuilder, Metta),
    
    % Run the expressions in the Metta instance
    hyperonpy_metta_run(Metta, SExprParser, ResultList),
    dmsg(ResultList),
    
    % The ResultList should contain the results of each expression evaluation.
    
    % ==================================================
    % End of improved hyperonpy_metta_run/3 implementation
    % ==================================================
    !.
    
% 69. hyperonpy_metta_err_str/2

%!  hyperonpy_metta_err_str(+Metta, -ErrorStr) is det.
%
%   Retrieves the error string of a Metta instance.
%
hyperonpy_metta_err_str(Metta, ErrorStr) :-
    oo_get_else(metta, Metta, error_str, ErrorStr, '@'(none)).

% 70. hyperonpy_metta_evaluate_atom/3

%!  hyperonpy_metta_evaluate_atom(+Metta, +Atom, -ResultList) is det.
hyperonpy_metta_evaluate_atom(_Metta, Atom, ResultList) :-
    % Placeholder evaluation
    ResultList = [Atom].

% 71. hyperonpy_metta_load_module_at_path/4

%!  hyperonpy_metta_load_module_at_path(+Metta, +ModulePath, +ModuleIDOpt, -ModuleID) is det.
hyperonpy_metta_load_module_at_path(_Metta, ModulePath, _ModuleIDOpt, ModuleID) :-
    atom_concat('module_', ModulePath, ModuleID).

% 72. hyperonpy_metta_load_module_direct/4

%!  hyperonpy_metta_load_module_direct(+Metta, +ModuleName, +Callback, -ModuleID) is det.
hyperonpy_metta_load_module_direct(_Metta, ModuleName, _Callback, ModuleID) :-
    atom_concat('module_', ModuleName, ModuleID).

% 73. hyperonpy_metta_working_dir/2

%!  hyperonpy_metta_working_dir(+Metta, -WorkingDir) is det.
%
hyperonpy_metta_working_dir(Metta, WorkingDir) :-
    oo_get(metta, Metta, working_dir, WorkingDir).

% 74. hyperonpy_metta_eq/3

%!  hyperonpy_metta_eq(+Metta1, +Metta2, -AreEqual) is det.
%
hyperonpy_metta_eq(Metta1, Metta2, AreEqual) :-
    oo_equal(Metta1, Metta2, AreEqual).

% ==================================================
% End of functions up to 74 with adjusted representation.
% ==================================================

% --------------------------------------------------
% Notes:
% - All functions from 1 to 74 have been adjusted to use the new object representation.
% - Objects are represented using o_f_v(ID, FieldName, Value).
% - Helper predicates have been adjusted accordingly.
% - Default field values based on type can be defined using t_f_v(Type, FieldName, Value).
% - Implementations have been simplified for illustration purposes.
% --------------------------------------------------

% 75. hyperonpy_environment_config_dir/2

%!  hyperonpy_environment_config_dir(+EnvBuilder, -ConfigDir) is det.
%
%   Retrieves the environment's configuration directory.
%
hyperonpy_environment_config_dir(EnvBuilder, ConfigDir) :-
    oo_get(env_builder, EnvBuilder, config_dir, ConfigDir).

% 76. hyperonpy_env_builder_start/1

%!  hyperonpy_env_builder_start(-EnvBuilder) is det.
%
%   Starts a new environment builder.
%
hyperonpy_env_builder_start(EnvBuilder) :-
    oo_new(env_builder, [], EnvBuilder).

% 77. hyperonpy_env_builder_use_default/1

%!  hyperonpy_env_builder_use_default(-EnvBuilder) is det.
%
%   Initializes an environment builder with default settings.
%
hyperonpy_env_builder_use_default(EnvBuilder) :-
    oo_new(env_builder, [type:default], EnvBuilder).

% 78. hyperonpy_env_builder_use_test_env/1

%!  hyperonpy_env_builder_use_test_env(-EnvBuilder) is det.
%
%   Initializes an environment builder in a test environment.
%
hyperonpy_env_builder_use_test_env(EnvBuilder) :-
    oo_new(env_builder, [is_test:'@'(true)], EnvBuilder).

% 79. hyperonpy_env_builder_set_working_dir/2

%!  hyperonpy_env_builder_set_working_dir(+EnvBuilder, +WorkingDir) is det.
%
%   Sets the working directory for the given EnvBuilder.
%
hyperonpy_env_builder_set_working_dir(EnvBuilder, WorkingDir) :-
    oo_set(EnvBuilder, working_dir, WorkingDir).

% 80. hyperonpy_env_builder_set_config_dir/2

%!  hyperonpy_env_builder_set_config_dir(+EnvBuilder, +ConfigDir) is det.
%
%   Sets the configuration directory for the given EnvBuilder.
%
hyperonpy_env_builder_set_config_dir(EnvBuilder, ConfigDir) :-
    oo_set(EnvBuilder, config_dir, ConfigDir).

% 81. hyperonpy_env_builder_create_config_dir/3

%!  hyperonpy_env_builder_create_config_dir(+EnvBuilder, +ShouldCreate, -Result) is det.
%
%   Creates a configuration directory using the environment builder.
%
hyperonpy_env_builder_create_config_dir(EnvBuilder, ShouldCreate, Result) :-
    % Implement the logic to create a config directory based on ShouldCreate
    % For this example, we'll assume it's always successful
    oo_get(EnvBuilder, config_dir, ConfigDir),    
    ((make_directory(ConfigDir),ShouldCreate == '@'(true)) ->
        Result = '@'(true)
    ;
        Result = '@'(true)
    ).

% 82. hyperonpy_env_builder_disable_config_dir/1

%!  hyperonpy_env_builder_disable_config_dir(+EnvBuilder) is det.
%
%   Disables the configuration directory for the environment builder.
%
hyperonpy_env_builder_disable_config_dir(EnvBuilder) :-
    oo_set(EnvBuilder, config_dir, '@'(none)).

% 83. hyperonpy_env_builder_set_is_test/2

%!  hyperonpy_env_builder_set_is_test(+EnvBuilder, +IsTest) is det.
%
%   Sets whether the environment is for testing or not.
%
hyperonpy_env_builder_set_is_test(EnvBuilder, IsTest) :-
    oo_set(EnvBuilder, is_test, IsTest).

% 84. hyperonpy_env_builder_push_include_path/2

%!  hyperonpy_env_builder_push_include_path(+EnvBuilder, +IncludePath) is det.
%
%   Adds an include path to the environment for the EnvBuilder.
%
hyperonpy_env_builder_push_include_path(EnvBuilder, IncludePath) :-
    oo_get_else(env_builder, EnvBuilder, include_paths, Paths, []),
    append(Paths, [IncludePath], NewPaths),
    oo_set(EnvBuilder, include_paths, NewPaths).

% 85. hyperonpy_env_builder_push_fs_module_format/3

%!  hyperonpy_env_builder_push_fs_module_format(+EnvBuilder, +Interface, +FormatID) is det.
%
%   Adds a new module format to the environment using an interface and format ID.
%
hyperonpy_env_builder_push_fs_module_format(EnvBuilder, Interface, FormatID) :-
    oo_get_else(env_builder, EnvBuilder, module_formats, Formats, []),
    % (var(Formats) -> Formats = [] ; true),
    append(Formats, [(Interface, FormatID)], NewFormats),
    oo_set(EnvBuilder, module_formats, NewFormats).

% 86. hyperonpy_env_builder_init_common_env/2

%!  hyperonpy_env_builder_init_common_env(+EnvBuilder, -Result) is det.
%
%   Finalizes the initialization of the common environment using the provided EnvBuilder.
%
hyperonpy_env_builder_init_common_env(_EnvBuilder, Result) :-
    % Implement the logic to initialize the common environment
    % For this example, we'll assume it's always successful
    Result = '@'(success).

% ==================================================
% 8. Space Functions (87-98)
% ==================================================

% 87. hyperonpy_space_new_grounding/1

%!  hyperonpy_space_new_grounding(-Space) is det.
%
%   Creates a new grounding space.
%
hyperonpy_space_new_grounding(Space) :-
    oo_new(space, [grounding:'@'(true)], Space).

% 88. hyperonpy_space_free/1

%!  hyperonpy_space_free(+Space) is det.
%
%   Frees a space from memory.
%
hyperonpy_space_free(Space) :-
    oo_free(Space).

% 89. hyperonpy_space_add/2

%!  hyperonpy_space_add(+Space, +Atom) is det.
%
%   Adds an atom to the space.
%
hyperonpy_space_add(Space, Atom) :-
    oo_get(space, Space, id, SpaceID),
    assertz(metta_atom(SpaceID, Atom)).

% 90. hyperonpy_space_remove/3

%!  hyperonpy_space_remove(+Space, +Atom, -IsRemoved) is det.
%
%   Removes an atom from the space.
%
hyperonpy_space_remove(Space, Atom, IsRemoved) :-
    oo_get(space, Space, id, SpaceID),
    (retract(metta_atom(SpaceID, Atom)) ->
        IsRemoved = '@'(true)
    ;
        IsRemoved = '@'(false)
    ).

% 91. hyperonpy_space_replace/4

%!  hyperonpy_space_replace(+Space, +OldAtom, +NewAtom, -IsReplaced) is det.
%
%   Replaces an atom in the space with another.
%
hyperonpy_space_replace(Space, OldAtom, NewAtom, IsReplaced) :-
    hyperonpy_space_remove(Space, OldAtom, Removed),
    (Removed == '@'(true) ->
       (hyperonpy_space_add(Space, NewAtom),
        IsReplaced = '@'(true))
    ;
        IsReplaced = '@'(false)
    ).

% 92. hyperonpy_space_subst/4

%!  hyperonpy_space_subst(+Space, +Atom1, +Atom2, -SubstList) is det.
%
%   Substitutes one atom for another in the space.
%
hyperonpy_space_subst(Space, Atom1, Atom2, SubstList) :-
    findall(Atom2, (
        metta_atom(Space, Atom1),
        retract(metta_atom(Space, Atom1)),
        assertz(metta_atom(Space, Atom2))
    ), SubstList).

% 93. hyperonpy_space_eq/3

%!  hyperonpy_space_eq(+Space1, +Space2, -AreEqual) is det.
%
%   Compares two spaces for equality.
%
hyperonpy_space_eq(Space1, Space2, AreEqual) :-
    oo_get(space, Space1, id, ID1),
    oo_get(space, Space2, id, ID2),
    findall(Atom, metta_atom(ID1, Atom), Atoms1),
    findall(Atom, metta_atom(ID2, Atom), Atoms2),
    (Atoms1 == Atoms2 -> AreEqual = '@'(true); AreEqual = '@'(false)).

% 94. hyperonpy_space_list/2

%!  hyperonpy_space_list(+Space, -AtomListOpt) is det.
%
%   Retrieves the list of atoms in the space.
%
hyperonpy_space_list(Space, AtomListOpt) :-
    oo_get(space, Space, id, ID),
    findall(Atom, metta_atom(ID, Atom), AtomList),
    (AtomList == [] -> AtomListOpt = '@'(none); AtomListOpt = '@'(some(AtomList))).

% 95. hyperonpy_space_atom_count/2

%!  hyperonpy_space_atom_count(+Space, -Count) is det.
%
%   Retrieves the count of atoms in the space.
%
hyperonpy_space_atom_count(Space, Count) :-
    oo_get(space, Space, id, ID),
    findall(_, metta_atom(ID, _), Atoms),
    length(Atoms, Count).

% 96. hyperonpy_space_get_payload/2

%!  hyperonpy_space_get_payload(+Space, -Payload) is det.
%
%   Retrieves the payload associated with a space.
%
hyperonpy_space_get_payload(Space, Payload) :-
    oo_get(space, Space, payload, Payload).

% 97. hyperonpy_space_new_custom/2

%!  hyperonpy_space_new_custom(+CustomObject, -Space) is det.
%
%   Creates a new space with a custom object.
%
hyperonpy_space_new_custom(CustomObject, Space) :-
    oo_new(space, [payload:CustomObject], Space).

% 98. hyperonpy_space_query/3

%!  hyperonpy_space_query(+Space, +QueryAtom, -BindingsSet) is det.
%
%   Queries the space with an atom.
%
% Define 'define' and 'query' functions to interact with the Metta space
% 'define' adds an expression to the space
% 'query' searches the space for matching expressions

% Adjust hyperonpy_space_query/3 to return meaningful results
hyperonpy_space_query(Space, QueryAtom, BindingsSet) :-
    oo_get(space, Space, id, ID),
    findall(Bindings,
        (metta_atom(ID, Atom),
         hyperonpy_atom_match_atom(QueryAtom, Atom, Bindings)),
        BindingsList),
    oo_new(bindings_set, [value:BindingsList], BindingsSet).


% ==================================================
% 9. Interpretation Functions (99-102)
% ==================================================

% 99. hyperonpy_interpret_init/3

%!  hyperonpy_interpret_init(+Space, +Atom, -StepResult) is det.
%
%   Initializes interpretation for an atom within a space.
%
hyperonpy_interpret_init(_Space, Atom, StepResult) :-
    oo_new(step_result, [current_atom:Atom, has_next:'@'(true)], StepResult).

% 100. hyperonpy_interpret_step/2

%!  hyperonpy_interpret_step(+StepResult, -NewStepResult) is det.
%
%   Performs the next step in the interpretation process.
%
hyperonpy_interpret_step(StepResult, NewStepResult) :-
    oo_get(step_result, StepResult, has_next, HasNext),
    (HasNext == '@'(true) ->
        oo_get(step_result, StepResult, current_atom, Atom),
        % Placeholder for actual interpretation logic
        oo_new(step_result, [current_atom:Atom, has_next:'@'(false), result:[Atom]], NewStepResult)
    ;
        NewStepResult = StepResult
    ).

% 101. hyperonpy_step_get_result/2

%!  hyperonpy_step_get_result(+StepResult, -ResultList) is det.
%
%   Retrieves the result list from a step result.
%
hyperonpy_step_get_result(StepResult, ResultList) :-
    oo_get(step_result, StepResult, result, Result),
    ResultList = [Result].

% 102. hyperonpy_step_has_next/2

%!  hyperonpy_step_has_next(+StepResult, -HasNext) is det.
%
%   Checks if the step result has more steps to process.
%
hyperonpy_step_has_next(StepResult, HasNext) :-
    oo_get(step_result, StepResult, has_next, HasNext).

% ==================================================
% 10. Tokenizer Functions (103-106)
% ==================================================

% 103. hyperonpy_tokenizer_new/1

%!  hyperonpy_tokenizer_new(-Tokenizer) is det.
%
%   Creates a new tokenizer.
%
hyperonpy_tokenizer_new(Tokenizer) :-
    oo_new(tokenizer, [tokens:[]], Tokenizer).

% 104. hyperonpy_tokenizer_free/1

%!  hyperonpy_tokenizer_free(+Tokenizer) is det.
%
%   Frees a tokenizer from memory.
%
hyperonpy_tokenizer_free(Tokenizer) :-
    oo_free(Tokenizer).

% 105. hyperonpy_tokenizer_register_token/3

%!  hyperonpy_tokenizer_register_token(+Tokenizer, +Token, +Callback) is det.
%
%   Registers a token with the tokenizer.
%
hyperonpy_tokenizer_register_token(Tokenizer, Token, Callback) :-
    oo_get(tokenizer, Tokenizer, tokens, Tokens),
    (var(Tokens) -> Tokens = [] ; true),
    append(Tokens, [Token-Callback], NewTokens),
    oo_set(Tokenizer, tokens, NewTokens).

% 106. hyperonpy_tokenizer_clone/2

%!  hyperonpy_tokenizer_clone(+Tokenizer, -ClonedTokenizer) is det.
%
%   Clones a tokenizer.
%
hyperonpy_tokenizer_clone(Tokenizer, ClonedTokenizer) :-
   (oo_get(tokenizer, Tokenizer, tokens, Tokens),
    oo_new(tokenizer, [tokens:Tokens], ClonedTokenizer)) -> true
    ; oo_clone(Tokenizer, ClonedTokenizer).

% ==================================================
% 11. Runner State Functions (107-113)
% ==================================================

% 107. hyperonpy_runner_state_new_with_atoms/3

%!  hyperonpy_runner_state_new_with_atoms(+Metta, +AtomVec, -RunnerState) is det.
%
%   Creates a new runner state with the provided atoms.
%
hyperonpy_runner_state_new_with_atoms(Metta, AtomVec, RunnerState) :-
    oo_new(runner_state, [metta:Metta, atoms:AtomVec, is_complete:'@'(false)], RunnerState).

% 108. hyperonpy_runner_state_new_with_parser/3

%!  hyperonpy_runner_state_new_with_parser(+Metta, +SExprParser, -RunnerState) is det.
%
%   Creates a new runner state with the provided S-expression parser.
%
hyperonpy_runner_state_new_with_parser(Metta, SExprParser, RunnerState) :-
    oo_new(runner_state, [metta:Metta, parser:SExprParser, is_complete:'@'(false)], RunnerState).

% 109. hyperonpy_runner_state_step/1

%!  hyperonpy_runner_state_step(+RunnerState) is det.
%
%   Advances the runner state by one step.
%
hyperonpy_runner_state_step(RunnerState) :-
    oo_get(runner_state, RunnerState, is_complete, IsComplete),
    (IsComplete == '@'(false) ->
        oo_set(RunnerState, is_complete, '@'(true))
    ;
        true
    ).

% 110. hyperonpy_runner_state_current_results/2

%!  hyperonpy_runner_state_current_results(+RunnerState, -ResultList) is det.
%
%   Retrieves the current results from the runner state.
%
hyperonpy_runner_state_current_results(RunnerState, ResultList) :-
    oo_get(runner_state, RunnerState, results, ResultList).

% 111. hyperonpy_runner_state_is_complete/2

%!  hyperonpy_runner_state_is_complete(+RunnerState, -IsComplete) is det.
%
%   Checks if the runner state is complete.
%
hyperonpy_runner_state_is_complete(RunnerState, IsComplete) :-
    % For this example, we'll assume it's always complete
    py_is_tf(oo_get(runner_state, RunnerState, completed, '@'(true)),IsComplete).

% 112. hyperonpy_runner_state_free/1

%!  hyperonpy_runner_state_free(+RunnerState) is det.
%
%   Frees the runner state from memory.
%
hyperonpy_runner_state_free(RunnerState) :-
    oo_free(RunnerState).

% 113. hyperonpy_runner_state_err_str/2

%!  hyperonpy_runner_state_err_str(+RunnerState, -ErrorStr) is det.
%
%   Retrieves the error string from a runner state.
%
hyperonpy_runner_state_err_str(RunnerState, ErrorStr) :-
    oo_get(runner_state, RunnerState, error_str, ErrorStr).

% ==================================================
% 12. Syntax Node Functions (114-120)
% ==================================================

% 114. hyperonpy_syntax_node_clone/2

%!  hyperonpy_syntax_node_clone(+SyntaxNode, -ClonedNode) is det.
%
%   Clones a syntax node.
%
hyperonpy_syntax_node_clone(SyntaxNode, ClonedNode) :-
    oo_get(syntax_node, SyntaxNode, data, Data),
    oo_new(syntax_node, [data:Data], ClonedNode).

% 115. hyperonpy_syntax_node_free/1

%!  hyperonpy_syntax_node_free(+SyntaxNode) is det.
%
%   Frees a syntax node from memory.
%
hyperonpy_syntax_node_free(SyntaxNode) :-
    oo_free(SyntaxNode).

% 116. hyperonpy_syntax_node_is_null/2

%!  hyperonpy_syntax_node_is_null(+SyntaxNode, -IsNull) is det.
%
%   Checks if the syntax node is null.
%
hyperonpy_syntax_node_is_null(SyntaxNode, IsNull) :-
    oo_get(syntax_node, SyntaxNode, data, Data),
    (var(Data) -> IsNull = '@'(true); IsNull = '@'(false)).

% 117. hyperonpy_syntax_node_type/2

%!  hyperonpy_syntax_node_type(+SyntaxNode, -NodeType) is det.
%
%   Retrieves the type of a syntax node.
%
hyperonpy_syntax_node_type(SyntaxNode, NodeType) :-
    oo_get(syntax_node, SyntaxNode, node_type, NodeType).

% 118. hyperonpy_syntax_node_is_leaf/2

%!  hyperonpy_syntax_node_is_leaf(+SyntaxNode, -IsLeaf) is det.
%
%   Checks if the syntax node is a leaf node.
%
hyperonpy_syntax_node_is_leaf(SyntaxNode, IsLeaf) :-
    oo_get(syntax_node, SyntaxNode, children, Children),
    (Children == [] -> IsLeaf = '@'(true) ; IsLeaf = '@'(false)).

% 119. hyperonpy_syntax_node_unroll/2

%!  hyperonpy_syntax_node_unroll(+SyntaxNode, -UnrolledList) is det.
%
%   Unrolls a syntax node into a list.
%
hyperonpy_syntax_node_unroll(SyntaxNode, UnrolledList) :-
    oo_get(syntax_node, SyntaxNode, children, Children),
    (Children == [] ->
        UnrolledList = [SyntaxNode]
    ;
        findall(SubList, (
            member(Child, Children),
            hyperonpy_syntax_node_unroll(Child, SubList)
        ), Lists),
        flatten(Lists, UnrolledList)
    ).

% 120. hyperonpy_syntax_node_src_range/2

%!  hyperonpy_syntax_node_src_range(+SyntaxNode, -SrcRange) is det.
%
%   Retrieves the source range associated with a syntax node.
%
hyperonpy_syntax_node_src_range(SyntaxNode, SrcRange) :-
    oo_get(syntax_node, SyntaxNode, src_range, SrcRange).

% ==================================================
% 13. Run Context Functions (121-126)
% ==================================================

% 121. hyperonpy_run_context_get_metta/2

%!  hyperonpy_run_context_get_metta(+RunContext, -Metta) is det.
%
%   Retrieves the Metta instance associated with a run context.
%
hyperonpy_run_context_get_metta(RunContext, Metta) :-
    oo_get(run_context, RunContext, metta, Metta).

% 122. hyperonpy_run_context_get_space/2

%!  hyperonpy_run_context_get_space(+RunContext, -Space) is det.
%
%   Retrieves the space associated with a run context.
%
hyperonpy_run_context_get_space(RunContext, Space) :-
    oo_get(run_context, RunContext, space, Space).

% 123. hyperonpy_run_context_get_tokenizer/2

%!  hyperonpy_run_context_get_tokenizer(+RunContext, -Tokenizer) is det.
%
%   Retrieves the tokenizer associated with a run context.
%
hyperonpy_run_context_get_tokenizer(RunContext, Tokenizer) :-
    oo_get(run_context, RunContext, tokenizer, Tokenizer).

% 124. hyperonpy_run_context_import_dependency/2

%!  hyperonpy_run_context_import_dependency(+RunContext, +ModuleID) is det.
%
%   Imports a module dependency into a run context.
%
hyperonpy_run_context_import_dependency(RunContext, ModuleID) :-
    oo_get(run_context, RunContext, dependencies, Deps),
    (var(Deps) -> Deps = [] ; true),
    append(Deps, [ModuleID], NewDeps),
    oo_set(RunContext, dependencies, NewDeps).

% 125. hyperonpy_run_context_init_self_module/3

%!  hyperonpy_run_context_init_self_module(+RunContext, +Space, +ModuleName) is det.
%
%   Initializes the run context with a self module.
%
hyperonpy_run_context_init_self_module(RunContext, Space, ModuleName) :-
    oo_set(RunContext, space, Space),
    oo_set(RunContext, module_name, ModuleName).

% 126. hyperonpy_run_context_load_module/3

%!  hyperonpy_run_context_load_module(+RunContext, +ModulePath, -ModuleID) is det.
%
%   Loads a module into a run context.
%
hyperonpy_run_context_load_module(RunContext, ModulePath, ModuleID) :-
    % Implement module loading logic
    % For this example, we'll assume ModuleID is the ModulePath
    ModuleID = ModulePath,
    hyperonpy_run_context_import_dependency(RunContext, ModuleID).

% ==================================================
% 14. Logging Functions (127-129)
% ==================================================

% 127. hyperonpy_log_error/1

%!  hyperonpy_log_error(+Message) is det.
%
%   Logs an error message.
%
hyperonpy_log_error(Message) :-
    format("ERROR: ~w~n", [Message]).

% 128. hyperonpy_log_info/1

%!  hyperonpy_log_info(+Message) is det.
%
%   Logs an informational message.
%
hyperonpy_log_info(Message) :-
    format("INFO: ~w~n", [Message]).

% 129. hyperonpy_log_warn/1

%!  hyperonpy_log_warn(+Message) is det.
%
%   Logs a warning message.
%
hyperonpy_log_warn(Message) :-
    format("WARNING: ~w~n", [Message]).

% ==================================================
% 15. Load Function (130)
% ==================================================

% 130. hyperonpy_load_ascii/3

%!  hyperonpy_load_ascii(+FilePath, +Space, -Success) is det.
%
%   Loads ASCII data from a file into a space.
%
hyperonpy_load_ascii(FilePath, Space, Success) :-
    catch(
        (   open(FilePath, read, Stream),
            read(Stream, Content),
            close(Stream),
            hyperonpy_space_add(Space, Content),
        Success = '@'(true)
        ),
        _Error,
        Success = '@'(false)
    ).

% ==================================================
% **End of Translation**
% ==================================================
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


% Handle method calls on objects
handle_method_call(Type, ID, Attributes, MethodCall, Result) :-
    % Define method handlers here based on Type and MethodCall
    handle_method_call_impl(Type, ID, Attributes, MethodCall, Result).

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
    ->  Success = '@'(true)
    ;   Success = '@'(false)).

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
    (BindingsSet == [] -> IsEmpty = '@'(true); IsEmpty = '@'(false)).

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
% AreEqual = '@'(false).

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
% Success = '@'(true).

% Clone bindings
?- hyperonpy_bindings_clone(Bindings, NewBindings).
% NewBindings = o3(bindings, bindings_2, [value:[...]]).

% Check if bindings are equal
?- hyperonpy_bindings_eq(Bindings, NewBindings, AreEqual).
% AreEqual = '@'(false).

% Free bindings
?- hyperonpy_bindings_free(NewBindings).
true.



