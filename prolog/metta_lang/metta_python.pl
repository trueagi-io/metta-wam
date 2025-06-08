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

%*********************************************************************************************
% PROGRAM FUNCTION: integrates Prolog with Python and Rust environments to execute, manage, and
% query logical operations across different spaces, handling Python exceptions, module loading,
% and providing utilities for converting between Prolog terms and Python/Rust objects.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Uncomment this when loading from non user context like for ecapsulation
%:- '$set_source_module'('user').

% Set the depth of Python backtrace to 10 for debugging Python errors in Prolog.
:- initialization(set_prolog_flag(py_backtrace_depth,10)).

% Enable Python backtracing in case of errors when calling Python from Prolog.
:- initialization(set_prolog_flag(py_backtrace,true)).

% Set an empty list for Python argument values.
:- initialization(set_prolog_flag(py_argv,[])).

/*
Core in Rust:
   The core logic of the MeTTa system is implemented in Rust for its performance
   and safety, making it robust and efficient.

Python Extensions:
   Python is integrated via FFI (Foreign Function Interface) to allow customization
   and interaction with Rust code. Python's flexibility and ecosystem provide easy
   extensibility to the system.

Prolog Extensions with Python:
   Just like Rust allows Python extensions, Prolog also supports the extension of
   functionality through Python (and Rust via Python). This allows Python and Rust
   developers to continue working with the system easily.
*/
 :- use_module(library(filesex)).

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
 :- ensure_loaded(metta_interp).

%!  janus_initialization is det.
%
%   Ensures that the Janus Prolog module is loaded. This predicate attempts to load
%   the Janus module from its known location. If the Janus module is already loaded,
%   it reloads it to ensure all changes are included. If it cannot find the Janus module
%   in the default location, it looks for it in a specific fallback path.
%
%   @example
%     % If Janus is already loaded:
%     ?- janus_initialization.
%     true.
%
%     % If Janus is not found:
%     ?- janus_initialization.
%     false.
%
 :- module_property(janus,file(_)) -> true; janus:ensure_loaded(library(janus)).


py_call_warg(G):- py_c_c(G,GG), py_call(GG).
py_call_warg(G,A):- py_c_c(G,GG), py_call(GG,A).
py_call_warg(G,A,Opts):- py_c_c(G,GG), py_call(GG,A,Opts).

py_c_c(G,GG):- \+ compound(G),!,GG=G.
py_c_c(M:G,M:GG):- !, py_c_c(G,GG).
py_c_c(identity(X),identity(X)):-!.
py_c_c(G,GG):- is_list(G), maplist(py_arg,G,GG),!.
py_c_c(G,GG):- compound_name_arguments(G,F,A), maplist(py_arg,A,AA), !, compound_name_arguments(GG,F,AA).


% Declare the multifile and dynamic predicates for determining the type of logical space.
% These predicates are used to determine whether the given space is Python-based or not.
:- multifile(is_python_space/1).
:- dynamic(is_python_space/1).
:- volatile(is_python_space/1).

%!  is_rust_space(+GSpace) is semidet.
%
%   Checks if the given space is a Rust-based space by checking if it is marked as a
%   Python space. This allows Rust and Python spaces to be treated similarly in certain
%   contexts.
%
%   @arg GSpace The space to be checked.
%
is_rust_space(GSpace):- is_python_space(GSpace).

%!  is_not_prolog_space(+GSpace) is semidet.
%
%   Determines if the given space is not a Prolog-based space. This predicate succeeds
%   if the space is a Rust space or if it does not belong to the Prolog asserted or
%   non-backtrackable (nb) spaces.
%
%   @arg GSpace The space to be checked.
%
is_not_prolog_space(GSpace):-
    % Check if it is a Rust space (which includes Python spaces).
    is_rust_space(GSpace),!.
is_not_prolog_space(GSpace):- fail,
    % Check if the space is neither an asserted space nor an nb space.
    \+ is_asserted_space(GSpace), \+ is_nb_space(GSpace),!.

%!  with_safe_argv(:Goal) is det.
%
%   Executes the given Goal with an empty `argv` Prolog flag, ensuring that the `argv`
%   flag is restored to its original state after the Goal has been executed. This is
%   useful when interacting with Python from Prolog, as Python may expect specific
%   arguments in `argv`.
%
%   @arg Goal The goal to be executed with an empty `argv`.
%
%   @example
%     % Execute a goal without any Prolog command-line arguments:
%     ?- with_safe_argv(my_python_goal).
%     true.
%
with_safe_argv(Goal):-
    % Store the current 'argv' flag value.
    current_prolog_flag(argv,Was),
    % Temporarily set 'argv' to an empty list and execute the goal.
    setup_call_cleanup(
        set_prolog_flag(argv,[]),   % Temporarily set argv to []
        py_catch(Goal),             % Execute the goal with exception handling.
        % Ensure that 'argv' is restored to its original state.
        set_prolog_flag(argv, Was)
    ).

%!  with_safe_argv(:G1, :G2) is det.
%
%   Executes the two given goals `G1` and `G2` with an empty `argv` flag, restoring
%   the original value of `argv` after their execution. This variant allows for executing
%   two goals sequentially.
%
%   @arg G1 The first goal to execute.
%   @arg G2 The second goal to execute.
%
with_safe_argv(G1, G2):-
    % Sequentially execute G1 and G2 using the safe argv environment.
    with_safe_argv((G1, G2)).

%!  py_catch(:Goal) is det.
%
%   A custom exception handling mechanism for catching and handling exceptions
%   while calling Python from Prolog. If an exception occurs during the execution of
%   the Goal, the exception is logged using `pybug/1`, and the Python traceback is printed
%   with `py_dump/0`. The original goal is then retried with tracing enabled for debugging.
%
%   @arg Goal The goal whose execution is wrapped with exception handling.
%
py_catch((G1, G2)):-
    % Handle exceptions for two goals executed sequentially.
    !,py_catch(G1),py_catch(G2).

py_catch(Goal):-
    % Catch any exceptions during goal execution.
    catch(Goal, E,
        (   pybug(E = py_catch(Goal)),  % Log the exception and goal that caused it.
            py_dump,                    % Print the Python traceback.
            trace,                      % Enable Prolog tracing for debugging.
            Goal)).                     % Retry the goal with tracing enabled.

% uncomment this and comment the above     when you need to trace
%py_catch(Goal):- trace,catch(Goal,E,(pybug(E),py_dump)),!.

%!  py_dump is det.
%
%   Dumps the current Python traceback using the `traceback` module. This is typically
%   used when an exception occurs while interacting with Python from Prolog to provide
%   insight into the cause of the exception.
%
py_dump:-
    % Call the Python function traceback:print_exc() to print the exception stack trace.
    py_call(traceback:print_exc()).

%!  py_call_c(:Goal) is det.
%
%   Calls the Python goal `G` while using the custom exception handling mechanism
%   `py_catch/1`. This is a safe way to invoke Python from Prolog, ensuring that
%   exceptions are properly caught and handled.
%
%   @arg Goal The Python goal to be executed.
%
py_call_c(G):- py_catch(py_call_warg(G)).

%!  py_call_c(:Goal, -Result) is det.
%
%   Calls the Python goal `G` and retrieves the result in `R`, with exception handling
%   provided by `py_catch/1`. This ensures that Python calls are safely executed and
%   exceptions are properly managed.
%
%   @arg Goal The Python goal to be executed.
%   @arg Result The result of the Python goal execution.
%
py_call_c(G, R):- py_catch(py_call_warg(G, R)).

%!  py_is_module(+M) is semidet.
%
%   Checks if the given `M` is a Python module. This is achieved by calling Python
%   and checking the type of the object. This operation is performed within a safe
%   `argv` context using `with_safe_argv/1`.
%
%   @arg M The object to check if it is a Python module.
%
py_is_module(M):- notrace((with_safe_argv(py_is_module_unsafe(M)))).

%!  py_is_module_unsafe(+M) is semidet.
%
%   Checks if `M` is a Python module without wrapping it in `with_safe_argv/1`. It first
%   checks if `M` is an object, then determines its type. If the type is `module`, it succeeds.
%   If not, it tries to call `M` and checks the type of the returned object.
%
%   @arg M The object to check if it is a Python module.
%
py_is_module_unsafe(M):- py_is_object(M),!,py_type(M, module).
py_is_module_unsafe(M):- atom(M), atom_concat(_,'.metta',M),!,fail.
py_is_module_unsafe(M):- catch((py_call_warg(M, X),py_type(X, module)), _, fail).

%!  py_is_py(+V) is semidet.
%
%   Determines if the given `V` is a Python object. It handles various types of objects
%   such as tuples, lists, dictionaries, and atomic values. It also checks if the value
%   has the `pyobj` attribute to determine if it is a Python object.
%
%   @arg V The variable or term to check if it is a Python object.
%

%py_is_py(_):- \+ py_is_enabled,!,fail.
py_is_py(V):- var(V),!,get_attr(V, pyobj, _),!.
py_is_py(V):- atomic(V),!,\+ atom(V),py_is_object(V),!.
py_is_py(V):- \+ callable(V),!,fail.
py_is_py(V):- \+ compound(V),!,fail.
py_is_py('@'(S)) :-
    % Succeeds if S is a non-variable and recognized as a Python constant.
    !, nonvar(S), is_py_const(S,_).
py_is_py(V):- is_list(V),!,fail.
py_is_py(V):- py_is_list(V),!.
py_is_py(V):- py_is_tuple(V),!.
py_is_py(V):- py_is_py_dict(V),!.


%!  is_py_const(+Const) is nondet.
%
%   Succeeds if the given Const is a recognized Python constant.
%
%   @arg Const The constant to be evaluated.
%
%   @example
%     ?- is_py_const('True').
%     true.
%
%     ?- is_py_const('Unknown').
%     false.
%
is_py_const(none,'None').
is_py_const(false,'False').
is_py_const(true,'True').

%!  py_resolve(+V, -Py) is det.
%
%   Resolves a variable or term `V` to its Python equivalent `Py`. If `V` is a variable,
%   it retrieves its `pyobj` attribute. If it is not a compound term, it assumes `V` is
%   already a Python object. For lists, it attempts to resolve each element. Otherwise,
%   it treats `V` as its resolved form.
%
%   @arg V The variable or term to be resolved.
%   @arg Py The resolved Python equivalent.
%
py_resolve(V, Py):- var(V),!,get_attr(V, pyobj, Py),!.
py_resolve(V, Py):- \+ compound(V),!,py_is_object(V),Py = V.
py_resolve(V, Py):- is_list(V),!,fail, maplist(py_resolve,V,Py).
py_resolve(V, Py):- V=Py.
%!  py_is_tuple(+X) is semidet.
%
%   Checks if the given term `X` is a Python tuple. The function resolves
%   the term `X` (typically some reference to a Python object) and checks
%   whether it is a tuple, but not a string (as strings can sometimes
%   behave like sequences in Python).
%
%   The predicate `py_is_tuple/1` is semi-deterministic, meaning it
%   succeeds if `X` can be determined to be a tuple and fails otherwise.
%   It is not fully deterministic because the input could resolve to
%   something other than a tuple.
%
%   @arg X The term to check if it is a Python tuple.
%
py_is_tuple(X):-
    % Resolve the Prolog term `X` to a Python object `V`.
    py_resolve(X, V),
    % Check if the resolved Python object `V` is a tuple.
    py_is_tuple_res(V).

% Helper predicate to check if the resolved object `V` is a tuple.
py_is_tuple_res(V):-
    % If `V` is a compound term, attempt to match it as a tuple-like structure.
    compound(V), !,
    % Check if `V` is a compound with the name `'-'`, which represents a tuple in some Prolog-Python integration.
    % This checks if `V` has a compound structure representing a tuple-like form.
    compound_name_arity(V, '-', _).

% Continue checking if the term is atomic and an object but not a string.
py_is_tuple_res(V):-
    % If `V` is atomic (i.e., a basic Prolog term, not a compound term).
    atomic(V),
    % Check if `V` is a Python object.
    py_is_object(V), !,  % Cut to prevent backtracking once object check succeeds.
    % Ensure the type of `V` is not `str` (strings should not be considered tuples).
    \+ py_type(V, str),
    % Finally, check if `V` is of type `tuple`.
    py_type(V, tuple).

% The commented out code below seems to have been an alternative tuple-checking strategy.
% It uses `py_tuple/2` to extract or transform a tuple, ensuring that the tuple is identical
% to itself and that it is not a string.
% py_is_tuple_res(V):- py_tuple(V,T), py_tuple(T,TT), T==TT, \+ py_type(V, str).

%!  py_is_py_dict(+X) is semidet.
%
%   Checks if the given term `X` is a Python dictionary. This is done by determining
%   if the resolved object is of type `dict`.
%
%   @arg X The term to check if it is a Python dictionary.
%
py_is_py_dict(X):- atomic(X),py_is_object(X),py_type(X,dict).
%py_is_py_dict(X):- py_resolve(X,V),py_dict(V,T),py_dict(T,TT),T==TT.

%!  py_is_list(+X) is semidet.
%
%   Checks if the given term `X` is a Python list. This is determined by resolving
%   the term and verifying its type is `list`.
%
%   @arg X The term to check if it is a Python list.
%
py_is_list(X):- py_resolve(X,V),py_is_object(V),py_type(V,list).
%py_is_list(V):- py_is_tuple(V).

% Evaluations and Iterations
:- initialization(ensure_py_loaded_early_maybe(metta_python_builtin)).


load_builtin_module:- ensure_py_loaded(metta_python_builtin).


%!  py_call_method_and_args_sig(+RetType,+Sig,+List, -Py) is det.
%
%   Converts a list `List` into a Python call, returning the result as `Py`.
%
%   @arg RetType The return type required or var.
%   @arg Sig Any singature overloads required or [].
%   @arg O The input list to be converted toi call.
%   @arg Py The result.
py_call_method_and_args_sig(RetType,Specialize,Sym,Adjusted,Res):-
   if_t(Specialize\==[], debug_info(todo,warn(ignoring(specialize=Specialize)))),
   py_call_method_and_args(Sym,Adjusted,Ret),
   py_metta_return_value(RetType, Ret,Res).

%!  py_call_method_and_args(+List, -Py) is det.
%
%   Converts a list `List` into a Python call, returning the result as `Py`.
%
%   @arg O The input list to be converted toi call.
%   @arg Py The result.
py_call_method_and_args([F|List], Py):- py_call_method_and_args(F,List, Py).

py_call_method_and_args(F,List, Py):- is_list(F),F=[Input],atom(Input),py_atom(Input,PyCallable),PyCallable\==F,!,py_call_method_and_args(PyCallable,List, Py).
py_call_method_and_args(F,List, Py):- select([Kw|Args],List,NewList), Kw=='Kwargs', must_det_lls((make_kw_args(Args,KeyWordArgs),
   maplist(py_arg,NewList,PyArgs),
   py_list([F|PyArgs],PyList),
   py_obi(py_call_method_and_args_kw(KeyWordArgs,PyList),Py))),!.
py_call_method_and_args(F,List, Py):- must_det_lls((maplist(py_arg,List,PyArgs),py_obi(py_call_method_and_args([F|PyArgs]),Py))),!.

pair_arg(NonCompound,_,_):- \+ compound(NonCompound), !,fail.
% Handle compound terms like (key=value)
pair_arg(Compound, Key,PyValue) :- compound(Compound), Compound = (Key=Value), !, py_arg(Value, PyValue).
% Handle list with key-value pair represented as [key, ':', value]
pair_arg([Key, Delimiter, Value], Key,PyValue) :- Delimiter == ':', !, py_arg(Value, PyValue).
% Handle list with key-value pair represented as [key=value]
pair_arg([KeyEquals, Value], Key,PyValue) :- symbol(KeyEquals), atom_concat(Key, '=', KeyEquals), !, py_arg(Value, PyValue).
% Handle list with key-value pair represented as [key:value]
pair_arg([KeyColon, Value], Key,PyValue) :- symbol(KeyColon),atom_concat(Key, ':', KeyColon), !, py_arg(Value, PyValue).

make_kw_args(KwArgs,KeyWordArgs):- lists_to_pairlist(KwArgs,Pairs),py_dict(Pairs,KeyWordArgs).
lists_to_pairlist(L,L):- \+ compound(L),!.
lists_to_pairlist([Kw,Val|Args],[[Key,Value]|KeyWordArgs]):- symbol(Kw),pair_arg_s([Kw,Val],Key,Value),!, lists_to_pairlist(Args,KeyWordArgs).
lists_to_pairlist([Kw,Eq,Val|Args],[[Key,Value]|KeyWordArgs]):- symbol(Kw),pair_arg_s([Kw,Eq,Val],Key,Value),!, lists_to_pairlist(Args,KeyWordArgs).
lists_to_pairlist([List|Args],[[Key,Value]|KeyWordArgs]):- pair_arg_s(List,Key,Value),!, lists_to_pairlist(Args,KeyWordArgs).
lists_to_pairlist([[List|Args]],[[Key,Value]|KeyWordArgs]):- pair_arg_s(List,Key,Value),!, lists_to_pairlist(Args,KeyWordArgs).
%lists_to_pairlist([],[]).

pair_arg_s(List,Key,PyValue):- pair_arg(List,Key,PyValue),!.
pair_arg_s([Key,Value],Key,PyValue):- symbolic(Key), py_arg(Value, PyValue),!.



py_term(Term, PyObject):- is_ftVar(Term),!,py_obi(identity(prolog(Term)), PyObject).
py_term([Quoted|Thing], Obj) :- is_list(Thing), Quoted = 'quote', Thing = [Arg],!,py_term(Arg, Obj).
py_term(Term, PyObject):- py_obi(identity(prolog(Term)), PyObject).
py_identity(Term, PyObject):- py_obi(identity((Term)), PyObject).

% !((py-atom print) "Hello, World!" [ = end "\n\n\n" ])
% py_arg(Var,Var):- var(Var),!.
% Handle variable input
py_arg(Variable, PyObject) :- var(Variable),!,py_term(Variable, PyObject).
% Handle symbols (atoms)
py_arg(Symbol, PyObject) :- symbol(Symbol),!,py_term(Symbol, PyObject).
% Handle numbers
py_arg(Number, PyObject) :- number(Number),!,py_identity(Number, PyObject).
% Handle strings
py_arg(String, PyObject) :- string(String),!,py_identity(String, PyObject).
% Handle py-term Arg
py_arg([Quoted|Thing], Obj) :- is_list(Thing), Quoted = 'quote', Thing = [Arg],!,py_term(Arg, Obj).
% Handle lists
py_arg(List, PyList) :- is_list(List), !, maplist(py_arg,List, PyArgs), py_obi(py_list(PyArgs),PyList),!.
% Handle Python objects
py_arg(Tuple, PyObject) :- compound(Tuple),compound_name_arguments(Tuple,'-',ArgsA),maplist(py_arg,ArgsA,ArgsB),compound_name_arguments(Mapped,'-',ArgsB),py_ocall_direct(tuple(Mapped),PyObject).
%py_arg(Tuple, PyObject) :- py_type(Tuple,'tuple'),!,map_tuple(py_arg,Tuple, Mapped),py_ocall(tuple(Mapped),PyObject),!. %py_tuple(Mapped,PyObject).
py_arg(Dict, PyObject) :- is_dict(Dict), py_dict(Dict, PyObject), !.
py_arg(Dict, PyObject) :- compound(Dict),Dict='{}'(_), py_dict(Dict, PyObject), !.
%py_arg(Dict, PyObject) :- compound(Dict),Dict='{}'(KVL),conjuncts_to_list(KVL,List),is_dict(Number), py_dict(Dict, PyObject), !.
py_arg(PythonObject, PythonObject) :- py_is_object(PythonObject), !.
% Handle compound terms like (key=value)
py_arg(Compound, {Key: PyValue}) :- pair_arg(Compound, Key,PyValue).
% Handle Python-native objects
py_arg(PythonNativeObject, PythonNativeObject) :- py_is_py(PythonNativeObject), !.
% Handle general terms by converting to Python objects
py_arg(GeneralTerm, PyObject) :- py_term(GeneralTerm, PyObject), !.
%py_arg(Symbol,After):- symbol(Symbol),atom_string(Symbol,Str),load_hyperon_module,py_call_warg(hyperon_module:rust_py_symbol(Str),After),!.
% Default case
py_arg(Final, Final).


%!  py_ppp(+V) is det.
%
%   Pretty prints the Python object `V` by using Prolog output redirection.
%   The object `V` is first printed with `py_pp/1`, and the output is processed
%   using `pych_chars/2` to clean unwanted characters like newlines or special markers.
%   Finally, the cleaned output is formatted and printed to the console.
%
%   @arg V The Python object to be pretty printed.
%
%   @example
%     % Pretty print a Python object:
%     ?- py_ppp(my_python_object).
%     my_python_object_cleaned_output
%
py_ppp(V):- py_is_object(V), py_type(V,'Term'),py_call(repr(V),String),!,write(String).

py_ppp(V):- py_is_object(V), py_type(V,'SymbolAtom'),py_call(repr(V),String),!,write(String).
py_ppp(V):-
    % Ensure all buffered output is flushed before printing.
    flush_output, py_pp_str(V,String),!,write(String),
    % Ensure the output is fully flushed after printing.
    !, flush_output.

py_pp_str(V,String):- py_is_object(V), py_type(V,'SymbolAtom'),!,py_call(repr(V),String).
py_pp_str(V,String):- string(V), !, sformat(String,'~q',[V]).
py_pp_str(V,String):-
   janus:opts_kws([], Kws),
    PFormat=..[pformat, V|Kws],    % Format and print the cleaned output.
    py_call(pprint:PFormat, String).


%atom_codes(Codes,P),writeq(Codes),
%py_ppp(V):- !, flush_output, py_mbi(print_nonl(V),_),!,flush_output.
%py_ppp(V):- writeq(py(V)),!.
%py_ppp(V):-once((py_is_object(V),py_to_pl(V,PL))),V\=@=PL,!,print(PL).
%py_ppp(V):-metta_py_pp(V).


% `is_metta_python_module_src/2` is declared as dynamic because it stores the
% source code of a loaded Python module. Since module contents can be modified
% at runtime (e.g., reloading or updating a module), this predicate needs to be
% dynamically asserted and retracted.
:- dynamic(is_metta_python_module_src/2).

% `did_ensure_py_loaded/2` is declared as dynamic because it keeps track of
% whether a Python module has already been loaded. This prevents redundant
% reloading and ensures that initialization happens only once per module.
:- dynamic(did_ensure_py_loaded/2).

% `did_ensure_py_loaded/2` is also declared as volatile. The volatile declaration
% ensures that the predicate does not persist when saving and restoring the Prolog
% state. Since this predicate only tracks runtime state (i.e., which modules have
% been loaded in the current session), it does not make sense to retain it across
% different Prolog executions.
:- volatile(did_ensure_py_loaded/2).

%!  get_metta_python_module_src(+Module, -String) is det.
%
%   Retrieves the Python source code for a given `Module`. If the content has
%   already been loaded, it retrieves the cached version; otherwise, it reads
%   the module file and asserts it for future use.
%
%   @param Module The name of the Python module.
%   @param String The source code of the Python module as a string.
%
get_metta_python_module_src(Module, String) :-
    is_metta_python_module_src(Module, String), !.
get_metta_python_module_src(Module, String) :-
    guess_py_module_file(Module, AbsFile),
    read_file_to_string(AbsFile, String, [encoding(utf8)]),
    asserta(is_metta_python_module_src(Module, String)), !.

%!  split_py_module_name(+Module, -FileSpec) is det.
%
%   Splits a Metta Python module name into a corresponding file specification.
%   Currently, it returns the module name as the file specification without modification.
%
%   @param Module The name of the Python module.
%   @param FileSpec The resulting file name (same as `Module` for now).
%
split_py_module_name(Module, Module).

%!  guess_py_module_file(+Module, -AbsFile) is semidet.
%
%   Determines the absolute file path for a given `Module`. It searches in
%   known source directories and resolves the file path using `absolute_file_name/3`.
%
%   @param Module The name of the Python module.
%   @param AbsFile The absolute path of the module file.
%
guess_py_module_file(Module, AbsFile) :-
    split_py_module_name(Module, FileSpec),
    search_py_module_src_dir(Dir),
    exists_directory(Dir),
    absolute_file_name(FileSpec, AbsFile, [
        relative_to(Dir),
        %access(exists),
        access(read),
        file_errors(fail),
        %file_type(python),
        extensions(['py'])
    ]), !.

%!  search_py_module_src_dir(-Dir) is nondet.
%
%   Finds a directory that contains Metta Python source files.
%   It searches in various predefined locations, including:
%   - User-defined Metta source directories.
%   - The Metta root directory.
%   - The current working directory.
%   - The `PYTHONPATH` environment variable.
%   - Common relative paths.
%   - Python's system path (`sys.path`).
%
%   @param Dir A valid directory containing Python source files.
%
search_py_module_src_dir(Dir) :-
    findall(D, search_py_module_src_dir0(D), AList),
    maplist(any_to_atom, AList, List),!,
    list_to_set_member(List, Dir).

list_to_set_member(List, Dir) :-
    list_to_set(List, Set),
    member(Dir, Set).


search_py_module_src_dir0(Dir) :- user:is_metta_src_dir(Dir).
search_py_module_src_dir0(Dir) :- metta_root_dir(Dir).
search_py_module_src_dir0(Dir) :- working_directory(Dir, Dir).
search_py_module_src_dir0(Dir) :-
    getenv('PYTHONPATH', CurrentPythonPath),
    symbolic_list_concat(List, ':', CurrentPythonPath),
    list_to_set_member(List, Dir).
search_py_module_src_dir0(Dir) :- find_mettalog_relative_path('./python/', Dir).
%search_py_module_src_dir0(Dir) :- find_mettalog_relative_path('../hyperon-experimental`/', Dir).
search_py_module_src_dir0(Dir) :- py_call(sys:path, PyList), list_to_set_member(PyList, Dir).
search_py_module_src_dir0(Dir) :- py_lib_dirs(List), list_to_set_member(List, Dir).

%!  ensure_py_loaded(+Module) is det.
%
%   Ensures that the Metta Python module is loaded into the system.
%   If the module has already been loaded, it succeeds immediately.
%   Otherwise, it:
%   1. Retrieves the Python module as a string.
%   2. Calls `py_module/2` to initialize the module in the Python runtime.
%   3. Asserts that the module has been loaded to prevent duplicate loads.
%
%   @param Module The name of the Metta Python module.
%
ensure_py_loaded(Module) :- did_ensure_py_loaded(Module, _), !.
ensure_py_loaded(Module) :-
    get_metta_python_module_src(Module, String),
    catch(
        (
            ignore(notrace(with_safe_argv(py_module(Module, String)))),
            assertz(did_ensure_py_loaded(Module, String))
        ),
        E,
        (show_python_path, write_src_nl([E = Module, src = String]))
    ),
    !.

    %!  ensure_py_loaded_early_maybe(+Module) is det.
%
%   Attempts to ensure that the Python integration for Metta is loaded.
%   It first tries to lazily load the Python interface (`lazy_load_python/0`).
%   If that fails, it proceeds to manually load the Metta Python module.
%
%   @param Module The name of the Metta Python module.
%
%
%   Ensures that `ensure_py_loaded_early_maybe/1` is executed:
%   - When the system starts.
%   - When a saved state is restored.
%
ensure_py_loaded_early_maybe(_Module) :-
    clause(lazy_load_python,true), !.
ensure_py_loaded_early_maybe(Module) :-
    ensure_py_loaded(Module).

:- initialization(ensure_py_loaded_early_maybe(metta_python_proxy)).
:- initialization(ensure_py_loaded_early_maybe(metta_python_hyperon)).


%!  py_mcall(+I, -O) is semidet.
%
%   Calls a Python method (py_call_warg/3) with the input term I and unifies the result with O.
%   This call converts Python objects to Prolog strings and dictionaries to Prolog terms.
%
%   @arg I Input term to call the Python function.
%   @arg O Output term that unifies with the result of the Python call.
%
py_mcall(I,O):- catch(py_call_warg(I,M,[py_object(false), py_string_as(string),py_dict_as({})]),E,py_error_fail(E)),!,O=M.

%!  py_scall(+I, -O) is semidet.
%
%   Calls a Python function and converts the result to a Prolog string.
%
%   @arg I Input term to call the Python function.
%   @arg O Output term that unifies with the result of the Python call as a string.
%
py_scall(I,O):- catch(py_call_warg(I,M,[py_string_as(string)]),E,py_error_fail(E)),!,O=M.

%!  py_acall(+I, -O) is semidet.
%
%   Calls a Python function and converts the result to a Prolog atom.
%
%   @arg I Input term to call the Python function.
%   @arg O Output term that unifies with the result of the Python call as an atom.
%
py_acall(I,O):- catch(py_call_warg(I,M,[py_string_as(atom)]),E,py_error_fail(E)),!,O=M.

%!  py_ocall(+I, -O) is semidet.
%
%   Calls a Python function and converts the result to a Prolog term with string representation.
%
%   @arg I Input term to call the Python function.
%   @arg O Output term that unifies with the result of the Python call as a string.
%
py_ocall(I,O):- catch(py_call_warg(I,M,[py_object(true),py_string_as(string)]),E,py_error_fail(E)),!,O=M.

py_ocall_direct(I,O):- catch(py_call(I,M,[py_object(true),py_string_as(string)]),E,py_error_fail(E)),!,O=M.

%!  py_bi(+I, -O, +Opts) is semidet.
%
%   Calls a built-in Python function with specified options.
%   First ensures that the built-in Python module is loaded.
%
%   @arg I Input term to call the Python built-in function.
%   @arg O Output term that unifies with the result of the Python call.
%   @arg Opts Options passed to the Python call.
%
py_bi(I,O,Opts):- load_builtin_module,catch(py_call_warg(metta_python_builtin:I,M,Opts),E,py_error_fail(E)),!,O=M.

%!  py_obi(+I, -O) is semidet.
%
%   Calls a built-in Python function and converts the result to a Prolog term with string representation.
%   Ensures that the built-in Python module is loaded before the call.
%
%   @arg I Input term to call the Python built-in function.
%   @arg O Output term that unifies with the result of the Python call.
%
py_obi(I,O):- load_builtin_module,py_ocall_direct(metta_python_builtin:I,O).

%!  py_mbi(+I, -O) is semidet.
%
%   Calls a built-in Python method and unifies the result with O.
%   Ensures that the built-in Python module is loaded before the call.
%
%   @arg I Input term to call the Python built-in function.
%   @arg O Output term that unifies with the result of the Python call.
%
py_mbi(I,O):- load_builtin_module,py_mcall(metta_python_builtin:I,O).

%?- py_call_warg(type(hi-there), P),py_pp(P).

%!  get_str_rep(+I, -O) is semidet.
%
%   Retrieves the string representation of a Python object using a built-in method.
%
%   @arg I Input Python object to get the string representation.
%   @arg O Output term that unifies with the string representation of the input object.
%
get_str_rep(I,O):- py_mbi(get_str_rep(I),O),!.

%!  py_atom(+I, -O) is det.
%
%   Converts the input term I to a Python-compatible atom or retrieves its Python equivalent.
%   Handles different types of inputs including variables, lists, atoms, and Python objects.
%
%   @arg I Input term to be converted or retrieved.
%   @arg O Output term that unifies with the Python atom or its Prolog equivalent.
%
py_atom(I,O):- var(I),!,O=I.
py_atom(A,Py):- atomic(A),!,catch(py_obi(make_py_atom(A),Py),E,py_error_fail(E)),!.
py_atom([I|Is],O):- !,py_dot(I,II),py_dot_from(II,Is,O),!.
py_atom(I,O):- atomic(I),!,py_atomic(I,O).
py_atom(I,O):- py_ocall_direct(I,O),!.
py_atom(I,O):- I=O.

%!  py_atom_type(+I, +Type, -O) is det.
%
%   Similar to py_atom/2 but includes a type indicator.
%   This allows converting or retrieving Python atoms with a specified type.
%
%   @arg I Input term to be converted or retrieved.
%   @arg Type Type indicator (currently unused in the code).
%   @arg O Output term that unifies with the Python atom or its Prolog equivalent.
%
py_atom_type(I,_Type,O):- var(I),!,O=I.
py_atom_type([I|Is],_Type,O):- !,py_dot(I,II),py_dot_from(II,Is,O).
py_atom_type(I,_Type,O):- atomic(I),!,py_atomic(I,O).
py_atom_type(I,_Type,O):- py_ocall(I,O),!.
py_atom_type(I,_Type,O):- I=O.

%!  py_atomic(+I, -O) is semidet.
%
%   Converts the input term I into a Python atomic object.
%   Handles various cases such as lists, strings, and objects.
%
%   @arg I Input term to be converted.
%   @arg O Output term that unifies with the Python atomic object.
%
py_atomic([],O):- py_ocall("[]",O),!.
py_atomic(I,O):- py_is_object(I),!,O=I.
py_atomic(I,O):- string(I),py_eval_string(I,O),!.
py_atomic(I,O):- symbol(I),py_eval_string(I,O),!.
py_atomic(I,O):- py_ocall(I,O),!.
py_atomic(I,O):- py_eval_string(I,O),!.
py_atomic(I,O):- \+ symbol_contains(I,'('), atomic_list_concat([A,B|C],'.',I), py_dot([A,B|C],O),!.
py_atomic(I,O):- string(I),py_dot(I,O),!.
py_atomic(I,O):- I=O.

%!  get_globals(-O) is det.
%
%   Retrieves the current global variables from the Python interpreter and unifies
%   them with the variable O.
%
%   @arg O The output which will unify with the global variables.
get_globals(O):- py_mbi(get_globals(),O).

%!  get_locals(-O) is det.
%
%   Retrieves the current local variables from the Python interpreter and unifies
%   them with the variable O.
%
%   @arg O The output which will unify with the local variables.
get_locals(O):- py_mbi(get_locals(),O).

%!  merge_modules_and_globals(-O) is det.
%
%   Merges Python modules and global variables into a single dictionary and unifies
%   it with the variable O.
%
%   @arg O The output which will unify with the merged modules and globals.
merge_modules_and_globals(O):- py_mbi(merge_modules_and_globals(), O).

%!  py_eval_string(+I, -O) is det.
%
%   Evaluates the Python expression given by the input string I and unifies the result
%   with the output O.
%
%   @arg I The input string representing a Python expression.
%   @arg O The output which will unify with the evaluation result.
py_eval_string(I, O):- py_obi(eval_string(I),O).

%!  py_eval_string(+I) is det.
%
%   Evaluates the Python expression given by the input string I and outputs any errors
%   or results using the predicate pybug/1.
%
%   @arg I The input string representing a Python expression.
py_eval_string(I):- py_eval_string(I, O),pybug(O).

%!  py_exec(+I, -O) is det.
%
%   Executes the Python code given by the input string I and unifies the output O
%   with the result of the execution.
%
%   @arg I The input string representing Python code.
%   @arg O The output which will unify with the result of the execution.
py_exec(I,O):- py_mbi(exec_string(I), O).

%!  py_exec(+I) is det.
%
%   Executes the Python code given by the input string I and outputs any errors or
%   results using the predicate pybug/1.
%
%   @arg I The input string representing Python code.
py_exec(I):- py_exec(I, O),pybug(O).

%!  py_dot(+I, -O) is det.
%
%   Converts the string I to an atom and retrieves the corresponding Python object,
%   unifying it with O. If I is already an atom, the object is directly retrieved.
%
%   @arg I The input string or atom representing a Python object.
%   @arg O The output which will unify with the Python object.
py_dot(I,O):- string(I),py_obi(eval_string(I),O),!.
py_dot(I,O):- py_atom(I,O).

%!  py_dot_from(+From, +I, -O) is det.
%
%   Recursively traverses and retrieves a Python object by following a path
%   represented by a list of names or a dot-separated string.
%
%   @arg From The initial Python object from which to start.
%   @arg I The path to traverse, represented as a list or dot-separated string.
%   @arg O The output which will unify with the resulting Python object.
py_dot_from(From,I,O):- I==[],!,O=From.
py_dot_from(From,[I|Is],O):- !,py_dot_from(From, I, M),py_dot_from(M, Is, O).
py_dot_from(From,I,O):- atomic_list_concat([A,B|C],'.',I),!,py_dot_from(From,[A,B|C],O).
py_dot_from(From,I,O):- make_py_dot(From,I,O).

make_py_dot(A,B,Py):- catch(py_obi(make_py_dot(A,B),Py),E,py_error_fail(E)),!.
make_py_dot(A,B,Py):- py_dot([A,B],Py),!.

make_py_dot(A,B,_Specialize,Py):- catch(py_obi(make_py_dot(A,B),Py),E,py_error_fail(E)),!.
make_py_dot(A,B,_Specialize,Py):- py_dot([A,B],Py),!.

py_error_fail(E):- wdmsg(E), \+ is_extreme_debug, !,fail.
py_error_fail(E):- E = error(python_error(_ErrorType, Value), _), print_python_traceback(Value), fail.
py_error_fail(E):- wdmsg(E),bt,wdmsg(E),!,trace,fail.


print_python_traceback(Value) :-
    py_call(metta_python_builtin:format_python_exception(Value), TraceList),
    format('Python traceback:\n~s', [TraceList]).

%!  py_eval_object(+Var, -VO) is det.
%
%   Evaluates a Python object, recursively handling lists and checking if the object
%   is a Python function to call it if necessary. Unifies the result with VO.
%
%   @arg Var The input variable to be evaluated.
%   @arg VO The output which will unify with the evaluated object.
py_eval_object(Var,VO):- var(Var),!,VO=Var.
py_eval_object([V|VI],VO):- py_is_function(V),!,py_eval_from(V,VI,VO).
py_eval_object([V|VI],VO):- maplist(py_eval_object,[V|VI],VO).
py_eval_object(VO,VO).

%!  py_is_function(+O) is semidet.
%
%   Succeeds if O is a Python function, fails otherwise.
%
%   @arg O The input object to check.
py_is_function(O):- \+ py_is_object(O),!,fail.
%py_is_function(PyObject) :- maybe_py_deref(PyObject,PyObject2),!,py_is_function(PyObject2).
py_is_function(PyObject) :-
    py_type(PyObject, Type),
    py_is_method_type(Type),!.
py_is_function(PyObject):- py_call(callable(PyObject),C),!,C= @(true).
py_is_method_type(type).
py_is_method_type(builtin_function_or_method).
py_is_method_type(ufunc).
py_is_method_type('numpy.ufunc').
%py_is_method_type('OperatorAtom').
py_is_method_type(function).
py_is_method_type(method).
py_is_method_type('method-wrapper').

py_is_callable(O):- atomic(O),py_is_function(O).

%!  py_eval_from(+From, +I, -O) is det.
%
%   Evaluates a Python object from a starting point by traversing a given path
%   and calling functions along the way.
%
%   @arg From The initial Python object from which to start.
%   @arg I The path to traverse or function to call, represented as a list or string.
%   @arg O The output which will unify with the result of the evaluation.
py_eval_from(From,I,O):- I==[],!,py_dot(From,O).
py_eval_from(From,[I],O):- !,py_fcall(From,I,O).
py_eval_from(From,[I|Is],O):- !,py_dot_from(From,I,M),py_eval_from(M,Is,O).
py_eval_from(From,I,O):- atomic_list_concat([A,B|C],'.',I),!,py_eval_from(From,[A, B|C], O).
py_eval_from(From,I,O):- py_fcall(From,I,O).

%!  py_fcall(+From, +I, -O) is det.
%
%   Calls a Python function or method I from the object From and unifies the result with O.
%
%   @arg From The Python object from which to call the function.
%   @arg I The function name.
%   @arg O The output which will unify with the result of the function call.
py_fcall(From,I,O):- py_ocall(From:I,O).

%!  ensure_space_py(+Space, -GSpace) is det.
%
%   Ensures that Space is a valid Python object, either by verifying it or
%   by defaulting to the primary Metta space if necessary.
%
%   @arg Space The space to check or unify.
%   @arg GSpace The global space or resolved space.
ensure_space_py(Space,GSpace):- py_is_object(Space),!,GSpace=Space.
ensure_space_py(Space, GSpace):- var(Space),ensure_primary_metta_space(GSpace),Space=GSpace.
ensure_space_py(metta_self,GSpace):- ensure_primary_metta_space(GSpace),!.

%!  ensure_rust_metta(-MeTTa) is det.
%
%   Ensures that the given MeTTa instance is a valid object. If it is already
%   known and stored in the dynamic predicate `is_metta/1`, it succeeds immediately.
%   Otherwise, it initializes the MeTTa instance and stores it using `asserta/1`.
%
%   @arg MeTTa The MeTTa instance that will be ensured or initialized.
:- dynamic(is_metta/1).
:- volatile(is_metta/1).

ensure_rust_metta(MeTTa):-
    is_metta(MeTTa),                            % Check if MeTTa is already known and valid.
    py_is_object(MeTTa),!.                      % Ensure that MeTTa is a valid Python object.
ensure_rust_metta(MeTTa):-
    with_safe_argv(ensure_rust_metta1(MeTTa)),  % Initialize MeTTa with safe arguments.
    asserta(is_metta(MeTTa)).                   % Store the MeTTa instance for future use.

%!  ensure_rust_metta0(-MeTTa) is det.
%
%   Attempts to initialize a MeTTa instance from different Python sources.
%   Tries the `get_metta` method from MettaLearner, or falls back on other
%   MeTTa-related Python calls.
%
%   @arg MeTTa The MeTTa instance that will be initialized.
ensure_rust_metta0(MeTTa):-
    ensure_mettalog_py(MettaLearner),           % Ensure MettaLearner is available.
    py_call_warg(MettaLearner:'get_metta'(),MeTTa),  % Call the `get_metta` method.
    py_is_object(MeTTa).
ensure_rust_metta0(MeTTa):-
    py_call_warg('mettalog':'MeTTaLog'(), MeTTa).    % Fallback: Call MeTTaLog constructor.
ensure_rust_metta0(MeTTa):- ensure_rust_metta1(MeTTa).

ensure_rust_metta1(MeTTa):-
    py_call_warg(hyperon:runner:'MeTTa'(), MeTTa),!. % Fallback: Call MeTTa from hyperon.

%!  ensure_rust_metta is det.
%
%   Ensures that a MeTTa instance is available. Calls `ensure_rust_metta/1` with
%   an unbound variable to initialize a new instance if needed.
ensure_rust_metta:- ensure_rust_metta(_).

%!  ensure_mettalog_py(-MettaLearner) is det.
%
%   Ensures that the MettaLearner instance is initialized. If it iss already stored
%   in the dynamic predicate `is_mettalog/1`, it succeeds immediately. Otherwise,
%   it initializes the MettaLearner and stores it.
%
%   @arg MettaLearner The MettaLearner instance that will be ensured or initialized.
:- dynamic(is_mettalog/1).
:- volatile(is_mettalog/1).

ensure_mettalog_py(MettaLearner):-
    is_mettalog(MettaLearner), !.  % Check if MettaLearner is already known.
ensure_mettalog_py(MettaLearner):-
    with_safe_argv(  % Ensure safety for argument passing.
        (add_wanted_py_lib_dirs,  % Ensure the Python library directory is available.
            %py_call_warg('mettalog',MettaLearner),
            %py_call_warg('motto',_),
            %py_call_warg('motto.sparql_gate':'sql_space_atoms'(),Res1),pybug(Res1),
            %py_call_warg('motto.llm_gate':'llmgate_atoms'(MeTTa),Res2),pybug(Res2),
        if_trace(python,not_compatio(pybug(is_mettalog(MettaLearner)))),  % Log any issues.
        asserta(is_mettalog(MettaLearner))  % Store the MettaLearner instance.
        )).

%!  ensure_mettalog_py is det.
%
%   Initializes the MettaLearner instance by setting environment variables
%   and invoking the necessary Python modules.
ensure_mettalog_py:-
   % once finished we also are required to have these as well
   %load_builtin_module,
   %load_hyperon_module,
    setenv('VSPACE_VERBOSE', 0),  % Set the environment variable for verbosity.
    with_safe_argv(ensure_mettalog_py(_)), !.  % Safely initialize MettaLearner.

:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).

%!  space_type_method(+SpaceType, +Method, +Implementation) is det.
%
%   Maps a method for a specific space type to its implementation.
%
%   This predicate associates a method name with the corresponding implementation
%   for a given space type. It is used to define different actions (such as adding,
%   removing, or querying atoms) for a specific type of space (e.g., a Prolog space or
%   a Rust space).
%
%   @arg SpaceType       The type of the space (e.g., `is_not_prolog_space`).
%   @arg Method          The name of the method to be mapped (e.g., `add_atom`).
%   @arg Implementation  The actual implementation function or predicate that
%                        performs the desired operation (e.g., `add_to_space`).
%
%   @examples
%     % Example of mapping methods for a non-Prolog space:
%     ?- space_type_method(is_not_prolog_space, new_space, new_rust_space).
%
%     % This maps the `new_space` method for `is_not_prolog_space` to the
%     % `new_rust_space` implementation.
%
space_type_method(is_not_prolog_space,new_space,new_rust_space).
space_type_method(is_not_prolog_space,add_atom,add_to_space).
space_type_method(is_not_prolog_space,remove_atom,remove_from_space).
space_type_method(is_not_prolog_space,replace_atom,replace_in_space).
space_type_method(is_not_prolog_space,atom_count,atom_count_from_space).
space_type_method(is_not_prolog_space,get_atoms,query_from_space).
space_type_method(is_not_prolog_space,atom_iter,atoms_iter_from_space).
space_type_method(is_not_prolog_space,query,query_from_space).

%!  ensure_primary_metta_space(-GSpace) is det.
%
%   Ensures that the primary `GroundingSpace` is available and returns it.
%   If the space is already stored in `is_primary_metta_space/1`, it succeeds immediately.
%   Otherwise, it attempts to initialize a new space through the Rust MeTTa environment.
%
%   @arg GSpace The `GroundingSpace` that will be ensured or initialized.
:- dynamic(is_primary_metta_space/1).
:- volatile(is_primary_metta_space/1).

ensure_primary_metta_space(GSpace):-
    is_primary_metta_space(GSpace),!.                % Check if the primary space is already known.
ensure_primary_metta_space(GSpace):-
    ensure_rust_metta(MeTTa),               % Ensure that the Rust MeTTa environment is initialized.
    with_safe_argv(py_call_warg(MeTTa:space(), GSpace)),  % Call the space method on MeTTa.
    asserta(is_primary_metta_space(GSpace)).         % Store the new space.
ensure_primary_metta_space(GSpace):-
    new_rust_space(GSpace).                          % Fallback: initialize a new Rust space.

%!  ensure_primary_metta_space is det.
%
%   Ensures that the primary `GroundingSpace` is available. Calls `ensure_primary_metta_space/1`
%   with an unbound variable to initialize the space if necessary.
ensure_primary_metta_space:- ensure_primary_metta_space(_).

%!  new_rust_space(-GSpace) is det.
%
%   Initializes a new instance of `GroundingSpace` from the hyperon base in the Rust environment.
%   This predicate ensures that the space is available by calling the appropriate Python bindings.
%
%   @arg GSpace The `GroundingSpace` instance that will be initialized.
 :- if(\+ current_predicate(new_rust_space/1)).
% Initialize a new hyperon.base.GroundingSpace and get a reference
new_rust_space(GSpace):-
    with_safe_argv(py_call_warg(hyperon:base:'GroundingSpace'(),GSpace)),  % Create a new GroundingSpace.
    asserta(is_python_space(GSpace)).  % Store the new space.
 :- endif.

%!  query_from_space(+Space, +QueryAtom, -Result) is det.
%
%   Queries the given `GroundingSpace` with a `QueryAtom` and returns the result.
%   The `GroundingSpace` is ensured before performing the query, and the result
%   is obtained through Python bindings.
%
%   @arg Space The space from which the query is made.
%   @arg QueryAtom The atom used to query the space.
%   @arg Result The result of the query, which will unify with the output.
 :- if(\+ current_predicate(query_from_space/3)).

query_from_space(Space,QueryAtom,Result):-
    ensure_space(Space,GSpace),                % Ensure the space is valid.
    py_call_warg(GSpace:'query'(QueryAtom),Result). % Perform the query in the space.

%!  replace_in_space(+Space, +FromAtom, +ToAtom) is det.
%
%   Replaces an atom in the given `GroundingSpace` by removing `FromAtom` and adding `ToAtom`.
%   This operation is performed in the Rust-based `GroundingSpace` using Python bindings.
%
%   @arg Space The space in which the replacement will occur.
%   @arg FromAtom The atom to be replaced.
%   @arg ToAtom The new atom that will replace the `FromAtom`.
replace_in_space(Space,FromAtom,ToAtom):-
    ensure_space(Space,GSpace),                     % Ensure the space is valid.
    py_call_warg(GSpace:'replace'(FromAtom,ToAtom), _).  % Perform the replacement.

%!  atom_count_from_space(+Space, -Count) is det.
%
%   Retrieves the number of atoms in the given `GroundingSpace`.
%   This operation calls the Python method `atom_count` to get the total count.
%
%   @arg Space The space from which the atom count will be retrieved.
%   @arg Count The number of atoms in the space.
atom_count_from_space(Space,Count):-
    ensure_space(Space,GSpace),            % Ensure the space is valid.
    py_call_warg(GSpace:'atom_count'(),Count).  % Retrieve the atom count.

%!  atoms_from_space(+Space, -Atoms) is det.
%
%   Retrieves all atoms from the given `GroundingSpace`.
%   The Python method `get_atoms` is used to fetch the atoms from the space.
%
%   @arg Space The space from which the atoms will be retrieved.
%   @arg Atoms The list of atoms present in the space.
atoms_from_space(Space,Atoms):- ensure_space(Space, GSpace),  % Ensure the space is valid.
    py_call_warg(GSpace:'get_atoms'(),Atoms).                       % Retrieve all atoms.

%!  atom_from_space(+Space, -Sym) is nondet.
%
%   Retrieves individual atoms from the given `GroundingSpace` by iterating through the atoms.
%   The predicate will succeed for each atom available in the space.
%
%   @arg Space The space from which an atom will be retrieved.
%   @arg Sym The atom retrieved from the space.
atom_from_space(Space,Sym):-
    atoms_iter_from_space(Space,Atoms),     % Get the iterator of atoms.
    elements(Atoms,Sym).                    % Iterate through the atoms.

%!  atoms_iter_from_space(+Space, -Atoms) is det.
%
%   Retrieves an iterator for the atoms in the given `GroundingSpace`.
%   This allows atoms to be fetched one by one as needed.
%
%   @arg Space The space from which the atom iterator will be retrieved.
%   @arg Atoms The iterator of atoms.
atoms_iter_from_space(Space,Atoms):-
    % Ensure the space is valid.
    ensure_space(Space,GSpace),
    % Retrieve the iterator.
    with_safe_argv(py_call_warg(src:'mettalog':get_atoms_iter_from_space(GSpace),Atoms)),
    % for debugging print the atoms
    %py_call_warg(GSpace:'atoms_iter'(), Atoms).
    true.
 :- endif.

%!  metta_py_pp(+V) is det.
%
%   Pretty-prints a Python object or Prolog term. If the input is a Python object,
%   it is first converted to a Prolog term and then printed. Otherwise, the term
%   is directly printed.
%
%   @arg V The value (either Python or Prolog) to be printed.
metta_py_pp(V):-
    py_is_enabled, once((py_is_object(V), py_to_pl(V, PL))),  % Convert Python object to Prolog term.
    V\=@=PL,!,
    metta_py_pp(PL).                                          % Recursively print the Prolog term.
metta_py_pp(V):-
    atomic(V),py_is_enabled,py_is_object(V),py_pp(V),!.       % Pretty-print atomic Python objects.
metta_py_pp(V):- format('~p',[V]),!.                         % Print the Prolog term.

%!  py_to_pl(+I, -O) is det.
%
%   Converts a Python object I to a Prolog term O.
%
%   @arg I The input Python object.
%   @arg O The output Prolog term after conversion.
py_to_pl(I,O):- py_to_pl(_,I,O).

%!  py_to_pl(+VL, +I, -O) is det.
%
%   Helper predicate that calls `py_to_pl/6` with initial parameters.
%   It handles the conversion of Python objects to Prolog terms.
%
%   @arg VL The variable list used for tracking conversions.
%   @arg I The input Python object.
%   @arg O The output Prolog term.
py_to_pl(VL,I,O):- ignore(VL=[vars]),py_to_pl(VL,[],[],_,I,O),!.

%!  is_var_or_nil(+I) is semidet.
%
%   Succeeds if the input I is either a Prolog variable or an empty list.
%
%   @arg I The input term, which can be a variable or an empty list.
is_var_or_nil(I):- var(I),!.            % Succeed if the input is a variable.
is_var_or_nil([]).                      % Succeed if the input is an empty list.

%!  py_to_pl(+VL, +Par, +Cir, -CirO, +L, -E) is det.
%
%   Main predicate for converting a Python object or structure into a Prolog term.
%   It handles Python objects, compound terms, lists, and dictionaries, while also
%   dealing with circular references through `Cir` (circular reference tracking).
%
%   @arg VL Variable list used for tracking variable conversions.
%   @arg Par Parameter list (currently ignored in this implementation).
%   @arg Cir Circular reference list used to track objects already encountered.
%   @arg CirO Output circular reference list after conversion.
%   @arg L The Python object or structure to be converted.
%   @arg E The resulting Prolog term after conversion.
%
% Print debug information (commented out).
% py_to_pl(VL,Par,_Cir,_,L,_):- pybug(py_to_pl(VL,Par,L)),fail.

% If L is a variable,unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- var(L),!,E=L.
% If L is an empty list, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- L == [],!,E = L.
% If O is a Python object, convert it to Prolog by calling `pyo_to_pl`.
py_to_pl(VL,Par,Cir,CirO,O,E):- py_is_object(O),py_class(O,Cl),!, pyo_to_pl(VL,Par,[O = E | Cir],
    CirO,Cl,O,E).
% we might need to switch dicts to their prolog correspondence later
%py_to_pl(_VL,_Par,Cir,Cir,L,E):- py_is_dict(L),!,py_mbi(identity(L),E).
% If L is in the circular reference list, handle the circular reference.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- member(N-NE,Cir),N==L,!,(E=L;NE=E),!.
% If LORV is a variable or nil, unify LORV:B directly.
py_to_pl(_VL,_Par,Cir,Cir,LORV:B,LORV:B):- is_var_or_nil(LORV),!.
py_to_pl(_VL,_Par,Cir,Cir,LORV:_B:_C,LORV):- is_var_or_nil(LORV),!.
% If L is not callable, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- \+callable(L),!,E=L.
% Convert annotated lists [H|T]:B:C.
py_to_pl(VL,Par,Cir,CirO,[H|T]:B:C,[HH|TT]):- py_to_pl(VL,Par,Cir,CirM,H:B:C,HH),
    py_to_pl(VL,Par,CirM,CirO,T:B:C,TT),!.
py_to_pl(VL,Par,Cir,CirO,[H|T]:B,[HH|TT]):- py_to_pl(VL,Par,Cir,CirM,H:B,HH),
    py_to_pl(VL,Par,CirM,CirO,T:B,TT).
% Handle Python objects with callable methods A:B:C or A:B.
py_to_pl(VL,Par,Cir,CirO,A:B:C,AB):- py_is_object(A),callable(B),py_call_warg(A:B,R),
    py_to_pl(VL,Par,Cir,CirO,R:C,AB).
py_to_pl(VL,Par,Cir,CirO,A:B,AB):- py_is_object(A),callable(B),py_call_warg(A:B,R),
    py_to_pl(VL,Par,Cir,CirO,R,AB).
% Convert compound terms like A:B or A-B.
py_to_pl(VL,Par,Cir,CirO,A:B,AA:BB):- !,py_to_pl(VL,Par,Cir,CirM,A,AA),py_to_pl(VL,Par,CirM,CirO,B,BB).
py_to_pl(VL,Par,Cir,CirO,A-B,AA-BB):- !,py_to_pl(VL,Par,Cir,CirM,A,AA),py_to_pl(VL,Par,CirM,CirO,B,BB).
py_to_pl(_VL,_Par,Cir,Cir,L,E):- \+ callable(L),!,E = L.
% If L is an atom, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- atom(L),!,E=L.
% Convert lists.
py_to_pl(VL,Par,Cir,CirO,[H|T],[HH|TT]):- !,py_to_pl(VL,Par,Cir,CirM,H,HH),
    py_to_pl(VL,Par,CirM,CirO,T,TT).
% Handle dictionaries.
py_to_pl(VL,Par,Cir,CirO,L,E):- is_dict(L,F),!,dict_pairs(L,F,NV),!,py_to_pl(VL,Par,Cir,CirO,NV,NVL),
    dict_pairs(E,F,NVL).
% If L is not callable, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- \+ callable(L),!,E = L.
%next phase code
%py_to_pl(VL,Par,Cir,CirO,A:B:C,AB):-  py_is_object(A),callable(B),py_call_warg(A:B,R),!,
%    py_to_pl(VL,Par,[A:B-AB|Cir],CirO,R:C,AB).
%py_to_pl(VL,Par,Cir,CirO,A:B,AB):-  py_is_object(A),callable(B),py_call_warg(A:B,R),!,
%    py_to_pl(VL,Par,[A:B-AB|Cir],CirO,R,AB).
% Convert compound terms using `compound_name_arguments/3`.
py_to_pl(VL,Par,Cir,CirO,A,AA):- compound(A),!,compound_name_arguments(A,F,L),
    py_to_pl(VL,Par,Cir,CirO,L,LL),compound_name_arguments(AA,F,LL).
% Default case: unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,E,E).

/*
%!  varname_to_real_var(+RL, -E) is det.
%
%   Converts a variable name (given as an atom RL) to its corresponding Prolog variable.
%   It ensures that the variable is stored in the `cvariable_names` non-backtrackable store.
%   If the variable name is not already stored, it initializes it and retrieves the Prolog variable.
%
%   @arg RL The variable name as a lowercase atom.
%   @arg E The Prolog variable corresponding to the given variable name.
varname_to_real_var(RL, E):-
    upcase_atom(RL, R),  % Convert the variable name to uppercase.
    varname_to_real_var0(R, E).

%!  varname_to_real_var0(+R, -E) is det.
%
%   Internal helper to retrieve or initialize the Prolog variable corresponding to the uppercase name R.
%   It first checks if the `cvariable_names` non-backtrackable store exists, then looks for the variable.
%
%   @arg R The uppercase variable name.
%   @arg E The Prolog variable associated with the name.
varname_to_real_var0(R,E):- nb_current('cvariable_names',VL),!,varname_to_real_var0(R,VL,E).
varname_to_real_var0(R,E):- nb_setval('cvariable_names',[R=v(_)]),!,varname_to_real_var0(R,E).
varname_to_real_var0(R,[],E):- nb_setval('cvariable_names',[R=v(_)]),!,varname_to_real_var0(R,E).
varname_to_real_var0(R,VL,E):- member(N=V,VL), N==R,!,arg(1,V,E).
varname_to_real_var0(R,VL,E):- extend_container(VL,R=v(_)),varname_to_real_var0(R,E).
*/
%!  extend_container(+Container, +Element) is det.
%
%   Extends the list stored in the second argument of the `Container`.
%   The `Element` is added to the front of the list, and the `Container`
%   is updated using `nb_setarg/3` to ensure the modification is non-backtrackable.
%
%   @arg Container The container whose list will be extended.
%   @arg Element The element to add to the list in the container.
extend_container(Container,Element):-
    arg(2,Container,List),                  % Get the current list from the container.
    nb_setarg(2,Container,[Element|List]).  % Add the new element to the list.

%!  rinto_varname(+R, -RN) is det.
%
%   Converts a representation `R` into a Prolog-friendly variable name `RN`.
%   If `R` is numeric, it creates a variable name like `NumN`. If `R` is an atom,
%   it converts it to uppercase.
%
%   @arg R The input representation (either an atom or number).
%   @arg RN The resulting variable name as an atom.
rinto_varname(R,RN):- atom_number(R,N),atom_concat('Num',N,RN).
rinto_varname(R,RN):- upcase_atom(R,RN).         % Convert to uppercase for non-numeric atoms.

%!  real_VL_var(+RL, +VL, -E) is det.
%
%   Maps a variable name `RL` to its corresponding Prolog variable `E` in the variable list `VL`.
%   If `RL` is nonvar, it first converts the name to its canonical form using `rinto_varname/2`.
%   If `E` is a compound term with the form `$VAR`, it attempts to find or create the corresponding variable.
%
%   @arg RL The variable name (atom or a term `$VAR`).
%   @arg VL The variable list, mapping variable names to Prolog variables.
%   @arg E The resulting Prolog variable.
real_VL_var(RL,VL,E):- nonvar(RL),!,rinto_varname(RL, R),!,real_VL_var0(R,VL,E).
real_VL_var(RL,VL,E):- member(N=V,VL),V==E,!,RL=N.      % If the variable exists, return its name.
real_VL_var(RL,VL,E):-
    % If the variable is compound, resolve it.
    compound(E),E = '$VAR'(RL),ignore(real_VL_var0(RL,VL,E)),!.
real_VL_var(RL,VL,E):- format(atom(RL),'~p',[E]),member(N=V,VL),N==RL,!,V=E.
real_VL_var(RL,VL,E):- format(atom(RL),'~p',[E]),real_VL_var0(RL,VL,E).

%!  real_VL_var0(+R, +VL, -E) is det.
%
%   Helper predicate for `real_VL_var/3`, performing the actual lookup of a variable name `R`
%   in the list `VL` and returning the corresponding Prolog variable `E`.
%
%   @arg R The canonical form of the variable name.
%   @arg VL The variable list, mapping names to variables.
%   @arg E The Prolog variable corresponding to the name.
real_VL_var0(R,VL,E):- member(N=V,VL),N==R,!,V=E.  % Lookup the variable.
real_VL_var0(R,VL,E):- extend_container(VL,R=E),!. % If not found, extend the list with the new variable.

%!  pyo_to_pl(+VL, +Par, +Cir, -CirO, +Cl, +O, -E) is det.
%
%   Converts a Python object `O` of class `Cl` to a Prolog term `E`.
%   This predicate handles different cases, such as objects of class `VariableAtom`,
%   and other Python objects that have member values or methods.
%
%   @arg VL The variable list for handling Python-Prolog variable mappings.
%   @arg Par Parameter list (to handle context and recursion).
%   @arg Cir Circular reference list (to avoid infinite loops).
%   @arg CirO Output circular reference list after conversion.
%   @arg Cl The class of the Python object.
%   @arg O The Python object to be converted.
%   @arg E The resulting Prolog term after conversion.

pyo_to_pl(VL,_Par,Cir,Cir,Cl,O,E):- Cl=='VariableAtom',!,py_call_warg(O:get_name(),R),real_VL_var(R,VL,E),!.
pyo_to_pl(VL,Par,Cir,CirO,Cl,O,E):-
    class_to_pl1(Par,Cl,M),py_member_values(O,M,R),!, % Fetch Python object member values.
    py_to_pl(VL,[Cl | Par],Cir,CirO,R,E).             % Recursively convert member values.
pyo_to_pl(VL,Par,Cir,CirO,Cl,O,E):-
    class_to_pl(Par,Cl,M),                            % Find the method for the class conversion.
    py_member_values(O,M,R),!,                        % Get the member values.
    py_to_pl(VL,[Cl | Par],Cir,CirO,R,E).
pyo_to_pl(VL,Par,Cir,CirO,Cl,O,E):-
    catch(py_obj_dir(O,L),_,fail),          % Fetch the object directory (attributes/methods).
    pybug(py_obj_dir(O,L)),                 % Log debug information.
    py_decomp(M),                           % Decompose object.
    meets_dir(L,M),pybug(py_decomp(M)),
    py_member_values(O,M,R),
    member(N-_,Cir),R \== N,!,              % Avoid circular references.
    py_to_pl(VL,[Cl | Par],Cir,CirO,R,E),!.

%This is a more readable fallback we might switch to
%pyo_to_pl(_VL,_Par,Cir,Cir,Cl,O,E):- get_str_rep(O,Str),E=..[Cl,Str].

% Fallback case: If L is not callable, unify E with L.
pyo_to_pl(_VL,_Par,Cir,Cir,_Cl,O,E):- O = E,!.

%!  pl_to_rust(+Var, -Py) is det.
%
%   Converts a Prolog term `Var` to its equivalent Rust/Hyperon representation, returning it as `Py`.
%   This predicate calls `pl_to_rust/3` with an uninitialized variable list.
%
%   @arg Var The Prolog term to be converted.
%   @arg Py The resulting Rust/Hyperon representation.
pl_to_rust(Var,Py):- pl_to_rust(_VL,Var,Py).

%!  pl_to_rust(+VL, +Var, -Py) is det.
%
%   Converts a Prolog term `Var` to its Rust/Hyperon representation, using a variable list `VL`
%   for handling variable conversions. This handles lists, variables, atoms, and strings.
%
%   @arg VL The variable list for tracking variable-to-name mappings.
%   @arg Var The Prolog term to be converted.
%   @arg Py The resulting Rust/Hyperon representation.
% Initialize the variable list if unbound.
pl_to_rust(VL,Var,Py):- var(VL),!,ignore(VL = [vars]),pl_to_rust(VL,Var,Py).
% Convert lists by recursively converting each element to its Rust/Hyperon equivalent.
pl_to_rust(_VL,Sym,Py):- is_list(Sym),!,
    maplist(pl_to_rust,Sym,PyL), % Convert each element in the list.
    py_call(src:'mettalog':'MkExpr'(PyL),Py),!.  % Create a Rust/Hyperon expression from the list.
% Convert Prolog variables to Rust/Hyperon variables.
pl_to_rust(VL,Var,Py):- var(Var),!,
    real_VL_var(Sym,VL,Var), % Get the variable name.
    py_call('hyperon.atoms':'V'(Sym),Py),!.
% Convert Prolog `$VAR` variables to Rust/Hyperon variables.
pl_to_rust(VL,'$VAR'(Sym),Py):- !,
    real_VL_var(Sym,VL,_), % Ensure the variable name is handled correctly.
    py_call('hyperon.atoms':'V'(Sym),Py),!.
% Handle variables encoded with a '$' prefix.
pl_to_rust(VL,DSym,Py):- atom(DSym),
    atom_concat('$',VName,DSym), % Extract the variable name after the '$'.
    rinto_varname(VName,Sym),!,  % Convert the variable name.
    pl_to_rust(VL,'$VAR'(Sym),Py).
% Convert atoms to Rust/Hyperon symbols.
pl_to_rust(_VL,Sym,Py):- atom(Sym),!,py_call('hyperon.atoms':'S'(Sym),Py),!.
%pl_to_rust(VL,Sym,Py):- is_list(Sym),maplist(pl_to_rust,Sym,PyL),py_call('hyperon.atoms':'E'(PyL),Py),!.
% Convert strings to Rust/Hyperon value atoms.
pl_to_rust(_VL,Sym,Py):- string(Sym),!,py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.
% Convert Python objects directly to Rust/Hyperon value atoms.
pl_to_rust(_VL,Sym,Py):- py_is_object(Sym), % Check if the term is a Python object.
    py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.
% Convert any remaining terms to Rust/Hyperon value atoms.
pl_to_rust(_VL,Sym,Py):- py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.

%!  py_list(+MeTTa, -PyList) is det.
%
%   Converts a Prolog term `MeTTa` into a Python list.
%
%   @arg MeTTa The Prolog term to be converted.
%   @arg PyList The resulting Python list.

py_list(MeTTa,PyList):- py_arg(MeTTa,PyList),!. %,
%py_list(MeTTa,PyList):- pl_to_py(MeTTa,PyList).

%!  py_tuple(+O, -Py) is det.
%
%   Converts a list `O` into a Python tuple, returning it as `Py`.
%
%   @arg O The input list to be converted.
%   @arg Py The resulting Python tuple.

py_tuple(O,Py):- py_arg(O,Arg),!,py_obi(py_tuple(Arg),Py),!.
%py_tuple(O,Py):- py_obi(py_tuple(O),Py),!. % Alternative method to create a Python tuple.
% py_tuple(O,Py):- py_ocall(tuple(O),Py),!.  % Call Python tuple function.


%!  py_chain(+O, -Py) is det.
%
%   This converts metta Expression into a list of Python objects , Then OR's them via __or__/__nor__ operator
%   The Python result is returned as `Py` after some processing.
%
%   @arg O The input to the chain (likely some input object).
%   @arg Py The final output resulting from the `py_chain` call.
%
py_chain(I, Py):-
    % First, load the `hyperon_module` to ensure it is available for calling.
    load_hyperon_module,
    % The actual call to the `hyperon_module:py_chain/2` function in Python. This likely passes
    % the argument `I` and returns a result `M`. The `py_ocall/2` mechanism calls the Python method.
    py_ocall_direct(hyperon_module:py_chain(I), M),
    % Finally, the result from the Python call, `M`, is returned via `rust_return/2` to `O`.
    % The `rust_return/2` might be a utility for handling MeTTaLog-Python interop and ensures
    % that `O` receives the final processed result from the chain.
    rust_return(M, Py).


%!  py_dict(+O, -Py) is det.
%
%   Converts a Prolog term `O` into a Python dictionary, returning it as `Py`.
%
%   @arg O The Prolog term to be converted.
%   @arg Py The resulting Python dictionary.
py_dict(O,Py):- catch(py_is_py_dict(O),_,fail),!, % Check if `O` is already a Python dictionary.
    O=Py.                                         % If it is,return it unchanged.
py_dict(O,Py):- py_ocall_direct(dict(O),Py),!.  % Otherwise, convert `O` to a Python dictionary.

% ?- py_list([1,2.0,"string"],X),py_type(X,Y).
% ?- py_list_index([1,2.0,"string"],X),py_type(X,Y).

%!  py_nth(+L, +Nth, -E) is det.
%
%   Retrieves the Nth element from a Python list `L` and unifies it with `E`.
%
%   @arg L The Python list.
%   @arg Nth The index of the element to retrieve.
%   @arg E The resulting element at the specified index.

py_nth(L,Nth,E):- is_list(L),!,nth0(Nth,L,E).
py_nth(L,Nth,E):- py_obi(py_nth(L,Nth),E).

py_nth0(Nth,L,E):- is_list(L),!,nth0(Nth,L,E).
py_nth0(Nth,L,E):- py_obi(py_nth(L,Nth),E).
%py_nth0(Nth,L,E):- py_obi(py_nth(L,Nth),E).
% py_nth(L,Nth,E):- py_obi(py_nth(L,Nth),E).

%!  py_len(+L, -E) is det.
%
%   Retrieves the length of a Python list `L` and unifies it with `E`.
%
%   @arg L The Python list.
%   @arg E The length of the list.
py_len(L,E):- py_mbi(py_len(L),E).

%!  py_o(+O, -Py) is det.
%
%   Converts a Prolog term `O` to its equivalent Python object `Py` using `py_obi`.
%
%   @arg O The Prolog term to be converted.
%   @arg Py The resulting Python object.
py_o(O,Py):- py_obi(identity(O),Py),!.

%!  py_m(+O, -Py) is det.
%
%   Converts a Prolog term `O` to its equivalent Python object `Py` using `py_mbi`.
%
%   @arg O The Prolog term to be converted.
%   @arg Py The resulting Python object.
py_m(O,Py):- py_mbi(identity(O),Py),!.

%!  pl_to_py(+Var, -Py) is det.
%
%   Converts a Prolog term `Var` to its equivalent Python object `Py`.
%
%   @arg Var The Prolog term to be converted.
%   @arg Py The resulting Python object.
pl_to_py(Var,Py):- pl_to_py(_VL,Var,Py).

%!  pl_to_py(+VL, +Var, -Py) is det.
%
%   Converts a Prolog term `Var` to its equivalent Python object `Py`, using a variable list `VL`.
%   This handles lists, variables, atoms, floats, strings, and integers.
%
%   @arg VL The variable list for tracking variable-to-name mappings.
%   @arg Var The Prolog term to be converted.
%   @arg Py The resulting Python object.
pl_to_py(VL,Var,Py):- var(VL),!,ignore(VL = [vars]), % Initialize the variable list if unbound.
    pl_to_py(VL,Var,Py).
% Handle Python objects directly.
pl_to_py(_VL,'#\\'(Sym),Py):- !, to_py_char(Sym,Py).
pl_to_py(_VL,Sym,Py):- py_is_object(Sym),!,Sym = Py.
%pl_to_py(_VL,O,Py):- py_is_dict(O),!,py_obi(identity(O),Py).
% Convert Prolog floats to Python floats.
pl_to_py(_VL,MeTTa,Python):- float(MeTTa),!,py_obi(float_conversion(MeTTa),Python).
% Convert Prolog strings to Python strings.
pl_to_py(_VL,MeTTa,Python):- string(MeTTa),!,py_obi(string_conversion(MeTTa),Python).
% Convert Prolog integers to Python integers.
pl_to_py(_VL,MeTTa,Python):- integer(MeTTa),!,py_obi(int_conversion(MeTTa),Python).
% Convert Prolog lists to Python lists.
pl_to_py(VL,Sym,Py):- is_list(Sym),!,maplist(pl_to_py(VL),Sym,PyL),py_obi(py_list(PyL),Py).
% Convert Prolog variables to Python variables.
pl_to_py(VL,Var,Py):- var(Var),!,real_VL_var(Sym,VL,Var),py_call_warg('hyperon.atoms':'V'(Sym),Py),!.
% Convert Prolog `$VAR` variables to Python variables.
pl_to_py(VL,'$VAR'(Sym),Py):- !,real_VL_var(Sym,VL,_),py_call_warg('hyperon.atoms':'V'(Sym),Py),!.
pl_to_py(_VL,O,Py):-py_type(O,_),!,O=Py.
% % %pl_to_py(_VL,O,Py):- py_is_dict(O),!,O=Py.
%pl_to_py(VL,DSym,Py):- atom(DSym),atom_concat('$',VName,DSym),rinto_varname(VName,Sym),!,pl_to_py(VL,'$VAR'(Sym),Py).
%pl_to_py(_VL,Sym,Py):- atom(Sym),!,py_call_warg('hyperon.atoms':'S'(Sym),Py),!.
%pl_to_py(_VL,Sym,Py):- string(Sym),!,py_call_warg('hyperon.atoms':'S'(Sym),Py),!.
%pl_to_py(VL,Sym,Py):- is_list(Sym),maplist(pl_to_py,Sym,PyL),py_call_warg('hyperon.atoms':'E'(PyL),Py),!.
%pl_to_py(_VL,Sym,Py):- py_is_object(Sym),py_call_warg('hyperon.atoms':'ValueAtom'(Sym),Py),!.
% Default case: return the input term as-is if no other conversion applies.
pl_to_py(_VL,MeTTa,MeTTa).
%pl_to_py(_VL,Sym,Py):- py_call_warg('hyperon.atoms':'ValueAtom'(Sym),Py),!.

%!  py_key(+O, -I) is det.
%
%   Retrieves the key from a Python dictionary-like object `O` and unifies it with `I`.
%
%   @arg O The Python object.
%   @arg I The resulting key.
py_key(O,I):- py_m(O,M),key(M,I).

%!  py_items(+O, -I) is det.
%
%   Retrieves the items from a Python dictionary-like object `O` and unifies them with `I`.
%
%   @arg O The Python object.
%   @arg I The resulting items.
py_items(O,I):- py_m(O,M),items(M, I).

%!  py_values(+O, -K, -V) is det.
%
%   Retrieves the key-value pairs from a Python dictionary-like object `O`.
%   The pairs are returned as `K` (key) and `V` (value).
%
%   @arg O The Python object.
%   @arg K The key from the dictionary.
%   @arg V The value corresponding to the key.

%py_values(O,K,V):- py_m(O,M),values(M,K,V).
py_values(O,K,V):- py_items(O,L),member(K:V,L).

%elements(Atoms,E):- is_list(Atoms),!,

%!  meets_dir(+L, +M) is semidet.
%
%   Checks if a term `M` meets the structure defined in the list `L`.
%   If `M` is an atom, it checks for membership in `L`. If `M` is a list or compound,
%   it checks if each element of `M` matches an element in `L`.
%
%   @arg L The list of atoms or terms.
%   @arg M The term to be checked against `L`.
meets_dir(L,M):- atom(M),!,member(M,L),!.
meets_dir(L,M):- is_list(M),!,maplist(meets_dir(L),M).
meets_dir(L,M):- compound_name_arity(M,N,0),!,member(N,L),!.
meets_dir(L,M):- compound(M),!,compound_name_arguments(M,F,[A | AL]),!,maplist(meets_dir(L),[F,A | AL]).

%!  py_member_values(+O, +C, -R) is det.
%
%   Retrieves member values from a Python object `O` based on class `C` and unifies them with `R`.
%   Handles cases where `O` or `C` are lists, or where `C` is a function with arguments.
%
%   @arg O The Python object.
%   @arg C The class or method to retrieve the value from.
%   @arg R The resulting member values.
py_member_values(O,C,R):- is_list(O),!,maplist(py_member_values,O,C,R).
py_member_values(O,C,R):- is_list(C),!,maplist(py_member_values(O),C,R).
%py_member_values(O,C,R):- atom(C),!,compound_name_arity(CC,C,0),!,py_call_warg(O:CC,R).
py_member_values(O,f(F,AL),R):- !,py_member_values(O,[F | AL],[RF | RAL]),
    compound_name_arguments(R,RF,RAL).
py_member_values(O,C,R):- py_call_warg(O:C,R,[py_string_as(atom),py_object(false)]).

%!  py_to_str(+PyObj, -Str) is det.
%
%   Converts a Python object `PyObj` to its string representation `Str`.
%
%   @arg PyObj The Python object to convert.
%   @arg Str The resulting string representation.
py_to_str(PyObj,Str):- with_output_to(string(Str),py_pp(PyObj,[nl(false)])).

%!  tafs is det.
%
%   A test predicate that retrieves atoms from a space, converts between Python and Prolog representations,
%   and prints the results.
tafs:-
    atoms_from_space(Space,_),
    py_to_pl(VL,Space,AA),
    ppt(aa(Pl,aa)),
    pl_to_rust(VL,AA,Py),
    ppt(py(Pl,py)),
    pl_to_rust(VL,Py,Pl),
    ppt(pl(Pl,pl)),
    atoms_from_space(Space,[A]),
    py_to_pl(VL,A,AA),
    atoms_from_space(Space,[A]),
    py_obj_dir(A,D),
    writeq(D),
    !,py_to_pl(VL,D:get_object(),AA),
    writeq(AA),
    !,fail.

%!  py_class(+A,-AA) is det.
%
%   Retrieves the class name of a Python object `A` and unifies it with `AA`.
%
%   @arg A The Python object.
%   @arg AA The name of the class.
py_class(A,AA):- py_call_warg(A:'__class__',C),py_call_warg(C:'__name__',AA,[py_string_as(atom)]),!.

%!  py_decomp(+M,+C) is det.
%
%   Decomposes a compound term `M` and checks if it matches class `C`.
%
%   @arg M The compound term to decompose.
%   @arg C The class to check against.
py_decomp(M,C):- py_decomp(M),compound_name_arity(C,M,0).

%!  class_to_pl1(+Par,+Class,-Method) is det.
%
%   Defines the method mapping for converting various Python classes into Prolog terms.
%
%   @arg Par Parameters (ignored in most cases).
%   @arg Class The Python class to be mapped.
%   @arg Method The method associated with the class for conversion.
class_to_pl1(_Par,'GroundingSpaceRef',get_atoms()).
class_to_pl1(_Par,'ExpressionAtom',get_children()).
class_to_pl1(_Par,'SpaceRef',get_atoms()).
class_to_pl1(_Par,'VariableAtom','__repr__'()).
class_to_pl1(_Par,'SymbolAtom',get_name()).
class_to_pl1(_Par,'bool','__repr__'()).

%!  class_to_pl(+Par, +Class, -Method) is det.
%
%   Defines additional method mappings for Python classes.
%
%   @arg Par Parameters (used for length-based decisions).
%   @arg Class The Python class to be mapped.
%   @arg Method The method associated with the class for conversion.
class_to_pl(_Par,'ValueAtom','__repr__'()).
class_to_pl(_Par,'ValueObject','value').
class_to_pl(Par,'GroundedAtom','__repr__'()):- length(Par,Len),Len>=5,!.
class_to_pl(Par,_,'__str__'()):- length(Par,Len),Len>15,!.
class_to_pl(_Par,'GroundedAtom',get_object()).

/*


class_to_pl(Par,'bool','__repr__'()).

*/

%!  py_decomp(+Method) is det.
%
%   Declares methods and functions that are commonly used in Python object deconstruction.
%   These methods are used to inspect and convert Python objects into Prolog terms.
%
%   @arg Method The method or function to be used during object deconstruction.
py_decomp('__repr__'()).
py_decomp('__str__'()).
py_decomp(get_atoms()).
py_decomp(get_children()).
py_decomp(get_object()).
py_decomp(get_name()).
py_decomp(value()).
py_decomp('__class__':'__name__').
%py_decomp(f(get_grounded_type(),['__str__'()])).
py_decomp(f('__class__',['__str__'()])).

%__class__
%get_type()

%atoms_from_space(Space,[Atoms]),py_pp(Atoms),py_call_warg(Atoms:get_object(),A),atoms_from_space(A,Dir),member(E,Dir),py_obj_dir(E,C),py_call_warg(E:get_children(),CH),py_pp(CH).

%!  remove_from_space(+Space, +Sym) is det.
%
%   Removes an atom `Sym` from the specified `GroundingSpace`.
%   This uses Python bindings to call the `remove` method on the `GroundingSpace` object.
%
%   @arg Space The grounding space from which the atom will be removed.
%   @arg Sym The atom to be removed from the space.
 :- if(\+ current_predicate(remove_from_space/2)).
remove_from_space(Space,Sym):- ensure_space(Space,GSpace),py_call_warg(GSpace:'remove'(Sym),_).
 :- endif.

%!  add_to_space(+Space, +Sym) is det.
%
%   Adds an atom `Sym` to the specified `GroundingSpace`.
%   This uses Python bindings to call the `add` method on the `GroundingSpace` object.
%
%   @arg Space The grounding space to which the atom will be added.
%   @arg Sym The atom to be added to the space.
 :- if(\+ current_predicate(add_to_space/2)).
add_to_space(Space,Sym):-  ensure_space(Space,GSpace),py_call_warg(GSpace:'add'(Sym),_).
 :- endif.

%!  must_det_llp(+Goals) is det.
%
%   Executes a list of goals deterministically, ensuring each goal is fully resolved.
%   If `Goals` consists of a conjunction, each part is processed separately.
%   Any debugging information is printed using `pybug/1`.
%
%   @arg Goals The list of goals to be executed deterministically.
must_det_llp((A,B)):- !,must_det_llp(A),must_det_llp(B).

must_det_llp(B):- pybug(B),!,once(ignore(must_det_ll(B))).

% dynamic allows modifications at runtime.
:- dynamic (is_pymod_in_space/2).
:- dynamic (is_pymod_loaded/2).

%!  py_ready is semidet.
%
%   Checks if Python integration is ready by verifying the current state.
%   This predicate succeeds if the nb_current/2 term indicates that the Python
%   integration is ready, or if it determines that the mettalog system is not active.
%   It fails otherwise.
%
%   @example Check if Python is ready:
%       ?- py_ready.
%
py_ready:- nb_current('$py_ready','true'),!.
py_ready:- \+ is_mettalog(_),!,fail.
% py_ready:- is_metta(_),!.
py_ready.

%!  pybug(+P) is det.
%
%   Logs the given term P using fbug/1, with an additional check for Python readiness.
%   If Python is not ready (checked by py_ready/0), it directly logs P as a bug.
%   Otherwise, it will attempt to process P using py_pp/1 or py_to_pl/2 before logging.
%
%   @arg P The term to be logged as a bug.
%
%   @example Log a bug if Python is not ready:
%       ?- pybug('Some error message').
%
%   @example Log a processed bug using py_pp:
%       ?- pybug(P).
%
%   @see fbug/1 for the underlying logging mechanism.
%
%   @see py_ready/0 for checking the readiness of Python integration.
%
%   @see py_to_pl/2 for converting Python terms to Prolog.
%

%pybug(P):- py_pp(P),!.
pybug(P):- \+ py_ready,!,fbug(P).
pybug(P):- fbug(P).

%!  pypp(+P) is det.
%
%   Converts a Python term P to a Prolog term using py_to_pl/2, then logs it.
%   If the conversion fails, it logs the original term P.
%
%   @arg P The Python term to be converted and logged.
%
%   @example Log a Python term after conversion:
%       ?- pypp(PythonTerm).
%
pypp(P):- py_to_pl(P,PL),!,fbug(PL),!.
pypp(P):- fbug(P),!.

%!  'extend-py!'(+Module, -R) is det.
%
%   Extends the current environment with the specified Python module.
%   Calls the extend_py/2 predicate to integrate the Python module into the system.
%
%   @arg Module The name of the Python module to be integrated.
%   @arg R      The result of the extension process (typically an empty list).
%
%   @example Extend the environment with a Python module:
%       ?- 'extend-py!'('some_module', R).
%
'extend-py!'(Module,R):- (notrace((extend_py(Module,R)))).

%!  extend_py(+Module, -R) is det.
%
%   Integrates the specified Python module into the current environment.
%   It attempts to extend the current Prolog environment by calling self_extend_py/4,
%   which handles the actual module loading and integration.
%
%   @arg Module The name of the Python module to be integrated.
%   @arg R      The result of the extension process (typically an empty list).
%
%   @example Integrate a Python module:
%       ?- extend_py('my_python_module',R).
%
extend_py(Module,R):- current_self(Self),self_extend_py(Self,Module,_Base, R).

%!  self_extend_py(+Self, +Module) is det.
%
%   A simplified form of self_extend_py/4 that does not handle the base or result.
%
%   @arg Self   The current environment or context.
%   @arg Module The name of the Python module to be integrated.
%
%   @example Extend with a module using the current self context:
%       ?- self_extend_py(Self,'module_name').
%
self_extend_py(Self,Module):- self_extend_py(Self,Module,_Base,_).

%!  self_extend_py(+Self, +Module, +File, -R) is det.
%
%   Integrates a Python module into the current context by loading the specified file
%   or module. It sets the '$py_ready' flag after loading the module successfully.
%   This is done in a safe execution environment using with_safe_argv/1.
%
%   @arg Self   The current environment or context.
%   @arg Module The name of the Python module to be integrated.
%   @arg File   The file to be loaded (if available), otherwise the module is loaded.
%   @arg R      The result of the operation (typically an empty list).
%
%   @example Extend the environment with a Python module from a file:
%       ?- self_extend_py(Self,'module_name','file_name',R).
%
self_extend_py(Self,Module,File,R):-
    with_safe_argv((
        assert_new(is_pymod_in_space(Module,Self)),
        (nonvar(File) -> Use = File ; Use = Module),
        pybug('extend-py!'(Use)),
        % py_call_warg(mettalog:use_mettalog()),
        (assumed_loaded(Use) -> true ; py_load_modfile(Use)),
        % listing(ensure_rust_metta/1),
        % ensure_mettalog_py,
        nb_setval('$py_ready','true'),
        % working_directory(PWD,PWD),maybe_py_add_lib_dir(PWD),
        % replace_in_string(["/"="."],Module,ToPython),
        % py_mcall(mettalog:import_module_to_rust(ToPython)),
        % sformat(S,'!(import! &self ~w)',[Use]),rust_metta_exec(S,R),
        R = [],
        % py_module_exists(Module),
        % py_call_warg(MeTTa:load_py_module(ToPython),Result),
        true)),!.


assumed_loaded(mettalog).
assumed_loaded(random).

%!  py_load_modfile(+Use) is det.
%
%   Loads a Python module file into the current environment by calling the mettalog
%   interface through py_ocall/2. If the file is successfully loaded, the result is
%   logged using pybug/1. This predicate also handles the case where the provided Use
%   is a directory by recursively trying to load an '_init_.py' file from the directory.
%
%   @arg Use The module name or file path to be loaded.
%
%   @example Load a Python module file:
%       ?- py_load_modfile('path_to_python_file.py').
%
py_load_modfile(Use):- py_ocall(mettalog:load_functions(Use),R),!,pybug(R).
py_load_modfile(Use):- exists_directory(Use),!,directory_file_path(Use,'_init_.py',File),
    py_load_modfile(File).
py_load_modfile(Use):- file_to_modname(Use,Mod),read_file_to_string(Use,Src,[]),!,py_module(Mod,Src).

%!  file_to_modname(+Filename, -ModName) is det.
%
%   Converts a given file path or filename into a Python module name by replacing
%   certain path components and extensions. This handles several common filename patterns
%   like '../', './', '_init_.py', and '.py'.
%
%   @arg Filename The file path or name to be converted.
%   @arg ModName  The resulting Python module name.
%
%   @example Convert a file path to a module name:
%       ?- file_to_modname('./some_module.py',ModName).
%
file_to_modname(Filename,ModName):- symbol_concat('../',Name,Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- symbol_concat('./',Name,Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- symbol_concat(Name,'/_init_.py',Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- symbol_concat(Name,'.py',Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- replace_in_string(["/"="."],Filename,ModName).

%import_module_to_rust(ToPython):- sformat(S,'!(import! &self ~w)',[ToPython]),rust_metta_exec(S).

%!  rust_metta_run(+S,-Run) is det.
%
%   Executes a metta command in the Rust environment by calling rust_metta_exec/2.
%   If the command S is a variable, the execution is delayed using freeze/2 until S is
%   instantiated.
%
%   @arg S   The metta command to be executed, as a string.
%   @arg Run The result of the execution.
%
%   @example Execute a command in Rust metta:
%       ?- rust_metta_run('command_string', Result).
%
rust_metta_run(S,Run):- var(S),!,freeze(S,rust_metta_run(S,Run)).
rust_metta_run(RR,Res):- rust_metta_exec(RR,L),!,member(ResL,L),member(Res,ResL).


%!  rust_metta_exec(+I, -Run) is det.
%
%   The actual implementation of rust_metta_run/2 that handles executing the Rust
%   metta command via py_ocall/2. It loads the hyperon module and converts the result
%   to a Prolog term using rust_return/2.
%
%   @arg I   The input command to be executed in Rust.
%   @arg Run The result of the execution.
%
%   @example Run a command in Rust and retrieve the result:
%       ?- rust_metta_exec('some_command',Result).
%
rust_metta_exec(S,Run):- var(S),!,freeze(S,rust_metta_exec(S,Run)).
%rust_metta_exec(exec(S),Run):- \+ callable(S),string_concat('!',S,SS),!,rust_metta_exec(SS,Run).
rust_metta_exec(S,Run):- coerce_string(S,R),!,rust_metta_exec1(R,Run).
%rust_metta_exec(I,O):-

rust_metta_exec1(I,O):- ensure_py_loaded(hyperon_module),!,py_ocall(hyperon_module:rust_metta_exec(I),M),!,
    rust_return(M,O).
rust_metta_exec1(R,Run):-
    with_safe_argv((((
        % ensure_rust_metta(MeTTa),
        py_call_warg(mettalog:rust_metta_run(R),Run)
    )))).

%!  rust_return(+M, -O) is det.
%
%   Processes the result M of a Rust metta run and converts it into a Prolog term O.
%   If M is an iterable, it tries to convert it to a Prolog list or single value.
%
%   @arg M The result from Rust, typically a Python object or iterable.
%   @arg O The resulting Prolog term.
%
%   @example Convert the result of a Rust metta run:
%       ?- rust_return(M,O).
%

rust_return(M,O):-
    (py_iter(M,R,[py_object(true)]),py_iter(R,R1,[py_object(true)]))*->rust_to_pl(R1,O);
    (fail,rust_to_pl(M,O)).
%rust_return(M,O):- rust_to_pl(M,O).
%rust_return(M,O):- py_iter(M,R,[py_object(true)]),rust_to_pl(R,O).
%rust_return(M,O):- py_iter(M,O). %,delist1(R,O).

%!  delist1(+R,-Result) is det.
%
%   Removes a list wrapper around a single result. If the list contains only one element,
%   it unifies the result with that element. Otherwise, it leaves the result unchanged.
%
%   @arg R       The input list or single value.
%   @arg Result  The output, either the single value or the original list.
%
%   @example Handle a single-element list:
%       ?- delist1([42], Result).
%
delist1([R],R):- !.
delist1(R,R).  % Optionally, log a warning here if necessary.

%!  rust_to_pl(+L, -P) is det.
%
%   Converts a Rust structure (or Python object) into a Prolog term. Handles various
%   types of input such as lists, compounds, atoms, and Python objects.
%   It recursively processes lists and compounds, ensuring that the Rust types
%   are mapped to Prolog equivalents.
%
%   @arg L The input term from Rust (or Python).
%   @arg P The output Prolog term.
%
%   @example Convert a list of Rust objects to Prolog:
%       ?- rust_to_pl([rust_atom1, rust_atom2], PrologTerm).
%
rust_to_pl(L,P):- var(L),!,L=P.
% nb_current(last_debug_info, debug_info(always(porting), hyperon_throws_error(should_be(number, W)))), py_call(W:get_object(),O),py_call(O:value,OO).
% rust_to_pl([],P):- !,P=[].
rust_to_pl(L,P):- is_list(L),!,maplist(rust_to_pl,L,P).
rust_to_pl(R,P):- compound(R),!,compound_name_arguments(R,F,RR),maplist(rust_to_pl,RR,PP),
    compound_name_arguments(P,F,PP).
rust_to_pl(R,P):- \+ py_is_object(R),!,P = R.

% rust_to_pl(R,PT):- py_type(R,'GroundedAtom'), py_call(W:get_grounded_type(),O), py_call(repr(O),OO).

rust_to_pl(R,P):- py_type(R,'ValueObject'),py_ocall(R:'value',L),!,rust_to_pl(L,P).
rust_to_pl(R,PT):-
            py_type(R,'GroundedAtom'),
            py_ocall(R:get_grounded_type(),T),
            rust_to_pl(T,TT),
            py_ocall(R:get_object(),L), L\==R,
            rust_to_pl(L,P),
            combine_term_l(TT,P,PT), !.
rust_to_pl(R,P):- py_type(R,'ExpressionAtom'),py_mcall(R:get_children(),L),!,maplist(rust_to_pl,L,P).
rust_to_pl(R,P):- py_type(R,'SymbolAtom'),py_acall(R:get_name(),P),!.
rust_to_pl(R,P):- py_type(R,'VariableAtom'),py_scall(R:get_name(),N),!,as_var(N,P),!.
%rust_to_pl(R,P):- py_type(R,'VariableAtom'),py_acall(R:get_name(),N),!,atom_concat('$',N,P).
rust_to_pl(R,N):- py_type(R,'OperationObject'),py_acall(R:name(),N),!,cache_op(N,R).
rust_to_pl(R,P):- py_type(R,'SpaceRef'),!,P = R.
rust_to_pl(R,P):- py_is_list(R),py_m(R,L),R \== L,!,rust_to_pl(L,P).
rust_to_pl(R,PT):- py_type(R,T),combine_term_l(T,R,PT),!.
%rust_to_pl(R,P):- py_acall(R:'__repr__'(),P),!.
rust_to_pl(R,P):- load_hyperon_module,!,py_ocall(hyperon_module:rust_deref(R),M),!,
    (R \== M -> rust_to_pl(M,P) ; M = P).

to_py_char(R,Py):- load_hyperon_module,!,py_ocall(hyperon_module:rust_to_py_char(R),Py),!.

%!  as_var(+N,-Var) is det.
%
%   Converts a Rust variable name N into a Prolog variable format. If the name is '_',
%   it represents an anonymous variable. Otherwise, it converts the name into the
%   '$VAR'(S) format, where S is the string representation of the variable name.
%
%   @arg N   The variable name from Rust.
%   @arg Var The corresponding Prolog variable.
%
%   @example Convert a Rust variable name to a Prolog variable:
%       ?- as_var('X', PrologVar).
%
as_var('_',_):- !.
as_var(N,'$VAR'(S)):- sformat(S,'_~w',[N]),!.

%!  rust_metta_exec(+S) is det.
%
%   Executes a metta command S in the Rust environment and prints the result.
%   After executing the command, it converts the Python object result into a Prolog
%   representation and prints it.
%
%   @arg S The metta command to be executed.
%
%   @example Run a command in the Rust metta environment:
%       ?- rust_metta_exec('command_string').
%
rust_metta_exec(S):-rust_metta_exec(S,Py),print_py(Py).

 :-volatile(cached_py_op/2).

%!  cache_op(+N, +R) is det.
%
%   Caches the operation N with the corresponding Python object R. The cache is made
%   persistent using the cached_py_op/2 predicate.
%
%   @arg N The name of the operation.
%   @arg R The corresponding Python object.
%
cache_op(N,R):- asserta_if_new(cached_py_op(N,R)),fbug(cached_py_op(N,R)).

 :-volatile(cached_py_type/2).

%!  cache_type(+N,+R) is det.
%
%   Caches the type N with the corresponding Python object R. The cache is made
%   persistent using the cached_py_type/2 predicate.
%
%   @arg N The name of the type.
%   @arg R The corresponding Python object.
%
cache_type(N,R):- asserta_if_new(cached_py_type(N,R)),fbug(cached_py_type(N,R)).

%!  print_py(+Py) is det.
%
%   Converts a Python object Py to a Prolog term and prints it. It uses py_to_pl/2
%   to handle the conversion before printing.
%
%   @arg Py The Python object to be converted and printed.
%
%   @example Convert and print a Python object:
%       ?- print_py(PyObject).
%
print_py(Py):- py_to_pl(Py,R), pp(R).

%!  combine_term_l(+Type, +RustTerm, -PrologTerm) is det.
%
%   Combines a Rust type and term into a Prolog term. This predicate handles different
%   Rust types such as 'OperationObject', 'Number', 'Bool', and others, mapping them
%   to appropriate Prolog structures. If the Rust term matches specific types, it
%   passes the term through unchanged. For other types, it wraps the term in a structure
%   indicating its type.
%
%   @arg Type       The type of the Rust term (e.g., 'OperationObject', 'Number').
%   @arg RustTerm   The original term from Rust.
%   @arg PrologTerm The resulting Prolog term.
%
%   @example Combine a Rust term and type into a Prolog structure:
%       ?- combine_term_l('Number', 42, PrologTerm).
%
combine_term_l('OperationObject',P,P):-!.
combine_term_l(_,P,V):- py_is_object(P), py_type(P,'ValueObject'),py_call(P:value,V),!.
combine_term_l('Bool',P,P):-!.
combine_term_l('Number',P,P):-!.
combine_term_l('ValueObject',R,P):-R=P,!. %rust_to_pl(R,P),!.
combine_term_l('%Undefined%',R,P):-rust_to_pl(R,P),!.
combine_term_l('hyperon::space::DynSpace',P,P):-!.
combine_term_l([Ar|Stuff],Op,Op):- Ar == (->),!,cache_type(Op,[Ar|Stuff]).
combine_term_l(T,P,ga(P,T)).

%!  coerce_string(+S, -R) is det.
%
%   Converts a term S to a string if necessary. If S is already a string, it is unified
%   with R. If it is not a string, it converts S to a string using write_src/1.
%
%   @arg S The input term, either an atom, string, or other type.
%   @arg R The resulting string.
%
%   @example Coerce a term to a string:
%       ?- coerce_string(123, Result).
%

%coerce_string(S,R):- atom(S),sformat(R,'~w',[S]),!.
coerce_string(S,R):- string(S),!,S=R.
coerce_string(S,R):- with_output_to(string(R),write_src_woi(S)),!.

%!  load_functions_motto is det.
%
%   Loads functions related to the "motto" in the current context by calling the Python
%   mettalog interface. The result of the function call is processed using pypp/1.
%
%   @example Load functions for the motto:
%       ?- load_functions_motto.
%
load_functions_motto:- load_functions_motto(Def),pypp(Def).

%!  load_functions_motto(-Def) is det.
%
%   Loads the "motto" functions using the mettalog interface. The result is returned
%   as Def and processed in the Prolog context.
%
%   @arg Def The definition returned by the mettalog interface.
%
%   @example Load and retrieve motto functions:
%       ?- load_functions_motto(Def).
%
load_functions_motto(Def):- load_functions_ext,
    with_safe_argv(py_call_warg(mettalog:load_functions_motto(),Def)).

%!  load_functions_ext is det.
%
%   Loads external functions in the current context by calling the mettalog interface.
%   The result is processed using pypp/1.
%
%   @example Load external functions:
%       ?- load_functions_ext.
%
load_functions_ext:- load_functions_ext(Def),pypp(Def).

%!  load_functions_ext(-Def) is det.
%
%   Loads external functions using the mettalog interface. The result is returned
%   as Def and processed in the Prolog context.
%
%   @arg Def The definition returned by the mettalog interface.
%
%   @example Load and retrieve external functions:
%       ?- load_functions_ext(Def).
%
load_functions_ext(Def):- with_safe_argv(py_call_warg(mettalog:load_functions_ext(),Def)).

%!  example_usage is det.
%
%   Demonstrates an example of using metta space to perform a query. It ensures that
%   the primary metta space is available, runs a query, and prints the result.
%
%   @example Perform an example query in metta space:
%       ?- example_usage.
%
example_usage:-
    with_safe_argv(ensure_primary_metta_space(GSpace)),
    %some_query(Query),
    Query = [],with_safe_argv(query_from_space(GSpace,Query,Result)),writeln(Result).


%atoms_from_space(Sym):-  atoms_iter_from_space(metta_self,Atoms),py_iter(Atoms,Sym).

%!  atom_count_from_space(-Count) is det.
%
%   Retrieves the count of atoms in the metta space. The count is returned as Count.
%
%   @arg Count The number of atoms in the space.
%
%   @example Retrieve the atom count from the space:
%       ?- atom_count_from_space(Count).
%

atom_count_from_space(Count):- atom_count_from_space(metta_self,Count).

:-dynamic(wanted_py_lib_dir/1).
% :-prolog_load_context(directory,ChildDir),
%    file_directory_name(ChildDir,ParentDir),
%    file_directory_name(ParentDir,GParentDir),
%    pfcAdd_Now(wanted_py_lib_dir(GParentDir)).

%:- .
%:- ensure_rust_metta.
%:- with_safe_argv(ensure_primary_metta_space(_GSpace)).
/*
Rust: The core of MeTTa is implemented in Rust, which provides performance and safety features.

Python Extensions: Python is used for extending the core functionalities. Python communicates with
Rust via a Foreign Function Interface (FFI) or similar mechanisms.

Prolog: The Prolog code is an additional layer that allows you to extend or customize parts of
MeTTa using Python and Rust. It maintains the system's extensibility.

VSpace is a space with its backend in Prolog, it implies that you're using Prolog's logic
programming capabilities to manage and manipulate a particular domain, which in this context
is referred to as a "space" (possibly akin to the GroundingSpace in Python, but implemented in Prolog).

To integrate VSpace with the existing Python and Rust components, similar interfacing
techniques could be used. You could expose Prolog predicates as functions that can be
called from Python or Rust, and likewise, call Python or Rust functions from within Prolog.


*/

% when using this file alone uncomment the next line
%:- ensure_loaded(metta_interp).

%!  add_wanted_py_lib_dirs is det.
%
%   Ensures that Python library directories are added to the Python path.
%   It retrieves all directories from wanted_py_lib_dir/1 facts and adds them
%   using maybe_py_add_lib_dir/1. After that, it synchronizes the Python path.
%
%   @example Ensure Python library directories are added:
%       ?- add_wanted_py_lib_dirs.
%
add_wanted_py_lib_dirs:-
    with_safe_argv((
        forall(wanted_py_lib_dir(GParentDir),ignore(catch(maybe_py_add_lib_dir(GParentDir),_,true))),
        sync_python_path
    )).


maybe_py_add_lib_dir(Path):- is_list(Path),!,maplist(maybe_py_add_lib_dir,Path).
maybe_py_add_lib_dir(Path):- atom_length(Path,0),!.
maybe_py_add_lib_dir(Path):-
  absolute_file_name(Path,ABS2),
  py_lib_dirs(Dirs),member(Dir,Dirs),
  absolute_file_name(Dir,ABS1),
  ABS1=ABS2,!.
maybe_py_add_lib_dir(Path):- string(Path),atom_string(APath,Path),!,maybe_py_add_lib_dir(APath).
maybe_py_add_lib_dir(Path):- \+ atom(Path),!.
maybe_py_add_lib_dir(Path):- \+ exists_directory(Path),!.
maybe_py_add_lib_dir(Path):- py_add_lib_dir(Path,first),!.



find_mettalog_relative_path(RelDir, AbsPath) :-
    % Get the environment variable METTALOG_DIR
    metta_root_dir(Dir),
    absolute_file_name(RelDir, AbsPath, [relative_to(Dir),file_type(directory), access(exist)]).

maybe_py_add_relative_lib_dir(RelDir):-
 ignore((find_mettalog_relative_path(RelDir, AbsPath),maybe_py_add_lib_dir(AbsPath))).

%!  sync_python_path is det.
%
%   Synchronizes the Python path by ensuring that all relevant directories
%   are added to the 'PYTHONPATH' environment variable. It checks the current
%   Python path and updates it with directories from the Prolog environment
%   if necessary.
%
%   @example Synchronize the Python path:
%       ?- sync_python_path.
%
sync_python_path:- !.
really_sync_python_path:-
    working_directory(PWD,PWD),maybe_py_add_lib_dir(PWD),
    ignore((
        getenv('PYTHONPATH',CurrentPythonPath),
        symbolic_list_concat(List,':',CurrentPythonPath),
        list_to_set(List,Set),
        maybe_py_add_lib_dir(Set))),
    maybe_py_add_relative_lib_dir('./python/'),
    %maybe_py_add_relative_lib_dir('../hyperon-experimental/python'),
    %py_call(sys:path,SP), write_src_nl(py_call('sys.path',SP)),
    py_lib_dirs(DirsL),list_to_set(DirsL,Dirs),
    symbolic_list_concat(Dirs,':',NewPythonPath),
    setenv('PYTHONPATH',NewPythonPath),
    catch(try_resolve_python_modules,E,(show_python_path,write_src_nl(E))).
    %write_src_nl(py_lib_dirs(NewPythonPath)),

try_resolve_python_modules:-
    %py_call(hyperon,_),
    %py_call(hyperonpy,_),
    %py_call(mettalog,_),
    !.

show_python_path:-
  py_call(sys:path,SP), write_src_nl(py_call('sys.path',SP)).

%!  is_rust_operation(+List) is semidet.
%
%   Checks if a given list represents a Rust operation by calling the mettalog interface
%   to verify if the function (first element of the list) with the given arity (the length
%   of the remaining list elements) is a defined operation.
%
%   @arg List The list representing a possible Rust operation.
%
%   @example Check if a list is a Rust operation:
%       ?- is_rust_operation([function_name,arg1,arg2]).
%
is_rust_operation([Fun | Args]):- get_list_arity(Args,Arity),
    py_call_warg(mettalog:get_operation_definition_with_arity(Fun,Arity),O),O \== '@'('none').

%!  get_list_arity(+Args,-Arity) is det.
%
%   Calculates the arity (length) of the given list of arguments.
%   If the argument is not a list, it returns -1.
%
%   @arg Args  The list of arguments.
%   @arg Arity The calculated arity of the list or -1 if not a list.
%
%   @example Calculate the arity of a list:
%       ?- get_list_arity([arg1, arg2], Arity).
%
get_list_arity(Args,Arity):- is_list(Args),!,length(Args,Arity).
get_list_arity(_Args,-1).

% Set various Prolog flags to control how debugging and answers are written to the output.

% Set the debugger write options.
% These options include displaying terms with quotes, using portray/1 to show terms,
% and limiting the output depth to 60. Additionally, attributes of variables are
% portrayed, and spacing is set to show the next argument on the same line.
:- initialization(set_prolog_flag(debugger_write_options,[quoted(true),portray(true),max_depth(60),attributes(portray),
    spacing(next_argument)
])).

% Set the answer write options.
% These options are similar to the debugger write options and control how answers
% are printed in the REPL. Answers are quoted, portrayed, limited to 60 levels of depth,
% and attributes of variables are portrayed, with spacing between arguments.
:- initialization(set_prolog_flag(answer_write_options,[quoted(true),portray(true),max_depth(60),attributes(portray),
    spacing(next_argument)
])).

% Set a flag to limit the depth of Python backtraces to 50 levels.
:- initialization(set_prolog_flag(py_backtrace_depth,50)).

% Enable Python backtrace support.
:- initialization(set_prolog_flag(py_backtrace,true)).

% Set the default Python argument vector to an empty list.
:- initialization(set_prolog_flag(py_argv,[])).

% Register initialization hooks that will be called during system after_load /**/.
% These predicates will be invoked when the system is restored from a saved state.
:- initialization(on_restore1).
:- initialization(on_restore2).



%!  on_restore1 is det.
%
%   Called during Prolog initialization to ensure the Python integration with
%   mettalog is properly set up by calling ensure_mettalog_py/0.
%
%   @example Initialize mettalog Python integration:
%       ?- on_restore1.
%
on_restore1:-
    ensure_mettalog_py.


on_restore2:- !.
%on_restore2:- load_builtin_module.
%:- load_hyperon_module.

%!  subst_each_var(+Vars, +Term, -Output) is det.
%
%   Replaces each variable in the list Vars with an anonymous variable (_) in the
%   given Term, producing the Output term. This process substitutes variables one
%   by one, ensuring all occurrences of each variable are replaced.
%
%   @arg Vars   The list of variables to be substituted.
%   @arg Term   The input term where variables are replaced.
%   @arg Output The resulting term after substitutions.
%
%   @example Substitute variables with anonymous variables:
%       ?- subst_each_var([X, Y], term(X, Y), Output).
%

% grab the 1st variable Var
subst_each_var([Var|RestOfVars],Term,Output):- !,
    % replace all occurences of Var with _ (Which is a new anonymous variable)
    subst(Term,Var,_ ,Mid),
    % Do the RestOfVars
    subst_each_var(RestOfVars,Mid,Output).

% no more vars left to replace
subst_each_var(_,TermIO,TermIO).

py_portray(O):- py_is_object(O),write(' '),py_ppp(O),!.
% py_portray(O):- is_list(O),write(' '),write_src(O),!.
py_portray(O):- py_is_py(O),write(' '),py_ppp(O),!.

:- dynamic(user:portray/1).
:- multifile(user:portray/1).
%user:portray(O):- nonvar(O), py_portray(O),!,write(' ').

