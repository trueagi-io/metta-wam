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

:- encoding(iso_latin_1).
:- flush_output.
:- setenv('RUST_BACKTRACE',full).
%:- '$set_source_module'('user').
:- set_prolog_flag(py_backtrace_depth,10).
:- set_prolog_flag(py_backtrace, true).
:- set_prolog_flag(py_argv,[]).
%:- set_prolog_flag(argv,[]).
/*
# Core in Rust
In the original version, the core logic and functionalities of the MeTTa system are implemented in Rust. Rust is known for its performance and safety features, making it a suitable choice for building robust, high-performance systems.

# Python Extensions
Python is used to extend or customize MeTTa. Typically, Python interacts with the Rust core through a Foreign Function Interface (FFI) or similar bridging mechanisms. This allows Python programmers to write code that can interact with the lower-level Rust code, while taking advantage of Python's ease of use and rich ecosystem.

# Prolog Allows Python Extensions
Just like the Rust core allowed for Python extensions, the Prolog code also permits Python and Rust developers (thru python right now) to extend or customize parts of MeTTa. This maintains the system?s extensibility and allows users who are more comfortable with Python to continue working with the system effectively.

*/
:- use_module(library(filesex)).

:-
  (module_property(janus,file(File))->
    janus:ensure_loaded(File);
   (exists_file('/usr/local/lib/swipl/library/ext/swipy/janus.pl')
        -> janus:ensure_loaded('/usr/local/lib/swipl/library/ext/swipy/janus.pl')
        ; janus:ensure_loaded(library(janus)))).

:- multifile(is_python_space/1).
:- dynamic(is_python_space/1).
:- volatile(is_python_space/1).

is_rust_space(GSpace):- is_python_space(GSpace).

is_not_prolog_space(GSpace):-  is_rust_space(GSpace), !.
is_not_prolog_space(GSpace):-  \+ is_asserted_space(GSpace), \+ is_nb_space(GSpace), !.

with_safe_argv(Goal):-
  current_prolog_flag(argv,Was),
  setup_call_cleanup(set_prolog_flag(argv,[]),
    py_catch(Goal),
  set_prolog_flag(argv,Was)).
with_safe_argv(G1,G2):- with_safe_argv((G1,G2)).
py_catch((G1,G2)):-!,py_catch(G1),py_catch(G2).
py_catch(Goal):- catch(Goal,E,(pybug(E=py_catch(Goal)),py_dump,trace,Goal)).
%py_catch(Goal):- trace,catch(Goal,E,(pybug(E),py_dump)),!.
py_dump:- py_call(traceback:print_exc()).

py_call_c(G):- py_catch(py_call(G)).
py_call_c(G,R):- py_catch(py_call(G,R)).

py_is_module(M):-notrace((with_safe_argv(py_is_module_unsafe(M)))).

py_is_module_unsafe(M):- py_is_object(M),!,py_type(M,module).
py_is_module_unsafe(M):- catch((py_call(M,X),py_type(X,module)),_,fail).

%py_is_py(_):- \+ py_is_enabled, !, fail.
py_is_py(V):- var(V),!, get_attr(V,pyobj,_),!.
py_is_py(V):- compound(V),!,fail.
py_is_py(V):- is_list(V),!,fail.
py_is_py(V):- atomic(V), !, \+ atom(V), py_is_object(V),!.
py_is_py(V):- \+ callable(V),!,fail.
py_is_py(V):- py_is_tuple(V),!.
py_is_py(V):- py_is_py_dict(V),!.
py_is_py(V):- py_is_list(V),!.

py_resolve(V,Py):- var(V),!, get_attr(V,pyobj,Py),!.
py_resolve(V,Py):- \+ compound(V),!,py_is_object(V),Py=V.
py_resolve(V,Py):- is_list(V),!,fail,maplist(py_resolve,V,Py).
py_resolve(V,Py):- V=Py.

py_is_tuple(X):- py_resolve(X,V), py_tuple(V,T),py_tuple(T,TT),T==TT, \+ py_type(V,str).
py_is_py_dict(X):- atomic(X),py_is_object(X),py_type(X,dict).
%py_is_py_dict(X):- py_resolve(X,V), py_dict(V,T), py_dict(T,TT), T==TT.
py_is_list(X):- py_resolve(X,V), py_type(V,list).
%py_is_list(V):- py_is_tuple(V).

% Evaluations and Iterations
:- thread_local(did_load_builtin_module/0).
:- volatile(did_load_builtin_module/0).
:- dynamic(did_load_builtin_module/0).
load_builtin_module:- did_load_builtin_module,!.
load_builtin_module:- assert(did_load_builtin_module),
py_module(builtin_module,
'
import sys
#import numpy

the_modules_and_globals=None

def eval_string(s):
    global the_modules_and_globals
    global_vars = the_modules_and_globals
    local_vars = locals()
    return eval(s,global_vars,local_vars)

def exec_string(s):
    global the_modules_and_globals
    global_vars = the_modules_and_globals
    local_vars = locals()
    return exec(s,global_vars,local_vars)

def py_nth(s,nth):
    return s[nth]

def identity(s):
    return s

def get_globals():
    return globals()

def merge_modules_and_globals():
    # Get all currently loaded modules
    loaded_modules = sys.modules

    # Get all global variables
    global_vars = globals()

    # Prepare a combined dictionary
    global the_modules_and_globals
    combined_dict = the_modules_and_globals
    if combined_dict is None:
        combined_dict = {}

    # Add modules with a prefix or special key to distinguish them
    for mod_name, mod_obj in loaded_modules.items():
        combined_dict[f"module_{mod_name}"] = mod_obj
        combined_dict[f"{mod_name}"] = mod_obj

    # Add global variables with a prefix or special key
    for var_name, var_value in global_vars.items():
        combined_dict[f"global_{var_name}"] = var_value
        combined_dict[f"{var_name}"] = var_value

    the_modules_and_globals = combined_dict
    return combined_dict

def get_locals():
    return locals()

def iter_collection(s):
    return iter(s)

def string_conversion(s):
    return str(s)

def string_representation(s):
    return repr(s)

def py_len(s):
    return len(s)

def py_list(s):
    return list(s)

def py_dict(s):
    return dict(s)

def py_dict0():
    return dict()

def py_map(s):
    return map(s)

def py_tuple(s):
    return tuple(s)

def py_set(s):
    return set(s)

def absolute_value(num):
    return abs(num)

def all_true(iterable):
    return all(iterable)

def any_true(iterable):
    return any(iterable)

def as_ascii(s):
    return ascii(s)

def binary_conversion(num):
    return bin(num)

def boolean_conversion(val):
    return bool(val)

def chr_conversion(num):
    return chr(num)

def hexadecimal_conversion(num):
    return hex(num)

def octal_conversion(num):
    return oct(num)

# Arithmetic and Conversion
def int_conversion(s):
    return int(s)

def float_conversion(s):
    return float(s)

def complex_conversion(real, imag=0):
    return complex(real, imag)

def divmod_func(a, b):
    return divmod(a, b)

def pow_func(base, exp):
    return pow(base, exp)

# Collection Handling
def sorted_iterable(iterable, key=None, reverse=False):
    return sorted(iterable, key=key, reverse=reverse)

def sum_iterable(iterable, start=0):
    return sum(iterable, start)

def min_value(*args, key=None):
    return min(*args, key=key)

def max_value(*args, key=None):
    return max(*args, key=key)

# Type and Attribute Handling
def type_of(obj):
    return type(obj)

def isinstance_of(obj, classinfo):
    return isinstance(obj, classinfo)

def print_nonl(sub):
    return print(sub, end="")

def issubclass_of(sub, superclass):
    return issubclass(sub, superclass)

def getattr_func(obj, name, default=None):
    return getattr(obj, name, default)

def setattr_func(obj, name, value):
    setattr(obj, name, value)

def hasattr_func(obj, name):
    return hasattr(obj, name)

# File and I/O
def open_file(filename, mode="r", buffering=-1):
    return open(filename, mode, buffering)

# Exception Handling
def raise_exception(exctype, msg=None):
    if msg:
        raise exctype(msg)
    else:
        raise exctype()

# Miscellaneous
def callable_check(obj):
    return callable(obj)

def id_func(obj):
    return id(obj)

def help_func(obj):
    help(obj)  # This will print the help to the standard output

import inspect

def get_str_rep(func):
    if not inspect.isfunction(func):
        return func
    if func.__module__ == "__main__":
        return func.__name__
    return f"{func.__module__}.{func.__name__}"

the_modules_and_globals = merge_modules_and_globals()

').

pych_chars(Chars,P):- \+ is_list(Chars), !, P = Chars.
pych_chars(Chars,P):- append(O,`\r@(none)`,Chars),!,pych_chars(O,P).
pych_chars(Chars,P):- append(O,`\n@(none)`,Chars),!,pych_chars(O,P).
pych_chars(Chars,P):- append(O,`@(none)`,Chars),!,pych_chars(O,P).
pych_chars(Chars,P):- append(O,[WS],Chars),code_type(WS,new_line),!,pych_chars(O,P).
pych_chars(Chars,P):- append(O,[WS],Chars),code_type(WS,end_of_line),!,pych_chars(O,P).
pych_chars(P,P).


py_ppp(V):-flush_output, with_output_to(codes(Chars), once(py_pp(V))),
 pych_chars(Chars,P),!,format('~s',[P]),!,flush_output.

%atom_codes(Codes,P),writeq(Codes),
%py_ppp(V):- !, flush_output, py_mbi(print_nonl(V),_),!,flush_output.
%py_ppp(V):- writeq(py(V)),!.
%py_ppp(V):-once((py_is_object(V),py_to_pl(V,PL))),V\=@=PL,!,print(PL).
%py_ppp(V):-metta_py_pp(V).

% Evaluations and Iterations
:- thread_local(did_load_hyperon_module/0).
:- volatile(did_load_hyperon_module/0).
:- dynamic(did_load_hyperon_module/0).
load_hyperon_module:- did_load_hyperon_module,!.
load_hyperon_module:- assert(did_load_hyperon_module),
 py_module(hyperon_module,'

from hyperon.base import Atom
from hyperon.atoms import OperationAtom, E, GroundedAtom, GroundedObject
from hyperon.ext import register_tokens
from hyperon.ext import register_atoms
from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa
from hyperon.atoms import *
import hyperonpy as hp

import sys
import readline
import os
import atexit

class MeTTaVS(MeTTa):
    def copy(self):
        return self

runner = MeTTaVS()

def rust_metta_run(obj):
    return runner.run(obj)

def rust_unwrap(obj):
    if isinstance(obj,SymbolAtom):
        return obj.get_name()
    if isinstance(obj,ExpressionAtom):
        return obj.get_children()
    if isinstance(obj,ValueAtom):
        return obj.value()
    if isinstance(obj,GroundedAtom):
        if obj.get_object_type()==AtomType.UNDEFINED:
            return obj.get_object()
    # if isinstance(obj,GroundedAtom): return obj.get_object()
    if isinstance(obj,GroundedObject):
        return obj.content
    return obj

def rust_deref(obj):
  while True:
    undone = rust_unwrap(obj)
    if undone is obj: return obj
    if undone is None: return obj
    obj = undone

').


py_mcall(I,O):- catch(py_call(I,M,[py_object(false),py_string_as(string),py_dict_as({})]),error(_,_),fail),!,O=M.
py_scall(I,O):- catch(py_call(I,M,[py_string_as(string)]),error(_,_),fail),!,O=M.
py_acall(I,O):- catch(py_call(I,M,[py_string_as(atom)]),error(_,_),fail),!,O=M.
py_ocall(I,O):- catch(py_call(I,M,[py_object(true),py_string_as(string)]),error(_,_),fail),!,O=M.


py_bi(I,O,Opts):- load_builtin_module,catch(py_call(builtin_module:I,M,Opts),error(_,_),fail),!,O=M.
py_obi(I,O):- load_builtin_module,py_ocall(builtin_module:I,O).
py_mbi(I,O):- load_builtin_module,py_mcall(builtin_module:I,O).
%?- py_call(type(hi-there), P),py_pp(P).
get_str_rep(I,O):- py_mbi(get_str_rep(I),O),!.

py_atom(I,O):- var(I),!,O=I.
py_atom([I|Is],O):-!, py_dot(I,II),py_dot_from(II,Is,O),!.
py_atom(I,O):- atomic(I),!,py_atomic(I,O).
py_atom(I,O):- py_ocall(I,O),!.
py_atom(I,O):- I=O.

py_atom_type(I,_Type,O):- var(I),!,O=I.
py_atom_type([I|Is],_Type,O):-!, py_dot(I,II),py_dot_from(II,Is,O).
py_atom_type(I,_Type,O):- atomic(I),!,py_atomic(I,O).
py_atom_type(I,_Type,O):- py_ocall(I,O),!.
py_atom_type(I,_Type,O):- I=O.

py_atomic([],O):-py_ocall("[]",O),!.
py_atomic(I,O):- py_is_object(I),!,O=I.
py_atomic(I,O):- string(I),py_eval(I,O),!.
py_atomic(I,O):- py_ocall(I,O),!.
py_atomic(I,O):- py_eval(I,O),!.
py_atomic(I,O):- \+ symbol_contains(I,'('),atomic_list_concat([A,B|C],'.',I),py_dot([A,B|C],O),!.
py_atomic(I,O):- string(I), py_dot(I,O),!.
py_atomic(I,O):- I=O.

get_globals(O):- py_mbi(get_globals(),O).
get_locals(O):- py_mbi(get_locals(),O).
merge_modules_and_globals(O):- py_mbi(merge_modules_and_globals(),O).
py_eval(I,O):- py_obi(eval_string(I),O).
py_eval(I):- py_eval(I,O),pybug(O).
py_exec(I,O):- py_mbi(exec_string(I),O).
py_exec(I):- py_exec(I,O),pybug(O).

py_dot(I,O):- string(I),atom_string(A,I),py_atom(A,O),A\==O,!.
py_dot(I,O):- py_atom(I,O).

py_dot_from(From,I,O):- I==[],!,O=From.
py_dot_from(From,[I|Is],O):- !, py_dot_from(From,I,M),py_dot_from(M,Is,O).
py_dot_from(From,I,O):- atomic_list_concat([A,B|C],'.',I),!,py_dot_from(From,[A,B|C],O).
py_dot_from(From,I,O):- py_dot(From,I,O).

py_eval_object(Var,VO):- var(Var),!,VO=Var.
py_eval_object([V|VI],VO):- py_is_function(V),!,py_eval_from(V,VI,VO).
py_eval_object([V|VI],VO):- maplist(py_eval_object,[V|VI],VO).
py_eval_object(VO,VO).

py_is_function(O):- \+ py_is_object(O),!,fail.
py_is_function(O):- py_type(O, function),!.
%py_is_function(O):- py_type(O, method),!.

py_eval_from(From,I,O):- I==[],!,py_dot(From,O).
py_eval_from(From,[I],O):- !, py_fcall(From,I,O).
py_eval_from(From,[I|Is],O):- !, py_dot_from(From,I,M),py_eval_from(M,Is,O).
py_eval_from(From,I,O):- atomic_list_concat([A,B|C],'.',I),!,py_eval_from(From,[A,B|C],O).
py_eval_from(From,I,O):- py_fcall(From,I,O).

py_fcall(From,I,O):- py_ocall(From:I,O).

ensure_space_py(Space,GSpace):- py_is_object(Space),!,GSpace=Space.
ensure_space_py(Space,GSpace):- var(Space),ensure_primary_metta_space(GSpace), Space=GSpace.
ensure_space_py(metta_self,GSpace):- ensure_primary_metta_space(GSpace),!.

:- dynamic(is_metta/1).
:- volatile(is_metta/1).
ensure_rust_metta(MeTTa):- is_metta(MeTTa),py_is_object(MeTTa),!.
ensure_rust_metta(MeTTa):- with_safe_argv(ensure_rust_metta0(MeTTa)),asserta(is_metta(MeTTa)).

ensure_rust_metta0(MeTTa):- ensure_mettalog_py(MettaLearner), py_call(MettaLearner:'get_metta'(),MeTTa),
  py_is_object(MeTTa).
ensure_rust_metta0(MeTTa):- py_call('mettalog':'MeTTaLog'(),MeTTa).
ensure_rust_metta0(MeTTa):- py_call(hyperon:runner:'MeTTa'(),MeTTa),!.

ensure_rust_metta:- ensure_rust_metta(_).

:- dynamic(is_mettalog/1).
:- volatile(is_mettalog/1).
ensure_mettalog_py(MettaLearner):- is_mettalog(MettaLearner),!.
ensure_mettalog_py(MettaLearner):-
   with_safe_argv(
   (want_py_lib_dir,
    %py_call('mettalog',MettaLearner),
    %py_call('motto',_),
    %py_call('motto.sparql_gate':'sql_space_atoms'(),Res1),pybug(Res1),
    %py_call('motto.llm_gate':'llmgate_atoms'(MeTTa),Res2),pybug(Res2),

   pybug(is_mettalog(MettaLearner)),
   asserta(is_mettalog(MettaLearner)))).

ensure_mettalog_py:-
  %load_builtin_module,
  %load_hyperon_module,
  setenv('VSPACE_VERBOSE',0),
  with_safe_argv(ensure_mettalog_py(_)),!.



:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).
space_type_method(is_not_prolog_space,new_space,new_rust_space).
space_type_method(is_not_prolog_space,add_atom,add_to_space).
space_type_method(is_not_prolog_space,remove_atom,remove_from_space).
space_type_method(is_not_prolog_space,replace_atom,replace_in_space).
space_type_method(is_not_prolog_space,atom_count,atom_count_from_space).
space_type_method(is_not_prolog_space,get_atoms,query_from_space).
space_type_method(is_not_prolog_space,atom_iter,atoms_iter_from_space).
space_type_method(is_not_prolog_space,query,query_from_space).

:- dynamic(is_primary_metta_space/1).
:- volatile(is_primary_metta_space/1).
% Initialize a new hyperon.base.GroundingSpace and get a reference
ensure_primary_metta_space(GSpace) :- is_primary_metta_space(GSpace),!.
ensure_primary_metta_space(GSpace) :- ensure_rust_metta(MeTTa),
   with_safe_argv(py_call(MeTTa:space(),GSpace)),
    asserta(is_primary_metta_space(GSpace)).
ensure_primary_metta_space(GSpace) :- new_rust_space(GSpace).
ensure_primary_metta_space:- ensure_primary_metta_space(_).

:- if( \+ current_predicate(new_rust_space/1 )).
% Initialize a new hyperon.base.GroundingSpace and get a reference
new_rust_space(GSpace) :-
    with_safe_argv(py_call(hyperon:base:'GroundingSpace'(), GSpace)),
    asserta(is_python_space(GSpace)).
:- endif.

:- if( \+ current_predicate(query_from_space/3 )).
% Query from hyperon.base.GroundingSpace
query_from_space(Space, QueryAtom, Result) :-
    ensure_space(Space,GSpace),
    py_call(GSpace:'query'(QueryAtom), Result).


% Replace an atom in hyperon.base.GroundingSpace
replace_in_space(Space, FromAtom, ToAtom) :-
    ensure_space(Space,GSpace),
    py_call(GSpace:'replace'(FromAtom, ToAtom), _).

% Get the atom count from hyperon.base.GroundingSpace
atom_count_from_space(Space, Count) :-
    ensure_space(Space,GSpace),
    py_call(GSpace:'atom_count'(), Count).

% Get the atoms from hyperon.base.GroundingSpace
atoms_from_space(Space, Atoms) :-
    ensure_space(Space,GSpace),
    py_call(GSpace:'get_atoms'(), Atoms).

atom_from_space(Space, Sym):-
   atoms_iter_from_space(Space, Atoms),elements(Atoms,Sym).

% Get the atom iterator from hyperon.base.GroundingSpace
atoms_iter_from_space(Space, Atoms) :-
    ensure_space(Space,GSpace),
    with_safe_argv(py_call(src:'mettalog':get_atoms_iter_from_space(GSpace),Atoms)),
    %py_call(GSpace:'atoms_iter'(), Atoms).
    true.
:- endif.

metta_py_pp(V):- py_is_enabled,once((py_is_object(V),py_to_pl(V,PL))),V\=@=PL,!,metta_py_pp(PL).
metta_py_pp(V):- atomic(V),py_is_enabled,py_is_object(V),py_pp(V),!.
metta_py_pp(V):- format('~p',[V]),!.

% py_to_pl/2 - Converts a Python object to a Prolog term.
py_to_pl(I,O):- py_to_pl(_,I,O).

% py_to_pl/3 - Calls py_to_pl/6 with initial parameters.
py_to_pl(VL,I,O):- ignore(VL=[vars]), py_to_pl(VL,[],[],_,I,O),!.

% is_var_or_nil/1 - Checks if the input is a variable or an empty list.
is_var_or_nil(I):- var(I),!.
is_var_or_nil([]).

% py_to_pl/6 - Main conversion predicate.
% print what we are doing
%py_to_pl(VL,Par,_Cir,_,L,_):- pybug(py_to_pl(VL,Par,L)),fail.
% If L is a variable, E is unified with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- var(L),!,E=L.
% If L is an empty list, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- L ==[],!,E=L.

% If O is an object, convert it to Prolog.
py_to_pl(VL, Par, Cir, CirO, O, E) :- py_is_object(O), py_class(O, Cl), !,
    pyo_to_pl(VL, Par, [O = E | Cir], CirO, Cl, O, E).
% If L is in the Cir list, unify E with L.

%py_to_pl(_VL,_Par,Cir,Cir,L,E):- py_is_dict(L),!,py_mbi(identity(L),E).
py_to_pl(_VL,_Par,Cir,Cir,L,E):- member(N-NE,Cir), N==L, !, (E=L;NE=E), !.
% If LORV is a variable or nil, unify it directly.
py_to_pl(_VL,_Par,Cir,Cir, LORV:B,LORV:B):- is_var_or_nil(LORV),  !.
py_to_pl(_VL,_Par,Cir,Cir, LORV:_B:_C,LORV):- is_var_or_nil(LORV),  !.
% If L is not callable, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- \+ callable(L),!,E=L.
% Convert lists with annotations.
py_to_pl(VL, Par, Cir, CirO, [H|T]:B:C, [HH|TT]) :-
    py_to_pl(VL, Par, Cir, CirM, H:B:C, HH),
    py_to_pl(VL, Par, CirM, CirO, T:B:C, TT),!.
py_to_pl(VL, Par, Cir, CirO, [H|T]:B, [HH|TT]) :-
    py_to_pl(VL, Par, Cir, CirM, H:B, HH),
    py_to_pl(VL, Par, CirM, CirO, T:B, TT).
% Handle objects with callable methods.
py_to_pl(VL, Par, Cir, CirO, A:B:C, AB) :-
    py_is_object(A),
    callable(B),
    py_call(A:B, R),
    py_to_pl(VL, Par, Cir, CirO, R:C, AB).
py_to_pl(VL, Par, Cir, CirO, A:B, AB) :-
    py_is_object(A),
    callable(B),
    py_call(A:B, R),
    py_to_pl(VL, Par, Cir, CirO, R, AB).

% Convert compound terms.
py_to_pl(VL, Par, Cir, CirO, A:B, AA:BB) :- !,
    py_to_pl(VL, Par, Cir, CirM, A, AA),
    py_to_pl(VL, Par, CirM, CirO, B, BB).
py_to_pl(VL, Par, Cir, CirO, A-B, AA-BB) :- !,
    py_to_pl(VL, Par, Cir, CirM, A, AA),
    py_to_pl(VL, Par, CirM, CirO, B, BB).

py_to_pl(_VL,_Par,Cir,Cir,L,E):- \+ callable(L),!,E=L.

% If L is an atom, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- atom(L),!,E=L.

% Convert lists.
py_to_pl(VL, Par, Cir, CirO, [H|T], [HH|TT]) :- !,
    py_to_pl(VL, Par, Cir, CirM, H, HH),
    py_to_pl(VL, Par, CirM, CirO, T, TT).

% Handle dictionaries.
py_to_pl(VL, Par, Cir, CirO, L, E) :-  is_dict(L, F), !,
    dict_pairs(L, F, NV), !,
    py_to_pl(VL, Par, Cir, CirO, NV, NVL),
    dict_pairs(E, F, NVL).

% If L is not callable, unify E with L.
py_to_pl(_VL,_Par,Cir,Cir,L,E):- \+ callable(L),!,E=L.
%py_to_pl(VL,Par,Cir,CirO,A:B:C,AB):-  py_is_object(A),callable(B),py_call(A:B,R),!, py_to_pl(VL,Par,[A:B-AB|Cir],CirO,R:C,AB).
%py_to_pl(VL,Par,Cir,CirO,A:B,AB):-  py_is_object(A),callable(B),py_call(A:B,R),!, py_to_pl(VL,Par,[A:B-AB|Cir],CirO,R,AB).

% Convert compound terms using compound_name_arguments/3.
py_to_pl(VL, Par, Cir, CirO, A, AA) :- compound(A), !,
    compound_name_arguments(A, F, L),
    py_to_pl(VL, Par, Cir, CirO, L, LL),
    compound_name_arguments(AA, F, LL).

% Default case: unify E with E.
py_to_pl(_VL,_Par,Cir,Cir,E,E).
/*
varname_to_real_var(RL,E):- upcase_atom(RL,R),varname_to_real_var0(R,E).
varname_to_real_var0(R,E):- nb_current('cvariable_names',VL),!,varname_to_real_var0(R,VL,E).
varname_to_real_var0(R,E):- nb_setval('cvariable_names',[R=v(_)]),!,varname_to_real_var0(R,E).
varname_to_real_var0(R,[],E):- nb_setval('cvariable_names',[R=v(_)]),!,varname_to_real_var0(R,E).
varname_to_real_var0(R,VL,E):- member(N=V,VL), N==R,!,arg(1,V,E).
varname_to_real_var0(R,VL,E):- extend_container(VL,R=v(_)),varname_to_real_var0(R,E).*/
% Predicate to extend the list inside the container
extend_container(Container, Element) :-
    arg(2, Container, List),
    nb_setarg(2, Container, [Element|List]).

rinto_varname(R,RN):- atom_number(R,N),atom_concat('Num',N,RN).
rinto_varname(R,RN):- upcase_atom(R,RN).
real_VL_var(RL,VL,E):- nonvar(RL), !, rinto_varname(RL,R),!,real_VL_var0(R,VL,E).
real_VL_var(RL,VL,E):- member(N=V,VL), V==E,!,RL=N.
real_VL_var(RL,VL,E):- compound(E),E='$VAR'(RL),ignore(real_VL_var0(RL,VL,E)),!.
real_VL_var(RL,VL,E):- format(atom(RL),'~p',[E]), member(N=V,VL), N==RL,!,V=E.
real_VL_var(RL,VL,E):- format(atom(RL),'~p',[E]), real_VL_var0(RL,VL,E).
real_VL_var0(R,VL,E):- member(N=V,VL), N==R,!,V=E.
real_VL_var0(R,VL,E):- extend_container(VL,R=E),!. % ,E='$VAR'(R).

pyo_to_pl(VL,_Par,Cir,Cir,Cl,O,E):- Cl=='VariableAtom', !, py_call(O:get_name(),R), real_VL_var(R,VL,E),!.
pyo_to_pl(VL,Par,Cir,CirO,Cl,O,E):- class_to_pl1(Par,Cl,M),py_member_values(O,M,R), !, py_to_pl(VL,[Cl|Par],Cir,CirO,R,E).
pyo_to_pl(VL,Par,Cir,CirO,Cl,O,E):- class_to_pl(Par,Cl,M), % pybug(class_to_pl(Par,Cl,M)),
   py_member_values(O,M,R), !, py_to_pl(VL,[Cl|Par],Cir,CirO,R,E).
pyo_to_pl(VL,Par,Cir,CirO,Cl,O,E):- catch(py_obj_dir(O,L),_,fail),pybug(py_obj_dir(O,L)),py_decomp(M),meets_dir(L,M),pybug(py_decomp(M)),
  py_member_values(O,M,R), member(N-_,Cir), R\==N, !, py_to_pl(VL,[Cl|Par],Cir,CirO,R,E),!.
    % If L is not callable, unify E with L.
%pyo_to_pl(_VL,_Par,Cir,Cir,Cl,O,E):- get_str_rep(O,Str), E=..[Cl,Str].
pyo_to_pl(_VL,_Par,Cir,Cir,_Cl,O,E):- O = E,!.

pl_to_rust(Var,Py):- pl_to_rust(_VL,Var,Py).
pl_to_rust(VL,Var,Py):- var(VL),!,ignore(VL=[vars]),pl_to_rust(VL,Var,Py).

pl_to_rust(_VL,Sym,Py):- is_list(Sym),!, maplist(pl_to_rust,Sym,PyL), py_call(src:'mettalog':'MkExpr'(PyL),Py),!.
pl_to_rust(VL,Var,Py):- var(Var), !, real_VL_var(Sym,VL,Var), py_call('hyperon.atoms':'V'(Sym),Py),!.
pl_to_rust(VL,'$VAR'(Sym),Py):- !, real_VL_var(Sym,VL,_),py_call('hyperon.atoms':'V'(Sym),Py),!.
pl_to_rust(VL,DSym,Py):- atom(DSym),atom_concat('$',VName,DSym), rinto_varname(VName,Sym),!, pl_to_rust(VL,'$VAR'(Sym),Py).
pl_to_rust(_VL,Sym,Py):- atom(Sym),!, py_call('hyperon.atoms':'S'(Sym),Py),!.
%pl_to_rust(VL,Sym,Py):- is_list(Sym), maplist(pl_to_rust,Sym,PyL), py_call('hyperon.atoms':'E'(PyL),Py),!.
pl_to_rust(_VL,Sym,Py):- string(Sym),!, py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.
pl_to_rust(_VL,Sym,Py):- py_is_object(Sym),py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.
pl_to_rust(_VL,Sym,Py):- py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.

py_list(MeTTa,PyList):- pl_to_py(MeTTa,PyList).

py_tuple(O,Py):- py_ocall(tuple(O),Py),!.
py_tuple(O,Py):- py_obi(py_tuple(O),Py),!.

py_dict(O,Py):- catch(py_is_py_dict(O),_,fail),!,O=Py.
py_dict(O,Py):- py_ocall(dict(O),Py),!.

% ?- py_list([1, 2.0, "string"], X),py_type(X,Y).
% ?- py_list_index([1, 2.0, "string"], X),py_type(X,Y).
py_nth(L,Nth,E):- py_obi(py_nth(L,Nth),E).
py_len(L,E):- py_mbi(py_len(L),E).
py_o(O,Py):- py_obi(identity(O),Py),!.
py_m(O,Py):- py_mbi(identity(O),Py),!.
pl_to_py(Var,Py):- pl_to_py(_VL,Var,Py).
pl_to_py(VL,Var,Py):- var(VL),!,ignore(VL=[vars]),pl_to_py(VL,Var,Py).
pl_to_py(_VL,Sym,Py):- py_is_object(Sym),!,Sym=Py.
%pl_to_py(_VL,O,Py):- py_is_dict(O),!,py_obi(identity(O),Py).
pl_to_py(_VL,MeTTa,Python):- float(MeTTa), !, py_obi(float_conversion(MeTTa),Python).
pl_to_py(_VL,MeTTa,Python):- string(MeTTa), !, py_obi(string_conversion(MeTTa),Python).
pl_to_py(_VL,MeTTa,Python):- integer(MeTTa), !, py_obi(int_conversion(MeTTa),Python).
pl_to_py(VL,Sym,Py):- is_list(Sym),!, maplist(pl_to_py(VL),Sym,PyL), py_obi(py_list(PyL),Py).
pl_to_py(VL,Var,Py):- var(Var), !, real_VL_var(Sym,VL,Var), py_call('hyperon.atoms':'V'(Sym),Py),!.
pl_to_py(VL,'$VAR'(Sym),Py):- !, real_VL_var(Sym,VL,_),py_call('hyperon.atoms':'V'(Sym),Py),!.
pl_to_py(_VL,O,Py):- py_type(O,_),!,O=Py.
% % %pl_to_py(_VL,O,Py):- py_is_dict(O),!,O=Py.
%pl_to_py(VL,DSym,Py):- atom(DSym),atom_concat('$',VName,DSym), rinto_varname(VName,Sym),!, pl_to_py(VL,'$VAR'(Sym),Py).
%pl_to_py(_VL,Sym,Py):- atom(Sym),!, py_call('hyperon.atoms':'S'(Sym),Py),!.
%pl_to_py(_VL,Sym,Py):- string(Sym),!, py_call('hyperon.atoms':'S'(Sym),Py),!.
%pl_to_py(VL,Sym,Py):- is_list(Sym), maplist(pl_to_py,Sym,PyL), py_call('hyperon.atoms':'E'(PyL),Py),!.
%pl_to_py(_VL,Sym,Py):- py_is_object(Sym),py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.
pl_to_py(_VL,MeTTa,MeTTa).
%pl_to_py(_VL,Sym,Py):- py_call('hyperon.atoms':'ValueAtom'(Sym),Py),!.

py_key(O,I):- py_m(O,M),key(M,I).
py_items(O,I):- py_m(O,M),items(M,I).
%py_values(O,K,V):- py_m(O,M),values(M,K,V).
py_values(O,K,V):- py_items(O,L),member(K:V,L).

%elements(Atoms,E):- is_list(Atoms),!,
meets_dir(L,M):- atom(M),!,member(M,L),!.
meets_dir(L,M):- is_list(M),!,maplist(meets_dir(L),M).
meets_dir(L,M):- compound_name_arity(M,N,0),!,member(N,L),!.
meets_dir(L,M):- compound(M),!,compound_name_arguments(M,F,[A|AL]),!,maplist(meets_dir(L),[F,A|AL]).

py_member_values(O,C,R):- is_list(O),!,maplist(py_member_values,O,C,R).
py_member_values(O,C,R):- is_list(C),!,maplist(py_member_values(O),C,R).
%py_member_values(O,C,R):- atom(C),!,compound_name_arity(CC,C,0),!,py_call(O:CC,R).
py_member_values(O,f(F,AL),R):- !,py_member_values(O,[F|AL],[RF|RAL]), compound_name_arguments(R,RF,RAL).
py_member_values(O,C,R):- py_call(O:C,R,[py_string_as(atom),py_object(false)]).

py_to_str(PyObj,Str):-
   with_output_to(string(Str),py_pp(PyObj,[nl(false)])).

 tafs:-
    atoms_from_space(Space, _),py_to_pl(VL,Space,AA), print_tree(aa(Pl,aa)),pl_to_rust(VL,AA,Py), print_tree(py(Pl,py)),pl_to_rust(VL,Py,Pl),print_tree(pl(Pl,pl))
    ,
    atoms_from_space(Space, [A]),py_to_pl(VL,A,AA),
    atoms_from_space(Space, [A]),py_obj_dir(A,D),writeq(D),!,py_to_pl(VL,D:get_object(),AA),writeq(AA),!,fail.

py_class(A,AA):- py_call(A:'__class__',C), py_call(C:'__name__',AA,[py_string_as(atom)]),!.
py_decomp(M,C):- py_decomp(M), compound_name_arity(C,M,0).


class_to_pl1(_Par,'GroundingSpaceRef',get_atoms()).
class_to_pl1(_Par,'ExpressionAtom',get_children()).
class_to_pl1(_Par,'SpaceRef',get_atoms()).
class_to_pl1(_Par,'VariableAtom','__repr__'()).
class_to_pl1(_Par,'SymbolAtom',get_name()).
class_to_pl1(_Par,'bool','__repr__'()).
class_to_pl(_Par,'ValueAtom','__repr__'()).
class_to_pl(_Par,'ValueObject','value').
class_to_pl(Par,'GroundedAtom','__repr__'()):- length(Par,Len),Len>=5,!.
class_to_pl(Par,_,'__str__'()):- length(Par,Len),Len>15,!.
class_to_pl(_Par,'GroundedAtom',get_object()).

/*


class_to_pl(Par,'bool','__repr__'()).

*/
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

%atoms_from_space(Space, [Atoms]),py_pp(Atoms),py_call(Atoms:get_object(),A),atoms_from_space(A,Dir),member(E,Dir),py_obj_dir(E,C),py_call(E:get_children(),CH),py_pp(CH).


% Remove an atom from hyperon.base.GroundingSpace
:- if( \+ current_predicate(remove_from_space/2 )).
remove_from_space(Space, Sym) :-
    ensure_space(Space,GSpace),
    py_call(GSpace:'remove'(Sym), _).
:- endif.

% Add an atom to hyperon.base.GroundingSpace
:- if( \+ current_predicate(add_to_space/2 )).
add_to_space(Space, Sym) :-
    ensure_space(Space,GSpace),
    py_call(GSpace:'add'(Sym), _).
:- endif.

must_det_llp((A,B)):-!, must_det_llp(A), must_det_llp(B).
must_det_llp(B):- pybug(B),!,once(ignore(must_det_ll(B))).

:- dynamic(is_pymod_in_space/2).
:- dynamic(is_pymod_loaded/2).

py_ready:- nb_current('$py_ready','true'),!.
py_ready:- \+ is_mettalog(_),!,fail.
%py_ready:- is_metta(_),!.
py_ready.

%pybug(P):- py_pp(P),!.
pybug(P):- \+ py_ready,!, fbug(P).
pybug(P):- fbug(P).
pypp(P):- py_to_pl(P,PL),!,fbug(PL),!.
pypp(P):- fbug(P),!.

'extend-py!'(Module,R):- (notrace((extend_py(Module,R)))).
extend_py(Module,R):-
  current_self(Self),
  self_extend_py(Self,Module,_Base,R).
self_extend_py(Self,Module):-
  self_extend_py(Self,Module,_Base,_).

self_extend_py(Self,Module,File,R):-
 with_safe_argv((
  assert_new(is_pymod_in_space(Module,Self)),
  (nonvar(File)-> Use=File ; Use=Module),
  pybug('extend-py!'(Use)),
   %py_call(mettalog:use_mettalog()),
  (Use==mettalog->true;py_load_modfile(Use)),
  %listing(ensure_rust_metta/1),
  %ensure_mettalog_py,
  nb_setval('$py_ready','true'),
  %working_directory(PWD,PWD), py_add_lib_dir(PWD),
  %replace_in_string(["/"="."],Module,ToPython),
  %py_mcall(mettalog:import_module_to_rust(ToPython)),
  %sformat(S,'!(import! &self ~w)',[Use]),rust_metta_run(S,R),
  R = [],
  %py_module_exists(Module),
  %py_call(MeTTa:load_py_module(ToPython),Result),
  true)),!.

py_load_modfile(Use):- py_ocall(mettalog:load_functions(Use),R),!,pybug(R).
py_load_modfile(Use):- exists_directory(Use),!,directory_file_path(Use,'_init_.py',File),py_load_modfile(File).
py_load_modfile(Use):- file_to_modname(Use,Mod),read_file_to_string(Use,Src,[]),!,py_module(Mod,Src).

file_to_modname(Filename,ModName):- symbol_concat('../',Name,Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- symbol_concat('./',Name,Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- symbol_concat(Name,'/_init_.py',Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- symbol_concat(Name,'.py',Filename),!,file_to_modname(Name,ModName).
file_to_modname(Filename,ModName):- replace_in_string(["/"="."],Filename,ModName).

%import_module_to_rust(ToPython):- sformat(S,'!(import! &self ~w)',[ToPython]),rust_metta_run(S).
rust_metta_run(S,Run):- var(S),!,freeze(S,rust_metta_run(S,Run)).
%rust_metta_run(exec(S),Run):- \+ callable(S), string_concat('!',S,SS),!,rust_metta_run(SS,Run).
rust_metta_run(S,Run):- coerce_string(S,R),!,rust_metta_run1(R,Run).
%rust_metta_run(I,O):-
rust_metta_run1(I,O):- load_hyperon_module, !, py_ocall(hyperon_module:rust_metta_run(I),M),!,rust_return(M,O).
rust_metta_run1(R,Run):- % run
  with_safe_argv((((
  %ensure_rust_metta(MeTTa),
  py_call(mettalog:rust_metta_run(R),Run))))).

rust_return(M,O):- (py_iter(M,R,[py_object(true)]),py_iter(R,R1,[py_object(true)]))*->rust_to_pl(R1,O);(fail,rust_to_pl(M,O)).
%rust_return(M,O):- rust_to_pl(M,O).
%rust_return(M,O):- py_iter(M,R,[py_object(true)]),rust_to_pl(R,O).
%rust_return(M,O):- py_iter(M,O). %,delist1(R,O).
delist1([R],R):-!.
delist1(R,R). % Maybe warn here?

rust_to_pl(L,P):- var(L),!,L=P.
%rust_to_pl([],P):- !, P=[].
rust_to_pl(L,P):- is_list(L),!,maplist(rust_to_pl,L,P).
rust_to_pl(R,P):- compound(R),!,compound_name_arguments(R,F,RR),maplist(rust_to_pl,RR,PP),compound_name_arguments(P,F,PP).
rust_to_pl(R,P):- \+ py_is_object(R),!,P=R.
rust_to_pl(R,P):- py_type(R,'ExpressionAtom'),py_mcall(R:get_children(),L),!,maplist(rust_to_pl,L,P).
rust_to_pl(R,P):- py_type(R,'SymbolAtom'),py_acall(R:get_name(),P),!.
rust_to_pl(R,P):- py_type(R,'VariableAtom'),py_scall(R:get_name(),N),!,as_var(N,P),!.
%rust_to_pl(R,P):- py_type(R,'VariableAtom'),py_acall(R:get_name(),N),!,atom_concat('$',N,P).
rust_to_pl(R,N):- py_type(R,'OperationObject'),py_acall(R:name(),N),!,cache_op(N,R).
rust_to_pl(R,P):- py_type(R,'SpaceRef'),!,P=R. % py_scall(R:'__str__'(),P),!.
rust_to_pl(R,P):- py_type(R,'ValueObject'),py_ocall(R:'value'(),L),!,rust_to_pl(L,P).
rust_to_pl(R,PT):- py_type(R,'GroundedAtom'),py_ocall(R:get_grounded_type(),T),rust_to_pl(T,TT),py_ocall(R:get_object(),L),!,rust_to_pl(L,P),combine_term_l(TT,P,PT).
rust_to_pl(R,P):- py_is_list(R),py_m(R,L),R\==L,!,rust_to_pl(L,P).
rust_to_pl(R,PT):- py_type(R,T),combine_term_l(T,R,PT),!.
%rust_to_pl(R,P):- py_acall(R:'__repr__'(),P),!.
rust_to_pl(R,P):-
  load_hyperon_module, !, py_ocall(hyperon_module:rust_deref(R),M),!,
  (R\==M -> rust_to_pl(M,P) ; M=P).

as_var('_',_):-!.
as_var(N,'$VAR'(S)):-sformat(S,'_~w',[N]),!.

rust_metta_run(S):-
  rust_metta_run(S,Py),
  print_py(Py).

:- volatile(cached_py_op/2).
cache_op(N,R):- asserta_if_new(cached_py_op(N,R)),fbug(cached_py_op(N,R)).
:- volatile(cached_py_type/2).
cache_type(N,R):- asserta_if_new(cached_py_type(N,R)),fbug(cached_py_type(N,R)).

print_py(Py):-
  py_to_pl(Py,R), print(R),nl.

combine_term_l('OperationObject',P,P):-!.
combine_term_l('Number',P,P):-!.
combine_term_l('Bool',P,P):-!.
combine_term_l('ValueObject',R,P):-R=P,!. %rust_to_pl(R,P),!.
combine_term_l('%Undefined%',R,P):-rust_to_pl(R,P),!.
combine_term_l('hyperon::space::DynSpace',P,P):-!.
combine_term_l([Ar|Stuff],Op,Op):- Ar == (->), !, cache_type(Op,[Ar|Stuff]).
combine_term_l(T,P,ga(P,T)).

%coerce_string(S,R):- atom(S), sformat(R,'~w',[S]),!.
coerce_string(S,R):- string(S),!,S=R.
coerce_string(S,R):- with_output_to(string(R),write_src(S)),!.

load_functions_motto:- load_functions_motto(Def),pypp(Def).
load_functions_motto(Def):-
 load_functions_ext,
 with_safe_argv(py_call(mettalog:load_functions_motto(),Def)).

load_functions_ext:- load_functions_ext(Def),pypp(Def).
load_functions_ext(Def):-
 with_safe_argv(py_call(mettalog:load_functions_ext(),Def)).

% Example usage
example_usage :-
    with_safe_argv(ensure_primary_metta_space(GSpace)),
    %some_query(Query),
    Query = [],
    with_safe_argv(query_from_space(GSpace, Query , Result)),
    writeln(Result).

%atoms_from_space(Sym):-  atoms_iter_from_space(metta_self, Atoms),py_iter(Atoms,Sym).
atom_count_from_space(Count):-  atom_count_from_space(metta_self, Count).


%:- .
%:- ensure_rust_metta.
%:- with_safe_argv(ensure_primary_metta_space(_GSpace)).
/*
Rust: The core of MeTTa is implemented in Rust, which provides performance and safety features.

Python Extensions: Python is used for extending the core functionalities. Python communicates with Rust via a Foreign Function Interface (FFI) or similar mechanisms.

Prolog: The Prolog code is an additional layer that allows you to extend or customize parts of MeTTa using Python and Rust. It maintains the system's extensibility.


VSpace is a space with its backend in Prolog, it implies that you're using Prolog's logic programming capabilities to manage and manipulate a particular domain, which in this context is referred to as a "space" (possibly akin to the GroundingSpace in Python, but implemented in Prolog).

To integrate VSpace with the existing Python and Rust components, similar interfacing techniques could be used. You could expose Prolog predicates as functions that can be called from Python or Rust, and likewise, call Python or Rust functions from within Prolog.


*/

%:- ensure_loaded(metta_interp).

:- dynamic(want_py_lib_dir/1).
:- prolog_load_context(directory, ChildDir),
   file_directory_name(ChildDir, ParentDir),
   file_directory_name(ParentDir, GParentDir),
   pfcAdd_Now(want_py_lib_dir(GParentDir)).

want_py_lib_dir:-
   with_safe_argv((forall(want_py_lib_dir(GParentDir),
                         py_add_lib_dir(GParentDir)),
    sync_python_path)).

sync_python_path:-
  working_directory(PWD,PWD), py_add_lib_dir(PWD),
   ignore(( getenv('PYTHONPATH', CurrentPythonPath),
    symbolic_list_concat(List, ':', CurrentPythonPath),
    list_to_set(List,Set),
    py_lib_dirs(DirsA),
    forall(member(E,Set),if_t( \+member(E,DirsA), if_t( \+ atom_length(E,0), py_add_lib_dir(E)))))),
    py_lib_dirs(DirsL),
    list_to_set(DirsL,Dirs),
    fbug(py_lib_dirs(Dirs)),
    symbolic_list_concat(Dirs, ':',NewPythonPath),
    setenv('PYTHONPATH', NewPythonPath).

is_rust_operation([Fun|Args]):-
  get_list_arity(Args,Arity),
  py_call(mettalog:get_operation_definition_with_arity(Fun,Arity),O),O\=='@'('none').

get_list_arity(Args,Arity):- is_list(Args),!,length(Args,Arity).
get_list_arity(_Args,-1).

:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(60), attributes(portray), spacing(next_argument)] ).
:- set_prolog_flag(answer_write_options,[quoted(true), portray(true), max_depth(60), attributes(portray), spacing(next_argument)] ).
:- set_prolog_flag(py_backtrace_depth,50).
:- set_prolog_flag(py_backtrace, true).
:- set_prolog_flag(py_argv , []).
:- initialization(on_restore1,restore).
:- initialization(on_restore2,restore).

% Declare `metta_python_proxy/1` as dynamic so it can be modified at runtime.
:- dynamic(metta_python_proxy/1).
% Read the content of the file './metta_python_proxy.py' into the variable `String`.
% Then, assert the content of the file as a fact `metta_python_proxy/1`.
:- read_file_to_string('./metta_python_proxy.py', String, []),
   assertz(metta_python_proxy(String)),!.
% Declare `did_load_metta_python_proxy/0` as volatile, meaning it will not be saved to a saved state.
% This is useful when you don't want this predicate to persist across sessions or save states.
:- volatile(did_load_metta_python_proxy/0).
% If `did_load_metta_python_proxy/0` is not already asserted, it asserts the fact to indicate that the proxy has been loaded.
% It retrieves the `metta_python_proxy/1` fact (which contains the content of the file).
% Then, it calls `py_module/2` with the module name and the Python code as arguments.
% The cut (`!`) ensures no backtracking occurs once this is executed.
load_metta_python_proxy :- did_load_metta_python_proxy.
load_metta_python_proxy :-
    assert(did_load_metta_python_proxy),
    metta_python_proxy(String),
    py_module(metta_python_proxy, String),!.
% Ensure that `load_metta_python_proxy/0` is called when the program is initialized (on startup).
% This will trigger the loading of the Python proxy module during initialization.
:- initialization(load_metta_python_proxy).
:- initialization(load_metta_python_proxy,restore).



% py_initialize(, +Argv, +Options)
on_restore1:- ensure_mettalog_py.
on_restore2:- !.
%on_restore2:- load_builtin_module.
%:- load_hyperon_module.



% grab the 1st variable Var
subst_each_var([Var|RestOfVars],Term,Output):- !,
    % replace all occurences of Var with _ (Which is a new anonymous variable)
    subst(Term, Var, _ ,Mid),
    % Do the RestOfVars
    subst_each_var(RestOfVars,Mid,Output).
% no more vars left to replace
subst_each_var(_, TermIO, TermIO).




