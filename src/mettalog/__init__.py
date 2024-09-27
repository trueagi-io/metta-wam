#!/usr/bin/env python3

#if __name__ != "mettalog":f


# Version Space Candidate Elimination inside of MeTTa
# This implementation focuses on bringing this machine learning algorithm into the MeTTa relational programming environment.
# Douglas R. Miles 2023

# Standard Library Imports
import atexit, io, inspect, json, os, re, subprocess, sys, traceback
import sys
import os
import importlib.util
import importlib
import inspect
import types
import inspect
import ast
from typing import *
from typing import List, Dict, Set, Callable
from typing import get_type_hints
from collections import Counter
from glob import glob
from time import monotonic_ns, time
import traceback
from mettalog import *

# Global Variables
deref_op = False
METTALOG_VERBOSE = os.environ.get("METTALOG_VERBOSE")
# 0 = for scripts/demos
# 1 = developer
# 2 = debugger
verbose = 1
if METTALOG_VERBOSE is not None:
    try: verbose = int(METTALOG_VERBOSE) # Convert it to an integer
    except ValueError: ""

def print_exception_stack(e):
    global verbose
    # become increasingly verbose!
    verbose += 1
    print_l_cmt(1, f"Exception occurred: {e}")
    traceback.print_exc()

def print_l_cmt(Level, obj):
    global verbose
    if Level > verbose: return
    print("; " + obj)

print_l_cmt(2, f";; ...doing {__file__}...{__package__} name={__name__}")



try: from typing_extensions import *
except ImportError as e: 
    if verbose >= 0: print_exception_stack(e)

# Third-Party Imports
try: from pyswip import (Atom as PySwipAtom, Term, call, Functor, PL_discard_foreign_frame, PL_new_term_ref, PL_open_foreign_frame,
                         registerForeign, PL_PRUNED, PL_retry, PL_FA_NONDETERMINISTIC, PL_foreign_control, PL_foreign_context, PL_FIRST_CALL, PL_REDO, Variable, Prolog as PySwip)
except ImportError as e:
    if verbose >= 0: print_exception_stack(e)

try: from pyswip.easy import newModule, Query
except ImportError as e:
    if verbose >= 0: print_exception_stack(e)

try: import hyperonpy as hp
except ImportError as e:
    if verbose >= 0: print_exception_stack(e)
try: from hyperon.atoms import *
except ImportError as e:
    if verbose >= 0: print_exception_stack(e)
try: from hyperon.base import AbstractSpace, SpaceRef, GroundingSpace, interpret
except ImportError as e:
    if verbose >= 0: print_exception_stack(e)
try: from hyperon.base import *
except ImportError as e:
    if verbose >= 0: print_exception_stack(e)
try: from hyperon.ext import register_atoms, register_tokens
except ImportError as e:
    if verbose >= 0: print_exception_stack(e)
try: 
    from hyperon.runner import MeTTa
except ImportError as e:
    if verbose >= 0: 
        print_exception_stack(e)



# Error Handling for Janus
try: import janus_swi as janus
except Exception as e:
    if verbose >= 0: print_exception_stack(e)



try:
    from janus_swi import *
    janus.query_once("Y is X+1", {"X":1})
except Exception as e:
    #if verbose>0: print_exception_stack(e)
    try:
        from janus import *
        #janus.query_once("Y is X+1", {"X":1})
    except Exception as e:
        if verbose >= 0: print_exception_stack(e)
# Error Handling for OpenAI
try:
    import openai
    try: openai.api_key = os.environ["OPENAI_API_KEY"]
    except KeyError: ""
except Exception as e:
    if verbose >= 0: print_exception_stack(e)

import os
from importlib import import_module
import importlib.util
import sys
import hyperonpy as hp
from hyperonpy import EnvBuilder, ModuleId
from hyperon.atoms import Atom, AtomType, OperationAtom
from hyperon.base import GroundingSpaceRef, Tokenizer, SExprParser
from hyperon.runner import _PyFileMeTTaModFmt


the_python_runner = globals().get('the_python_runner') or None

class MeTTaLog(MeTTa):
    """This class creates an instance of the MeTTaLog runner"""

    def metta(self): return self

    def __init__(self, cmetta = None, space = None, env_builder = None):
        # super().__init__(cmetta,space,env_builder)
        # parent == self
        #   means no parent MeTTa yet
        self.parent = self
        self.pymods = {}
        global the_python_runner
        the_python_runner = self
        #self.metta = self
        if cmetta is not None:
            self.cmetta = cmetta
        else:
            if space is None:
                space = GroundingSpaceRef()
            if env_builder is None:
                env_builder = hp.env_builder_start()
            hp.env_builder_push_fs_module_format(env_builder, _PyFileMeTTaModFmt, 5000) #5000 is an arbitrary number unlikely to conflict with the arbitrary number chosen by other formats
            #LP-TODO-Next, add an fs_module_fmt arg to the standardized way to init environments, so that
            # the Python user can define additional formats without tweaking any hyperon files.  To make
            # this convenient it probably means making a virtual ModuleFormat base class

            builtin_mods_path = os.path.join(os.path.dirname(__file__), 'exts')
            print_l_cmt(2, f"builtin_mods_path={builtin_mods_path}")
            hp.env_builder_push_include_path(env_builder, builtin_mods_path)
            self.cmetta = hp.metta_new(space.cspace, env_builder)


    def set_cmetta(self, mettac):
        if isinstance(mettac, MeTTa):
            mettac = mettac.cmetta
        self.cmetta = mettac
        #self.load_py_module("hyperon.stdlib")
        #hp.load_module(self.cmetta, "stdlib")
        self.register_atom('no-longer-extend-py!',
                           OperationAtom('no-longer-extend-py!',
                                         lambda name: self.load_py_module(name) or [],
                                         [AtomType.UNDEFINED, AtomType.ATOM], unwrap=False))
        self.register_atom("transform", OperationAtom("transform", lambda pattern, template: the_running_metta_space().subst(pattern, template),
                                                      type_names=[AtomType.ATOM, AtomType.ATOM, AtomType.UNDEFINED], unwrap=False))
        self.register_atom("join", OperationAtom("join", lambda a, b: interpret(the_running_metta_space(), a) + interpret(the_running_metta_space(), b),
                                                 type_names=[AtomType.ATOM, AtomType.ATOM, AtomType.ATOM], unwrap=False))



    def __del__(self):
        hp.metta_free(self.cmetta)

    def __eq__(self, other):
        """Checks if two MeTTa runner handles point to the same runner."""
        return (hp.metta_eq(self.cmetta, other.cmetta))

    def space(self):
        """Gets the space for the runner's top-level module"""
        return GroundingSpaceRef._from_cspace(hp.metta_space(self.cmetta))

    def tokenizer(self):
        """Gets the tokenizer for the runner's top-level module"""
        return Tokenizer._from_ctokenizer(hp.metta_tokenizer(self.cmetta))

    def working_dir(self):
        """Returns the working dir from the environment associated with the runner"""
        return hp.metta_working_dir(self.cmetta)

    def register_token(self, regexp, constr):
        """Registers a token"""
        register_mettalog_token_op(self, regexp, constr)
        self.tokenizer().register_token(regexp, constr)

    def register_atom(self, name, symbol):
        """Registers an Atom"""
        register_mettalog_atom_op(self, name, symbol)        
        self.register_token(name, lambda _: symbol)

    def _parse_all(self, program):
        parser = SExprParser(program)
        while True:
            atom = parser.parse(self.tokenizer())
            if atom is None:
                break
            yield atom

    def parse_all(self, program):
        """Parse an entire program from text into atoms, using the Tokenizer of the runner's top module"""
        return list(self._parse_all(program))

    def parse_single(self, program):
        """Parse the next single token from the text program"""
        return next(self._parse_all(program))

    def load_module_direct_from_func(self, mod_name, private_to, py_loader_func):
        """Loads a module into the runner using a loader function, with the specified name and scope"""
        def loader_func(c_run_context, c_descriptor):
            run_context = RunContext(c_run_context)
            descriptor = ModuleDescriptor(c_descriptor)
            py_loader_func(run_context, descriptor)
        mod_id = hp.metta_load_module_direct(self.cmetta, mod_name, private_to.c_module_descriptor, loader_func)
        err_str = hp.metta_err_str(self.cmetta)
        if (err_str is not None):
            raise RuntimeError(err_str)
        return mod_id

    def load_module_direct_from_pymod(self, mod_name, private_to, pymod_name):
        """Loads a module into the runner directly from a Python module, with the specified name and scope"""
        if not isinstance(pymod_name, str):
            pymod_name = repr(pymod_name)
        loader_func = _priv_make_module_loader_func_for_pymod(pymod_name)
        return self.load_module_direct_from_func(mod_name, private_to, loader_func)

    def load_module_at_path(self, path, mod_name=None):
        """
        Loads a module into the runner directly from resource at a file system path, trying the formats
        from the runner's environment in succession
        """
        mod_id = hp.metta_load_module_at_path(self.cmetta, path, mod_name)
        err_str = hp.metta_err_str(self.cmetta)
        if (err_str is not None):
            raise RuntimeError(err_str)
        return mod_id

    def run(self, program, flat=False):
        return call_mettalog(program,parseWithRust=False,returnMetta=True, flat=flat)

    def run_hyperon(self, program, flat=False):
        """Runs the MeTTa code from the program string containing S-Expression MeTTa syntax"""
        parser = SExprParser(program)
        results = hp.metta_run(self.cmetta, parser.cparser)
        err_str = hp.metta_err_str(self.cmetta)
        if err_str is not None:
            err_str = str(err_str)
            if err_str != '':
                print('RuntimeError: "', err_str, "")
                raise RuntimeError(err_str)

        if results is None:
            return results
        if flat:
            return [Atom._from_catom(catom) for result in results for catom in result]
        else:
            return [[Atom._from_catom(catom) for catom in result] for result in results]

    def load_py_module(self, name):
        """Loads the given python module"""
        if not isinstance(name, str):
            name = repr(name)
        try:
            mod = import_module(name)
            self.pymods[name] = mod
            for n in dir(mod):
                obj = getattr(mod, n)
                if '__name__' in dir(obj) and obj.__name__ == 'metta_register':
                    obj(self)
            return mod
        except Error as e:
            print_exception_stack(e)
            return None

    def import_file(self, fname):
        """Loads the program file and runs it"""
        path = fname.split(os.sep)
        if len(path) == 1:
            path = ['.'] + path
        f = open(os.sep.join(path), "r")
        program = f.read()
        f.close()
        # changing cwd
        # TODO: Changing the working dir will not be necessary when the stdlib ops can access the correct runner context.  See https://github.com/trueagi-io/hyperon-experimental/issues/410
        prev_cwd = os.getcwd()
        os.chdir(os.sep.join(path[:-1]))
        result = self.run(program)
        # restoring cwd
        os.chdir(prev_cwd)
        return result

    # Borrowed impl from Adam Vandervorst

    def lazy_import_file(self, fname):
        path = fname.split(os.sep)
        with open(os.sep.join(self.cwd + path), "r") as f:
            program = f.read()
        self.lazy_run(self._parse_all(program))

    def lazy_run(self, stream):
        for i, (expr, result_set) in enumerate(self.lazy_run_loop(stream)):
            if result_set:
                print(f"> {color_expr(expr)}")
                for result in result_set:
                    print(color_expr(result))
            else:
                print(f"> {color_expr(expr)} /")

    def lazy_run_loop(self, stream):
        interpreting = False
        commented = False
        for expr in stream:
            if expr == S('!') and not commented:
                interpreting = True
            elif expr == S('/*'):
                commented = True
            elif expr == S('*/'):
                commented = False
            elif interpreting and not commented:
                yield expr, interpret(the_running_metta_space(), expr)
                interpreting = False
            elif not commented:
                the_running_metta_space().add_atom(expr)
    def copy(self):
        return self


is_init = True
oper_dict = {}
janus_dict = {}
syms_dict = {}

@staticmethod
def get_metta():
    return the_python_runner

def parent_space():
    return get_metta().parent.space()

def  child_space():
    return get_metta().space()

def   self_space():
    return the_new_runner_space

space_refs = {
    #'&vspace': lambda: the_verspace,
    #'&gptspace': lambda: the_gptspace,
    #'&flybase': lambda: the_flybase,
    '&parent': lambda: parent_space(),
    '&child': lambda: child_space(),
    '&self': self_space}


##def export_to_metta(func):
#    setattr(func, 'metta', True)
#    if verbose > 3: print_cmt(f"{func}={getattr(func, 'export_to_metta', False)}")
#    return func

def export_flags(**kwargs):
    def decorator(func):
        if verbose > 1: print(f";   export_flags({repr(func)})", end=" ")
        for n in kwargs:
            setattr(func, n, kwargs[n])
        if verbose > 1:
            for n in kwargs:
                print(f"{repr(n)}={repr(getattr(func, n, None))}", end=" ")
            print()
        return func
    return decorator

    
def get_call_parts(func):
    sig = inspect.signature(func)
    params = sig.parameters
    # Constructing full parameter strings
    param_parts = []
    params_call_parts = []
    var_args = var_kwargs = None

    for param_name, param in params.items():
        part = param_name
        call_part = param_name
        if param.kind == inspect.Parameter.VAR_POSITIONAL:
            var_args = f'*{param_name}'
            part = var_args
            call_part = var_args
        elif param.kind == inspect.Parameter.VAR_KEYWORD:
            var_kwargs = f'**{param_name}'
            part = var_kwargs
            call_part = var_kwargs
        elif param.default != inspect.Parameter.empty and isinstance(param.default, (int, str)):
            part += f'={repr(param.default)}'
        param_parts.append(part)
        params_call_parts.append(call_part)

    return param_parts, params_call_parts


def add_janus_methods(module, dict = janus_dict):
    for name, func in inspect.getmembers(module):
        if inspect.isfunction(func):
            if getattr(func, "Janus", False) or getattr(func, "MeTTa", False):
                use_name = getattr(func, 'name', name)
                non_underscore_attrs = {attr: getattr(func, attr) for attr in dir(func) if not attr.startswith('_')}
                if len(non_underscore_attrs) == 0: continue
                param_parts, params_call_parts = get_call_parts(func)
                add_to_janus(use_name, param_parts, params_call_parts, func, non_underscore_attrs, janus_dict)


def foreign_framed(func):
    def wrapper(*args, **kwargs):
        swipl_fid = PL_open_foreign_frame()
        result = None
        try:
            result = func(*args, **kwargs)
        except Exception as e:
            if verbose > 0: print_cmt(f"Error: {e}")
            if verbose > 0: traceback.print_exc()
        finally:
            PL_discard_foreign_frame(swipl_fid)
        return result
    return wrapper

@export_flags(MeTTa=True)
def add_python_module(module, dict=oper_dict):
    for name, func in inspect.getmembers(module):
        if inspect.isfunction(func):
            add_python_function(name, func, dict)

def add_python_function(name, func, dict):
    use_name = getattr(func, 'name', name)
    non_underscore_attrs = {attr: getattr(func, attr) for attr in dir(func) if not attr.startswith('_')}
    if len(non_underscore_attrs) == 0: return False
    param_parts, params_call_parts = get_call_parts(func)

    added = False

    if getattr(func, "Janus", False):
        added = add_to_janus(use_name, param_parts, params_call_parts,
                             func, non_underscore_attrs, janus_dict) or added

    if getattr(func, 'MeTTa', False):
        added = add_to_metta(use_name, param_parts, params_call_parts,
                             getattr(func, 'op', "OperationAtom"), getattr(func, 'unwrap', False),
                             func, non_underscore_attrs, dict) or added

    if not added and verbose > 3:
        print_cmt(f"unused({name}({param_parts})) attributes: {non_underscore_attrs}")

    return added


@export_flags(MeTTa=True)
def add_to_metta(name, param_parts, params_call_parts, op_kind, unwrap, func, non_underscore_attrs, dict=oper_dict):
    hyphens, underscores = name.replace('_', '-'), name.replace('-', '_')

    # Construct the param_str from param_parts
    metta_params_str = ' '.join(param_parts)

    s = f"!({hyphens})" if metta_params_str == "" else f"!({hyphens} {metta_params_str})"
    add_to_history_if_unique(s)

    if hyphens in dict:
        return True

    # Construct the param_str from param_parts
    param_str = ', '.join(param_parts)
    # Using params_call_parts in the function call inside lambda
    params_for_call = ', '.join(params_call_parts)

    # Constructing the source code to execute
    src = f'op = {op_kind}("{hyphens}", lambda {param_str}: [{underscores}({params_for_call})], unwrap={unwrap})'
    local_vars = {}

    if verbose > 1:
        print_cmt(f"{src} # {non_underscore_attrs}"[5:])

    try:
        exec(src, globals(), local_vars)
        dict[hyphens] = local_vars['op']
        dict[underscores] = local_vars['op']
        return True
    except SyntaxError as e:
        print_cmt(f"Syntax error in executing: {src}")
        print_cmt(f"Error details: {e}")
        return False


def add_to_janus(name, param_parts, params_call_parts, func, non_underscore_attrs, dict = janus_dict):

    if getattr(func, 'CallsVSpace', False): return False
    #if not getattr(func, "Janus", False): return False

    suggestedName = getattr(func, 'name', name)

    if suggestedName is not None:
        use_name = suggestedName
    else: use_name = name

    for key, item in dict.items():
        if key == use_name:
            return True

    suggestedFlags = getattr(func, 'flags', None)
    if suggestedFlags is None:
        suggestedFlags = 0

    suggestedArity = getattr(func, 'arity', None)
    if suggestedArity is None:
        num_args = len(param_parts)
        suggestedArity = num_args
        func.arity = suggestedArity

    if verbose > 1:
        print_cmt(f"registerForeign({use_name}, arity = {param_parts}/{suggestedArity}, flags = {suggestedFlags} ) {non_underscore_attrs}")

    #if not getattr(func, "Janus", False): return False
    dict[use_name] = func
    registerForeign(func, arity = suggestedArity, flags = suggestedFlags )
    return True



import importlib

def find_methods_with_annotation(module_name, annotation_string):
    # Load the module
    module = importlib.import_module(module_name)

    # List to hold methods that match the annotation
    matching_methods = []

    # Iterate over all attributes in the module
    for attr_name in dir(module):
        attr = getattr(module, attr_name)

        # Check if the attribute is a callable (function/method)
        if callable(attr):
            # Check if the attribute has annotations
            if hasattr(attr, '__annotations__'):
                for annot in attr.__annotations__.values():
                    # Check if the annotation matches the given string
                    if annotation_string in str(annot):
                        matching_methods.append(attr_name)

    return matching_methods

# Example usage
# methods = find_methods_with_annotation('math', 'float')
# print(methods)


# Function to find subclasses of clazz in a module
def find_subclasses_of(module, clazz):
    subclasses = {}
    for name, obj in inspect.getmembers(module):
        if inspect.isclass(obj) and issubclass(obj, clazz) and obj is not clazz:
            subclasses[name] = obj
    return subclasses.items()

    for name, claz in find_subclasses_of(module, AbstractSpace):
        print(f"found class {claz} with name {name}")
        # inspect the constructor and syntesize a function that will create an object


@export_flags(MeTTa=True)
def add_to_swip(name, dict = oper_dict):
    hyphens, underscores = name.replace('_', '-'), name.replace('-', '_')
    repl_loop.add_to_history_if_unique(f"!({hyphens})")
    if hyphens not in dict:
        src, local_vars = f'op = lambda : [swip_exec("{underscores}")]', {}
        exec(src, {}, local_vars)
        if verbose > 1: print_cmt(f"swip: {hyphens}")
        dict[hyphens] = OperationAtom(hyphens, local_vars['op'], unwrap=False)


vspace_ordinal = 0

# Mainly a sanity loading test class
class MettaLearner:
    pass  # Use 'pass' as a placeholder for an empty class body


realMetta = None

def metta_register_v(metta):
    print_l_cmt(2, f";; metta_register_v={get_metta()}/{metta}")
    global realMetta
    try:
        if not metta is None:
            if not isinstance(metta, MeTTaLog):
                realMetta = metta
  #get_metta().set_cmetta(metta)
  #print(";;", metta.pymods)
        #print(";;", get_metta().pymods)

        return register_vspace_atoms_for_ra(realMetta)
    except Exception as e:
        if verbose >= 0: print_exception_stack(e)


@export_flags(MeTTa=True)
@register_atoms(pass_metta=True)
def register_vspace_atoms(metta):
    return register_vspace_atoms_for_ra(metta)

def register_vspace_atoms_for_ra(mettaIn):

    global realMetta

    if mettaIn is None:
        mettaIn = realMetta

    metta = mettaIn

    global oper_dict
    if verbose > 1: print_cmt(f"register_vspace_atoms metta={metta} get_metta() = {get_metta()} {self_space_info()}")

    counter = 0
    #if not metta is None: get_metta().set_cmetta(metta)

    if not isinstance(metta, VSpace):
        if not metta is None:
            get_metta().parent = metta

    def new_value_atom_func():
        nonlocal counter
        counter += 1
        return [ValueAtom({'A': counter, 6: 'B'})]


    # DMILES:  I actujally like the behaviour below.
    newValueAtom = OperationAtom('new-value-atom', new_value_atom_func, unwrap=False)
    # (new-value-atom)
    # this was stored in the space..
    # !(match &self $ $)
    # and each time the space is matched the counter will be incremented
    # !(match &self $ $)
    # !(match &self $ $)
    # (new-value-atom)
    # they share a counter
    # !(match &self $ $)

    runnerAtom = G(get_metta(), AtomType.ATOM)
    current_module = sys.modules['mettalog']
    add_python_module(current_module, dict = oper_dict)
    oper_dict.update({
        r"new-v-space": OperationAtom('new-v-space', lambda: [G(SpaceRef(VSpace()))], unwrap=False),
        r"the-v-space": OperationAtom('the-v-space', lambda: [G(SpaceRef(the_verspace))], unwrap=False),


        r"new-value-atom": newValueAtom,
    #'&self': runnerAtom,
    #'&swip': ValueAtom(swip),



        '&my-dict': ValueAtom({'A': 5, 6: 'B'}),
        'get-by-key': OperationAtom('get-by-key', lambda d, k: d[k]),

        # Our FFI to PySWIP
        #'load-vspace': OperationAtom('load-vspace', lambda: [load_vspace()]),
        'mine-overlaps': OperationAtom('mine-overlaps', lambda: [mine_overlaps()]),
        'try-overlaps': OperationAtom('try-overlaps', lambda: [try_overlaps()]),
        'load-flybase-full': OperationAtom('load-flybase-full', lambda: [load_flybase("inf")]),
        'load-flybase-tiny': OperationAtom('load-flybase-tiny', lambda: [load_flybase(20000)]),

        r"fb.test-nondeterministic-foreign": OperationAtom('test-nondeterministic-foreign', lambda: test_nondeterministic_foreign, unwrap=False),

        'vspace-main': OperationAtom('vspace-main', vspace_main),
        'mettalog::vspace-main': OperationAtom('vspace-main', lambda *args: [vspace_main(*args)]),
        'swip-exec': OperationAtom('swip-exec', lambda s: [swip_exec(s)]),
        'py-eval-int': OperationAtom('py-eval-int', lambda s: [eval(s)]) })

    #add_python_module(sys.modules[__name__], dict = oper_dict)
    #print("oper_dict", oper_dict)
    return oper_dict


# For now lets test with only  Atoms
@register_tokens(pass_metta=True)
def register_vspace_tokens(metta):

    if verbose > 1: print_cmt(f"register_vspace_tokens metta={metta} {self_space_info()}")

    if hasattr(get_metta(), "set_cmetta"):
        get_metta().set_cmetta(metta.cmetta)

    if hasattr(get_metta(), "cmetta"):
        get_metta().cmetta = metta.cmetta


    if not isinstance(metta, VSpace):
        get_metta().parent = metta

    def run_resolved_symbol_op(m, atom, *args):
        expr = E(atom, *args)
        if m is None: m = metta

        if verbose > 1: print_cmt(f"run_resolved_symbol_op: atom={atom}, args={args}, expr={expr} metta={metta} {self_space_info()}")
        result1 = hp.metta_evaluate_atom(m.cmetta, expr.catom)
        result = [Atom._from_catom(catom) for catom in result1]
        if verbose > 1: print_cmt(f"run_resolved_symbol_op: result1={result1}, result={result}")
        return result

    def resolve_atom(metta, token):
        return _resolve_atom(metta, token, verbose)

    def _resolve_atom(metta, token, verbose):
        # TODO: nested modules...
        verbose = verbose + 1

        if token is None: return token

        if verbose > 1: print_cmt(f"resolve_atom: token={token}/{type(token)} metta={metta}")

        if "::" in token:
            runner_name, atom_name = token.split('::')
        else:
            atom_name = token
            runner_name = None

        if atom_name in oper_dict:
            if verbose > 1: print_cmt(f"resolve_atom: token={token} metta={metta}")
            return oper_dict[atom_name]

        atom_name2 = atom_name.replace('_', '-')

        if atom_name2 in oper_dict:
            if verbose > 0: print_cmt(f"resolve_atom: token={token} metta={metta}")
            return oper_dict[atom_name2]

        if atom_name == "vspace-main":
            vspace_main()
            return
        # FIXME: using `run` for this is an overkill

        if runner_name:
            ran = metta.run('! ' + runner_name)[0][0];
            if verbose > 1: print_cmt(f"resolve_atom: token={token} ran={type(ran)} metta={metta} {self_space_info()}")
            try:
                this_runner = ran.get_object()
            except Exception as e:
                # If there's an error, print it
                #print(f"Error ran.get_object: {e}")
                this_runner = get_metta()

        
        #if !isinstance(this_runner, MeTTa): this_runner = metta


        atom = this_runner.run('! ' + atom_name)[0][0]

        # A hack to make get_metta()::&self work
        # TODO? the problem is that we need to return an operation to make this
        # work in parent expressions, thus, it is unclear how to return pure
        # symbols

        if atom.get_type() == hp.AtomKind.GROUNDED:
            return atom

        # TODO: borrow atom type to op
        return OperationAtom( token, lambda *args: run_resolved_symbol_op(get_metta(), atom, *args), unwrap=False)

    def resolve_underscores(metta, token):
        atom_name = token.replace('_', '-')
        if atom_name in oper_dict:
            if verbose > 1: print_cmt(f"resolve_atom: token={token} metta={metta}")
            return oper_dict[atom_name]

    syms_dict.update({
        '&gptspace': lambda _: G(asSpaceRef(getSpaceByName('&gptspace'))),
        '&flybase': lambda _: G(asSpaceRef(getSpaceByName('&flybase'))),
        #'&vspace': lambda _: G(getSpaceByName('&vspace')),
        #'&vbase_class': lambda _: G((the_verspace)),
        '&parent_ref': lambda _: G(asSpaceRef(getSpaceByName("&parent"))),
        '&parent': lambda _: G((getSpaceByName("&parent"))),
        '&child': lambda _: G((getSpaceByName("&child"))),
        '&the_runner': lambda _: ValueAtom(get_metta()),
        '&the_metta': lambda _: ValueAtom(get_metta().parent),
        r"[^\s]+::[^\s]+": lambda token: resolve_atom(metta, token)
    #r"[^\s][^\s]+[^!]": lambda token: resolve_atom(metta, token)
    #r"[^\s]+_[^\s]+": lambda token: resolve_underscores(metta, token)
    })
    for key in syms_dict:
        if key.startswith("&"):
            repl_loop.add_to_history_if_unique(f"!{key}")
    return syms_dict


def res_unify(s, v):
    if isinstance(v, str):
        if isinstance(s, str):
            return s == v
        return s.unify(swipAtom(v))
    return s.unify(v)


class PySwipQ(Query):

    def __init__(self, *terms, **kwargs):
        if verbose > 1:
            for obj in terms:
                println(obj)
        super().__init__(*terms, **kwargs)

    def nextSolution(self):
        return Query.nextSolution()
        #return PL_next_solution(Query.qid)
    #nextSolution = staticmethod(nextSolution)

    def cutQuery(self):
        Query.cutQuery()
        #PL_cut_query(Query.qid)
    #cutQuery = staticmethod(cutQuery)

    def closeQuery(self):
        Query.closeQuery()
  #    if Query.qid is not None:
  #      PL_close_query(Query.qid)
  #      Query.qid = None
  #closeQuery = staticmethod(closeQuery)

access_error = True

@export_flags(MeTTa=True, name="print", unwrap=True)
def println(orig, prefix=""):
    return println_impl(orig, prefix)

def unwrap_pyobjs(metta_obj):
    if isinstance(metta_obj, ExpressionAtom):
        return metta_obj
    elif isinstance(metta_obj, GroundedAtom):
        metta_obj = metta_obj.get_object()
    if isinstance(metta_obj, ValueObject):
        metta_obj = metta_obj.value
    return metta_obj

def flush_console():
    try:
        if sys.__stdout__ is not None: sys.__stdout__.flush()
    except Exception: ""
    try:
        if sys.__stderr__ is not None: sys.__stderr__.flush()
    except Exception: ""
    try:
        if sys.stderr is not None and not (sys.stderr is sys.__stderr__): sys.sys.stderr.flush()
    except Exception: ""
    try:
        if sys.stdout is not None and not (sys.stdout is sys.__stdout__): sys.sys.stdout.flush()
    except Exception: ""



def println_impl(orig, prefix=""):
    """
    Prints the given object and returns it.

    Args:
        orig: The object to be printed.

    Returns:
        The same object that was passed in.
    """
    try:
        prefix_print(prefix, orig)
    except Exception as e:
        if verbose > 0: print(f"println-Error: {e}")
    flush_console()
    return orig

def prefix_print(prefix, orig):

    obj = unwrap_pyobjs(orig)

    if isinstance(obj, str):
        objlns = obj.splitlines()
        for r in objlns:
            print(prefix, r)
        return

    if isinstance(obj, (AbstractSpace, GroundingSpaceRef, SpaceRef)):
        s = obj
        f = getattr(s, "atom_count", None)
        if f is not None: prefix_print(prefix + " atom-count:", f())
        f = getattr(s, "get_atoms", None)
        if f is not None:
            prefix_print(prefix + " ", f())
            return
        f = getattr(s, "atoms_iter", None)
        if f is not None:
            prefix_print(prefix + " ", f())
            return
        f = getattr(s, "query", None)
        if f is not None:
            prefix_print(prefix + " ", f(V("_")))
            return

    if isinstance(obj, (int, float)):
        prefix_print(prefix + " ", repr(obj))
        return

    try:
        if hasattr(obj, '__next__'):  # Check if obj is an iterator
            while True:
                try:
                    prefix_print(prefix + " ", next(obj))
                except StopIteration:
                    break
        else:
            for r in obj:             # Check if obj is an iteratable
                prefix_print(prefix + " ", r)

        return

    except TypeError: ""

    #from pyswip import Term, Variable, Functor, Atom as PySwipAtom
    if isinstance(obj, (Term, Variable)):
        fn = Functor("writeln")
        print(prefix, end=' tv:')
        call(fn(obj))
        return

    if hasattr(obj, '__str__'):
        prefix_print(prefix + " s:", str(obj))
        return

    if isinstance(obj, (Functor, PySwipAtom)):
        fn = Functor("writeq")
        print(prefix, end=' q')
        call(fn(obj))
        return

    prefix_print(prefix + " ", repr(obj))

@export_flags(MeTTa=True)
def pt1(obj):
    if isinstance(obj, str):
        print(f"{repr(obj)}", end= " ")
    elif not isinstance(obj, (Term, Variable)):
        print(f" pt: {type(obj)}={str(obj)}={repr(obj)}", end= " ")
        if isinstance(obj, list):
            obj = obj[0]
            print(f" pt(0): {type(obj)}={str(obj)}={repr(obj)}", end= " ")
    else:
        fn = Functor("pp")
        call(fn(obj))
    return obj

@export_flags(MeTTa=True)
def pt(*objs):
    r = objs
    for o in objs:
        if isinstance(o, str):
            print(o, end="")
        else: r = pt1(o)
    print()
    return r



@export_flags(Janus=True)
def add_to_history_if_unique_pl(item, position_from_last=1):
    try: import readline
    except ImportError: import pyreadline3 as readline
    for i in range(1, readline.get_current_history_length() + 1):
        if readline.get_history_item(i) == item: return
    insert_to_history(item, position_from_last)


@export_flags(MeTTa=True)
def print_l_e(obj):
    if obj is None:
        print("None!")
        return obj

    if isinstance(obj, str):
        print(obj)
        return obj

    try:
        # Attempt to iterate over the object
        for item in obj:
            try:
                color_expr(item)
            except Exception:
                print(item)
    except TypeError:
        # If a TypeError is raised, the object is not iterable
        # if verbose>0: print_cmt(type(obj))
        print(obj)
    return obj



def is_iterable(obj, exclude=(str, bytes)):
    try:
        iter(obj)
        return not isinstance(obj, exclude)  # Exclude strings and bytes
    except TypeError:
        return False


@export_flags(MeTTa=True)
def test_s(metta_obj: Atom) -> Atom:
    circles = Circles()
    pt(metta_obj)
    swip_obj = m2s(circles, metta_obj)
    pt(swip_obj)
    new_mo = s2m(circles, swip_obj)
    pt(new_mo)
    return new_mo

def the_running_metta_space():
    global the_new_runner_space
    global the_python_runner
    #if get_metta().parent!=get_metta():
    #    return get_metta().parent.space()
    if the_new_runner_space is not None: return the_new_runner_space
    return get_metta().parent.space()



def res_unify(s, v):
    if isinstance(v, str):
        if isinstance(s, str):
            return s == v
        return s.unify(swipAtom(v))
    return s.unify(v)


def MkExpr(py_list):
    list = py_list
    return E(*list)

class IteratorAndConversionDict:
    def __init__(self, iterator=None, conversion_dict=None):
        self.iterator = iterator
        self.conversion_dict = conversion_dict if conversion_dict is not None else {}

    def set_iterator(self, iterator):
        self.iterator = iterator

    def set_conversion_dict(self, conversion_dict):
        self.conversion_dict = conversion_dict

    def get_iterator(self):
        return self.iterator

    def get_conversion_dict(self):
        return self.conversion_dict



@export_flags(Janus=True)
def swip_to_metta_wrapper(swip_obj, metta_obj):
    circles = Circles()
    result1 = m2s(circles, s2m(circles, swip_obj))
    result2 = m2s(circles, metta_obj)
    #metta_obj.unify(m2s(circles,result))
    return result2.unify(result1)
    #return True

@export_flags(Janus=True)
def metta_to_swip_wrapper(metta_obj, swip_obj):
    circles = Circles()
    result1 = m2s(circles, metta_obj)
    result2 = m2s(circles, swip_obj)
    #swip_obj.unify(result)
    return result2.unify(result1)
    #return True


NeedNameSpaceInSWIP = True
@export_flags(MeTTa=True, unwrap=True)
def load_vspace():
    global NeedNameSpaceInSWIP
    #os.path.dirname(__file__)}/canary/metta_interp
    metta_interp_file = "/opt/hyperon/metta-wam/src/canary/metta_interp"
    load_metta_interp = f"user:ensure_loaded('{metta_interp_file}')"
    swip_exec(load_metta_interp)
    if NeedNameSpaceInSWIP:
        NeedNameSpaceInSWIP = False
        swip.retractall("was_asserted_space('&self')")
        swip.assertz("py_named_space('&self')")

@export_flags(MeTTa=True, CallsVSpace=True)
def mine_overlaps():
    load_vspace()
    swip_exec("mine_overlaps")
    #readline_add_history("!(try-overlaps)")


@export_flags(MeTTa=True, CallsVSpace=True)
def try_overlaps():
    load_vspace()
    swip_exec("try_overlaps")

@export_flags(MeTTa=True, CallsVSpace=True)
def learn_vspace():
    load_vspace()
    swip_exec("learn_vspace(60)")

@export_flags(MeTTa=True, CallsVSpace=True)
def mettalog_repl():
    load_vspace()
    swip_exec("repl")


@export_flags(MeTTa=True)
def register_mettalog_op_new(fn, n):
    arg_types = [AtomType.ATOM] * (n) + [AtomType.UNDEFINED]
    op = OperationAtom(fn, lambda *args:
                       print_cmt(f"eval_mettalog('{fn}', {args})") +
                       eval_mettalog(fn, *args),
                       type_names=arg_types,
                       unwrap=True)
    get_metta().register_atom(fn, op)
    return op


@export_flags(MeTTa=True, CallsVSpace=True)
def use_mettalog():
    print_l_cmt(2, "uuuusssseeee-mmmmetttttaalogg")
    load_vspace()
    return replace_space_rust_functions()

@export_flags(MeTTa=True)
def replace_space_rust_functions():
    op = register_mettalog_op("pragma!", 2)
    #return op
    register_mettalog_op("if", 3)
    register_mettalog_op("if", 2)
    register_mettalog_op("let", 3)
    register_mettalog_op("let*", 3)
    register_mettalog_op("eval", 1)
    op = register_mettalog_op("match", 3)
    #op = register_mettalog_op("import!", 2)
    return op

@export_flags(MeTTa=True)
def override_metta(meTTa):
    #replace_space_rust_functions()
    pass

@export_flags(MeTTa=True)
def register_mettalog_op(fn, n):
    arg_types = [AtomType.ATOM] * (n) + [AtomType.UNDEFINED]
    n_args = ', '.join(['arg' + str(i) for i in range(n)])
    local_vars = {}
    src = f'lop = lambda {n_args}: eval_mettalog("{fn}", {n_args})'
    exec(src, globals(), local_vars)
    lop = local_vars['lop']
    #print_cmt(src) print_cmt(type(get_metta()))
    op = OperationAtom(fn, lop, type_names=arg_types, unwrap=False)
    oper_dict[fn] = op
    get_metta().register_atom(fn, op)

    return op

def eval_mettalog(fn, *args):
    print_cmt(f"eval_mettalog('{fn}', {args})")
    return list(_eval_mettalog(fn, args))

def _eval_mettalog(fn, *args):
    circles = Circles()
    expr = [fn] + list(args) # Prepend fn to args list
    swip_obj = m2s(circles, expr)
    flush_console()
    call_sexpr = Functor("call_sexpr", 5)
    #user = newModule("user")
    X = Variable()
    q = PySwipQ(call_sexpr(argmode, selected_space_name, str(expr), swip_obj, X))
    while q.nextSolution():
        flush_console()
        r = X.value
        println(r)
        yield s2m(circles, r)
    q.closeQuery()
    flush_console()

@export_flags(MeTTa=True, CallsVSpace=True)
def mettalog_pl():
    load_vspace()
    swip_exec("break")

@export_flags(CallsVSpace=True)
def load_flybase(size):
    load_vspace()
    swip_exec(f"load_flybase({size})")
    #readline_add_history("!(mine-overlaps)")


@export_flags(MeTTa=True)
def swip_exec(qry):
    swip_exec_ff(qry)

@foreign_framed
def swip_exec_ff(qry):
    #from src import swip
    #if is_init==True:
    #   print_cmt("Not running Query: ",qry)
    #   return
    for r in swip.query(qry):
        print_cmt(r)



def timeFrom(w, t0):
    elapsed_ns = monotonic_ns() - t0
    elapsed_s = elapsed_ns / 1e9
    elapsed_ms = elapsed_ns / 1e6
    elapsed_us = elapsed_ns / 1e3

    if elapsed_s >= 1:
        print_cmt(f"{w} took {elapsed_s:.5f} seconds")
    elif elapsed_ms >= 1:
        print_cmt(f"{w} took {elapsed_ms:.5f} milliseconds")
    else:
        print_cmt(f"{w} took {elapsed_us:.5f} microseconds")



def call_mettalog(line, parseWithRust = False, returnMetta = False, flat=False):

    if parseWithRust or returnMetta: circles = Circles()

    if parseWithRust:
        expr =  get_metta().parse_single(sline)
        if verbose > 1: print_cmt(f"% S-Expr {line}")
        if verbose > 1: print_cmt(f"% M-Expr {expr}")        
        swip_obj = m2s(circles, expr);
        if verbose > 1: print_cmt(f"% P-Expr {swip_obj}")
    else:
        swip_obj = line

    flush_console()
    call_sexpr = Functor("call_sexpr", 5)
    user = newModule("user")
    X = Variable()
    q = PySwipQ(call_sexpr(argmode, selected_space_name, str(line), swip_obj, X))
    while q.nextSolution():
        flush_console()
        if returnMetta: yield expr, s2m1(circles, X.value)
        else: yield X.value
    q.closeQuery()
    flush_console()




def redirect_stdout(inner_function):
    old_stdout = sys.stdout # Save the current stdout stream
    new_stdout = io.StringIO() # Create a new StringIO buffer
    sys.stdout = new_stdout # Redirect stdout to the new buffer
    try:
        inner_function() # Execute the inner function
    finally:
        sys.stdout = old_stdout # Restore the original stdout stream
    output = new_stdout.getvalue() # Retrieve the output from the new buffer
    new_stdout.close() # Close the new buffer
    return output

@staticmethod
def vspace_init():
    if getattr(vspace_init, "is_init_ran", False) == True:
        return
    vspace_init.is_init_for_ran = True

    t0 = monotonic_ns()
    #os.system('clear')
    print_l_cmt(1, underline(f"Version-Space Init: {__file__}\n"))
    #import site
    #print_cmt ("Site Packages: ",site.getsitepackages())
    #test_nondeterministic_foreign()
    #if os.path.isfile(f"{get_metta().cwd}autoexec.metta"):
    #    get_metta().lazy_import_file("autoexec.metta")
    # @TODO fix this metta_to_swip_tests1()
    #load_vspace()
    current_module = sys.modules['mettalog']
    add_janus_methods(current_module, dict = oper_dict)
    from mettalog.ff_tests import reg_pyswip_foreign
    f = reg_pyswip_foreign
    #redirect_stdout(f)
    f()
    if verbose > 0: timeFrom("init", t0)
    flush_console()


# Exporting to another CSV (for demonstration)
#df.to_csv("exported.csv", index=False)
#print_cmt("\n### Data Exported to 'exported.csv' ###")


from collections.abc import Iterable

def is_lisp_dashed(s):
    pattern = re.compile('^[A-Za-z0-9-_:]+$')
    return bool(pattern.match(s))

def item_string(lst, functor=""):
    if isinstance(lst, str):
        if len(lst) == 0:
            return '""'
        if any(char in lst for char in [' ', '"', "'", "(", ")", ".", "\\"]):
            return json.dumps(lst)
        if isinstance(lst, (int, float)):
            return repr(lst)
        if is_float_string(lst):
            return repr(float(lst))
        if lst.isdigit():
            return repr(int(lst))
        if lst.isalnum():
            if lst[0].isdigit(): return json.dumps(lst)
            return lst
        if lst == "#":
            return lst
        if is_lisp_dashed(lst):
            return lst
        return json.dumps(lst)

    try:
        if isinstance(lst, Iterable):
            return '(' + functor + ' '.join([item_string(vv) for vv in lst]) + ')'
        else:
            return str(lst)
    except TypeError:
        return str(lst)

def list_string(lst, functor="# "):
    try:
        if isinstance(lst, Iterable) and not isinstance(lst, str):
            if len(lst) == 0:
                return '()'
            return '(' + functor + ' '.join([item_string(vv) for vv in lst]) + ')'
        else:
            return item_string(lst)
    except TypeError:
        return item_string(lst)

def is_float_string(s):
    return bool(re.fullmatch(r'[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?', s))

def import_metta_file(string):
    global argmode
    if argmode == "mettalog":
        load_vspace()
        if verbose > 0: print("; load_metta_file=", string)
        swip_exec(f"load_metta_file('{selected_space_name}','{string}')")
    else: 
        if verbose > 0: print("; import_file=", string)
        #get_metta().import_file(string)
        space = get_metta().space()
        space_obj = space #.get_object()
        print(f"space_obj.cspace={space_obj.cspace}")
        hp.load_ascii(string, space_obj.cspace)
        #['Space', 'Symbol', 'Unit'],
        return [Atoms.UNIT]




import os
import sys



@export_flags(MeTTa=True)
def vspace_main(*args):
    is_init = False
    #os.system('clear')
    t0 = monotonic_ns()
    if verbose > 0: print_cmt(underline("Version-Space Main\n"))
    flush_console()
    #if is_init==False: load_vspace()
    #if is_init==False: load_flybase()
    #if is_init==False:

    global needsRepl
    global argmode
    argnum = 0

    if isinstance(args, str):
        args = [args]

    if isinstance(args, list):
        while argnum < len(args):
            arg = args[argnum]
            print(f"arg={arg}")
            argnum = argnum + 1
            flush_console()
            if arg in ["--ast"]:
                needsRepl = False
                module_name = args[argnum]
                argnum = argnum + 1
                print_ast(module_name)
                continue
            if isinstance(arg, str):
                needsRepl = False
                handle_arg(arg)
                continue

    flush_console()

    if needsRepl:
        needsRepl = False
        repl_loop.repl(get_metta(),mode=argmode)        

    if verbose > 1: timeFrom("main", t0)
    flush_console()

def vspace_main_from_python(sysargv1toN=sys.argv[1:]):
    vspace_main(sysargv1toN)

if __name__ == "__main__":
    needsRepl = True

def handle_arg(string, skip_filetypes=['.metta', '.md','.pl', '.png', '.jpg', '.obo']):

    lower = string.lower()

    global needsRepl
    global argmode
    if lower in ["--repl"]:
        needsRepl = False
        repl_loop.repl(get_metta(),mode=argmode)
        return

    if lower in ["--metta","--mettalog","--python"]:
        argmode = lower.lstrip('-')
        if verbose > 0: print("; argmode=", argmode)
        return

    if os.path.isfile(string):
        if lower.endswith('.metta'):
            import_metta_file(string)
            return

    global needed_Skip
    if string == "--analyze": sys.exit(needed_Skip)

    if os.path.isdir(string):
        # If it's a directory, traverse it
        for root, _, files in os.walk(string):
            for file in files:
                try:
                    if any(file.endswith(ext) for ext in skip_filetypes):
                        if verbose > 0: print_cmt(f"Skipping file: {file}")
                        continue
                    handle_arg([os.path.join(root, file)], skip_filetypes)
                except Exception as e:
                    print_cmt(f"An error occurred while processing {string}: {e}")
        return

    elif os.path.isfile(string):
        if lower.endswith('.csv'):
            analyze_csv_basename(string, sep=',')
            return
        elif lower.endswith('.tsv'):
            analyze_csv_basename(string, sep='\t')
            return
        else:
            # Read only the first few lines
            try:
                analyze_csv_basename(string)
            except UnicodeDecodeError:
                print_cmt(f"Passing in file: {string}")
                with open(string, 'r') as file:
                    for i, line in enumerate(file):
                        if i >= 10:
                            break
                        print_cmt(line.strip())
            return

    print_cmt(f"Skipping: {string}")

mettaLearner = MettaLearner()


def process_module(main_module, combined_dict, depth):
    if combined_dict is None:
        combined_dict = {}
    # Iterate over all members of the module
    modname = main_module.__name__
    stuff = modules_registry.get(modname)
    if stuff is not None:
        return combined_dict
    modules_registry[modname] = main_module
    before = set(op_registry_atoms.keys())
    load_module_to_rust(main_module)
    after = set(op_registry_atoms.keys())
    new_operations = after - before
    if(len(new_operations)!=0):
       new_operationsDict = {modname: op_registry_atoms[modname] for modname in new_operations}
       print_operations_table(modname, new_operationsDict)
       combined_dict.update(new_operationsDict)
       return combined_dict
    # interception    
    for name, member in inspect.getmembers(main_module):
        if inspect.ismodule(member):
            # Recursive call commented out, remove continue to enable
            continue
            # Check if module is already processed or if recursion is too deep
            if name not in modules_registry:
                if depth < 1:
                    print_l_cmt(3, f"skip-load module: {name}")
                    continue
                print_l_cmt(2, f"rec-load module: {name}")
                # Register module and recursively process it
                # modules_registry[name] = member
                combined_dict = process_module(member, combined_dict, depth - 1)
            continue  # This might be redundant due to the continue above

        # Check if the object is a function and matches certain criteria
        if inspect.isfunction(member):
            if ((depth == 2 or 
                 name.endswith('_ops') or 
                 name.startswith('metta_register') or 
                 name.endswith('_for_ra') or 
                 name.endswith('_atoms') or 
                 hasattr(member, 'register_atoms_internal') or 
                 hasattr(member, 'register_atoms') or 
                 name.endswith('_for_rt') or 
                 name.endswith('_tokens') or 
                 hasattr(member, 'register_tokens_internal') or 
                 hasattr(member, 'register_tokens'))):
                if depth < 0:
                    print_l_cmt(2, f"skipping Function: {name}")
                    continue
                print_l_cmt(2, f"Found function matching criteria: {name}/{member} in {main_module}")
                new_dict = update_dict_from_function(name, member, {})
                print_operations_table(name, new_dict)
                combined_dict.update(new_dict)
                op_registry_atoms.update(combined_dict)

    return combined_dict


def load_module(name_or_path):
    print_l_cmt(2, f"Searching for: {name_or_path}")
    before = set(sys.modules.keys())

    module = None  # Initialize module variable

    if name_or_path.endswith('.py'):
        file_path = os.path.abspath(name_or_path)
        module_name = os.path.splitext(os.path.basename(file_path))[0]
        print_l_cmt(1, f"Detected .py file. Loading from path: {file_path}, as module: {module_name}")
        spec = importlib.util.spec_from_file_location(module_name, file_path)
        if spec is None:
            raise ImportError(f"Could not load module from {file_path}")
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    else:
        print_l_cmt(1, f"Assuming {name_or_path} is a module name.")
        module = importlib.import_module(name_or_path)

    after = set(sys.modules.keys())
    new_modules = after - before
    print_l_cmt(2, f"New modules loaded: {new_modules}")
    return module, {name: sys.modules[name] for name in new_modules}

def update_dict_from_function(name, func, current_dict):
    global the_python_runner
    print_l_cmt(2, f"Processing function: {name}/{func.__name__}/{func.__qualname__}")
    try:
        sig = inspect.signature(func)
        params = sig.parameters
        if len(params) == 0:
            print_l_cmt(2, f"Using {name}: with no arguments.")
            result = func()
        elif len(params) == 1:
            print_l_cmt(2, f"Using {name}: with one argument.")
            # Assuming get_metta() is a placeholder for actual argument passing
            print_l_cmt(2, f"the_python_runner={the_python_runner}")
            result = func(the_python_runner)  # Replace None with actual argument if needed
        else:
            print_l_cmt(2, f"Skipping {name}: requires more than one argument.")
            return current_dict  # Skip functions requiring more than one argument
        print_l_cmt(2, f"Function result: {result}")
        if isinstance(result, dict):
            current_dict.update(result)
            return current_dict
        current_dict[name] = func
        return current_dict
    except Exception as e:
        print_l_cmt(1, f"Error processing function {name}: {e}")
        print_exception_stack(e)
    return current_dict


def register_mettalog_atom_op(m, name, symbol):
    if op_registry_atoms.get(name) is None:
        #print_l_cmt(2, f"register_atom: {name} {symbol}")
        print_operations_table(name, {name:symbol}, quiet = True)
    op_registry_atoms[name]=symbol


def register_mettalog_token_op(m, regexp, constr):
    if op_registry_atoms.get(regexp) is None:
        print_l_cmt(2, f"register_token: {regexp} {constr}")
    op_registry_tokens[regexp]=constr


def print_operations_table(tablename, operations_dict, quiet = False):

    if len(operations_dict)==0: return
    # Find the length of the longest key
    max_key_length = max(len(name) for name in operations_dict.keys())
    # Make the column 4 characters wider than the longest key
    column_width = min(9, max_key_length + 4)

    if not quiet: print_l_cmt(2,'')  # Adds a new line for clarity
    if not quiet: print_l_cmt(2,f"Operations Table: {tablename}")
    # Adjust the Name column width dynamically
    header = f"{'Name':<{column_width-2}} | {'Operation'}"
    #print(';', end='')
    if not quiet: print_l_cmt(2,header)
    #print(';', end='')
    if not quiet: print_l_cmt(2,"-" * min(80, len(header)))

    global deref_op
    for name, operation in operations_dict.items():
        original_operation = operation
        # Attempt to unwrap the operation if it supports the get_object() method
        unwrapped_operation = None
        try:
            if deref_op:
                unwrapped_operation = operation.get_object()
            # Only override the original operation if unwrapped_operation is not None
            if unwrapped_operation is not None:
                operation = (unwrapped_operation)
        except AttributeError:
            pass  # If get_object() doesn't exist or fails, proceed with the original operation

        unwrapped_operation = None
        try:
            #strplus  =   (" " + operation.unwrap )
            if deref_op:
                unwrapped_operation = operation.get_object()
            # Only override the original operation if unwrapped_operation is not None
            if unwrapped_operation is not None:
                operation = unwrapped_operation
        except AttributeError:
            pass  # If get_object() doesn't exist or fails, proceed with the original operation

        props = {}
        operation_str = summarize_data_tree(props, operation) # Get the string representation of the operation
        if len(props) > 0:
            operation_str = summarize_data_tree({}, props)
        #operation_str = capture_print_output(print_data_tree, operation, max_depth=2)
        operation_type = type(operation).__name__  # Get the type name of the operation
        func_op = props.get('op')
        func_str = summarize_data_tree({}, func_op)
        # arity is the number of time ":" appears in the string  func_str
        kwarity = func_str.count("**")
        dwarity = func_str.count("=") 
        varity = func_str.count("*") - (kwarity*2)
        arity = func_str.count(",") + func_str.count(")") - func_str.count("()") + 1
        lowarity = arity - dwarity - varity
        higharity = arity
        #print_l_cmt(2,'; ', end='')
        if lowarity==higharity:
            arityStr = f" a={lowarity}"
        else:
            arityStr = f" a={lowarity}-{higharity}"

        props['descr'] = operation_str
        props['func_str'] = func_str
        props['original'] = original_operation
        props['arity'] = arity
        global op_registry_functions
        op_registry_functions[f"{name}/{arity}"] = props
        op_registry_functions[f"{name}"] = props
        print_l_cmt(2,f"{name:<{column_width}} | {operation_str}{arityStr}")


import inspect


def is_static_method(obj, attr_name):
    """Check if an attribute is a static method."""
    attribute = getattr(obj, attr_name, None)
    if inspect.isfunction(attribute):
        # If it's a function, check if it's bound or not
        return obj.__dict__.get(attr_name, None) is attribute
    return False

def is_static_variable(obj, attr_name):
    """Heuristics to check if an attribute seems like a static variable."""
    # Example heuristic: exclude capital case names assuming they might be constants
    return attr_name.isupper()


def summarize_data_tree(props, obj, depth=0, max_depth=5, seen_summaries=None):

    if depth > max_depth:
        return "..."

    if seen_summaries is None:
        seen_summaries = set()


    if isinstance(obj, (AtomKind)):
        return obj.name

    if isinstance(obj, (int, float, bool, SymbolAtom, ExpressionAtom, AtomType, AtomKind)):
        summary = repr(obj)
        return summary

    if isinstance(obj, (int, float, str, SymbolAtom, ExpressionAtom, AtomType)):
        summary = repr(obj)
        if summary not in seen_summaries:
            seen_summaries.add(summary)
            return summary
        else:
            return "$" + summary[2:1]



    if isinstance(obj, dict):
        # Expand the list comprehension for dict_summary into a for loop
        dict_summary = []
        #props.update(obj)
        for k, v in obj.items():
            #props[k]=v
            item_summary = summarize_data_tree(props, v, depth + 1, max_depth, seen_summaries)
            dict_summary.append(f"{repr(k)}: {item_summary}")
        summary = f"Dict({len(obj)}){{" + ", ".join(dict_summary) + "}}"
    elif isinstance(obj, (list, tuple, set)):
        # Expand the list comprehension for list_summary into a for loop
        list_summary = []
        for item in obj:
            item_summary = summarize_data_tree(props, item, depth + 1, max_depth, seen_summaries)
            list_summary.append(item_summary)
        summary = f"{type(obj).__name__}({len(obj)})[" + ", ".join(list_summary) + "]"
    elif inspect.isfunction(obj) or inspect.ismethod(obj) or isinstance(obj, type(lambda: None)):
        #param_parts, params_call_parts = get_call_parts(obj)
        #props['params_call_parts'] = params_call_parts
        #props['param_parts'] = param_parts
        from mettalog.ast_source_util import generate_function_signature
        summary = generate_function_signature(obj)
        
    elif is_iterable(obj, exclude=(str, bytes, dict, list, tuple, set)):
        # Expand the list comprehension for iterable_summary into a for loop
        iterable_summary = []
        for item in obj:
            item_summary = summarize_data_tree(props, item, depth + 1, max_depth, seen_summaries)
            iterable_summary.append(item_summary)
        summary = f"Iterable({len(obj)})[{', '.join(iterable_summary)}]"
    else:
        attr_summaries = []
        for attr_name in dir(obj):
            # Similar logic as before, adjusted to handle expanded for loop structure
            if attr_name.startswith("__") or attr_name in ["catom","match_atom", "_from_catom", "execute"] or is_static_method(obj, attr_name) or is_static_variable(obj, attr_name):
                continue
            try:
                attribute = getattr(obj, attr_name)
                summary = handle_attribute(props, attribute, attr_name, depth, max_depth, seen_summaries)
                if summary:
                    attr_summaries.append(summary)
            except Exception as e:
                attr_summaries.append(f"{attr_name}: <Error: {e}>")

        if len(attr_summaries) == 0:
            try:
                newobj = obj.__dict__()
                if newobj is not None:
                    return summarize_data_tree(props, newobj, depth, max_depth, seen_summaries)
            except Exception as e:
                ""

        typeName = type(obj).__name__
        if attr_summaries: typeName = ""
        summary = f"{typeName}{{" + ", ".join(attr_summaries) + "}}" if attr_summaries else repr(obj)

    printable_summary = ("$" + summary[2:1]) if summary in seen_summaries else summary
    seen_summaries.add(summary)

    return printable_summary

def handle_attribute(props, attribute, attr_name, depth, max_depth, seen_summaries):
    """Handles summarizing an individual attribute."""
    if callable(attribute) and attr_name.startswith("get_") and not inspect.signature(attribute).parameters:
        value = attribute()  # Call the getter function
        summary = summarize_data_tree(props, value, depth + 1, max_depth)
        attr_name = attr_name[4:]  # Remove 'get_' prefix for getter functions without parameters
        maybe_merge(props, attr_name, value)
    elif callable(attribute) and not inspect.signature(attribute).parameters:
        return None  # Skip non-getter callable attributes without parameters
    elif isinstance(attribute, property):
        value = attribute.fget(attribute) if attribute.fget else attribute  # Use the getter if available
        maybe_merge(props, attr_name, value)
        summary = summarize_data_tree(props, value, depth + 1, max_depth)
    else:
        value = attribute
        maybe_merge(props, attr_name, value)
        summary = summarize_data_tree(props, value, depth + 1, max_depth)

    if summary not in seen_summaries:
        seen_summaries.add(summary)
        return f"{attr_name}: {summary}"
    else:
        return f"{attr_name}: ${summary[2:1]}"


def maybe_merge(props, attr_name, value):
    """
    Updates the 'props' dictionary based on the type of 'value'.

    If 'value' is a dictionary, it merges its items into 'props'.
    Otherwise, it adds 'value' to 'props' under the key 'attr_name'.

    Args:
        props (dict): The dictionary to update.
        attr_name (str): The key to use if 'value' is not a dictionary.
        value: The value to merge into 'props' or assign to 'props[attr_name]'.
    """
    if isinstance(value, dict):
        # If value is a dictionary, merge its items into props
        props.update(value)
        ""
    elif is_simple_obj(value):
    # If value is not a dictionary, assign it to props under the key attr_name
        props[attr_name] = value  # Corrected to use 'value' instead of 'obj'
    else:
        "" # props[attr_name] = value  # Corrected to use 'value' instead of 'obj'

def is_simple_obj(obj):
    if isinstance(obj, (int, float, bool, SymbolAtom, ExpressionAtom, AtomType, AtomKind)):
        return True
    if (inspect.isfunction(obj) or inspect.ismethod(obj) or isinstance(obj, type(lambda: None))):
        return True
    if isinstance(obj, (str, bytes, dict, list, tuple, set)):
        return True
    return False


def import_module_to_rust(name_or_path_or_module):

    print_l_cmt(2, f"import_module_to_rust: {name_or_path_or_module}")
    try:
        main_module, _ = load_module(name_or_path_or_module) if not isinstance(name_or_path_or_module, types.ModuleType) else (name_or_path_or_module, {})
        load_module_to_rust(main_module)
    except ImportError as e:
        print_l_cmt(1, f"ImportError: {name_or_path_or_module} {e}")
        print_exception_stack(e)
        raise

import os
from contextlib import contextmanager

@contextmanager
def temporary_change_dir(new_dir):
    """Temporarily change the working directory."""
    original_dir = os.getcwd()  # Save the current working directory
    old_runner_cwd = the_python_runner.cwd
    try:
        the_python_runner.cwd = new_dir
        os.chdir(new_dir)  # Change to the new directory
        yield
    finally:
        the_python_runner.cwd = old_runner_cwd
        os.chdir(original_dir)  # Revert to the original directory

def load_module_to_rust(module):
    # Get the directory name from the module's __file__ attribute
    module_file = os.path.abspath(module.__file__)
    module_dir = os.path.dirname(module_file)

    # Check if the module is a package (__init__.py)
    if os.path.basename(module_file) == '__init__.py':
        # If so, move up one directory to get the package or main script directory
        module_dir = os.path.dirname(module_dir)

    print_l_cmt(2, f"load_module_to_rust directory: {module_dir}")  # Just for verification

    # Assuming the short name is the module's __name__ attribute
    short_name = module.__name__.split('.')[-1]  # Get the last part after a dot if it's a sub-module
    print_l_cmt(2, f"load_module_to_rust short name: {short_name}")  # Just for verification

    with temporary_change_dir(module_dir):
        # Place your Rust integration code here. The working directory is temporarily the module's directory.
        # This mock function represents calling Rust's functionality - replace with actual call
        get_metta().run_hyperon(f"!(import! &self {short_name})")
        # After the block, the working directory will revert to what it was.



def load_functions0(name_or_path_or_module, combined_dict):
    print_l_cmt(2, f"Import: {name_or_path_or_module}")
    try:
        main_module, _ = load_module(name_or_path_or_module) if not isinstance(name_or_path_or_module, types.ModuleType) else (name_or_path_or_module, {})
        #the_python_runner.load_py_module(main_module.__name__)        
    except ImportError as e:
        print_l_cmt(1, f"ImportError: {name_or_path_or_module} {e}")
        print_exception_stack(e)
        raise
    before = set(op_registry_atoms.keys())
    combined_dict = process_module(main_module, combined_dict, 1)
    after = set(op_registry_atoms.keys())
    new_operations = after - before
    new_operationsDict = {name: op_registry_atoms[name] for name in new_operations}

    if new_operations:
        print_l_cmt(2, "New operations loaded:")
        #print_operations_table(name_or_path_or_module,new_operationsDict)
    else:
        print_l_cmt(2, "No new operations were loaded.")

    return combined_dict




@export_flags(MeTTa=True, Janus=True)
def load_functions(name_or_path):
    combined_dict = {}
    #combined_dict = load_functions_motto()
    print_l_cmt(2, f"Load functions: {name_or_path}")
    print_l_cmt(2, f"the_python_runner={the_python_runner}")
    combined_dict = load_functions0(name_or_path, combined_dict)
    return combined_dict

did_load_functions_ext = None
def load_functions_ext():
    global did_load_functions_ext
    if did_load_functions_ext is not None: return did_load_functions_ext
    did_load_functions_ext = {}
    print_l_cmt(1, f"Load Ext functions")
    #did_load_functions_ext = load_functions0("hyperon.stdlib", did_load_functions_ext)
    did_load_functions_ext = load_functions0("mettalog", did_load_functions_ext)
    #did_load_functions_ext = load_functions0(f"{__file__}", did_load_functions_ext)
    global op_registry_atoms
    op_registry_atoms.update(did_load_functions_ext)
    return did_load_functions_ext

did_load_functions_motto = None
def load_functions_motto():
    from motto import sparql_gate
    from motto import llm_gate
    from motto.llm_gate import llmgate_atoms_for_ra, postproc_atoms_for_ra
    from motto.sparql_gate import sql_space_atoms_for_ra
    global did_load_functions_motto
    if did_load_functions_motto is not None: return did_load_functions_motto
    did_load_functions_motto = {}
    print_l_cmt(1, "Loading functions for metta-motto")
    the_python_runner = get_metta()
    print_l_cmt(2, f"the_python_runner={the_python_runner}")
    dict1 = sql_space_atoms_for_ra()
    print_l_cmt(2, f"dict1={dict1}")
    dict2 = llmgate_atoms_for_ra(the_python_runner)
    print_l_cmt(2, f"dict2={dict2}")
    dict3 = postproc_atoms_for_ra()
    print_l_cmt(2, f"dict3={dict3}")
    #combined_dict = load_functions0(name_or_path, combined_dict)
    did_load_functions_motto.update(dict3)
    did_load_functions_motto.update(dict2)
    did_load_functions_motto.update(dict1)
    global op_registry_atoms
    op_registry_atoms.update(did_load_functions_motto)
    return did_load_functions_motto

op_registry_atoms = {}
modules_registry = {}
op_registry_functions = {}
op_registry_tokens = {}

@export_flags(MeTTa=True, Janus=True)
def eval_name_args(name, *args):
    """Evaluates an operation by name, encapsulated in a GroundedAtom, with the provided arguments."""
    global op_registry_atoms
    global op_registry_functions
    arity = len(*args) + 1
    if arity >= 0:
        # Attempt to fetch operation properties using name and arity
        grounded_atom = op_registry_atoms.get(f"{name}/{arity}")
        if grounded_atom is not None:
            # Construct and return a definition string based on available properties
            # This assumes 'description' or other relevant fields exist in oper_props
            try:
                result = grounded_atom.execute(*args)
                return result
            except Exception as e:
                if verbose >= 0: print_exception_stack(e)
                raise e
        oper_props = op_registry_functions.get(f"{name}/{arity}")
        if oper_props is not None:
            try:
                result = oper_props.get('op')(*args)
                return result
            except Exception as e:
                if verbose >= 0: print_exception_stack(e)
                raise e

    # Attempt to fetch operation properties using name and arity
    grounded_atom = op_registry_atoms.get(name)
    if grounded_atom is not None:
        # Construct and return a definition string based on available properties
        # This assumes 'description' or other relevant fields exist in oper_props
        try:
            result = grounded_atom.execute(*args)
            return result
        except Exception as e:
            if verbose >= 0: print_exception_stack(e)
            raise e
    oper_props = op_registry_functions.get(name)
    if oper_props is not None:
        try:
            result = oper_props.get('op')(*args)
            return result
        except Exception as e:
            if verbose >= 0: print_exception_stack(e)
            raise e

    raise ValueError(f"Operation {name} not found in the registry.")

@export_flags(MeTTa=True, Janus=True, arity=3)
def get_operation_definition_with_args(name, *args):
    return get_operation_definition_with_arity(name, len(*args) + 1)

@export_flags(MeTTa=True, Janus=True, arity=3)
def get_operation_definition_with_arity(name, arity):
    """Returns the definition of an operation by name and arity."""
    global op_registry_atoms
    global op_registry_functions

    if arity >= 0:
        # Attempt to fetch operation properties using name and arity
        oper_props = op_registry_atoms.get(f"{name}/{arity}")
        if oper_props is not None:
            # Construct and return a definition string based on available properties
            # This assumes 'description' or other relevant fields exist in oper_props
            return oper_props

        # Attempt to fetch operation properties using name and arity
        oper_props = op_registry_functions.get(f"{name}/{arity}")
        if oper_props is not None:
            # Construct and return a definition string based on available properties
            # This assumes 'description' or other relevant fields exist in oper_props
            return oper_props

    # Attempt to fetch operation properties using name and arity
    oper_props = op_registry_atoms.get(f"{name}")
    if oper_props is not None:
        # Construct and return a definition string based on available properties
        # This assumes 'description' or other relevant fields exist in oper_props
        return oper_props

    # Attempt to fetch operation properties using name and arity
    oper_props = op_registry_functions.get(f"{name}")
    if oper_props is not None:
        # Construct and return a definition string based on available properties
        # This assumes 'description' or other relevant fields exist in oper_props
        return oper_props

    return None

# Example usage:
# Assuming `op_registry_atoms` and `op_registry_functions` are defined and populated appropriately
# name = "some_operation_name"
# arity = 2  # Number of arguments the operation expects
# print(get_operation_definition(name, arity))

#vspace_main_from_python()


def rust_metta_run(f):
    #print(f"{the_python_runner}={type(f)}/{repr(f)}"),
    ret = the_python_runner.run_hyperon(f)
    return ret




# All execution happens here
swip = globals().get('swip') or PySwip()
selected_space_name = globals().get('selected_space_name') or "&self"

from mettalog.vspace import *
from mettalog.repl_loop import *



try:
    the_verspace = globals().get('the_verspace') or VSpace("&verspace")
except Exception as e: 
    if verbose>0: print(f"; Error: {e}")
try:
    the_flybase = globals().get('the_flybase') or VSpace("&flybase")
except Exception as e: 
    if verbose>0: print(f"; Error: {e}")
try:
    the_nb_space = globals().get('the_nb_space') or VSpace("&nb")
except Exception as e: 
    if verbose>0: print(f"; Error: {e}")
#try: the_gptspace = globals().get('the_gptspace') or GptSpace()
#except Exception as e: if verbose>0: print(f"; Error: {e}")

argmode = None
sys_argv_length = len(sys.argv)

from mettalog.repl_loop import add_to_history_if_unique

# assume all functions below here are defined
if the_python_runner is None:  #MakeInteractiveMeTTa() #def MakeInteractiveMeTTa(): #global get_metta(),the_old_runner_space,the_new_runner_space,sys_argv_length
    try:
        the_python_runner = MeTTaLog()
        #get_metta() = MeTTaNew()
        the_python_runner.cwd = [os.path.dirname(os.path.dirname(__file__))]
        the_old_runner_space = get_metta().space()
        the_new_runner_space = get_metta().space()
        print_l_cmt(1, f"; The sys.argv list is: {sys.argv}")
        vspace_init()
        load_functions_ext()
        #get_metta().rust_metta_run("!(extend-py! metta_space/mettalog)")
    except Exception as e:
        if verbose >= 0: print_exception_stack(e)


is_init = False

if __name__ != "mettalog":
    vspace_main_from_python()


print_l_cmt(2, f";; ...did {__file__}...{__package__} name={__name__}")
