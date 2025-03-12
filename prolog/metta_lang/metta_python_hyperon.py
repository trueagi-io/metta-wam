
from hyperon import *
from hyperon.base import Atom
from hyperon.atoms import OperationAtom, E, GroundedAtom, GroundedObject
from hyperon.ext import register_tokens
from hyperon.ext import register_atoms
from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa
from hyperon.atoms import *
from hyperon.stdlib import *
#import hyperonpy as hp


import sys
import readline
import os
import atexit

class MeTTaVS(MeTTa):
    def copy(self):
        return self

runner = MeTTaVS()

def get_children(metta_iterable):
    try:
        return iter(metta_iterable)
    except TypeError:
        try:
            return iter([metta_iterable])  # Encapsulate in a list and then iterate
        except TypeError:
            raise ValueError("Provided object cannot be iterated or converted to an iterable.")

# chain python objects with |  (syntactic sugar for langchain)
def py_chain_metta(metta_tuple):
    unwrap1 = rust_deref(metta_tuple)
    objects = [rust_deref(a) for a in get_children(unwrap1)]
    result = objects[0]
    for obj in objects[1:]:
        result = result | obj
    return result

def py_chain(objects):
    result = objects[0]
    for obj in objects[1:]:
        result = result | obj
    return result


def rust_metta_run(obj):
    return runner.run(obj)

def always_unwrap_python_object(rust):
  return hyperon.stdlib.try_unwrap_python_object(rust)


def rust_py_char(ch):
  from hyperon.stdlib import Char
  return Char(ch)

def rust_py_symbol(ch):
  return SymbolAtom(str(ch))

import io
import sys

def rust_symbol_atom(arg):
    captured_output = io.StringIO()  # Create a StringIO object to capture output
    sys.stdout = captured_output    # Redirect standard output to StringIO
    try:
        print(arg, end='')                  # Call print with the argument
    finally:
        sys.stdout = sys.__stdout__  # Restore standard output
    return captured_output.getvalue()  # Get the captured output as a string

import janus
# from janus import *

def rust_unwrap(obj):
    if obj is None:
        return obj

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

    if isinstance(obj, janus.Term):
        return obj

    # Check if obj is a list or a tuple, but not a string
    if isinstance(obj, (list, tuple)) and not isinstance(obj, str):
        return type(obj)(rust_deref(element) for element in obj)

    return always_unwrap_python_object(obj)

def rust_deref(obj):
  while True:
    undone = rust_unwrap(obj)
    if undone is obj: return obj
    if undone is None: return obj
    obj = undone
