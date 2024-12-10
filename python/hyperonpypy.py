
# Add a bit more tracing/debugging in the code below 
# Make sure you give me back the complete and workign versions of the entire content)  this is taking the place of a the old pybind11 C++ file so module level functions and class names and class function  cannot be renamed or ommited.  Continue  and dont worry about space limitations 

"""
===============================================================================
Metta Script Runner and REPL Environment
===============================================================================

Usage Example:
--------------
To run this script:
    `python metta_script.py -v 2 -p /path/to/modules my_module script1.metta --repl`
    - This command sets verbosity to DEBUG, adds search paths and modules, and runs
      the specified Metta script then enters the --repl for further debugging.

Logging Levels:
---------------
- SILENT = 0: No output is shown.
- USER = 1: Basic user-related messages and output.
- DEBUG = 2: Detailed debug-level information for developers.
- TRACE = 3: Tracing output for fine-grained insights into execution.

Environment Variables:
----------------------
- METTALOG_VERBOSE: Set this to one of the verbosity levels to control logging output.
  Example: `export METTALOG_VERBOSE=2` for DEBUG level.

===============================================================================
"""

import os
import sys
#import readline
import atexit
import traceback
import re
import warnings
from enum import Enum
from typing import Any, List, Optional, Dict, Callable, Union, Tuple
from importlib import import_module
import importlib.util
import site
#from hyperon.atoms import Atom, AtomKind, OperationAtom
#from hyperon.base import GroundingSpaceRef, Tokenizer, SExprParser
#from hyperon import *
import hyperon

# Module-level verbosity levels
SILENT = 0  # No output
USER = 1    # Basic output for users
DEBUG = 2   # Detailed debug information
TRACE = 3   # Granular trace-level output

# Default verbosity level
METTALOG_VERBOSE = USER

# Log messages based on verbosity level
def mesg(level, message, m2=None, m3=None):
    if METTALOG_VERBOSE >= level:
        print(f"{message} {m2 or ''} {m3 or ''}")

def set_no_mock_objects_flag(flag: bool):
    global mock_throw_error
    mock_throw_error = flag

def get_no_mock_objects_flag() -> bool:
    return mock_throw_error
# Testing and Examples


# Set verbosity level
def set_verbosity(level):
    global METTALOG_VERBOSE
    level = int(level)
    if level in [SILENT, USER, DEBUG, TRACE]:
        METTALOG_VERBOSE = level
        mesg(DEBUG, f"Verbosity set to level {level}")
    else:        
        print(f"Invalid verbosity level '{level}' provided. Defaulting to USER level.")
        METTALOG_VERBOSE = USER

# Initialize verbosity from environment variable
try:
    set_verbosity(os.getenv("METTALOG_VERBOSE", USER))
except Exception as e:
    mesg(USER, f"An error occurred: {e}")
    if METTALOG_VERBOSE >= DEBUG:
        traceback.print_exc()

# Command-line verbosity handling
try:
    i = 0
    while i < len(sys.argv):
        arg = sys.argv[i]
        i += 1

        if arg in ("-v", "--verbosity"):
            try:
                set_verbosity(sys.argv[i])
                break
            except (ValueError, IndexError):
                print("Invalid verbosity level. Defaulting to USER.")
                set_verbosity(USER)

    mesg(DEBUG, f"Argv={sys.argv}")

except Exception as e:
    mesg(USER, f"An error occurred: {e}")
    if METTALOG_VERBOSE >= DEBUG:
        traceback.print_exc()

# Enums for AtomKind, SerialResult, SyntaxNodeType
class AtomKind(Enum):
    SYMBOL = 1 #"SYMBOL"
    VARIABLE = 2 #"VARIABLE"
    EXPR = 3 # "EXPR"
    GROUNDED = 4 #"GROUNDED"

class SerialResult(Enum):
    OK = 0
    NOT_SUPPORTED = 1

# SyntaxNodeType Enum
class SyntaxNodeType(Enum):
    COMMENT = "COMMENT"
    VARIABLE_TOKEN = "VARIABLE_TOKEN"
    STRING_TOKEN = "STRING_TOKEN"
    WORD_TOKEN = "WORD_TOKEN"
    OPEN_PAREN = "OPEN_PAREN"
    CLOSE_PAREN = "CLOSE_PAREN"
    WHITESPACE = "WHITESPACE"
    LEFTOVER_TEXT = "LEFTOVER_TEXT"
    EXPRESSION_GROUP = "EXPRESSION_GROUP"
    ERROR_GROUP = "ERROR_GROUP"

# Mock CStruct equivalent
class CStruct:
    def __init__(self, obj=None):
        self.obj = obj
    def __init__(self, l2=None, **kwargs):
        self.__dict__.update(kwargs)

    def ptr(self):
        return self.obj


# Placeholder for AtomKind
#class AtomKind:
#    SYMBOL = 'Symbol'
#    VARIABLE = 'Variable'
#    GROUNDED = 'Grounded'
#    EXPRESSION = 'Expression'
#    UNDEFINED = 'Undefined'


# Global variable for tracing messages
def trace_msg(msg: str):
    mesg(DEBUG,f"TRACE: {msg}")

# Global variable to control whether to throw errors in mocks
mock_throw_error = True

# ------------------ Atom Handling ------------------

class CAtom:
    def __init__(
        self,
        name: Optional[str] = None,
        atom_kind: Optional[int] = AtomKind.GROUNDED,
        grounded_object: Optional[Any] = None,
        children: Optional[List['CAtom']] = None
    ):
        self.name = name
        self.atom_kind = atom_kind
        self.grounded_object = grounded_object
        self.children = children or []

    def is_grounded(self) -> bool:
        return self.grounded_object is not None and self.atom_kind == AtomKind.GROUNDED

    def execute(self, args: List['CAtom']) -> List['CAtom']:
        if self.is_grounded():
            return self.grounded_object.execute(args)
        else:
            warnings.warn("Attempted to execute a non-grounded atom.")
            return []

    def match(self, other: 'CAtom') -> 'CBindingsSet':
        return atom_match_atom(self, other)

    def __eq__(self, other) -> bool:
        if not isinstance(other, CAtom):
            return False
        if self.is_grounded() and other.is_grounded():
            return self.grounded_object == other.grounded_object
        return (
            self.name == other.name and
            self.atom_kind == other.atom_kind and
            self.children == other.children
        )

    def __str__(self) -> str:
        if self.atom_kind == AtomKind.EXPR:
            return f"({' '.join(str(child) for child in self.children)})"
        elif self.atom_kind == AtomKind.VARIABLE:
            return f"${self.name}"
        elif self.atom_kind == AtomKind.SYMBOL:
            return f"{self.name}"
        #elif self.is_grounded():
        #    return self.grounded_object.serialize()
        else:
            return f"CAtom({self.name},{type(self.grounded_object)},{self.grounded_object})"

    def __repr__(self) -> str:
        return self.__str__()

    def clone(self) -> 'CAtom':
        return CAtom(
            name=self.name,
            atom_kind=self.atom_kind,
            grounded_object=self.grounded_object.clone() if self.is_grounded() else None,
            children=[child.clone() for child in self.children]
        )

    def is_null(self) -> bool:
        return self.name is None and not self.children and not self.grounded_object

    def get_children(self) -> List['CAtom']:
        if self.atom_kind == AtomKind.EXPR:
            return self.children
        else:
            warnings.warn("Attempted to get children of a non-expression atom.")
            return []

    def get_symbol(self) -> str:
        if self.atom_kind == AtomKind.SYMBOL:
            return self.name
        else:
            warnings.warn("Attempted to get symbol of a non-symbol atom.")
            return ""

    def get_name(self) -> str:
        if self.atom_kind == AtomKind.VARIABLE:
            return self.name
        else:
            warnings.warn("Attempted to get name of a non-variable atom.")
            return ""

    def to_pyobj(self) -> Any:
        if self.is_grounded():
            return self.grounded_object.pyobj
        elif self.atom_kind == AtomKind.EXPR:
            return [child.to_pyobj() for child in self.children]
        elif self.atom_kind == AtomKind.SYMBOL:
            return self.name
        elif self.atom_kind == AtomKind.VARIABLE:
            return f"${self.name}"
        else:
            warnings.warn("Attempted to convert an unknown atom type to Python object.")
            return None

    def get_type(self) -> 'CAtom':
        if self.is_grounded():
            return self.grounded_object.typ
        return self.atom_kind

    def serialize(self, serializer: 'Serializer') -> None:
        if self.is_grounded():
            self.grounded_object.serialize_with(serializer)
        elif self.atom_kind == AtomKind.SYMBOL:
            serializer.serialize_str(self.name)
        elif self.atom_kind == AtomKind.VARIABLE:
            serializer.serialize_str(f"${self.name}")
        elif self.atom_kind == AtomKind.EXPR:
            serializer.serialize_str("(")
            for child in self.children:
                child.serialize(serializer)
            serializer.serialize_str(")")
        else:
            warnings.warn("Unknown atom type for serialization.")

    @staticmethod
    def from_pyobj(pyobj: Any) -> 'CAtom':
        if isinstance(pyobj, CAtom):
            return pyobj.clone()
        elif isinstance(pyobj, list):
            children = [CAtom.from_pyobj(child) for child in pyobj]
            return CAtom.atom_expr(children)
        elif isinstance(pyobj, str):
            if pyobj.startswith('$'):
                return CAtom.atom_var(pyobj[1:])
            else:
                return CAtom.atom_sym(pyobj)
        else:
            return CAtom.atom_gnd(pyobj, CAtom.atom_sym(AtomKind.UNDEFINED))

    @staticmethod
    def atom_sym(name: str) -> 'CAtom':
        return CAtom(name=name, atom_kind=AtomKind.SYMBOL)

    @staticmethod
    def atom_var(name: str) -> 'CAtom':
        return CAtom(name=name, atom_kind=AtomKind.VARIABLE)

    @staticmethod
    def atom_gnd(pyobj: Any, typ: 'CAtom' = None) -> 'CAtom':
        if typ is None:
            typ = CAtomType.UNDEFINED
        grounded_object = CGroundedObject(typ, pyobj)
        return CAtom(
            name=None,
            atom_kind=AtomKind.GROUNDED,
            grounded_object=grounded_object
        )

    @staticmethod
    def atom_expr(children: List['CAtom']) -> 'CAtom':
        return CAtom(
            name=None,
            atom_kind=AtomKind.EXPR,
            children=children
        )

    def __iter__(self):
        if self.atom_kind == AtomKind.EXPR:
            return iter(self.children)
        else:
            return iter([])

# Atom Types as Atoms
class CAtomType:
    SYMBOL = CAtom.atom_sym("Symbol")
    VARIABLE = CAtom.atom_sym("Variable")
    EXPRESSION = CAtom.atom_sym("Expression")
    GROUNDED = CAtom.atom_sym("Grounded")
    UNDEFINED = CAtom.atom_sym("%Undefined%")    
    TYPE = CAtom.atom_sym("Type")
    ATOM = CAtom.atom_sym("Atom")
    GROUNDED_SPACE = CAtom.atom_sym("Space")
    UNIT = CAtom.atom_sym("Unit")

# CAtoms Class
class CAtoms:
    EMPTY = CAtom.atom_sym("Empty")
    UNIT = CAtom.atom_sym("Unit")
    METTA = CAtom.atom_sym("MeTTa")

# Module-level functions for CAtom
def atom_sym(name: str) -> CAtom:
    return CAtom.atom_sym(name)

def atom_var(name: str) -> CAtom:
    return CAtom.atom_var(name)

def atom_var_parse_name(name: str) -> CAtom:
    return CAtom.atom_var(name)

def atom_gnd(pyobj: Any, typ: Optional[CAtom] = None) -> CAtom:
    return CAtom.atom_gnd(pyobj, typ)

def atom_expr(children: List[CAtom]) -> CAtom:
    return CAtom.atom_expr(children)

def atom_clone(atom: CAtom) -> CAtom:
    return atom.clone()

def atom_is_null(atom: CAtom) -> bool:
    return atom.is_null()

def atom_get_children(atom: CAtom) -> List[CAtom]:
    return atom.get_children()

def atom_get_symbol(atom: CAtom) -> str:
    return atom.get_symbol()

def atom_get_name(atom: CAtom) -> str:
    return atom.get_name()

def atom_is_grounded(atom: CAtom) -> bool:
    return atom.is_grounded()

def atom_get_type(atom: CAtom) -> CAtom:
    return atom.get_type()

def atom_to_pyobj(atom: CAtom) -> Any:
    return atom.to_pyobj()

def atom_to_str(atom: CAtom) -> str:
    return str(atom)

def atom_eq(a: CAtom, b: CAtom) -> bool:
    return a == b

def atom_is_error(atom: CAtom) -> bool:
    if atom.atom_kind == AtomKind.EXPR:
        if len(atom.children) >= 2:
            first_child = atom.children[0]
            if first_child.atom_kind == AtomKind.SYMBOL and first_child.name == "Error":
                return True
    return False

def atom_error_message(atom: CAtom) -> str:
    if atom_is_error(atom):
        return str(atom.children[1])
    else:
        return ""

def atom_free(atom: CAtom):
    mesg(TRACE,f"Freeing atom: {atom}")

def atom_get_metatype(atom: CAtom) -> AtomKind:
    if not isinstance(atom, CAtom):
        raise TypeError(f"Expected CAtom type but got {type(atom)}")
    if atom.atom_kind == AtomKind.SYMBOL:
        return AtomKind.SYMBOL
    elif atom.atom_kind == AtomKind.VARIABLE:
        return AtomKind.VARIABLE
    elif atom.atom_kind == AtomKind.EXPR:
        return AtomKind.EXPR
    elif atom.is_grounded():
        return AtomKind.GROUNDED
    else:
        return None

def atom_get_space(atom: CAtom) -> 'CSpace':
    return None  # Placeholder; actual implementation depends on grounded spaces

def atom_get_object(atom: CAtom) -> Any:
    if atom.is_grounded():
        return atom.grounded_object.pyobj
    else:
        return None

def atom_is_cgrounded(atom: CAtom) -> bool:
    return atom.is_grounded()

def atom_get_grounded_type(atom: CAtom) -> CAtom:
    if atom.is_grounded():
        return atom.grounded_object.typ
    else:
        return None

def atom_iterate(atom: CAtom) -> List[CAtom]:
    atoms_list = []

    def _iterate(current_atom: CAtom):
        atoms_list.append(current_atom)
        for child in current_atom.get_children():
            _iterate(child)

    _iterate(atom)
    return atoms_list

def atom_match_atom(a: CAtom, b: CAtom) -> 'CBindingsSet':
    return atom_match(a, b)

def atoms_are_equivalent(first: CAtom, second: CAtom) -> bool:
    return first == second

# ------------------ Grounded Objects ------------------

class CGroundedObject:
    def __init__(self, typ: CAtom, pyobj: Any):
        self.typ = typ
        self.pyobj = pyobj

    def execute(self, args: List[CAtom]) -> List[CAtom]:
        if hasattr(self.pyobj, 'execute'):
            try:
                py_args = [arg.to_pyobj() for arg in args]
                result = self.pyobj.execute(*py_args)
                if isinstance(result, list):
                    return [CAtom.from_pyobj(res) for res in result]
                else:
                    return [CAtom.from_pyobj(result)]
            except Exception as e:
                if mock_throw_error:
                    raise e
                else:
                    warnings.warn(f"Execution error: {e}")
                    return []
        else:
            warnings.warn("Grounded object does not implement 'execute' method.")
            return []

    def match_(self, other: CAtom) -> 'CBindingsSet':
        if hasattr(self.pyobj, 'match_'):
            try:
                result = self.pyobj.match_(other.to_pyobj())
                if result:
                    bindings_set = CBindingsSet()
                    for binding_dict in result:
                        bindings = CBindings()
                        for var_name, value in binding_dict.items():
                            var_atom = CAtom.atom_var(var_name)
                            value_atom = CAtom.from_pyobj(value)
                            bindings.add_var_binding(var_atom, value_atom)
                        bindings_set.push(bindings)
                    return bindings_set
                else:
                    return CBindingsSet()
            except Exception as e:
                if mock_throw_error:
                    raise e
                else:
                    warnings.warn(f"Matching error: {e}")
                    return CBindingsSet()
        else:
            if self.pyobj == other.grounded_object.pyobj:
                return CBindingsSet([CBindings()])
            else:
                return CBindingsSet()

    def serialize(self) -> str:
        if hasattr(self.pyobj, 'serialize'):
            try:
                return self.pyobj.serialize()
            except Exception as e:
                if mock_throw_error:
                    raise e
                else:
                    warnings.warn(f"Serialization error: {e}")
                    return str(self.pyobj)
        else:
            return str(self.pyobj)

    def serialize_with(self, serializer: 'Serializer') -> None:
        if hasattr(self.pyobj, 'serialize_with'):
            self.pyobj.serialize_with(serializer)
        else:
            serializer.serialize_str(str(self.pyobj))

    def __eq__(self, other):
        if isinstance(other, CGroundedObject):
            return self.pyobj == other.pyobj
        return False
    
    def __str__(self):
        unwrapped_value = self.pyobj
        unwrapped_value = unwrap_atom(unwrapped_value)
        return f"CGroundedObject({self.typ}, {repr(unwrapped_value)}, {type(unwrapped_value)})"


    def clone(self) -> 'CGroundedObject':
        return CGroundedObject(self.typ, self.pyobj)


def unwrap_atom(unwrapped_value):
        # Try to call serialize() and ignore exceptions
        try:
            if hasattr(unwrapped_value, 'serialize') and callable(unwrapped_value.serialize):
                unwrapped_value = unwrapped_value.serialize()
        except Exception as e:
            # Ignore exceptions during serialization
            pass

        # Check if the object has a get_object() method and call it if present
        if hasattr(unwrapped_value, 'get_object') and callable(unwrapped_value.get_object):
            unwrapped_value = unwrapped_value.get_object()

        # Check if the object has a value() method and call it if present
        if hasattr(unwrapped_value, 'value') and callable(unwrapped_value.value):
            unwrapped_value = unwrapped_value.value()

        # Check for a .content attribute
        if hasattr(unwrapped_value, 'content'):
            unwrapped_value = unwrapped_value.content
        
        # Check for a .value attribute (not method)
        if hasattr(unwrapped_value, 'value'):
            unwrapped_value = unwrapped_value.value                    


        # Try to call serialize() and ignore exceptions
        try:
            if hasattr(unwrapped_value, 'serialize') and callable(unwrapped_value.serialize):
                unwrapped_value = unwrapped_value.serialize()
        except Exception as e:
            # Ignore exceptions during serialization
            pass

        return unwrapped_value

def gnd_obj_new(pyobj: Any, typ: CAtom) -> CGroundedObject:
    return CGroundedObject(typ, pyobj)

def gnd_obj_clone(gnd_obj: CGroundedObject) -> CGroundedObject:
    return gnd_obj.clone()

def gnd_obj_free(gnd_obj: CGroundedObject):
    trace_msg(f"Freeing grounded object: {gnd_obj}")

def gnd_obj_eq(gnd_obj1: CGroundedObject, gnd_obj2: CGroundedObject) -> bool:
    return gnd_obj1 == gnd_obj2

def gnd_obj_to_str(gnd_obj: CGroundedObject) -> str:
    return str(gnd_obj)

def atom_gnd_serialize(atom: CAtom, serializer: 'Serializer') -> SerialResult:
    if atom.is_grounded():
        atom.serialize(serializer)
        return SerialResult.OK
    else:
        return SerialResult.NOT_SUPPORTED

# ------------------ Grounding Space (CSpace) ------------------

class CSpace:
    def __init__(self):
        self.atoms: List[CAtom] = []

    def add(self, atom: CAtom):
        self.atoms.append(atom)

    def remove(self, atom: CAtom) -> bool:
        if atom in self.atoms:
            self.atoms.remove(atom)
            return True
        else:
            warnings.warn(f"Atom {atom} not found in the space.")
            return False

    def replace(self, old_atom: CAtom, new_atom: CAtom) -> bool:
        for i, atom in enumerate(self.atoms):
            if atom == old_atom:
                self.atoms[i] = new_atom
                return True
        warnings.warn(f"Atom {old_atom} not found for replacement.")
        return False

    def atom_count(self) -> int:
        return len(self.atoms)

    def query(self, pattern: CAtom) -> 'CBindingsSet':
        result_set = CBindingsSet()
        for atom in self.atoms:
            bindings = atom_match_atom_single(atom, pattern)
            if bindings is not None:
                result_set.push(bindings)
        return result_set

    def subst(self, pattern: CAtom, templ: CAtom) -> List[CAtom]:
        results = []
        bindings_set = self.query(pattern)
        for bindings in bindings_set.bindings_list:
            substituted_atom = substitute(templ, bindings)
            results.append(substituted_atom)
        return results

    def __iter__(self):
        return iter(self.atoms)

    def __eq__(self, other):
        if isinstance(other, CSpace):
            return self.atoms == other.atoms
        return False

    def __str__(self):
        return f"CSpace({id(self)},{', '.join(str(atom) for atom in self.atoms)})"

def space_new_grounding() -> CSpace:
    apply_monkey_patches()
    #if METTALOG_VERBOSE >= DEBUG: traceback.print_stack() 
    space = CSpace()
    trace_msg(f"space_new_grounding: {space}")
    return space

def space_new_custom(py_space_obj: Any) -> CSpace:
    # Placeholder for custom space; returns standard CSpace
    apply_monkey_patches()
    #if METTALOG_VERBOSE >= DEBUG: traceback.print_stack() 
    space = CSpace()
    trace_msg(f"space_new_custom: {space}")
    return space

def space_free(space: CSpace):
    trace_msg(f"Freeing space: {space}")

def space_get_payload(space: CSpace) -> Any:
    return None  # Placeholder

def space_add(space: CSpace, atom: CAtom):
    space.add(atom)

def space_remove(space: CSpace, atom: CAtom) -> bool:
    return space.remove(atom)

def space_replace(space: CSpace, old_atom: CAtom, new_atom: CAtom) -> bool:
    return space.replace(old_atom, new_atom)

def space_eq(space1: CSpace, space2: CSpace) -> bool:
    return space1 == space2

def space_atom_count(space: CSpace) -> int:
    return space.atom_count()

def space_list(space: CSpace) -> List[CAtom]:
    return space.atoms

def space_query(space: CSpace, pattern: CAtom) -> 'CBindingsSet':
    return space.query(pattern)

def space_subst(space: CSpace, pattern: CAtom, templ: CAtom) -> List[CAtom]:
    return space.subst(pattern, templ)

def space_iterate(space: CSpace) -> Optional[List[CAtom]]:
    return space.atoms


# Functions that may throw errors if mock_throw_error is True
def load_ascii(name: str, space: CSpace):
    try:
        with open(name, 'r') as f:
            content = f.read()
            parser = CSExprParser(content)
            while True:
                atom = parser.parse()
                if atom is None:
                    break
                space.add(atom)
        return True
    except FileNotFoundError:
        if mock_throw_error:
            raise RuntimeError(f"load_ascii: file '{name}' not found")
        else:
            warnings.warn(f"load_ascii: file '{name}' not found")
            return False

# Continue with the rest of your module as before...

# The rest of the module remains unchanged, including the definitions of classes like CTokenizer, CSExprParser, CBindings, CBindingsSet, CInterpreter, CMetta, EnvBuilder, Serializer, etc.

# Due to space limitations, please ensure that you include the rest of the classes and functions from your previous code into this module.

# ------------------ Tokenizer Class ------------------

class CTokenizer:
    def __init__(self):
        self.tokens = []

    def register_token(self, regex: str, callback: Callable):
        self.tokens.append((re.compile(regex), callback))

    def tokenize(self, text: str) -> List[str]:
        results = []
        position = 0
        while position < len(text):
            match_found = False
            for regex, callback in self.tokens:
                match = regex.match(text, position)
                if match:
                    token = callback(match.group())
                    results.append(token)
                    position = match.end()
                    match_found = True
                    break
            if not match_found:
                position += 1  # Skip unrecognized characters
        return results

def tokenizer_new() -> CTokenizer:
    return CTokenizer()

def tokenizer_free(tokenizer: CTokenizer):
    trace_msg(f"Freeing tokenizer: {tokenizer}")

def tokenizer_clone(tokenizer: CTokenizer) -> CTokenizer:
    cloned_tokenizer = CTokenizer()
    cloned_tokenizer.tokens = tokenizer.tokens.copy()
    return cloned_tokenizer

def tokenizer_register_token(tokenizer: CTokenizer, regex: str, constr: Callable):
    tokenizer.register_token(regex, constr)

# ------------------ Syntax Node Functions ------------------

class CSyntaxNode:
    def __init__(self, node=None):
        self.node = node

    def is_null(self) -> bool:
        return self.node is None

    def is_leaf(self) -> bool:
        return not hasattr(self.node, 'children') or not self.node.children

def syntax_node_free(cnode: CSyntaxNode):
    trace_msg(f"Freeing syntax node: {cnode}")

def syntax_node_clone(cnode: CSyntaxNode) -> CSyntaxNode:
    return CSyntaxNode(node=cnode.node)

def syntax_node_type(cnode: CSyntaxNode) -> SyntaxNodeType:
    return SyntaxNodeType.WORD_TOKEN  # Simplified placeholder

def syntax_node_is_null(cnode: CSyntaxNode) -> bool:
    return cnode.is_null()

def syntax_node_is_leaf(cnode: CSyntaxNode) -> bool:
    return cnode.is_leaf()

def syntax_node_src_range(cnode: CSyntaxNode) -> range:
    return range(0, 0)  # Placeholder

def syntax_node_unroll(cnode: CSyntaxNode) -> List[CSyntaxNode]:
    return [CSyntaxNode(node=leaf) for leaf in cnode.node.unroll()] if cnode.node else []

# ------------------ S-expression Parser ------------------

class CSExprParser:
    def __init__(self, text: str):
        self.text = text
        self.tokens = self.tokenize(text)
        self.position = 0
        self.cparser = self 
        self.error_message = None

    def tokenize(self, text: str) -> List[str]:
        return text.replace('(', ' ( ').replace(')', ' ) ').split()

    def parse(self, tokenizer: Optional[CTokenizer] = None) -> Optional[CAtom]:
        if self.position >= len(self.tokens):
            return None
        token = self.tokens[self.position]
        self.position += 1
        if token == '(':
            children = []
            while self.position < len(self.tokens) and self.tokens[self.position] != ')':
                child = self.parse()
                if child is not None:
                    children.append(child)
                else:
                    if self.error_message:
                        warnings.warn(self.error_message)
                        return None
            if self.position < len(self.tokens):
                self.position += 1  # Skip ')'
            else:
                self.error_message = "Unmatched '(' in expression."
                warnings.warn(self.error_message)
                return None
            return CAtom.atom_expr(children)
        elif token == ')':
            self.error_message = "Unexpected ')' in expression."
            warnings.warn(self.error_message)
            return None
        elif token.startswith('$'):
            return CAtom.atom_var(token[1:])
        else:
            return CAtom.atom_sym(token)

    def err_str(self) -> Optional[str]:
        return self.error_message

    def parse_to_syntax_tree(self) -> CSyntaxNode:
        return CSyntaxNode(node=self.text)  # Placeholder

    def sexpr_parser_err_str(self):
        """Returns the error string from the parser or None if no error."""
        err_str = self.error_message
        if err_str:
        	return err_str
        if self.cparser is not None and self.cparser is not self:
        	err_str = self.cparser.sexpr_parser_err_str()
        if err_str is None:
            mesg(DEBUG,"No error in SExprParser")
            return None
        else:
            print(f"SExprParser error: {err_str}")
            return err_str



def sexpr_parse_new(text: str) -> CSExprParser:
    return CSExprParser(text)

def sexpr_parse_run(parser: CSExprParser) -> Optional[CAtom]:
    return parser.parse()

def sexpr_parse_err_str(parser: CSExprParser) -> Optional[str]:
    return parser.err_str()

def sexpr_parse_free(parser: CSExprParser):
    trace_msg(f"Freeing S-expression parser: {parser}")

# ------------------ MeTTa CInterpreter ------------------

class CMetta:
    def __init__(self, space: CSpace):
        self.space = space
        self.error_message = None

    def run(self, parser: CSExprParser) -> List[List[CAtom]]:
        results = []
        while True:
            expr = parser.parse()
            if expr is None:
                if parser.err_str():
                    self.error_message = parser.err_str()
                break
            self.space.add(expr)
            results.append([expr])
        if self.error_message:
            warnings.warn(f"Parser error: {self.error_message}")
        return results

    def evaluate_atom(self, atom: CAtom) -> List[CAtom]:
        interpreter = CInterpreter(self.space, atom)
        while interpreter.has_next():
            interpreter.next()
        return interpreter.get_result()

    def err_str(self) -> Optional[str]:
        return self.error_message

def metta_new(space: CSpace, env_builder: Any = None) -> CMetta:
    trace_msg(f"metta_new: {space} {env_builder}")
    return CMetta(space)

def metta_free(cmetta: CMetta):
    trace_msg(f"Freeing MeTTa interpreter: {cmetta}")

def metta_err_str(cmetta: CMetta) -> Optional[str]:
    return cmetta.err_str()

def metta_eq(a: CMetta, b: CMetta) -> bool:
    return a.space == b.space

def metta_space(cmetta: CMetta) -> CSpace:
    return cmetta.space

def metta_tokenizer(cmetta: CMetta) -> CTokenizer:
    return CTokenizer()  # Placeholder

def metta_working_dir(cmetta: CMetta) -> str:
    return os.getcwd()

def metta_load_module_direct(cmetta: CMetta, mod_name: str, py_loader_func: Callable) -> 'ModuleId':
    py_loader_func()
    return ModuleId(id_value=mod_name)

def metta_load_module_at_path(cmetta: CMetta, path: str, mod_name: Optional[str] = None) -> 'ModuleId':
    return ModuleId(id_value=mod_name or path)

def metta_run(cmetta: CMetta, parser: CSExprParser) -> List[List[CAtom]]:
    return cmetta.run(parser)

def metta_evaluate_atom(cmetta: CMetta, catom: CAtom) -> List[CAtom]:
    return cmetta.evaluate_atom(catom)



# CRunContext Mock Class
#class CRunContext:
#    """Mock class for run context."""
#    def get_space(self):
#        return CSpace()


# CModuleDescriptor Mock
class CModuleDescriptor:
    """Describes a module with metadata."""
    def __init__(self, name, description):
        self.name = name
        self.description = description

    def __str__(self):
        return f"Module: {self.name}, Description: {self.description}"


# ModuleId Mock Class
class ModuleId(CStruct):
    def __init__(self, mod_id: int):
        super().__init__(mod_id)

    def is_valid(self):
        return self.obj is not None


# ------------------ RunnerState Class ------------------

class CRunnerState:
    def __init__(self, metta: CMetta, parser: CSExprParser):
        self.metta = metta
        self.parser = parser
        self.complete = False

    def step(self):
        if self.complete:
            warnings.warn("RunnerState is already complete.")
            return
        result = self.metta.run(self.parser)
        if not result:
            self.complete = True

    def is_complete(self) -> bool:
        return self.complete

    def current_results(self) -> List[List[CAtom]]:
        return [self.metta.space.atoms]

    def __str__(self):
        return f"RunnerState(complete={self.complete}, current_results={self.current_results()})"

def runner_state_new_with_parser(cmetta: CMetta, cparser: CSExprParser) -> CRunnerState:
    return CRunnerState(cmetta, cparser)

def runner_state_new_with_atoms(cmetta: CMetta, atoms: 'CVecAtom') -> CRunnerState:
    parser = CSExprParser("")  # Empty parser as placeholder
    return CRunnerState(cmetta, parser)

def runner_state_step(cstate: CRunnerState):
    cstate.step()

def runner_state_free(runner_state: CRunnerState):
    trace_msg(f"Freeing RunnerState: {runner_state}")

def runner_state_err_str(state: CRunnerState) -> Optional[str]:
    return state.metta.err_str()

def runner_state_is_complete(state: CRunnerState) -> bool:
    return state.is_complete()

def runner_state_current_results(state: CRunnerState) -> List[List[CAtom]]:
    return state.current_results()

# ------------------ Environment Builder ------------------

class EnvBuilder:
    def __init__(self):
        self.working_dir = ""
        self.config_dir = ""
        self.include_paths = []
        self.is_test_env = False

    def set_working_dir(self, path: str):
        self.working_dir = path

    def set_config_dir(self, path: str):
        self.config_dir = path

    def use_test_env(self):
        """Set the environment to be used for testing."""
        self.is_test_env = True

    def create_config_dir(self, should_create: bool):
        if should_create:
            os.makedirs(self.config_dir, exist_ok=True)

    def disable_config_dir(self):
        self.config_dir = ""

    def set_is_test(self, is_test: bool):
        self.is_test_env = is_test

    def push_include_path(self, path: str):
        self.include_paths.append(path)

    def push_fs_module_format(self, interface: Any, fmt_id: int):
        pass  # Placeholder

    def build(self) -> Dict[str, Any]:
        """Finalize the environment setup and return the environment context."""
        return {
            'working_dir': self.working_dir,
            'config_dir': self.config_dir,
            'include_paths': self.include_paths,
            'is_test_env': self.is_test_env
        }
        

def environment_config_dir() -> str:
    return os.getcwd()

def env_builder_start() -> EnvBuilder:
    return EnvBuilder()

def env_builder_use_default() -> EnvBuilder:
    return EnvBuilder()

def env_builder_use_test_env() -> EnvBuilder:
    builder = EnvBuilder()
    builder.set_is_test(True)
    return builder

def env_builder_init_common_env(builder: EnvBuilder):
    pass  # Placeholder

def env_builder_set_working_dir(builder: EnvBuilder, path: str):
    builder.set_working_dir(path)

def env_builder_set_config_dir(builder: EnvBuilder, path: str):
    builder.set_config_dir(path)

def env_builder_create_config_dir(builder: EnvBuilder, should_create: bool):
    builder.create_config_dir(should_create)

def env_builder_disable_config_dir(builder: EnvBuilder):
    builder.disable_config_dir()

def env_builder_set_is_test(builder: EnvBuilder, is_test: bool):
    builder.set_is_test(is_test)

def env_builder_push_include_path(builder: EnvBuilder, path: str):
    builder.push_include_path(path)

def env_builder_push_fs_module_format(builder: EnvBuilder, interface: Any, fmt_id: int):
    builder.push_fs_module_format(interface, fmt_id)

def env_builder_free(env_builder: EnvBuilder):
    trace_msg(f"Freeing EnvBuilder: {env_builder}")

# ------------------ Serializer Classes ------------------


class Serializer(CStruct):
    def __init__(self):
        self.serialized_data = ""

    def serialize_bool(self, value: bool) -> None:
        self.serialized_data += f"Boolean({value})"

    def serialize_int(self, value: int) -> None:
        self.serialized_data += f"Integer({value})"

    def serialize_float(self, value: float) -> None:
        self.serialized_data += f"Float({value})"

    def serialize_str(self, value: str) -> None:
        self.serialized_data += f"String({value})"

    def get_serialized_data(self) -> str:
        return self.serialized_data


class PySerializer(Serializer): pass

# Serializer that bridges between Python and C, but in Python it's simplified
class PythonToCSerializer(Serializer):
     def __init__(self, api, context):
         super().__init__()
         self.api = api
         self.context = context

     def serialize_bool(self, value: bool):
         return self.api.serialize_bool(self.context, value)

     def serialize_int(self, value: int):
         return self.api.serialize_longlong(self.context, value)

     def serialize_float(self, value: float):
         return self.api.serialize_double(self.context, value)

 # C-to-Python serializer, using Python method references instead of pointers
class CToPythonSerializer:
     def __init__(self, serializer: Serializer):
         self.serializer = serializer

     @staticmethod
     def serialize_bool(serializer, value: bool):
         return serializer.serialize_bool(value)

     @staticmethod
     def serialize_longlong(serializer, value: int):
         return serializer.serialize_int(value)

     @staticmethod
     def serialize_double(serializer, value: float):
         return serializer.serialize_float(value)


# Equivalent to the py_serialize function
def py_serialize(gnd, api, context):
    # This is just an example of how we would call Python logic in place of the original pybind11 import
    hyperon = __import__('hyperon.atoms')  # In Python, we just import the required module
    _priv_call_serialize_on_grounded_atom = getattr(hyperon, '_priv_call_serialize_on_grounded_atom')

    # Assuming `gnd` is an instance of `GroundedObject`
    pyobj = gnd.pyobj  # Direct attribute access for simplicity
    py_serializer = PythonToCSerializer(api, context)
    
    result = _priv_call_serialize_on_grounded_atom(pyobj, py_serializer)
    return result

 
def serializer_new() -> Serializer:
    return PythonToCSerializer()

def serializer_free(serializer: Serializer):
    trace_msg(f"Freeing serializer: {serializer}")

def serializer_get_serialized_data(serializer: PythonToCSerializer) -> str:
    return serializer.get_serialized_data()


# ------------------ Module Handling ------------------

class ModuleId:
    def __init__(self, id_value):
        self.id_value = id_value

    def is_valid(self) -> bool:
        return bool(self.id_value)

    def __str__(self):
        return f"ModuleId({self.id_value})"

def module_id_is_valid(module_id: ModuleId) -> bool:
    return module_id.is_valid()

def module_id_to_str(module_id: ModuleId) -> str:
    return str(module_id)

def module_id_free(module_id: ModuleId):
    trace_msg(f"Freeing ModuleId: {module_id}")

# ------------------ Vector of Atoms -----------------------

class CVecAtom:
    def __init__(self, atoms: Optional[List[CAtom]] = None):
        self.atoms = atoms or []

    def push(self, atom: CAtom):
        self.atoms.append(atom)

    def pop(self) -> CAtom:
        if self.atoms:
            return self.atoms.pop()
        else:
            warnings.warn("Attempted to pop from an empty CVecAtom.")
            return CAtom()

    def __iter__(self):
        return iter(self.atoms)

    def __len__(self):
        return len(self.atoms)

def atom_vec_new() -> CVecAtom:
    return CVecAtom()

def atom_vec_from_list(pylist: List[CAtom]) -> CVecAtom:
    return CVecAtom(atoms=pylist.copy())

def atom_vec_free(atom_vec: CVecAtom):
    trace_msg(f"Freeing atom vector: {atom_vec}")

def atom_vec_len(atom_vec: CVecAtom) -> int:
    return len(atom_vec)

def atom_vec_push(atom_vec: CVecAtom, atom: CAtom):
    atom_vec.push(atom)

def atom_vec_pop(atom_vec: CVecAtom) -> CAtom:
    return atom_vec.pop()

# ------------------ CInterpreter Classes ------------------

class CStepResult:
    def __init__(self, result_atoms: List[CAtom], has_more_steps: bool = False):
        self.result_atoms = result_atoms
        self.has_more_steps = has_more_steps

    def get_result(self) -> List[CAtom]:
        return self.result_atoms

    def has_next(self) -> bool:
        return self.has_more_steps

    def __str__(self):
        return f"StepResult(result_atoms={self.result_atoms}, has_more_steps={self.has_more_steps})"

    def clone(self) -> 'CStepResult':
        return CStepResult([atom.clone() for atom in self.result_atoms], self.has_more_steps)

def interpret_init(space: CSpace, expr: CAtom) -> CStepResult:
    return CStepResult([expr], has_more_steps=True)

def interpret_step(step_result: CStepResult) -> CStepResult:
    if step_result.has_next():
        # Simplified interpretation step
        return CStepResult(step_result.result_atoms, has_more_steps=False)
    else:
        warnings.warn("No more steps available in interpreter.")
        return step_result

def step_has_next(step_result: CStepResult) -> bool:
    return step_result.has_next()

def step_get_result(step_result: CStepResult) -> List[CAtom]:
    return step_result.get_result()

def step_result_free(step_result: CStepResult):
    trace_msg(f"Freeing step result: {step_result}")

# ------------------ Atom Matching Function ------------------

def atoms_are_equivalent(first: CAtom, second: CAtom) -> bool:
    return first == second

# ------------------ Utility Functions ------------------

def check_type(space: CSpace, atom: CAtom, typ: CAtom) -> bool:
    return atom.get_type() == typ

def validate_atom(space: CSpace, atom: CAtom) -> bool:
    return True  # Simplified; actual implementation depends on validation rules

def get_atom_types(space: CSpace, atom: CAtom) -> List[CAtom]:
    return [atom.get_type()]

def func_to_string_no_arg(func: Callable) -> str:
    return func()

def func_to_string(func: Callable, *args) -> str:
    return func(*args)


# ------------------ Bindings and BindingsSet ------------------

class CBindings:
    def __init__(self):
        self.bindings: Dict[str, CAtom] = {}

    def add_var_binding(self, var: CAtom, atom: CAtom):
        var_name = var.get_name()
        if var_name in self.bindings:
            if self.bindings[var_name] != atom:
                warnings.warn(f"Conflict in bindings for variable '{var_name}'.")
        self.bindings[var_name] = atom

    def resolve(self, var: CAtom) -> Optional[CAtom]:
        return self.bindings.get(var.get_name())

    def is_empty(self) -> bool:
        return not bool(self.bindings)

    def clone(self) -> 'CBindings':
        new_bindings = CBindings()
        new_bindings.bindings = self.bindings.copy()
        return new_bindings

    def merge(self, other_bindings: 'CBindings') -> 'CBindingsSet':
        new_bindings = self.clone()
        for var_name, atom in other_bindings.bindings.items():
            if var_name in new_bindings.bindings and new_bindings.bindings[var_name] != atom:
                return CBindingsSet()  # Conflict; return empty set
            new_bindings.bindings[var_name] = atom
        return CBindingsSet([new_bindings])

    def __eq__(self, other):
        if isinstance(other, CBindings):
            return self.bindings == other.bindings
        return False

    def __str__(self):
        return str(self.bindings)

def bindings_new() -> CBindings:
    return CBindings()

def bindings_free(bindings: CBindings):
    trace_msg(f"Freeing bindings: {bindings}")

def bindings_clone(bindings: CBindings) -> CBindings:
    return bindings.clone()

def bindings_merge(self_bindings: CBindings, other_bindings: CBindings) -> 'CBindingsSet':
    return self_bindings.merge(other_bindings)

def bindings_eq(left: CBindings, right: CBindings) -> bool:
    return left == right

def bindings_add_var_binding(bindings: CBindings, var: CAtom, atom: CAtom) -> bool:
    bindings.add_var_binding(var, atom)
    return True

def bindings_is_empty(bindings: CBindings) -> bool:
    return bindings.is_empty()

def bindings_narrow_vars(bindings: CBindings, vars: 'CVecAtom'):
    bindings.bindings = {var.get_name(): bindings.bindings[var.get_name()] for var in vars.atoms if var.get_name() in bindings.bindings}

def bindings_resolve(bindings: CBindings, var: CAtom) -> Optional[CAtom]:
    return bindings.resolve(var)

def bindings_to_str(bindings: CBindings) -> str:
    return str(bindings)

def bindings_list(bindings: CBindings) -> List[Tuple[CAtom, CAtom]]:
    return [(CAtom.atom_var(var_name), atom) for var_name, atom in bindings.bindings.items()]

class CBindingsSet:
    def __init__(self, bindings_list: Optional[List[CBindings]] = None):
        self.bindings_list = bindings_list or []

    def push(self, bindings: CBindings):
        self.bindings_list.append(bindings)

    def is_empty(self) -> bool:
        return len(self.bindings_list) == 0

    def is_single(self) -> bool:
        return len(self.bindings_list) == 1

    def list(self) -> List[CBindings]:
        return self.bindings_list

    def unpack(self) -> List[Dict[str, CAtom]]:
        return [bindings.bindings for bindings in self.bindings_list]

    def __eq__(self, other):
        if isinstance(other, CBindingsSet):
            return self.bindings_list == other.bindings_list
        return False

    def __str__(self):
        return f"CBindingsSet({self.bindings_list})"

def bindings_set_empty() -> CBindingsSet:
    return CBindingsSet()

def bindings_set_single() -> CBindingsSet:
    return CBindingsSet([CBindings()])

def bindings_set_free(bindings_set: CBindingsSet):
    trace_msg(f"Freeing bindings set: {bindings_set}")

def bindings_set_eq(set1: CBindingsSet, set2: CBindingsSet) -> bool:
    return set1 == set2

def bindings_set_is_empty(bindings_set: CBindingsSet) -> bool:
    return bindings_set.is_empty()

def bindings_set_is_single(bindings_set: CBindingsSet) -> bool:
    return bindings_set.is_single()

def bindings_set_to_str(bindings_set: CBindingsSet) -> str:
    return str(bindings_set)

def bindings_set_clone(bindings_set: CBindingsSet) -> CBindingsSet:
    cloned_list = [bindings.clone() for bindings in bindings_set.bindings_list]
    return CBindingsSet(cloned_list)

def bindings_set_from_bindings(bindings: CBindings) -> CBindingsSet:
    return CBindingsSet([bindings])

def bindings_set_push(bindings_set: CBindingsSet, bindings: CBindings):
    bindings_set.push(bindings)

def bindings_set_add_var_binding(bindings_set: CBindingsSet, var: CAtom, value: CAtom):
    new_bindings_list = []
    for bindings in bindings_set.bindings_list:
        if bindings_add_var_binding(bindings, var, value):
            new_bindings_list.append(bindings)
    bindings_set.bindings_list = new_bindings_list

def bindings_set_add_var_equality(bindings_set: CBindingsSet, var_a: CAtom, var_b: CAtom):
    new_bindings_list = []
    for bindings in bindings_set.bindings_list:
        val_a = bindings.resolve(var_a)
        val_b = bindings.resolve(var_b)
        if val_a and val_b:
            if val_a == val_b:
                new_bindings_list.append(bindings)
        elif val_a:
            bindings.add_var_binding(var_b, val_a)
            new_bindings_list.append(bindings)
        elif val_b:
            bindings.add_var_binding(var_a, val_b)
            new_bindings_list.append(bindings)
        else:
            bindings.add_var_binding(var_a, var_b)
            new_bindings_list.append(bindings)
    bindings_set.bindings_list = new_bindings_list

def bindings_set_merge_into(set1: CBindingsSet, set2: CBindingsSet):
    merged_bindings_list = []
    for bindings1 in set1.bindings_list:
        for bindings2 in set2.bindings_list:
            merged_set = bindings_merge(bindings1, bindings2)
            if not merged_set.is_empty():
                merged_bindings_list.extend(merged_set.bindings_list)
    set1.bindings_list = merged_bindings_list

def bindings_set_list(bindings_set: CBindingsSet) -> List[CBindings]:
    return bindings_set.list()

def bindings_set_unpack(bindings_set: CBindingsSet) -> List[Dict[str, CAtom]]:
    return bindings_set.unpack()

# ------------------ Substitute Function ------------------

def substitute(atom: CAtom, bindings: CBindings) -> CAtom:
    if atom.atom_kind == AtomKind.VARIABLE:
        resolved_atom = bindings.resolve(atom)
        if resolved_atom:
            return resolved_atom
        else:
            return atom
    elif atom.atom_kind == AtomKind.EXPR:
        substituted_children = [substitute(child, bindings) for child in atom.children]
        return CAtom.atom_expr(substituted_children)
    else:
        return atom.clone()

# ------------------ Additional Utility Functions ------------------

def set_no_mock_objects_flag(flag: bool):
    global mock_throw_error
    mock_throw_error = flag

def get_no_mock_objects_flag() -> bool:
    return mock_throw_error

# Now you have the complete `hyperonpy.py` module. Please combine this code with the previous code blocks to have the full module ready for use in your project.
# Continue from the previous code...

# ------------------ Utility Functions ------------------

def func_to_string_no_arg(func: Callable) -> str:
    return func()

def func_to_string(func: Callable, *args) -> str:
    return func(*args)

def check_type(space: CSpace, atom: CAtom, typ: CAtom) -> bool:
    return atom.get_type() == typ

def validate_atom(space: CSpace, atom: CAtom) -> bool:
    # Placeholder for validation logic
    return True

def get_atom_types(space: CSpace, atom: CAtom) -> List[CAtom]:
    return [atom.get_type()]

# ------------------ Load ASCII Function ------------------

def load_ascii(name: str, space: CSpace):
    try:
        with open(name, 'r') as f:
            content = f.read()
            parser = CSExprParser(content)
            while True:
                atom = parser.parse()
                if atom is None:
                    break
                space.add(atom)
        return True
    except FileNotFoundError:
        if mock_throw_error:
            raise RuntimeError(f"load_ascii: file '{name}' not found")
        else:
            warnings.warn(f"load_ascii: file '{name}' not found")
            return False

# ------------------ Logging Functions ------------------

def log_error(msg: str):
    warnings.warn(f"ERROR: {msg}")

def log_warn(msg: str):
    warnings.warn(f"WARNING: {msg}")

def log_info(msg: str):
    print(f"INFO: {msg}")

# ------------------ Example Classes for Grounded Objects ------------------

class AddGroundedObject:
    def __init__(self):
        self.typ = CAtom.atom_sym("Function+")

    def execute(self, *args):
        print(f"args={args}")
        trace_break()
        if len(args) != 2:
            raise ValueError("Add function requires exactly 2 arguments.")
        
        return int(args[0]) + int(args[1])

    def serialize(self):
        return "<AddFunction+>"

    def clone(self):
        return AddGroundedObject()

def trace_break():
    print("TRACE: Press any key to continue...")
    wait_for_key() #keyboard.read_event()  # This waits for a key event


def wait_for_key():    
    import sys  
    fd = sys.stdin.fileno()
    import termios
    old_settings = termios.tcgetattr(fd)
    try:
        import tty  
        tty.setraw(fd)
        sys.stdin.read(1)  # This waits for a single key press
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    print("Key pressed!")

# ------------------ Example Usage ------------------

def demo0():
    # Create a space
    space = CSpace()

    # Create atoms
    atom1 = CAtom.atom_sym("Hello")
    atom2 = CAtom.atom_var("X")
    grounded_atom = CAtom.atom_gnd(AddGroundedObject())

    # Add atoms to the space
    space.add(atom1)
    #space.add(atom2)
    space.add(grounded_atom)


    # Query the space
    result = space.query(atom2)
    mesg(DEBUG,"Query0 result:", result)

    # Create an expression
    expr = CAtom.atom_expr([grounded_atom, CAtom.atom_sym("2"), CAtom.atom_sym("3")])

    # Interpret the expression
    interpreter = CInterpreter(space, expr)
    while interpreter.has_next():
        interpreter.next()
        mesg(DEBUG,"CInterpreter step result:", interpreter.get_step_result())

    # Get the final result
    final_result = interpreter.get_result()
    mesg(DEBUG,"Final interpretation result:", final_result)

# Uncomment to run the example
# example_usage()

# ------------------ CInterpreter Class ------------------

class CInterpreter:
    """CInterpreter for evaluating expressions within a grounding space."""
    def __init__(self, gnd_space: CSpace, expr: CAtom):
        self.gnd_space = gnd_space
        self.expr = expr
        self.step_result = CStepResult([expr], has_more_steps=True)
        self.step_count = 0
        self.max_steps = 10  # Adjust as needed

    def has_next(self) -> bool:
        return self.step_result.has_next()

    def next(self):
        if self.has_next():
            self.step_count += 1
            # Simplified execution logic
            if self.step_count >= self.max_steps:
                # Execution limit reached
                self.step_result = CStepResult(self.step_result.get_result(), has_more_steps=False)
                warnings.warn("Maximum steps reached in interpreter.")
            else:
                # Evaluate the expression
                evaluated_atoms = self.evaluate_atom(self.expr)
                self.step_result = CStepResult(evaluated_atoms, has_more_steps=False)
        else:
            warnings.warn("No more steps available in interpreter.")


    def evaluate_atom(self, atom: CAtom) -> List[CAtom]:
	    def safe_evaluate_atom(child):
	        # Try to evaluate the child and return the first element if successful
	        evaluated = self.evaluate_atom(child)
	        # If the evaluation is a non-empty list, return its first element, otherwise fallback to child
	        return evaluated[0] if isinstance(evaluated, list) and evaluated else child
	
	    print(f"evaluate_atom({atom})")
	    if atom.is_grounded():
	        print(f"evaluate_atom.is_grounded({atom})")
	        # Execute grounded atom with safely evaluated arguments
	        args = [safe_evaluate_atom(child) for child in atom.get_children()]
	        return atom.execute(args)
	    elif atom.atom_kind == AtomKind.EXPR:
	        print(f"evaluate_atom.expression({atom})")
	        # Evaluate expression with safely evaluated children
	        evaluated_children = [safe_evaluate_atom(child) for child in atom.children]
	        return [CAtom.atom_expr(evaluated_children)]
	    else:
	        # Return atom as is for other types (e.g., variables, symbols)
	        return [atom]

    def get_result(self) -> List[CAtom]:
        if self.has_next():
            warnings.warn("Interpretation not finished yet.")
            return []
        return self.step_result.get_result()

    def get_step_result(self) -> List[CAtom]:
        return self.step_result.get_result()

    def is_complete(self) -> bool:
        return not self.has_next()

def atom_match_atom_single(atom1: CAtom, atom2: CAtom) -> Optional[CBindings]:
    bindings = CBindings()
    if atom1.atom_kind == AtomKind.VARIABLE:
        bindings.add_var_binding(atom1, atom2)
        return bindings
    elif atom2.atom_kind == AtomKind.VARIABLE:
        bindings.add_var_binding(atom2, atom1)
        return bindings
    elif atom1.atom_kind == atom2.atom_kind:
        if atom1.atom_kind == AtomKind.SYMBOL:
            if atom1.name == atom2.name:
                return bindings
            else:
                return None
        elif atom1.atom_kind == AtomKind.EXPR:
            if len(atom1.children) != len(atom2.children):
                return None
            for child1, child2 in zip(atom1.children, atom2.children):
                child_bindings = atom_match_atom_single(child1, child2)
                if child_bindings is None:
                    return None
                else:
                    merged_bindings = bindings.merge(child_bindings)
                    if merged_bindings.is_empty():
                        return None
                    else:
                        bindings = merged_bindings.bindings_list[0]
            return bindings
        elif atom1.is_grounded() and atom2.is_grounded():
            if atom1.grounded_object == atom2.grounded_object:
                return bindings
            else:
                return None
    else:
        return None

def atom_match_atom(atom1: CAtom, atom2: CAtom) -> CBindingsSet:
    bindings = atom_match_atom_single(atom1, atom2)
    if bindings:
        return CBindingsSet([bindings])
    else:
        return CBindingsSet()

def atom_match(a: CAtom, b: CAtom) -> CBindingsSet:
    return atom_match_atom(a, b)



def demo1():
    # Create a grounding space
    space = CSpace()

    # Create atoms
    atom1 = CAtom.atom_sym("X")
    atom2 = CAtom.atom_var("Y")
    grounded_atom = CAtom.atom_gnd(42, CAtom.atom_sym("Int"))

    # Add atoms to space
    space.add(atom1)
    space.add(atom2)
    space.add(grounded_atom)

    # Query space
    result = space.query(atom1)
    mesg(DEBUG,"Query result:", result)

    # Run interpreter on an expression
    expr = CAtom.atom_expr([atom1, atom2])
    interpreter = CInterpreter(space, expr)
    while interpreter.has_next():
        interpreter.next()
        mesg(DEBUG,"CInterpreter step result:", interpreter.get_step_result())

    # Final result from interpreter
    mesg(DEBUG,"Final interpretation result:", interpreter.get_result())

    # Test serialization
    serializer = PythonToCSerializer()
    grounded_atom.serialize(serializer)
    mesg(DEBUG,"Serialized grounded atom:", serializer.get_serialized_data())

    # Testing environment builder
    env_builder = EnvBuilder()
    env_builder.set_working_dir("/path/to/dir")
    env_builder.push_include_path("/path/to/include")
    environment = env_builder.build()
    mesg(DEBUG,"Environment:", environment)




# Note: Implement any other missing functions as required.

# Testing and Examples
def demo2():
    # Create a space
    space = space_new_grounding()

    # Create atoms
    symbol_atom = CAtom.atom_sym("X")
    variable_atom = CAtom.atom_var("Y")
    grounded_type = CAtom.atom_sym("Int")
    grounded_atom = CAtom.atom_gnd(42, grounded_type)

    # Add atoms to the space
    space.add(symbol_atom)
    space.add(variable_atom)
    space.add(grounded_atom)

    # Query the space
    query_atom = CAtom.atom_sym("X")
    query_result = space.query(query_atom)
    mesg(DEBUG,"Query result:", [str(binding.bindings) for binding in query_result.list()])

    # Replace an atom
    new_atom = CAtom.atom_sym("Z")
    space.replace(symbol_atom, new_atom)
    mesg(DEBUG,"Space after replacement:", space)

    # Interpret an expression
    expr_atom = CAtom.atom_expr([variable_atom, grounded_atom])
    interpreter = CInterpreter(space, expr_atom)

    while interpreter.has_next():
        interpreter.next()
        mesg(DEBUG,"Step result:", interpreter.get_step_result())

    # Final result from interpreter
    mesg(DEBUG,"Final interpretation result:", interpreter.get_result())

    # Example: Vector of Atoms
    atom_vector = CVecAtom([])
    atom_vector.push(CAtom.atom_sym("Hydrogen"))
    atom_vector.push(CAtom.atom_sym("Oxygen"))

    mesg(DEBUG,"Atoms in Vector:")
    for atom in atom_vector:
        print(atom)

    # Example: Module Descriptor
    mod_desc = CModuleDescriptor(name="Physics", description="Physics simulation module")
    print(mod_desc)

    # Example: Expression Parser
    expr_parser = CSExprParser("(2 + (3 * 5))")
    result = expr_parser.parse()
    mesg(DEBUG,"Parsed Expression Result:", result)

    # Example: Space with Atoms
    space2 = space_new_grounding()
    space2.add(CAtom.atom_sym("Proton"))
    space2.add(CAtom.atom_sym("Neutron"))
    mesg(DEBUG,"Items in Space2:")
    for item in space2.atoms:
        print(item)

    # Example: CInterpreter usage
    interpreter2 = CInterpreter(space2, CAtom.atom_sym("sample_expr"))
    while interpreter2.has_next():
        interpreter2.next()
        mesg(DEBUG,"Interpreter2 Step result:", interpreter2.get_step_result())

    mesg(DEBUG,"Final Interpretation Result:", interpreter2.get_result())

    # Example: Logging
    log_info("This is an informational message.")
    log_warn("This is a warning message.")
    log_error("This is an error message.")

    # Example: Environment Builder
    env_builder = EnvBuilder()
    env_builder.set_working_dir("/path/to/working/dir")
    env_builder.set_config_dir("/path/to/config/dir")
    env_builder.push_include_path("/path/to/include")
    env_builder.use_test_env()
    environment = env_builder.build()
    mesg(DEBUG,"Environment:", environment)

    # Example: Runner State
    metta = metta_new(space)
    runner_state = CRunnerState(metta, expr_parser)
    while not runner_state.is_complete():
        runner_state.step()
        mesg(DEBUG,"Runner State Current Results:", runner_state.current_results())

    # Example: Atom Matching
    pattern = CAtom.atom_expr([CAtom.atom_var("Var1"), CAtom.atom_sym("B")])
    expression = CAtom.atom_expr([CAtom.atom_sym("A"), CAtom.atom_sym("B")])

    bindings_set = atom_match_atom(expression, pattern)
    if not bindings_set.is_empty():
        for bindings in bindings_set.list():
            mesg(DEBUG,"Match found with bindings:", bindings.bindings)
    else:
        mesg(DEBUG,"No match found.")

    # Example: Using atom_get_metatype
    metatype = atom_get_metatype(symbol_atom)
    mesg(DEBUG,"Metatype of symbol_atom:", metatype)


has_applied_monkey_patches = None

def apply_monkey_patches():
    global has_applied_monkey_patches
    
    # Check if monkey patches have already been applied
    if has_applied_monkey_patches is not None and has_applied_monkey_patches is True:
        return

    mesg(DEBUG, f"hyperonpy::apply_monkey_patches")
    # Set the flag to indicate that the patches are being applied
    has_applied_monkey_patches = True

    # Importing inside the function to avoid circular import issues
    from hyperon.atoms import SymbolAtom, ExpressionAtom, GroundedAtom, VariableAtom, ValueAtom

    def monkey_patch_class(cls):
        # Save the original __init__ method
        original_init = cls.__init__
        
        # Define the new __init__ method
        def new_init(self, *args, **kwargs):
            original_init(self, *args, **kwargs)
            # Only add 'value' if it doesn't already exist
            if not hasattr(self, 'value'):
                self.value = self
        
        # Only patch __init__ to add 'value' if missing
        cls.__init__ = new_init

        # Only add get_object if it does not already exist
        if not hasattr(cls, 'get_object'):
            cls.get_object = lambda self: self

        # Only add __iter__ if it does not already exist
        if not hasattr(cls, '__iter__'):
            cls.__iter__ = lambda self: iter([self])

        # Check if the class already has a __getitem__ method
        original_getitem = getattr(cls, '__getitem__', None)

        # Define the new __getitem__ method
        def new_getitem(self, index):
            # If there was an original __getitem__, call it
            if original_getitem:
                try:
                    return original_getitem(self, index)
                except Exception as e:
                    print(f"Original __getitem__ raised an exception: {e}")
            # Otherwise, return the instance itself
            return self

        # Monkey patch the __getitem__ method
        cls.__getitem__ = new_getitem

    # Apply the monkey patch function to multiple classes
    monkey_patch_class(SymbolAtom)
    monkey_patch_class(ExpressionAtom)
    monkey_patch_class(VariableAtom)
    monkey_patch_class(GroundedAtom)
    monkey_patch_class(ValueAtom)

# History file for REPL
def install_history():
    histfile = os.path.join(os.path.expanduser("~"), ".metta_history")

    try:
        readline.set_history_length(10000)
        readline.read_history_file(histfile)
        h_len = readline.get_current_history_length()
    except FileNotFoundError:
        open(histfile, 'wb').close()
        h_len = 0

    def save_history(prev_h_len, histfile):
        """Save command history to the history file."""
        new_h_len = readline.get_current_history_length()
        readline.set_history_length(10000)
        readline.append_history_file(new_h_len - prev_h_len, histfile)

    atexit.register(save_history, h_len, histfile)

global runner
runner = None
global wont_need_repl
wont_need_repl=True

# Manual command-line argument processing in real-time
def process_args():
    wont_need_repl = False  # Keep track if we won't need to start the REPL
    i = 1  # Skip the first argument (script name)

    while i < len(sys.argv):
        arg = sys.argv[i]

        if arg in ("--demo"):
            # demo0()
            demo1()
            demo2()

        if arg in ("-v", "--verbosity"):
            try:
                verbosity_level = int(sys.argv[i + 1])
                set_verbosity(verbosity_level)
                i += 1  # Skip next item
            except (ValueError, IndexError):
                print("Invalid verbosity level. Defaulting to USER.")
                set_verbosity(USER)

        elif arg in ("-p", "--path"):
            if i + 1 < len(sys.argv):
                path = sys.argv[i + 1]
                mesg(DEBUG, f"Adding path: {path}")
                env_builder_push_include_path(cb,path)
                i += 1
        elif arg == "--version":
            mesg(USER, f"Hyperon version: {hyperon.__version__}")
            sys.exit(0)  # Exit after showing version

        elif arg in ("-h", "--help"):        
            print_help()
            sys.exit(0)  # Exit after showing help

        elif arg in ("-f", "--file"):
            if i + 1 < len(sys.argv):
                file = sys.argv[i + 1]                
                load_file(file)
                wont_need_repl = True  # Mark that a file was loaded
                i += 1

        else:
            # Assume this is a file to process
            load_file(arg)
            wont_need_repl = True  # Mark that a file was loaded

        i += 1

    return wont_need_repl

# Print help information
def print_help():
    # Get the script name dynamically
    script_name = os.path.basename(sys.argv[0])  # This will get the current script's name

    help_text = f"""
MeTTaLog Runner/REPL in Python

Usage: {script_name} [options] [files]

Options:
  -v, --verbosity <level>   Set verbosity level: 0 (SILENT), 1 (USER), 2 (DEBUG), 3 (TRACE)
  -p, --path <path>         Add search paths for modules
  -f, --file <file>         MeTTa script files to execute
  --repl                    Enter REPL
  --version                 Print the version and exit
  --help                    Display this help message
    """
    print(help_text)

def make_runner():
    global runner
    if runner is not None:
        return runner
    mesg(DEBUG, f"hyperonpy::make_runner")
    return hyperon.runner.MeTTa(env_builder=hyperon.Environment.custom_env(working_dir=os.path.dirname(".")))

# Load and execute a file
def load_file(file):    
    global runner

    wont_need_repl = True
    if file == "--repl":
        # Start REPL immediately and ignore further arguments
        if runner is None: 
            runner = make_runner()
        REPL(runner).main_loop()
        return

    try:
        mesg(DEBUG, f"Executing MeTTa script from: {file}")
        with open(file) as f:
            program = f.read()        
            if runner is None: runner = make_runner()
            for result in runner.run(program):
                mesg(USER, f"Result: {result}")
    except FileNotFoundError:
        mesg(USER, f"Error: File '{file}' not found.")
    except Exception as e:
        mesg(USER, f"An error occurred: {e}")
        if METTALOG_VERBOSE >= DEBUG:
            traceback.print_exc()

# REPL for interactive input with command history support
class REPL:
    def __init__(self, runner):
        self.history = []  # Initialize command history
        self.runner = runner

    def main_loop(self):
        #install_history()
        while True:
            try:
                line = input("metta> ")  # Use input function for user input

                if line == '.history':
                    for idx, item in enumerate(self.history):
                        mesg(USER, f"{idx + 1}: {item}")
                    continue

                # If input is not empty, evaluate it
                if line:
                    self.history.append(line)
                    if self.runner is None:
                        global runner
                        if runner is None: runner = make_runner()
                        self.runner = runner
                    result = self.runner.run(line)
                    if result is not None:
                        mesg(USER, result)

            except (KeyboardInterrupt, EOFError):
                # Exit REPL gracefully when Ctrl-D (EOF) is pressed
                mesg(USER, "\nExiting REPL...")
                return

            except Exception as e:
                mesg(USER, f"Error: {e}")
                if METTALOG_VERBOSE >= DEBUG:
                    traceback.print_exc()




# Main function
def main():
    mesg(DEBUG,"hyperonpy::main")
    apply_monkey_patches()
    wont_need_repl = False
    # Process command-line arguments and track if REPL or files were handled
    wont_need_repl = process_args()

    # If no files were loaded and --repl wasn't triggered, enter REPL
    if not wont_need_repl:
        load_file("--repl")

def hyperonpy():
    mesg(DEBUG,"hyperonpy::hyperonpy")
    #from hyperon.runner import MeTTa
    #runner = MeTTa(env_builder=hyperon.Environment.custom_env(working_dir=os.path.dirname(".")))
    #apply_monkey_patches()
    # Process command-line arguments and track if REPL or files were handled
    wont_need_repl = process_args()

    # If no files were loaded and --repl wasn't triggered, enter REPL
    # if not wont_need_repl: REPL(runner).main_loop()


if __name__ == "__main__":
    main()
else:
    mesg(DEBUG,f"__name__={__name__}")
    hyperonpy()

#    Please continue where you left off /// Make sure you give me back the complete and workign versions of the entire content)  this is taking the place of a the old pybind11 C++ file so module level functions and class names and class function  cannot be renamed or ommited.  Continue  and dont worry about space limitations 

