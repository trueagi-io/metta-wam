#!/usr/bin/env python3


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
from typing_extensions import *
from typing import get_type_hints
from collections import Counter
from glob import glob
from time import monotonic_ns, time
import traceback

from mettalog import *
print_l_cmt(2, f";; ...doing {__file__}...{__package__} name={__name__}")

def addSpaceName(name, space):
    global syms_dict, space_refs
    prev = getSpaceByName(name)
    name = str(name)
    if not name.startswith("&"):
        name = "&" + name
    syms_dict[name] = lambda _: G(asSpaceRef(space))
    if prev is None:
        space_refs[name] = lambda : space

def getSpaceByName(name):
    global space_refs
    if name is ValueAtom:
        name = name.get_value()
    if name is GroundingSpace:
        return name
    name = str(name)
    if not name.startswith("&"):
        name = "&" + name
    found = space_refs.get(name, None)
    if found is None: return None
    return found()

def getNameBySpace(target_space):
    if target_space is None:
        return None
    global space_refs, syms_dict
    # Search in space_refs
    for name, space_func in space_refs.items():
        S = space_func()
        if S is target_space:
            return name
        if S:
            if id(S) == id(target_space):
                return name
    # Search in syms_dict
    for name, space_func in syms_dict.items():
        GR = space_func(None)
        if GR:
            if id(GR) == id(target_space):
                return name
            if id(GR.get_object()) == id(target_space):
                return name
    return None

vspace_ordinal = 0

@export_flags(Janus=True)
def get_atoms_iter_from_space(space_name):
    space = getSpaceByName(space_name)
    if space:
        get_iterator = getattr(space, "atoms_iter", None) # Create a new iterator
        if get_iterator is not None:
            return get_iterator()
        else:
            get_iterator = getattr(space, "get_atoms", None) # Create a new iterator
            if get_iterator is not None:
                return iter(get_iterator)
            else:
                V = V("X")
                iterator = space.query(V) # Create a new iterator
                return iterator

context_atom_iters = {}

@export_flags(Janus=True, arity=2, flags=PL_FA_NONDETERMINISTIC)
def atoms_iter_from_space(space_name, result, context):
    global idKey, context_atom_iters
    control = PL_foreign_control(context)
    context = PL_foreign_context(context)
    id = context

    if control == PL_FIRST_CALL:
        id = idKey
        idKey = idKey + 1
        iterator = get_atoms_iter_from_space(space_name)
        if iterator is not None:
            try:
                circles = Circles()
                while True:
                    value = next(iterator)
                    if res_unify(result, m2s(circles, value)):
                        context_atom_iters[id] = IteratorAndConversionDict(iterator, circles)  # Store it in the dictionary
                        return PL_retry(context)
                    return PL_retry(context)
            except StopIteration:
                del context_atom_iters[id]  # Clean up
        return False

    elif control == PL_REDO:
        iteratorAndCircs = context_atom_iters.get(id)
        if iteratorAndCircs is not None:
            try:
                iterator = iteratorAndCircs.get_iterator()
                circles = iteratorAndCircs.get_conversion_dict()
                while True:
                    value = next(iterator)
                    if res_unify(result, m2s(circles, value)):
                        return PL_retry(context)
                del context_atom_iters[id]  # Clean up
                return False
            except StopIteration:
                del context_atom_iters[id]  # Clean up
                return False
        pass

    elif control == PL_PRUNED:
        # Clean up the iterator when we're done
        if id in context_atom_iters:
            del context_atom_iters[id]
        pass


# Define the foreign functions
@export_flags(Janus=True)
def query_from_space(space_name, query_atom, result):
    space = getSpaceByName(space_name)
    if space:
        atoms = space.query(query_atom)
        return res_unify(result, atoms)
    return False

@export_flags(Janus=True)
def add_to_space(space_name, atom):
    space = getSpaceByName(space_name)
    if space:
        circles = Circles()
        atom = s2m(circles, atom)
        if isinstance(space, SpaceRef):
            return space.add_atom(atom)
        return space.add(atom)
    return False

@export_flags(Janus=True)
def remove_from_space(space_name, atom):
    space = getSpaceByName(space_name)
    if space:
        circles = Circles()
        atom = s2m(circles, atom)
        return space.remove(atom)
    return False

@export_flags(Janus=True)
def replace_in_space(space_name, from_atom, to_atom):
    space = getSpaceByName(space_name)
    if space:
        circles = Circles()
        to_atom = s2m(circles, to_atom)
        from_atom = s2m(circles, from_atom)
        return space.replace(from_atom, to_atom)
    return False

@export_flags(Janus=True)
def atom_count_from_space(space_name, result):
    space = getSpaceByName(space_name)
    if space:
        return res_unify(result, space.atom_count())
    return False

@export_flags(Janus=True)
def get_atoms_from_space(space_name, result):
    space = getSpaceByName(space_name)
    if space:
        circles = Circles()
        atoms = list(space.get_atoms())
        satoms = [m2s(circles, atom) for atom in atoms]
        return res_unify(result, satoms)
    return False


@export_flags(Janus=True)
def find_rust_space(space_name, result):
    space = getSpaceByName(space_name)
    named = getNameBySpace(space)
    if space:
        return res_unify(result, named)
    return False

rustspace_ordinal = 0
@export_flags(Janus=True)
def new_rust_space(result):
    rustspace_ordinal = rustspace_ordinal + 1
    name = f"&vspace_{rustspace_ordinal}"
    space = GroundingSpace()
    addSpaceName(name, space)
    return res_unify(result, swipAtom(name))

# subclass to later capture any utility we can add to 'subst'
def asSpaceRef(obj):
    if isinstance(obj, (VSpaceRef, SpaceRef)):
        return obj
    return VSpaceRef(obj)

class VSpaceRef(SpaceRef):

    """
    A reference to a Space, which may be accessed directly, wrapped in a grounded atom,
    or passed to a MeTTa interpreter.
    """

    def __init__(self, space_obj):
        """
        Initialize a new SpaceRef based on the given space object, either a CSpace
        or a custom Python object.
        """
        super().__init__(space_obj)
        self.py_space_obj = space_obj
        #if type(space_obj) is hp.CSpace:
        #    self.cspace = space_obj
        #else:
        #    self.cspace = hp.space_new_custom(space_obj)

    def is_VSpace(self):
        return isinstance(self.py_space_obj, VSpace)

    def get_atoms(self):
        """
        Returns a list of all Atoms in the Space, or None if that is impossible
        """
        if self.is_VSpace():
            return self.py_space_obj.get_atoms()

        res = hp.space_list(self.cspace)
        if res == None:
            return None
        result = []
        for r in res:
            result.append(Atom._from_catom(r))
        return result


    def __del__(self):
        """Free the underlying CSpace object """
        return
        if self.is_VSpace(): self.py_space_obj.__del__()
        else: hp.space_free(self.cspace)

    def __eq__(self, other):
        """Compare two SpaceRef objects for equality, based on their underlying spaces."""
        if not isinstance(other, SpaceRef): return False
        if self.is_VSpace(): return get_payload(self) is other.get_payload(self)
        else: return hp.space_eq(self.cspace, other.cspace)


    @staticmethod
    def _from_cspace(cspace):
        """
        Create a new SpaceRef based on the given CSpace object.
        """
        return asSpaceRef(cspace)

    def copy(self):
        """
        Returns a new copy of the SpaceRef, referencing the same underlying Space.
        """
        return self

    def add_atom(self, atom):
        """
        Add an Atom to the Space.
        """
        if self.is_VSpace():
            return self.py_space_obj.add(atom)

        hp.space_add(self.cspace, atom.catom)

    def remove_atom(self, atom):
        """
        Delete the specified Atom from the Space.
        """
        if self.is_VSpace():
            return self.py_space_obj.remove(atom)

        return hp.space_remove(self.cspace, atom.catom)

    def replace_atom(self, atom, replacement):
        """
        Replaces the specified Atom, if it exists in the Space, with the supplied replacement.
        """
        if self.is_VSpace():
            return self.py_space_obj.replace(atom, replacement)

        return hp.space_replace(self.cspace, atom.catom, replacement.catom)

    def atom_count(self):
        """
        Returns the number of Atoms in the Space, or -1 if it cannot be readily computed.
        """

        if self.is_VSpace():
            return self.py_space_obj.atom_count()

        return hp.space_atom_count(self.cspace)


    def get_payload(self):
        """
        Returns the Space object referenced by the SpaceRef, or None if the object does not have a
        direct Python interface.
        """
        if self.is_VSpace():
            return self.py_space_obj;

        return hp.space_get_payload(self.cspace)

    def query(self, pattern):
        """
        Performs the specified query on the Space, and returns the result as a BindingsSet.
        """
        if self.is_VSpace():
            return self.py_space_obj.query(pattern);

        result = hp.space_query(self.cspace, pattern.catom)
        return BindingsSet(result)

    def subst(self, pattern, templ):
        """
        Performs a substitution within the Space
        """

        if self.is_VSpace():
            return self.py_space_obj.subst(pattern, templ);

        cspace = super().cspace
        return [Atom._from_catom(catom) for catom in
                hp.space_subst(cspace, pattern.catom,
                               templ.catom)]



@export_flags(MeTTa=True)
class VSpace(AbstractSpace):

    def from_space(self, cspace):
        self.gspace = GroundingSpaceRef(cspace)

    def __init__(self, space_name=None, unwrap=False):
        super().__init__()
        #addSpaceName(ispace_name,self)
        if space_name is None:
            global vspace_ordinal
            ispace_name = f"&vspace_{vspace_ordinal}"
            vspace_ordinal = vspace_ordinal + 1
            space_name = ispace_name
        self.sp_name = PySwipAtom(space_name)
        swip.assertz(f"was_asserted_space('{space_name}')")
        #swip.assertz(f"was_space_type('{space_name}',asserted_space)")
        self.sp_module = newModule("user")
        self.unwrap = unwrap
        addSpaceName(space_name, self)

    def __del__(self):
        return
        pass

    def swip_space_name(self):
        return swipRef(self.sp_name)
        #return self.sp_name

    @foreign_framed
    def query(self, query_atom):
        new_bindings_set = BindingsSet.empty()
        #swipl_load = PL_new_term_ref()
        metta_vars = [atom for atom in query_atom.iterate() if atom.get_type() == AtomKind.VARIABLE]
        metaVarNames = [str(atom) for atom in metta_vars]
        circles = Circles()
        swivars = [m2s(circles, item, 1) for item in metta_vars]
        varsList = Variable()
        varsList.unify(swivars)
        varNames = Variable()
        varNames.unify(metaVarNames)
        swip_obj = m2s(circles, query_atom)
        if verbose > 1: print_cmt(f"circles={circles}")
        #if verbose>1: print_cmt(f"metta_vars={metta_vars}, swivars={swivars}")
        q = PySwipQ(Functor('metta_iter_bind', 4)
                    (self.swip_space_name(), swip_obj, varsList, varNames), module=self.sp_module)

        while q.nextSolution():
            swivars = varsList.value
            bindings = Bindings()
            vn = 0
            for mv in metta_vars:
                svar = swivars[vn]
                sval = svar
                if verbose > 1: pt(f"svar({vn})=", svar, " ")
                if isinstance(svar, Variable):
                    sval = sval.value
                else: sval = svar
                if verbose > 1: pt(f"sval({vn})=", sval, " ")
                mval = s2m(circles, sval)
                if verbose > 1: pt(f"mval({vn})=", mval, " ")
                bindings.add_var_binding(mv, mval)
                vn = vn + 1

            new_bindings_set.push(bindings)
        q.closeQuery()
        return new_bindings_set

    def _call(self, functor_name, *args):
        q = PySwipQ(Functor(functor_name, len(args) + 1)(self.swip_space_name(), *args), module=self.sp_module)
        try: return q.nextSolution()
        except Exception as e:
            if verbose > 0: print_cmt(f"Error: {e}")
            if verbose > 0: traceback.print_exc()
        finally: q.closeQuery()

    @foreign_framed
    def add(self, atom):
        circles = Circles()
        return self._call("add-atom", m2s(circles, atom))

    @foreign_framed
    def add_atom(self, atom):
        circles = Circles()
        return self._call("add-atom", m2s(circles, atom))

    @foreign_framed
    def remove_atom(self, atom):
        circles = Circles()
        return self._call("remove-atom", m2s(circles, atom))

    @foreign_framed
    def remove(self, atom):
        circles = Circles()
        return self._call("remove-atom", m2s(circles, atom))

    @foreign_framed
    def replace(self, from_atom, to_atom):
        circles = Circles()
        return self._call("replace-atom", m2s(circles, from_atom), m2s(circles, to_atom))

    @foreign_framed
    def subst(self, pattern, templ):
        """
        Performs a substitution within the Space
        """
        circles = Circles()
        return self._call("subst_pattern_template", m2s(circles, pattern), m2s(circles, templ))

    @foreign_framed
    def atom_count(self):
        result = list(swip.query(f"'atom-count'('{self.sp_name}',AtomCount)"))
        if verbose > 1: print_cmt(result)
        if result is None: return 0
        if len(result) == 0: return 0
        CB = result[0]
        if CB is None: return 0
        C = CB['AtomCount']
        if not isinstance(C, int):
            C = C.value
        return C

    @foreign_framed
    def get_atoms(self):
        circles = Circles()
        result = list(swip.query(f"'get-atoms'('{self.sp_name}',AtomsList)"))
        if result is None: return []
        if len(result) == 0: return []
        CB = result[0]
        if CB is None: return []
        C = CB['AtomsList']
        if verbose > 1: print_cmt(f"get_atoms={type(C)}")
        R = s2m(circles, C)
        return R

    def atoms_iter(self):

        swipl_fid = PL_open_foreign_frame()
        Atoms = Variable("Iter")
        q = PySwipQ(Functor("atoms_iter", 2)(self.swip_space_name(), Atoms), module=self.sp_module)

        def closeff():
            nonlocal swipl_fid
            ff = swipl_fid
            swipl_fid = None
            if ff is not None:
                PL_discard_foreign_frame(ff)


        class LazyIter:

            circles = Circles()

            def __init__(self, q, v):
                self.q, self.v = q, v

            def __iter__(self):
                return self

            def __next__(self):
                if self.q.nextSolution():
                    return s2m(circles, self.v.value.value)
                closeff()
                raise StopIteration

            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc_value, traceback):
                self.q.closeQuery()
                closeff()

        return LazyIter(q, Atoms)

    def copy(self):
        return self

class VSpaceCallRust(VSpace):
    def __init__(self, space_name=None, unwrap=False):
        super().__init__()

@export_flags(MeTTa=True)
class FederatedSpace(VSpace):

    def __init__(self, space_name, unwrap=False):
        super().__init__(space_name, unwrap)

    def _checked_impl(self, method_name, *args):
        if access_error:
            raise Exception(f"Error in FederatedSpace.{method_name}: Implementation for {method_name}({', '.join(map(str, args))}) is not complete.")
        return super()

    def query(self, query_atom):
        return self._checked_impl("query", query_atom).query(query_atom)

    def add(self, atom):
        return self._checked_impl("add", atom).add(atom)

    def remove(self, atom):
        return self._checked_impl("remove", atom).remove(atom)

    def replace(self, from_atom, to_atom):
        return self._checked_impl("replace", from_atom, to_atom).replace(from_atom, to_atom)

    def atom_count(self):
        return self._checked_impl("atom_count").atom_count()

    def atoms_iter(self):
        return self._checked_impl("atoms_iter").atoms_iter()

    def copy(self):
        return self


def self_space_info():
    return ""


from hyperon.atoms import *
from hyperon.ext import register_atoms

access_error = True


@export_flags(MeTTa=False)
def s2m(circles, swip_obj, depth=0):
    r = s2m1(circles, swip_obj, depth)
    if verbose <= 1: return r
    for i in range(depth + 1):
        print("   ", end='')
    print_cmt(f"r({type(r)})={str(r)}/{repr(r)}")
    return r

def s2m1(circles, swip_obj, depth=0):

    if verbose > 1:
        for i in range(depth):
            print("   ", end='')
        print_cmt(f's2m({len(circles)},{type(swip_obj)}): {str(swip_obj)}/{repr(swip_obj)}')

    # Already converted
    if isinstance(swip_obj, (VariableAtom, GroundedAtom, Atom, ExpressionAtom)):
        return swip_obj

    if isinstance(swip_obj, str):
        return S(swip_obj)

    assert isinstance(circles, Circles), f"circles must be an instance of the Circles class not {type(circles)}"

    # Handle numbers and convert them to ValueAtom objects in MeTTa
    if isinstance(swip_obj, (int, float)):
        return ValueAtom(swip_obj)

    #oid = id(swip_obj)

    for n in circles.original_keys():
        v = circles[n]
        if v is swip_obj:
            return n

    var = circles.get(swip_obj, None)
    if var is not None:
        return var


    if isinstance(swip_obj, PySwipAtom):
        return S(str(swip_obj))

    if isinstance(swip_obj, Variable):
        sval = swip_obj.get_value()
        if isinstance(sval, Variable):
            sval = sval.get_value()
        if isinstance(sval, Variable):
            n = swip_obj.chars
            mname = sv2mv(n) if n else "$Var"
            mV = V(mname)
            circles[mname] = swip_obj
            circles[id(mV)] = swip_obj
            circles[swip_obj] = mV
        return s2m(circles, sval)

    if isinstance(swip_obj, Functor):
        # Convert the functor to an expression in MeTTa
        if isinstance(swip_obj.name, PySwipAtom):
            sfn = swip_obj.name.value
        else: sfn = swip_obj.name
        if sfn == "[|]": sfn = "::"
        fn = S(sfn)
        argz = [s2m(circles, arg) for arg in swip_obj.args]
        return E(fn, *argz)

    # Handle PySwip lists
    #if isinstance(swip_obj, list):



    mva = [s2m(circles, item) for item in swip_obj]
    try:
        return E(*mva)
    except TypeError:
        return ExpressionAtom(mva)


    raise ValueError(f"Unknown PySwip object type: {type(swip_obj)} {swip_obj}")

mylist_expr = E()
def sv2mv(s):
    return s.replace("_", "$", 1) if s.startswith("_") else "$" + s


@export_flags(MeTTa=False)
def m2s(circles, metta_obj, depth=0):
    r = m2s1(circles, metta_obj, depth)
    if depth == 0:
        v = swipRef(r)
    else:
        v = r
    if verbose <= 1: return v
    for i in range(depth + 1):
        print("   ", end='')

    print(f"r({type(r)})={r}")
    return v

def swipAtom(m):
    a = PySwipAtom(str(m))
    return a

def swipRef(a):
    if isinstance(a, (Term)):
        return a
    v = Variable()
    v.unify(a)
    return v



def m2s1(circles, metta_obj, depth=0, preferStringToAtom = None, preferListToCompound = False):

    var = circles.get(metta_obj, None)
    if var is not None:
        return var

    metta_obj = unwrap_pyobjs(metta_obj)

    var = circles.get(metta_obj, None)
    if var is not None:
        return var

    if verbose > 1:
        for i in range(depth):
            print("   ", end='')
        print(f'm2s({len(circles)},{type(metta_obj)}): {metta_obj}')

    if isinstance(metta_obj, (Variable, PySwipAtom, Functor, Term)):
        return metta_obj

    if isinstance(metta_obj, str):
        return metta_obj

    if isinstance(metta_obj, bool):
        if metta_obj is True:
            return swipAtom("True")
        else:
            return swipAtom("False")

    elif isinstance(metta_obj, (int, float)):
        return metta_obj

    elif isinstance(metta_obj, OperationObject):
        return m2s1(circles, metta_obj.id, depth + 1)

    elif isinstance(metta_obj, SymbolAtom):
        if preferStringToAtom is None:
            preferStringToAtom = (depth > 0)

        name = metta_obj.get_name();
        #if preferStringToAtom: return name
        return swipAtom(name)

    sV = None

    if isinstance(metta_obj, VariableAtom):
        oid = mv2svn(metta_obj)
        var = circles.get("$" + oid, None)
        # We are in a circluar reference?
        if var is not None:
            #print(f"{oid}={len(circles)}={type(circles)}={type(metta_obj)}")
            return var

        sV = Variable(name = oid)
        circles["$" + oid] = sV
        circles[metta_obj] = sV
        circles[sV] = metta_obj
        return sV

    oid = id(metta_obj)

    preferListToCompound = True
    if isinstance(metta_obj, SpaceRef):
        return swipAtom(getNameBySpace(metta_obj))
        #L = E(S("SpaceRef"),S(getNameBySpace(metta_obj)))
        #L = list_to_termv(L.get_children())
        #L = list_to_termv(circles,metta_obj.get_atoms(),depth+1)
    elif isinstance(metta_obj, list):
        L = list_to_termv(circles, metta_obj, depth + 1)
    elif isinstance(metta_obj, ExpressionAtom):
        L = list_to_termv(circles, metta_obj.get_children(), depth + 1)
    elif isinstance(metta_obj, tuple):
        L = list_to_termv(circles, tuple_to_list(metta_obj), depth + 1)
    else:
        raise ValueError(f"Unknown MeTTa object type_1: {metta_obj} {type(metta_obj)} {dir(metta_obj)}")

    if depth == 0:
        sV = Variable()
        sV.unify(L)
        circles[oid] = sV
        circles[sV] = metta_obj
        circles[metta_obj] = sV
        return sV

    circles[L] = metta_obj
    circles[metta_obj] = L
    return L

def tuple_to_list(t):
    return list(map(tuple_to_list, t)) if isinstance(t, (tuple, list)) else t

# Example usage:
#nested_tuple = (1, 2, (3, 4, (5, 6)), 7)
#converted_list = tuple_to_list(nested_tuple)
#print(converted_list)  # Output will be [1, 2, [3, 4, [5, 6]], 7]

def mv2svn(metta_obj):
    named = metta_obj.get_name().replace('$', '_')
    if len(named) == 0: return "_0"
    s = named[0]
    if(s == '_' or (s.isalpha() and  s.isupper())):
        return named
    else:
        return "_" + named



def m2s3(circles, metta_obj, depth, preferStringToAtom, preferListToCompound):
    for name, value in circles:
        if  name is metta_obj:
            return value

    if isinstance(metta_obj, SpaceRef):
        return swiplist_to_swip(circles, metta_obj.get_atoms(), depth + 1)

    if isinstance(metta_obj, list):
        return swiplist_to_swip(circles, metta_obj)

    if isinstance(metta_obj, ExpressionAtom):
        ch = metta_obj.get_children()
        length = len(ch)
        retargs = []
        if (length == 0):
            return swiplist_to_swip(circles, retargs)


    # for testing
    if preferListToCompound:
        for i in range(0, length):
            retargs.append(m2s(circles, ch[i], depth + 1))
        return swiplist_to_swip(circles, retargs)


    f = m2s1(circles, ch[0], depth + 1, preferStringToAtom = True)

    for i in range(1, length):
        retargs.append(m2s(circles, ch[i], depth + 1))

    # Convert MeTTa list to PySwip list
    if ch[0].get_name() == "::":
        return swiplist_to_swip(circles, retargs)

    # Converting to functor... Maybe a list later on
    return Functor(f, len(retargs), list_to_termv(circles, retargs))

    if verbose > 0: print_cmt(f"Unknown MeTTa object type: {type(metta_obj)}={metta_obj}")

    raise ValueError(f"Unknown MeTTa object type_3: {type(metta_obj)}")

def swiplist_to_swip(circles, retargs, depth=0):
    sv = [m2s1(circles, item, depth) for item in retargs]
    v = Variable()
    v.unify(sv)
    return v

def list_to_termv(circles, retargs, depth=0):
    sv = [m2s1(circles, item, depth) for item in retargs]
    return sv



@export_flags(MeTTa=True)
def sync_space(named):
    ""


import re



@export_flags(MeTTa=True)
def test_custom_m_space():

    class TestSpace(AbstractSpace):

        def __init__(self, unwrap=False):
            super().__init__()
            self.atoms_list = []
            self.unwrap = unwrap

        # NOTE: this is a naive implementation barely good enough to pass the tests
        # Don't take this as a guide to implementing a space query function
        def query(self, query_atom):

            # Extract only the variables from the query atom
            circles = list(filter(lambda atom: atom.get_type() == AtomKind.VARIABLE, query_atom.iterate()))

            # Match the query atom against every atom in the space
            # BindingsSet() creates a binding set with the only matching result
            # We use BindingsSet.empty() to support multiple results
            new_bindings_set = BindingsSet.empty()
            for space_atom in self.atoms_list:
                match_results = space_atom.match_atom(query_atom)

                # Merge in the bindings from this match, after we narrow the match_results to
                # only include variables vars in the query atom
                for bindings in match_results.iterator():
                    bindings.narrow_vars(circles)
                    if not bindings.is_empty():
                        # new_bindings_set.merge_into(bindings) would work with BindingsSet(), but
                        # it would return an empty result for multiple alternatives and merge bindings
                        # for different variables from alternative branches, which would be a funny
                        # modification of query, but with no real use case
                        # new_bindings_set.push(bindings) adds an alternative binding to the binding set
                        new_bindings_set.push(bindings)

            return new_bindings_set

        def add(self, atom):
            self.atoms_list.append(atom)

        def remove(self, atom):
            if atom in self.atoms_list:
                self.atoms_list.remove(atom)
                return True
            else:
                return False

        def replace(self, from_atom, to_atom):
            if from_atom in self.atoms_list:
                self.atoms_list.remove(from_atom)
                self.atoms_list.append(to_atom)
                return True
            else:
                return False

        def atom_count(self):
            return len(self.atoms_list)

        def atoms_iter(self):
            return iter(self.atoms_list)

    test_custom_space(lambda: TestSpace())





class Circles:
    def __init__(self, initial_data=None):
        self.data = {}
        if initial_data:
            for key, value in initial_data.items():
                self.__setitem__(key, value)

    def _get_key(self, key):
        try:
            hash_key = hash(key)
            return ('hash', hash_key)
        except TypeError:
            id_key = id(key)
            return ('id', id_key)

    def __getitem__(self, key):
        key_type, key_value = self._get_key(key)
        return self.data[(key_type, key_value)][1]

    def __setitem__(self, key, value):
        key_type, key_value = self._get_key(key)
        self.data[(key_type, key_value)] = (key, value)

    def __delitem__(self, key):
        key_type, key_value = self._get_key(key)
        del self.data[(key_type, key_value)]

    def __contains__(self, key):
        key_type, key_value = self._get_key(key)
        return (key_type, key_value) in self.data

    def __len__(self):
        return len(self.data)

    def __iter__(self):
        for key_tuple in self.data.keys():
            yield key_tuple

    def original_keys(self):
        for key, _ in self.data.values():
            yield key

    def get(self, key, default=None):
        key_type, key_value = self._get_key(key)
        if (key_type, key_value) in self.data:
            return self.data[(key_type, key_value)][1]
        else:
            return default

    def items(self):
        return [(key, value) for key, value in self.data.values()]

    def keys(self):
        return [key for key, _ in self.data.values()]

    def values(self):
        return [value for _, value in self.data.values()]

    def clear(self):
        self.data.clear()

    def pop(self, key, default=None):
        key_type, key_value = self._get_key(key)
        return self.data.pop((key_type, key_value), (None, default))[1]

    def popitem(self):
        _, (key, value) = self.data.popitem()
        return (key, value)

    def setdefault(self, key, default=None):
        key_type, key_value = self._get_key(key)
        return self.data.setdefault((key_type, key_value), (key, default))[1]

    def update(self, other):
        for key, value in other.items():
            self.__setitem__(key, value)






@export_flags(MeTTa=True)
def test_custom_v_space():
    #test_custom_space(lambda: (lambda vs: vs.incrHome() and vs)(VSpace()))
    test_custom_v_space1()
    test_custom_v_space2()

@export_flags(MeTTa=True)
def test_custom_v_space1():
    test_custom_space(lambda: VSpace())

@export_flags(MeTTa=True)
def test_custom_v_space2():
    test_custom_space(lambda: the_nb_space)

    #test_custom_space(lambda: the_new_runner_space)

def test_custom_space(LambdaSpaceFn):

    def passTest(msg):
        print(f"Pass Test:({msg})")

    def failTest(msg):
        print(f"raise AssertionError({msg})")
        #raise AssertionError(msg)

    def self_assertEqualNoOrder(list1, list2, msg=None):
        """
        Asserts that two lists are equal, regardless of their order.
        """
        def py_sorted(n):

            class MyIterable:
                def __init__(self, data):
                    self.data = data
                    self.index = 0

                def __iter__(self):
                    return self

                def __next__(self):
                    if self.index < len(self.data):
                        result = self.data[self.index]
                        self.index += 1
                        return result
                    raise StopIteration

            try:
                if isinstance(n, ExpressionAtom):
                    return py_sorted(n.get_children())
                return sorted(n)
            except TypeError:
                def custom_sort(item):
                    try:
                        if isinstance(item, (int, float)):
                            return (0, item)
                        elif isinstance(item, ExpressionAtom):
                            return py_sorted(item.get_children())
                        else:
                            return (1, str(item))
                    except TypeError:
                        return (1, str(item))

            try: return sorted(n, key=custom_sort)
            except TypeError: n # return sorted(MyIterable(n), key=custom_sort)


        if py_sorted(list1) != py_sorted(list2):
            failTest(msg or f"Lists differ: {list1} != {list2}")
        else: passTest(msg or f" {list1} == {list2} ")

    def self_assertTrue(expr, msg=None):
        """
        Asserts that an expression is true.
        """
        if not expr:
            failTest(msg or f"Expression is not true: {expr}")
        else: passTest(msg or f"Expression is true: {expr}")

    def self_assertFalse(expr, msg=None):
        """
        Asserts that an expression is false.
        """
        if expr:
            failTest(msg or f"Expression is not false: {expr}")
        else: passTest(msg or f"Expression is false: {expr}")

    def self_assertEqual(val1, val2, msg=None):
        """
        Asserts that two values are equal.
        """
        if val1 != val2:
            failTest(msg or f"Values differ: {val1} != {val2}")
        else: passTest(msg or f"Values same: {val1} == {val2}")


    print(f"test_custom_space--------------------------------------------:({LambdaSpaceFn})------------------------------------------")



    test_space = LambdaSpaceFn()
    test_space.test_attrib = "Test Space Payload Attrib"

    kb = asSpaceRef(test_space)


    kb.add_atom(S("a"))
    kb.add_atom(S("b"))
    #kb.add_atom(E(S("a"),S("b")))

    self_assertEqual(kb.atom_count(), 2)
    self_assertEqual(kb.get_payload().test_attrib, "Test Space Payload Attrib")
    self_assertEqualNoOrder(kb.get_atoms(), [S("a"), S("b")])

    kb = asSpaceRef(LambdaSpaceFn())
    kb.add_atom(S("a"))
    kb.add_atom(S("b"))
    kb.add_atom(S("c"))

    self_assertTrue(kb.remove_atom(S("b")), "remove_atom on a present atom should return true")
    self_assertFalse(kb.remove_atom(S("bogus")), "remove_atom on a missing atom should return false")
    self_assertEqualNoOrder(kb.get_atoms(), [S("a"), S("c")])

    kb = asSpaceRef(LambdaSpaceFn())
    kb.add_atom(S("a"))
    kb.add_atom(S("b"))
    kb.add_atom(S("c"))

    self_assertTrue(kb.replace_atom(S("b"), S("d")))
    self_assertEqualNoOrder(kb.get_atoms(), [S("a"), S("d"), S("c")])

    kb = asSpaceRef(LambdaSpaceFn())
    kb.add_atom(E(S("A"), S("B")))
    kb.add_atom(E(S("C"), S("D")))
    # Checking that multiple matches can be returned
    kb.add_atom(E(S("A"), S("E")))

    result = kb.query(E(S("A"), V("XX")))
    self_assertEqualNoOrder(result, [{"XX": S("B")}, {"XX": S("E")}])

    m = MeTTaLog()

    # Make a little space and add it to the MeTTa interpreter's space
    little_space = asSpaceRef(LambdaSpaceFn())
    little_space.add_atom(E(S("A"), S("B")))
    space_atom = G(little_space)
    m.space().add_atom(E(S("little-space"), space_atom))

    # Make sure we can get the little space back, and then query it
    kb_result = m.space().query(E(S("little-space"), V("s")))
    result_atom = kb_result[0].get("s")
    self_assertEqual(result_atom, space_atom)

    result = result_atom.get_object().query(E(S("A"), V("v")))
    self_assertEqualNoOrder(result, [{"v": S("B")}])

    # Add the MeTTa space to the little space for some space recursion
    if verbose > 1: print_cmt("mspace")
    mspace = m.space()
    gmspace = G(mspace)
    A = E(S("big-space"), gmspace)
    if verbose > 1: print_cmt("little_space.add_atom")
    little_space.add_atom(A)
    if verbose > 1: print_cmt("Next Space")
    nested = asSpaceRef(LambdaSpaceFn())
    nested.add_atom(E(S("A"), S("B")))
    space_atom = G(nested)

    runner = MeTTaLog()
    runner.space().add_atom(space_atom)
    runner.tokenizer().register_token("nested", lambda token: space_atom)

    result = runner.run("!(match nested (A $x1) $x1)")
    self_assertEqual([[S("B")]], result)
    print(f"test_custom_space--------------------------------------------:({LambdaSpaceFn})------------------------------------------")



print_l_cmt(2, f";; ...did {__file__}...{__package__} name={__name__}")
