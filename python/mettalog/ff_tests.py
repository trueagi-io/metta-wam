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
from typing_extensions import *
from typing import get_type_hints
from collections import Counter
from glob import glob
from time import monotonic_ns, time
import traceback

from mettalog import *
print_l_cmt(2, f";; ...doing {__file__}...{__package__} name={__name__}")
from mettalog.repl_loop import add_to_history_if_unique, repl


def reg_pyswip_foreign():

    #current_module = sys.modules["__main__"]
    try: 
        ""
        current_module = sys.modules[__name__]
        add_janus_methods(current_module, dict = oper_dict)
    except Exception as e: ""

    redirect_stdout(test_nondeterministic_foreign)


    def py_eval(e, result):
        return res_unify(result, eval(str(e)))
    py_eval.arity = 2
    registerForeign(py_eval)

    # Register the foreign functions in PySwip
    #registerForeign(new_rust_space, arity=1)
    #registerForeign(query_from_space, arity=3)
    #registerForeign(add_to_space, arity=2)
    #registerForeign(remove_from_space, arity=2)
    #registerForeign(replace_in_space, arity=3)
    #registerForeign(atom_count_from_space, arity=2)
    #registerForeign(atoms_iter_from_space, arity=2)
    #registerForeign(get_atoms_from_space, arity=2)
    add_to_history_if_unique.arity = 1
    registerForeign(add_to_history_if_unique)



    #?- query_from_space('example', 'my_atom', Result).
    #?- add_to_space('example', 'new_atom').
    #?- remove_from_space('example', 'some_atom').
    #?- replace_in_space('example', 'old_atom', 'new_atom').
    #?- atom_count_from_space('example', Count).
    #?- atoms_iter_from_space('example', Atoms).


@export_flags(MeTTa=True, Janus=True)
def test_nondeterministic_foreign1():

    def nondet(a, context):
        control = PL_foreign_control(context)
        context = PL_foreign_context(context)
        if control == PL_FIRST_CALL:
            context = 0
            a.unify(int(context))
            context += 1
            return PL_retry(context)
        elif control == PL_REDO:
            a.unify(int(context))
            if context == 10:
                return False
            context += 1
            return PL_retry(context)
        elif control == PL_PRUNED:
            pass


    nondet.arity = 1
    registerForeign(nondet, flags=PL_FA_NONDETERMINISTIC)
    result = list(swip.query("nondet(X)"))

    print_cmt(result)

    if len(result) != 10:
        print_cmt('Query should return 10 results')

    for i in range(10):
        if {'X': i} not in result:
            print_cmt('Expected result X:{} not present'.format(i))


@export_flags(MeTTa=True, Janus=True)
def test_nondeterministic_foreign2():

    def number_generator():
        for i in range(10):
            yield i

    iterator = number_generator()

    def nondet2(a, context):
        control = PL_foreign_control(context)
        context = PL_foreign_context(context)
        #global iterator  # Use the global iterator object

        if control == PL_FIRST_CALL:
            try:
                value = next(iterator)  # Start the iterator
                a.unify(int(value) + 1)  # Add 1 to yield numbers from 1 to 10
                return PL_retry(context)
            except StopIteration:
                return False
        elif control == PL_REDO:
            try:
                value = next(iterator)
                a.unify(int(value) + 1)  # Add 1 to yield numbers from 1 to 10
                return PL_retry(context)
            except StopIteration:
                return False
        elif control == PL_PRUNED:
            pass

    nondet2.arity = 1
    registerForeign(nondet2, flags=PL_FA_NONDETERMINISTIC)
    result = list(swip.query("nondet2(X)"))

    print_cmt(result)

    if len(result) != 10:
        print_cmt('Query should return 10 results')

idKey = 1
@export_flags(MeTTa=True, Janus=True)
def test_nondeterministic_foreign3():

    def number_generator(size):
        for i in range(size):
            yield i

    context_iterators = {}  # Dictionary to store iterators by context

    def nondet3(sz, a, context):
        global idKey
        control = PL_foreign_control(context)
        context = PL_foreign_context(context)
        id = context

        if control == PL_FIRST_CALL:
            id = idKey
            idKey = idKey + 1
            iterator = number_generator(sz)  # Create a new iterator
            context_iterators[id] = iterator  # Store it in the dictionary
            try:
                value = next(iterator)
                a.unify(int(value) + 1)
                context = id
                return PL_retry(context)
            except StopIteration:
                del context_iterators[id]  # Clean up
                return False

        elif control == PL_REDO:
            iterator = context_iterators.get(id)
            if iterator is not None:
                try:
                    value = next(iterator)
                    a.unify(int(value) + 1)
                    return PL_retry(context)
                except StopIteration:
                    del context_iterators[id]  # Clean up
                    return False
            pass

        elif control == PL_PRUNED:
            # Clean up the iterator when we're done
            if id in context_iterators:
                del context_iterators[id]
            pass

    nondet3.arity = 2
    registerForeign(nondet3, arity=2, flags=PL_FA_NONDETERMINISTIC)
    result = list(swip.query("nondet3(4,X)"))

    print_cmt(result)

    if len(result) != 4:
        print_cmt('nondet3 should return 4 results')


@export_flags(MeTTa=True, Janus=True)
def test_nondeterministic_foreign():

    test_nondeterministic_foreign1()
    test_nondeterministic_foreign2()
    test_nondeterministic_foreign3()


    def hello(t):
        print_cmt("Hello,", t)

    hello.arity = 1

    registerForeign(hello, arity=1)


    def hello1(t):
        readline.replace_history_item(0, t)
        print_cmt("Hello1,", t)


    hello1.arity = 1

    registerForeign(hello1, arity=1)




    swip.assertz("father(michael,john)")
    swip.assertz("father(michael,gina)")

    result = list(swip.query("father(michael,X), hello(X)"))

    print_cmt(result)

    if len(result) != 2:
        print_cmt('Query should return two results')
    for name in ('john', 'gina'):
        if {'X': name} not in result:
            print_cmt('Expected result X:{} not present'.format(name))


    #def test_atoms_and_strings_distinction(self):
    test_string = "string"

    def get_str(string):
        string.value = test_string

    def test_for_string(string, test_result):
        test_result.value = (test_string == string.decode('utf-8'))

    get_str.arity = 1
    test_for_string.arity = 2

    registerForeign(get_str)
    registerForeign(test_for_string)

    result = list(swip.query("get_str(String), test_for_string(String, Result)"))

    print_cmt(result)

    if result[0]['Result'] != 'true':
        print_cmt('A string return value should not be converted to an atom.')

    print_cmt()
    print_cmt()
    print_cmt()
    flush_console()


@export_flags(MeTTa=True)
def metta_to_swip_tests1():
    # Register the methods as foreign predicates
    registerForeign(swip_to_metta_wrapper, arity=2)
    registerForeign(metta_to_swip_wrapper, arity=2)
    circles = Circles()
    # Usage:
    swip_functor = Functor(PySwipAtom("example"), 2, [PySwipAtom("sub1"), 3.14])
    print_cmt(f"swip_functor={swip_functor}"),
    metta_expr = s2m(circles, swip_functor)
    print_cmt(f"metta_expr={metta_expr}"),
    converted_back_to_swip = m2s(circles, metta_expr)
    print_cmt(f"converted_back_to_swip={converted_back_to_swip}"),


    # Now you can use the methods in PySwip queries
    print_cmt(list(swip.query("swip_to_metta_wrapper('example', X).")))
    print_cmt(list(swip.query("metta_to_swip_wrapper(X, 'example').")))

@export_flags(MeTTa=True)
def metta_to_swip_tests2():
    # Register the methods as foreign predicates
    registerForeign(swip_to_metta_wrapper, arity=2)
    registerForeign(metta_to_swip_wrapper, arity=2)

    circles = Circles()
    # Now you can use the methods in PySwip queries
    println(list(swip.query("swip_to_metta_wrapper('example', X).")))
    println(list(swip.query("metta_to_swip_wrapper(X, 'example').")))

    # Usage:
    swip_list = ["a", "b", 3]
    metta_expr = s2m(circles, swip_list)
    converted_back_to_swip = m2s(circles, metta_expr)
    swip_functor = Functor(PySwipAtom("example"), 2, [PySwipAtom("sub1"), 3.14])
    metta_expr = s2m(circles, swip_functor)
    converted_back_to_swip = m2s(circles, metta_expr)

print_l_cmt(2, f";; ...did {__file__}...{__package__} name={__name__}")
