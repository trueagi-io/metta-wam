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

import os
from importlib import import_module
import importlib.util
import sys
import hyperonpy as hp
from hyperonpy import EnvBuilder, ModuleId
from hyperon.atoms import Atom, AtomType, OperationAtom
from hyperon.base import GroundingSpaceRef, Tokenizer, SExprParser
from hyperon.runner import _PyFileMeTTaModFmt

from hyperonpy import *
from hyperon.atoms import *
from hyperon.base import *
from hyperon.runner import *


def color(t, c):
    cmap = [90, 91, 31, 93, 92, 32, 36, 96, 94, 34, 35, 95, 38]
    return f"\033[{cmap[c % len(cmap)]}m{t}\033[0m"


def oblique(t):
    return f"\033[3m{t}\033[0m"


def underline(t):
    return f"\033[4m{t}\033[0m"


def expr_vars(expr):
    if isinstance(expr, SymbolAtom):
        return []
    elif isinstance(expr, VariableAtom):
        return [str(expr)]
    elif isinstance(expr, ExpressionAtom):
        return [e for c in expr.get_children() for e in expr_vars(c)]
    elif isinstance(expr, GroundedAtom):
        return []
    else:
        raise Exception("Unexpected sexpr type: " + str(type(expr)))


def color_expr(expr, level=0, unif_vars=None):
    name = str(expr)
    if level == 0:
        unif_vars = frozenset(e for e, c in Counter(expr_vars(expr)).items() if c > 1) \
            if unif_vars is None else frozenset()
    if isinstance(expr, SymbolAtom):
        return name
    elif isinstance(expr, VariableAtom):
        return oblique(name) if name in unif_vars else name
    elif isinstance(expr, ExpressionAtom):
        return (color("(", level) +
                " ".join(color_expr(c, level + 1, unif_vars) for c in expr.get_children()) +
                color(")", level))
    elif isinstance(expr, GroundedAtom):
        return underline(name)
    else:
        raise Exception("Unexpected sexpr type: " + str(type(expr)))

from mettalog import *
def print_cmt(*args, prefix=";; "):
    
    for arg in args:
        println_impl(arg, prefix=prefix)
        flush_console()


def get_sexpr_input(prmpt):
    expr, inside_quotes, prev_char = "", False, None

    while True:
        line = input(prmpt)
        flush_console()
        for char in line:
            if char == '"' and prev_char != '\\':
                inside_quotes = not inside_quotes
            expr += char
            prev_char = char

        if not inside_quotes and expr.count("(") == expr.count(")"):
            break
        prmpt = "continue...>>> "
        expr += " "

    return expr




# Readline Imports (Platform Specific)
try: import readline
except ImportError: import pyreadline3 as readline

histfile = os.path.join(os.path.expanduser("~"), ".metta_history")

try:
    readline.set_history_length(300)
    readline.read_history_file(histfile)
    readline.set_history_length(300)
    h_len = readline.get_current_history_length()
except FileNotFoundError:
    open(histfile, 'wb').close()
    h_len = 0

def add_to_history_if_unique(item, position_from_last=1):
    for i in range(1, readline.get_current_history_length() + 1):
        if readline.get_history_item(i) == item: return
    insert_to_history(item, position_from_last)

def insert_to_history(item, position_from_last=5):
    hist = [readline.get_history_item(i) for i in range(1, readline.get_current_history_length() + 1)]
    # Remove the item from the list if it already exists before the insertion position
    insertion_position = max(0, len(hist) - position_from_last)
    if item in hist[:insertion_position]:
        hist.remove(item)
    # Insert the item at the desired position
    hist.insert(insertion_position, item)
    # Clear and repopulate the history
    readline.clear_history()
    for h in hist:
        readline.add_history(h)

def readline_add_history(t):
    readline.add_history(t)

def insertDefaultHistory():
    insert_to_history('!(get-by-key &my-dict "A")')
    insert_to_history("@metta !")
    insert_to_history("!(mine-overlaps)")
    insert_to_history("!(try-overlaps)")
    insert_to_history("!(load-flybase-full)")
    insert_to_history("!(load-flybase-tiny)")
    #insert_to_history("!(load-vspace)")
    insert_to_history("!(learn-vspace)")
    insert_to_history('!(match &parent (gene_map_table $Dmel $abo $G  $C $D $E)  (gene_map_table $Dmel $abo $G  $C $D $E) )')
    insert_to_history('!(match &parent (gene_map_table $Dmel $abo (GeneValueNode "FBgn0000018") $C $D $E) (gene_map_table $Dmel $abo (GeneValueNode "FBgn0000018") $C $D $E))')
    insert_to_history('!(add-atom &parent (gene_map_table (ConceptNode "Dmel") (ConceptNode "abo") (GeneValueNode "FBgn0000018") (ConceptNode "2-44") (ConceptNode "32C1-32C1") (StringValue "2L:10973443..10975293(-1)")))')
    insert_to_history("!(match &parent $ $)")
    insert_to_history("!(match &flybase $ $)")
    insert_to_history('!(match &flybase (gene_map_table $Dmel $abo $G  $C $D $E)  (gene_map_table $Dmel $abo $G  $C $D $E) )')
    insert_to_history('!(match &flybase (gene_map_table $Dmel $abo (GeneValueNode "FBgn0000018") $C $D $E) (gene_map_table $Dmel $abo (GeneValueNode "FBgn0000018") $C $D $E))')
    insert_to_history('!(add-atom &flybase (gene_map_table (ConceptNode "Dmel") (ConceptNode "abo") (GeneValueNode "FBgn0000018") (ConceptNode "2-44") (ConceptNode "32C1-32C1") (StringValue "2L:10973443..10975293(-1)")))')
    insert_to_history('!(match &flybase (gene_map_table $Dmel $abo FBgn0000018 $C $D $E) (gene_map_table $Dmel $abo FBgn0000018 $C $D $E))')
    insert_to_history('!(test_custom_v_space)', position_from_last=1)
    insert_to_history('!(import! &self mettalog)', position_from_last=1)
    insert_to_history('!(import! &self motto)', position_from_last=1)

OPTIONS = ['apple', 'banana', 'cherry', 'date', 'elderberry']

# The completer function
def completer(text, state):
    options = [i for i in OPTIONS if i.startswith(text)]
    if state < len(options):
        return options[state]
    else:
        return None

# Register the completer function
readline.set_completer(completer)

# Use the tab key for completion
readline.parse_and_bind('tab: complete')

def save(prev_h_len, histfile):
    new_h_len = readline.get_current_history_length()
    readline.set_history_length(400)
    readline.append_history_file(new_h_len - prev_h_len, histfile)
atexit.register(save, h_len, histfile)


argmode = "metta"

def split_or_none(s, delimiter):
    parts = s.split(delimiter, 1)  # split only at the first occurrence
    return parts[0], (parts[1] if len(parts) > 1 else None)

def repl_loop_impl(theMeTTa=None, get_sexpr_input=get_sexpr_input, print_cmt=print_cmt, mode="metta"):

        verbose = 2

        if theMeTTa is None:
            theMeTTa = get_metta()

        submode = "+"  # default `submode` is to add information
        the_new_runner_space = theMeTTa.space()

        def maybe_submode(line):
            nonlocal submode
            lastchar = line[-1]
            if "+-?!^".find(lastchar)>=0:
                submode=lastchar

        def parse_single(s):
            return theMeTTa.parse_single(s)

        def the_running_metta_space():
            return the_new_runner_space

        selected_space_name = "&self"


        #from mettalog import selected_space_name,the_running_metta_space,the_new_runner_space
        
        #global selected_space_name


        #history = []
        load_vspace()

        while True:
            try:
                flush_console()
                # Use the input function to get user input
                prmpt = f"; {mode}@{selected_space_name} {submode}> "

                line = get_sexpr_input(prmpt)

                #print_cmt(f"You entered: {line}\n")

                if line:
                    sline = line.lstrip().rstrip()
                    add_to_history_if_unique(line, position_from_last=1)
                else:
                    continue

                if len(sline) == 1:
                    if "+-?!^".find(sline) >= 0:
                        maybe_submode(sline)
                        continue

                if sline.endswith(".") and not sline.startswith(";") and not sline.startswith("%"):
                    swip_exec(line)
                    continue


                if not line.startswith(" "):
                    line = " " + line

                if sline.rstrip() == '?':
                    expr = parse_single("(match &self $ $)")
                    yield expr, interpret(the_running_metta_space(), expr)
                    continue

                # Check for history commands
                if sline.rstrip() == '.h':
                    for idx, item in enumerate(history):
                        print_cmt(f"{idx + 1}: {item}")
                    continue

                # Switch to python mode
                elif sline.startswith("@p"):
                    mode = "python"
                    print_cmt("Switched to Python mode.")
                    maybe_submode(line.rstrip())
                    add_to_history_if_unique("@swip")
                    add_to_history_if_unique("@metta")
                    continue

                elif sline.startswith("@space"):
                    cmd_, named = split_or_none(sline, " ")
                    if named is None:
                        print_cmt("@spaces: " + " ".join(space_refs))
                        shownAlready = {}
                        for n in space_refs:
                            v = space_refs[n]
                            if v:
                                s = v()
                                if s:
                                    print_cmt(f"==============================================================")
                                    was = shownAlready.get(id(s))
                                    if was:
                                        print_cmt(f"ALREADY {n} SHOWN as {was}")
                                        continue
                                    n = f"Name: {n}"
                                    shownAlready[id(s)] = n
                                    print_cmt(n)
                                    print_cmt(s)

                        print_cmt(f"==============================================================")

                    else:
                        found = getSpaceByName(named)
                        if found is not None:
                            selected_space_name = named
                            the_new_runner_space = found
                            print_cmt(f"switching to {named}")
                        else:
                            print_cmt(f"Space not found: '{named}'")
                            print_cmt("try:" + " ".join(space_refs))
                    continue

                # Switch to MeTTaLog mode
                elif sline.startswith("@sm") or sline.startswith("@mettal") or sline.startswith("@ml"):
                    mode = "mettalog"
                    print_cmt("Switched to MeTTaLog mode.")
                    continue

                # Switch to swip mode
                elif sline.startswith("@s"):
                    mode = "swip"
                    print_cmt("Switched to Swip mode.")
                    maybe_submode(line.rstrip())
                    add_to_history_if_unique("break")
                    add_to_history_if_unique("listing(maybe_corisponds/2)")
                    add_to_history_if_unique("synth_query(4,Query)")
                    continue

                # Switch to metta mode
                elif sline.startswith("@m"):
                    mode = "metta"
                    print_cmt("Switched to MeTTa mode.")
                    maybe_submode(line.rstrip())
                    add_to_history_if_unique("!(match &self $ $)")
                    continue

                elif sline.startswith("@v"):
                    verbose = int(sline.split()[1])
                    os.environ["METTALOG_VERBOSE"] = str(verbose)
                    print_cmt(f"Verbosity level set to {verbose}")
                    continue

                elif sline.startswith("@a"): # @arg
                    argmode = mode
                    arg = sline.split()[1]
                    handle_arg(arg)
                    mode = argmode
                    continue

                elif sline.startswith("@l"): # @load
                    argmode = mode
                    arg = sline.split()[1]
                    handle_arg(arg)
                    mode = argmode
                    continue

                # Show help
                elif sline.startswith("@h"):
                    print_cmt("Help:")
                    print_cmt("@m     - Switch to MeTTa mode")
                    print_cmt("@m +   -   changes to: Add bare atoms (default)")
                    print_cmt("@m !   -         Interpret bare atoms")
                    print_cmt("@m -   -         Remove bare atoms")
                    #print_cmt("@m ?   -         Query bare atoms")
                    print_cmt("@m ^   - Interpret atoms as if there are in files (+)")
                    print_cmt("@p     - Switch to Python mode")
                    print_cmt("@s     - Switch to Swip mode")
                    print_cmt("@sm,ml   - Switch to MeTTaLog mode")
                    print_cmt("@space   - Change the &self of the_runner_space")
                    print_cmt("@v ###   - Verbosity 0-3")
                    print_cmt("@h     - Display this help message")
                    print_cmt("@arg   - Act as if this arg was passed to the command")
                    print_cmt("       example: '@arg 1-VSpaceTest.metta'  loads and runs this metta file")
                    print_cmt("Ctrl-D   - Exit interpreter")
                    print_cmt(".s     - Save session")
                    print_cmt(".l     - Load the latest session")
                    print_cmt(".q     - Quit the session")
                    print_cmt(".h     - Display command history")
                    print_cmt("\nFrom your shell you can use..")
                    print_cmt("\texport METTALOG_VERBOSE=2")
                    flush_console()
                    continue

                prefix = sline[0]

                if mode == "swip":
                    if prefix == "%":
                        print_cmt(line) # comment
                        continue
                    else:
                        swip_exec(line)
                        continue

                elif mode == "mettalog":
                    if prefix == ";":
                        print_cmt(line) # comment
                        continue
                    else:

                        if "+-?!^".find(prefix) < 0:
                            prefix = submode
                            line = sline
                        else:
                            line = line[2:].strip()

                        if prefix == '!':
                            expr = parse_single(line)
                            expr = E(S("!"), expr)
                        else:
                            expr = parse_single(line)

                        if verbose > 1: print_cmt(f"% S-Expr {line}")
                        if verbose > 1: print_cmt(f"% M-Expr {expr}")
                        circles = Circles()
                        swipl_fid = PL_open_foreign_frame()
                        t0 = monotonic_ns()
                        try:
                            swip_obj = m2s(circles, expr);
                            if verbose > 1: print_cmt(f"% P-Expr {swip_obj}")
                            call_sexpr = Functor("call_sexpr", 5)
                            user = newModule("user")
                            X = Variable()
                            try:
                                print("mettalog...")
                                q = PySwipQ(call_sexpr(prefix, selected_space_name, str(line), swip_obj, X))
                                while q.nextSolution():
                                    print("mettalog...sol")
                                    flush_console()
                                    yield expr, s2m1(circles, X.value)
                            finally:
                                q.closeQuery()
                                flush_console()
                                continue
                        finally:
                            if verbose > 0: timeFrom("MeTTaLog", t0)
                            PL_discard_foreign_frame(swipl_fid)
                    continue

                elif mode == "python":
                    if prefix == "#":
                        print_cmt(line) # comment
                        continue
                    try:
                        t0 = monotonic_ns()
                        result = eval(line)
                        println(result)
                        continue
                    finally:
                        if verbose > 0: timeFrom("python", t0)
                elif mode == "metta":
                    try:
                        t0 = monotonic_ns()
                        rest = line[2:].strip()
                        if prefix == ";":
                            print_cmt(line) # comment
                            continue
                        elif sline.startswith(".s"):
                            name = f"session_{round(time())}.mettar" if rest == "" else (
                                rest if rest.endswith("mettar") else rest + ".mettar")
                            with open(os.sep.join(cwd + name), 'w') as f:
                                f.writelines(history)
                            continue
                        elif sline.startswith(".l"):
                            name = max(glob("session_*.mettar")) if rest == "" else (
                                rest if rest.endswith("mettar") else rest + ".mettar")
                            lazy_import_file(name)
                            continue
                        elif sline.startswith(".q"):
                            break

                        if "+-?!^".find(prefix) < 0:
                            prefix = submode
                            rest = line

                        #print_cmt(f"submode={submode} rest={rest} ")

                        if prefix == "!":
                            expr = parse_single(rest)
                            yield expr, interpret(the_running_metta_space(), expr)
                            continue
                        elif prefix == "?":
                            expr = parse_single(rest)
                            yield expr, the_running_metta_space().subst(expr, expr)
                            continue
                        elif prefix == "+":
                            expr = parse_single(rest)
                            println(the_running_metta_space().add_atom(expr))
                            continue
                        elif prefix == "-":
                            expr = parse_single(rest)
                            println(the_running_metta_space().remove_atom(expr))
                            continue
                        elif prefix == "^":
                            println(theMeTTa.run(line));
                            continue
                        else:
                            expr = parse_single(rest)
                            yield expr, interpret(the_running_metta_space(), expr)
                            continue
                    finally:
                        if verbose > 0: timeFrom("MeTTa", t0)

            except EOFError:
                sys.stderr = sys.__stderr__
                if verbose > 0: print_cmt("\nCtrl^D EOF...")
                flush_console()
                return [True] #sys.exit(0)

            except KeyboardInterrupt as e:
                if verbose > 0: print_cmt(f"\nCtrl+C: {e}")
                if verbose > 0:
                    buf = io.StringIO()
                    sys.stderr = buf
                    traceback.print_exc()
                    sys.stderr = sys.__stderr__
                    print_cmt(buf.getvalue().replace('rolog', 'ySwip'))
                #sys.exit(3)
                continue

            except Exception as e:
                if verbose > 0: print_cmt(f"Error: {e}")
                if verbose > 0:
                    buf = io.StringIO()
                    sys.stderr = buf
                    traceback.print_exc()
                    sys.stderr = sys.__stderr__
                    print_cmt(buf.getvalue().replace('rolog', 'ySwip'))
                continue

def repl(theMeTTa=None, get_sexpr_input=get_sexpr_input, print_cmt=print_cmt, mode="metta"):
    #load_vspace()
    argmode = mode
    if argmode is None:
        argmode = "metta"
    for i, (expr, result_set) in enumerate(repl_loop_impl(theMeTTa,get_sexpr_input=get_sexpr_input, 
                                                          print_cmt=print_cmt, mode=argmode)):
        if result_set:
            try:
                for result in result_set:
                    print_cmt(color_expr(result))
                    flush_console()
            except TypeError:
                print_cmt(color_expr(result_set))
                flush_console()
        else:
            print_cmt(f"[/]")
            flush_console()

print_l_cmt(2, f";; ...did {__file__}...{__package__} name={__name__}")
