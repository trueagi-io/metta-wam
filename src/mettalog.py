from time import monotonic_ns, time
import atexit
import os
import re
import readline
import sys
import inspect
from collections import Counter
from glob import glob
import hyperonpy as hp
from hyperon.atoms import V, S, E, ValueAtom, GroundedAtom, ExpressionAtom, G, AtomType, MatchableObject, OperationAtom, OperationObject, BindingsSet, Atom
from hyperon.runner import MeTTa
from hyperon.ext import register_atoms, register_tokens
from hyperon.base import AbstractSpace, SpaceRef
from hyperon import *
import openai
try:
 openai.api_key = os.environ["OPENAI_API_KEY"]
except KeyError:
 ""

histfile = os.path.join(os.path.expanduser("~"), ".metta_history")
is_init = True

verbose = 1

try:
    readline.set_history_length(10000)
    readline.read_history_file(histfile)
    h_len = readline.get_current_history_length()
except FileNotFoundError:
    open(histfile, 'wb').close()
    h_len = 0

def save(prev_h_len, histfile):
    new_h_len = readline.get_current_history_length()
    readline.set_history_length(10000)
    readline.append_history_file(new_h_len - prev_h_len, histfile)
atexit.register(save, h_len, histfile)


import numpy as np
class VSNumpyValue(MatchableObject):

    def __eq__(self, other):
        return isinstance(other, VSNumpyValue) and\
               (self.content.shape == other.content.shape) and\
               (self.content == other.content).all()

    def match_(self, other):
        sh = self.content.shape
        bindings = {}
        if isinstance(other, GroundedAtom):
            other = other.get_object()
        # Match by equality with another VSNumpyValue
        if isinstance(other, VSNumpyValue):
            return [{}] if other == self else []
        # if isinstance(other, VSPatternValue):
        #     other = other.to_expr()
        if isinstance(other, ExpressionAtom):
            ch = other.get_children()
            # TODO: constructors and operations
            if len(ch) != sh[0]:
                return []
            for i in range(len(ch)):
                res = self.content[i]
                typ = _np_atom_type(res)
                res = VSNumpyValue(res)
                if isinstance(ch[i], VariableAtom):
                    bindings[ch[i].get_name()] = G(res, typ)
                elif isinstance(ch[i], ExpressionAtom):
                    bind_add = res.match_(ch[i])
                    if bind_add == []:
                        return []
                    bindings.update(bind_add[0])
        return [] if len(bindings) == 0 else [bindings]


class VSPatternValue(MatchableObject):

    def match_(self, other):
        if isinstance(other, GroundedAtom):
            other = other.get_object().content
        if not isinstance(other, VSPatternValue):
            return other.match_(self)
        # TODO: match to patterns
        return []


class VSPatternOperation(OperationObject):

    def __init__(self, name, op, unwrap=False, rec=False):
        super().__init__(name, op, unwrap)
        self.rec = rec

    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        if self.rec:
            args = args[0].get_children()
            args = [self.execute(arg)[0]\
                if isinstance(arg, ExpressionAtom) else arg for arg in args]
        # If there is a variable or VSPatternValue in arguments, create VSPatternValue
        # instead of executing the operation
        for arg in args:
            if isinstance(arg, GroundedAtom) and\
               isinstance(arg.get_object(), VSPatternValue) or\
               isinstance(arg, VariableAtom):
                return [G(VSPatternValue([self, args]))]
        return super().execute(*args, res_typ=res_typ)


def _np_atom_type(npobj):
    return E(S('NPArray'), E(*[ValueAtom(s, 'Number') for s in npobj.shape]))

def wrapnpop(func):
    def wrapper(*args):
        a = [arg.get_object().value for arg in args]
        res = func(*a)
        typ = _np_atom_type(res)
        return [G(VSNumpyValue(res), typ)]
    return wrapper


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


def export_to_metta(func):
    setattr(func, 'export_to_metta', True)
    #print(f"{func}={getattr(func, 'export_to_metta', False)}")
    return func

def export_to_pyswip(func):
    setattr(func, 'export_to_pyswip', True)
    #print(func)
    return func

@export_to_metta
def printl(obj):
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
            except Error:
                print(item)
    except TypeError:
        # If a TypeError is raised, the object is not iterable
        # if verbose>0: print(type(obj))
        print(obj)
    return obj

@export_to_metta
def printp(obj):
    print(obj)
    return obj


def get_sexpr_input(prmpt):
    expr, inside_quotes, prev_char = "", False, None

    while True:
        line = input(prmpt)
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


def metta_space():
    #if the_python_runner.parent!=the_python_runner:
    #    return the_python_runner.parent.space()
    return the_runner_space

# Borrowed impl from Adam Vandervorst
class ExtendedMeTTa(MeTTa):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.register_atom("transform", OperationAtom("transform", lambda pattern, template: metta_space().subst(pattern, template),
                                                      type_names=[AtomType.ATOM, AtomType.ATOM, AtomType.UNDEFINED], unwrap=False))
        self.register_atom("join", OperationAtom("join", lambda a, b: interpret(metta_space(), a) + interpret(metta_space(), b),
                                                 type_names=[AtomType.ATOM, AtomType.ATOM, AtomType.ATOM], unwrap=False))

# Borrowed impl from Adam Vandervorst
class LazyMeTTa(ExtendedMeTTa):
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
                yield expr, interpret(metta_space(), expr)
                interpreting = False
            elif not commented:
                metta_space().add_atom(expr)

# Borrowed impl from Adam Vandervorst
class InteractiveMeTTa(LazyMeTTa):

    def __init__(self):
        super().__init__()
        # parent == self
        #   means no parent MeTTa yet
        self.parent = self

    # Add the string to the history
    readline.add_history("@swip")
    readline.add_history("@metta+")
    readline.add_history("!(match &self $ $)")
    #readline.add_history("!(into-metta)")
    #readline.add_history("!(into-vspace)")
    readline.add_history("!(mine-overlaps)")
    readline.add_history("!(try-overlaps)")
    readline.add_history("!(load-flybase-full)")
    readline.add_history("!(load-flybase-tiny)")
    readline.add_history("!(load-vspace)")
    readline.add_history('!(get-by-key &my-dict "A")')

    def maybe_submode(self, line):
        lastchar = line[-1]
        if "+-?!^".find(lastchar)>=0:
            self.submode=lastchar

    def repl_loop(self):

        global verbose
        self.mode = "metta"
        self.submode = "+"
        self.history = []

        while True:
            try:
                # Use the input function to get user input
                prmpt = self.mode + " "+ self.submode + "> "

                line = get_sexpr_input(prmpt)
                if not line.startswith(" "):
                    line = " " + line
                if line:
                    sline = line.lstrip()
                    self.history.append(line)
                else:
                    continue

                if sline.rstrip() == '?':
                    expr = self.parse_single("(match &self $ $)")
                    yield expr, interpret(metta_space(), expr)
                    continue

                # Check for history commands
                if sline.rstrip() == '.h':
                    for idx, item in enumerate(self.history):
                        print(f"{idx + 1}: {item}")
                    continue

                # Switch to python mode
                elif sline.startswith("@p"):
                    self.mode = "python"
                    print("Switched to Python mode.")
                    self.maybe_submode(line.rstrip())
                    readline.add_history("@swip")
                    readline.add_history("@metta")
                    continue

                elif sline.startswith("@space"):
                    global the_runner_space
                    named = sline.split()[1]
                    if named in space_refs:
                        print(f"named={named}")
                        the_runner_space = space_refs[named]()
                    continue

                # Switch to swip mode
                elif sline.startswith("@s"):
                    self.mode = "swip"
                    print("Switched to Swip mode.")
                    self.maybe_submode(line.rstrip())
                    readline.add_history("break")
                    readline.add_history("listing(maybe_corisponds/2)")
                    readline.add_history("synth_query(4,Query)")
                    continue

                # Switch to metta mode
                elif sline.startswith("@m"):
                    self.mode = "metta"
                    print("Switched to MeTTa mode.")
                    self.maybe_submode(line.rstrip())
                    readline.add_history("!(match &self $ $)")
                    continue

                elif sline.startswith("@v"):
                    verbose = int(sline.split()[1])
                    print(f"Verbosity level set to {verbose}")
                    continue

                # Show help
                elif sline.startswith("@h"):
                    print("Help:")
                    print("@m       - Switch to MeTTa mode.")
                    print("@m +     - Default Mode: Add bare atoms.")
                    print("@m -     -   changes to: Remove bare atoms.")
                    print("@m ?     -               Query bare atoms.")
                    print("@m !     -               Interpret bare atoms.")
                    print("@m ^     - Interpret atoms as if there are in files (+)")
                    print("@p       - Switch to Python mode.")
                    print("@s       - Switch to Swip mode.")
                    print("@space   - Change the &self of the_runner_space.")
                    print("@v ###   - Verbosity 0-3")
                    print("@h       - Display this help message.")
                    print("Ctrl-D   - Exit interpreter.")
                    print(".s        - Save session.")
                    print(".l        - Load the latest session.")
                    print(".q        - Quit the session.")
                    print(".h        - Display command history.")
                    continue

                prefix = sline[0]

                if self.mode == "swip":
                    if prefix == "%":
                        print(line) # comment
                        continue
                    if not sline.startswith("("):
                       swip_exec(line)
                    else:
                       expr = self.parse_single(sline)
                       if verbose>1: print(f"% S-Expr {line}")
                       if verbose>1: print(f"% M-Expr {expr}")
                       swip_obj = metta_to_swip(expr);
                       if verbose>1: print(f"% P-Expr {swip_obj}")
                       call_sexpr = Functor("call_sexpr", 2)
                       user = newModule("user")
                       X = Variable()
                       q = Query(call_sexpr(swip_obj, X), module=user)
                       while q.nextSolution():
                           print(X.value)
                       q.closeQuery()
                       continue

                elif self.mode == "python":
                    if prefix == "#":
                        print(line) # comment
                        continue
                    result = eval(line)
                    printl(result)
                    continue

                elif self.mode == "metta":
                    rest = line[2:].strip()
                    if prefix == ";":
                        print(line) # comment
                        continue
                    elif sline.startswith(".s"):
                        name = f"session_{round(time())}.mettar" if rest == "" else (
                            rest if rest.endswith("mettar") else rest + ".mettar")
                        with open(os.sep.join(self.cwd + name), 'w') as f:
                            f.writelines(history)
                        continue
                    elif sline.startswith(".l"):
                        name = max(glob("session_*.mettar")) if rest == "" else (
                            rest if rest.endswith("mettar") else rest + ".mettar")
                        self.lazy_import_file(name)
                        continue
                    elif sline.startswith(".q"):
                        break

                    if "+-?!^".find(prefix)<0:
                        prefix = self.submode
                        rest = line

                    #print(f"submode={self.submode} rest={rest} ")

                    if prefix == "!":
                        expr = self.parse_single(rest)
                        yield expr, interpret(metta_space(), expr)
                        continue
                    elif prefix == "?":
                        expr = self.parse_single(rest)
                        yield expr, metta_space().subst(expr, expr)
                        continue
                    elif prefix == "+":
                        expr = self.parse_single(rest)
                        metta_space().add_atom(expr)
                        continue
                    elif prefix == "-":
                        expr = self.parse_single(rest)
                        metta_space().remove_atom(expr)
                        continue
                    elif prefix == "^":
                        printl(the_python_runner.run(line));
                        continue
                    else:
                        expr = self.parse_single(rest)
                        yield expr, interpret(metta_space(), expr)
                        continue

            except KeyboardInterrupt:
                if verbose>0: print("\nCtrl+C Exiting...")
                sys.exit(3)

            except EOFError:
                # Handle Ctrl+D to exit
                if verbose>0: print("\n^D EOF...")
                sys.exit(0)

            except Exception as e:
                # If there's an error, print it
                if verbose>0: print(f"Error: {e}")
                continue

    def repl(self):
        for i, (expr, result_set) in enumerate(self.repl_loop()):
            if result_set:
                for result in result_set:
                    print(color_expr(result))
            else:
                print(f"/")


    def copy(self):
        return self





def _response2bindings(txt):
        res = re.findall(r'\{.*?\}', txt)
        new_bindings_set = BindingsSet.empty()
        if res == []:
            return new_bindings_set
        res = res[0][1:-1]
        _var, val = res.split(':')
        var = re.findall(r'\".*?\"', _var)
        var = var[0][1:-1] if len(var) > 0 else _var.replace(" ", "")
        if var[0] == '$':
            var = var[1:]
        var = V(var)
        try:
            val = ValueAtom(int(val))
            bindings = Bindings()
            bindings.add_var_binding(var, val)
            new_bindings_set.push(bindings)
        except ValueError:
            ss = re.findall(r'\".*?\"', val)
            if ss == []:
                ss = ['"' + val + '"']
            for s in ss:
                val = S(s[1:-1])
                bindings = Bindings()
                bindings.add_var_binding(var, val)
                new_bindings_set.push(bindings)
        return new_bindings_set


class GptSpace(GroundingSpace):
    def query(self, query_atom):
        tot_str = "Answer the question taking into account the following information (each fact is in brackets):\n"
        for atom in self.atoms_iter():
            tot_str += str(atom) + "\n"
        tot_str += "If the question contains letters in brackets with $ sign, for example ($x), provide the answer in the json format in curly brackets, that is { $x: your answer }.\n"
        # tot_str += "If information is not provided, return the entry to be queried in JSON {unknown value: UNKNOWN}."
        tot_str += "The question is: " + str(query_atom)[1:-1] + "?"
        response = openai.ChatCompletion.create(
                model="gpt-3.5-turbo-0613",
                messages=[{'role': 'system', 'content': 'Reason carefully about user request'},
                    {'role': "user", "content": tot_str}],
                temperature=0)
        txt = response['choices'][0]['message']['content']
        return _response2bindings(txt)

    def copy(self):
        return self

class GptIntentSpace(GroundingSpace):
    def query(self, query_atom):
        tot_str = "Analyze the topic of the utterance: " + str(query_atom)[1:-1] + "\n"
        tot_str += "Try to pick the most relevant topic from the following list (each topic in brackets):"
        for atom in self.atoms_iter():
            tot_str += str(atom) + "\n"
        tot_str += "If neither of the listed topics seems relevant, answer (chit-chat)."
        tot_str += "Provide the answer in the json format in curly brackets in the form { topic: your answer }.\n"
        response = openai.ChatCompletion.create(
                model="gpt-3.5-turbo-0613",
                messages=[{'role': 'system', 'content': 'Reason carefully about user request'},
                    {'role': "user", "content": tot_str}],
                temperature=0)
        txt = response['choices'][0]['message']['content']
        return _response2bindings(txt)

    def copy(self):
        return self






use_error_code = True

class FederatedSpace(AbstractSpace):

    def __init__(self, unwrap=True):
        super().__init__()
        self.atoms_list = []
        self.unwrap = unwrap

    # NOTE: this is a naive implementation barely good enough to pass the tests
    # Don't take this as a guide to implementing a space query function
    def query(self, query_atom):

        if use_error_code:
            raise Exception(f"Error in FederatedSpace.query: Implementation for query({query_atom}) is not complete.")

        # Extract only the variables from the query atom
        query_vars = list(filter(lambda atom: atom.get_type() == AtomKind.VARIABLE, query_atom.iterate()))

        # Match the query atom against every atom in the space
        # BindingsSet() creates a binding set with the only matching result
        # We use BindingsSet.empty() to support multiple results
        new_bindings_set = BindingsSet.empty()
        for space_atom in self.atoms_list:
            match_results = space_atom.match_atom(query_atom)

            # Merge in the bindings from this match, after we narrow the match_results to
            # only include variables vars in the query atom
            for bindings in match_results.iterator():
                bindings.narrow_vars(query_vars)
                if not bindings.is_empty():
                    # new_bindings_set.merge_into(bindings) would work with BindingsSet(), but
                    # it would return an empty result for multiple alternatives and merge bindings
                    # for different variables from alternative branches, which would be a funny
                    # modification of query, but with no real use case
                    new_bindings_set.push(bindings)
        return new_bindings_set

    def add(self, atom):
        if use_error_code:
            raise Exception(f"Error in FederatedSpace.add: Implementation for adding atom {atom} is not complete.")
        self.atoms_list.append(atom)

    def remove(self, atom):
        if use_error_code:
            raise Exception(f"Error in FederatedSpace.remove: Implementation for removing atom {atom} is not complete.")
        if atom in self.atoms_list:
            self.atoms_list.remove(atom)
            return True
        else:
            return False

    def replace(self, from_atom, to_atom):
        if use_error_code:
            raise Exception(f"Error in FederatedSpace.replace: Implementation for replacing atom {from_atom} with {to_atom} is not complete.")
        if from_atom in self.atoms_list:
            self.atoms_list.remove(from_atom)
            self.atoms_list.append(to_atom)
            return True
        else:
            return False

    def atom_count(self):
        if use_error_code:
            raise Exception("Error in FederatedSpace.atom_count: Implementation for counting atoms is not complete.")
        return len(self.atoms_list)

    def atoms_iter(self):
        if use_error_code:
            raise Exception("Error in FederatedSpace.atoms_iter: Implementation for iterating over atoms is not complete.")
        return iter(self.atoms_list)

    def copy(self):
        return self


class VSpace(AbstractSpace):

    def __init__(self, unwrap=True):
        super().__init__()
        self.atoms_list = []
        self.unwrap = unwrap

    # NOTE: this is a naive implementation barely good enough to pass the tests
    # Don't take this as a guide to implementing a space query function
    def query(self, query_atom):

        if use_error_code:
            raise Exception(f"Error in VSpace.query: Implementation for query({query_atom}) is not complete.")

        # Extract only the variables from the query atom
        query_vars = list(filter(lambda atom: atom.get_type() == AtomKind.VARIABLE, query_atom.iterate()))

        # Match the query atom against every atom in the space
        # BindingsSet() creates a binding set with the only matching result
        # We use BindingsSet.empty() to support multiple results
        new_bindings_set = BindingsSet.empty()
        for space_atom in self.atoms_list:
            match_results = space_atom.match_atom(query_atom)

            # Merge in the bindings from this match, after we narrow the match_results to
            # only include variables vars in the query atom
            for bindings in match_results.iterator():
                bindings.narrow_vars(query_vars)
                if not bindings.is_empty():
                    # new_bindings_set.merge_into(bindings) would work with BindingsSet(), but
                    # it would return an empty result for multiple alternatives and merge bindings
                    # for different variables from alternative branches, which would be a funny
                    # modification of query, but with no real use case
                    new_bindings_set.push(bindings)
        return new_bindings_set

    def add(self, atom):
        if use_error_code:
            raise Exception(f"Error in VSpace.add: Implementation for adding atom {atom} is not complete.")
        self.atoms_list.append(atom)

    def remove(self, atom):
        if use_error_code:
            raise Exception(f"Error in VSpace.remove: Implementation for removing atom {atom} is not complete.")
        if atom in self.atoms_list:
            self.atoms_list.remove(atom)
            return True
        else:
            return False

    def replace(self, from_atom, to_atom):
        if use_error_code:
            raise Exception(f"Error in VSpace.replace: Implementation for replacing atom {from_atom} with {to_atom} is not complete.")
        if from_atom in self.atoms_list:
            self.atoms_list.remove(from_atom)
            self.atoms_list.append(to_atom)
            return True
        else:
            return False

    def atom_count(self):
        if use_error_code:
            raise Exception("Error in VSpace.atom_count: Implementation for counting atoms is not complete.")
        return len(self.atoms_list)

    def atoms_iter(self):
        if use_error_code:
            raise Exception("Error in VSpace.atoms_iter: Implementation for iterating over atoms is not complete.")
        return iter(self.atoms_list)

    def copy(self):
        return self


@register_atoms(pass_metta=True)
def register_vspace_atoms(metta):

    counter = 0
    if verbose>0: print(f"register_vspace_atoms metta={metta} the_python_runner={the_python_runner}")

    if not isinstance(metta, VSpace):
        the_python_runner.parent = metta

    def new_value_atom_func():
        nonlocal counter
        counter += 1
        return [ValueAtom({'A': counter, 6: 'B'})]

    # We don't add types for operations, because numpy operations types are too loose
    nmVectorAtom = G(VSPatternOperation('np.vector', wrapnpop(lambda *args: np.array(args)), unwrap=False))
    nmArrayAtom = G(VSPatternOperation('np.array', wrapnpop(lambda *args: np.array(args)), unwrap=False, rec=True))
    nmAddAtom = G(VSPatternOperation('np.add', wrapnpop(np.add), unwrap=False))
    nmSubAtom = G(VSPatternOperation('np.sub', wrapnpop(np.subtract), unwrap=False))
    nmMulAtom = G(VSPatternOperation('np.mul', wrapnpop(np.multiply), unwrap=False))
    nmDivAtom = G(VSPatternOperation('np.div', wrapnpop(np.divide), unwrap=False))
    nmMMulAtom = G(VSPatternOperation('np.matmul', wrapnpop(np.matmul), unwrap=False))


    # DMILES:  I actujally like the behaviour below.. I can hack in sall sort of cool infernce control this way
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

    runnerAtom = G(the_python_runner, AtomType.ATOM)
    add_exported_methods(oper_dict,sys.modules[__name__])
    oper_dict.update({
        r"np\.vector": nmVectorAtom,
        r"np\.array": nmArrayAtom,
        r"np\.add": nmAddAtom,
        r"np\.sub": nmSubAtom,
        r"np\.mul": nmMulAtom,
        r"np\.matmul": nmMMulAtom,
        r"np\.div": nmDivAtom,

        r"new-gpt-space": OperationAtom('new-gpt-space', lambda: [G(SpaceRef(GptSpace()))], unwrap=False),
        r"new-gpt-intent-space": OperationAtom('new-gpt-intent-space', lambda: [G(SpaceRef(GptIntentSpace()))], unwrap=False),
        r"new-v-space": OperationAtom('new-v-space', lambda: [G(SpaceRef(VSpace()))], unwrap=False),

        r"the-v-space": OperationAtom('new-v-space', lambda: [G(SpaceRef(the_vspace))], unwrap=False),


        r"new-value-atom": newValueAtom,
        #'&self': runnerAtom,
        #'&swip': ValueAtom(swip),

        '&my-dict': ValueAtom({'A': 5, 6: 'B'}),
        'get-by-key': OperationAtom('get-by-key', lambda d, k: d[k]),

        # Our FFI to PySWIP
        'load-vspace': OperationAtom('load-vspace', lambda: [load_vspace()]),
        'mine-overlaps': OperationAtom('mine-overlaps', lambda: [mine_overlaps()]),
        'try-overlaps': OperationAtom('try-overlaps', lambda: [try_overlaps()]),
        'load-flybase-full': OperationAtom('load-flybase-full', lambda: [load_flybase("inf")]),
        'load-flybase-tiny': OperationAtom('load-flybase-tiny', lambda: [load_flybase(1000)]),

        r"fb.test-nondeterministic-foreign": OperationAtom('test-nondeterministic-foreign', lambda: test_nondeterministic_foreign, unwrap=False),

        'vspace-main': OperationAtom('vspace-main', lambda: [vspace_main()]),
        'metta_learner::vspace-main': OperationAtom('vspace-main', lambda: [vspace_main()]),
        'swip-exec': OperationAtom('swip-exec', lambda s: [swip_exec(s)]),
        'py-eval': OperationAtom('py-eval', lambda s: [eval(s)]) })

    return oper_dict

oper_dict = {}
syms_dict = {}

def add_exported_methods(dict,module):
    for name, obj in inspect.getmembers(module):
        if inspect.isfunction(obj):
            if getattr(obj, 'export_to_metta', False):
                sig = inspect.signature(obj)
                params = sig.parameters
                num_args = len([p for p in params.values() if p.default == p.empty and p.kind == p.POSITIONAL_OR_KEYWORD])
                add_pyop(dict, name, num_args)

def add_pyop(dict, name, length):
    hyphens, underscores = name.replace('_', '-'), name.replace('-', '_')
    mettavars, pyvars = (' '.join(f"${chr(97 + i)}" for i in range(length))).strip(), (', '.join(chr(97 + i) for i in range(length))).strip()
    s = f"!({hyphens})" if mettavars == "" else f"!({hyphens} {mettavars})"
    #print(s)
    readline.add_history(s);
    if hyphens not in dict:
        src, local_vars = f'op = OperationAtom( "{hyphens}", lambda {pyvars}: [{underscores}({pyvars})])', {}
        exec(src, globals(), local_vars)
        #print(f'metta: OperationAtom("{hyphens}",{src}, unwrap=False)')
        dict[hyphens] = local_vars['op']


def add_swip(dict, name):
    hyphens, underscores = name.replace('_', '-'), name.replace('-', '_')
    readline.add_history(f"!({hyphens})")
    if hyphens not in dict:
        src, local_vars = f'op = lambda : [swip_exec("{underscores}")]', {}
        exec(src, {}, local_vars)
        print(f"swip: {hyphens}")
        dict[hyphens] = OperationAtom(hyphens, local_vars['op'], unwrap=False)



# For now lets test with only  Atoms
@register_tokens(pass_metta=True)
def register_vspace_tokens(metta):

    if verbose>0: print(f"register_vspace_tokens metta={metta} the_python_runner={the_python_runner}")

    if not isinstance(metta, VSpace):
        the_python_runner.parent = metta

    def run_resolved_symbol_op(the_python_runner, atom, *args):
        expr = E(atom, *args)
        if verbose>0: print(f"run_resolved_symbol_op: atom={atom}, args={args}, expr={expr} metta={metta} the_python_runner={the_python_runner}")
        result1 = hp.metta_evaluate_atom(the_python_runner.cmetta, expr.catom)
        result = [Atom._from_catom(catom) for catom in result1]
        if verbose>0: print(f"run_resolved_symbol_op: result1={result1}, result={result}")
        return result

    def resolve_atom(metta, token):
        # TODO: nested modules...
        runner_name, atom_name = token.split('::')

        if atom_name in oper_dict:
            return oper_dict[atom_name]

        if atom_name=="vspace-main":
            vspace_main()
            return
        # FIXME: using `run` for this is an overkill
        ran = metta.run('! ' + runner_name)[0][0];
        if verbose>0: print(f"resolve_atom: token={token} ran={type(ran)} metta={metta} the_python_runner={the_python_runner}")
        try:
            this_runner = ran.get_object()
        except Exception as e:
            this_runner = the_python_runner
            # If there's an error, print it
            #print(f"Error ran.get_object: {e}")

        #if !isinstance(this_runner, MeTTa):

        #if !isinstance(this_runner, MeTTa): this_runner = metta

        atom = this_runner.run('! ' + atom_name)[0][0]
        # A hack to make the_python_runner::&self work
        # TODO? the problem is that we need to return an operation to make this
        # work in parent expressions, thus, it is unclear how to return pure
        # symbols
        if atom.get_type() == hp.AtomKind.GROUNDED:
            return atom

        # TODO: borrow atom type to op
        return OperationAtom( token, lambda *args: run_resolved_symbol_op(the_python_runner, atom, *args), unwrap=False)

    syms_dict.update({
        '&gptspace': lambda _: G(SpaceRef(the_gptspace)),
        '&flybase': lambda _: G(SpaceRef(the_flybase)),
        '&vspace': lambda _: G(SpaceRef(the_vspace)),
        '&vbase_class': lambda _: G((the_vspace)),
        '&parent_ref': lambda _: G(SpaceRef(the_python_runner.parent.space())),
        '&parent': lambda _: G(the_python_runner.parent.space()),
        '&child': lambda _: G(the_python_runner.space()),
        '&child_ref': lambda _: G(SpaceRef(the_python_runner.space())),
        '&the_runner': lambda _: ValueAtom(the_python_runner),
        '&the_metta': lambda _: ValueAtom(the_python_runner.parent),
        r"[^\s]+::[^\s]+": lambda token: resolve_atom(metta, token)
    })
    for key in syms_dict:
        if key.startswith("&"):
            readline.add_history(f"!{key}")
    return syms_dict

space_refs = {
    '&gptspace': lambda: the_gptspace,
    '&flybase': lambda: the_flybase,
    '&vspace': lambda: the_vspace,
    '&parent': lambda: the_python_runner.parent.space(),
    '&child': lambda: the_python_runner.space()}



from pyswip import registerForeign, PL_foreign_context, PL_foreign_control, PL_FIRST_CALL, PL_REDO, PL_PRUNED, PL_retry, PL_FA_NONDETERMINISTIC, Variable, Prolog as PySwip, Atom as PySwipAtom

@export_to_metta
def test_nondeterministic_foreign():
    #from metta_vspace import swip
    swip= swip;
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

    print(result)

    if len(result) != 10:
        print('Query should return 10 results')

    for i in range(10):
        if {'X': i} not in result:
            print('Expected result X:{} not present'.format(i))


    def hello(t):
        print("Hello,", t)

    hello.arity = 1

    registerForeign(hello)

    swip.assertz("father(michael,john)")
    swip.assertz("father(michael,gina)")

    result = list(swip.query("father(michael,X), hello(X)"))

    print(result)

    if len(result) != 2:
        print('Query should return two results')
    for name in ('john', 'gina'):
        if {'X': name} not in result:
            print('Expected result X:{} not present'.format(name))


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

    print(result)

    if result[0]['Result'] != 'true':
          print('A string return value should not be converted to an atom.')

    print()
    print()
    print()

@export_to_metta
def swip_to_metta(swip_obj):

    if isinstance(swip_obj, str):
        return S(swip_obj)

    if isinstance(swip_obj, PySwipAtom):
        return S(swip_obj.get_value())

    if isinstance(swip_obj, Variable):
        return V(swip_obj.chars if swip_obj.chars else "Var")

    if isinstance(swip_obj, Functor):
        # Convert the functor to an expression in Metta
        if isinstance(swip_obj.name, PySwipAtom):
            sfn = swip_obj.name.value
        else:
            sfn = swip_obj.name

        if sfn=="[|]":
            sfn = "::";

        fn = S(sfn)

        # Create an array of arguments first
        argz = [swip_to_metta(arg) for arg in swip_obj.args]

        args_len = len(argz)

        # Handle the creation of E based on the length of argz
        if args_len == 1:
            main_expr = E(fn, argz[0])
        elif args_len == 2:
            main_expr = E(fn, argz[0], argz[1])
        elif args_len == 3:
            main_expr = E(fn, argz[0], argz[1], argz[2])
        elif args_len == 4:
            main_expr = E(fn, argz[0], argz[1], argz[2], argz[3])
        elif args_len == 5:
            main_expr = E(fn, argz[0], argz[1], argz[2], argz[3], argz[4])
        elif args_len == 6:
            main_expr = E(fn, argz[0], argz[1], argz[2], argz[3], argz[4], argz[5])
        elif args_len == 7:
            main_expr = E(fn, argz[0], argz[1], argz[2], argz[3], argz[4], argz[5], argz[6])
        elif args_len == 8:
            main_expr = E(fn, argz[0], argz[1], argz[2], argz[3], argz[4], argz[5], argz[6], argz[7])
        elif args_len == 9:
            main_expr = E(fn, argz[0], argz[1], argz[2], argz[3], argz[4], argz[5], argz[6], argz[7], argz[8])
        elif args_len == 10:
            main_expr = E(fn, argz[0], argz[1], argz[2], argz[3], argz[4], argz[5], argz[6], argz[7], argz[8], argz[9])
        return main_expr

    # Handle numbers and convert them to ValueAtom objects in Metta
    if isinstance(swip_obj, (int, float)):
        return ValueAtom(swip_obj)

    # Handle PySwip lists
    if isinstance(swip_obj, list):
        list_expr = E("::")
        for item in swip_obj:
            list_expr.add_sub_expression(swip_to_metta(item))
        return list_expr

    raise ValueError(f"Unknown PySwip object type: {type(swip_obj)}")

@export_to_metta
def metta_to_swip(metta_obj):

    if isinstance(metta_obj, ValueAtom):
        return metta_obj.get_value()

    if isinstance(metta_obj, SymbolAtom):
        return PySwipAtom(metta_obj.get_value())


    if isinstance(metta_obj, E):
        # Convert the main expression and its sub-expressions to a Functor in PySwip
        if metta_obj.get_value() == "::":  # Convert Metta list to PySwip list
            return [metta_to_swip(sub_expr) for sub_expr in metta_obj.sub_expressions]
        else:
            args = [metta_to_swip(sub_expr) for sub_expr in metta_obj.sub_expressions]
            return Functor(PySwipAtom(metta_obj.get_value()), len(args), args)

    if isinstance(metta_obj, V):
        return Variable(name=metta_obj.get_value())

    raise ValueError(f"Unknown Metta object type: {type(metta_obj)}")


@export_to_pyswip
def swip_to_metta_wrapper(swip_obj, metta_obj):
    result = swip_to_metta(swip_obj)
    metta_obj.unify(metta_to_swip(result))
    return True

@export_to_pyswip
def metta_to_swip_wrapper(metta_obj, swip_obj):
    result = metta_to_swip(metta_obj)
    swip_obj.unify(result)
    return True

@export_to_metta
def metta_to_swip_tests1():
    # Register the methods as foreign predicates
    registerForeign(swip_to_metta_wrapper, arity=2)
    registerForeign(metta_to_swip_wrapper, arity=2)

    # Usage:
    swip_functor = Functor(PySwipAtom("example"), 2, [PySwipAtom("sub1"), 3.14])
    print(f"swip_functor={swip_functor}"),
    metta_expr = swip_to_metta(swip_functor)
    print(f"metta_expr={metta_expr}"),
    converted_back_to_swip = metta_to_swip(metta_expr)
    print(f"converted_back_to_swip={converted_back_to_swip}"),


    # Now you can use the methods in PySwip queries
    print(list(swip.query("swip_to_metta_wrapper('example', X).")))
    print(list(swip.query("metta_to_swip_wrapper(X, 'example').")))

@export_to_metta
def metta_to_swip_tests2():
    # Register the methods as foreign predicates
    registerForeign(swip_to_metta_wrapper, arity=2)
    registerForeign(metta_to_swip_wrapper, arity=2)

    # Now you can use the methods in PySwip queries
    printl(list(swip.query("swip_to_metta_wrapper('example', X).")))
    printl(list(swip.query("metta_to_swip_wrapper(X, 'example').")))

    # Usage:
    swip_list = ["a", "b", 3]
    metta_expr = swip_to_metta(swip_list)
    converted_back_to_swip = metta_to_swip(metta_expr)
    swip_functor = Functor(PySwipAtom("example"), 2, [PySwipAtom("sub1"), 3.14])
    metta_expr = swip_to_metta(swip_functor)
    converted_back_to_swip = metta_to_swip(metta_expr)


@export_to_metta
def swip_exec(qry):
    #from metta_vspace import swip
    #if is_init==True:
    #   print("Not running Query: ",qry)
    #   return
    for r in swip.query(qry):
        print(r)


@export_to_metta
def load_vspace():
   swip_exec(f"ensure_loaded('{os.path.dirname(__file__)}/pyswip/swi_flybase')")

@export_to_metta
def mine_overlaps():
   load_vspace()
   swip_exec("mine_overlaps")

@export_to_metta
def try_overlaps():
   load_vspace()
   swip_exec("try_overlaps")

def load_flybase(size):
   load_vspace()
   swip_exec(f"load_flybase({size})")

@export_to_metta
def vspace_main():
    is_init=False
    #os.system('clear')
    t0 = monotonic_ns()
    print(underline("Version-Space Main\n"))
    #if is_init==False: load_vspace()
    #if is_init==False: load_flybase()
    #if is_init==False:

    the_python_runner.repl()
    print(f"\nmain took {(monotonic_ns() - t0)/1e9:.5} seconds")

def vspace_init():
    t0 = monotonic_ns()
    #os.system('clear')
    print(underline(f"Version-Space Init: {__file__}\n"))
    #import site
    #print ("Site Packages: ",site.getsitepackages())
    #test_nondeterministic_foreign()

    if os.path.isfile(f"{the_python_runner.cwd}autoexec.metta"):
        the_python_runner.lazy_import_file("autoexec.metta")
    # @TODO fix this metta_to_swip_tests1()
    #load_vspace()
    print(f"\nInit took {(monotonic_ns() - t0)/1e9:.5} seconds")

def mark_decorator(name):
    def decorator(func):
        if hasattr(func, "__decorators__"):
            func.__decorators__.append(name)
        else:
            func.__decorators__ = [name]
        return func
    return decorator

# All execution happens here
#export_to_metta = mark_decorator("export_to_metta")
#export_to_pyswip = mark_decorator("export_to_pyswip")
#staticmethod = mark_decorator("staticmethod")

swip = PySwip()
the_gptspace = GptSpace()
the_vspace = VSpace()
the_flybase = the_vspace
the_python_runner = InteractiveMeTTa();
the_python_runner.cwd = [os.path.dirname(os.path.dirname(__file__))]
the_python_runner.run("!(extend-py! metta_learner)")
the_runner_space = the_python_runner.space()
#the_python_runner.run("!(extend-py! VSpace)")
#the_python_runner.run("!(extend-py! GptSpace)")
is_init_ran = False
if is_init_ran == False:
    is_init_ran = True
    vspace_init()

if __name__ == "__main__":
    vspace_main()

#from . import metta_learner


