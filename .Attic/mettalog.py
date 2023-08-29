#import openai
#openai.api_key = os.environ["OPENAI_API_KEY"]
from time import monotonic_ns, time
import atexit
import os
import re
import readline
import sys
from collections import Counter
from glob import glob
import hyperonpy as hp
from hyperon.atoms import V, S, E, ValueAtom, GroundedAtom, ExpressionAtom, G, AtomType, MatchableObject, OperationAtom, OperationObject, BindingsSet, Atom
from hyperon.runner import MeTTa
from hyperon.ext import register_atoms, register_tokens
from hyperon.base import AbstractSpace, SpaceRef
from hyperon import *

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


class ExtendedMeTTa(MeTTa):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.register_atom("transform", OperationAtom("transform", lambda pattern, template: self.space().subst(pattern, template),
                                                      type_names=[AtomType.ATOM, AtomType.ATOM, AtomType.UNDEFINED], unwrap=False))
        self.register_atom("join", OperationAtom("join", lambda a, b: interpret(self.space(), a) + interpret(self.space(), b),
                                                 type_names=[AtomType.ATOM, AtomType.ATOM, AtomType.ATOM], unwrap=False))

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
                yield expr, interpret(self.space(), expr)
                interpreting = False
            elif not commented:
                self.space().add_atom(expr)


def print_enumerable(obj):
    if obj is None:
        print("None!")
        return
    if isinstance(obj, str):
        print(obj)
        return

    try:
        # Attempt to iterate over the object
        for item in obj:
            color_expr(item)
    except TypeError:
        # If a TypeError is raised, the object is not iterable
        if verbose>0: print(type(obj))
        print(obj)

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


class InteractiveMeTTa(LazyMeTTa):

    # Add the string to the history
    readline.add_history("@swip")
    readline.add_history("@metta+")
    readline.add_history("!(match &self $ $)")
    readline.add_history("!(load-vspace)")
    readline.add_history("!(load-flybase)")
    readline.add_history("!(mine-overlaps)")
    readline.add_history("!(try-overlaps)")
    readline.add_history('!(get-by-key &my-dict "A")')
    # readline.add_history("!(get-by-key &my-dict 6)")
    #readline.add_history("!(extend-py! vspace)")

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
                if line:
                    sline = line.lstrip()
                    self.history.append(line)
                else:
                    continue


                if sline.rstrip() == '?':
                    expr = self.parse_single("(match &self $ $)")
                    yield expr, interpret(self.space(), expr)
                    continue

                # Check for history commands
                if sline.rstrip() == '.history':
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
                    print("@v ###   - Verbosity 0-3")
                    print("@h       - Display this help message.")
                    print("Ctrl-D   - Exit interpreter.")
                    print("s        - Save session.")
                    print("l        - Load the latest session.")
                    print("q        - Quit the session.")
                    print(".history - Display command history.")
                    continue

                prefix = sline[0]

                if self.mode == "swip":
                    if prefix == "%":
                        print(line) # comment
                        continue
                    if not sline.startswith("("):
                       swipexec(line)
                    else:
                       expr = self.parse_single(sline)
                       if verbose>1: print(f"% S-Expr {line}")
                       if verbose>1: print(f"% M-Expr {expr}")
                       swip_obj = atomspace_to_swip(expr);
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
                    print_enumerable(result)
                    continue

                elif self.mode == "metta":
                    rest = line[1:].strip()

                    if prefix == ";":
                        print(line) # comment
                        continue

                    elif prefix == "s":
                        name = f"session_{round(time())}.mettar" if rest == "" else (
                            rest if rest.endswith("mettar") else rest + ".mettar")
                        with open(os.sep.join(self.cwd + name), 'w') as f:
                            f.writelines(history)
                        continue
                    elif prefix == "l":
                        name = max(glob("session_*.mettar")) if rest == "" else (
                            rest if rest.endswith("mettar") else rest + ".mettar")
                        self.lazy_import_file(name)
                        continue
                    elif prefix == "q":
                        break

                    if "+-?!^".find(prefix)<0:
                        prefix = self.submode;
                        rest = line

                    #print(f"submode={self.submode} rest={rest} ")


                    if prefix == "!":
                        expr = self.parse_single(rest)
                        yield expr, interpret(self.space(), expr)
                        continue
                    elif prefix == "?":
                        expr = self.parse_single(rest)
                        yield expr, self.space().subst(expr, expr)
                        continue
                    elif prefix == "+":
                        expr = self.parse_single(rest)
                        self.space().add_atom(expr)
                        continue
                    elif prefix == "-":
                        expr = self.parse_single(rest)
                        self.space().remove_atom(expr)
                        continue
                    elif prefix == "^":
                        print_enumerable(the_runner.run(line));
                        continue
                    else:
                        expr = self.parse_single(rest)
                        yield expr, interpret(self.space(), expr)
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


class FlySpace(AbstractSpace):
    def __init__(self, unwrap=True):
        super().__init__()
        self.atoms_list = []
        self.unwrap = unwrap

    # NOTE: this is a naive implementation barely good enough to pass the tests
    # Don't take this as a guide to implementing a space query function
    def query(self, query_atom):

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

    def copy(self):
        return self

class VSpace(InteractiveMeTTa):


    def __init__(self):
        super().__init__()
        self.parent = self

    def copy(self):
        return self

    def query(self, query_atom):
        tot_str = "Answer the question taking into account the following information (each fact is in brackets):\n"
        for atom in self.atoms_iter():
            tot_str += str(atom) + "\n"
        tot_str += "If the question contains letters in brackets with $ sign, for example ($x), provide the answer in the json format in curly brackets, that is { $x: your answer }.\n"
        # tot_str += "If information is not provided, return the entry to be queried in JSON {unknown value: UNKNOWN}."
        tot_str += "The question is: " + str(query_atom)[1:-1] + "?"
        #response = openai.ChatCompletion.create(
        #       model="gpt-3.5-turbo-0613",
        #       messages=[{'role': 'system', 'content': 'Reason carefully about user request'},
        #           {'role': "user", "content": tot_str}],
        #       temperature=0)
        #txt = response['choices'][0]['message']['content']
        return tot_str #_response2bindings(txt)



@register_atoms(pass_metta=True)
def register_vspace_atoms(metta):

    counter = 0
    if verbose>0: print(f"register_vspace_atoms metta={metta} the_runner={the_runner}")

    if not isinstance(metta, VSpace):
        the_runner.parent = metta

    def new_value_atom_func():
        nonlocal counter
        counter += 1
        return [ValueAtom({'A': counter, 6: 'B'})]

    # FIXME: we don't add types for operations, because numpy operations types
    # are too loose
    nmVectorAtom = G(VSPatternOperation('np.vector', wrapnpop(lambda *args: np.array(args)), unwrap=False))
    nmArrayAtom = G(VSPatternOperation('np.array', wrapnpop(lambda *args: np.array(args)), unwrap=False, rec=True))
    nmAddAtom = G(VSPatternOperation('np.add', wrapnpop(np.add), unwrap=False))
    nmSubAtom = G(VSPatternOperation('np.sub', wrapnpop(np.subtract), unwrap=False))
    nmMulAtom = G(VSPatternOperation('np.mul', wrapnpop(np.multiply), unwrap=False))
    nmDivAtom = G(VSPatternOperation('np.div', wrapnpop(np.divide), unwrap=False))
    nmMMulAtom = G(VSPatternOperation('np.matmul', wrapnpop(np.matmul), unwrap=False))
    newFlySpaceAtom = OperationAtom('new-fly-space', lambda: [G(SpaceRef(FlySpace()))], unwrap=False)
    newVSpaceAtom = OperationAtom('new-v-space', lambda: [G(SpaceRef(VSpace()))], unwrap=False)
    newValueAtom = OperationAtom('new-value-atom', new_value_atom_func, unwrap=False)
    testNDFFI = OperationAtom('test-nondeterministic-foreign', lambda: test_nondeterministic_foreign, unwrap=False)
    # (new-value-atom)
    # this was stored in the space..
    # !(match &self $ $)
    # and each time the space is matched the counter will be incremented
    # !(match &self $ $)
    # !(match &self $ $)
    # (new-value-atom)
    # they share a counter
    # !(match &self $ $)
    runnerAtom = G(the_runner, AtomType.ATOM)
    return {
        r"np\.vector": nmVectorAtom,
        r"np\.array": nmArrayAtom,
        r"np\.add": nmAddAtom,
        r"np\.sub": nmSubAtom,
        r"np\.mul": nmMulAtom,
        r"np\.matmul": nmMMulAtom,
        r"np\.div": nmDivAtom,
        r"new-fly-space": newFlySpaceAtom,
        r"new-v-space": newVSpaceAtom,
        r"new-value-atom": newValueAtom,
        r"metta_learner": runnerAtom,
        #'&self': the_runner,
        #'&gswip': ValueAtom(gswip),

        '&my-dict': ValueAtom({'A': 5, 6: 'B'}),
        'get-by-key': OperationAtom('get-by-key', lambda d, k: d[k]),
        'load-vspace': OperationAtom('load-vspace', lambda: [load_vspace()]),
        'load-flybase': OperationAtom('load-flybase', lambda: [load_flybase()]),
        r"fb.test-nondeterministic-foreign": testNDFFI,

        'vspace-main': OperationAtom('vspace-main', lambda: [vspace_main()]),
        'swip-exec': OperationAtom('swip-exec', lambda s: [swipexec(s)]),
        'py-eval': OperationAtom('py-eval', lambda s: [eval(s)])

    }


@register_tokens(pass_metta=True)
def register_vspace_tokens(metta):

    if verbose>0: print(f"register_vspace_tokens metta={metta} the_runner={the_runner}")

    if not isinstance(metta, VSpace):
        the_runner.parent = metta

    def run_resolved_symbol_op(the_runner, atom, *args):
        expr = E(atom, *args)
        if verbose>0: print(f"run_resolved_symbol_op: atom={atom}, args={args}, expr={expr} metta={metta} the_runner={the_runner}")
        result1 = hp.metta_evaluate_atom(the_runner.cmetta, expr.catom)
        result = [Atom._from_catom(catom) for catom in result1]
        if verbose>0: print(f"run_resolved_symbol_op: result1={result1}, result={result}")
        return result

    def resolve_atom(metta, token):
        # TODO: nested modules...
        runner_name, atom_name = token.split('::')
        if atom_name=="vspace-main":
            vspace_main()
            return
        # FIXME: using `run` for this is an overkill
        ran = metta.run('! ' + runner_name)[0][0];
        if verbose>0: print(f"resolve_atom: token={token} ran={type(ran)} metta={metta} the_runner={the_runner}")
        try:
            this_runner = ran.get_object()
        except Exception as e:
            this_runner = the_runner
            # If there's an error, print it
            #print(f"Error ran.get_object: {e}")

        #if !isinstance(this_runner, MeTTa):

        #if !isinstance(this_runner, MeTTa): this_runner = metta

        atom = this_runner.run('! ' + atom_name)[0][0]
        # A hack to make the_runner::&self work
        # TODO? the problem is that we need to return an operation to make this
        # work in parent expressions, thus, it is unclear how to return pure
        # symbols
        if atom.get_type() == hp.AtomKind.GROUNDED:
            return atom
        # TODO: borrow atom type to op
        return OperationAtom( token, lambda *args: run_resolved_symbol_op(the_runner, atom, *args), unwrap=False)

    return {
        '&flyspace': lambda _: ValueAtom(the_flyspace),
        '&vspace': lambda _: ValueAtom(the_runner),
        '&parent': lambda _: ValueAtom(the_runner.parent),
        '&the_runner': lambda _: ValueAtom(the_runner),
        '&runner': lambda _: ValueAtom(the_runner),
        r"[^\s]+::[^\s]+": lambda token: resolve_atom(metta, token)
    }



from pyswip import registerForeign, PL_foreign_context, PL_foreign_control, PL_FIRST_CALL, PL_REDO, PL_PRUNED, PL_retry, PL_FA_NONDETERMINISTIC, Variable, Prolog as PySwip, Atom as PySwipAtom

def test_nondeterministic_foreign():
    #from metta_vspace import gswip
    swip= gswip;
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

def swip_to_atomspace(swip_obj):

    if isinstance(swip_obj, str):
        return S(swip_obj)

    if isinstance(swip_obj, Atom):
        return S(swip_obj.get_value())

    if isinstance(swip_obj, Variable):
        return V(swip_obj.chars if swip_obj.chars else "Var")

    if isinstance(swip_obj, Functor):
        # Convert the functor to an expression in Atomspace
        if isinstance(swip_obj.name, Atom):
            sfn = swip_obj.name.value
        else:
            sfn = swip_obj.name

        if sfn=="[|]":
            sfn = "::";

        fn = S(sfn)

        # Create an array of arguments first
        argz = [swip_to_atomspace(arg) for arg in swip_obj.args]

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

    # Handle numbers and convert them to ValueAtom objects in Atomspace
    if isinstance(swip_obj, (int, float)):
        return ValueAtom(swip_obj)

    # Handle PySwip lists
    if isinstance(swip_obj, list):
        list_expr = E("::")
        for item in swip_obj:
            list_expr.add_sub_expression(swip_to_atomspace(item))
        return list_expr

    raise ValueError(f"Unknown PySwip object type: {type(swip_obj)}")

@staticmethod
def atomspace_to_swip(atomspace_obj):

    if isinstance(atomspace_obj, ValueAtom):
        return atomspace_obj.get_value()

    if isinstance(atomspace_obj, SymbolAtom):
        return Atom(atomspace_obj.get_value())


    if isinstance(atomspace_obj, E):
        # Convert the main expression and its sub-expressions to a Functor in PySwip
        if atomspace_obj.get_value() == "::":  # Convert Atomspace list to PySwip list
            return [atomspace_to_swip(sub_expr) for sub_expr in atomspace_obj.sub_expressions]
        else:
            args = [atomspace_to_swip(sub_expr) for sub_expr in atomspace_obj.sub_expressions]
            return Functor(Atom(atomspace_obj.get_value()), len(args), args)

    if isinstance(atomspace_obj, V):
        return Variable(name=atomspace_obj.get_value())

    raise ValueError(f"Unknown Atomspace object type: {type(atomspace_obj)}")



@staticmethod
def swip_to_atomspace_wrapper(swip_obj, atomspace_obj):
    result = swip_to_atomspace(swip_obj)
    atomspace_obj.unify(atomspace_to_swip(result))
    return True

@staticmethod
def atomspace_to_swip_wrapper(atomspace_obj, swip_obj):
    result = atomspace_to_swip(atomspace_obj)
    swip_obj.unify(result)
    return True

@staticmethod
def atomspace_to_swip_tests1():
    # Register the methods as foreign predicates
    registerForeign(swip_to_atomspace_wrapper, arity=2)
    registerForeign(atomspace_to_swip_wrapper, arity=2)

    # Usage:
    swip_functor = Functor(Atom("example"), 2, [Atom("sub1"), 3.14])
    print(f"swip_functor={swip_functor}"),
    atomspace_expr = swip_to_atomspace(swip_functor)
    print(f"atomspace_expr={atomspace_expr}"),
    converted_back_to_swip = atomspace_to_swip(atomspace_expr)
    print(f"converted_back_to_swip={converted_back_to_swip}"),


    # Now you can use the methods in PySwip queries
    print(list(swip.query("swip_to_atomspace_wrapper('example', X).")))
    print(list(swip.query("atomspace_to_swip_wrapper(X, 'example').")))

@staticmethod
def atomspace_to_swip_tests2():
    # Register the methods as foreign predicates
    registerForeign(swip_to_atomspace_wrapper, arity=2)
    registerForeign(atomspace_to_swip_wrapper, arity=2)

    # Now you can use the methods in PySwip queries
    list(swip.query("swip_to_atomspace_wrapper('example', X)."))
    list(swip.query("atomspace_to_swip_wrapper(X, 'example')."))

    # Usage:
    swip_list = ["a", "b", 3]
    atomspace_expr = swip_to_atomspace(swip_list)
    converted_back_to_swip = atomspace_to_swip(atomspace_expr)
    swip_functor = Functor(Atom("example"), 2, [Atom("sub1"), 3.14])
    atomspace_expr = swip_to_atomspace(swip_functor)
    converted_back_to_swip = atomspace_to_swip(atomspace_expr)



def swipexec(qry):
    #from metta_vspace import gswip
    #if is_init==True:
    #   print("Not running Query: ",qry)
    #   return
    for r in gswip.query(qry):
        print(r)

def load_vspace():
   swipexec(f"ensure_loaded('{os.path.dirname(__file__)}/pyswip/swi_flybase')")

def load_flybase():
   load_vspace()
   swipexec("load_flybase")


def vspace_init():
    t0 = monotonic_ns()
    #os.system('clear')
    print(underline(f"Version-Space Init: {__file__}\n"))
    #import site
    #print ("Site Packages: ",site.getsitepackages())
    #test_nondeterministic_foreign()

    if os.path.isfile(f"{the_runner.cwd}autoexec.metta"):
        the_runner.lazy_import_file("autoexec.metta")
    # @TODO fix this atomspace_to_swip_tests1()
    #load_vspace()
    print(f"\nInit took {(monotonic_ns() - t0)/1e9:.5} seconds")


def vspace_main():
    is_init=False
    #os.system('clear')
    t0 = monotonic_ns()
    print(underline("Version-Space Main\n"))
    #if is_init==False: load_vspace()
    #if is_init==False: load_flybase()
    #if is_init==False:

    the_runner.repl()
    print(f"\nmain took {(monotonic_ns() - t0)/1e9:.5} seconds")

# All execution happens here
gswip = PySwip()
the_flyspace = FlySpace();
the_runner = VSpace();
the_runner.cwd = [os.path.dirname(os.path.dirname(__file__))]
the_runner.run("!(extend-py! metta_learner)")
the_runner.run("!(extend-py! VSpace)")
the_runner.run("!(extend-py! FlySpace)")
is_init_ran = False
if is_init_ran == False:
    is_init_ran = True
    vspace_init()

if __name__ == "__main__":
    vspace_main()

#from . import metta_learner


