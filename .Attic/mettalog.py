# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import os
import pyswip as pl
import pyswip as lrn
import uuid
import tempfile
import platform
from contextlib import contextmanager
from hyperon.atoms import *
from hyperon.ext import register_atoms
from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa
from hyperon.ext import register_atoms

import numpy as np

@contextmanager
def temp_file(temp_dir=None):
    pltfrm = platform.system()
    if pltfrm == "Windows":
        # create a temporary file manually and delete it on close
        if temp_dir is not None:
            # convert to a raw string for windows
            temp_dir = temp_dir.encode('unicode-escape').decode().replace('\\\\', '\\')
        else:
            temp_dir = ""

        fname = os.path.join(temp_dir, uuid.uuid4().hex)
        with open(fname, mode='w') as f:
            yield f
        # delete the file manually
        os.remove(fname)
    else:
        with tempfile.NamedTemporaryFile(mode='w', suffix='.pl') as f:
            yield f


class MeTTaC(MeTTa):

    def copy(self):
        return self


class NumpyValue(MatchableObject):

    def __eq__(self, other):
        return isinstance(other, NumpyValue) and\
               (self.content.shape == other.content.shape) and\
               (self.content == other.content).all()

    def match_(self, other):
        sh = self.content.shape
        bindings = {}
        if isinstance(other, GroundedAtom):
            other = other.get_object()
        # Match by equality with another NumpyValue
        if isinstance(other, NumpyValue):
            return [{}] if other == self else []
        # if isinstance(other, PatternValue):
        #     other = other.to_expr()
        if isinstance(other, ExpressionAtom):
            ch = other.get_children()
            # TODO: constructors and operations
            if len(ch) != sh[0]:
                return []
            for i in range(len(ch)):
                res = self.content[i]
                typ = _np_atom_type(res)
                res = NumpyValue(res)
                if isinstance(ch[i], VariableAtom):
                    bindings[ch[i].get_name()] = G(res, typ)
                elif isinstance(ch[i], ExpressionAtom):
                    bind_add = res.match_(ch[i])
                    if bind_add == []:
                        return []
                    bindings.update(bind_add[0])
        return [] if len(bindings) == 0 else [bindings]


class PatternValue(MatchableObject):

    def match_(self, other):
        if isinstance(other, GroundedAtom):
            other = other.get_object().content
        if not isinstance(other, PatternValue):
            return other.match_(self)
        # TODO: match to patterns
        return []


class PatternOperation(OperationObject):

    def __init__(self, name, op, unwrap=False, rec=False):
        super().__init__(name, op, unwrap)
        self.rec = rec

    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        if self.rec:
            args = args[0].get_children()
            args = [self.execute(arg)[0]\
                if isinstance(arg, ExpressionAtom) else arg for arg in args]
        # If there is a variable or PatternValue in arguments, create PatternValue
        # instead of executing the operation
        for arg in args:
            if isinstance(arg, GroundedAtom) and\
               isinstance(arg.get_object(), PatternValue) or\
               isinstance(arg, VariableAtom):
                return [G(PatternValue([self, args]))]
        return super().execute(*args, res_typ=res_typ)


def _np_atom_type(npobj):
    return E(S('NPArray'), E(*[ValueAtom(s, 'Number') for s in npobj.shape]))

def wrapnpop(func):
    def wrapper(*args):
        a = [arg.get_object().value for arg in args]
        res = func(*a)
        typ = _np_atom_type(res)
        return [G(NumpyValue(res), typ)]
    return wrapper

@register_atoms
def flyspace_atoms():

    # FIXME: we don't add types for operations, because numpy operations types
    # are too loose
    nmVectorAtom = G(PatternOperation('np.vector', wrapnpop(lambda *args: np.array(args)), unwrap=False))
    nmArrayAtom = G(PatternOperation('np.array', wrapnpop(lambda *args: np.array(args)), unwrap=False, rec=True))
    nmAddAtom = G(PatternOperation('np.add', wrapnpop(np.add), unwrap=False))
    nmSubAtom = G(PatternOperation('np.sub', wrapnpop(np.subtract), unwrap=False))
    nmMulAtom = G(PatternOperation('np.mul', wrapnpop(np.multiply), unwrap=False))
    nmDivAtom = G(PatternOperation('np.div', wrapnpop(np.divide), unwrap=False))
    nmMMulAtom = G(PatternOperation('np.matmul', wrapnpop(np.matmul), unwrap=False))
    # We don't use metta here, but we could...
    content = '''
        (: fact (-> Number Number))
        (= (fact $x)
           (case $x
            ((0 1)
             ($_ (* $x (fact (- $x 1)))))
           )
        )

        (some content)
        (= (self-from-self)
           (match &self (some $x) $x))

        something

        (= (call_func $f $arg) ($f $arg))
    '''
    runner = MeTTaC()
    runner.run(content)
    runnerAtom = G(runner, AtomType.ATOM)
    newNSpaceAtom = OperationAtom('new-fly-space', lambda: [G(SpaceRef(FlySpace()))], unwrap=False)
    newISpaceAtom = OperationAtom('new-intent-space', lambda: [G(SpaceRef(IntentSpace()))], unwrap=False)
    return {
        r"np\.vector": nmVectorAtom,
        r"np\.array": nmArrayAtom,
        r"np\.add": nmAddAtom,
        r"np\.sub": nmSubAtom,
        r"np\.mul": nmMulAtom,
        r"np\.matmul": nmMMulAtom,
        r"np\.div": nmDivAtom,
        r"new-fly-space": newNSpaceAtom,
        r"new-intent-space": newISpaceAtom,
        r"r": runnerAtom,
        r"metta_learner": runnerAtom
    }



class IsolatedMettaLearner(pl.Prolog):
    """
    Simple wrapper around the basic Interpreter-class from pyswip for use in notebooks. This wrapper uses a dedictated
    Mettalearner module for each of it's instances, separating them from one another.
    """
    
    class _QueryWrapper(pl.Prolog._QueryWrapper):
        def __call__(self, query, maxresult, catcherrors, normalize):
            for t in super().__call__(query, maxresult, catcherrors, False):
                if normalize:
                    try:
                        v = t.value
                    except AttributeError:
                        v = {}
                        for r in [x.value for x in t]:
                            r = self._normalize_values(r)
                            v.update(r)
                    yield v
                else:
                    yield t
                
        def _normalize_values(self, values):
            from pyswip.easy import Atom, Functor
            if isinstance(values, Atom):
                return values.value
            if isinstance(values, Functor):
                normalized = values.name.value
                if values.arity:
                    normalized_args = ([str(self._normalize_values(arg)) for arg in values.args])
                    normalized = normalized + '(' + ', '.join(normalized_args) + ')'
                return normalized
            elif isinstance(values, dict):
                return {key: self._normalize_values(v) for key, v in values.items()}
            elif isinstance(values, (list, tuple)):
                return [self._normalize_values(v) for v in values]
            return values
    
    def __init__(self, module=None):
        """
        Create a new mettalearner instance in it's own module to isolate it from other running mettalearner code.
        
        Parameters:
        ---
        module: str or None
            The module to connect this instance to. If None (default) a new random module is created
        """
        if module is None:
            module = "m" + uuid.uuid4().hex
        self.module_name = str(module)
        self.module = pl.newModule(self.module_name)
        
    def asserta(self, assertion, catcherrors=False):
        """
        call asserta/1 in the mettalearner instance
        """
        next(self.query(assertion.join(["asserta((", "))."]), catcherrors=catcherrors))

    def assertz(self, assertion, catcherrors=False):
        """
        call assertz/1 in the mettalearner instance
        """
        next(self.query(assertion.join(["assertz((", "))."]), catcherrors=catcherrors))

    def dynamic(self, term, catcherrors=False):
        """
        call dynamic/1 in the mettalearner instance
        """
        next(self.query(term.join(["dynamic((", "))."]), catcherrors=catcherrors))

    def retract(self, term, catcherrors=False):
        """
        call retract/1 in the mettalearner instance
        """
        next(self.query(term.join(["retract((", "))."]), catcherrors=catcherrors))

    def retractall(self, term, catcherrors=False):
        """
        call retractall/1 in the mettalearner instance
        """
        next(self.query(term.join(["retractall((", "))."]), catcherrors=catcherrors))
        
    def consult(self, knowledge_base, file=True, catcherrors=False, temp_dir=None):
        """
        Load the specified knowledge_base in the mettalearner interpreter. To circumvent a SWI-Mettalearner limitation,
        a new temporary file is created on every consult.
        
        Parameters:
        ---
        knowledge_base: str
            The knowledge base to load. This has to be a string containing either the filename (default)
            or the facts to load (if file is False). The knowledge base will be written into a temporary
            file before it is loaded into mettalearner.
        file: bool
            If True (default), the knowledge_base parameter is interpreted as a filename. If False the knowledge_base
            is assumed to contain the facts to load.
        catcherrors: bool
            Catch errors that might occur.
        temp_dir: str
            Optional temporary directory used for writing the knowledge base to a mettalearner file. Applies only on windows systems, 
            ignored otherwise.
        """
        # write all facts into a tempfile first to circumvent the mettalearner-consult limitation
        if file:
            with open(knowledge_base, 'r') as f:
                knowledge_base = f.read()

        pltfrm = platform.system()
        with temp_file(temp_dir) as f:
            f.write(knowledge_base)
            f.flush()
            f.seek(0)

            fname = f.name
            if pltfrm == "Windows":
                # replace backslash with forward slash because mettalearner apparently does not like windows paths...
                fname = fname.replace("\\", "/")
            next(self.query(fname.join(["consult('", "')"]), catcherrors=catcherrors))

    def query(self, query, maxresult=-1, catcherrors=True, normalize=True):
        """
        Run a mettalearner query and return a python-generator.
        If the query is a yes/no question, returns {} for yes, and nothing for no.
        Otherwise returns a generator of dicts with variables as keys.
        
        Parameters:
        ---
        query: str
            The mettalearner query to process.
        maxresult: int
            The maximum number of results to compute (default: -1 = all results).
        catcherrors: bool
            Catch errors that might occur (default: True).        
        normalize: bool
            Convert the mettalearner result objects (Terms) back to their python representation (default: True).
        
        Returns:
        ---
        query: _QueryWrapper
            The query result as an iterator.
        
        >>> mettalearner = IsolatedMettaLearner()
        >>> mettalearner.assertz("father(michael,john)")
        >>> mettalearner.assertz("father(michael,gina)")
        >>> bool(list(mettalearner.query("father(michael,john)")))
        True
        >>> bool(list(mettalearner.query("father(michael,olivia)")))
        False
        >>> print sorted(mettalearner.query("father(michael,X)"))
        [{'X': 'gina'}, {'X': 'john'}]
        """
        return self._QueryWrapper()(self.module_name + ":" + query, maxresult, catcherrors, normalize)


from hyperon.base import Atom
from hyperon.atoms import OperationAtom, E
from hyperon.ext import register_tokens
import hyperonpy as hp


@register_tokens(pass_metta=True)
def my_resolver_atoms(metta):

    def run_resolved_symbol_op(runner, atom, *args):
        expr = E(atom, *args)
        result = hp.metta_evaluate_atom(runner.cmetta, expr.catom)
        result = [Atom._from_catom(catom) for catom in result]
        return result

    def resolve_atom(metta, token):
        # TODO: nested modules...
        runner_name, atom_name = token.split('::')
        # FIXME: using `run` for this is an overkill,
        #        but there is no good Python API for this;
        #        we may have an interface function for
        #        `tokenizer` to resolve individual symbols -
        #        metta.tokenizer().find_token ...
        #        or something else...
        # TODO: assert
        runner = metta.run('! ' + runner_name)[0][0].get_object()
        atom = runner.run('! ' + atom_name)[0][0]
        # A hack to make runner::&self work
        # TODO? the problem is that we need to return an operation to make this
        # work in parent expressions, thus, it is unclear how to return pure
        # symbols
        if atom.get_type() == hp.AtomKind.GROUNDED:
            return atom
        # TODO: borrow atom type to op
        return OperationAtom(
            token,
            lambda *args: run_resolved_symbol_op(runner, atom, *args),
            unwrap=False)

    return {
        r"[^\s]+::[^\s]+": lambda token: resolve_atom(metta, token)
    }



from hyperon.atoms import G, AtomType
from hyperon.runner import MeTTa
from hyperon.ext import register_atoms
from pyswip import Prolog, registerForeign
from pyswip import *

    @staticmethod
    def prolog_to_atomspace(prolog_obj):
        if isinstance(prolog_obj, Atom):
            return S(prolog_obj.get_value())
        
        if isinstance(prolog_obj, Variable):
            return V(prolog_obj.chars if prolog_obj.chars else "Var")
        
        if isinstance(prolog_obj, Functor):
            # Convert the functor to an expression in Atomspace
            main_expr = E(prolog_obj.name.value)
            for arg in prolog_obj.args:
                main_expr.add_sub_expression(Converter.prolog_to_atomspace(arg))
            return main_expr
        
        # Handle numbers and convert them to ValueAtom objects in Atomspace
        if isinstance(prolog_obj, (int, float)):
            return ValueAtom(prolog_obj)
        
        # Handle Prolog lists
        if isinstance(prolog_obj, list):
            list_expr = E("::")
            for item in prolog_obj:
                list_expr.add_sub_expression(Converter.prolog_to_atomspace(item))
            return list_expr
        
        raise ValueError(f"Unknown Prolog object type: {type(prolog_obj)}")

    @staticmethod
    def atomspace_to_prolog(atomspace_obj):
        if isinstance(atomspace_obj, S):
            return Atom(atomspace_obj.get_value())
        
        if isinstance(atomspace_obj, V):
            return Variable(name=atomspace_obj.get_value())
        
        if isinstance(atomspace_obj, E):
            # Convert the main expression and its sub-expressions to a Functor in Prolog
            if atomspace_obj.get_value() == "::":  # Convert Atomspace list to Prolog list
                return [Converter.atomspace_to_prolog(sub_expr) for sub_expr in atomspace_obj.sub_expressions]
            else:
                args = [Converter.atomspace_to_prolog(sub_expr) for sub_expr in atomspace_obj.sub_expressions]
                return Functor(Atom(atomspace_obj.get_value()), len(args), args)
        
        if isinstance(atomspace_obj, ValueAtom):
            return atomspace_obj.get_value()
        
        raise ValueError(f"Unknown Atomspace object type: {type(atomspace_obj)}")



    @staticmethod
    def prolog_to_atomspace_wrapper(prolog_obj, atomspace_obj):
        result = Converter.prolog_to_atomspace(prolog_obj)
        atomspace_obj.unify(result)
        return True

    @staticmethod
    def atomspace_to_prolog_wrapper(atomspace_obj, prolog_obj):
        result = Converter.atomspace_to_prolog(atomspace_obj)
        prolog_obj.unify(result)
        return True

    @staticmethod
    def atomspace_to_prolog_tests():
        # Register the methods as foreign predicates
        registerForeign(Converter.prolog_to_atomspace_wrapper, arity=2)
        registerForeign(Converter.atomspace_to_prolog_wrapper, arity=2)
        
        # Now you can use the methods in Prolog queries
        prolog = Prolog()
        list(prolog.query("prolog_to_atomspace('example', X)."))
        list(prolog.query("atomspace_to_prolog(X, 'example')."))
        
        # Usage:
        prolog_list = ["a", "b", 3]
        atomspace_expr = Converter.prolog_to_atomspace(prolog_list)
        converted_back_to_prolog = Converter.atomspace_to_prolog(atomspace_expr)
        prolog_functor = Functor(Atom("example"), 2, [Atom("sub1"), 3.14])
        atomspace_expr = Converter.prolog_to_atomspace(prolog_functor)
        converted_back_to_prolog = Converter.atomspace_to_prolog(atomspace_expr)


from hyperon import *
from hyperon.ext import register_atoms
#import openai
import os
import re
#openai.api_key = os.environ["OPENAI_API_KEY"]

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

class FlyVSpace(GroundingSpace):
    def query(self, query_atom):
        tot_str = "Answer the question taking into account the following information (each fact is in brackets):\n"
        for atom in self.atoms_iter():
            tot_str += str(atom) + "\n"
        tot_str += "If the question contains letters in brackets with $ sign, for example ($x), provide the answer in the json format in curly brackets, that is { $x: your answer }.\n"
        # tot_str += "If information is not provided, return the entry to be queried in JSON {unknown value: UNKNOWN}."
        tot_str += "The question is: " + str(query_atom)[1:-1] + "?"
        #response = openai.ChatCompletion.create(
        #        model="gpt-3.5-turbo-0613",
        #        messages=[{'role': 'system', 'content': 'Reason carefully about user request'},
        #            {'role': "user", "content": tot_str}],
        #        temperature=0)
        #txt = response['choices'][0]['message']['content']
        return tot_str #_response2bindings(txt)

