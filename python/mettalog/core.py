import inspect
import janus_swi
import types
import importlib.util
import os
import sys
from enum import Enum, auto
from hyperon import *
from hyperon.ext import register_atoms
import uuid
import requests
import json
import hyperlog
import mettalog_libraries
import mettalog_sandbox
from hyperlog import MeTTaLogImpl
#for mork backend
from mettalog_sandbox.mork.mork import MORKSpace

METTALOG_DIR = os.environ.get("METTALOG_DIR", ".")
VERBOSE_DEBUG = os.environ.get("METTALOG_VERBOSE")
# 0 = for scripts/demos
# 1 = developer
# 2 = verbose_debugger
VERBOSE_DEBUG = 1


def load_metta_module(subpath, module_name, register_as=None):
    """
    Dynamically loads a Python module from a subpath inside `mettalog_libraries`.

    Args:
        subpath (str): Relative path from `mettalog_libraries` to the .py file.
                       Example: 'metta-morp-mlog/mettamorph.py'
        module_name (str): Internal name for the module loader (can be anything).
        register_as (str): If given, registers module in sys.modules under this name.

    Returns:
        The imported module object.
    """
    try:
        import mettalog_libraries
    except ImportError as e:
        raise RuntimeError("Could not import 'mettalog_libraries'. Is it in sys.path?") from e

    base_path = os.path.join(METTALOG_DIR, "libraries")
    full_path = os.path.join(base_path, *subpath.split('/'))

    if not os.path.isfile(full_path):
        raise FileNotFoundError(f"Module file does not exist: {full_path}")

    spec = importlib.util.spec_from_file_location(module_name, full_path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    if register_as:
        sys.modules[register_as] = module

    return module

#for metta-morph backend
mettamorph = load_metta_module("metta-morph-mlog/extend/mettamorph.py", "mettamorph")

# Controls which backend is used to resolve method calls
class MeTTaBackend(Enum):
    METTALOG = auto()
    HE_RUST = auto()
    METTA_MORPH = auto()
    LOGGED_FACADE = auto()
    ORIGINAL_METHOD = auto()
    MORK_CLIENT = auto()


DefaultBackend = MeTTaBackend.LOGGED_FACADE
OverrideBackend = MeTTaBackend.LOGGED_FACADE



# Track what kind of member this MeTTaLog proxy represents
class MeTTaMemberKind(Enum):
    UNKNOWN = auto()
    MODULE = auto()
    FIELD = auto()
    CALL = auto()
    CLASS = auto()
    FUNC = auto()
    METHOD = auto()
    ITEM = auto()
    INSTANCE = auto()
    ATTR = auto()


# Example backend shim (simulates MeTTa call)
class metta_client:
    @staticmethod
    def apply_once(predicate="from_pymetta", path=None, selfref=None, swimod="user", args=(), kwargs=None, fullpath=None, backend=MeTTaBackend.LOGGED_FACADE, retType=any, asIterator = False,  fail=None):
        if VERBOSE_DEBUG>1:
            print("\n?? [janus_swi.apply_once]")
            print(f"  swimod  : {swimod}")
            print(f"  fullpath : {fullpath}")
            print(f"  args     : {args}")
            print(f"  kwargs   : {kwargs}")
            print(f"  path     :")
            for step in path:
                print(f"    - {step}")
            print(f"  selfref  : {selfref}")

        metta_exec = MeTTaLog.convert_to_metta_exec(predicate=predicate, path=path, selfref=selfref, args=args, kwargs=kwargs, fullpath=fullpath)

        if backend is MeTTaBackend.METTALOG:
            module = swimod
            input = (path, selfref, args, kwargs, fullpath)
            predicate = "from_pymetta"
            if asIterator:
                ret = janus_swi.apply(module, predicate, input, fail=obj)
            else:
                ret = janus_swi.apply_once(module, predicate, input, fail=obj)
            
        if backend is MeTTaBackend.MORK_CLIENT:
            ret = theMORKSpace.query(metta_exec)

        if backend is MeTTaBackend.METTA_MORPH:
            ret = MeTTaBackend.METTA_MORPH

        if backend is MeTTaBackend.MORK_CLIENT:
            ret = heMeTTa.run(metta_exec)

        if backend is MeTTaBackend.LOGGED_FACADE:
            if fail is not None: return fail                
            ret = f"result_of({swimod}, fullpath={fullpath})"

        return ret

class MeTTaLog:


    @staticmethod
    def convert_to_metta_exec(predicate="from_pymetta", path=None, selfref=None, swimod="user", args=(), kwargs=None, fullpath=None, backend=MeTTaBackend.LOGGED_FACADE, retType=any, asIterator = False,  fail=None):
        """
        Convert a Python call structure into a MeTTa symbolic expression (S-expression).

        Args:
            *args: (swimod, path, selfref, args, kwargs, fullpath)

        Returns:
            A string representing the MeTTa call expression.
        """

        def sexpr_arg(val):
            if isinstance(val, str):
                return f'"{val}"'
            elif isinstance(val, (int, float, bool)):
                return str(val).lower() if isinstance(val, bool) else str(val)
            elif isinstance(val, MeTTaLog):
                return f"'{val._name}"
            return repr(val)

        path_expr = " ".join(f"(attr '{p[1]}')" if p[0] == "attr" else f"(item {sexpr_arg(p[1])})" for p in path)
        args_expr = " ".join(sexpr_arg(a) for a in args)
        kwargs_expr = " ".join(f":{k} {sexpr_arg(v)}" for k, v in (kwargs or {}).items())

        return f"({predicate} {fullpath} (path {path_expr}) (args {args_expr}) ({kwargs_expr}))"


    def simulate_instance(self, instance):
        """Optionally attach an instance to inform original method context."""
        self._real_attrs["__instance__"] = instance
        self._meta["kind"] = MeTTaMemberKind.INSTANCE

    def simulate_class(self, class_name):
        """Annotate the symbolic proxy with class metadata."""
        self._meta["class"] = class_name
        # Attach a real Python callable to this node, and use it for execution

    def bind_original(self, func):
        # self._meta["mocked"] = False  # Optional: use this if true monkey-patching is implemented
        """Bind a real Python method to be invoked when this node is called using ORIGINAL_METHOD."""
        self._real_attrs["__callable__"] = func
        self._backend = MeTTaBackend.ORIGINAL_METHOD

    _trace = []  # Trace log for all symbolic accesses/calls
    _mock_attrs = {
        "foo", "bar", "baz", "login", "logout",
        "config", "session", "query", "user", "profile",
        "math", "sqrt", "engine", "start", "activate", "connect",
        "api", "version", "agent", "control", "panel", "button"
    }

    def __init__(self, name="root", depth=0, history=None, swimod=None, autounbox=False,
                 backend=MeTTaBackend.ORIGINAL_METHOD):
        self._name = name
        self._depth = depth
        self._history = history or []
        self._real_attrs = {}
        self._real_items = {}
        self._swimod = swimod
        self._meta = {"kind": MeTTaMemberKind.UNKNOWN}
        self._methods = {}
        self._autounbox = autounbox
        self._backend = backend
        # self._backend = backend  # Which resolution strategy to use (Prolog, original method, etc.)

    def __getattr__(self, name):
        if name in self._real_attrs:
            return self._real_attrs[name]

        def looks_like_class(name):
            return name and name[0].isupper() and any(c.islower() for c in name[1:])

        nested = MeTTaLog(f"{self._name}.{name}", self._depth + 1, self._history[:], self._swimod,
                          autounbox=self._autounbox)

        if name in MeTTaLog._mock_attrs:
            nested._meta["kind"] = MeTTaMemberKind.FIELD
            self._real_attrs[name] = nested
            return nested

        self._history.append(("attr", name))
        MeTTaLog._trace.append(("access", nested._name, ("attr", name), self._swimod, None))

        if looks_like_class(name) and self._meta.get("kind") in {MeTTaMemberKind.UNKNOWN, MeTTaMemberKind.MODULE}:
            nested._meta["kind"] = MeTTaMemberKind.CLASS
        else:
            nested._meta["kind"] = MeTTaMemberKind.FIELD

        self._real_attrs[name] = nested
        return nested

    def __setattr__(self, name, value):
        if name.startswith("_"):
            super().__setattr__(name, value)
        else:
            self._real_attrs[name] = value
            if isinstance(value, (str, int, float)):
                if isinstance(value, str):
                    assigned_type = "string"
                elif isinstance(value, int):
                    assigned_type = "int"
                elif isinstance(value, float):
                    assigned_type = "float"
                self._real_attrs[f"__meta__::{name}"] = {"assigned_type": assigned_type}
            print(f"?? Set attr: {self._name}.{name} = {value}")

    def __getitem__(self, key):
        if key in self._real_items:
            return self._real_items[key]
        self._history.append(("item", key))
        node = MeTTaLog(f"{self._name}[{repr(key)}]", self._depth + 1, self._history[:], self._swimod, autounbox=self._autounbox)
        node._meta["kind"] = MeTTaMemberKind.ITEM
        MeTTaLog._trace.append(("access", node._name, ("item", key), self._swimod, None))
        return node

    def __setitem__(self, key, value):
        self._real_items[key] = value
        print(f"?? Set item: {self._name}[{repr(key)}] = {value}")

    def __call__(self, *args, **kwargs):
        # Prevent method call if a real value (like a number or string) is stored instead
        if "__result__" in self._real_attrs and not callable(self._real_attrs.get("__callable__")):
            return self.result()

        # Main dispatcher when a MeTTaLog node is called
        # Branches based on self._backend:
        swimod = self._swimod or "user"

        method = self._real_attrs.get("__callable__")
        if self._backend == MeTTaBackend.ORIGINAL_METHOD and callable(method):
            sig = inspect.signature(method)
            #if list(sig.parameters.values())[0].name == 'self':
            #    params = list(sig.parameters.values())
            if params and params[0].name == 'self':
                return method(self, *args, **kwargs)
            else:
                return method(*args, **kwargs)

        # Fall back to symbolic call
        result = metta_client.apply_once(
            swimod=swimod,            
            path=self._history[:],
            selfref=self,
            args=args,
            kwargs=kwargs,
            fullpath=self._name
        )

        node = MeTTaLog(f"{self._name}()", self._depth + 1, self._history[:], swimod, autounbox=self._autounbox)
        node._real_attrs["__result__"] = result
        node._meta["kind"] = MeTTaMemberKind.CALL
        if isinstance(result, (str, int, float, bool)):
            node._meta["return_type"] = type(result).__name__

        if self._autounbox and self._meta.get("kind") in {MeTTaMemberKind.CALL, MeTTaMemberKind.FIELD,
                                                          MeTTaMemberKind.ITEM} and isinstance(result,
                                                                                               (str, int, float)):
            return result
        return node


    def result(self):
        return self._real_attrs.get("__result__")

    def add_method(self, name, func):
        def wrapped(*args, **kwargs):
            self._history.append(("method", {"name": name, "args": args, "kwargs": kwargs}))
            result = func(self, *args, **kwargs)
            node = MeTTaLog(f"{self._name}.{name}()", self._depth + 1, self._history[:], autounbox=self._autounbox)
            node._real_attrs["__result__"] = result
            node._meta["kind"] = MeTTaMemberKind.METHOD
            node._meta["return_type"] = type(result).__name__
            return result if self._autounbox and isinstance(result, (str, int, float)) else node

        self._real_attrs[name] = wrapped
        self._methods[name] = wrapped

    def simulate(self, new_name=None):
        copy = MeTTaLog(
            name=new_name or self._name,
            depth=self._depth,
            history=self._history[:],
            swimod=self._swimod,
            autounbox=self._autounbox
        )
        copy._meta = self._meta.copy()
        copy._methods = self._methods.copy()
        for key, val in self._real_attrs.items():
            if isinstance(val, MeTTaLog):
                copy._real_attrs[key] = val.simulate()
            else:
                copy._real_attrs[key] = val
        copy._real_items = dict(self._real_items)
        return copy

    def simulate_module(self, new_name=None):
        mod = self.simulate(new_name=new_name)
        mod._meta["kind"] = MeTTaMemberKind.MODULE
        mod._history = []
        return mod

    def __repr__(self):
        return f"<MeTTaLog '{self._name}'>"

    def __str__(self):
        result = self._real_attrs.get("__result__")
        return result if isinstance(result, str) else f"<MeTTaLog: {self._name}>"

    def __int__(self):
        result = self._real_attrs.get("__result__")
        if isinstance(result, int):
            return result
        raise TypeError(f"{self._name} result is not an int")

    def __float__(self):
        result = self._real_attrs.get("__result__")
        if isinstance(result, (float, int)):
            return float(result)
        raise TypeError(f"{self._name} result is not a float")

    def __dir__(self):
        # Blend mocked and dynamically created members
        static = set(self._real_attrs.keys())
        return sorted(static | MeTTaLog._mock_attrs)

    @staticmethod
    def report():
        print("\n=== MeTTaLog Trace ===")
        for kind, name, path, swimod, result in MeTTaLog._trace:
            print(f"{kind.upper()} {name} via {swimod} ? {result}")

    @staticmethod
    def to_prolog_facts():
        return MeTTaLog.to_atomspace()

    @staticmethod
    def to_atomspace():
        def render(entry):
            kind, name, event, swimod, result = entry
            if kind == "access":
                typ, val = event
                if typ == "attr":
                    return f"(access '{name} (attr '{val}))"
                elif typ == "item":
                    return f"(access '{name} (item {repr(val)}))"
            elif kind == "call":
                calls = [e for e in event if isinstance(e, tuple) and e[0] == "call"]
                if not calls:
                    return f"; malformed call event for {name}: no 'call' entry"
                call_data = calls[-1][1]
                if isinstance(call_data, dict):
                    args = call_data.get("args", ())
                    kwargs = call_data.get("kwargs", {})
                elif isinstance(call_data, tuple) and len(call_data) == 2:
                    args, kwargs = call_data
                else:
                    return f"; unknown call format: {call_data}"
                args_sexpr = " ".join(repr(a) for a in args)
                kwargs_sexpr = " ".join(f":{k} {repr(v)}" for k, v in kwargs.items())
                return f"(call '{name} ({args_sexpr} {kwargs_sexpr}))"
            return f"; unknown {entry}"

        facts = [render(e) for e in MeTTaLog._trace]

        # Emit kind information and return type facts for each created node
        for obj in globals().values():
            if isinstance(obj, MeTTaLog):
                kind = obj._meta.get("kind")
                if kind:
                    facts.append(f"(member-kind '{obj._name} '{kind.name.lower()})")
                return_type = obj._meta.get("return_type")
                if return_type:
                    facts.append(f"(has-return-type '{obj._name} '{return_type})")

                return "\n".join(facts)



# Optional dynamic proxy importer for real Python modules
# Dynamically create a MeTTaLog module tree from a real Python module
# Includes synthesized metadata to reflect what's callable vs constant

def create_mettalog_proxy(name, real_module, autounbox=True):
    """
    Creates a MeTTaLog module proxy that wraps a real Python module.
    Always binds callable members as original methods.
    """
    """
    Creates a MeTTaLog module proxy that wraps a real Python module,
    tracking whether each item is a synthesized function or constant.
    """
    backend_type = MeTTaBackend.ORIGINAL_METHOD if getattr(real_module, "__module__", "").startswith(
        "mettalog") else MeTTaBackend.METTA
    proxy = MeTTaLog(name=name, autounbox=autounbox, backend=backend_type)
    proxy._meta["mocked"] = False
    proxy.simulate_instance(real_module)
    proxy.simulate_class(name)
    proxy._meta["kind"] = MeTTaMemberKind.MODULE

    for attr_name in dir(real_module):
        if attr_name.startswith("_"):
            continue
        attr = getattr(real_module, attr_name)

        if isinstance(attr, types.FunctionType) or callable(attr):
            def make_func(f, fname):
                def method(self, *args, **kwargs):
                    result = f(*args, **kwargs)
                    node = MeTTaLog(f"{self._name}.{fname}()", self._depth + 1, autounbox=self._autounbox)
                    node._real_attrs["__result__"] = result
                    node._meta["kind"] = MeTTaMemberKind.FUNC
                    node._meta["return_type"] = type(result).__name__
                    return result if self._autounbox and isinstance(result, (str, int, float)) else node

                return method

            wrapped_func = make_func(attr, attr_name)
            # Always wrap and bind original method logic
            proxy._real_attrs[attr_name] = wrapped_func_proxy = MeTTaLog(
                name=f"{proxy._name}.{attr_name}",
                autounbox=autounbox,
                backend=MeTTaBackend.ORIGINAL_METHOD
            )
            wrapped_func_proxy.bind_original(attr)
            
            
        # Always bind real Python function unless mocking
        if isinstance(attr, types.FunctionType) or (getattr(attr, '__module__', '') or '').startswith('mettalog'):
            if isinstance(wrapped_func, MeTTaLog):
                wrapped_func.bind_original(attr)
            proxy._real_attrs[f"__meta__::{attr_name}"] = {"synthesized": "function", 
               "mocked": proxy._meta.get("mocked", False)}
            proxy._methods[attr_name] = wrapped_func
            proxy._real_attrs[attr_name].__callable__ = attr  # attach original reference
            if hasattr(proxy._real_attrs[attr_name], '__call__'):
                proxy._real_attrs[attr_name]._backend = MeTTaBackend.ORIGINAL_METHOD
            proxy._real_attrs[f"__meta__::{attr_name}"] = {"synthesized": "function"}

        elif isinstance(attr, (str, int, float, bool)):
            proxy._real_attrs[attr_name] = attr
            proxy._real_attrs[f"__meta__::{attr_name}"] = {
                "assigned_type": type(attr).__name__,
                "synthesized": "constant"
            }

    return proxy



# corrected utf-8 glyphs (✅ → ✔️, ❌ → ✖️, ❓ → ❔, etc.)
# replaced all instances of "?" with appropriate UTF-8 symbols (in this context, usually "🔍", "✔️", "✖️", "❔", etc.)
# below is continuation with the corrected symbols replacing "??"

def quick_test_facades():
    """
    Runs a quick self-test by:
    - Wrapping the `random` module with MeTTaLog
    - Cloning it into a new module node
    - Attempting to call and print each top-level method's result
    - Returning the simulated module
    """
    print("=== 🔍 QUICK SELFTEST: Cloning random module ===")
    import random
    rand_instance = random.Random()
    rand_facade = create_mettalog_proxy("mettalog.random.Random", rand_instance, autounbox=True)
    rand_proxy = create_mettalog_proxy("mettalog.random", random, autounbox=False)

    for attr_name in dir(rand_instance):
        if attr_name.startswith("_"):
            continue
        attr = getattr(rand_instance, attr_name)
        if callable(attr):
            rand_facade.add_method(attr_name, attr)
            rand_attr = rand_facade._real_attrs.get(attr_name)
            if isinstance(rand_attr, MeTTaLog):
                try:
                    sig = inspect.signature(attr)
                    params = list(sig.parameters.values())
                    if len(params) == 0:
                        test_result = attr()
                    else:
                        print(f"   ❔ skipping test call: {attr_name} requires args")
                        test_result = None
                    rand_attr._meta["return_type"] = type(test_result).__name__
                except Exception:
                    pass
        else:
            rand_facade._real_attrs[attr_name] = attr
            rand_facade._real_attrs[f"__meta__::{attr_name}"] = {
                "assigned_type": type(attr).__name__,
                "synthesized": "constant"
            }

    rand_proxy._real_attrs["Random"] = rand_facade
    rand_proxy._real_attrs["__meta__::Random"] = {"synthesized": "facade"}

    rand_facade.simulate_instance(rand_instance)
    rand_facade.simulate_class("mettalog.random.Random")
    simulate = rand_proxy.simulate_module("random")

    for k in sorted(simulate._real_attrs):
        if not k.startswith("__"):
            print(f" ✔️ {simulate._name}.{k}")
            attr = getattr(simulate, k)
            if callable(attr):
                try:
                    result = attr().result()
                    print(f"   ✔️ result: {result}")
                except Exception as e:
                    print(f"   ✖️ error calling: {e}")

    return simulate


def quick_selftest():
    # maybe..
    # quick_test_facades()
    print(mettalog.run("!(+ 1 1)"))
    print(mettalog.run("!(import! &self mettamorph)"))

    print(heMeTTa.run("!(+ 1 1)"))
    print(heMeTTa.run("!(import! &self mettamorph)"))
    print(heMeTTa.run("!(import! &self mettalog)"))

    print(theMORKSpace.query("(+ 1 1)"))


# Global root instance
mettalog = MeTTaLog()
mettalog._autounbox = True
heMeTTa = MeTTa()
theMORKSpace = MORKSpace(mettalog if True else heMeTTa)



if __name__ == "__main__":
    # Call quick_selftest directly instead of routing through mettalog.core
    print(quick_selftest())
    print(MeTTaLog.to_atomspace())
    print(MeTTaLog.report())

