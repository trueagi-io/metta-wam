import os
import atexit
import tempfile
import collections.abc
from hyperon import *
from hyperon.ext import register_atoms
from hyperon.atoms import OperationAtom
import janus_swi as janus


PROLOG_BOOTSTRAP = r"""
:- dynamic facaded/0.
:- dynamic user:hyperlog_engine_state/2.
:- use_module(library(debug)).
:- nodebug(hyperlog).

startup_for_py(EngineID) :-
    assertz(user:hyperlog_engine_state(EngineID, running)).

shutdown_for_py(EngineID) :-
    retractall(user:hyperlog_engine_state(EngineID, _)).

set_facade_mode(true) :- !,
    (facaded -> true ; assertz(facaded)).
set_facade_mode(false) :- !,
    retractall(facaded),
    ensure_mettalog_modules.

ensure_mettalog_modules :-
    ( current_predicate(eval_args/3) -> true ; ensure_loaded(library(metta_rt)) ).

parse_metta_expr(X, X) :- facaded, !.
parse_metta_expr(String, Result) :-
    ensure_mettalog_modules,
    user:py_parse_sexpr(String, Result).

parse_for_py(_ID, S, P) :- parse_metta_expr(S, P).
parse_all_for_py(_ID, S, P) :- parse_metta_expr(S, P).

eval_sexpr_for_py(_ID, Input, Input) :- facaded, !.
eval_sexpr_for_py(ID, Input, Result) :-
    ensure_mettalog_modules,
    user:py_engine_eval_args(ID, Input, Result).

run_for_py(ID, String, Result) :- metta_eval(ID, String, Result).

metta_eval(_ID, Eval, Eval) :- facaded, !.
metta_eval(ID, Eval, Result) :-
    user:py_engine_eval_args(ID, Eval, Result).

:- ensure_mettalog_modules.

"""


def _write_temp_prolog_file(code: str):
    tmp = tempfile.NamedTemporaryFile("w+", suffix=".pl", delete=False)
    tmp.write(code)
    tmp.flush()
    return tmp.name


def _initialize_prolog_driver():
    temp_file = _write_temp_prolog_file(PROLOG_BOOTSTRAP)
    janus.consult(temp_file)


# ✅ Prolog bootstrap happens at module load (similar to __init__)
_initialize_prolog_driver()


def pretty_print_result(result, prefix="  => "):
    if isinstance(result, collections.abc.Iterator) and not isinstance(result, (str, bytes, list, tuple, dict, set)):
        try:
            for i, item in enumerate(result):
                print(f"{prefix}[{i}] {item}")
        except Exception as e:
            print(f"Error iterating result: {e}")

            print(f"{prefix}{result}")
    elif isinstance(result, (list, tuple, set)):
        for i, item in enumerate(result):
            print(f"{prefix}[{i}] {item}")
    else:
        print(f"{prefix}{result}")


class MeTTaLogImpl:
    _instance_counter = 0

    def __init__(self, localPath="default", prelude=None, debug=False, facade=False):
        self.localPath = localPath
        self.debug = debug
        self.facade = facade
        self._history = []

        MeTTaLogImpl._instance_counter += 1
        self.engine_id = f"hyperlog_{MeTTaLogImpl._instance_counter:04d}"

        if self.debug:
            janus.cmd("prolog", "debug", "hyperlog")
        else:
            janus.cmd("prolog", "nodebug", "hyperlog")

        if self.debug:
            print(f"[{self.engine_id}] starting engine (facade={self.facade})")

        janus.apply("user", "startup_for_py", self.engine_id)
        janus.apply("user", "set_facade_mode", self.facade)

        if prelude:
            self.load(prelude)

        atexit.register(self.shutdown)

    def set_debug(self, flag: bool):
        self.debug = flag
        print(f"[{self.engine_id}] Debug mode {'ON' if flag else 'OFF'}")
        if flag:
            janus.cmd("prolog", "debug", "hyperlog")
        else:
            janus.cmd("prolog", "nodebug", "hyperlog")

    def set_facade(self, flag: bool):
        if self.debug:
            print(f"[{self.engine_id}] set_facade({flag})")
        janus.apply("user", "set_facade_mode", flag)
        self.facade = flag
        if not flag:
            janus.apply("user", "ensure_mettalog_modules")

    def shutdown(self):
        if self.debug:
            print(f"[{self.engine_id}] shutdown_for_py")
        try:
            janus.apply("user", "shutdown_for_py", self.engine_id)
        except Exception as e:
            if self.debug:
                print(f"[{self.engine_id}] Shutdown error: {e}")

    def parse_all(self, code):
        if self.debug:
            print(f"[{self.engine_id}] parse_all: {code}")
        return janus.apply("user", "parse_all_for_py", self.engine_id, code)

    def parse(self, code):
        if self.debug:
            print(f"[{self.engine_id}] parse: {code}")
        return janus.apply_once("user", "parse_for_py", self.engine_id, code)

    def run(self, code):
        if self.debug:
            print(f"[{self.engine_id}] run: {code}")
        self._history.append(code)
        return janus.apply("user", "run_for_py", self.engine_id, code)

    def load(self, code):
        return self.run(code)

    def query(self, code):
        if self.debug:
            print(f"[{self.engine_id}] query: {code}")
        result = self.run(code)
        pretty_print_result(result)
        return result

    def clone(self, localPath=None):
        clone_path = localPath or self.localPath
        new_facade = MeTTaLogImpl(localPath=clone_path, debug=self.debug, facade=self.facade)
        for code in self._history:
            new_facade.run(code)
        return new_facade

    def load(self, file): return self.query(f"(include {file})")
    def import_(self, file): return self.query(f"(import &self {file})")
    def transaction(self, code): return self.query(f"(thread:transaction! {code})")
    def snapshot(self, code): return self.query(f"(thread:snapshot! {code})")
    def spawn(self, code): return self.query(f"(thread:spawn! {code})")
    def async_(self, code): return self.query(f"(thread:async! {code})")
    def await_(self, token): return self.query(f"(thread:await! {token})")
    def shared(self, *exprs): return self.query(f"(shared {' '.join(exprs)})")
    def isolated(self, *exprs): return self.query(f"(isolated {' '.join(exprs)})")
    def mutex(self, *exprs): return self.query(f"(mutex-run! {' '.join(exprs)})")

    def __repr__(self):
        return f"<MeTTaLogImpl id={self.engine_id} facade={self.facade} debug={self.debug}>"


def main():
    # 🚀 Initialize a MeTTaLog engine instance
    # Enables debug output and starts in facade mode (no evaluation, just passthrough)
    print("creating mettalog engine...")
    metta = MeTTaLogImpl(debug=True, facade=True)

    # ➕ Run a simple arithmetic expression
    # This will just return the parsed form if in facade mode
    res = metta.run("!(+ 10 5)")
    pretty_print_result(res)

    # 🧠 Define a rule: (f $x) = (+ $x 40)
    # This will be stored for later application
    metta.run("(= (f $x) (+ $x 40))")

    # 🔁 Apply the rule: (f 2)
    # This should produce (+ 2 40) → 42 (if not in facade)
    res = metta.run("!(f 2)")
    pretty_print_result(res)

    # 📦 Parse an expression: "(+ 2 2)"
    # Parsing just returns the syntax tree
    parsed = metta.parse("(+ (f 0) 2)")
    pretty_print_result(parsed)

    # 🧮 Evaluate the parsed expression
    # This will run the expression if not in facade mode
    res = metta.run(parsed)
    pretty_print_result(res)


if __name__ == "__main__":
    main()

