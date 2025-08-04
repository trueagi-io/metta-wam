import os
import atexit

import janus_swi as janus

def _initialize_prolog_driver():

    dir = METTALOG_DIR

    janus.consult("hyperlog", 
f"""

% :- set_default_module(user).
:- use_module(library(debug)).
:- nodebug(hyperlog).

ensure_mettalog_modules:- user:user_ensure_mettalog_modules.
user_ensure_mettalog_modules :-
    %pack_list_installed,
    user:ensure_loaded(library(metta_rt)),
    %pwd,
    %user:listing(user:metta_atom_asserted/2),
    %user:listing(user:hyperlog_startup/1),
    % listing(user:metta_function_asserted),
    % listing(user:compiled_clauses),          
    !.


:- initialization(user:ensure_mettalog_modules).

""")


METTALOG_DIR = os.environ.get("METTALOG_DIR", ".")
VERBOSE_DEBUG = os.environ.get("METTALOG_VERBOSE")
# 0 = for scripts/demos
# 1 = developer
# 2 = verbose_debugger
VERBOSE_DEBUG = 1

# ✅ Prolog bootstrap happens at module load (similar to __init__)
_initialize_prolog_driver()


def pretty_print_result(result, prefix="  => "):
    if not isinstance(result, (str, bytes)):
        try:
            # Attempt indexed iteration (like for generators or custom iterables)
            for i, item in enumerate(result):
                print(f"{prefix}[{i}] {item}  type:{type(item)}")
            return
        except Exception: pass

        try:
            # Attempt flat iteration (like for sets, dict keys, etc.)
            for item in result:
                print(f"{prefix}{item} type:{type(item)}")
            return

        except Exception: pass

    # Final fallback: just print the object
    print(f"{prefix}{result}  type:{type(result)}")




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

        janus.cmd("user", "hyperlog_startup", self.engine_id)
        janus.cmd("user", "hyperlog_set",self.engine_id,"localPath",localPath)

        if prelude:
            self.load(prelude)

        atexit.register(self.shutdown)

    def set_debug(self, flag: bool):
        self.debug = flag
        if flag:
            janus.cmd("prolog", "debug", "hyperlog")
        else:
            janus.cmd("prolog", "nodebug", "hyperlog")

    def shutdown(self): 
        try:
            janus.cmd("user", "hyperlog_shutdown", self.engine_id)
        except Exception as e:
            if self.debug:
                print(f"[{self.engine_id}] Shutdown error: {e}")

    def parse_all(self, code):
        if self.debug:
            print(f"[{self.engine_id}] parse_all: {code}")
        return janus.apply("user", "hyperlog_parse_all", self.engine_id, code)

    def parse(self, code):
        if self.debug:
            print(f"[{self.engine_id}] parse: {code}")
        return janus.apply_once("user", "hyperlog_parse", self.engine_id, code)

    def run(self, code):
        if self.debug:
            print(f"[{self.engine_id}] run: {code}")
        self._history.append(code)
        return list(janus.apply("user", "hyperlog_run", self.engine_id, code))

    def load(self, code):
        return self.run(code)

    def query(self, code):
        if self.debug:
            print(f"[{self.engine_id}] query: {code}")
        self._history.append(code)
        return janus.apply("user", "hyperlog_query", self.engine_id, code)

    def clone(self, localPath=None):
        clone_path = localPath or self.localPath
        new_facade = MeTTaLogImpl(localPath=clone_path, debug=self.debug, facade=self.facade)
        for code in self._history:
            new_facade.run(code)
        return new_facade
    
    def import_(self, file): return self.query(f"(import! &self {file})")
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
    metta = MeTTaLogImpl(debug=True, facade=False)
    print("created")
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

    metta.run("!(add-atom &self (= (g $x) (+ $x 40)))")
    res = metta.run("!(g 3)")
    pretty_print_result(res)

    # res = metta.run("!(help!)")
    # pretty_print_result(res)


    # 📦 Parse an expression: "(+ 2 2)"
    # Parsing just returns the syntax tree
    parsed = metta.parse("!(+ (f 0) 2)")
    pretty_print_result(parsed)

    # 🧮 Evaluate the parsed expression
    # This will run the expression if not in facade mode
    res = metta.run(parsed)
    pretty_print_result(res)

    #janus.cmd("user","prolog")
    import sys
    if len(sys.argv) > 1:
        first_arg = sys.argv[1]
        # python -m hyperlog repl
        # or
        # python -m hyperlog prolog
        janus.cmd("user", first_arg)
    else:
        janus.cmd("janus","py_shell")



if __name__ == "__main__":
    main()

