from hyperon import *
from hyperon.ext import register_atoms
from hyperon.atoms import OperationAtom
import janus_swi as janus

class MeTTaLogImpl:

    def __init__(self, consult_file='mettalog_driver.pl', localPath="default", prelude=None, debug=False):
        self.localPath = localPath
        self.debug = debug
        self._history = []
        if self.debug:
            print(f"[MeTTaLog] consulting {consult_file}")
        janus.consult(consult_file)
        if prelude:
            self.load(prelude)

    def parse_all(self, code):
        """Parse a string into a MeTTa term."""
        if self.debug:
            print(f"[MeTTaLog] parse: {code}")
        return janus.apply("user", "parse_all", code)

    def run(self, code):
        """Evaluate a MeTTa expression or string."""
        if self.debug:
            print(f"[MeTTaLog] run: metta_eval('{code}', Result)")
        self._history.append(code)
        return janus.apply("user", "run_for_py", code)

    def load(self, code):
        """Alias for .run but semantically for setup"""
        return self.run(code)

    def query(self, code):
        if self.debug:
            print(f"[{self.localPath}] query: {code}")
        results = self.run(code)
        if self.debug:
            for i, r in enumerate(results):
                print(f"  [{i}] => {r}")
        return results

    def clone(self, localPath=None):
        clone_path = localPath or f"{self.localPath}_clone"
        new_facade = MeTTaLogImpl(localPath=clone_path, debug=self.debug)
        for code in self._history:
            new_facade.run(code)
        return new_facade

    # --- MeTTaLog concurrency primitives ---

    def transaction(self, code):
        """Run an expression in a transaction context"""
        return self.query(f"(thread:transaction! {code})")

    def snapshot(self, code):
        """Evaluate an expression in a snapshot context"""
        return self.query(f"(thread:snapshot! {code})")

    def spawn(self, code):
        """Spawn a new thread to evaluate an expression"""
        return self.query(f"(thread:spawn! {code})")

    def async_(self, code):
        """Start async task (returns token/handle)"""
        return self.query(f"(thread:async! {code})")

    def await_(self, token):
        """Wait for a result from an async task"""
        return self.query(f"(thread:await! {token})")

    def shared(self, *exprs):
        return self.query(f"(shared {' '.join(exprs)})")

    def isolated(self, *exprs):
        return self.query(f"(isolated {' '.join(exprs)})")

    def mutex(self, *exprs):
        return self.query(f"(mutex-run! {' '.join(exprs)})")

    def __repr__(self):
        return f"<MeTTaLogImpl localPath={self.localPath}>"

