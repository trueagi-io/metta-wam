
import janus_swi

class MettaLog:
    def __init__(self, localPath="default", prelude=None, debug=False):
        self.localPath = localPath
        self.debug = debug
        self._history = []

        if prelude:
            self.load(prelude)

    def run(self, code: str):
        if self.debug:
            print(f"[{self.localPath}] run: {code}")
        self._history.append(code)
        return self._history

    def load(self, code: str):
        """Alias for .run but semantically for setup"""
        return self.run(code)

    def query(self, expr: str):
        if self.debug:
            print(f"[{self.localPath}] query: {expr}")
        results = run(expr)
        if self.debug:
            for i, r in enumerate(results):
                print(f"  [{i}] => {r}")
        return results

    def clone(self, localPath=None):
        clone_path = localPath or f"{self.localPath}_clone"
        new_facade = MettaLog(localPath=clone_path, debug=self.debug)
        for code in self._history:
            new_facade.run(code)
        return new_facade

    # --- MeTTaLog concurrency primitives ---

    def transaction(self, expr: str):
        """Run an expression in a transaction context"""
        return self.query(f"(thread:transaction! {expr})")

    def snapshot(self, expr: str):
        """Evaluate an expression in a snapshot context"""
        return self.query(f"(thread:snapshot! {expr})")

    def spawn(self, expr: str):
        """Spawn a new thread to evaluate an expression"""
        return self.query(f"(thread:spawn! {expr})")

    def async_(self, expr: str):
        """Start async task (returns token/handle)"""
        return self.query(f"(thread:async! {expr})")

    def await_(self, token: str):
        """Wait for a result from an async task"""
        return self.query(f"(thread:await! {token})")

    def shared(self, *exprs: str):
        return self.query(f"(shared {' '.join(exprs)})")

    def isolated(self, *exprs: str):
        return self.query(f"(isolated {' '.join(exprs)})")

    def mutex(self, *exprs: str):
        return self.query(f"(mutex-run! {' '.join(exprs)})")

    def __repr__(self):
        return f"<MettaLog localPath={self.localPath}>"

