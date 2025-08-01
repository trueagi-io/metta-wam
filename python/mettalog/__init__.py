import sys
import os
import types
from types import SimpleNamespace
import importlib
import importlib.util
from .core import MeTTaLog, do_register_atoms
from .import_hook import *
from .core import *
#from .runtime import *
#from .runtime import load_functions as mod_load_functions
#import .runtime
#from .vspace import *

UseNativeTypeResults = False

_dynamic_symbols = {}  # cache for symbolic stubs

# 1. Support direct `from mettalog.dynamic import Foo`
def __getattr__(name):
    """Access dynamic top-level attributes like mettalog.Foo"""
    if name not in _dynamic_symbols:
        _dynamic_symbols[name] = MeTTaLog(name)
    return _dynamic_symbols[name]

__path__ = []  # Optional: behave as a namespace package


class TopLevelProxy(types.ModuleType):
    def __init__(self, name, path):
        super().__init__(name)
        self.__real_path__ = path
        self.__loaded__ = {}

    def __getattr__(self, name):
        if name in self.__loaded__:
            return self.__loaded__[name]

        file_path = os.path.join(self.__real_path__, f"{name}.py")
        dir_path = os.path.join(self.__real_path__, name)

        if os.path.isfile(file_path) or os.path.isdir(dir_path):
            spec = importlib.util.spec_from_file_location(
                f"{self.__name__}.{name}",
                file_path if os.path.isfile(file_path) else os.path.join(dir_path, "__init__.py")
            )
            mod = importlib.util.module_from_spec(spec)
            spec.loader.exec_module(mod)
            self.__loaded__[name] = mod
            return mod

        if name[0].isupper():
            class DynamicClass(MeTTaLog):
                def __init__(self, *args, **kwargs):
                    super().__init__(name)
                    self.args = args
                    self.kwargs = kwargs
                    self.name = name

                def __repr__(self):
                    return f"<{name} args={self.args} kwargs={self.kwargs}>"

            self.__loaded__[name] = DynamicClass
            return DynamicClass
        else:
            proxy = MeTTaLog(name)
            self.__loaded__[name] = proxy
            return proxy

    def __dir__(self):
        try:
            entries = set(os.listdir(self.__real_path__))
            files = {f[:-3] for f in entries if f.endswith(".py")}
            dirs = {d for d in entries if os.path.isdir(os.path.join(self.__real_path__, d))}
            return sorted(files | dirs | set(self.__loaded__.keys()))
        except FileNotFoundError:
            return list(self.__loaded__.keys())


# 2. Create a dynamic submodule like mettalog.hyperon

class _DynamicSubmodule(types.ModuleType):
    def __init__(self, fullname):
        super().__init__(fullname)
        self.__name__ = fullname
        self._symbol_cache = {}
        self._explicit_all = {
            "ValueAtom", "Environment", "GroundedAtom",
            "OperationAtom", "Char", "MeTTa", "register_atoms"
             "E", "Environment", "Char", "Atom", "V", "S"

        }

        try:
            self._real_module = importlib.import_module("hyperon")
        except ImportError:
            self._real_module = None

        # Preload known exports
        for name in self._explicit_all:
            if self._real_module and hasattr(self._real_module, name):
                value = getattr(self._real_module, name)
            else:
                value = MeTTaLog(name)
            self._symbol_cache[name] = value
            setattr(self, name, value)

    def __getattr__(self, name):
        if name in self._symbol_cache:
            return self._symbol_cache[name]
        if self._real_module and hasattr(self._real_module, name):
            value = getattr(self._real_module, name)
        else:
            value = MeTTaLog(name)
        self._symbol_cache[name] = value
        return value

    def __dir__(self):
        # Combine everything known or potentially usable
        return sorted(
            set(self._symbol_cache.keys()) |
            self._explicit_all |
            self._real_names
        )

    #@property
    #def __all__(self):
        # Prime all names from real Hyperon and explicit list
    #    all_names = self._explicit_all | self._real_names | set(self._symbol_cache.keys())
    #    for name in all_names:
    #        getattr(self, name)
    #    return sorted(all_names)


# 4. Loader that installs dynamic submodules like mettalog.dynamic.hyperon
class _DynamicModuleLoader:
    def __init__(self, fullname):
        self.fullname = fullname

    def create_module(self, spec):
        return _DynamicSubmodule(self.fullname)

    def exec_module(self, module):
        sys.modules[self.fullname] = module


# 3. Custom finder to resolve dynamic modules
class _DynamicModuleFinder:
    def find_spec(self, fullname, path, target=None):
        if fullname.startswith("mettalog.dynamic.") and fullname.count('.') == 2:
            return importlib.util.spec_from_loader(fullname, _DynamicModuleLoader(fullname), origin="dynamic")
        return None

    def find_spec_from_file(self, fullname, path, target=None):
        if fullname == "mettalog":
            base_path = os.path.dirname(__file__)
            return types.SimpleNamespace(
                name=fullname,
                loader=TopLevelLoader(base_path),
                origin="virtual-mettalog",
                is_package=True
            )
        return None

class TopLevelLoader:
    def __init__(self, path):
        self._path = path

    def create_module(self, spec):
        return TopLevelProxy(spec.name, self._path)

    def exec_module(self, module):
        pass

# Register all synthetic submodules with __all__ and preloaded attributes
_exports = ["E", "MeTTa", "register_atoms", "Environment", "Char"]

for submod in [
    "dynamic",
    "hyperon",
    "hyperon.ext",
    "hyperon.atoms"
]:
    mod = _DynamicSubmodule(f"mettalog.{submod}")
    mod.__all__ = _exports
    sys.modules[f"mettalog.{submod}"] = mod

# 5. Install once
if False:
    if not any(isinstance(f, _DynamicModuleFinder) for f in sys.meta_path):
        sys.meta_path.insert(0, _DynamicModuleFinder())

if False:
 sys.meta_path.insert(0, TopLevelFinder())
