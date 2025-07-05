import sys
import os
import types
import importlib
import importlib.util
from .core import MeTTaLog

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

class TopLevelFinder:
    def find_spec(self, fullname, path, target=None):
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

sys.meta_path.insert(0, TopLevelFinder())
