from enum import Enum, auto

# Controls which backend is used to resolve method calls
class MeTTaBackend(Enum):
    METTA = auto()
    LOGGED_FACADE = auto()
    ORIGINAL_METHOD = auto()
import types

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
    ATTR = auto()
...

# Global root instance
mettalog = MeTTaLog()
mettalog._autounbox = True
