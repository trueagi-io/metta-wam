from hyperon.ext import register_atoms
from hyperon import *

@register_atoms
def count_atoms():
    count = OperationAtom(
        'tuple-count', 
        lambda atom: [ValueAtom(len(atom.get_children()), 'Number')], [AtomType.ATOM, "Number"], 
        unwrap=False)
    return {
        r"tuple-count": count
    }
