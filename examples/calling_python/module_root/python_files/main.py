from hyperon import *
from hyperon.ext import register_atoms
from hyperon.atoms import OperationAtom


def reduct(metta: MeTTa, expr):
    return metta.parse_all(expr)


@register_atoms(pass_metta=True)
def main(metta):
    reductFunc = OperationAtom(
        "py_reduct",
        lambda expr: reduct(metta, expr),
        ["Expression", "Expression"],
        unwrap=False,
    )

    return {
        r"py_reduct": reductFunc,
    }
