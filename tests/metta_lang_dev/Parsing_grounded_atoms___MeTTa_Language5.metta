
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import *
# A function to be registered
def dup_str(s, n):
    r =
    for i in range(n):
        r += s
    return r
metta = MeTTa()
# Create an atom. "dup-str" is its internal name
dup_str_atom = OperationAtom(, dup_str)
# Interpreter will call this operation atom provided directly
print(metta.evaluate_atom(E(dup_str_atom, ValueAtom(), ValueAtom(3))))
# Let us add a function calling `dup-str`
metta.run(
)
# The parser doesn't know it, so dup-str will not be reduced
print(metta.run(
))
# Now the token is registered. New expression will be reduced.
# However, `(= (test-dup-str) (dup-str "a" 2))` was added
# before `dup-str` token was introduced. Thus, it will still
# remain not reduced.
metta.register_atom(, dup_str_atom)
print(metta.run(

;  !(assertEqualToResult ( )  (  )  ) 
