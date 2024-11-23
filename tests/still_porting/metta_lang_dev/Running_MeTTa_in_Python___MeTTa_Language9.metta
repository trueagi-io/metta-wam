
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import S, SymbolAtom, Atom
symbol_atom = S()
print(symbol_atom.get_name()) # MyAtom
print(symbol_atom.get_metatype()) # AtomKind.SYMBOL
print(type(symbol_atom)) # SymbolAtom
print(isinstance(symbol_atom, SymbolAtom)) # True
print(isinstance(symbol_atom, Atom)) # True

;  !(assertEqualToResult ( )  (  )  ) 
