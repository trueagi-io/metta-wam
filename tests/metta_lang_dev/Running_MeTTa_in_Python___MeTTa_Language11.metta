
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
from hyperon import E, S, V, MeTTa
metta = MeTTa()
expr_atom = E(S(), V(), S())
print(expr_atom) # (Parent $x Bob)
print(expr_atom.get_metatype()) # AtomKind.EXPR
print(expr_atom.get_children()) # [Parent, $x, Bob]
# Let us use expr_atom in the query
metta = MeTTa()
metta.run(
)
print(metta.space().query(expr_atom)) # [{ $x &lt;- Pam }, { $x &lt;- Tom }]
result = metta.run()[0]
print(result) # [(Retrieved Tom) (Retrieved Pam)]
# Ignore 'Retrieved' in expressions and print Pam, Tom
for r in result:
	print(r.get_children()[1])

;  !(assertEqualToResult ( )  (  )  ) 
