
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


from hyperon import *
metta = MeTTa()
expr1 = metta.parse_single('(+ 1 S)')
expr2 = E(S('+'), S('1'), S('S'))
print('Expr1: ', expr1)
print('Expr2: ', expr2)
print('Equal: ', expr1 == expr2)
for atom in expr1.get_children():
    print(f'type({atom})={type(atom)}')


    

;  !(assertEqualToResult ( )  (  )  ) 
