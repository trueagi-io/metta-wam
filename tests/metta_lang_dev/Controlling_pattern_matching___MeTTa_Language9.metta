
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (let $x (+ 1 2) (quote $x)) 
; (quote 3)
; readyworks
!(assertEqualToResult ( let $x (+ 1 2) (quote $x) )  ( (quote 3) )  )


(: Z Nat)

;! (get-metatype (get-type Z)) 
; (get-type Z) is Expression
; readyworks
!(assertEqualToResult ( get-metatype (get-type Z) )  ( (Expression) )  )

;! (let $x (get-type Z) (get-metatype $x)) 
; Nat is Symbol
; readyworks
!(assertEqualToResult ( let $x (get-type Z) (get-metatype $x) )  (  (Symbol) )  )




;  !(assertEqualToResult ( )  (  )  ) 
