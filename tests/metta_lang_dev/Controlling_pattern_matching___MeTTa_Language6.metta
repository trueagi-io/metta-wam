
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(: foo (-> Number Bool))
(= (foo $x) (+ $x 1))
;! (get-type (foo $x)) 
; Bool
; notready
!(assertEqualToResult ( get-type (foo $x) )  ( Bool )  )

;! (get-type (+ $x 1)) 
; Number
; notready 
!(assertEqualToResult ( get-type (+ $x 1) )  ( Number )  )

;! (get-type =) 
; (-> $t $t Atom)
; notready
!(assertEqualToResult ( get-type = )  ( (-> $t $t Atom) )  )

;! (= (foo $x) (+ $x 1)) 
; BadType
; ready 
!(assertEqualToResult ( = (foo $x) (+ $x 1) )  ( (= (foo $x) (+ $x 1)) )  )


;  !(assertEqualToResult ( )  (  )  ) 
