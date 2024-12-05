
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


! (pragma! type-check auto) 
; ()
(: foo (-> Number Bool))

; (= (foo $x) (+ $x 1)) 

; BadType



; ready works 
!(assertEqualToResult ( = (foo $x) (+ $x 1) )  ( (= (foo $x) (+ $x 1)) )  ) 
