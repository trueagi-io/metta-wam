
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (bind! &x (foo1 (foo2 3) 45 (A (v))))
;[()]


!(println "xx 122 ") 
; ready works 
!(assertEqualToResult ( bind! &x (foo1 (foo2 3) 45 (A (v))) ) ( () ) ) 

; ! &x
; [(foo1 (foo2 3) 45 (A (v)))]

; notready 
!(assertEqualToResult ( &x )  ( (foo1 (foo2 3) 45 (A (v))) )  )


;  !(assertEqualToResult ( )  (  )  ) 
