
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; This function takes two atoms of type A and B and returns an atom of type C
;(: foo2 (-&gt; A B C))
;(: a A)
;(: b B)
;! (get-type (foo2 a b)) ; C
;! (get-type (foo2 b a)) ; empty
;! (get-type (foo2 a)) ; empty
;! (foo2 a c) ; no error
;! (foo2 b a) ; type error (the interpreter stops on error)


; This function takes two atoms of type A and B and returns an atom of type C
(: foo2 (-> A B C))
(: a A)
(: b B)

; ! (get-type (foo2 a b)) 
; ready works
!(assertEqualToResult ( get-type (foo2 a b) )  ( C )  ) 
; C
;! (get-type (foo2 b a)) 
; ready works
!(assertEqualToResult ( get-type (foo2 b a) )  ( )  ) 
; empty
;! (get-type (foo2 a)) 
; ready works
!(assertEqualToResult ( get-type (foo2 a) )  ( )  ) 
; empty
;! (foo2 a c) 
; ready works
!(assertEqualToResult ( foo2 a c )  ( (foo2 a c) )  ) 
; no error
;! (foo2 b a) 
; ready works
!(assertEqualToResult ( foo2 b a)  ( (foo2 b a) )  ) 
; type error (the interpreter stops on error)
;! () 
; ready works
!(assertEqualToResult ( foo2 c)  ( (foo2 c) )  ) 
; would also be type error


;  !(assertEqualToResult ( )  (  )  ) 
