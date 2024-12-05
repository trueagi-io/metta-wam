
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; This function accepts an atom of type A and returns an atom of type B
;(: foo (-&gt; A B))
;(: a A)
;(: b B)
;! (foo a) ; no error
;! (get-type (foo b)) ; no result
;! (b foo) ; notice: no error
;! (get-type (b foo)) ; (B (-&gt; A B))

;  !(assertEqualToResult ( )  (  )  ) 

; This function accepts an atom of type A and returns an atom of type B
(: foo (-> A B))
(: a A)
(: b B)

; ready works
!(assertEqualToResult (foo a)    ( (foo a) )  ) 
; ready works
!(assertEqualToResult (get-type (foo b))   (  )  ) 
; ready works
!(assertEqualToResult (b foo)   ( (b foo) )  ) 
; notice: no error
; notready
!(assertEqualToResult (get-type (b foo))  ( (B (-> A B)) )  )  

; ready works

!(assertEqualToResult (foo b)   ( (foo b) )  ) 
; type error
