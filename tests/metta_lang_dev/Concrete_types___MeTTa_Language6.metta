
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (foo b) 
; type error(: foo (-&gt; A B))
;(: a A)
;! (get-type (foo c))
;! (get-type (g a))


(: foo (-> A B))
(: a A)

;! ()
; not ready 
!(assertEqualToResult ( get-type (foo c) )  ( B )  ) 
;! ()
; notready 
!(assertEqualToResult ( get-type (g a) )  ( (%Undefined% A) %Undefined% )  ) 

;  !(assertEqualToResult ( )  (  )  ) 
