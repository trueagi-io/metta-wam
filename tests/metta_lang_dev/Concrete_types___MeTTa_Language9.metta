
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;(: foo (-&gt; A B))
;(: bar (-&gt; B B A))
;(: a A)
;! (get-type (bar (foo a) (foo a)))

(: foo-pair (-> (A B) C))
(: a A)
(: b B)


;! (get-type (foo-pair a b)) 
; empty
; ready works 
!(assertEqualToResult ( get-type (foo-pair a b) )  (  )  )
;! (get-type (foo-pair (a b))) 
; C
; notready
!(assertEqualToResult ( get-type (foo-pair (a b)) )  ( (C C) )  )


;  !(assertEqualToResult ( )  (  )  ) 
