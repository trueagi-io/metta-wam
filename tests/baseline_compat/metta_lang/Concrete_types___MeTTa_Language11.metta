
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;(: foo (-&gt; (-&gt; A B) C))
;(: bar (-&gt; A B))
;(: a A)
;! (get-type (foo bar)) ; C
;! (get-type (foo (bar a))) ; empty


(: foo (-> C (-> A B)))
(: bar (-> B A))
(: a A)
(: c C)

;! (get-type (...))

; ready works
!(assertEqualToResult ( get-type (...) )  (  )  ) 
