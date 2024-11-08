
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;(: foo (-&gt; C (-&gt; A B)))
;(: bar (-&gt; B A))
;(: a A)
;(: c C)
;! (get-type (...))


;! (get-type 1) 
; Number
; ready works
!(assertEqualToResult ( get-type 1 )  ( Number )  ) 
;! (get-type 1.1) 
; Number
; ready works
!(assertEqualToResult ( get-type 1.1 )  ( Number )  ) 
;! (get-type +) 
; (-> Number Number Number)
; ready works
!(assertEqualToResult ( get-type + )  ( (-> Number Number Number) )  ) 
;! (get-type (+ 1 2.1)) 
; Number
; ready works
!(assertEqualToResult ( get-type (+ 1 2.1) )  ( Number )  ) 



;  !(assertEqualToResult ( )  (  )  ) 
