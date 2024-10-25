
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (foo2 c) ; would also be type error

;  !(assertEqualToResult ( )  (  )  ) 


(: foo (-> A B))
(: bar (-> B B A))
(: a A)

;! (get-type (bar (foo a) (foo a)))
; ready works
!(assertEqualToResult ( get-type (bar (foo a) (foo a)) )  ( A )  ) 

;! (get-type (foo (bar (foo a) (foo a))))
; ready works
!(assertEqualToResult ( get-type (foo (bar (foo a) (foo a))) )  ( B )  ) 