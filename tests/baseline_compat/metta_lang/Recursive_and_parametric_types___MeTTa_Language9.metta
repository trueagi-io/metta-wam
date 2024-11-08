
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: Nil (List $t))
(: Cons (-> $t (List $t) (List $t)))
(: first (-> (List $t) $t))
(: append (-> (List $t) (List $t) (List $t)))

;! (get-type
;    (+ 1
;      (first (append (Cons 1 Nil)
;                     (Cons 2 Nil)))))
; [Number]


!(println "xx 94 ") 
 ; ready works 
!(assertEqualToResult ( get-type (+ 1 (first (append (Cons 1 Nil) (Cons 2 Nil)))) ) ( Number ) ) 

;  !(assertEqualToResult ( )  (  )  ) 
