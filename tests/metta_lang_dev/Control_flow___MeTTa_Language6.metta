
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (test 1) 1)
(= (test 1) 0)
(= (test 2) 2)

;(= (test 1) 1)
;(= (test 1) 0)
;(= (test 2) 2)

; ! (let $W (test $X) (println! ("test" $X => $W)))



; ready works 
!(assertEqualToResult ( let $W (test $X) (println! ("test" $X => $W)))  (  () () ())  ) 
