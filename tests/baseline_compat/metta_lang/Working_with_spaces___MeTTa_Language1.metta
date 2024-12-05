
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
; aa 
(: add-foo-eq (-> Atom (->)))
(= (add-foo-eq $x)
   (add-atom &self (= (foo) $x)))

;! (foo) 
; (foo) 
!(println "xx 96 ") 
; ready works 
!(assertEqualToResult ( foo ) ( (foo) ) ) 


;! (add-foo-eq (+ 1 2)) 
; ()
!(println "xx 97 ") 
; ready works 
!(assertEqualToResult ( add-foo-eq (+ 1 2) ) ( () ) ) 

;! (add-foo-eq (+ 3 4)) 
; ()
!(println "xx 98 ") 
; ready works
!(assertEqualToResult ( add-foo-eq (+ 3 4) ) ( () ) ) 


;! (foo) 
; [3, 7]
!(println "xx 99 ") 
; notready 
!(assertEqualToResult ( foo ) ( 3 7 ) ) 


;! (match &self (= (foo) $x)
;    (quote $x)) 
; [(quote (+ 1 2)), (quote (+ 3 4))]
!(println "xx 100 ") 
; notready 
!(assertEqualToResult ( match &self (= (foo) $x) (quote $x) ) ( (quote (+ 1 2)) (quote (+ 3 4)) ) ) 


;[(foo)]
;[()]
;[()]
;[7, 3]
;[(quote (+ 3 4)), (quote (+ 1 2))]


;  !(assertEqualToResult ( )  (  )  ) 
