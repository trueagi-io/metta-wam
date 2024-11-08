
; 20241014 r.v.v converted metta-lang.dev to metta 
(= (test 1) (trace! (test 1) 1))
(= (test 1) (trace! (test 0) 0))
;! (test 1)
;[()]


;!(println "xx 12 ") 
; notready !!!!  HANGS 
;!(assertEqualToResult ( test 1 ) ( ) ) 

!(println "xx 12-3 ") 

(Parent Bob Ann)

; notready
! (assertEqual
   (match &self (Parent $x Ann) $x)
   (unify (Parent $x Ann) (Parent Bob $y) $x )) 


; ready works 
! (assertEqual (+ 1 2) ( 3 ) ) 
;! (assertEqual (+ 1 2) (+ 1 4)) 
; Error-expression


;(Parent Bob Ann)
(Parent Pam Ann)

; ready works 
! (assertEqualToResult
    (match &self (Parent $x Ann) $x)
    (Bob Pam))  


(= (bin) 0)
(= (bin) 1)

; ready works
! (assertEqualToResult (bin) (0 1)) 

; ready works
! (assertEqualToResult (+ 1 2) (3)) 

; ready works 
! (assertEqualToResult   (+ 1 untyped-symbol)   ((+ 1 untyped-symbol))) 


; notready
! (assertEqualToResult (+ 1 2) ((+ 1 2))) 




