(Parent Bob Ann)
(Parent Pam Ann)

; ready works
! (assertEqualToResult  (match &self (Parent $x Ann) $x)   (Bob Pam))  


(= (bin) 0)
(= (bin) 1)
; ready works
! (assertEqualToResult (bin) (0 1)) 

; ready works
! (assertEqualToResult (+ 1 2) (3)) 
; ready works
! (assertEqualToResult  (+ 1 untyped-symbol)   ((+ 1 untyped-symbol))) 

;! (assertEqualToResult (+ 1 2) ((+ 1 2))) 