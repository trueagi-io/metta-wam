
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


; `atom-eq` returns True, when arguments are identical
; (can be unified with the same variable)
(: atom-eq (-> Atom Atom Bool))
(= (atom-eq $x $x) True)

; These expressions are identical:
;! (atom-eq (+ 1 2) (+ 1 2))
;[True]


!(println "xx 40 ") 
 ; ready works
!(assertEqualToResult ( atom-eq (+ 1 2) (+ 1 2) ) ( True ) ) 



; the following will not be reduced because the expressions are not the same 
; (even though the result of their evaluation would be)
;! (atom-eq 3 (+ 1 2))
;[(atom-eq 3 (+ 1 2))]


!(println "xx 41 ") 
 ; notready 
!(assertEqualToResult ( atom-eq 3 (+ 1 2) ) ( (atom-eq 3 (+ 1 2)) ) ) 

;  !(assertEqualToResult ( )  (  )  ) 
