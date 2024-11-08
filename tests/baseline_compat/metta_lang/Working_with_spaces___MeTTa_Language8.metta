
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

;! (add-reduct &self (= (get-space) (new-space))) 
; ()


!(println "xx 114 ") 
;  notready 
!(assertEqualToResult ( add-reduct &self (= (get-space) (new-space)) ) ( () ) ) 

;! (add-atom (get-space) (Parent Bob Ann)) 
; ()


!(println "xx 115 ") 
; ready works 
!(assertEqualToResult ( add-atom (get-space) (Parent Bob Ann) ) ( () ) ) 

;! (get-space) 
; GroundingSpace-addr


!(println "xx 116 ") 
; notready 
!(assertEqualToResult ( get-space ) ( GroundingSpace-addr ) ) 

; ! (match (get-space) (Parent $x $y) ($x $y)) 
; (Bob Ann)


!(println "xx 117 ") 
; ready works 
!(assertEqualToResult ( match (get-space) (Parent $x $y) ($x $y) ) ( (Bob Ann) ) ) 

;[()]
;[()]
;[GroundingSpace-0x5642ce134c88]
;[(Bob Ann)]



;  !(assertEqualToResult ( )  (  )  ) 
