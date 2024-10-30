
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

; ! (bind! &space (new-space)) 
; [()]


!(println "xx 125 ") 
; ready works 
!(assertEqualToResult ( bind! &space (new-space) ) ( () ) ) 

;! (add-atom &space (Parent Bob Ann)) 
; [()]

!(println "xx 126 ") 
; ready works  
!(assertEqualToResult ( add-atom &space (Parent Bob Ann) ) ( () ) ) 

; ! &space 
; [GroundingSpace-addr]
!(assertEqualToResult ( &space )  ( GroundingSpace-addr )  )

;! (match &space (Parent $x $y) ($x $y)) 
; [(Bob Ann)]


!(println "xx 127 ") 
; ready works  
!(assertEqualToResult ( match &space (Parent $x $y) ($x $y) ) ( (Bob Ann) ) ) 

; ! (match &self (Parent $x $y) ($x $y)) 
; [empty]

!(println "xx 128 ") 
; ready works  
!(assertEqualToResult ( match &self (Parent $x $y) ($x $y) ) ( ) ) 


;[()]
;[()]
;[GroundingSpace-0x5642cc16ede8]
;[(Bob Ann)]
;[]





;  !(assertEqualToResult ( )  (  )  ) 
