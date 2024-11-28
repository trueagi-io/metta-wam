
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(Atom to remove)

;! (match &self (Atom to remove) "Atom exists") 
; ["Atom exists"]

!(println "xx 104 ") 
 ; ready works  
!(assertEqualToResult ( match &self (Atom to remove) "Atom exists" ) ( "Atom exists" ) ) 

;! (remove-atom &self (Atom to remove)) 
; [()]


!(println "xx 105 ") 
 ; ready works  
!(assertEqualToResult ( remove-atom &self (Atom to remove) ) ( () ) ) 

;! (match &self (Atom to remove) "Unexpected") 
; []]

!(println "xx 106 ") 
; ready works  
!(assertEqualToResult ( match &self (Atom to remove) "Unexpected" ) ( ) ) 

; ! (remove-atom &self (Atom to remove)) 
; [()]


!(println "xx 107 ") 
; ready works 
!(assertEqualToResult ( remove-atom &self (Atom to remove) ) ( () ) ) 

;["Atom exists"]
;[()]
;[]
;[()]



;  !(assertEqualToResult ( )  (  )  ) 
