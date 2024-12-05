
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


; people_kb.metta
(Female Pam)
(Male Tom)
(Male Bob)
(Female Liz)
(Female Pat)
(Female Ann)
(Male Jim)
(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Jim)


; main.metta
; notready  file doesnt exist 
! (import! &people people_kb)
(= (get-sister $x)
   (match &people
     (, (Parent $y $x)
        (Parent $y $z)
        (Female $z))
     $z
   )
)

;! (get-sister Bob)
; [] 
!(println "xx 129 ") 
; ready works  
!(assertEqualToResult ( get-sister Bob ) ( ) ) 

; ! (get-atoms &self)
;[GroundingSpace-top:stdlib, GroundingSpace-top:corelib]

!(println "xx 130 ") 
; notready 
!(assertEqualToResult ( get-atoms &self ) ( GroundingSpace-top:stdlib GroundingSpace-top:corelib ) ) 


;  !(assertEqualToResult ( )  (  )  ) 
