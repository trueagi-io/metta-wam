
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(Parent Bob Ann)

; notready 
! (assertEqualToResult
    (match &self (Parent Tom $x) $x)
    (empty)) 

; ()

; ready works 
! (assertEqualToResult
    (match &self (Parent Tom $x) $x)
    ()) 

; ()



;  !(assertEqualToResult ( )  (  )  ) 
