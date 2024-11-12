
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


;! (bind! &res (println! "test"))
; "test"


!(println "xx 124 ") 
; notready 
!(assertEqualToResult ( bind! &res (println! "test") ) ( "test" ) ) 


!(println "xx 125 ") 
;! &res
;[()]
; notready 
!(assertEqualToResult ( &res )  (  )  )


!(println "xx 126 ") 
! &res
;[()]
; notready 
!(assertEqualToResult ( &res )  (  )  )


;[()]


;  !(assertEqualToResult ( )  (  )  ) 
