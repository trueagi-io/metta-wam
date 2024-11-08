
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
;! (println!)


;! (println! "This is a string")
!(assertEqualToResult ( println! "This is a string" )  ( "This is a string" )  )
;! (println! ($v1 "string" 5))
!(assertEqualToResult ( println! ($v1 "string" 5) )  ( ($v1 "string" 5) )  )



;  !(assertEqualToResult ( )  (  )  ) 
