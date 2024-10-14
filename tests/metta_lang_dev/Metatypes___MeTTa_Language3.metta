
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (get-type Error) ; (-&gt; Atom Atom ErrorType)
! (get-metatype Error) ; just Symbol
! (get-type (Error Foo Boo)) ; ErrorType
! (Error (+ 1 2) (+ 1 +)) ; arguments are not evaluated

;  !(assertEqualToResult ( )  (  )  ) 
