
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (let (test 1) 2 NO ) ; empty(= (is-frog Sam) True)
(= (print-if-frog $x)
   (let True (is-frog $x)
     (println! ($x is frog!))))
! (print-if-frog Sam) ; ()

;  !(assertEqualToResult ( )  (  )  ) 
