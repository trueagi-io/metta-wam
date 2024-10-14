
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (factorial $x)
   (if (&gt; $x 0)
       (* $x (factorial (- $x 1)))
       1))

;  !(assertEqualToResult ( )  (  )  ) 
