
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (+ 2 (superpose (3 4 5))) ; [5, 6, 7](= (ikb) international-klein-blue)
(= (color) green)
(= (color) yellow)
(= (color) (ikb))
!(assertEqual
  (match &amp;self (= (color) $x) $x)
  (superpose ((ikb) yellow green))) ; ()
!(assertEqualToResult
   (match &amp;self (= (color) $x) $x)
  ((ikb) yellow green)) ; Error

;  !(assertEqualToResult ( )  (  )  ) 
