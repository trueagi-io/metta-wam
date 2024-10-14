
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (zip (:: A (:: B ())) (:: 1 ())) ; (:: (A 1) (zip (:: B ()) ()))(= (zip $list1 $list2)
   (case ($list1 $list2)
         (((() ()) ())
          (((:: $x $xs) (:: $y $ys)) (:: ($x $y) (zip $xs $ys)))
          ($else ERROR)
         )
   )
)
! (zip (:: A (:: B ())) (:: 1 (:: 2 ()))) ; (:: (A 1) (:: (B 2) ()))
! (zip (:: A (:: B ())) (:: 1 ())) ; (:: (A 1) ERROR)

;  !(assertEqualToResult ( )  (  )  ) 
