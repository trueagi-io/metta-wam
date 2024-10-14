
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: NilNum ListNum)
(: ConsNum (-&gt; Number ListNum ListNum))
! (get-type (ConsNum 1 (ConsNum 2 (ConsNum 3 NilNum)))) ; ListNum
! (ConsNum 1 (ConsNum  NilNum)) ; BadType

;  !(assertEqualToResult ( )  (  )  ) 
