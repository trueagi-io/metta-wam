
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
     (py-list (1 2)) (py-list (3 4))) ; 1 * 3 + 2 * 4 = 11! (py-dict (() ())) ; creates a dict {"a":"b", "b":"c"}
! (py-tuple (1 5)) ; creates a tuple (1, 5)
! (py-list (1 (2 (3)))) ; creates a nested list [1, [2, [3, '3']]]

;  !(assertEqualToResult ( )  (  )  ) 
