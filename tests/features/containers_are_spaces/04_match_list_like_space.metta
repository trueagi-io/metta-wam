

(= (kb)
    ((SS-P_List A_List B_List)
     (SS-Q_List A_List C_List)))

(= (kb2)
    ((PQ SS-P_List B_List)
     (PQ SS-Q_List C_List)))

!(assertEqualToResult
  (match  (kb)
    ($SS A_List $X)
    ($SS A_List $X))
  ((SS-P_List A_List B_List)
   (SS-Q_List A_List C_List)))

; Basic Match
!(assertEqual
   (match (kb) ($rel A_List $x) ($rel $x))
   (superpose ((SS-P_List B_List) (SS-Q_List C_List))))

; cases can be used for deconstruction
!(assertEqual
  (superpose ((P_List B_List) (Q_List C_List)))
  (case
   (match (kb) ($rel A_List $x) ($rel $x))
    (((SS-P_List $y) (P_List $y))
     ((SS-Q_List $y) (Q_List $y)))))


; %void% can be used to capture empty results
!(assertEqual (case (match (kb) ($rel B_List $x) ($rel $x))
    (((SS-P_List $y) (P_List $y))
    ((SS-Q_List $y) (Q_List $y))
    (%void% no-match
     )))
     no-match )


