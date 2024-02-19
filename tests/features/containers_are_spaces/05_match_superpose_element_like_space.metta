

(= (kb)
  (superpose
    (((SS-P_Each A_Each B_Each))
     ((SS-Q_Each A_Each C_Each)))))

!(assertEqualToResult
  (match  (kb)
    ($SS A_Each $X)
    ($SS A_Each $X))
  ((SS-P_Each A_Each B_Each)
   (SS-Q_Each A_Each C_Each)))

; Basic Match
!(assertEqual
   (match (kb) ($rel A_Each $x) ($rel $x))
   (superpose ((SS-P_Each B_Each) (SS-Q_Each C_Each))))

; cases can be used for deconstruction
!(assertEqual
  (superpose ((P_Each B_Each) (Q_Each C_Each)))
  (case
   (match (kb) ($rel A_Each $x) ($rel $x))
    (((SS-P_Each $y) (P_Each $y))
     ((SS-Q_Each $y) (Q_Each $y)))))


; %void% can be used to capture empty results
!(assertEqual (case (match (kb) ($rel B_Each $x) ($rel $x))
    (((SS-P_Each $y) (P_Each $y))
    ((SS-Q_Each $y) (Q_Each $y))
    (%void% no-match
     )))
     no-match )

