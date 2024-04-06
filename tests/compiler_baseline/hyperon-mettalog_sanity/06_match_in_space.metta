
!(bind! &kb (new-space))


!(add-atom &kb (SS-P_AS A_AS B_AS))
!(add-atom &kb (SS-Q_AS A_AS C_AS))

(PQ SS-P_BAD B_BAD)
(PQ SS-Q_BAD C_BAD)

!(assertEqualToResult
  (match   &kb
    ($SS A_AS $X)
    ($SS A_AS $X))
  ((SS-P_AS A_AS B_AS)
   (SS-Q_AS A_AS C_AS)))

; Basic Match
!(assertEqual
   (match  &kb  ($rel A_AS $x) ($rel $x))
   (superpose ((SS-P_AS B_AS) (SS-Q_AS C_AS))))

; cases can be used for deconstruction
!(assertEqual
  (superpose ((P_AS B_AS) (Q_AS C_AS)))
  (case
   (match  &kb  ($rel A_AS $x) ($rel $x))
    (((SS-P_AS $y) (P_AS $y))
     ((SS-Q_AS $y) (Q_AS $y)))))


; %void% can be used to capture empty results
!(assertEqual (case (match  &kb  ($rel B_AS $x) ($rel $x))
    (((SS-P_AS $y) (P_AS $y))
    ((SS-Q_AS $y) (Q_AS $y))
    (%void% no-match
     )))
     no-match )


