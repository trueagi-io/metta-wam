
! (* (+ 1 2) (- 8 3)) ; 15
(= (square $x) (* $x $x))
! (square (+ 2 3)) ; 25
(= (triple $x) ($x $x $x))
(= (grid3x3 $x) (triple (triple $x)))
! (grid3x3 (square (+ 1 2))) ; ((9 9 9) (9 9 9) (9 9 9))


;[15]
;[25]
;[((9 9 9) (9 9 9) (9 9 9))]
