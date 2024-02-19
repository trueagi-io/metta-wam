
!(pragma! compile full)
;!(memoize fibonacci)

(= (equals $X $X) True)
(= (fibonacci  0 0) True)
(= (fibonacci  1 1) True)
(= (fibonacci  $N1 $N4)
  (and
     (> $N1 1)
     (equals  $N2 (-  $N1 1))
     (equals  $N3 (-  $N1 2))
     (fibonacci  $N2 $N5)
     (fibonacci  $N3 $N6)
     (equals  $N4 (+ $N5 $N6))
     True))

!(match &self (fibonacci 10 $W) $W)

!(assertEqual (fibonacci 1 $W) True)
;!(assertEqual (fibonacci 10 55) True)
;!(assertEqual (fibonacci 10 54) False)

