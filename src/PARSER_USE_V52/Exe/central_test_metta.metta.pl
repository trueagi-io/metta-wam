(= (simple-deduction-strength-formula $As $Bs $Cs $ABs $BCs)
  (if
     (and
        (conditional-probability-consistency $As $Bs $ABs)
        (conditional-probability-consistency $Bs $Cs $BCs))

     (if (< 0.99 $Bs)

        $Cs

        (+ (* $ABs $BCs) (/ (* (- 1 $ABs) (- $Cs (* $Bs $BCs))) (- 1 $Bs))  )   )

     0))