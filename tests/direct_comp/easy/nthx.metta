
(: nthx (-> Atom Atom Atom))

(= (nthx $n $x)
    (if (== $n 0)
        (car-atom $x)
        (let* (($rest (cdr-atom $x)) ($nn (- $n 1)))
            (nthx $nn $rest))))

!(nthx 0 (10 11 12 13 14 15))
!(nthx 3 (10 11 12 13 14 15))


