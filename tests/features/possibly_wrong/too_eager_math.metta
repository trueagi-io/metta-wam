(: plus-got (-> Atom Atom Atom))
(= (plus-got $a $b) (+ $a $b))
(= (plus-got $a $b) (got $a $b))

!(plus-got 1 (+ 2 2))
!(plus-got 1 (plus 2 2))
!(plus-got 1 (plus 2 2))

