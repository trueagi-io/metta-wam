(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) (let $x 0 (let $x 1 $x)))

(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))




(GreaterThan 42 21)

(= (EqualOrGreater $x $y)
   (or (> $x $y) (== $x $y)))

!(match &self (GreaterThan $x $y)
    (If (and (== (get-type $x) Number) (== (get-type $y) Number))
        (If (EqualOrGreater $x $y) yay)))
