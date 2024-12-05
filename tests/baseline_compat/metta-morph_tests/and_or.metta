(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) (let $x 0 (let $x 1 $x)))

(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))


!(If (or (and True False) True) 1 2)
