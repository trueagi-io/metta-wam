(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

!(If (> 1 2) (3 4) (5 6))
