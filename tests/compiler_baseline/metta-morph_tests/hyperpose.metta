(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) (let $x 0 (let $x 1 $x)))

(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))


!(assertEqual (hyperpose (1 2)) (superpose (1 2)))

!(assertEqual (hyperpose (1 (If (== 1 2) 1)))
  (superpose  (1 (If (== 1 2) 1))))

!(assertEqual (hyperpose ((If (== 1 2) 1) 1))
  (superpose  ((If (== 1 2) 1) 1)))

!(assertEqual (hyperpose (1 2 3))
  (superpose  (1 2 3)))

!(assertEqual (hyperpose ((If (== 1 2) 1) 2 3))
  (superpose  ((If (== 1 2) 1) 2 3)))

!(assertEqual (hyperpose (1 (If (== 1 2) 1) 3))
  (superpose  (1 (If (== 1 2) 1) 3)))

!(assertEqual (hyperpose (1 2 (If (== 1 2) 1)))
  (superpose  (1 2 (If (== 1 2) 1))))

!(assertEqual (hyperpose (1 (If (== 1 2) 1) (If (== 1 2) 1)))
  (superpose  (1 (If (== 1 2) 1) (If (== 1 2) 1))))

!(assertEqual (hyperpose (1 2 3 4))
  (superpose  (1 2 3 4)))

!(assertEqual (hyperpose ((1 2) (3 4)))
  (superpose  ((1 2) (3 4))))

