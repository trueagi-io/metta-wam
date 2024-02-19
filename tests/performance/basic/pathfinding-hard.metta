
!(pragma! compile full)

(= (edge a b 2) True)
(= (edge b a 2) True)
(= (edge b c 3) True)
(= (edge c b 3) True)
(= (edge c d 4) True)
(= (edge d c 4) True)
(= (edge b e 1) True)
(= (edge e b 1) True)
(= (edge e f 5) True)
(= (edge f e 5) True)
(= (edge f g 6) True)
(= (edge g f 6) True)

(= (path $x $x 0) True) ; base case, node to itself with distance 0
(= (path $x $y $w) (and (edge $x $z $d) (path $z $y $dw) (is $w (+ $d $dw)))) ; recursive case with weights

; Finding all nodes reachable from a given node.
(= (reachable $x $x) True) ; base case, a node is always reachable from itself
(= (reachable $x $y) (or (and (edge $x $z _) (reachable $z $y)))) ; recursive case



; Pathfinding tests
!(assertEqual (path a f 11) True)
!(assertEqual (reachable a f) True)


!(assertEqual (path a c $_) True)



