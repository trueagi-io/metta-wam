; Curried plus
(: plus (-> Number (-> Number Number)))
(= ((plus $x) $y) (+ $x $y))
; Define inc as partial evaluation of plus
(: inc (-> (-> Number Number)))
(= (inc) (plus 1))

!(assertEqualToResult ((inc) 2) (3))




