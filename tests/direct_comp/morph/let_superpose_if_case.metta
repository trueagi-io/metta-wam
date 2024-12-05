(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) (let $x 0 (let $x 1 $x)))

(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

(= (f $x) 42)

!(let $y (superpose (3 4 5))
      (If (> $y 3)
          (case (1 $y)
	               (((1 3) (f 0))
                        ((1 4) (42 42))
                        ($else (1 1))))))

!(assertEqualToResult
   (let $y (superpose (3 4 5))
      (If (> $y 3)
          (case (1 $y) (((1 3) (f 0))
                        ((1 4) (42 42))
                       ($else (1 1))))))
     ((42 42) (1 1)))

