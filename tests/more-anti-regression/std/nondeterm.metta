(= (color) green)
(= (color) yellow)
(= (color) red)

!(assertEqual
  (match &self (= (color) $x) $x)
  (superpose (red yellow green)))

!(assertEqual (color)
        (superpose (red yellow green)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For the ordinary symbolic expression, if its subexpressions
; are nondeterministic, and no reduction rule is available
; for some combinations of their values, the whole expression
; for these values will appear in the set of final results,
; but without reduction. However, `match` can be called
; directly to filter out non-reducible results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(= (foo) red)
(= (foo) boo)
(= (eq $x $x) T)
(= (find-equal $x $y)
    (match &self (= (eq $x $y) T) $x))

!(assertEqual
  (find-equal (color) (foo))
  red)