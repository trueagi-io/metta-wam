(= (range $start $end $step) (if (< $start $end) $start (empty)))
(= (range $start $end $step) (if (< $start $end) (range (+ $start $step) $end $step) (empty)))

(= (eq? $x $y) (== $x $y))
(= (union $x $y) $x)
(= (union $x $y) $y)
(= (intersection $x $x) $x)
(= (intersection $x $y) (empty))
(= (right-only $c $u) (if (right-only_ $c $u) $u (empty)))
(= (right-only_ $c $u) (eq? () (collapse (intersection $u (superpose $c)))))
; !(union (range 1 13 1) (range 10 20 2))
; !(intersection (range 1 13 1) (range 10 20 2))
; !(right-only (collapse (range 1 16 1)) (range 10 20 2))

(= (fizzbuzz $n)
  (let* (($m3 (collapse (range 3 $n 3)))
         ($m5 (collapse (range 5 $n 5)))
         ($r (collapse (range 1 $n 1)))
         ($m35 (collapse (intersection (superpose $m3) (superpose $m5))))
         ($m3n5 (right-only $m5 (superpose $m3)))
         ($m5n3 (right-only $m3 (superpose $m5)))
         ($n35 (right-only $m35 (superpose $r))))
        (union (union (FizzBuzz (superpose $m35)) (Nothing $n35)) (union (Fizz $m3n5) (Buzz $m5n3)))))

!(fizzbuzz 50)


(== 2 (superpose (1 2 3 4)))


(== 2 (superpose (1 2 3 $x)))

