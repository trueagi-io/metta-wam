;; setup

!(bind! &var (new-state 0))
(= (increment)
   (sequential ((do (change-state! &var (+ 1 (get-state &var))))
               (get-state &var))))

;; sequential + do

(= (foo 1) (bar $x))
(= (foo 2) (bar $x))
(= (bar 1) (increment))
(= (bar 2) (increment))

!(assertEqualToResult (sequential ((do (foo $x))
                                   (get-state &var)))
                      (4))

;; sequential order
;; if sequential order is not fixed, this test has a 1/132 chance of succeeding (10! / 12!)

!(assertEqualToResult (sequential ((do (change-state! &var 0))
                                   (increment) (increment) (increment) (increment) (increment)
                                   (increment) (increment) (increment) (increment) (increment)
                                   (get-state &var)))
                      (1 2 3 4 5 6 7 8 9 10 10))
