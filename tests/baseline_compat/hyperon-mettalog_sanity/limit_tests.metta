;; limit! returns the first N results

!(assertEqualToResult (limit! 0 (sequential (1 2 3))) ())
!(assertEqualToResult (limit! 2 (sequential (1 2 3))) (1 2))

;; execution stops after reaching N results

!(bind! &var (new-state 0))
(= (increment)
   (sequential ((do (change-state! &var (+ 1 (get-state &var))))
               (get-state &var))))

!(assertEqualToResult (limit! 2 (sequential ((increment) (increment) (increment)))) (1 2))

!(get-state &var)

!(assertEqualToResult (get-state &var) (2))

;; cleanup

!(change-state! &var 0)

!(assertEqualToResult (get-state &var) (0))

;; multiple limits (almost certainly a mistake in practice)

!(assertEqualToResult (limit! (sequential (1 2 3)) (sequential ((increment) (increment) (increment)))) (1 2 3 4 5 6))
!(assertEqualToResult (get-state &var) (6))

;; limit preventing infinite recursion

(= (infinite) 1)
(= (infinite) (+ (infinite) 1))
!(assertEqualToResult (limit! 4 (infinite)) (1 2 3 4))

;; basic number-of with 1 result

!(assertEqualToResult (number-of (+ 1 2)) (1))

;; basic number-of with superpose

!(assertEqualToResult (number-of (superpose (1 2 3))) (3))

;; number-of with limit

!(change-state! &var 0)
!(assertEqualToResult (number-of (limit! 2 (sequential ((increment) (increment) (increment))))) (2))
!(assertEqualToResult (get-state! &var) (2))
!(change-state! &var 0)

!(assertEqualToResult (number-of (limit! 4 (infinite))) (4))

;; number-of with variable

(= (test A) (input A))
(= (test B) (input B))

!(assertEqualToResult (number-of (test $x)) (2))

;; offset! ignores first N results

!(assertEqualToResult (offset! 2 (sequential (1 2 3 4))) (3 4))

;; offset! first N results are still generated, just discarded

!(change-state! &var 0)
!(assertEqualToResult (offset! 2 (sequential ((increment) (increment) (increment) (increment)))) (3 4))

;; offset! and limit!

!(assertEqualToResult (offset! 2 (limit! 4 (infinite))) (3 4))

;; limit! and offset!

!(assertEqualToResult (limit! 4 (offset! 2 (infinite))) (3 4 5 6))
