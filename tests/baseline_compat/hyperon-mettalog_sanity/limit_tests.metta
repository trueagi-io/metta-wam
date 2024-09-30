;; limit! returns the first N results

!(assertEqualToResult (limit! 0 (sequential (1 2 3))) ())
!(assertEqualToResult (limit! 2 (sequential (1 2 3))) (1 2))

;; execution stops after reaching N results

!(bind! &var (new-state 0))
(= (increment)
   (sequential ((do (change-state! &var (+ 1 (get-state &var))))
               (get-state &var))))

!(assertEqualToResult (limit! 2 (sequential ((increment) (increment) (increment)))) (1 2))
!(assertEqualToResult (get-state! &var) (2))

;; cleanup

!(change-state! &var 0)
!(assertEqualToResult (get-state! &var) (0))

;; multiple limits (almost certainly a mistake in practice)

!(assertEqualToResult (limit! (sequential (1 2 3)) (sequential ((increment) (increment) (increment)))) (1 2 3 4 5 6))
!(assertEqualToResult (get-state! &var) (6))
