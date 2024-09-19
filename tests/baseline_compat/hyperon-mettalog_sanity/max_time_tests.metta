;; max-time! returns result of Form if execution completes in Time seconds

!(assertEqualToResult (catch (max-time! 1 (+ 2 2)) time_limit_exceeded time_limit_exceeded) (4))

;; max-time! throws time_limit_exceeded if execution takes longer than Time seconds

!(assertEqualToResult (catch (max-time! 0.05 (sleep! 2)) time_limit_exceeded time_limit_exceeded) (time_limit_exceeded))

;; max-time! evaluates the first arg

!(assertEqualToResult (catch (max-time! (+ 1 1) (+ 2 2)) time_limit_exceeded time_limit_exceeded) (4))

;; max-time! returns all results

!(assertEqualToResult (max-time! 1 (superpose (a b c))) (a b c))

;; max-time! executes form for each time limit

!(bind! &var (new-state 1))
(= (increment)
   (sequential ((do (change-state! &var (+ 1 (get-state &var))))
               (get-state &var))))

(= (time-limit) 1)
(= (time-limit) 2)

!(assertEqualToResult (catch (max-time! (time-limit) (increment)) time_limit_exceeded time_limit_exceeded) (2 3))
