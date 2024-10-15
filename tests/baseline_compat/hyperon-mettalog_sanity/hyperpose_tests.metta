; Define 'is-even' predicate
(: is-even (-> Number Bool))
(= (is-even $X) (== (% $X 2) 0))

;; basic hyperpose evaluation

!(assertEqualToResult (catch (max-time! 1 (hyperpose ((is-even 1) (is-even 2) (is-even 3))))
                        time_limit_exceeded time_limit_exceeded)
    (False True False))

!(assertEqualToResult (catch (max-time! 1 (hyperpose ((> 1 2) (is-even 2) (superpose (3 4)))))
                        time_limit_exceeded time_limit_exceeded)
    (False True 3 4))

;; test recursive hyperpose

!(assertEqualToResult (catch (max-time! 1 (hyperpose ((hyperpose (1 2)) (hyperpose (3 4)))))
                        time_limit_exceeded time_limit_exceeded)
    (1 2 3 4))
