(: recur-until-flip-true (-> Number Bool))
(= (recur-until-flip-true $n)
  (if (> $n 40)         ; set constant here for max
    False
    (if (flip)
       True
       (recur-until-flip-true (+ 1 $n)))))

(: recur-until-flip-false (-> Number Bool))
(= (recur-until-flip-false $n)
  (if (> $n 40)         ; set constant here for max
    False
    (if (flip)
       (recur-until-flip-false (+ 1 $n))
       True)))

!(assertEqualToResult (recur-until-flip-true 1) (True))
!(assertEqualToResult (recur-until-flip-false 1) (True))