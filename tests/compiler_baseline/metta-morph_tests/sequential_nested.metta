(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

(: do (-> Expression %Undefined%))
(= (do $1) (case $1 ()))



!(bind! &var (new-state 1))
(= (increment)
   (superpose ((do (change-state! &var (+ 1 (get-state &var))))
               (get-state &var))))

!(sequential ((increment) (increment)))
!(sequential ((increment) (increment) (increment)))
!(sequential ((sequential ((increment) (increment) (increment)))))
!(sequential ((sequential ((increment) (increment) (increment))) (sequential ((increment) (increment) (increment)))))
!(sequential ((sequential ((increment) (increment) (increment))) (increment) (increment) (increment)))
!(do (sequential ((sequential ((increment) (increment) (increment))) (increment) (increment) (increment))))
!(get-state &var)
!(sequential ((increment)))
!(increment)
!(get-state &var)
!(sequential ((change-state! &var (+ 1 (get-state &var)))))
!(get-state &var)

