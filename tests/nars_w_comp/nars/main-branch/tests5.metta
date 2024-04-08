
!(import! &self NARS)

!(AddBeliefEvent ((((sheep --> (IntSet see)) &/ (^ count)) =/> (sheep --> (IntSet counted))) (1.0 0.9)))
!(BeliefCycle (get-state &currentTime))
!(AddBeliefEvent ((sheep --> (IntSet white)) (1.0 0.9)))

;later that day:
!(AddBeliefEvent (((ExtSet instance1) --> (IntSet white)) (1.0 0.9)))
!(AddBeliefEvent (((ExtSet instance1) --> (IntSet see)) (1.0 0.9)))

!(EternalQuestion ((ExtSet instance1) --> sheep))
;[(Event (((ExtSet instance1) --> sheep) (1.0 0.30916030534351147)) (eternal (4 3) (0 0.0)))]
!(EternalQuestion (sheep --> (IntSet see)))
;[(Event ((sheep --> (IntSet see)) (1.0 0.28712091374556914)) (eternal (5 4 3) (0 0.0)))]
!(AddGoalEvent ((sheep --> (IntSet counted)) (1.0 0.9)))
;[(^ count)]
