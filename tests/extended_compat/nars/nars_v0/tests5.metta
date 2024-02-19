!(import! &self NARS.metta)

!(AddBeliefEvent ((((sheep --> ([ see ])) &/ (^ count)) =/> (sheep --> ([ counted ]))) (1.0 0.9)))
!(BeliefCycle (get-state &currentTime))
!(AddBeliefEvent ((sheep --> ([ white ])) (1.0 0.9)))

;later that day:
!(AddBeliefEvent ((({ instance1 }) --> ([ white ])) (1.0 0.9)))
!(AddBeliefEvent ((({ instance1 }) --> ([ see ])) (1.0 0.9)))

!(assertEqual
  (EternalQuestion (({ instance1 }) --> sheep))
  (Event ((({ instance1 }) --> sheep) (1.0 0.30916030534351147)) (eternal (4 3) (0 0.0))))

!(assertEqual
  (EternalQuestion (sheep --> ([ see ])))
  (Event ((sheep --> ([ see ])) (1.0 0.28712091374556914)) (eternal (5 4 3) (0 0.0))))

!(assertEqual
  (AddGoalEvent ((sheep --> ([ counted ])) (1.0 0.9)))
  (^ count))
