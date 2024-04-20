

!(import! &self NARS)

!(AddBeliefEvent (((corridorIn &/ (^ forward)) =/> doorAt) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (((windowAt &/ (^ open)) =/> Outside) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (((doorAt &/ (^ open)) =/> Outside) (1.0 0.9)))
!(check-state)

!(AddBeliefEvent (corridorIn (1.0 0.9)))
!(check-state)
!(AddGoalEvent (Outside (1.0 0.9)))
;expected: [None]
!(check-state)

;needs one more goal cycle for planning 2 steps
!(GoalCycle (get-state &currentTime))
!(check-state)
!(GoalCycle (get-state &currentTime))
;expected: [(^ forward)]
!(check-state)

!(AddBeliefEvent (doorAt (1.0 0.9)))
!(check-state)
!(AddGoalEvent (Outside (1.0 0.9)))
;expected: [(^ open)]
!(check-state)

