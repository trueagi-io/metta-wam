;; The next few comments passes only to MeTTaLog
;;MeTTaLog: !(pragma! compiler full)
;;MeTTaLog: !(pragma! trace-on-fail False)
;;MeTTaLog: !(pragma! trace-on-pass False)
;;MeTTaLog: !(pragma! trace-on-error False)
;;MeTTaLog: !(pragma! trace-on-overflow False)
;;MeTTaLog: !(pragma! trace-on-load False)

!(import! &self NARS.metta)

;;MeTTaLog: !(pragma! trace-on-load True)

!(AddBeliefEvent (((corridorIn &/ (^ forward)) =/> doorAt) (1.0 0.9)))
!(AddBeliefEvent (((windowAt &/ (^ open)) =/> Outside) (1.0 0.9)))
!(AddBeliefEvent (((doorAt &/ (^ open)) =/> Outside) (1.0 0.9)))

!(AddBeliefEvent (corridorIn (1.0 0.9)))
!(assertEqual (AddGoalEvent (Outside (1.0 0.9)))
;expected:
  None)

;needs one more goal cycle for planning 2 steps
!(GoalCycle (get-state &currentTime))

!(assertEqual (GoalCycle (get-state &currentTime))
;expected:
  (^ forward))

!(AddBeliefEvent (doorAt (1.0 0.9)))
!(assertEqual (AddGoalEvent (Outside (1.0 0.9)))
;expected:
    (^ open))
