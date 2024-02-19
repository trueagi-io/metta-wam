;; The next few comments passes only to MeTTaLog
;;MeTTaLog: ;; !(pragma! compiler full)
;;MeTTaLog: !(pragma! trace-on-fail False)
;;MeTTaLog: !(pragma! trace-on-pass False)
;;MeTTaLog: !(pragma! trace-on-error True)
;;MeTTaLog: !(pragma! trace-on-overflow True)
;;MeTTaLog: !(pragma! trace-on-load False)
;;MeTTaLog: !(pragma! trace-on-exec True)

!(import! &self NARS.metta)

;;MeTTaLog: !(pragma! trace-on-load True)
;;MeTTaLog: !(pragma! trace-on-exec True)
;;MeTTaLog: !(pragma! trace-on-overflow True)

; debug
; !(assertEqual (TupleCount (get-atoms &belief_events)) 0)

!("sheep see")
!(AddBeliefEvent ((((sheep --> ([ see ])) &/ (^ count)) =/> (sheep --> ([ counted ]))) (1.0 0.9)))
!("current time")
!(BeliefCycle (get-state &currentTime))
!("sheep white")
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

;;MeTTaLog: !(pragma! trace-on-eval True)
