
;; The next few comments passes only to MeTTaLog
;;MeTTaLog: !(pragma! compiler full)
;;MeTTaLog: !(pragma! trace-on-fail False)
;;MeTTaLog: !(pragma! trace-on-pass False)
;;MeTTaLog: !(pragma! trace-on-error False)
;;MeTTaLog: !(pragma! trace-on-overflow False)
;;MeTTaLog: !(pragma! trace-on-load False)

!(import! &self NARS.metta)

;;MeTTaLog: !(pragma! trace-on-load True)

!(AddBeliefEvent (ballleft (1.0 0.9)))
!(AddBeliefEvent ((^ left) (1.0 0.9)))
!(AddBeliefEvent (ballhit (1.0 0.9)))

!(AddBeliefEvent (ballleft (1.0 0.9)))
!(AddBeliefEvent ((^ left) (1.0 0.9)))

!(AddBeliefEvent (ballright (1.0 0.9)))
!(AddBeliefEvent ((^ left) (1.0 0.9)))
!(AddBeliefEvent (ballhit (1.0 0.9)))

!(AddBeliefEvent (ballright (1.0 0.9)))
!(AddBeliefEvent ((^ right) (1.0 0.9)))
!(AddBeliefEvent (ballhit (1.0 0.9)))

!(AddBeliefEvent (ballright (1.0 0.9)))
!(AddBeliefEvent ((^ right) (1.0 0.9)))
!(AddBeliefEvent (ballhit (1.0 0.9)))

;just for looking at the hypotheses:
!(EternalQuestion ((ballleft &/ (^ $Op)) =/> ballhit))
!(EternalQuestion ((ballright &/ (^ $Op)) =/> ballhit))

!(AddBeliefEvent (ballleft (1.0 0.9)))
!(assertEqual (AddGoalEvent (ballhit (1.0 0.9)))
 (^ left))

!(AddBeliefEvent (ballright (1.0 0.9)))
!(assertEqual
   (AddGoalEvent (ballhit (1.0 0.9)))
   (^ right))
