
!(import! &self NARS)

!(nop (pragma! load show))

!(AddBeliefEvent (ballleft (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((^ left) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (ballhit (1.0 0.9)))
!(check-state)

!(AddBeliefEvent (ballleft (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((^ left) (1.0 0.9)))
!(check-state)

!(AddBeliefEvent (ballright (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((^ left) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (ballhit (1.0 0.9)))
!(check-state)

!(AddBeliefEvent (ballright (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((^ right) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (ballhit (1.0 0.9)))
!(check-state)

!(AddBeliefEvent (ballright (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((^ right) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (ballhit (1.0 0.9)))
!(check-state)

;just for looking at the hypotheses:
!(EternalQuestion ((ballleft &/ (^ $Op)) =/> ballhit))
; returned: ((Event (None (0.5 0.0)) (eternal () 0.0)))
!(check-state)
!(EternalQuestion ((ballright &/ (^ $Op)) =/> ballhit))
; returned: ((Event (None (0.5 0.0)) (eternal () 0.0)))
!(check-state)

!(AddBeliefEvent (ballleft (1.0 0.9)))
!(check-state)
!(AddGoalEvent (ballhit (1.0 0.9)))
;expected: [(^ left)]
!(check-state)

!(AddBeliefEvent (ballright (1.0 0.9)))
!(check-state)
!(AddGoalEvent (ballhit (1.0 0.9)))
;expected: [(^ right)]
!(check-state)
