!(import! &self NARS.metta)

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
