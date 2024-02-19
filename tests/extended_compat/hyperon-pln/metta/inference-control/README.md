# Inference Control Experiments

## Description

First, we experiment with inference control as pruning mechanism.  The
simplest way to do that is to replace the maximum depth by a
termination predicate.  It is unclear what should be the domain of
that termination predicate.  Should it be the proof so far?  Something
external?  Best is to let it user defined.
