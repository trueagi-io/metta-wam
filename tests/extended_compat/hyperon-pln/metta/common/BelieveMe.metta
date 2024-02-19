;; Dummy function to satisfy any output type constraint.  Can be used
;; to please to the type checker when there is no such function
;; available.  Typical usage is for filling a missing proof.
(: believe_me (-> $a))
