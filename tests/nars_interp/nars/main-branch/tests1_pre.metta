

!(import! &self NARS)

;;NARS test
!(AddBeliefEvent (((ExtSet garfield) --> cat) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (((cat x sky) --> like) (1.0 0.9)))
!(check-state)

;debug:
!(TupleCount (collapse (CountElement  (get-atoms &belief_events))))
;[8]
!(TupleCount (collapse (CountElement (get-atoms &attentional_focus))))
;[8]
!(TupleCount (collapse (CountElement (get-atoms &concepts))))
;[100]

