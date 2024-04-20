

!(import! &self NARS)

;;NARS test
!(AddBeliefEvent (((ExtSet garfield) --> cat) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent (((cat x sky) --> like) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((sky --> (IntSet blue)) (1.0 0.9)))
!(check-state)

;The following question needs both a deduction and abduction step:
!(EternalQuestion (((ExtSet garfield) x (IntSet blue)) --> like))
;expected: [(Event ((((ExtSet garfield) x (IntSet blue)) --> like) (1.0 0.2965825874694874)) (eternal (Cons 2 (Cons 1 (Cons 3 Nil))) 0.643288027761712))]

;Lets stress the control mechanism as these type of events with common extension or intension causes dozens of derivations:
!(AddBeliefEvent ((A --> cat) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((B --> cat) (1.0 0.9)))
!(check-state)
!(AddBeliefEvent ((C --> cat) (1.0 0.9)))
!(check-state)

;--(EternalQuestion ((A & B) --> cat))
;expected: [(Event (((A & B) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 4 (Cons 5 Nil)) (5 0.4525)))]

;--(EternalQuestion ((B & C) --> cat))
;expected: [(Event (((B & C) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 5 (Cons 6 Nil)) (6 0.4525)))]

;--(EternalQuestion (((A & B) & C) --> cat))
;expected: [(Event ((((A & B) & C) --> cat) (1.0 0.42163100057836905)) (eternal (Cons 5 (Cons 4 (Cons 6 Nil))) (6 0.195593125)))

!(AddBeliefEvent ((((ExtSet garfield) x (IntSet blue)) --> like) (1.0 0.9)))
!(check-state)

!(EternalQuestion (((ExtSet garfield) x (IntSet blue)) --> like))
;expected: [(Event ((((ExtSet garfield) * (IntSet blue)) --> like) (1.0 0.5692683291397822)) (eternal (Cons 7 (Cons 2 (Cons 1 (Cons 3 Nil)))) 0.0))]
;Please notice that it has revised it with the prior derived result, as you can also see in the evidence trail 1,2,3 being included
!(check-state)

;debug:
!(TupleCount (collapse (CountElement  (get-atoms &belief_events))))
;[8]
!(TupleCount (collapse (CountElement (get-atoms &attentional_focus))))
;[8]
!(TupleCount (collapse (CountElement (get-atoms &concepts))))
;[100]

