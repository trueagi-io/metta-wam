
;; will only be fast enough (for our 10 second limit) if compiled anyways?
!(pragma! compiler full)

!(import! &self NARS.metta)


!(pragma! trace-on-pass False)
!(pragma! trace-on-fail True)
!(pragma! trace-on-overflow True)
!(pragma! trace-on-error True)
!(pragma! tracelen 20)

; debug
!(assertEqual (TupleCount (get-atoms &belief_events)) 0)


;;NARS test
!(AddBeliefEvent ((({ garfield }) --> cat) (1.0 0.9)))
!(AddBeliefEvent (((cat * sky) --> like) (1.0 0.9)))
!(AddBeliefEvent ((sky --> ([ blue ])) (1.0 0.9)))


; debug
!("(get-atoms &belief_events)="  (get-atoms &belief_events))
!(assertEqual (TupleCount (get-atoms &belief_events)) 3)

;The following question needs both a deduction and abduction step:
!(assertEqual
  (EternalQuestion ((({ garfield }) * ([ blue ])) --> like))
  (Event (((({ garfield }) * ([ blue ])) --> like) (1.0 0.2965825874694874)) (eternal (Cons 2 (Cons 1 (Cons 3 Nil))) 0.643288027761712)))

;Lets stress the control mechanism as these type of events with common extension or intension causes dozens of derivations:
!(AddBeliefEvent ((A --> cat) (1.0 0.9)))
!(AddBeliefEvent ((B --> cat) (1.0 0.9)))
!(AddBeliefEvent ((C --> cat) (1.0 0.9)))

; debug
!(assertEqual (TupleCount (get-atoms &belief_events)) 7)

!(assertEqual (EternalQuestion ((A & B) --> cat))
  (Event (((A & B) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 4 (Cons 5 Nil)) (5 0.4525))))

!(assertEqual (EternalQuestion ((B & C) --> cat))
 (Event (((B & C) --> cat) (1.0 0.44751381215469616)) (eternal (Cons 5 (Cons 6 Nil)) (6 0.4525))))

!(assertEqual (EternalQuestion (((A & B) & C) --> cat))
  (Event ((((A & B) & C) --> cat) (1.0 0.42163100057836905)) (eternal (Cons 5 (Cons 4 (Cons 6 Nil))) (6 0.195593125))))

!(AddBeliefEvent (((({ garfield }) * ([ blue ])) --> like) (1.0 0.9)))

!(assertEqual (EternalQuestion ((({ garfield }) * ([ blue ])) --> like))
 (Event (((({ garfield }) * ([ blue ])) --> like) (1.0 0.5692683291397822)) (eternal (Cons 7 (Cons 2 (Cons 1 (Cons 3 Nil)))) 0.0)))
;Please notice that it has revised it with the prior derived result, as you can also see in the evidence trail 1,2,3 being included

;debug:
!(assertEqual (CollapseCardinality (get-atoms &belief_events)) 8)
!(assertEqual (CollapseCardinality (get-atoms &attentional_focus)) 8)
!(assertEqual (CollapseCardinality (get-atoms &concepts)) 100)

