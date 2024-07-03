!(pragma! compile full)

;!(nop (pragma! load silent))
;spaces
!(bind! &concepts (new-space))
!(bind! &attentional_focus (new-space))


!(bind! &kb0 (new-space))
!(add-atom &kb0 1)
!(add-atom &kb0 2)
!(add-atom &kb0 2.0)
!(add-atom &kb0 (bind! &belief_events (new-space)))
!(add-atom &kb0 (space 1 2 3))
!(add-atom &kb0 (5 a (2) 3))

;spaces
!(bind! &belief_events (new-space))
!(bind! &goal_events (new-space))
!(bind! &currentTime (new-state 1))
!(bind! &evidentialBase (new-state 1))
!(bind! &FIFO (new-state ()))
!(bind! &tempbest (new-state ()))
!(bind! &tempbestscore (new-state 0))

;; stdlib extension
(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) ())
(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))
(= (max $1 $2) (If (> $1 $2) $1 $2))
(= (min $1 $2) (If (< $1 $2) $1 $2))
(= (abs $x) (If (< $x 0) (- 0 $x) $x))
(: sequential (-> Expression %Undefined% ))
(= (sequential $1) (superpose $1))

(: do (-> Expression %Undefined% ))
(= (do $1) (case $1 ()))
;(= (TupleCount ()) 0)
;(= (TupleCount (1)) 1)
;(= (BuildTupleCounts $TOld $C $N)
;   (let $T (collapse (superpose (1 (superpose $TOld))))
;        (superpose ((add-atom &self (= (TupleCount $T) (+ $C 2)))
;                    (If (< $C $N) (BuildTupleCounts $T (+ $C 1) $N))))))*/

(: CountElement (-> Expression Number))
(= (CountElement $x) (case $x (($y 1))))

; (: = (-> Atom Atom Atom)) !(get-atoms &self)

; (= (TupleCount $list) (length! $list))
(= (TupleCount $tuple) (if (== $tuple ()) 0 (+ 1 (TupleCount (cdr-atom $tuple)))))
(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))


!(CountElement (get-atoms &kb0))

!(collapse (CountElement (get-atoms &kb0)))


!(TupleCount (collapse (CountElement (get-atoms &kb0))))

