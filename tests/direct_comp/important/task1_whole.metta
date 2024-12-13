
;; took 37 seconds

(:object a) (:object b)
(:object c) ;;
(:init (
    (on-table a)
    (on-table b)
    (clear a)
    (clear b)
    (arm-empty)
    (on-table c) (clear c) ;;
))
(:goal (And (
    (on a b)
    (on b c) ;;
)))


(:action pickup 
    :parameters ($x)
    :precondition (And ((clear $x) (on-table $x) (arm-empty)))
    :effect (And ((holding $x) (Not (clear $x)) 
        (Not (on-table $x)) (Not (arm-empty)))
    )
)
(:action putdown
    :parameters ($x)
    :precondition (And ((holding $x)))
    :effect (And ((clear $x) (arm-empty) (on-table $x)
        (Not (holding $x))))
)

(:action stack
    :parameters ($x $y)
    :precondition (And (
        (clear $y) (holding $x)
    ))
    :effect (And (
        (arm-empty) (clear $x) (on $x $y) 
        (Not (clear $y)) (Not (holding $x))
    ))
)

(:action unstack
    :parameters ($x $y)
    :precondition (And ((on $x $y) (clear $x) (arm-empty)))
    :effect (And (
        (holding $x) (clear $y) (Not (on $x $y))
        (Not (clear $x)) (Not (arm-empty))
    ))
)

;; TODO: how to levarage native MeTTa for typing?

;; (State id fluent) ;; only true fluent

;; check satisfaction of a ground formula

;!(import! &self task0) ;; Hyperon requires to be at the top
;!(import! &self task1) ;; Hyperon requires to be at the top
;!(import! &self task2) ;; Hyperon requires to be at the top

(= (formula-satisfaction $stateId (Not $expr))
    (not (formula-satisfaction $stateId $expr))
)
(= (formula-satisfaction $stateId (Or $exprList))
    (if (== $exprList ())
        False
        (let* (
            (($head $rest) (decons-atom $exprList))
            ($v (formula-satisfaction $stateId $head))
            ) (if $v True (or $v (formula-satisfaction $stateId (Or $rest)))))
    )
)
(= (formula-satisfaction $stateId (And $exprList))
    (if (== $exprList ())
        True
        (let* (
            (($head $rest) (decons-atom $exprList))
            ($v (formula-satisfaction $stateId $head))
            ) (if (not $v) False (and $v (formula-satisfaction $stateId (And $rest)))))
    )
)

(= (formula-satisfaction $stateId $expr) ;; base case, match positive fluent
    (case $expr (
        ((And $x) (empty))
        ((Or $x) (empty))
        ((Not $x) (empty))
        ($1 (let $all (collapse (match &self (State $stateId $expr) True))
                (if (== $all ()) False True)))
    ))
)

;; check if an action is applicable in a state
(= (action-applicable $stateId $actionId $args)
    (match &self (:action $actionId
            :parameters $params 
            :precondition $preds 
            :effect $effs
    )  (unf $args $params 
            (formula-satisfaction $stateId $preds) 
            (Error $params FailedUnification)))
)

;; (:object x) TODO: object typing, how to only extract objects with potential updating? in a state?
;; all possible grounding for a variable ~ objects
(= (args-combination $params)
    (if (== $params ()) ()
        (let* (
            (($1 $rest) (decons-atom $params))
            ($h (match &self (:object $o) $o))
            ($r (args-combination $rest))
        ) (cons-atom $h $r))
    )
)

;; for each action & combination of args, try if its precondition is satisfied in state
(= (actions-applicable $stateId)
    (match &self (:action $actionId
            :parameters $params 
            :precondition $preds 
            :effect $effs
    ) (let* (
        ($args (args-combination $params))
        ($1 (unf $args $params True (Error $params FailedUnification)))
        ) (if (formula-satisfaction $stateId $preds) ($actionId $params) (empty))
    ))
)

(= (action-applyEffect $actionId $args)
    (match &self (:action $actionId
            :parameters $params 
            :precondition $preds 
            :effect $effs
    ) (unf $args $params $effs FailedUnification))
)

(= (signed-fluents $list)
    (if (== $list ())
        (() ())
        (let* (
            (($h $rest) (decons-atom $list))
            (($pos $neg) (signed-fluents $rest))
            (($isNeg $f) (unf $h (Not $v) (True $v) (False $h)))
        ) (if $isNeg 
            ($pos (cons-atom $f $neg))
            ((cons-atom $f $pos) $neg)
        ))
    )
)

;; diffing algorithm? 
(= (state-diff $allStates $diffs)
    (let* (
        ((And $d) $diffs)
        (($pos $neg) (signed-fluents $d))
        ($s1 (collapse (subtraction  ;; remove negative fluents
            (superpose $allStates)
            (superpose $neg)
        )))
        ($s2 (collapse (union  ;; add any positive fluents
            (superpose $s1)
            (superpose $pos)
        )))
    )  $s2)
)

;; a list of atom to add for the next non-determinisitc state (out of all possible states)
(= (state-transition $stateId) 
    (let* (
        (($actionId $args) (actions-applicable $stateId))
        ($effsDiff (action-applyEffect $actionId $args))
        ($stsEffs (collapse (match &self (State $stateId $form) $form)))
        ($newState (state-diff $stsEffs $effsDiff))
    ) 
        ; $newState
        (if (== $newState ()) 
            (Error ($stateId $effsDiff $stsEffs) "Empty State") 
            (($actionId $args)  $newState)
        )
    )
)

(= (state-visited? $stateFluents)
    (if (== $stateFluents ()) ()
        (let* (
            (($h $rest) (decons-atom $stateFluents))
            ($included (collapse (match &self (State $id $h) $id)))
        ) (if (== $rest ()) 
            $included 
            (let $other (state-visited? $rest) (L-intersection $included $other))
        ))
    )
)
(= (state-visited $stateId)
    (let $fs (collapse (match &self (State $stateId $form) $form))
        (state-visited? $fs)
    )
)

(= (state-enqueued? $stateId)
    (let $s (collapse (match &self (State $stateId $f) $stateId))
        (if (== $s ()) False True)
    )
)

(= (goal-satisfied $stateId)
    (match &self (:goal $expr) (formula-satisfaction $stateId $expr))
)

(= (retrace-steps $toStateId)
    (if (== $toStateId 0)
        ()
        (let* (
            (($fromId $action $args) (match &self 
                    (Succ $fromId ($action $args) $toStateId)
                    ($fromId $action $args)))
            ($prevSteps (retrace-steps $fromId))
        ) (L-push-back $prevSteps ($action $args)))
    )
)

;; Performance note: use space for global state versus pass as arguments, 
;; depends on complexity of (match)
;; TODO: match in space versus list

;!(import! &self alist)

(= (add-state-fluents! $stateId $fluents)
    (if (== $fluents ())
        ()
        (let* (
            (($h $rest) (decons-atom $fluents))
            (() (add-atom &self (State $stateId $h)))
        ) (add-state-fluents! $stateId $rest))
    )
)

;; states = [(fluents) (fluents2)]
;; for each next state, add its fluents and enqueue id
(= (enqueue-next-states! $fromID $states $nextUID)
    (if (== $states ())
        $nextUID
        (let* (
            (($state $rest) (decons-atom $states))
            ((($action $args) $stateFluents) $state)
            ($visited (state-visited? $stateFluents))
        ) (if (== $visited ())  
            (let* (
                (() (add-state-fluents! $nextUID $stateFluents))
                (() (add-atom &self (Succ $fromID ($action $args) $nextUID)))
                (() (println! (ENQUEUED $nextUID)))
                ($id (+ $nextUID 1))
            ) (enqueue-next-states! $fromID $rest $id))

            (enqueue-next-states! $fromID $rest $nextUID)) ;; skipped
        ))
)

(= (fw-state-search $curState $nextUID $statesLimit) ;; CHECK VISITED STATE?
    
    (let () 
            ; ()
            (println! 
                (Processing $curState 
                    (collapse (match &self (State $curState $f) $f))
                    (collapse (match &self (Succ $from ($action $args) $curState) ($from ($action $args))))
                    ; (state-visited $curState)
                ))
        (if (not (state-enqueued? $curState)) FailedToReachGoal
        (if (goal-satisfied $curState) (retrace-steps $curState)
        (if (> $curState $statesLimit)  (Error $statesLimit "Limit of states exploration reached.")   
        (let* (
            ($nextStates (collapse (state-transition $curState)))
            ($uid (enqueue-next-states! $curState $nextStates $nextUID))
            ($nextFront (+ $curState 1))
        ) (fw-state-search $nextFront $uid $statesLimit)))))
    )
)

(= (planner-main $statesLimit)
    (let* (
        ($initState ((None None) (match &self (:init $f) $f)))
        ($nextUID (enqueue-next-states! -1 ($initState) 0))
    ) 
    ; ()
    (fw-state-search 0 $nextUID $statesLimit)
    )
)

;; NEXT: modularize space

;; work around for unify predicate
(= (unf $x $y $s $f)
    (let $unf (collapse 
        (let $x $y True)
    ) (if (== $unf ()) $f (let $x $y $s)))
)

;; state fluents must all be grounded, TODO: semantics for non-ground state?
; !(add-atom &self (State 1 (clear b))) 
; !(add-atom &self (State 1 (on-table b)))
; !(add-atom &self (State 1 (arm-empty)))
; !(state-visited? ((clear b) (on-table b) (arm-empty)))

; !(formula-satisfaction 1 (clear b))
; !(formula-satisfaction 1 (Not (holding b)))
; !(formula-satisfaction 1 (Or ((clear b) (holding b))))
; !(formula-satisfaction 1 (And ((clear b) (Not (holding b)))))
; !(formula-satisfaction 1 
;     (Or ((arm-empty) (And ((holding b) (clear b)))))
; )

; !(action-applicable 1 pickup (b))
; !(action-applicable 1 putdown (b))
; !(actions-applicable 1)

;; TODO: mutable data structure in MeTTa?

;; O(n) queue
(= (make-queue) ())
(= (empty-queue? $q) (unify $q () True False))
(= (front-queue $q) 
    (if (empty-queue? $q)
        (Error $q "Can't get front of empty queue")
        (car-atom $q)
    )
)
(= (pop-queue $q) (cdr-atom $q))
(= (insert-queue $q $item) 
    (if (== $q ())
        ($item)
        (let* (
            (($head $tail) (decons-atom $q))
            ($inserted (insert-queue $tail $item))
        ) (cons-atom $head $inserted))
    )
)

; !(let* (
;     ($q (make-queue))
;     ($q1 (insert-queue $q 1))
;     (() (println! $q1))
;     ($q2 (insert-queue $q1 3))
;     ($el (front-queue $q2))
;     ($q3 (pop-queue $q2))
;     (() (println! (Element $el)))
;     (() (println! $q3))
;     ($q4 (insert-queue $q3 5))
;     (() (println! $q4))
; ) ())
(:action pickup 
    :parameters ($x)
    :precondition (And ((clear $x) (on-table $x) (arm-empty)))
    :effect (And ((holding $x) (Not (clear $x)) 
        (Not (on-table $x)) (Not (arm-empty)))
    )
)
(:action putdown
    :parameters ($x)
    :precondition (And ((holding $x)))
    :effect (And ((clear $x) (arm-empty) (on-table $x)
        (Not (holding $x))))
)

(:action stack
    :parameters ($x $y)
    :precondition (And (
        (clear $y) (holding $x)
    ))
    :effect (And (
        (arm-empty) (clear $x) (on $x $y) 
        (Not (clear $y)) (Not (holding $x))
    ))
)

(:action unstack
    :parameters ($x $y)
    :precondition (And ((on $x $y) (clear $x) (arm-empty)))
    :effect (And (
        (holding $x) (clear $y) (Not (on $x $y))
        (Not (clear $x)) (Not (arm-empty))
    ))
)




;; Collection of general-purpose utilities for list (or atom as list)
;; Inefficient and untyped, for quick prototyping only

(= (L-empty? $list)
    (if (== $list ()) True False)
)

(= (L-size $list)
    (if (L-empty? $list)
        0
        (let $tail (cdr-atom $list) (+ 1 (L-size $tail)))
    )
)


(= (L-push-front $list $item)
    (cons-atom $item $list)
)

(= (L-push-back $list $item)
    (if (L-empty? $list)
        ($item)
        (let* (
            (($h $t) (decons-atom $list))
            ($pushed (L-push-back $t $item))
        ) (cons-atom $h $pushed))
    )
)


(= (L-pop-front $list)
    (if (L-empty? $list)
        (Error L-pop-front "Empty list")
        (cdr-atom $list)
    )
)

(= (L-pop-back $list)
    (if (L-empty? $list)
        (Error L-pop-back "Empty list")
        (let ($h $t) (decons-atom $list)
            (if (L-empty? $t)
                ()
                (let $popped (L-pop-back $t)
                    (cons-atom $h $popped)
                )
            )
        )
    )
)

(= (L-append $list $list2)
    (if (L-empty? $list)
        $list2
        (let* (
            (($h $rest) (decons-atom $list))
            ($appended (L-append $rest $list2))
        ) (cons-atom $h $appended))
    )
)

(= (L-front $list)
    (if (L-empty? $list)
        (Error L-front "Empty list")
        (car-atom $list)
    )
)

(= (L-back $list)
    (if (L-empty? $list)
        (Error L-back "Empty list")
        (let ($h $rest) (decons-atom $list)
            (if (L-empty? $rest) 
                $h 
                (L-back $rest)
            )
        )
    )
)

(: L-index (-> Expression Number Atom))
(= (L-index $list $index)
    (if (or (< $index 0) (and (>= $index 0) (L-empty? $list)))
        (Error L-index "Index out of range")
        (if (== $index 0)
            (car-atom $list)
            (let* (
                ($rest (cdr-atom $list))
                ($idx (- $index 1))
            ) (L-index $rest $idx))
        )
    )
)


(= (L-contains? $list $item)
    (if (L-empty? $list)
        False
        (let ($h $t) (decons-atom $list)
            (if (== $h $item) True (L-contains? $t $item))
        )
    )
)

(= (L-subset? $list1 $list2)
    (if (== $list1 ())
        True
        (let ($h $rest) (decons-atom $list1)
            (if (L-contains? $list2 $h)
                (L-subset? $rest $list2)
                False
            )
        )
    )
)

(= (L-seteq? $list1 $list2)
    (and (L-subset? $list1 $list2)
        (L-subset? $list2 $list1))
)

(= (L-intersection $list1 $list2)
    (if (== $list1 ())
        ()
        (let* (
            (($h $t) (decons-atom $list1))
            ($inters (L-intersection $t $list2))
        ) (if (L-contains? $list2 $h)
            (cons-atom $h $inters)
            $inters
        ))
    )
)



; !(state-transition 1)
;; Hyperon doesn't allow for importing multiple files into same space
!(planner-main 100)
