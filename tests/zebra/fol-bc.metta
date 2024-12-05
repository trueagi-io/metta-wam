;; First-order logic backward chainer using match

;; TODO: this is inefficient, we just need at least 1 match
(= (has-match $space $g)
    (let $m (collapse (match $space $g True))
        (if (== $m ())
            False
            True
        )
    )
)

;; Base case
(= (backward-chain $goal $kb $rb)
    (case $goal (
        ((is $a $b) (let $a $b (is $a $b))) ;; prolog eval & assign
        ((bool $expr) (if $expr $goal (empty))) ;; if predicate is a boolean. This might be unnecessary
        ((naf $expr) (if (has-match $kb $expr) (empty) $goal)) ;; negation-as-failure
        ($_ (match $kb $goal $goal))
    )) 
)

;; Recursive Case
(= (backward-chain $goal $kb $rb)
    (match $rb ($goal :- $body)
        (match-body $body $kb $rb $goal)
    )
)

;; do matching for each atom in the body, then returns goal
(= (match-body $body $kb $rb $goal)
    (if (== $body ())
        $goal
        (let* (
            (($cur $rest) (decons-atom $body))
            ; (() (println! (IN $cur $goal)))
            ($cur (backward-chain $cur $kb $rb))  ;; chain on cur & unify back to propagate vars to $goal
            ; (() (println! (OUT $cur $goal)))
        ) 
            (match-body $rest $kb $rb $goal)
        )
    )
)

