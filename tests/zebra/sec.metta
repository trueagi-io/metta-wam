;; Extremely simplified event calculus with deductive temporal projection task
;; No constraints or preconditions

(: InitiallyP (-> Atom Atom))
(: InitiallyN (-> Atom Atom))
(: Holds (-> Atom Number Atom))
(: Happens (-> Atom Number Atom))
(: Initiates (-> Atom Atom Atom))
(: Terminates (-> Atom Atom Atom))

;; TODO: use with limit, we only need check at least 1
(= (has-match $space $pattern)
    (let $all (collapse (match $space $pattern True))
        (if (== $all ())
            False True
        )
    )
)

(: add-holds! (-> Grounded Atom Atom Number (->)))
(= (add-holds! $kb $fluent $TrueOrFalse $time)
    (if (or 
            (== $fluent NIL) 
            (has-match $kb (Holds ($fluent := $TrueOrFalse) $time))) 
        ()
        (add-atom $kb (Holds ($fluent := $TrueOrFalse) $time)))
)

(: add-holds-list! (-> Grounded Expression Atom Number (->)))
(= (add-holds-list! $kb $fluents $TrueOrFalse $time)
    (if (== $fluents ())
        ()
        (let* (
            (($head $rest) (decons-atom $fluents))
            (() (add-holds! $kb $head $TrueOrFalse $time))
        ) (add-holds-list! $kb $rest $TrueOrFalse $time))
    )
)

(: temporal-project-helper! (-> Grounded Number Number (->)))
(= (temporal-project-helper! $kb 0 $maxTime)
    (let* (
        ($posFluents (collapse (match $kb (InitiallyP $x) $x)))
        ($negFluents (collapse (match $kb (InitiallyN $y) $y)))
        (() (add-holds-list! $kb $posFluents true 0))
        (() (add-holds-list! $kb $negFluents false 0)) 
    ) (temporal-project-helper! $kb 1 $maxTime))
)
(= (temporal-project-helper! $kb $t $maxTime)
    (if (or (== $t 0) (> $t $maxTime))  ()
        (let* (
            ($tp (- $t 1))
            ;; if initiated
            ($posFluents (collapse (match $kb
                (, (Happens $ep $tp)
                    (Initiates $ep $fp)
                ) $fp
            )))
            (() (add-holds-list! $kb $posFluents true $t))

            ;; iertia for pos fluents
            ($prevPosFluents (collapse (match $kb (Holds ($fp1 := true) $tp) $fp1)))
            ($1_ (map-atom $prevPosFluents $posF1 (let ()
                ;; covertly negation-as-failure
                (if (has-match $kb (, (Happens $ep1 $tp) (Terminates $ep1 $posF1))) 
                    ()
                    (add-holds! $kb $posF1 true $t)
                )
                ()
            )))
            
            ;; if terminated
            ($negFluents (collapse (match $kb
                (, (Happens $en $tp)
                    (Terminates $en $fn)
                ) $fn
            )))
            (() (add-holds-list! $kb $negFluents false $t))

            ; ;; inertia for negative fluents
            ($prevNegFluents (collapse (match $kb (Holds ($fn1 := false) $tp) $fn1)))
            ($2_ (map-atom $prevNegFluents $negF1 (let ()
                (if (has-match $kb (, (Happens $en1 $tp) (Initiates $en1 $negF1)))
                    ()
                    (add-holds! $kb $negF1 false $t)
                )
                ()
            )))
        ) (temporal-project-helper! $kb (+ 1 $t) $maxTime))
    )
)

(= (temporal-project! $kb $maxTime)
    (temporal-project-helper! $kb 0 $maxTime)
)

(= (add-atoms! $kb $atoms-list)
    (if (== $atoms-list ()) ()
        (let* (
            (($head $rest) (decons-atom $atoms-list))
            (() (add-atom $kb $head))
        ) (add-atoms! $kb $rest))
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem Encodings ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yale Shooting scenario
!(bind! &yaleshooting (new-space))
!(add-atoms! &yaleshooting (
    (InitiallyP Unloaded) (InitiallyP Alive)
    (InitiallyN Loaded) (InitiallyN Dead)
    (Initiates Load Loaded) (Terminates Load Unloaded)
    (Initiates Shoot Dead) (Terminates Shoot Alive)
    (Initiates Shoot Unloaded) (Terminates Shoot Loaded)
    (Happens Load 1) (Happens Wait 2) (Happens Shoot 3)
))

!(temporal-project! &yaleshooting 4)

!(match &yaleshooting 
    (, (Holds ($f := true) $t)
        (Holds ($f := false) $t)
    )
    (Error (Yaleshooting $f $t) ContradictingFluent)
)
!(assertEqualToResult 
    (match &yaleshooting (Holds (Unloaded := true) $t) $t)
    (0 1 4))
!(assertEqualToResult 
    (match &yaleshooting (Holds (Unloaded := false) $t) $t)
    (2 3))
!(assertEqualToResult 
    (match &yaleshooting (Holds (Loaded := true) $t) $t)
    (2 3))
!(assertEqualToResult 
    (match &yaleshooting (Holds (Loaded := false) $t) $t)
    (0 1 4))
!(assertEqualToResult 
    (match &yaleshooting (Holds (Alive := true) $t) $t)
    (0 1 2 3))
!(assertEqualToResult 
    (match &yaleshooting (Holds (Alive := false) $t) $t)
    (4))
!(assertEqualToResult 
    (match &yaleshooting (Holds (Dead := false) $t) $t)
    (0 1 2 3))
!(assertEqualToResult 
    (match &yaleshooting (Holds (Dead := true) $t) $t)
    (4))
