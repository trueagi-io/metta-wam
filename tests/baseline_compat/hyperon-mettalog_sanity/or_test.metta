!(bind! &evaluated (new-state False))
(: set-evaluated (-> Bool))  ; this must return boolean
(= (set-evaluated)
   ((change-state! &evaluated True) True))

; !(pragma! or-impl eager-or) ; evals all like superpose (hyperon's current version)
!(pragma! or-impl lazy-or) ; evals until the first True happens (we think that is what people assume happens)

;; assertEqualToResult is used here to force all solutions (backtracking)
!(bind! &evaluated (new-state False))
!(assertEqualToResult (or True (set-evaluated)) (True))
!(assertEqual (get-state &evaluated) False)


;; assertEqualToResult is used here to force all solutions (backtracking)
!(bind! &evaluated (new-state False))
!(assertEqualToResult (or False (set-evaluated)) (True))
!(assertEqual (get-state &evaluated) True)


;; Basic `or` Logic
!(assertEqual (or True False) True)
!(assertEqual (or False True) True)
!(assertEqual (or True True) True)
!(assertEqual (or False False) False)

;; Makes sure they only return single nondet result
!(assertEqualToResult (or True False) (True))
!(assertEqualToResult (or False True) (True))
!(assertEqualToResult (or True True) (True))
!(assertEqualToResult (or False False) (False))

;; or with Expressions
!(assertEqual (or (> 5 3) (< 2 1)) True)
!(assertEqual (or (== 1 1) (== 2 2)) True)
!(assertEqual (or (not True) (and True False)) False)


(: mprogn (-> Atom Atom Atom ))
(= (mprogn $code1 $code2) (let $_ (eval $code1) (eval $code2)))

;; or with Side Effects to Ensure Both Expressions Are Evaluated
!(assertEqualToResult (or (mprogn (println! "First") True) (mprogn (println! "Second") False)) (True))
!(assertEqualToResult (or (mprogn (println! "First") True) (mprogn (println! "Second") True)) (True))
!(assertEqualToResult (or (mprogn (println! "First") False) (mprogn (println! "Second") True)) (True))
!(assertEqualToResult (or (mprogn (println! "First") False) (mprogn (println! "Second") False)) (False))


;; test superposes booleans
;
!(assertEqualToResult  
     (or (superpose (False True)) (superpose (False True))) 
     (False True True True))
