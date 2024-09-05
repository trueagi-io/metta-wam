;; Basic or Logic
!(assertEqual (or True False) True)
!(assertEqual (or False True) True)
!(assertEqual (or True True) True)
!(assertEqual (or False False) False)

;; or with Expressions
!(assertEqual (or (> 5 3) (< 2 1)) True)
!(assertEqual (or (== 1 1) (== 2 2)) True)
!(assertEqual (or (not True) (and True False)) False)

(: mprogn (-> Atom Atom Atom ))
(= (mprogn $code1 $code2) (let $_ (eval $code1) (eval $code2)))

;; or with Side Effects to Ensure Both Expressions Are Evaluated
!(assertEqual (or (mprogn (println! "First") True) (mprogn (println! "Second") False)) True)
!(assertEqual (or (mprogn (println! "First") True) (mprogn (println! "Second") True)) True)
!(assertEqual (or (mprogn (println! "First") False) (mprogn (println! "Second") True)) True)
!(assertEqual (or (mprogn (println! "First") False) (mprogn (println! "Second") False)) False)
