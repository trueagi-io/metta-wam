;; Basic XOR Logic
!(assertEqual (xor True False) True)
!(assertEqual (xor False True) True)
!(assertEqual (xor True True) False)
!(assertEqual (xor False False) False)

;; XOR with Expressions
!(assertEqual (xor (> 5 3) (< 2 1)) True)
!(assertEqual (xor (== 1 1) (== 2 2)) False)
!(assertEqual (xor (not True) (and True False)) False)

(: mprogn (-> Atom Atom Atom ))
(= (mprogn $code1 $code2) (let $_ (eval $code1) (eval $code2)))

;; XOR with Side Effects to Ensure Both Expressions Are Evaluated
!(assertEqual (xor (mprogn (println! "First") True) (mprogn (println! "Second") False)) True)
!(assertEqual (xor (mprogn (println! "First") True) (mprogn (println! "Second") True)) False)
!(assertEqual (xor (mprogn (println! "First") False) (mprogn (println! "Second") True)) True)
!(assertEqual (xor (mprogn (println! "First") False) (mprogn (println! "Second") False)) False)
