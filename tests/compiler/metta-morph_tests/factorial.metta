(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))


(Arity facF 2)

(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))

!(call-string! "listing(facF/2).")

!(facF 10)
