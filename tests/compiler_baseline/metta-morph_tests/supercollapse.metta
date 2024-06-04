

(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))


(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))

(: TupleConcat (-> Any Any Expression))

!(TupleConcat (a b c) (e f g))
!(TupleConcat () (e f g))
!(TupleConcat (a b c) ())


(= (range $K $N)
   (If (< $K $N)
       (TupleConcat ($K) (range (+ $K 1) $N))
       ()))

(unused
	(pragma! trace-on-exec False)
	(pragma! trace-on-eval False)
	(pragma! trace-length 1090)
	(pragma! stack-max 100))


!(range 1 1)

!(range 1 3)

!(range 2 6)

!(range 1 10)


