

(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

; (: TupleConcat (-> Any Any AnyRet))

(= (TupleConcat $Ev1 $Ev2) 
  (let ($V1 $V2) ($Ev1 $Ev2)
		 (collapse (superpose ((superpose $V1) (superpose $V2))))))

!(TupleConcat (a b c) (e f g))
!(TupleConcat () (e f g))
!(TupleConcat (a b c) ())

(= (range $K $N)
   (If (< $K $N)
       (TupleConcat ($K) (range (+ $K 1) $N))
       ()))

!(TupleConcat (a b c) (range 1 1))

(unused
	(pragma! trace-on-exec False)
	(pragma! trace-on-eval False)
	(pragma! trace-length 1090)
	(pragma! stack-max 100))


!(range 1 1)

!(range 1 3)

!(range 2 6)

!(range 1 10)


