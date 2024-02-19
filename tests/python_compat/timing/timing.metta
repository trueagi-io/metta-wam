(= (factorial $n)
   (If (== $n 0)
       1
       (* $n (factorial (- $n 1)))))

(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))

(= (range $K $N)
   (If (< $K $N)
       (TupleConcat ($K) (range (+ $K 1) $N))
       ()))

(= (TupleCount $tuple) (If (== $tuple ()) 0 (+ 1 (TupleCount (cdr-atom $tuple)))))

(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True overlap)))))))
