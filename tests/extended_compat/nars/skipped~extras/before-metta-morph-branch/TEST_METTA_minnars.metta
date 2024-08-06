;; stdlib extension
(: If (-> Bool Atom Atom))
(: If Function) ;; FP
(= (If True $then) $then)
(= (If False $then) (let $x 0 (let $x 1 $x)))
(: If (-> Bool Atom Atom Atom))
(: If Function) ;; FP
(= (If $cond $then $else) (if $cond $then $else))

(: TupleConcat (-> List List List))
(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))

(: max (-> Number Number Number))
(arity max 3) ;; FP
(= (max $1 $2) (If (> $1 $2) $1 $2))

(: min (-> Number Number Number))
(arity min 3) ;; FP
(= (min $1 $2) (If (< $1 $2) $1 $2))

(: abs (-> Number Number))
(arity abs 2) ;; FP
(= (abs $x) (If (< $x 0) (- 0 $x) $x))
(: sequential (-> Expression %Undefined%))
(arity sequential 1) ;; FP
(= (sequential $1) (superpose $1))
(: do (-> Expression %Undefined%))
(arity do 1) ;; FP
(= (do $1) (case $1 ()))


(: TupleCount (-> List Number))
(: TupleCount BinaryPredicate)
(arity TupleCount 999) ;; FP
(= (TupleCount ()) 0)
(= (TupleCount (1)) 1)
(arity BuildTupleCounts 3) ;; FP
(= (BuildTupleCounts $TOld $C $N)
   (let $T (collapse (superpose (1 (superpose $TOld))))
        (superpose ((trace! (= (TupleCount $T) (+ $C 2)) ())
                    (If (< $C $N) (BuildTupleCounts $T (+ $C 1) $N))))))
(: CountElement (-> Expression Number))
(arity CountElement 2) ;; FP
(= (CountElement $x) (case $x (($y 1))))
(: CollapseCardinality (-> Expression Number))
(arity CollapseCardinality 2) ;; FP
(= (CollapseCardinality $expression) (TupleCount (collapse (CountElement $expression))))

(= (TupleCount (1 1)) 2)
(= (TupleCount (1 1 1)) 3)
(= (TupleCount (1 1 1 1)) 4)
(= (TupleCount (1 1 1 1 1)) 5)
(= (TupleCount (1 1 1 1 1 1)) 6)
(= (TupleCount (1 1 1 1 1 1 1)) 7)
(= (TupleCount (1 1 1 1 1 1 1 1)) 8)
(= (TupleCount (1 1 1 1 1 1 1 1 1)) 9)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1)) 10)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1)) 11)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1)) 12)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1)) 13)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 14)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 15)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 16)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 17)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 18)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 19)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 20)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 21)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 22)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 23)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 24)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 25)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 26)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 27)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 28)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 29)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 30)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 31)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 32)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 33)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 34)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 35)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 36)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 37)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 38)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 39)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 40)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 41)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 42)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 43)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 44)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 45)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 46)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 47)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 48)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 49)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 50)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 51)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 52)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 53)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 54)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 55)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 56)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 57)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 58)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 59)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 60)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 61)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 62)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 63)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 64)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 65)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 66)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 67)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 68)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 69)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 70)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 71)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 72)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 73)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 74)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 75)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 76)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 77)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 78)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 79)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 80)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 81)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 82)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 83)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 84)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 85)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 86)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 87)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 88)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 89)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 90)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 91)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 92)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 93)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 94)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 95)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 96)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 97)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 98)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 99)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 100)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 101)
(= (TupleCount (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 102)


;; Truth functions

(: Truth_c2w (-> Number  Number))
(arity Truth_c2w 2) ;; FP
(= (Truth_c2w $c) (/ $c (- 1 $c)))

(: Truth_w2c (->  Number  Number))
(arity Truth_w2c 2) ;; FP
(= (Truth_w2c $w) (/ $w (+ $w 1)))

(: Truth_Deduction (-> TruthValue TruthValue TruthValue))
(arity Truth_Deduction 999) ;; FP
(= (Truth_Deduction ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))

(: Truth_Abduction (-> TruthValue TruthValue TruthValue))
(arity Truth_Abduction 999) ;; FP
(= (Truth_Abduction ($f1 $c1) ($f2 $c2)) ($f2 (Truth_w2c (* (* $f1 $c1) $c2))))

(: Truth_Induction (-> TruthValue TruthValue TruthValue))
(arity Truth_Induction 3) ;; FP
(= (Truth_Induction $T1 $T2) (Truth_Abduction $T2 $T1))

(: Truth_Exemplification (-> TruthValue TruthValue TruthValue))
(arity Truth_Exemplification 3) ;; FP
(= (Truth_Exemplification ($f1 $c1) ($f2 $c2)) (1.0 (Truth_w2c (* (* $f1 $f2) (* $c1 $c2)))))

(: Truth_StructuralDeduction (-> TruthValue TruthValue))
(arity Truth_StructuralDeduction 2) ;; FP
(= (Truth_StructuralDeduction $T) (Truth_Deduction $T (1.0 0.9)))

(:  Truth_Negation (-> TruthValue TruthValue))
(arity Truth_Negation 2) ;; FP
(= (Truth_Negation ($f $c)) ((- 1 $f) $c))

(:  Truth_StructuralDeductionNegated (-> TruthValue TruthValue))
(arity Truth_StructuralDeductionNegated 2) ;; FP
(= (Truth_StructuralDeductionNegated $T) (Truth_Negation (Truth_StructuralDeduction $T)))

(: Truth_Intersection (-> TruthValue TruthValue TruthValue))
(arity Truth_Intersection 3) ;; FP
(= (Truth_Intersection ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* $c1 $c2)))

(: Truth_StructuralIntersection (-> TruthValue TruthValue))
(arity Truth_StructuralIntersection 2) ;; FP
(= (Truth_StructuralIntersection $T) (Truth_Intersection $T (1.0 0.9)))

(: Truth_or (-> TruthValue TruthValue Number))
(arity Truth_or 3) ;; FP
(= (Truth_or $a $b) (- 1 (* (- 1 $a) (- 1 $b))))

(: Truth_Comparison (-> TruthValue TruthValue TruthValue))
(arity Truth_Comparison 3) ;; FP
(= (Truth_Comparison ($f1 $c1) ($f2 $c2)) (let $f0 (Truth_or $f1 $f2) ((If (== $f0 0.0) 0.0 (/ (* $f1 $f2) $f0)) (Truth_w2c (* $f0 (* $c1 $c2))))))

(: (Truth_Analogy (-> TruthValue TruthValue TruthValue))
(arity Truth_Analogy 3) ;; FP
(= (Truth_Analogy ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* (* $c1 $c2) $f2)))

(: (Truth_Resemblance (-> TruthValue TruthValue TruthValue))
(arity Truth_Resemblance 3) ;; FP
(= (Truth_Resemblance ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* (* $c1 $c2) (Truth_or $f1 $f2))))

(: (Truth_Union (-> TruthValue TruthValue TruthValue))
(arity Truth_Union 3) ;; FP
(= (Truth_Union ($f1 $c1) ($f2 $c2)) ((Truth_or $f1 $f2) (* $c1 $c2)))

(: (Truth_Difference (-> TruthValue TruthValue TruthValue))
(arity Truth_Difference 3) ;; FP
(= (Truth_Difference ($f1 $c1) ($f2 $c2)) ((* $f1 (- 1 $f2)) (* $c1 $c2)))

(: (Truth_DecomposePNN (-> TruthValue TruthValue TruthValue))
(arity Truth_DecomposePNN 3) ;; FP
(= (Truth_DecomposePNN ($f1 $c1) ($f2 $c2)) (let $fn (* $f1 (- 1 $f2)) ((- 1 $fn) (* $fn (* $c1 $c2)))))

(: (Truth_DecomposeNPP (-> TruthValue TruthValue TruthValue))
(arity Truth_DecomposeNPP 3) ;; FP
(= (Truth_DecomposeNPP ($f1 $c1) ($f2 $c2)) (let $f (* (- 1 $f1) $f2) ($f (* $f (* $c1 $c2)))))

(: (Truth_DecomposePNP (-> TruthValue TruthValue TruthValue))
(arity Truth_DecomposePNP 3) ;; FP
(= (Truth_DecomposePNP ($f1 $c1) ($f2 $c2)) (let $f (* $f1 (- 1 $f2)) ($f (* $f (* $c1 $c2)))))

(: (Truth_DecomposePPP (-> TruthValue TruthValue TruthValue))
(arity Truth_DecomposePPP 3) ;; FP
(= (Truth_DecomposePPP $v1 $v2) (Truth_DecomposeNPP (Truth_Negation $v1) $v2))

(: (Truth_DecomposeNNN (-> TruthValue TruthValue TruthValue))
(arity Truth_DecomposeNNN 3) ;; FP
(= (Truth_DecomposeNNN ($f1 $c1) ($f2 $c2)) (let $fn (* (- 1 $f1) (- 1 $f2)) ((- 1 $fn) (* $fn (* $c1 $c2)))))

(: (Truth_Eternalize (-> TruthValue TruthValue))
(arity Truth_Eternalize 2) ;; FP
(= (Truth_Eternalize ($f $c)) ($f (Truth_w2c $c)))

(: (Truth_Revision (-> TruthValue TruthValue TruthValue))
(arity Truth_Revision 3) ;; FP
(= (Truth_Revision ($f1 $c1) ($f2 $c2)) 
   (let* (($w1 (Truth_c2w $c1)) ($w2 (Truth_c2w $c2)) ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w)) ($c (Truth_w2c $w)))
          ((min 1.00 $f) (min 0.99 (max (max $c $c1) $c2)))))
(: Truth_Expectation (-> TruthValue TruthValue))
(arity Truth_Expectation 2) ;; FP
(= (Truth_Expectation ($f $c)) (+ (* $c (- $f 0.5)) 0.5))



(: |- (-> Expression Expression Expression))
(arity |- 3) ;; FP


;;NAL-1
;;!Syllogistic rules for Inheritance:
(= (|- (($a --> $b) $T1) (($b --> $c) $T2)) (($a --> $c) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($a --> $b) $T1) (($a --> $c) $T2)) (($c --> $b) (Truth_Induction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($a --> $c) $T1) (($b --> $c) $T2)) (($b --> $a) (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($a --> $b) $T1) (($b --> $c) $T2)) (($c --> $a) (Truth_Exemplification $T1 $T2)))
;;NAL-2
;;!Rules for Similarity:
(arity |- 999) ;; FP
(= (|- (($S <-> $P) $T)) (($P <-> $S) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- (($M <-> $P) $T1) (($S <-> $M) $T2)) (($S <-> $P) (Truth_Resemblance $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) (($S <-> $P) (Truth_Comparison $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($S <-> $P) (Truth_Comparison $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $P) $T1) (($S <-> $M) $T2)) (($S --> $P) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($P --> $M) $T1) (($S <-> $M) $T2)) (($P --> $S) (Truth_Analogy $T1 $T2)))
;;!Dealing with properties and instances:
(arity |- 999) ;; FP
(= (|- (($S --> ({ $P })) $T)) (($S <-> ({ $P })) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- ((([ $S ]) --> $P) $T)) ((([ $S ]) <-> $P) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- ((({ $M }) --> $P) $T1) (($S <-> $M) $T2)) ((({ $S }) --> $P) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($P --> ([ $M ])) $T1) (($S <-> $M) $T2)) (($P --> ([ $S ])) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((({ $A }) <-> ({ $B }))) ($A <-> $B) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- ((([ $A ]) <-> ([ $B ]))) ($A <-> $B) (Truth_StructuralIntersection $T)))
;;NAL-3
;;!Set decomposition:
(arity |- 999) ;; FP
(= (|- ((({ $A $B }) --> $M) $T)) ((({ $A }) --> $M) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- ((({ $A $B }) --> $M) $T)) ((({ $B }) --> $M) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- ((M --> ([ $A $B ])) $T)) ((M --> ([ $A ])) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- ((M --> ([ $A $B ])) $T)) ((M --> ([ $B ])) (Truth_StructuralDeduction $T)))
;;!Extensional and intensional intersection decomposition:
(arity |- 999) ;; FP
(= (|- ((($S | $P) --> $M) $T)) (($S --> $M) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- (($M --> ($S & $P)) $T)) (($M --> $S) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- ((($S | $P) --> $M) $T)) (($P --> $M) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- (($M --> ($S & $P)) $T)) (($M --> $P) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- ((($A ~ $S) --> $M) $T)) (($A --> $M) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- (($M --> ($B - $S)) $T)) (($M --> $B) (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- ((($A ~ $S) --> $M) $T)) (($S --> $M) (Truth_StructuralDeductionNegated $T)))
(arity |- 999) ;; FP
(= (|- (($M --> ($B - $S)) $T)) (($M --> $S) (Truth_StructuralDeductionNegated $T)))
;;!Extensional and intensional intersection composition: (sets via reductions)
;(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) ((($P | $S) --> $M) (Truth_Intersection $T1 $T2)))
;(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) ((($P & $S) --> $M) (Truth_Union $T1 $T2)))
;(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) ((($P ~ $S) --> $M) (Truth_Difference $T1 $T2)))
;(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($M --> ($P & $S)) (Truth_Intersection $T1 $T2)))
;(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($M --> ($P | $S)) (Truth_Union $T1 $T2)))
;(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($M --> ($P - $S)) (Truth_Difference $T1 $T2)))
;;!Extensional and intensional intersection decomposition:
(arity |- 999) ;; FP
(= (|- (($S --> $M) $T1) ((($S | $P) --> $M) $T2)) (($P --> $M) (Truth_DecomposePNN $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($P --> $M) $T1) ((($S | $P) --> $M) $T2)) (($S --> $M) (Truth_DecomposePNN $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($S --> $M) $T1) ((($S & $P) --> $M) $T2)) (($P --> $M) (Truth_DecomposeNPP $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($P --> $M) $T1) ((($S & $P) --> $M) $T2)) (($S --> $M) (Truth_DecomposeNPP $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($S --> $M) $T1) ((($S ~ $P) --> $M) $T2)) (($P --> $M) (Truth_DecomposePNP $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($S --> $M) $T1) ((($P ~ $S) --> $M) $T2)) (($P --> $M) (Truth_DecomposeNNN $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $S) $T1) (($M --> ($S & $P)) $T2)) (($M --> $P) (Truth_DecomposePNN $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $P) $T1) (($M --> ($S & $P)) $T2)) (($M --> $S) (Truth_DecomposePNN $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $S) $T1) (($M --> ($S | $P)) $T2)) (($M --> $P) (Truth_DecomposeNPP $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $P) $T1) (($M --> ($S | $P)) $T2)) (($M --> $S) (Truth_DecomposeNPP $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $S) $T1) (($M --> ($S - $P)) $T2)) (($M --> $P) (Truth_DecomposePNP $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M --> $S) $T1) (($M --> ($P - $S)) $T2)) (($M --> $P) (Truth_DecomposeNNN $T1 $T2)))
;; NAL-4
;;!Transformation rules between product and image:
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T)) (($A --> ($R /1 $B)) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T)) (($B --> ($R /2 $A)) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T)) ((($R \1 $B) --> $A) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T)) ((($R \2 $A) --> $B) (Truth_StructuralIntersection $T)))
;;other direction of same rules (as these are bi-directional)
;(= (|- (($A --> ($R /1 $B)) $T)) ((($A * $B) --> $R) (Truth_StructuralIntersection $T)))
;(= (|- (($B --> ($R /2 $A)) $T)) ((($A * $B) --> $R) (Truth_StructuralIntersection $T)))
;(= (|- ((($R \1 $B) --> $A) $T)) (($R --> ($A * $B)) (Truth_StructuralIntersection $T)))
;(= (|- ((($R \2 $A) --> $B) $T)) (($R --> ($A * $B)) (Truth_StructuralIntersection $T)))
;;!Comparative relations
;(= (|- ((({ $R }) |-> ([ $P ])) $T1) ((({ $S }) |-> ([ $P ])) $T2)) (((({ $R }) * ({ $S })) --> (>>> $P )) (Truth_FrequencyGreater $T1 $T2)))
;(= (|- ((($A * $B) --> (>>> $P)) $T1) ((($B * $C) --> (>>> $P)) $T2)) ((($A * $C) --> (>>> $P)) (Truth_Deduction $T1 $T2)))
;(= (|- ((({ $R }) |-> ([ $P ])) $T1) ((({ $S }) |-> ([ $P ])) $T2)) (((({ $R }) * ({ $S })) --> (=== $P)) (Truth_FrequencyEqual $T1 $T2)))
;(= (|- ((($A * $B) --> (=== $P)) $T1) ((($B * $C) --> (=== $P)) $T2)) ((($A * $C) --> (=== $P)) (Truth_Deduction $T1 $T2)))
;(= (|- ((($A * $B) --> (=== $P)) $T)) ((($B * $A) --> (=== $P)) (Truth_StructuralIntersection $T)))
;;!Optional rules for more efficient reasoning about relation components:
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) ((($C * $B) --> $R) $T2)) (($C --> $A) (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) ((($A * $C) --> $R) $T2)) (($C --> $B) (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($R --> ($C * $B)) $T2)) (($C --> $A) (Truth_Induction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($R --> ($A * $C)) $T2)) (($C --> $B) (Truth_Induction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) (($C --> $A) $T2)) ((($C * $B) --> $R) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) (($A --> $C) $T2)) ((($C * $B) --> $R) (Truth_Induction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) (($C <-> $A) $T2)) ((($C * $B) --> $R) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) (($C --> $B) $T2)) ((($A * $C) --> $R) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) (($B --> $C) $T2)) ((($A * $C) --> $R) (Truth_Induction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) (($C <-> $B) $T2)) ((($A * $C) --> $R) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($A --> $C) $T2)) (($R --> ($C * $B)) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($C --> $A) $T2)) (($R --> ($C * $B)) (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($C <-> $A) $T2)) (($R --> ($C * $B)) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($B --> $C) $T2)) (($R --> ($A * $C)) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($C --> $B) $T2)) (($R --> ($A * $C)) (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($C <-> $B) $T2)) (($R --> ($A * $C)) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) ((($C * $B) --> $R) $T2)) (($A <-> $C) (Truth_Comparison $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($A * $B) --> $R) $T1) ((($A * $C) --> $R) $T2)) (($B <-> $C) (Truth_Comparison $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($R --> ($C * $B)) $T2)) (($A <-> $C) (Truth_Comparison $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($R --> ($A * $B)) $T1) (($R --> ($A * $C)) $T2)) (($B <-> $C) (Truth_Comparison $T1 $T2)))
;;NAL-5
;;!Negation conjunction and disjunction decomposition:
(arity |- 999) ;; FP
(= (|- ((! $A) $T)) ($A (Truth_Negation $T)))
(arity |- 999) ;; FP
(= (|- (($A && $B) $T)) ($A (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- (($A && $B) $T)) ($B (Truth_StructuralDeduction $T)))
(arity |- 999) ;; FP
(= (|- (($A && $B) $T)) (($B && $A) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- ($S $T1) (($S && $A) $T2)) ($A (Truth_DecomposePNN $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ($S $T1) (($S || $A) $T2)) ($A (Truth_DecomposeNPP $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ($S $T1) (((! $S) && $A) $T2)) ($A (Truth_DecomposeNNN $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ($S $T1) (((! $S) || $A) $T2)) ($A (Truth_DecomposePPP $T1 $T2)))
;;!Syllogistic rules for Implication:
(arity |- 999) ;; FP
(= (|- (($A ==> $B) $T1) (($B ==> $C) $T2)) (($A ==> $C) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($A ==> $B) $T1) (($A ==> $C) $T2)) (($C ==> $B) (Truth_Induction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($A ==> $C) $T1) (($B ==> $C) $T2)) (($B ==> $A) (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($A ==> $B) $T1) (($B ==> $C) $T2)) (($C ==> $A) (Truth_Exemplification $T1 $T2)))
;;!Conditional composition for conjunction and disjunction:
(arity |- 999) ;; FP
(= (|- (($A ==> $C) $T1) (($B ==> $C) $T2)) ((($A && $B) ==> $C) (Truth_Union $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($A ==> $C) $T1) (($B ==> $C) $T2)) ((($A || $B) ==> $C) (Truth_Intersection $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($C ==> $A) $T1) (($C ==> $B) $T2)) (($C ==> ($A && $B)) (Truth_Intersection $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($C ==> $A) $T1) (($C ==> $B) $T2)) (($C ==> ($A || $B)) (Truth_Union $T1 $T2)))
;;!Multi-conditional inference:
(arity |- 999) ;; FP
(= (|- ((($S && $P) ==> $M) $T1) (($S ==> $M) $T2)) ($P (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($C && $M) ==> $P) $T1) (($S ==> $M) $T2)) ((($C && $S) ==> $P) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($C && $P) ==> $M) $T1) ((($C && $S) ==> $M) $T2)) (($S ==> $P) (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ((($C && $M) ==> $P) $T1) (($M ==> $S) $T2)) ((($C && $S) ==> $P) (Truth_Induction $T1 $T2)))
;;!Rules for equivalence:
(arity |- 999) ;; FP
(= (|- (($S <=> $P) $T)) (($P <=> $S) (Truth_StructuralIntersection $T)))
(arity |- 999) ;; FP
(= (|- (($S ==> $P) $T1) (($P ==> $S) $T2)) (($S <=> $P) (Truth_Intersection $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($P ==> $M) $T1) (($S ==> $M) $T2)) (($S <=> $P) (Truth_Comparison $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M ==> $P) $T1) (($M ==> $S) $T2)) (($S <=> $P) (Truth_Comparison $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M ==> $P) $T1) (($S <=> $M) $T2)) (($S ==> $P) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($P ==> $M) $T1) (($S <=> $M) $T2)) (($P ==> $S) (Truth_Analogy $T1 $T2)))
(arity |- 999) ;; FP
(= (|- (($M <=> $P) $T1) (($S <=> $M) $T2)) (($S <=> $P) (Truth_Resemblance $T1 $T2)))
;;!Higher-order decomposition
(arity |- 999) ;; FP
(= (|- ($A $T1) (($A ==> $B) $T2)) ($B (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ($A $T1) ((($A && $B) ==> $C) $T2)) (($B ==> $C) (Truth_Deduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ($B $T1) (($A ==> $B) $T2)) ($A (Truth_Abduction $T1 $T2)))
(arity |- 999) ;; FP
(= (|- ($A $T1) (($A <=> $B) $T2)) ($B (Truth_Analogy $T1 $T2)))
;;NAL term reductions
;;!Extensional intersection, union, conjunction reductions:

(: ($A & $A) Atom))
(: ($A | $A) Atom))
(: ($A && $A) Atom))
(: ($A || $A) Atom))

(= ($A & $A) $A)
(= ($A | $A) $A)
(= ($A && $A) $A)
(= ($A || $A) $A)
;;!Extensional set reductions:
(= (({ $A }) | ({ $B })) ({ $A $B }))
(= (({ $A $B }) | ({ $C })) ({ ($A . $B) $C }))
(= (({ $C }) | ({ $A $B }) ) ({ $C ($A . $B) }))
;;!Intensional set reductions:
(= (([ $A ]) & ([ $B ])) ([ $A $B ]) )
(= (([ $A $B ]) & ([ $C ])) ([ ($A . $B) $C ]))
(= (([ $A ]) & ([ $B $C ])) ([ $A ($B . $C) ]))
;;!Reduction for set element copula:
(= ({ ( $A . $B ) }) ({ $A $B }))
(= ([ ( $A . $B ) ]) ([ $A $B ]))
(= (|- ($T $T1) ($T $T2)) ($T (Truth_Revision $T1 $T2)))

;Whether evidence was just counted once
(arity StampDisjoint 2) ;; FP
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True overlap)))))))

;Exhaustive-until-depth deriver
(arity Derive 4) ;; FP
(= (Derive $beliefs $depth $maxdepth)
   (If (> $depth $maxdepth)
       $beliefs
       (let $derivations 
            (collapse (let* (((Sentence $x $Ev1) (superpose $beliefs))
                             ((Sentence $y $Ev2) (superpose $beliefs))
                             ($stamp (TupleConcat $Ev1 $Ev2)))
                            (If (StampDisjoint $Ev1 $Ev2)
                                (case (|- $x $y) ((($T $TV) (Sentence ($T $TV) $stamp)))))))
            (Derive (TupleConcat $beliefs $derivations) (+ $depth 1) $maxdepth))))

;retrieve the best candidat
(arity BestCandidate 4) ;; FP
(= (BestCandidate $evaluateCandidateFunction $bestCandidate $tuple)
  (If (== $tuple ())
      $bestCandidate
      (let* (($head (car-atom $tuple))
             ($tail (cdr-atom $tuple)))
            (If (> ($evaluateCandidateFunction $head)
                   ($evaluateCandidateFunction $bestCandidate))
                (BestCandidate $evaluateCandidateFunction $head $tail)
                (BestCandidate $evaluateCandidateFunction $bestCandidate $tail)))))

;candidate evaluation based on confidence
(arity ConfidenceRank 2) ;; FP
(= (ConfidenceRank (($f $c) $Ev)) $c)
(= (ConfidenceRank Nil) 0)

;pose a question of a certain term to the system on some knowledge base
(arity Question 4) ;; FP
(= (Question $kb $term $steps)
   (BestCandidate ConfidenceRank Nil (collapse (let $x (Derive $kb 1 $steps) 
                                                    (case (superpose $x)
                                                          (((Sentence ($T $TV) $Ev) (case (== $T $term)
                                                                                          ((True ($TV $Ev)))))))))))
;; How much evidence is there that garfield is an artist given the following input using up to 3 inference steps? 
!(Question ((Sentence ((({ garfield }) --> cat) (1.0 0.9)) (1)) 
            (Sentence (((cat * sky) --> like) (1.0 0.9)) (2)) 
            (Sentence ((sky --> ([ blue ])) (1.0 0.9)) (3))
            (Sentence ((((({ garfield }) * ([ blue ])) --> like) ==> (({ garfield }) --> artist)) (1.0 0.9)) (4)))
           (({ garfield }) --> artist) 3)
