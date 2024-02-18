; !(import! &self examples/RUN_tests1.metta)

!(extend-py! mettalog)

;A "better" if which works for both if-then and if-then-else:
(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) ())
(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

;Concats tuples:
(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))

;Sequential by exploiting order preservation of current superpose implementation:
(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

;Do not return result of expression
(: do (-> Expression %Undefined%))
(= (do $1) (case $1 ()))







; functional recursive factorial
(= (factorial 0) 1)
(= (factorial $n) (* $n (factorial (- $n 1))))

;;!(assertEqual (factorial 0) 1)
;;!(assertEqual (factorial 1) 1)
;;!(assertEqual (factorial 2) 2)
;;!(assertEqual (factorial 4) 24)
;;!(assertEqual (factorial 5) 120)


; functional tail-recursive factorial
(= (factorial-tail-rec $n) (factorial-tail-rec-helper $n 1))
(= (factorial-tail-rec-helper 0 $acc) $acc)
(= (factorial-tail-rec-helper $n $acc) (factorial-tail-rec-helper (- $n 1) (* $n $acc)))

;;!(assertEqual (factorial-tail-rec 0) 1)
;;!(assertEqual (factorial-tail-rec 1) 1)
;;!(assertEqual (factorial-tail-rec 2) 2)
;;!(assertEqual (factorial-tail-rec 3) 6)
;;!(assertEqual (factorial-tail-rec 4) 24)
;;!(assertEqual (factorial-tail-rec 5) 120)

; functional factorial using accumulators
(= (factorial-acc $n) (factorial-acc-helper $n 1))
(= (factorial-acc-helper 0 $acc) $acc)
(= (factorial-acc-helper $n $acc) (factorial-acc-helper (- $n 1) (* $n $acc)))

;;!(assertEqual (factorial-acc 0) 1)
;;!(assertEqual (factorial-acc 1) 1)
;;!(assertEqual (factorial-acc 2) 2)
;;!(assertEqual (factorial-acc 3) 6)
;;!(assertEqual (factorial-acc 4) 24)
;;!(assertEqual (factorial-acc 5) 120)

(= (is $x $x) true)

; logical recursive factorial
(= (factorial-pred 0 1) true)
(= (factorial-pred $n $out)
   (and (> $n 0)
      (is $f2 (- $n 1))
      (factorial-pred $f2 $r)
      (is $out (* $n $r))))

;;!(assertTrue (factorial-pred 0 1))
;;!(assertTrue (factorial-pred 1 1))
;;!(assertTrue (factorial-pred 2 2))
;;!(assertTrue (factorial-pred 3 6))
;;!(assertTrue (factorial-pred 4 24))
;;!(assertTrue (factorial-pred 5 120))

; logical factorial using accumulators
(= (factorial-pred-acc $f $accout) (factorial-pred-acc-helper $f 1 $accout))
(= (factorial-pred-acc-helper 0 $accout $accout) true)
(= (factorial-pred-acc-helper $n $f $accout)
   (and (> $n 0)
      (is $x (* $f $n))
      (is $f1 (- $n 1))
      (factorial-pred-acc-helper $f1 $x $accout)))

;;!(assertTrue (factorial-pred-acc 0 1))
;;!(assertTrue (factorial-pred-acc 1 1))
;;!(assertTrue (factorial-pred-acc 2 2))
;;!(assertTrue (factorial-pred-acc 3 6))
;;!(assertTrue (factorial-pred-acc 4 24))
;;!(assertTrue (factorial-pred-acc 5 120))

; logical tail-recursive factorial
(= (factorial-pred-tail-rec $f $rout) (factorial-pred-tail-rec-helper $f 1 $rout))
(= (factorial-pred-tail-rec-helper 0 $h $h) true)
(= (factorial-pred-tail-rec-helper $n1 $f $rout)
   (and (> $n1 0)
      (is $x (* $f $n1))
      (is $f2 (- $n1 1))
      (factorial-pred-tail-rec-helper $f2 $x $rout)))

;;!(assertTrue (factorial-pred-tail-rec 0 1))
;;!(assertTrue (factorial-pred-tail-rec 1 1))
;;!(assertTrue (factorial-pred-tail-rec 2 2))
;;!(assertTrue (factorial-pred-tail-rec 3 6))
;;!(assertTrue (factorial-pred-tail-rec 4 24))
;;!(assertTrue (factorial-pred-tail-rec 5 120))




!(mettalog::vspace-main)


