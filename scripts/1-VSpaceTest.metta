
!(extend-py! metta_learner)

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






(= (is $x $x) True)

; LOGICAL Recursive Factorial
(= (factorial-pred 0 1) True)
(= (factorial-pred $N $R-X)
   (and (> $N 0)
      (is $F2 (- $N 1))
      (factorial-pred $F2 $R)
      (is $R-X (* $N $R))))

;;!(assertTrue (factorial-pred 0 1))
;;!(assertTrue (factorial-pred 1 1))
;;!(assertTrue (factorial-pred 2 2))
;;!(assertTrue (factorial-pred 3 6))
;;!(assertTrue (factorial-pred 4 24))
;;!(assertTrue (factorial-pred 5 120))

; LOGICAL Factorial Using Accumulators
(= (factorial-pred-acc $F $A) (factorial-pred-acc-helper $F 1 $A))
(= (factorial-pred-acc-helper 0 $A $A) True)
(= (factorial-pred-acc-helper $N $F $A)
   (and (> $N 0)
      (is $X (* $F $N))
      (is $F1 (- $N 1))
      (factorial-pred-acc-helper $F1 $X $A)))

;;!(assertTrue (factorial-pred-acc 0 1))
;;!(assertTrue (factorial-pred-acc 1 1))
;;!(assertTrue (factorial-pred-acc 2 2))
;;!(assertTrue (factorial-pred-acc 3 6))
;;!(assertTrue (factorial-pred-acc 4 24))
;;!(assertTrue (factorial-pred-acc 5 120))

; LOGICAL Tail-Recursive Factorial
(= (factorial-pred-tail-rec $F $R-H) (factorial-pred-tail-rec-helper $F 1 $R-H))
(= (factorial-pred-tail-rec-helper 0 $H $H) True)
(= (factorial-pred-tail-rec-helper $N1 $F $H)
   (and (> $N1 0)
      (is $X (* $F $N1))
      (is $F2 (- $N1 1))
      (factorial-pred-tail-rec-helper $F2 $X $H)))

;;!(assertTrue (factorial-pred-tail-rec 0 1))
;;!(assertTrue (factorial-pred-tail-rec 1 1))
;;!(assertTrue (factorial-pred-tail-rec 2 2))
;;!(assertTrue (factorial-pred-tail-rec 3 6))
;;!(assertTrue (factorial-pred-tail-rec 4 24))
;;!(assertTrue (factorial-pred-tail-rec 5 120))



; FUNCTIONAL Recursive Factorial
(= (factorial 0) 1)
(= (factorial $n) (* $n (factorial (- $n 1))))

;;!(assertEqual (factorial 0) 1)
;;!(assertEqual (factorial 1) 1)
;;!(assertEqual (factorial 2) 2)
;;!(assertEqual (factorial 4) 24)
;;!(assertEqual (factorial 5) 120)


; FUNCTIONAL Tail-Recursive Factorial
(= (factorial-tail-rec $n) (factorial-tail-rec-helper $n 1))
(= (factorial-tail-rec-helper 0 $acc) $acc)
(= (factorial-tail-rec-helper $n $acc) (factorial-tail-rec-helper (- $n 1) (* $n $acc)))

;;!(assertEqual (factorial-tail-rec 0) 1)
;;!(assertEqual (factorial-tail-rec 1) 1)
;;!(assertEqual (factorial-tail-rec 2) 2)
;;!(assertEqual (factorial-tail-rec 3) 6)
;;!(assertEqual (factorial-tail-rec 4) 24)
;;!(assertEqual (factorial-tail-rec 5) 120)

; FUNCTIONAL Factorial Using Accumulators
(= (factorial-acc $n) (factorial-acc-helper $n 1))
(= (factorial-acc-helper 0 $acc) $acc)
(= (factorial-acc-helper $n $acc) (factorial-acc-helper (- $n 1) (* $n $acc)))

;;!(assertEqual (factorial-acc 0) 1)
;;!(assertEqual (factorial-acc 1) 1)
;;!(assertEqual (factorial-acc 2) 2)
;;!(assertEqual (factorial-acc 3) 6)
;;!(assertEqual (factorial-acc 4) 24)
;;!(assertEqual (factorial-acc 5) 120)





!(metta_learner::vspace-main)

