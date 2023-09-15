
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

!(metta_learner::vspace-main)

