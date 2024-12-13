
(= (same $X $X) True)
(= (eq $X $Y)
	(let $C (same $X $Y) (if (== $C True) True False)))

(= (unif $X $Y)
	(unify $X $Y True False))




!(assertEqualToResult (unif (+ 1 1) ($_1 $_2 $_)) (False)) ;; eager eval?
!(assertEqualToResult (unif (+ 1 1) 2) (True)) ;; eager eval on plus?
!(assertEqualToResult (unif (+ 1 1) (+ 1 1)) (True))
!(assertEqualToResult (unif (+ 1 1) (+ 0 2)) (True)) ;; eager eval?
!(assertEqualToResult (unif (+ 1 1) (+ $a $b)) (False)) ;; eager eval?
!(assertEqualToResult (unif $x $x) (True))
!(assertEqualToResult (unif $x $y) (True)) ;; identity?
!(assertEqualToResult (unif (+ 1 1) $x) (True)) ;; eager eval?
!(assertEqualToResult (unif (+ 1 2 3) (+ 1 2 3)) (True)) ;; Arity error?
!(assertEqualToResult (unif (+ 1 2 3) (+ 1 $two 3)) (True))
!(assertEqualToResult (unif (+ 1 2) (+ 1 $two)) (False)) ;; overly eager eval?

!(assertEqualToResult (same (+ 1 1) ($_1 $_2 $_)) ((same 2 ($_1 $_2 $_))))
!(assertEqualToResult (same (+ 1 1) 2) (True))
!(assertEqualToResult (same (+ 1 1) (+ 1 1)) (True))
!(assertEqualToResult (same (+ 1 1) (+ 0 2)) (True))
!(assertEqualToResult (same (+ 1 2 3) (+ 1 2 3)) (True))
!(assertEqualToResult (same (+ 1 1) (+ $a $b)) ((same 2 (+ $a $b))))
!(assertEqualToResult (same $x $y) (True))
!(assertEqualToResult (same $x $x) (True))
!(assertEqualToResult (same (+ 1 1) $x) (True))
!(assertEqualToResult (same (+ 1 2 3) (+ 1 2 3)) (True))
!(assertEqualToResult (same (+ 1 2 3) (+ 1 $two 3)) (True))
!(assertEqualToResult (same (+ 1 2) (+ 1 $two)) ((same 3 (+ 1 $two))))

!(assertEqualToResult (eq (+ 1 1) ($_1 $_2 $_)) (False))
!(assertEqualToResult (eq (+ 1 1) 2) (True))
!(assertEqualToResult (eq (+ 1 1) (+ 1 1)) (True))
!(assertEqualToResult (eq (+ 1 1) (+ 0 2)) (True))
!(assertEqualToResult (eq (+ 1 2 3) (+ 1 2 3)) (True))
!(assertEqualToResult (eq (+ 1 1) (+ $a $b)) (False))
!(assertEqualToResult (eq $x $y) (True))
!(assertEqualToResult (eq $x $x) (True))
!(assertEqualToResult (eq (+ 1 1) $x) (True))
!(assertEqualToResult (eq (+ 1 2 3) (+ 1 2 3)) (True))
!(assertEqualToResult (eq (+ 1 2 3) (+ 1 $two 3)) (True))
!(assertEqualToResult (eq (+ 1 2) (+ 1 $two)) (False))





(unify-atom (-> Atom Atom))
(= (unify-atom $X $Y)
    (unify $X $Y (quote (u $X $Y)) False))

!(assertEqualToResult (unify-atom (+ 1 1) ($_1 $_2 $_)) (False)) ;; eager eval?
!(assertEqualToResult (unify-atom (+ 1 1) 2) ((quote (u 2 2)))) ;; eager eval?
!(assertEqualToResult (unify-atom (+ 1 1) (+ 1 1)) ((quote (u 2 2))))
!(assertEqualToResult (unify-atom (+ 1 1) (+ 0 2)) ((quote (u 2 2)))) ;; eager eval?
!(assertEqualToResult (unify-atom (+ 1 1) (+ $a $b)) (False)) ;; eager eval?
!(assertEqualToResult (unify-atom $x $x) ((quote (u $x $x))))
!(assertEqualToResult (unify-atom $x $y) ((quote (u $y $y)))) ;; identity?
!(assertEqualToResult (unify-atom (+ 1 1) $x) ((quote (u 2 2)))) ;; eager eval?
!(assertEqualToResult (unify-atom (+ 1 2 3) (+ 1 2 3)) ((quote (u (Error (+ 1 2 3) IncorrectNumberOfArguments) (Error (+ 1 2 3) IncorrectNumberOfArguments))))) ;; Arity error?
!(assertEqualToResult (unify-atom (+ 1 2 3) (+ 1 $two 3)) ((quote (u (Error (+ 1 2 3) IncorrectNumberOfArguments) (Error (+ 1 2 3) IncorrectNumberOfArguments)))))
!(assertEqualToResult (unify-atom (+ 1 2) (+ 1 $two)) (False)) ;; overly eager eval?

