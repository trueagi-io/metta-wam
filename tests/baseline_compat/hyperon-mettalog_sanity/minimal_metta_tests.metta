;; remove Empty result

(= (returns-empty) Empty)
(= (returns-empty) 1)
(= (chain-to-empty) (returns-empty))

!(assertEqualToResult (returns-empty) (1))
!(assertEqualToResult (eval (returns-empty)) (1))
!(assertEqualToResult (chain-to-empty) (1))
!(assertEqualToResult (eval (chain-to-empty)) (1))
!(assertEqualToResult (chain (eval (returns-empty)) $x (quote $x)) ((quote 1) (quote Empty)))
!(assertEqualToResult (chain (eval (chain-to-empty)) $x (quote $x)) ((quote (returns-empty))))

;; include empty expression ()

(= (returns-empty-expression) ())
(= (returns-empty-expression) 1)
(= (chain-to-empty-expression) (returns-empty-expression))

!(assertEqualToResult (returns-empty-expression) (1 ()))
!(assertEqualToResult (eval (returns-empty-expression)) (1 ()))
!(assertEqualToResult (chain-to-empty-expression) (1 ()))
!(assertEqualToResult (eval (chain-to-empty-expression)) (1 ()))
!(assertEqualToResult (chain (eval (returns-empty-expression)) $x (quote $x)) ((quote 1) (quote ())))
!(assertEqualToResult (chain (eval (chain-to-empty-expression)) $x (quote $x)) ((quote (returns-empty-expression))))

;; return original form for NotReducible

(= (returns-not-reducible) NotReducible)
(= (returns-not-reducible)  1)
(= (chain-to-not-reducible) (returns-not-reducible))

!(assertEqualToResult (returns-not-reducible)  (1 (returns-not-reducible)))
!(assertEqualToResult (chain-to-not-reducible)  (1 (returns-not-reducible)))
!(assertEqualToResult (eval (returns-not-reducible))  (1 (eval (returns-not-reducible))))
!(assertEqualToResult (eval (chain-to-not-reducible))  (1 (returns-not-reducible)))
!(assertEqualToResult (chain (eval (returns-not-reducible)) $x (quote $x)) ((quote 1) (quote NotReducible)))
!(assertEqualToResult (chain (eval (chain-to-not-reducible)) $x (quote $x)) ((quote (returns-not-reducible))))

;; collapse-bind operates on Minimal MeTTa

(= (bar A) (input A))
(= (bar B) (input B))
(= (foo $x) (bar $x))

!(assertEqualToResult (chain (collapse-bind (foo $x)) $a (quote $a)) ((quote (((foo $x) {  })))))
!(assertEqualToResult (chain (collapse-bind (eval (foo $x))) $a (quote $a)) ((quote (((bar $x) {  })))))
!(assertEqualToResult (chain (collapse-bind (metta (foo $x) %Undefined% &self)) $a (quote $a)) ((quote (((input B) { $x <- B }) ((input A) { $x <- A })))))

;; superpose-bind complements collapse-bind

!(assertEqualToResult (chain (collapse-bind (foo $x)) $a 
                            (chain (superpose-bind $a) $b (quote $b))) 
                    ((quote (foo $x))))
!(assertEqualToResult (chain (collapse-bind (eval (foo $x))) $a 
                            (chain (superpose-bind $a) $b (quote $b))) 
                    ((quote (bar $x))))
!(assertEqualToResult (chain (collapse-bind (metta (foo $x) %Undefined% &self)) $a 
                            (chain (superpose-bind $a) $b (quote $b))) 
                    ((quote (input B)) (quote (input A))))

;; metta handles type

(= (foobar) 1)
(= (foobar) "a")
(= (chain-to-foobar) (foobar))

!(assertEqualToResult (metta (foobar) Number &self) (1))
!(assertEqualToResult (metta (foobar) String &self) ("a"))
!(assertEqualToResult (collapse-bind (metta (foobar) $type &self)) ((1 { $type <- Number }) ("a" { $type <- String })))
!(assertEqualToResult (metta (chain-to-foobar) Number &self) (1))
!(assertEqualToResult (metta (chain-to-foobar) String &self) ("a"))
!(assertEqualToResult (collapse-bind (metta (chain-to-foobar) $type &self)) ((1 { $type <- Number }) ("a" { $type <- String })))


;; Minimal MeTTa interpretation & evaluation step
(= (n/0/0) (n/1/0))
(= (n/0/0) (n/1/1))
(= (n/1/0) (n/2/0))
        
!(assertEqual  ;; don't reduce non-Minimal Metta instruction
    (chain (n/0/0) $x (quote $x)) 
    (quote (n/0/0))) 
!(assertEqualToResult  ;; eval makes exactly one step
    (chain (eval (n/0/0)) $x (quote $x))  
    ((quote (n/1/0)) (quote (n/1/1))))
!(assertEqualToResult  ;; `metta` fully reduces
    (chain (metta (n/0/0) %Undefined% &self) $x (quote $x))
    ((quote (n/1/1)) (quote (n/2/0))))
    
!(assertEqual ;; unify is a Minimal-MeTTa construct, not a space-editing API as it is currently treated in MettaLog
    (chain (unify ($x b) (a $y) ($x $y) Failed) $v $v)
	(a b))

!(assertEqual ;; nested chain
	(chain (chain A $x (p $x)) $y (quote $y))
	(quote (p A)))
!(assertEqual ;; cons-atom is minimal metta, got evaluated
	(chain (cons-atom p (q)) $v (quote $v))
	(quote (p q)))
!(assertEqual ;; decons-atom is minimal metta, got evalauted
	(chain (decons-atom (p q)) $x (quote $x))
	(quote (p (q))))

;; function / return, mutate argument via call struct
(= (f1) (function (return ())))
(= (f0 $b) (function
	(chain (eval (f1)) $_
	(unify $b value
		(return ())
		(return (Error () "Unexpected error")) ))))
!(assertEqual 
	(chain (eval (f0 $a)) $_ $a)
	value)