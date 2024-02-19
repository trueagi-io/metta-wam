
;; Define foo
(: foo (-> Atom Variable Atom))
(= (foo Z $var) $var)
(= (foo (R Z) $var) (R (foo Z $var)))

;; Test foo
!(foo Z $a) ;; returns [$a] that's expected result, but
!(foo (R Z) $a) ;; returns [(R $X#19)] instead of [(R $a)]
