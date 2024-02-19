;; Define composition operator
(: . (-> (-> $b $c) (-> $a $b) (-> $a $c)))
;; Below, the second $b is replaced by $d to disable type constraint
;; on composition
;; (: . (-> (-> $b $c) (-> $a $d) (-> $a $c)))
(= ((. $g $f) $x) ($g ($f $x)))

;; Define well-typed.  Returns True iff its argument is well typed.
(: well-typed (-> Atom Bool))
(= (well-typed $x) (case $x
                     (((Error $v $m) False)
                      ($_ True))))

;; Define functions to compose
(: f (-> Number String))
(= (f $x) (if (== $x 42) "42" "Not-42"))
(: g (-> String Bool))
(= (g $x) (== $x "42"))
(: h (-> Bool Number))
(= (h True) 0)
(= (h False) 1)

;; ;; ;; Test composition via superpose
!(. (superpose (f g)) (superpose (f g)))
; !(collapse (. (superpose (f g)) (superpose (f g))))
!(well-typed (. (superpose (f g)) (superpose (f g))))
; !(get-type (. (superpose (f g)) (superpose (f g))))


;; Defining functions to compose using non-determinism
;; (: ndfs (-> Atom))
(= (fns) f)
(= (fns) g)
(= (fns) h)
; !(get-type (. (fns) (fns)))

;!((. (fns) (fns)) 42)

!(g "42")
!((. g f) 42)

!(superpose ((. f f) (. f g) (. g f) (. g g) (. h g)))


