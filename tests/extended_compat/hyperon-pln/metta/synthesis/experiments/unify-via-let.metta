;; Simple unification
!(let (Link A $y) (Link $x B) (Link $x $y))  ; [(Link A B)]

;; Simple unification with the arrow operator
!(let (-> A $y) (-> $x B) (-> $x $y))  ; [(-> A B)]

;; When using `let`, the second argument gets evaluated before binding, so
;; unifying a term with the result of a function (right-handside of `let`) works
(= (f $x) (Link $x B))
!(let (Link A $y) (f $x) (Link $x $y))  ; [(Link A B)]

;; But unifying a left-handside function term shouldn't work
(= (g $y) (Link A $y))
!(let (g $y) (Link $x B) (Link $x $y))  ; []

;; We would need to force evaluation of the first term `(g $y) for this
;; unification to work. For example by including it in its own binding
;; on the right hand side:
!(let* (($gy (g $y))
        ($gy (Link $x B)))
       (Link $x $y))  ; [(Link A B)]

;; Simple multi-unification via superpose
!(let (Link A $y) (superpose ((Link $x B) (Node A) (Link $x C))) (Link $x $y))  ; [(Link A B), (Link A C)]

;; Unify results of functions (via an intermediate variable, $z)
!(let* (($z (g $y)) ($z (f $x))) $z)  ; [(Link A B)]
