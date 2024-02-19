;; Simple unification
!(case (Link $x B) (((Link A $y) (Link $x $y))))  ; [(Link A B)]

;; Simple unification with the arrow operator
!(case (-> $x B) (((-> A $y) (-> $x $y))))  ; [(-> A B)]

;; Unify a term with the result of function (right-handside)
(= (f $x) (Link $x B))
!(case (f $x) (((Link A $y) (Link $x $y))))  ; [(Link A B)]

;; Unify a term with the result of function (left-handside, not
;; supposed to work)
(= (g $y) (Link A $y))
!(case (Link $x B) (((g $y) (Link $x $y))))  ; []

;; Simple multi-unification via superpose
!(case (superpose ((Link $x B) (Node A) (Link $x C))) (((Link A $y) (Link $x $y))))  ; [(Link A B), (Link A C)]

;; Unify results of functions (via an intermediate variable, $z)
!(case (g $y) (($z (case (f $x) (($z $z))))))  ; [(Link A B)]
