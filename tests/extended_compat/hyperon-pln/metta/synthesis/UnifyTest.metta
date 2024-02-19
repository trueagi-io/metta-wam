;; Import module
!(import! &self Unify.metta)

;; Test unify.
;; Should output (Link A B)
!(unify (Link $x B) (Link A $y) (Link $x $y))

;; Test unify*.
;; Unify `Link $x B $z` with `Link A $y $z`, and `$z` with `C`
;; Should output (Link A B C)
!(unify* (((Link $x B $z) (Link A $y $z))
          ($z C))
         (Link $x $y $z))

;; Test complex unify* query
;; After a series of unifications, it should define the function composition `(. g f)`
;; having type `(-> Number Bool)`
!(unify* (((: $ructor (-> $premise1 $premise2 $conclusion)) (: . (-> (-> $b $c) (-> $a $b) (-> $a $c))))
          ((: ($ructor $proof1 $proof2) $conclusion) (: $term (-> Number Bool)))
          ((: $proof1 $premise1) (: g (-> String Bool)))
          ((: $proof2 $premise2) (: f (-> Number String))))
         (: $term (-> Number Bool)))
