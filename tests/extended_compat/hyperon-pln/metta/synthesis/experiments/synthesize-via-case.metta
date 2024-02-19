;; Import modules
!(import! &self ../../common/Num.metta)

;; Define set of rules
(= (rules) (: . (-> (-> $b $c) (-> $a $b) (-> $a $c))))

;; Define set of facts
(= (facts) (superpose ((: f (-> Number String))
                       (: g (-> String Bool))
                       (: h (-> Bool Number)))))

;; Given a query under the form (: TERM TYPE), enumerate all programs
;; up to a given depth that are consistent with the query, using rules
;; and facts.
(: syn (-> Atom Nat Atom))

; Base case: match against facts directly
(= (syn $query $depth) (case (facts) (($query $query))))

; Inductive case
(= (syn $query (S $k)) (case (rules)
                         (((: $rule (-> $premise1 $premise2 $conclusion))
                           (case $query
                             (((: ($rule $prf1 $prf2) $conclusion)
                               (case (syn (: $prf1 $premise1) $k)
                                 (((: $prf1 $premise1)
                                   (case (syn (: $prf2 $premise2) $k)
                                     (((: $prf2 $premise2)
                                       $query)))))))))))))
