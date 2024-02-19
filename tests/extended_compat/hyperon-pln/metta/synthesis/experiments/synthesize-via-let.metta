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
(= (syn $query $depth) (let $query (facts) $query))

; Inductive case
; Decompose rules into premises, trying rules and facts up to a depth limit.
; Returns the query if it finds a valid derivation using the available knowledge
(= (syn $query (S $k)) (let* (((: $rule (-> $premise1 $premise2 $conclusion)) (rules))
                              ((: ($rule $prf1 $prf2) $conclusion) $query)
                              ((: $prf1 $premise1) (syn (: $prf1 $premise1) $k))
                              ((: $prf2 $premise2) (syn (: $prf2 $premise2) $k)))
                         $query))
