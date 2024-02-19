;; Import modules
!(import! &self ../../common/Num.metta)
!(import! &self ../Unify.metta)

;; Enumerate all programs up to a given depth that are consistent with
;; the query, using the given axioms and rules.
;;
;; The arguments are:
;;
;; $query: an Atom under the form (: TERM TYPE).  The atom may contain
;;         free variables within TERM and TYPE to form various sort of
;;         queries, such as:
;;         1. Backward chaining: (: $term (Inheritance $x Mammal))
;;         2. Forward chaining: (: ($rule $premise AXIOM) $type)
;;         3. Mixed chaining: (: ($rule $premise AXIOM) (Inheritance $x Mammal))
;;         4. Type checking: (: TERM TYPE)
;;         5. Type inference: (: TERM $type)
;;
;; $axiom: a nullary function to axiom, to non-deterministically pick
;;         up an axiom.  An axiom is an Atom of the form
;;         (: TERM TYPE).
;;
;; $rule: a nullary function to rule, to non-deterministically pick up
;;        a rule.  A rule is a function mapping premises to
;;        conclusion, where premises and conclusion have the form
;;        (: TERM TYPE).
;;
;; $depth: a Nat representing the maximum depth of the generated
;;         programs.
;;
;; up to a given depth that are consistent with the query, using the
;; provided rule and axiom non-deterministic functions.  An axiom is
;; an Atom of the form
;;
;; TODO: recurse over curried rules instead of duplicating code over
;; tuples.
(: synthesize (-> Atom (-> Atom) (-> Atom) Nat Atom))
;; Nullary rule (axiom)
(= (synthesize $query $axiom $rule $depth)
   (unify $query ($axiom) $query))
;; Binary rule
(= (synthesize $query $axiom $rule (S $k))
   (unify* (((: $ructor (-> $premise1 $premise2 $conclusion)) ($rule))
            ((: ($ructor $proof1 $proof2) $conclusion) $query)
            ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $axiom $rule $k))
            ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $axiom $rule $k)))
           $query))
