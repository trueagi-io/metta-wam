;; Import modules

;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Enumerate all programs up to a given depth that are consistent with
;; the query
;;		 using the given axiom non-deterministic functions and rules.
;;
;; The arguments are:
;;
;; $query: an Atom under the form (: TERM TYPE).  The atom may contain
;;         free variables within TERM and TYPE to form various sort of
;;         queries
;;		 such as:
;;         1. Backward chaining: (: $term (Inheritance $x Mammal))
;;         2. Forward chaining: (: ($rule $premise AXIOM) $type)
;;         3. Mixed chaining: (: ($rule $premise AXIOM) (Inheritance $x Mammal))
;;         4. Type checking: (: TERM TYPE)
;;         5. Type inference: (: TERM $type)
;;
;; $kb: a nullary function to axiom
;;		 to non-deterministically pick up
;;      an axiom.  An axiom is an Atom of the form (: TERM TYPE).
;;
;; $rb: a nullary function to rule
;;		 to non-deterministically pick up a
;;      rule.  A rule is a function mapping premises to conclusion
;;		
;;      where premises and conclusion have the form (: TERM TYPE).
;;
;; $depth: a Nat representing the maximum depth of the generated
;;         programs.
;;
;; TODO: recurse over curried rules instead of duplicating code over
;; tuples.
(: synthesize (-> $a (-> $kt) (-> $rt) Nat $a))
;; Nullary rule (axiom)
(= (synthesize $query $kb $rb $depth)
   (let $query ($kb) $query))
;; Unary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise $conclusion)) ($rb))
          ((: ($ructor $proof) $conclusion) $query)
          ((: $proof $premise) (synthesize (: $proof $premise) $kb $rb $k)))
     $query))
;; Binary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k)))
     $query))
;; Trinary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $premise3 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2 $proof3) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k))
          ((: $proof3 $premise3) (synthesize (: $proof3 $premise3) $kb $rb $k)))
     $query))
;; Quaternary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $premise3 $premise4 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2 $proof3 $proof4) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k))
          ((: $proof3 $premise3) (synthesize (: $proof3 $premise3) $kb $rb $k))
          ((: $proof4 $premise4) (synthesize (: $proof4 $premise4) $kb $rb $k)))
     $query))
;; Quintenary rule
(= (synthesize $query $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $premise3 $premise4 $premise5 $conclusion)) ($rb))
          ((: ($ructor $proof1 $proof2 $proof3 $proof4 $proof5) $conclusion) $query)
          ((: $proof1 $premise1) (synthesize (: $proof1 $premise1) $kb $rb $k))
          ((: $proof2 $premise2) (synthesize (: $proof2 $premise2) $kb $rb $k))
          ((: $proof3 $premise3) (synthesize (: $proof3 $premise3) $kb $rb $k))
          ((: $proof4 $premise4) (synthesize (: $proof4 $premise4) $kb $rb $k))
          ((: $proof5 $premise5) (synthesize (: $proof5 $premise5) $kb $rb $k)))
     $query))

(: kb (-> Atom))
(= (kb) (: a A))
(= (kb) (: a B))
(= (kb) (: abc (Implication (AndLink A B) C)))
(= (kb) (: cde (Implication (OrLink C D) E)))

!(assertEqualToResult
    (kb)
    ( (: abc (Implication (AndLink A B) C))
      (: a A) 
	  (: a B)
      (: cde (Implication (OrLink C D) E))))


(: rb (-> Atom))
(= (rb) (: ModusPonens (-> (ImplicationLink $p $q) $p $q)))
(= (rb) (: Deduction (-> (ImplicationLink $p $q) (ImplicationLink $q $r) (ImplicationLink $p $r))))
(= (rb) (: DisjunctiveSyllogism (-> (OrLink $p $q) (NotLink $p) $q)))
(= (rb) (: DisjunctiveSyllogism (-> (OrLink $p $q) (NotLink $q) $p)))
(= (rb) (: ConjunctionIntroduction (-> $p $q (AndLink $p $q))))
(= (rb) (: ConjunctionEliminationLeft (-> (AndLink $p $q) $p)))
(= (rb) (: ConjunctionEliminationRight (-> (AndLink $p $q) $q)))
(= (rb) (: DisjunctionIntroduction (-> $p $q (OrLink $p $q))))


!(assertEqualToResult 
     (synthesize (: $term $type) kb rb (S Z))
        ((: abc (Implication (AndLink A B) C))
		 (: cde (Implication (OrLink C D) E)) (: a A) (: a B)
		 (: (ConjunctionIntroduction abc abc) (AndLink (Implication (AndLink A B) C) (Implication (AndLink A B) C)))
		 (: (ConjunctionIntroduction abc cde) (AndLink (Implication (AndLink A B) C) (Implication (OrLink C D) E)))
		 (: (ConjunctionIntroduction abc a) (AndLink (Implication (AndLink A B) C) A))
		 (: (ConjunctionIntroduction abc a) (AndLink (Implication (AndLink A B) C) B))
		 (: (ConjunctionIntroduction cde abc) (AndLink (Implication (OrLink C D) E) (Implication (AndLink A B) C)))
		 (: (ConjunctionIntroduction cde cde) (AndLink (Implication (OrLink C D) E) (Implication (OrLink C D) E)))
		 (: (ConjunctionIntroduction cde a) (AndLink (Implication (OrLink C D) E) A))
		 (: (ConjunctionIntroduction cde a) (AndLink (Implication (OrLink C D) E) B))
		 (: (ConjunctionIntroduction a abc) (AndLink A (Implication (AndLink A B) C)))
		 (: (ConjunctionIntroduction a cde) (AndLink A (Implication (OrLink C D) E)))
		 (: (ConjunctionIntroduction a a) (AndLink A A))
		 (: (ConjunctionIntroduction a a) (AndLink A B))
		 (: (ConjunctionIntroduction a abc) (AndLink B (Implication (AndLink A B) C)))
		 (: (ConjunctionIntroduction a cde) (AndLink B (Implication (OrLink C D) E)))
		 (: (ConjunctionIntroduction a a) (AndLink B A))
		 (: (ConjunctionIntroduction a a) (AndLink B B))
		 (: (DisjunctionIntroduction abc abc) (OrLink (Implication (AndLink A B) C) (Implication (AndLink A B) C)))
		 (: (DisjunctionIntroduction abc cde) (OrLink (Implication (AndLink A B) C) (Implication (OrLink C D) E)))
		 (: (DisjunctionIntroduction abc a) (OrLink (Implication (AndLink A B) C) A))
		 (: (DisjunctionIntroduction abc a) (OrLink (Implication (AndLink A B) C) B))
		 (: (DisjunctionIntroduction cde abc) (OrLink (Implication (OrLink C D) E) (Implication (AndLink A B) C)))
		 (: (DisjunctionIntroduction cde cde) (OrLink (Implication (OrLink C D) E) (Implication (OrLink C D) E)))
		 (: (DisjunctionIntroduction cde a) (OrLink (Implication (OrLink C D) E) A))
		 (: (DisjunctionIntroduction cde a) (OrLink (Implication (OrLink C D) E) B))
		 (: (DisjunctionIntroduction a abc) (OrLink A (Implication (AndLink A B) C)))
		 (: (DisjunctionIntroduction a cde) (OrLink A (Implication (OrLink C D) E)))
		 (: (DisjunctionIntroduction a a) (OrLink A A))
		 (: (DisjunctionIntroduction a a) (OrLink A B))
		 (: (DisjunctionIntroduction a abc) (OrLink B (Implication (AndLink A B) C)))
		 (: (DisjunctionIntroduction a cde) (OrLink B (Implication (OrLink C D) E)))
		 (: (DisjunctionIntroduction a a) (OrLink B A))
		 (: (DisjunctionIntroduction a a) (OrLink B B))))

!(assertEqualToResult 
  (synthesize (: $term $type) kb rb Z)
  ((: a A) (: a B) (: abc (Implication (AndLink A B) C)) (: cde (Implication (OrLink C D) E))))


