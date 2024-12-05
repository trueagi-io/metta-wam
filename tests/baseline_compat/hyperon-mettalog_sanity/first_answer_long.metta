
;;  MeTTaLog
;
; deb12user@DEBIAN12B:~/metta-wam$ time mettalog tests/baseline_compat/hyperon-mettalog_sanity/first_answer_long.metta
; [(: a A)
 (: a B)
 (: abc (Implication (AndLink A B) C))
 (: cde (Implication (OrLink C D) E))
 (: (ConjunctionEliminationLeft (ConjunctionIntroduction a a)) A)
 (: (ConjunctionEliminationLeft (ConjunctionIntroduction a a)) A)]
; % 626
695 inferences
 0.194 CPU in 0.199 seconds (97% CPU
 3231083 Lips)
; ; DEBUG Exit code of METTA_CMD: 7
;
; real    0m2.132s
; user    0m2.095s
; sys     0m0.041s
; deb12user@DEBIAN12B:~/metta-wam$

;;  MeTTa in Rust
; 
; deb12user@DEBIAN12B:~/metta-wam$ time metta tests/baseline_compat/hyperon-mettalog_sanity/first_answer_long.metta
; [(: cde (Implication (OrLink C D) E))
 (: abc (Implication (AndLink A B) C))
 (: (ConjunctionEliminationRight (ConjunctionIntroduction cde cde)) (Implication (OrLink C D) E))
 (: (ConjunctionEliminationRight (ConjunctionIntroduction cde a)) B)
 (: a A)
 (: a B)]
;
; real    2m37.504s
; user    2m36.943s
; sys     0m0.540s
; deb12user@DEBIAN12B:~/metta-wam$

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

(: rb (-> Atom))
(= (rb) (: ModusPonens (-> (ImplicationLink $p $q) $p $q)))
(= (rb) (: Deduction (-> (ImplicationLink $p $q) (ImplicationLink $q $r) (ImplicationLink $p $r))))
(= (rb) (: DisjunctiveSyllogism (-> (OrLink $p $q) (NotLink $p) $q)))
(= (rb) (: DisjunctiveSyllogism (-> (OrLink $p $q) (NotLink $q) $p)))
(= (rb) (: ConjunctionIntroduction (-> $p $q (AndLink $p $q))))
(= (rb) (: ConjunctionEliminationLeft (-> (AndLink $p $q) $p)))
(= (rb) (: ConjunctionEliminationRight (-> (AndLink $p $q) $q)))
(= (rb) (: DisjunctionIntroduction (-> $p $q (OrLink $p $q))))

;!(assertEqual
;  (synthesize (: $term $type) kb rb (S (S Z)))
;  ...)

; !(let n o p)

(: first-few (-> Number Atom Atom))
(= (first-few $n $a) 
  (if (== $n 0) (let n o p)
   (if (== $n 1)
	  (car-atom $a) 
      (let $t (cdr-atom $a) (superpose ((car-atom $a) (first-few (- $n 1) $t)))))))
(: limit (-> Number Atom Atom))
(= (limit $n $x) (let $a (collapse $x) (first-few $n $a)))


; !(limit 1 (synthesize (: $term $type) kb rb (S (S Z))))
; !(limit 6 (synthesize (: $term $type) kb rb (S (S Z))))
!(limit 6 (unique (synthesize (: $term $type) kb rb (S (S Z)))))


; (: a A)
; (: a B)
; (: abc (Implication (AndLink A B) C))
; (: cde (Implication (OrLink C D) E))
; (: (ConjunctionEliminationLeft (ConjunctionIntroduction a a)) A)
; (: (ConjunctionEliminationLeft (ConjunctionIntroduction a a)) A)
