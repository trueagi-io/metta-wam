;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples of both backward and forward curried chainers.             ;;
;;                                                                     ;;
;; The curried forward chainer relies on the curried backward chainer  ;;
;; to unfold the rules.                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; Nat ;;
;;;;;;;;;

;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Define <=
(: <= (-> $a $a Bool))
(= (<= $x $y) (or (< $x $y) (== $x $y)))

;; Define cast functions between Nat and Number
(: fromNumber (-> Number Nat))
(= (fromNumber $n) (if (<= $n 0) Z (S (fromNumber (- $n 1)))))
(: fromNat (-> Nat Number))
(= (fromNat Z) 0)
(= (fromNat (S $k)) (+ 1 (fromNat $k)))

;;;;;;;;;;;;;;;;;;;;
;; Knowledge base ;;
;;;;;;;;;;;;;;;;;;;;

;; Simple knowledge base for expository purpose.  Do not confuse `->`,
;; the MeTTa arrow type, and `→`, a regular MeTTa symbol denoting a
;; relationship.

!(bind! &kb (new-space))

!(add-atom &kb (: a A))
!(add-atom &kb (: ab (→ A B)))
!(add-atom &kb (: bc (→ B C)))
!(add-atom &kb (: ModusPonens
                (-> (→ $p $q)           ; Premise 1
                    (-> $p              ; Premise 2
                        $q))))          ; Conclusion

;;;;;;;;;;;;;;;;;;;;;;
;; Backward chainer ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Curried Backward Chainer.  The arguments of the backward chainer
;; are:
;;
;; * Knowledge base: pointer to a space containing axioms and rules in
;;   the format (: <NAME> <RULE>).  Note that rules are explicitely
;;   curried, meaning that a rule with two premises is represented by
;;
;;   (: <NAME> (-> <PREMISE1> (-> <PREMISE2> <CONCLUSION>)))
;;
;; * Query: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables that may be
;;   filled by the backward chainer.
;;
;; * Maximum depth: maximum depth of the generated proof tree.
;;
;; A result is the query with its variables grounded, fully or
;; partially.  If multiple results are possible, they are returned as
;; a superposition.
(: bc (-> $a                            ; Knowledge base space
          $b                            ; Query
          Nat                           ; Maximum depth
          $b))                          ; Result
;; Base case
(= (bc $kb (: $prf $ccln) $_) (match $kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc $kb (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc $kb (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc $kb (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;; Test curred backward chainer
!(assertEqual
  (bc &kb (: $prf A) (fromNumber 0))
  (: a A))
!(assertEqual
  (bc &kb (: $prf B) (fromNumber 2))
  (: ((ModusPonens ab) a) B))
!(assertEqual
  (bc &kb (: $prf C) (fromNumber 3))
  (: ((ModusPonens bc) ((ModusPonens ab) a)) C))

;;;;;;;;;;;;;;;;;;;;;
;; Forward chainer ;;
;;;;;;;;;;;;;;;;;;;;;

;; Curried Forward Chainer.  The arguments of the forward chainer are:
;;
;; * Knowledge base: pointer to a space containing axioms and rules in
;;   the format (: <NAME> <RULE>).  Note that rules are explicitely
;;   curried, meaning that a rule with two premises is represented by
;;
;;   (: <NAME> (-> <PREMISE1> (-> <PREMISE2> <CONCLUSION>)))
;;
;; * Source: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables.  Beware that
;;   the source is assumed to be true.  That is especially important
;;   to consider when introducing free variables, because these free
;;   variables will be treated as if they are univerally quantified.
;;
;; * Maximum depth: maximum depth of the generated proof tree.
;;
;; A result is a conclusion that can be reached from the source.  If
;; multiple results are possible, they are returned as a
;; superposition.
(: fc (-> $a                            ; Knowledge base space
          $b                            ; Source
          Nat                           ; Maximum depth
          $b))                          ; Conclusion
;; Base case
(= (fc $kb (: $prf $prms) $_) (: $prf $prms))
;; Recursive step
(= (fc $kb (: $prfarg $prms) (S $k))
   (let (: $prfabs (-> $prms $ccln)) (bc $kb (: $prfabs (-> $prms $ccln)) $k)
     (fc $kb (: ($prfabs $prfarg) $ccln) $k)))
(= (fc $kb (: $prfabs (-> $prms $ccln)) (S $k))
    (let (: $prfarg $prms) (bc $kb (: $prfarg $prms) $k)
     (fc $kb (: ($prfabs $prfarg) $ccln) $k)))

;; Test curried forward chainer
!(assertEqualToResult
  (fc &kb (: ab (→ A B)) (fromNumber 1))
  ((: ab (→ A B))
   (: (ModusPonens ab) (-> A B))))
!(assertEqualToResult
  (fc &kb (: ab (→ A B)) (fromNumber 2))
  ((: ab (→ A B))
   (: (ModusPonens ab) (-> A B))
   (: ((ModusPonens ab) a) B)))
!(assertEqualToResult
  (fc &kb (: a A) (fromNumber 3))
  ((: a A)
   (: ((ModusPonens ab) a) B)
   (: ((ModusPonens bc) ((ModusPonens ab) a)) C)))
