;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of a chainer, Nat, plus and some proofs.
;;
;; Similar to NatStandaloneTest.metta but the chainer assumes rules
;; are formatted as dependent products.  In that manner we can make a
;; more verbatim replication of
;;
;; https://idris2.readthedocs.io/en/latest/tutorial/theorems.html#proving-theorems
;;
;; Eventually we should be able to use that chainer to do constructive
;; mathematics with dependent sum/product in place of
;; existential/universal quantifier.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge and rule base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!(bind! &kb (new-space))

;; Knowledge base

;; Refl is disabled because not necessary in all examples so far

;; Equality is reflexive.  We use === instead of == to make sure it
;; does not get reduced by the MeTTa interpreter.
;;
;; TODO: maybe we want to curry === as well.
!(add-atom &kb (: Refl (=== $x $x)))

;; ;; Equality is transitive
;; !(add-atom &kb (: Trans (-> (: $prf1 (=== $x $y))     ; Premise 1
;;                             (-> (: $prf2 (=== $y $z)) ; Premise 2
;;                                 (=== $x $z)))))       ; Conclusion

;; ;; Equality is symmetric
;; !(add-atom &kb (: Sym (-> (: $prf (=== $x $y)) ; Premise
;;                           (=== $y $x))))       ; Conclusion

;; ;; Equality respects function application
;; !(add-atom &kb (: Cong (-> (: $f (-> $a $b))              ; Premise 1
;;                            (-> (: $prf (=== $x $x'))      ; Premise 2
;;                                (=== ($f $x) ($f $x')))))) ; Conclusion

;; ;; Rule of replacement (nullary operator)
;; ;; TODO: could use Replace1 combine with identity function instead.
;; !(add-atom &kb (: Replace0 (-> (: $prf1 (=== $x $x')) ; Premise 1
;;                                (-> (: $prf2 $x)       ; Premise 2
;;                                    $x'))))            ; Conclusion

;; ;; Structural induction on Nat
;; !(add-atom &kb (: NatInd
;;                   (-> (: $x Nat)                                        ; Premise 1
;;                       (-> (: $p (-> Nat Type))                          ; Premise 2
;;                           (-> (: $prf_base ($p Z))                      ; Premise 3 (base case)
;;                               (-> (: $prf_rec (-> ($p $x) ($p (S $x)))) ; Premise 4 (inductive step)
;;                                   ($p $x')))))))                        ; Conclusion

;; ;; Definition of plus (base case)
;; ;; TODO: maybe could be replaced by (=== (plus Z) id)
;; !(add-atom &kb (: plusBase (-> (: $y Nat)                ; Premise
;;                                (=== ((plus Z) $y) $y)))) ; Conclusion

;; ;; Definition of plus (recursive step)
;; !(add-atom &kb (: plusRec (-> (: $x Nat)                                          ; Premise 1
;;                               (-> (: $y Nat)                                      ; Premise 2
;;                                   (=== ((plus (S $x)) $y) (S ((plus $x) $y))))))) ; Conclusion

;; ;; Property expressing that for any natural, Z is the right identity
;; ;; of plus.  Note that the property definition is the axiom, not the
;; ;; property itself as it is what we attempt to prove.
;; !(add-atom &kb (: plusRightIdProp (-> (: $x Nat)                                       ; Premise
;;                                       (=== (plusRightId $x) (=== ((plus $x) Z) $x))))) ; Conclusion

;; ;; Composition operator
;; !(add-atom &kb (: . (-> (-> $b $c)         ; Premise 1
;;                         (-> (-> $a $b)     ; Premise 2
;;                             (-> $a $c))))) ; Conclusion

;; Anything can serve as a variable.  Even a closed term, it simply
;; will not unify with any other closed term than itself, which makes
;; for a very specific, yet valid, abstraction.
!(add-atom &kb (: $x Variable))

;; Lambda
;;
;; TODO: we could add as premise that $t is a type.
!(add-atom &kb (: lambda (-> (: $x Variable)             ; Premise 1
                             (-> (: $prf $thm)           ; Premise 2
                                 (-> (: $x $t) $thm))))) ; Conclusion

;; ;; The witness of (: $x $t) is $x.
;; ;; It is probably a bad idea!
;; !(add-atom &kb (: $x (: $x $t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backward DTL Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Similar to the backward chainer in NatStandaloneTest but rules are
;; assumed represented as dependent products.  Meaning rule premises
;; must contain type-of relationships.  That is, instead of
;;
;; (: RULE_CONSTRUCTOR (-> PREMISE CONCLUSION))
;;
;; it must be
;;
;; (: RULE_CONSTRUCTOR (-> (: PROOF PREMISE) CONCLUSION))
;;
;; regardless of whether PROOF is present or not in CONCLUSION.
;; Typically PROOF will be a variable, but it can also be a term
;; containing variables, in which case CONCLUSION may contain that
;; term, or variables of that term.

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_)
   ;; Query the knowledge for a rule or a fact
   (trace! (⊷ bc-base (: $prf $ccln) $_)
   (match &kb (: $prf $ccln) (: $prf $ccln))))

;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (trace! (⊷ bc-rec (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (;; Recurse on proof abstraction
          ((: $prfabs (-> (: $prfarg $prms) $ccln))
           (bc (: $prfabs (-> (: $prfarg $prms) $ccln)) $k))
          ;; Recurse on proof argument
          ((: $prfarg $prms)
           (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln))))

;;;;;;;;;;;;;;;
;; Reduction ;;
;;;;;;;;;;;;;;;

;; Reduction rules to simplify proofs and reduce redundancy

;; TODO: these rules should be proven first.  Then they could
;; automatically be inserted.

;; Involution of symmetry
(= (Sym (Sym $prf)) $prf)

;; Composition to application
(= (((. $g) $f) $x) ($g ($f $x)))

;; Define plus
(: plus (-> Nat (-> Nat Nat)))
(= ((plus Z) $y) $y)
(= ((plus (S $x)) $y) (S ((plus $x) $y)))

;;;;;;;;;;;;
;; Proofs ;;
;;;;;;;;;;;;

;; ;; Prove that Z is left identity of plus
;; ;; !(assertEqual
;;   !(bc (: $prf (=== ((plus Z) $y) $y)) Z)
;;   ;; (: Refl (=== $y $y)))

;; ;; Prove that any natural equals to itself
;; ;; !(assertEqual
;;   !(bc (: $prf (-> (: $y Nat) (=== $y $y))) (fromNumber 2))
;;   ;; (: ((lambda $y) Refl) (-> (: $y Nat) (=== $y $y))))

;; Prove that (=== (=== (plus Z Z) Z) (ZRID_plus Z)).
;;
;; The following proof tree is expected to prove that:
;;
;; ---------------------------------------(plusRightIdProp)
;; (=== (plusRightId Z) (=== (plus Z Z) Z))
;; ---------------------------------------(Sym)
;; (=== (=== (plus Z Z) Z) (plusRightId Z))
;;
;; Or in MeTTa format:
;;
;; (Sym plusRightIdProp)
;; !(assertEqual
  !(bc (: $prf (=== (=== ((plus Z) Z) Z) (plusRightId Z))) (fromNumber 2))
  ;; (: (Sym plusRightIdProp) (=== (=== (plus Z Z) Z) (plusRightId Z))))

;; ;; Prove that (plusRightId Z) holds.  Meaning that Z is the right
;; ;; identity of plus for Z.
;; ;;
;; ;; The following proof tree does that:
;; ;;
;; ;; ---------------------------------------(plusRightIdProp)
;; ;; (=== (plusRightId Z) (=== (plus Z Z) Z))
;; ;; ---------------------------------------(Sym)  -----------------(plusBase)
;; ;; (=== (=== (plus Z Z) Z) (plusRightId Z))      (=== (plus Z Z) Z)
;; ;; ---------------------------------------------------------------(Replace0)
;; ;;                        (plusRightId Z)
;; ;;
;; ;; or in MeTTa format:
;; ;;
;; ;; (Replace0 (Sym plusRightIdProp) plusBase)
;; ;;
;; ;; TODO: re-enable when assertContainResults is introduced
;; !(assertEqualToResult
;;   (bc (: $prf (plusRightId Z)) (fromNumber 4))
;;   ((: ((Replace0 plusBase) ((Replace0 (Sym plusBase)) ((Replace0 (Sym plusRightIdProp)) plusBase))) (plusRightId Z))
;;    (: ((Replace0 ((Trans plusBase) (Sym plusRightIdProp))) ((Replace0 (Sym plusBase)) plusBase)) (plusRightId Z))
;;    (: ((Replace0 ((Trans plusRightIdProp) (Sym plusRightIdProp))) ((Replace0 (Sym plusRightIdProp)) plusBase)) (plusRightId Z))
;;    (: ((Replace0 (Sym plusRightIdProp)) ((Trans (Sym plusBase)) ((Trans plusBase) plusBase))) (plusRightId Z))
;;    (: ((Replace0 (Sym plusRightIdProp)) plusBase) (plusRightId Z))))

;; ;; Prove that (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))
;; ;;
;; ;; The proof tree should be:
;; ;;
;; ;; ------------------------------------(plusRec)
;; ;; (=== (plus (S $x) Z) (S (plus $x Z)))
;; ;; -------------------------------------------------------------(Trans)  -----------------------------------------------------(Cong)
;; ;; (-> (=== (S (plus $x Z)) (S $x)) (=== (plus (S $x) Z) (S $x)))        (-> (=== (plus $x Z) $x) (=== (S (plus $x Z)) (S $x)))
;; ;; ---------------------------------------------------------------------------------------------------------------------------(.)
;; ;;                                  (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))
;; ;;
;; ;; or in MeTTa format:
;; ;;
;; ;; ((. (Trans plusRec)) Cong)
;; ;;
;; ;; TODO: re-enable when assertContainResults is introduced
;; ;; !(assertEqualToResult
;;   !(bc (: $prf (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))) (fromNumber 3))
;;   ;; (: ((. (Trans plusRec)) Cong) (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x))))

;; ;; Prove that (-> (plusRightId $x) (plusRightId (S $x)))
;; ;;
;; ;; The proof tree should be:
;; ;;
;; ;;                                                                                                                                       ------------------------------------(plusRec)
;; ;;                                                                                                                                       (=== (plus (S $x) Z) (S (plus $x Z)))
;; ;; ------------------------------------------(plusRightIdProp)  ------------------------------------------------------(plusRightIdProp)  -------------------------------------------------------------(Trans)  -----------------------------------------------------(Cong)
;; ;; (=== (plusRightId $x) (=== (plus $x Z) $x))                  (=== (plusRightId (S $x)) (=== (plus (S $x) Z) (S $x)))                  (-> (=== (S (plus $x Z)) (S $x)) (=== (plus (S $x) Z) (S $x)))        (-> (=== (plus $x Z) $x) (=== (S (plus $x Z)) (S $x)))
;; ;; ------------------------------------------(Sym)              ------------------------------------------------------(Sym)              ---------------------------------------------------------------------------------------------------------------------------(.)
;; ;; (=== (=== (plus $x Z) $x) (plusRightId $x))                  (=== (=== (plus (S $x) Z) (S $x)) (plusRightId (S $x)))                                                   (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))
;; ;; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(Replace2)
;; ;;                                                                                                   (-> (plusRightId $x) (plusRightId (S $x)))
;; ;;
;; ;; or in MeTTa format:
;; ;;
;; ;; (((Replace2 (Sym plusRightIdProp)) (Sym plusRightIdProp)) ((. (Trans plusRec)) Cong))
;; ;;
;; ;; There is also a proof not involving Replace2, using two instances of Replace0 instead
;; ;;
;; ;; ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp))))
;; ;;
;; ;; It's a bit more verbose but requires a smaller rule base which
;; ;; speeds up backward chaining, so we are aiming for that instead.
;; ;;
;; ;; TODO: re-enable when assertContainResults is introduced
;; ;; !(assertEqualToResult
;;   !(bc (: $prf (-> (plusRightId $x) (plusRightId (S $x)))) (fromNumber 4))
;;   ;; (: ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp)))) (-> (plusRightId $x) (plusRightId (S $x))))

;; ;; Prove that Z is the right identity of plus
;; ;;
;; ;; ---------------------------------------(plusRightIdProp)
;; ;; (=== (plusRightId Z) (=== (plus Z Z) Z))
;; ;; ---------------------------------------(Sym)  -----------------(plusBase)
;; ;; (=== (=== (plus Z Z) Z) (plusRightId Z))      (=== (plus Z Z) Z)                   <PROOF IS PROVIDED ABOVE>
;; ;; ---------------------------------------------------------------(Replace0)  -----------------------------------------(Replace2)
;; ;;                        (plusRightId Z)                                     (-> (plusRightId $x) (plusRightId (S $x)))
;; ;;                        ---------------------------------------------------------------------------------------------(NatInd)
;; ;;                                                               (plusRightId $x)
;; ;;
;; ;; or in MeTTa format
;; ;;
;; ;; ((NatInd ((Replace0 (Sym plusRightIdProp)) plusBase)) ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp)))))
;; ;;
;; ;; We provide half of the proof otherwise the search takes too much memory (over 64GB)
;; !(assertEqual
;;   (bc (: ((NatInd ((Replace0 (Sym plusRightIdProp)) plusBase)) $subprf) (plusRightId $x)) (fromNumber 5))
;;   (: ((NatInd ((Replace0 (Sym plusRightIdProp)) plusBase)) ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp))))) (plusRightId $x)))
