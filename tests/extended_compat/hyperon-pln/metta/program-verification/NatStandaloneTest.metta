;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of an omnichainer, Nat, plus and some proofs.            ;;
;;                                                                     ;;
;; The main proof of interest is that Z is the right identity of plus. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The omnichainer is expected to have the following properties:
;;
;; - Rules are curried.
;;
;; - There is no distinction between axioms and rules.
;;
;; - Rule composition, using the traditional composition operator `.`
;;   is included in the chaining process (whether it is embedded in
;;   the chainer implementation or is an external rule remains to be
;;   determined).
;;
;; Given such omnichainer, the inductive property that x + 0 = x
;; implies (S x) + 0 = (S x) should then have the following proof
;; abstraction:
;;
;; ------------------------------------(plusRec)
;; (=== (plus (S $x) Z) (S (plus $x Z)))
;; -------------------------------------------------------------(Trans)  -----------------------------------------------------(Cong)
;; (-> (=== (S (plus $x Z)) (S $x)) (=== (plus (S $x) Z) (S $x)))        (-> (=== (plus $x Z) $x) (=== (S (plus $x Z)) (S $x)))
;; ---------------------------------------------------------------------------------------------------------------------------(.)
;;                                  (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))
;;
;; or in MeTTa format:
;;
;; ((. (Trans plusRec)) Cong)

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

;; ;; Equality is reflexive.  We use === instead of == to make sure it
;; ;; does not get reduced by the MeTTa interpreter.
;; !(add-atom &kb (: Refl (=== $x $x)))

;; Equality is transitive
!(add-atom &kb (: Trans (-> (=== $x $y)         ; Premise 1
                            (-> (=== $y $z)     ; Premise 2
                                (=== $x $z))))) ; Conclusion

;; Equality is symmetric
!(add-atom &kb (: Sym (-> (=== $x $y)    ; Premise
                          (=== $y $x)))) ; Conclusion

;; Equality respects function application
!(add-atom &kb (: Cong (-> (=== $x $x')                ; Premise
                           (=== ($op $x) ($op $x'))))) ; Conclusion

;; Rule of replacement (nullary operator)
;; TODO: could use Replace1 combine with identity function instead.
!(add-atom &kb (: Replace0 (-> (=== $x $x') ; Premise 1
                               (-> $x       ; Premise 2
                                   $x'))))  ; Conclusion

;; The following replacement rules are disabled because not necessary
;; in the examples so far

;; ;; Rule of replacement (unary operator)
;; !(add-atom &kb (: Replace1 (-> (=== $x $x')      ; Premise 1
;;                                (-> ($op $x)      ; Premise 2
;;                                    ($op $x'))))) ; Conclusion

;; ;; Rule of replacement (binary operator)
;; !(add-atom &kb (: Replace2 (-> (=== $x $x')               ; Premise 1
;;                                (-> (=== $y $y')           ; Premise 2
;;                                    (-> ($op $x $y)        ; Premise 3
;;                                        ($op $x' $y')))))) ; Conclusion

;; Structural induction on Nat
!(add-atom &kb (: NatInd
                  (-> ($p Z)                       ; Premise 1 (base case)
                      (-> (-> ($p $x) ($p (S $x))) ; Premise 2 (inductive step)
                          ($p $x')))))             ; Conclusion

;; Definition of plus (base case)
!(add-atom &kb (: plusBase (=== (plus Z $y) $y)))

;; Definition of plus (recursive step)
!(add-atom &kb (: plusRec (=== (plus (S $x) $y) (S (plus $x $y)))))

;; ;; If Z is the right identity of plus for x then it is the right
;; ;; identity of plus for (S x).  NEXT: prove it.
;; !(add-atom &kb (: plusRightIdInd
;;                   (-> (plusRightId $x)        ; Premise
;;                       (plusRightId (S $x))))) ; Conclusion

;; Property expressing that for any natural, Z is the right identity
;; of plus.  Note that the property definition is the axiom, not the
;; property itself as it is what we attempt to prove.
!(add-atom &kb (: plusRightIdProp (=== (plusRightId $x) (=== (plus $x Z) $x))))

;; Composition operator
!(add-atom &kb (: . (-> (-> $b $c)         ; Premise 1
                        (-> (-> $a $b)     ; Premise 2
                            (-> $a $c))))) ; Conclusion

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backward DTL Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Similar to the traditional backward chaining DTL but rules are
;; curried.  This allows to partially apply rule which is useful for
;; inferring proof abstractions.

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_) (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

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

;;;;;;;;;;;;
;; Proofs ;;
;;;;;;;;;;;;

;; Prove that Z is left identity of plus
!(assertEqual
  (bc (: $prf (=== (plus Z $y) $y)) Z)
  (: plusBase (=== (plus Z $y) $y)))

;; Prove that (=== (=== (plus Z Z) Z) (ZRID_plus Z)).
;;
;; The following proof tree is expected to prove that:
;;
;; -------------------------------------(plusRightIdProp)
;; (=== (plusRightId Z) (=== (plus Z Z) Z))
;; -------------------------------------(Sym)
;; (=== (=== (plus Z Z) Z) (plusRightId Z))
;;
;; on in MeTTa format:
;;
;; (Sym plusRightIdProp)
!(assertEqual
  (bc (: $prf (=== (=== (plus Z Z) Z) (plusRightId Z))) (fromNumber 2))
  (: (Sym plusRightIdProp) (=== (=== (plus Z Z) Z) (plusRightId Z))))

;; Prove that (plusRightId Z) holds.  Meaning that Z is the right
;; identity of plus for Z.
;;
;; The following proof tree does that:
;;
;; ---------------------------------------(plusRightIdProp)
;; (=== (plusRightId Z) (=== (plus Z Z) Z))
;; ---------------------------------------(Sym)  -----------------(plusBase)
;; (=== (=== (plus Z Z) Z) (plusRightId Z))      (=== (plus Z Z) Z)
;; ---------------------------------------------------------------(Replace0)
;;                        (plusRightId Z)
;;
;; or in MeTTa format:
;;
;; (Replace0 (Sym plusRightIdProp) plusBase)
;;
;; TODO: re-enable when assertContainResults is introduced
!(assertEqualToResult
  (bc (: $prf (plusRightId Z)) (fromNumber 4))
  ((: ((Replace0 plusBase) ((Replace0 (Sym plusBase)) ((Replace0 (Sym plusRightIdProp)) plusBase))) (plusRightId Z))
   (: ((Replace0 ((Trans plusBase) (Sym plusRightIdProp))) ((Replace0 (Sym plusBase)) plusBase)) (plusRightId Z))
   (: ((Replace0 ((Trans plusRightIdProp) (Sym plusRightIdProp))) ((Replace0 (Sym plusRightIdProp)) plusBase)) (plusRightId Z))
   (: ((Replace0 (Sym plusRightIdProp)) ((Trans (Sym plusBase)) ((Trans plusBase) plusBase))) (plusRightId Z))
   (: ((Replace0 (Sym plusRightIdProp)) plusBase) (plusRightId Z))))

;; Prove that (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))
;;
;; The proof tree should be:
;;
;; ------------------------------------(plusRec)
;; (=== (plus (S $x) Z) (S (plus $x Z)))
;; -------------------------------------------------------------(Trans)  -----------------------------------------------------(Cong)
;; (-> (=== (S (plus $x Z)) (S $x)) (=== (plus (S $x) Z) (S $x)))        (-> (=== (plus $x Z) $x) (=== (S (plus $x Z)) (S $x)))
;; ---------------------------------------------------------------------------------------------------------------------------(.)
;;                                  (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))
;;
;; or in MeTTa format:
;;
;; ((. (Trans plusRec)) Cong)
;;
;; TODO: re-enable when assertContainResults is introduced
;; !(assertEqualToResult
  !(bc (: $prf (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))) (fromNumber 3))
  ;; (: ((. (Trans plusRec)) Cong) (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x))))

;; Prove that (-> (plusRightId $x) (plusRightId (S $x)))
;;
;; The proof tree should be:
;;
;;                                                                                                                                       ------------------------------------(plusRec)
;;                                                                                                                                       (=== (plus (S $x) Z) (S (plus $x Z)))
;; ------------------------------------------(plusRightIdProp)  ------------------------------------------------------(plusRightIdProp)  -------------------------------------------------------------(Trans)  -----------------------------------------------------(Cong)
;; (=== (plusRightId $x) (=== (plus $x Z) $x))                  (=== (plusRightId (S $x)) (=== (plus (S $x) Z) (S $x)))                  (-> (=== (S (plus $x Z)) (S $x)) (=== (plus (S $x) Z) (S $x)))        (-> (=== (plus $x Z) $x) (=== (S (plus $x Z)) (S $x)))
;; ------------------------------------------(Sym)              ------------------------------------------------------(Sym)              ---------------------------------------------------------------------------------------------------------------------------(.)
;; (=== (=== (plus $x Z) $x) (plusRightId $x))                  (=== (=== (plus (S $x) Z) (S $x)) (plusRightId (S $x)))                                                   (-> (=== (plus $x Z) $x) (=== (plus (S $x) Z) (S $x)))
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(Replace2)
;;                                                                                                   (-> (plusRightId $x) (plusRightId (S $x)))
;;
;; or in MeTTa format:
;;
;; (((Replace2 (Sym plusRightIdProp)) (Sym plusRightIdProp)) ((. (Trans plusRec)) Cong))
;;
;; There is also a proof not involving Replace2, using two instances of Replace0 instead
;;
;; ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp))))
;;
;; It's a bit more verbose but requires a smaller rule base which
;; speeds up backward chaining, so we are aiming for that instead.
;;
;; TODO: re-enable when assertContainResults is introduced
;; !(assertEqualToResult
  !(bc (: $prf (-> (plusRightId $x) (plusRightId (S $x)))) (fromNumber 4))
  ;; (: ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp)))) (-> (plusRightId $x) (plusRightId (S $x))))

;; Prove that Z is the right identity of plus
;;
;; ---------------------------------------(plusRightIdProp)
;; (=== (plusRightId Z) (=== (plus Z Z) Z))
;; ---------------------------------------(Sym)  -----------------(plusBase)
;; (=== (=== (plus Z Z) Z) (plusRightId Z))      (=== (plus Z Z) Z)                   <PROOF IS PROVIDED ABOVE>
;; ---------------------------------------------------------------(Replace0)  -----------------------------------------(Replace2)
;;                        (plusRightId Z)                                     (-> (plusRightId $x) (plusRightId (S $x)))
;;                        ---------------------------------------------------------------------------------------------(NatInd)
;;                                                               (plusRightId $x)
;;
;; or in MeTTa format
;;
;; ((NatInd ((Replace0 (Sym plusRightIdProp)) plusBase)) ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp)))))
;;
;; We provide the clue that structural induction is to be so that the
;; backward chainer only has to find the base case and the inductive
;; step, otherwise the search takes too much memory (over 64GB).
!(assertEqual
  (bc (: ((NatInd $basecase) $indstep) (plusRightId $x)) (fromNumber 5))
  (: ((NatInd ((Replace0 (Sym plusRightIdProp)) plusBase)) ((. (Replace0 (Sym plusRightIdProp))) ((. (Trans plusRec)) ((. Cong) (Replace0 plusRightIdProp))))) (plusRightId $x)))

;; TODO: try to reproduce
;;
;; https://idris2.readthedocs.io/en/latest/tutorial/theorems.html#proving-theorems
;;
;; For that one may need to use the following rule
;;
;; (: $x (: $x $t))
;;
;; or maybe something like
;;
;; (: (CTor $x) (-> (: $x $t) ($p $x)))
;;
;; In order to emulate dependent product or such.
