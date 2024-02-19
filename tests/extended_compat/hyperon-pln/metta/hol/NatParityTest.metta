;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of a chainer, Nat, plus and some proofs about the
;; existance of the parity properties of Nat such as Even and Odd.
;;
;; Implement a sigma type, take example from
;;
;; https://idris2.readthedocs.io/en/latest/tutorial/typesfuns.html#dependent-pairs
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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge/rule base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

!(bind! &kb (new-space))

;; The following code is translated into axioms and rules to handled
;; by the backward chainer.
;;
;; ;; Define Σ type (called DPair in Idris)
;; (: Σ (-> (: $a Type) (-> $a Type) Type))
;;
;; ;; Define DPair constructor
;; (: MkΣ (-> (: $p (-> $a Type)) (: $x $a) ($p $x) (Σ $a $p)))
;;
;; ;; Define the even property
;; (: Even (-> Nat Type))
;; (: MkEvenZ (Even Z))
;; (: MkEvenSS (-> (Even $k) (Even (S (S $k)))))
;;
;; ;; Define examples of even numbers (0 and 2)
;; (: (MkΣ Even Z MkEvenZ) (Σ Nat Even))
;; (: (MkΣ Even (S (S Z)) (MkEvenSS MkEvenZ)) (Σ Nat Even))
;; (: $prf (Σ Nat Even))
;;
;; ;; Define double function
;; (: double (-> Nat Nat))
;; (= (double Z) Z)
;; (= (double (S $k)) (S (S (double $k))))
;;
;; ;; Σ access functions
;; (: Σ.val (-> (Σ $a $p) $a))
;; (= (Σ.val (MkΣ $prop $val $prf)) $val)
;; (: Σ.prf (-> (Σ $a $p) $p))
;; (= (Σ.prf (MkΣ $prop $val $prf)) $prf)
;;
;; ;; Define double function, with the guaranty that the output is even
;; (: doubleΣ (-> Nat (Σ Nat Even)))
;; (= (doubleΣ Z) (MkΣ Even Z MkEvenZ))
;; (= (doubleΣ (S $k)) (MkΣ Even
;;                          (S (S (Σ.val (doubleΣ $k))))
;;                          (MkEvenSS (Σ.prf (doubleΣ $k)))))
;;
;; ;; For all x, (double x) is even
;; ;; ∀x (Even (double x))
;; (: double-even-prf (-> (: $x Nat) (Even (double $x))))
;;
;; ;; Independent product type (i.e. conjunction)
;; (: ⊗ (-> Type Type Type))
;;
;; ;; Equality
;; (: === (-> $a $a Type))
;;
;; ;; ;; NEXT
;;
;; ;; ;; For all x, there exists k such that k = (double x) and k is even
;; ;; ;; ∀x ∃k k=(double x) ∧ (Even k)
;; ;; (: double-Σ-even-prf (-> ($x : Nat)
;; ;;                          (Σ Nat (λ $k (⊗ (=== $k (double $x)) (Even $k))))))
;;
;; ;; (: double-Σ-even-prf (-> ($x : Nat) (Σ Nat (EqualDoubleAndEven $x))))
;; ;; (: EqualDoubleAndEven (-> Nat (-> Nat Type)))
;; ;; (= ((EqualDoubleAndEven $x) $k) (⊗ (=== $k (double $x)) (Even $k)))
;; ;; ;; Or, alternatively
;; ;; ;; (= (EqualDoubleAndEven $x) (λ $k (× (=== $k (double $x)) (Even $k))))

;; Define Nat
!(add-atom &kb (: Nat Type))
!(add-atom &kb (: Z Nat))
!(add-atom &kb (: S (-> (: $x Nat) Nat)))

;; NEXT: deal with asummptions like (: $k Nat)
;; !(add-atom &kb (: $k

;; Define Even
!(add-atom &kb (: Even (-> (: $x Nat) Type)))
!(add-atom &kb (: MkEvenZ (Even Z)))
!(add-atom &kb (: MkEvenSS (-> (: $prf (Even $k))   ; Premise
                               (Even (S (S $k)))))) ; Conclusion

;; Define Σ
!(add-atom &kb (: MkΣ (-> (: $p (-> (: $_ $a) Type)) ; Premise 1.  We
                                                     ; need to use (: $_ $a)
                                                     ; as opposed to just $a
                                                     ; to be fully consistent with
                                                     ; The (: PROOF PREMISE)
                                                     ; notation, till it becomes
                                                     ; optional.
                          (-> (: $x $a)              ; Premise 2
                              (-> (: $prf ($p $x))   ; Premise 3
                                  (Σ $a $p))))))     ; Conclusion

;; Equality is transitive
!(add-atom &kb (: Trans (-> (: $prf1 (=== $x $y))     ; Premise 1
                            (-> (: $prf2 (=== $y $z)) ; Premise 2
                                (=== $x $z)))))       ; Conclusion

;; Equality is symmetric
!(add-atom &kb (: Sym (-> (: $prf (=== $x $y)) ; Premise
                          (=== $y $x))))       ; Conclusion

;; Equality respects function application
!(add-atom &kb (: Cong (-> (: $f (-> (: $_ $a) $b))                 ; Premise 1
                           (-> (: $x $a)                            ; Premise 2
                               (-> (: $x' $a)                       ; Premise 3
                                   (-> (: $prf (=== $x $x'))        ; Premise 4
                                       (=== ($f $x) ($f $x')))))))) ; Conclusion

;; Rule of replacement
!(add-atom &kb (: Replace (-> (: $prf1 (=== $x $x')) ; Premise 1
                              (-> (: $prf2 $x)       ; Premise 2
                                  $x'))))            ; Conclusion

;; ;; Modus Ponens (application)
;; !(add-atom &kb (: ModusPonens (-> (: $f (-> (: $x $a) $b)) ; Premise 1
;;                                   (-> (: $x $a)            ; Premise 2
;;                                       $b))))               ; Conclusion

;; Define double
!(add-atom &kb (: double (-> (: $k Nat) Nat)))
!(add-atom &kb (: double_base (=== (double Z) Z)))
!(add-atom &kb (: double_rec (-> (: $k Nat)
                                 (=== (double (S $k)) (S (S (double $k)))))))

;; Composition operator
!(add-atom &kb (: . (-> (: $g (-> (: $y $b) $c))     ; Premise 1
                        (-> (: $f (-> (: $x $a) $b)) ; Premise 2
                            (-> (: $x $a) $c)))))    ; Conclusion

;; Flip operator
!(add-atom &kb (: flip (-> (: $f (-> (: $x $a) (-> (: $y $b) $c))) ; Premise
                           (-> (: $y $b) (-> (: $x $a) $c)))))     ; Conclusion

;; ;; Anything can serve as a variable.  Even a closed term, it simply
;; ;; will not unify with any other closed term than itself, which makes
;; ;; for a very specific, yet valid, abstraction.
;; !(add-atom &kb (: $x Variable))

;; ;; Lambda abstraction
;; !(add-atom &kb (: λ (-> (: $x Variable)           ; Premise 1
;;                         (-> (: $y $b)             ; Premise 2
;;                             (-> (: $x $a) $b))))) ; Conclusion

;;;;;;;;;;;;;;;;;;;;;;
;; Backward chainer ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Similar to the backward chainer in NatDTLTest but rules are
;; optionally represented as dependent products.  Meaning rule
;; premises may contain type-of relationships.  That is, instead of
;;
;; (: RULE_CONSTRUCTOR (-> PREMISE CONCLUSION))
;;
;; it must be
;;
;; (: RULE_CONSTRUCTOR (-> (: PROOF PREMISE) CONCLUSION))
;;
;; Typically PROOF will be a variable, but it can also be a term
;; containing variables, in which case CONCLUSION may contain that
;; term, or variables of that term.
;;
;; Unlike for the backward chainer in NatDTLTest, the non-dependent
;; format is supported as well.  EDIT: not yet!

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_)
   ;; Query the knowledge for a rule or a fact
   (match &kb (: $prf $ccln) (: $prf $ccln)))

;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (;; Recurse on proof abstraction
          ((: $prfabs (-> (: $prfarg $prms) $ccln))
           (bc (: $prfabs (-> (: $prfarg $prms) $ccln)) $k))
          ;; Recurse on proof argument
          ((: $prfarg $prms)
           (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;;;;;;;;;;;;;;;
;; Reduction ;;
;;;;;;;;;;;;;;;

;; Reduction rules to simplify proofs and reduce redundancy

;; TODO: these rules should be proven first.  Then they could
;; automatically be inserted.

;; Involution of symmetry
(= (Sym (Sym $f)) $f)

;; Composition to application
(= (((. $g) $f) $x) ($g ($f $x)))

;; Involution of flip
(= (flip (flip $f)) $f)

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

;; ;; Synthesize natural numbers
;; !(bc (: $prf Nat) (fromNumber 3))

;; Synthesize unary functions over natural numbers, that is
;; prove (-> (: $k Nat) Nat).
;; NEXT: explore lambda abstraction once the PANIC bug is fixed
!(bc (: $prf (-> (: $k Nat) Nat)) (fromNumber 3))

;; ;; Prove that 0 is even
;; !(bc (: $prf (Even (fromNumber 0))) (fromNumber 0))

;; ;; Prove that 2 is even
;; !(bc (: $prf (Even (fromNumber 2))) (fromNumber 1))

;; ;; Synthesize even numbers
;; !(bc (: $prf (Σ Nat Even)) (fromNumber 4))

;; ;; Prove that (double Z) is even.
;; ;;
;; ;; A possible proof of that is represented by the following proof tree:
;; ;;
;; ;;                                  --(Z)       -----------------(double_base)
;; ;;                                  Nat         (=== (double Z) Z)
;; ;; -------------------(Even) --(Z)  --(double)  -----------------(Sym)
;; ;; (-> (: $_ Nat) Type)      Nat    Nat         (=== Z (double Z))
;; ;; --------------------------------------------------------------(Cong)  -------(MkEvenZ)
;; ;;                (=== (Even Z) (Even (double Z)))                       (Even Z)
;; ;;                --------------------------------------------------------------(Replace)
;; ;;                                       (Even (double Z))
;; ;;
;; ;; Or in MeTTa format:
;; ;;
;; ;; (: ((Replace ((((Cong Even) Z) (double Z)) (Sym double_base))) MkEvenZ) (Even (double Z)))
;; !(bc (: $prf (Even (double Z))) (fromNumber 6))

;; ;; Prove
;; ;;
;; ;; (-> (: Even (-> (: $_ Nat) Type))
;; ;;     (-> (: (double (S $k)) Nat)
;; ;;         (-> (: (S (S (double $k))) Nat)
;; ;;             (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;; ;;                 (=== (Even (double (S $k))) (Even (S (S (double $k)))))))))
;; ;;
;; ;; which should merely be Cong.
;; !(assertEqual
;;   (bc (: $prf (-> (: Even (-> (: $_ Nat) Type))
;;                   (-> (: (double (S $k)) Nat)
;;                       (-> (: (S (S (double $k))) Nat)
;;                           (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;;                               (=== (Even (double (S $k))) (Even (S (S (double $k))))))))))
;;       (fromNumber 0))
;;   (: Cong (-> (: Even (-> (: $_ Nat) Type))
;;               (-> (: (double (S $k)) Nat)
;;                   (-> (: (S (S (double $k))) Nat)
;;                       (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;;                           (=== (Even (double (S $k))) (Even (S (S (double $k)))))))))))

;; ;; Prove
;; ;;
;; ;; (-> (: (double (S $k)) Nat)
;; ;;     (-> (: (S (S (double $k))) Nat)
;; ;;         (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;; ;;             (=== (Even (double (S $k))) (Even (S (S (double $k))))))))
;; ;;
;; ;; which should be (Cong Even).
;; !(assertEqual
;;   (bc (: $prf (-> (: (double (S $k)) Nat)
;;                   (-> (: (S (S (double $k))) Nat)
;;                       (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;;                           (=== (Even (double (S $k))) (Even (S (S (double $k)))))))))
;;       (fromNumber 1))
;;   (: (Cong Even) (-> (: (double (S $k)) Nat)
;;                      (-> (: (S (S (double $k))) Nat)
;;                          (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;;                              (=== (Even (double (S $k))) (Even (S (S (double $k))))))))))

;; ;; Prove NEXT
;; ;;
;; ;; (-> (: $k Nat)
;; ;;     (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;; ;;         (=== (Even (double (S $k))) (Even (S (S (double $k)))))))
;; ;;
;; ;; which could be
;; ;;
;; ;; (λ $k (((Cong Even) (double (S $k))) (S (S (double $k)))))
;; ;;
;; ;; or something not involving λ like
;; ;;
;; ;; (((Cong Even) (double (S $k))) (S (S (double $k)))) double_rec
;; ;;
;; ;; (λ $k (f (double_rec $k)))
;; ;;
;; ;; double_rec
;; ;;
;; ;; though this one is wrong, it should be corrected, maybe using flip,
;; ;; composition or such.
;;   !(bc (: $prf (-> (: $k Nat)
;;                    (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;;                        (=== (Even (double (S $k))) (Even (S (S (double $k))))))))
;;        (fromNumber 7))

;; ;; Prove
;; ;;
;; ;; (-> (: $k Nat) (-> (: Even (-> (: $_ Nat) Type)) (-> (: (double (S $k)) Nat) (-> (: (S (S (double $k))) Nat) (-> (: $eq (=== (double (S $k)) (S (S (double $k))))) (=== (Even (double (S $k))) (Even (S (S (double $k))))))))))
;; ;;
;; ;; (λ $k Cong)
;; !(bc (: $prf (-> (: $k Nat)
;;                  (-> (: Even (-> (: $_ Nat) Type))
;;                      (-> (: (double (S $k)) Nat)
;;                          (-> (: (S (S (double $k))) Nat)
;;                              (-> (: $eq (=== (double (S $k)) (S (S (double $k)))))
;;                                  (=== (Even (double (S $k))) (Even (S (S (double $k)))))))))))
;;      (fromNumber 5))

;; ;; Prove that (-> (: $k Nat) (=== (Even (double (S $k))) (Even (S (S (double $k))))))
;; ;;
;; ;; A possible proof tree would be
;; ;;
;; ;;
;; ;; 
;; ;; ----------------------------------------------------------------------------------------------
;; ;; (-> (: $k Nat) (=== (Even (double (S $k))) (Even (S (S (double $k))))))
;; !(bc (: $prf (-> (: $k Nat) (=== (Even (double (S $k))) (Even (S (S (double $k))))))) (fromNumber 7))



;; ;; Prove that (=== (Even (S (S (double $k)))) (Even (double (S $k))))
;; ;;
;; ;; (same as above but Sym)
;; ;;
;; !(bc (: $prf (=== (Even (S (S (double $k)))) (Even (double (S $k))))) (fromNumber 5))
;; !(bc (: $prf (-> (: $k Nat) (=== (Even (S (S (double $k)))) (Even (double (S $k))))) (fromNumber 5)))


;; Prove that if (double k) is even, then (double (S k)) is even
;;
;; A possible proof of that is represented by the following proof tree:
;;
;; NEXT: test subproofs
;;
;;                                                                                                                                                                                         --------------------------------------------------------(double_rec)  ---------($k)
;;                                                                                                                                                                                         (-> (: $k Nat) (=== (double (S $k)) (S (S (double $k)))))             (: $k Nat)
;;                                                                        -------------------(Even)  ----------------------(double (S $k))  --------------------------(S (S (double $k)))  -------------------------------------------------------------------------------(ModusPonens)
;;                                                                        (-> (: $_ Nat) Type)       (: (double (S $k)) Nat)                (: (S (S (double $k))) Nat)                    (=== (double (S $k)) (S (S (double $k))))
;;                                                 -----------------($e)  ---------------------------------------------------------------------------------------------------------------------------------------------------------(Cong)
;;                                                 (Even (double $k))                                           (=== (Even (double (S $k))) (Even (S (S (double $k)))))
;;                                                 -------------------------(MkEvenSS)                          ------------------------------------------------------(Sym)
;;                                                 (Even (S (S (double $k))))                                   (=== (Even (S (S (double $k)))) (Even (double (S $k))))
;;                    -----------------($e)        -------------------------------------------------------------------------------------------------------------------(Replace)
;;                    (Even (double $k))           (Even (double (S $k)))
;; --($k)             --------------------------------------------------(λ)
;; Nat                (-> (: $e (Even (double $k))) (Even (double (S $k))
;; ---------------------------------------------------------------------(λ)
;;  (-> (: $k Nat) (-> (: $e (Even (double $k))) (Even (double (S $k)))
;;
;; Or in MeTTa format:
;;
;; (λ (: $k Nat) (λ (: $e (Even (double $k))) (Replace (MkEvenSS $e) (Sym (Cong Even (double (S $k)) (S (S (double $k))) (ModusPonens double_rec $k))))))
;;
;; NEXT: is it possible to do better by using composition?  ANSWER: probably with composing double_rec, Trans, Sym and Cong

;; ;; NEXT: this fails with the following error
;; ;;
;; ;; thread '<unnamed>' panicked at /home/nilg/Work/TrueAGI/hyperon-experimental/lib/src/atom/matcher.rs:199:14:
;; ;; Unexpected state
;; ;; note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
;; ;; fatal runtime error: Rust panics must be rethrown
;; ;; Aborted (core dumped)
;; !(bc (: $prf (-> (: $k Nat) (-> (: $e (Even (double $k))) (Even (double (S $k)))))) (fromNumber 8))

;; ;; ;; Prove that for all x, (double x) is even
;; ;; !(bc (: $prf (-> (: $x Nat) (Even (double $x)))) (fromNumber 3))
