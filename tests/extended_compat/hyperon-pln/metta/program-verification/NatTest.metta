;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of Nat, plus and some proofs.                            ;;
;;                                                                     ;;
;; The main proof of interest is that Z is the right identity of plus. ;;
;; That experiment does not prove it completely because it assumes its ;;
;; inductive property.                                                 ;;
;;                                                                     ;;
;; For the complete proof see NatStandaloneTest.metta.                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Import synthesizer (Nat is already defined in it)
!(import! &self ../synthesis/Synthesize.metta)

;; Knowledge base
(: kb (-> Atom))
(= (kb) (superpose
         (;; Equality is reflexive.  We use === instead of == to make sure it
          ;; ;; does not get reduced by the MeTTa interpreter.
          ;; (: EqRefl (=== $x $x))

          ;; Base case of plus function definition
          (: Base_plus (=== (plus Z $y) $y))

          ;; ;; Recursive step of plus function definition
          ;; (: Rec_plus (=== (plus (S $x) $y) (S (plus $x $y))))

          ;; If Z is the right identity of x then it is the right
          ;; identity of (S x).  This can be proved but is provided
          ;; here for simplicity.  See NatStandaloneTest.metta for the
          ;; complete proof.
          (: IndZRID_plus (-> (ZRID_plus $x) (ZRID_plus (S $x))))

          ;; Property expressing that for any natural, Z is the right
          ;; identity of plus.  Note that the property definition is
          ;; the axiom, not the property itself as it is what we
          ;; attempt to prove.
          (: PropZRID_plus (=== (ZRID_plus $x) (=== (plus $x Z) $x))))))

;; Rule base
(: rb (-> Atom))
(= (rb) (superpose
         (;; Equality is transitive
          ;; (: EqTrans (-> ;; Premises
          ;;                (=== $x $y)
          ;;                (=== $y $z)
          ;;                ;; Conclusion
          ;;                (=== $x $z)))

          ;; Equality is symmetric
          (: EqSym (-> ;; Premise
                       (=== $x $y)
                       ;; Conclusion
                       (=== $y $x)))

          ;; Structural preservation of equality
          (: EqStruct1 (-> ;; Premise
                           (=== $x $x')
                           ;; Conclusion
                           (=== ($op $x) ($op $x'))))
          ;; (: EqStruct2 (-> ;; Premises
          ;;                  (=== $x $x')
          ;;                  (=== $y $y')
          ;;                  ;; Conclusion
          ;;                  (=== ($op $x $y) ($op $x' $y'))))

          ;; Rule of replacement
          (: Repl0 (-> ;; Premise
                       (=== $x $x')
                       $x
                       ;; Conclusion
                       $x'))
          ;; (: Repl1 (-> ;; Premises
          ;;              (=== $x $x')
          ;;              ($op $x)
          ;;              ;; Conclusion
          ;;              ($op $x')))
          ;; (: Repl2 (-> ;; Premises
          ;;              (=== $x $x')
          ;;              (=== $y $y')
          ;;              ($op $x $y)
          ;;              ;; Conclusion
          ;;              ($op $x' $y')))

          ;; (: IndZRID_plus (-> (ZRID_plus $x) (ZRID_plus (S $x))))

          ;; Structural induction on Nat
          ;; (: IndNat (-> ;; Premises
          ;;               (: $p (-> Nat Type)) ; property
          ;;               ($p Z)               ; base case
          ;;               (-> (: $x Nat)
          ;;               ($p $x)
          ;;               ($p (S $x)))     ; inductive step
          ;;               ;; Conclusion
          ;;               (-> (: $x' Nat) ($p $x')))))))
          ;; Structural induction on Nat
          (: IndNat (-> ;; Premises
                        ($p Z)               ; base case
                        (-> ($p $x)
                            ($p (S $x)))     ; inductive step
                        ;; Conclusion
                        ($p $x'))))))

;; Prove that Z is left identity of plus
;; TODO: re-enable when subtitution is fixed
;; !(assertEqual
  !(synthesize (: $prf (=== (plus Z $y) $y)) kb rb Z)
  ;; (: Base_plus (=== (plus Z $y) $y)))

;; Prove that (=== (plus Z Z) Z)
!(assertEqual
  (synthesize (: $prf (=== (plus Z Z) Z)) kb rb Z)
  (: Base_plus (=== (plus Z Z) Z)))

;; Prove that (=== (=== (plus Z Z) Z) (ZRID_plus Z)).
;;
;; The following proof tree is expected to prove that:
;;
;; -------------------------------------(PropZRID_plus)
;; (=== (ZRID_plus Z) (=== (plus Z Z) Z))
;; -------------------------------------(EqSym)
;; (=== (=== (plus Z Z) Z) (ZRID_plus Z))
;;
;; on in MeTTa format:
;;
;; (EqSym PropZRID_plus)
!(assertEqual
  (synthesize (: $prf (=== (=== (plus Z Z) Z) (ZRID_plus Z))) kb rb (fromNumber 2))
  (: (EqSym PropZRID_plus) (=== (=== (plus Z Z) Z) (ZRID_plus Z))))

;; Prove that (ZRID_plus Z) holds.  Meaning that Z is the right
;; identity of plus for Z.
;;
;; The following proof tree does that:
;;
;; -------------------------------------(PropZRID_plus)
;; (=== (ZRID_plus Z) (=== (plus Z Z) Z))
;; -------------------------------------(EqSym)  -----------------(Base_plus)
;; (=== (=== (plus Z Z) Z) (ZRID_plus Z))        (=== (plus Z Z) Z)
;; ---------------------------------------------------------------(Repl0)
;;                        (ZRID_plus Z)
;;
;; or in MeTTa format:
;;
;; (Repl0 (EqSym PropZRID_plus) Base_plus)
!(assertEqual
  (synthesize (: $prf (ZRID_plus Z)) kb rb (fromNumber 2))
  (: (Repl0 (EqSym PropZRID_plus) Base_plus) (ZRID_plus Z)))

;; Prove that (-> (ZRID_plus $x) (ZRID_plus (S $x)))
;;
;; It is an axiom for now.
;; TODO: re-enable when subtitution is fixed
;; !(assertEqual
  !(synthesize (: $prf (-> (ZRID_plus $x) (ZRID_plus (S $x)))) kb rb Z)
  ;; (: IndZRID_plus (-> (ZRID_plus $x) (ZRID_plus (S $x)))))

;; Prove that Z is the right identity of plus
;;
;; -------------------------------------(PropZRID_plus)
;; (=== (ZRID_plus Z) (=== (plus Z Z) Z))
;; -------------------------------------(EqSym)  -----------------(Base_plus)
;; (=== (=== (plus Z Z) Z) (ZRID_plus Z))        (=== (plus Z Z) Z)
;; ---------------------------------------------------------------(Repl0)  -------------------------------------(IndZRID_plus)
;;                        (ZRID_plus Z)                                    (-> (ZRID_plus $x) (ZRID_plus (S $x)))
;;                        --------------------------------------------------------------------------------------(IndNat)
;;                                                               (ZRID_plus $x)
;;
;; TODO: re-enable when an assertContainResults is supported
;;
;; !(assertEqual
  !(synthesize (: $prf (ZRID_plus $x)) kb rb (fromNumber 3))
  ;; (: (IndNat (Repl0 (EqSym PropZRID_plus) Base_plus) IndZRID_plus) (ZRID_plus $x)))
