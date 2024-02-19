;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of Nat and proofs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Import synthesizer (Nat is already defined in it)
!(import! &self ../synthesis/Synthesize.metta)

;; Knowledge base
(: kb (-> Atom))
(= (kb) (superpose
         (;; Equality is reflexive.  We use === instead of == to make sure it
          ;; does not get reduced by the MeTTa interpreter.
          (: EqRefl (=== $x $x))

          ;; Base case of append function definition
          (: Base_plus (=== (plus Z $y) $y))

          ;; Recursive step of append function definition
          (: Rec_plus (=== (plus (S $x) $y) (S (plus $x $y))))

          ;; Property expressing that for any natural, Z is the right
          ;; identity of plus.  Note that the property definition is
          ;; the axiom, not the property itself as it is what we
          ;; attempt to prove.
          (: PropZRID_plus (=== (ZRID_plus $x) (=== (plus $x Z) $x))))))

;; Rule base
(: rb (-> Atom))
(= (rb) (superpose
         (;; Equality is transitive
          (: EqTrans (-> ;; Premises
                         (=== $x $y)
                         (=== $y $z)
                         ;; Conclusion
                         (=== $x $z)))

          ;; Equality is symmetric
          (: EqSym (-> ;; Premise
                       (=== $x $y)
                       ;; Conclusion
                       (=== $y $x)))

          ;; Structural preservation of equality
          (: UnaryEqStruct (-> ;; Premise
                               (=== $x $x')
                               ;; Conclusion
                               (=== ($op $x) ($op $x'))))
          (: BinaryEqStruct (-> ;; Premises
                                (=== $x $x')
                                (=== $y $y')
                                ;; Conclusion
                                (=== ($op $x $y) ($op $x' $y'))))

          ;; Rule of replacement
          (: UnaryRepl (-> ;; Premises
                           (=== $x $x')
                           ($op $x)
                           ;; Conclusion
                           ($op $x')))
          (: BinaryRepl (-> ;; Premises
                            (=== $x $x')
                            (=== $y $y')
                            ($op $x $y)
                            ;; Conclusion
                            ($op $x' $y')))

          ;; Structural induction on Nat
          (: StructuralInductionNat (-> ;; Premises
                                     (: $p (-> Nat Type)) ; property
                                     ($p Z)               ; base case
                                     (-> (: $x Nat)
                                         ($p $x)
                                         ($p (S $x)))     ; inductive step
                                     ;; Conclusion
                                     (-> (: $x' Nat) ($p $x')))))))

(: IndZRID_plus (-> (ZRID_plus $x) (ZRID_plus (S $x))))
(=== (IndZRID_plus $prf) NEXT)

;; Prove that Z is left identity of plus
!(synthesize (: $proof (=== (plus Z $y) $y)) kb rb Z)

;; Prove that Z is the right identity of plus
;; !(synthesize (: $proof (=== (plus $x Z) $x)) kb rb (fromNumber 3))
