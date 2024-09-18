;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of Nat and proofs of simple statements such as 2 + 2 = 4 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (: Rec_plus (=== (plus (S $x) $y) (S (plus $x $y)))))))

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

          ;; Structural preservation of equality, unary and binary
          (: EqStruct1 (-> ;; Premise
                           (=== $x $x')
                           ;; Conclusion
                           (=== ($op $x) ($op $x'))))
          (: EqStruct2 (-> ;; Premises
                           (=== $x $x')
                           (=== $y $y')
                           ;; Conclusion
                           (=== ($op $x $y) ($op $x' $y'))))

          ;; Rule of replacement, unary and binary
          (: Repl1 (-> ;; Premises
                       (=== $x $x')
                       ($op $x)
                       ;; Conclusion
                       ($op $x')))
          ;; Disabled to speed things up
          ;; (: Repl2 (-> ;; Premises
          ;;              (=== $x $x')
          ;;              (=== $y $y')
          ;;              ($op $x $y)
          ;;              ;; Conclusion
          ;;              ($op $x' $y')))
          )))

;; Prove that Z is left identity of plus
;; TODO: re-enable once subtitution is fixed
;; !(assertEqual
  !(synthesize (: $proof (=== (plus Z $y) $y)) kb rb Z)
  ;; (: Base_plus (=== (plus Z $y#90) $y#90)))

;; Prove that 1 + 0 = 1
;;
;; They are many possible proof trees, the simplest one seems to be
;;
;;                                                   -----------------(Base_plus)
;;                                                   (=== (plus Z Z) Z)
;; ----------------------------------(Rec_plus)  -------------------------(EqStruct1)
;; (=== (plus (S Z) Z) (S (plus Z Z)))           (=== (S (plus Z Z)) (S Z))
;; -----------------------------------------------------------------------(EqTrans)
;;                     (=== (plus (S Z) Z) (S Z))
;;
;; or in MeTTa format:
;;
;; (EqTrans Rec_plus (EqStruct1 Base_plus))
;;
;; The second simplest one seems to be
;;
;;                                                -----------------(Base_plus)
;;                                                (=== (plus Z Z) Z)
;; ----------------------------------(EqRefl)  -------------------------(EqStruct1)  ----------------------------------(Rec_plus)
;; (=== (plus (S Z) Z) (plus (S Z) Z))         (=== (S (plus Z Z)) (S Z))                (=== (plus (S Z) Z) (S (plus Z Z)))
;; ------------------------------------------------------------------------------------------------------------------------(Repl2)
;;                                                (=== (plus (S Z) Z) (S Z))
;; or in MeTTa format:
;;
;; (Repl2 EqRefl (EqStruct1 Base_plus) Rec_plus) (=== (plus (S Z) Z) (S Z)))
;;
;; But is not generated because BinaryRepl
!(assertEqualToResult
  (synthesize (: $proof (=== (plus (S Z) Z) (S Z))) kb rb (fromNumber 2))
  ((: (EqTrans Rec_plus (EqStruct1 Base_plus))
      (=== (plus (S Z) Z) (S Z)))
   (: (EqTrans (EqTrans EqRefl Rec_plus) (EqStruct1 Base_plus))
      (=== (plus (S Z) Z) (S Z)))
   (: (EqTrans (EqTrans Rec_plus EqRefl) (EqStruct1 Base_plus))
      (=== (plus (S Z) Z) (S Z)))))

;; Prove that 1 + 1 = 2
;;
;; The two simplest proof trees are the same as 1 + 0 = 1.
;;
;;
;; They are many possible proof trees, the simplest is the same as the
;; one for 1 + 0 = 1, that is:
;;
;;                                                         -----------------------(Base_plus)
;;                                                         (=== (plus Z (S Z) (S Z)
;; ------------------------------------------(Rec_plus)  ----------------------------(EqStruct1)
;; (=== (plus (S Z) (S Z)) (S (plus (S Z) Z)))           (=== (S (plus Z Z)) (S (S Z))
;; ----------------------------------------------------------------------------------(EqTrans)
;;                          (=== (plus (S Z) (S Z)) (S (S Z)))
;;
;; or in MeTTa format:
;;
;; (EqTrans Rec_plus (EqStruct1 Base_plus))
;;
;; This is due to the left recursive definition of plus.  The proof
;; tree of 1 + N = (S N) where N is any large number would be equally
;; simple.
!(assertEqualToResult
  (synthesize (: $proof (=== (plus (S Z) (S Z)) (S (S Z)))) kb rb (fromNumber 2))
  ((: (EqTrans Rec_plus (EqStruct1 Base_plus))
      (=== (plus (S Z) (S Z)) (S (S Z))))
   (: (EqTrans (EqTrans EqRefl Rec_plus) (EqStruct1 Base_plus))
      (=== (plus (S Z) (S Z)) (S (S Z))))
   (: (EqTrans (EqTrans Rec_plus EqRefl) (EqStruct1 Base_plus))
      (=== (plus (S Z) (S Z)) (S (S Z))))))

;; Prove that 1 + 5 = 6
!(assertEqualToResult
  (synthesize (: $proof (=== (plus (fromNumber 1) (fromNumber 5)) (fromNumber 6))) kb rb (fromNumber 2))
  ((: (EqTrans Rec_plus (EqStruct1 Base_plus))
      (=== (plus (S Z) (S (S (S (S (S Z)))))) (S (S (S (S (S (S Z))))))))
   (: (EqTrans (EqTrans EqRefl Rec_plus) (EqStruct1 Base_plus))
      (=== (plus (S Z) (S (S (S (S (S Z)))))) (S (S (S (S (S (S Z))))))))
   (: (EqTrans (EqTrans Rec_plus EqRefl) (EqStruct1 Base_plus))
      (=== (plus (S Z) (S (S (S (S (S Z)))))) (S (S (S (S (S (S Z))))))))))

;; Prove that 2 + 2 = 4
;;
;;
;;                                                                          --------------------------------------------------(Rec_plus)              ---------------------------------(Base_plus)
;;                                                                          (=== (plus (S Z) (S (S Z))) (S (plus Z (S (S Z)))))                       (=== (plus Z (S (S Z))) (S (S Z)))
;; ----------------------------------------------------------(Rec_plus)  ----------------------------------------------------------(EqStruct1)  -----------------------------------------(EqStruct1)
;; (=== (plus (S (S Z)) (S (S Z))) (S (plus (S Z) (S (S Z)))))           (=== (S (plus (S Z) (S (S Z)))) (S (S (plus Z (S (S Z))))))            (=== (S (plus Z (S (S Z)))) (S (S (S Z))))
;; --------------------------------------------------------------------------------------------------------------------------------(EqTrans)  -------------------------------------------------(EqStruct1)
;;                                    (=== (plus (S (S Z)) (S (S Z))) (S (S (plus Z (S (S Z))))))                                             (=== (S (S (plus Z (S (S Z))))) (S (S (S (S Z)))))
;; ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(EqTrans)
;;                                                                           (=== (plus (S (S Z)) (S (S Z))) (S (S (S (S Z)))))
;;
;; or in MeTTa format:
;;
;; (EqTrans (EqTrans Rec_plus (EqStruct1 Rec_plus)) (EqStruct1 (EqStruct1 Base_plus)))
;; TODO: re-enable when we have found a way to get rid of the cruft
;; !(assertEqualToResult
  !(synthesize (: $proof (=== (plus (fromNumber 2) (fromNumber 2)) (fromNumber 4))) kb rb (fromNumber 3))
 ;; (: (EqTrans (EqTrans Rec_plus (EqStruct1 Rec_plus)) (EqStruct1 (EqStruct1 Base_plus))) (=== (plus (S (S Z)) (S (S Z))) (S (S (S (S Z))))))
 ;; (: (EqTrans (EqTrans (EqTrans EqRefl Rec_plus) (EqStruct1 Rec_plus)) (EqStruct1 (EqStruct1 Base_plus))) (=== (plus (S (S Z)) (S (S Z))) (S (S (S (S Z))))))
 ;; (: (EqTrans (EqTrans (EqTrans Rec_plus EqRefl) (EqStruct1 Rec_plus)) (EqStruct1 (EqStruct1 Base_plus))) (=== (plus (S (S Z)) (S (S Z))) (S (S (S (S Z))))))
