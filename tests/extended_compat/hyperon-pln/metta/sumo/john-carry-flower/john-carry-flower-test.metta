;; Test reasoning on the content of orientation.kif.metta.  Before
;; running that file, make sure `orientation.kif.metta` has been
;; properly generated.  See `README.md` for more information on how to
;; do that.

;; Import synthesizer
!(import! &self ../../synthesis/Synthesize.metta)

;; Import knowledge base
!(import! &kb john-carry-flower.kif.metta)

;; Import rule base
!(import! &rb ../rule-base.metta)

;; Label each statement in the knowledge base as a witness of a type,
;; because that is what the synthesizer expects.  For now each witness
;; is defined by using the `WitnessOf` construct.  So for instance the
;; statement
;;
;; (instance A Object)
;;
;; has the corresponding witness
;;
;; (WitnessOf (instance A Object))
;;
;; Then add the labelled statement in the labelled-kb space.
!(bind! &labelled-kb (new-space))
!(match &kb $x (add-atom &labelled-kb (: (WitnessOf $x) $x)))

;; Define knowledge base (called kb but different than &kb)
(: kb (-> Atom))
(= (kb) (match &labelled-kb $x $x))

;; Define rule base (called rb but different than &rb)
(: rb (-> Atom))
(= (rb) (match &rb $x $x))

;; ;; Call synthesizer on query (∧ (instance $C Carrying) (agent $C John))
;; !(synthesize (: $proof (∧ (instance $C Carrying) (agent $C John)))
;;              kb rb (fromNumber 1))

;; ;; Call synthesizer on query (∧ (instance $C Carrying) (agent $C John) (instance $F Flower))
;; !(synthesize (: $proof (∧ (instance $C Carrying) (agent $C John) (instance $F Flower)))
;;              kb rb (fromNumber 2))

;; Call synthesizer on query (instance JohnsCarry Transfer)
;;
;; Should synthesize the following proof tree
;;                                                              ----------------------------     ------------------------------
;;                                                              (subclass Carrying Transfer)     (instance JohnsCarry Carrying)
;;                                                              ---------------------------------------------------------------(BinaryConjunctionIntroduction)
;; (⟹ (∧ (subclass $X $Y) (instance $Z $X)) (instance $Z $Y))  (∧ (subclass Carrying Transfer) (instance JohnsCarry Carrying))
;; ----------------------------------------------------------------------------------------------------------------------------(ModusPonens)
;;                                             (instance JohnsCarry Transfer)
!(synthesize (: $proof (instance JohnsCarry Transfer))
             kb rb (fromNumber 2))

;; Call synthesizer on query (∧ (instance JohnsCarry Transfer) (objectTransferred JohnsCarry JohnsVase) (orientation JohnsFlower JohnsVase Inside))
;;
;; Should synthesize the following proof tree
;;
;;
;;                                                              ----------------------------     ------------------------------
;;                                                              (subclass Carrying Transfer)     (instance JohnsCarry Carrying)
;;                                                              ---------------------------------------------------------------(BinaryConjunctionIntroduction)
;; (⟹ (∧ (subclass $X $Y) (instance $Z $X)) (instance $Z $Y))  (∧ (subclass Carrying Transfer) (instance JohnsCarry Carrying))
;; ----------------------------------------------------------------------------------------------------------------------------(ModusPonens)   ----------------------------------------  ------------------------------------------
;;                                             (instance JohnsCarry Transfer)                                                                  (objectTransferred JohnsCarry JohnsVase)  (orientation JohnsFlower JohnsVase Inside)
;;                                             ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(TrinaryConjunctionIntroduction)
;;                                                                       (∧ (instance JohnsCarry Transfer) (objectTransferred JohnsCarry JohnsVase) (orientation JohnsFlower JohnsVase Inside))
!(synthesize (: $proof (∧ (instance JohnsCarry Transfer) (objectTransferred JohnsCarry JohnsVase) (orientation JohnsFlower JohnsVase Inside)))
             kb rb (fromNumber 3))

;; Call synthesizer on query (objectTransferred JohnsCarry JohnsFlower)
;;
;; Should synthesize the following proof tree
;;                                                                                                                                       ----------------------------     ------------------------------
;;                                                                                                                                       (subclass Carrying Transfer)     (instance JohnsCarry Carrying)
;;                                                                                                                                       ---------------------------------------------------------------(BinaryConjunctionIntroduction)
;;                                                                          (⟹ (∧ (subclass $X $Y) (instance $Z $X)) (instance $Z $Y))  (∧ (subclass Carrying Transfer) (instance JohnsCarry Carrying))
;;                                                                          ----------------------------------------------------------------------------------------------------------------------------(ModusPonens)   ----------------------------------------  ------------------------------------------
;;                                                                                                                      (instance JohnsCarry Transfer)                                                                  (objectTransferred JohnsCarry JohnsVase)  (orientation JohnsFlower JohnsVase Inside)
;; -----------------------------------------------------------------------------------------------------------------    ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(TrinaryConjunctionIntroduction)
;; (⟹ (∧ (instance $T Transfer) (objectTransferred $T $O) (orientation $O2 $O Inside)) (objectTransferred $T $O2))    (∧ (instance JohnsCarry Transfer) (objectTransferred JohnsCarry JohnsVase) (orientation JohnsFlower JohnsVase Inside))
;; ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(ModusPonens)
;;                                                                                              (objectTransferred JohnsCarry JohnsFlower)
!(synthesize (: $proof (objectTransferred JohnsCarry JohnsFlower))
             kb rb (fromNumber 4))

;; Call synthesizer on query (∧ (instance $C Carrying)
;;                              (agent $C John)
;;                              (instance $F Flower)
;;                              (objectTransferred $C $F))
;;
;; Should synthesize the following proof tree
;;                                                                                                                                                                                                                                                                              ----------------------------     ------------------------------
;;                                                                                                                                                                                                                                                                              (subclass Carrying Transfer)     (instance JohnsCarry Carrying)
;;                                                                                                                                                                                                                                                                              ---------------------------------------------------------------(BinaryConjunctionIntroduction)
;;                                                                                                                                                                                                                 (⟹ (∧ (subclass $X $Y) (instance $Z $X)) (instance $Z $Y))  (∧ (subclass Carrying Transfer) (instance JohnsCarry Carrying))
;;                                                                                                                                                                                                                 ----------------------------------------------------------------------------------------------------------------------------(ModusPonens)   ----------------------------------------  ------------------------------------------
;;                                                                                                                                                                                                                 (instance JohnsCarry Transfer)                                                                                                              (objectTransferred JohnsCarry JohnsVase)  (orientation JohnsFlower JohnsVase Inside)
;;                                                                                            -----------------------------------------------------------------------------------------------------------------    --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(TrinaryConjunctionIntroduction)
;;                                                                                            (⟹ (∧ (instance $T Transfer) (objectTransferred $T $O) (orientation $O2 $O Inside)) (objectTransferred $T $O2))    (∧ (instance JohnsCarry Transfer) (objectTransferred JohnsCarry JohnsVase) (orientation JohnsFlower JohnsVase Inside))
;; ------------------------------   -----------------------   -----------------------------   ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(ModusPonens)
;; (instance JohnsCarry Carrying)   (agent JohnsCarry John)   (instance JohnsFlower Flower)   (objectTransferred JohnsCarry JohnsFlower)
;; --------------------------------------------------------------------------------------------------------------------------------------(QuaternaryConjuctionIntroduction)
;;  (∧ (instance JohnsCarry Carrying) (agent JohnsCarry John) (instance JohnsFlower Flower) (objectTransferred JohnsCarry JohnsFlower))
;;
;; Corresponding to the MeTTa expression:
;;
;; (: (QuaternaryConjunctionIntroduction (WitnessOf (instance JohnsCarry Carrying))
;;                                       (WitnessOf (agent JohnsCarry John))
;;                                       (WitnessOf (instance JohnsFlower Flower))
;;                                       (ModusPonens (WitnessOf (⟹ (∧ (instance JohnsCarry Transfer) (objectTransferred JohnsCarry JohnsVase) (orientation JohnsFlower JohnsVase Inside)) (objectTransferred JohnsCarry JohnsFlower)))
;;                                                    (TrinaryConjunctionIntroduction (ModusPonens (WitnessOf (⟹ (∧ (subclass Carrying Transfer) (instance JohnsCarry Carrying)) (instance JohnsCarry Transfer)))
;;                                                                                                 (BinaryConjunctionIntroduction (WitnessOf (subclass Carrying Transfer)) (WitnessOf (instance JohnsCarry Carrying))))
;;                                                                                    (WitnessOf (objectTransferred JohnsCarry JohnsVase))
;;                                                                                    (WitnessOf (orientation JohnsFlower JohnsVase Inside)))))
;;    (∧ (instance JohnsCarry Carrying) (agent JohnsCarry John) (instance JohnsFlower Flower) (objectTransferred JohnsCarry JohnsFlower)))
!(synthesize (: $proof (∧ (instance $C Carrying)
                          (agent $C John)
                          (instance $F Flower)
                          (objectTransferred $C $F)))
             kb rb (fromNumber 5))
