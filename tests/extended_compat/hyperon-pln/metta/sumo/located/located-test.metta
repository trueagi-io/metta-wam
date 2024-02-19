;; Test reasoning on located.kif.metta.

;; Import synthesizer
!(import! &self ../../synthesis/Synthesize.metta)

;; Import knowledge base
!(import! &kb located.kif.metta)

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

;; Call synthesizer on query (located John JohnsHouse)
;;
;; It should generate the following proof
;;
;; -----------------------------------------------------------  ------------------------------------
;; (⟹ (orientation $OBJ1 $OBJ2 Inside) (located $OBJ1 $OBJ2))  (orientation John JohnsHouse Inside)
;; -------------------------------------------------------------------------------------------------(ModusPonens)
;;                                          (located John JohnsHouse)
!(synthesize (: $proof (located John JohnsHouse)) kb rb (S Z))

;; Call synthesizer on query (located John SanJose)
;;
;; It should generate the following proof
;;
;;                                                     -----------------------------------------------------------  ------------------------------------
;;                                                     (⟹ (orientation $OBJ1 $OBJ2 Inside) (located $OBJ1 $OBJ2))  (orientation John JohnsHouse Inside)
;;                                                     -------------------------------------------------------------------------------------------------(ModusPonens)     ----------------------------
;;                                                                                         (located John JohnsHouse)                                                      (located JohnsHouse SanJose)
;; --------------------------------------------------------------------------------        ----------------------------------------------------------------------------------------------------------(ConjunctionIntroduction)
;; (⟹ (∧ (located $INST1 $INST2) (located $INST2 $INST3)) (located $INST1 $INST3))                                       (∧ (located John JohnsHouse) (located JohnsHouse SanJose))
;; ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------(ModusPonens)
;;                                                                                 (located John SanJose)
!(synthesize (: $proof (located John SanJose)) kb rb (fromNumber 3))
