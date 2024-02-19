;; Test reasoning on the content of orientation.kif.metta.  Before
;; running that file, make sure `orientation.kif.metta` has been
;; properly generated.  See `README.md` for more information on how to
;; do that.

;; Import synthesizer
!(import! &self ../../synthesis/Synthesize.metta)

;; Import knowledge base
!(import! &kb orientation.kif.metta)

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

;; Call synthesizer on query (orientation A B Right) (already axiom)
!(synthesize (: $proof (orientation A B Right)) kb rb Z)

;; Call synthesizer on query (orientation A C Right)
!(synthesize (: $proof (orientation A C Right)) kb rb (fromNumber 2))
