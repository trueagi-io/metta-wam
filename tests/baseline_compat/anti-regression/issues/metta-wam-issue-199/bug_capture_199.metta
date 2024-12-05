;; Import synthesize
!(import! &self synthesize_capture_199)

;; Define axiom
(: axiom (-> Atom))
(= (axiom) (: f (-> Number String)))

;; Test synthesize
!(synthesize (: $term $type) (capture (axiom)))

