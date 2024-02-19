;; Test subtyping

;; Import synthesizer
!(import! &self ../synthesis/Synthesize.metta)

;; Import rule base
!(import! &rb rule-base.metta)

;; Define knowledge base for that test
(: kb (-> Atom))
(= (kb) (superpose (;; Reflexivity axiom
                    (: STRefl (<: $T $T))
                    ;; Subtype relationships
                    (: CM (<: Cat Mammal))
                    (: MA (<: Mammal Animal))
                    (: AP (<: Animal Physical))
                    ;; Explicit coercion
                    (: felix Cat)
                    (: weigh (-> Physical Weight))
                    ;; Implicit coercion
                    (: fC (: felix Cat))
                    (: wPW (: weigh (-> Physical Weight))))))

;; Define rule base (called rb but different than &rb)
(: rb (-> Atom))
(= (rb) (match &rb $x $x))

;; Check that felix is a cat
!(assertEqual
  (synthesize (: $proof (: felix Cat)) kb rb Z)
  (: fC (: felix Cat)))

;; Check that felix is a mammal (explicit coercion)
!(synthesize (: (coerce $proof felix) Mammal) kb rb (fromNumber 1))

;; Check that felix is a mammal (implicit coercion)
!(synthesize (: $proof (: felix Mammal)) kb rb (fromNumber 1))

;; Check that cat is a subtype of animal
!(synthesize (: $proof (<: Cat Animal)) kb rb (fromNumber 1))

;; Check that felix is an animal (explicit coercion)
!(synthesize (: (coerce $proof felix) Animal) kb rb (fromNumber 2))

;; Check that felix is an animal (implicit coercion)
!(synthesize (: $proof (: felix Animal)) kb rb (fromNumber 2))

;; Check that (-> Physical Weight) is a subtype of (-> Cat Weight)
!(synthesize (: $proof (<: (-> Physical Weight) (-> Cat Weight))) kb rb (fromNumber 3))

;; Find a function to weigh something physical (explicit coercion).
!(synthesize (: $fun (-> Physical Weight)) kb rb (fromNumber 0))

;; Find a function to weigh something physical and its associated
;; proof (implicit coercion).
!(synthesize (: $proof (: $fun (-> Physical Weight))) kb rb (fromNumber 0))

;; Find a function to weigh a cat (explicit coercion).
!(synthesize (: $fun (-> Cat Weight)) kb rb (fromNumber 3))

;; Find a function to weigh a cat and its associated proof (implicit
;; coercion).
!(synthesize (: $proof (: $fun (-> Cat Weight))) kb rb (fromNumber 3))
