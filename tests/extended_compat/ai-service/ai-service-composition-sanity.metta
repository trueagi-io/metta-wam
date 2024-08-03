;; Simple AI service composition example.  The backward chainer is
;; used to synthesize the composition of three AI services, spleeter,
;; vocals-translator and mixer, to turn an English song into a Chinese
;; song.

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

;;;;;;;;;;;;;;;;;;;;;
;; De Bruijn Index ;;
;;;;;;;;;;;;;;;;;;;;;

;; Define DeBruijn type
(: DeBruijn Type)

;; Define DeBruijn constructors
(: z DeBruijn)                        ; Zero
(: s (-> DeBruijn DeBruijn))          ; Successor

;;;;;;;;;;
;; List ;;
;;;;;;;;;;

;; Declaration of List data type and constructors
(: List (-> $a Type))
(: Nil (List $a))
(: Cons (-> $a (List $a) (List $a)))

;;;;;;;;;;;;;;;;;;;;;
;; Match over list ;;
;;;;;;;;;;;;;;;;;;;;;

;; Similar to match but takes a list of terms instead of a space.
(: match' (-> (List Atom) $a $a $a))
(= (match' Nil $pattern $rewrite) (empty))
(= (match' (Cons $head $tail) $pattern $rewrite) (let $pattern $head $rewrite))
(= (match' (Cons $head $tail) $pattern $rewrite) (match' $tail $pattern $rewrite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge and rule base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!(bind! &kb (new-space))

;;;;;;;;;;;;;;
;; Spleeter ;;
;;;;;;;;;;;;;;

;; Constructor
!(add-atom &kb (: sound-spleeter.MkDTLOutput
                  (-> (: $l NaturalLanguage)
                      (-> (: $voc (VocalsIn $l))
                          (-> (: $inst Instrumental)
                              (sound-spleeter.DTLOutput $l))))))

;; Access vocals
!(add-atom &kb (: sound-spleeter.DTLOutput.vocals
                  (-> (: $out (sound-spleeter.DTLOutput $l))
                      (VocalsIn $l))))

;; Access accomp
!(add-atom &kb (: sound-spleeter.DTLOutput.accomp
                  (-> (: $out (sound-spleeter.DTLOutput $l))
                      Instrumental)))

;; Method
!(add-atom &kb (: sound-spleeter.spleeter
                  (-> (: $song (SongIn $l))
                      (sound-spleeter.DTLOutput $l))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Vocals Translator ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Method
!(add-atom &kb (: vocals-translator.translate
                  (-> (: $en-vocals (VocalsIn "English"))
                      (VocalsIn "Chinese"))))

;;;;;;;;;;;
;; Mixer ;;
;;;;;;;;;;;

;; Method
!(add-atom &kb (: mixer.mix (-> (: $inst Instrumental)
                                (-> (: $voc (VocalsIn $l))
                                    (SongIn $l)))))

;;;;;;;;;;;;;;;;;;;;;;
;; Backward chainer ;;
;;;;;;;;;;;;;;;;;;;;;;

;; The following code is copied from NEXT.

;; Curried Backward Chainer with lambda abstraction and dependent
;; types.  A lambda abstraction is represented by
;;
;; (λ <INDEX> <BODY>)
;;
;; where <INDEX> is a De Bruijn index as define above, such as z or
;; (s z), and <BODY> is a term possibly containing <INDEX>.  Note
;; that the use of De Bruijn index in lambda abstraction is somewhat
;; unconventional here.  It differs from what is described in
;; https://en.wikipedia.org/wiki/De_Bruijn_index in three ways:
;;
;; 1. The index is explicitely attached to a λ by being its first
;;    argument.  For instance the lambda term λx.x, which would
;;    traditionally be represented by λ1 using De Bruijn index, would
;;    be represented here by the MeTTa term (λ z z).
;;
;; 2. As seen in the example above the index here starts at 0,
;;    represented by z, instead of 1.
;;
;; 3. The index increases as the lambda abstraction gets deeper.  For
;;    instance λx.λy.x, which would traditionally be represented by
;;    λλ2 using De Bruijn index, is represented here by the MeTTa term
;;    (λ z (λ (s z) z).
;;
;; This differences are due to the way the proof abstraction recursive
;; step is defined in the bc, as well as the need for having non
;; overlapping pattern matching between the two recursive steps.
;; Specifically, if λ had only one argument, then (λ <BODY>) would
;; overlap with (<ABS> <ARG>).  Having λ take 2 arguments instead of 1
;; has the disadvantage of making lambda abstraction not as compact.
;; On the flip side, the benefit is that the scope of an index is
;; easier to track.
;;
;; The arguments of the backward chainer are:
;;
;; * Knowledge base: pointer to a space containing axioms and rules in
;;   the format (: <NAME> <RULE>).  Note that rules are explicitely
;;   curried, meaning that a rule with two premises is represented by
;;
;;   (: <NAME> (-> <PREMISE1> (-> <PREMISE2> <CONCLUSION>)))
;;
;;   In addition, each premise must be an inline typing relationship,
;;   to represent dependent types.  It should be noted that such
;;   typing relationship must be provided even if the witness is not
;;   present in the rest of the definition.  For instance <PREMISE1>
;;   would have the format
;;
;;   (: <TERM1> <TYPE1>)
;;
;; * Environment: a list of typing relationship between De Bruijn
;;   index and type, such as
;;
;;   (Cons (: z String) (Cons (: (s z) Number) Nil))
;;
;; * De Bruijn Index: De Bruijn Index to use if a lambda abstraction
;;   is introduced.
;;
;; * Maximum depth: maximum depth of the generated proof tree.
;;
;; * Query: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables that may be
;;   filled by the backward chainer.  Note that for arguments of
;;   applications must be type annotated (unknowns types are of course
;;   allowed).  For instance the following query
;;
;;   (: (ModusPonens ab) $thrm)
;;
;;   would not work, instead one needs to provide
;;
;;   (: (ModusPonens (: ab $lemma)) $thrm)
;;
;; A result is the query with its variables grounded, fully or
;; partially.  If multiple results are possible, they are returned as
;; a superposition.
;;
;; As explained, the proof arguments must be type annotated.  Without
;; such type annotation, lambda abstraction sometimes leads to
;; incorrect results.  It is suspected to come from the fact that
;; otherwise the proof application recursive step loses bindings about
;; the premise.  Having annotated proof is however a good thing, to
;; display a proof tree and to have an indepth view into the proof.
;; In order to remove (resp. add) type annotation one can use
;; remove-type-annotation (resp. add-type-annotation).
(: bc (-> $a                            ; Knowledge base space
          $b                            ; Environment
          DeBruijn                      ; De Bruijn Index
          Nat                           ; Maximum depth
          $c                            ; Query
          $c))                          ; Result

;; Base cases

;; Match the knowledge base
(= (bc $kb $env $idx $_ (: $prf $thrm))
   (match $kb (: $prf $thrm) (: $prf $thrm)))
;; Match the environment
(= (bc $kb $env $idx $_ (: $prf $thrm))
   (match' $env (: $prf $thrm) (: $prf $thrm)))

;; Recursive steps

;; Proof application
(= (bc $kb $env $idx (S $k) (: ($prfabs (: $prfarg $prms)) $thrm))
   (let* (((: $prfabs (-> (: $prfarg $prms) $thrm))
           (bc $kb $env $idx $k (: $prfabs (-> (: $prfarg $prms) $thrm))))
          ((: $prfarg $prms)
           (bc $kb $env $idx $k (: $prfarg $prms))))
     (: ($prfabs (: $prfarg $prms)) $thrm)))
;; Proof abstraction
(= (bc $kb $env $idx (S $k) (: (λ $idx $prfbdy) (-> (: $prfarg $prms) $thrm)))
   (let (: $prfbdy $thrm)
     (bc $kb (Cons (: $idx $prms) $env) (s $idx) $k (: $prfbdy $thrm))
     (: (λ $idx $prfbdy) (-> (: $prfarg $prms) $thrm))))

;;;;;;;;;;;;;;;
;; Reduction ;;
;;;;;;;;;;;;;;;

;; Reduction rules to simplify proofs and reduce redundancy

;; Identity function
(= ((λ $x $x) $y) $y)

;; Constant function
(= ((λ (s z) z) $x) z)

;; Application function.  This is incorrect because $x is supposed not
;; to contain (s z) and we have no way to guaranty that via pattern
;; matching.  This incorrectness is probably tolerable because such
;; situation may not arise here.
(= ((λ (s z) ((s z) $x)) $y) ($y $x))

;;;;;;;;;;
;; Test ;;
;;;;;;;;;;

;; Partially synthesize the English to Chinese singing AI service
;; composition by filling holes in a provided composition.  The holes
;; are $accomp, $vocals and $spleeter.
;;
;; WARNING: this may takes several hours (or days) and eat up all your
;; memory.  The output is also going to be gigantic, so you may want
;; to redirect it to a file.
!(bc &kb Nil z (fromNumber 3)
     (: (λ z ((λ (s z) ((mixer.mix $accomp) $vocals)) $spleeter))
        (-> (: $s (SongIn "English")) (SongIn "Chinese"))))
