!(import! &self ../../hyperon-pln/metta/synthesis/Synthesize)

!(bind! &db (new-space))
!(add-atom &db (Inheritance Allen man))
!(add-atom &db (Inheritance Allen human))
!(add-atom &db (Inheritance man mortal))

(= (refdb) &db)
(= (get-witness $x) (: (WitnessOf $x) $x))
(= (sourcedb) (match &db $x (axiom $x)))

(= (tuple-count $tuple) (if (== $tuple ()) 0 (+ 1 (tuple-count (cdr-atom $tuple)))))

(= (fromDeb $pattern)
      (case ((get-metatype $pattern) $pattern)
         (((Variable $_) $pattern)
          (($_ ($a (VarIdx Z) $b)) ($a $Xvar $b))
          (($_ ($a $b (VarIdx (S Z)))) ($a $b $Yvar))
          ($_ $pattern)
          )))

(= (count $pattern)
    (case (match &self (= (refdb) $db) $db)
        (($db (let $dptrn (fromDeb $pattern) (tuple-count (collapse (match $db $dptrn $dptrn)))))
         (%void% 0))))

; Axiom:
(: kb (-> Atom))
(= (kb) (superpose ((get-witness (sourcedb)))))

;; Specialization Rule
(= (specialize-ruleX)
  (: specilizeX
    (-> (axiom ($link $x $y))
        (SpecializationOf ($link (VarIdx Z) $y) ($link (VarIdx Z) (VarIdx (S Z)))))))

(= (specialize-ruleY)
  (: specilizeY
    (-> (axiom ($link $x $y))
        (SpecializationOf ($link $x (VarIdx (S Z))) ($link (VarIdx Z) (VarIdx (S Z)))))))

(: -> (-> Atom Atom Type))

;; support rule
(= (support-rule)
  (: support
    (-> (SpecializationOf $sptrn $aptrn)
        (SupportOf $sptrn (count $sptrn)))))

(: rb (-> Atom))
(= (rb) (superpose ((specialize-ruleX)
                    (specialize-ruleY)
                    (support-rule)
)))

!(synthesize (: $proof $something) kb rb (fromNumber 2))
