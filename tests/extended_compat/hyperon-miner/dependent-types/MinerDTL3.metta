!(import! &self ../../hyperon-pln/metta/synthesis/Synthesize.metta)
!(import! &self ../utils/MinerUtils.metta)
!(import! &succ succAxiom)

(= (get-witness $x) (: (WitnessOf $x) $x))
(= (specializations) (match &specializations $x $x))
(= (cnj-specializations)
   (superpose ((, (specializations) (specializations))
               (, (specializations) (specializations) (specializations)))))

(= (specializations-axiom)
   (let ($link $a $b) (specializations)
    (specializationOf ($link $a $b) ($link (VarIdx Z) (VarIdx (S Z))))))

(= (support-axiom)
   (superpose ((let $sp (specializations) (supportOf $sp (countNat $sp)))
               (let $cnj (cnj-specializations) (supportOf $cnj (countNat $cnj))))))

(= (succ-axiom) (match &succ $x $x))

(: kb (-> Atom))
(= (kb) (superpose ((get-witness (specializations-axiom))
                    (get-witness (support-axiom))
                    (zero-lt-succ-axiom)
                    (succ-axiom))))

;; Minimum support rule
(= (minsupport-rule)
  (: minsupport
    (-> (supportOf $ptrn $cnt)
        (⍃ (ms) $cnt)
        (minsup $ptrn (ms)))))

;; Minimum support transitivity rule
(: ms-transitivity-rule (-> Atom))
(= (ms-transitivity-rule)
    (: ms-transitivity
      (-> (minsup $sptrn $ms)
          (specializationOf $sptrn $aptrn)
          (minsup $aptrn $ms))))

;; iSurprisingness rule
(: surprisingness-rule (-> Atom))
(= (surprisingness-rule)
    (: surprisingness
      (-> (minsup $ptrn $ms)
          (supportOf $ptrn $cnt)
          (surp $ptrn (isurp? ($ptrn $cnt))))))

(: rb (-> Atom))
(= (rb) (superpose ((minsupport-rule)
                    (ms-transitivity-rule)
                    (surprisingness-rule)
                    (succ-monotonicity-rule))))
