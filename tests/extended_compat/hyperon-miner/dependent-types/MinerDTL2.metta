!(import! &self ../../hyperon-pln/metta/synthesis/Synthesize.metta)
!(import! &self ../utils/MinerUtils.metta)
!(import! &succ succAxiom)

(= (get-witness $x) (: (WitnessOf $x) $x))
(= (succ-axiom) (match &succ $x $x))

(= (specializations-axiom)
    (let ($link $a $b) (match &specializations $x $x)
      (specializationOf ($link $a $b) ($link (VarIdx Z) (VarIdx (S Z))))))

(: -> (-> Atom Atom Atom Type))

(: kb (-> Atom))
(= (kb) (superpose ((get-witness (specializations-axiom))
                    (succ-axiom)
                    (: trueaxiom (true)))))

;; support rule
(= (support-rule)
  (: support
    (-> (true)
        (specializationOf $sptrn $aptrn)
        (supportOf $sptrn (countNat $sptrn)))))

;; Minimum support rule
(= (minsupport-rule)
  (: minsupport
    (-> (supportOf $ptrn $cnt)
        (⍃ $ms $cnt)
        (minsup $ptrn $ms))))

;; Conjunction rule
(= (support-cnj-rule)
  (: support-cnj
    (-> (minsup $ptrn1 $m)
        (minsup $ptrn2 $m)
        (supportOf (, $ptrn1 $ptrn1) (countNat (, $ptrn1 $ptrn1))))))

;; Minimum support transitivity rule
(: ms-transitivity-rule (-> Atom))
(= (ms-transitivity-rule)
    (: ms-transitivity
      (-> (minsup $sptrn $m)
          (specializationOf $sptrn $aptrn)
          (minsup $aptrn $m))))

;; iSurprisingness rule
(: surprisingness-rule (-> Atom))
(= (surprisingness-rule)
    (: surprisingness
      (-> (minsup $ptrn $m)
          (supportOf $ptrn $cnt)
          (surp $ptrn (isurp? ($ptrn $cnt))))))

(: rb (-> Atom))
(= (rb) (superpose ((support-rule)
                    (minsupport-rule)
                    (support-cnj-rule)
                    (ms-transitivity-rule)
                    (surprisingness-rule))))
