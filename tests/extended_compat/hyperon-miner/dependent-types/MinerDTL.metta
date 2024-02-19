!(import! &self ../../hyperon-pln/metta/synthesis/Synthesize.metta)
; !(import! &self ../utils/MinerUtils.metta)
!(import! &succ succAxiom.metta)

(= (succ-axiom) (match &succ $x $x))

(= (abstractions) 
   (let* ((($link $a $b) (abstract-patterns))
          ($aptrn ($link (VarIdx Z) (VarIdx (S Z)))))
    (superpose ((: (sp_witness $sptrn) (specializationOf (specialize $link) $aptrn))
                (: (ap_support) (supportOf $aptrn (countNat $aptrn)))))))

(: -> (-> Atom Atom Atom Atom Type))

(: kb (-> Atom))
(= (kb) (superpose ((abstractions)
                    (succ-axiom)
                    (: ms_Threshold (ms_threshold (msNat)))
                    (: trueAxiom (true)))))

;; Minimum support rule
(= (minsupport-rule)
  (: minsupport 
    (-> (supportOf $ptrn $cnt)
        (ms_threshold $ms)
        (⍃ $ms $cnt)
        (minsup $ptrn))))

;; apriori-rule
(= (apriori-rule)
  (: apriori-rule
    (-> (true)
        (specializationOf $sptrn $aptrn)
        (minsup $aptrn)
        (supportOf $sptrn (countNat $sptrn)))))

;; Conjunction rule
(= (support-cnj-rule)
  (: support-cnj
    (-> (true)
        (minsup $ptrn1)
        (minsup $ptrn2)
        (supportOf (, $ptrn1 $ptrn2) (countNat (, $ptrn1 $ptrn2))))))

;; iSurprisingness rule
(: surprisingness-rule (-> Atom))
(= (surprisingness-rule) 
    (: surprisingness 
      (-> (true)
          (minsup $ptrn)
          (supportOf $ptrn $cnt)
          (surp $ptrn (isurp? ($ptrn $cnt))))))

(: rb (-> Atom))
(= (rb) (superpose ((minsupport-rule)
                    (apriori-rule)
                    (support-cnj-rule)
                    (surprisingness-rule))))
