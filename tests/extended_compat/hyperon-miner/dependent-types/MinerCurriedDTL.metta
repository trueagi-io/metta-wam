; !(import! &self ../utils/MinerUtils.metta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge and rule base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test
; !(import! &db ../data/sample.metta)
!(bind! &db (new-space))
!(add-atom &db (Inheritance B A))
!(add-atom &db (Inheritance C A))
!(add-atom &db (Inheritance D E))
!(add-atom &db (Inheritance C E))

;; Parameters
(= (highsurp) 0.8)
(= (ms) 1)
!(init-miner &db (ms) (highsurp))

!(bind! &kb (new-space))

(: -> (-> Atom Atom Type))

!(add-atom &kb (: ms_Threshold (ms_threshold (msNat))))

;; Import a lookup file for ⍃
!(import! &kb succAxiom.metta)

;; or apply the following rule
; !(add-atom &kb (: ZeroLTSucc (⍃ Z (S $k))))
;; If x ⍃ y then (S x) ⍃ (S y)
; !(add-atom &kb (: SuccMonotonicity 
;                 (-> (⍃ $x $y)
;                     (⍃ (S $x) (S $y)))))

;; add specialization of most abstract patterns
!(let* ((($link $a $b) (abstract-patterns))
        ($aptrn ($link (VarIdx Z) (VarIdx (S Z)))))
    (superpose ((add-atom &kb (: (sp_witness $sptrn) (specializationOf (specialize $link) $aptrn))))))

;; Minimum support rule
!(add-atom &kb (: minsupport 
                (-> (supportOf $ptrn $cnt)
                    (-> (ms_threshold $ms)
                        (-> (⍃ $ms $cnt)
                            (minsup $ptrn))))))
;; apriori-rule
!(add-atom &kb (: apriori-rule
                (-> (minsup $aptrn)
                    (-> (specializationOf $sptrn $aptrn)
                        (supportOf $sptrn (countNat $sptrn))))))

;; Conjunction rule
;; TODO: add a premise that $ptrn1 and $ptrn2 are different
!(add-atom &kb (: support-cnj
                (-> (minsup $ptrn1)
                    (-> (minsup $ptrn2)
                        (supportOf (, $ptrn1 $ptrn2) (countNat (, $ptrn1 $ptrn2)))))))

;; iSurprisingness rule
!(add-atom &kb (: surprisingness 
                (-> (minsup $ptrn)
                    (-> (supportOf $ptrn $cnt)
                        (surp $ptrn (isurp? ($ptrn $cnt)))))))
