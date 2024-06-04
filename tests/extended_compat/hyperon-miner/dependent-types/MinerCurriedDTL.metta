;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Pattern Miner rules ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimum support rule
(: minsupport 
    (-> (supportOf $ptrn $cnt)
        (-> (ms_threshold $ms)
            (-> (⍃ $ms $cnt)
                (minsup $ptrn)))))
;; apriori-rule
(: apriori-rule
    (-> (minsup $aptrn)
        (-> (specializationOf $sptrn $aptrn)
            (supportOf $sptrn (count $sptrn)))))

;; Conjunction rule
(: support-cnj
    (-> (minsup $ptrn1)
        (-> (minsup $ptrn2)
            (-> (isDiff $ptrn1 $ptrn2)
                (supportOf (, $ptrn1 $ptrn2) (count (, $ptrn1 $ptrn2)))))))

;; iSurprisingness rule
(: surprisingness 
    (-> (minsup $ptrn)
        (-> (supportOf $ptrn $cnt)
            (surp $ptrn (isurp? ($ptrn $cnt))))))
