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

;; Define max
(: max (-> $a $a $a))
(= (max $x $y) (if (> $x $y) $x $y))

;; Define min
(: min (-> $a $a $a))
(= (min $x $y) (if (< $x $y) $x $y))

;; Tuple count grounded python function
;;!(extend-py! ../utils/helper.py)
;; This is very slow, use the grounded python function instead
(= (tuple-count $tuple) (if (== $tuple ()) 0 (+ 1 (tuple-count (cdr-atom $tuple)))))

!(bind! &specializations (new-space))
!(bind! &abstractions (new-space))

;; Initialization
(= (init-miner $db $msup $highsurp)
    (superpose ((extract-valuation $db)
                (add-atom &self (= (refdb) $db))
                (add-atom &self (= (msNat) (fromNumber $msup)))
                (add-atom &self (= (highsurp) $highsurp)))))

;; Search for Triplets in DB 
;; collect valuation sets and generate specializations
(= (extract-valuation $db)
   (match $db ($link $x $y) 
    (superpose ((build-specialization ($link $x (VarIdx (S Z))))
                (build-specialization ($link (VarIdx Z) $y))
                (add-abstractions ($link (VarIdx Z) (VarIdx (S Z))))))))

(= (build-specialization $sp)
   (case (match &specializations $sp $sp)
        ((%void% (add-atom &specializations $sp)))))

(= (add-abstractions $aptrn)
   (case (match &abstractions $aptrn $aptrn)
        ((%void% (add-atom &abstractions $aptrn)))))

;; Get unique links
(= (get-links) (match &abstractions ($link $x $y) $link))

;; Get abstract patterns
(= (abstract-patterns) (match &abstractions $aptrn $aptrn))

;; Specialize a pattern
(= (specialize $link) (match &specializations ($link $x $y) ($link $x $y)))

;; all Specializations
(= (all-specialization) (match &specializations $x $x))

;; Todo: implement Debruijn index
;; Define DeBruijn Index
(: DeBruijn Type)
(: VarIdx (-> Nat DeBruijn))
(= (fromDeb $pattern $Xvar $Yvar)
      (case ((get-metatype $pattern) $pattern)
         (((Variable $_) $pattern)
          (($_ $pattern)
            (let ($link $a $b) $pattern
                (case ($link (get-type $a) (get-type $b))
                  ((($link DeBruijn %Undefined%) ($link $Xvar $b))
                   (($link %Undefined% DeBruijn) ($link $a $Yvar))
                   (($link DeBruijn DeBruijn) ($link $Xvar $Yvar))
                   ($_ $pattern)))))
          ($_ $pattern))))

(= (fromDebruijn $ptrn $Xvar $Yvar)
    (case $ptrn (
      ((, $p1 $p2) (, (fromDeb $p1 $Xvar $Yvar) (fromDeb $p2 $Xvar $Yvar)))
      ((, $p1 $p2 $p3) (, (fromDeb $p1 $Xvar $Yvar) (fromDeb $p2 $Xvar $Yvar) (fromDeb $p3 $Xvar $Yvar)))
      ($_ (fromDeb $ptrn $Xvar $Yvar)))))

; Count the number of instances of a given pattern
(= (count $pattern)
    (let $result 
      (case (match &self (= (refdb) $db) $db)
          (($db (let $dptrn (fromDebruijn $pattern $Xvar $Yvar) (collapse (match $db $dptrn $dptrn))))
           (%void% ())))
      (tuple-count $result)))

(= (countNat $pattern) (fromNumber (count $pattern)))

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

!(let* (($mn (msNat))
        ($atom (: ms_Threshold (ms_threshold $mn))))
    (add-atom &kb $atom))

;; Import a lookup file for ⍃
!(import! &kb2 succAxiom)
!(match &kb2 $1 (add-atom &kb $1))

;; add specialization of most abstract patterns
!(let* ((($link $a $b) (abstract-patterns))
        ($aptrn ($link (VarIdx Z) (VarIdx (S Z))))
        ($sptrn (specialize $link))
        ($atom (: (sp_witness $sptrn) (specializationOf $sptrn $aptrn))))
    (add-atom &kb $atom))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Backward chaining Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_) (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Forward chaining Curried  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base case
;; The source is assumed to be true
(: fc (-> $a Nat $a))
(= (fc (: $proof $premise) $_) (: $proof $premise))
;; Recursive steps
(= (fc (: $prfarg $premise) (S $k))
   (let (: $prfabs (-> $premise $ccln)) (bc (: $prfabs (-> $premise $ccln)) $k)
     (fc (: ($prfabs $prfarg) $ccln) $k)))
(= (fc (: $prfabs (-> $prms $ccln)) (S $k))
    (let (: $prfarg $prms) (bc (: $prfarg $prms) $k)
     (fc (: ($prfabs $prfarg) $ccln) $k)))

;;;;;;;;;;;;
;; Helper ;;
;;;;;;;;;;;;

;; Define ad-atom-nodup, that adds an atom only if it is not already
;; in the atomspace
(: add-atom-nodup (-> $st Atom ()))
(= (add-atom-nodup $space $atom)
   (case (match $space $atom $atom)
     (($atom ())
      (%void% (add-atom $space $atom)))))

;; Add all atoms from an expression to a given atomspace
(: add-atoms-nodup (-> $st Expression ()))
(= (add-atoms-nodup $space $atoms)
   (if (== $atoms ())
       ()
       (let* (($head (car-atom $atoms))
              ($tail (cdr-atom $atoms))
              ($dummy (add-atom-nodup $space $head)))
         (add-atoms-nodup $space $tail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterative Chainer Wrapped Around Forward DTL Curried (collapse) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the forward revertant chainer iteratively, adding the
;; conclusions to the knowledge space after each iterations.  To avoid
;; irreproducible behavior (due to the side effects of modifying the
;; atomspace), each call of the forward revertant chainer collapses
;; between iterations.
(: ifc (-> $a                            ; Premise
           Nat                           ; Depth
           Nat                           ; Number of iterations
           $a))                          ; Conclusion

;; Base case.  For now it terminates at exactly iteration Z to avoid
;; collecting too many redundant results.
(= (ifc $prms $depth Z) $prms)

;; Iterative step
(= (ifc $prms $depth (S $k))
   (let* (($cres (collapse (fc $prms $depth)))
          ($dummy (add-atoms-nodup &kb $cres)))
     (ifc (superpose $cres) $depth $k)))


;;;;;;;;;;
;; Test ;;
;;;;;;;;;;
(= (expectedHedras)
 ((: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance B (VarIdx (S Z)))))) ms_Threshold) (-> (⍃ (S Z) (S Z)) (minsup (Inheritance B (VarIdx (S Z))))))
  (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance B (VarIdx (S Z)))))) (-> (ms_threshold $ms-991746) (-> (⍃ $ms-991746 (S Z)) (minsup (Inheritance B (VarIdx (S Z)))))))
  (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance B (VarIdx (S Z))))) (supportOf (Inheritance B (VarIdx (S Z))) (S Z)))
  (: (((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A)))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S Z))))) (minsup (Inheritance (VarIdx Z) A)))
  (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A)))) ms_Threshold) (-> (⍃ (S Z) (S (S Z))) (minsup (Inheritance (VarIdx Z) A))))
  (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A)))) (-> (ms_threshold $ms-1280957) (-> (⍃ $ms-1280957 (S (S Z))) (minsup (Inheritance (VarIdx Z) A)))))
  (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A))) (supportOf (Inheritance (VarIdx Z) A) (S (S Z))))
  (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance D (VarIdx (S Z)))))) ms_Threshold) (-> (⍃ (S Z) (S Z)) (minsup (Inheritance D (VarIdx (S Z))))))
  (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance D (VarIdx (S Z)))))) (-> (ms_threshold $ms-1776441) (-> (⍃ $ms-1776441 (S Z)) (minsup (Inheritance D (VarIdx (S Z)))))))
  (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance D (VarIdx (S Z))))) (supportOf (Inheritance D (VarIdx (S Z))) (S Z)))
  (: (((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E)))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S Z))))) (minsup (Inheritance (VarIdx Z) E)))
  (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E)))) ms_Threshold) (-> (⍃ (S Z) (S (S Z))) (minsup (Inheritance (VarIdx Z) E))))
  (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E)))) (-> (ms_threshold $ms-2065652) (-> (⍃ $ms-2065652 (S (S Z))) (minsup (Inheritance (VarIdx Z) E)))))
  (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E))) (supportOf (Inheritance (VarIdx Z) E) (S (S Z))))
  (: (((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z)))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S Z))))) (minsup (Inheritance C (VarIdx (S Z)))))
  (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z)))))) ms_Threshold) (-> (⍃ (S Z) (S (S Z))) (minsup (Inheritance C (VarIdx (S Z))))))
  (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z)))))) (-> (ms_threshold $ms-2574671) (-> (⍃ $ms-2574671 (S (S Z))) (minsup (Inheritance C (VarIdx (S Z)))))))
  (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z))))) (supportOf (Inheritance C (VarIdx (S Z))) (S (S Z))))
  (: (apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (-> (specializationOf $sptrn-785335 (Inheritance (VarIdx Z) (VarIdx (S Z)))) (supportOf $sptrn-785335 (countNat $sptrn-785335)))), 
  (: (support-cnj (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (-> (minsup $ptrn2-785333) (supportOf (, (Inheritance (VarIdx Z) (VarIdx (S Z))) $ptrn2-785333) (countNat (, (Inheritance (VarIdx Z) (VarIdx (S Z))) $ptrn2-785333))))) 
  (: (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z))))))) (minsup (Inheritance (VarIdx Z) (VarIdx (S Z))))) 
  (: ((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (-> (⍃ (S Z) (S (S (S (S Z))))) (minsup (Inheritance (VarIdx Z) (VarIdx (S Z)))))) 
  (: (minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) (-> (ms_threshold $ms-607716) (-> (⍃ $ms-607716 (S (S (S (S Z))))) (minsup (Inheritance (VarIdx Z) 
																																						   (VarIdx (S Z))))))) 
  (: (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z)))) (supportOf (Inheritance (VarIdx Z) (VarIdx (S Z))) (S (S (S (S Z))))))))

(= (expected) ;; reversed on doulgas rust metta
((: (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z)))) (supportOf (Inheritance (VarIdx Z) (VarIdx (S Z))) (S (S (S (S Z))))))
 (: (minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) (-> (ms_threshold $ms_643543) (-> (⍃ $ms_643543 (S (S (S (S Z))))) (minsup (Inheritance (VarIdx Z) (VarIdx (S Z)))))))
 (: ((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (-> (⍃ (S Z) (S (S (S (S Z))))) (minsup (Inheritance (VarIdx Z) (VarIdx (S Z))))))
 (: (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z))))))) (minsup (Inheritance (VarIdx Z) (VarIdx (S Z)))))
 (: (support-cnj (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (-> (minsup $ptrn2_649976) (supportOf (,(Inheritance (VarIdx Z) (VarIdx (S Z))) $ptrn2_649976) (countNat (, (Inheritance (VarIdx Z) (VarIdx (S Z))) $ptrn2_649976)))))
 (: (apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (-> (specializationOf $sptrn_649978 (Inheritance (VarIdx Z) (VarIdx (S Z)))) (supportOf $sptrn_649978 (countNat $sptrn_649978))))
 (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance B (VarIdx (S Z))))) (supportOf (Inheritance B (VarIdx (S Z))) (S Z)))
 (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance B (VarIdx (S Z)))))) (-> (ms_threshold $ms_674183) (-> (⍃ $ms_674183 (S Z)) (minsup (Inheritance B (VarIdx (S Z)))))))
 (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance B (VarIdx (S Z)))))) ms_Threshold) (-> (⍃ (S Z) (S Z)) (minsup (Inheritance B (VarIdx (S Z))))))
 (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A))) (supportOf (Inheritance (VarIdx Z) A) (S (S Z))))
 (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A)))) (-> (ms_threshold $ms_778237) (-> (⍃ $ms_778237 (S (S Z))) (minsup (Inheritance (VarIdx Z) A)))))
 (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A)))) ms_Threshold) (-> (⍃ (S Z) (S (S Z))) (minsup (Inheritance (VarIdx Z) A))))
 (: (((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) A)))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S Z))))) (minsup (Inheritance (VarIdx Z) A)))
 (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z))))) (supportOf (Inheritance C (VarIdx (S Z))) (S (S Z))))
 (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z)))))) (-> (ms_threshold $ms_1059493) (-> (⍃ $ms_1059493 (S (S Z))) (minsup (Inheritance C (VarIdx (S Z)))))))
 (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z)))))) ms_Threshold) (-> (⍃ (S Z) (S (S Z))) (minsup (Inheritance C (VarIdx (S Z))))))
 (: (((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance C (VarIdx (S Z)))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S Z))))) (minsup (Inheritance C (VarIdx (S Z)))))
 (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E))) (supportOf (Inheritance (VarIdx Z) E) (S (S Z))))
 (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E)))) (-> (ms_threshold $ms_1343428) (-> (⍃ $ms_1343428 (S (S Z))) (minsup (Inheritance (VarIdx Z) E)))))
 (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E)))) ms_Threshold) (-> (⍃ (S Z) (S (S Z))) (minsup (Inheritance (VarIdx Z) E))))
 (: (((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance (VarIdx Z) E)))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S Z))))) (minsup (Inheritance (VarIdx Z) E)))
 (: ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance D (VarIdx (S Z))))) (supportOf (Inheritance D (VarIdx (S Z))) (S Z)))
 (: (minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) (sp_witness (Inheritance D (VarIdx (S Z)))))) (-> (ms_threshold $ms_1624566) (-> (⍃ $ms_1624566 (S Z)) (minsup (Inheritance D (VarIdx (S Z)))))))
 (: ((minsupport ((apriori-rule (((minsupport (supportwitness (Inheritance (VarIdx Z) 
      (VarIdx (S Z))))) ms_Threshold) (WitnessOf (⍃ (S Z) (S (S (S (S Z)))))))) 
				   (sp_witness (Inheritance D (VarIdx (S Z)))))) ms_Threshold)
       (-> (⍃ (S Z) (S Z)) (minsup (Inheritance D (VarIdx (S Z))))))))

(= (eachfc)
  (let* (($aptrn (Inheritance (VarIdx Z) (VarIdx (S Z))))
		 ($cnt   (countNat $aptrn))
		 ($atom  (: (supportwitness $aptrn) (supportOf $aptrn $cnt)))
		 ($depth (fromNumber 8)))
	(fc $atom $depth)))


;; !(eachfc)
!(let $expected (expected)
  (assertEqualToResult (eachfc) $expected))

; !(expected)

!(let $expected (expected)
  (assertEqualToResult
   (let $aptrn (Inheritance (VarIdx Z) (VarIdx (S Z)))
	  (fc (: (supportwitness $aptrn) 
			 (supportOf $aptrn (countNat $aptrn))) (fromNumber 8))) 
	$expected))

