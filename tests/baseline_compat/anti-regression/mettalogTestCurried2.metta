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

(= (tuple-count $tuple) (if (== $tuple ()) 0 (+ 1 (tuple-count (cdr-atom $tuple)))))

; Count the number of instances of a given pattern
(= (count $pattern)
    (let $result 
      (case (match &self (= (refdb) $db) $db)
          (($db (let $dptrn (fromDebruijn $pattern $Xvar $Yvar) (collapse (match $db $dptrn $dptrn))))
           (%void% ())))
      (tuple-count $result)))

(= (countNat $pattern) (fromNumber (count $pattern)))

!(bind! &kb (new-space))
(: -> (-> Atom Atom Type))

;; KB
!(let $sptrn (: SP (specializationOf (Inheritance B (VarIdx (S Z))) (Inheritance (VarIdx Z) (VarIdx (S Z)))))
    (add-atom &kb $sptrn))

;; apriori-rule
!(add-atom &kb (: apriori-rule
                (-> (minsup $aptrn)
                    (-> (specializationOf $sptrn $aptrn)
                        (supportOf $sptrn (countNat $sptrn))))))

;; Test
; !(import! &db ../data/sample.metta)
!(bind! &db (new-space))
!(add-atom &db (Inheritance B A))
!(add-atom &db (Inheritance C A))
!(add-atom &db (Inheritance D E))
!(add-atom &db (Inheritance C E))

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

;; Test
(= (refdb) &db)
!(let (: $prf $concl) 
    (let* (($aptrn (Inheritance (VarIdx Z) (VarIdx (S Z))))
            ($atom (: MP (minsup $aptrn)))
            ($depth (fromNumber 2)))
        (fc $atom $depth))
(: PROOFTrace $concl))
