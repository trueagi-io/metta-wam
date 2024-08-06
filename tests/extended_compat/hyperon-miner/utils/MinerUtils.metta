; !(register-module! ../../hyperon-miner)
!(import! &self hyperon-miner:utils:DeBruijnIndex)

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
                (add-atom &self (= (highsurp) $highsurp))
                (get-universe-size $db))))

(= (init-miner $db $kb $msup $highsurp)
    (superpose ((extract-valuation $db $kb)
                (add-atom &self (= (refdb) $db))
                (add-atom $kb (: msT (ms_threshold $msup)))
                (add-atom $kb (: hsT (hs_threshold $highsurp)))
                (get-universe-size $db))))

;; Search for Triplets in DB 
;; collect valuation sets and generate specializations
(= (extract-valuation $db)
   ;(match $db (: $p ($link $x $y)) ;; To ignore type definition in bio-as
   (match $db ($link $x $y) 
    (superpose ((build-specialization ($link $x (VarIdx (S Z))))
                (build-specialization ($link (VarIdx Z) $y))
                (add-abstractions ($link (VarIdx Z) (VarIdx (S Z))))))))

(= (extract-valuation $db $kb)
   ;(match $db (: $p ($link $x $y)) ;; To ignore type definition in bio-as
   (match $db ($link $x $y) 
    (superpose ((build-specialization ($link $x (VarIdx (S Z))) $kb)
                (build-specialization ($link (VarIdx Z) $y) $kb)
                (add-abstractions ($link (VarIdx Z) (VarIdx (S Z))))))))

(= (build-specialization $sp)
   (case (match &specializations (specializationOf $sp $aptrn) $sp)
        ((%void% (let* ((($link $x $y) $sp)
                        ($aptrn ($link (VarIdx Z) (VarIdx (S Z)))))
                    (add-atom &specializations (specializationOf $sp $aptrn)))))))

(= (build-specialization $sp $kb)
   (case (match $kb (: $wt (specializationOf $sp $aptrn)) $sp)
        ((%void% (let* ((($link $x $y) $sp)
                        ($aptrn ($link (VarIdx Z) (VarIdx (S Z)))))
                    (add-atom $kb (: (sp_witness $sp) (specializationOf $sp $aptrn))))))))

(= (add-abstractions $aptrn)
   (case (match &abstractions $aptrn $aptrn)
        ((%void% (add-atom &abstractions $aptrn)))))

;; Get unique links
(= (get-links) (match &abstractions ($link $x $y) $link))

;; Get abstract patterns
(= (abstract-patterns) (match &abstractions $aptrn $aptrn))
(= (abstract-patterns $link) ($link (VarIdx Z) (VarIdx (S Z))))

;; Specialize a pattern
(= (specialize $link) 
    (let $aptrn ($link (VarIdx Z) (VarIdx (S Z))) 
     (match &specializations (specializationOf $sp $aptrn) $sp)))

;; all Specializations
(= (all-specialization) (match &specializations $x $x))

; Count the number of instances of a given pattern
(= (count $pattern)
    (let* (($dptrn (Deb2var $pattern (Cons $Xvar (Cons $Yvar Nil))))
           ($result (collapse (match (refdb) $dptrn $dptrn))))
        (tuple-count $result)))

(= (countNat $pattern) (fromNumber (count $pattern)))

;; Return candidate patterns in the format (candidate pattern count) 
;; for patterns with a support value >= the minimum support.
;; the count here will later be used for calculating surprisingness.
(= (support $pattern $minsup)
    (let $cnt (count $pattern)
      (if (> (+ $cnt 1) $minsup) (candidate $pattern $cnt) (superpose ()))))

;; Extract a pattern from a Candidate pattern of form
;; (candidate pattern count)
(= (get-pattern $cnpattern)
  (let (candidate $pattern $cnt) $cnpattern $pattern))

;; flatten combination
;; TODO: apply reduction rule
(= (flattenPattern $pattern)
    (case $pattern 
        (((, (, $a $b) $c) (, $a $b $c))
         ((, $a (, $b $c)) (, $a $b $c))
         ($_ $pattern))))

;; Get universe size of a given db
(= (get-universe-size $dbspace) 
    (add-atom &self 
      (= (universe-size) (let $u (collapse (match $dbspace $x 1)) (tuple-count $u)))))

;; Probability formula
(= (prob $count) (/ $count (universe-size)))

;; iSurprisingness formula
(= (iSurprisingness $pattern)
    (case $pattern (
      ((candidate (, $p1 $p2) $cnt)
      (let* (($pp1 (prob (count $p1)))
          ($pp2 (prob (count $p2)))
          ($p1p2 (* $pp1 $pp2)) ;; probability estimate 
          ($p (prob $cnt))) ;; empirical
          (/ (max (- $p $p1p2) (- $p1p2 $p)) $p)))
      ((candidate (, $p1 $p2 $p3) $cnt)
      (let* (($pp1 (prob (count $p1)))
          ($pp2 (prob (count $p2)))
          ($pp3 (prob (count $p3)))
          ($pp1p2 (prob (count (, $p1 $p2))))
          ($pp1p3 (prob (count (, $p1 $p3))))
          ($pp2p3 (prob (count (, $p2 $p3))))
          ;; probability estimate
          ($maxP (max (* $pp1p2 $pp3) (max (* $pp1p3 $pp2) (max (* $pp2p3 $pp1) (* $pp1 (* $pp2 $pp3))))))
          ($minP (min (* $pp1p2 $pp3) (min (* $pp1p3 $pp2) (min (* $pp2p3 $pp1) (* $pp1 (* $pp2 $pp3))))))
          ($p (prob $cnt))) ;; empirical
          (/ (max (- $p $maxP) (- $minP $p)) $p)))
      ($_ 0))))

(= (isurp? ($ptrn $cnt)) (if (> (iSurprisingness (candidate $ptrn $cnt)) (highsurp)) true false))
