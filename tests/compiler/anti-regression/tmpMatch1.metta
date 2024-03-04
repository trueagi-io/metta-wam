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

(= (init-miner $db $msup $highsurp $target-link)
    (superpose ((extract-valuation $db $target-link)
                (add-atom &self (= (refdb) $db))
                (add-atom &self (= (msNat) (fromNumber $msup)))
                (add-atom &self (= (highsurp) $highsurp))
                (get-universe-size $db))))

;; Search for Triplets in DB 
;; collect valuation sets and generate specializations
(= (extract-valuation $db)
   (match $db ($link $x $y) 
    (superpose ((build-specialization ($link $x (VarIdx (S Z))))
                (build-specialization ($link (VarIdx Z) $y))
                (add-abstractions ($link (VarIdx Z) (VarIdx (S Z))))))))

;; Given target link
(= (extract-valuation $db $target-link)
   (match $db ($target-link $x $y)
    (superpose ((build-specialization ($target-link $x (VarIdx (S Z))))
                (build-specialization ($target-link (VarIdx Z) $y))))))

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
                  ((($link DeBruijn DeBruijn) ($link $Xvar $Yvar))
    			   (($link DeBruijn $_) ($link $Xvar $b))
				   (($link $_ DeBruijn) ($link $a $Yvar))

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
(= (flatten $pattern)
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
;; MeTTa port of pattern miner, using match.

;; Given a collection of data trees db, a minimum support ms and an
;; initial collection of patterns Candidate (containing at least the identity
;; pattern), the pattern mining algorithm works as follows

;; 1. Select a pattern P from C
;; 2. Extract the valuation set of P over db, called V
;; 3. Determine the shallow abstractions of V, called A
;; 4. Specialize P by composing it with elements in A
;; 5. Add the resulting specializations with enough support in C,
;;    discard the others
;; 6. Repeat till termination

;; Import utils
; !(import! &self ../utils/MinerUtils.metta)

;; Get candidate patterns depth zero
(= (get-candidate Z $linkType $ms)
    (let $sp (specialize $linkType)
      (support $sp $ms)))

;; Get candidate patterns depth n. (maximum is three)
(= (get-candidate (S $k) $linkType $ms)
    (let $comb (, (get-pattern (get-candidate $k $linkType $ms))
                  (get-pattern (get-candidate Z $linkType $ms)))
      (support (flatten $comb) $ms)))

;; Frequent patterns Miner
(= (miner $db $ms $depth)
    (superpose ((init-miner $db $ms $highsurp)
                (let $link (get-links)
                  (get-candidate $depth $link $ms)))))

;; Surprising patterns miner
(= (miner-surprising $db $ms $depth $highsurp)
  (let* (($cptrn (miner $db $ms $depth))
         ($isurp (iSurprisingness $cptrn)))
         (if (> $isurp $highsurp) (surp (get-pattern $cptrn) $isurp) (superpose ()))))
; !(import! &self MinerMatch.metta)

;; Load sample data
; !(import! &db ../data/sample.metta)
!(bind! &db (new-space))
!(add-atom &db (Inheritance B A))
!(add-atom &db (Inheritance C A))
!(add-atom &db (Inheritance D E))
!(add-atom &db (Inheritance C E))

;; Parameters
(= (ms) 1)
;; Max depth (number of conjuncts) is 3
;; it starts from 0, 
(= (depth) (fromNumber 0))
; (= (depth) (fromNumber 1))
; (= (depth) (fromNumber 2))
(= (highsurp) 0.8)

; !(miner &db (ms) (depth))

;; frequent patterns 
!(assertEqualToResult 
  (miner &db (ms) (depth))
  (() () () () () () () () () ()
   (candidate (Inheritance (VarIdx Z) E) 2)
   (candidate (Inheritance (VarIdx Z) A) 2)
   (candidate (Inheritance B (VarIdx (S Z))) 1)
   (candidate (Inheritance C (VarIdx (S Z))) 2)
   (candidate (Inheritance D (VarIdx (S Z))) 1)))