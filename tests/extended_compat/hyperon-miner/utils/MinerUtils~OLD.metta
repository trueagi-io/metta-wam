;; Import Num
; !(import! &self ../common/Num.metta)
;; Define max
(: max (-> $a $a $a))
(= (max $x $y) (if (> $x $y) $x $y))

;; Define min
(: min (-> $a $a $a))
(= (min $x $y) (if (< $x $y) $x $y))

;; Tuple count grounded python function
!(extend-py! ../utils/helper.py)
;; This is very slow, use the grounded python function instead
(= (tuple-count $tuple) (if (== $tuple ()) 0 (+ 1 (tuple-count (cdr-atom $tuple)))))

;; 
(= (new-named-space $name)
   (let $space (new-space)
        (add-atom &self (= (named-space $name) $space))))
!(new-named-space Xspace)
!(new-named-space Yspace)
!(new-named-space Linkspace)

;; Gathers unique valuation sets on a separate space. For instance, 
;; given the input (Inheritance Allen man):
;;    Linkspace contains Inheritance
;;    Xspace contains (Inheritance Allen)
;;    Yspace contains (Inheritance man)
;; The X and Y values are linked to their respective link types to avoid 
;; creating patterns (during specialization) that do not exist in the space.
(= (add-valuation $link $x $y)
   (case (match (named-space Xspace) $x $x)
        ((%void% (add-atom (named-space Xspace) ($link $x))))))
(= (add-valuation $link $x $y)
   (case (match (named-space Yspace) $y $y)
        ((%void% (add-atom (named-space Yspace) ($link $y))))))
(= (add-valuation $link $x $y)
   (case (match (named-space Linkspace) $link $link)
        ((%void% (add-atom (named-space Linkspace) $link)))))

;; Extract the valuation sets of a given db
(= (extract-valuation $dbspace) 
  (match $dbspace ($link $X $Y) (add-valuation $link $X $Y)))

;; Get unique link typess
(= (get-links) (match (named-space Linkspace) $link $link))

;; Specialize a pattern
(= (specialize $link $Xvar $Yvar)
   (case (match (named-space Yspace) ($link $y) $y)
    (($r ($link $Xvar $r))
     (%void%  (superpose ())))))
(= (specialize $link $Xvar $Yvar)
   (case (match (named-space Xspace) ($link $x) $x)
    (($r ($link $r $Yvar))
     (%void%  (superpose ())))))

;; Ignore non-strongly connected components
;; e.g (, (Inheritance $Xvar "Concept man") (Inheritance "Concept Abe" $Yvar))
;; is not strongly connected
(= (get-var ($link $a $b) $Xvar $Yvar)
    (if (== $link ,)  (superpose ()) 1))
(= (get-var (, ($link $a $b) ($link $c $d)) $Xvar $Yvar)
(if (and (== $a $c) (== $b $d)) None 
    (if (and (== $a $Xvar) (== $c $Xvar)) $Xvar 
        (if (and (== $b $Yvar) (== $d $Yvar)) $Yvar None))))
(= (get-var (, ($link $a $b) ($link $c $d) ($link $e $f)) $Xvar $Yvar)
(if (or (and (== $a $e) (== $b $f)) (and (== $c $e) (== $d $f))) None
(let* (($p1p3 (get-var (, ($link $a $b) ($link $e $f)) $Xvar $Yvar))
       ($p2p3 (get-var (, ($link $c $d) ($link $e $f)) $Xvar $Yvar)))
    (if (or (== $p1p3 None) (== $p2p3 None)) None $p1p3))))

;; Count the number of instances of a given pattern
(= (count $pattern $dbspace $Xvar $Yvar)
    (let $result 
      (let $var (get-var $pattern $Xvar $Yvar) 
        (if (== $var None) () (collapse (match $dbspace $pattern $var))))
      (tuple-count $result)))

;; Return candidate patterns in the format (candidate pattern, count) 
;; for patterns with a support value >= the minimum support.
;; the count here will later be used for calculating surprisingness.
(= (support $pattern $dbspace $minsup $Xvar $Yvar)
    (let $cnt (count $pattern $dbspace $Xvar $Yvar)
      (if (> (+ $cnt 1) $minsup) (candidate $pattern $cnt)  (superpose ()))))

;; Extract a pattern from a Candidate pattern of form
;; (candidate pattern count)
(= (get-pattern $cnpattern)
  (let (candidate $pattern $cnt) $cnpattern $pattern))

;; flatten combination
;; TODO: apply reduction rule
(= (, (, $a $b) $c) (, $a $b $c))

;; Get universe size of a given db
(= (get-universe-size $dbspace) 
    (add-atom &self 
      (= (universe-size) 
        (let $u (collapse (match $dbspace $x 1)) (tuple-count $u)))))

;; Probability formula
(= (prob $count) 
  (/ $count (universe-size)))

;; iSurprisingness formula
(= (iSurprisingness $pattern $dbspace)
    (case $pattern (
      ((candidate (, $p1 $p2) $cnt)
      (let* (($pp1 (prob (count $p1 $dbspace $Xvar $Yvar)))
          ($pp2 (prob (count $p2 $dbspace $Xvar $Yvar)))
          ($p1p2 (* $pp1 $pp2)) ;; probability estimate 
          ($p (prob $cnt))) ;; empirical
          (/ (max (- $p $p1p2) (- $p1p2 $p)) $p)))
      ((candidate (, $p1 $p2 $p3) $cnt)
      (let* (($pp1 (prob (count $p1 $dbspace $Xvar $Yvar)))
          ($pp2 (prob (count $p2 $dbspace $Xvar $Yvar)))
          ($pp3 (prob (count $p3 $dbspace $Xvar $Yvar)))
          ($pp1p2 (prob (count (, $p1 $p2) $dbspace $Xvar $Yvar)))
          ($pp1p3 (prob (count (, $p1 $p3) $dbspace $Xvar $Yvar)))
          ($pp2p3 (prob (count (, $p2 $p3) $dbspace $Xvar $Yvar)))
          ;; probability estimate
          ($maxP (max (* $pp1p2 $pp3) (max (* $pp1p3 $pp2) (max (* $pp2p3 $pp1) (* $pp1 (* $pp2 $pp3))))))
          ($minP (min (* $pp1p2 $pp3) (min (* $pp1p3 $pp2) (min (* $pp2p3 $pp1) (* $pp1 (* $pp2 $pp3))))))
          ($p (prob $cnt))) ;; empirical
          (/ (max (- $p $maxP) (- $minP $p)) $p)))
      ($_ 0))))
