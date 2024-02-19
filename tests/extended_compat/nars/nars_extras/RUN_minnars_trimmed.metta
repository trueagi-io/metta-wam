(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) (nop))
(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

(= (max $1 $2) (if (> $1 $2) $1 $2))
(= (min $1 $2) (if (< $1 $2) $1 $2))

(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))

;; Truth functions
(= (Truth_c2w $c) (/ $c (- 1 $c)))
(= (Truth_w2c $w) (/ $w (+ $w 1)))
(= (Truth_Deduction ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))
(= (Truth_Abduction ($f1 $c1) ($f2 $c2)) ($f2 (Truth_w2c (* (* $f1 $c1) $c2))))
(= (Truth_Induction $T1 $T2) (Truth_Abduction $T2 $T1))
(= (Truth_Revision ($f1 $c1) ($f2 $c2))
   (let* (($w1 (Truth_c2w $c1)) ($w2 (Truth_c2w $c2)) ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w)) ($c (Truth_w2c $w)))
          ((min 1.00 $f) (min 0.99 (max (max $c $c1) $c2)))))

;; Inference rules
; NAL-1/5 (selected subset)
(= (|- ($T $T1) ($T $T2)) ($T (Truth_Revision $T1 $T2)))
(= (|- (($a --> $b) $T1) (($b --> $c) $T2)) (($a --> $c) (Truth_Deduction $T1 $T2)))
(= (|- (($a --> $b) $T1) (($a --> $c) $T2)) (($c --> $b) (Truth_Induction $T1 $T2)))
(= (|- (($a --> $c) $T1) (($b --> $c) $T2)) (($b --> $a) (Truth_Abduction $T1 $T2)))
(= (|- (($a ==> $b) $T1) (($b ==> $c) $T2)) (($a ==> $c) (Truth_Deduction $T1 $T2)))
(= (|- (($a ==> $b) $T1) (($a ==> $c) $T2)) (($c ==> $b) (Truth_Induction $T1 $T2)))
(= (|- (($a ==> $c) $T1) (($b ==> $c) $T2)) (($b ==> $a) (Truth_Abduction $T1 $T2)))
(= (|- ($A $T1) (($A ==> $B) $T2)) ($B (Truth_Deduction $T1 $T2)))
(= (|- ($A $T1) ((($A && $B) ==> $C) $T2)) (($B ==> $C) (Truth_Deduction $T1 $T2)))
(= (|- ($B $T1) (($A ==> $B) $T2)) ($A (Truth_Abduction $T1 $T2)))
; NAL-4 (selected subset)
(= (|- ((($A x $B) --> $R) $T1) ((($C x $B) --> $R) $T2)) (($C --> $A) (Truth_Abduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) ((($A x $C) --> $R) $T2)) (($C --> $B) (Truth_Abduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($C x $B)) $T2)) (($C --> $A) (Truth_Induction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($A x $C)) $T2)) (($C --> $B) (Truth_Induction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C --> $A) $T2)) ((($C x $B) --> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($A --> $C) $T2)) ((($C x $B) --> $R) (Truth_Induction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C --> $B) $T2)) ((($A x $C) --> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($B --> $C) $T2)) ((($A x $C) --> $R) (Truth_Induction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($A --> $C) $T2)) (($R --> ($C x $B)) (Truth_Deduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C --> $A) $T2)) (($R --> ($C x $B)) (Truth_Abduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($B --> $C) $T2)) (($R --> ($A x $C)) (Truth_Deduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C --> $B) $T2)) (($R --> ($A x $C)) (Truth_Abduction $T1 $T2)))

;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True overlap)))))))

;; Exhaustive-until-depth deriver
(= (Derive $beliefs $depth $maxdepth)
   (If (> $depth $maxdepth)
       $beliefs
       (let $derivations
            (collapse (let* (((Sentence $x $Ev1) (superpose $beliefs))
                             ((Sentence $y $Ev2) (superpose $beliefs))
                             ($stamp (TupleConcat $Ev1 $Ev2)))
                            (If (StampDisjoint $Ev1 $Ev2)
                                (case (|- $x $y) ((($T $TV) (Sentence ($T $TV) $stamp)))))))
            (Derive (TupleConcat $beliefs $derivations) (+ $depth 1) $maxdepth))))

;retrieve the best candidate
(= (BestCandidate $evaluateCandidateFunction $bestCandidate $tuple)
  (If (== $tuple ())
      $bestCandidate
      (let* (($head (car-atom $tuple))
             ($tail (cdr-atom $tuple)))
            (If (> ($evaluateCandidateFunction $head)
                   ($evaluateCandidateFunction $bestCandidate))
                (BestCandidate $evaluateCandidateFunction $head $tail)
                (BestCandidate $evaluateCandidateFunction $bestCandidate $tail)))))

;candidate evaluation based on confidence
(= (ConfidenceRank (($f $c) $Ev)) $c)
(= (ConfidenceRank ()) 0)

;pose a question of a certain term to the system on some knowledge base
(= (Question $kb $term $steps)
   (BestCandidate ConfidenceRank () (collapse (let $x (Derive $kb 1 $steps)
                                                   (case (superpose $x)
                                                         (((Sentence ($T $TV) $Ev) (case (== $T $term)
                                                                                          ((True ($TV $Ev)))))))))))

(flonum-print-precision 17)

;<EVALUATIONS>

!(assertEqualToResult
  (Question ((Sentence (((ExtSet garfield) --> cat) (1.0 0.9)) (1))
                (Sentence (((cat x sky) --> like) (1.0 0.9)) (2))
                (Sentence ((sky --> (IntSet blue)) (1.0 0.9)) (3))
                (Sentence (((((ExtSet garfield) x (IntSet blue)) --> like) ==> ((ExtSet garfield) --> artist)) (1.0 0.9)) (4))
          )
          ((ExtSet garfield) --> artist)
          3)

 (((1.0 0.37946790052053214) (2 1 3 4))))
