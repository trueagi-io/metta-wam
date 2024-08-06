!(pragma! compile full)

;!(nop (pragma! load silent))
;spaces
!(bind! &concepts (new-space))
!(bind! &attentional_focus (new-space))

;spaces
!(bind! &belief_events (new-space))
!(bind! &goal_events (new-space))
!(bind! &currentTime (new-state 1))
!(bind! &evidentialBase (new-state 1))

;; stdlib extension
(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) ())
(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))
(= (TupleConcat $Ev1 $Ev2) (collapse (superpose ((superpose $Ev1) (superpose $Ev2)))))
(= (max $1 $2) (If (> $1 $2) $1 $2))
(= (min $1 $2) (If (< $1 $2) $1 $2))
(= (abs $x) (If (< $x 0) (- 0 $x) $x))
(: sequential (-> Expression %Undefined% ))
(= (sequential $1) (superpose $1))
(: do (-> Expression %Undefined% ))
(= (do $1) (case $1 ()))
;(= (TupleCount ()) 0)
;(= (TupleCount (1)) 1)
;(= (BuildTupleCounts $TOld $C $N)
;   (let $T (collapse (superpose (1 (superpose $TOld))))
;        (superpose ((add-atom &self (= (TupleCount $T) (+ $C 2)))
;                    (If (< $C $N) (BuildTupleCounts $T (+ $C 1) $N))))))*/
; (= (TupleCount $list) (length! $list))
(= (TupleCount $tuple) (if (== $tuple ()) 0 (+ 1 (TupleCount (cdr-atom $tuple)))))

(: CountElement (-> Expression Number))
(= (CountElement $x) (case $x (($y 1))))
;; Useful interop functions for querying when MeTTa-NARS is compiled as a library to be used via Mettamorph
(= (add-morph-atom $space $entry) (add-atom $space $entry))
(= (get-morph-atoms $space) (get-atoms $space))
(= (get-concept-terms) (match (superpose (&attentional_focus &concepts)) (Concept $TermX $Belief $BeliefEvent $CPrio) $TermX))
(= (get-concept-beliefs) (match (superpose (&attentional_focus &concepts)) (Concept $TermX $Belief $BeliefEvent $CPrio) $Belief))
(= (get-concept-terms-of-positive-beliefs) (match (superpose (&attentional_focus &concepts)) (Concept $TermX (Event ($term ($f $c)) ($occT $Ev $Prio)) $BeliefEvent $CPrio) 
   (If (> $f 0.5) ($term (confidence: $c)))))
(= (get-concept-terms-of-positive-temporal-beliefs) (match (superpose (&attentional_focus &concepts)) (Concept $TermX $EternalEvent (Event ($term ($f $c)) ($occT $Ev $Prio)) $CPrio) 
   (If (> $f 0.5) ($term (time: $occT) (confidence: $c)))))
;; Truth functions
(= (Truth_c2w $c) (/ $c (- 1 $c)))
(= (Truth_w2c $w) (/ $w (+ $w 1)))
(= (Truth_Deduction ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))
(= (Truth_Abduction ($f1 $c1) ($f2 $c2)) ($f2 (Truth_w2c (* (* $f1 $c1) $c2))))
(= (Truth_Induction $T1 $T2) (Truth_Abduction $T2 $T1))
(= (Truth_Exemplification ($f1 $c1) ($f2 $c2)) (1.0 (Truth_w2c (* (* $f1 $f2) (* $c1 $c2)))))
(= (Truth_StructuralDeduction $T) (Truth_Deduction $T (1.0 0.9)))
(= (Truth_Negation ($f $c)) ((- 1 $f) $c))
(= (Truth_StructuralDeductionNegated $T) (Truth_Negation (Truth_StructuralDeduction $T)))
(= (Truth_Intersection ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* $c1 $c2)))
(= (Truth_StructuralIntersection $T) (Truth_Intersection $T (1.0 0.9)))
(= (Truth_or $a $b) (- 1 (* (- 1 $a) (- 1 $b))))
(= (Truth_Comparison ($f1 $c1) ($f2 $c2)) (let $f0 (Truth_or $f1 $f2) ((If (== $f0 0.0) 0.0 (/ (* $f1 $f2) $f0)) (Truth_w2c (* $f0 (* $c1 $c2))))))
(= (Truth_Analogy ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* (* $c1 $c2) $f2)))
(= (Truth_Resemblance ($f1 $c1) ($f2 $c2)) ((* $f1 $f2) (* (* $c1 $c2) (Truth_or $f1 $f2))))
(= (Truth_Union ($f1 $c1) ($f2 $c2)) ((Truth_or $f1 $f2) (* $c1 $c2)))
(= (Truth_Difference ($f1 $c1) ($f2 $c2)) ((* $f1 (- 1 $f2)) (* $c1 $c2)))
(= (Truth_DecomposePNN ($f1 $c1) ($f2 $c2)) (let $fn (* $f1 (- 1 $f2)) ((- 1 $fn) (* $fn (* $c1 $c2)))))
(= (Truth_DecomposeNPP ($f1 $c1) ($f2 $c2)) (let $f (* (- 1 $f1) $f2) ($f (* $f (* $c1 $c2)))))
(= (Truth_DecomposePNP ($f1 $c1) ($f2 $c2)) (let $f (* $f1 (- 1 $f2)) ($f (* $f (* $c1 $c2)))))
(= (Truth_DecomposePPP $v1 $v2) (Truth_DecomposeNPP (Truth_Negation $v1) $v2))
(= (Truth_DecomposeNNN ($f1 $c1) ($f2 $c2)) (let $fn (* (- 1 $f1) (- 1 $f2)) ((- 1 $fn) (* $fn (* $c1 $c2)))))
(= (Truth_Eternalize ($f $c)) ($f (Truth_w2c $c)))
(= (Truth_Revision ($f1 $c1) ($f2 $c2)) 
   (let* (($w1 (Truth_c2w $c1)) ($w2 (Truth_c2w $c2)) ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w)) ($c (Truth_w2c $w)))
          ((min 1.00 $f) (min 0.99 (max (max $c $c1) $c2)))))
(= (Truth_Expectation ($f $c)) (+ (* $c (- $f 0.5)) 0.5))
;;NAL-1
;;!Syllogistic rules for Inheritance:
(= (|- (($a --> $b) $T1) (($b --> $c) $T2)) (($a --> $c) (Truth_Deduction $T1 $T2)))
(= (|- (($a --> $b) $T1) (($a --> $c) $T2)) (($c --> $b) (Truth_Induction $T1 $T2)))
(= (|- (($a --> $c) $T1) (($b --> $c) $T2)) (($b --> $a) (Truth_Abduction $T1 $T2)))
(= (|- (($a --> $b) $T1) (($b --> $c) $T2)) (($c --> $a) (Truth_Exemplification $T1 $T2)))
;;NAL-2
;;!Rules for Similarity:
(= (|- (($S <-> $P) $T)) (($P <-> $S) (Truth_StructuralIntersection $T)))
(= (|- (($M <-> $P) $T1) (($S <-> $M) $T2)) (($S <-> $P) (Truth_Resemblance $T1 $T2)))
(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) (($S <-> $P) (Truth_Comparison $T1 $T2)))
(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($S <-> $P) (Truth_Comparison $T1 $T2)))
(= (|- (($M --> $P) $T1) (($S <-> $M) $T2)) (($S --> $P) (Truth_Analogy $T1 $T2)))
(= (|- (($P --> $M) $T1) (($S <-> $M) $T2)) (($P --> $S) (Truth_Analogy $T1 $T2)))
;;!Dealing with properties and instances:
(= (|- (($S --> (ExtSet $P)) $T)) (($S <-> (ExtSet $P)) (Truth_StructuralIntersection $T)))
(= (|- (((IntSet $S) --> $P) $T)) (((IntSet $S) <-> $P) (Truth_StructuralIntersection $T)))
(= (|- (((ExtSet $M) --> $P) $T1) (($S <-> $M) $T2)) (((ExtSet $S) --> $P) (Truth_Analogy $T1 $T2)))
(= (|- (($P --> (IntSet $M)) $T1) (($S <-> $M) $T2)) (($P --> (IntSet $S)) (Truth_Analogy $T1 $T2)))
(= (|- (((ExtSet $A) <-> (ExtSet $B)))) (($A <-> $B) (Truth_StructuralIntersection $T)))
(= (|- (((IntSet $A) <-> (IntSet $B)))) (($A <-> $B) (Truth_StructuralIntersection $T)))
;;NAL-3
;;!Set decomposition:
(= (|- (((ExtSet $A $B) --> $M) $T)) (((ExtSet $A) --> $M) (Truth_StructuralDeduction $T)))
(= (|- (((ExtSet $A $B) --> $M) $T)) (((ExtSet $B) --> $M) (Truth_StructuralDeduction $T)))
(= (|- ((M --> (IntSet $A $B)) $T)) ((M --> (IntSet $A)) (Truth_StructuralDeduction $T)))
(= (|- ((M --> (IntSet $A $B)) $T)) ((M --> (IntSet $B)) (Truth_StructuralDeduction $T)))
;;!Extensional and intensional intersection decomposition:
(= (|- ((($S | $P) --> $M) $T)) (($S --> $M) (Truth_StructuralDeduction $T)))
(= (|- (($M --> ($S & $P)) $T)) (($M --> $S) (Truth_StructuralDeduction $T)))
(= (|- ((($S | $P) --> $M) $T)) (($P --> $M) (Truth_StructuralDeduction $T)))
(= (|- (($M --> ($S & $P)) $T)) (($M --> $P) (Truth_StructuralDeduction $T)))
(= (|- ((($A ~ $S) --> $M) $T)) (($A --> $M) (Truth_StructuralDeduction $T)))
(= (|- (($M --> ($B - $S)) $T)) (($M --> $B) (Truth_StructuralDeduction $T)))
(= (|- ((($A ~ $S) --> $M) $T)) (($S --> $M) (Truth_StructuralDeductionNegated $T)))
(= (|- (($M --> ($B - $S)) $T)) (($M --> $S) (Truth_StructuralDeductionNegated $T)))
;;!Extensional and intensional intersection composition: (sets via reductions)
;(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) ((($P | $S) --> $M) (Truth_Intersection $T1 $T2)))
;(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) ((($P & $S) --> $M) (Truth_Union $T1 $T2)))
;(= (|- (($P --> $M) $T1) (($S --> $M) $T2)) ((($P ~ $S) --> $M) (Truth_Difference $T1 $T2)))
;(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($M --> ($P & $S)) (Truth_Intersection $T1 $T2)))
;(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($M --> ($P | $S)) (Truth_Union $T1 $T2)))
;(= (|- (($M --> $P) $T1) (($M --> $S) $T2)) (($M --> ($P - $S)) (Truth_Difference $T1 $T2)))
;;!Extensional and intensional intersection decomposition:
(= (|- (($S --> $M) $T1) ((($S | $P) --> $M) $T2)) (($P --> $M) (Truth_DecomposePNN $T1 $T2)))
(= (|- (($P --> $M) $T1) ((($S | $P) --> $M) $T2)) (($S --> $M) (Truth_DecomposePNN $T1 $T2)))
(= (|- (($S --> $M) $T1) ((($S & $P) --> $M) $T2)) (($P --> $M) (Truth_DecomposeNPP $T1 $T2)))
(= (|- (($P --> $M) $T1) ((($S & $P) --> $M) $T2)) (($S --> $M) (Truth_DecomposeNPP $T1 $T2)))
(= (|- (($S --> $M) $T1) ((($S ~ $P) --> $M) $T2)) (($P --> $M) (Truth_DecomposePNP $T1 $T2)))
(= (|- (($S --> $M) $T1) ((($P ~ $S) --> $M) $T2)) (($P --> $M) (Truth_DecomposeNNN $T1 $T2)))
(= (|- (($M --> $S) $T1) (($M --> ($S & $P)) $T2)) (($M --> $P) (Truth_DecomposePNN $T1 $T2)))
(= (|- (($M --> $P) $T1) (($M --> ($S & $P)) $T2)) (($M --> $S) (Truth_DecomposePNN $T1 $T2)))
(= (|- (($M --> $S) $T1) (($M --> ($S | $P)) $T2)) (($M --> $P) (Truth_DecomposeNPP $T1 $T2)))
(= (|- (($M --> $P) $T1) (($M --> ($S | $P)) $T2)) (($M --> $S) (Truth_DecomposeNPP $T1 $T2)))
(= (|- (($M --> $S) $T1) (($M --> ($S - $P)) $T2)) (($M --> $P) (Truth_DecomposePNP $T1 $T2)))
(= (|- (($M --> $S) $T1) (($M --> ($P - $S)) $T2)) (($M --> $P) (Truth_DecomposeNNN $T1 $T2)))
;; NAL-4
;;!Transformation rules between product and image:
(= (|- ((($A x $B) --> $R) $T)) (($A --> ($R /1 $B)) (Truth_StructuralIntersection $T)))
(= (|- ((($A x $B) --> $R) $T)) (($B --> ($R /2 $A)) (Truth_StructuralIntersection $T)))
(= (|- (($R --> ($A x $B)) $T)) ((($R \1 $B) --> $A) (Truth_StructuralIntersection $T)))
(= (|- (($R --> ($A x $B)) $T)) ((($R \2 $A) --> $B) (Truth_StructuralIntersection $T)))
;;other direction of same rules (as these are bi-directional)
;(= (|- (($A --> ($R /1 $B)) $T)) ((($A x $B) --> $R) (Truth_StructuralIntersection $T)))
;(= (|- (($B --> ($R /2 $A)) $T)) ((($A x $B) --> $R) (Truth_StructuralIntersection $T)))
;(= (|- ((($R \1 $B) --> $A) $T)) (($R --> ($A x $B)) (Truth_StructuralIntersection $T)))
;(= (|- ((($R \2 $A) --> $B) $T)) (($R --> ($A x $B)) (Truth_StructuralIntersection $T)))
;;!Comparative relations
;(= (|- ((({ $R }) |-> ([ $P ])) $T1) ((({ $S }) |-> ([ $P ])) $T2)) (((({ $R }) x ({ $S })) --> (>>> $P )) (Truth_FrequencyGreater $T1 $T2)))
;(= (|- ((($A x $B) --> (>>> $P)) $T1) ((($B x $C) --> (>>> $P)) $T2)) ((($A x $C) --> (>>> $P)) (Truth_Deduction $T1 $T2)))
;(= (|- ((({ $R }) |-> ([ $P ])) $T1) ((({ $S }) |-> ([ $P ])) $T2)) (((({ $R }) x ({ $S })) --> (=== $P)) (Truth_FrequencyEqual $T1 $T2)))
;(= (|- ((($A x $B) --> (=== $P)) $T1) ((($B x $C) --> (=== $P)) $T2)) ((($A x $C) --> (=== $P)) (Truth_Deduction $T1 $T2)))
;(= (|- ((($A x $B) --> (=== $P)) $T)) ((($B x $A) --> (=== $P)) (Truth_StructuralIntersection $T)))
;;!Optional rules for more efficient reasoning about relation components:
(= (|- ((($A x $B) --> $R) $T1) ((($C x $B) --> $R) $T2)) (($C --> $A) (Truth_Abduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) ((($A x $C) --> $R) $T2)) (($C --> $B) (Truth_Abduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($C x $B)) $T2)) (($C --> $A) (Truth_Induction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($A x $C)) $T2)) (($C --> $B) (Truth_Induction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C --> $A) $T2)) ((($C x $B) --> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($A --> $C) $T2)) ((($C x $B) --> $R) (Truth_Induction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C <-> $A) $T2)) ((($C x $B) --> $R) (Truth_Analogy $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C --> $B) $T2)) ((($A x $C) --> $R) (Truth_Deduction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($B --> $C) $T2)) ((($A x $C) --> $R) (Truth_Induction $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) (($C <-> $B) $T2)) ((($A x $C) --> $R) (Truth_Analogy $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($A --> $C) $T2)) (($R --> ($C x $B)) (Truth_Deduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C --> $A) $T2)) (($R --> ($C x $B)) (Truth_Abduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C <-> $A) $T2)) (($R --> ($C x $B)) (Truth_Analogy $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($B --> $C) $T2)) (($R --> ($A x $C)) (Truth_Deduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C --> $B) $T2)) (($R --> ($A x $C)) (Truth_Abduction $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($C <-> $B) $T2)) (($R --> ($A x $C)) (Truth_Analogy $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) ((($C x $B) --> $R) $T2)) (($A <-> $C) (Truth_Comparison $T1 $T2)))
(= (|- ((($A x $B) --> $R) $T1) ((($A x $C) --> $R) $T2)) (($B <-> $C) (Truth_Comparison $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($C x $B)) $T2)) (($A <-> $C) (Truth_Comparison $T1 $T2)))
(= (|- (($R --> ($A x $B)) $T1) (($R --> ($A x $C)) $T2)) (($B <-> $C) (Truth_Comparison $T1 $T2)))
;;NAL-5
;;!Negation conjunction and disjunction decomposition:
(= (|- ((neg $A) $T)) ($A (Truth_Negation $T)))
(= (|- (($A && $B) $T)) ($A (Truth_StructuralDeduction $T)))
(= (|- (($A && $B) $T)) ($B (Truth_StructuralDeduction $T)))
(= (|- (($A && $B) $T)) (($B && $A) (Truth_StructuralIntersection $T)))
(= (|- ($S $T1) (($S && $A) $T2)) ($A (Truth_DecomposePNN $T1 $T2)))
(= (|- ($S $T1) (($S || $A) $T2)) ($A (Truth_DecomposeNPP $T1 $T2)))
(= (|- ($S $T1) (((neg $S) && $A) $T2)) ($A (Truth_DecomposeNNN $T1 $T2)))
(= (|- ($S $T1) (((neg $S) || $A) $T2)) ($A (Truth_DecomposePPP $T1 $T2)))
;;!Syllogistic rules for Implication:
(= (|- (($A ==> $B) $T1) (($B ==> $C) $T2)) (($A ==> $C) (Truth_Deduction $T1 $T2)))
(= (|- (($A ==> $B) $T1) (($A ==> $C) $T2)) (($C ==> $B) (Truth_Induction $T1 $T2)))
(= (|- (($A ==> $C) $T1) (($B ==> $C) $T2)) (($B ==> $A) (Truth_Abduction $T1 $T2)))
(= (|- (($A ==> $B) $T1) (($B ==> $C) $T2)) (($C ==> $A) (Truth_Exemplification $T1 $T2)))
;;!Conditional composition for conjunction and disjunction:
(= (|- (($A ==> $C) $T1) (($B ==> $C) $T2)) ((($A && $B) ==> $C) (Truth_Union $T1 $T2)))
(= (|- (($A ==> $C) $T1) (($B ==> $C) $T2)) ((($A || $B) ==> $C) (Truth_Intersection $T1 $T2)))
(= (|- (($C ==> $A) $T1) (($C ==> $B) $T2)) (($C ==> ($A && $B)) (Truth_Intersection $T1 $T2)))
(= (|- (($C ==> $A) $T1) (($C ==> $B) $T2)) (($C ==> ($A || $B)) (Truth_Union $T1 $T2)))
;;!Multi-conditional inference:
(= (|- ((($S && $P) ==> $M) $T1) (($S ==> $M) $T2)) ($P (Truth_Abduction $T1 $T2)))
(= (|- ((($C && $M) ==> $P) $T1) (($S ==> $M) $T2)) ((($C && $S) ==> $P) (Truth_Deduction $T1 $T2)))
(= (|- ((($C && $P) ==> $M) $T1) ((($C && $S) ==> $M) $T2)) (($S ==> $P) (Truth_Abduction $T1 $T2)))
(= (|- ((($C && $M) ==> $P) $T1) (($M ==> $S) $T2)) ((($C && $S) ==> $P) (Truth_Induction $T1 $T2)))
;;!Rules for equivalence:
(= (|- (($S <=> $P) $T)) (($P <=> $S) (Truth_StructuralIntersection $T)))
(= (|- (($S ==> $P) $T1) (($P ==> $S) $T2)) (($S <=> $P) (Truth_Intersection $T1 $T2)))
(= (|- (($P ==> $M) $T1) (($S ==> $M) $T2)) (($S <=> $P) (Truth_Comparison $T1 $T2)))
(= (|- (($M ==> $P) $T1) (($M ==> $S) $T2)) (($S <=> $P) (Truth_Comparison $T1 $T2)))
(= (|- (($M ==> $P) $T1) (($S <=> $M) $T2)) (($S ==> $P) (Truth_Analogy $T1 $T2)))
(= (|- (($P ==> $M) $T1) (($S <=> $M) $T2)) (($P ==> $S) (Truth_Analogy $T1 $T2)))
(= (|- (($M <=> $P) $T1) (($S <=> $M) $T2)) (($S <=> $P) (Truth_Resemblance $T1 $T2)))
;;!Higher-order decomposition
(= (|- ($A $T1) (($A ==> $B) $T2)) ($B (Truth_Deduction $T1 $T2)))
(= (|- ($A $T1) ((($A && $B) ==> $C) $T2)) (($B ==> $C) (Truth_Deduction $T1 $T2)))
(= (|- ($B $T1) (($A ==> $B) $T2)) ($A (Truth_Abduction $T1 $T2)))
(= (|- ($A $T1) (($A <=> $B) $T2)) ($B (Truth_Analogy $T1 $T2)))
;;NAL term reductions
;;!Extensional intersection, union, conjunction reductions:
(= (reduced ($A & $A)) $A)
(= (reduced ($A | $A)) $A)
(= (reduced ($A && $A)) $A)
(= (reduced ($A || $A)) $A)
;;!Extensional set reductions:
(= (reduced ((ExtSet $A) | (ExtSet $B))) (ExtSet $A $B))
(= (reduced ((ExtSet $A $B) | (ExtSet $C))) (ExtSet ($A SetElement $B) $C))
(= (reduced ((ExtSet $C) | (ExtSet $A $B) )) (ExtSet $C ($A SetElement $B)))
;;!Intensional set reductions:
(= (reduced ((IntSet $A) & (IntSet $B))) (IntSet $A $B) )
(= (reduced ((IntSet $A $B) & (IntSet $C))) (IntSet ($A SetElement $B) $C))
(= (reduced ((IntSet $A) & (IntSet $B $C))) (IntSet $A ($B SetElement $C)))
;;!Reduction for set element copula:
(= (reduced (ExtSet ( $A SetElement $B ))) (ExtSet $A $B))
(= (reduced (IntSet ( $A SetElement $B ))) (IntSet $A $B))
;params
(= (BeliefEventsMax) 10)
(= (GoalEventsMax) 10)

;states
(= (increment $atom) (sequential ((change-state! $atom (+ 1 (get-state $atom))))))
(= (UpdateReasonerState) (sequential ((trace! (time := (get-state &currentTime)) 42) 
                                        (increment &currentTime) (increment &evidentialBase))))
(= (GetReasonerState) ((get-state &currentTime) ((get-state &evidentialBase))))

;priority of events
(= (EventPriorityNow ($T $P) $t) (* $P (/ 1 (+ 1 (- $t $T)))))

;retrieve the best candidate (allows to use tuples / collapse results / spaces as a PQ)
!(bind! &tempbest (new-state ()))
!(bind! &tempbestscore (new-state 0))
(= (BestCandidate $tuple $evaluateCandidateFunction $t)
   (sequential ((do (change-state! &tempbestscore 0))
                (do (change-state! &tempbest ()))
                (do (let* (($x (superpose $tuple))
                               ($fx ($evaluateCandidateFunction $x $t)))
                              (superpose ((If (> $fx (get-state &tempbestscore))
                                              (sequential ((change-state! &tempbest $x)
                                                           (change-state! &tempbestscore $fx))))))))
                (get-state &tempbest))))

;functions to select highest-priority events in belief and goal PQ
(= (PriorityOf (Event $Sentence ($occT $Ev $Prio)) $t) (EventPriorityNow $Prio $t))
(= (SelectHighestPriorityEvent $collection $t)
   (BestCandidate (collapse (get-atoms $collection)) PriorityOf $t))

;a belief event to process, which demands adding it to the PQ and updating its concept
(= (ProcessBeliefEvent $Ev $t)
   (sequential ((add-atom &belief_events $Ev)
                (UpdateConcept $Ev $t))))

;bound the size of the attentional focus for tasks / events
(= (BoundEvents $collection $Threshold $Increment $TargetAmount $t)
   (sequential ((do (let* (($Ev (get-atoms $collection))
                            ((Event $Sentence ($Time $Evidence $EPrio)) $Ev))
                          (If (< (EventPriorityNow $EPrio $t) $Threshold)
                              (remove-atom $collection $Ev))))
                (let $CurrentAmount (TupleCount (collapse (CountElement (get-atoms $collection))))
                     (If (> $CurrentAmount $TargetAmount)
                         (BoundEvents $collection (+ $Threshold $Increment) $Increment $TargetAmount $t))))))
;params
(= (AttentionalFocusConceptsMax) 10)


;priority of concepts
(= (ConceptPriorityNow ($T $P) $t) (* $P (/ 1 (+ 1 (- $t $T)))))

;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (If (or (== $Ev1 ()) (== $Ev2 ())) ;check only needed due to a bug in the MeTTa interpreter
       True                           ;https://github.com/trueagi-io/hyperon-experimental/issues/481
       (== () (collapse (let* (($x (superpose $Ev1))
                               ($y (superpose $Ev2)))
                              (case (== $x $y) ((True overlap))))))))

;revise if there is no evidential overlap, else use higher-confident candidate
(=
    (RevisionAndChoice
      (Event ($Term1 ($f1 $c1)) (eternal $ev1 $EPrio1))
      (Event ($Term2 ($f2 $c2)) (eternal $ev2 $EPrio2)))
    (let $ConclusionStamp
      (TupleConcat $ev1 $ev2)
      (If
        (StampDisjoint $ev1 $ev2)
        (Event
          ($Term1 (Truth_Revision ($f1 $c1) ($f2 $c2)))
          (eternal $ConclusionStamp (0 0.0)))
        (If
          (> $c1 $c2)
          (Event ($Term1 ($f1 $c1)) (eternal $ev1 (0 0.0)))
          (Event ($Term2 ($f2 $c2)) (eternal $ev2 (0 0.0)))))))


;update beliefs in existing concept with the new event or create new concept to enter the new evidence
(= (UpdateConcept $NewEvent $t)
   (let* (((Event ($Term $TV) ($Time $Evidence $EPrio)) $NewEvent)
          ($NewEventEternalized (Eternalize $NewEvent)))
			(case (match &attentional_focus (Concept $TermX $Belief $BeliefEvent $CPrio) 
 					       (If (== $TermX $Term) (Concept $TermX $Belief $BeliefEvent $CPrio)))
			  (((Concept $TermX $Belief $BeliefEvent $CPrio) (sequential ((remove-atom &attentional_focus (Concept $TermX $Belief $BeliefEvent $CPrio))
				   (let* (((Event $RevSentence $Metadata) (RevisionAndChoice $Belief $NewEventEternalized))
						  ($RevisedBelief (Event (trace! (@ $RevSentence) $RevSentence) $Metadata))
						  ($MaxPrio (If (> (EventPriorityNow $EPrio $t) (ConceptPriorityNow $CPrio $t))
										$EPrio $CPrio)))
						 (add-atom &attentional_focus (Concept $Term $RevisedBelief $NewEvent $MaxPrio))))))
			   (%void% (case (match &concepts (Concept $TermX2 $Belief2 $BeliefEvent2 $CPrio2)
							 (If (== $TermX2 $Term) (Concept $TermX2 $Belief2 $BeliefEvent2 $CPrio2)))
								 (((Concept $TermX2 $Belief2 $BeliefEvent2 $CPrio2) (sequential ((remove-atom &concepts (Concept $TermX2 $Belief2 $BeliefEvent2 $CPrio2))
									  (add-atom &attentional_focus (Concept $TermX2 $Belief2 $BeliefEvent2 $CPrio2))
									  (UpdateConcept $NewEvent $t))))
								 (%void% (sequential ((add-atom &attentional_focus (Concept $Term $NewEventEternalized $NewEvent $EPrio))))))))))))

;bound the size of attentional focus of concepts
(= (BoundAttention $Threshold $Increment $TargetAmount $t)
   (sequential ((do (let* (($C (get-atoms &attentional_focus))
                           ((Concept $Term (Event $Sentence $Metadata) $BeliefEvent $CPrio) $C))
					      (let $CPN (ConceptPriorityNow $CPrio $t)
                            (If (< $CPN $Threshold) 
							    (sequential 
							     ((remove-atom &attentional_focus $C)
                                  (add-atom &concepts $C))))) ))
                (let $CurrentAmount 
				     (TupleCount (collapse (CountElement (get-atoms &attentional_focus))))
                     (If (> $CurrentAmount $TargetAmount)
                         (BoundAttention (+ $Threshold $Increment) $Increment $TargetAmount $t))))))

;get eternal belief of concept
(: EternalQuestion (-> Expression %Undefined%))
(= (EternalQuestion $Term) (case (match (superpose (&attentional_focus &concepts)) (Concept $TermX $Belief $BeliefEvent $CPrio) (If (== $Term $TermX) $Belief))
                                 (($Ev $Ev) (%void% (Event (None (0.5 0.0)) (eternal () 0.0))))))

(: HowAchieveQuestion (-> Expression %Undefined%))
(= (HowAchieveQuestion $Cons) (case (match (superpose (&attentional_focus &concepts)) (Concept (($Prec &/ (^ $Op)) =/> $ConsX) $Belief $BeliefEvent $CPrio) (If (== $Cons $ConsX) $Belief))
                                    (($Ev $Ev) (%void% (Event (None (0.5 0.0)) (eternal () 0.0))))))

(: WhatWillHappenQuestion (-> Expression %Undefined%))
(= (WhatWillHappenQuestion $Prec) (case (match (superpose (&attentional_focus &concepts)) (Concept ($PrecX =/> $Cons) $Belief $BeliefEvent $CPrio) (If (== $Prec $PrecX) $Belief))
                                    (($Ev $Ev) (%void% (Event (None (0.5 0.0)) (eternal () 0.0))))))


;get event belief of concept
(: EventQuestion (-> Expression %Undefined%))
(= (EventQuestion $Term) (case (match (superpose (&attentional_focus &concepts)) (Concept $TermX $Belief $BeliefEvent $CPrio) (If (== $TermX $Term) $BeliefEvent))
                               (($Ev $Ev) (%void% (Event (None (0.5 0.0)) (0 () 0.0))))))
;;Declarative inference (deriving events and knowledge from observed events)

;Derived belief event priority
(= (ConclusionPriority $EPrio $CPrio $ConcTV) (* (* $EPrio $CPrio) (Truth_Expectation $ConcTV)))

;making declarative inferences on two events (task from PQ and belief from concept)
(= (Conclude (Event $S1 ($time1 $ev1 $prio1)) (Event $S2 ($time2 $ev2 $prio2)) $CPrio $t)
   (let $ConclusionStamp (TupleConcat $ev1 $ev2)
        (If (StampDisjoint $ev1 $ev2)
            (let ($ConcTerm $ConcTV) (superpose ((|- $S1 $S2) (|- $S2 $S1)))
                 (Event ($ConcTerm $ConcTV)
                        ($time1 $ConclusionStamp
                                ($t (ConclusionPriority (EventPriorityNow $prio1 $t)
                                                        (ConceptPriorityNow $CPrio $t)
                                                        $ConcTV))))))))

;find a belief for the task to generate conclusions with
(= (ReasonWithTask (Event $S1 ($time1 $ev1 $prio1)) $t)
   (let ($Belief $CPrio) (case (get-atoms &attentional_focus)
                               (((Concept $Term (Event $SE2 ($timeE2 $evE2 $prioE2)) (Event $S2 ($time2 $ev2 $prio2)) $CPrio)
                                 (If (and (not (== $time1 eternal)) 
									     (> (abs (- $time1 $time2)) 20))
                                     ((Event $SE2 ($timeE2 $evE2 $prioE2)) $Cprio)
                                     ((Event $S2 ($time2 $ev2 $prio2)) $CPrio)))))
        (case (Conclude (Event $S1 ($time1 $ev1 $prio1)) (TemporallyAlignedBelief $time1 $Belief)
				 $CPrio $t)
              (((Event $1 $2) 
				  (ProcessBeliefEvent (Event (trace! (.: $1) $1) $2) $t))))))

;select the highest priority belief event from the PQ and use it for reasoning
(= (BeliefCycle $t) (do (sequential ((let $Ev (SelectHighestPriorityEvent &belief_events $t)
                                          (sequential ((remove-atom &belief_events $Ev)
                                                       (ReasonWithTask $Ev $t))))
                                     (UpdateReasonerState)
                                     (BoundEvents &belief_events 0.0 0.1 (BeliefEventsMax) $t)
                                     (BoundAttention 0.0 0.1 (AttentionalFocusConceptsMax) $t)))))
;;Temporal inference (sequence and implication formation based on FIFO)

;use the event's evidence to induce a time-independent belief which can be used in the future
(= (Eternalize $Ev) (let (Event ($Term $TV) ($Time $Evidence $EPrio)) $Ev
                         (If (== $Time eternal) $Ev
                             (Event ($Term (Truth_Eternalize $TV)) (eternal $Evidence (0 0.0))))))


;use evidence of an event at a slightly different moment in time
(= (Projection (Event ($Term ($f $c)) ($Time $Evidence $EPrio)) $TargetTime)
   (Event ($Term ($f (* $c (min 1 (/ 1 (max 1 (abs (- $Time $TargetTime))))))))
	  ($TargetTime $Evidence $EPrio)))


;make the belief occurrence time compatible with the task's
(= (TemporallyAlignedBelief $TaskTime $Belief) (If (== $TaskTime eternal)
                                                   (Eternalize $Belief)
                                                   (Projection $Belief $TaskTime)))

;FIFO max. size bound
!(bind! &FIFO (new-state ()))
(= (EventWithFIFO3 $New ($1 $2 $3)) ($New $1 $2))
(= (EventWithFIFO3 $New ($1 $2)) ($New $1 $2))
(= (EventWithFIFO3 $New ($1)) ($New $1))
(= (EventWithFIFO3 $New ()) ($New))

; !(set-debug! e)

;Add event to FIFO
(= (EventToFIFO $Ev)
   (let $newlist (EventWithFIFO3 $Ev (get-state &FIFO))
        (change-state! &FIFO $newlist)))

;Form a sequence of two events
(= (TemporalSequence $Ev1 (Event ($Term2 $Truth2) ($Time2 $Evidence2 $EPrio2)))
   (let (Event ($Term1 $Truth1) ($Time1 $Evidence1 $EPrio1)) (Projection $Ev1 $Time2)
        (Event (($Term1 &/ $Term2) (Truth_Intersection $Truth1 $Truth2)) ($Time2 (TupleConcat $Evidence1 $Evidence2) (0 0.0)))))

;Form a temporal implication between two events
(= (TemporalImplication $Ev1 (Event ($Term2 $Truth2) ($Time2 $Evidence2 $EPrio2)))
   (let (Event ($Term1 $Truth1) ($Time1 $Evidence1 $EPrio1)) (Projection $Ev1 $Time2)
        (trace! (.: (($Term1 =/> $Term2) (Truth_Induction $Truth1 $Truth2)))
                (Event (($Term1 =/> $Term2) (Truth_Induction $Truth1 $Truth2)) ($Time2 (TupleConcat $Evidence1 $Evidence2) (0 0.0))))))

;Whether an event's term is an operation
(= (IsOp (Event ($Term $Truth) $Metadata))
   (case $Term (((^ $Opname) True)
                ($Otherwise False))))

;Find implications in the event FIFO:
;procedural implications
(= (TemporalImplicationInduction ($Cons $Op $Prec))
   (If (and (IsOp $Op) (and (not (IsOp $Cons)) (not (IsOp $Prec))))
       (let $PrecOp (TemporalSequence $Prec $Op)
            (TemporalImplication $PrecOp $Cons))))
;and temporal without operation
;(= (TemporalImplicationInduction ($Cons $Prec $Trail))
;   (If (and (not (IsOp $Prec)) (not (IsOp $Cons)))
;       (TemporalImplication $Prec $Cons)))

;Add negative evidence for implications which predicted the input unsuccessfully
(= (NegConfirmation $PrecTerm $ObservedCons $t)
   (let (Event (($PrecTerm =/> $PredictedCons) $ImpTV) $ImpMetadata) (WhatWillHappenQuestion $PrecTerm)
        (If (not (== $ObservedCons $PredictedCons))
            (UpdateConcept (Event (($PrecTerm =/> $PredictedCons) (0.0 0.1)) ($t () (0 0.0))) $t))))

;Check if the implication's preconditions are met to anticipate the by the implication predicted outcome
;(= (Anticipate ($Pos $Pre $Trail) $t)
;   (let* (((Event ($PreTerm $PreTV) $PreMetadata) $Pre)
;          ((Event ($PosTerm $PosTV) $PosMetadata) $Pos))
;         (If (not (IsOp $Pre))
;             (NegConfirmation $PreTerm $PosTerm $t))))
(= (Anticipate ($Pos $Op $Pre) $t)
   (let* (((Event ($PreTerm $PreTV) $PreMetadata) $Pre)
          ((Event ($OpTerm $OpTV) $OpMetadata) $Op)
          ((Event ($PosTerm $PosTV) $PosMetadata) $Pos))
         (If (and (IsOp $Op) (not (IsOp $Pre)))
             (NegConfirmation ($PreTerm &/ $OpTerm) $PosTerm $t))))

;Input procedure
;(: AddBeliefEvent (-> Atom Atom))
;(= (AddBeliefEvent $Sentence) 
;    ((AddBeliefEvent0 $Sentence) (check-state-r)))

;(: AddBeliefEvent (-> Atom %Undefined%))
(= (AddBeliefEvent $Sentence)
  (let* ((($t $evidentialBase) (GetReasonerState))
          ($InputEvent (Event (trace! (.: $Sentence) $Sentence) ($t $evidentialBase ($t 1.0))))
          ($notAnOperation (not (IsOp $InputEvent))))
         (do (sequential ((EventToFIFO $InputEvent)
                           (Anticipate (get-state &FIFO) $t)
                           (If $notAnOperation
                               (sequential ((let $InducedHypothesis (TemporalImplicationInduction (get-state &FIFO))
                                                 (UpdateConcept $InducedHypothesis $t))
                                            (ProcessBeliefEvent $InputEvent $t)
                                            (BeliefCycle $t)))
                               (UpdateReasonerState)))))))
	    

;;Procedural inference (decision making with operation execution and subgoaling)

;Derived goal event priority
(= (SubgoalPriority $EPrio $ConcTV) (* $EPrio (Truth_Expectation $ConcTV)))

;Expectation of an operation is the truth expectation of its desire value
(= (OpExpectation (Decision ($Opname $DVOp) $Subgoal) $t) (Truth_Expectation $DVOp))

;Inject executed operation as an event and return its name
(= (Execute $Opname) (superpose ((AddBeliefEvent ($Opname (1.0 0.9))) $Opname)))

;Add subgoals to the PQ
(= (DeriveSubgoals $Options)
   (do (let (Decision $Op (Event $S $Metadata)) (superpose $Options)
            (let $Subgoal (Event (trace! (!: $S) $S) $Metadata)
                 (add-atom &goal_events $Subgoal)))))

;execute the operation which most likely gets the goal achieved in current contexts, and if contexts are not yet fulfilled, derive them as subgoals
(= (BestDecision $t (Event ($Term $DV) ($GoalTime $GoalEvBase $GoalPrio)))
   (let $Options (collapse (let* (((Event ((($Prec &/ (^ $Op)) =/> $Term) $ImpTV) ($ImpTime $ImpEvBase $ImpPrio))
                                   (HowAchieveQuestion $Term))
                                  ($DVPrecOp (Truth_Deduction $DV $ImpTV))
                                  ((Event ($PrecTerm $PrecTV) $PrecMetadata)
                                   (Projection (EventQuestion $Prec) $t))
                                  ($DVOp (Truth_Deduction $PrecTV $DVPrecOp))
                                  ($DVPrec (Truth_StructuralDeduction $DVPrecOp))
                                  ($SubgoalStamp (TupleConcat $GoalEvBase $ImpEvBase))
                                  ($SubgoalPrio (SubgoalPriority (EventPriorityNow $GoalPrio $t) $DVPrec)))
                                 (If (StampDisjoint $GoalEvBase $ImpEvBase)
                                     (Decision ((^ $Op) $DVOp) (Event ($Prec $DVPrec)
                                                                      ($t $SubgoalStamp ($t $SubgoalPrio)))))))
        (let (Decision ($Opname $DVOp) $Subgoal) (BestCandidate $Options OpExpectation $t)
             (If (> (Truth_Expectation $DVOp) 0.5)
                 (Execute $Opname)
                 (DeriveSubgoals $Options)))))

;select the highest priority goal event from the PQ and use it for decision making
(= (GoalCycle $t) (sequential ((let $Ev (SelectHighestPriorityEvent &goal_events $t)
                                    (sequential ((do (remove-atom &goal_events $Ev))
                                                 (BestDecision $t $Ev))))
                               (do (UpdateReasonerState))
                               (do (BoundEvents &goal_events 0.0 0.1 (GoalEventsMax) $t)))))

;Input procedure
(= (AddGoalEvent $Sentence)
   (let* ((($t $evidentialBase) (GetReasonerState))
          ($InputEvent (Event (trace! (!: $Sentence) $Sentence) ($t $evidentialBase ($t 1.0)))))
         (sequential ((do (add-atom &goal_events $InputEvent))
                      (GoalCycle $t)))))


(: checkEqualToResult (-> Atom Atom Atom))
(= (checkEqualToResult $eval $results)
  (let $was (collapse $eval)
	 (if (== $was $results) ()
	   (let $q (quote (Expected: $results  Got: $was ))
		   (let () (println! $q) $q)))))
  

(= (name-list $name $list)
	 (let $len (TupleCount $list) 
	   ($len $name $list)))


(= (check-state)
 ((n=v currentTime (get-state &currentTime))
  (n=v evidentialBase (get-state &evidentialBase))
  (name-list concepts (collapse (get-atoms &concepts)))
  (name-list attentional_focus (collapse (get-atoms &attentional_focus)))
  (name-list FIFO (get-state &FIFO))
  (n=v tempbest (get-state &tempbest))
  (name-list belief_events (collapse (get-atoms &belief_events)))
  (name-list goal_events (collapse (get-atoms &goal_events)))))


!(compile-easy!)
