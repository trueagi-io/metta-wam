(: If (-> Bool Atom Atom))
(= (If True $then) $then)
(= (If False $then) (let $x 0 (let $x 1 $x)))

(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

(: do (-> Expression %Undefined%))
(= (do $1) (case $1 ()))

(= (max $1 $2) (if (> $1 $2) $1 $2))
(= (min $1 $2) (if (< $1 $2) $1 $2))
(= (abs $x) (If (< $x 0) (- 0 $x) $x))

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
(= (Truth_Expectation ($f $c)) (+ (* $c (- $f 0.5)) 0.5))

;;NAL-1
;;!Syllogistic rules for Inheritance:
(= (|- ($T $T1) ($T $T2)) ($T (Truth_Revision $T1 $T2)))
(= (|- (($a --> $b) $T1) (($b --> $c) $T2)) (($a --> $c) (Truth_Deduction $T1 $T2)))
(= (|- (($a --> $b) $T1) (($a --> $c) $T2)) (($c --> $b) (Truth_Induction $T1 $T2)))
(= (|- (($a --> $c) $T1) (($b --> $c) $T2)) (($b --> $a) (Truth_Abduction $T1 $T2)))

;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True overlap)))))))

;actually that is quite cool and 4x faster in MeTTa than below (yet still 130 times slower than metta-morph with below)
;but it depends on more advanced pattern matching features which are not yet in metta-morph
;(= (query $Term)
;   (match &self (, (= (|- ($A $T1) ($B $T2)) ($Term ($f $T1 $T2)))
;                   (($A $T1) $Ev1) (($B $T2) $Ev2))
;          (If (StampDisjoint $Ev1 $Ev2)
;              (($Term ($f $T1 $T2)) (TupleConcat $Ev1 $Ev2)))))

(= (query $Term)
   (match &self (, (($A $T1) $Ev1) (($B $T2) $Ev2))
          (let ($TermNew $T) (|- ($A $T1) ($B $T2))
               (If (and (== $TermNew $Term) (StampDisjoint $Ev1 $Ev2))
                   (($Term $T) (TupleConcat $Ev1 $Ev2))))))

;choice between two options of different term
(= (Choice (($Term1 $T1) $ev1) (($Term2 $T2) $ev2))
   (If (> (Truth_Expectation $T1) (Truth_Expectation $T2))
       (($Term1 $T1) $ev1)
       (($Term2 $T2) $ev2)))

;revise if there is no evidential overlap, else use higher-confident candidate
(= (RevisionAndChoice (($Term1 ($f1 $c1)) $ev1) (($Term2 ($f2 $c2)) $ev2))
   (let $ConclusionStamp (TupleConcat $ev1 $ev2)
        (If (StampDisjoint $ev1 $ev2)
            (($Term2 (Truth_Revision ($f1 $c1) ($f2 $c2))) $ConclusionStamp)
            (If (> $c1 $c2)
                (($Term1 ($f1 $c1)) $ev1)
                (($Term2 ($f2 $c2)) $ev2)))))

;reduce beliefs
(= (reduceBeliefs $revChoiceOrBoth $option $options)
   (If (== $options ())
       $option
       (let* (($head (car-atom $options))
              ($rest (cdr-atom $options))
              ($revi ($revChoiceOrBoth $option $head)))
             (reduceBeliefs $revChoiceOrBoth $revi $rest))))

;an empty event for reduction purposes
(= (EmptyEvent todo) ((x (1.0 0.0)) ()))

;evidence query tries to maximize evidence for the passed statement term
(= (evidenceQuery $Term) (reduceBeliefs RevisionAndChoice (EmptyEvent todo) (collapse (query $Term))))

;choice query picks the option of highest truth expectation among the options of different term
(= (choiceQuery $Terms)
   (let $options (collapse (evidenceQuery (superpose $Terms)))
        (reduceBeliefs Choice (EmptyEvent todo) $options)))



(((dog --> (IntSet brown)) (1.0 0.9)) (1))
(((dog --> (IntSet small)) (1.0 0.9)) (2))
(((dog --> (IntSet furry)) (1.0 0.9)) (3))
(((dog --> (IntSet barks)) (1.0 0.9)) (4))

(((duck --> (IntSet yellow)) (1.0 0.9)) (5))
(((duck --> (IntSet small)) (1.0 0.9)) (6))
(((duck --> (IntSet feathered)) (1.0 0.9)) (7))
(((duck --> (IntSet quacks)) (1.0 0.9)) (8))

(((swan --> (IntSet white)) (1.0 0.9)) (9))
(((swan --> (IntSet big)) (1.0 0.9)) (10))
(((swan --> (IntSet feathered)) (1.0 0.9)) (11))

((((ExtSet sam) --> (IntSet white)) (1.0 0.9)) (12))
((((ExtSet sam) --> (IntSet small)) (1.0 0.9)) (13))
((((ExtSet sam) --> (IntSet quacks)) (1.0 0.9)) (14))

!(let ($S $EV) (choiceQuery (((ExtSet sam) --> duck)
                             ((ExtSet sam) --> swan)
                             ((ExtSet sam) --> dog)))
      $S)


;  !(repl!)
