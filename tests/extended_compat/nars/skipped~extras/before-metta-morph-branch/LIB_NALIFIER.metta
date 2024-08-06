!(import! LIB_MINNARS.metta)








(arity query 2) ;; FP
(= (query $Term)
   (match &self (, (= (|- ($A $T1) ($B $T2)) ($Term ($f $T1 $T2)))
                      (($A $T1) $Ev1) (($B $T2) $Ev2))
          (If (StampDisjoint $Ev1 $Ev2)
              (($Term ($f $T1 $T2)) (TupleConcat $Ev1 $Ev2)))))

;choice between two options of different term
(arity Choice 3) ;; FP
(= (Choice (($Term1 $T1) $ev1) (($Term2 $T2) $ev2))
   (If (> (Truth_Expectation $T1) (Truth_Expectation $T2))
       (($Term1 $T1) $ev1)
       (($Term2 $T2) $ev2)))

;revise if there is no evidential overlap, else use higher-confident candidate
(arity RevisionAndChoice 3) ;; FP
(= (RevisionAndChoice (($Term1 ($f1 $c1)) $ev1) (($Term2 ($f2 $c2)) $ev2))
   (let $ConclusionStamp (TupleConcat $ev1 $ev2)
        (If (StampDisjoint $ev1 $ev2)
            (($Term2 (Truth_Revision ($f1 $c1) ($f2 $c2))) $ConclusionStamp)
            (If (> $c1 $c2)
                (($Term1 ($f1 $c1)) $ev1)
                (($Term2 ($f2 $c2)) $ev2)))))

;reduce beliefs
(arity reduceBeliefs 3) ;; FP
(= (reduceBeliefs $revChoiceOrBoth $option $options)
   (If (== $options ())
       $option
       (let* (($head (car-atom $options))
              ($rest (cdr-atom $options))
              ($revi ($revChoiceOrBoth $option $head)))
             (reduceBeliefs $revChoiceOrBoth $revi $rest))))

;an empty event for reduction purposes
(arity EmptyEvent 1) ;; FP
(= (EmptyEvent) ((x (1.0 0.0)) ()))

;evidence query tries to maximize evidence for the passed statement term
(arity evidenceQuery 2) ;; FP
(= (evidenceQuery $Term) (reduceBeliefs RevisionAndChoice (EmptyEvent) (collapse (query $Term))))

;choice query picks the option of highest truth expectation among the options of different term
(arity choiceQuery 2) ;; FP
(= (choiceQuery $Terms)
   (let $options (collapse (evidenceQuery (superpose $Terms)))
        (reduceBeliefs Choice (EmptyEvent) $options)))

