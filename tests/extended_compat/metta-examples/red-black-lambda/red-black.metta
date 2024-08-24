; WIP

!(import! &LC lambda-theory)

(= (RedLambda $RV $BLC) (RTheory (match &LC (LambdaTheory ($RV $BLC) $rhs) $rhs)))
(= (BlackLambda $BV $RLC) (BTheory (match &LC (LambdaTheory ($BV $RLC) $rhs) $rhs)))

!(RedLambda BTheory String)
!(assertEqualToResult
   (RedLambda BTheory String)
   ((RTheory (: equivalent (-> Term (-> Term Bool))))
	(RTheory (: Abstraction (-> String (-> Term Term))))
	(RTheory (= ((equivalent $l_28558) $r_28559) ???))
	(RTheory (: reduce (-> Term (Option Term))))
	(RTheory (: Application (-> Term (-> Term Term))))
	(RTheory (: Mention (-> String Term)))
	(RTheory (: Ground (-> BTheory Term)))
	(RTheory (= (reduce $t_28547) ???))
	(RTheory (: Term Type))))

(= (objectify (RTheory $x)) (RC $x))
(= (objectify (BTheory $x)) (BC $x))

!(objectify (RedLambda String BTheory))
!(assertEqualToResult 
   (objectify (RedLambda String BTheory))
   ((RC (: equivalent (-> Term (-> Term Bool))))
	(RC (: Abstraction (-> BTheory (-> Term Term))))
	(RC (= ((equivalent $l_44946) $r_44947) ???))
	(RC (: reduce (-> Term (Option Term))))
	(RC (: Application (-> Term (-> Term Term))))
	(RC (: Mention (-> BTheory Term)))
	(RC (: Ground (-> String Term)))
	(RC (= (reduce $t_44935) ???))
	(RC (: Term Type))))

!(objectify (BlackLambda String RTheory))
!(assertEqualToResult
   (objectify (BlackLambda String RTheory))
   ((BC (: equivalent (-> Term (-> Term Bool))))
	(BC (: Abstraction (-> RTheory (-> Term Term))))
	(BC (= ((equivalent $l_78359) $r_78360) ???))
	(BC (: reduce (-> Term (Option Term))))
	(BC (: Application (-> Term (-> Term Term))))
	(BC (: Mention (-> RTheory Term)))
	(BC (: Ground (-> String Term)))
	(BC (= (reduce $t_78348) ???))
	(BC (: Term Type))))


; (RedBlackLambda (rc (match &self (LambdaTheory ($RV $BLC) $rhs) $rhs)))
; (RedBlackLambda (: RBTerm))
; RBTerm = Either[RC.Term,BC.Term]
; (: RBTerm (Either (RC Term) (BC Term)))
;