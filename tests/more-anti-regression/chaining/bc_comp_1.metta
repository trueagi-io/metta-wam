

!(pragma! compile full)

(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_1) (match &kb (: $prf $ccln) (: $prf $ccln)))
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) 
			          (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) 
			          (bc (: $prfarg $prms) $k)))		   
     (: ($prfabs $prfarg) $ccln)))


