



(= (bc (: $prf $ccln) $_1) (match &kb (: $prf $ccln) (: $prf $ccln)))
;; if conclusion equals (⍃ $X $Y), then return (: CPU (⍃ $X $Y)) 
;; if $x and $Y are fully grounded and (< $X $Y)
(= (bc (: CPU (⍃ $X $Y)) $_2) 
    (if (and (and (is-closed $X) (is-closed $Y)) (< $X $Y))
        (: CPU (⍃ $X $Y))
        (empty)))

;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) 
			          (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) 
			          (bc (: $prfarg $prms) $k)))
		   
     (: ($prfabs $prfarg) $ccln)))


