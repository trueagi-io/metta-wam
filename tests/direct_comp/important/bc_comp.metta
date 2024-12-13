;!(pragma! compile full)

(: is-variable (-> Atom Bool))
(= (is-variable $x) (== (get-metatype $x) Variable))

(: lazy-or (-> Bool Atom Bool))
(= (lazy-or False $x) $x)
(= (lazy-or True $x) True)

(: is-expression (-> Atom Bool))
(= (is-expression $x) (== (get-metatype $x) Expression))

(: is-ground (-> Atom Bool))
(= (is-ground $x) (if (is-variable $x)
		False
		(if (== () $x) True
		(if (is-expression $x)
			(and (let $head (car-atom $x) (is-ground $head))
				 (let $tail (cdr-atom $x) (is-ground $tail)))
			True))))


;; KB
!(bind! &kb (new-space))

!(add-atom &kb (: axiom (nums 2 3)))

!(add-atom &kb (: rule1 
                (-> (nums $x $y)
                    (rule1output $x $y))))

!(add-atom &kb 
    (: rule 
       (-> (rule1output $x $y)
        (-> (lt $x $y)
            (less $x $y)))))

; !(add-atom &kb 
;     (: rule 
;         (-> (lt $x $y)
;             (-> (rule1output $x $y)
;                 (less $x $y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Backward chaining Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(: bc (-> $a hyperon::space::DynSpace Nat $a))
;; Base case
(= (bc (: $prf $ccln) $space $_1) (match $space (: $prf $ccln) (: $prf $ccln)))
;; if conclusion equals (lt $X $Y), then return (: CPU (lt $X $Y)) 
;; if $x and $Y are fully grounded and (< $X $Y)
(= (bc (: CPU (lt $X $Y)) $space $_2) 
	(if (and (and (is-ground $X) (is-ground $Y)) (< $X $Y))
		(: CPU (lt $X $Y))
		(empty)))

;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) $space (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) 
			          (bc (: $prfabs (-> $prms $ccln)) $space $k))
          ((: $prfarg $prms) 
			          (bc (: $prfarg $prms) $space $k)))
		   
     (: ($prfabs $prfarg) $ccln)))

;!(pragma! e trace)
;!(pragma! e-args debug)

; Test
!(bc (: CPU (lt 2 3)) &kb Z)
!(bc (: CPU (lt 4 3)) &kb Z)

;!(pragma! eval debug)

;! ( rtrace! (bc (: $prf (less $x $y)) (S (S (S Z)))))

!(bc (: $prf (less $x $y)) &kb (S (S (S Z))))


