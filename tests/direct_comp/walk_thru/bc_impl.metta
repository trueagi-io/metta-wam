!(pragma! load debug)
!(pragma! log True)


(: is-variable (-> Atom Bool))
(= (is-variable $x) 
    (== (get-metatype $x) Variable))

(: lazy-or (-> Bool Atom Bool))
(= (lazy-or False $x) $x)
(= (lazy-or True $x) True)

(: is-expression (-> Atom Bool))
(= (is-expression $x) 
   (== (get-metatype $x) Expression))

(: is-closed (-> Atom Bool))
(= (is-closed $x) 
  (if (is-variable $x)
	  False
	  (if (== () $x) True
	  (if (is-expression $x)
		  (and (let $head (car-atom $x) (is-closed $head))
			   (let $tail (cdr-atom $x) (is-closed $tail)))
		  True))))

; function
; :(X,Y, [:,X,Y]).

;; KB
!(bind! &kb (new-space))

!(add-atom &kb (: axiom (nums 2 3)))

!(add-atom &kb (: rule1 
                (-> (nums $x $y)
                    (rule1output $x $y))))

!(add-atom &kb 
    (: rule 
       (-> (rule1output $x $y)
        (-> (⍃ $x $y)
            (less $x $y)))))

; !(add-atom &kb 
;     (: rule 
;         (-> (⍃ $x $y)
;             (-> (rule1output $x $y)
;                 (less $x $y)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Backward chaining Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: bc (-> $a hyperon::space::DynSpace Nat $a))

;; Base case
;; (= (bc (: $prf $ccln) $space $_1)
;;   (match $space (: $prf $ccln)
;;                 (: $prf $ccln)))
;;
;; (= (bc $X $space $_1)
;;     (match $space $X   $X))
(:- (bc (: $prf $ccln) $space $_ $OUT) 
   (match $space (: $prf $ccln) (: $prf $ccln) $OUT))


;; if conclusion equals (⍃ $X $Y), then return (: CPU (⍃ $X $Y)) 
;; if $x and $Y are fully grounded and (< $X $Y)
;(= (bc (: CPU (⍃ $X $Y)) $space $_2) 
;	(if (and (and (is-closed $X) (is-closed $Y)) (< $X $Y))
;		(: CPU (⍃ $X $Y))
;		(empty)))
(:- (bc (: CPU (⍃ $X $Y)) $space $_ (: CPU (⍃ $X $Y))) 
     (is-closed $X) (is-closed $Y) (!) (< $X $Y))
   


;; Recursive step
;(= (bc (: ($prfabs $prfarg) $ccln) $space (S $k))
;   (let* (( (: $prfabs (-> $prms $ccln)) 
;					  (bc (: $prfabs (-> $prms $ccln)) $space $k ))
;		  ((: $prfarg $prms) 
;					  (bc (: $prfarg $prms) $space $k)))
;	 (: ($prfabs $prfarg) $ccln)))

;(:- (bc (: ($prfabs $prfarg) $ccln) $space (S $k) $RES)
;   (=! $RES (: ($prfabs $prfarg) $ccln))
;   (=! $OUT (: $prfabs (-> $prms $ccln))) 
;          (bc (: $prfabs (-> $prms $ccln)) $space $k $OUT)   
;   (=! $OUT2 (: $prfarg $prms))
;          (bc (: $prfarg $prms) $space $k $OUT2))


;; Recursive step
(:- (bc (: ($prfabs $prfarg) $ccln) $space (S $k) (: ($prfabs $prfarg) $ccln))
   (bc (: $prfabs (-> $prms $ccln)) $space $k (: $prfabs (-> $prms $ccln)))
   (bc (: $prfarg $prms) $space $k (: $prfarg $prms)))



; function(A,B,C) = O
; predicate(A,B,C,O)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make things compiled
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load the compile ontology
!(ensure-compiler!)

; !(pfcWatch!)

!(compile! is-variable)
!(compile! is-expression)
;; !(compile! lazy-or)
!(compile! is-closed)
!(compile! bc)
!(listing! metta_compiled_predicate)

!(pragma! log False)
!(pragma! compatio True)

; Test
!(bc (: CPU (⍃ 2 3)) &kb Z)
!(bc (: CPU (⍃ 4 3)) &kb Z)

; !(pragma! eval debug)

;! ( rtrace! (bc (: $prf (less $x $y)) (S (S (S Z)))))

!(bc (: $prf (less $x $y)) &kb (S (S (S Z))))


