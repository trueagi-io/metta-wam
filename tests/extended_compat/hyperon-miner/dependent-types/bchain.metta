(: is-variable (-> Atom Bool))
(= (is-variable $x) (== (get-metatype $x) Variable))

(: lazy-or (-> Bool Atom Bool))
(= (lazy-or False $x) $x)
(= (lazy-or True $x) True)

(: is-expression (-> Atom Bool))
(= (is-expression $x) (== (get-metatype $x) Expression))

(: is-closed (-> Atom Bool))
(= (is-closed $x) (if (is-variable $x)
                      False
                      (if (is-expression $x)
                          (lazy-or (let $head (car-atom $x) (is-closed $head))
                                   (let $tail (cdr-atom $x) (is-closed $tail)))
                          True)))

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

!(add-atom &kb (: rule2 
                (-> (less $x $y)
                    (-> (isDiff $x $y))
                        (this is always true))))

; !(add-atom &kb 
;     (: rule 
;         (-> (⍃ $x $y)
;             (-> (rule1output $x $y)
;                 (less $x $y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DTL Backward chaining Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_) (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))
;; System proof for ⍃
;; if conclusion equals (⍃ $X $Y), then return (: CPU (⍃ $X $Y)) 
;; if $x and $Y are fully grounded and (< $X $Y)
(= (bc (: CPU (⍃ $X $Y)) $_) 
    (if (and (and (is-closed $X) (is-closed $Y)) (< $X $Y))
        (: CPU (⍃ $X $Y))
        (empty)))
;; System proof for isDiff
(= (bc (: CPUDiff (isDiff $X $Y)) $_)
    (if (and (and (is-closed $X) (is-closed $Y)) (== $X $Y))
        (empty)
        (: CPUDiff (isDiff $X $Y))))
; Test
!(bc (: CPU (⍃ 2 3)) Z)
!(bc (: CPU (⍃ 4 3)) Z)
!(bc (: CPUDiff (isDiff (hello world) (this world))) Z)
!(bc (: CPUDiff (isDiff (test pattern) (test pattern))) Z)
!(bc (: $prf (less $x $y)) (S (S (S Z))))
!(bc (: $prf (this is always true)) (S (S (S (S (S (S (S Z))))))))
