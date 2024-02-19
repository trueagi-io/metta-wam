;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Define <=
(: <= (-> $a $a Bool))
(= (<= $x $y) (or (< $x $y) (== $x $y)))

;; Define cast functions between Nat and Number
(: fromNumber (-> Number Nat))
(= (fromNumber $n) (if (<= $n 0) Z (S (fromNumber (- $n 1)))))
(: fromNat (-> Nat Number))
(= (fromNat Z) 0)
(= (fromNat (S $k)) (+ 1 (fromNat $k)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge/rule base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

!(bind! &kb (new-space))

;; K combinator
!(add-atom &kb (: AK (-> (: $x $a)            
                         (-> (: $y $b) $a)))) 

;; S combinator
!(add-atom &kb (: AS (-> (: $g (-> (: $x $a) (-> (: $y $b) $c))) 
                         (-> (: $f (-> (: $x $a) $b))            
                             (-> (: $x $a) $c)))))               

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_)
   (match &kb (: $prf $ccln) (: $prf $ccln)))

;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (
          ((: $prfabs (-> (: $prfarg $prms) $ccln))
           (bc (: $prfabs (-> (: $prfarg $prms) $ccln)) $k))          
          ((: $prfarg $prms)
           (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))  

!(bc (: $prf (-> (: $f (-> (: $x $a) (-> (: $y $b) $c))) (-> (: $y $b) (-> (: $x $a) $c))))
     (fromNumber 2))


