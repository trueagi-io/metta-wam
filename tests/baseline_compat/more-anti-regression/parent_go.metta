
!(import! &kb go_rel)


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

;; Curried Backward Chainer
(: bc (-> $a Nat $a))
;; Base case
(= (bc (: $prf $ccln) $_)
     (let () (println! (bc-base (: $prf $ccln)))
          (match &kb (: $prf $ccln) 
               (let () (println! (bc-base-ground (: $prf $ccln))) (: $prf $ccln)))))

;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
  (let () (println! (bc-rec (: ($prfabs $prfarg) $ccln) (S $k))) 
     (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
               ((: $prfarg $prms) (bc (: $prfarg $prms) $k)))
               (: ($prfabs $prfarg) $ccln))))


;!(bc (: $prf (member $g $o $k)) (fromNumber 3))

!(bc_impl! (: $prf (member $g $o $k)) (fromNumber 3))

!(pragma! rtrace true)

!(bc_impl! (: $prf (member $g $o $k)) (fromNumber 3))

!(pragma! rtrace false)

;!(bcm!)
!(bc_impl! (: $prf (member $g $o $k)) (fromNumber 3))
