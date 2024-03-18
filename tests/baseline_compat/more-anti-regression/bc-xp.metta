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


; (= (rb) (superpose ((:- (path $x $z) ((link $x $z)))
;                 (:- (path $x $z) ((path $x $y) (link $y $z))))))


!(bind! &kb (new-space))

!(add-atom &kb (:- (link a b) ()))
!(add-atom &kb (:- (link b c) ()))
!(add-atom &kb (:- (path a b) ()))
!(add-atom &kb (:- (path b c) ()))
!(add-atom &kb (:- (path $x $z) ((:- (path $x $y) $r) (:- (link $y $z) $s))))
; !(add-atom &kb (:- (path $x $z) ((:- (link $x $z) $q))))

; !(add-atom &kb (:- A ()))
; !(add-atom &kb (:- (R B A) ()))
; !(add-atom &kb (:- $q ((:- (R B A) $r) (:- $p $s))))


; (link a b)
; (link b c)

; (= (bc-chain $query $_)
;     (trace! (⊷ bc-chain $query $_)
;     (match &kb $query $query)))


(= (bc-chain (:- $ccln ()) $depth)
   (trace! (⊷ bc-chain (:- $ccln ()) $depth)
        (match &kb (:- $ccln ()) (:- $ccln ()))))

(= (bc-chain (:- $ccln $premises) (S $k))
   (trace! (⊷ bc-chain (:- $ccln $premises) (S $k))
   (match &kb (:- $ccln $premises)
          (let $r (collapse (bc-chain (superpose $premises) $k))
            (:- $ccln $r)))))
 

; !(bc-chain (:- B $prf) (fromNumber 1))
!(bc-chain (:- (path a c) $prf) (fromNumber 2))
; !(bc-chain (:- (path b $y) ()) Z)
