;;;;;;;;;
;; Nat ;;
;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge and rule base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!(bind! &kb (new-space))

!(add-atom &kb (: r1 (≤ 0 3)))
!(add-atom &kb (: r2 (≤ 7 9)))
!(add-atom &kb (: r3 (≤ $x $x)))
!(add-atom &kb (: r4 (≤ $x (plus $x 0))))
!(add-atom &kb (: r5 (≤ (plus $x 0) $x)))
!(add-atom &kb (: r6 (≤ (plus $x $y) (plus $y $x))))
!(add-atom &kb (: r7 (-> (≤ $w $y)                             ; Premise 1
                         (-> (≤ $x $z)                         ; Premise 2
                             (≤ (plus $w $x) (plus $y $z)))))) ; Conclusion
!(add-atom &kb (: r8 (-> (≤ $x $y)         ; Premise 1
                         (-> (≤ $y $z)     ; Premise 2
                             (≤ $x $z))))) ; Conclusion

;; Base case
(: bc (-> $a Nat $a))
(= (bc (: $prf $ccln) $_)
   ;(trace! (⊷ bc-base (: $prf $ccln) $_)
   (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   ;(trace! (⊷ bc-step (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln))
           (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms)
           (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;; Prove that 7≤3+9
!(bc (: $prf (≤ 7 (plus 3 9))) (fromNumber 4))
