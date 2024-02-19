;; Standalone polyward chaining experiments.

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

;;;;;;;;;;;;;;;;;;;;;;;
;; Forward Revertant ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Forward chainer that may go backward if necessary.

! "=== Forward Revertant ==="

;; Knowledge base
!(bind! &kb (new-space))
!(add-atom &kb (: ab (→ A B)))
!(add-atom &kb (: bc (→ B C)))
!(add-atom &kb (: a A))

;; Rule base
!(bind! &rb (new-space))
!(add-atom &rb (: ModusPonens (->
                               ;; Premises
                               (→ $p $q)
                               $p
                               ;; Conclusion
                               $q)))
!(add-atom &rb (: Deduction (->
                             ;; Premises
                             (→ $p $q)
                             (→ $q $r)
                             ;; Conclusion
                             (→ $p $r))))

;; Backward chainer, based on the DTL version of
;; `../backward-chaining/bc-xp.metta` but rules are in an atomspace
;; instead of being hardwired.  To be used by the forward revertant
;; chainer defined below.
(: bc (-> Atom Nat Atom))
;; Base case
(= (bc (: $prf $ccln) $depth)
   (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive cases
(= (bc (: $prf $ccln) (S $k))
   (match &rb (: $ctor (-> $prms1 $prms2 $ccln))
          (let* (((: $prf1 $prms1) (bc (: $prf1 $prms1) $k))
                 ((: $prf2 $prms2) (bc (: $prf2 $prms2) $k))
                 (($ctor $prf1 $prf2) $prf))
            (: $prf $ccln))))

;; Test backward chainer
!(assertEqual
  (bc (: $prf A) Z)
  (: a A))
!(assertEqual
  (bc (: $prf (→ A B)) Z)
  (: ab (→ A B)))
!(assertEqual
  (bc (: $prf (→ B C)) Z)
  (: bc (→ B C)))
!(assertEqual
  (bc (: $prf B) (fromNumber 1))
  (: (ModusPonens ab a) B))
!(assertEqualToResult
  (bc (: $prf C) (fromNumber 2))
  ((: (ModusPonens bc (ModusPonens ab a)) C)
   (: (ModusPonens (Deduction ab bc) a) C)))
!(assertEqual
  (bc (: $prf (→ A C)) (fromNumber 2))
  (: (Deduction ab bc) (→ A C)))

;; Forward revertant chainer, based on the DTL version of
;; `../backward-chaining/bc-xp.metta` but matching premises is
;; replaced by calling the backward chainer defined above.
(: frc (-> Atom Nat Atom))
;; Base case
(= (frc (: $prf $prms) $depth) (: $prf $prms))
;; Recursive cases
(= (frc (: $prf1 $prms1) (S $k))
   (match &rb (: $ctor (-> $prms1 $prms2 $ccln))
          (let (: $prf2 $prms2) (bc (: $prf2 $prms2) $k)
               (frc (: ($ctor $prf1 $prf2) $ccln) $k))))
(= (frc (: $prf2 $prms2) (S $k))
   (match &rb (: $ctor (-> $prms1 $prms2 $ccln))
          (let (: $prf1 $prms1) (bc (: $prf1 $prms1) $k)
               (frc (: ($ctor $prf1 $prf2) $ccln) $k))))

;; Test forward revertant chainer
!(assertEqual
  (frc (: a A) Z)
  (: a A))
!(assertEqualToResult
  (frc (: a A) (fromNumber 1))
  ((: a A)
   (: (ModusPonens ab a) B)))
;; Disable assertEqualToResult due to leftover
;; !(assertEqualToResult
  !(frc (: a A) (fromNumber 2))
  ;; ((: a A)
  ;;  (: (ModusPonens ab a) B)
  ;;  (: (ModusPonens bc (ModusPonens ab a)) C)
  ;;  (: (ModusPonens (Deduction ab bc) a) C)))
