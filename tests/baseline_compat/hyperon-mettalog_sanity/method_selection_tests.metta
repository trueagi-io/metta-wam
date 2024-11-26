;; define some-value returning Number and String
(= (some-value) 1)
(= (some-value) "a")
(= (some-value) 'a')
(= (some-value) ())
(= (some-value) (1 2 3))
(= (some-value) (println! "did it"))
(= (some-value) S)
(= (some-value) Z)
(= (some-value) (S Z))
(= (some-value) (S (S Z)))
(= (some-value) &self)


;; Collection of functions operating on numbers

;; Define max
(: max (-> $a $a $a))
(= (max $x $y) (if (> $x $y) $x $y))

;; Define min
(: min (-> $a $a $a))
(= (min $x $y) (if (< $x $y) $x $y))

;; Clamp a number to be within a certain range
(: clamp (-> $a $a $a $a))
(= (clamp $x $l $u) (max $l (min $u $x)))

;; Define abs
(: abs (-> $a $a))
(= (abs $x) (if (< $x 0) (* -1 $x) $x))

;; Define <=
(: <= (-> $a $a Bool))
(= (<= $x $y) (or (< $x $y) (== $x $y)))

;; Define >=
(: >= (-> $a $a Bool))
(= (>= $x $y) (or (> $x $y) (== $x $y)))

;; Define approximately equal
(: approxEq (-> $a $a $a Bool))
(= (approxEq $x $y $epsilon) (<= (abs (- $x $y)) $epsilon))

;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Define cast functions between Nat and Number
(: fromNumber (-> Number Nat))
(= (fromNumber $n) (if (<= $n 0) Z (S (fromNumber (- $n 1)))))
(: fromNat (-> Nat Number))
(= (fromNat Z) 0)
(= (fromNat (S $k)) (+ 1 (fromNat $k)))

;; Define a generic less than operator, ⩻, for Nat.  < cannot be used
;; because it is a built-in, its type is hardwired and cannot be
;; overloaded.
(: ⩻ (-> Nat Nat Bool))
(= (⩻ $_ Z) False)
(= (⩻ Z (S $_)) True)
(= (⩻ (S $x) (S $y)) (⩻ $x $y))

;; Overload ⩻ for Number.
(: ⩻ (-> Number Number Bool))
(= (⩻ $x $y) (< $x $y))

;; Return the ceiling of a non negative number.  If the number is
;; negative it returns 1.
(: ceil (-> Number Number))
(= (ceil $n) (fromNat (fromNumber $n)))

;; Convert Number to Bool.  Anything above 0 converts to True.
(: number->bool (-> Number Bool))
(= (number->bool $x) (< 0 $x))

;; Convert Bool to Number.  False converts to 0, True converts to 1.
(: bool->number (-> Bool Number))
(= (bool->number False) 0)
(= (bool->number True) 1)

;; Define a less than type.  Note that it is purposefully different
;; than ⩻ as it is a type, not an operator.  Inhabitants of (⩻ x y)
;; are proofs that x ⩻ y == True.  For now ⩻ is only axiomatized for
;; Nat.
(: ⩻ (-> $t $t Type))



;; Test elementary functions
!(assertEqual (fromNumber 1) (S Z))
!(assertEqual (fromNat (S (S Z))) 2)
!(assertEqual (ceil 2.4) 3)
!(assertEqual (abs -10) 10)
!(assertEqual (approxEq 10 10.0001 1e-3) True)
!(assertEqual (approxEq 10 10.0001 1e-9) False)
!(assertEqual (⩻ Z (S Z)) True)

