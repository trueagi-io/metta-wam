
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

;; Define a generic less than operator, lessThan, for Nat.  < cannot be used
;; because it is a built-in, its type is hardwired and cannot be
;; overloaded.
(: lessThan (-> Nat Nat Bool))
(= (lessThan $_ Z) False)
(= (lessThan Z (S $_)) True)
(= (lessThan (S $x) (S $y)) (lessThan $x $y))
;; Overload lessThan for Number.
(: lessThan (-> Number Number Bool))
(= (lessThan $x $y) (< $x $y))

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

;; Test elementary functions
!(assertEqual (fromNumber 1) (S Z))
!(assertEqual (fromNat (S (S Z))) 2)
!(assertEqual (ceil 2.4) 3)
!(assertEqual (abs -10) 10)
!(assertEqual (approxEq 10 10.0001 1e-3) True)
!(assertEqual (approxEq 10 10.0001 1e-9) False)
!(assertEqual (lessThan Z (S Z)) True)

