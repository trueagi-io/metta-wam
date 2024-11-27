
;; Collection of functions operating on numbers

;; Define max
(: max (-> Number Number Number))
(= (max $x $y) (if (> $x $y) $x $y))

;; Define min
(: min (-> Number Number Number))
(= (min $x $y) (if (< $x $y) $x $y))

;; Clamp a number to be within a certain range
(: clamp (-> Number Number Number Number))
(= (clamp $x $l $u) (max $l (min $u $x)))

;; Define abs
(: abs (-> Number Number))
(= (abs $x) (if (< $x 0) (* -1 $x) $x))

;; Define lte
(: lte (-> Number Number Bool))
(= (lte $x $y) (or (< $x $y) (== $x $y)))

;; Define gte
(: gte (-> Number Number Bool))
(= (gte $x $y) (or (> $x $y) (== $x $y)))

;; Define approximately equal
(: approxEq (-> Number Number Number Bool))
(= (approxEq $x $y $epsilon) (lte (abs (- $x $y)) $epsilon))


;; Tests for max
!(assertEqual (max 3 5) 5)  ;; Test when the second number is larger
!(assertEqual (max 7 2) 7)  ;; Test when the first number is larger
!(assertEqual (max 4 4) 4)  ;; Test when both numbers are equal

;; Tests for min
!(assertEqual (min 3 5) 3)  ;; Test when the first number is smaller
!(assertEqual (min 7 2) 2)  ;; Test when the second number is smaller
!(assertEqual (min 4 4) 4)  ;; Test when both numbers are equal

;; Tests for clamp
!(assertEqual (clamp 5 0 10) 5)  ;; Test when value is within the range
!(assertEqual (clamp -1 0 10) 0) ;; Test when value is below the lower bound
!(assertEqual (clamp 15 0 10) 10) ;; Test when value is above the upper bound
!(assertEqual (clamp 0 0 10) 0)  ;; Test edge case at the lower bound
!(assertEqual (clamp 10 0 10) 10) ;; Test edge case at the upper bound

;; Tests for abs
!(assertEqual (abs 5) 5)     ;; Test positive number
!(assertEqual (abs -5) 5)    ;; Test negative number
!(assertEqual (abs 0) 0)     ;; Test zero

;; Tests for lte
!(assertEqual (lte 3 5) True)  ;; Test when first number is less
!(assertEqual (lte 5 5) True)  ;; Test when numbers are equal
!(assertEqual (lte 7 5) False) ;; Test when first number is greater

;; Tests for gte
!(assertEqual (gte 7 5) True)  ;; Test when first number is greater
!(assertEqual (gte 5 5) True)  ;; Test when numbers are equal
!(assertEqual (gte 3 5) False) ;; Test when first number is less

;; Tests for approxEq
!(assertEqual (approxEq 3.0 3.1 0.1) False) ;; Test when difference exceeds epsilon
!(assertEqual (approxEq 3.0 3.05 0.1) True) ;; Test when difference is within epsilon
!(assertEqual (approxEq 3.0 3.0 0.1) True)  ;; Test when numbers are exactly equal
!(assertEqual (approxEq 3.0 2.9 0.1) True)  ;; Test when difference is just within epsilon
!(assertEqual (approxEq 3.0 2.8 0.1) False) ;; Test when difference is slightly beyond epsilon

;; Combined test cases
!(assertEqual (clamp (max 3 7) 0 5) 5)  ;; Clamp the maximum of two numbers
!(assertEqual (clamp (min -3 2) -5 5) -3) ;; Clamp the minimum of two numbers
!(assertEqual (abs (min -3 2)) 3)        ;; Absolute value of the minimum
!(assertEqual (approxEq (abs -5) 5 0) True) ;; Absolute value and approximate equality


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

!(pragma! coerce auto)

;; Tests for max
!(assertEqual (max (S Z) (S (S Z))) (S (S Z))) ;; max(1, 2) = 2
!(assertEqual (max (S (S Z)) Z) (S (S Z)))     ;; max(2, 0) = 2
!(assertEqual (max (S Z) (S Z)) (S Z))         ;; max(1, 1) = 1

;; Tests for min
!(assertEqual (min (S Z) (S (S Z))) (S Z))     ;; min(1, 2) = 1
!(assertEqual (min (S (S Z)) Z) Z)             ;; min(2, 0) = 0
!(assertEqual (min (S Z) (S Z)) (S Z))         ;; min(1, 1) = 1

;; Tests for clamp
!(assertEqual (clamp (S Z) Z (S (S (S Z)))) (S Z)) ;; clamp(1, 0, 3) = 1
!(assertEqual (clamp Z (S Z) (S (S Z))) (S Z))     ;; clamp(0, 1, 2) = 1
!(assertEqual (clamp (S (S (S Z))) Z (S (S Z))) (S (S Z))) ;; clamp(3, 0, 2) = 2

;; Tests for lte
!(assertEqual (lte (S Z) (S (S Z))) True)  ;; lte(1, 2) = True
!(assertEqual (lte (S (S Z)) (S (S Z))) True)  ;; lte(2, 2) = True
!(assertEqual (lte (S (S Z)) Z) False)  ;; lte(2, 0) = False

;; Tests for gte
!(assertEqual (gte (S (S Z)) (S Z)) True)  ;; gte(2, 1) = True
!(assertEqual (gte (S Z) (S Z)) True)  ;; gte(1, 1) = True
!(assertEqual (gte Z (S Z)) False)  ;; gte(0, 1) = False

;; Tests for approxEq
!(assertEqual (approxEq (S Z) (S Z) Z) True) ;; approxEq(1, 1, 0) = True
!(assertEqual (approxEq (S Z) (S (S Z)) Z) False) ;; approxEq(1, 2, 0) = False

;; Combined test cases
!(assertEqual (clamp (max (S Z) (S (S Z))) Z (S (S (S Z)))) (S (S Z))) ;; max(1, 2) clamped to [0, 3] = 2
!(assertEqual (clamp (min (S (S Z)) (S (S Z))) (S Z) (S (S (S Z)))) (S (S Z))) ;; min(2, 2) clamped to [1, 3] = 2
!(assertEqual (approxEq (max Z (min (S (S Z)) (S Z))) (S Z) Z) True) ;; approxEq(max(0, min(2, 1)), 1, 0) = True
