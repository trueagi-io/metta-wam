;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

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


!("============ Test lessThan ============")

;; Tests for Nat
!(assertEqualToResult (lessThan Z (S Z)) (True))         ;; 0 < 1 (Nat)
!(assertEqualToResult (lessThan Z Z) (False))           ;; 0 < 0 (Nat)
!(assertEqualToResult (lessThan (S Z) Z) (False))       ;; 1 < 0 (Nat)
!(assertEqualToResult (lessThan (S Z) (S Z)) (False))   ;; 1 < 1 (Nat)
!(assertEqualToResult (lessThan (S Z) (S (S Z))) (True)) ;; 1 < 2 (Nat)
!(assertEqualToResult (lessThan (S (S Z)) (S (S Z))) (False)) ;; 2 < 2 (Nat)
!(assertEqualToResult (lessThan (S (S Z)) (S Z)) (False)) ;; 2 < 1 (Nat)
!(assertEqualToResult (lessThan (S Z) (S (S (S Z)))) (True)) ;; 1 < 3 (Nat)

;; Tests for Number
!(assertEqualToResult (lessThan 0 1) (True))            ;; 0 < 1 (Number)
!(assertEqualToResult (lessThan 0 0) (False))           ;; 0 < 0 (Number)
!(assertEqualToResult (lessThan 1 0) (False))           ;; 1 < 0 (Number)
!(assertEqualToResult (lessThan 2 2) (False))           ;; 2 < 2 (Number)
!(assertEqualToResult (lessThan 1 2) (True))            ;; 1 < 2 (Number)
!(assertEqualToResult (lessThan 3 2) (False))           ;; 3 < 2 (Number)

;; Mixed tests to ensure no type confusion
!(assertEqualToResult (lessThan Z 0) ())           ;; Z < 0 (Nat and Number)
!(assertEqualToResult (lessThan 0 Z) ())           ;; 0 < Z (Number and Nat)
!(assertEqualToResult (lessThan Z 1) ())            ;; Z < 1 (Nat and Number)
!(assertEqualToResult (lessThan 1 Z) ())           ;; 1 < Z (Number and Nat)
!(assertEqualToResult (lessThan (S Z) 1) ())       ;; 1 (Nat) < 1 (Number)
!(assertEqualToResult (lessThan 1 (S Z)) ())       ;; 1 (Number) < 1 (Nat)
!(assertEqualToResult (lessThan (S (S Z)) 3) ())    ;; 2 (Nat) < 3 (Number)
!(assertEqualToResult (lessThan 2 (S (S Z))) ())   ;; 2 (Number) < 2 (Nat)

!("============ End Tests ============")

