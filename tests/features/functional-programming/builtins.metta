; =============================================================
; Purpose: Define predicate 'is-even' to check if a number is even.
; Operation: Uses modulo operator to determine if a number is divisible by 2 without remainder.
; Usage: General utility function for checking evenness of numbers.
; =============================================================
; Define 'is-even' predicate
(: is-even (-> Number Bool))
(= (is-even $X) (== (% $X 2) 0))

; =============================================================
; Purpose: Define function 'plus-ternary' to add three numbers
; =============================================================
(: plus-ternary (-> Number Number Number Number))
(= (plus-ternary $N1 $N2 $N3) (+ $N1 (+ $N2 $N3)))

;; test maplist! with unary function

!(assertEqualToResult (maplist! is-even (1 2 3)) ((False True False)))

;; test maplist! with binary function

!(assertEqualToResult (maplist! < (1 2 3) (2 2 2)) ((True False False)))

;; test maplist! with ternary function

!(assertEqualToResult (maplist! plus-ternary (1 2 3) (4 5 6) (7 8 9)) ((12 15 18)))

;; test recursive maplist!

!(assertEqual (maplist! maplist! (< >) ((1 2 3) (1 2 3)) ((2 2 2) (2 2 2))) ((True False False) (False False True)))

;; the arguments are not evaluated

(: my-list (-> Expression))
(= (my-list) (1 2 3))

!(assertEqualToResult (maplist! get-type (my-list)) (((-> Expression))))