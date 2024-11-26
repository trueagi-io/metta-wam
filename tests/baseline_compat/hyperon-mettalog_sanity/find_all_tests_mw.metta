; =============================================================
; Purpose: Define predicate 'is-even' to check if a number is even.
; Operation: Uses modulo operator to determine if a number is divisible by 2 without remainder.
; Usage: General utility function for checking evenness of numbers.
; =============================================================
; Define 'is-even' predicate
(: is-even (-> Number Bool))
(= (is-even $X) (== (% $X 2) 0))

;; add some "knowledge" for testing purposes
(my-numbers 1)
(my-numbers 2)
(my-numbers 7)
(my-numbers 212)
(my-pairs 2 4)
(my-pairs 4 8)

;; basic findall
!(assertEqualToResult (findall! $X (match &self (my-numbers $X) $X)) ((1 2 7 212)))

;; second arg of match doesn't matter, just the variable bindings
!(assertEqualToResult (findall! $X (match &self (my-numbers $X) (this-predicate-doesnt-matter $X))) ((1 2 7 212)))

;; results are further evaluated
!(assertEqualToResult (findall! (is-even $X) (match &self (my-numbers $X) $X)) ((False True False True)))

;; multiple bindings
!(assertEqualToResult (findall! ($X $Y) (match &self (my-pairs $X $Y) (this-predicate-doesnt-matter $X $Y))) (((2 4) (4 8))))
!(assertEqualToResult (findall! ($Y $X) (match &self (my-pairs $X $Y) (this-predicate-doesnt-matter $X $Y))) (((4 2) (8 4))))

;; multiple bindings further evaluation
!(assertEqualToResult (findall! (/ $X $Y) (match &self (my-pairs $X $Y) (this-predicate-doesnt-matter $X $Y))) ((0.5 0.5)))
!(assertEqualToResult (findall! (/ $Y $X) (match &self (my-pairs $X $Y) (this-predicate-doesnt-matter $X $Y))) ((2 2)))
