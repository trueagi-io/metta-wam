; =============================================================
; Purpose: Define predicate 'is-even' to check if a number is even.
; Operation: Uses modulo operator to determine if a number is divisible by 2 without remainder.
; Usage: General utility function for checking evenness of numbers.
; =============================================================
; Define 'is-even' predicate
(: is-even (-> Number Bool))
(= (is-even $X) (== (% $X 2) 0))

; =============================================================
; Purpose: Define predicate 'is-odd' to check if a number is odd.
; Operation: Uses modulo operator to determine if a number is divisible by 2 without remainder.
; Usage: General utility function for checking oddness of numbers.
; =============================================================
; Define 'is-odd' predicate
(: is-odd (-> Number Bool))
(= (is-odd $X) (== (% $X 2) 1))

; =============================================================
; Purpose: Define function 'plus-ternary' to add three numbers
; =============================================================
(: plus-ternary (-> Number Number Number Number))
(= (plus-ternary $N1 $N2 $N3) (+ $N1 (+ $N2 $N3)))

; =============================================================
; maplist! tests
; =============================================================

;; test maplist! with unary function

!(assertEqualToResult (maplist! is-even (1 2 3)) ((False True False)))

;; test maplist! with binary function

!(assertEqualToResult (maplist! < (1 2 3) (2 2 2)) ((True False False)))

;; test maplist! with ternary function

!(assertEqualToResult (maplist! plus-ternary (1 2 3) (4 5 6) (7 8 9)) ((12 15 18)))

;; test recursive maplist!

!(assertEqualToResult (maplist! maplist! (< >) ((1 2 3) (1 2 3)) ((2 2 2) (2 2 2))) (((True False False) (False False True))))

;; the arguments are not evaluated

(: my-list (-> Expression))
(= (my-list) (1 2 3))

!(assertEqualToResult (maplist! get-type (my-list)) (((-> Expression))))

; =============================================================
; concurrent-maplist! tests
; =============================================================

;; test concurrent-maplist! with unary function

!(assertEqualToResult (catch (max-time! 1 (concurrent-maplist! is-even (1 2 3))) 
                        time_limit_exceeded time_limit_exceeded)
    ((False True False)))

;; test concurrent-maplist! with binary function

!(assertEqualToResult (catch (max-time! 1 (concurrent-maplist! < (1 2 3) (2 2 2))) 
                        time_limit_exceeded time_limit_exceeded)
    ((True False False)))

;; test concurrent-maplist! with ternary function

;!(assertEqualToResult (catch (max-time! 1 (concurrent-maplist! plus-ternary (1 2 3) (4 5 6) (7 8 9))) 
;                        time_limit_exceeded time_limit_exceeded)
;    ((12 15 18)))

;; test recursive concurrent-maplist!

!(assertEqualToResult (catch (max-time! 1 (concurrent-maplist! concurrent-maplist! (is-even is-odd) ((1 2 3) (1 2 3))))
                        time_limit_exceeded time_limit_exceeded)
    (((False True False) (True False True))))

;; the arguments are not evaluated

!(assertEqualToResult (catch (max-time! 1 (maplist! get-type (my-list)))
                        time_limit_exceeded time_limit_exceeded)
 (((-> Expression))))
