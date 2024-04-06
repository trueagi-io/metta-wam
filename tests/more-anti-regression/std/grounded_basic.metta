;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Custom functions over grounded functions behave ordinarily
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (sqr $x) (* $x $x))
!(assertEqualToResult
  (sqr 4)
  (16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Evaluation of grounded operations over nondeterministic
; expressions work in the same way as of custom symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual
    (+ (superpose (1 2 3)) 1)
    (superpose (2 3 4)))

(= (bin) 0)
(= (bin) 1)
!(assertEqualToResult
    (+ 1 (bin))
    (1 2))

!(assertEqualToResult
    (let $x (+ 2 3)
        (* $x $x))
    
    (25))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Grounded symbols work with non-determinism based "reasoning"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Uses the grounded call `bin`, which returns a list of
; both possible bits (0, 1), to recursively construct all possible
; binary lists of length `$n`, appending bits one by one.
(= (gen $n)
    (if (> $n 0)
        (:: (bin) (gen (- $n 1)))
        nil))

; Note: `::` is just a custom symbol, used here as a constructor,
; but any other symbol can be used for this.

; Calculates the sum of element-wise products between two lists
(= (subsum nil nil) 0)
(= (subsum (:: $x $xs) (:: $b $bs))
    (+ (* $x $b) 
        (subsum $xs $bs)))

!(subsum (:: 3 (:: 7 (:: 5 nil))) (:: 1 (:: 2 (:: 3 nil))))

; Non-determinism "reasoning":
; Among all 3-bit binary lists, return the one whose `subsum`
; with (:: 3 (:: 7 (:: 5 nil))) equals 8, or `nop` if not found
; (`superpose` is used to return an empty result acting as termination
; of evaluation of the branch)


!(let $t (gen 3)
    (if (== (subsum (:: 3 (:: 7 (:: 5 nil))) $t) 5)
        $t
        (superpose ())))