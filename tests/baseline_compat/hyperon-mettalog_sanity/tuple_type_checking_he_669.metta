; Test: Passing an incorrectly typed tuple to `foo_fixed`
!(assertEqualToResult
   (foo_fixed (wrong type))
   ((Error (wrong type) BadType)))
; Explanation:
; The tuple `(wrong type)` does not match the expected tuple type `(A B)`, so the type check fails,
; and an error wrapped in a list is returned.

; Test: Checking a tuple with mixed undefined and defined types
(: foo_mixed (-> (%Undefined% T) %Undefined%))
(= (foo_mixed $x) $x)

!(assertEqualToResult
   (foo_mixed (partially_defined %Undefined%))
   ((Error (partially_defined %Undefined%) BadType)))
; Explanation:
; The tuple `(partially_defined %Undefined%)` fails to meet the type requirements and returns
; an error in a list.

; Test: Recursive factorial definition with stricter type checking
(: fact_strict (-> Integer Integer))
(= (fact_strict $n) (if (== $n 0) 1 (* (fact_strict (- $n 1)) $n)))

!(assertEqualToResult
   (fact_strict 5)
   ((120)))
; Explanation:
; Stricter type checking for the recursive factorial function ensures that input is properly typed.

; Test: Attempting to call `fact_strict` with a tuple
!(assertEqualToResult
   (fact_strict (5 6))
   ((Error (5 6) BadType)))
; Explanation:
; The tuple `(5 6)` is not a valid Integer, so the type check fails, and the error is returned in a list.
