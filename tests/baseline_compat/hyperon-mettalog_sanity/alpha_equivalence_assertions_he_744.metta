; Test File: Alpha Equivalence Assertions in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/744

; Example 1: Basic Alpha Equivalence
; ----------------------------------
; Define a simple function
(= (foo) (R $x $y))

; Test: AssertEqual fails without alpha-equivalence
!(assertEqual (foo) (R $x $y))
; Expected: [(Error (assertEqual (foo) (R $x $y)) 
; Expected: [(R $x $y)]
; Got: [(R $x#31 $y#32)]
; Missed result: (R $x $y))]

; Test: AssertAlphaEqual should pass with alpha-equivalence support
!(assertAlphaEqual (foo) (R $x $y))

; Example 2: Nested Alpha Equivalence
; -----------------------------------
; Define a function with nested variables
(= (bar) (S $x (T $y)))

; Test: AssertAlphaEqual should correctly identify equivalence
!(assertAlphaEqual (bar) (S $a (T $b)))

; Example 3: Complex Structures
; -----------------------------
; Define a complex structure
(= (baz) (U (R $x $y) (V $z)))

; Test: Alpha equivalence for complex structures
!(assertAlphaEqual (baz) (U (R $a $b) (V $c)))

; Example 4: Non-equivalent Structures
; ------------------------------------
; Test: Ensure non-equivalent structures are not considered equivalent
!(assertNotAlphaEqual (baz) (U (R $a $b) (W $c)))
