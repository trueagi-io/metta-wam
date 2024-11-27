; Test File: Function Overloading Bug with Structured Arguments

; https://github.com/trueagi-io/hyperon-experimental/issues/630

; Test 1: Unary lambda
; ---------------------
(: λ (-> Variable Atom Atom))
(= ((λ $x $f) $y) (let $x $y $f))

; Test: Call unary lambda with one argument
!(assertEqualToResult
   ((λ $x (+ 1 $x)) 1)
   (2))

; Test 2: Binary lambda
; ----------------------
(: λ (-> Variable Variable Atom Atom Atom))
(= ((λ $x $y $f) $z $w) (let* (($x $z) ($y $w)) $f))

; Test: Call binary lambda with two arguments
!(assertEqualToResult
   ((λ $x $y (+ $x $y)) 1 2)
   (3))

; Bug Validation: Ensure unary and binary lambda results do not duplicate
; ------------------------------------------------------------
; Expected Results:
; - Unary lambda should return [2]
; - Binary lambda should return [3]
; - Should not return duplicates, e.g., [3, 3]
!(assertEqualToResult
   ((λ $x $y (+ $x $y)) 1 2)
   (3)) ; Ensures no duplicates

; Test 3: Properly working overloaded functions (control group)
; -------------------------------------------------------------

; Unary foo
(: foo (-> Number Number))
(= (foo $x) (+ 1 $x))

; Test: Call unary foo
!(assertEqualToResult
   (foo 1)
   (2))

; Binary foo
(: foo (-> Number Number Number))
(= (foo $x $y) (+ $x $y))

; Test: Call binary foo
!(assertEqualToResult
   (foo 1 2)
   (3))

; Test 4: Comparison of structured vs non-structured overloads
; -------------------------------------------------------------
; Ensures structured `λ` functions produce correct results without duplication
!(assertEqualToResult
   ((λ $x (+ 1 $x)) 1)
   (2))

!(assertEqualToResult
   ((λ $x $y (+ $x $y)) 1 2)
   (3))

