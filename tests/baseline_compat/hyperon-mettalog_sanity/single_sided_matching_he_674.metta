; Test File: Single-Sided Matching in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/674

; Issue Overview:
; MeTTa currently uses double-sided matching as the default, where both sides of a pattern
; can contribute substitutions during unification. Single-sided matching, commonly used
; in functional programming and combinatory logic, only allows one side (the "pattern") to
; generate substitutions. This test demonstrates the current behavior and highlights the
; need for single-sided matching.

; Supporting Functions for Single-Sided Matching
; ----------------------------------------------

; Define a grounded function for single-sided matching
(: single-side-match (-> Atom Atom))
(= (single-side-match $expr)
   (grounded-single-side-match $expr))

; Define assertSingleSideEqualToResult for testing single-sided matching
(: assertSingleSideEqualToResult (-> Atom Atom Atom))
(= (assertSingleSideEqualToResult $expr $expected)
   (let
      $result (single-side-match $expr)
      (if (== $result $expected)
          (Success)
          (Error (assertSingleSideEqualToResult $expr $expected $result)))))

; Example 1: Current Double-Sided Matching
; ----------------------------------------
; Define a reduction rule for SKI combinatory logic
(= ((S K) K) I)

; Test: Matching ((S K) $x) with double-sided unification
!(assertEqualToResult
   ((S K) $x)
   (I))
; Explanation:
; The current implementation substitutes `$x` from the pattern `(S K) K` and reduces to `I`.
; However, this is incorrect for SKI combinatory logic, where single-sided matching is required.

; Example 2: Expected Single-Sided Matching Behavior
; --------------------------------------------------
; Test: Matching ((S K) $x) with single-sided unification (using single-side-match)
!(assertSingleSideEqualToResult
   ((S K) $x)
   ((S K) $x))
; Explanation:
; Single-sided matching would leave the expression unreduced, as `$x` does not unify with
; any specific term in the pattern `(S K) K`.

; Example 3: Infinite Recursion Prevention
; ----------------------------------------
; Define a reduction rule for a symmetry operator
(= (: (Sym (: (Sym (: $x $a)) $b)) $c) (: $x $a))

; Test: Current double-sided behavior leads to infinite recursion
!(assertEqualToResult
   (: (Sym (: (Sym (: $x $a)) $b)) $c)
   (InfiniteRecursion))
; Explanation:
; Double-sided matching causes the pattern to reapply itself infinitely.

; Test: Single-sided matching avoids infinite recursion (using single-side-match)
!(assertSingleSideEqualToResult
   (: (Sym (: (Sym (: $x $a)) $b)) $c)
   (: $x $a))
; Explanation:
; With single-sided matching, the reduction applies only once, avoiding infinite recursion.

; Example 4: Differentiating Entry Points
; ---------------------------------------
; Define a function with two entry points: one double-sided, one single-sided
(= (foo (A $x)) (B $x))  ; Double-sided
(=^ (foo (C $x)) (D $x)) ; Hypothetical single-sided operator

; Test: Double-sided entry point
!(assertEqualToResult
   (foo $x)
   ((B $x)))

; Test: Single-sided entry point triggers only when explicitly matched
!(assertSingleSideEqualToResult
   (foo (C $x))
   ((D $x)))
; Explanation:
; The single-sided entry point should only trigger when `(C $x)` matches explicitly.

; Summary:
; - Current double-sided matching reduces terms aggressively, even when substitutions are undesired.
; - Single-sided matching, as demonstrated in the hypothetical `assertSingleSideEqualToResult`,
;   would give developers control over when reductions occur.
; - This feature would prevent infinite recursion, align MeTTa with combinatory logic semantics,
;   and allow finer control over pattern matching.
