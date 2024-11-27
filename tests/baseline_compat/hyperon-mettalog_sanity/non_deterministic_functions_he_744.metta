; Test File: Non-Deterministic Functions and Referential Transparency in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/744

; Issue Overview:
; In MeTTa, non-deterministic functions do not behave in a referentially transparent way when untyped.
; Specifically, untyped arguments are evaluated before the function is called, causing the function to apply
; element-wise to all combinations of argument elements. This results in a different behavior compared to 
; when the function is used directly.

; Example: Demonstrating the Issue
; ---------------------------------

; Top-level call with subtraction and intersection
; This call subtracts the intersection of two sets from one set.
!(assertEqualToResult
   (subtraction (superpose (a b d)) (intersection (superpose (a b c)) (superpose (a b d))))
   (d))
; Explanation:
; The expected result here is `d`, which correctly reflects the subtraction operation.

; Wrapping the logic in a function
; Define an untyped function `test6`
(= (test6 $set $subset) (subtraction $subset (intersection $set $subset)))

; Test: Call the untyped function with the same inputs
!(assertEqualToResult
   (test6 (superpose (a b c)) (superpose (a b d)))
   (b d a d a b d))
; Explanation:
; When `test6` is called without a type declaration, its arguments are evaluated element-wise
; due to the semantics of non-determinism in MeTTa. The function is applied to all combinations
; of elements from `(a b c)` and `(a b d)`, producing a non-deterministic result.

; Example: Fixing the Issue with Typing
; -------------------------------------
; Add a type declaration for `test6`
(: test6 (-> Atom Atom Atom))
(= (test6 $set $subset) (subtraction $subset (intersection $set $subset)))

; Test: Call the typed function with the same inputs
!(assertEqualToResult
   (test6 (superpose (a b c)) (superpose (a b d)))
   (d))
; Explanation:
; By typing the function, the arguments are not evaluated prematurely. Instead, the sets are passed
; as a whole to the function, resulting in the correct and referentially transparent behavior.

; Summary:
; - The issue arises because untyped arguments are evaluated before the function call in MeTTa.
; - This causes the function to behave in a non-deterministic, element-wise manner.
; - Adding a type declaration resolves the issue by preserving referential transparency.
