; Test File: Type Checking for the Symbol Meta-Type in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/673

; Issue Overview:
; The Symbol meta-type is expected to restrict inputs to atomic symbols, but it currently
; does not properly validate arguments, allowing complex expressions to pass type checks.
; This test demonstrates the issue and includes expected behaviors.

; Example 1: Type Checking for Symbols
; ------------------------------------
; Define a function `foo` that accepts only Symbols
(: foo (-> Symbol Type))
(= (foo $x) $x)

; Test: Passing a valid Symbol
!(assertEqualToResult
   (foo S)
   (S))
; Explanation:
; The input `S` is a valid Symbol, so the type check passes and returns `S`.

; Test: Passing an invalid expression
!(assertEqualToResult
   (foo (This is an expression))
   (Error (This is an expression) BadType))
; Explanation:
; The input `(This is an expression)` is not a Symbol, so the type check should fail.

; Example 2: Type Checking for Variables
; --------------------------------------
; Define a function `foo_var` to test Variable meta-type
(: foo_var (-> Variable Type))
(= (foo_var $x) (get-type $x))

; Test: Passing a number to `foo_var`
!(assertEqualToResult
   (foo_var 100)
   (Error 100 BadType))
; Explanation:
; The input `100` is not a Variable, so the type check should fail.

; Test: Passing a valid Variable
!(assertEqualToResult
   (foo_var $x)
   (%Undefined%))
; Explanation:
; The input `$x` is a Variable, and since it is unbound, `get-type` returns `%Undefined%`.

; Test: Passing a Symbol to `foo_var`
!(assertEqualToResult
   (foo_var S)
   (Error S BadType))
; Explanation:
; The input `S` is not a Variable, so the type check should fail.

; Test: Passing a complex expression to `foo_var`
!(assertEqualToResult
   (foo_var (an expression))
   (Error (an expression) BadType))
; Explanation:
; The input `(an expression)` is not a Variable, so the type check should fail.

; Example 3: Testing with Undefined Types
; ---------------------------------------
; Test: Check type of an expression directly
!(assertEqualToResult
   (get-type (This is an expression))
   (%Undefined%))
; Explanation:
; The type of `(This is an expression)` is `%Undefined%`, as it is not explicitly typed.

; Example 4: Suggested Fix Behavior
; ---------------------------------
; Test: After fix, ensure proper type checking for Symbols
(: foo_fixed (-> Symbol Type))
(= (foo_fixed $x) $x)

!(assertEqualToResult
   (foo_fixed (This is an expression))
   (Error (This is an expression) BadType))
; Explanation:
; If fixed, this behavior should be consistent with the expected type checking for Symbols.

; Summary:
; - Current behavior allows complex expressions to pass type checks incorrectly.
; - Expected behavior restricts inputs to valid Symbols or Variables based on meta-type.
﻿; Test: Passing an undefined type to `foo`
!(assertEqualToResult
   (foo (%Undefined%))
   ((Error (%Undefined%) BadType)))
; Explanation:
; `%Undefined%` is not a valid Symbol, so the type check fails and returns an error wrapped in a list.

; Test: Using `foo_var` with a valid variable
!(assertEqualToResult
   (foo_var $valid_variable)
   ((%Undefined%)))
; Explanation:
; `$valid_variable` is a valid Variable, but since it is unbound, its type is `%Undefined%`.

; Test: Using `foo_var` with a partially typed tuple
!(assertEqualToResult
   (foo_var (typed $x))
   ((Error (typed $x) BadType)))
; Explanation:
; The tuple `(typed $x)` is not a valid Variable and should return an error wrapped in a list.

; Test: Confirming type for direct input
!(assertEqualToResult
   (get-type $x)
   ((%Undefined%)))
; Explanation:
; Since `$x` is an unbound Variable, its type is `%Undefined%` and should be returned in a list.
