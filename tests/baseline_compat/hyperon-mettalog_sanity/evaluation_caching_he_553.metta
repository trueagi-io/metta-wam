; Test File: Evaluation Caching in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/553

; Issue Overview:
; This test explores unexpected behavior caused by caching in MeTTa, where cached results 
; for expressions lead to incorrect evaluation outcomes. The issue specifically arises when 
; evaluating functions that generate non-deterministic results, such as random numbers.

; Define the Python Module
!(py-module random_binding 
"from hyperon.atoms import OperationAtom
from hyperon.ext import register_atoms
import random

@register_atoms
def my_atoms():
    return {
        'random': OperationAtom('random', random.random),
    }
")

; Example 1: Testing Non-Deterministic Functionality
; ---------------------------------------------------
!(extend-py! random_binding)

; Test: Evaluating `(random)` directly twice
!(assertEqualToResult
   (let $r1 (random) (let $r2 (random) (== $r1 $r2)))
   (False))
; Explanation:
; The `random` function should produce non-deterministic results, so evaluating it twice 
; directly should not return the same value.

; Test: Binding and evaluating `(random)` twice
!(assertEqualToResult
   (let $a (random) (let $b (random) (== $a $b)))
   (False))
; Explanation:
; Assigning `random` to variables `$a` and `$b` and comparing them should result in `False` 
; since the values are generated independently.

; Example 2: Caching Issue with Function Wrapping
; -----------------------------------------------
; Define a function `aa` that wraps `random`
(= (aa) (random))

; Test: Evaluating `(aa)` twice directly
!(assertEqualToResult
   (let $r1 (aa) (let $r2 (aa) (== $r1 $r2)))
   (True))
; Explanation:
; This behavior is caused by caching. The first evaluation of `(aa)` is cached, so subsequent 
; evaluations return the same cached result, which is incorrect for non-deterministic functions.

; Test: Assigning `(aa)` to variables and comparing
!(assertEqualToResult
   (let $a (aa) (let $b (aa) (== $a $b)))
   (True))
; Explanation:
; Again, due to caching, `(aa)` produces the same result when evaluated multiple times, 
; resulting in an incorrect `True` output.

; Example 3: Binding Results to Variables
; ---------------------------------------
!(bind! a1 (aa))
!(bind! a2 (aa))

; Test: Comparing bound variables `a1` and `a2`
!(assertEqualToResult
   (== a1 a2)
   (False))
; Explanation:
; When the results of `(aa)` are explicitly bound to separate variables `a1` and `a2`, 
; caching does not apply, and the correct `False` result is returned as expected.

; Summary:
; - Non-deterministic functions such as `random` should not be cached.
; - The current behavior incorrectly caches results for expressions like `(aa)`, causing 
;   subsequent evaluations to return identical results.
