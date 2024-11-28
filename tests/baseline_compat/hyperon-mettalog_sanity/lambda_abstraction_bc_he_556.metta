; Test File: Backward Chaining on Lambda Abstraction in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/556

; Issue Overview:
; The MeTTa interpreter previously crashed when performing backward chaining on lambda abstraction.
; This test reproduces the original conditions to validate that the issue is resolved.

; Example 1: Knowledge Base Setup
; --------------------------------
!(bind! &kb (new-space))

; Add successor function
!(add-atom &kb (: S (-> (: $x Nat) Nat)))

; Add a generic variable
!(add-atom &kb (: $x Var))

; Add lambda abstraction
!(add-atom &kb (: λ (-> (: $x Var) (-> (: $y $b) (-> (: $x $a) $b)))))

; Example 2: Backward Chainer Definitions
; ---------------------------------------
; Base case: Query the knowledge base for a rule or fact
(= (bc (: $prf $ccln) $_)
   (match &kb (: $prf $ccln) (: $prf $ccln)))

; Recursive step: Recurse on proof abstraction and argument
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (;; Recurse on proof abstraction
          ((: $prfabs (-> (: $prfarg $prms) $ccln))
           (bc (: $prfabs (-> (: $prfarg $prms) $ccln)) $k))
          ;; Recurse on proof argument
          ((: $prfarg $prms)
           (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

; Example 3: Tests
; ----------------
; Test: Prove unary functions over natural numbers with depth 2
!(assertEqualToResult
   (bc (: $prf (-> (: $k Nat) Nat)) (S (S Z)))
   ())
; Explanation:
; The backward chainer should not crash and return an empty result when the depth is 2.

; Test: Prove unary functions over natural numbers with depth 5
!(assertEqualToResult
   (bc (: $prf (-> (: $k Nat) Nat)) (S (S (S (S (S Z))))))
   ())
; Explanation:
; The backward chainer should not crash, even at a depth of 5. This test validates the fix.

; Summary:
; - Previously, the interpreter crashed with backward chaining on lambda abstraction at certain depths.
; - The tests validate that backward chaining functions correctly without runtime errors at various depths.
