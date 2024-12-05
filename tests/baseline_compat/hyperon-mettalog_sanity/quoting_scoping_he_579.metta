; Test File: Quoting/Scoping Mechanism in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/579

; Issue Overview:
; This test demonstrates scenarios where unification and substitution of variables in MeTTa
; lead to unexpected results. It highlights the need for a quoting/scoping mechanism to
; prevent unification/substitution in specific contexts.

; Example 1: Basic Quoting Issue
; --------------------------------
!(assertEqualToResult
   (let (quote $x) (quote A) $x)
   (A))
; Explanation:
; Without a scoping mechanism, `quote` does not isolate `$x`, and substitution occurs.

!(assertEqualToResult
   (let (quote $x) (quote (+ 1 1)) $x)
   (2))
; Explanation:
; `quote` fails to preserve the quoted structure, and evaluation proceeds, producing `2`.

; Example 2: Real-World Example with Combinatory Logic
; ---------------------------------------------------
; Knowledge Base Setup
!(bind! &kb (new-space))

; Add K and S combinators to the knowledge base
!(add-atom &kb (: AK (-> $a (-> $b $a))))
!(add-atom &kb (: AS (-> (-> $a (-> $b $c)) (-> (-> $a $b) (-> $a $c)))))

; Define Backward Chainer
; Base case
(= (bc (: $prf $ccln) $_)
   (match &kb (: $prf $ccln) (: $prf $ccln)))

; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (
         ((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
         ((: $prfarg $prms) (bc (: $prfarg $prms) $k))
     )
     (: ($prfabs $prfarg) $ccln)))

; Synthesize function composition (.)
!(assertEqualToResult
   (bc (: $prg (-> (-> $b $c) (-> (-> $a $b) (-> $a $c)))) (S (S (S Z))))
   ((AS (AK AS)) AK)) ; Expected solution

; Test with maximum depth 0 to prevent variable generalization
!(assertEqualToResult
   (bc (: $prg (-> (-> $b $c) (-> (-> $a $b) (-> $a $c)))) Z)
   (AK)) ; Demonstrates a specific form due to absence of scoping.

; Example 3: Proposed Solution with Scoped Variables
; --------------------------------------------------
; Ideally, a scoping mechanism would enable queries like:
; !(bc (: $prg (∀ $a (∀ $b (∀ $c (-> (-> $b $c) (-> (-> $a $b) (-> $a $c))))))) Z)
; Such scoping would restrict solutions to fully generic combinatory logic structures.

; Summary:
; - These examples demonstrate the need for a quoting/scoping mechanism in MeTTa.
; - Scoping would allow more precise reasoning about programs and type variables.
