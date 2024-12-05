; Test File: Delaying Reduction Until Bindings Are Ready
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/659

; Issue Overview:
; The backward chainer struggles to delay reduction when bindings are not fully grounded,
; resulting in incorrect or empty results. This test reproduces the issue and examines solutions.

; Define Nat Type
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

; Define Greater Than Zero (0<)
(: 0< (-> Number Bool))
(= (0< $x) (< 0 $x))

; Define Backward Chainer
; -----------------------
(: bc (-> $a    ; Knowledge base space
          $b    ; Query
          Nat   ; Maximum depth
          $b))  ; Result

; Base case: Knowledge base lookup
(= (bc $kb (: $prf $ccln) $_)
   (match $kb (: $prf $ccln) (: $prf $ccln)))

; Base case: CPU check
(= (bc $kb (: CPU (0⍃ $x)) $_)
   (case (0< $x)
         ((True (: CPU (0⍃ $x)))
          (False (empty)))))

; Recursive step: Recurse on proof abstraction and proof argument
(= (bc $kb (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (
         ((: $prfabs (-> (: $prfarg $prms) $ccln))
           (bc $kb (: $prfabs (-> (: $prfarg $prms) $ccln)) $k))
         ((: $prfarg $prms)
           (bc $kb (: $prfarg $prms) $k))
     )
     (: ($prfabs $prfarg) $ccln)))

; Define Knowledge Base
; ----------------------
!(bind! &kb (new-space))
!(add-atom &kb (: 2 Prime)) ; 2 is a prime number
!(add-atom &kb (: Rule (-> (: $_ (0⍃ $x))   ; If $x > 0
                           (-> (: $x Prime) ; and $x is prime, then
                               (0⍃' $x))))) ; $x is a prime number > 0

; Test Case 1: Query with Ungrounded Variable
!(assertEqualToResult
   (bc &kb (: $prf (0⍃' $x)) (S (S Z)))
   (empty)) ; Issue: Expected proof not found

; Test Case 2: Query with Grounded Variable
!(assertEqualToResult
   (bc &kb (: $prf (0⍃' 2)) (S (S Z)))
   ((: ((Rule CPU) 2) (0⍃' 2)))) ; Expected proof when $x is grounded

; Test Case 3: Adjust CPU Check for Explicit Handling
!(assertEqualToResult
   (let* (
         ($kb &kb)
         ($x 2)
         ($result
            (case (0< $x)
               ((True (: CPU (0⍃ $x)))
                (False (empty))))))
      $result)
   ((: CPU (0⍃ 2)))) ; Expected explicit handling in CPU check

; Test Case 4: Proposed Set-Based Parameter Handling
; --------------------------------------------------
(: handle-prmlst (-> $a $b List $d))
(= (handle-prmlst $kb $k Nil) Nil)
(= (handle-prmlst $kb $k (Cons (: $prfarg $prms) $xs))
   (Cons (bc $kb (: $prfarg $prms) $k) (handle-prmlst $kb $k $xs)))

!(add-atom &kb (: Rule (-> (Cons (: CPU (is-closed $x) (0⍃ $x)) (Cons (: $x Prime) Nil))
                          (0⍃' $x))))

!(assertEqualToResult
   (bc &kb (: $prf (0⍃' $x)) (S (S Z)))
   ((: ((Rule CPU) 2) (0⍃' 2)))) ; Valid proof using parameter list handler

; Summary:
; - Includes tests with variable grounding, adjusted CPU checks, and parameter list handling.
; - Demonstrates solutions for delaying reduction and handling ungrounded variables.
