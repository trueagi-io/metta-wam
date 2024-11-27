; Test File: Delaying Reduction Until Bindings Are Ready
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/659

; Issue Overview:
; The backward chainer struggles to delay reduction when bindings are not fully grounded,
; resulting in incorrect or empty results. This test reproduces the issue and validates behavior.

; Define Nat Type
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

; Define Greater Than Zero (0<)
(: 0< (-> Number Bool))
(= (0< $x) (< 0 $x))

; Define Backward Chainer
; -----------------------
(: bc (-> $a                           ; Knowledge base space
          $b                           ; Query
          Nat                          ; Maximum depth
          $b))                         ; Result

; Base case: Knowledge base lookup
(= (bc $kb (: $prf $ccln) $_)
   (match $kb (: $prf $ccln) (: $prf $ccln)))

; Base case: CPU check
(= (bc $kb (: CPU (0⍃ $x)) $_)
   (if (0< $x) (: CPU (0⍃ $x)) (empty)))

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

; Test Case 3: Query with Reordered Premises
!(add-atom &kb (: Rule (-> (: $x Prime)       ; If $x is prime
                           (-> (: $_ (0⍃ $x)) ; and $x > 0, then
                               (0⍃' $x)))))   ; $x is a prime number > 0
!(assertEqualToResult
   (bc &kb (: $prf (0⍃' $x)) (S (S Z)))
   ((: ((Rule CPU) 2) (0⍃' 2)))) ; Expected proof with reordered premises

; Test Case 4: Using is-closed to Delay Reduction
(: is-closed (-> Atom Bool))
(= (is-closed $x)
   (case (get-metatype $x)
         ((Symbol True)
          (Grounded True)
          (Variable False)
          (Expression (if (== $x ())
                         True
                         (and (let $head (car-atom $x) (is-closed $head))
                              (let $tail (cdr-atom $x) (is-closed $tail))))))))

(: 0< (-> Number Bool))
(= (0< $x) (if (is-closed $x) (< 0 $x) (empty)))

!(assertEqualToResult
   (bc &kb (: $prf (0⍃' $x)) (S (S Z)))
   ((: ((Rule CPU) 2) (0⍃' 2)))) ; Expected behavior with is-closed applied

; Summary:
; - Highlights the need for delaying reduction until bindings are fully grounded.
; - Includes tests with variable grounding, reordered premises, and an `is-closed` mechanism.
