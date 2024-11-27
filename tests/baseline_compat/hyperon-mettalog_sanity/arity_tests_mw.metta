; Testing `predicate-arity` and `function-arity`

; Create a new space for testing
!(bind! &test-space (new-space))

; Test nullary function
(: foo (-> Number))
!(assertEqualToResult (predicate-arity foo) (1))
!(assertEqualToResult (function-arity foo) (0))

; Test multi-arity function (unary and binary)
(: bar (-> Number Number))
(: bar (-> Number Number Number))
!(assertEqualToResult (predicate-arity bar) (2 3))
!(assertEqualToResult (function-arity bar) (1 2))

; Test changed arity for `foo`
(: foo (-> Number Number))
!(assertEqualToResult (predicate-arity foo) (1 2))
!(assertEqualToResult (function-arity foo) (0 1))

; Example 1: Testing `predicate-arity`
; -----------------------------------

; Enable the built-in function `size-atom` as a predicate with arity 2
(add-atom &test-space (predicate-arity size-atom 2))

; Test: Match using `size-atom` as a predicate
!(assertEqualToResult 
   (match &test-space (size-atom (a b c) $size) $size)
   (3))

; Test: Reverse execution using `size-atom`
; This functionality is just part of how size-atom was designed to run bi-directionaly
!(assertEqualToResult 
   (match &test-space (size-atom $new-atom 4) $new-atom)
   (($1 $2 $3 $4)))


; Example 2: Testing `predicate-arity`
; -----------------------------------

; Define rules for `max`
(add-atom &test-space (max $X $Y $X) (<= $X $Y))
(add-atom &test-space (max $X $Y $Y) (> $X $Y))
; Declare `max` predicate with arity 3
(add-atom &test-space (predicate-arity max 3))
; Test: Use `max` declaratively as a predicate
!(assertEqualToResult 
   (match &test-space (max 5 10 $max) $max)
   (10))

; Test: Reverse execution using `max`
; This functionality uses mettalog's builtin finite domain equational solver
!(assertEqualToResult 
   (match &test-space (max $a $b 10) ($a $b))
   (
     ((<exists> (<= 10 $)) 10) ; these can return in reverse order
     (10 (<exists> (<= 10 $)))  
   ))


; Example 3: Testing `function-arity`
; -----------------------------------
; Enable `max` as a callable function
(add-atom &test-space (function-arity max 2))

; Test: Use `max` procedurally as a function
!(assertEqualToResult 
   (max 5 10) 
   (10))

; Test: Use `max` procedurally as reversable a function
; This functionality uses mettalog's builtin finite domain equational solver
!(assertEqualToResult 
    (let ($_ ($_  $a $b) $_)
         (== (max $a $b) 10)
                 ($a $b)     )
   (
     ((<exists> (<= 10 $)) 10)
     (10 (<exists> (<= 10 $)))  
   ))



