; Test File: Idiomatic Negation and Set Difference in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/805

; Create a new space for testing
!(bind! &test-space (new-space))

; Example 1: Friend of Bob AND a Carpenter
; ---------------------------------------
(add-atom &test-space (friend bob larry))
(add-atom &test-space (friend bob mike))
(add-atom &test-space (carpenter larry))
(add-atom &test-space (teacher bob))
(add-atom &test-space (doctor mike))

; Test: Friend of Bob AND a carpenter
!(assertEqualToResult
   (match &test-space (friend bob $f) (match &test-space (carpenter $f) $f))
   (larry))

; Example 2: Friend of Bob AND NOT a Carpenter
; --------------------------------------------
; Test: Friend of Bob AND NOT a carpenter
!(assertEqualToResult
   (unify &test-space
     (friend bob $f)
     (unify &test-space (carpenter $f) (empty) $f)
     (empty))
   (mike))

; Example 3: Mammals That ARE NOT Pets
; ------------------------------------
(add-atom &test-space (mammal dog))
(add-atom &test-space (mammal cat))
(add-atom &test-space (mammal tiger))
(add-atom &test-space (pet dog))
(add-atom &test-space (pet cat))
(add-atom &test-space (bird parrot))
(add-atom &test-space (bird vulture))
(add-atom &test-space (pet parrot))

; Test: Mammals that ARE NOT pets
!(assertEqualToResult
   (unify &test-space
     (mammal $m)
     (unify &test-space (pet $m) (empty) $m)
     (empty))
   (tiger))

; Example 4: OR/Union Query
; -------------------------
; Test: Mammals OR Birds
!(assertEqualToResult
   (superpose (
      (unify &test-space (mammal $m) $m (empty))
      (unify &test-space (bird $b) $b (empty))
   ))
   (tiger cat dog vulture parrot))

; Example 5: Everything EXCEPT Pets
; ---------------------------------
; Test: Returns anything that is NOT a pet
!(assertEqualToResult
   (unify &test-space
     ($p $s)
     (unify &test-space (pet $s) (empty) $s)
     (empty))
   (tiger vulture))

