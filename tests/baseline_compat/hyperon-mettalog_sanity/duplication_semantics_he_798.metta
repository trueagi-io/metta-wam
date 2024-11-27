; Test File: Duplication Semantics in MeTTa
; Bug Report: https://github.com/trueagi-io/hyperon-experimental/issues/798

; Example 1: Asserting Multiple Identical Atoms
; --------------------------------------------
!(add-atom &self (is ball red))
!(add-atom &self (is ball red))
!(add-atom &self (is ball red))
!(add-atom &self (is ball red))
!(add-atom &self (is ball red))

; Test: Match returns all duplicates
!(assertEqualToResult
   (match &self (is ball $color) $color)
   (red red red red red))

; Example 2: Removing a Duplicated Atom
; -------------------------------------
!(remove-atom &self (is ball red))

; Test: All duplicates are removed
!(assertEqualToResult
   (match &self (is ball $color) $color)
   ())

; Example 3: Space-Specific Behavior
; ----------------------------------
; Assert the same atom in a space again
!(add-atom &self (is ball red))
!(add-atom &self (is ball red))

; Test: Verify duplicates exist
!(assertEqualToResult
   (match &self (is ball $color) $color)
   (red red))

; Remove one instance
!(remove-atom &self (is ball red))

; Test: Current implementation removes all duplicates
!(assertEqualToResult
   (match &self (is ball $color) $color)
   ())

; Example 4: Probabilistic Use Case
; ---------------------------------
!(add-atom &self (observation ball red))
!(add-atom &self (observation ball red))
!(add-atom &self (observation ball red))

; Test: Count duplicates for probabilistic reasoning
!(assertEqualToResult
   (match &self (observation ball $color) $color)
   (red red red))

