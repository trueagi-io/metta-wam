;; Test basic subtraction functionality
;; This checks that subtracting (b c) from (a b c d) correctly returns (a d).
!(assertEqual
  (subtraction (superpose (a b c d)) (superpose (b c)))
  (superpose (a d))
)

;; Test subtraction with multiple subsequent lists
;; Here, subtracting (b c) and (d) from (a b c d) should result in (a).
!(assertEqual
  (subtraction (subtraction (superpose (a b c d)) (superpose (b c))) (superpose (d)))
  (superpose (a))
)

;; Test subtraction with nested structures
;; This test ensures that nested elements like (foo (bar baz)) are handled correctly.
!(assertEqual
  (subtraction (superpose ((foo bar) (bar baz) qux)) (superpose ((bar baz) qux)))
  (superpose ((foo bar)))
)

;; Test subtraction with mixed types
;; This checks that the function handles lists with symbols, numbers, and mixed content.
!(assertEqual
  (subtraction (superpose (1 2 3 foo bar)) (superpose (2 foo)))
  (superpose (1 3 bar))
)

;; Test subtraction with duplicates
;; This test ensures that duplicates are treated correctly.
!(assertEqual
  (subtraction (superpose (a b b c d)) (superpose (b c)))
  (superpose (a d))
)

;; Test subtraction with `unique` applied outside
;; In this case, `unique` is applied after the subtraction, removing duplicates.
!(assertEqual
  (unique (subtraction (superpose (a b b c)) (superpose (b c c d))))
  (superpose (a))
)

;; Test subtraction with `unique` applied inside
;; In this test, `unique` is applied to each input set before performing the subtraction.
!(assertEqual
  (subtraction (unique (superpose (a b b c))) (unique (superpose (b c c d))))
  (superpose (a))
)

;; Test variable substitution during subtraction
;; This ensures that variables can be used and are properly instantiated during the operation.
!(assertEqual
  (subtraction (superpose ($x $y)) (superpose (b)))
  (superpose ($x $y))
)

;; Test subtraction with empty lists
;; Subtracting an empty list should return the original list.
!(assertEqual
  (subtraction (superpose (a b c)) (superpose ()))
  (superpose (a b c))
)

;; Test subtraction with variables and nested structures
;; This checks how variables interact with nested lists during subtraction.
!(assertEqual
  (subtraction (superpose ((foo $x) (bar $y))) (superpose ((bar $y) (foo qux))))
  (superpose ((foo $x)))
)

