

(: list-to-set (-> Expression Expression))
(= (list-to-set $list)
    (collapse (unique (superpose $list))))

;; Test unique with simple duplicates
;; This test verifies that `unique` removes all duplicates from (a b b c c c d), resulting in [a, b, c, d].
!(assertEqual
  (unique (superpose (a b b c c c d)))
  (superpose (a b c d))
)

;; Test unique with no duplicates
;; This test ensures that `unique` leaves a list unchanged if it already contains only unique elements.
!(assertEqual
  (unique (superpose (a b c d)))
  (superpose (a b c d))
)

;; Test unique with a single element
;; This checks that `unique` works correctly for a single-element list, leaving it unchanged as [a].
!(assertEqual
  (unique (superpose (a)))
  (superpose (a))
)

;; Test unique with an empty list
;; This confirms that `unique` handles an empty list without errors, correctly returning an empty result.
!(assertEqual
  (unique (superpose ()))
  (superpose ())
)

;; Test unique with all elements being the same
;; This verifies that when all elements are the same (a a a a), `unique` reduces it to a single element [a].
!(assertEqual
  (unique (superpose (a a a a)))
  (superpose (a))
)

;; Test unique with nested lists
;; This ensures that unique identifies identical nested lists and reduces them to a single instance.
!(assertEqual
  (unique (superpose ((1 2) (1 2) (3 4))))
  (superpose ((1 2) (3 4)))
)

;; Test unique with mixed types
;; This test checks that `unique` handles different data types correctly and removes duplicates while preserving the order.
!(assertEqual
  (unique (superpose (1 "hello" 1 "world" "hello" 3.14 3.14)))
  (superpose (1 "hello" "world" 3.14))
)

;; Test list-to-set with simple duplicates
;; This test ensures that list-to-set correctly collapses duplicates into a unique set.
!(assertEqual
  (list-to-set (a b b c c c d))
  (a b c d)
)

;; Test list-to-set with nested lists
;; This test checks that list-to-set properly collapses duplicates within nested lists.
!(assertEqual
  (list-to-set ((1 2) (1 2) (3 4)))
  ((1 2) (3 4))
)

;; Test unique with variables
;; This test checks that unique handles variables correctly, ensuring that each unique instance is preserved.
!(assertEqual
  (unique (superpose ($x $y $x $z)))
  (superpose ($x $y $z))
)

;; Test unique with variables in nested lists
;; This test checks that unique correctly handles variables within nested lists, preserving each unique nested structure.
!(assertEqual
  (unique (superpose ((:: $x $y) (:: $x $z) (:: $x $y) (:: $a $b))))
  (superpose ((:: $x $y) (:: $x $z) (:: $a $b)))
)

;; Test unique with repeated variables in nested lists
;; This test ensures that unique treats identical nested lists with variables as duplicates, reducing them to a single instance.
!(assertEqual
  (unique (superpose ((:: $x $y) (:: $x $y) (:: $x $z) (:: $x $y))))
  (superpose ((:: $x $y) (:: $x $z)))
)

;; Test unique with mixed types and variables in nested lists
;; This test checks that unique handles a mix of nested structures with variables, symbols, and other data types correctly.
!(assertEqual
  (unique (superpose ((:: 1 $x) (:: $x "hello") (:: 1 $x) (:: $y $z) (:: $x "hello"))))
  (superpose ((:: 1 $x) (:: $x "hello") (:: $y $z)))
)

;; Test unique with complex nested structures including variables
;; This test verifies that unique handles more complex nested structures containing variables, preserving each unique structure.
!(assertEqual
  (unique (superpose ((:: $x (:: a b)) (:: $x (:: a b)) (:: (:: 1 $x) (:: 2 $y)) (:: (:: 1 $x) (:: 2 $y)))))
  (superpose ((:: $x (:: a b)) (:: (:: 1 $x) (:: 2 $y))))
)


