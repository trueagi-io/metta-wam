; 16:28 18-8-2024
; !(intersect (superpose (a b c d )) (superpose ( c d e f )))


!(assertEqual (intersection (superpose (a b c d )) (superpose ( c d e f ))) (superpose (c d)))

; Intersection Tests with Variable Substitution and Evaluation

;; Test basic intersection functionality
;; This checks that intersecting (a b c d) with (b c) correctly returns (b c).
!(assertEqual
  (intersection (superpose (a b c d)) (superpose (b c)))
  (superpose (b c))
)

;; Test intersection with multiple subsequent lists
;; Here, intersecting (a b c d) with (b c) and then (d) should result in an empty set.
!(assertEqual
  (intersection (intersection (superpose (a b c d)) (superpose (b c))) (superpose (d)))
  (superpose ())
)

;; Test intersection with nested structures
;; This test ensures that nested elements like (foo (bar baz)) are handled correctly.
!(assertEqual
  (intersection (superpose ((foo bar) (bar baz) qux)) (superpose ((bar baz) qux)))
  (superpose ((bar baz) qux))
)

;; Test intersection with mixed types
;; This checks that the function handles lists with symbols, numbers, and mixed content.
!(assertEqual
  (intersection (superpose (1 2 3 foo bar)) (superpose (2 foo)))
  (superpose (2 foo))
)
Test intersection with duplicates
;; This test ensures that duplicates are treated correctly in the intersection.
!(assertEqual
  (intersection (superpose (a b b c d)) (superpose (b c)))
  (superpose (b c))
)

;; Test intersection with `unique` applied outside
;; In this case, `unique` is applied after the intersection, removing duplicates.
!(assertEqual
  (unique (intersection (superpose (a b b c)) (superpose (b c c d))))
  (superpose (b c))
)

;; Test intersection with empty lists
;; Intersecting an empty list should return an empty set.
!(assertEqual
  (intersection (superpose (a b c)) (superpose ()))
  (superpose ())
)

;; Test intersection with disjoint sets
;; This checks that intersecting two disjoint sets returns an empty set.
!(assertEqual
  (intersection (superpose (a b c)) (superpose (x y z)))
  (superpose ())
)

;; Test intersection with overlapping sets
;; This test checks that intersecting sets with some common elements returns only those common elements.
!(assertEqual
  (intersection (superpose (a b c d)) (superpose (b c x y)))
  (superpose (b c))
)

;; Test intersection with identical sets
;; This ensures that intersecting two identical sets returns the original set.
!(assertEqual
  (intersection (superpose (a b c d)) (superpose (a b c d)))
  (superpose (a b c d))
)

;; Test intersection with self-referential structures
;; This test checks the behavior when intersecting a list with itself.
!(assertEqual
  (intersection (superpose (a b c (a b c))) (superpose (a b c (a b c))))
  (superpose (a b c (a b c)))
)

;; Test intersection with a single element set
;; This test ensures that intersecting a set with a single-element set returns the element if it's present.
!(assertEqual
  (intersection (superpose (a b c d)) (superpose (c)))
  (superpose (c))
)

;; Test intersection with a superset and subset
;; This checks that intersecting a superset with its subset returns the subset.
!(assertEqual
  (intersection (superpose (a b c d e f)) (superpose (b c d)))
  (superpose (b c d))
)

;; Test intersection with a mix of lists and individual elements
;; This ensures that mixed types are correctly intersected.
!(assertEqual
  (intersection (superpose (a (b c) d)) (superpose ((b c) e f)))
  (superpose ((b c)))
)

;; Test intersection with nested lists and empty list
;; This checks the behavior when one of the lists is empty, even when nested lists are involved.
!(assertEqual
  (intersection (superpose ((a b) (c d))) (superpose ()))
  (superpose ())
)

;; Test intersection with mixed numeric and symbolic lists
;; This test checks the behavior when intersecting lists containing both numbers and symbols.
!(assertEqual
  (intersection (superpose (1 2 3 foo bar)) (superpose (3 foo baz)))
  (superpose (3 foo))
)

;; Test intersection with variables and nested structures
;; This checks how variables interact with nested lists during intersection.
!(assertEqual
  (intersection (superpose ((foo $x) (bar $y))) (superpose ((bar $y) (foo qux))))
  (superpose ((bar $y)))
)

;; Test intersection with variables and lists
;; This ensures variables within lists are correctly intersected.
!(assertEqual
  (intersection (superpose ($x b c)) (superpose (b $y c)))
  (superpose (b c))
)

;; Test intersection with variables and complex structures
;; This checks that complex structures with variables are correctly intersected.
!(assertEqual
  (intersection (superpose (($x b) (c d))) (superpose ((a $y) (c d))))
  (superpose ((c d)))
)

;; Test intersection with expressions that evaluate to a value
;; This ensures that expressions like (+ 1 2) that unify with 3 are handled correctly.
!(assertEqualToResult
  (pred-intersection unified (superpose (($x (+ 1 2)) (3 4))) (superpose ((3 $y) (4 (+ 1 2)))))
  ((3 3) (3 4) (4 3))
)

;; Test intersection with nested structures and expressions
;; This checks that variables and expressions are correctly intersected within nested structures.
!(assertEqual
  (intersection (superpose ((foo (+ 1 2)) (bar $y))) (superpose ((foo 3) (bar baz))))
  (superpose ((foo 3)))
)

;; Test intersection with nested structures and expressions
;; This checks that variables and expressions are correctly intersected within nested structures.
!(assertNotEqual
  (pred-intersection unified (superpose ((foo (+ 1 2)) (bar $y))) (superpose ((foo 3) (bar baz))))
  (superpose ((foo 3) (bar $y)))
)

;; Test intersection with variables and evaluated expressions
;; This ensures that variables and evaluated expressions are correctly intersected.
!(assertEqual
  (intersection (superpose (($x b) (+ 2 2))) (superpose ((a $y) 4)))
  (superpose (4))
)

;; Test intersection with a potential infinite loop due to improper occurs check
;; This test ensures that the intersection function correctly handles cases where a variable
;; might unify with a structure containing that same variable, which should not be allowed.
!(assertEqual
  (intersection (superpose ($x (f $x))) (superpose ((f (f $x)))))
  (superpose ())
)

;; Test intersection with self-referential structures
;; This test ensures that self-referential structures correctly intersect when they should.
!(assertEqual
  (intersection (superpose ($y (f $x))) (superpose ($a (f $b))))
  (superpose ((f $x)))
)

;; Test intersection with self-referential structures
;; This test ensures that self-referential structures correctly intersect when they should.
!(assertEqual
  (intersection (superpose ((f $x) $y )) (superpose ((f $b) $a )))
  (superpose ((f $x)))
)

;; once alpha equivalence is established then the ideities take hold
!(assertEqual 
  (intersection (superpose ((f $x) $x )) (superpose ((f $a) $a )))
  (superpose ((f $x) $x))
)

;; Reversed: alpha equivalence is established too late?
!(assertEqual
  (intersection (superpose ($x (f $x) )) (superpose ($a (f $a))))
  (superpose ($x (f $x)))
)

;; Test intersection with potential variable misidentification
;; This test ensures that variables are correctly identified and don't lead to erroneous unifications.
!(assertEqual
  (intersection (superpose ($y (f $x))) (superpose ($x (f $x))))
  (superpose ((f $x)))
)

;; Test intersection with potential variable misidentification
;; This test ensures that variables are correctly identified and don't lead to erroneous unifications.
!(assertEqual
  (intersection (superpose ($x (f $y))) (superpose ((f $y) $x)))
  (superpose ($x (f $y)))
)



;
; (= (intersection $x $y) (superpose (intersect-elements (collapse $x) (collapse $y))))

; A Test to show we can define a function to be used like how normal people are imagining
(= (intersect-elements $x $y) (collapse (intersection (superpose $x) (superpose $y))))
!(assertEqual (intersect-elements (1 2 3 foo bar) (2 foo)) (2 foo))

"

!(bind! &ctx1 (make-space (f1 f2 f3)))
!(bind! &ctx2 (make-space (f1 f2 f3 f4 f5)))
(intersection (get-atoms &ctx1) (get-atoms &ctx2)) 
    
!(get-atoms &ctx1) -> 1 2 3 4 5 6 7 ... .. ... 
!(get-atoms &ctx2) -> 1 2 3 4 5 6 7 ... .. ... 
  
(= (intersection $x $y) (superpose (intersection-element (collapse $x) (collapse $y))))

(intersection (get-atoms &ctx1) (get-atoms &ctx2)) 
 1 2 3 4 5 ... 

 - Rust Prolog
 - Sicstus CLP(FD) 1000x 
 - GNUProlog
 - XSB-Prolog
"
