;; define foo

(: foo (-> Number Number))
(= (foo $x) (+ $x 1))

!(assertEqualToResult (foo 1) (2))
!(assertEqualToResult (foo 2) (3))
!(assertEqualToResult (get-type foo) (-> Number Number))

;; foo compiles successfully
!(assertEqualToResult (compile! foo) (()))

;; foo still works the same
!(assertEqualToResult (foo 1) (2))
!(assertEqualToResult (foo 2) (3))
!(assertEqualToResult (get-type foo) (-> Number Number))

;; foo's definition doesn't change
(= (foo $x) (+ $x 2))
!(assertEqualToResult (foo 1) (2))
!(assertEqualToResult (foo 2) (3))

!(remove-atom &self (= (foo $x) (+ $x 1)))
!(assertEqualToResult (foo 1) (2))
!(assertEqualToResult (foo 2) (3))

;; neither does the type definition

!(remove-atom &self (= (foo $x) (+ $x 2)))
!(remove-atom &self (: foo (-> Number Number)))
(: foo (-> String Expression))
(= (foo $x) (stringToChars $x))

!(assertEqualToResult (get-type foo) (-> Number Number))
!(assertEqualToResult (foo 1) (2))
!(assertEqualToResult (foo 2) (3))

;; create new space, define bar + foobar in that space
!(bind! &compile-test-space (new-space))

!(add-atom &compile-test-space (: bar (-> Number Number)))
!(add-atom &compile-test-space (= (bar $x) (* $x 2)))

!(add-atom &compile-test-space (: foobar (-> Number Number)))
!(add-atom &compile-test-space (= (foobar $x) (* (+ $x 1) 2)))

!(assertEqualToResult (metta (bar 2) Number &compile-test-space) (4))
!(assertEqualToResult (metta (foobar 2) Number &compile-test-space) (6))

;; space compiles successfully
!(assertEqualToResult (compile-space! &compile-test-space) (()))

;; bar + foobar still work the same
!(assertEqualToResult (metta (bar 2) Number &compile-test-space) (4))
!(assertEqualToResult (metta (foobar 2) Number &compile-test-space) (6))

;; their definitions don't change
!(add-atom &compile-test-space (= (bar $x) (* $x 3)))
!(remove-atom &compile-test-space (= (bar $x) (* $x 2)))
!(add-atom &compile-test-space (= (foobar $x) (* (+ $x 2) 2)))
!(remove-atom &compile-test-space (= (foobar $x) (* (+ $x 1) 2)))

!(assertEqualToResult (metta (bar 2) Number &compile-test-space) (4))
!(assertEqualToResult (metta (foobar 2) Number &compile-test-space) (6))
