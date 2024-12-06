;; create a new space, add to it, query it

!(assertEqualToResult (bind! &new-space (new-space)) (()))
!(assertEqualToResult (add-atom &new-space (likes John Jane)) (()))
!(assertEqualToResult (match &new-space (likes $x $y) ($x $y)) ((John Jane)))
!(assertEqualToResult (match &self (likes $x $y) ($x $y)) ())

;; modify atoms in new space

!(assertEqualToResult (add-atom &new-space (likes Kate Karl)) (()))
!(assertEqualToResult (match &new-space (likes $x $y) ($x $y)) ((John Jane) (Kate Karl)))

!(assertEqualToResult (remove-atom &new-space (likes John Jane)) (()))
!(assertEqualToResult (match &new-space (likes $x $y) ($x $y)) ((Kate Karl)))

;; evaluate in new space with metta function

!(assertEqualToResult (add-atom &new-space (= (foo $x) (+ $x 1))) (()))
!(assertEqualToResult (metta (foo 1) Number &new-space) (2))

;; modify function definition in new space

!(assertEqualToResult (add-atom &new-space (= (foo $x) (+ $x 2))) (()))
!(assertEqualToResult (metta (foo 1) Number &new-space) (2 3))

!(assertEqualToResult (remove-atom &new-space (= (foo $x) (+ $x 1))) (()))
!(assertEqualToResult (metta (foo 1) Number &new-space) (3))
