!(bind! &new-space (new-space))

(likes John Jess)
(= (foo $x) (+ $x 2))

; space should still be blank
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ())
!(assertEqualToResult (metta (foo 1) Number &new-space) ((foo 1)))

; space should still have the likes/2
!(add-atom &new-space (likes John Jane))
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ((John Jane)))

!(add-atom &new-space (= (foo $x) (+ $x 1)))
; space should still have the likes/2
!(assertEqualToResult (metta (foo 1) Number &new-space) (2))


!(add-atom &new-space &self)

!(add-atom &new-space (likes John Jane))
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ((John Jess) (John Jane)))

!(add-atom &new-space (= (foo $x) (+ $x 1)))
!(assertEqualToResult (metta (foo 1) Number &new-space) (2 3))
