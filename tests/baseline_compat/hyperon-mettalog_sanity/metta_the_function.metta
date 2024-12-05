; Create a new space and bind it to &new-space
!(bind! &new-space (new-space))

; Add (likes John Jane) to &new-space
!(add-atom &new-space (likes John Jane))
; Test that matching 'likes' returns (John Jane)
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ((John Jane)))

; Add a definition of foo to &new-space
!(add-atom &new-space (= (foo $x) (+ $x 1)))
; Test that (foo 1) evaluates to 2 in &new-space
!(assertEqualToResult (metta (foo 1) Number &new-space) (2))
