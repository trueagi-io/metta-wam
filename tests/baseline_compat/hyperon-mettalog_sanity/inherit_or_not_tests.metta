; Create a new space and bind it to &new-space
!(bind! &new-space (new-space))

; Declare that John likes Jess
(likes John Jess)
; Define function foo that adds 2 to its argument
(= (foo $x) (+ $x 2))

; Test that matching 'likes' in &new-space yields no results
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ())
; Test that evaluating (foo 1) in &new-space returns (foo 1)
!(assertEqualToResult (metta (foo 1) Number &new-space) ((foo 1)))

; Add (likes John Jane) to &new-space
!(add-atom &new-space (likes John Jane))
; Test that matching 'likes' now returns (John Jane)
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ((John Jane)))

; Add a new definition of foo to &new-space
!(add-atom &new-space (= (foo $x) (+ $x 1)))
; Test that (foo 1) now evaluates to 2 in &new-space
!(assertEqualToResult (metta (foo 1) Number &new-space) (2))

; Add the current self context to &new-space
!(add-atom &new-space &self)

; Test that matching 'likes' returns both (John Jess) and (John Jane)
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ((John Jess) (John Jane)))

; Test that (foo 1) now evaluates to both 2 and 3 in &new-space
!(assertEqualToResult (metta (foo 1) Number &new-space) (2 3))

; Remove the self context from &new-space
!(remove-atom &new-space &self)
; Test that after removal, (foo 1) evaluates _ONLY_ to 2 in &new-space
!(assertEqualToResult (metta (foo 1) Number &new-space) (2))
; Test that matching 'likes' now returns only (John Jane)
!(assertEqualToResult (metta (match &self (likes $x $y) ($x $y)) Atom &new-space) ((John Jane)))
