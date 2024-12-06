;; coerce is no-op when type already matches

!(assertEqualToResult (coerce Bool True) (True))
!(assertEqualToResult (coerce Bool False) (False))
!(assertEqualToResult (coerce Number 1) (1))
!(assertEqualToResult (coerce Number 1.5) (1.5))
!(assertEqualToResult (coerce String "string") ("string"))

;; coerce Bool makes 0 and () False, everything else True

!(assertEqualToResult (coerce Bool 0) (False))
!(assertEqualToResult (coerce Bool ()) (False))
!(assertEqualToResult (coerce Bool 1) (True))
!(assertEqualToResult (coerce Bool "string") (True))
!(assertEqualToResult (coerce Bool (1 2 3)) (True))

;; coerce Number handles bools, number strings

!(assertEqualToResult (coerce Number False) (0))
!(assertEqualToResult (coerce Number True) (1))
!(assertEqualToResult (coerce Number "1") (1))
!(assertEqualToResult (coerce Number "1.5") (1.5))
!(assertEqualToResult (coerce Number "2147483647") (2147483647))
!(assertEqualToResult (coerce Number "2147483648") (2147483648))
!(assertEqualToResult (coerce Number "not a number string") ())

;; coerce String acts as if printing object

!(assertEqualToResult (coerce String False) ("'False'"))
!(assertEqualToResult (coerce String True) ("'True'"))
!(assertEqualToResult (coerce String 1) ("1"))
!(assertEqualToResult (coerce String 1.5) ("1.5"))
!(assertEqualToResult (coerce String ()) ("[]"))
!(assertEqualToResult (coerce String (1 2 3)) ("[1,2,3]"))
