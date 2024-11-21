;; Basic evaluation

!(assertEqualToResult (rust !(+ 1 2)) (3))
!(assertEqualToResult (rust! (+ 1 2)) (3))

;; Rust uses a separate atomspace

;; Match

!(assertEqualToResult (rust (likes John Jane)) ())
!(assertEqualToResult (rust !(match &self (likes John $who) $who)) (Jane))
!(assertEqualToResult (rust! (match &self (likes John $who) $who)) (Jane))
!(assertEqualToResult (match &self (likes John $who) $who) ())

;; Function evaluation

!(assertEqualToResult (rust (= (foo $x) (+ $x 1))) ())
!(assertEqualToResult (rust !(foo 1)) (2))
!(assertEqualToResult (rust! (foo 1)) (2))
!(assertEqualToResult (foo 1) ((foo 1)))
