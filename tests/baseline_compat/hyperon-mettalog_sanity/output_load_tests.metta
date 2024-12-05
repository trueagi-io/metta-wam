(= (filename) "output_load_tests.tmp") ;; perhaps this should be uniquified?

;; write new file
(= (foo 1) (print "(bar 1)"))
(= (foo 2) (print "(bar 2)"))
!(assertEqualToResult (let $s (open! (filename) write)
                           (call-cleanup! (with-output-to! $s (foo $x)) (close! $s)))
                      ("(bar 1)" "(bar 2)"))
!(assertEqualToResult (let $f (filename) (load-file! &self $f)) (()))
!(assertEqualToResult (match &self (bar $x) $x) (1 2))

;; cleanup
!(remove-atom &self (= (foo 1) (print "(bar 1)")))
!(remove-atom &self (= (foo 2) (print "(bar 2)")))
!(remove-atom &self (bar 1))
!(remove-atom &self (bar 2))
!(assertEqualToResult (match &self (bar $x) $x) ())

;; append adds to end
(= (foo 3) (print "(bar 3)"))
(= (foo 4) (print "(bar 4)"))
!(assertEqualToResult (let $s (open! (filename) append) 
                           (call-cleanup! (with-output-to! $s (foo $x)) (close! $s))) 
                      ("(bar 3)" "(bar 4)"))
!(assertEqualToResult (let $f (filename) (load-file! &self $f)) (()))
!(assertEqualToResult (match &self (bar $x) $x) (1 2 3 4))

;; cleanup
!(remove-atom &self (= (foo 3) (print "(bar 3)")))
!(remove-atom &self (= (foo 4) (print "(bar 4)")))
!(remove-atom &self (bar 1))
!(remove-atom &self (bar 2))
!(remove-atom &self (bar 3))
!(remove-atom &self (bar 4))
!(assertEqualToResult (match &self (bar $x) $x) ())

;; update overwrites but does not clear
(= (foo 5) (print "(bar 5)"))
(= (foo 6) (print "(bar 6)"))
!(assertEqualToResult (let $s (open! (filename) update) 
                           (call-cleanup! (with-output-to! $s (foo $x)) (close! $s))) 
                      ("(bar 5)" "(bar 6)"))
!(assertEqualToResult (let $f (filename) (load-file! &self $f)) (()))
!(assertEqualToResult (match &self (bar $x) $x) (3 4 5 6))

;; cleanup
!(remove-atom &self (= (foo 5) (print "(bar 5)")))
!(remove-atom &self (= (foo 6) (print "(bar 6)")))
!(remove-atom &self (bar 3))
!(remove-atom &self (bar 4))
!(remove-atom &self (bar 5))
!(remove-atom &self (bar 6))
!(assertEqualToResult (match &self (bar $x) $x) ())

;; write clears existing file
(= (foo 1) (print "(bar 1)"))
(= (foo 2) (print "(bar 2)"))
!(assertEqualToResult (let $s (open! (filename) write)
                           (call-cleanup! (with-output-to! $s (foo $x)) (close! $s)))
                      ("(bar 1)" "(bar 2)"))
!(assertEqualToResult (let $f (filename) (load-file! &self $f)) (()))
!(assertEqualToResult (match &self (bar $x) $x) (1 2))

;; test save-space!

!(bind! &gen-space (new-space))
!(bind! &load-space (new-space))

!(add-atom &gen-space (likes John Jane))
!(add-atom &gen-space (= (foo $x) (+ $x 1)))

!(assertEqualToResult (match &gen-space (likes $x $y) ($x $y)) ((John Jane)))
!(assertEqualToResult (metta (foo 1) Number &new-space) (2))

!(assertEqualToResult (save-space! &gen-space "save-space-test.tmp") (()))
!(assertEqualToResult (load-file! &load-space "save-space-test.tmp") (()))

!(assertEqualToResult (match &load-space (likes $x $y) ($x $y)) ((John Jane)))
!(assertEqualToResult (metta (foo 1) Number &load-space) (2))
