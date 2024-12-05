;; define foo returning Number and String
(= (foo) 1)
(= (foo) "a")

;; eval-for returns of appropriate type
!(assertEqualToResult (eval-for Number (foo)) (1))
!(assertEqualToResult (eval-for String (foo)) ("a"))

;; eval-for works with unbound type
!(assertEqualToResult (eval-for $type (foo)) (1 "a"))
!(assertEqualToResult (let $result (eval-for $type (foo)) ($result $type)) ((1 Number) ("a" String)))

;; eval-for works when type arg must be evaluated

(= (type) Number)
!(assertEqualToResult (eval-for (type) (foo)) (1))

(= (type) String)
!(assertEqualToResult (eval-for (type) (foo)) (1 "a"))

;; set up incrementer and define bar returning Number and String
!(bind! &var (new-state 0))
(= (increment)
   (sequential ((do (change-state! &var (+ 1 (get-state &var))))
               (get-state &var))))

(= (bar) (sequential ((do (increment)) 1)))
(= (bar) (sequential ((do (increment)) "a")))

!(assertEqualToResult (get-state &var) (0))

;; eval-for does not re-evaluate paths

!(assertEqualToResult (eval-for $type (bar)) (1 "a"))
!(assertEqualToResult (get-state &var) (2))

!(assertEqualToResult (eval-for (type) (bar)) (1 "a"))
!(assertEqualToResult (get-state &var) (4))

;; eval-for does one evaluation step
(= (chain-to-foo) (foo))

!(assertEqualToResult (chain (eval-for Expression (chain-to-foo)) $x (quote $x)) ((quote (foo))))
!(assertEqualToResult (chain (eval-for $type (chain-to-foo)) $x (quote $x)) ((quote (foo))))
!(assertEqualToResult (chain (eval-for Number (chain-to-foo)) $x (quote $x)) ())
!(assertEqualToResult (chain (eval-for String (chain-to-foo)) $x (quote $x)) ())
