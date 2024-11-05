;; nullary

(: foo (-> Number))

!(assertEqualToResult (predicate-arity foo) (1))
!(assertEqualToResult (function-arity foo) (0))

;; multi-arity (unary + binary)

(: bar (-> Number Number))
(: bar (-> Number Number Number))

!(assertEqualToResult (predicate-arity bar) (2 3))
!(assertEqualToResult (function-arity bar) (1 2))

;; changed arity

(: foo (-> Number Number))

!(assertEqualToResult (predicate-arity foo) (1 2))
!(assertEqualToResult (function-arity foo) (0 1))
