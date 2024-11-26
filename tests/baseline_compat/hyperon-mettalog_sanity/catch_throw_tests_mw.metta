;; when no exception is thrown, catch returns result of Form

(= (foo) 1)
(= (foo) 2)

!(assertEqualToResult (catch (foo) Error Error) (1 2))

;; catch handles throw

!(assertEqualToResult (catch (throw Error) Error "An exception was caught") ("An exception was caught"))

;; the innermost catch that unifies with the thrown exception is used

!(assertEqualToResult (catch (catch (throw Error) Error "Error level 2") Error "Error level 1") ("Error level 2"))
!(assertEqualToResult (catch (catch (throw Error1) Error2 "Error level 2") Error1 "Error level 1") ("Error level 1"))

;; a catch that itself throws an exception

!(assertEqualToResult (catch (catch (throw Error2) Error2 (throw Error1)) Error1 "Error level 1") ("Error level 1"))

;; catch handles mix of exceptional and unexceptional paths
;;; this test case may change if result order should be nondeterministic, or catch should find all results

(= (bar) 1)
(= (bar) 2)
(= (bar) (throw Error))
(= (bar) 4)

!(assertEqualToResult (catch (bar) Error "An exception was caught") (1 2 "An exception was caught"))

;; catch/throw unification

!(assertEqualToResult (catch (throw (Error "This is an error message")) (Error $message) $message) ("This is an error message"))

;; catcher (catch arg2) not evaluated

!(assertEqualToResult (catch (catch (throw 3) (+ 1 2) "Error level 2") 3 "Error level 1") ("Error level 1"))

;; exception (throw arg) is evaluated

!(assertEqualToResult (catch (catch (throw (+ 1 2)) (+ 1 2) "Error level 2") 3 "Error level 1") ("Error level 1"))

;; recover (catch arg3) is evaluated if exception is caught

!(assertEqualToResult (catch (throw (Error 2)) (Error $x) (+ $x 1)) (3))
