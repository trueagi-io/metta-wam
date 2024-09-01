


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converted Rust Tests with Original Source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_incorrect_args() {
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(eval)")),
;;         vec![expr!("Error" ("eval") "expected: (eval <atom>), found: (eval)")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(eval a b)")),
;;         vec![expr!("Error" ("eval" "a" "b") "expected: (eval <atom>), found: (eval a b)")]);
;; }

;; Converted:
!(assertEqual (eval) ("Error" (eval) "expected: (eval <atom>), found: (eval)"))
!(assertEqual (eval a b) ("Error" (eval a b) "expected: (eval <atom>), found: (eval a b)"))

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_atom() {
;;     let result = call_interpret(&space("(= a b)"), &metta_atom("(eval a)"));
;;     assert_eq!(result, vec![metta_atom("b")]);
;; }

;; Converted:
(= a b)
!(assertEqual (eval a) b)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_atom_no_definition() {
;;     let result = call_interpret(&space(""), &metta_atom("(eval a)"));
;;     assert_eq!(result, vec![metta_atom("NotReducible")]);
;; }

;; Converted:
!(assertEqual (eval a) NotReducible)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_empty_expression() {
;;     let result = call_interpret(&space(""), &metta_atom("(eval ())"));
;;     assert_eq!(result, vec![metta_atom("NotReducible")]);
;; }

;; Converted:
!(assertEqual (eval ()) NotReducible)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_grounded_value() {
;;     let result = call_interpret(&space(""), &expr!("eval" {6}));
;;     assert_eq!(result, vec![metta_atom("NotReducible")]);
;; }

;; Converted:
!(assertEqual (eval {6}) NotReducible)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_pure_expression() {
;;     let space = space("(= (foo $a B) $a)");
;;     let result = call_interpret(&space, &metta_atom("(eval (foo A $b))"));
;;     assert_eq!(result, vec![metta_atom("A")]);
;; }

;; Converted:
(= (foo $a B) $a)
!(assertEqual (eval (foo A $b)) A)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_pure_expression_non_determinism() {
;;     let space = space("
;;         (= color red)
;;         (= color green)
;;         (= color blue)
;;     ");
;;     let result = call_interpret(&space, &metta_atom("(eval color)"));
;;     assert_eq_no_order!(result, vec![
;;         metta_atom("red"),
;;         metta_atom("green"),
;;         metta_atom("blue"),
;;     ]);
;; }

;; Converted:
(= color red)
(= color green)
(= color blue)
!(assertEqualNoOrder (eval color) (red green blue))

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_pure_expression_no_definition() {
;;     let result = call_interpret(&space(""), &metta_atom("(eval (foo A))"));
;;     assert_eq!(result, vec![metta_atom("NotReducible")]);
;; }

;; Converted:
!(assertEqual (eval (foo A)) NotReducible)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_pure_expression_variable_in_space() {
;;     let space = space("$t (= (foo $a B) $a)");
;;     let result = call_interpret(&space, &metta_atom("(eval (foo A $b))"));
;;     assert_eq!(result, vec![metta_atom("A")]);
;; }

;; Converted:
$t
(= (foo $a B) $a)
!(assertEqual (eval (foo A $b)) A)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_pure_expression_variable_name_conflict() {
;;     let space = space("(= (foo ($W)) True)");
;;     let result = call_interpret(&space, &metta_atom("(eval (foo $W))"));
;;     assert_eq!(result[0], sym!("True"));
;; }

;; Converted:
(= (foo ($W)) True)
!(assertEqual (eval (foo $W)) True)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_grounded_expression() {
;;     let result = call_interpret(&space(""), &expr!("eval" ({MulXUndefinedType(7)} {6})));
;;     assert_eq!(result, vec![expr!({42})]);
;; }

;; Converted:
!(assertEqual (eval (MulXUndefinedType 7) 6) 42)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_grounded_expression_empty() {
;;     let result = call_interpret(&space(""), &expr!("eval" ({ReturnNothing()} {6})));
;;     assert_eq!(result, vec![]);
;; }

;; Converted:
!(assertEqual (eval (ReturnNothing 6)) ())

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_grounded_expression_noreduce() {
;;     let result = call_interpret(&space(""), &expr!("eval" ({NonReducible()} {6})));
;;     assert_eq!(result, vec![expr!("NotReducible")]);
;; }

;; Converted:
!(assertEqual (eval (NonReducible 6)) NotReducible)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_grounded_expression_error() {
;;     let result = call_interpret(&space(""), &expr!("eval" ({ThrowError()} {"Test error"})));
;;     assert_eq!(result, vec![expr!("Error" ({ThrowError()} {"Test error"}) "Test error")]);
;; }

;; Converted:
!(assertEqual (eval (Error ThrowError "Test error")) (Error (Error ThrowError "Test error") "Test error"))

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_variable_operation() {
;;     let space = space("(= (foo $a B) $a)");
;;     let result = call_interpret(&space, &metta_atom("(eval ($a A $b))"));
;;     #[cfg(feature = "variable_operation")]
;;     assert_eq!(result, vec![metta_atom("A")]);
;;     #[cfg(not(feature = "variable_operation"))]
;;     assert_eq!(result, vec![NOT_REDUCIBLE_SYMBOL]);
;; }

;; Converted:
(= (foo $a B) $a)
!(assertEqual (eval ($a A $b)) A)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_variable_via_call_direct_equality() {
;;     let space = space("
;;         (= (bar) (function (return ())))
;;         (= (foo $b) (function
;;           (chain (eval (bar)) $_
;;           (unify $b value
;;             (return ())
;;             (return (Error () \"Unexpected error\")) ))))");
;;     let result = call_interpret(&space,
;;         &metta_atom("(chain (eval (foo $a)) $_ $a)"));
;;     assert_eq!(result[0], sym!("value"));
;; }

;; Converted:
(= (bar) (function (return ())))
(= (foo $b) (function
  (chain (eval (bar)) $_
    (unify $b value
      (return ())
      (return (Error () "Unexpected error"))))))
!(assertEqual (chain (eval (foo $a)) $_ $a) value)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_variable_via_call_struct_equality() {
;;     let formal_arg_struct = space("
;;         (= (bar) (function (return ())))
;;         (= (foo ($b)) (function
;;           (chain (eval (bar)) $_
;;           (unify $b value
;;             (return ())
;;             (return (Error () \"Unexpected error\")) ))))");
;;     let result = call_interpret(&formal_arg_struct,
;;         &metta_atom("(chain (eval (foo $a)) $_ $a)"));
;;     assert_eq!(result[0], expr!(("value")));
;;
;;     let actual_arg_struct = space("
;;         (= (bar) (function (return ())))
;;         (= (foo $b) (function
;;           (chain (eval (bar)) $_
;;           (unify $b (value)
;;             (return ())
;;             (return (Error () \"Unexpected error\")) ))))");
;;     let result = call_interpret(&actual_arg_struct,
;;         &metta_atom("(chain (eval (foo ($a))) $_ $a)"));
;;     assert_eq!(result[0], sym!("value"));
;; }

;; Converted:
(= (bar) (function (return ())))
(= (foo ($b)) (function
  (chain (eval (bar)) $_
    (unify $b value
      (return ())
      (return (Error () "Unexpected error"))))))
!(assertEqual (chain (eval (foo $a)) $_ $a) ("value"))

(= (bar) (function (return ())))
(= (foo $b) (function
  (chain (eval (bar)) $_
    (unify $b (value)
      (return ())
      (return (Error () "Unexpected error"))))))
!(assertEqual (chain (eval (foo ($a))) $_ $a) value)

;; Original:
;; #[test]
;; fn interpret_atom_evaluate_variable_operation_nested() {
;;     let space = space("(= ((baz $a) $b) ($a $b))");
;;     let result = call_interpret(&space, &metta_atom("(eval (($a A) B))"));
;;     #[cfg(feature = "variable_operation")]
;;     assert_eq!(result, vec![metta_atom("(A B)")]);
;;     #[cfg(not(feature = "variable_operation"))]
;;     assert_eq!(result, vec![NOT_REDUCIBLE_SYMBOL]);
;; }

;; Converted:
(= ((baz $a) $b) ($a $b))
!(assertEqual (eval (($a A) B)) (A B))

;; Original:
;; #[test]
;; fn interpret_atom_chain_incorrect_args() {
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(chain n $v t o)")),
;;         vec![expr!("Error" ("chain" "n" v "t" "o") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v t o)")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(chain n v t)")),
;;         vec![expr!("Error" ("chain" "n" "v" "t") "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n v t)")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(chain n $v)")),
;;         vec![expr!("Error" ("chain" "n" v) "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v)")]);
;; }

;; Converted:
!(assertEqual (chain n $v t o) ("Error" (chain n v t o) "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v t o)"))
!(assertEqual (chain n v t) ("Error" (chain n v t) "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n v t)"))
!(assertEqual (chain n $v) ("Error" (chain n v) "expected: (chain <nested> (: <var> Variable) <templ>), found: (chain n $v)"))

;; Original:
;; #[test]
;; fn interpret_atom_chain_atom() {
;;     let result = call_interpret(&space(""), &expr!("chain" ("A" () {6} y) x ("bar" x)));
;;     assert_eq!(result, vec![expr!("bar" ("A" () {6} y))]);
;; }

;; Converted:
!(assertEqual (chain (A () {6} y) x (bar x)) (bar (A () {6} y)))

;; Original:
;; #[test]
;; fn interpret_atom_chain_evaluation() {
;;     let space = space("(= (foo $a B) $a)");
;;     let result = call_interpret(&space, &metta_atom("(chain (eval (foo A $b)) $x (bar $x))"));
;;     assert_eq!(result, vec![metta_atom("(bar A)")]);
;; }

;; Converted:
(= (foo $a B) $a)
!(assertEqual (chain (eval (foo A $b)) $x (bar $x)) (bar A))

;; Original:
;; #[test]
;; fn interpret_atom_chain_nested_evaluation() {
;;     let space = space("(= (foo $a B) $a)");
;;     let result = call_interpret(&space, &metta_atom("(chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y))"));
;;     assert_eq!(result, vec![metta_atom("(baz (bar A))")]);
;; }

;; Converted:
(= (foo $a B) $a)
!(assertEqual (chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y)) (baz (bar A)))

;; Original:
;; #[test]
;; fn interpret_atom_chain_nested_value() {
;;     let result = call_interpret(&space(""), &metta_atom("(chain (chain A $x (bar $x)) $y (baz $y))"));
;;     assert_eq!(result, vec![metta_atom("(baz (bar A))")]);
;; }

;; Converted:
!(assertEqual (chain (chain A $x (bar $x)) $y (baz $y)) (baz (bar A)))

;; Original:
;; #[test]
;; fn interpret_atom_chain_expression_non_determinism() {
;;     let space = space("
;;         (= (color) red)
;;         (= (color) green)
;;         (= (color) blue)
;;     ");
;;     let result = call_interpret(&space, &metta_atom("(chain (eval (color)) $x (bar $x))"));
;;     assert_eq_no_order!(result, vec![
;;         metta_atom("(bar red)"),
;;         metta_atom("(bar green)"),
;;         metta_atom("(bar blue))"),
;;     ]);
;; }

;; Converted:
(= (color) red)
(= (color) green)
(= (color) blue)
!(assertEqualNoOrder (chain (eval (color)) $x (bar $x)) ((bar red) (bar green) (bar blue)))

;; Original:
;; #[test]
;; fn interpret_atom_chain_return() {
;;     let result = call_interpret(&space(""), &metta_atom("(chain Empty $x (bar $x))"));
;;     assert_eq!(result, vec![metta_atom("(bar Empty)")]);
;; }

;; Converted:
!(assertEqual (chain Empty $x (bar $x)) (bar Empty))

;; Original:
;; #[test]
;; fn interpret_atom_chain_keep_var_from_evaluated_part() {
;;     let result = call_interpret(&space("(= (even 4) True)"), &metta_atom("(chain (eval (even $x)) $res (= (is-even $x) $res))"));
;;     assert_eq!(result, vec![metta_atom("(= (is-even 4) True)")]);
;; }

;; Converted:
(= (even 4) True)
!(assertEqual (chain (eval (even $x)) $res (= (is-even $x) $res)) (= (is-even 4) True))

;; Original:
;; #[test]
;; fn interpret_atom_unify_incorrect_args() {
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(unify a p t e o)")),
;;         vec![expr!("Error" ("unify" "a" "p" "t" "e" "o") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t e o)")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(unify a p t)")),
;;         vec![expr!("Error" ("unify" "a" "p" "t") "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t)")]);
;; }

;; Converted:
!(assertEqual (unify a p t e o) ("Error" (unify a p t e o) "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t e o)"))
!(assertEqual (unify a p t) ("Error" (unify a p t) "expected: (unify <atom> <pattern> <then> <else>), found: (unify a p t)"))

;; Original:
;; #[test]
;; fn interpret_atom_unify_then() {
;;     let result = call_interpret(&space(""), &metta_atom("(unify (A $b) ($a B) ($a $b) Empty)"));
;;     assert_eq!(result, vec![metta_atom("(A B)")]);
;; }

;; Converted:
!(assertEqual (unify (A $b) ($a B) ($a $b) Empty) (A B))

;; Original:
;; #[test]
;; fn interpret_atom_unify_else() {
;;     let result = call_interpret(&space(""), &metta_atom("(unify (A $b C) ($a B D) ($a $b) Empty)"));
;;     assert_eq!(result, vec![]);
;; }

;; Converted:
!(assertEqual (unify (A $b C) ($a B D) ($a $b) Empty) ())

;; Original:
;; #[test]
;; fn interpret_atom_decons_atom_incorrect_args() {
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(decons-atom a)")),
;;         vec![expr!("Error" ("decons-atom" "a") "expected: (decons-atom (: <expr> Expression)), found: (decons-atom a)")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(decons-atom (a) (b))")),
;;         vec![expr!("Error" ("decons-atom" ("a") ("b")) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom (a) (b))")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(decons-atom)")),
;;         vec![expr!("Error" ("decons-atom") "expected: (decons-atom (: <expr> Expression)), found: (decons-atom)")]);
;; }

;; Converted:
!(assertEqual (decons-atom a) ("Error" (decons-atom a) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom a)"))
!(assertEqual (decons-atom (a) (b)) ("Error" (decons-atom (a) (b)) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom (a) (b))"))
!(assertEqual (decons-atom) ("Error" (decons-atom) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom)"))

;; Original:
;; #[test]
;; fn interpret_atom_decons_atom_empty() {
;;     let result = call_interpret(&space(""), &metta_atom("(decons-atom ())"));
;;     assert_eq!(result, vec![expr!("Error" ("decons-atom" ()) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom ())")]);
;; }

;; Converted:
!(assertEqual (decons-atom ()) ("Error" (decons-atom ()) "expected: (decons-atom (: <expr> Expression)), found: (decons-atom ())"))

;; Original:
;; #[test]
;; fn interpret_atom_decons_atom_single() {
;;     let result = call_interpret(&space(""), &metta_atom("(decons-atom (a))"));
;;     assert_eq!(result, vec![metta_atom("(a ())")]);
;; }

;; Converted:
!(assertEqual (decons-atom (a)) (a ()))

;; Original:
;; #[test]
;; fn interpret_atom_decons_atom_list() {
;;     let result = call_interpret(&space(""), &metta_atom("(decons-atom (a b c))"));
;;     assert_eq!(result, vec![metta_atom("(a (b c))")]);
;; }

;; Converted:
!(assertEqual (decons-atom (a b c)) (a (b c)))

;; Original:
;; #[test]
;; fn interpret_atom_cons_atom_incorrect_args() {
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(cons-atom a (e) o)")),
;;         vec![expr!("Error" ("cons-atom" "a" ("e") "o") "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a (e) o)")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(cons-atom a e)")),
;;         vec![expr!("Error" ("cons-atom" "a" "e") "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a e)")]);
;;     assert_eq!(call_interpret(&space(""), &metta_atom("(cons-atom a)")),
;;         vec![expr!("Error" ("cons-atom" "a") "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a)")]);
;; }

;; Converted:
!(assertEqual (cons-atom a (e) o) ("Error" (cons-atom a (e) o) "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a (e) o)"))
!(assertEqual (cons-atom a e) ("Error" (cons-atom a e) "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a e)"))
!(assertEqual (cons-atom a) ("Error" (cons-atom a) "expected: (cons-atom <head> (: <tail> Expression)), found: (cons-atom a)"))

;; Original:
;; #[test]
;; fn interpret_atom_cons_atom_empty() {
;;     let result = call_interpret(&space(""), &metta_atom("(cons-atom a ())"));
;;     assert_eq!(result, vec![metta_atom("(a)")]);
;; }

;; Converted:
!(assertEqual (cons-atom a ()) (a))

;; Original:
;; #[test]
;; fn interpret_atom_cons_atom_single() {
;;     let result = call_interpret(&space(""), &metta_atom("(cons-atom a (b))"));
;;     assert_eq!(result, vec![metta_atom("(a b)")]);
;; }

;; Converted:
!(assertEqual (cons-atom a (b)) (a b))

;; Original:
;; #[test]
;; fn interpret_atom_cons_atom_list() {
;;     let result = call_interpret(&space(""), &metta_atom("(cons-atom a (b c))"));
;;     assert_eq!(result, vec![metta_atom("(a b c)")]);
;; }

;; Converted:
!(assertEqual (cons-atom a (b c)) (a b c))

;; Original:
;; #[test]
;; fn test_superpose_bind() {
;;     let vars: Variables = [ "a", "b", "c" ].into_iter().map(VariableAtom::new).collect();
;;     let atom = Atom::expr([Atom::sym("superpose-bind"),
;;         Atom::expr([atom_bindings_into_atom(expr!("foo" a b), bind!{ a: expr!("A"), c: expr!("C") })])]);
;;     let stack = Stack{ prev: None, atom, ret: no_handler, finished: false, vars: vars.clone() };
;;
;;     let result = superpose_bind(stack, bind!{ b: expr!("B"), d: expr!("D") });
;;
;;     assert_eq!(result, vec![InterpretedAtom(
;;             Stack{ prev: None, atom: expr!("foo" a b), ret: no_handler, finished: true, vars: Variables::new() },
;;             bind!{ a: expr!("A"), b: expr!("B"), c: expr!("C"), d: expr!("D") }
;;     )]);
;; }

;; Converted:
!(assertEqual (superpose-bind (foo A B) (bind b: B d: D)) (InterpretedAtom (foo A B) (bind a: A b: B c: C d: D)))

;; Original:
;; #[test]
;; fn metta_turing_machine() {
;;     let space = space("
;;         (= (tm $rule $state $tape)
;;           (function (eval (tm-body $rule $state $tape))) )
;;
;;         (= (tm-body $rule $state $tape)
;;           (unify $state HALT
;;             (return $tape)
;;             (chain (eval (read $tape)) $char
;;               (chain (eval ($rule $state $char)) $res
;;                 (unify $res ($next-state $next-char $dir)
;;                   (chain (eval (move $tape $next-char $dir)) $next-tape
;;                     (eval (tm-body $rule $next-state $next-tape)) )
;;                   (return (Error (tm-body $rule $state $tape) \"Incorrect state\")) )))))
;;
;;         (= (read ($head $hole $tail)) $hole)
;;
;;         (= (move ($head $hole $tail) $char N) ($head $char $tail))
;;         (= (move ($head $hole $tail) $char L) (function
;;           (chain (cons-atom $char $head) $next-head
;;             (chain (decons-atom $tail) $list
;;               (unify $list ($next-hole $next-tail)
;;                 (return ($next-head $next-hole $next-tail))
;;                 (return ($next-head 0 ())) )))))
;;         (= (move ($head $hole $tail) $char R) (function
;;           (chain (cons-atom $char $tail) $next-tail
;;             (chain (decons-atom $head) $list
;;               (unify $list ($next-hole $next-head)
;;                 (return ($next-head $next-hole $next-tail))
;;                 (return (() 0 $next-tail)) )))))
;;
;;         (= (busy-beaver A 0) (B 1 R))
;;         (= (busy-beaver A 1) (C 1 L))
;;
;;         (= (busy-beaver B 0) (A 1 L))
;;         (= (busy-beaver B 1) (B 1 R))
;;
;;         (= (busy-beaver C 0) (B 1 L))
;;         (= (busy-beaver C 1) (HALT 1 N))
;;
;;     ");
;;     let result = interpret(space, &metta_atom("(eval (tm busy-beaver A (() 0 ())))"));
;;     assert_eq!(result, Ok(vec![metta_atom("((1 1) 1 (1 1 1))")]));
;; }

;; Converted:
(= (tm $rule $state $tape)
  (function (eval (tm-body $rule $state $tape))))
(= (tm-body $rule $state $tape)
  (unify $state HALT
    (return $tape)
    (chain (eval (read $tape)) $char
      (chain (eval ($rule $state $char)) $res
        (unify $res ($next-state $next-char $dir)
          (chain (eval (move $tape $next-char $dir)) $next-tape
            (eval (tm-body $rule $next-state $next-tape)))
          (return (Error (tm-body $rule $state $tape) "Incorrect state")))))))
(= (read ($head $hole $tail)) $hole)
(= (move ($head $hole $tail) $char N) ($head $char $tail))
(= (move ($head $hole $tail) $char L)
  (function
    (chain (cons-atom $char $head) $next-head
      (chain (decons-atom $tail) $list
        (unify $list ($next-hole $next-tail)
          (return ($next-head $next-hole $next-tail))
          (return ($next-head 0 ())))))))
(= (move ($head $hole $tail) $char R)
  (function
    (chain (cons-atom $char $tail) $next-tail
      (chain (decons-atom $head) $list
        (unify $list ($next-hole $next-head)
          (return ($next-head $next-hole $next-tail))
          (return (() 0 $next-tail)))))))
(= (busy-beaver A 0) (B 1 R))
(= (busy-beaver A 1) (C 1 L))
(= (busy-beaver B 0) (A 1 L))
(= (busy-beaver B 1) (B 1 R))
(= (busy-beaver C 0) (B 1 L))
(= (busy-beaver C 1) (HALT 1 N))
!(assertEqual (eval (tm busy-beaver A (() 0 ()))) ((1 1) 1 (1 1 1)))

;; Original:
;; #[test]
;; fn interpret_minimal_metta_smoketest() {
;;     let space = space("
;;         (= (foo $a B) $a)
;;         (= (fu $x) (function (chain (eval (foo $x B)) $r (return $r))))
;;         (= (color) red)
;;         (= (color) green)
;;         (= (color) blue)
;;     ");
;;     let result = interpret(&space, &metta_atom("(chain (chain A $x $x) $y $y)"));
;;     assert_eq!(result, Ok(vec![metta_atom("A")]));
;;     let result = interpret(&space, &metta_atom("(chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y))"));
;;     assert_eq!(result, Ok(vec![metta_atom("(baz (bar A))")]));
;;     let result = interpret(&space, &metta_atom("(chain (chain (eval (fu A)) $x (bar $x)) $y (baz $y))"));
;;     assert_eq!(result, Ok(vec![metta_atom("(baz (bar A))")]));
;;     let result = interpret(&space, &metta_atom("(unify (A $b) ($a B) ($a $b) Empty)"));
;;     assert_eq!(result, Ok(vec![metta_atom("(A B)")]));
;;     let result = interpret(&space, &metta_atom("(decons-atom (a b c))"));
;;     assert_eq!(result, Ok(vec![metta_atom("(a (b c))")]));
;;     let result = interpret(&space, &metta_atom("(cons-atom a (b c))"));
;;     assert_eq!(result, Ok(vec![metta_atom("(a b c)")]));
;;     let result = interpret(&space, &metta_atom("(chain (collapse-bind (eval (color))) $collapsed (superpose-bind $collapsed))")).unwrap();
;;     assert_eq_no_order!(result, vec![metta_atom("red"), metta_atom("green"), metta_atom("blue")]);
;;     let result = interpret(&space, &metta_atom("((P $a B) $a)"));
;;     assert_eq!(result, Ok(vec![metta_atom("((P $a B) $a)")]));
;;     let result = interpret(&space, &metta_atom("(collapse-bind (eval (color)))")).unwrap();
;;     assert_eq!(result.len(), 1);
;;     assert_eq_no_order!(atom_as_slice(&result[0]).unwrap(), [
;;         atom_bindings_into_atom(expr!("red"), bind!{}),
;;         atom_bindings_into_atom(expr!("green"), bind!{}),
;;         atom_bindings_into_atom(expr!("blue"), bind!{})
;;     ]);
;; }

;; Converted:
(= (foo $a B) $a)
(= (fu $x) (function (chain (eval (foo $x B)) $r (return $r))))
(= (color) red)
(= (color) green)
(= (color) blue)
!(assertEqual (chain (chain A $x $x) $y $y) A)
!(assertEqual (chain (chain (eval (foo A $b)) $x (bar $x)) $y (baz $y)) (baz (bar A)))
!(assertEqual (chain (chain (eval (fu A)) $x (bar $x)) $y (baz $y)) (baz (bar A)))
!(assertEqual (unify (A $b) ($a B) ($a $b) Empty) (A B))
!(assertEqual (decons-atom (a b c)) (a (b c)))
!(assertEqual (cons-atom a (b c)) (a b c))
!(assertEqualNoOrder (chain (collapse-bind (eval (color))) $collapsed (superpose-bind $collapsed)) (red green blue))
!(assertEqual ((P $a B) $a) ((P $a B) $a))
!(assertEqualNoOrder (collapse-bind (eval (color))) (red green blue))


;; Original:
;; #[test]
;; fn interpret_duplicated_types() {
;;     let space = DynSpace::new(space("
;;         (: foo (-> A A))
;;         (: foo (-> A A))
;;         (: foo (-> Atom A))
;;         (: a A)
;;         (= (foo $x) a)
;;     "));
;;     let result = interpret(&space, &Atom::expr([METTA_SYMBOL, expr!("foo" "a"), ATOM_TYPE_UNDEFINED, Atom::gnd(space.clone())]));
;;     assert_eq!(result, Ok(vec![metta_atom("a")]));
;; }

;; Converted:
(: foo (-> A A))
(: foo (-> A A))
(: foo (-> Atom A))
(: a A)
(= (foo $x) a)
!(assertEqual (eval (foo a) %Undefined% &self) a)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converted Rust Tests with Original Source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;    #[test]
;;    fn case_op_internal_variables_has_priority_in_template() {
;;        let space = DynSpace::new(GroundingSpace::new());
;;        let case_op = CaseOp::new(space.clone());
;;
;;        assert_eq!(case_op.execute(&mut vec![expr!(x "A"), expr!(((x x)))]),
;;            Ok(vec![expr!(x "A")]));
;;    }
;; Converted:
(let space (new-space))
!(assertEqual (case! space (x "A") (((x x)))) (x "A"))


;;    fn assert_runtime_error(actual: Result<Vec<Atom>, ExecError>, expected: Regex) {
;;        match actual {
;;            Err(ExecError::Runtime(msg)) => assert!(expected.is_match(msg.as_str()),
;;                "Incorrect error message:\nexpected: {:?}\n  actual: {:?}", expected.to_string(), msg),
;;            _ => assert!(false, "Error is expected as result, {:?} returned", actual),
;;        }
;;    }
;; Converted:
(let assert-runtime-error (actual expected)
  (match actual
    (Err (ExecError::Runtime msg)) (assert (expected.is_match msg))
    _ (assert false)))


;;    #[test]
;;    fn assert_equal_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) (A B))
;;            (= (foo) (B C))
;;            (= (bar) (B C))
;;            (= (bar) (A B))
;;            (= (err) (A B))
;;        "));
;;
;;        let assert_equal_op = AssertEqualOp::new(space);
;;
;;        assert_eq!(assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]), unit_result());
;;
;;        let actual = assert_equal_op.execute(&mut vec![expr!(("foo")), expr!(("err"))]);
;;        let expected = Regex::new("\nExpected: \\[(A B)\\]\nGot: \\[\\((B C)|, |(A B)\\){3}\\]\nExcessive result: (B C)").unwrap();
;;        assert_runtime_error(actual, expected);
;;
;;        let actual = assert_equal_op.execute(&mut vec![expr!(("err")), expr!(("foo"))]);
;;        let expected = Regex::new("\nExpected: \\[\\((B C)|, |(A B)\\){3}\\]\nGot: \\[(A B)\\]\nMissed result: (B C)").unwrap();
;;        assert_runtime_error(actual, expected);
;;    }
;; Converted:
(let space (new-space))
(= (foo) (A B))
(= (foo) (B C))
(= (bar) (B C))
(= (bar) (A B))
(= (err) (A B))
!(assertEqual (assert-equal! space ("foo") ("bar")) (unit-result))
(let actual (assert-equal! space ("foo") ("err")))
(let expected (Regex::new "\nExpected: \\[(A B)\\]\nGot: \\[\\((B C)|, |(A B)\\){3}\\]\nExcessive result: (B C)"))
!(assert-runtime-error actual expected)
(let actual (assert-equal! space ("err") ("foo")))
(let expected (Regex::new "\nExpected: \\[\\((B C)|, |(A B)\\){3}\\]\nGot: \\[(A B)\\]\nMissed result: (B C)"))
!(assert-runtime-error actual expected)


;;    #[test]
;;    fn assert_equal_to_result_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) (A B))
;;            (= (foo) (B C))
;;        "));
;;        let assert_equal_to_result_op = AssertEqualToResultOp::new(space);
;;
;;        assert_eq!(assert_equal_to_result_op.execute(&mut vec![
;;                expr!(("foo")), expr!(("B" "C") ("A" "B"))]),
;;                unit_result());
;;    }
;; Converted:
(let space (new-space))
(= (foo) (A B))
(= (foo) (B C))
!(assertEqual (assert-equal-to-result! space ("foo") (("B" "C") ("A" "B"))) (unit-result))


;;    #[test]
;;    fn collapse_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) (A B))
;;            (= (foo) (B C))
;;        "));
;;        let collapse_op = CollapseOp::new(space);
;;
;;        let actual = collapse_op.execute(&mut vec![expr!(("foo"))]).unwrap();
;;        assert_eq!(actual.len(), 1);
;;        assert_eq_no_order!(
;;            *atom_as_expr(&actual[0]).unwrap().children(),
;;            vec![expr!("B" "C"), expr!("A" "B")]);
;;    }
;; Converted:
(let space (new-space))
(= (foo) (A B))
(= (foo) (B C))
(let actual (collapse! space ("foo")))
!(assertEqual (length actual) 1)
!(assertEqualNoOrder (children (first actual)) ((B C) (A B)))


;;    #[test]
;;    fn superpose_op() {
;;        let space = DynSpace::new(GroundingSpace::new());
;;        let superpose_op = SuperposeOp::new(space);
;;        assert_eq!(superpose_op.execute(&mut vec![expr!("A" ("B" "C"))]),
;;            Ok(vec![sym!("A"), expr!("B" "C")]));
;;    }
;; Converted:
(let space (new-space))
!(assertEqual (superpose! space ("A" ("B" "C"))) (("A") ("B" "C")))


;;    #[test]
;;    fn unique_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) (A (B C)))
;;            (= (foo) (A (B C)))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) Z)
;;        "));
;;        let unique_op = UniqueOp::new(space);
;;        let actual = unique_op.execute(&mut vec![expr!(("foo"))]).unwrap();
;;        assert_eq_no_order!(actual,
;;                   vec![expr!("A" ("B" "C")), expr!("f" "g"), expr!("Z")]);
;;    }
;; Converted:
(let space (new-space))
(= (foo) (A (B C)))
(= (foo) (A (B C)))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) Z)
(let actual (unique! space ("foo")))
!(assertEqualNoOrder actual ((A (B C)) (f g) (Z)))


;;    #[test]
;;    fn union_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) (A (B C)))
;;            (= (foo) (A (B C)))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) Z)
;;            (= (bar) (A (B C)))
;;            (= (bar) p)
;;            (= (bar) p)
;;            (= (bar) (Q a))
;;        "));
;;        let union_op = UnionOp::new(space);
;;        let actual = union_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]).unwrap();
;;        assert_eq_no_order!(actual,
;;                   vec![expr!("A" ("B" "C")), expr!("A" ("B" "C")), expr!("f" "g"), expr!("f" "g"), expr!("f" "g"), expr!("Z"),
;;                        expr!("A" ("B" "C")), expr!("p"), expr!("p"), expr!("Q" "a")]);
;;    }
;; Converted:
(let space (new-space))
(= (foo) (A (B C)))
(= (foo) (A (B C)))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) Z)
(= (bar) (A (B C)))
(= (bar) p)
(= (bar) p)
(= (bar) (Q a))
(let actual (union! space ("foo") ("bar")))
!(assertEqualNoOrder actual ((A (B C)) (A (B C)) (f g) (f g) (f g) (Z) (A (B C)) (p) (p) (Q a)))


;;    #[test]
;;    fn intersection_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) Z)
;;            (= (foo) (A (B C)))
;;            (= (foo) (A (B C)))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) (P b))
;;            (= (bar) (f g))
;;            (= (bar) (f g))
;;            (= (bar) (A (B C)))
;;            (= (bar) p)
;;            (= (bar) p)
;;            (= (bar) (Q a))
;;            (= (bar) Z)
;;
;;            (= (nsl) 5)
;;            (= (nsl) 4)
;;            (= (nsl) 3)
;;            (= (nsl) 2)
;;            (= (nsr) 5)
;;            (= (nsr) 3)
;;        "));
;;        let intersection_op = IntersectionOp::new(space);
;;        let actual = intersection_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]).unwrap();
;;        assert_eq_no_order!(actual,
;;                   vec![expr!("A" ("B" "C")), expr!("f" "g"), expr!("f" "g"), expr!("Z")]);
;;
;;        assert_eq_no_order!(intersection_op.execute(&mut vec![expr!(("nsl")), expr!(("nsr"))]).unwrap(),
;;                   vec![expr!("5"), expr!("3")]);
;;    }
;; Converted:
(let space (new-space))
(= (foo) Z)
(= (foo) (A (B C)))
(= (foo) (A (B C)))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) (P b))
(= (bar) (f g))
(= (bar) (f g))
(= (bar) (A (B C)))
(= (bar) p)
(= (bar) p)
(= (bar) (Q a))
(= (bar) Z)
(= (nsl) 5)
(= (nsl) 4)
(= (nsl) 3)
(= (nsl) 2)
(= (nsr) 5)
(= (nsr) 3)
(let actual (intersection! space ("foo") ("bar")))
!(assertEqualNoOrder actual ((A (B C)) (f g) (f g) (Z)))
!(assertEqualNoOrder (intersection! space ("nsl") ("nsr")) (5 3))


;;    #[test]
;;    fn subtraction_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) Z)
;;            (= (foo) S)
;;            (= (foo) S)
;;            (= (foo) (A (B C)))
;;            (= (foo) (A (B C)))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) (f g))
;;            (= (foo) (P b))
;;            (= (bar) (f g))
;;            (= (bar) (A (B C)))
;;            (= (bar) p)
;;            (= (bar) p)
;;            (= (bar) (Q a))
;;            (= (bar) Z)
;;            (= (bar) S)
;;            (= (bar) S)
;;            (= (bar) S)
;;        "));
;;        let subtraction_op = SubtractionOp::new(space);
;;        let actual = subtraction_op.execute(&mut vec![expr!(("foo")), expr!(("bar"))]).unwrap();
;;        assert_eq_no_order!(actual,
;;                   vec![expr!("A" ("B" "C")), expr!("f" "g"), expr!("f" "g"), expr!("P" "b")]);
;;    }
;; Converted:
(let space (new-space))
(= (foo) Z)
(= (foo) S)
(= (foo) S)
(= (foo) (A (B C)))
(= (foo) (A (B C)))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) (f g))
(= (foo) (P b))
(= (bar) (f g))
(= (bar) (A (B C)))
(= (bar) p)
(= (bar) p)
(= (bar) (Q a))
(= (bar) Z)
(= (bar) S)
(= (bar) S)
(= (bar) S)
(let actual (subtraction! space ("foo") ("bar")))
!(assertEqualNoOrder actual ((A (B C)) (f g) (f g) (P b)))


;;    #[test]
;;    fn superpose_op_type() {
;;        let space = DynSpace::new(GroundingSpace::new());
;;        assert!(validate_atom(space.borrow().as_space(), &expr!({SumOp{}}
;;            ({SuperposeOp::new(space.clone())} ({Number::Integer(1)} {Number::Integer(2)} {Number::Integer(3)}))
;;            {Number::Integer(1)})));
;;    }
;; Converted:
(let space (new-space))
!(assert (validate-atom space (sum! (superpose! space (1 2 3)) 1)))


;;    #[test]
;;    fn superpose_op_multiple_interpretations() {
;;        let metta = Metta::new(Some(EnvBuilder::test_env()));
;;        let parser = SExprParser::new("
;;            (= (f) A)
;;            (= (f) B)
;;            (= (g) C)
;;            (= (g) D)
;;
;;            !(superpose ((f) (g)))
;;        ");
;;
;;        assert_eq_metta_results!(metta.run(parser),
;;            Ok(vec![vec![expr!("A"), expr!("B"), expr!("C"), expr!("D")]]));
;;    }
;; Converted:
(= (f) A)
(= (f) B)
(= (g) C)
(= (g) D)
!(assertEqualToResult (superpose ((f) (g))) (A B C D))


;;    #[test]
;;    fn superpose_op_superposed_with_collapse() {
;;        let metta = Metta::new(Some(EnvBuilder::test_env()));
;;        let parser = SExprParser::new("
;;            (= (f) A)
;;            (= (f) B)
;;
;;            !(let $x (collapse (f)) (superpose $x))
;;        ");
;;
;;        assert_eq_metta_results!(metta.run(parser),
;;            Ok(vec![vec![expr!("A"), expr!("B")]]));
;;    }
;; Converted:
(= (f) A)
(= (f) B)
!(assertEqualToResult (let $x (collapse (f)) (superpose $x)) (A B))


;;    #[test]
;;    fn superpose_op_consumes_interpreter_errors() {
;;        let metta = Metta::new(Some(EnvBuilder::test_env()));
;;        let parser = SExprParser::new("
;;            (: f (-> A B))
;;            (= (f $x) $x)
;;
;;            (: a A)
;;            (: b B)
;;
;;            !(superpose ((f (superpose ())) (f a) (f b)))
;;        ");
;;
;;        assert_eq!(metta.run(parser), Ok(vec![vec![
;;                expr!("Error" ("f" ({SuperposeOp{space:metta.space().clone()}} ())) "NoValidAlternatives"),
;;                expr!("a"), expr!("Error" "b" "BadType")]]));
;;    }
;; Converted:
(: f (-> A B))
(= (f $x) $x)
(: a A)
(: b B)
!(assertEqual (superpose ((f (superpose ())) (f a) (f b))) (Error (f (superpose ())) "NoValidAlternatives") (a) (Error b "BadType"))


;;    #[test]
;;    fn get_type_op() {
;;        let space = DynSpace::new(metta_space("
;;            (: B Type)
;;            (: C Type)
;;            (: A B)
;;            (: A C)
;;        "));
;;
;;        let get_type_op = GetTypeOp::new(space);
;;        assert_eq_no_order!(get_type_op.execute(&mut vec![sym!("A")]).unwrap(),
;;            vec![sym!("B"), sym!("C")]);
;;    }
;; Converted:
(let space (new-space))
(: B Type)
(: C Type)
(: A B)
(: A C)
!(assertEqualNoOrder (get-type! space A) (B C))



;;    #[test]
;;    fn get_type_op_non_valid_atom() {
;;        let space = DynSpace::new(metta_space("
;;            (: f (-> Number String))
;;            (: 42 Number)
;;            (: \"test\" String)
;;        "));
;;
;;        let get_type_op = GetTypeOp::new(space);
;;        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "42")]).unwrap(),
;;            vec![sym!("String")]);
;;        assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "\"test\"")]).unwrap(),
;;            Vec::<Atom>::new());
;;    }
;; Converted:
(let space (new-space))
(: f (-> Number String))
(: 42 Number)
(: "test" String)
!(assertEqualNoOrder (get-type! space ("f" 42)) ("String"))
!(assertEqualNoOrder (get-type! space ("f" "test")) ())


;;    #[test]
;;    fn add_atom_op() {
;;        let space = DynSpace::new(GroundingSpace::new());
;;        let satom = Atom::gnd(space.clone());
;;        let res = AddAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
;;        assert_eq!(res, vec![UNIT_ATOM()]);
;;        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
;;        assert_eq_no_order!(space_atoms, vec![expr!(("foo" "bar"))]);
;;    }
;; Converted:
(let space (new-space))
(add-atom space (foo bar))
!(assertEqualNoOrder (get-atoms space) ((foo bar)))


;;    #[test]
;;    fn remove_atom_op() {
;;        let space = DynSpace::new(metta_space("
;;            (foo bar)
;;            (bar foo)
;;        "));
;;        let satom = Atom::gnd(space.clone());
;;        let res = RemoveAtomOp{}.execute(&mut vec![satom, expr!(("foo" "bar"))]).expect("No result returned");
;;        // REM: can return Bool in future
;;        assert_eq!(res, vec![UNIT_ATOM()]);
;;        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
;;        assert_eq_no_order!(space_atoms, vec![expr!(("bar" "foo"))]);
;;    }
;; Converted:
(let space (new-space))
(add-atom space (foo bar))
(add-atom space (bar foo))
(remove-atom space (foo bar))
!(assertEqualNoOrder (get-atoms space) ((bar foo)))


;;    #[test]
;;    fn get_atoms_op() {
;;        let space = DynSpace::new(metta_space("
;;            (foo bar)
;;            (bar foo)
;;        "));
;;        let satom = Atom::gnd(space.clone());
;;        let res = GetAtomsOp{}.execute(&mut vec![satom]).expect("No result returned");
;;        let space_atoms: Vec<Atom> = space.borrow().as_space().atom_iter().unwrap().cloned().collect();
;;        assert_eq_no_order!(res, space_atoms);
;;        assert_eq_no_order!(res, vec![expr!(("foo" "bar")), expr!(("bar" "foo"))]);
;;    }
;; Converted:
(let space (new-space))
(add-atom space (foo bar))
(add-atom space (bar foo))
!(assertEqualNoOrder (get-atoms space) ((foo bar) (bar foo)))


;;    #[test]
;;    fn car_atom_op() {
;;        let res = CarAtomOp{}.execute(&mut vec![expr!(("A" "C") "B")]).expect("No result returned");
;;        assert_eq!(res, vec![expr!("A" "C")]);
;;    }
;; Converted:
!(assertEqual (car-atom (("A" "C") "B")) ("A" "C"))


;;    #[test]
;;    fn cdr_atom_op() {
;;        let res = CdrAtomOp{}.execute(&mut vec![expr!(("A"))]).expect("No result returned");
;;        assert_eq!(res, vec![expr!()]);
;;        let res = CdrAtomOp{}.execute(&mut vec![expr!(("A" "C") ("D" "E") "B")]).expect("No result returned");
;;        assert_eq!(res, vec![expr!(("D" "E") "B")]);
;;     let res = CdrAtomOp{}.execute(&mut vec![]);
;;        assert_eq!(res, Err(ExecError::Runtime("cdr-atom expects non-empty expression".into())));
;;    }
;; Converted:
!(assertEqual (cdr-atom (("A"))) ())
!(assertEqual (cdr-atom (("A" "C") ("D" "E") "B")) (("D" "E") "B"))
!(assertEqual (cdr-atom ()) (error "cdr-atom expects non-empty expression"))


;;    #[test]
;;    fn cons_atom_op() {
;;        let res = ConsAtomOp{}.execute(&mut vec![expr!("A"), expr!()]).expect("No result returned");
;;        assert_eq!(res, vec![expr!(("A"))]);
;;        let res = ConsAtomOp{}.execute(&mut vec![expr!("A" "F"), expr!(("B" "C") "D")]).expect("No result returned");
;;        assert_eq!(res, vec![expr!(("A" "F") ("B" "C") "D")]);
;;    }
;; Converted:
!(assertEqual (cons-atom ("A") ()) (("A")))
!(assertEqual (cons-atom ("A" "F") (("B" "C") "D")) (("A" "F") ("B" "C") "D"))


;;    #[test]
;;    fn bind_new_space_op() {
;;        let tokenizer = Shared::new(Tokenizer::new());
;;
;;        let bind_op = BindOp::new(tokenizer.clone());
;;
;;        assert_eq!(bind_op.execute(&mut vec![sym!("&my"), sym!("definition")]), unit_result());
;;        let borrowed = tokenizer.borrow();
;;        let constr = borrowed.find_token("&my");
;;        assert!(constr.is_some());
;;        assert_eq!(constr.unwrap()("&my"), Ok(sym!("definition")));
;;    }
;; Converted:
(let &my definition)
!(assertEqual (&my) definition)


;;    #[test]
;;    fn case_op() {
;;        let space = DynSpace::new(metta_space("
;;            (= (foo) (A B))
;;        "));
;;
;;        let case_op = CaseOp::new(space.clone());
;;
;;        assert_eq!(case_op.execute(&mut vec![expr!(("foo")),
;;                expr!(((n "B") n) ("Empty" "D"))]),
;;            Ok(vec![Atom::sym("A")]));
;;        assert_eq!(case_op.execute(&mut vec![expr!({MatchOp{}} {space} ("B" "C") ("C" "B")),
;;                expr!(((n "C") n) ("Empty" "D"))]),
;;            Ok(vec![Atom::sym("D")]));
;;    }
;; Converted:
(let space (new-space))
(= (foo) (A B))
!(assertEqual (case! space ("foo") (((n "B") n) ("Empty" "D"))) "A")
!(assertEqual (case! space (match! space ("B" "C") ("C" "B")) (((n "C") n) ("Empty" "D"))) "D")


;;    #[test]
;;    fn case_op_external_vars_at_right_are_kept_untouched() {
;;        let space = DynSpace::new(GroundingSpace::new());
;;        let case_op = CaseOp::new(space.clone());
;;
;;        assert_eq!(case_op.execute(&mut vec![expr!(ext), expr!(((t t)))]),
;;            Ok(vec![expr!(ext)]));
;;        assert_eq!(case_op.execute(&mut vec![expr!(ext "A"), expr!(((t t)))]),
;;            Ok(vec![expr!(ext "A")]));
;;    }
;; Converted:
(let space (new-space))
!(assertEqual (case! space ext (((t t)))) ext)
!(assertEqual (case! space (ext "A") (((t t)))) (ext "A"))

