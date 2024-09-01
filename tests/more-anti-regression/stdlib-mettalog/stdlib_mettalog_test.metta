
; !(include! &corelib stdlib_mettalog)
; !(import! &self stdlib_mettalog)


; This script demonstrates how one can document MeTTa code and get
; help using the documenatation.

; Let's document a function which has two arguments and returns value.
; One can use `@doc` expression to do it. First argument of the expression is an
; atom being documented. Other arguments describe the atom, describe function
; parameters and return value.
(@doc some-func
	 (@desc "Test function")
	 (@params (
			  (@param "First argument")
			  (@param "Second argument")
			  ))
	 (@return "Return value"))

; Function type is required to document the function
(: Arg1Type Type)
(: Arg2Type Type)
(: ReturnType Type)
(: some-func (-> Arg1Type Arg2Type ReturnType))

; `get-doc` function returns a `@doc-formal` expression which contains the full
; documentation of the atom including user defined description and types.
!(assertEqual
   (get-doc some-func)
   (@doc-formal (@item some-func) (@kind function)
			   (@type (-> Arg1Type Arg2Type ReturnType))
			   (@desc "Test function")
			   (@params (
						(@param (@type Arg1Type) (@desc "First argument"))
						(@param (@type Arg2Type) (@desc "Second argument"))))
			   (@return (@type ReturnType) (@desc "Return value"))))

; Same approach can be used to document single atom of any @kind.
(@doc SomeSymbol (@desc "Test symbol atom having specific type"))
(: SomeSymbol SomeType)

!(assertEqual
   (get-doc SomeSymbol)
   (@doc-formal (@item SomeSymbol) (@kind atom) (@type SomeType)
			   (@desc "Test symbol atom having specific type")))

; Grounded atoms are also can be documented using `@doc` expressions. Type of
; the grounded atom is a part of its implementation.
(@doc some-gnd-atom
	 (@desc "Test function")
	 (@params (
			  (@param "First argument")
			  (@param "Second argument")
			  ))
	 (@return "Return value")
	 )

; As some-gnd-function is not imported really in this example type is not
; available and @doc-formal contains %Undefined% instead.
!(assertEqual
   (get-doc some-gnd-atom)
   (@doc-formal (@item some-gnd-atom) (@kind function)
			   (@type %Undefined%)
			   (@desc "Test function")
			   (@params (
						(@param (@type %Undefined%) (@desc "First argument"))
						(@param (@type %Undefined%) (@desc "Second argument"))))
			   (@return (@type %Undefined%) (@desc "Return value"))))

; If atom is not documented then `get-doc` returns "No documentation"
; description.
!(assertEqual
   (get-doc NoSuchAtom)
   (@doc-formal (@item NoSuchAtom) (@kind atom) (@type %Undefined%) (@desc "No documentation")))

; Same result is returned if for instance documentation for the function
; application is queried.
!(assertEqual
   (get-doc (some-func arg1 arg2))
   (@doc-formal (@item (some-func arg1 arg2)) (@kind atom) (@type ReturnType) (@desc "No documentation")))

; `help!` function gets the documentation and prints it in a human readable
; format.
!(help! some-func)
; Output:
;
; Function some-func: (-> Arg1Type Arg2Type ReturnType) Test function
; Parameters:
;   Arg1Type First argument
;   Arg2Type Second argument
; Return: (@type ReturnType) Return value
;

!(help! SomeSymbol)
; Output:
;
; Atom SomeSymbol: SomeType Test symbol atom having specific type
;

!(help! some-gnd-atom)
; Output:
;
; Function some-gnd-atom: %Undefined% Test function
; Parameters:
;   %Undefined% First argument
;   %Undefined% Second argument
; Return: (@type %Undefined%) Return value
;


!(help! NoSuchAtom)
; Output:
;
; Atom NoSuchAtom: %Undefined% No documentation
;

!(help! (some-func arg1 arg2))
; Output:
;
; Atom (some-func arg1 arg2): ReturnType No documentation
;




































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converted Rust Tests with Original Source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original:
;; #[test]
;; fn test_error_is_used_as_an_argument() {
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     let parser = SExprParser::new(r#"
;;!(get-type Error)
;;!(get-metatype Error)
;;!(get-type (Error Foo Boo))
;;!(Error (+ 1 2) (+ 1 +))
;;     "#);
;;     assert_eq_metta_results!(metta.run(parser), Ok(vec![
;;         vec![expr!("->" "Atom" "Atom" "ErrorType")],
;;         vec![expr!("Symbol")],
;;         vec![expr!("ErrorType")],
;;         vec![expr!("Error" ({SumOp{}} {Number::Integer(1)} {Number::Integer(2)}) ({SumOp{}} {Number::Integer(1)} {SumOp{}}))],
;;     ]));
;; }
;; Converted:
!(assertEqualToResults (get-type Error) (-> Atom Atom ErrorType))
!(assertEqualToResults (get-metatype Error) (Symbol))
!(assertEqualToResults (get-type (Error Foo Boo)) (ErrorType))
!(assertEqualToResults (Error (+ 1 2) (+ 1 +)) (Error ({SumOp{}} {Number::Integer(1)} {Number::Integer(2)}) ({SumOp{}} {Number::Integer(1)} {SumOp{}})))


















;;;;;;;;;;;;;;;;;;;;;;
; Unify
;;;;;;;;;;;;;;;;;;;;;;
!(assertEqual (eval (unify (a $b 1 (d)) (a $a 1 (d)) ok nok)) ok)
!(assertEqual (eval (unify (a $b c) (a b $c) (ok $b $c) nok)) (ok b c))
!(assertEqual (eval (unify $a (a b c) (ok $a) nok)) (ok (a b c)))
!(assertEqual (eval (unify (a b c) $a (ok $a) nok)) (ok (a b c)))
!(assertEqual (eval (unify (a b c) (a b d) ok nok)) nok)
!(assertEqual (eval (unify ($x a) (b $x) ok nok)) nok)


!(assertEqual (decons-atom (a b c)) (a (b c)))
!(assertEqual (decons-atom (a b)) (a (b)))
!(assertEqual (decons-atom (a)) (a ()))
;> !(decons-atom ())
;[(Error (decons-atom ()) expected: (decons-atom (: <expr> Expression)), found: (decons-atom ()))]





;;;;;;;;;;;;;;;;;;;;;;
; Metta GetTypeOp
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn get_type_op() {
;;     let space = DynSpace::new(metta_space("
(: B Type)
(: C Type)
(: A B)
(: A C)
;;     "));
;;
;;     let get_type_op = GetTypeOp::new(space.clone());
;;     assert_eq_no_order!(get_type_op.execute(&mut vec![sym!("A"), expr!({space.clone()})]).unwrap(),
;;         vec![sym!("B"), sym!("C")]);
;; }
;; Defines a type space and checks if A belongs to types B and C using GetTypeOp.
!(assertEqualToResult (eval (get-type A &self)) (B C))

;;;;;;;;;;;;;;;;;;;;;;
; Metta GetTypeOp Non-Valid Atom
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn get_type_op_non_valid_atom() {
;;     let space = DynSpace::new(metta_space("
(: f (-> Number String))
(: 42 Number)
(: "test" String)
;;     "));
;;
;;     let get_type_op = GetTypeOp::new(space.clone());
;;     assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "42"), expr!({space.clone()})]).unwrap(),
;;         vec![sym!("String")]);
;;     assert_eq_no_order!(get_type_op.execute(&mut vec![expr!("f" "\"test\""), expr!({space.clone()})]).unwrap(),
;;         vec![EMPTY_SYMBOL]);
;; }
!(assertEqualToResult (eval (get-type (f 42) &self)) (String))
!(assertEqual (eval (get-type (f "test") &self)) (empty))

;; Original:
;; #[test]
;; fn metta_car_atom() {
;;     let result = run_program("!(eval (car-atom (A $b)))");
;;     assert_eq!(result, Ok(vec![vec![expr!("A")]]));
;;     let result = run_program("!(eval (car-atom ($a B)))");
;;     assert_eq!(result, Ok(vec![vec![expr!(a)]]));
;;     let result = run_program("!(eval (car-atom ()))");
;;     assert_eq!(result, Ok(vec![vec![expr!("Error" ("car-atom" ()) {Str::from_str("car-atom expects a non-empty expression as an argument")})]]));
;;     let result = run_program("!(eval (car-atom A))");
;;     assert_eq!(result, Ok(vec![vec![expr!("Error" ("car-atom" "A") {Str::from_str("car-atom expects a non-empty expression as an argument")})]]));
;; }

;; Converted:
!(assertEqual (eval (car-atom (A $b))) (A))
!(assertEqual (eval (car-atom ($a B))) (a))
!(assertEqual (eval (car-atom ())) (Error (car-atom ()) "car-atom expects a non-empty expression as an argument"))
!(assertEqual (eval (car-atom A)) (Error (car-atom "A") "car-atom expects a non-empty expression as an argument"))

;; Original:
;; #[test]
;; fn metta_cdr_atom() {
;;     assert_eq!(run_program(&format!("!(cdr-atom (a b c))")), Ok(vec![vec![expr!("b" "c")]]));
;;     assert_eq!(run_program(&format!("!(cdr-atom ($a b $c))")), Ok(vec![vec![expr!("b" c)]]));
;;     assert_eq!(run_program(&format!("!(cdr-atom ())")), Ok(vec![vec![expr!("Error" ("cdr-atom" ()) {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
;;     assert_eq!(run_program(&format!("!(cdr-atom a)")), Ok(vec![vec![expr!("Error" ("cdr-atom" "a") {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
;;     assert_eq!(run_program(&format!("!(cdr-atom $a)")), Ok(vec![vec![expr!("Error" ("cdr-atom" a) {Str::from_str("cdr-atom expects a non-empty expression as an argument")})]]));
;; }

;; Converted:
!(assertEqual (eval (cdr-atom (a b c))) (b c))
!(assertEqual (eval (cdr-atom ($a b $c))) (b c))
!(assertEqual (eval (cdr-atom ())) (Error (cdr-atom ()) "cdr-atom expects a non-empty expression as an argument"))
!(assertEqual (eval (cdr-atom a)) (Error (cdr-atom "a") "cdr-atom expects a non-empty expression as an argument"))
!(assertEqual (eval (cdr-atom $a)) (Error (cdr-atom a) "cdr-atom expects a non-empty expression as an argument"))


;;;;;;;;;;;;;;;;;;;;;;
; Metta Switch
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_switch() {
;;     let result = run_program("!(eval (switch (A $b) ( (($a B) ($b $a)) ((B C) (C B)) )))");
;;     assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
;;     let result = run_program("!(eval (switch (A $b) ( ((B C) (C B)) (($a B) ($b $a)) )))");
;;     assert_eq!(result, Ok(vec![vec![expr!("B" "A")]]));
;;     let result = run_program("!(eval (switch (A $b) ( ((B C) (C B)) ((D E) (E B)) )))");
;;     assert_eq!(result, Ok(vec![vec![]]));
;; }
;; Tests the switch operation with various patterns and variables.

!(assertEqual (eval (case (A $b) ( (($a B) ($b $a)) ((B C) (C B)) ))) (B A))
!(assertEqual (eval (case (A $b) ( ((B C) (C B)) (($a B) ($b $a)) ))) (B A))
!(assertEqual (eval (case (A $b) ( ((B C) (C B)) ((D E) (E B)) ))) ())

;!(assertEqualToResult (switch (A $b) ( (($a B) ($b $a)) ((B C) (C B)) )) ((B A)(B A)(B A)(B A)))
;!(assertEqualToResult (case (A $b) ( (($a B) ($b $a)) ((B C) (C B)) )) ((B A)(B A)(B A)(B A)))

!(assertEqual (eval (switch (A $b) ( ((B C) (C B)) (($a B) ($b $a))) )) (B A))
!(assertEqual (eval (switch (A $b) ( (($a B) ($b $a)) ((B C) (C B)) ))) (B A))
!(assertEqual (eval (switch (A $b) ( ((B C) (C B)) (($a B) ($b $a)) ))) (B A))
!(assertEqual (eval (switch (A $b) ( ((B C) (C B)) ((D E) (E B)) ))) ())

;;;;;;;;;;;;;;;;;;;;;;
; Metta Case Empty
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_case_empty() {
;;     let result = run_program("!(case Empty ( (ok ok) (%void% nok) ))");
;;     assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
;;     let result = run_program("!(case (unify (C B) (C B) ok Empty) ( (ok ok) (%void% nok) ))");
;;     assert_eq!(result, Ok(vec![vec![expr!("ok")]]));
;;     let result = run_program("!(case (unify (B C) (C B) ok nok) ( (ok ok) (nok nok) ))");
;;     assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
;;     let result = run_program("!(case (unify (B C) (C B) ok Empty) ( (ok ok) (%void% nok) ))");
;;     assert_eq!(result, Ok(vec![vec![expr!("nok")]]));
;; }
;; Demonstrates case operations with various scenarios, including empty cases and unification.
!(assertEqual (eval (case Empty ((ok ok) (%void% nok)))) nok)
!(assertEqual (eval (case (unify (C B) (C B) ok Empty) ((ok ok) (%void% nok)))) ok)
!(assertEqual (eval (case (unify (B C) (C B) ok nok) ((ok ok) (nok nok)))) nok)
!(assertEqual (eval (case (unify (B C) (C B) ok Empty) ((ok ok) (%void% nok)))) nok)


;;;;;;;;;;;;;;;;;;;;;;
; Metta Is-Function
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_is_function() {
;;     let result = run_program("!(eval (is-function (-> $t)))");
;;     assert_eq!(result, Ok(vec![vec![expr!({Bool(true)})]]));
;;     let result = run_program("!(eval (is-function (A $t)))");
;;     assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
;;     let result = run_program("!(eval (is-function %Undefined%))");
;;     assert_eq!(result, Ok(vec![vec![expr!({Bool(false)})]]));
;; }
;; Evaluates if given expressions are recognized as functions.
!(assertEqual (eval (is-function (-> $t))) True)
!(assertEqual (eval (is-function (A $t))) False)
!(assertEqual (eval (is-function %Undefined%)) False)

;;;;;;;;;;;;;;;;;;;;;;
; Metta Type-Cast
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_type_cast() {
;;     assert_eq!(run_program("(: a A) !(eval (type-cast a A &self))"), Ok(vec![vec![expr!("a")]]));
;;     assert_eq!(run_program("(: a A) !(eval (type-cast a B &self))"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
;;     assert_eq!(run_program("(: a A) !(eval (type-cast a %Undefined% &self))"), Ok(vec![vec![expr!("a")]]));
;;     assert_eq!(run_program("!(eval (type-cast a B &self))"), Ok(vec![vec![expr!("a")]]));
;;     assert_eq!(run_program("!(eval (type-cast 42 Number &self))"), Ok(vec![vec![expr!({(42)})]]));
;;     assert_eq!(run_program("!(eval (type-cast 42 %Undefined% &self))"), Ok(vec![vec![expr!({(42)})]]));
;;     assert_eq!(run_program("(: a A) !(eval (type-cast a Atom &self))"), Ok(vec![vec![expr!("a")]]));
;;     assert_eq!(run_program("(: a A) !(eval (type-cast a Symbol &self))"), Ok(vec![vec![expr!("a")]]));
;;     assert_eq!(run_program("!(eval (type-cast 42 Grounded &self))"), Ok(vec![vec![expr!({(42)})]]));
;;     assert_eq!(run_program("!(eval (type-cast () Expression &self))"), Ok(vec![vec![expr!()]]));
;;     assert_eq!(run_program("!(eval (type-cast (a b) Expression &self))"), Ok(vec![vec![expr!("a" "b")]]));
;; NEW TODO     assert_eq!(run_program("!(eval (type-cast $v Variable &self))"), Ok(vec![vec![expr!(v)]]));
;; NEW TODO     assert_eq!(run_program("(: a A) (: b B) !(eval (type-cast (a b) (A B) &self))"), Ok(vec![vec![expr!("a" "b")]]));
;; NEW TODO     assert_eq!(run_program("(: a A) (: a B) !(eval (type-cast a A &self))"), Ok(vec![vec![expr!("a")]]));
;; }
;; Type-cast operations with various types and validation checks.
!(assertEqual (eval (type-cast a B &self)) (Error a BadType))
!(assertEqual (eval (type-cast 42 Number &self)) 42)
!(assertEqual (eval (type-cast 42 %Undefined% &self)) 42)
!(assertEqual (eval (type-cast 42 Grounded &self)) 42)
!(assertEqual (eval (type-cast () Expression &self)) ())
!(assertEqual (eval (type-cast (a b) Expression &self)) (a b))
(: a A)
!(assertEqual (eval (type-cast a A &self)) a)
!(assertEqual (eval (type-cast a B &self)) (Error a BadType))
!(assertEqual (eval (type-cast a %Undefined% &self)) a)
!(assertEqual (eval (type-cast a Atom &self)) a)
!(assertEqual (eval (type-cast a Symbol &self)) a)

;; NEW TODOs
!(assertEqual (eval (type-cast $v Variable &self)) $v)
(: a A) (: b B)
!(assertEqual (eval (type-cast (a b) (A B) &self)) (a b))
(: a A) (: a B)
!(assertEqual (eval (type-cast a A &self)) a)

;;;;;;;;;;;;;;;;;;;;;;
; Metta Filter-Atom
;;;;;;;;;;;;;;;;;;;;;;
;; Filtering atoms based on a conditional check.
;; #[test]
;; fn metta_filter_atom() {
;;     assert_eq!(run_program("!(eval (filter-atom () $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!()]]));
;;     assert_eq!(run_program("!(eval (filter-atom (a (b) $c) $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!("a" ("b") c)]]));
;;     assert_eq!(run_program("!(eval (filter-atom (a (Error (b) \"Test error\") $c) $x (eval (if-error $x False True))))"), Ok(vec![vec![expr!("a" c)]]));
;; }
!(assertEqual (eval (filter-atom () $x (eval (if-error $x False True)))) ())
!(assertEqual (eval (filter-atom (a (b) $c) $x (eval (if-error $x False True)))) (a (b) c))
!(assertEqual (eval (filter-atom (a (Error (b) "Test error") $c) $x (eval (if-error $x False True)))) (a c))

;;;;;;;;;;;;;;;;;;;;;;
; Metta Map-Atom
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_map_atom() {
;;     assert_eq!(run_program("!(eval (map-atom () $x ($x mapped)))"), Ok(vec![vec![expr!()]]));
;;     assert_eq!(run_program("!(eval (map-atom (a (b) $c) $x (mapped $x)))"), Ok(vec![vec![expr!(("mapped" "a") ("mapped" ("b")) ("mapped" c))]]));
;; }
;; Applying a function to each atom in a list and returning a new list with the results.
!(assertEqual (eval (map-atom () $x ($x 'mapped))) ())
!(assertEqual (eval (map-atom (a (b) $c) $x (mapped $x))) ((mapped a) (mapped (b)) (mapped c)))

;;;;;;;;;;;;;;;;;;;;;;
; Metta Foldl-Atom
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_foldl_atom() {
;;     assert_eq!(run_program("!(eval (foldl-atom () 1 $a $b (eval (+ $a $b))))"), Ok(vec![vec![expr!({(1)})]]));
;;     assert_eq!(run_program("!(eval (foldl-atom (1 2 3) 0 $a $b (eval (+ $a $b))))"), Ok(vec![vec![expr!({(6)})]]));
;; }
;; Folding (reducing) a list from the left with a function.
!(assertEqual (eval (foldl-atom () 1 $a $b (eval (+ $a $b)))) (1))
!(assertEqual (eval (foldl-atom (1 2 3) 0 $a $b (eval (+ $a $b)))) (6))

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Single Atom As Atom
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_single_atom_as_atom() {
;;     let result = run_program("!(eval (interpret A Atom &self))");
;;     assert_eq!(result, Ok(vec![vec![expr!("A")]]));
;; }
;; Checking if a single atom is correctly interpreted as itself.
!(assertEqual (eval (interpret A Atom &self)) A)

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Single Atom As Meta-Type
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_single_atom_as_meta_type() {
;;     assert_eq!(run_program("!(eval (interpret A Symbol &self))"), Ok(vec![vec![expr!("A")]]));
;;     assert_eq!(run_program("!(eval (interpret $x Variable &self))"), Ok(vec![vec![expr!(x)]]));
;;     assert_eq!(run_program("!(eval (interpret (A B) Expression &self))"), Ok(vec![vec![expr!("A" "B")]]));
;;     assert_eq!(run_program("!(eval (interpret 42 Grounded &self))"), Ok(vec![vec![expr!({(42)})]]));
;; }
;; Interpreting single atoms as different types and verifying the outcomes.
!(assertEqual (eval (interpret A Symbol &self)) A)
!(assertEqual (eval (interpret $x Variable &self)) x)
!(assertEqual (eval (interpret (A B) Expression &self)) (A B))
!(assertEqual (eval (interpret 42 Grounded &self)) 42)

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Symbol or Grounded Value As Type
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_symbol_or_grounded_value_as_type() {
;;     assert_eq!(run_program("(: a A) !(eval (interpret a A &self))"), Ok(vec![vec![expr!("a")]]));
;;     assert_eq!(run_program("(: a A) !(eval (interpret a B &self))"), Ok(vec![vec![expr!("Error" "a" "BadType")]]));
;;     assert_eq!(run_program("!(eval (interpret 42 Number &self))"), Ok(vec![vec![expr!({(42)})]]));
;; }
;; Interpreting symbols or grounded values as specified types.
(: a A)
!(assertEqual (eval (interpret a A &self)) a)
!(assertEqual (eval (interpret a B &self)) (Error "a" "BadType"))
!(assertEqual (eval (interpret 42 Number &self)) 42)

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Variable As Type
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_variable_as_type() {
;;     assert_eq!(run_program("!(eval (interpret $x %Undefined% &self))"), Ok(vec![vec![expr!(x)]]));
;;     assert_eq!(run_program("!(eval (interpret $x SomeType &self))"), Ok(vec![vec![expr!(x)]]));
;; }
;; Interpreting variables as unspecified or specific types.
!(assertEqual (eval (interpret $x %Undefined% &self)) $x)
!(assertEqual (eval (interpret $x SomeType &self)) $x)

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Empty Expression As Type
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_empty_expression_as_type() {
;;     assert_eq!(run_program("!(eval (interpret () %Undefined% &self))"), Ok(vec![vec![expr!(())]]));
;;     assert_eq!(run_program("!(eval (interpret () SomeType &self))"), Ok(vec![vec![expr!(())]]));
;; }
;; Interpreting an empty expression as unspecified or a specific type.
!(assertEqual (eval (interpret () %Undefined% &self)) ())
!(assertEqual (eval (interpret () SomeType &self)) ())

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Single Atom As Variable Type
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_single_atom_as_variable_type() {
;;     let result = run_program("
(: S Int)
;;         !(chain (eval (interpret S $t &self)) $res (: $res $t))
;;     ");
;;     assert_eq!(result, Ok(vec![vec![expr!(":" "S" "Int")]]));
;; }
!(assertEqual (chain (eval (interpret S $t &self)) $res (: $res $t)) (: S Int))

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Func
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_func() {
;;     let result = run_program("
(: a T)
(: foo (-> T T))
(= (foo $x) $x)
(= (bar $x) $x)
;;         !(eval (interpret (foo (bar a)) %Undefined% &self))
;;     ");
;;     assert_eq!(result, Ok(vec![vec![expr!("a")]]));
!(assertEqual (eval (interpret (foo (bar a)) %Undefined% &self)) a)
(reset-program) ;; Assuming a way to clear previous definitions
;;     let result = run_program("
(: b B)
(: foo (-> T T))
(= (foo $x) $x)
;;         !(eval (interpret (foo b) %Undefined% &self))
;;     ");
;;     assert_eq!(result, Ok(vec![vec![expr!("Error" "b" "BadType")]]));
!(assertEqual (eval (interpret (foo b) %Undefined% &self)) (Error "b" "BadType"))
;;     let result = run_program("
(: Nil (List $t))
(: Z Nat)
(: S (-> Nat Nat))
(: Cons (-> $t (List $t) (List $t)))
;;         !(eval (interpret (Cons S (Cons Z Nil)) %Undefined% &self))
;;     ");
;;     assert_eq!(result, Ok(vec![vec![expr!("Error" ("Cons" "Z" "Nil") "BadType")]]));
;; }
!(assertEqual (eval (interpret (Cons S (Cons Z Nil)) %Undefined% &self)) (Error ("Cons" "Z" "Nil") "BadType"))

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Tuple
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_tuple() {
;;     assert_eq!(run_program("!(eval (interpret-tuple () &self))"), Ok(vec![vec![expr!(())]]));
;;     assert_eq!(run_program("!(eval (interpret-tuple (a) &self))"), Ok(vec![vec![expr!(("a"))]]));
;;     assert_eq!(run_program("!(eval (interpret-tuple (a b) &self))"), Ok(vec![vec![expr!(("a" "b"))]]));
;; Interpreting tuples as specified types.
!(assertEqual (eval (interpret-tuple () &self)) ())
!(assertEqual (eval (interpret-tuple (a) &self)) (a))
!(assertEqual (eval (interpret-tuple (a b) &self)) (a b))
;;     let result = run_program("
(= (foo $x) (bar $x))
(= (bar $x) (baz $x))
(= (baz $x) $x)
;;         !(eval (interpret-tuple ((foo A) (foo B)) &self))
;;     ");
;;     assert_eq!(result, Ok(vec![vec![expr!("A" "B")]]));
;; }
!(assertEqual (eval (interpret-tuple ((foo A) (foo B)) &self)) (A B))

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Expression As Type
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_expression_as_type() {
;;     assert_eq!(run_program("(= (foo $x) $x) !(eval (interpret (foo a) %Undefined% &self))"), Ok(vec![vec![expr!("a")]]));
;;     assert_eq!(run_program("!(eval (interpret (foo a) %Undefined% &self))"), Ok(vec![vec![expr!("foo" "a")]]));
;;     assert_eq!(run_program("!(eval (interpret () SomeType &self))"), Ok(vec![vec![expr!(())]]));
;; }
;; Testing the interpretation of expressions as a type, with variable resetting for clean state.
(= (foo $x) $x)
!(assertEqual (eval (interpret (foo a) %Undefined% &self)) a)
(reset-program)
!(assertEqual (eval (interpret (foo a) %Undefined% &self)) (foo a))
!(assertEqual (eval (interpret () SomeType &self)) ())

;;;;;;;;;;;;;;;;;;;;;;
; Metta Interpret Single Atom With Two Types
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_interpret_single_atom_with_two_types() {
;;     let result = run_program("
(: a A) (: a B) 
;;     !(eval (interpret a %Undefined% &self))");
;;     assert_eq!(result, Ok(vec![vec![expr!("a")]]));
;; }
;; Handling the case where a single atom can be interpreted as having multiple types.
!(assertEqual (eval (interpret a %Undefined% &self)) a)

;;;;;;;;;;;;;;;;;;;;;;
; Metta Assert Equal Op
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_assert_equal_op() {
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     let assert = AssertEqualOp::new(metta.space().clone());
;;     let program = "
(= (foo $x) $x)
(= (bar $x) $x)
;;     ";
;;     assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
;;     assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar A))")), Ok(vec![
;;         vec![UNIT_ATOM()],
;;     ]));
;;     assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) (bar B))")), Ok(vec![
;;         vec![expr!("Error" ({assert.clone()} ("foo" "A") ("bar" "B")) "\nExpected: [B]\nGot: [A]\nMissed result: B")],
;;     ]));
;;     assert_eq!(metta.run(SExprParser::new("!(assertEqual (foo A) Empty)")), Ok(vec![
;;         vec![expr!("Error" ({assert.clone()} ("foo" "A") "Empty") "\nExpected: []\nGot: [A]\nExcessive result: A")]
;;     ]));
;; }
!(assertEqual (eval (assertEqual (foo A) (bar A))) ())
!(assertEqual (eval (assertEqual (foo A) (bar B))) (Error "Expected: [B] Got: [A] Missed result: B"))
!(assertEqual (eval (assertEqual (foo A) Empty)) (Error "Expected: [] Got: [A] Excessive result: A"))

;;;;;;;;;;;;;;;;;;;;;;
; Metta Assert Equal To Result Op
;;;;;;;;;;;;;;;;;;;;;;
;; #[test]
;; fn metta_assert_equal_to_result_op() {
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     let assert = AssertEqualToResultOp::new(metta.space().clone());
;;     let program = "
(= (foo) A)
(= (foo) B)
(= (bar) C)
(= (baz) D)
(= (baz) D)
;;     ";
;;     assert_eq!(metta.run(SExprParser::new(program)), Ok(vec![]));
;;     assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (foo) (A B))")), Ok(vec![
;;         vec![UNIT_ATOM()],
;;     ]));
;;     assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (bar) (A))")), Ok(vec![
;;         vec![expr!("Error" ({assert.clone()} ("bar") ("A")) "\nExpected: [A]\nGot: [C]\nMissed result: A")],
;;     ]));
;;     assert_eq!(metta.run(SExprParser::new("!(assertEqualToResult (baz) (D))")), Ok(vec![
;;         vec![expr!("Error" ({assert.clone()} ("baz") ("D")) "\nExpected: [D]\nGot: [D, D]\nExcessive result: D")]
;;     ]));
;; }
;; More granular assertion comparing operation results to expected outcomes.
!(assertEqualToResult (foo) (A B) (()))
!(assertEqualToResult (bar) (A) (Error "Expected: [A] Got: [C] Missed result: A"))
!(assertEqualToResult (baz) (D) (Error "Expected: [D] Got: [D, D] Excessive result: D"))





;; Original:
;; #[test]
;; fn test_variable_keeps_value_in_different_sub_expressions() {
;;     let program = "
;;      (= (eq $x $x) True)
;;      (= (plus Z $y) $y)
;;      (= (plus (S $k) $y) (S (plus $k $y)))
;;      
;;      !(metta (eq (plus Z $n) $n) %Undefined% &self)
;;      !(metta (eq (plus (S Z) $n) $n) %Undefined% &self)
;;     ";
;;     assert_eq_metta_results!(run_program(program),
;;         Ok(vec![vec![expr!({Bool(true)})], vec![expr!("eq" ("S" n) n)]]));
;; }

;; Converted:
(= (eq $x $x) True)
(= (plus Z $y) $y)
(= (plus (S $k) $y) (S (plus $k $y)))
!(assertEqual (metta (eq (plus Z $n) $n) %Undefined% &self) (True))
!(assertEqual (metta (eq (plus (S Z) $n) $n) %Undefined% &self) ("eq" ("S" n) n))

;; Original:
;; #[test]
;; fn test_variable_defined_via_variable() {
;;     let program = "
;;      (= (myif T $y) $y)
;;      (= (mynot F) T)
;;      (= (a $z) (mynot (b $z)))
;;      (= (b d) F)
;;      
;;      !(metta (myif (a $x) $x) %Undefined% &self)
;;     ";
;;     assert_eq_metta_results!(run_program(program),
;;         Ok(vec![vec![expr!("d")]]));
;; }

;; Converted:
(= (myif T $y) $y)
(= (mynot F) T)
(= (a $z) (mynot (b $z)))
(= (b d) F)
!(assertEqual (metta (myif (a $x) $x) %Undefined% &self) d)

;; Original:
;; #[test]
;; fn test_variable_name_conflict() {
;;     let program = "
;;      (= (a ($W)) True)
;;      
;;      !(metta (a $W) %Undefined% &self)
;;     ";
;;     assert_eq_metta_results!(run_program(program),
;;         Ok(vec![vec![expr!({Bool(true)})]]));
;; }

;; Converted:
(= (a ($W)) True)
!(assertEqual (metta (a $W) %Undefined% &self) True)

;; Original:
;; #[test]
;; fn test_variable_name_conflict_renaming() {
;;     let program = "
;;      (= (b ($x $y)) (c $x $y))
;;      
;;      !(metta (a (b $a) $x $y) %Undefined% &self)
;;     ";
;;     let result = run_program(program);
;;     assert!(result.is_ok_and(|res| res.len() == 1 && res[0].len() == 1 &&
;;         atoms_are_equivalent(&res[0][0], &expr!("a" ("c" a b) c d))));
;; }

;; Converted:
(= (b ($x $y)) (c $x $y))
!(assertEqual (metta (a (b $a) $x $y) %Undefined% &self) (a (c a b) c d))

;; Original:
;; #[test]
;; fn test_operation_is_expression() {
;;     let program = "
;;      (: foo (-> (-> A A)))
;;      (: a A)
;;      (= (foo) bar)
;;      (= (bar $x) $x)
;;      
;;      !(metta ((foo) a) %Undefined% &self)
;;     ";
;;     assert_eq_metta_results!(run_program(program), Ok(vec![vec![expr!("a")]]));
;; }

;; Converted:
(: foo (-> (-> A A)))
(: a A)
(= (foo) bar)
(= (bar $x) $x)
!(assertEqual (metta ((foo) a) %Undefined% &self) a)

;; Original:
;; static ID_NUM: &Operation = &Operation{
;;     name: "id_num",
;;     execute: |_, args| {
;;         let arg_error = || ExecError::from("id_num expects one argument: number");
;;         let num = args.get(0).ok_or_else(arg_error)?;
;;         Ok(vec![num.clone()])
;;     },
;;     typ: "(-> Number Number)",
;; };

;; Converted:
;; (Not directly translatable to MeTTa, as it's Rust-specific operation)

;; Original:
;; #[test]
;; fn test_return_bad_type_error() {
;;     let program1 = "
;;      (: myAtom myType)
;;      (: id_a (-> A A))
;;      (= (id_a $a) $a)
;;      
;;      !(metta (id_a myAtom) %Undefined% &self)
;;     ";
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
;;         |_| Atom::gnd(ID_NUM));
;;     assert_eq!(metta.run(SExprParser::new(program1)),
;;         Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));
;;     let program2 = "
;;      !(metta (id_num myAtom) %Undefined% &self)
;;     ";
;;     assert_eq!(metta.run(SExprParser::new(program2)),
;;         Ok(vec![vec![expr!("Error" "myAtom" "BadType")]]));
;; }

;; Converted:
(: myAtom myType)
(: id_a (-> A A))
(= (id_a $a) $a)
!(assertEqual (metta (id_a myAtom) %Undefined% &self) (Error myAtom BadType))
!(assertEqual (metta (id_num myAtom) %Undefined% &self) (Error myAtom BadType))

;; Original:
;; #[test]
;; fn test_return_incorrect_number_of_args_error() {
;;     let program1 = "
;;      (: a A)
;;      (: b B)
;;      (: c C)
;;      (: foo (-> A B C))
;;      (= (foo $a $b) c)
;;      
;;      !(metta (foo a b) %Undefined% &self)
;;     ";
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     metta.tokenizer().borrow_mut().register_token(Regex::new("id_num").unwrap(),
;;         |_| Atom::gnd(ID_NUM));
;;     assert_eq!(metta.run(SExprParser::new(program1)),
;;         Ok(vec![vec![expr!("c")]]));

;;     let program2 = "!(metta (foo a) %Undefined% &self)";

;;     assert_eq!(metta.run(SExprParser::new(program2)),
;;         Ok(vec![vec![expr!("Error" ("foo" "a") "IncorrectNumberOfArguments")]]));

;;     let program3 = "!(metta (foo a b c) %Undefined% &self)";

;;     assert_eq!(metta.run(SExprParser::new(program3)),
;;         Ok(vec![vec![expr!("Error" ("foo" "a" "b" "c") "IncorrectNumberOfArguments")]]));
;; }

;; Converted:
(: a A)
(: b B)
(: c C)
(: foo (-> A B C))
(= (foo $a $b) c)
!(assertEqual (metta (foo a b) %Undefined% &self) c)
!(assertEqual (metta (foo a) %Undefined% &self) (Error (foo a) IncorrectNumberOfArguments))
!(assertEqual (metta (foo a b c) %Undefined% &self) (Error (foo a b c) IncorrectNumberOfArguments))

;; Original:
;; #[test]
;; fn use_sealed_to_make_scoped_variable() {
;;     assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![]]));
;;     assert_eq!(run_program("!(let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
;;            (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
;; }

;; Converted:
!(assertEqual (let $x (input $x) (output $x)) ())
!(assertEqual (let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x)))) (let $sv (input $x) $st)) ("output" ("input" x)))

;;;;;;;;;;;;;;;;;;;;;;
; Test Pragma Interpreter Bare Minimal
;;;;;;;;;;;;;;;;;;;;;;

;; #[test]
;; fn test_pragma_interpreter_bare_minimal() {
;;     let program = "
(= (bar) baz)
(= (foo) (bar))
;;         !(eval (foo))
;;         !(pragma! interpreter bare-minimal)
;;         !(eval (foo))
;;     ";
;;     assert_eq!(metta_results!(run_program(program)),
;;         Ok(vec![
;;             vec![expr!("baz")],
;;             vec![UNIT_ATOM()],
;;             vec![expr!(("bar"))],
;;         ]));
;; }
!(assertEqual (eval (foo)) baz)
!(assertEqual (pragma! interpreter bare-minimal) ())
!(assertEqual (eval (foo)) bar)

;;;; tref.register_token(regex(r"if-equal"), move |_| { is_equivalent.clone() });
;;;; tref.register_token(regex(r"register-module!"), move |_| { register_module_op.clone() });
;;;; tref.register_token(regex(r"mod-space!"), move |_| { mod_space_op.clone() });
;;;; tref.register_token(regex(r"print-mods!"), move |_| { print_mods_op.clone() });
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Un-commented Rust Tests with Original Source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original: 
;; #[test]
;; fn println_op() {
;;     assert_eq!(PrintlnOp{}.execute(&mut vec![sym!("A")]), unit_result());
;; }
;; Converted:
!(assertEqual (println! "A") ())

;; Original: 
;; #[test]
;; fn trace_op() {
;;     assert_eq!(TraceOp{}.execute(&mut vec![sym!("\"Here?\""), sym!("42")]),
;;                Ok(vec![sym!("42")]));
;; }
;; Converted:
!(assertEqual (trace! "Here?" 42) 42)

;; Original:
;; #[test]
;; fn nop_op() {
;;     assert_eq!(NopOp{}.execute(&mut vec![]), unit_result());
;; }
;; Converted:
!(assertEqual (nop) ())

;; Original:
;; #[test]
;; fn let_op() {
;;     assert_eq!(LetOp{}.execute(&mut vec![expr!(a b), expr!("A" "B"), expr!(b a)]),
;;         Ok(vec![expr!("B" "A")]));
;; }
;; Converted:
!(assertEqual (($a $b) (A B) ($b $a)) (B A))

;; Original:
;; #[test]
;; fn let_op_external_vars_at_right_are_kept_untouched() {
;;     assert_eq!(LetOp{}.execute(&mut vec![expr!(t), expr!(ext), expr!(t)]),
;;         Ok(vec![expr!(ext)]));
;;     assert_eq!(LetOp{}.execute(&mut vec![expr!(t), expr!(ext "A"), expr!(t)]),
;;         Ok(vec![expr!(ext "A")]));
;; }
;; Converted:
!(assertEqual ($t $ext $t) $ext)
!(assertEqual ($t ("ext" "A") $t) ("ext" "A"))

;; Original:
;; #[test]
;; fn let_op_internal_variables_has_priority_in_template() {
;;     assert_eq!(LetOp{}.execute(&mut vec![expr!(x), expr!(x "A"), expr!(x)]),
;;         Ok(vec![expr!(x "A")]));
;; }
;; Converted:
!(assertEqual ($x ($x A) $x) ($x A))

;; Original:
;; #[test]
;; fn let_op_keep_variables_equalities_issue290() {
;;     assert_eq_metta_results!(run_program("!(let* (($f f) ($f $x)) $x)"), Ok(vec![vec![expr!("f")]]));
;;     assert_eq_metta_results!(run_program("!(let* (($f $x) ($f f)) $x)"), Ok(vec![vec![expr!("f")]]));
;;     assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $y A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
;;     assert_eq_metta_results!(run_program("!(let (quote ($x $x)) (quote ($z $y)) (let $z A ($z $y)))"), Ok(vec![vec![expr!("A" "A")]]));
;; }
;; Converted:
!(assertEqualToResults (let* (($f f) ($f $x)) $x) (f))
!(assertEqualToResults (let* (($f $x) ($f f)) $x) (f))
!(assertEqualToResults (let ((quote ($x $x))) ((quote ($z $y))) (let $y A ($z $y))) (A A))
!(assertEqualToResults (let ((quote ($x $x))) ((quote ($z $y))) (let $z A ($z $y))) (A A))

;; Original:
;; #[test]
;; fn let_op_variables_visibility_pr262() {
;;     let program = "
;;         ;; Knowledge
;;            (? P Q)
;;            (? Q R)
;;
;;         ;; Rule
;;            (= (rule (? $p $q) (? $q $r)) (? $p $r))
;;
;;         ;; Query (does not work as expected)
;;    (= (query $kb)
;;               (let* (($pq (? $p $q))
;;                      ($qr (? $q $r)))
;;              (match $kb
;;                ;; Premises
;;                (, $pq $qr)
;;                ;; Conclusion
;;                (rule $pq $qr))))
;;
;;         ;; Call
;;         !(query &self)
;;            ;; [(? P R)]
;;     ";
;;        assert_eq_metta_results!(run_program(program), Ok(vec![vec![expr!("?" "P" "R")]]));
;; }
;; Converted:
(let* (($pq (? $p $q)) ($qr (? $q $r)))
    (match $kb
      (, $pq $qr)
      (rule $pq $qr)))
!(assertEqualToResults (run_program program) (? P R))

;; Original:
;; #[test]
;; fn let_var_op() {
;;     assert_eq!(LetVarOp{}.execute(&mut vec![expr!(), sym!("B")]),
;;         Ok(vec![sym!("B")]));
;;     assert_eq!(LetVarOp{}.execute(&mut vec![expr!(((a "A"))), expr!(a)]),
;;         Ok(vec![expr!({LetOp{}} a "A" a)]));
;;     assert_eq!(LetVarOp{}.execute(&mut vec![expr!((a "A") (b "B")), expr!(b a)]),
;;         Ok(vec![expr!({LetOp{}} a "A" ({LetVarOp{}} ((b "B")) (b a)))]));
;; }
;; Converted:
!(assertEqual (let* () B) B)
!(assertEqual (let* (($a A)) $a) (let $a A $a))
!(assertEqual (let* (($a A) ($b B)) ($b $a)) (let $a A (let* (($b B)) ($b $a))))

;; Original:
;; #[test]
;; fn state_ops() {
;;     let result = NewStateOp{}.execute(&mut vec![expr!("A" "B")]).unwrap();
;;     let old_state = result.get(0).ok_or("error").unwrap();
;;     assert_eq!(old_state, &Atom::gnd(StateAtom::new(expr!("A" "B"))));
;;     let result = ChangeStateOp{}.execute(&mut vec!(old_state.clone(), expr!("C" "D"))).unwrap();
;;     let new_state = result.get(0).ok_or("error").unwrap();
;;     assert_eq!(old_state, new_state);
;;     assert_eq!(new_state, &Atom::gnd(StateAtom::new(expr!("C" "D"))));
;;     let result = GetStateOp{}.execute(&mut vec![new_state.clone()]);
;;     assert_eq!(result, Ok(vec![expr!("C" "D")]))
;; }
;; Converted:
!(assertEqual (new_state! "A" "B") (state! "A" "B"))
!(assertEqual (change_state! (state! "A" "B") ("C" "D")) (state! "C" "D"))
!(assertEqual (get_state! (state! "C" "D")) ("C" "D"))

;; Original:
;; #[test]
;; fn test_stdlib_uses_rust_grounded_tokens() {
;;     assert_eq!(run_program("!(if True ok nok)"), Ok(vec![vec![Atom::sym("ok")]]));
;; }
;; Converted:
!(assertEqualToResults (if True ok nok) (ok))

;; Original:
;; #[test]
;; fn test_let_op_inside_other_operation() {
;;     assert_eq!(run_program("!(and True (let $x False $x))"), Ok(vec![vec![expr!({Bool(false)})]]));
;; }
;; Converted:
!(assertEqualToResults (and True (let $x False $x)) (Bool(false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Converted Rust Tests with Original Source
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Original:
;; #[test]
;; fn test_quote() {
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     let parser = SExprParser::new("
;;         (= (foo) a)
;;         (= (foo) b)
;;         !(foo)
;;         !(quote (foo))
;;     ");
;;     assert_eq_metta_results!(metta.run(parser),
;;         Ok(vec![
;;             vec![expr!("a"), expr!("b")],
;;             vec![expr!("quote" ("foo"))],
;;         ]));
;; }
;; Converted:
(= (foo) a)
(= (foo) b)
!(assertEqualToResults (foo) (a b))
!(assertEqualToResults (quote (foo)) (quote (foo)))

;; Original:
;; #[test]
;; fn test_unify() {
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     let parser = SExprParser::new("
;;         !(unify (a $b 1 (d)) (a $a 1 (d)) ok nok)
;;         !(unify (a $b c) (a b $c) (ok $b $c) nok)
;;         !(unify $a (a b c) (ok $a) nok)
;;         !(unify (a b c) $a (ok $a) nok)
;;         !(unify (a b c) (a b d) ok nok)
;;         !(unify ($x a) (b $x) ok nok)
;;     ");
;;
;;     assert_eq_metta_results!(metta.run(parser),
;;         Ok(vec![
;;             vec![expr!("ok")],
;;             vec![expr!("ok" "b" "c")],
;;             vec![expr!("ok" ("a" "b" "c"))],
;;             vec![expr!("ok" ("a" "b" "c"))],
;;             vec![expr!("nok")],
;;             vec![expr!("nok")]
;;         ]));
;; }
;; Converted:
!(assertEqualToResults (unify (a $b 1 (d)) (a $a 1 (d)) ok nok) (ok))
!(assertEqualToResults (unify (a $b c) (a b $c) (ok $b $c) nok) (ok b c))
!(assertEqualToResults (unify $a (a b c) (ok $a) nok) (ok (a b c)))
!(assertEqualToResults (unify (a b c) $a (ok $a) nok) (ok (a b c)))
!(assertEqualToResults (unify (a b c) (a b d) ok nok) (nok))
!(assertEqualToResults (unify ($x a) (b $x) ok nok) (nok))

;; Original:
;; #[test]
;; fn test_empty() {
;;     let metta = Metta::new(Some(EnvBuilder::test_env()));
;;     let parser = SExprParser::new("
;;         !(empty)
;;     ");
;;
;;     assert_eq_metta_results!(metta.run(parser),
;;         Ok(vec![vec![]]));
;; }

;; Converted:
!(assertEqual (empty) ())

;; Original:
;; #[test]
;; fn sealed_op_runner() {
;;     let nested = run_program("!(sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b)))))");
;;     let simple_replace = run_program("!(sealed ($x $y) (quote (= ($y $z))))");
;;
;;     assert!(crate::atom::matcher::atoms_are_equivalent(&nested.unwrap()[0][0], &expr!("quote" ("=" (a b c) (z)))));
;;     assert!(crate::atom::matcher::atoms_are_equivalent(&simple_replace.unwrap()[0][0], &expr!("quote" ("=" (y z)))));
;; }

;; Converted:
!(assertEqual (sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b))))) (quote (= (a b c) (z))))
!(assertEqual (sealed ($x $y) (quote (= ($y $z)))) (quote (= (y z))))

;; Original:
;; #[test]
;; fn sealed_op_execute() {
;;     let val = SealedOp{}.execute(&mut vec![expr!(x y), expr!("="(y z))]);
;;     assert!(crate::atom::matcher::atoms_are_equivalent(&val.unwrap()[0], &expr!("="(y z))));
;; }

;; Converted:
!(assertEqual (sealedOpExecute (x y) (= (y z))) (= (y z)))

;; Original:
;; #[test]
;; fn use_sealed_to_make_scoped_variable() {
;;     assert_eq!(run_program("!(let $x (input $x) (output $x))"), Ok(vec![vec![expr!("output" ("input" x))]]));
;;     assert_eq!(run_program("!(let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
;;            (let $sv (input $x) $st))"), Ok(vec![vec![expr!("output" ("input" x))]]));
;; }

;; Converted:
!(assertEqual (let $x (input $x) (output $x)) ("output" ("input" x)))
!(assertEqual (let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x)))) (let $sv (input $x) $st)) ("output" ("input" x)))

;; #[derive(Clone, PartialEq, Debug)]
;; pub struct SomeGndAtom { }

;; impl Display for SomeGndAtom {
;;     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
;;         write!(f, "some-gnd-atom")
;;     }
;; }

;; impl Grounded for SomeGndAtom {
;;     fn type_(&self) -> Atom {
;;         Atom::expr([ARROW_SYMBOL, sym!("Arg1Type"), sym!("Arg2Type"), sym!("ReturnType")])
;;     }
;; }
