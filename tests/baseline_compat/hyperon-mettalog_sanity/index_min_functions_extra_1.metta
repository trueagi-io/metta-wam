; Test run-metta using nested context, expecting (foo a) evaluates to 'a' in nested space
!(bind! &outer-space (new-space))
!(bind! &nested-space (new-space))
!(add-atom &nested-space (= (foo $x) $x))
!(assertEqualToResult (metta (foo a) Atom &nested-space) ((foo a)))
!(assertEqualToResult (metta (foo a) Symbol &nested-space) (a))
!(assertEqualToResult (metta (foo a) %Undefined% &nested-space) (a))

; Test get-type operation for atom 'A' in a space with types B and C
!(bind! &type-space (new-space))
!(add-atom &type-space (: B Type))
!(add-atom &type-space (: C Type))
!(add-atom &type-space (: A B))
!(add-atom &type-space (: A C))
; Expecting types B and C for atom 'A'
!(H-E-Fails (assertEqualToResult (evalc (get-type A) &type-space) (B C)))
!(H-E-Fails (assertEqualToResult (metta (get-type A) %Undefined% &type-space) (B C)))
!(assertEqualToResult (metta (get-type-space &type-space A) %Undefined% &type-space) (B C))
!(assertEqualToResult (evalc (get-type-space &type-space A) &type-space) (B C))

; Test get-type operation with non-valid atom, expecting 'String' type
!(bind! &nonvalid-space (new-space))
!(add-atom &nonvalid-space (: f (-> Number String)))
!(add-atom &nonvalid-space (: 42 Number))
!(add-atom &nonvalid-space (: "test" String))
; Expecting 'String' when applying 'f' to 42
!(assertEqualToResult (get-type-space &nonvalid-space (f 42)) (String))
; Expecting Undefined when applying 'f' to "test" (invalid argument)
!(assertEqualToResult (get-type-space &nonvalid-space (f "test")) ())

; Test car-atom with expression (A $b), expecting 'A'
!(assertEqualToResult (eval (car-atom (A $b))) (A))

; Test car-atom with expression ($a B), expecting '$a'
!(assertEqualToResult (eval (car-atom ($a B))) ($a))

; Test car-atom with empty expression, expecting error
!(H-E-Fails (assertEqual (eval (car-atom ())) (Error (car-atom ()) "car-atom expects a non-empty expression as an argument")))

; Test car-atom with atom A, expecting error
!(H-E-Fails (assertEqualToResult (eval (car-atom A)) ((Error (car-atom A) "car-atom expects a non-empty expression as an argument"))))

; Test cdr-atom with expression (a b c), expecting (b c)
!(assertEqualToResult (cdr-atom (a b c)) ((b c)))

; Test cdr-atom with expression ($a b $c), expecting (b $c)
!(assertEqualToResult (cdr-atom ($a b $c)) ((b $c)))

; Test cdr-atom with empty expression, expecting error
!(H-E-Fails (assertEqualToResult (cdr-atom ()) ((Error (cdr-atom ()) "cdr-atom expects a non-empty expression as an argument"))))

; Test cdr-atom with atom 'a', expecting error
!(H-E-Fails (assertEqualToResult (cdr-atom a) ((Error (cdr-atom a) "cdr-atom expects a non-empty expression as an argument"))))

; Test size-atom with expression (5 4 3 2 1), expecting 5
!(H-E-Fails (assertEqual (size-atom (5 4 3 2 1)) 5))

; Test size-atom with empty expression, expecting 0
!(H-E-Fails (assertEqualToResult (size-atom ()) (0)))

; Test min-atom with numbers (5 4 5.5), expecting 4
!(H-E-Fails (assertEqualToResult (min-atom (5 4 5.5)) (4)))

; Test min-atom with empty expression, expecting error
!(assertEqualToResult (min-atom ()) ((Error (min-atom ()) Empty expression)))

; Test min-atom with non-number elements (3 A B 5), expecting error
!(assertEqualToResult (min-atom (3 A B 5)) ((Error (min-atom 3 A B 5) "Only numbers are allowed in expression")))

; Test max-atom with numbers (5 4 5.5), expecting 5.5
!(assertEqualToResult (max-atom (5 4 5.5)) (5.5))

; Test max-atom with empty expression, expecting error
!(assertEqualToResult (max-atom ()) ((Error (max-atom ()) "Empty expression")))

; Test max-atom with non-number elements (3 A B 5), expecting error
!(assertEqualToResult (max-atom (3 A B 5)) ((Error (max-atom 3 A B 5) "Only numbers are allowed in expression")))

; Test index-atom with expression (5 4 3 2 1) and index 2, expecting 3
!(assertEqualToResult (index-atom (5 4 3 2 1) 2) (3))

; Test index-atom with expression (A B C D E) and index 5, expecting error
!(assertEqualToResult (index-atom (A B C D E) 5) ((Error (index-atom (A B C D E) 5) "Index is out of bounds")))

; Test random-int between 0 and 5, expecting value in [0,5)
!(assertEqualToResult (chain (eval (random-int 0 5)) $rint (and (>= $rint 0) (< $rint 5))) (True))

; Test random-int with zero range (0,0), expecting error
!(assertEqualToResult (random-int 0 0) ((Error (random-int 0 0) "Range is empty")))

; Test random-float between 0.0 and 5.0, expecting value in [0.0,5.0)
!(assertEqualToResult (chain (eval (random-float 0.0 5.0)) $rfloat (and (>= $rfloat 0.0) (< $rfloat 5.0))) (True))

; Test random-float with invalid range (0, -5), expecting error
!(assertEqualToResult (random-float 0 -5) ((Error (random-float 0 -5) "Range is empty")))

; Test switch operation with matching patterns, expecting (B A)
!(assertEqualToResult (eval (switch (A $b) ( (($a B) ($b $a)) ((B C) (C B)) ))) ((B A)))

; Test switch operation with no matching patterns, expecting empty result
!(assertEqualToResult (eval (switch (A $b) ( ((B C) (C B)) ((D E) (E B)) ))) ())

; Test is-function with functional type, expecting True
!(assertEqualToResult (eval (is-function (-> $t))) (True))

; Test is-function with non-functional type, expecting False
!(assertEqualToResult (eval (is-function (A $t))) (False))

; Test type-cast with matching type, expecting 'a'
!(add-atom &self (: a A))
!(assertEqualToResult (eval (type-cast a A &self)) (a))

; Test type-cast with non-matching type, expecting error
!(assertEqualToResult (eval (type-cast a B &self)) ((Error a "BadType")))

; Test filter-atom to remove errors, expecting (a c)
!(assertEqualToResult (eval (filter-atom (a (Error (b) "Test error") $c) $x (eval (if-error $x False True)))) ((a c)))

; Test map-atom to apply function to each element, expecting mapped elements
!(assertEqualToResult (eval (map-atom (a (b) $c) $x (mapped $x))) (( (mapped a) (mapped (b)) (mapped $c) )))

; Test foldl-atom to sum elements, expecting total sum
!(assertEqualToResult (eval (foldl-atom (1 2 3) 0 $a $b (eval (+ $a $b)))) (6))

; Test metta interpreting single atom as Atom type
!(assertEqualToResult (metta A Atom &self) (A))

; Test metta interpreting symbol as its type
!(add-atom &self (: a A))
!(assertEqualToResult (metta a A &self) (a))

; Test metta interpreting expression with function
!(add-atom &self (= (foo $x) $x))
!(assertEqualToResult (metta (foo a) %Undefined% &self) (a))

; Test assertEqual operation with matching expressions, expecting success
!(assertEqual (foo A) (foo A))

; Test assertEqual operation with non-matching expressions, expecting error
!(assertNotEqual (foo A) (foo B))

; Test superpose operation with multiple expressions, expecting a list of results
!(assertEqualToResult (superpose (red yellow green)) ((red yellow green)))

; Test collapse operation, expecting one of the possible values
!(add-atom &self (= (color) red))
!(add-atom &self (= (color) green))
!(add-atom &self (= (color) blue))
!(assertEqualToResult (collapse (color)) (red green blue))

; Test let operation without variable bindings, expecting 'result'
!(assertEqualToResult (let () result) ((result)))

; Test let operation with variable binding, expecting substituted result
!(assertEqualToResult (let (P A $b) (P $a B) (P $b $a)) ((P B A)))

; Test quote and unquote operations
!(add-atom &self (= (foo) A))
!(add-atom &self (= (bar $x) $x))
; Using quote to prevent evaluation
!(assertEqualToResult (bar (quote (foo))) ((quote (foo))))
; Using unquote to force evaluation
!(assertEqualToResult (bar (unquote (quote (foo)))) ((A)))

