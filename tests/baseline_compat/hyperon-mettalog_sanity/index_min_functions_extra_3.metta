; Test mod-space! operation to import a module into a new space
!(bind! &new-space (new-space))
; Add the stdlib module to the new space
!(add-atom &new-space (mod-space! stdlib))
; Get atoms from the new space, expecting to include stdlib module
!(assertEqualToResult (get-atoms &new-space) ((mod-space! stdlib)))

; Test removing duplicated atom
; Create a new space
!(bind! &space (new-space))
; Add 'a' multiple times to the space
!(add-atom &space a)
!(add-atom &space a)
!(add-atom &space a)
; Remove 'a' once, expecting 'a' to remain twice
!(remove-atom &space a)
; Get atoms from space, expecting two 'a's
!(assertEqualToResult (get-atoms &space) ((a a)))

; Test that an expression with multiple types returns all types
; Define types and assign types to 'a' and 'b'
!(add-atom &self (: A Type))
!(add-atom &self (: AA Type))
!(add-atom &self (: B Type))
!(add-atom &self (: BB Type))
!(add-atom &self (: a A))
!(add-atom &self (: a AA))
!(add-atom &self (: b B))
!(add-atom &self (: b BB))
; Test get-type of (a b), expecting multiple types
!(assertEqualToResult (get-type (a b) &self) (((A B) (AA B) (A BB) (AA BB) %Undefined%)))

; Test index-atom operation with valid index
; Test index-atom with index 2 in list (5 4 3 2 1), expecting 3
!(assertEqualToResult (index-atom (5 4 3 2 1) 2) ((3)))

; Test index-atom operation with out-of-bounds index
; Test index-atom with index 5 in list (A B C D E), expecting error
!(assertEqualToResult (index-atom (A B C D E) 5) ((Error (index-atom (A B C D E) 5) "Index is out of bounds")))

; Test handling of empty space during removal
; Create a new space
!(bind! &space (new-space))
; Add atom 'a' to the space
!(add-atom &space a)
; Attempt to remove atom 'b' which is not in the space, expecting False
!(assertEqualToResult (remove-atom &space b) ((False)))

; Test that an operation can be an expression
; Define higher-order function 'foo' and 'bar'
!(add-atom &self (: foo (-> (-> A A))))
!(add-atom &self (: a A))
!(add-atom &self (= (foo) bar))
!(add-atom &self (= (bar $x) $x))
; Test that applying 'foo' to 'a' evaluates to 'a'
!(assertEqualToResult (metta ((foo) a) %Undefined% &self) ((a)))

; Test variable name conflict resolution
; Define a function 'b' that takes a tuple and returns a 'c' expression
!(add-atom &self (= (b ($x $y)) (c $x $y)))
; Test that variable names are correctly handled to avoid conflicts
!(assertEqualToResult (metta (a (b $a) $x $y) %Undefined% &self) ((a (c $a $y) $x $y)))

; Test sealed operation to create scoped variables
; Use 'sealed' to prevent variable capture
!(assertEqualToResult (sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b))))) ((quote (= ($a $x $c) ($b)))))
; Use 'sealed' to replace variables uniquely
!(assertEqualToResult (sealed ($x $y) (quote (= ($y $z)))) ((quote (= ($y $z)))))

; Test using 'sealed' in a 'let' expression to create scoped variables
; Use 'sealed' to prevent variable capture in 'let' bindings
!(assertEqualToResult (let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
               (let $sv (input $x) $st)) ((output (input $x))))

; Test pragma interpreter with bare-minimal mode
; Define functions 'foo' and 'bar'
!(add-atom &self (= (bar) baz))
!(add-atom &self (= (foo) (bar)))
; Test that '!(foo)' does not evaluate in default interpreter
!(assertEqualToResult (foo) ((baz)))
; Switch to bare-minimal interpreter
!(pragma! interpreter bare-minimal)
; Test that '!(foo)' does not evaluate in bare-minimal interpreter
!(assertEqualToResult (foo) ((foo)))
; Use 'eval' to force evaluation in bare-minimal interpreter
!(assertEqualToResult (eval (foo)) ((baz)))

; Test that Error can be used as an argument and has appropriate types
; Test get-type of 'Error', expecting (-> Atom Atom ErrorType)
!(assertEqualToResult (get-type Error) (((-> Atom Atom ErrorType))))
; Test get-metatype of 'Error', expecting 'Symbol'
!(assertEqualToResult (get-metatype Error) ((Symbol)))
; Test get-type of an 'Error' expression, expecting 'ErrorType'
!(assertEqualToResult (get-type (Error Foo Boo)) ((ErrorType)))
; Test constructing an 'Error' expression with invalid arguments
!(assertEqualToResult (Error (+ 1 2) (+ 1 +)) ((Error (+ 1 2) (+ 1 +))))

; Test string parsing with various inputs
; Test that '!(id "test")' returns ("test")
!(assertEqualToResult (id "test") (("test")))
; Test that '!(id "te st")' returns ("te st")
!(assertEqualToResult (id "te st") (("te st")))
; Test that '!(id "te\"st")' returns ("te\"st")
!(assertEqualToResult (id "te\"st") (("te\"st")))
; Test that '!(id "")' returns ((""))
!(assertEqualToResult (id "") (("")))
; Test that '!(id "te\nst")' returns ("te\nst")
!(assertEqualToResult (id "te\nst") (("te\nst")))
; Test that '!("te\nst" test)' returns (("te\nst" test))
!(assertEqualToResult ("te\nst" test) (("te\nst" test)))

; Test that an expression with multiple function types resolves correctly
; Define functions 'f_sym', 'f_expr', and 'f_var' with different types
!(add-atom &self (: f_sym (-> Symbol D)))
!(add-atom &self (: f_expr (-> Expression D)))
!(add-atom &self (: f_var (-> Variable D)))
!(add-atom &self (: b B))
; Test applying 'f_sym' to an expression, expecting 'D'
!(assertEqualToResult (get-type (f_sym (b))) ((D)))
; Test applying 'f_expr' to an expression, expecting 'D'
!(assertEqualToResult (get-type (f_expr (b))) ((D)))
; Test applying 'f_var' to an expression, expecting 'D'
!(assertEqualToResult (get-type (f_var (b))) ((D)))

; Test that variables keep their value in different sub-expressions
; Define equality and addition functions
!(add-atom &self (= (eq $x $x) True))
!(add-atom &self (= (plus Z $y) $y))
!(add-atom &self (= (plus (S $k) $y) (S (plus $k $y))))
; Test that (eq (plus Z $n) $n) evaluates to True
!(assertEqualToResult (metta (eq (plus Z $n) $n) %Undefined% &self) ((True)))
; Test that (eq (plus (S Z) $n) $n) evaluates to False
!(assertEqualToResult (metta (eq (plus (S Z) $n) $n) %Undefined% &self) (()))

; Test that variables defined via other variables are handled correctly
; Define functions 'myif', 'mynot', 'a', and 'b'
!(add-atom &self (= (myif T $y) $y))
!(add-atom &self (= (mynot F) T))
!(add-atom &self (= (a $z) (mynot (b $z))))
!(add-atom &self (= (b d) F))
; Test that (myif (a $x) $x) evaluates correctly
!(assertEqualToResult (metta (myif (a $x) $x) %Undefined% &self) ((d)))

; Test that variable name conflicts are resolved by renaming
; Define a function with a potential variable name conflict
!(add-atom &self (= (a ($W)) True))
; Test that matching 'a' with variable $W works correctly
!(assertEqualToResult (metta (a $W) %Undefined% &self) ((True)))

; Test that operations can be higher-order functions
; Define types and functions
!(add-atom &self (: foo (-> (-> A A))))
!(add-atom &self (: a A))
!(add-atom &self (= (foo) bar))
!(add-atom &self (= (bar $x) $x))
; Test that ((foo) a) evaluates to 'a'
!(assertEqualToResult (metta ((foo) a) %Undefined% &self) ((a)))

; Test the use of sealed operation for scope management
; Test that sealed variables prevent name clashes
!(assertEqualToResult (sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b))))) ((quote (= ($a $x $c) ($b)))))
; Test that sealed variables are uniquely replaced
!(assertEqualToResult (sealed ($x $y) (quote (= ($y $z)))) ((quote (= ($y $z)))))

; Test using sealed to create scoped variables in 'let' expressions
!(assertEqualToResult (let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
               (let $sv (input $x) $st)) ((output (input $x))))

; Test that the interpreter can switch to bare-minimal mode
; Define functions 'foo' and 'bar'
!(add-atom &self (= (bar) baz))
!(add-atom &self (= (foo) (bar)))
; Test that '!(foo)' evaluates to 'baz' in default interpreter
!(assertEqualToResult (foo) ((baz)))
; Switch to bare-minimal interpreter
!(pragma! interpreter bare-minimal)
; Test that '!(foo)' does not evaluate in bare-minimal interpreter
!(assertEqualToResult (foo) ((foo)))
; Use 'eval' to force evaluation in bare-minimal interpreter
!(assertEqualToResult (eval (foo)) ((baz)))

; Test handling of empty expressions in metta_interpret
; Test that interpreting an empty expression returns an empty tuple
!(assertEqualToResult (metta () %Undefined% &self) (()))

; Test that interpreting an expression with unknown function returns the expression itself
!(assertEqualToResult (metta (unknown-func a) %Undefined% &self) ((unknown-func a)))

; Test that interpreting an expression with known function evaluates correctly
!(add-atom &self (= (known-func $x) ($x $x)))
!(assertEqualToResult (metta (known-func a) %Undefined% &self) ((a a)))

; Test that metta can interpret expressions with multiple possible outcomes
; Define multiple definitions for 'color'
!(add-atom &self (= (color) blue))
!(add-atom &self (= (color) red))
!(add-atom &self (= (color) green))
; Test that interpreting 'color' returns all possible colors
!(assertEqualToResult (metta (color) %Undefined% &self) ((blue red green)))

; Test that variables are correctly assigned in higher-order functions
; Define functions 'b' and 'c'
!(add-atom &self (= (b ($x $y)) (c $x $y)))
; Test that variables in 'b' are correctly handled
!(assertEqualToResult (metta (a (b $a) $x $y) %Undefined% &self) ((a (c $a $y) $x $y)))

; Test that error is returned when incorrect number of arguments are provided
; Define function 'foo' with two arguments
!(add-atom &self (: foo (-> A B C)))
!(add-atom &self (: a A))
!(add-atom &self (: b B))
!(add-atom &self (: c C))
!(add-atom &self (= (foo $a $b) c))
; Test that calling 'foo' with correct arguments returns 'c'
!(assertEqualToResult (metta (foo a b) %Undefined% &self) ((c)))
; Test that calling 'foo' with insufficient arguments returns an error
!(assertEqualToResult (metta (foo a) %Undefined% &self) ((Error (foo a) "IncorrectNumberOfArguments")))

; Test that metta can handle function calls with variables as types
; Define a function 'id_num' that returns its argument if it's a number
!(add-atom &self (= (id_num $x) $x))
; Test that 'id_num' returns 'myAtom' even if it's not a number (since types are not enforced)
!(assertEqualToResult (metta (id_num myAtom) %Undefined% &self) ((myAtom)))

; Test that metta returns a 'BadType' error when type checking fails
; Define 'myAtom' with type 'myType' and function 'id_a' of type (-> A A)
!(add-atom &self (: myAtom myType))
!(add-atom &self (: id_a (-> A A)))
!(add-atom &self (= (id_a $a) $a))
; Test that calling 'id_a' with 'myAtom' returns a 'BadType' error
!(assertEqualToResult (metta (id_a myAtom) %Undefined% &self) ((Error myAtom "BadType")))
