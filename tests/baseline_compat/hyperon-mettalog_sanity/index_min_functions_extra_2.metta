; Test frog reasoning example to deduce that Fritz is a frog
; Define that Fritz croaks and eats-flies
!(add-atom &self (= (is Fritz croaks) True))
!(add-atom &self (= (is Fritz eats-flies) True))

; Define that Tweety chirps, is yellow, and eats-flies
!(add-atom &self (= (is Tweety chirps) True))
!(add-atom &self (= (is Tweety yellow) True))
!(add-atom &self (= (is Tweety eats-flies) True))

; Define a rule: if someone croaks and eats-flies, they are a frog
!(add-atom &self (= (if (and (is $x croaks) (is $x eats-flies)) (= (is $x frog) True) Empty)))

; Test that Fritz is deduced to be a frog
!(assertEqualToResult (metta (is Fritz frog) %Undefined% &self) ((True)))

; Test that Tweety is not deduced to be a frog
!(assertEqualToResult (metta (is Tweety frog) %Undefined% &self) (()))

; Test that variable keeps value in different sub-expressions
; Define equality and addition functions
!(add-atom &self (= (eq $x $x) True))
!(add-atom &self (= (plus Z $y) $y))
!(add-atom &self (= (plus (S $k) $y) (S (plus $k $y))))

; Test that (eq (plus Z $n) $n) evaluates to True
!(assertEqualToResult (metta (eq (plus Z $n) $n) %Undefined% &self) ((True)))

; Test that (eq (plus (S Z) $n) $n) evaluates to False (empty result)
!(assertEqualToResult (metta (eq (plus (S Z) $n) $n) %Undefined% &self) (()))

; Test that operation is an expression
; Define types and functions
!(add-atom &self (: foo (-> (-> A A))))
!(add-atom &self (: a A))
!(add-atom &self (= (foo) bar))
!(add-atom &self (= (bar $x) $x))

; Test that ((foo) a) evaluates to 'a'
!(assertEqualToResult (metta ((foo) a) %Undefined% &self) ((a)))

; Test handling of bad type error
; Define atom with type 'myType' and function 'id_a' of type (-> A A)
!(add-atom &self (: myAtom myType))
!(add-atom &self (: id_a (-> A A)))
!(add-atom &self (= (id_a $a) $a))

; Test that (id_a myAtom) results in a bad type error
!(assertEqualToResult (metta (id_a myAtom) %Undefined% &self) ((Error myAtom "BadType")))

; Test error when incorrect number of arguments are provided
; Define function 'foo' of type (-> A B C)
!(add-atom &self (: a A))
!(add-atom &self (: b B))
!(add-atom &self (: c C))
!(add-atom &self (: foo (-> A B C)))
!(add-atom &self (= (foo $a $b) c))

; Test that (foo a b) evaluates to 'c' (correct number of arguments)
!(assertEqualToResult (metta (foo a b) %Undefined% &self) ((c)))

; Test that (foo a) results in an incorrect number of arguments error
!(assertEqualToResult (metta (foo a) %Undefined% &self) ((Error (foo a) "IncorrectNumberOfArguments")))

; Test that 'Error' can be used as an argument and has appropriate types
; Test get-type of 'Error', expecting (-> Atom Atom ErrorType)
!(assertEqualToResult (get-type Error) (((-> Atom Atom ErrorType))))

; Test get-metatype of 'Error', expecting 'Symbol'
!(assertEqualToResult (get-metatype Error) ((Symbol)))

; Test get-type of an 'Error' expression, expecting 'ErrorType'
!(assertEqualToResult (get-type (Error Foo Boo)) ((ErrorType)))

; Test constructing an 'Error' expression with invalid arguments
!(assertEqualToResult (Error (+ 1 2) (+ 1 +)) ((Error (+ 1 2) (+ 1 +))))

; Test parsing of strings with special characters
; Test that '!(id "test")' returns "test"
!(assertEqualToResult (id "test") (("test")))

; Test that '!(id "te st")' returns "te st"
!(assertEqualToResult (id "te st") (("te st")))

; Test that '!(id "te\"st")' returns 'te"st'
!(assertEqualToResult (id "te\"st") (("te\"st")))

; Test that '!(id "")' returns empty string
!(assertEqualToResult (id "") (("")))

; Test that '!(id "te\nst")' returns 'te\nst'
!(assertEqualToResult (id "te\nst") (("te\nst")))

; Test that '!("te\nst"test)' returns a tuple with 'te\nst' and 'test'
!(assertEqualToResult ("te\nst" test) ((("te\nst") test)))

; Test metta_superpose with function calls
; Define functions 'foo' and 'bar'
!(add-atom &self (= (foo) FOO))
!(add-atom &self (= (bar) BAR))

; Test superpose with expressions ((foo) (bar) BAZ)
!(assertEqualToResult (superpose ((foo) (bar) BAZ)) ((FOO BAR BAZ)))

; Test metta_collapse to select one of multiple possible values
; Define multiple possible values for 'color'
!(add-atom &self (= (color) red))
!(add-atom &self (= (color) green))
!(add-atom &self (= (color) blue))

; Test collapse of 'color', expecting one of red, green, or blue
!(assertEqualToResult (collapse (color)) ((red)) ((green)) ((blue)))

; Test metta_let_novar with no variable bindings
; Expecting 'result'
!(assertEqualToResult (let () result) ((result)))

; Test metta_let_novar with variable substitution
; Substitute in (P $b $a)
!(assertEqualToResult (let (P A $b) (P $a B) (P $b $a)) ((P B A)))

; Test metta_let_var with multiple variable bindings
; Binding ((P A $b) (P $a B))
!(assertEqualToResult (let* ( ((P A $b) (P $a B)) ) (P $b $a)) ((P B A)))

; Test metta_quote and metta_unquote operations
; Define functions 'foo' and 'bar'
!(add-atom &self (= (foo) A))
!(add-atom &self (= (bar $x) $x))

; Use quote to prevent evaluation
!(assertEqualToResult (bar (quote (foo))) ((quote (foo))))

; Use unquote to force evaluation before 'bar' is called
!(assertEqualToResult (bar (unquote (quote (foo)))) ((A)))

; Use unquote after 'bar' is called
!(assertEqualToResult (unquote (bar (quote (foo)))) ((A)))

; Test get-doc for a function call
; Define function 'some-func' with types and documentation
!(add-atom &self (: Arg1Type Type))
!(add-atom &self (: Arg2Type Type))
!(add-atom &self (: RetType Type))
!(add-atom &self (: some-func (-> Arg1Type Arg2Type RetType)))
!(add-atom &self (@doc some-func
  (@desc "Test function")
  (@params (
    (@param "First argument")
    (@param "Second argument")
  ))
  (@return "Return value")
))

; Test get-doc for function call (some-func a b)
!(assertEqualToResult (get-doc (some-func a b)) ((@doc-formal
  (@item some-func)
  (@kind function)
  (@type (-> Arg1Type Arg2Type RetType))
  (@desc "Test function")
  (@params (
    (@param "First argument")
    (@param "Second argument")
  ))
  (@return "Return value")
)))

; Test get-doc for a function without type information
!(add-atom &self (@doc some-func-no-type
  (@desc "Test function without type")
  (@params (
    (@param "First argument")
    (@param "Second argument")
  ))
  (@return "Return value")
))

; Test get-doc for 'some-func-no-type', expecting Undefined type
!(assertEqualToResult (get-doc some-func-no-type) ((@doc-formal
  (@item some-func-no-type)
  (@kind function)
  (@type %Undefined%)
  (@desc "Test function without type")
  (@params (
    (@param "First argument")
    (@param "Second argument")
  ))
  (@return "Return value")
)))

; Test handling of Error when used as an argument
; Test get-type of 'Error', expecting (-> Atom Atom ErrorType)
!(assertEqualToResult (get-type Error) (((-> Atom Atom ErrorType))))

; Test get-metatype of 'Error', expecting 'Symbol'
!(assertEqualToResult (get-metatype Error) ((Symbol)))

; Test get-type of an 'Error' expression, expecting 'ErrorType'
!(assertEqualToResult (get-type (Error Foo Boo)) ((ErrorType)))

; Test constructing an 'Error' expression with invalid arguments
!(assertEqualToResult (Error (+ 1 2) (+ 1 +)) ((Error (+ 1 2) (+ 1 +))))


; Test get-type operation for expression (a b) with multiple types
; Define types and atom types
!(add-atom &self (: A Type))
!(add-atom &self (: AA Type))
!(add-atom &self (: B Type))
!(add-atom &self (: BB Type))
!(add-atom &self (: a A))
!(add-atom &self (: a AA))
!(add-atom &self (: b B))
!(add-atom &self (: b BB))

; Test get-type of (a b), expecting multiple possible types
!(assertEqualToResult (get-type (a b) &self) (((A B) (AA B) (A BB) (AA BB) %Undefined%)))

; Test that an operation can be an expression
; Define higher-order function 'foo' and 'bar'
!(add-atom &self (: foo (-> (-> A A))))
!(add-atom &self (: a A))
!(add-atom &self (= (foo) bar))
!(add-atom &self (= (bar $x) $x))

; Test that applying 'foo' to 'a' evaluates to 'a'
!(assertEqualToResult (metta ((foo) a) %Undefined% &self) ((a)))

; Test handling of conflicting variable names
; Define function with conflicting variable names
!(add-atom &self (= (b ($x $y)) (c $x $y)))

; Test that variable names are correctly handled
!(assertEqualToResult (metta (a (b $a) $x $y) %Undefined% &self) ((a (c $a) $x $y)))

; Test sealed operation to create scoped variables
; Using 'sealed' to prevent variable capture
!(assertEqualToResult (sealed ($x) (sealed ($a $b) (quote (= ($a $x $c) ($b))))) ((quote (= ($a $x $c) ($b)))))

; Test that 'sealed' prevents variable capture during execution
!(assertEqualToResult (sealed ($x $y) (quote (= ($y $z)))) ((quote (= ($y $z)))))

; Test using 'sealed' to make scoped variables
; Using 'sealed' in a 'let' expression
!(assertEqualToResult (let (quote ($sv $st)) (sealed ($x) (quote ($x (output $x))))
               (let $sv (input $x) $st)) ((output (input $x))))

; Test pragma interpreter with bare-minimal mode
; Define function 'foo' that depends on 'bar'
!(add-atom &self (= (bar) baz))
!(add-atom &self (= (foo) (bar)))

; we probably wont support this in MeTTaLog
!(pragma! interpreter bare-minimal)

; Test that '!(foo)' does not evaluate in bare-minimal interpreter
!(assertEqualToResult (foo) ((foo)))

; Use 'eval' to force evaluation
!(assertEqualToResult (eval (foo)) ((baz)))
