

(is-mettalog)

;; !(pragma! UndeclaredArg Atom)
;; !(pragma! UndeclaredArg %Undefined%)
;; !(pragma! UndeclaredReturn %Undefined%)
;; !(pragma! UndeclaredReturn Atom)

(: RelationType Type)
(:> RelationType Type)
(: DataFunctor RelationType)
(: Constructor RelationType)

(: ==> (-> Atom Atom Atom))

(==> (, $A $_) $A)
(==> (, $_ $B) $B)
(==> (, $A $B $C) (, (, $A $B) $C))
(==> (, $A $B $C $D) (, (, $A $B $C) $D))
(==> (, $A $B $C $D $E) (, (, $A $B $C) (, $D $E)))
(==>
  (: $Rel DataFunctor)
  (, 
	(argNType $Rel $_ Atom)
	(returnType $Rel Atom)))

;(==> (: $Rel DataFunctor) (: $Rel (-> Atom)))
(==> (: $Rel DataFunctor) (: $Rel (-> Atom Atom)))
(==> (: $Rel DataFunctor) (: $Rel (-> Atom Atom Atom)))
(==> (: $Rel DataFunctor) (: $Rel (-> Atom Atom Atom Atom)))
(==> (: $Rel DataFunctor) (: $Rel (-> Atom Atom Atom Atom Atom)))
(==> (: $Rel DataFunctor) (: $Rel (-> Atom Atom Atom Atom Atom Atom)))
(==> (: $Rel DataFunctor) (: $Rel (-> Atom Atom Atom Atom Atom Atom Atom)))
(: @doc DataFunctor)
(: @desc DataFunctor)
(: @param DataFunctor)
(: @return DataFunctor)
(: @doc-formal DataFunctor)
(: @item DataFunctor)
(: : DataFunctor)
(: :> DataFunctor)
(: iz DataFunctor)


(: = DataFunctor)
(: HE= DataFunctor)

(: @doc (-> Atom Atom Atom Atom Atom))
(: @doc (-> Atom Atom Atom))



(==>
  (: $Rel Constructor)
  (, 
	(argNType $Rel $_ %Undefined%)
	(returnType $Rel Atom)))


;;(iz (@kind function) DataFunctor)
;;(iz (@kind atom) DataFunctor)
(: (@kind $_) DataFunctor)
(: @type DataFunctor)
(: @params DataFunctor)

;; Type Declarations with Documentation
(@doc Any
  (@desc "The universal type; any value belongs to this type."))
(: Any Type)

(@doc DontEvalType
  (@desc "Marks types that should not be evaluated unless forced."))
(: DontEvalType Type)

(@doc DoEvalType
  (@desc "Marks types that should be evaluated when passed as a parameter."))
(: DoEvalType Type)

(@doc MaterialType
  (@desc "Material types indicate that a value should be evaluated until it becomes this type. Examples include: String, Symbol, Expression.  and Atom and Any are not a Material types"))
(: MaterialType Type)

; In fileio
;(: Stream MaterialType)
;(type-coercion String Stream open-for-read)
(type-coercion (String Location) String repr)
(type-coercion String (String Location) parse)
(type-coercion Number String repr)
(type-coercion String Number parse)

; Material types can usually be parse'd/repr'd from String
(: Bool MaterialType)
(: Expression MaterialType)
(: (String Location) MaterialType)
(: Number MaterialType)
(: String MaterialType)
(: Symbol MaterialType)
(: Variable MaterialType)
; but not all
(: SpaceType MaterialType)
(: StateMonad MaterialType)

(@doc Atom
  (@desc "Type representing any atom."))
(: Atom Type)
(: Atom DontEvalType)

(@doc Expression
  (@desc "Type representing an S-Expression, which is a combination of atoms."))
(: Expression Type)
(:> Expression Atom)

(@doc CodeType
  (@desc "A type of Atom/Value that represents code to evaluate (such as then/else forms)"))
(: CodeType Type)
(:> CodeType DontEvalType)
(:> CodeType Type)

(@doc (-> $res)
  (@desc "A type of Atom/Value that represents code to evaluate (such as then/else forms)."))
(: (-> $res) CodeType)
(:> (-> $res) Atom)

(@doc (Expression (Eval $res))
  (@desc "Type representing an S-Expression, with several Atoms of subtype (Eval $res)."))
(: (Expression (Eval $res)) Type)
(: (Expression (Eval $res)) CodeType)
(:> (Expression (Eval $res)) Expression)

(@doc HostCode
  (@desc "Type representing a Host Expression, specially compiled by the host."))
(: HostCode Type)
(:> HostCode Atom)
(: HostCode DontEvalType)
(: HostCode CodeType)

(@doc LazyBool
  (@desc "A (-> $res) that when evaluated returns True or False but doesn't imply evaluation will always occur. Used for lazy conditional logic."))
(: LazyBool Type)
(:> LazyBool Bool)
(: LazyBool DontEvalType)
(: LazyBool CodeType)


;; Inference Rule: All types not explicitly declared as DontEvalType default to DoEvalType
(==>
   (, (: $type Type) (not (ist &corelib (: $type DontEvalType))))
   (: $type DoEvalType))

(@doc Bool
  (@desc "Boolean type of True or False."))
(: Bool Type)

(@doc Number
  (@desc "Numeric type, including integers and floating-point numbers."))
(: Number Type)

(@doc SpaceType
  (@desc "Dynamic space type, representing an Atomspace."))
(: SpaceType Type)

(@doc ReturnType
  (@desc "Type representing a function's return value."))
(: ReturnType Type)

(@doc Symbol
  (@desc "Type representing a symbol or identifier."))
(: Symbol Type)

(@doc StateMonad
  (@desc "Type representing a state monad, used for encapsulating stateful computations."))
(: StateMonad Type)

(@doc Type
  (@desc "Type representing a type."))
(: Type Type)

(@doc True
  (@desc "Boolean value representing truth."))
(: True Bool)

(@doc False
  (@desc "Boolean value representing falsehood."))
(: False Bool)

(@doc %Undefined%
  (@desc "Special type representing an undefined value or type."))
(: %Undefined% Type)
(: %Undefined% DoEvalType)

(@doc Variable
  (@desc "Type representing a variable in the language."))
(: Variable Type)
(: Variable MaterialType) ; Material types indicate somehnting should be evaluated until it becomes this type. exmamples String, Symbol, Expression, 

(@doc :
  (@desc "Type declarion operator"))
(@doc <:
  (@desc "Super Type declarion operator"))
(: : %Undefined%) ; match hyperons weirdd return value of !(get-type :)


(: if-empty (-> (-> $res) (-> $res) (-> $res) $res))
(: if-non-empty-expression (-> (-> $res) (-> $res) (-> $res) $res))
(: switch (-> %Undefined% Expression %Undefined%))
(: unify (-> Atom Atom (-> $res) (-> $res) $res))

;(: current-predicate-arity (-> Predicate Number)) ; returns only the first type of a symbol

(: get-vtype (-> Atom Atom)) ; returns only the first type of a symbol
(: get-vtype (-> Atom SpaceType Atom)) ; returns only the first type of a symbol

(: get-types (-> Atom Atom)) ; returns only the first type of a symbol
(: get-types (-> Atom SpaceType Atom)) ; returns only the first type of a symbol

(: get-dtype (-> Atom Atom)) ; returns only the data value types of a symbol
(: get-dtype (-> Atom SpaceType Atom)) ; returns only the data value types of a symbol

(: get-ftype (-> Atom Atom)) ; returns only the function types of a symbol
(: get-ftype (-> Atom SpaceType Atom)) ; returns only the function types of a symbol

(: pragma! (-> Atom Atom (->)))
;(: = (-> Atom Atom %Undefined%))
(: match (-> SpaceType MatchAtom (-> $res) $res))
;(: case (-> Expression Atom Atom))
(: combine (-> $t $t $t))
(: import! (-> SpaceType Atom (->)))


(@doc type-check
  (@desc "The value of type-check determines MeTTa's type-checking behavior. Set via pragma!. When set to auto (i.e. !(pragma! type-check auto)), types are checked immediately on adding an expression to the space. By default, when unset (or set to anything other than auto), types are checked only on evaluation. For example !(+ 1 \"2\") would trigger a type violation, but (= (foo $x) (+ $x \"2\")) would not, unless type-check is set to auto, in which case both would trigger type violations."))
(: type-check Symbol)

(iz predicate-arity MeTTaLog)

(@doc predicate-arity
  (@desc "Specifies the arity (number of arguments) for a given predicate, allowing it to be queriable in the system's match framework. This is particularly useful for enabling built-in functions, such as `size-atom`, to be used as predicates in declarative contexts and run in reverse to compute inputs based on outputs.


For example:
  ; Enable the built-in function `size-atom` that takes an atom and returns the size
    as a predicate with arity 2
  (predicate-arity size-atom 2)


  ; Now `size-atom` can be used as a predicate in pattern matching
  !(match &dyn-space '(size-atom (a b c) $size) 
         (The abc tuple was len $size))
  ; This pattern will resolve `Size = 3` and execute the action.


Additionally, by running `size-atom` in reverse, you can compute a new atom based on a desired size:
  !(match &dyn-space '(size-atom $new-atom 4) 
         (The new atom is $new-atom))
  ; This resolves `$new-atom` to a tuple of size 4, such as ($1 $2 $3 $4).


This reverse functionality is made possible because predicates can describe relationships, allowing you to infer inputs from outputs.")
  (@params (
    (@param "Predicate symbol" "The name of the predicate whose arity is being defined.")
  ))
  (@return "The number of arguments required for the predicate."))
    
(: predicate-arity (-> Symbol Number))
(predicate-arity predicate-arity 2)
(function-arity predicate-arity 1)




(@doc function-arity
  (@desc "Defines the arity of a function, allowing predicates or built-in facts to also behave as callable functions. This enables procedural-style execution where the last argument of the predicate becomes the function's return value, and the system internally resolves the function using a `match` query. 


For example:
  ; Declare the built-in predicate `max` with arity 3
  (predicate-arity max 3)

  ; Enable `max` as a function
  (function-arity max 2)


  ; Define the rules for `max`
  (= (max $X $Y $Y) (<= $X $Y))
  (= (max $X $Y $X) (> $X $Y))


  ; Using `max` declaratively as a predicate
  !(match &self (max (5 10) $max)
         (The maximum is $max))
  [(The maximum is 10)]


  ; Using `max` procedurally as a function
  !(max 5 10)
  [10]
  


  ; Reverse execution with `max`
  !(let True (== (max $a $b) 10) ($a $b)) ; as a function
  [(#(exists $a (=< $a 10)) 10), (10 #(exists $b (=< 10 $b )))]
  !(match &self (max $a $b 10) ($a $b)) ; or as a predicate
  [(#(exists $a (=< $a 10)) 10), (10 #(exists $b (=< 10 $b )))]


  This dual behavior allows predicates to act as functions, bridging procedural and declarative paradigms. By defining `function-arity`, the function automatically resolves using the logic of the associated predicate.")
  (@params (
    (@param "Function symbol" "The name of the function or predicate to enable as a callable function.")
  ))
  (@return "The number of arguments expected by the function."))
(: function-arity (-> Symbol Number))
(predicate-arity function-arity 2)
(function-arity function-arity 1)

;; MettaMorph-If Function

(iz MettaMorph-If MeTTaMorph)
(@doc MettaMorph-If
  (@desc "Conditional function that evaluates and returns one of the provided atoms based on a boolean condition.")
  (@params (
    (@param "Boolean condition")
    (@param "Atom to return if condition is True")
    (@param "Atom to return if condition is False (optional)")))
  (@return "Either the second or third argument depending on the condition"))

(: MettaMorph-If (-> Bool (-> $res) $res))
(= (MettaMorph-If True $then) $then)
; (= (MettaMorph-If False $then) (empty))
(= (MettaMorph-If False $then) (let $n 0 (let $n 1 $n))) ; Placeholder for False condition

(: MettaMorph-If (-> Bool (-> $res) (-> $res) $res))
(= (MettaMorph-If $cond $then $else) (if $cond $then $else))


;; Arity Assignments

(predicate-arity predicate-arity 2)
;(predicate-arity : 3)
; (= (: $F P1) (predicate-arity $F 1))

;; Source Predicate and Function Types

(iz SrcPredicate MeTTa)
(@doc SrcPredicate
  (@desc "Type representing a source predicate."))
(: SrcPredicate Type)

(iz SrcFunction MeTTa)
(@doc SrcFunction
  (@desc "Type representing a source function."))
(: SrcFunction Type)

;; MeTTaResult Type and Values

(iz MeTTaResult MeTTa)
(@doc MeTTaResult
  (@desc "Type representing the result of a MeTTa evaluation."))
(: MeTTaResult Type)

(iz NotReducible MeTTaResult)
(@doc NotReducible
  (@desc "Result indicating that an atom cannot be reduced further."))
(: NotReducible MeTTaResult)

(iz Empty MeTTaResult)
(@doc Empty
  (@desc "Result indicating an empty evaluation result."))
(: Empty MeTTaResult)

;; Subtype Relations

(iz ValueAtom MeTTa)
(@doc ValueAtom
  (@desc "Type representing a value atom."))
(:> ValueAtom Atom)

(iz ForeignObject MeTTa)
(@doc ForeignObject
  (@desc "Type representing a foreign object, such as a Python object."))
(: ForeignObject Type)

(:> ValueAtom Grounded)
(:> ForeignObject ValueAtom)

(iz PyObject MeTTa)
(@doc PyObject
  (@desc "Type representing a Python object."))
(:> PyObject ForeignObject)

;; Space Subtype Relation

(@doc SpaceType
  (@desc "Dynamic space type, representing an Atomspace."))
(:> SpaceType Grounded)



(iz py-list MeTTa)
(@doc py-list
  (@desc "Converts a MeTTa Expression into a Python list and returns it as a PyObject.")
  (@params (
    (@param (@desc "A MeTTa List that will be converted into a Python list"))
  ))
  (@return (@desc "A Python list object that represents the given MeTTa Expression as a PyObject"))
)
(: py-list (-> Expression PyObject))

(iz py-chain MeTTa)
(@doc py-chain
  (@desc "Chains together a list of Python objects contained in a MeTTa Expression, applying the Python vertical bar | OR operation jointly to all elements.")
  (@params (
    (@param (@desc "A MeTTa list of atoms, each embedding a Python object.")) 
  ))
  (@return (@desc "A MeTTa atom which embeds the result of applying the Python OR | operator to all elements of the list."))
)
(: py-chain (-> Expression PyObject))

(iz py-eval MeTTaLog)
(@doc py-eval
  (@desc "Evaluates a Python expression from a string and returns the result as a PyObject.")
  (@params (
    (@param (@desc "A string representing a Python expression that will be evaluated"))
  ))
  (@return (@desc "The result of evaluating the Python expression as a PyObject"))
)
(: py-eval (-> String PyObject))

(iz py-exec! MeTTaLog)
(@doc py-exec!
  (@desc "Executes some Python sourcecode from a string !(py-exec! \"import torch\") -> True.  !(py-exec! \"import torche\") -> False. ")
  (@params (
    (@param (@type String) (@desc "A string representing a Python sourcecode that will be executed"))
  ))
  (@return (@desc "The True|False results of executing the Python expression"))
)
(: py-exec! (-> String Bool))


; !(import! &corelib "src/canary/stdlib_mettalog.metta")
;!(println! "!(import! &corelib \"src/canary/stdlib_mettalog.metta\")")

(iz findall! MeTTaLog)
(@doc findall!
  (@desc "Takes a Template and a Goal. Returns the list resulting from substituting all bindings from solving Goal into Template.
  See Prolog's built-in predicate findall/3.")
  (@params (
    (@param "Template")
    (@param "Goal")))
  (@return "Result list of all bindings for Goal substituted into Template"))
(: findall! (-> Expression Expression Expression))

;; Functional Programming
(iz maplist! MeTTaLog)
(@doc maplist!
  (@desc "Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). 
 The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).
 F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).
 Use concurrent-maplist! for a multi-threaded, nondeterministic version.
 See Prolog's built-in predicate maplist.")
  (@params (
    (@param "Function to be applied")
    (@param "List")))
  (@return "Result of applying Function to List(s)"))
(: maplist! (-> Function Expression Expression))
(: maplist! (-> Function Expression Expression Expression))
(: maplist! (-> Function Expression Expression Expression Expression))

;; Functional Programming
(iz concurrent-maplist! MeTTaLog)
(@doc concurrent-maplist!
  (@desc "Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). 
 The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).
 F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).
 The applications of F to the list items are processed in parallel. Because of the overhead of this approach, a speedup is only likely if F is expensive to evaluate.
 Use maplist! for a single-threaded, deterministic version.
 See Prolog's built-in predicate concurrent-maplist.")
  (@params (
    (@param "Function to be applied")
    (@param "List")))
  (@return "Result of applying Function to List(s)"))
(: concurrent-maplist! (-> Function Expression Expression))
(: concurrent-maplist! (-> Function Expression Expression Expression))
(: concurrent-maplist! (-> Function Expression Expression Expression Expression))

(iz throw MeTTaLog)
(@doc throw
  (@desc "Raises an exception. See also `catch`; the system will look for the innermost catch such that Exception unifies with Catcher.")
  (@params (
    (@param "Exception")))
  (@return "Does not return - raises an exception"))
(: throw (-> Atom ErrorType))

(iz catch MeTTaLog)
(@doc catch
  (@desc "Executes Form. If an exception is raised with `throw` during execution of Form while this is the innermost catch such that Catcher unifies with Exception, the exception is caught. Recover is then executed with bindings from Catcher.")
  (@params (
    (@param "Form")
    (@param "Catcher")
    (@param "Recover")))
  (@return "Result of Form if no exception is raised. Result of Recover (with bindings from Catcher) if an exception is caught."))
(: catch (-> (-> $res) ErrorType (-> $res) $res))

(iz max-time! MeTTaLog)
(@doc max-time!
  (@desc "Executes Form. If execution takes longer than Time, will raise a time_limit_exceeded exception. See also `catch`.")
  (@params (
    (@param "Time (in seconds)")
    (@param "Form")))
  (@return "Result of Form if execution completes within Time. Raises an exception otherwise."))
(: max-time! (-> Number (-> $res) $res))

(iz sleep! MeTTaLog)
(@doc sleep!
  (@desc "Sleeps for N seconds.")
  (@params (
    (@param "N")))
  (@return "Returns True after sleeping completes."))
(: sleep! (-> Number Bool))

(iz limit! MeTTaLog)
(@doc limit!
  (@desc "Executes Form generating at most Limit results. Results are returned as soon as they become available.")
  (@params (
    (@param "Limit")
    (@param "Form")))
  (@return "First Limit results of Form."))
(: limit! (-> Number (-> $res) $res))

(iz number-of MeTTaLog)
(@doc number-of
  (@desc "Returns the number of results Form generates")
  (@params (
    (@param "Form")))
  (@return "Number of results of Form."))
(: number-of (-> Atom Number))

(iz offset! MeTTaLog)
(@doc offset!
  (@desc "Executes Form ignoring the first Count results. Results are returned as soon as they become available.")
  (@params (
    (@param "Count")
    (@param "Form")))
  (@return "Results of Form after ignoring the first Count results that are generated."))
(: offset! (-> Number (-> $res) $res))

(iz call! MeTTaLog)
(@doc call!
  (@desc "Trampolines to Prolog's call. Only works when the predicate and each arg are provided separately. e.g. !(call! ls /) will print the root directory but !(call! ls(/)) will fail.")
  (@params (
    (@param "Form")))
  (@return "True if the call succeeds, False otherwise."))
(: call! (-> (VarArg Atom) %Undefined%))

(iz call-p! MeTTaLog)
(@doc call-p!
  (@desc "Like call-fn! except it does not append the called term with a return arg.")
  (@params (
    (@param "Form")))
  (@return "True if the call succeeds, False otherwise."))
(: call-p! (-> (VarArg Atom) Bool))

(iz call-fn! MeTTaLog)
(@doc call-fn!
  (@desc "Trampolines to Prolog's call after appending the return argument.  Each arg are provided separately. e.g. !(call-fn! length (1 2 3)) will return 3.")
  (@params (
    (@param "Form")))
  (@return "appends a return argument to a form and calls it"))
(: call-fn! (-> (VarArg Atom) Atom))


(iz call-unit! MeTTaLog)
(@doc call-unit!
  (@desc "Calls code and returns the Unit atom on Success unless there is an Error")
  (@params (
	(@param "Form to be called by host system")))
  (@return "Unit atom or Error"))
(: call-unit! (-> (VarArg MatchQ) (->)))

(iz call-for! MeTTaLog)
(@doc call-for!
  (@desc "Calls code (2nd argument...) and returns the first Argument on Success unless there is an Error")
  (@params (
	(@param "Value to be returned")
	(@param "Form to be called by host system")))
  (@return "returns the first argument"))
(: call-for! (-> (Atom $t) (VarArg MatchQ) $t))

(iz call-string! MeTTaLog)
(@doc call-string!
  (@desc "Executes String as Prolog code. For example, (call-string! \"member(1,[1,2,3])\") returns [()] while (call-string! \"member(4,[1,2,3])\") returns []. (call-string! \"member(X,[1,2,3])\") returns [(1), (2), (3)].")
  (@params (
    (@param "PrologSrc")))
  (@return "A list of the binding values. If there are no bindings but the Prolog query is True, returns the empty list, if fails returns Empty."))
(: call-string! (-> String Atom))

(iz finally MeTTaLog)
(@doc finally
  (@desc "Same as (setup-call-cleanup! true Form Cleanup). setup-call-cleanup! is recommended instead if Cleanup is intended to undo prior side-effects - place those side-effects in Setup.")
  (@params (
    (@param "Form")
    (@param "Cleanup")))
  (@return "Result of Form."))
(: finally (-> (-> $res) (-> $res) $res))

(iz call-cleanup! MeTTaLog)
(@doc call-cleanup!
  (@desc "Same as (setup-call-cleanup! true Form Cleanup). setup-call-cleanup! is recommended instead if Cleanup is intended to undo prior side-effects - place those side-effects in Setup.")
  (@params (
    (@param "Form")
    (@param "Cleanup")))
  (@return "Result of Form."))
(: call-cleanup! (-> (-> $res) (-> $res) $res))

(iz setup-call-cleanup! MeTTaLog)
(@doc setup-call-cleanup!
  (@desc "Executes Setup, then Form, then finally Cleanup. Setup is protected from interrupts (e.g. max-time!). In most uses, Setup will perform temporary side-effects required by Form that are finally undone by Cleanup. Cleanup is run even if Form raises an exception. For each result of Setup, Form is run to completion, then Cleanup is run.")
  (@params (
    (@param "Setup")
    (@param "Form")
    (@param "Cleanup")))
  (@return "Result of Form."))
(: setup-call-cleanup! (-> (-> $res) (-> $res) (-> $res) $res))

(iz predicate-arity MeTTaLog)
(@doc predicate-arity
  (@desc "Returns the arity of Function's predicate form, i.e. the function-arity + 1. (The additional argument being the function's result as an argument to the predicate.)")
  (@params (
    (@param "Function")))
  (@return "Arity of Function's predicate form."))

(iz function-arity MeTTaLog)
(@doc function-arity
  (@desc "Returns the arity of Function.")
  (@params (
    (@param "Function")))
  (@return "Arity of Function."))


(iz with-output-to! MeTTaLog)
(@doc with-output-to!
  (@desc "Evaluates expression with all outupt (e.g. from print) redirected to Stream. See also open! and close!.")
  (@params (
    (@param "Stream")
    (@param "Expression")))
  (@return "Result of Expression"))
(: with-output-to! (-> Stream (-> $res) $res))

(iz load-file! MeTTaLog)
(@doc load-file!
  (@desc "Loads the contents of Filename into Space, returning () if successful. Can load Python code (.py) or MeTTa (.metta); if ambiguous, assumes MeTTa. Like import! but favors MeTTa over Python when the file type is ambiguous.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: load-file! (-> SpaceType String (->)))

(iz load-ascii MeTTa)
(@doc load-ascii
  (@desc "Loads the contents of Filename into Space, returning () if successful. Assumes the file is an ASCII file. Works like include!.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: load-ascii (-> SpaceType (String Location) Expression))

(iz transfer! MeTTa)
(@doc transfer!
  (@desc "Loads the contents of Filename into &self, as include. Returns () if successful, throws an exception otherwise.")
  (@params (
    (@param "Filename")))
  (@return "Expression"))
(: transfer! (-> (String Location) (->)))

(iz save-space! MeTTa)
(@doc save-space!
  (@desc "Writes the contents of Space into Filename, returning () if successful.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: save-space! (-> SpaceType (String Location) (->)))

(iz rtrace! MeTTaLog)
(@doc rtrace!
  (@desc "Fully evaluates input Atom, providing a complete trace of the evaluation.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "Result of evaluation"))
(: rtrace! (-> (-> $res) $res))

(iz rust MeTTaLog)
(@doc rust
  (@desc "Interface with the rust / Hyperon MeTTa implementation. Enters Atom into rust atomspace. If Atom is evaluated (i.e. by being of the form !<atom>), returns the result of evaluation. See also rust!.")
  (@params (
    (@param "Atom to be entered into the space")))
  (@return "Result of entering Atom into the space"))
(: rust (-> (-> $res) $res))

(iz rust! MeTTaLog)
(@doc rust!
  (@desc "Like rust but evaluates the atom rather than entering into the space. (rust! <atom>) and (rust !<atom>) are identical.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "Result of evaluation"))
(: rust! (-> (-> $res) $res))

(iz coerce MeTTaLog)
(@doc coerce
  (@desc "Cast (coerce) Value to be of Type. Supports the basic types Atom (no-op), Number, String, and Bool.
  Number: Converts number strings and bools to numbers. True is 1, False is 0.
  String: Coerced as if Value were printed.
  Bool: False, 0, and () are False, all other values are True.")
  (@params (
    (@param "Type")
    (@param "Value")))
  (@return "Value cast to Type"))
(: coerce (-> Type Atom Atom))
(iz = MeTTa)
(@doc = 
  (@desc "A symbol used to define reduction rules for expressions.")
  (@params (
    (@param "Pattern to be matched against expression to be reduced")
    (@param "Result of reduction or transformation of the first pattern")))
  (@return "Not reduced itself unless custom equalities over equalities are added") )
(: = (-> $t $t %Undefined%))
;(: = (-> Atom Atom Atom))

(iz ErrorType MeTTa)
(@doc ErrorType (@desc "Type of the atom which contains error"))
(: ErrorType Type)

(iz Error MeTTa)
(@doc Error
  (@desc "Error constructor")
  (@params (
    (@param "Atom which contains error")
    (@param "Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments")))
  (@return "Instance of the error atom"))
(: Error (-> Atom Atom ErrorType))

(iz return MinimalMeTTa)
(@doc return
  (@desc "Returns value from the (function ...) expression")
  (@params (
    (@param "Value to be returned")))
  (@return "Passed argument"))
; probably should be 
(: return (-> $t $t))
;;(: return (-> Atom Atom))


(iz function MinimalMeTTa)
(@doc function
  (@desc "Evaluates the argument until it becomes (return <result>). Then (function (return <result>)) is reduced to the <result>.")
  (@params (
    (@param "Atom to be evaluated")))
  (@return "Result of atom's evaluation"))
(: function (-> (-> $res) $res))

(iz eval MeTTa)
(@doc eval
  (@desc "Evaluates input atom, performs one step of the evaluation. Empty results (Empty, ()) are removed from the result set. If no results are produced for a non-grounded function, eval returns NotReducible.")
  (@params (
    (@param "Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function")))
  (@return "Result of evaluation"))
(: eval (-> (-> $res) $res))

(iz eval MinimalMeTTa)
(@doc evalc
  (@desc "Evaluates input atom, makes one step of the evaluation")
  (@params (
    (@param "Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function")
    (@param "Space to evaluate atom in its context")))
  (@return "Result of evaluation"))
(: evalc (-> (-> $res) Grounded %Undefined%))
(ML= (evalc $eval $space)
  (with-space $space (eval $eval)))

(iz chain MinimalMeTTa)
(@doc chain
  (@desc "Evaluates first argument Atom, binds it to the Variable (second argument) and then evaluates third argument Template with Variable substituted in. When evaluation of the first Atom brings more than a single result, chain returns one instance of the Template expression for each result. The first argument Atom is only evaluated if it is part of the Minimal MeTTa specification; evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation).")
  (@params (
    (@param "Atom to be evaluated")
    (@param "Variable")
    (@param "Template which will be evaluated at the end with Variable substituted")))
  (@return "Result of evaluating third input argument"))
(: chain (-> (-> $res) Variable (-> $res) $res))
(ML= (chain $eval $var $after)
  (let $var $eval $after))

(iz unify MeTTa)
(@doc unify
  (@desc "Like Match but allows any sort of container for the first argument. (Match only allows MeTTa spaces.)")
  (@params (
	(@param "The collection or space to match or the first atom to unify with")
    (@param "Second atom to unify with")
    (@param "Result if two atoms unified successfully")
    (@param "Result otherwise")))
  (@return "Third argument when first two atoms are matched of forth one otherwise"))
(: unify (-> Atom Atom (-> $res) (-> $res) $res))

(iz if-unify MinimalMeTTaHelper)
(@doc if-unify
  (@desc "Matches two first terms and returns third argument if they are matched and fourth argument otherwise")
  (@params (
	(@param "First term to unify with")
	(@param "Second atom to unify with")
	(@param "Result if two atoms unified successfully")
	(@param "Result otherwise")))
  (@return "Third argument when first two atoms are unifiable or fourth one otherwise"))
(: if-unify (-> Atom Atom (-> $res) (-> $res) $res))
(HE= (if-unify $a $b $then $else)
  (case (if-unify-or-empty $a $b) 
	  ((unified $then) 
	   (Empty $else))))

(iz if-unify-or-empty MinimalMeTTaHelper)
(@doc if-unify-or-empty
  (@desc "Attempts to unify two atoms and returns a result. Returns Empty if they cannot be unified.")
  (@params (
    (@param "First atom")
    (@param "Second atom")))
  (@return "Unification result or Empty"))
(: if-unify-or-empty (-> Atom Atom Atom))
(= (if-unify-or-empty $a $a) unified)
(= (if-unify-or-empty $a $b) (empty))

(iz cons-atom MeTTa)
(@doc cons-atom
  (@desc "Constructs an expression using two arguments")
  (@params (
    (@param "Head of an expression")
    (@param "Tail of an expression")))
  (@return "New expression consisting of the two input arguments"))
(: cons-atom (-> Atom Expression Expression))
; AKA? (: cons (-> Atom Atom Atom))

(iz decons-atom MeTTa)
(@doc decons-atom
  (@desc "Works as a reverse to cons-atom function. It gets Expression as an input and returns it split into head and tail, e.g. (decons-atom (Cons X Nil)) -> (Cons (X Nil))")
  (@params (
    (@param "Expression to be Deconsed")))
  (@return "Deconsed expression"))
(: decons-atom (-> Expression Atom))
(ML= (decons-atom $term)
  (eval-for! ($h $t) (@ unify_with_occurs_check $term [$h | $t])))

;; TODO in MeTTaLog
(@doc context-space
  (@desc "Returns the space which is used as a context space in atom evaluation")
  (@params ())
  (@return "Context space"))
(: context-space (-> SpaceType))

(iz min-atom MeTTa)
(@doc min-atom
  (@desc "Returns atom with minimum value in the expression (first argument). Only numbers are allowed.")
  (@params (
    (@param "Expression containing atoms of Number type")))
  (@return "Minimum value in the expression. Error if expression contains non-numeric value or is empty."))
(: min-atom (-> (Expression Number) Number))
(ML= (min-atom $L) (call-fn! min_list $L))

(iz max-atom MeTTa)
(@doc max-atom
  (@desc "Returns atom with maximum value in the expression (first argument). Only numbers are allowed.")
  (@params (
	(@param "Expression containing atoms of Number type")))
  (@return "Maximum value in the expression. Error if expression contains non-numeric value or is empty."))
(: max-atom (-> (Expression Number) Number))
(ML= (max-atom $L) (call-fn! max_list $L))

(iz size-atom MeTTa)
(@doc size-atom
  (@desc "Returns the size of an expression (first argument).")
  (@params (
    (@param "Expression to measure the size of")))
  (@return "Size of the expression"))
(: size-atom (-> Expression Integer))
(is-fn-1 size-atom length)

(iz index-atom MeTTa)
(@doc index-atom
  (@desc "Returns atom from an expression (first argument) using index (second argument) or error if index is out of bounds")
  (@params (
    (@param "Expression")
    (@param "Index")))
  (@return "Atom at the specified index in the expression. Error if index is out of bounds."))
(: index-atom (-> Expression Number Atom))
(is-fn-21 index-atom nth0)
;; ==> (= (index-atom $L $N) (call-fn! nth0 $N $L))


(iz pow-math MeTTa)
(@doc pow-math
  (@desc "Takes base (first argument) and power (second argument) and returns result of a power function (base ^ power)")
  (@params (
    (@param "Base. Could be a float.")
    (@param "Power.")))
  (@return "Result of base raised to the power"))


; Define a variable using a function call to compute the power of a base number to an exponent
; The result of the computation is stored in the variable `pow-math`

; Assign the result of a function call to `pow-math`
(= (pow-math $B $P)
   ; Call the function `pow` with arguments `$B` and `$P` to calculate the power
   (call-fn! pow $B $P))



(: is-op-1 DataFunctor)
(: is-op-2 DataFunctor)
(: is-pred DataFunctor)

(==> (is-op-1 $m $p) (is-fn-1 $m $p))
(==> (is-op-1 $m $_) (: $m (-> Number Number)))
(==> (is-op-2 $m $p) (is-fn-2 $m $p))
(==> (is-op-2 $m $_) (: $m (-> Number Number Number)))
(==> (is-pred $m $p) (is-pred-1 $m $p))
(==> (is-pred $m $_) (: $m (-> Number Bool)))


(: is-fn-1   DataFunctor) (==> (is-fn-1   $m $p) (= ($m $a)       (call-fn! $p $a)))
(: is-fn-r   DataFunctor) (==> (is-fn-r   $m $p) (= ($m $a)       (call-fn-r! $p $a)))
(: is-fn-2   DataFunctor) (==> (is-fn-2   $m $p) (= ($m $a $b)    (call-fn! $p $a $b)))
(: is-fn-2   DataFunctor) (==> (is-fn-21  $m $p) (= ($m $L $N)    (call-fn! $p $N $L)))
(: is-fn-3   DataFunctor) (==> (is-fn-3   $m $p) (= ($m $a $b $c) (call-fn! $p $a $b $c)))
(: is-pred-1 DataFunctor) (==> (is-pred-1 $m $p) (= ($m $a)       (call-p! $p $a)))
(: is-pred-2 DataFunctor) (==> (is-pred-2 $m $p) (= ($m $a $b)    (call-p! $p $a $b)))

(is-fn-1 py-list py_list)

(iz sqrt-math MeTTa)
(@doc sqrt-math
  (@desc "Returns square root for input number (first argument) which should be >= 0")
  (@params (
    (@param "Input number")))
  (@return "Result of a square root function"))
(is-op-1 sqrt-math sqrt)

(iz abs-math MeTTa)
(@doc abs-math
  (@desc "Returns absolute value of input number (first argument)")
  (@params (
    (@param "Input number")))
  (@return "Absolute value"))
(is-op-1 abs-math abs)

(iz log-math MeTTa)
(@doc log-math
  (@desc "Returns result of a logarithm function given base (first argument) and input number (second argument)")
  (@params (
    (@param "Base")
    (@param "Input number")))
  (@return "Result of log function"))
; (is-op-2 log-math log2) corrected this implementation
(= (log-math $base $x)  
  (function   
    (eval (if (or (<= $base 0)  (or (== $base 1) (<= $x 0)))  
      (return (Error log-math "Invalid logarithm arguments"))  
      (return (/ (call-fn! log $x) (call-fn! log $base)))))))

(iz trunc-math MeTTa)
(@doc trunc-math
  (@desc "Returns integer part of the input value (first argument)")
  (@params (
    (@param "Float value")))
  (@return "Integer part of float"))
(= (trunc-math $x) (call-fn! truncate $x))

(iz ceil-math MeTTa)
(@doc ceil-math
  (@desc "Returns the smallest integer greater than or equal to the input value (first argument)")
  (@params (
    (@param "Float value")))
  (@return "Integer value greater than or equal to the input"))
(is-op-1 ceil-math ceiling)

(iz floor-math MeTTa)
(@doc floor-math
  (@desc "Returns the smallest integer less than or equal to the input value (first argument)")
  (@params (
    (@param "Float value")))
  (@return "Integer value less than or equal to the input"))
(is-op-1 floor-math floor)

(iz round-math MeTTa)
(@doc round-math
  (@desc "Returns the nearest integer to the input float value (first argument)")
  (@params (
    (@param "Float value")))
  (@return "Nearest integer to the input"))
(is-op-1 round-math round)

(iz sin-math MeTTa)
(@doc sin-math
  (@desc "Returns result of the sine function for an input value in radians (first argument)")
  (@params (
    (@param "Angle in radians")))
  (@return "Result of the sine function"))
(is-op-1 sin-math sin)

(iz asin-math MeTTa)
(@doc asin-math
  (@desc "Returns result of the arcsine function for an input value (first argument)")
  (@params (
    (@param "Float number")))
  (@return "Result of the arcsine function"))
(is-op-1 asin-math asin)

(iz cos-math MeTTa)
(@doc cos-math
  (@desc "Returns result of the cosine function for an input value in radians (first argument)")
  (@params (
    (@param "Angle in radians")))
  (@return "Result of the cosine function"))
(is-op-1 cos-math cos)

(iz acos-math MeTTa)
(@doc acos-math
  (@desc "Returns result of the arccosine function for an input value (first argument)")
  (@params (
    (@param "Float number")))
  (@return "Result of the arccosine function"))
(is-op-1 acos-math acos)

(iz tan-math MeTTa)
(@doc tan-math
  (@desc "Returns result of the tangent function for an input value in radians (first argument)")
  (@params (
    (@param "Angle in radians")))
  (@return "Result of the tangent function"))
(is-op-1 tan-math tan)

(iz atan-math MeTTa)
(@doc atan-math
  (@desc "Returns result of the arctangent function for an input value (first argument)")
  (@params (
    (@param "Float number")))
  (@return "Result of the tangent function"))
(is-op-1 atan-math atan)

(iz isnan-math MeTTa)
(@doc isnan-math
  (@desc "Returns True if input value is NaN. False - otherwise")
  (@params (
    (@param "Number")))
  (@return "True/False"))
; (is-pred isnan-math is_NaN) 
(= (isnan-math $x) (call-fn! isnan $x))

(iz isinf-math MeTTa)
(@doc isinf-math
  (@desc "Returns True if input value is positive or negative infinity. False - otherwise")
  (@params (
    (@param "Number")))
  (@return "True/False"))
; this is deduced: (: isinf (-> Number Bool))
; (is-pred isinf-math is_Inf)  
(= (isinf-math $x) (call-fn! isinf $x))



;; Some code still uses the old syntax
(iz random-int-2 MeTTa)
(@doc random-int-2
  (@desc "Returns random int number from range defined by two numbers (first and second argument)")
  (@params (
	(@param "Range start")
	(@param "Range end")))
  (@return "Random int number from defined range"))
(is-op-2 random-int-2 random)

(iz random-float-2 MeTTa)
(@doc random-float-2
  (@desc "Returns random float number from range defined by two numbers (first and second argument)")
  (@params (
	(@param "Range start")
	(@param "Range end")))
  (@return "Random float number from defined range"))
(is-op-2 random-int-2 random)







(iz collapse-bind MeTTa)
(@doc collapse-bind
  (@desc "Evaluates the Atom (first argument) and returns an expression which contains all alternative evaluations in a form (Atom Bindings). Bindings are represented in a form of a grounded atom { <var> <- <binding>, ... }. See also the complement superpose-bind. Note that, like chain, collapse-bind only evaluates Minimal Metta expressions. Evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation).")
  (@params (
    (@param "Minimal MeTTa operation to be evaluated")))
  (@return "All alternative evaluations"))
;; collapse-bind because `collapse` doesnt guarantee shared bindings
(: collapse-bind (-> (-> $res) Expression))

(iz superpose-bind MeTTa)
(@doc superpose-bind
  (@desc "Complement to the collapse-bind. It takes result of collapse-bind (first argument) and returns only result atoms without bindings. Primarily used with some filtering step on the collapse-bind results, i.e. collapse-bind -> <filter> -> superpose-bind.")
  (@params (
    (@param "Expression in form (Atom Binding)")))
  (@return "Non-deterministic list of Atoms"))
;; superpose-bind because `superpose` doesnt guarentee shared bindings
(: superpose-bind (-> (Expression (Eval $res)) Atom))

; Helper Minimal Metta?
(@doc metta
  (@desc "Run MeTTa interpreter on atom.")
  (@params (
    (@param "Atom to be interpreted")
    (@param "Type of input atom")
    (@param "Atomspace where intepretation should take place")))
  (@return "Result of interpretation"))
  
(: metta (-> Atom Type Grounded Atom))
(: metta (-> (-> $res) (Type $t) SpaceType $res))

(iz id MinimalMeTTa)
(@doc id
  (@desc "Returns its argument")
  (@params (
    (@param "Input argument")))
  (@return "Input argument"))
(: id (-> $t $t))
(= (id $x) $x)

(@doc noeval
  (@desc "Returns its argument")
  (@params (
    (@param "Input argument")))
  (@return "Input argument"))
(: noeval (-> Atom Atom))
(= (noeval $x) $x)

(@doc atom-subst
  (@desc "Substitutes variable passed as a second argument in the third argument by the first argument")
  (@params (
    (@param "Value to use for replacement")
    (@param "Variable to replace")
    (@param "Template to replace variable by the value")))
  (@return "Template with substituted variable"))
(: atom-subst (-> Atom Variable Atom Atom))
(= (atom-subst $atom $var $templ)
  (function (chain (eval (noeval $atom)) $var (return $templ))) )

(iz if-decons-expr MinimalMeTTaHelper)
(@doc if-decons-expr
  (@desc "Checks if first argument is non empty expression. If so gets tail and head from the first argument and returns forth argument using head and tail values. Returns fifth argument otherwise.")
  (@params (
    (@param "Expression to be deconstructed")
    (@param "Head variable")
    (@param "Tail variable")
    (@param "Template to return if first argument is a non-empty expression")
    (@param "Default value to return otherwise")))
  (@return "Either template with head and tail replaced by values or default value"))
(: if-decons-expr (-> Expression Variable Variable (-> $res) (-> $res) $res))
;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
(= (if-decons-expr $atom $head $tail $then $else) (if-decons $atom $head $tail $then $else))

; (iz if-decons MeTTa) ?
(: if-decons (-> Expression Variable Variable (-> $res) (-> $res) $res))
(= (if-decons $atom $head $tail $then $else)
  (function (eval (if-equal $atom ()
    (return $else)
    (chain (decons-atom $atom) $list
      (if-unify $list ($head $tail) (return $then) (return $else)) )))))


; Difference between `switch` and `case` is a way how they interpret `Empty`
; result. `CaseOp` interprets first argument inside itself and then manually
; checks whether result is empty. `switch` is interpreted in a context of
; main interpreter. Minimal interpreter correctly passes `Empty` as an
; argument to the `switch` but when `switch` is called from MeTTa interpreter
; (for example user evaluates `!(switch (if-unify A B ok Empty) ...)` then
; emptiness of the first argument is checked by interpreter and it will
; break execution when `Empty` is returned.

(iz switch MinimalMeTTaHelper)
(@doc switch
  (@desc "Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)")
  (@params (
    (@param "Atom to be matched with patterns")
    (@param "Tuple of pairs mapping condition patterns to results")))
  (@return "Result which corresponds to the pattern which is matched with the passed atom first"))

(-> %Undefined% Expression %Undefined%) 
(= (switch $atom $list) (case (eval $atom) $list))



(iz type-cast MeTTa)
(@doc type-cast
  (@desc "Casts atom passed as a first argument to the type passed as a second argument using space as a context")
  (@params (
    (@param "Atom to be casted")
    (@param "Type to cast atom to")
    (@param "Context atomspace")))
  (@return "Atom if casting is successful, (Error ... BadType) otherwise"))
(: type-cast (-> Atom Atom Atom Atom))
;; This implementation is old and may not be sufficient.
(HE= (type-cast $atom $type $space)
  (function (chain (eval (get-metatype $atom)) $meta
    (eval (if-equal $type $meta
      (return $atom)
      (chain (eval (collapse-bind (eval (get-type $atom $space)))) $collapsed
        (chain (eval (map-atom $collapsed $pair (eval (first-from-pair $pair)))) $actual-types
            (chain (eval (foldl-atom $actual-types False $a $b (eval (match-type-or $a $b $type)))) $is-some-comp
              (eval (if $is-some-comp
                (return $atom)
                (return (Error $atom BadType)) ))))))))))

(iz match-types MeTTa)
(@doc match-types
  (@desc "Checks if two types can be unified and returns third argument if so, fourth - otherwise")
  (@params (
    (@param "First type")
    (@param "Second type")
    (@param "Atom to be returned if types can be unified")
    (@param "Atom to be returned if types cannot be unified")))
  (@return "Third or fourth argument"))
(: match-types (-> Atom Atom Atom Atom Atom))
(= (match-types $type1 $type2 $then $else)
  (function (eval (if-equal $type1 %Undefined%
    (return $then)
    (eval (if-equal $type2 %Undefined%
      (return $then)
      (eval (if-equal $type1 Atom
        (return $then)
        (eval (if-equal $type2 Atom
          (return $then)
          (if-unify $type1 $type2 (return $then) (return $else)) ))))))))))

; Helper MinimalMeTTa
(iz first-from-pair MinimalMeTTaHelper)
(@doc first-from-pair
  (@desc "Gets a pair as a first argument and returns first atom from pair")
  (@params (
    (@param "Pair")))
  (@return "First atom from a pair"))
(= (first-from-pair $pair)
  (function
    (if-unify $pair ($first $second)
      (return $first)
      (return (Error (first-from-pair $pair) "incorrect pair format")))))

(iz match-type-or HelperMM)
(@doc match-type-or
  (@desc "Checks if two types (second and third arguments) can be unified and returns result of OR operation between first argument and type checking result")
  (@params (
    (@param "Boolean value")
    (@param "First type")
    (@param "Second type")))
  (@return "True or False"))
(HE= (match-type-or $folded $next $type)
  (function
    (chain (eval (match-types $next $type True False)) $matched
      (chain (eval (or $folded $matched)) $or (return $or)) )))

(iz filter-atom MeTTa)
(@doc filter-atom
  (@desc "Function takes list of atoms (first argument), variable (second argument) and filter predicate (third argument) and returns list with items which passed filter. E.g. (filter-atom (1 2 3 4) $v (eval (> $v 2))) will give (3 4)")
  (@params (
    (@param "List of atoms")
    (@param "Variable")
    (@param "Filter predicate")))
  (@return "Filtered list"))
(: filter-atom (-> Expression Variable Atom Expression))
(HE= (filter-atom $list $var $filter)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (filter-atom $tail $var $filter)) $tail-filtered
      (chain (eval (atom-subst $head $var $filter)) $filter-expr
        (chain $filter-expr $is-filtered
          (eval (if $is-filtered
            (chain (cons-atom $head $tail-filtered) $res (return $res))
            (return $tail-filtered) )))))
    (return ()) ))))

(iz map-atom MeTTa)
(@doc map-atom
  (@desc "Function takes list of atoms (first argument), variable to be used inside (second variable) and an expression which will be evaluated for each atom in list (third argument). Expression should contain variable. So e.g. (map-atom (1 2 3 4) $v (eval (+ $v 1))) will give (2 3 4 5)")
  (@params (
    (@param "List of atoms")
    (@param "Variable name")
    (@param "Template using variable")))
  (@return "Result of evaluating template for each atom in a list"))
(: map-atom (-> Expression Variable Atom Expression))
(= (map-atom $list $var $map)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (map-atom $tail $var $map)) $tail-mapped
      (chain (eval (atom-subst $head $var $map)) $map-expr
        (chain $map-expr $head-mapped
          (chain (cons-atom $head-mapped $tail-mapped) $res (return $res)) )))
    (return ()) ))))

(iz foldl-atom MeTTa)
(@doc foldl-atom
  (@desc "Function takes list of values (first argument), initial value (second argument) and operation (fifth argument) and applies it consequently to the list of values, using init value as a start. It also takes two variables (third and fourth argument) to use them inside")
  (@params (
    (@param "List of values")
    (@param "Init value")
    (@param "Variable")
    (@param "Variable")
    (@param "Operation")))
  (@return "Result of applying operation to the list of values"))
(: foldl-atom (-> Expression Atom Variable Variable Atom %Undefined%))
(= (foldl-atom $list $init $a $b $op)
  (function (eval (if-decons-expr $list $head $tail
    (chain (eval (atom-subst $init $a $op)) $op-init
      (chain (eval (atom-subst $head $b $op-init)) $op-head
        (chain (context-space) $space
          (chain (metta $op-head %Undefined% $space) $head-folded
            (chain (eval (foldl-atom $tail $head-folded $a $b $op)) $res (return $res)) ))))
      (return $init) ))))


  
; TODO: Type is used here, but there is no definition for the -> type
; constructor for instance, thus in practice it matches because -> has
; %Undefined% type. We need to assign proper type to -> and other type
; constructors but it is not possible until we support vararg types.
(iz is-function-type MeTTa) ; or Helper?
(@doc is-function-type
  (@desc "Function checks if input type is a function type")
  (@params (
    (@param "Type notation")))
  (@return "True if input type notation is a function type, False - otherwise"))
(: is-function-type (-> Type Bool))
(= (is-function-type $type)
  (let $type-meta (get-metatype $type)
    (case $type-meta (
      (Expression
        (let $first (car-atom $type)
          (if (== $first ->) True False) ))
      ($_ False) ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MeTTa interpreter implementation ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(iz if MeTTa)
(@doc if
  (@desc "Replace itself by one of the arguments depending on condition.")
  (@params (
    (@param "Boolean condition")
    (@param "Result when condition is True")
    (@param "Result when condition is False")))
  (@return "Second or third argument") )
(: if (-> Bool (-> $res) (-> $res) $res))
(HE= (if True $then $else) $then)
(HE= (if False $then $else) $else)
;`$then`, `$else` should be of `Atom` type to avoid evaluation
; and infinite cycle in inference


(iz or MeTTa)
(@doc or
  (@desc "Logical disjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "True if any of input arguments is True, False - otherwise"))
(: or (-> Bool LazyBool Bool))
(HE= (or False False) False)
(HE= (or False True) True)
(HE= (or True False) True)
(HE= (or True True) True)

(iz and MeTTa)
(@doc and
  (@desc "Logical conjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if both arguments are True, False - otherwise"))
(: and (-> Bool LazyBool Bool))
(HE= (and False False) False)
(HE= (and False True) False)
(HE= (and True False) False)
(HE= (and True True) True)

(iz not MeTTa)
(@doc not
  (@desc "Logical negation")
  (@params (
    (@param "Argument")))
  (@return "Negates boolean input argument (False -> True, True -> False)"))
(: not (-> Bool Bool))
(HE= (not True) False)
(HE= (not False) True)

(iz let MeTTa)
(@doc let
  (@desc "Let function is utilized to establish temporary variable bindings within an expression. It allows introducing variables (first argument), assign values to them (second argument), and then use these values within the scope of the let block")
  (@params (
    (@param "Variable name (or several variables inside brackets ())")
    (@param "Expression to be bound to variable (it is being reduced before bind)")
    (@param "Expression which will be reduced and in which variable (first argument) could be used")))
  (@return "Result of third argument's evaluation"))
(: let (-> Atom %Undefined% (-> $res) Atom))
(HE= (let $pattern $atom $template)
  (if-unify $atom $pattern $template Empty))

(iz let* MeTTa)
(@doc let*
  (@desc "Same as let, but first argument is a tuple containing tuples of variables and their bindings, e.g. (($v (+ 1 2)) ($v2 (* 5 6)))")
  (@params (
    (@param "Tuple of tuples with variables and their bindings")
    (@param "Expression which will be evaluated if each pair can be unified")))
  (@return "Second argument or Empty"))
(: let* (-> Expression (-> $res) $res))
(HE= (let* $pairs $template)
  (eval (if-decons-expr $pairs ($pattern $atom) $tail
    (let $pattern $atom (let* $tail $template))
    $template )))

(iz add-reduct MeTTa)
(@doc add-reduct
  (@desc "Prevents atom from being reduced")
  (@params (
    (@param "Atom")))
  (@return "Quoted atom"))

(@doc add-reduct-rust1
  (@desc "Reduces atom (second argument) and adds it into the atomspace (first argument)")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))
  
(: add-reduct-rust1 (-> SpaceType %Undefined% (->)))
(= (add-reduct-minimal $dst $atom) (add-atom $dst $atom))
(iz add-reduct MeTTa)
(: add-reduct (-> Grounded %Undefined% (->)))
(= (add-reduct $dst $atom) (add-atom $dst $atom))


(iz stringToChars MeTTa)
(@doc stringToChars
  (@desc "Converts a string into a list expression of characters.")
  (@params (
    (@param "String to be converted.")))
  (@return "Expression representing the list of characters."))
(: stringToChars (-> String Expression))

(iz charsToString MeTTa)
(@doc charsToString
  (@desc "Converts a list expression of characters into a string.")
  (@params (
    (@param "Expression representing the list of characters.")))
  (@return "Converted string."))
(: charsToString (-> Expression String))

(iz parse MeTTa)
(@doc parse
  (@desc "Parses MeTTa code from a string and returns the corresponding atom.")
  (@params (
    (@param "String containing MeTTa code.")))
  (@return "Parsed atom."))
(: parse (-> String Atom))

(iz repr MeTTa)
(@doc repr
  (@desc "Creates a string representation of an atom.")
  (@params (
    (@param "Atom to be represented as a string.")))
  (@return "String representation of the atom."))
(: repr (-> Atom String))

(iz car-atom MeTTa)
(@doc car-atom
  (@desc "Extracts the first atom of an expression as a tuple")
  (@params (
    (@param "Expression")))
  (@return "First atom of an expression"))
(: car-atom (-> Expression %Undefined%))
(HE= (car-atom $atom)
  (eval (if-decons-expr $atom $head $_
    $head
    (Error (car-atom $atom) "car-atom expects a non-empty expression as an argument") )))

(iz cdr-atom MeTTa)
(@doc cdr-atom
  (@desc "Extracts the tail of an expression (all except first atom)")
  (@params (
    (@param "Expression")))
  (@return "Tail of an expression"))
(: cdr-atom (-> Expression Expression))
(HE= (cdr-atom $atom)
  (eval (if-decons-expr $atom $_ $tail
    $tail
    (Error (cdr-atom $atom) "cdr-atom expects a non-empty expression as an argument") )))

(iz quote MeTTa)
(@doc quote
  (@desc "Prevents atom from being reduced")
  (@params (
    (@param "Atom")))
  (@return "Quoted atom"))
(: quote (-> Atom Atom))
(= (quote $atom) NotReducible)

(iz unquote MeTTa)
(@doc unquote
  (@desc "Unquotes quoted atom, e.g. (unquote (quote $x)) returns $x")
  (@params (
    (@param "Quoted atom")))
  (@return "Unquoted atom"))
(: unquote (-> %Undefined% %Undefined%))
(= (unquote (quote $atom)) $atom)

; TODO: there is no way to define operation which consumes any number of
; arguments and returns unit
(iz nop MeTTa)
(@doc nop
  (@desc "Outputs unit atom")
  (@params ())
  (@return "Unit atom"))
(: nop (-> EmptyType))
(HE= (nop) ())
  
(iz nop MeTTa)
(@doc nop
  (@desc "Outputs unit atom for any input")
  (@params (
    (@param "Anything")))
  (@return "Unit atom"))
(: nop (-> Atom (->)))
(HE= (nop $x) ())

(iz empty MeTTa)
; TODO: can be replaced by Empty atom and removed, kept for compatibility
(@doc empty
  (@desc "Cuts evaluation of the non-deterministic branch and removes it from the result")
  (@params ())
  (@return "Nothing"))
(: empty (-> %Undefined%))
(= (empty) (let a b never-happens))
(HE= (empty) Empty)

;(= (empty-rust1) (let a b never-happens))
; TODO: MINIMAL added for compatibility, remove after migration
;(= (empty-minimal) Empty)


(iz unique MeTTa)
(@doc unique
  (@desc "Function takes non-deterministic input (first argument) and returns only unique entities. E.g. (unique (superpose (a b c d d))) -> [a, b, c, d]")
  (@params (
	(@param "Non-deterministic set of values")))
  (@return "Unique non-deterministic values from input set"))
(: unique (-> Atom Atom))
(ML= (unique $code) (unique-by == $code))
(: unique (-> Atom %Undefined%))
(HE= (unique $arg) (let $c (collapse $arg) (let $u (unique-atom $c) (superpose $u))))


(iz union MeTTa)
(@doc union
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their union. E.g. (union (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]")
  (@params (
	(@param "Non-deterministic set of values")
	(@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Union of sets"))
(: union (-> (-> $res) (-> $res) $res))
(HE= (union $arg1 $arg2)
   (let $c1 (collapse $arg1) (let $c2 (collapse $arg2)
     (let $u (union-atom $c1 $c2) (superpose $u)))))
(ML= (union $code1 $code2)
  (union-by == $code1 $code2))


(iz intersection MeTTa)
(@doc intersection
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their intersection. E.g. (intersection (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]")
  (@params (
	(@param "Non-deterministic set of values")
	(@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Intersection of sets"))
(: intersection (-> (-> $res) (-> $res) $res))
(HE= (intersection $arg1 $arg2)
   (let $c1 (collapse $arg1) (let $c2 (collapse $arg2)
     (let $u (intersection-atom $c1 $c2) (superpose $u)))))
(ML= (intersection $code1 $code2)
  (intersection-by == $code1 $code2))


(iz subtraction MeTTa)
(@doc subtraction
  (@desc "Function takes two non-deterministic inputs (first and second argument) and returns their subtraction. E.g. !(subtraction (superpose (a b b c)) (superpose (b c c d))) -> [a, b]")
  (@params (
	(@param "Non-deterministic set of values")
	(@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Subtraction of sets"))
(: subtraction (-> (-> $res) (-> $res) $res))
(HE= (subtraction $arg1 $arg2)
   (let $c1 (collapse $arg1) (let $c2 (collapse $arg2)
     (let $u (subtraction-atom $c1 $c2) (superpose $u)))))
(ML= (subtraction $code1 $code2)
  (subtraction-by == $code1 $code2))

(@doc add-reducts
  (@desc "Function takes space and expression, evaluates atoms in it and adds them into given space")
  (@params (
    (@param "Space")
    (@param "Expression")))
  (@return "Unit atom"))
(: add-reducts (-> Grounded %Undefined% (->)))
(= (add-reducts $space $tuple)
    (foldl-atom $tuple () $a $b (add-atom $space $b)))

(@doc add-atoms
  (@desc "Function takes space and expression and adds atoms in Expression into given space without reducing them")
  (@params (
    (@param "Space")
    (@param "Expression")))
  (@return "Unit atom"))
(: add-atoms (-> Grounded Expression (->)))
(= (add-atoms $space $tuple)
    (foldl-atom $tuple () $a $b (add-atom $space $b)))

(@doc assertIncludes
  (@desc "Checks if the content in the second argument is included in the results of the first argument's evaluation")
  (@params (
    (@param "First expression")
    (@param "Second expression")))
  (@return "Unit atom if the second argument is included in the results of the first argument's evaluation, error - otherwise"))
(: assertIncludes (-> (-> $res) Expression (->)))
(= (assertIncludes $atom $content)
    (let $eval_atom (collapse $atom)
        (let $diff (subtraction-atom $content $eval_atom)
            (if (== $diff ())
                ()
                (Error (assertIncludes $atom $content) 
                  (assertIncludes error: $diff not included in result: $eval_atom))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Documentation formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(@doc @doc
  (@desc "Used for documentation purposes. Function documentation starts with @doc")
  (@params (
    (@param "Function name")
    (@param "Function description. Starts with @desc")
    (@param "(Optional) parameters description starting with @params which should contain one or more @param symbols")
    (@param "(Optional) description of what function will return. Starts with @return")))
  (@return "Function documentation using @doc-formal"))
(: @doc (-> Atom DocDescription DocInformal))
(: @doc (-> Atom DocDescription DocParameters DocReturnInformal DocInformal))
(iz @doc DataFunctor)

(iz @desc MeTTa)
(@doc @desc
  (@desc "Used for documentation purposes. Description of function starts with @desc as a part of @doc")
  (@params (
    (@param "String containing function description")))
  (@return "Function description"))
(: @desc (-> String DocDescription))
(iz @desc DataFunctor)

(iz @param MeTTa)
(@doc @param
  (@desc "Used for documentation purposes. Description of function parameter starts with @param as a part of @params which is a part of @doc")
  (@params (
    (@param "String containing parameter description")))
  (@return "Parameter description"))
(: @param (-> String DocParameterInformal))
(: @param (-> DocType DocDescription DocParameter))
(iz @param DataFunctor)

(iz @return MeTTa)
(@doc @return
  (@desc "Used for documentation purposes. Description of function return value starts with @return as a part of @doc")
  (@params (
    (@param "String containing return value description")))
  (@return "Return value description"))
(: @return (-> String DocReturnInformal))
(: @return (-> DocType DocDescription DocReturn))
(iz @return DataFunctor)

(iz @doc-formal MeTTa)
(@doc @doc-formal
  (@desc "Used for documentation purposes. get-doc returns documentation starting with @doc-formal symbol. @doc-formal contains 6 or 4 parameters depending on the entity being described (functions being described using 6 parameters, atoms - 4 parameters)")
  (@params (
    (@param "Function/Atom name for which documentation is to be displayed. Format (@item name)")
    (@param "Contains (@kind function) or (@kind atom) depends on entity which documentation is displayed")
    (@param "Contains type notation of function/atom")
    (@param "Function/atom description")
    (@param "(Functions only). Description of function parameters")
    (@param "(Functions only). Description of function's return value")))
  (@return "Expression containing full documentation on function"))
(: @doc-formal (-> DocItem DocKindFunction DocType DocDescription DocParameters DocReturn DocFormal))
(: @doc-formal (-> DocItem DocKindAtom DocType DocDescription DocFormal))
(iz @doc-formal DataFunctor)

(iz @item MeTTa)
(@doc @item
  (@desc "Used for documentation purposes. Converts atom/function's name to DocItem")
  (@params (
    (@param "Atom/Function name to be documented")))
  (@return "(@item Atom) entity"))
(: @item (-> Atom DocItem))
(iz @item DataFunctor)

; TODO: help! gives two outputs
;Atom (@kind function): (%Undefined% (-> Atom Atom)) Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
;Atom (@kind function): DocKindFunction Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
(iz (@kind function) MeTTa)
(@doc (@kind function)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case"))
(: (@kind function) DocKindFunction)
(iz (@kind function) DataFunctor)

(iz (@kind atom) MeTTa)
(@doc (@kind atom)
  (@desc "Used for documentation purposes. Shows type of entity to be documented. (@kind atom) in this case"))
(: (@kind atom) DocKindAtom)
(iz (@kind atom) DataFunctor)

(iz @type MeTTa)
(@doc @type
  (@desc "Used for documentation purposes. Converts atom/function's type to DocType")
  (@params (
    (@param "Atom/Function type to be documented")))
  (@return "(@type Type) entity"))
(: @type (-> Type DocType))
(iz @type DataFunctor)

(iz @params MeTTa)
(@doc @params
  (@desc "Used for function documentation purposes. Contains several @param entities with description of each @param")
  (@params (
    (@param "Several (@param ...) entities")))
  (@return "DocParameters containing description of all parameters of function in form of (@params ((@param ...) (@param ...) ...))"))
(: @params (-> Expression DocParameters))
(iz @params DataFunctor)

(iz get-doc MeTTa)
(@doc get-doc
  (@desc "Returns documentation for the given Atom/Function")
  (@params (
    (@param "Atom/Function name for which documentation is needed")))
  (@return "Documentation for the given atom/function"))
(: get-doc (-> Atom %Undefined%))
(= (get-doc $atom)
  (let $meta-type (get-metatype $atom)
    (case $meta-type (
      (Expression (get-doc-atom $atom))
      ($_ (get-doc-single-atom $atom)) ))))

(iz get-doc-single-atom PrivateRelation)
(@doc get-doc-single-atom
  (@desc "Function used by get-doc to get documentation on either function or atom. It checks if input name is the name of function or atom and calls correspondent function")
  (@params (
    (@param "Atom/Function name for which documentation is needed")))
  (@return "Documentation for the given atom/function"))
(: get-doc-single-atom (-> Atom %Undefined%))
(= (get-doc-single-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (if (is-function-type $type)
      (get-doc-function $atom $type)
      (get-doc-atom $atom) ))))

(iz get-doc-function PrivateRelation)
(@doc get-doc-function
  (@desc "Function used by get-doc-single-atom to get documentation on a function. It returns documentation on a function if it exists or default documentation with no description otherwise")
  (@params (
    (@param "Function name for which documentation is needed")
    (@param "Type notation for this function")))
  (@return "Documentation for the given function"))
(: get-doc-function (-> Atom Type %Undefined%))
(= (get-doc-function $name $type)
  (let $top-space (mod-space! top)
  (unify $top-space (@doc $name $desc (@params $params) $ret)
    (let $type' (if (== $type %Undefined%) (undefined-doc-function-type $params) (cdr-atom $type))
    (let ($params' $ret') (get-doc-params $params $ret $type')
      (@doc-formal (@item $name) (@kind function) (@type $type) $desc (@params $params') $ret')))
    (@doc-formal (@item $name) (@kind function) (@type $type) (@desc "No documentation")) )))

(iz undefined-doc-function-type PrivateRelation)
(@doc undefined-doc-function-type
  (@desc "Function used by get-doc-single-atom in case of absence of function's type notation")
  (@params (
    (@param "List of parameters for the function we want to get documentation for")))
  (@return "List of %Undefined% number of which depends on input list size. So for two parameters function will return (%Undefined% %Undefined% %Undefined%)"))
(: undefined-doc-function-type (-> Expression Type))
(= (undefined-doc-function-type $params)
  (if (== () $params) (%Undefined%)
    (let $params-tail (cdr-atom $params)
    (let $tail (undefined-doc-function-type $params-tail)
      (cons-atom %Undefined% $tail) ))))

(iz get-doc-params PrivateRelation)
(@doc get-doc-params
  (@desc "Function used by get-doc-function to get function's parameters documentation (including return value)")
  (@params (
    (@param "List of parameters in form of ((@param Description) (@param Description)...)")
    (@param "Return value's description in form of (@return Description)")
    (@param "Type notation without -> starting symbol e.g. (Atom Atom %Undefined%)")))
  (@return "United list of params and return value each augmented with its type. E.g. (((@param (@type Atom) (@desc Description)) (@param (@type Atom) (@desc Description2))) (@return (@type Atom) (@desc Description)))"))
(: get-doc-params (-> Expression Atom Expression (Expression Atom)))
(= (get-doc-params $params $ret $types)
  (let $head-type (car-atom $types)
  (let $tail-types (cdr-atom $types)
    (if (== () $params)
      (let (@return $ret-desc) $ret
        (() (@return (@type $head-type) (@desc $ret-desc))) )
      (let (@param $param-desc) (car-atom $params)
        (let $tail-params (cdr-atom $params)
        (let ($params' $result-ret) (get-doc-params $tail-params $ret $tail-types)
        (let $result-params (cons-atom (@param (@type $head-type) (@desc $param-desc)) $params')
          ($result-params $result-ret) ))))))))

(iz get-doc-atom PrivateRelation)
(@doc get-doc-atom
  (@desc "Function used by get-doc (in case of input type Expression) and get-doc-single-atom (in case input value is not a function) to get documentation on input value")
  (@params (
    (@param "Atom's name to get documentation for")))
  (@return "Documentation on input Atom"))
(: get-doc-atom (-> Atom %Undefined%))
(= (get-doc-atom $atom)
  (let $top-space (mod-space! top)
  (let $type (get-type-space $top-space $atom)
    (unify $top-space (@doc $atom $desc)
      (@doc-formal (@item $atom) (@kind atom) (@type $type) $desc)
      (unify $top-space (@doc $atom $desc' (@params $params) $ret)
        (get-doc-function $atom %Undefined%)
        (@doc-formal (@item $atom) (@kind atom) (@type $type) (@desc "No documentation")) )))))

(iz help! MeTTa)
(@doc help!
  (@desc "Function prints documentation for the input atom. Without parameters prints the list of the stdlib functions.")
  (@params (
    (@param "Input to get documentation for")))
  (@return "Unit atom"))
(: help! (-> Atom (->)))
(= (help! $atom)
  (case (get-doc $atom) (
    ((@doc-formal (@item $item) (@kind function) (@type $type) (@desc $descr)
                 (@params $params)
                 (@return (@type $ret-type) (@desc $ret-desc)))
      (let () (println! (format-args "Function {}: {} {}" ($item $type $descr)))
      (let () (println! (format-args "Parameters:" ()))
      (let () (for-each-in-atom $params help-param!)
      (let () (println! (format-args "Return: (type {}) {}" ($ret-type $ret-desc)))
      () )))))
    ((@doc-formal (@item $item) (@kind function) (@type $type) (@desc $descr))
      (let () (println! (format-args "Function {} (type {}) {}" ($item $type $descr)))
      () ))
    ((@doc-formal (@item $item) (@kind atom) (@type $type) (@desc $descr))
      (let () (println! (format-args "Atom {}: {} {}" ($item $type $descr)))
      () ))
    ($other (Error $other "Cannot match @doc-formal structure") ))))

(@doc help!
  (@desc "Without parameters prints the list of the stdlib functions.")
  (@params ())
  (@return "Unit atom"))

(: help! (-> (->)))
(= (help!) (let $top-space (mod-space! top)
    (unify $top-space (@doc $name (@desc $desc))
      (let () (println! (format-args "{}\n\t{}" ($name $desc))) Empty)
      Empty)))


(iz help-param! PrivateRelation)
(@doc help-param!
  (@desc "Function used by function help! to output parameters using println!")
  (@params (
    (@param "Parameters list")))
  (@return "Unit atom"))
(: help-param! (-> Atom (->)))
(= (help-param! $param)
  (let (@param (@type $type) (@desc $desc)) $param
    (println! (format-args "  {} {}" ((type $type) $desc))) ))

(iz for-each-in-atom PrivateRelation)
(@doc for-each-in-atom
  (@desc "Applies function passed as a second argument to each atom inside first argument")
  (@params (
    (@param "Expression to each atom in which function will be applied")
    (@param "Function to apply")))
  (@return "Unit atom"))
(: for-each-in-atom (-> Expression Atom (->)))
(= (for-each-in-atom $expr $func)
  (if (noreduce-eq $expr ())
    ()
    (let $head (car-atom $expr)
      (let $tail (cdr-atom $expr)
      (let $_ ($func $head)
      (for-each-in-atom $tail $func) )))))

(iz noreduce-eq PrivateRelation)
(@doc noreduce-eq
  (@desc "Checks equality of two atoms without reducing them")
  (@params (
    (@param "First atom")
    (@param "Second atom")))
  (@return "True if not reduced atoms are equal, False - otherwise"))
(: noreduce-eq (-> Atom Atom Bool))
(= (noreduce-eq $a $b) (== (quote $a) (quote $b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Grounded function's documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(iz add-atom MeTTa)
(@doc add-atom
  (@desc "Adds atom into the atomspace without reducing it")
  (@params (
    (@param "Atomspace to add atom into")
    (@param "Atom to add")))
  (@return "Unit atom"))
(: add-atom (-> SpaceType Atom (->)))

(iz get-type MeTTa)
(@doc get-type
  (@desc "Returns type notation of input atom")
  (@params (
    (@param "Atom to get type for")))
  (@return "Type notation or %Undefined% if there is no type for input Atom"))
(: get-type (-> Atom Atom))
(: get-type (-> Atom SpaceType Atom))

(iz get-type-space MeTTa)
(@doc get-type-space
  (@desc "Returns type notation of input Atom (second argument) relative to a specified atomspace (first argument)")
  (@params (
    (@param "Atomspace where type notation for input atom will be searched")
    (@param "Atom to get type for")))
  (@return "Type notation or %Undefined% if there is no type for input Atom in provided atomspace"))
(iz get-type-space MeTTa)
(: get-type-space (-> SpaceType Atom Atom))
(= (get-type-space $space $atom)
	(get-type $atom $space))

(iz get-metatype MeTTa)
(@doc get-metatype
  (@desc "Returns metatype of the input atom")
  (@params (
    (@param "Atom to get metatype for")))
  (@return "Metatype of input atom"))
(: get-metatype (-> Atom Atom))
  

(iz if-equal MinimalMeTTa)
(@doc if-equal
  (@desc "Checks if first two arguments are equal and evaluates third argument if equal, fourth argument - otherwise")
  (@params (
    (@param "First argument")
    (@param "Second argument")
    (@param "Atom to be evaluated if arguments are equal")
    (@param "Atom to be evaluated if arguments are not equal")))
  (@return "Evaluated third or fourth argument"))
(: if-equal (-> Atom Atom (-> $res) (-> $res) $res))


(iz new-space MeTTa)
(@doc new-space
  (@desc "Creates new Atomspace which could be used further in the program as a separate from &self Atomspace")
  (@params ())
  (@return "Reference to a new space"))
(: new-space (-> SpaceType))


(iz remove-atom MeTTa)
(@doc remove-atom
  (@desc "Removes atom from the input Atomspace")
  (@params (
    (@param "Reference to the space from which the Atom needs to be removed")
    (@param "Atom to be removed")))
  (@return "Unit atom"))
(: remove-atom (-> SpaceType Atom (->)))


(iz get-atoms MeTTa)
(@doc get-atoms
  (@desc "Shows all atoms in the input Atomspace")
  (@params (
    (@param "Reference to the space")))
  (@return "List of all atoms in the input space"))

(: get-atoms (-> SpaceType Atom))



(iz new-state MeTTa)
(@doc new-state
  (@desc "Creates a new state atom wrapping its argument")
  (@params (
    (@param "Atom to be wrapped")))
  (@return "Returns (State $value) where $value is an argument to a new-state"))
(: new-state (-> $tnso (StateMonad $tnso)))


(iz change-state! MeTTa)
(@doc change-state!
  (@desc "Changes input state's wrapped atom to another value (second argument). E.g. (change-state! (State 5) 6) -> (State 6)")
  (@params (
    (@param "State created by new-state function")
    (@param "Atom which will replace wrapped atom in the input state")))
  (@return "State with replaced wrapped atom"))
(: change-state! (-> (StateMonad $tcso) $tcso (StateMonad $tcso)))


(iz get-state MeTTa)
(@doc get-state
  (@desc "Gets a state as an argument and returns its wrapped atom. E.g. (get-state (State 5)) -> 5")
  (@params (
    (@param "State")))
  (@return "Atom wrapped by state"))
(: get-state (-> (StateMonad $tgso) $tgso))



(iz match MeTTa)
(@doc match
  (@desc "Searches for all declared atoms corresponding to the given pattern (second argument) and produces the output pattern (third argument)")
  (@params (
    (@param "Atomspace to search pattern")
    (@param "Pattern atom to be searched")
    (@param "Output template typically containing variables from the input pattern")))
  (@return "If match was successfull it outputs template (third argument) with filled variables (if any were present in pattern) using matched pattern (second argument). Empty - otherwise"))

(: match (-> SpaceType MatchAtom (-> $res) $res))

;(HE= (match $space $pattern $template)
;  (unify $space $pattern $template Empty))

(iz register-module! MeTTa)
(@doc register-module!
  (@desc "Takes a file system path (first argument) and loads the module into the runner")
  (@params (
    (@param "File system path")))
  (@return "Unit atom"))
(: register-module! (-> (String Location) (->)))


(iz mod-space! MeTTa)
(@doc mod-space!
  (@desc "Returns the space of the module (first argument) and tries to load the module if it is not loaded into the module system")
  (@params (
    (@param "Module name")))
  (@return "Space name"))
(: mod-space! (-> Atom SpaceType))

(= (mod-space! self) &self)
(= (mod-space! top) &self)
(= (mod-space! corelib) &corelib)
(= (mod-space! stdlib) &stdlib)
(= (mod-space! catalog) &catalog)


(iz print-mods! MeTTa)
(@doc print-mods!
  (@desc "Prints all modules with their correspondent spaces")
  (@params ())
  (@return "Unit atom"))
(: print-mods! (-> (->)))

(@doc =alpha
  (@desc "Checks alpha equality of two expressions")
  (@params (
    (@param "First expression")
    (@param "Second expression")))
  (@return "True if both expressions are alpha equal, False - otherwise"))

(iz assertEqual MeTTa)
(@doc assertEqual
  (@desc "Compares (sets of) results of evaluation of two expressions")
  (@params (
    (@param "First expression")
    (@param "Second expression")))
  (@return "Unit atom if both expressions after evaluation are equal, error - otherwise"))
(: assertEqual (-> (-> $res) (-> $res) (->)))
(= (assertEqual $actual $expected)
   (chain (context-space) $space
     (chain (metta (collapse $actual) %Undefined% $space) $actual-results
       (chain (metta (collapse $expected) %Undefined% $space) $expected-results
         (_assert-results-are-equal $actual-results $expected-results (assertEqual $actual $expected)) ))))

(iz assertNotEqual MeTTaLog)
(@doc assertNotEqual
  (@desc "Compares (sets of) results of evaluation of two expressions")
  (@params (
    (@param "First expression")
    (@param "Second expression")))
  (@return "Unit atom if both expressions after evaluation are not equal, error - otherwise"))
(: assertNotEqual (-> (-> $res) (-> $res) (->)))
(iz assertAlphaEqual MeTTa)
(@doc assertAlphaEqual
  (@desc "Compares (sets of) results of evaluation of two expressions using alpha equality")
  (@params (
    (@param "First expression")
    (@param "Second expression")))
  (@return "Unit atom if both expressions after evaluation are alpha equal, error - otherwise"))
(: assertAlphaEqual (-> (-> $res) (-> $res) (->)))
(= (assertAlphaEqual $actual $expected)
   (chain (context-space) $space
     (chain (metta (collapse $actual) %Undefined% $space) $actual-results
       (chain (metta (collapse $expected) %Undefined% $space) $expected-results
         (_assert-results-are-alpha-equal $actual-results $expected-results (assertAlphaEqual $actual $expected)) ))))

(iz assertEqualToResult MeTTa)
(@doc assertEqualToResult
  (@desc "Same as assertEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation")
  (@params (
    (@param "First expression (it will be evaluated)")
    (@param "Second expression containing the expected evaluation results (it won't be evaluated)")))
  (@return "Unit atom if both expressions after evaluation of the first argument are equal, error - otherwise"))
(: assertEqualToResult (-> (-> $res) Expression (->)))
(= (assertEqualToResult $actual $expected-results)
   (chain (context-space) $space
     (chain (metta (collapse $actual) %Undefined% $space) $actual-results
       (_assert-results-are-equal $actual-results $expected-results (assertEqualToResult $actual $expected-results)) )))


(iz assertNotEqualToResult MeTTaLog)
(@doc assertNotEqualToResult
  (@desc "Same as assertNotEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation")
  (@params (
    (@param "First expression (it will be evaluated)")
    (@param "Second expression (it won't be evaluated)")))
  (@return "Unit atom if both expressions after evaluation are not equal, error - otherwise"))
(: assertNotEqualToResult (-> (-> $res) Expression (->)))

(@doc assertAlphaEqualToResult
  (@desc "Same as assertAlphaEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation")
  (@params (
    (@param "First expression (it will be evaluated)")
    (@param "Second expression (it won't be evaluated)")))
  (@return "Unit atom if both expressions after evaluation of the first argument are alpha equal, error - otherwise"))
(: assertAlphaEqualToResult (-> (-> $res) Expression (->)))
(= (assertAlphaEqualToResult $actual $expected-results)
   (chain (context-space) $space
     (chain (metta (collapse $actual) %Undefined% $space) $actual-results
       (_assert-results-are-alpha-equal $actual-results $expected-results (assertAlphaEqualToResult $actual $expected-results)) )))


(iz superpose MeTTa)
(@doc superpose
  (@desc "Turns a tuple (first argument) into a nondeterministic result")
  (@params (
    (@param "Tuple to be converted")))
  (@return "Argument converted to nondeterministic result"))
(: superpose (-> (Expression (Eval $res)) $res))
(: superpose (-> Expression %Undefined%))

(iz collapse MeTTa)
(@doc collapse
  (@desc "Converts a nondeterministic result into a tuple")
  (@params (
    (@param "Atom which will be evaluated")))
  (@return "Tuple"))
(: collapse Atom Atom)
(: collapse (-> (-> $res) (Expression $res)))
(= (collapse $atom)
  (function
    (chain (context-space) $space
      (chain (collapse-bind (metta $atom %Undefined% $space)) $eval
        (chain (eval (foldl-atom $eval () $res $item (_collapse-add-next-atom-from-collapse-bind-result $res $item))) $result
          (return $result) )))))



(@doc _collapse-add-next-atom-from-collapse-bind-result
  (@desc "Adds atom from collapse-bind result to the passed list")
  (@params (
    (@param "List to add an atom into")
    (@param "collapse-bind result to extract atom from, should be a pair of atom and bindings")))
  (@return "List with new atom as a head"))

(iz case MeTTa)
(@doc case
  (@desc "Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)")
  (@params (
    (@param "Atom (it will be evaluated)")
    (@param "Tuple of pairs mapping condition patterns to results")))
  (@return "Result of evaluating Atom bound to met condition"))
(: case (-> (-> $res) (Expression (Atom (-> $res))) %Undefined%))

(iz capture MeTTa)
(@doc capture
  (@desc "Wraps an atom and captures the current space")
  (@params (
    (@param "Function name which space needs to be captured")))
  (@return "Function"))
(: capture (-> Atom Atom))

(iz hyperpose MeTTa)
(@doc hyperpose
  (@desc "Turns a tuple (first argument) into a nondeterministic result, as superpose, but is explicitly concurrent. Each item of the tuple may be processed in parallel, depending on the number of threads available (which is the minimum of the tuple size and the number of cores available).")
  (@params (
    (@param "Tuple to be converted")))
  (@return "Argument converted to nondeterministic result"))
(: hyperpose (-> Expression %Undefined%))

(iz sequential MeTTa)
(@doc sequential
  (@desc "Turns a tuple (first argument) into a nondeterministic result, as superpose, but evaluation order of the elements of the tuple is fixed left to right. In that sense the result order is deterministic iff evaluating the tuple elements is deterministic.")
  (@params (
    (@param "Tuple to be evaluated")))
  (@return "Sequential results of the tuple's elements."))
(: sequential (-> Expression %Undefined%))

(iz do MeTTa)
(@doc do
  (@desc "Completely evaluates form, returning nothing. Typically used for side-effects. A common pattern is (sequential ((do <side-effect-form>) <form-that-needs-side-effect>)).")
  (@params (
    (@param "Form")))
  (@return "None"))
(: do (-> (-> $res) $res))

(iz pragma! MeTTa)
(@doc pragma!
  (@desc "Changes global key's (first argument) value to a new one (second argument).
Possible pragmas:
  (pragma! type-check auto) - check type of the atom before evaluation
  (pragma! interpreter bare-minimal) - use minimal MeTTa semantics when evaluating atom
  (pragma! max-stack-depth <number>) - limit depth of the interpreter's stack, 0 - no limit (default behavior)")
  (@params (
    (@param "Key's name")
    (@param "New value")))
  (@return "Unit atom"))
(: pragma! (-> Symbol %Undefined% (->)))


(iz import! MeTTa)
(@doc import!
  (@desc "Imports module using its relative path (second argument) and binds it to the token (first argument) which will represent imported atomspace. If first argument is &self then everything will be imported to current atomspace. The source is imported only the first time import! is called. Can load Python code (.py) or MeTTa (.metta); if ambiguous, assumes Python.")
  (@params (
    (@param "Symbol, which is turned into the token for accessing the imported module")
    (@param "Module name")))
  (@return "Unit atom"))

(: import! (-> SpaceType (String Location) (->)))

(iz include MeTTa)
(@doc include
  (@desc "Works just like include! but with &self as a first argument. So everything from input file will be included in the current atomspace and evaluated")
  (@params (
    (@param "Name of metta script to import")))
  (@return "Unit atom"))
(: include (-> (String Location) Expression))


(iz include! MeTTa)
(@doc include!
  (@desc "Everything from input file will be included in the current atomspace and evaluated, as if it were being evaluated at the REPL. Unlike import!, the source is evaluated every time include! is called.")
  (@params (
    (@param "Space")
    (@param "Filename")))
  (@return "Expression"))
(: include! (-> SpaceType String Expression))

(iz bind! MeTTa)
(@doc bind!
  (@desc "Registers a new token which is replaced with an atom during the parsing of the rest of the program")
  (@params (
    (@param "Token name")
    (@param "Atom, which is associated with the token after reduction")))
  (@return "Unit atom"))
(: bind! (-> Symbol %Undefined% (->)))

  
(iz trace! MeTTa)
(@doc trace!
  (@desc "Prints its first argument and returns second. Both arguments will be evaluated before processing")
  (@params (
    (@param "Atom to print")
    (@param "Atom to return")))
  (@return "Evaluated second input"))
(: trace! (-> %Undefined% $a $a))


(iz println! MeTTa)
(@doc println!
  (@desc "Prints a line of text to the console")
  (@params (
    (@param "Expression/atom to be printed out afer evaluation")))
  (@return "Unit atom"))
(: println! (-> %Undefined% (->)))
(= (println! $term) (println-atom (eval $term)))


(iz format-args MeTTa)
(@doc format-args
  (@desc "Fills {} symbols in the input expression with atoms from the second expression. E.g. (format-args (Probability of {} is {}%) (head 50)) gives [(Probability of head is 50%)]. Atoms in the second input value could be variables")
  (@params (
    (@param "Expression with {} symbols to be replaced")
    (@param "Atoms to be placed inside expression instead of {}")))
  (@return "Expression with replaced {} with atoms"))
(: format-args (-> String Expression String))


(iz sealed MeTTa)
(@doc sealed
  (@desc "Replaces all occurrences of any var from var list (first argument) inside atom (second argument) by unique variable. Can be used to create a locally scoped variables")
  (@params (
    (@param "Variable list e.g. ($x $y)")
    (@param "Atom which uses those variables")))
  (@return "Second argument but with variables being replaced with unique variables"))
(: sealed (-> Expression Atom Atom))

(iz &self MeTTa)

(@doc &self
  (@desc "Returns reference to the current atomspace")
  (@params ())
  (@return "Reference to the current atomspace"))

; TODO: help! not working for operations which are defined in both Python and
; Rust standard library: +, -, *, /, %, <, >, <=, >=, ==

(iz + MeTTa)
(@doc +
  (@desc "Sums two numbers")
  (@params (
    (@param "Addend")
    (@param "Augend")))
  (@return "Sum"))
(: + (-> Number Number Number))


(iz - MeTTa)
(@doc -
  (@desc "Subtracts second argument from first one")
  (@params (
    (@param "Minuend")
    (@param "Deductible")))
  (@return "Difference"))
(: - (-> Number Number Number))


(iz * MeTTa)
(@doc *
  (@desc "Multiplies two numbers")
  (@params (
    (@param "Multiplier")
    (@param "Multiplicand")))
  (@return "Product"))
(: * (-> Number Number Number))


(iz / MeTTa)
(@doc /
  (@desc "Divides first argument by second one")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Fraction"))
(: / (-> Number Number Number))


(iz % MeTTa)
(@doc %
  (@desc "Modulo operator. It returns remainder of dividing first argument by second argument")
  (@params (
    (@param "Dividend")
    (@param "Divisor")))
  (@return "Remainder"))
(: % (-> Number Number Number))


(iz < MeTTa)
(@doc <
  (@desc "Less than. Checks if first argument is less than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than second, False - otherwise"))
(: < (-> Number Number Bool))


(iz > MeTTa)
(@doc >
  (@desc "Greater than. Checks if first argument is greater than second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than second, False - otherwise"))
(: > (-> Number Number Bool))


(iz <= MeTTa)
(@doc <=
  (@desc "Less than or equal. Checks if first argument is less than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is less than or equal to second, False - otherwise"))
(: <= (-> Number Number Bool))


(iz >= MeTTa)
(@doc >=
  (@desc "Greater than or equal. Checks if first argument is greater than or equal to second one")
  (@params (
    (@param "First number")
    (@param "Second number")))
  (@return "True if first argument is greater than or equal to second, False - otherwise"))
(: >= (-> Number Number Bool))


(iz == MeTTa)
(@doc ==
  (@desc "Checks equality for two arguments of the same type")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Returns True if two arguments are equal, False - otherwise. If arguments are of different type function returns Error currently"))
(: == (-> $t $t Bool))


(iz xor MeTTa)
(@doc xor
  (@desc "Exclusive disjunction of two arguments")
  (@params (
    (@param "First argument")
    (@param "Second argument")))
  (@return "Return values are the same as logical disjunction, but when both arguments are True xor will return False"))

(: xor (-> Bool Bool Bool))

(iz flip MeTTa)
(@doc flip
  (@desc "Produces random boolean value")
  (@params ())
  (@return "Returns uniformly distributed random boolean value"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              NON-DETERMINISTIC SET OPERATIONS WITH PREDICATES          ;;
;;                                                                        ;;
;; These functions extend basic set operations by introducing a comparator;;
;; predicate as the first argument. The predicate determines how          ;;
;; elements are compared for equality, unification, or identity.          ;;
;;                                                                        ;;
;; A few examples of comparator predicates:                              ;;
;;   - =alpha       : Structural equivalence, allowing variable renaming  ;;
;;   - =identical   : Strict structural identity, including bindings       ;;
;;   - =will   : Checks if unification is possible in any way         ;;
;;   - =u=       : Standard unification with side effects               ;;
;;   - =references  : Ensures atoms refer to the same memory object        ;;
;;                                                                        ;;
;; Functions:                                                             ;;
;;   - `unique-by`       : Removes duplicates using a custom predicate  ;;
;;   - `union-by`        : Merges two sets with predicate-based checks  ;;
;;   - `intersection-by` : Computes intersection using a predicate      ;;
;;   - `subtraction-by`  : Subtracts elements based on a predicate      ;;
;;                                                                        ;;
;; These functions handle **non-deterministic data**, using `superpose`.  ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(iz unique-by MeTTaLog)
(@doc unique-by
  (@desc "Function takes a comparator predicate and a non-deterministic input (second argument) and returns only unique entities according to the predicate. 
  E.g. (unique-by =alpha (superpose (a b c d d))) -> [a, b, c, d]")
  (@params (
	(@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
	(@param "Non-deterministic set of values")))
  (@return "Unique non-deterministic values from input set based on predicate"))
(: unique-by (-> (-> $res $res Bool) (-> $res) $res))
(= (unique-by $op $code)
	(let $answers (new-queue)
	 (let $result (eval $code)
	    (if (enque-by $op $answers $result)
	       $result
		   (empty)))))


(iz union-by MeTTaLog)
(@doc union-by
  (@desc "Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their union. 
  E.g. (union-by =u= (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]")
  (@params (
	(@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
	(@param "Non-deterministic set of values")
	(@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Union of sets based on predicate"))
(: union-by (-> (-> $res $res Bool) (-> $res) (-> $res) $res))

(iz intersection-by MeTTaLog)
(@doc intersection-by
  (@desc "Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their intersection. 
  E.g. (intersection-by =will (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]")
  (@params (
	(@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
	(@param "Non-deterministic set of values")
	(@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Intersection of sets based on predicate"))
(: intersection-by (-> (-> $res $res Bool) (-> $res) (-> $res) $res))

(iz subtraction-by MeTTaLog)
(@doc subtraction-by
  (@desc "Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their subtraction. 
  E.g. !(subtraction-by =identical (superpose (a b b c)) (superpose (b c c d))) -> [a, b]")
  (@params (
	(@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
	(@param "Non-deterministic set of values")
	(@param "Another non-deterministic set of values")))
  (@return "Non-deterministic Subtraction of sets based on predicate"))
(: subtraction-by (-> (-> $res $res Bool) (-> $res) (-> $res) $res))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    UNIFICATION AND EQUIVALENCE PREDICATES              ;;
;;                                                                        ;;
;; These predicates determine whether two atoms can unify or are          ;;
;; structurally equivalent. Some predicates actively perform unification, ;;
;; while others only check whether unification *could* occur.             ;;
;;                                                                        ;;
;; **Theoretical Equivalence & Unification Predicates (Check Possibility Only):** ;;
;;   - `=alpha`        : Checks if two atoms are structurally equivalent, ;;
;;                       allowing renaming, but does **not** unify them.  ;;
;;   - `=will`         : Checks if two atoms *could* unify in theory, but ;;
;;                       does **not** modify bindings or unify variables. ;;
;;                                                                        ;;
;; **Unification Predicates (Perform Binding):**                          ;;
;;   - `=u=`        : **Performs** unification and binds variables.    ;;
;;   - `=alpha-unify`  : **Performs** unification with variable renaming. ;;
;;                                                                        ;;
;; **Strict Identity & Reference Predicates:**                            ;;
;;   - `=identical`    : Checks if two atoms are strictly identical,      ;;
;;                       including variable names and bindings.           ;;
;;   - `=references`   : Checks if two atoms reference the **same**       ;;
;;                       underlying entity in memory.                     ;;
;;                                                                        ;;
;; Predicates in the "Perform Binding" category actively modify variable  ;;
;; bindings when successful. The "Check Possibility Only" predicates do   ;;
;; **not** modify bindings; they merely verify whether equivalence or     ;;
;; unification might be possible.                                         ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(iz =alpha MeTTa)
(@doc =alpha
  (@desc "Predicate that **checks** if two atoms are structurally equivalent, allowing variable renaming.
  Unlike `=alpha-unify`, this predicate does **not** perform unification or modify variables—it only verifies whether the two atoms *could* be equivalent with renaming.")
  (@params (
	(@param "First atom")
	(@param "Second atom")))
  (@return "True if the atoms are structurally equivalent with renaming, false otherwise."))
(: =alpha (-> Atom Atom Bool))

(iz =will MeTTaLog)
(@doc =will
  (@desc "Predicate that **checks** if two atoms *could* unify in theory but does **not** perform unification.
  Unlike `=u=`, this only verifies whether unification is **possible** without actually binding variables.")
  (@params (
	(@param "First atom")
	(@param "Second atom")))
  (@return "True if unification is theoretically possible, false otherwise."))
(: =will (-> Atom Atom Bool))

(iz =u= MeTTaLog)
(@doc =u=
  (@desc "Predicate that determines if two atoms can be unified under standard unification rules, meaning they can be made identical by substituting variables appropriately. 
  We do **not** use `==` because `(== $x $y)` would be untrue and have **no side effects**, whereas `=u=` performs unification where possible.")
  (@params (
	(@param "First atom")
	(@param "Second atom")))
  (@return "True if the atoms can be unified, false otherwise."))
(: =u= (-> Atom Atom Bool))

(iz =alpha-unify MeTTaLog)
(@doc =alpha-unify
  (@desc "Predicate that **performs** unification while considering alpha-equivalence. 
  It allows variable renaming where necessary to achieve successful unification.")
  (@params (
	(@param "First atom")
	(@param "Second atom")))
  (@return "True if the atoms unify successfully, considering alpha-equivalence."))
(: =alpha-unify (-> Atom Atom Bool))

(iz =identical MeTTaLog)
(@doc =identical
  (@desc "Predicate that determines if two atoms are completely identical in structure, including variable names and their bindings. 
  This is a stricter comparison than `=alpha` or `=u=`, ensuring that variables and their values match exactly.")
  (@params (
	(@param "First atom")
	(@param "Second atom")))
  (@return "True if the atoms are strictly identical, false otherwise."))
(: =identical (-> Atom Atom Bool))

(iz =references MeTTaLog)
(@doc =references
  (@desc "Predicate that determines if two atoms reference the **same** underlying entity in memory.  like lisps `EQ` 
  This is the strictest form of equality, ensuring the atoms are not just identical in structure but are literally the **same instance**.")
  (@params (
	(@param "First atom")
	(@param "Second atom")))
  (@return "True if the atoms reference the same object, false otherwise."))
(: =references (-> Atom Atom Bool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   TUPLE-BASED SET OPERATIONS WITH PREDICATES           ;;
;;                                                                        ;;
;; These functions extend tuple-based set operations by introducing a     ;;
;; comparator predicate as the first argument. This allows elements to be ;;
;; compared using unification, alpha equivalence, or strict identity.     ;;
;;                                                                        ;;
;; A few examples of comparator predicates:                              ;;
;;   - =alpha       : Structural equivalence, allowing variable renaming  ;;
;;   - =identical   : Strict structural identity, including bindings       ;;
;;   - =will   : Checks if unification is possible in any way         ;;
;;   - =u=       : Standard unification with side effects               ;;
;;   - =references  : Ensures atoms refer to the same memory object        ;;
;;                                                                        ;;
;; Functions:                                                             ;;
;;   - `unique-atom-by`       : Removes duplicates using a predicate    ;;
;;   - `union-atom-by`        : Merges two tuples with predicate-based  ;;
;;   - `intersection-atom-by` : Computes intersection using a predicate ;;
;;   - `subtraction-atom-by`  : Subtracts elements based on a predicate ;;
;;                                                                        ;;
;; These functions handle **tuple-based data**, using `superpose` where   ;;
;; necessary to ensure compatibility with non-deterministic operations.   ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(iz unique-atom-by MeTTaLog)
(@doc unique-atom-by
  (@desc "Function takes a comparator predicate and a tuple and returns only unique entities according to the predicate. 
  E.g. (unique-atom-by =alpha (a b c d d)) -> (a b c d)")
  (@params (
    (@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
    (@param "List of values")))
  (@return "Unique values from input set based on predicate"))
(: unique-atom-by (-> Symbol Expression Expression))
(= (unique-atom-by $Pred $L1)
  (collapse (unique-by $Pred (superpose-atom $L1))))


(iz union-atom-by MeTTaLog)
(@doc union-atom-by
  (@desc "Function takes a comparator predicate and two tuples and returns their union. 
  E.g. (union-atom-by =u= (a b b c) (b c c d)) -> (a b b c b c c d)")
  (@params (
    (@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
    (@param "List of values")
    (@param "List of values")))
  (@return "Union of sets based on predicate"))
(= (union-atom-by $Pred $L1 $L2)
  (collapse (union-by $Pred (superpose-atom $L1) (superpose-atom $L2))))
(: union-atom-by (-> Symbol Expression Expression Expression))

(iz intersection-atom-by MeTTaLog)
(@doc intersection-atom-by
  (@desc "Function takes a comparator predicate and two tuples and returns their intersection. 
  E.g. (intersection-atom-by =will (a b c c) (b c c c d)) -> (b c c)")
  (@params (
    (@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
    (@param "List of values")
    (@param "List of values")))
  (@return "Intersection of sets based on predicate"))
(= (intersection-atom-by $Pred $L1 $L2)
  (collapse (intersection-by $Pred (superpose-atom $L1) (superpose-atom $L2))))
(: intersection-atom-by (-> Symbol Expression Expression Expression))

(iz subtraction-atom-by MeTTaLog)
(@doc subtraction-atom-by
  (@desc "Function takes a comparator predicate and two tuples and returns their subtraction. 
  E.g. (subtraction-atom-by =identical (a b b c) (b c c d)) -> (a b)")
  (@params (
    (@param "Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)")
    (@param "List of values")
    (@param "List of values")))
  (@return "Subtraction of sets based on predicate"))
(= (subtraction-atom-by $Pred $L1 $L2)
  (collapse (subtraction-by $Pred (superpose-atom $L1) (superpose-atom $L2))))
(: subtraction-atom-by (-> Atom Expression Expression Expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        TUPLE-BASED SET OPERATIONS                      ;;
;;                                                                        ;;
;; These functions operate on **tuples** instead of non-deterministic     ;;
;; sets. They provide the same core operations but assume that inputs     ;;
;; are lists of values rather than `superpose` expressions.               ;;
;;                                                                        ;;
;; Functions:                                                             ;;
;;   - `unique-atom`        : Removes duplicate values in a tuple         ;;
;;   - `union-atom`         : Merges two tuples, preserving duplicates    ;;
;;   - `intersection-atom`  : Returns values found in both tuples         ;;
;;   - `subtraction-atom`   : Removes elements found in the second tuple  ;;
;;                                                                        ;;
;; These functions assume strict equality comparisons.                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(iz unique-atom MeTTa)
(@doc unique-atom
  (@desc "Function takes tuple and returns only unique entities. E.g. (unique-atom (a b c d d)) -> (a b c d)")
  (@params (
    (@param "List of values")))
  (@return "Unique values from input set"))
(= (unique-atom $L1)
  (collapse (unique (superpose-atom $L1))))
(: unique-atom (-> Expression Expression))

(iz union-atom MeTTa)
(@doc union-atom
  (@desc "Function takes two tuples and returns their union. E.g. (union-atom (a b b c) (b c c d)) -> (a b b c b c c d)")
  (@params (
    (@param "List of values")
    (@param "List of values")))
  (@return "Union of sets"))
(: union-atom (-> Expression Expression Expression))
(= (union-atom $L1 $L2)
  (collapse (union (superpose-atom $L1) (superpose-atom $L2))))

(iz intersection-atom MeTTa)
(@doc intersection-atom
  (@desc "Function takes two tuples and returns their intersection. E.g. (intersection-atom (a b c c) (b c c c d)) -> (b c c)")
  (@params (
    (@param "List of values")
    (@param "List of values")))
  (@return "Intersection of sets"))
(: intersection-atom (-> Expression Expression Expression))
(= (intersection-atom $L1 $L2)
  (collapse (intersection (superpose-atom $L1) (superpose-atom $L2))))

(iz subtraction-atom MeTTa)
(@doc subtraction-atom
  (@desc "Function takes two tuples and returns their subtraction. E.g. !(subtraction-atom (a b b c) (b c c d)) -> (a b)")
  (@params (
    (@param "List of values")
    (@param "List of values")))
  (@return "Subtraction of sets"))
(: subtraction-atom (-> Expression Expression Expression))
(= (subtraction-atom $L1 $L2)
  (collapse (subtraction (superpose-atom $L1) (superpose-atom $L2))))


(iz superpose-atom MeTTa)
(@doc superpose-atom
  (@desc "Function take a tuple and returns each element E.g. !(format-args "{}" ((superpose-atom (1 (+ 1 1) 3)))) -> [\"1\" \"(+ 1 1)\" \"3\"])")
  (@params (
	(@param "List of values")))
  (@return "Each element non-deterministically"))
(: superpose-atom (-> Expression Atom))
(ML= (superpose-atom $L1)
  (call-for! $ele (@ member $ele $L1)))


(iz println-atom! MeTTa)
(@doc println-atom!
  (@desc "Prints an Atom it without evaluation")
  (@params (
	(@param "Anything")))
  (@return "Unit atom"))
(: println-atom! (-> Atom (->)))
(ML= (println-atom! $term)
  (call-unit! (@ write-src $term)))
(HE= (println-atom! $term)
  (println! (format-args "{}" ($term))))


(iz with-space MeTTaLog)
(@doc with-space
  (@desc "Temporally override &self while running MeTTa")
  (@params (
	(@param "Space to evaluate from inside of")
	(@param "Anything")))
  (@return "Result of evaluation"))
(: with-space (-> Grounded @Body ))
(ML= (with-space $space |.| $Body)
  (@ with_space $space $Body))





(iz git-module! MeTTa)
(@doc git-module!
  (@desc "Provides access to module in a remote git repo, from within MeTTa code. Similar to `register-module!`, this op will bypass the catalog search")
  (@params (
    (@param "URL to github repo")))
  (@return "Unit atom"))
(: git-module! (-> Atom (->)))
	

(iz get-vtype CodeFunction)
(iz get-types CodeFunction)
(iz get-dtype CodeFunction)
(iz get-ftype CodeFunction)
(iz pragma! CodeFunction)
(iz py-chain CodeFunction)
(iz cons-atom CodeFunction)
(iz type-cast CodeFunction)
(iz filter-atom CodeFunction)
(iz nop CodeFunction)
(iz get-metatype CodeFunction)
(iz capture CodeFunction)
(iz sealed CodeFunction)
(iz git-module! CodeFunction)



