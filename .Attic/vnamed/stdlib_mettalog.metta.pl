%% Generated from /home/deb12user/metta-wam/.Attic/vnamed/stdlib_mettalog.metta at 2025-06-17T11:44:24-07:00
:- style_check(-discontiguous).
:- style_check(-singleton).
:- include(library(metta_lang/metta_transpiled_header)).

%  ;; Type Declarations with Documentation
metta_atom_asserted('&corelib',['is-mettalog']).
metta_atom_asserted('&corelib',['@doc','Any',['@desc',"The universal type; any value belongs to this type."]]).
metta_atom_asserted('&corelib',[:,'Any','Type']).
metta_atom_asserted('&corelib',[:,'RandomGenerator','Type']).
metta_atom_asserted('&corelib',['@doc','RandomGenerator',['@desc',"Type representing a random number generator."]]).
metta_atom_asserted('&corelib',['@doc','Atom',['@desc',"Type representing any atom."]]).
metta_atom_asserted('&corelib',[:,'Atom','Type']).
metta_atom_asserted('&corelib',['@doc','LazyEvaluatable',['@desc',"A type of Atom/Value that hyperon does not implicitly evaluate"]]).
metta_atom_asserted('&corelib',[:,'LazyEvaluatable','Type']).
metta_atom_asserted('&corelib',[:>,'Atom','LazyEvaluatable']).
metta_atom_asserted('&corelib',['@doc','Bool',['@desc',"Boolean type of True or False."]]).
metta_atom_asserted('&corelib',[:,'Bool','Type']).
metta_atom_asserted('&corelib',['@doc','LazyBool',['@desc',"A LazyEvaluatable that when evaluated returns True or False."]]).
metta_atom_asserted('&corelib',[:,'LazyBool','Type']).
metta_atom_asserted('&corelib',[:>,'LazyBool','LazyEvaluatable']).
metta_atom_asserted('&corelib',['@doc','Expression',['@desc',"Type representing an S-Expression, which is a combination of atoms."]]).
metta_atom_asserted('&corelib',[:,'Expression','Type']).
metta_atom_asserted('&corelib',[:>,'Expression','LazyEvaluatable']).
metta_atom_asserted('&corelib',['@doc','Number',['@desc',"Numeric type, including integers and floating-point numbers."]]).
metta_atom_asserted('&corelib',[:,'Number','Type']).
metta_atom_asserted('&corelib',['@doc','hyperon::space::DynSpace',['@desc',"Dynamic space type, representing an Atomspace."]]).
metta_atom_asserted('&corelib',[:,'hyperon::space::DynSpace','Type']).
metta_atom_asserted('&corelib',['@doc','ReturnType',['@desc',"Type representing a function's return value."]]).
metta_atom_asserted('&corelib',[:,'ReturnType','Type']).
metta_atom_asserted('&corelib',['@doc','Symbol',['@desc',"Type representing a symbol or identifier."]]).
metta_atom_asserted('&corelib',[:,'Symbol','Type']).
metta_atom_asserted('&corelib',['@doc','StateMonad',['@desc',"Type representing a state monad, used for encapsulating stateful computations."]]).
metta_atom_asserted('&corelib',[:,'StateMonad','Type']).
metta_atom_asserted('&corelib',['@doc','Type',['@desc',"Type representing a type."]]).
metta_atom_asserted('&corelib',[:,'Type','Type']).
metta_atom_asserted('&corelib',['@doc','True',['@desc',"Boolean value representing truth."]]).
metta_atom_asserted('&corelib',[:,'True','Bool']).
metta_atom_asserted('&corelib',['@doc','False',['@desc',"Boolean value representing falsehood."]]).
metta_atom_asserted('&corelib',[:,'False','Bool']).
metta_atom_asserted('&corelib',['@doc','%Undefined%',['@desc',"Special type representing an undefined value or type."]]).
metta_atom_asserted('&corelib',[:,'%Undefined%','Type']).
metta_atom_asserted('&corelib',['@doc','Variable',['@desc',"Type representing a variable in the language."]]).
metta_atom_asserted('&corelib',[:,'Variable','Type']).
metta_atom_asserted('&corelib',['@doc',:,['@desc',"Type declarion operator"]]).
metta_atom_asserted('&corelib',['@doc',<:,['@desc',"Super Type declarion operator"]]).
metta_atom_asserted('&corelib',[:,:,'%Undefined%']).
%  ; match hyperons weirdd return value of !(get-type :)
metta_atom_asserted('&corelib',[:,'if-empty',[->,'Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[:,'if-non-empty-expression',[->,'Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[:,'if-not-reducible',[->,'Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[:,return,[->,'Atom','ReturnType']]).
metta_atom_asserted('&corelib',[:,switch,[->,'%Undefined%','Expression','Atom']]).
metta_atom_asserted('&corelib',[:,unify,[->,'Atom','Atom','Atom','Atom','%Undefined%']]).
%  ;(: current-predicate-arity (-> Predicate Number)) ; returns only the first type of a symbol
metta_atom_asserted('&corelib',[:,'get-vtype',[->,'Atom','Atom']]).
%  ; returns only the first type of a symbol
metta_atom_asserted('&corelib',[:,'get-vtype',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the first type of a symbol
metta_atom_asserted('&corelib',[:,'get-types',[->,'Atom','Atom']]).
%  ; returns only the first type of a symbol
metta_atom_asserted('&corelib',[:,'get-types',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the first type of a symbol
metta_atom_asserted('&corelib',[:,'get-dtype',[->,'Atom','Atom']]).
%  ; returns only the data value types of a symbol
metta_atom_asserted('&corelib',[:,'get-dtype',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the data value types of a symbol
metta_atom_asserted('&corelib',[:,'get-ftype',[->,'Atom','Atom']]).
%  ; returns only the function types of a symbol
metta_atom_asserted('&corelib',[:,'get-ftype',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the function types of a symbol
metta_atom_asserted('&corelib',[:,'pragma!',[->,'Atom','Atom',[->]]]).
%  ;(: = (-> Atom Atom %Undefined%))
metta_atom_asserted('&corelib',[:,match,[->,'hyperon::space::DynSpace','Atom','Atom','%Undefined%']]).
%  ;(: case (-> Expression Atom Atom))
metta_atom_asserted('&corelib',[:,'import!',[->,'hyperon::space::DynSpace','Atom',[->]]]).
metta_atom_asserted('&corelib',['@doc','type-check',['@desc',"The value of type-check determines MeTTa's type-checking behavior. Set via pragma!. When set to auto (i.e. !(pragma! type-check auto)), types are checked immediately on adding an expression to the space. By default, when unset (or set to anything other than auto), types are checked only on evaluation. For example !(+ 1 \"2\") would trigger a type violation, but (= (foo $x) (+ $x \"2\")) would not, unless type-check is set to auto, in which case both would trigger type violations."]]).
metta_atom_asserted('&corelib',[:,'type-check','Symbol']).
metta_atom_asserted('&corelib',[iz,'predicate-arity','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','predicate-arity',['@desc',"Specifies the arity (number of arguments) for a given predicate, allowing it to be queriable in the system's match framework. This is particularly useful for enabling built-in functions, such as `size-atom`, to be used as predicates in declarative contexts and run in reverse to compute inputs based on outputs.\n\n\nFor example:\n  ; Enable the built-in function `size-atom` that takes an atom and returns the size\n    as a predicate with arity 2\n  (predicate-arity size-atom 2)\n\n\n  ; Now `size-atom` can be used as a predicate in pattern matching\n  !(match &dyn-space '(size-atom (a b c) $size) \n         (The abc tuple was len $size))\n  ; This pattern will resolve `Size = 3` and execute the action.\n\n\nAdditionally, by running `size-atom` in reverse, you can compute a new atom based on a desired size:\n  !(match &dyn-space '(size-atom $new-atom 4) \n         (The new atom is $new-atom))\n  ; This resolves `$new-atom` to a tuple of size 4, such as ($1 $2 $3 $4).\n\n\nThis reverse functionality is made possible because predicates can describe relationships, allowing you to infer inputs from outputs."],['@params',[['@param',"Predicate symbol","The name of the predicate whose arity is being defined."]]],['@return',"The number of arguments required for the predicate."]]).
metta_atom_asserted('&corelib',[:,'predicate-arity',[->,'Symbol','Number']]).
metta_atom_asserted('&corelib',['predicate-arity','predicate-arity',2]).
metta_atom_asserted('&corelib',['function-arity','predicate-arity',1]).
metta_atom_asserted('&corelib',['@doc','function-arity',['@desc',"Defines the arity of a function, allowing predicates or built-in facts to also behave as callable functions. This enables procedural-style execution where the last argument of the predicate becomes the function's return value, and the system internally resolves the function using a `match` query. \n\n\nFor example:\n  ; Declare the built-in predicate `max` with arity 3\n  (predicate-arity max 3)\n\n  ; Enable `max` as a function\n  (function-arity max 2)\n\n\n  ; Define the rules for `max`\n  (= (max $X $Y $Y) (<= $X $Y))\n  (= (max $X $Y $X) (> $X $Y))\n\n\n  ; Using `max` declaratively as a predicate\n  !(match &self (max (5 10) $max)\n         (The maximum is $max))\n  [(The maximum is 10)]\n\n\n  ; Using `max` procedurally as a function\n  !(max 5 10)\n  [10]\n  \n\n\n  ; Reverse execution with `max`\n  !(let True (== (max $a $b) 10) ($a $b)) ; as a function\n  [(#(exists $a (=< $a 10)) 10), (10 #(exists $b (=< 10 $b )))]\n  !(match &self (max $a $b 10) ($a $b)) ; or as a predicate\n  [(#(exists $a (=< $a 10)) 10), (10 #(exists $b (=< 10 $b )))]\n\n\n  This dual behavior allows predicates to act as functions, bridging procedural and declarative paradigms. By defining `function-arity`, the function automatically resolves using the logic of the associated predicate."],['@params',[['@param',"Function symbol","The name of the function or predicate to enable as a callable function."]]],['@return',"The number of arguments expected by the function."]]).
metta_atom_asserted('&corelib',[:,'function-arity',[->,'Symbol','Number']]).
metta_atom_asserted('&corelib',['predicate-arity','function-arity',2]).
metta_atom_asserted('&corelib',['function-arity','function-arity',1]).
%  ;; MettaMorph-If Function
metta_atom_asserted('&corelib',[iz,'MettaMorph-If','MeTTaMorph']).
metta_atom_asserted('&corelib',['@doc','MettaMorph-If',['@desc',"Conditional function that evaluates and returns one of the provided atoms based on a boolean condition."],['@params',[['@param',"Boolean condition"],['@param',"Atom to return if condition is True"],['@param',"Atom to return if condition is False (optional)"]]],['@return',"Either the second or third argument depending on the condition"]]).
metta_atom_asserted('&corelib',[:,'MettaMorph-If',[->,'Bool','Atom','Atom']]).
%  ; (= (MettaMorph-If False $then) (empty))
%  ; Placeholder for False condition
metta_atom_asserted('&corelib',[:,'MettaMorph-If',[->,'Bool','Atom','Atom','Atom']]).
%  ;; Arity Assignments
metta_atom_asserted('&corelib',['predicate-arity','predicate-arity',2]).
%  ;(predicate-arity : 3)
%  ; (= (: $F P1) (predicate-arity $F 1))
%  ;; Source Predicate and Function Types
metta_atom_asserted('&corelib',[iz,'SrcPredicate','MeTTa']).
metta_atom_asserted('&corelib',['@doc','SrcPredicate',['@desc',"Type representing a source predicate."]]).
metta_atom_asserted('&corelib',[:,'SrcPredicate','Type']).
metta_atom_asserted('&corelib',[iz,'SrcFunction','MeTTa']).
metta_atom_asserted('&corelib',['@doc','SrcFunction',['@desc',"Type representing a source function."]]).
metta_atom_asserted('&corelib',[:,'SrcFunction','Type']).
%  ;; MeTTaResult Type and Values
metta_atom_asserted('&corelib',[iz,'MeTTaResult','MeTTa']).
metta_atom_asserted('&corelib',['@doc','MeTTaResult',['@desc',"Type representing the result of a MeTTa evaluation."]]).
metta_atom_asserted('&corelib',[:,'MeTTaResult','Type']).
metta_atom_asserted('&corelib',[iz,'NotReducible','MeTTaResult']).
metta_atom_asserted('&corelib',['@doc','NotReducible',['@desc',"Result indicating that an atom cannot be reduced further."]]).
metta_atom_asserted('&corelib',[:,'NotReducible','MeTTaResult']).
metta_atom_asserted('&corelib',[iz,'Empty','MeTTaResult']).
metta_atom_asserted('&corelib',['@doc','Empty',['@desc',"Result indicating an empty evaluation result."]]).
metta_atom_asserted('&corelib',[:,'Empty','MeTTaResult']).
%  ;; Subtype Relations
metta_atom_asserted('&corelib',[iz,'ValueAtom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','ValueAtom',['@desc',"Type representing a value atom."]]).
metta_atom_asserted('&corelib',[:>,'ValueAtom','Atom']).
metta_atom_asserted('&corelib',[iz,'ForeignObject','MeTTa']).
metta_atom_asserted('&corelib',['@doc','ForeignObject',['@desc',"Type representing a foreign object, such as a Python object."]]).
metta_atom_asserted('&corelib',[:,'ForeignObject','Type']).
metta_atom_asserted('&corelib',[:>,'ValueAtom','Grounded']).
metta_atom_asserted('&corelib',[:>,'ForeignObject','ValueAtom']).
metta_atom_asserted('&corelib',[iz,'PyObject','MeTTa']).
metta_atom_asserted('&corelib',['@doc','PyObject',['@desc',"Type representing a Python object."]]).
metta_atom_asserted('&corelib',[:>,'PyObject','ForeignObject']).
%  ;; Space Subtype Relation
metta_atom_asserted('&corelib',['@doc','hyperon::space::DynSpace',['@desc',"Dynamic space type, representing an Atomspace."]]).
metta_atom_asserted('&corelib',[:>,'hyperon::space::DynSpace','Grounded']).
metta_atom_asserted('&corelib',[iz,'py-list','MeTTa']).
metta_atom_asserted('&corelib',['@doc','py-list',['@desc',"Converts a MeTTa Expression into a Python list and returns it as a PyObject."],['@params',[['@param',['@desc',"A MeTTa List that will be converted into a Python list"]]]],['@return',['@desc',"A Python list object that represents the given MeTTa Expression as a PyObject"]]]).
metta_atom_asserted('&corelib',[:,'py-list',[->,'Expression','PyObject']]).
metta_atom_asserted('&corelib',[iz,'py-chain','MeTTa']).
metta_atom_asserted('&corelib',['@doc','py-chain',['@desc',"Chains together a list of Python objects contained in a MeTTa Expression, applying the Python vertical bar | OR operation jointly to all elements."],['@params',[['@param',['@desc',"A MeTTa list of atoms, each embedding a Python object."]]]],['@return',['@desc',"A MeTTa atom which embeds the result of applying the Python OR | operator to all elements of the list."]]]).
metta_atom_asserted('&corelib',[:,'py-chain',[->,'Expression','PyObject']]).
metta_atom_asserted('&corelib',[iz,'py-eval','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','py-eval',['@desc',"Evaluates a Python expression from a string and returns the result as a PyObject."],['@params',[['@param',['@desc',"A string representing a Python expression that will be evaluated"]]]],['@return',['@desc',"The result of evaluating the Python expression as a PyObject"]]]).
metta_atom_asserted('&corelib',[:,'py-eval',[->,'String','PyObject']]).
metta_atom_asserted('&corelib',[iz,'py-exec!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','py-exec!',['@desc',"Executes some Python sourcecode from a string !(py-exec! \"import torch\") -> True.  !(py-exec! \"import torche\") -> False. "],['@params',[['@param',['@type','String'],['@desc',"A string representing a Python sourcecode that will be executed"]]]],['@return',['@desc',"The True|False results of executing the Python expression"]]]).
metta_atom_asserted('&corelib',[:,'py-exec!',[->,'String','Bool']]).
%  ; !(import! &corelib "src/canary/stdlib_mettalog.metta")
%  ;!(println! "!(import! &corelib \"src/canary/stdlib_mettalog.metta\")")
metta_atom_asserted('&corelib',[iz,'findall!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','findall!',['@desc',"Takes a Template and a Goal. Returns the list resulting from substituting all bindings from solving Goal into Template.\n  See Prolog's built-in predicate findall/3."],['@params',[['@param',"Template"],['@param',"Goal"]]],['@return',"Result list of all bindings for Goal substituted into Template"]]).
metta_atom_asserted('&corelib',[:,'findall!',[->,'Expression','Expression','Expression']]).
%  ;; Functional Programming
metta_atom_asserted('&corelib',[iz,'maplist!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','maplist!',['@desc',"Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). \n The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).\n F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).\n Use concurrent-maplist! for a multi-threaded, nondeterministic version.\n See Prolog's built-in predicate maplist."],['@params',[['@param',"Function to be applied"],['@param',"List"]]],['@return',"Result of applying Function to List(s)"]]).
metta_atom_asserted('&corelib',[:,'maplist!',[->,'Function','Expression','Expression']]).
metta_atom_asserted('&corelib',[:,'maplist!',[->,'Function','Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',[:,'maplist!',[->,'Function','Expression','Expression','Expression','Expression']]).
%  ;; Functional Programming
metta_atom_asserted('&corelib',[iz,'concurrent-maplist!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','concurrent-maplist!',['@desc',"Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). \n The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).\n F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).\n The applications of F to the list items are processed in parallel. Because of the overhead of this approach, a speedup is only likely if F is expensive to evaluate.\n Use maplist! for a single-threaded, deterministic version.\n See Prolog's built-in predicate concurrent-maplist."],['@params',[['@param',"Function to be applied"],['@param',"List"]]],['@return',"Result of applying Function to List(s)"]]).
metta_atom_asserted('&corelib',[:,'concurrent-maplist!',[->,'Function','Expression','Expression']]).
metta_atom_asserted('&corelib',[:,'concurrent-maplist!',[->,'Function','Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',[:,'concurrent-maplist!',[->,'Function','Expression','Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',[iz,throw,'MeTTaLog']).
metta_atom_asserted('&corelib',['@doc',throw,['@desc',"Raises an exception. See also `catch`; the system will look for the innermost catch such that Exception unifies with Catcher."],['@params',[['@param',"Exception"]]],['@return',"Does not return - raises an exception"]]).
metta_atom_asserted('&corelib',[:,throw,[->,'Atom','ErrorType']]).
metta_atom_asserted('&corelib',[iz,catch,'MeTTaLog']).
metta_atom_asserted('&corelib',['@doc',catch,['@desc',"Executes Form. If an exception is raised with `throw` during execution of Form while this is the innermost catch such that Catcher unifies with Exception, the exception is caught. Recover is then executed with bindings from Catcher."],['@params',[['@param',"Form"],['@param',"Catcher"],['@param',"Recover"]]],['@return',"Result of Form if no exception is raised. Result of Recover (with bindings from Catcher) if an exception is caught."]]).
metta_atom_asserted('&corelib',[:,catch,[->,'Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'max-time!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','max-time!',['@desc',"Executes Form. If execution takes longer than Time, will raise a time_limit_exceeded exception. See also `catch`."],['@params',[['@param',"Time (in seconds)"],['@param',"Form"]]],['@return',"Result of Form if execution completes within Time. Raises an exception otherwise."]]).
metta_atom_asserted('&corelib',[:,'max-time!',[->,'Number','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'sleep!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','sleep!',['@desc',"Sleeps for N seconds."],['@params',[['@param',"N"]]],['@return',"Returns True after sleeping completes."]]).
metta_atom_asserted('&corelib',[:,'sleep!',[->,'Number','Bool']]).
metta_atom_asserted('&corelib',[iz,'limit!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','limit!',['@desc',"Executes Form generating at most Limit results. Results are returned as soon as they become available."],['@params',[['@param',"Limit"],['@param',"Form"]]],['@return',"First Limit results of Form."]]).
metta_atom_asserted('&corelib',[:,'limit!',[->,'Number','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'number-of','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','number-of',['@desc',"Returns the number of results Form generates"],['@params',[['@param',"Form"]]],['@return',"Number of results of Form."]]).
metta_atom_asserted('&corelib',[:,'number-of',[->,'Atom','Number']]).
metta_atom_asserted('&corelib',[iz,'offset!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','offset!',['@desc',"Executes Form ignoring the first Count results. Results are returned as soon as they become available."],['@params',[['@param',"Count"],['@param',"Form"]]],['@return',"Results of Form after ignoring the first Count results that are generated."]]).
metta_atom_asserted('&corelib',[:,'offset!',[->,'Number','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'call!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','call!',['@desc',"Trampolines to Prolog's call. Only works when the predicate and each arg are provided separately. e.g. !(call! ls /) will print the root directory but !(call! ls(/)) will fail."],['@params',[['@param',"Form"]]],['@return',"True if the call succeeds, False otherwise."]]).
metta_atom_asserted('&corelib',[:,'call!',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'call-p!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','call-p!',['@desc',"Like call-fn! except it does not append the called term with a return arg."],['@params',[['@param',"Form"]]],['@return',"True if the call succeeds, False otherwise."]]).
metta_atom_asserted('&corelib',[:,'call-p!',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'call-fn!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','call-fn!',['@desc',"Trampolines to Prolog's call after appending the return argument.  Each arg are provided separately. e.g. !(call-fn! length (1 2 3)) will return 3."],['@params',[['@param',"Form"]]],['@return',"appends a return argument to a form and calls it"]]).
metta_atom_asserted('&corelib',[:,'call-fn!',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'call-string!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','call-string!',['@desc',"Executes String as Prolog code. For example, (call-string! \"member(1,[1,2,3])\") returns [()] while (call-string! \"member(4,[1,2,3])\") returns []. (call-string! \"member(X,[1,2,3])\") returns [(1), (2), (3)]."],['@params',[['@param',"PrologSrc"]]],['@return',"A list of the binding values. If there are no bindings but the Prolog query is True, returns the empty list."]]).
metta_atom_asserted('&corelib',[:,'call-string!',[->,'String','Atom']]).
metta_atom_asserted('&corelib',[iz,'call-cleanup!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','call-cleanup!',['@desc',"Same as (setup-call-cleanup! true Form Cleanup). setup-call-cleanup! is recommended instead if Cleanup is intended to undo prior side-effects - place those side-effects in Setup."],['@params',[['@param',"Form"],['@param',"Cleanup"]]],['@return',"Result of Form."]]).
metta_atom_asserted('&corelib',[:,'call-cleanup!',[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'setup-call-cleanup!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','setup-call-cleanup!',['@desc',"Executes Setup, then Form, then finally Cleanup. Setup is protected from interrupts (e.g. max-time!). In most uses, Setup will perform temporary side-effects required by Form that are finally undone by Cleanup. Cleanup is run even if Form raises an exception. For each result of Setup, Form is run to completion, then Cleanup is run."],['@params',[['@param',"Setup"],['@param',"Form"],['@param',"Cleanup"]]],['@return',"Result of Form."]]).
metta_atom_asserted('&corelib',[:,'setup-call-cleanup!',[->,'Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'predicate-arity','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','predicate-arity',['@desc',"Returns the arity of Function's predicate form, i.e. the function-arity + 1. (The additional argument being the function's result as an argument to the predicate.)"],['@params',[['@param',"Function"]]],['@return',"Arity of Function's predicate form."]]).
metta_atom_asserted('&corelib',[iz,'function-arity','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','function-arity',['@desc',"Returns the arity of Function."],['@params',[['@param',"Function"]]],['@return',"Arity of Function."]]).
metta_atom_asserted('&corelib',[iz,'open!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','open!',['@desc',"Opens Filename as a stream under Mode. Mode is one of read, write, append, or update. Mode append opens the file for writing, positioning the file pointer at the end. Mode update opens the file for writing, positioning the file pointer at the beginning of the file without truncating the file."],['@params',[['@param',"Filename"],['@param',"Mode"]]],['@return',"Stream"]]).
metta_atom_asserted('&corelib',[:,'open!',[->,'String','Atom','Stream']]).
metta_atom_asserted('&corelib',[iz,'close!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','close!',['@desc',"Closes Steam, returning True on success."],['@params',[['@param',"Stream"]]],['@return',"Boolean"]]).
metta_atom_asserted('&corelib',[:,'close!',[->,'Stream','Boolean']]).
metta_atom_asserted('&corelib',[iz,'with-output-to!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','with-output-to!',['@desc',"Evaluates expression with all outupt (e.g. from print) redirected to Stream. See also open! and close!."],['@params',[['@param',"Stream"],['@param',"Expression"]]],['@return',"Result of Expression"]]).
metta_atom_asserted('&corelib',[:,'with-output-to!',[->,'Stream','Expression','Atom']]).
metta_atom_asserted('&corelib',[iz,'load-file!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','load-file!',['@desc',"Loads the contents of Filename into Space, returning () if successful. Can load Python code (.py) or MeTTa (.metta); if ambiguous, assumes MeTTa. Like import! but favors MeTTa over Python when the file type is ambiguous."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_atom_asserted('&corelib',[:,'load-file!',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_atom_asserted('&corelib',[iz,'load-ascii','MeTTa']).
metta_atom_asserted('&corelib',['@doc','load-ascii',['@desc',"Loads the contents of Filename into Space, returning () if successful. Assumes the file is an ASCII file. Works like include!."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_atom_asserted('&corelib',[:,'load-ascii',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_atom_asserted('&corelib',[iz,'transfer!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','transfer!',['@desc',"Loads the contents of Filename into &self, as include. Returns () if successful, throws an exception otherwise."],['@params',[['@param',"Filename"]]],['@return',"Expression"]]).
metta_atom_asserted('&corelib',[:,'transfer!',[->,'String',[->]]]).
metta_atom_asserted('&corelib',[iz,'save-space!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','save-space!',['@desc',"Writes the contents of Space into Filename, returning () if successful."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_atom_asserted('&corelib',[:,'save-space!',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_atom_asserted('&corelib',[iz,'rtrace!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','rtrace!',['@desc',"Fully evaluates input Atom, providing a complete trace of the evaluation."],['@params',[['@param',"Atom to be evaluated"]]],['@return',"Result of evaluation"]]).
metta_atom_asserted('&corelib',[:,'rtrace!',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,rust,'MeTTaLog']).
metta_atom_asserted('&corelib',['@doc',rust,['@desc',"Interface with the rust / Hyperon MeTTa implementation. Enters Atom into rust atomspace. If Atom is evaluated (i.e. by being of the form !<atom>), returns the result of evaluation. See also rust!."],['@params',[['@param',"Atom to be entered into the space"]]],['@return',"Result of entering Atom into the space"]]).
metta_atom_asserted('&corelib',[:,rust,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'rust!','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','rust!',['@desc',"Like rust but evaluates the atom rather than entering into the space. (rust! <atom>) and (rust !<atom>) are identical."],['@params',[['@param',"Atom to be evaluated"]]],['@return',"Result of evaluation"]]).
metta_atom_asserted('&corelib',[:,'rust!',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,coerce,'MeTTaLog']).
metta_atom_asserted('&corelib',['@doc',coerce,['@desc',"Cast (coerce) Value to be of Type. Supports the basic types Atom (no-op), Number, String, and Bool.\n  Number: Converts number strings and bools to numbers. True is 1, False is 0.\n  String: Coerced as if Value were printed.\n  Bool: False, 0, and () are False, all other values are True."],['@params',[['@param',"Type"],['@param',"Value"]]],['@return',"Value cast to Type"]]).
metta_atom_asserted('&corelib',[:,coerce,[->,'Type','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,=,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',=,['@desc',"A symbol used to define reduction rules for expressions."],['@params',[['@param',"Pattern to be matched against expression to be reduced"],['@param',"Result of reduction or transformation of the first pattern"]]],['@return',"Not reduced itself unless custom equalities over equalities are added"]]).
%  ;(: = (-> Atom Atom Atom))
metta_atom_asserted('&corelib',[iz,'ErrorType','MeTTa']).
metta_atom_asserted('&corelib',['@doc','ErrorType',['@desc',"Type of the atom which contains error"]]).
metta_atom_asserted('&corelib',[:,'ErrorType','Type']).
metta_atom_asserted('&corelib',[iz,'Error','MeTTa']).
metta_atom_asserted('&corelib',['@doc','Error',['@desc',"Error constructor"],['@params',[['@param',"Atom which contains error"],['@param',"Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments"]]],['@return',"Instance of the error atom"]]).
metta_atom_asserted('&corelib',[:,'Error',[->,'Atom','Atom','ErrorType']]).
metta_atom_asserted('&corelib',[iz,return,'MinimalMeTTa']).
metta_atom_asserted('&corelib',['@doc',return,['@desc',"Returns value from the (function ...) expression"],['@params',[['@param',"Value to be returned"]]],['@return',"Passed argument"]]).
%  ; probably should be (: return (-> $t $t)) 
metta_atom_asserted('&corelib',[:,return,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,function,'MinimalMeTTa']).
metta_atom_asserted('&corelib',['@doc',function,['@desc',"Evaluates the argument until it becomes (return <result>). Then (function (return <result>)) is reduced to the <result>."],['@params',[['@param',"Atom to be evaluated"]]],['@return',"Result of atom's evaluation"]]).
metta_atom_asserted('&corelib',[:,function,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,eval,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',eval,['@desc',"Evaluates input Atom, performs one step of the evaluation. Empty results (Empty, ()) are removed from the result set. If no results are produced for a non-grounded function, eval returns NotReducible."],['@params',[['@param',"Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function"]]],['@return',"Result of evaluation"]]).
metta_atom_asserted('&corelib',[:,eval,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,eval,'MinimalMeTTa']).
metta_atom_asserted('&corelib',['@doc',evalc,['@desc',"Evaluates input atom, makes one step of the evaluation"],['@params',[['@param',"Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function"],['@param',"Space to evaluate atom in its context"]]],['@return',"Result of evaluation"]]).
metta_atom_asserted('&corelib',[:,evalc,[->,'Atom','Grounded','%Undefined%']]).
metta_atom_asserted('&corelib',[iz,chain,'MinimalMeTTa']).
metta_atom_asserted('&corelib',['@doc',chain,['@desc',"Evaluates first argument Atom, binds it to the Variable (second argument) and then evaluates third argument Template with Variable substituted in. When evaluation of the first Atom brings more than a single result, chain returns one instance of the Template expression for each result. The first argument Atom is only evaluated if it is part of the Minimal MeTTa specification; evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation)."],['@params',[['@param',"Atom to be evaluated"],['@param',"Variable"],['@param',"Template which will be evaluated at the end with Variable substituted"]]],['@return',"Result of evaluating third input argument"]]).
metta_atom_asserted('&corelib',[:,chain,[->,'Atom','Variable','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,unify,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',unify,['@desc',"Like Match but allows any sort of container for the first argument. (Match only allows MeTTa spaces.)"],['@params',[['@param',"The collection or space to match or the first atom to unify with"],['@param',"Second atom to unify with"],['@param',"Result if two atoms unified successfully"],['@param',"Result otherwise"]]],['@return',"Third argument when found or fourth one otherwise"]]).
metta_atom_asserted('&corelib',[:,unify,[->,'Atom','Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'if-unify','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',['@doc','if-unify',['@desc',"Matches two first terms and returns third argument if they are matched and fourth argument otherwise"],['@params',[['@param',"First term to unify with"],['@param',"Second atom to unify with"],['@param',"Result if two atoms unified successfully"],['@param',"Result otherwise"]]],['@return',"Third argument when first two atoms are unifiable or fourth one otherwise"]]).
metta_atom_asserted('&corelib',[:,'if-unify',[->,'Atom','Atom','Atom','Atom','%Undefined%']]).
metta_atom_asserted('&corelib',[iz,'if-unify-or-empty','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',['@doc','if-unify-or-empty',['@desc',"Attempts to unify two atoms and returns a result. Returns Empty if they cannot be unified."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"Unification result or Empty"]]).
metta_atom_asserted('&corelib',[:,'if-unify-or-empty',[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'cons-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','cons-atom',['@desc',"Constructs an expression using two arguments"],['@params',[['@param',"Head of an expression"],['@param',"Tail of an expression"]]],['@return',"New expression consisting of the two input arguments"]]).
metta_atom_asserted('&corelib',[:,'cons-atom',[->,'Atom','Expression','Expression']]).
%  ; AKA? (: cons (-> Atom Atom Atom))
metta_atom_asserted('&corelib',[iz,'decons-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','decons-atom',['@desc',"Works as a reverse to cons-atom function. It gets Expression as an input and returns it split into head and tail, e.g. (decons-atom (Cons X Nil)) -> (Cons (X Nil))"],['@params',[['@param',"Expression to be Deconsed"]]],['@return',"Deconsed expression"]]).
metta_atom_asserted('&corelib',[:,'decons-atom',[->,'Expression','Expression']]).
%  ; AKA? (: decons (-> Atom Atom))
metta_atom_asserted('&corelib',[iz,'min-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','min-atom',['@desc',"Returns atom with minimum value in the expression (first argument). Only numbers are allowed."],['@params',[['@param',"Expression containing atoms of Number type"]]],['@return',"Minimum value in the expression. Error if expression contains non-numeric value or is empty."]]).
metta_atom_asserted('&corelib',[:,'min-atom',[->,'Expression','Number']]).
metta_atom_asserted('&corelib',[iz,'max-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','max-atom',['@desc',"Returns atom with maximum value in the expression (first argument). Only numbers are allowed."],['@params',[['@param',"Expression containing atoms of Number type"]]],['@return',"Maximum value in the expression. Error if expression contains non-numeric value or is empty."]]).
metta_atom_asserted('&corelib',[:,'max-atom',[->,'Expression','Number']]).
metta_atom_asserted('&corelib',[iz,'size-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','size-atom',['@desc',"Returns the size of an expression (first argument)."],['@params',[['@param',"Expression to measure the size of"]]],['@return',"Size of the expression"]]).
metta_atom_asserted('&corelib',[:,'size-atom',[->,'Expression','Integer']]).
metta_atom_asserted('&corelib',['is-fn-1','size-atom',length]).
metta_atom_asserted('&corelib',[iz,'index-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','index-atom',['@desc',"Returns atom from an expression (first argument) using index (second argument) or error if index is out of bounds"],['@params',[['@param',"Expression"],['@param',"Index"]]],['@return',"Atom at the specified index in the expression. Error if index is out of bounds."]]).
metta_atom_asserted('&corelib',[:,'index-atom',[->,'Expression','Number','Atom']]).
metta_atom_asserted('&corelib',['is-fn-21','index-atom',nth0]).
%  ;; ==> (= (index-atom $L $N) (call-fn! nth0 $N $L))
metta_atom_asserted('&corelib',[iz,'pow-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','pow-math',['@desc',"Takes base (first argument) and power (second argument) and returns result of a power function (base ^ power)"],['@params',[['@param',"Base. Could be a float."],['@param',"Power."]]],['@return',"Result of base raised to the power"]]).
%  ; Define a variable using a function call to compute the power of a base number to an exponent
%  ; The result of the computation is stored in the variable `pow-math`
%  ; Assign the result of a function call to `pow-math`
metta_atom_asserted('&corelib',['is-fn-1','py-list',py_list]).
metta_atom_asserted('&corelib',[iz,'sqrt-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','sqrt-math',['@desc',"Returns square root for input number (first argument) which should be >= 0"],['@params',[['@param',"Input number"]]],['@return',"Result of a square root function"]]).
metta_atom_asserted('&corelib',['is-op-1','sqrt-math',sqrt]).
metta_atom_asserted('&corelib',[iz,'abs-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','abs-math',['@desc',"Returns absolute value of input number (first argument)"],['@params',[['@param',"Input number"]]],['@return',"Absolute value"]]).
metta_atom_asserted('&corelib',['is-op-1','abs-math',abs]).
metta_atom_asserted('&corelib',[iz,'log-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','log-math',['@desc',"Returns result of a logarithm function given base (first argument) and input number (second argument)"],['@params',[['@param',"Base"],['@param',"Input number"]]],['@return',"Result of log function"]]).
metta_atom_asserted('&corelib',['is-op-2','log-math',log2]).
metta_atom_asserted('&corelib',[iz,'trunc-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','trunc-math',['@desc',"Returns integer part of the input value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Integer part of float"]]).
metta_atom_asserted('&corelib',['is-op-1','trunc-math',trunc]).
metta_atom_asserted('&corelib',[iz,ceil,'MeTTa']).
metta_atom_asserted('&corelib',['@doc','ceil-math',['@desc',"Returns the smallest integer greater than or equal to the input value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Integer value greater than or equal to the input"]]).
metta_atom_asserted('&corelib',['is-op-1','ceil-math',ceil]).
metta_atom_asserted('&corelib',[iz,floor,'MeTTa']).
metta_atom_asserted('&corelib',['@doc','floor-math',['@desc',"Returns the smallest integer less than or equal to the input value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Integer value less than or equal to the input"]]).
metta_atom_asserted('&corelib',['is-op-1','floor-math',floor]).
metta_atom_asserted('&corelib',[iz,round,'MeTTa']).
metta_atom_asserted('&corelib',['@doc','round-math',['@desc',"Returns the nearest integer to the input float value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Nearest integer to the input"]]).
metta_atom_asserted('&corelib',['is-op-1',round,round]).
metta_atom_asserted('&corelib',[iz,sin,'MeTTa']).
metta_atom_asserted('&corelib',['@doc','sin-math',['@desc',"Returns result of the sine function for an input value in radians (first argument)"],['@params',[['@param',"Angle in radians"]]],['@return',"Result of the sine function"]]).
metta_atom_asserted('&corelib',['is-op-1','sin-math',sin]).
metta_atom_asserted('&corelib',[iz,'asin-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','asin-math',['@desc',"Returns result of the arcsine function for an input value (first argument)"],['@params',[['@param',"Float number"]]],['@return',"Result of the arcsine function"]]).
metta_atom_asserted('&corelib',['is-op-1','asin-math',asin]).
metta_atom_asserted('&corelib',[iz,'cos-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','cos-math',['@desc',"Returns result of the cosine function for an input value in radians (first argument)"],['@params',[['@param',"Angle in radians"]]],['@return',"Result of the cosine function"]]).
metta_atom_asserted('&corelib',['is-op-1','cos-math',cos]).
metta_atom_asserted('&corelib',[iz,'acos-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','acos-math',['@desc',"Returns result of the arccosine function for an input value (first argument)"],['@params',[['@param',"Float number"]]],['@return',"Result of the arccosine function"]]).
metta_atom_asserted('&corelib',['is-op-1','acos-math',acos]).
metta_atom_asserted('&corelib',[iz,'tan-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','tan-math',['@desc',"Returns result of the tangent function for an input value in radians (first argument)"],['@params',[['@param',"Angle in radians"]]],['@return',"Result of the tangent function"]]).
metta_atom_asserted('&corelib',['is-op-1','tan-math',tan]).
metta_atom_asserted('&corelib',[iz,'atan-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','atan-math',['@desc',"Returns result of the arctangent function for an input value (first argument)"],['@params',[['@param',"Float number"]]],['@return',"Result of the tangent function"]]).
metta_atom_asserted('&corelib',['is-op-1','atan-math',atan]).
metta_atom_asserted('&corelib',[iz,'isnan-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','isnan-math',['@desc',"Returns True if input value is NaN. False - otherwise"],['@params',[['@param',"Number"]]],['@return',"True/False"]]).
metta_atom_asserted('&corelib',['is-pred','isnan-math',is_NaN]).
metta_atom_asserted('&corelib',[iz,'isinf-math','MeTTa']).
metta_atom_asserted('&corelib',['@doc','isinf-math',['@desc',"Returns True if input value is positive or negative infinity. False - otherwise"],['@params',[['@param',"Number"]]],['@return',"True/False"]]).
%  ; this is deduced: (: isinf (-> Number Bool))
metta_atom_asserted('&corelib',['is-pred','isinf-math',is_Inf]).
metta_atom_asserted('&corelib',[:,'random-int',[->,'RandomGenerator','Number','Number','Number']]).
metta_atom_asserted('&corelib',[iz,'random-int','UseRust']).
metta_atom_asserted('&corelib',['@doc','random-int',['@desc',"Returns random int number from range defined by two numbers (second and third argument)"],['@params',[['@param',"Random number generator instance"],['@param',"Range start"],['@param',"Range end"]]],['@return',"Random int number from defined range"]]).
metta_atom_asserted('&corelib',[iz,'random-float','UseRust']).
metta_atom_asserted('&corelib',[:,'random-float',[->,'RandomGenerator','Number','Number','Number']]).
metta_atom_asserted('&corelib',['@doc','random-float',['@desc',"Returns random float number from range defined by two numbers (second and third argument)"],['@params',[['@param',"Random number generator instance"],['@param',"Range start"],['@param',"Range end"]]],['@return',"Random float number from defined range"]]).
metta_atom_asserted('&corelib',[iz,'set-random-seed','MeTTa']).
metta_atom_asserted('&corelib',[iz,'set-random-seed','UseRust']).
metta_atom_asserted('&corelib',[:,'set-random-seed',[->,'RandomGenerator','Number',[->]]]).
metta_atom_asserted('&corelib',['@doc','set-random-seed',['@desc',"Sets a new seed (second argument) for random number generator (first argument)"],['@params',[['@param',"Random number generator instance"],['@param',"Seed"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[iz,'new-random-generator','MeTTa']).
metta_atom_asserted('&corelib',[iz,'new-random-generator','UseRust']).
metta_atom_asserted('&corelib',[:,'new-random-generator',[->,'Number','RandomGenerator']]).
metta_atom_asserted('&corelib',['@doc','new-random-generator',['@desc',"Creates new random number generator instance using seed as input (first argument)"],['@params',[['@param',"Seed"]]],['@return',"Instance of random number generator"]]).
metta_atom_asserted('&corelib',[iz,'reset-random-generator','MeTTa']).
metta_atom_asserted('&corelib',[:,'reset-random-generator',[->,'RandomGenerator','RandomGenerator']]).
metta_atom_asserted('&corelib',['@doc','reset-random-generator',['@desc',"Resets instance of random number generator (first argument) to its default behavior (StdRng::from_os_rng())"],['@params',[['@param',"Random number generator instance"]]],['@return',"Random number generator instance with default behavior"]]).
%  ;; Some code still uses the old syntax
metta_atom_asserted('&corelib',[iz,'random-int','MeTTa']).
metta_atom_asserted('&corelib',['@doc','random-int',['@desc',"Returns random int number from range defined by two numbers (first and second argument)"],['@params',[['@param',"Range start"],['@param',"Range end"]]],['@return',"Random int number from defined range"]]).
metta_atom_asserted('&corelib',['is-op-2','random-int',random]).
metta_atom_asserted('&corelib',[iz,'random-float','MeTTa']).
metta_atom_asserted('&corelib',['@doc','random-float',['@desc',"Returns random float number from range defined by two numbers (first and second argument)"],['@params',[['@param',"Range start"],['@param',"Range end"]]],['@return',"Random float number from defined range"]]).
metta_atom_asserted('&corelib',['is-op-2','random-int',random]).
metta_atom_asserted('&corelib',[iz,'collapse-bind','MeTTa']).
metta_atom_asserted('&corelib',['@doc','collapse-bind',['@desc',"Evaluates the Atom (first argument) and returns an expression which contains all alternative evaluations in a form (Atom Bindings). Bindings are represented in a form of a grounded atom { <var> <- <binding>, ... }. See also the complement superpose-bind. Note that, like chain, collapse-bind only evaluates Minimal Metta expressions. Evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation)."],['@params',[['@param',"Minimal MeTTa operation to be evaluated"]]],['@return',"All alternative evaluations"]]).
%  ;; collapse-bind because `collapse` doesnt guarantee shared bindings
metta_atom_asserted('&corelib',[:,'collapse-bind',[->,'Atom','Expression']]).
metta_atom_asserted('&corelib',[iz,'superpose-bind','MeTTa']).
metta_atom_asserted('&corelib',['@doc','superpose-bind',['@desc',"Complement to the collapse-bind. It takes result of collapse-bind (first argument) and returns only result atoms without bindings. Primarily used with some filtering step on the collapse-bind results, i.e. collapse-bind -> <filter> -> superpose-bind."],['@params',[['@param',"Expression in form (Atom Binding)"]]],['@return',"Non-deterministic list of Atoms"]]).
%  ;; superpose-bind because `superpose` doesnt guarentee shared bindings
metta_atom_asserted('&corelib',[:,'superpose-bind',[->,'Expression','Atom']]).
%  ; Helper Minimal Metta?
metta_atom_asserted('&corelib',['@doc',metta,['@desc',"Run MeTTa interpreter on atom."],['@params',[['@param',"Atom to be interpreted"],['@param',"Type of input atom"],['@param',"Atomspace where intepretation should take place"]]],['@return',"Result of interpretation"]]).
metta_atom_asserted('&corelib',[:,metta,[->,'Atom','Type','Grounded','Atom']]).
metta_atom_asserted('&corelib',[iz,id,'MinimalMeTTa']).
metta_atom_asserted('&corelib',['@doc',id,['@desc',"Returns its argument"],['@params',[['@param',"Input argument"]]],['@return',"Input argument"]]).
metta_atom_asserted('&corelib',[:,id,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'atom-subst','MinimalMeTTa']).
metta_atom_asserted('&corelib',['@doc','atom-subst',['@desc',"Substitutes variable passed as a second argument in the third argument by the first argument"],['@params',[['@param',"Value to use for replacement"],['@param',"Variable to replace"],['@param',"Template to replace variable by the value"]]],['@return',"Template with substituted variable"]]).
metta_atom_asserted('&corelib',[:,'atom-subst',[->,'Atom','Variable','Atom','Atom']]).
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
%  ;(= (atom-subst $atom $var $templ)
%  ;  (function (chain (eval (id $atom)) $var (return $templ))) )
metta_atom_asserted('&corelib',[iz,'if-decons-expr','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',['@doc','if-decons-expr',['@desc',"Checks if first argument is non empty expression. If so gets tail and head from the first argument and returns forth argument using head and tail values. Returns fifth argument otherwise."],['@params',[['@param',"Expression to be deconstructed"],['@param',"Head variable"],['@param',"Tail variable"],['@param',"Template to return if first argument is a non-empty expression"],['@param',"Default value to return otherwise"]]],['@return',"Either template with head and tail replaced by values or default value"]]).
metta_atom_asserted('&corelib',[:,'if-decons-expr',[->,'Expression','Variable','Variable','Atom','Atom','Atom']]).
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
metta_atom_asserted('&corelib',[:,'if-decons',[->,'Expression','Variable','Variable','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'if-error','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',['@doc','if-error',['@desc',"Checks if first argument is an error atom. Returns second argument if so or third argument otherwise."],['@params',[['@param',"Atom to be checked for the error"],['@param',"Value to return if first argument is an error"],['@param',"Value to return otherwise"]]],['@return',"Second or third argument"]]).
metta_atom_asserted('&corelib',[:,'if-error',[->,'Atom','Atom','Atom','Atom']]).
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
metta_atom_asserted('&corelib',[iz,'return-on-error','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',['@doc','return-on-error',['@desc',"Returns first argument if it is Empty or an error. Returns second argument otherwise."],['@params',[['@param',"Previous evaluation result"],['@param',"Atom for further evaluation"]]],['@return',"Return previous result if it is an error or Empty or continue evaluation"]]).
metta_atom_asserted('&corelib',[:,'return-on-error',[->,'Atom','Atom','Atom']]).
%  ; Difference between `switch` and `case` is a way how they interpret `Empty`
%  ; result. `CaseOp` interprets first argument inside itself and then manually
%  ; checks whether result is empty. `switch` is interpreted in a context of
%  ; main interpreter. Minimal interpreter correctly passes `Empty` as an
%  ; argument to the `switch` but when `switch` is called from MeTTa interpreter
%  ; (for example user evaluates `!(switch (if-unify A B ok Empty) ...)` then
%  ; emptiness of the first argument is checked by interpreter and it will
%  ; break execution when `Empty` is returned.
metta_atom_asserted('&corelib',[iz,switch,'MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',['@doc',switch,['@desc',"Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)"],['@params',[['@param',"Atom to be matched with patterns"],['@param',"Tuple of pairs mapping condition patterns to results"]]],['@return',"Result which corresponds to the pattern which is matched with the passed atom first"]]).
metta_atom_asserted('&corelib',[:,switch,[->,'Atom','Atom','Atom']]).
%  ; BEGIN - Yes, Douglas turned this sourcecode form into a a Value with the type Comment
%  
%  (: (
%  (: switch (-> %Undefined% Expression Atom))
%  (= (switch $atom $cases)
%    (function (chain (decons-atom $cases) $list
%      (chain (eval (switch-internal $atom $list)) $res
%        (chain (eval (if-equal $res NotReducible Empty $res)) $x (return $x)) ))))
%  
%  (iz switch-internal HelperMM)
%  (@doc switch-internal
%    (@desc "This function is being called inside switch function to test one of the cases and it calls switch once again if current condition is not met")
%    (@params (
%      (@param "Atom (it will be evaluated)")
%      (@param "Deconsed tuple of pairs mapping condition patterns to results")))
%    (@return "Result of evaluating of Atom bound to met condition"))
%  (= (switch-internal $atom (($pattern $template) $tail))
%    (function (if-unify $atom $pattern
%      (return $template)
%      (chain (eval (switch $atom $tail)) $ret (return $ret)) )))
%  
%  ) Comment)  
%  
%  ; ENDOF - Yes, Douglas turned this sourcecode form into a a Value with the type Comment
metta_atom_asserted('&corelib',[iz,'is-function','MinimalMeTTaHelper']).
%  ; TODO: Type is used here, but there is no definition for the -> type
%  ; constructor for instance, thus in practice it matches because -> has
%  ; %Undefined% type. We need to assign proper type to -> and other type
%  ; constructors but it is not possible until we support vararg types.
metta_atom_asserted('&corelib',['@doc','is-function',['@desc',"Function checks if input type is a function type"],['@params',[['@param',"Type atom"]]],['@return',"True if type is a function type, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,'is-function',[->,'Type','Bool']]).
%  ;; This impl is old and maybe not sufficiant?
metta_atom_asserted('&corelib',[iz,'type-cast','MeTTa']).
metta_atom_asserted('&corelib',['@doc','type-cast',['@desc',"Casts atom passed as a first argument to the type passed as a second argument using space as a context"],['@params',[['@param',"Atom to be casted"],['@param',"Type to cast atom to"],['@param',"Context atomspace"]]],['@return',"Atom if casting is successful, (Error ... BadType) otherwise"]]).
metta_atom_asserted('&corelib',[:,'type-cast',[->,'Atom','Atom','Atom','Atom']]).
%  ;; This implementation is old and may not be sufficient.
metta_atom_asserted('&corelib',[iz,'match-types','MeTTa']).
metta_atom_asserted('&corelib',['@doc','match-types',['@desc',"Checks if two types can be unified and returns third argument if so, fourth - otherwise"],['@params',[['@param',"First type"],['@param',"Second type"],['@param',"Atom to be returned if types can be unified"],['@param',"Atom to be returned if types cannot be unified"]]],['@return',"Third or fourth argument"]]).
metta_atom_asserted('&corelib',[:,'match-types',[->,'Atom','Atom','Atom','Atom','Atom']]).
%  ; Helper MinimalMeTTa
metta_atom_asserted('&corelib',[iz,'first-from-pair','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',['@doc','first-from-pair',['@desc',"Gets a pair as a first argument and returns first atom from pair"],['@params',[['@param',"Pair"]]],['@return',"First atom from a pair"]]).
metta_atom_asserted('&corelib',[iz,'match-type-or','HelperMM']).
metta_atom_asserted('&corelib',['@doc','match-type-or',['@desc',"Checks if two types (second and third arguments) can be unified and returns result of OR operation between first argument and type checking result"],['@params',[['@param',"Boolean value"],['@param',"First type"],['@param',"Second type"]]],['@return',"True or False"]]).
metta_atom_asserted('&corelib',[iz,'filter-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','filter-atom',['@desc',"Function takes list of atoms (first argument), variable (second argument) and filter predicate (third argument) and returns list with items which passed filter. E.g. (filter-atom (1 2 3 4) $v (eval (> $v 2))) will give (3 4)"],['@params',[['@param',"List of atoms"],['@param',"Variable"],['@param',"Filter predicate"]]],['@return',"Filtered list"]]).
metta_atom_asserted('&corelib',[:,'filter-atom',[->,'Expression','Variable','Atom','Expression']]).
metta_atom_asserted('&corelib',[iz,'map-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','map-atom',['@desc',"Function takes list of atoms (first argument), variable to be used inside (second variable) and an expression which will be evaluated for each atom in list (third argument). Expression should contain variable. So e.g. (map-atom (1 2 3 4) $v (eval (+ $v 1))) will give (2 3 4 5)"],['@params',[['@param',"List of atoms"],['@param',"Variable name"],['@param',"Template using variable"]]],['@return',"Result of evaluating template for each atom in a list"]]).
metta_atom_asserted('&corelib',[:,'map-atom',[->,'Expression','Variable','Atom','Expression']]).
metta_atom_asserted('&corelib',[iz,'foldl-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','foldl-atom',['@desc',"Function takes list of values (first argument), initial value (second argument) and operation (fifth argument) and applies it consequently to the list of values, using init value as a start. It also takes two variables (third and fourth argument) to use them inside"],['@params',[['@param',"List of values"],['@param',"Init value"],['@param',"Variable"],['@param',"Variable"],['@param',"Operation"]]],['@return',"Result of applying operation to the list of values"]]).
metta_atom_asserted('&corelib',[:,'foldl-atom',[->,'Expression','Atom','Variable','Variable','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'separate-errors','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',[:,'separate-errors',[->,'Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',[iz,'check-alternatives','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',[iz,interpret,'MeTTa']).
metta_atom_asserted('&corelib',[iz,'interpret-expression','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',[iz,'interpret-func','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',[iz,'interpret-args','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',[iz,'interpret-args-tail','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',[iz,'interpret-tuple','MinimalMeTTaHelper']).
metta_atom_asserted('&corelib',[iz,'metta-call','MinimalMeTTaHelper']).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Standard library written in MeTTa ;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; TODO: Type is used here, but there is no definition for the -> type
%  ; constructor for instance, thus in practice it matches because -> has
%  ; %Undefined% type. We need to assign proper type to -> and other type
%  ; constructors but it is not possible until we support vararg types.
metta_atom_asserted('&corelib',[iz,'is-function-type','MeTTa']).
%  ; or Helper?
metta_atom_asserted('&corelib',['@doc','is-function-type',['@desc',"Function checks if input type is a function type"],['@params',[['@param',"Type notation"]]],['@return',"True if input type notation is a function type, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,'is-function-type',[->,'Type','Bool']]).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; MeTTa interpreter implementation ;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_atom_asserted('&corelib',[iz,if,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',if,['@desc',"Replace itself by one of the arguments depending on condition."],['@params',[['@param',"Boolean condition"],['@param',"Result when condition is True"],['@param',"Result when condition is False"]]],['@return',"Second or third argument"]]).
%  ;`$then`, `$else` should be of `Atom` type to avoid evaluation
%  ; and infinite cycle in inference
metta_atom_asserted('&corelib',[iz,or,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',or,['@desc',"Logical disjunction of two arguments"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"True if any of input arguments is True, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,or,[->,'Bool','LazyBool','Bool']]).
metta_atom_asserted('&corelib',['ALT=',[or,'False','False'],'False']).
metta_atom_asserted('&corelib',['ALT=',[or,'False','True'],'True']).
metta_atom_asserted('&corelib',['ALT=',[or,'True','False'],'True']).
metta_atom_asserted('&corelib',['ALT=',[or,'True','True'],'True']).
metta_atom_asserted('&corelib',[iz,and,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',and,['@desc',"Logical conjunction of two arguments"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"Returns True if both arguments are True, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,and,[->,'Bool','LazyBool','Bool']]).
metta_atom_asserted('&corelib',['ALT=',[and,'False','False'],'False']).
metta_atom_asserted('&corelib',['ALT=',[and,'False','True'],'False']).
metta_atom_asserted('&corelib',['ALT=',[and,'True','False'],'False']).
metta_atom_asserted('&corelib',['ALT=',[and,'True','True'],'True']).
metta_atom_asserted('&corelib',[iz,not,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',not,['@desc',"Logical negation"],['@params',[['@param',"Argument"]]],['@return',"Negates boolean input argument (False -> True, True -> False)"]]).
metta_atom_asserted('&corelib',[:,not,[->,'Bool','Bool']]).
metta_atom_asserted('&corelib',['ALT=',[not,'True'],'False']).
metta_atom_asserted('&corelib',['ALT=',[not,'False'],'True']).
metta_atom_asserted('&corelib',[iz,let,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',let,['@desc',"Let function is utilized to establish temporary variable bindings within an expression. It allows introducing variables (first argument), assign values to them (second argument), and then use these values within the scope of the let block"],['@params',[['@param',"Variable name (or several variables inside brackets ())"],['@param',"Expression to be bound to variable (it is being reduced before bind)"],['@param',"Expression which will be reduced and in which variable (first argument) could be used"]]],['@return',"Result of third argument's evaluation"]]).
metta_atom_asserted('&corelib',[:,let,[->,'Atom','%Undefined%','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'let*','MeTTa']).
metta_atom_asserted('&corelib',['@doc','let*',['@desc',"Same as let, but first argument is a tuple containing tuples of variables and their bindings, e.g. (($v (+ 1 2)) ($v2 (* 5 6)))"],['@params',[['@param',"Tuple of tuples with variables and their bindings"],['@param',"Expression which will be evaluated if each pair can be unified"]]],['@return',"Second argument or Empty"]]).
metta_atom_asserted('&corelib',[:,'let*',[->,'Expression','Atom','%Undefined%']]).
metta_atom_asserted('&corelib',[iz,'add-reduct','MeTTa']).
metta_atom_asserted('&corelib',['@doc','add-reduct',['@desc',"Prevents atom from being reduced"],['@params',[['@param',"Atom"]]],['@return',"Quoted atom"]]).
metta_atom_asserted('&corelib',['@doc','add-reduct-rust1',['@desc',"Adds atom into the atomspace reducing it first"],['@params',[['@param',"Atomspace to add atom into"],['@param',"Atom to add"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'add-reduct-rust1',[->,'hyperon::space::DynSpace','%Undefined%',[->]]]).
metta_atom_asserted('&corelib',[iz,'add-reduct','MeTTa']).
metta_atom_asserted('&corelib',[:,'add-reduct',[->,'Grounded','%Undefined%',[->]]]).
metta_atom_asserted('&corelib',[iz,stringToChars,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',stringToChars,['@desc',"Converts a string into a list expression of characters."],['@params',[['@param',"String to be converted."]]],['@return',"Expression representing the list of characters."]]).
metta_atom_asserted('&corelib',[:,stringToChars,[->,'String','Expression']]).
metta_atom_asserted('&corelib',[iz,charsToString,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',charsToString,['@desc',"Converts a list expression of characters into a string."],['@params',[['@param',"Expression representing the list of characters."]]],['@return',"Converted string."]]).
metta_atom_asserted('&corelib',[:,charsToString,[->,'Expression','String']]).
metta_atom_asserted('&corelib',[iz,parse,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',parse,['@desc',"Parses MeTTa code from a string and returns the corresponding atom."],['@params',[['@param',"String containing MeTTa code."]]],['@return',"Parsed atom."]]).
metta_atom_asserted('&corelib',[:,parse,[->,'String','Atom']]).
metta_atom_asserted('&corelib',[iz,repr,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',repr,['@desc',"Creates a string representation of an atom."],['@params',[['@param',"Atom to be represented as a string."]]],['@return',"String representation of the atom."]]).
metta_atom_asserted('&corelib',[:,repr,[->,'Atom','String']]).
metta_atom_asserted('&corelib',[iz,'car-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','car-atom',['@desc',"Extracts the first atom of an expression as a tuple"],['@params',[['@param',"Expression"]]],['@return',"First atom of an expression"]]).
metta_atom_asserted('&corelib',[:,'car-atom',[->,'Expression','Atom']]).
metta_atom_asserted('&corelib',[iz,'cdr-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','cdr-atom',['@desc',"Extracts the tail of an expression (all except first atom)"],['@params',[['@param',"Expression"]]],['@return',"Tail of an expression"]]).
metta_atom_asserted('&corelib',[:,'cdr-atom',[->,'Expression','Expression']]).
metta_atom_asserted('&corelib',[iz,quote,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',quote,['@desc',"Prevents atom from being reduced"],['@params',[['@param',"Atom"]]],['@return',"Quoted atom"]]).
metta_atom_asserted('&corelib',[:,quote,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,unquote,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',unquote,['@desc',"Unquotes quoted atom, e.g. (unquote (quote $x)) returns $x"],['@params',[['@param',"Quoted atom"]]],['@return',"Unquoted atom"]]).
metta_atom_asserted('&corelib',[:,unquote,[->,'%Undefined%','%Undefined%']]).
%  ; TODO: there is no way to define operation which consumes any number of
%  ; arguments  and returns unit
metta_atom_asserted('&corelib',[iz,nop,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',nop,['@desc',"Outputs unit atom"],['@params',[]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,nop,[->,'EmptyType']]).
metta_atom_asserted('&corelib',['ALT=',[nop],[]]).
metta_atom_asserted('&corelib',[iz,nop,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',nop,['@desc',"Outputs unit atom for any input"],['@params',[['@param',"Anything"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,nop,[->,'Atom',[->]]]).
metta_atom_asserted('&corelib',[iz,empty,'MeTTa']).
%  ; TODO: can be replaced by Empty atom and removed, kept for compatibility
metta_atom_asserted('&corelib',['@doc',empty,['@desc',"Cuts evaluation of the non-deterministic branch and removes it from the result"],['@params',[]],['@return',"Nothing"]]).
metta_atom_asserted('&corelib',[:,empty,[->,'%Undefined%']]).
metta_atom_asserted('&corelib',['ALT=',[empty],[let,a,b,'never-happens']]).
%  ;For testing
%  ;(= (empty) Empty)
%  ;(= (empty-rust1) (let a b never-happens))
%  ; TODO: MINIMAL added for compatibility, remove after migration
%  ;(= (empty-minimal) Empty)
metta_atom_asserted('&corelib',[iz,unique,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',unique,['@desc',"Function takes non-deterministic input (first argument) and returns only unique entities. E.g. (unique (superpose (a b c d d))) -> [a, b, c, d]"],['@params',[['@param',"Non-deterministic set of values"]]],['@return',"Unique non-deterministic values from input set"]]).
metta_atom_asserted('&corelib',[:,unique,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,union,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',union,['@desc',"Function takes two non-deterministic inputs (first and second argument) and returns their union. E.g. (union (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]"],['@params',[['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Union of sets"]]).
metta_atom_asserted('&corelib',[:,union,[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,intersection,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',intersection,['@desc',"Function takes two non-deterministic inputs (first and second argument) and returns their intersection. E.g. (intersection (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]"],['@params',[['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Intersection of sets"]]).
metta_atom_asserted('&corelib',[:,intersection,[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,subtraction,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',subtraction,['@desc',"Function takes two non-deterministic inputs (first and second argument) and returns their subtraction. E.g. !(subtraction (superpose (a b b c)) (superpose (b c c d))) -> [a, b]"],['@params',[['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Subtraction of sets"]]).
metta_atom_asserted('&corelib',[:,subtraction,[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',['@doc','add-reducts',['@desc',"Function takes space and expression, evaluates atoms in it and adds them into given space"],['@params',[['@param',"Space"],['@param',"Expression"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'add-reducts',[->,'Grounded','%Undefined%',[->]]]).
metta_atom_asserted('&corelib',['@doc','add-atoms',['@desc',"Function takes space and expression and adds atoms in Expression into given space without reducing them"],['@params',[['@param',"Space"],['@param',"Expression"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'add-atoms',[->,'Grounded','Expression',[->]]]).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Documentation formatting functions
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_atom_asserted('&corelib',[iz,'@doc','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@doc',['@desc',"Used for documentation purposes. Function documentation starts with @doc"],['@params',[['@param',"Function name"],['@param',"Function description. Starts with @desc"],['@param',"(Optional) parameters description starting with @params which should contain one or more @param symbols"],['@param',"(Optional) description of what function will return. Starts with @return"]]],['@return',"Function documentation using @doc-formal"]]).
metta_atom_asserted('&corelib',[:,'@doc',[->,'Atom','DocDescription','DocInformal']]).
metta_atom_asserted('&corelib',[:,'@doc',[->,'Atom','DocDescription','DocParameters','DocReturnInformal','DocInformal']]).
metta_atom_asserted('&corelib',[iz,'@doc','DataFunctor']).
metta_atom_asserted('&corelib',[iz,'@desc','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@desc',['@desc',"Used for documentation purposes. Description of function starts with @desc as a part of @doc"],['@params',[['@param',"String containing function description"]]],['@return',"Function description"]]).
metta_atom_asserted('&corelib',[:,'@desc',[->,'String','DocDescription']]).
metta_atom_asserted('&corelib',[iz,'@desc','DataFunctor']).
metta_atom_asserted('&corelib',[iz,'@param','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@param',['@desc',"Used for documentation purposes. Description of function parameter starts with @param as a part of @params which is a part of @doc"],['@params',[['@param',"String containing parameter description"]]],['@return',"Parameter description"]]).
metta_atom_asserted('&corelib',[:,'@param',[->,'String','DocParameterInformal']]).
metta_atom_asserted('&corelib',[:,'@param',[->,'DocType','DocDescription','DocParameter']]).
metta_atom_asserted('&corelib',[iz,'@param','DataFunctor']).
metta_atom_asserted('&corelib',[iz,'@return','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@return',['@desc',"Used for documentation purposes. Description of function return value starts with @return as a part of @doc"],['@params',[['@param',"String containing return value description"]]],['@return',"Return value description"]]).
metta_atom_asserted('&corelib',[:,'@return',[->,'String','DocReturnInformal']]).
metta_atom_asserted('&corelib',[:,'@return',[->,'DocType','DocDescription','DocReturn']]).
metta_atom_asserted('&corelib',[iz,'@return','DataFunctor']).
metta_atom_asserted('&corelib',[iz,'@doc-formal','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@doc-formal',['@desc',"Used for documentation purposes. get-doc returns documentation starting with @doc-formal symbol. @doc-formal contains 6 or 4 parameters depending on the entity being described (functions being described using 6 parameters, atoms - 4 parameters)"],['@params',[['@param',"Function/Atom name for which documentation is to be displayed. Format (@item name)"],['@param',"Contains (@kind function) or (@kind atom) depends on entity which documentation is displayed"],['@param',"Contains type notation of function/atom"],['@param',"Function/atom description"],['@param',"(Functions only). Description of function parameters"],['@param',"(Functions only). Description of function's return value"]]],['@return',"Expression containing full documentation on function"]]).
metta_atom_asserted('&corelib',[:,'@doc-formal',[->,'DocItem','DocKindFunction','DocType','DocDescription','DocParameters','DocReturn','DocFormal']]).
metta_atom_asserted('&corelib',[:,'@doc-formal',[->,'DocItem','DocKindAtom','DocType','DocDescription','DocFormal']]).
metta_atom_asserted('&corelib',[iz,'@doc-formal','DataFunctor']).
metta_atom_asserted('&corelib',[iz,'@item','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@item',['@desc',"Used for documentation purposes. Converts atom/function's name to DocItem"],['@params',[['@param',"Atom/Function name to be documented"]]],['@return',"(@item Atom) entity"]]).
metta_atom_asserted('&corelib',[:,'@item',[->,'Atom','DocItem']]).
metta_atom_asserted('&corelib',[iz,'@item','DataFunctor']).
%  ; TODO: help! gives two outputs
%  ;Atom (@kind function): (%Undefined% (-> Atom Atom)) Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
%  ;Atom (@kind function): DocKindFunction Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
metta_atom_asserted('&corelib',[iz,['@kind',function],'MeTTa']).
metta_atom_asserted('&corelib',['@doc',['@kind',function],['@desc',"Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case"]]).
metta_atom_asserted('&corelib',[:,['@kind',function],'DocKindFunction']).
metta_atom_asserted('&corelib',[iz,['@kind',function],'DataFunctor']).
metta_atom_asserted('&corelib',[iz,['@kind',atom],'MeTTa']).
metta_atom_asserted('&corelib',['@doc',['@kind',atom],['@desc',"Used for documentation purposes. Shows type of entity to be documented. (@kind atom) in this case"]]).
metta_atom_asserted('&corelib',[:,['@kind',atom],'DocKindAtom']).
metta_atom_asserted('&corelib',[iz,['@kind',atom],'DataFunctor']).
metta_atom_asserted('&corelib',[iz,'@type','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@type',['@desc',"Used for documentation purposes. Converts atom/function's type to DocType"],['@params',[['@param',"Atom/Function type to be documented"]]],['@return',"(@type Type) entity"]]).
metta_atom_asserted('&corelib',[:,'@type',[->,'Type','DocType']]).
metta_atom_asserted('&corelib',[iz,'@type','DataFunctor']).
metta_atom_asserted('&corelib',[iz,'@params','MeTTa']).
metta_atom_asserted('&corelib',['@doc','@params',['@desc',"Used for function documentation purposes. Contains several @param entities with description of each @param"],['@params',[['@param',"Several (@param ...) entities"]]],['@return',"DocParameters containing description of all parameters of function in form of (@params ((@param ...) (@param ...) ...))"]]).
metta_atom_asserted('&corelib',[:,'@params',[->,'Expression','DocParameters']]).
metta_atom_asserted('&corelib',[iz,'@params','DataFunctor']).
metta_atom_asserted('&corelib',[iz,'get-doc','MeTTa']).
metta_atom_asserted('&corelib',['@doc','get-doc',['@desc',"Returns documentation for the given Atom/Function"],['@params',[['@param',"Atom/Function name for which documentation is needed"]]],['@return',"Documentation for the given atom/function"]]).
metta_atom_asserted('&corelib',[:,'get-doc',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'get-doc-single-atom','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','get-doc-single-atom',['@desc',"Function used by get-doc to get documentation on either function or atom. It checks if input name is the name of function or atom and calls correspondent function"],['@params',[['@param',"Atom/Function name for which documentation is needed"]]],['@return',"Documentation for the given atom/function"]]).
metta_atom_asserted('&corelib',[:,'get-doc-single-atom',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'get-doc-function','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','get-doc-function',['@desc',"Function used by get-doc-single-atom to get documentation on a function. It returns documentation on a function if it exists or default documentation with no description otherwise"],['@params',[['@param',"Function name for which documentation is needed"],['@param',"Type notation for this function"]]],['@return',"Documentation for the given function"]]).
metta_atom_asserted('&corelib',[:,'get-doc-function',[->,'Atom','Type','Atom']]).
metta_atom_asserted('&corelib',[iz,'undefined-doc-function-type','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','undefined-doc-function-type',['@desc',"Function used by get-doc-single-atom in case of absence of function's type notation"],['@params',[['@param',"List of parameters for the function we want to get documentation for"]]],['@return',"List of %Undefined% number of which depends on input list size. So for two parameters function will return (%Undefined% %Undefined% %Undefined%)"]]).
metta_atom_asserted('&corelib',[:,'undefined-doc-function-type',[->,'Expression','Type']]).
metta_atom_asserted('&corelib',[iz,'get-doc-params','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','get-doc-params',['@desc',"Function used by get-doc-function to get function's parameters documentation (including return value)"],['@params',[['@param',"List of parameters in form of ((@param Description) (@param Description)...)"],['@param',"Return value's description in form of (@return Description)"],['@param',"Type notation without -> starting symbol e.g. (Atom Atom Atom)"]]],['@return',"United list of params and return value each augmented with its type. E.g. (((@param (@type Atom) (@desc Description)) (@param (@type Atom) (@desc Description2))) (@return (@type Atom) (@desc Description)))"]]).
metta_atom_asserted('&corelib',[:,'get-doc-params',[->,'Expression','Atom','Expression',['Expression','Atom']]]).
metta_atom_asserted('&corelib',[iz,'get-doc-atom','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','get-doc-atom',['@desc',"Function used by get-doc (in case of input type Expression) and get-doc-single-atom (in case input value is not a function) to get documentation on input value"],['@params',[['@param',"Atom's name to get documentation for"]]],['@return',"Documentation on input Atom"]]).
metta_atom_asserted('&corelib',[:,'get-doc-atom',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'help!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','help!',['@desc',"Function prints documentation for the input atom. Without parameters prints the list of the stdlib functions."],['@params',[['@param',"Input to get documentation for"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'help!',[->,'Atom',[->]]]).
metta_atom_asserted('&corelib',['@doc','help!',['@desc',"Without parameters prints the list of the stdlib functions."],['@params',[]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'help!',[->,[->]]]).
metta_atom_asserted('&corelib',[iz,'help-param!','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','help-param!',['@desc',"Function used by function help! to output parameters using println!"],['@params',[['@param',"Parameters list"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'help-param!',[->,'Atom',[->]]]).
metta_atom_asserted('&corelib',[iz,'for-each-in-atom','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','for-each-in-atom',['@desc',"Applies function passed as a second argument to each atom inside first argument"],['@params',[['@param',"Expression to each atom in which function will be applied"],['@param',"Function to apply"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'for-each-in-atom',[->,'Expression','Atom',[->]]]).
metta_atom_asserted('&corelib',[iz,'noreduce-eq','PrivateRelation']).
metta_atom_asserted('&corelib',['@doc','noreduce-eq',['@desc',"Checks equality of two atoms without reducing them"],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if not reduced atoms are equal, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,'noreduce-eq',[->,'Atom','Atom','Bool']]).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Grounded function's documentation
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_atom_asserted('&corelib',[iz,'add-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','add-atom',['@desc',"Adds atom into the atomspace without reducing it"],['@params',[['@param',"Atomspace to add atom into"],['@param',"Atom to add"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'add-atom',[->,'hyperon::space::DynSpace','Atom',[->]]]).
metta_atom_asserted('&corelib',[iz,'get-type','MeTTa']).
metta_atom_asserted('&corelib',['@doc','get-type',['@desc',"Returns type notation of input atom"],['@params',[['@param',"Atom to get type for"]]],['@return',"Type notation or %Undefined% if there is no type for input Atom"]]).
metta_atom_asserted('&corelib',[:,'get-type',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[:,'get-type',[->,'Atom','hyperon::space::DynSpace','Atom']]).
metta_atom_asserted('&corelib',[iz,'get-type-space','MeTTa']).
metta_atom_asserted('&corelib',['@doc','get-type-space',['@desc',"Returns type notation of input Atom (second argument) relative to a specified atomspace (first argument)"],['@params',[['@param',"Atomspace where type notation for input atom will be searched"],['@param',"Atom to get type for"]]],['@return',"Type notation or %Undefined% if there is no type for input Atom in provided atomspace"]]).
metta_atom_asserted('&corelib',[iz,'get-type-space','MeTTa']).
metta_atom_asserted('&corelib',[:,'get-type-space',[->,'hyperon::space::DynSpace','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'get-metatype','MeTTa']).
metta_atom_asserted('&corelib',['@doc','get-metatype',['@desc',"Returns metatype of the input atom"],['@params',[['@param',"Atom to get metatype for"]]],['@return',"Metatype of input atom"]]).
metta_atom_asserted('&corelib',[:,'get-metatype',[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'if-equal','MinimalMeTTa']).
metta_atom_asserted('&corelib',['@doc','if-equal',['@desc',"Checks if first two arguments are equal and evaluates third argument if equal, fourth argument - otherwise"],['@params',[['@param',"First argument"],['@param',"Second argument"],['@param',"Atom to be evaluated if arguments are equal"],['@param',"Atom to be evaluated if arguments are not equal"]]],['@return',"Evaluated third or fourth argument"]]).
metta_atom_asserted('&corelib',[iz,'new-space','MeTTa']).
metta_atom_asserted('&corelib',['@doc','new-space',['@desc',"Creates new Atomspace which could be used further in the program as a separate from &self Atomspace"],['@params',[]],['@return',"Reference to a new space"]]).
metta_atom_asserted('&corelib',[:,'new-space',[->,'hyperon::space::DynSpace']]).
metta_atom_asserted('&corelib',[iz,'remove-atom','MeTTa']).
metta_atom_asserted('&corelib',['@doc','remove-atom',['@desc',"Removes atom from the input Atomspace"],['@params',[['@param',"Reference to the space from which the Atom needs to be removed"],['@param',"Atom to be removed"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'remove-atom',[->,'hyperon::space::DynSpace','Atom',[->]]]).
metta_atom_asserted('&corelib',[iz,'get-atoms','MeTTa']).
metta_atom_asserted('&corelib',['@doc','get-atoms',['@desc',"Shows all atoms in the input Atomspace"],['@params',[['@param',"Reference to the space"]]],['@return',"List of all atoms in the input space"]]).
metta_atom_asserted('&corelib',[:,'get-atoms',[->,'hyperon::space::DynSpace','Atom']]).
metta_atom_asserted('&corelib',[iz,'new-state','MeTTa']).
metta_atom_asserted('&corelib',['@doc','new-state',['@desc',"Creates a new state atom wrapping its argument"],['@params',[['@param',"Atom to be wrapped"]]],['@return',"Returns (State $value) where $value is an argument to a new-state"]]).
metta_atom_asserted('&corelib',[iz,'change-state!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','change-state!',['@desc',"Changes input state's wrapped atom to another value (second argument). E.g. (change-state! (State 5) 6) -> (State 6)"],['@params',[['@param',"State created by new-state function"],['@param',"Atom which will replace wrapped atom in the input state"]]],['@return',"State with replaced wrapped atom"]]).
metta_atom_asserted('&corelib',[iz,'get-state','MeTTa']).
metta_atom_asserted('&corelib',['@doc','get-state',['@desc',"Gets a state as an argument and returns its wrapped atom. E.g. (get-state (State 5)) -> 5"],['@params',[['@param',"State"]]],['@return',"Atom wrapped by state"]]).
metta_atom_asserted('&corelib',[iz,match,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',match,['@desc',"Searches for all declared atoms corresponding to the given pattern (second argument) and produces the output pattern (third argument)"],['@params',[['@param',"Atomspace to search pattern"],['@param',"Pattern atom to be searched"],['@param',"Output template typically containing variables from the input pattern"]]],['@return',"If match was successful it outputs template (third argument) with filled variables (if any were present in pattern) using matched pattern (second argument). Empty - otherwise"]]).
metta_atom_asserted('&corelib',[:,match,[->,'Atom','Atom','Atom','%Undefined%']]).
%  ;(ALT= (match $space $pattern $template)
%  ;  (unify $space $pattern $template Empty))
metta_atom_asserted('&corelib',[iz,'register-module!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','register-module!',['@desc',"Takes a file system path (first argument) and loads the module into the runner"],['@params',[['@param',"File system path"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'register-module!',[->,'Atom',[->]]]).
metta_atom_asserted('&corelib',[iz,'mod-space!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','mod-space!',['@desc',"Returns the space of the module (first argument) and tries to load the module if it is not loaded into the module system"],['@params',[['@param',"Module name"]]],['@return',"Space name"]]).
metta_atom_asserted('&corelib',[:,'mod-space!',[->,'Atom','hyperon::space::DynSpace']]).
metta_atom_asserted('&corelib',[=,['mod-space!',self],'&self']).
metta_function_asserted('&corelib',['mod-space!',self],'&self').
metta_atom_asserted('&corelib',[=,['mod-space!',top],'&self']).
metta_function_asserted('&corelib',['mod-space!',top],'&self').
metta_atom_asserted('&corelib',[=,['mod-space!',corelib],'&corelib']).
metta_function_asserted('&corelib',['mod-space!',corelib],'&corelib').
metta_atom_asserted('&corelib',[=,['mod-space!',stdlib],'&stdlib']).
metta_function_asserted('&corelib',['mod-space!',stdlib],'&stdlib').
metta_atom_asserted('&corelib',[=,['mod-space!',catalog],'&catalog']).
metta_function_asserted('&corelib',['mod-space!',catalog],'&catalog').
metta_atom_asserted('&corelib',[iz,'print-mods!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','print-mods!',['@desc',"Prints all modules with their correspondent spaces"],['@params',[]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'print-mods!',[->,[->]]]).
metta_atom_asserted('&corelib',['@doc','=alpha',['@desc',"Checks alpha equality of two expressions"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"True if both expressions are alpha equal, False - otherwise"]]).
metta_atom_asserted('&corelib',[iz,assertEqual,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',assertEqual,['@desc',"Compares (sets of) results of evaluation of two expressions"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"Unit atom if both expression after evaluation is equal, error - otherwise"]]).
metta_atom_asserted('&corelib',[:,assertEqual,[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,assertNotEqual,'MeTTaLog']).
metta_atom_asserted('&corelib',['@doc',assertNotEqual,['@desc',"Compares (sets of) results of evaluation of two expressions"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"Unit atom if both expressions after evaluation are not equal, error - otherwise"]]).
metta_atom_asserted('&corelib',[:,assertNotEqual,[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,assertEqualToResult,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',assertEqualToResult,['@desc',"Same as assertEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation"],['@params',[['@param',"First expression (it will be evaluated)"],['@param',"Second expression (it won't be evaluated)"]]],['@return',"Unit atom if both expression after evaluation is equal, error - otherwise"]]).
metta_atom_asserted('&corelib',[:,assertEqualToResult,[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,assertAlphaEqual,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',assertAlphaEqual,['@desc',"Compares (sets of) results of evaluation of two expressions using alpha equality"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"Unit atom if both expressions after evaluation are alpha equal, error - otherwise"]]).
metta_atom_asserted('&corelib',[iz,assertNotEqualToResult,'MeTTaLog']).
metta_atom_asserted('&corelib',['@doc',assertNotEqualToResult,['@desc',"Same as assertNotEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation"],['@params',[['@param',"First expression (it will be evaluated)"],['@param',"Second expression (it won't be evaluated)"]]],['@return',"Unit atom if both expressions after evaluation are not equal, error - otherwise"]]).
metta_atom_asserted('&corelib',[:,assertNotEqualToResult,[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',['@doc',assertAlphaEqualToResult,['@desc',"Same as assertAlphaEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation"],['@params',[['@param',"First expression (it will be evaluated)"],['@param',"Second expression (it won't be evaluated)"]]],['@return',"Unit atom if both expressions after evaluation of the first argument are alpha equal, error - otherwise"]]).
metta_atom_asserted('&corelib',[iz,superpose,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',superpose,['@desc',"Turns a tuple (first argument) into a nondeterministic result"],['@params',[['@param',"Tuple to be converted"]]],['@return',"Argument converted to nondeterministic result"]]).
metta_atom_asserted('&corelib',[:,superpose,[->,'Expression','%Undefined%']]).
metta_atom_asserted('&corelib',[iz,collapse,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',collapse,['@desc',"Converts a nondeterministic result into a tuple"],['@params',[['@param',"Atom which will be evaluated"]]],['@return',"Tuple"]]).
metta_atom_asserted('&corelib',[:,collapse,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,case,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',case,['@desc',"Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)"],['@params',[['@param',"Atom (it will be evaluated)"],['@param',"Tuple of pairs mapping condition patterns to results"]]],['@return',"Result of evaluating Atom bound to met condition"]]).
metta_atom_asserted('&corelib',[:,case,[->,'Atom','Expression','Atom']]).
metta_atom_asserted('&corelib',[iz,capture,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',capture,['@desc',"Wraps an atom and captures the current space"],['@params',[['@param',"Function name which space needs to be captured"]]],['@return',"Function"]]).
metta_atom_asserted('&corelib',[:,capture,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,hyperpose,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',hyperpose,['@desc',"Turns a tuple (first argument) into a nondeterministic result, as superpose, but is explicitly concurrent. Each item of the tuple may be processed in parallel, depending on the number of threads available (which is the minimum of the tuple size and the number of cores available)."],['@params',[['@param',"Tuple to be converted"]]],['@return',"Argument converted to nondeterministic result"]]).
metta_atom_asserted('&corelib',[:,hyperpose,[->,'Expression','%Undefined%']]).
metta_atom_asserted('&corelib',[iz,sequential,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',sequential,['@desc',"Turns a tuple (first argument) into a nondeterministic result, as superpose, but evaluation order of the elements of the tuple is fixed left to right. In that sense the result order is deterministic iff evaluating the tuple elements is deterministic."],['@params',[['@param',"Tuple to be evaluated"]]],['@return',"Sequential results of the tuple's elements."]]).
metta_atom_asserted('&corelib',[:,sequential,[->,'Expression','%Undefined%']]).
metta_atom_asserted('&corelib',[iz,do,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',do,['@desc',"Completely evaluates form, returning nothing. Typically used for side-effects. A common pattern is (sequential ((do <side-effect-form>) <form-that-needs-side-effect>))."],['@params',[['@param',"Form"]]],['@return',"None"]]).
metta_atom_asserted('&corelib',[:,do,[->,'Expression','%Undefined%']]).
metta_atom_asserted('&corelib',[iz,'pragma!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','pragma!',['@desc',"Changes global key's (first argument) value to a new one (second argument)"],['@params',[['@param',"Key's name"],['@param',"New value"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'pragma!',[->,'Symbol','%Undefined%',[->]]]).
metta_atom_asserted('&corelib',[iz,'import!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','import!',['@desc',"Imports module using its relative path (second argument) and binds it to the token (first argument) which will represent imported atomspace. If first argument is &self then everything will be imported to current atomspace. The source is imported only the first time import! is called. Can load Python code (.py) or MeTTa (.metta); if ambiguous, assumes Python."],['@params',[['@param',"Symbol, which is turned into the token for accessing the imported module"],['@param',"Module name"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'import!',[->,'Atom','Atom',[->]]]).
metta_atom_asserted('&corelib',[iz,include,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',include,['@desc',"Works just like include! but with &self as a first argument. So everything from input file will be included in the current atomspace and evaluated"],['@params',[['@param',"Name of metta script to import"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,include,[->,'Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'include!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','include!',['@desc',"Everything from input file will be included in the current atomspace and evaluated, as if it were being evaluated at the REPL. Unlike import!, the source is evaluated every time include! is called."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_atom_asserted('&corelib',[:,'include!',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_atom_asserted('&corelib',[iz,'bind!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','bind!',['@desc',"Registers a new token which is replaced with an atom during the parsing of the rest of the program"],['@params',[['@param',"Token name"],['@param',"Atom, which is associated with the token after reduction"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'bind!',[->,'Symbol','%Undefined%',[->]]]).
metta_atom_asserted('&corelib',[iz,'trace!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','trace!',['@desc',"Prints its first argument and returns second. Both arguments will be evaluated before processing"],['@params',[['@param',"Atom to print"],['@param',"Atom to return"]]],['@return',"Evaluated second input"]]).
metta_atom_asserted('&corelib',[iz,'println!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','println!',['@desc',"Prints a line of text to the console"],['@params',[['@param',"Expression/atom to be printed out"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'println!',[->,'%Undefined%',[->]]]).
metta_atom_asserted('&corelib',[iz,'format-args','MeTTa']).
metta_atom_asserted('&corelib',['@doc','format-args',['@desc',"Fills {} symbols in the input expression with atoms from the second expression. E.g. (format-args (Probability of {} is {}%) (head 50)) gives [(Probability of head is 50%)]. Atoms in the second input value could be variables"],['@params',[['@param',"Expression with {} symbols to be replaced"],['@param',"Atoms to be placed inside expression instead of {}"]]],['@return',"Expression with replaced {} with atoms"]]).
metta_atom_asserted('&corelib',[:,'format-args',[->,'String','Atom','String']]).
metta_atom_asserted('&corelib',[iz,sealed,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',sealed,['@desc',"Replaces all occurrences of any var from var list (first argument) inside atom (second argument) by unique variable. Can be used to create a locally scoped variables"],['@params',[['@param',"Variable list e.g. ($x $y)"],['@param',"Atom which uses those variables"]]],['@return',"Second argument but with variables being replaced with unique variables"]]).
metta_atom_asserted('&corelib',[:,sealed,[->,'Expression','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'&self','MeTTa']).
metta_atom_asserted('&corelib',['@doc','&self',['@desc',"Returns reference to the current atomspace"],['@params',[]],['@return',"Reference to the current atomspace"]]).
%  ; TODO: help! not working for operations which are defined in both Python and
%  ; Rust standard library: +, -, *, /, %, <, >, <=, >=, ==
metta_atom_asserted('&corelib',[iz,+,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',+,['@desc',"Sums two numbers"],['@params',[['@param',"Addend"],['@param',"Augend"]]],['@return',"Sum"]]).
metta_atom_asserted('&corelib',[:,+,[->,'Number','Number','Number']]).
metta_atom_asserted('&corelib',[iz,-,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',-,['@desc',"Subtracts second argument from first one"],['@params',[['@param',"Minuend"],['@param',"Deductible"]]],['@return',"Difference"]]).
metta_atom_asserted('&corelib',[:,-,[->,'Number','Number','Number']]).
metta_atom_asserted('&corelib',[iz,*,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',*,['@desc',"Multiplies two numbers"],['@params',[['@param',"Multiplier"],['@param',"Multiplicand"]]],['@return',"Product"]]).
metta_atom_asserted('&corelib',[:,*,[->,'Number','Number','Number']]).
metta_atom_asserted('&corelib',[iz,/,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',/,['@desc',"Divides first argument by second one"],['@params',[['@param',"Dividend"],['@param',"Divisor"]]],['@return',"Fraction"]]).
metta_atom_asserted('&corelib',[:,/,[->,'Number','Number','Number']]).
metta_atom_asserted('&corelib',[iz,'%','MeTTa']).
metta_atom_asserted('&corelib',['@doc','%',['@desc',"Modulo operator. It returns remainder of dividing first argument by second argument"],['@params',[['@param',"Dividend"],['@param',"Divisor"]]],['@return',"Remainder"]]).
metta_atom_asserted('&corelib',[:,'%',[->,'Number','Number','Number']]).
metta_atom_asserted('&corelib',[iz,<,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',<,['@desc',"Less than. Checks if first argument is less than second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is less than second, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,<,[->,'Number','Number','Bool']]).
metta_atom_asserted('&corelib',[iz,>,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',>,['@desc',"Greater than. Checks if first argument is greater than second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is greater than second, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,>,[->,'Number','Number','Bool']]).
metta_atom_asserted('&corelib',[iz,<=,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',<=,['@desc',"Less than or equal. Checks if first argument is less than or equal to second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is less than or equal to second, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,<=,[->,'Number','Number','Bool']]).
metta_atom_asserted('&corelib',[iz,>=,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',>=,['@desc',"Greater than or equal. Checks if first argument is greater than or equal to second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is greater than or equal to second, False - otherwise"]]).
metta_atom_asserted('&corelib',[:,>=,[->,'Number','Number','Bool']]).
metta_atom_asserted('&corelib',[iz,==,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',==,['@desc',"Checks equality for two arguments of the same type"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"Returns True if two arguments are equal, False - otherwise. If arguments are of different type function returns Error currently"]]).
metta_atom_asserted('&corelib',[iz,xor,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',xor,['@desc',"Exclusive disjunction of two arguments"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"Return values are the same as logical disjunction, but when both arguments are True xor will return False"]]).
metta_atom_asserted('&corelib',[:,xor,[->,'Bool','Bool','Bool']]).
metta_atom_asserted('&corelib',[iz,flip,'MeTTa']).
metta_atom_asserted('&corelib',['@doc',flip,['@desc',"Produces random boolean value"],['@params',[]],['@return',"Returns uniformly distributed random boolean value"]]).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;              NON-DETERMINISTIC SET OPERATIONS WITH PREDICATES          ;;
%  ;;                                                                        ;;
%  ;; These functions extend basic set operations by introducing a comparator;;
%  ;; predicate as the first argument. The predicate determines how          ;;
%  ;; elements are compared for equality, unification, or identity.          ;;
%  ;;                                                                        ;;
%  ;; A few examples of comparator predicates:                              ;;
%  ;;   - =alpha       : Structural equivalence, allowing variable renaming  ;;
%  ;;   - =identical   : Strict structural identity, including bindings       ;;
%  ;;   - =will   : Checks if unification is possible in any way         ;;
%  ;;   - =u=       : Standard unification with side effects               ;;
%  ;;   - =references  : Ensures atoms refer to the same memory object        ;;
%  ;;                                                                        ;;
%  ;; Functions:                                                             ;;
%  ;;   - `unique-by`       : Removes duplicates using a custom predicate  ;;
%  ;;   - `union-by`        : Merges two sets with predicate-based checks  ;;
%  ;;   - `intersection-by` : Computes intersection using a predicate      ;;
%  ;;   - `subtraction-by`  : Subtracts elements based on a predicate      ;;
%  ;;                                                                        ;;
%  ;; These functions handle **non-deterministic data**, using `superpose`.  ;;
%  ;;                                                                        ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_atom_asserted('&corelib',[iz,'unique-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','unique-by',['@desc',"Function takes a comparator predicate and a non-deterministic input (second argument) and returns only unique entities according to the predicate. \n  E.g. (unique-by =alpha (superpose (a b c d d))) -> [a, b, c, d]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"]]],['@return',"Unique non-deterministic values from input set based on predicate"]]).
metta_atom_asserted('&corelib',[:,'unique-by',[->,'Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'union-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','union-by',['@desc',"Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their union. \n  E.g. (union-by =u= (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Union of sets based on predicate"]]).
metta_atom_asserted('&corelib',[:,'union-by',[->,'Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'intersection-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','intersection-by',['@desc',"Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their intersection. \n  E.g. (intersection-by =will (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Intersection of sets based on predicate"]]).
metta_atom_asserted('&corelib',[:,'intersection-by',[->,'Atom','Atom','Atom','Atom']]).
metta_atom_asserted('&corelib',[iz,'subtraction-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','subtraction-by',['@desc',"Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their subtraction. \n  E.g. !(subtraction-by =identical (superpose (a b b c)) (superpose (b c c d))) -> [a, b]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Subtraction of sets based on predicate"]]).
metta_atom_asserted('&corelib',[:,'subtraction-by',[->,'Atom','Atom','Atom','Atom']]).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;                    UNIFICATION AND EQUIVALENCE PREDICATES              ;;
%  ;;                                                                        ;;
%  ;; These predicates determine whether two atoms can unify or are          ;;
%  ;; structurally equivalent. Some predicates actively perform unification, ;;
%  ;; while others only check whether unification *could* occur.             ;;
%  ;;                                                                        ;;
%  ;; **Theoretical Equivalence & Unification Predicates (Check Possibility Only):** ;;
%  ;;   - `=alpha`        : Checks if two atoms are structurally equivalent, ;;
%  ;;                       allowing renaming, but does **not** unify them.  ;;
%  ;;   - `=will`         : Checks if two atoms *could* unify in theory, but ;;
%  ;;                       does **not** modify bindings or unify variables. ;;
%  ;;                                                                        ;;
%  ;; **Unification Predicates (Perform Binding):**                          ;;
%  ;;   - `=u=`        : **Performs** unification and binds variables.    ;;
%  ;;   - `=alpha-unify`  : **Performs** unification with variable renaming. ;;
%  ;;                                                                        ;;
%  ;; **Strict Identity & Reference Predicates:**                            ;;
%  ;;   - `=identical`    : Checks if two atoms are strictly identical,      ;;
%  ;;                       including variable names and bindings.           ;;
%  ;;   - `=references`   : Checks if two atoms reference the **same**       ;;
%  ;;                       underlying entity in memory.                     ;;
%  ;;                                                                        ;;
%  ;; Predicates in the "Perform Binding" category actively modify variable  ;;
%  ;; bindings when successful. The "Check Possibility Only" predicates do   ;;
%  ;; **not** modify bindings; they merely verify whether equivalence or     ;;
%  ;; unification might be possible.                                         ;;
%  ;;                                                                        ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_atom_asserted('&corelib',[iz,'=alpha','MeTTa']).
metta_atom_asserted('&corelib',['@doc','=alpha',['@desc',"Predicate that **checks** if two atoms are structurally equivalent, allowing variable renaming.\n  Unlike `=alpha-unify`, this predicate does **not** perform unification or modify variables—it only verifies whether the two atoms *could* be equivalent with renaming."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms are structurally equivalent with renaming, false otherwise."]]).
metta_atom_asserted('&corelib',[:,'=alpha',[->,'Atom','Atom','Bool']]).
metta_atom_asserted('&corelib',[iz,'=will','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','=will',['@desc',"Predicate that **checks** if two atoms *could* unify in theory but does **not** perform unification.\n  Unlike `=u=`, this only verifies whether unification is **possible** without actually binding variables."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if unification is theoretically possible, false otherwise."]]).
metta_atom_asserted('&corelib',[:,'=will',[->,'Atom','Atom','Bool']]).
metta_atom_asserted('&corelib',[iz,'=u=','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','=u=',['@desc',"Predicate that determines if two atoms can be unified under standard unification rules, meaning they can be made identical by substituting variables appropriately. \n  We do **not** use `==` because `(== $x $y)` would be untrue and have **no side effects**, whereas `=u=` performs unification where possible."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms can be unified, false otherwise."]]).
metta_atom_asserted('&corelib',[:,'=u=',[->,'Atom','Atom','Bool']]).
metta_atom_asserted('&corelib',[iz,'=alpha-unify','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','=alpha-unify',['@desc',"Predicate that **performs** unification while considering alpha-equivalence. \n  It allows variable renaming where necessary to achieve successful unification."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms unify successfully, considering alpha-equivalence."]]).
metta_atom_asserted('&corelib',[:,'=alpha-unify',[->,'Atom','Atom','Bool']]).
metta_atom_asserted('&corelib',[iz,'=identical','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','=identical',['@desc',"Predicate that determines if two atoms are completely identical in structure, including variable names and their bindings. \n  This is a stricter comparison than `=alpha` or `=u=`, ensuring that variables and their values match exactly."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms are strictly identical, false otherwise."]]).
metta_atom_asserted('&corelib',[:,'=identical',[->,'Atom','Atom','Bool']]).
metta_atom_asserted('&corelib',[iz,'=references','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','=references',['@desc',"Predicate that determines if two atoms reference the **same** underlying entity in memory. \n  This is the strictest form of equality, ensuring the atoms are not just identical in structure but are literally the **same instance**."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms reference the same object, false otherwise."]]).
metta_atom_asserted('&corelib',[:,'=references',[->,'Atom','Atom','Bool']]).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;                   TUPLE-BASED SET OPERATIONS WITH PREDICATES           ;;
%  ;;                                                                        ;;
%  ;; These functions extend tuple-based set operations by introducing a     ;;
%  ;; comparator predicate as the first argument. This allows elements to be ;;
%  ;; compared using unification, alpha equivalence, or strict identity.     ;;
%  ;;                                                                        ;;
%  ;; A few examples of comparator predicates:                              ;;
%  ;;   - =alpha       : Structural equivalence, allowing variable renaming  ;;
%  ;;   - =identical   : Strict structural identity, including bindings       ;;
%  ;;   - =will   : Checks if unification is possible in any way         ;;
%  ;;   - =u=       : Standard unification with side effects               ;;
%  ;;   - =references  : Ensures atoms refer to the same memory object        ;;
%  ;;                                                                        ;;
%  ;; Functions:                                                             ;;
%  ;;   - `unique-atom-by`       : Removes duplicates using a predicate    ;;
%  ;;   - `union-atom-by`        : Merges two tuples with predicate-based  ;;
%  ;;   - `intersection-atom-by` : Computes intersection using a predicate ;;
%  ;;   - `subtraction-atom-by`  : Subtracts elements based on a predicate ;;
%  ;;                                                                        ;;
%  ;; These functions handle **tuple-based data**, using `superpose` where   ;;
%  ;; necessary to ensure compatibility with non-deterministic operations.   ;;
%  ;;                                                                        ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_atom_asserted('&corelib',[iz,'unique-atom-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','unique-atom-by',['@desc',"Function takes a comparator predicate and a tuple and returns only unique entities according to the predicate. \n  E.g. (unique-atom-by =alpha (a b c d d)) -> (a b c d)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"]]],['@return',"Unique values from input set based on predicate"]]).
metta_atom_asserted('&corelib',[:,'unique-atom-by',[->,'Atom','Expression','Expression']]).
metta_atom_asserted('&corelib',[iz,'union-atom-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','union-atom-by',['@desc',"Function takes a comparator predicate and two tuples and returns their union. \n  E.g. (union-atom-by =u= (a b b c) (b c c d)) -> (a b b c b c c d)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"],['@param',"List of values"]]],['@return',"Union of sets based on predicate"]]).
metta_atom_asserted('&corelib',[:,'union-atom-by',[->,'Atom','Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',[iz,'intersection-atom-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','intersection-atom-by',['@desc',"Function takes a comparator predicate and two tuples and returns their intersection. \n  E.g. (intersection-atom-by =will (a b c c) (b c c c d)) -> (b c c)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"],['@param',"List of values"]]],['@return',"Intersection of sets based on predicate"]]).
metta_atom_asserted('&corelib',[:,'intersection-atom-by',[->,'Atom','Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',[iz,'subtraction-atom-by','MeTTaLog']).
metta_atom_asserted('&corelib',['@doc','subtraction-atom-by',['@desc',"Function takes a comparator predicate and two tuples and returns their subtraction. \n  E.g. (subtraction-atom-by =identical (a b b c) (b c c d)) -> (a b)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"],['@param',"List of values"]]],['@return',"Subtraction of sets based on predicate"]]).
metta_atom_asserted('&corelib',[:,'subtraction-atom-by',[->,'Atom','Expression','Expression','Expression']]).
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ;;                        TUPLE-BASED SET OPERATIONS                      ;;
%  ;;                                                                        ;;
%  ;; These functions operate on **tuples** instead of non-deterministic     ;;
%  ;; sets. They provide the same core operations but assume that inputs     ;;
%  ;; are lists of values rather than `superpose` expressions.               ;;
%  ;;                                                                        ;;
%  ;; Functions:                                                             ;;
%  ;;   - `unique-atom`        : Removes duplicate values in a tuple         ;;
%  ;;   - `union-atom`         : Merges two tuples, preserving duplicates    ;;
%  ;;   - `intersection-atom`  : Returns values found in both tuples         ;;
%  ;;   - `subtraction-atom`   : Removes elements found in the second tuple  ;;
%  ;;                                                                        ;;
%  ;; These functions assume strict equality comparisons.                    ;;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_atom_asserted('&corelib',['@doc','unique-atom',['@desc',"Function takes tuple and returns only unique entities. E.g. (unique-atom (a b c d d)) -> (a b c d)"],['@params',[['@param',"List of values"]]],['@return',"Unique values from input set"]]).
metta_atom_asserted('&corelib',[:,'unique-atom',[->,'Expression','Expression']]).
metta_atom_asserted('&corelib',['@doc','union-atom',['@desc',"Function takes two tuples and returns their union. E.g. (union-atom (a b b c) (b c c d)) -> (a b b c b c c d)"],['@params',[['@param',"List of values"],['@param',"List of values"]]],['@return',"Union of sets"]]).
metta_atom_asserted('&corelib',[:,'union-atom',[->,'Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',['@doc','intersection-atom',['@desc',"Function takes two tuples and returns their intersection. E.g. (intersection-atom (a b c c) (b c c c d)) -> (b c c)"],['@params',[['@param',"List of values"],['@param',"List of values"]]],['@return',"Intersection of sets"]]).
metta_atom_asserted('&corelib',[:,'intersection-atom',[->,'Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',['@doc','subtraction-atom',['@desc',"Function takes two tuples and returns their subtraction. E.g. !(subtraction-atom (a b b c) (b c c d)) -> (a b)"],['@params',[['@param',"List of values"],['@param',"List of values"]]],['@return',"Subtraction of sets"]]).
metta_atom_asserted('&corelib',[:,'subtraction-atom',[->,'Expression','Expression','Expression']]).
metta_atom_asserted('&corelib',[iz,'git-module!','MeTTa']).
metta_atom_asserted('&corelib',['@doc','git-module!',['@desc',"Provides access to module in a remote git repo, from within MeTTa code. Similar to `register-module!`, this op will bypass the catalog search"],['@params',[['@param',"URL to github repo"]]],['@return',"Unit atom"]]).
metta_atom_asserted('&corelib',[:,'git-module!',[->,'Atom',[->]]]).
%% Finished generating /home/deb12user/metta-wam/.Attic/vnamed/stdlib_mettalog.metta at 2025-06-17T11:44:24-07:00

:- normal_IO.
:- initialization(transpiled_main, program).
