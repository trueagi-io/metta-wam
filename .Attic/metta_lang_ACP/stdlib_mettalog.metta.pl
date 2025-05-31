%% Generated from /home/deb12user/metta-wam/prolog/metta_lang_ACP/stdlib_mettalog.metta at 2025-05-24T14:34:56-07:00
:- set_prolog_flag(mettalog_rt,true).
%:- set_prolog_flag(mettalog_rt_args, ['--repl=false']).
%:- set_prolog_flag(mettalog_rt_args, ['--repl']).
:- include(library(metta_lang/metta_transpiled_header)).
%:- ensure_loaded(library(metta_lang/metta_interp)).
:- ensure_loaded(library(metta_rt)). % avoids starting the REPL
:- style_check(-discontiguous).
:- style_check(-singleton).

%  ;; Type Declarations with Documentation
metta_other_asserted('&corelib',['is-mettalog']).
metta_other_asserted('&corelib',['@doc','Any',['@desc',"The universal type; any value belongs to this type."]]).
metta_other_asserted('&corelib',[:,'Any','Type']).
metta_other_asserted('&corelib',[:,'RandomGenerator','Type']).
metta_other_asserted('&corelib',['@doc','RandomGenerator',['@desc',"Type representing a random number generator."]]).
metta_other_asserted('&corelib',['@doc','Atom',['@desc',"Type representing any atom."]]).
metta_other_asserted('&corelib',[:,'Atom','Type']).
metta_other_asserted('&corelib',['@doc','LazyEvaluatable',['@desc',"A type of Atom/Value that hyperon does not implicitly evaluate"]]).
metta_other_asserted('&corelib',[:,'LazyEvaluatable','Type']).
metta_other_asserted('&corelib',[:>,'Atom','LazyEvaluatable']).
metta_other_asserted('&corelib',['@doc','Bool',['@desc',"Boolean type of True or False."]]).
metta_other_asserted('&corelib',[:,'Bool','Type']).
metta_other_asserted('&corelib',['@doc','LazyBool',['@desc',"A LazyEvaluatable that when evaluated returns True or False."]]).
metta_other_asserted('&corelib',[:,'LazyBool','Type']).
metta_other_asserted('&corelib',[:>,'LazyBool','LazyEvaluatable']).
metta_other_asserted('&corelib',['@doc','Expression',['@desc',"Type representing an S-Expression, which is a combination of atoms."]]).
metta_other_asserted('&corelib',[:,'Expression','Type']).
metta_other_asserted('&corelib',[:>,'Expression','LazyEvaluatable']).
metta_other_asserted('&corelib',['@doc','Number',['@desc',"Numeric type, including integers and floating-point numbers."]]).
metta_other_asserted('&corelib',[:,'Number','Type']).
metta_other_asserted('&corelib',['@doc','hyperon::space::DynSpace',['@desc',"Dynamic space type, representing an Atomspace."]]).
metta_other_asserted('&corelib',[:,'hyperon::space::DynSpace','Type']).
metta_other_asserted('&corelib',['@doc','ReturnType',['@desc',"Type representing a function's return value."]]).
metta_other_asserted('&corelib',[:,'ReturnType','Type']).
metta_other_asserted('&corelib',['@doc','Symbol',['@desc',"Type representing a symbol or identifier."]]).
metta_other_asserted('&corelib',[:,'Symbol','Type']).
metta_other_asserted('&corelib',['@doc','StateMonad',['@desc',"Type representing a state monad, used for encapsulating stateful computations."]]).
metta_other_asserted('&corelib',[:,'StateMonad','Type']).
metta_other_asserted('&corelib',['@doc','Type',['@desc',"Type representing a type."]]).
metta_other_asserted('&corelib',[:,'Type','Type']).
metta_other_asserted('&corelib',['@doc','True',['@desc',"Boolean value representing truth."]]).
metta_other_asserted('&corelib',[:,'True','Bool']).
metta_other_asserted('&corelib',['@doc','False',['@desc',"Boolean value representing falsehood."]]).
metta_other_asserted('&corelib',[:,'False','Bool']).
metta_other_asserted('&corelib',['@doc','%Undefined%',['@desc',"Special type representing an undefined value or type."]]).
metta_other_asserted('&corelib',[:,'%Undefined%','Type']).
metta_other_asserted('&corelib',['@doc','Variable',['@desc',"Type representing a variable in the language."]]).
metta_other_asserted('&corelib',[:,'Variable','Type']).
metta_other_asserted('&corelib',['@doc',:,['@desc',"Type declarion operator"]]).
metta_other_asserted('&corelib',['@doc',<:,['@desc',"Super Type declarion operator"]]).
metta_other_asserted('&corelib',[:,:,'%Undefined%']).
%  ; match hyperons weirdd return value of !(get-type :)
metta_other_asserted('&corelib',[:,'if-empty',[->,'Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[:,'if-non-empty-expression',[->,'Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[:,'if-not-reducible',[->,'Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[:,return,[->,'Atom','ReturnType']]).
metta_other_asserted('&corelib',[:,switch,[->,'%Undefined%','Expression','Atom']]).
metta_other_asserted('&corelib',[:,unify,[->,'Atom','Atom','Atom','Atom','%Undefined%']]).
%  ;(: current-predicate-arity (-> Predicate Number)) ; returns only the first type of a symbol
metta_other_asserted('&corelib',[:,'get-vtype',[->,'Atom','Atom']]).
%  ; returns only the first type of a symbol
metta_other_asserted('&corelib',[:,'get-vtype',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the first type of a symbol
metta_other_asserted('&corelib',[:,'get-types',[->,'Atom','Atom']]).
%  ; returns only the first type of a symbol
metta_other_asserted('&corelib',[:,'get-types',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the first type of a symbol
metta_other_asserted('&corelib',[:,'get-dtype',[->,'Atom','Atom']]).
%  ; returns only the data value types of a symbol
metta_other_asserted('&corelib',[:,'get-dtype',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the data value types of a symbol
metta_other_asserted('&corelib',[:,'get-ftype',[->,'Atom','Atom']]).
%  ; returns only the function types of a symbol
metta_other_asserted('&corelib',[:,'get-ftype',[->,'Atom','hyperon::space::DynSpace','Atom']]).
%  ; returns only the function types of a symbol
metta_other_asserted('&corelib',[:,'pragma!',[->,'Atom','Atom',[->]]]).
%  ;(: = (-> Atom Atom %Undefined%))
metta_other_asserted('&corelib',[:,match,[->,'hyperon::space::DynSpace','Atom','Atom','%Undefined%']]).
%  ;(: case (-> Expression Atom Atom))
metta_other_asserted('&corelib',[:,combine,[->,_t,_t,_t]]).
metta_other_asserted('&corelib',[:,'import!',[->,'hyperon::space::DynSpace','Atom',[->]]]).
metta_other_asserted('&corelib',['@doc','type-check',['@desc',"The value of type-check determines MeTTa's type-checking behavior. Set via pragma!. When set to auto (i.e. !(pragma! type-check auto)), types are checked immediately on adding an expression to the space. By default, when unset (or set to anything other than auto), types are checked only on evaluation. For example !(+ 1 \"2\") would trigger a type violation, but (= (foo $x) (+ $x \"2\")) would not, unless type-check is set to auto, in which case both would trigger type violations."]]).
metta_other_asserted('&corelib',[:,'type-check','Symbol']).
metta_other_asserted('&corelib',[iz,'predicate-arity','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','predicate-arity',['@desc',"Specifies the arity (number of arguments) for a given predicate, allowing it to be queriable in the system's match framework. This is particularly useful for enabling built-in functions, such as `size-atom`, to be used as predicates in declarative contexts and run in reverse to compute inputs based on outputs.\n\n\nFor example:\n  ; Enable the built-in function `size-atom` that takes an atom and returns the size\n    as a predicate with arity 2\n  (predicate-arity size-atom 2)\n\n\n  ; Now `size-atom` can be used as a predicate in pattern matching\n  !(match &dyn-space '(size-atom (a b c) $size) \n         (The abc tuple was len $size))\n  ; This pattern will resolve `Size = 3` and execute the action.\n\n\nAdditionally, by running `size-atom` in reverse, you can compute a new atom based on a desired size:\n  !(match &dyn-space '(size-atom $new-atom 4) \n         (The new atom is $new-atom))\n  ; This resolves `$new-atom` to a tuple of size 4, such as ($1 $2 $3 $4).\n\n\nThis reverse functionality is made possible because predicates can describe relationships, allowing you to infer inputs from outputs."],['@params',[['@param',"Predicate symbol","The name of the predicate whose arity is being defined."]]],['@return',"The number of arguments required for the predicate."]]).
metta_other_asserted('&corelib',[:,'predicate-arity',[->,'Symbol','Number']]).
metta_other_asserted('&corelib',['predicate-arity','predicate-arity',2]).
metta_other_asserted('&corelib',['function-arity','predicate-arity',1]).
metta_other_asserted('&corelib',['@doc','function-arity',['@desc',"Defines the arity of a function, allowing predicates or built-in facts to also behave as callable functions. This enables procedural-style execution where the last argument of the predicate becomes the function's return value, and the system internally resolves the function using a `match` query. \n\n\nFor example:\n  ; Declare the built-in predicate `max` with arity 3\n  (predicate-arity max 3)\n\n  ; Enable `max` as a function\n  (function-arity max 2)\n\n\n  ; Define the rules for `max`\n  (= (max $X $Y $Y) (<= $X $Y))\n  (= (max $X $Y $X) (> $X $Y))\n\n\n  ; Using `max` declaratively as a predicate\n  !(match &self (max (5 10) $max)\n         (The maximum is $max))\n  [(The maximum is 10)]\n\n\n  ; Using `max` procedurally as a function\n  !(max 5 10)\n  [10]\n  \n\n\n  ; Reverse execution with `max`\n  !(let True (== (max $a $b) 10) ($a $b)) ; as a function\n  [(#(exists $a (=< $a 10)) 10), (10 #(exists $b (=< 10 $b )))]\n  !(match &self (max $a $b 10) ($a $b)) ; or as a predicate\n  [(#(exists $a (=< $a 10)) 10), (10 #(exists $b (=< 10 $b )))]\n\n\n  This dual behavior allows predicates to act as functions, bridging procedural and declarative paradigms. By defining `function-arity`, the function automatically resolves using the logic of the associated predicate."],['@params',[['@param',"Function symbol","The name of the function or predicate to enable as a callable function."]]],['@return',"The number of arguments expected by the function."]]).
metta_other_asserted('&corelib',[:,'function-arity',[->,'Symbol','Number']]).
metta_other_asserted('&corelib',['predicate-arity','function-arity',2]).
metta_other_asserted('&corelib',['function-arity','function-arity',1]).
%  ;; MettaMorph-If Function
metta_other_asserted('&corelib',[iz,'MettaMorph-If','MeTTaMorph']).
metta_other_asserted('&corelib',['@doc','MettaMorph-If',['@desc',"Conditional function that evaluates and returns one of the provided atoms based on a boolean condition."],['@params',[['@param',"Boolean condition"],['@param',"Atom to return if condition is True"],['@param',"Atom to return if condition is False (optional)"]]],['@return',"Either the second or third argument depending on the condition"]]).
metta_other_asserted('&corelib',[:,'MettaMorph-If',[->,'Bool','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[:,'MettaMorph-If',[->,'Bool','Atom','Atom']]).
metta_function_asserted('&corelib',['MettaMorph-If','True',_then],_then).
transpiler_clause_store('MettaMorph-If',[2],2,['Bool','Atom'],'Atom',[x(doeval,eager,[boolean]),x(noeval,lazy,[])],x(noeval,lazy,[]),['MettaMorph-If','True',_then],_then).


'mi__1_2_MettaMorph-If'(B,C,D) :- 
  E='mc__1_2_MettaMorph-If'(B,C,D) , 
  ci(true,'MettaMorph-If',2,['MettaMorph-If',B,C],D,true,E).




'me__1_2_MettaMorph-If'(B,C,D):-'mc__1_2_MettaMorph-If'(B,C,D).




'mc__1_2_MettaMorph-If'('True',A,A):-true.


metta_function_asserted('&corelib',['MettaMorph-If','False',_then],[let,_n,0,[let,_n,1,_n]]).
transpiler_clause_store('MettaMorph-If',[2],3,['Bool','Atom'],'Atom',[x(doeval,eager,[boolean]),x(noeval,lazy,[])],x(noeval,lazy,[]),['MettaMorph-If','False',_then],[let,_n,0,[let,_n,1,_n]]).


'mi__1_2_MettaMorph-If'(B,C,D) :- 
  E='mc__1_2_MettaMorph-If'(B,C,D) , 
  ci(true,'MettaMorph-If',2,['MettaMorph-If',B,C],D,true,E).




'me__1_2_MettaMorph-If'(B,C,D):-'mc__1_2_MettaMorph-If'(B,C,D).




'mc__1_2_MettaMorph-If'('False',_,A) :-  
  A =  
    ispuU(B,(B=0,B=1)).


%  ; Placeholder for False condition
metta_function_asserted('&corelib',['MettaMorph-If',_cond,_then,_else],[if,_cond,_then,_else]).
transpiler_clause_store('MettaMorph-If',[3],1,['Bool','Atom','Atom'],'Atom',[x(doeval,eager,[boolean]),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['MettaMorph-If',_cond,_then,_else],[if,_cond,_then,_else]).


'mi__1_3_MettaMorph-If'(B,C,D,E) :- 
  F='mc__1_3_MettaMorph-If'(B,C,D,E) , 
  ci( true, 
    'MettaMorph-If', 
    3, 
    ['MettaMorph-If',B,C,D], E,true,F).




'me__1_3_MettaMorph-If'(B,C,D,E) :-  
  'mc__1_3_MettaMorph-If'(B,C,D,E).




'mc__1_3_MettaMorph-If'(A,B,C,D) :-  
  D =  
    ispeEnN( E, 
      (is_True(A)*->F=B;F=C),as_p1_exec(F,E), 
      G, 
      (is_True(A)*->H=B;H=C),as_p1_expr(H,G)).


%  ;; Arity Assignments
metta_other_asserted('&corelib',['predicate-arity','predicate-arity',2]).
%  ;(predicate-arity : 3)
%  ; (= (: $F P1) (predicate-arity $F 1))
%  ;; Source Predicate and Function Types
metta_other_asserted('&corelib',[iz,'SrcPredicate','MeTTa']).
metta_other_asserted('&corelib',['@doc','SrcPredicate',['@desc',"Type representing a source predicate."]]).
metta_other_asserted('&corelib',[:,'SrcPredicate','Type']).
metta_other_asserted('&corelib',[iz,'SrcFunction','MeTTa']).
metta_other_asserted('&corelib',['@doc','SrcFunction',['@desc',"Type representing a source function."]]).
metta_other_asserted('&corelib',[:,'SrcFunction','Type']).
%  ;; MeTTaResult Type and Values
metta_other_asserted('&corelib',[iz,'MeTTaResult','MeTTa']).
metta_other_asserted('&corelib',['@doc','MeTTaResult',['@desc',"Type representing the result of a MeTTa evaluation."]]).
metta_other_asserted('&corelib',[:,'MeTTaResult','Type']).
metta_other_asserted('&corelib',[iz,'NotReducible','MeTTaResult']).
metta_other_asserted('&corelib',['@doc','NotReducible',['@desc',"Result indicating that an atom cannot be reduced further."]]).
metta_other_asserted('&corelib',[:,'NotReducible','MeTTaResult']).
metta_other_asserted('&corelib',[iz,'Empty','MeTTaResult']).
metta_other_asserted('&corelib',['@doc','Empty',['@desc',"Result indicating an empty evaluation result."]]).
metta_other_asserted('&corelib',[:,'Empty','MeTTaResult']).
%  ;; Subtype Relations
metta_other_asserted('&corelib',[iz,'ValueAtom','MeTTa']).
metta_other_asserted('&corelib',['@doc','ValueAtom',['@desc',"Type representing a value atom."]]).
metta_other_asserted('&corelib',[:>,'ValueAtom','Atom']).
metta_other_asserted('&corelib',[iz,'ForeignObject','MeTTa']).
metta_other_asserted('&corelib',['@doc','ForeignObject',['@desc',"Type representing a foreign object, such as a Python object."]]).
metta_other_asserted('&corelib',[:,'ForeignObject','Type']).
metta_other_asserted('&corelib',[:>,'ValueAtom','Grounded']).
metta_other_asserted('&corelib',[:>,'ForeignObject','ValueAtom']).
metta_other_asserted('&corelib',[iz,'PyObject','MeTTa']).
metta_other_asserted('&corelib',['@doc','PyObject',['@desc',"Type representing a Python object."]]).
metta_other_asserted('&corelib',[:>,'PyObject','ForeignObject']).
%  ;; Space Subtype Relation
metta_other_asserted('&corelib',['@doc','hyperon::space::DynSpace',['@desc',"Dynamic space type, representing an Atomspace."]]).
metta_other_asserted('&corelib',[:>,'hyperon::space::DynSpace','Grounded']).
metta_other_asserted('&corelib',[iz,'py-list','MeTTa']).
metta_other_asserted('&corelib',['@doc','py-list',['@desc',"Converts a MeTTa Expression into a Python list and returns it as a PyObject."],['@params',[['@param',['@desc',"A MeTTa List that will be converted into a Python list"]]]],['@return',['@desc',"A Python list object that represents the given MeTTa Expression as a PyObject"]]]).
metta_other_asserted('&corelib',[:,'py-list',[->,'Expression','PyObject']]).
metta_other_asserted('&corelib',[iz,'py-chain','MeTTa']).
metta_other_asserted('&corelib',['@doc','py-chain',['@desc',"Chains together a list of Python objects contained in a MeTTa Expression, applying the Python vertical bar | OR operation jointly to all elements."],['@params',[['@param',['@desc',"A MeTTa list of atoms, each embedding a Python object."]]]],['@return',['@desc',"A MeTTa atom which embeds the result of applying the Python OR | operator to all elements of the list."]]]).
metta_other_asserted('&corelib',[:,'py-chain',[->,'Expression','PyObject']]).
metta_other_asserted('&corelib',[iz,'py-eval','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','py-eval',['@desc',"Evaluates a Python expression from a string and returns the result as a PyObject."],['@params',[['@param',['@desc',"A string representing a Python expression that will be evaluated"]]]],['@return',['@desc',"The result of evaluating the Python expression as a PyObject"]]]).
metta_other_asserted('&corelib',[:,'py-eval',[->,'String','PyObject']]).
metta_other_asserted('&corelib',[iz,'py-exec!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','py-exec!',['@desc',"Executes some Python sourcecode from a string !(py-exec! \"import torch\") -> True.  !(py-exec! \"import torche\") -> False. "],['@params',[['@param',['@type','String'],['@desc',"A string representing a Python sourcecode that will be executed"]]]],['@return',['@desc',"The True|False results of executing the Python expression"]]]).
metta_other_asserted('&corelib',[:,'py-exec!',[->,'String','Bool']]).
%  ; !(import! &corelib "src/canary/stdlib_mettalog.metta")
%  ;!(println! "!(import! &corelib \"src/canary/stdlib_mettalog.metta\")")
metta_other_asserted('&corelib',[iz,'findall!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','findall!',['@desc',"Takes a Template and a Goal. Returns the list resulting from substituting all bindings from solving Goal into Template.\n  See Prolog's built-in predicate findall/3."],['@params',[['@param',"Template"],['@param',"Goal"]]],['@return',"Result list of all bindings for Goal substituted into Template"]]).
metta_other_asserted('&corelib',[:,'findall!',[->,'Expression','Expression','Expression']]).
%  ;; Functional Programming
metta_other_asserted('&corelib',[iz,'maplist!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','maplist!',['@desc',"Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). \n The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).\n F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).\n Use concurrent-maplist! for a multi-threaded, nondeterministic version.\n See Prolog's built-in predicate maplist."],['@params',[['@param',"Function to be applied"],['@param',"List"]]],['@return',"Result of applying Function to List(s)"]]).
metta_other_asserted('&corelib',[:,'maplist!',[->,'Function','Expression','Expression']]).
metta_other_asserted('&corelib',[:,'maplist!',[->,'Function','Expression','Expression','Expression']]).
metta_other_asserted('&corelib',[:,'maplist!',[->,'Function','Expression','Expression','Expression','Expression']]).
%  ;; Functional Programming
metta_other_asserted('&corelib',[iz,'concurrent-maplist!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','concurrent-maplist!',['@desc',"Takes a function F and one to three lists; Returns the result of applying F to each item in the list(s). \n The provided lists are not evaluated (this matches the behavior of e.g. cons-atom).\n F must have the number of lists provided as a valid arity (i.e. unary for one list, binary for two, ternary for three).\n The applications of F to the list items are processed in parallel. Because of the overhead of this approach, a speedup is only likely if F is expensive to evaluate.\n Use maplist! for a single-threaded, deterministic version.\n See Prolog's built-in predicate concurrent-maplist."],['@params',[['@param',"Function to be applied"],['@param',"List"]]],['@return',"Result of applying Function to List(s)"]]).
metta_other_asserted('&corelib',[:,'concurrent-maplist!',[->,'Function','Expression','Expression']]).
metta_other_asserted('&corelib',[:,'concurrent-maplist!',[->,'Function','Expression','Expression','Expression']]).
metta_other_asserted('&corelib',[:,'concurrent-maplist!',[->,'Function','Expression','Expression','Expression','Expression']]).
metta_other_asserted('&corelib',[iz,throw,'MeTTaLog']).
metta_other_asserted('&corelib',['@doc',throw,['@desc',"Raises an exception. See also `catch`; the system will look for the innermost catch such that Exception unifies with Catcher."],['@params',[['@param',"Exception"]]],['@return',"Does not return - raises an exception"]]).
metta_other_asserted('&corelib',[:,throw,[->,'Atom','ErrorType']]).
metta_other_asserted('&corelib',[iz,catch,'MeTTaLog']).
metta_other_asserted('&corelib',['@doc',catch,['@desc',"Executes Form. If an exception is raised with `throw` during execution of Form while this is the innermost catch such that Catcher unifies with Exception, the exception is caught. Recover is then executed with bindings from Catcher."],['@params',[['@param',"Form"],['@param',"Catcher"],['@param',"Recover"]]],['@return',"Result of Form if no exception is raised. Result of Recover (with bindings from Catcher) if an exception is caught."]]).
metta_other_asserted('&corelib',[:,catch,[->,'Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'max-time!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','max-time!',['@desc',"Executes Form. If execution takes longer than Time, will raise a time_limit_exceeded exception. See also `catch`."],['@params',[['@param',"Time (in seconds)"],['@param',"Form"]]],['@return',"Result of Form if execution completes within Time. Raises an exception otherwise."]]).
metta_other_asserted('&corelib',[:,'max-time!',[->,'Number','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'sleep!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','sleep!',['@desc',"Sleeps for N seconds."],['@params',[['@param',"N"]]],['@return',"Returns True after sleeping completes."]]).
metta_other_asserted('&corelib',[:,'sleep!',[->,'Number','Bool']]).
metta_other_asserted('&corelib',[iz,'limit!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','limit!',['@desc',"Executes Form generating at most Limit results. Results are returned as soon as they become available."],['@params',[['@param',"Limit"],['@param',"Form"]]],['@return',"First Limit results of Form."]]).
metta_other_asserted('&corelib',[:,'limit!',[->,'Number','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'number-of','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','number-of',['@desc',"Returns the number of results Form generates"],['@params',[['@param',"Form"]]],['@return',"Number of results of Form."]]).
metta_other_asserted('&corelib',[:,'number-of',[->,'Atom','Number']]).
metta_other_asserted('&corelib',[iz,'offset!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','offset!',['@desc',"Executes Form ignoring the first Count results. Results are returned as soon as they become available."],['@params',[['@param',"Count"],['@param',"Form"]]],['@return',"Results of Form after ignoring the first Count results that are generated."]]).
metta_other_asserted('&corelib',[:,'offset!',[->,'Number','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'call!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','call!',['@desc',"Trampolines to Prolog's call. Only works when the predicate and each arg are provided separately. e.g. !(call! ls /) will print the root directory but !(call! ls(/)) will fail."],['@params',[['@param',"Form"]]],['@return',"True if the call succeeds, False otherwise."]]).
metta_other_asserted('&corelib',[:,'call!',[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'call-p!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','call-p!',['@desc',"Like call-fn! except it does not append the called term with a return arg."],['@params',[['@param',"Form"]]],['@return',"True if the call succeeds, False otherwise."]]).
metta_other_asserted('&corelib',[:,'call-p!',[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'call-fn!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','call-fn!',['@desc',"Trampolines to Prolog's call after appending the return argument.  Each arg are provided separately. e.g. !(call-fn! length (1 2 3)) will return 3."],['@params',[['@param',"Form"]]],['@return',"appends a return argument to a form and calls it"]]).
metta_other_asserted('&corelib',[:,'call-fn!',[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'call-string!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','call-string!',['@desc',"Executes String as Prolog code. For example, (call-string! \"member(1,[1,2,3])\") returns [()] while (call-string! \"member(4,[1,2,3])\") returns []. (call-string! \"member(X,[1,2,3])\") returns [(1), (2), (3)]."],['@params',[['@param',"PrologSrc"]]],['@return',"A list of the binding values. If there are no bindings but the Prolog query is True, returns the empty list."]]).
metta_other_asserted('&corelib',[:,'call-string!',[->,'String','Atom']]).
metta_other_asserted('&corelib',[iz,'call-cleanup!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','call-cleanup!',['@desc',"Same as (setup-call-cleanup! true Form Cleanup). setup-call-cleanup! is recommended instead if Cleanup is intended to undo prior side-effects - place those side-effects in Setup."],['@params',[['@param',"Form"],['@param',"Cleanup"]]],['@return',"Result of Form."]]).
metta_other_asserted('&corelib',[:,'call-cleanup!',[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'setup-call-cleanup!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','setup-call-cleanup!',['@desc',"Executes Setup, then Form, then finally Cleanup. Setup is protected from interrupts (e.g. max-time!). In most uses, Setup will perform temporary side-effects required by Form that are finally undone by Cleanup. Cleanup is run even if Form raises an exception. For each result of Setup, Form is run to completion, then Cleanup is run."],['@params',[['@param',"Setup"],['@param',"Form"],['@param',"Cleanup"]]],['@return',"Result of Form."]]).
metta_other_asserted('&corelib',[:,'setup-call-cleanup!',[->,'Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'predicate-arity','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','predicate-arity',['@desc',"Returns the arity of Function's predicate form, i.e. the function-arity + 1. (The additional argument being the function's result as an argument to the predicate.)"],['@params',[['@param',"Function"]]],['@return',"Arity of Function's predicate form."]]).
metta_other_asserted('&corelib',[iz,'function-arity','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','function-arity',['@desc',"Returns the arity of Function."],['@params',[['@param',"Function"]]],['@return',"Arity of Function."]]).
metta_other_asserted('&corelib',[iz,'open!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','open!',['@desc',"Opens Filename as a stream under Mode. Mode is one of read, write, append, or update. Mode append opens the file for writing, positioning the file pointer at the end. Mode update opens the file for writing, positioning the file pointer at the beginning of the file without truncating the file."],['@params',[['@param',"Filename"],['@param',"Mode"]]],['@return',"Stream"]]).
metta_other_asserted('&corelib',[:,'open!',[->,'String','Atom','Stream']]).
metta_other_asserted('&corelib',[iz,'close!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','close!',['@desc',"Closes Steam, returning True on success."],['@params',[['@param',"Stream"]]],['@return',"Boolean"]]).
metta_other_asserted('&corelib',[:,'close!',[->,'Stream','Boolean']]).
metta_other_asserted('&corelib',[iz,'with-output-to!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','with-output-to!',['@desc',"Evaluates expression with all outupt (e.g. from print) redirected to Stream. See also open! and close!."],['@params',[['@param',"Stream"],['@param',"Expression"]]],['@return',"Result of Expression"]]).
metta_other_asserted('&corelib',[:,'with-output-to!',[->,'Stream','Expression','Atom']]).
metta_other_asserted('&corelib',[iz,'load-file!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','load-file!',['@desc',"Loads the contents of Filename into Space, returning () if successful. Can load Python code (.py) or MeTTa (.metta); if ambiguous, assumes MeTTa. Like import! but favors MeTTa over Python when the file type is ambiguous."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_other_asserted('&corelib',[:,'load-file!',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_other_asserted('&corelib',[iz,'load-ascii','MeTTa']).
metta_other_asserted('&corelib',['@doc','load-ascii',['@desc',"Loads the contents of Filename into Space, returning () if successful. Assumes the file is an ASCII file. Works like include!."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_other_asserted('&corelib',[:,'load-ascii',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_other_asserted('&corelib',[iz,'transfer!','MeTTa']).
metta_other_asserted('&corelib',['@doc','transfer!',['@desc',"Loads the contents of Filename into &self, as include. Returns () if successful, throws an exception otherwise."],['@params',[['@param',"Filename"]]],['@return',"Expression"]]).
metta_other_asserted('&corelib',[:,'transfer!',[->,'String',[->]]]).
metta_other_asserted('&corelib',[iz,'save-space!','MeTTa']).
metta_other_asserted('&corelib',['@doc','save-space!',['@desc',"Writes the contents of Space into Filename, returning () if successful."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_other_asserted('&corelib',[:,'save-space!',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_other_asserted('&corelib',[iz,'rtrace!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','rtrace!',['@desc',"Fully evaluates input Atom, providing a complete trace of the evaluation."],['@params',[['@param',"Atom to be evaluated"]]],['@return',"Result of evaluation"]]).
metta_other_asserted('&corelib',[:,'rtrace!',[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,rust,'MeTTaLog']).
metta_other_asserted('&corelib',['@doc',rust,['@desc',"Interface with the rust / Hyperon MeTTa implementation. Enters Atom into rust atomspace. If Atom is evaluated (i.e. by being of the form !<atom>), returns the result of evaluation. See also rust!."],['@params',[['@param',"Atom to be entered into the space"]]],['@return',"Result of entering Atom into the space"]]).
metta_other_asserted('&corelib',[:,rust,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'rust!','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','rust!',['@desc',"Like rust but evaluates the atom rather than entering into the space. (rust! <atom>) and (rust !<atom>) are identical."],['@params',[['@param',"Atom to be evaluated"]]],['@return',"Result of evaluation"]]).
metta_other_asserted('&corelib',[:,'rust!',[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,coerce,'MeTTaLog']).
metta_other_asserted('&corelib',['@doc',coerce,['@desc',"Cast (coerce) Value to be of Type. Supports the basic types Atom (no-op), Number, String, and Bool.\n  Number: Converts number strings and bools to numbers. True is 1, False is 0.\n  String: Coerced as if Value were printed.\n  Bool: False, 0, and () are False, all other values are True."],['@params',[['@param',"Type"],['@param',"Value"]]],['@return',"Value cast to Type"]]).
metta_other_asserted('&corelib',[:,coerce,[->,'Type','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,=,'MeTTa']).
metta_other_asserted('&corelib',['@doc',=,['@desc',"A symbol used to define reduction rules for expressions."],['@params',[['@param',"Pattern to be matched against expression to be reduced"],['@param',"Result of reduction or transformation of the first pattern"]]],['@return',"Not reduced itself unless custom equalities over equalities are added"]]).
metta_other_asserted('&corelib',[:,=,[->,_t,_t,'Atom']]).
%  ;(: = (-> Atom Atom Atom))
metta_other_asserted('&corelib',[iz,'ErrorType','MeTTa']).
metta_other_asserted('&corelib',['@doc','ErrorType',['@desc',"Type of the atom which contains error"]]).
metta_other_asserted('&corelib',[:,'ErrorType','Type']).
metta_other_asserted('&corelib',[iz,'Error','MeTTa']).
metta_other_asserted('&corelib',['@doc','Error',['@desc',"Error constructor"],['@params',[['@param',"Atom which contains error"],['@param',"Error message, can be one of the reserved symbols: BadType, IncorrectNumberOfArguments"]]],['@return',"Instance of the error atom"]]).
metta_other_asserted('&corelib',[:,'Error',[->,'Atom','Atom','ErrorType']]).
metta_other_asserted('&corelib',[iz,return,'MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc',return,['@desc',"Returns value from the (function ...) expression"],['@params',[['@param',"Value to be returned"]]],['@return',"Passed argument"]]).
%  ; probably should be (: return (-> $t $t)) 
metta_other_asserted('&corelib',[:,return,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,function,'MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc',function,['@desc',"Evaluates the argument until it becomes (return <result>). Then (function (return <result>)) is reduced to the <result>."],['@params',[['@param',"Atom to be evaluated"]]],['@return',"Result of atom's evaluation"]]).
metta_other_asserted('&corelib',[:,function,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,eval,'MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc',eval,['@desc',"Evaluates input Atom, performs one step of the evaluation. Empty results (Empty, ()) are removed from the result set. If no results are produced for a non-grounded function, eval returns NotReducible."],['@params',[['@param',"Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function"]]],['@return',"Result of evaluation"]]).
metta_other_asserted('&corelib',[:,eval,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,eval,'MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc',evalc,['@desc',"Evaluates input atom, makes one step of the evaluation"],['@params',[['@param',"Atom to be evaluated, can be reduced via equality expression (= ...) or by calling a grounded function"],['@param',"Space to evaluate atom in its context"]]],['@return',"Result of evaluation"]]).
metta_other_asserted('&corelib',[:,evalc,[->,'Atom','Grounded','%Undefined%']]).
metta_other_asserted('&corelib',[iz,chain,'MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc',chain,['@desc',"Evaluates first argument Atom, binds it to the Variable (second argument) and then evaluates third argument Template with Variable substituted in. When evaluation of the first Atom brings more than a single result, chain returns one instance of the Template expression for each result. The first argument Atom is only evaluated if it is part of the Minimal MeTTa specification; evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation)."],['@params',[['@param',"Atom to be evaluated"],['@param',"Variable"],['@param',"Template which will be evaluated at the end with Variable substituted"]]],['@return',"Result of evaluating third input argument"]]).
metta_other_asserted('&corelib',[:,chain,[->,'Atom','Variable','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,unify,'MeTTa']).
metta_other_asserted('&corelib',['@doc',unify,['@desc',"Like Match but allows any sort of container for the first argument. (Match only allows MeTTa spaces.)"],['@params',[['@param',"The collection or space to match or the first atom to unify with"],['@param',"Second atom to unify with"],['@param',"Result if two atoms unified successfully"],['@param',"Result otherwise"]]],['@return',"Third argument when found or fourth one otherwise"]]).
metta_other_asserted('&corelib',[:,unify,[->,'Atom','Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'if-unify','MinimalMeTTaHelper']).
metta_other_asserted('&corelib',['@doc','if-unify',['@desc',"Matches two first terms and returns third argument if they are matched and fourth argument otherwise"],['@params',[['@param',"First term to unify with"],['@param',"Second atom to unify with"],['@param',"Result if two atoms unified successfully"],['@param',"Result otherwise"]]],['@return',"Third argument when first two atoms are unifiable or fourth one otherwise"]]).
metta_other_asserted('&corelib',[:,'if-unify',[->,'Atom','Atom','Atom','Atom','%Undefined%']]).
metta_other_asserted('&corelib',['ALT=',['if-unify',_a,_a,_then,_else],_then]).
metta_other_asserted('&corelib',['ALT=',['if-unify',_a,_b,_then,_else],[case,['if-unify-or-empty',_a,_b],[['Empty',_else]]]]).
metta_other_asserted('&corelib',[iz,'if-unify-or-empty','MinimalMeTTaHelper']).
metta_other_asserted('&corelib',['@doc','if-unify-or-empty',['@desc',"Attempts to unify two atoms and returns a result. Returns Empty if they cannot be unified."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"Unification result or Empty"]]).
metta_other_asserted('&corelib',[:,'if-unify-or-empty',[->,'Atom','Atom','Atom']]).
metta_function_asserted('&corelib',['if-unify-or-empty',_a,_a],unified).
transpiler_clause_store('if-unify-or-empty',[2],2,['Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-unify-or-empty',_a,_a],unified).


'mi__1_2_if-unify-or-empty'(B,C,D) :- 
  E='mc__1_2_if-unify-or-empty'(B,C,D) , 
  ci( true, 
    'if-unify-or-empty', 
    2, 
    ['if-unify-or-empty',B,C], D,true,E).




'me__1_2_if-unify-or-empty'(B,C,D) :-  
  'mc__1_2_if-unify-or-empty'(B,C,D).




'mc__1_2_if-unify-or-empty'(A,A,B):-B=ispu(unified).


metta_function_asserted('&corelib',['if-unify-or-empty',_a,_b],[empty]).
transpiler_clause_store('if-unify-or-empty',[2],3,['Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-unify-or-empty',_a,_b],[empty]).


'mi__1_2_if-unify-or-empty'(B,C,D) :- 
  E='mc__1_2_if-unify-or-empty'(B,C,D) , 
  ci( true, 
    'if-unify-or-empty', 
    2, 
    ['if-unify-or-empty',B,C], D,true,E).




'me__1_2_if-unify-or-empty'(B,C,D) :-  
  'mc__1_2_if-unify-or-empty'(B,C,D).




'mc__1_2_if-unify-or-empty'(_,_,A) :-  
  A =  
    ispeEnN(B,mi__1_0_empty(B),C,C=[empty]).


metta_other_asserted('&corelib',[iz,'cons-atom','MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc','cons-atom',['@desc',"Constructs an expression using two arguments"],['@params',[['@param',"Head of an expression"],['@param',"Tail of an expression"]]],['@return',"New expression consisting of the two input arguments"]]).
metta_other_asserted('&corelib',[:,'cons-atom',[->,'Atom','Expression','Expression']]).
%  ; AKA? (: cons (-> Atom Atom Atom))
metta_other_asserted('&corelib',[iz,'decons-atom','MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc','decons-atom',['@desc',"Works as a reverse to cons-atom function. It gets Expression as an input and returns it split into head and tail, e.g. (decons-atom (Cons X Nil)) -> (Cons (X Nil))"],['@params',[['@param',"Expression to be Deconsed"]]],['@return',"Deconsed expression"]]).
metta_other_asserted('&corelib',[:,'decons-atom',[->,'Expression','Expression']]).
%  ; AKA? (: decons (-> Atom Atom))
metta_other_asserted('&corelib',[iz,'min-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','min-atom',['@desc',"Returns atom with minimum value in the expression (first argument). Only numbers are allowed."],['@params',[['@param',"Expression containing atoms of Number type"]]],['@return',"Minimum value in the expression. Error if expression contains non-numeric value or is empty."]]).
metta_other_asserted('&corelib',[:,'min-atom',[->,'Expression','Number']]).
metta_function_asserted('&corelib',['min-atom',_L],['call-fn!',min_list,_L]).
transpiler_clause_store('min-atom',[1],1,['Expression'],'Number',[x(noeval,eager,[])],x(doeval,eager,[number]),['min-atom',_L],['call-fn!',min_list,_L]).


'mi__1_1_min-atom'(B,C) :- 
  D='mc__1_1_min-atom'(B,C) , 
  ci(true,'min-atom',1,['min-atom',B],C,true,D).




'me__1_1_min-atom'(B,C):-'mc__1_1_min-atom'(B,C).




'mc__1_1_min-atom'(A,B) :-  
  'mc_n_1__call-fn!'(min_list,[A],B).


metta_other_asserted('&corelib',[iz,'max-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','max-atom',['@desc',"Returns atom with maximum value in the expression (first argument). Only numbers are allowed."],['@params',[['@param',"Expression containing atoms of Number type"]]],['@return',"Maximum value in the expression. Error if expression contains non-numeric value or is empty."]]).
metta_other_asserted('&corelib',[:,'max-atom',[->,'Expression','Number']]).
metta_other_asserted('&corelib',['is-fn-1','max-atom',max_list]).
metta_other_asserted('&corelib',[iz,'size-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','size-atom',['@desc',"Returns the size of an expression (first argument)."],['@params',[['@param',"Expression to measure the size of"]]],['@return',"Size of the expression"]]).
metta_other_asserted('&corelib',[:,'size-atom',[->,'Expression','Integer']]).
metta_other_asserted('&corelib',['is-fn-1','size-atom',length]).
metta_other_asserted('&corelib',[iz,'index-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','index-atom',['@desc',"Returns atom from an expression (first argument) using index (second argument) or error if index is out of bounds"],['@params',[['@param',"Expression"],['@param',"Index"]]],['@return',"Atom at the specified index in the expression. Error if index is out of bounds."]]).
metta_other_asserted('&corelib',[:,'index-atom',[->,'Expression','Number','Atom']]).
metta_other_asserted('&corelib',['is-fn-21','index-atom',nth0]).
%  ;; ==> (= (index-atom $L $N) (call-fn! nth0 $N $L))
metta_other_asserted('&corelib',[iz,'pow-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','pow-math',['@desc',"Takes base (first argument) and power (second argument) and returns result of a power function (base ^ power)"],['@params',[['@param',"Base. Could be a float."],['@param',"Power."]]],['@return',"Result of base raised to the power"]]).
%  ; Define a variable using a function call to compute the power of a base number to an exponent
%  ; The result of the computation is stored in the variable `pow-math`
%  ; Assign the result of a function call to `pow-math`
metta_function_asserted('&corelib',['pow-math',_B,_P],['call-fn!',pow,_B,_P]).
transpiler_clause_store('pow-math',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['pow-math',_B,_P],['call-fn!',pow,_B,_P]).


'mi__1_2_pow-math'(B,C,D) :- 
  E='mc__1_2_pow-math'(B,C,D) , 
  ci(true,'pow-math',2,['pow-math',B,C],D,true,E).




'me__1_2_pow-math'(B,C,D):-'mc__1_2_pow-math'(B,C,D).




'mc__1_2_pow-math'(A,B,C) :-  
  'mc_n_1__call-fn!'(pow,[A,B],C).


metta_other_asserted('&corelib',[==>,['is-op-1',_m,_p],['is-fn-1',_m,_p]]).
metta_other_asserted('&corelib',[==>,['is-op-1',_m,_],[:,_m,[->,'Number','Number']]]).
metta_other_asserted('&corelib',[==>,['is-op-2',_m,_p],['is-fn-2',_m,_p]]).
metta_other_asserted('&corelib',[==>,['is-op-2',_m,_],[:,_m,[->,'Number','Number','Number']]]).
metta_other_asserted('&corelib',[==>,['is-pred',_m,_p],['is-pred-1',_m,_p]]).
metta_other_asserted('&corelib',[==>,['is-pred',_m,_],[:,_m,[->,'Number','Bool']]]).
metta_other_asserted('&corelib',[==>,['is-fn-1',_m,_p],[=,[_m,_a],['call-fn!',_p,_a]]]).
metta_other_asserted('&corelib',[==>,['is-fn-r',_m,_p],[=,[_m,_a],['call-fn-r!',_p,_a]]]).
metta_other_asserted('&corelib',[==>,['is-fn-2',_m,_p],[=,[_m,_a,_b],['call-fn!',_p,_a,_b]]]).
metta_other_asserted('&corelib',[==>,['is-fn-21',_m,_p],[=,[_m,_L,_N],['call-fn!',_p,_N,_L]]]).
metta_other_asserted('&corelib',[==>,['is-fn-3',_m,_p],[=,[_m,_a,_b,_c],['call-fn!',_p,_a,_b,_c]]]).
metta_other_asserted('&corelib',[==>,['is-pred-1',_m,_p],[=,[_m,_a],['call-p!',_p,_a]]]).
metta_other_asserted('&corelib',[==>,['is-pred-2',_m,_p],[=,[_m,_a,_b],['call-p!',_p,_a,_b]]]).
metta_other_asserted('&corelib',['is-fn-1','py-list',py_list]).
metta_other_asserted('&corelib',[iz,'sqrt-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','sqrt-math',['@desc',"Returns square root for input number (first argument) which should be >= 0"],['@params',[['@param',"Input number"]]],['@return',"Result of a square root function"]]).
metta_other_asserted('&corelib',['is-op-1','sqrt-math',sqrt]).
metta_other_asserted('&corelib',[iz,'abs-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','abs-math',['@desc',"Returns absolute value of input number (first argument)"],['@params',[['@param',"Input number"]]],['@return',"Absolute value"]]).
metta_other_asserted('&corelib',['is-op-1','abs-math',abs]).
metta_other_asserted('&corelib',[iz,'log-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','log-math',['@desc',"Returns result of a logarithm function given base (first argument) and input number (second argument)"],['@params',[['@param',"Base"],['@param',"Input number"]]],['@return',"Result of log function"]]).
metta_other_asserted('&corelib',['is-op-2','log-math',log2]).
metta_other_asserted('&corelib',[iz,'trunc-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','trunc-math',['@desc',"Returns integer part of the input value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Integer part of float"]]).
metta_other_asserted('&corelib',['is-op-1','trunc-math',trunc]).
metta_other_asserted('&corelib',[iz,ceil,'MeTTa']).
metta_other_asserted('&corelib',['@doc','ceil-math',['@desc',"Returns the smallest integer greater than or equal to the input value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Integer value greater than or equal to the input"]]).
metta_other_asserted('&corelib',['is-op-1','ceil-math',ceil]).
metta_other_asserted('&corelib',[iz,floor,'MeTTa']).
metta_other_asserted('&corelib',['@doc','floor-math',['@desc',"Returns the smallest integer less than or equal to the input value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Integer value less than or equal to the input"]]).
metta_other_asserted('&corelib',['is-op-1','floor-math',floor]).
metta_other_asserted('&corelib',[iz,round,'MeTTa']).
metta_other_asserted('&corelib',['@doc','round-math',['@desc',"Returns the nearest integer to the input float value (first argument)"],['@params',[['@param',"Float value"]]],['@return',"Nearest integer to the input"]]).
metta_other_asserted('&corelib',['is-op-1',round,round]).
metta_other_asserted('&corelib',[iz,sin,'MeTTa']).
metta_other_asserted('&corelib',['@doc','sin-math',['@desc',"Returns result of the sine function for an input value in radians (first argument)"],['@params',[['@param',"Angle in radians"]]],['@return',"Result of the sine function"]]).
metta_other_asserted('&corelib',['is-op-1','sin-math',sin]).
metta_other_asserted('&corelib',[iz,'asin-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','asin-math',['@desc',"Returns result of the arcsine function for an input value (first argument)"],['@params',[['@param',"Float number"]]],['@return',"Result of the arcsine function"]]).
metta_other_asserted('&corelib',['is-op-1','asin-math',asin]).
metta_other_asserted('&corelib',[iz,'cos-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','cos-math',['@desc',"Returns result of the cosine function for an input value in radians (first argument)"],['@params',[['@param',"Angle in radians"]]],['@return',"Result of the cosine function"]]).
metta_other_asserted('&corelib',['is-op-1','cos-math',cos]).
metta_other_asserted('&corelib',[iz,'acos-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','acos-math',['@desc',"Returns result of the arccosine function for an input value (first argument)"],['@params',[['@param',"Float number"]]],['@return',"Result of the arccosine function"]]).
metta_other_asserted('&corelib',['is-op-1','acos-math',acos]).
metta_other_asserted('&corelib',[iz,'tan-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','tan-math',['@desc',"Returns result of the tangent function for an input value in radians (first argument)"],['@params',[['@param',"Angle in radians"]]],['@return',"Result of the tangent function"]]).
metta_other_asserted('&corelib',['is-op-1','tan-math',tan]).
metta_other_asserted('&corelib',[iz,'atan-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','atan-math',['@desc',"Returns result of the arctangent function for an input value (first argument)"],['@params',[['@param',"Float number"]]],['@return',"Result of the tangent function"]]).
metta_other_asserted('&corelib',['is-op-1','atan-math',atan]).
metta_other_asserted('&corelib',[iz,'isnan-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','isnan-math',['@desc',"Returns True if input value is NaN. False - otherwise"],['@params',[['@param',"Number"]]],['@return',"True/False"]]).
metta_other_asserted('&corelib',['is-pred','isnan-math',is_NaN]).
metta_other_asserted('&corelib',[iz,'isinf-math','MeTTa']).
metta_other_asserted('&corelib',['@doc','isinf-math',['@desc',"Returns True if input value is positive or negative infinity. False - otherwise"],['@params',[['@param',"Number"]]],['@return',"True/False"]]).
%  ; this is deduced: (: isinf (-> Number Bool))
metta_other_asserted('&corelib',['is-pred','isinf-math',is_Inf]).
metta_other_asserted('&corelib',[:,'random-int',[->,'RandomGenerator','Number','Number','Number']]).
metta_other_asserted('&corelib',[iz,'random-int','UseRust']).
metta_other_asserted('&corelib',['@doc','random-int',['@desc',"Returns random int number from range defined by two numbers (second and third argument)"],['@params',[['@param',"Random number generator instance"],['@param',"Range start"],['@param',"Range end"]]],['@return',"Random int number from defined range"]]).
metta_other_asserted('&corelib',[iz,'random-float','UseRust']).
metta_other_asserted('&corelib',[:,'random-float',[->,'RandomGenerator','Number','Number','Number']]).
metta_other_asserted('&corelib',['@doc','random-float',['@desc',"Returns random float number from range defined by two numbers (second and third argument)"],['@params',[['@param',"Random number generator instance"],['@param',"Range start"],['@param',"Range end"]]],['@return',"Random float number from defined range"]]).
metta_other_asserted('&corelib',[iz,'set-random-seed','MeTTa']).
metta_other_asserted('&corelib',[iz,'set-random-seed','UseRust']).
metta_other_asserted('&corelib',[:,'set-random-seed',[->,'RandomGenerator','Number',[->]]]).
metta_other_asserted('&corelib',['@doc','set-random-seed',['@desc',"Sets a new seed (second argument) for random number generator (first argument)"],['@params',[['@param',"Random number generator instance"],['@param',"Seed"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[iz,'new-random-generator','MeTTa']).
metta_other_asserted('&corelib',[iz,'new-random-generator','UseRust']).
metta_other_asserted('&corelib',[:,'new-random-generator',[->,'Number','RandomGenerator']]).
metta_other_asserted('&corelib',['@doc','new-random-generator',['@desc',"Creates new random number generator instance using seed as input (first argument)"],['@params',[['@param',"Seed"]]],['@return',"Instance of random number generator"]]).
metta_other_asserted('&corelib',[iz,'reset-random-generator','MeTTa']).
metta_other_asserted('&corelib',[:,'reset-random-generator',[->,'RandomGenerator','RandomGenerator']]).
metta_other_asserted('&corelib',['@doc','reset-random-generator',['@desc',"Resets instance of random number generator (first argument) to its default behavior (StdRng::from_os_rng())"],['@params',[['@param',"Random number generator instance"]]],['@return',"Random number generator instance with default behavior"]]).
%  ;; Some code still uses the old syntax
metta_other_asserted('&corelib',[iz,'random-int','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','random-int',['@desc',"Returns random int number from range defined by two numbers (first and second argument)"],['@params',[['@param',"Range start"],['@param',"Range end"]]],['@return',"Random int number from defined range"]]).
metta_other_asserted('&corelib',['is-op-2','random-int',random]).
metta_other_asserted('&corelib',[iz,'random-float','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','random-float',['@desc',"Returns random float number from range defined by two numbers (first and second argument)"],['@params',[['@param',"Range start"],['@param',"Range end"]]],['@return',"Random float number from defined range"]]).
metta_other_asserted('&corelib',['is-op-2','random-int',random]).
metta_other_asserted('&corelib',['@doc','file-read',['@desc',"Function takes path to the file in form of string and reads file as string"],['@params',[['@param',"Filepath (string atom)"]]],['@return',"Content of file (string atom), error if no file exists"]]).
metta_function_asserted('&corelib',['file-read',_filepath],['call-fn',file_read_content,_filepath]).
transpiler_clause_store('file-read',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['file-read',_filepath],['call-fn',file_read_content,_filepath]).


'mi__1_1_file-read'(B,C) :- 
  D='mc__1_1_file-read'(B,C) , 
  ci(true,'file-read',1,['file-read',B],C,true,D).




'me__1_1_file-read'(B,C):-'mc__1_1_file-read'(B,C).




'mc__1_1_file-read'(A,B) :-  
  'mc_n_1__call-fn'(file_read_content,[A],B).


metta_other_asserted('&corelib',['@doc','file-write',['@desc',"Function takes path to the file (string atom) and content to be written (string atom), creates file and puts content into it"],['@params',[['@param',"Filepath (string atom)"],['@param',"Content (string atom)"]]],['@return',"Unit atom if write successfull, error otherwise (if there is no such folder)"]]).
metta_function_asserted('&corelib',['file-write',_filepath,_content],['call-fn',file_write_content,_filepath,_content]).
transpiler_clause_store('file-write',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['file-write',_filepath,_content],['call-fn',file_write_content,_filepath,_content]).


'mi__1_2_file-write'(B,C,D) :- 
  E='mc__1_2_file-write'(B,C,D) , 
  ci(true,'file-write',2,['file-write',B,C],D,true,E).




'me__1_2_file-write'(B,C,D):-'mc__1_2_file-write'(B,C,D).




'mc__1_2_file-write'(A,B,C) :-  
  'mc_n_1__call-fn'(file_write_content,[A,B],C).


metta_other_asserted('&corelib',[iz,'collapse-bind','MeTTa']).
metta_other_asserted('&corelib',['@doc','collapse-bind',['@desc',"Evaluates the Atom (first argument) and returns an expression which contains all alternative evaluations in a form (Atom Bindings). Bindings are represented in a form of a grounded atom { <var> <- <binding>, ... }. See also the complement superpose-bind. Note that, like chain, collapse-bind only evaluates Minimal Metta expressions. Evaluation of non-Minimal MeTTa atoms can be controlled by wrapping in a call to eval (for one evaluation step) or metta (for full evaluation)."],['@params',[['@param',"Minimal MeTTa operation to be evaluated"]]],['@return',"All alternative evaluations"]]).
%  ;; collapse-bind because `collapse` doesnt guarantee shared bindings
metta_other_asserted('&corelib',[:,'collapse-bind',[->,'Atom','Expression']]).
metta_other_asserted('&corelib',[iz,'superpose-bind','MeTTa']).
metta_other_asserted('&corelib',['@doc','superpose-bind',['@desc',"Complement to the collapse-bind. It takes result of collapse-bind (first argument) and returns only result atoms without bindings. Primarily used with some filtering step on the collapse-bind results, i.e. collapse-bind -> <filter> -> superpose-bind."],['@params',[['@param',"Expression in form (Atom Binding)"]]],['@return',"Non-deterministic list of Atoms"]]).
%  ;; superpose-bind because `superpose` doesnt guarentee shared bindings
metta_other_asserted('&corelib',[:,'superpose-bind',[->,'Expression','Atom']]).
%  ; Helper Minimal Metta?
metta_other_asserted('&corelib',['@doc',metta,['@desc',"Run MeTTa interpreter on atom."],['@params',[['@param',"Atom to be interpreted"],['@param',"Type of input atom"],['@param',"Atomspace where intepretation should take place"]]],['@return',"Result of interpretation"]]).
metta_other_asserted('&corelib',[:,metta,[->,'Atom','Type','Grounded','Atom']]).
metta_other_asserted('&corelib',[iz,id,'MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc',id,['@desc',"Returns its argument"],['@params',[['@param',"Input argument"]]],['@return',"Input argument"]]).
metta_other_asserted('&corelib',[:,id,[->,'Atom','Atom']]).
metta_function_asserted('&corelib',[id,_x],_x).
transpiler_clause_store(id,[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),[id,_x],_x).


mi__1_1_id(B,C) :- 
  D=mc__1_1_id(B,C) , 
  ci(true,id,1,[id,B],C,true,D).




me__1_1_id(B,C):-mc__1_1_id(B,C).




mc__1_1_id(A,A):-true.


metta_other_asserted('&corelib',[iz,'atom-subst','MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc','atom-subst',['@desc',"Substitutes variable passed as a second argument in the third argument by the first argument"],['@params',[['@param',"Value to use for replacement"],['@param',"Variable to replace"],['@param',"Template to replace variable by the value"]]],['@return',"Template with substituted variable"]]).
metta_other_asserted('&corelib',[:,'atom-subst',[->,'Atom','Variable','Atom','Atom']]).
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
metta_function_asserted('&corelib',['atom-subst',_atom,_var,_templ],[function,[chain,[eval,[id,_atom]],_var,[return,_templ]]]).
transpiler_clause_store('atom-subst',[3],1,['Atom','Variable','Atom'],'Atom',[x(noeval,lazy,[]),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,lazy,[]),['atom-subst',_atom,_var,_templ],[function,[let,_var,[id,_atom],[return,_templ]]]).


'mi__1_3_atom-subst'(B,C,D,E) :- 
  F='mc__1_3_atom-subst'(B,C,D,E) , 
  ci( true, 
    'atom-subst', 
    3, 
    ['atom-subst',B,C,D], E,true,F).




'me__1_3_atom-subst'(B,C,D,E) :-  
  'mc__1_3_atom-subst'(B,C,D,E).




'mc__1_3_atom-subst'(A,B,C,D) :-  
  D =  
    ispeEnN( E, 
        ((
         (catch( ( mi__1_1_id(A,F)  ,
                   as_p1_exec(F,G) , 
                   B=G , 
                   throw(metta_return(C))), 
            metta_return(H), 
            H=C)),
         (as_p1_exec(C,E))  )), 
      I, 
        ((
         (catch( ( mi__1_1_id(A,F)  ,
                   as_p1_exec(F,G) , 
                   B=G , 
                   throw(metta_return(C))), 
            metta_return(J), 
            J=C)),
         (as_p1_expr(C,I))  ))).


metta_other_asserted('&corelib',[iz,'if-decons-expr','MinimalMeTTaHelper']).
metta_other_asserted('&corelib',['@doc','if-decons-expr',['@desc',"Checks if first argument is non empty expression. If so gets tail and head from the first argument and returns forth argument using head and tail values. Returns fifth argument otherwise."],['@params',[['@param',"Expression to be deconstructed"],['@param',"Head variable"],['@param',"Tail variable"],['@param',"Template to return if first argument is a non-empty expression"],['@param',"Default value to return otherwise"]]],['@return',"Either template with head and tail replaced by values or default value"]]).
metta_other_asserted('&corelib',[:,'if-decons-expr',[->,'Expression','Variable','Variable','Atom','Atom','Atom']]).
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
metta_function_asserted('&corelib',['if-decons-expr',_atom,_head,_tail,_then,_else],['if-decons',_atom,_head,_tail,_then,_else]).
transpiler_clause_store('if-decons-expr',[5],1,['Expression','Variable','Variable','Atom','Atom'],'Atom',[x(noeval,eager,[]),x(doeval,eager,['Variable']),x(doeval,eager,['Variable']),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-decons-expr',_atom,_head,_tail,_then,_else],[if,['decons-ht',_atom,_head,_tail],_then,_else]).


'mi__1_5_if-decons-expr'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_if-decons-expr'(B,C,D,E,F,G) , 
  ci( true, 
    'if-decons-expr', 
    5, 
    [ 'if-decons-expr', B,C,D,E, 
      F], G,true,H).




'me__1_5_if-decons-expr'(B,C,D,E,F,G) :-  
  'mc__1_5_if-decons-expr'(B,C,D,E,F,G).




'mc__1_5_if-decons-expr'(A,B,C,D,E,F) :-  
  F =  
    ispeEnN( G, 
        ((
         (  ((
             (  ((
                 ('mi__1_3_decons-ht'(A,B,C,H),is_True(H))*->
                 (I=D)  )));
             (I=E)  ))),
         (as_p1_exec(I,G))  )), 
      J, 
        ((
         (  ((
             (  ((
                 ('mi__1_3_decons-ht'(A,B,C,H),is_True(H))*->
                 (K=D)  )));
             (K=E)  ))),
         (as_p1_expr(K,J))  ))).


metta_other_asserted('&corelib',[:,'if-decons',[->,'Expression','Variable','Variable','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',['ALT=',['if-decons-expr',_atom,_head,_tail,_then,_else],[function,[eval,['if-equal',_atom,[],[return,_else],[chain,['decons-atom',_atom],_list,['if-unify',_list,[_head,_tail],[return,_then],[return,_else]]]]]]]).
metta_other_asserted('&corelib',[iz,'if-error','MinimalMeTTaHelper']).
metta_other_asserted('&corelib',['@doc','if-error',['@desc',"Checks if first argument is an error atom. Returns second argument if so or third argument otherwise."],['@params',[['@param',"Atom to be checked for the error"],['@param',"Value to return if first argument is an error"],['@param',"Value to return otherwise"]]],['@return',"Second or third argument"]]).
metta_other_asserted('&corelib',[:,'if-error',[->,'Atom','Atom','Atom','Atom']]).
%  ;; Maybe Implement from Interpreter? But our transpiler should brilliantt enbought to make this awesome prolog code instead
metta_function_asserted('&corelib',['if-error',_atom,_then,_else],[function,[chain,[eval,['get-metatype',_atom]],_meta,[eval,['if-equal',_meta,'Expression',[eval,['if-equal',_atom,[],[return,_else],[chain,['decons-atom',_atom],_list,['if-unify',_list,[_head,_tail],[eval,['if-equal',_head,'Error',[return,_then],[return,_else]]],[return,_else]]]]],[return,_else]]]]]).
transpiler_clause_store('if-error',[3],1,['Atom','Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['if-error',_atom,_then,_else],[function,[let,_meta,['get-metatype',_atom],[eval,[if,['metta-equal',_meta,'Expression'],[eval,[if,['metta-equal',_atom,[]],[return,_else],[let,_list,['decons-atom',_atom],[if,['metta-unify',_list,[_head,_tail]],[eval,[if,['metta-equal',_head,'Error'],[return,_then],[return,_else]]],[return,_else]]]]],[return,_else]]]]]).


'mi__1_3_if-error'(B,C,D,E) :- 
  F='mc__1_3_if-error'(B,C,D,E) , 
  ci( true, 
    'if-error', 
    3, 
    ['if-error',B,C,D], E,true,F).




'me__1_3_if-error'(B,C,D,E) :-  
  'mc__1_3_if-error'(B,C,D,E).




'mc__1_3_if-error'(A,B,C,D) :-  
  D =  
    ispeEnN( E, 
      catch( ( as_p1_expr(A,F)  ,
               'mi__1_1_get-metatype'(F,G) , 
               H=G , 
                 ((
                  (  ((
                      (I=['metta-equal',H,'Expression'],is_True(I))*->
                      ((   ((
                            (  ((
                                (( as_p1_exec(A,J)  ,
                                   K=['metta-equal',J,[]] , 
                                   is_True(K)))*->
                                (throw(metta_return(C)),L=C)  )));
                            (( as_p1_expr(A,M)  ,
                               'mi__1_1_decons-atom'(M,N) , 
                               O=N , 
                                 ((
                                  (  ((
                                      (( P=[Q,R]  ,
                                         'mi__1_2_metta-unify'(O,P,S) , 
                                         is_True(S)))*->
                                      ((   ((
                                            (  ((
                                                (T=['metta-equal',Q,'Error'],is_True(T))*->
                                                (throw(metta_return(B)),U=B)  )));
                                            (throw(metta_return(C)),U=C)  ))  ,
                                         as_p1_expr(U,V) , 
                                         W=[eval,V] , 
                                         X=W))  )));
                                  (throw(metta_return(C)),X=C)  )) , 
                               L=X))  ))  ,
                         as_p1_expr(L,Y) , 
                         Z=[eval,Y] , 
                         A1=Z))  )));
                  (throw(metta_return(C)),A1=C)  )) , 
               as_p1_expr(A1,B1) , 
               mi__1_1_eval(B1,E)), 
        metta_return(C1), 
        C1=E), 
      D1, 
      catch( ( as_p1_expr(A,F)  ,
               'mi__1_1_get-metatype'(F,G) , 
               H=G , 
                 ((
                  (  ((
                      (I=['metta-equal',H,'Expression'],is_True(I))*->
                      ((   ((
                            (  ((
                                (( as_p1_exec(A,J)  ,
                                   K=['metta-equal',J,[]] , 
                                   is_True(K)))*->
                                (throw(metta_return(C)),L=C)  )));
                            (( as_p1_expr(A,M)  ,
                               'mi__1_1_decons-atom'(M,N) , 
                               O=N , 
                                 ((
                                  (  ((
                                      (( P=[Q,R]  ,
                                         'mi__1_2_metta-unify'(O,P,S) , 
                                         is_True(S)))*->
                                      ((   ((
                                            (  ((
                                                (T=['metta-equal',Q,'Error'],is_True(T))*->
                                                (throw(metta_return(B)),U=B)  )));
                                            (throw(metta_return(C)),U=C)  ))  ,
                                         as_p1_expr(U,V) , 
                                         W=[eval,V] , 
                                         X=W))  )));
                                  (throw(metta_return(C)),X=C)  )) , 
                               L=X))  ))  ,
                         as_p1_expr(L,Y) , 
                         Z=[eval,Y] , 
                         A1=Z))  )));
                  (throw(metta_return(C)),A1=C)  )) , 
               as_p1_expr(A1,B1) , 
               D1=[eval,B1]), 
        metta_return(E1), 
        E1=D1)).


metta_other_asserted('&corelib',[iz,'return-on-error','MinimalMeTTaHelper']).
metta_other_asserted('&corelib',['@doc','return-on-error',['@desc',"Returns first argument if it is Empty or an error. Returns second argument otherwise."],['@params',[['@param',"Previous evaluation result"],['@param',"Atom for further evaluation"]]],['@return',"Return previous result if it is an error or Empty or continue evaluation"]]).
metta_other_asserted('&corelib',[:,'return-on-error',[->,'Atom','Atom','Atom']]).
metta_function_asserted('&corelib',['return-on-error',_atom,_then],[function,[eval,['if-equal',_atom,'Empty',[return,[return,'Empty']],[eval,['if-error',_atom,[return,[return,_atom]],[return,_then]]]]]]).
transpiler_clause_store('return-on-error',[2],1,['Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['return-on-error',_atom,_then],[function,[eval,[if,['metta-equal',_atom,'Empty'],[return,[return,'Empty']],[eval,['if-error',_atom,[return,[return,_atom]],[return,_then]]]]]]).


'mi__1_2_return-on-error'(B,C,D) :- 
  E='mc__1_2_return-on-error'(B,C,D) , 
  ci(true,'return-on-error',2,['return-on-error',B,C],D,true,E).




'me__1_2_return-on-error'(B,C,D):-'mc__1_2_return-on-error'(B,C,D).




'mc__1_2_return-on-error'(A,B,C) :-  
  C =  
    ispeEnN( D, 
      catch( 
           ((
            (  ((
                (  ((
                    (( as_p1_exec(A,E)  ,
                       F=['metta-equal',E,'Empty'] , 
                       is_True(F)))*->
                    (throw(metta_return('Empty')),throw(metta_return('Empty')),G='Empty')  )));
                (( H =  
                     ispeEnNC( I, 
                       as_p1_exec(A,I), J,as_p1_expr(A,J), 
                       throw(metta_return(A)),throw(metta_return(A)))  ,
                   K =  
                     ispeEnNC( L, 
                       as_p1_exec(B,L), 
                       M, 
                       as_p1_expr(B,M), 
                       throw(metta_return(B))) , 
                   N=['if-error',A,H,K] , 
                   as_p1_expr(N,O) , 
                   P=[eval,O] , 
                   G=P))  ))),
            (mi__1_1_eval(G,D))  )), metta_return(Q),Q=D), 
      R, 
      catch( 
           ((
            (  ((
                (  ((
                    (( as_p1_exec(A,E)  ,
                       F=['metta-equal',E,'Empty'] , 
                       is_True(F)))*->
                    (throw(metta_return('Empty')),throw(metta_return('Empty')),G='Empty')  )));
                (( H =  
                     ispeEnNC( I, 
                       as_p1_exec(A,I), J,as_p1_expr(A,J), 
                       throw(metta_return(A)),throw(metta_return(A)))  ,
                   K =  
                     ispeEnNC( L, 
                       as_p1_exec(B,L), 
                       M, 
                       as_p1_expr(B,M), 
                       throw(metta_return(B))) , 
                   N=['if-error',A,H,K] , 
                   as_p1_expr(N,O) , 
                   P=[eval,O] , 
                   G=P))  ))),
            (R=[eval,G])  )), metta_return(S),S=R)).


%  ; Difference between `switch` and `case` is a way how they interpret `Empty`
%  ; result. `CaseOp` interprets first argument inside itself and then manually
%  ; checks whether result is empty. `switch` is interpreted in a context of
%  ; main interpreter. Minimal interpreter correctly passes `Empty` as an
%  ; argument to the `switch` but when `switch` is called from MeTTa interpreter
%  ; (for example user evaluates `!(switch (if-unify A B ok Empty) ...)` then
%  ; emptiness of the first argument is checked by interpreter and it will
%  ; break execution when `Empty` is returned.
metta_other_asserted('&corelib',[iz,switch,'MinimalMeTTaHelper']).
metta_other_asserted('&corelib',['@doc',switch,['@desc',"Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)"],['@params',[['@param',"Atom to be matched with patterns"],['@param',"Tuple of pairs mapping condition patterns to results"]]],['@return',"Result which corresponds to the pattern which is matched with the passed atom first"]]).
metta_other_asserted('&corelib',[:,switch,[->,'Atom','Atom','Atom']]).
metta_function_asserted('&corelib',[switch,_atom,_list],[case,[eval,_atom],_list]).
transpiler_clause_store(switch,[2],1,['%Undefined%','Expression'],'Atom',[x(doeval,eager,['%Undefined%']),x(noeval,eager,[])],x(noeval,lazy,[]),[switch,_atom,[]],[case,[eval,_atom],[]]).


mi__1_2_switch(B,C,D) :- 
  E=mc__1_2_switch(B,C,D) , 
  ci(true,switch,2,[switch,B,C],D,true,E).




me__1_2_switch(B,C,D):-mc__1_2_switch(B,C,D).




mc__1_2_switch(A,[],B) :-  
  B =  
    ispuU( C, 
      (mi__1_1_eval(A,D)*->E=D;E='Empty'),mc__1_0_empty(C)).


%  ; BEGIN - Yes, Douglas turned this sourcecode form into a a Value with the type Comment
metta_other_asserted('&corelib',[:,[[:,switch,[->,'%Undefined%','Expression','Atom']],[=,[switch,_atom,_cases],[function,[chain,['decons-atom',_cases],_list,[chain,[eval,['switch-internal',_atom,_list]],_res,[chain,[eval,['if-equal',_res,'NotReducible','Empty',_res]],_x,[return,_x]]]]]],[iz,'switch-internal','HelperMM'],['@doc','switch-internal',['@desc',"This function is being called inside switch function to test one of the cases and it calls switch once again if current condition is not met"],['@params',[['@param',"Atom (it will be evaluated)"],['@param',"Deconsed tuple of pairs mapping condition patterns to results"]]],['@return',"Result of evaluating of Atom bound to met condition"]],[=,['switch-internal',_atom,[[_pattern,_template],_tail]],[function,['if-unify',_atom,_pattern,[return,_template],[chain,[eval,[switch,_atom,_tail]],_ret,[return,_ret]]]]]],'Comment']).
%  ; ENDOF - Yes, Douglas turned this sourcecode form into a a Value with the type Comment
metta_other_asserted('&corelib',[iz,'is-function','MinimalMeTTaHelper']).
%  ; TODO: Type is used here, but there is no definition for the -> type
%  ; constructor for instance, thus in practice it matches because -> has
%  ; %Undefined% type. We need to assign proper type to -> and other type
%  ; constructors but it is not possible until we support vararg types.
metta_other_asserted('&corelib',['@doc','is-function',['@desc',"Function checks if input type is a function type"],['@params',[['@param',"Type atom"]]],['@return',"True if type is a function type, False - otherwise"]]).
metta_other_asserted('&corelib',[:,'is-function',[->,'Type','Bool']]).
%  ;; This impl is old and maybe not sufficiant?
metta_function_asserted('&corelib',['is-function',_type],[function,[chain,[eval,['get-metatype',_type]],_meta,[eval,[switch,[_type,_meta],[[[_type,'Expression'],[eval,['if-decons-expr',_type,_head,_tail,['if-unify',_head,->,[return,'True'],[return,'False']],[return,['Error',['is-function',_type],"is-function non-empty expression as an argument"]]]]],[[_type,_meta],[return,'False']]]]]]]).
transpiler_clause_store('is-function',[1],1,['Type'],'Bool',[x(doeval,eager,['Type'])],x(doeval,eager,[boolean]),['is-function',_type],[function,[let,_meta,['get-metatype',_type],[eval,[switch,[_type,_meta],[[[_type,'Expression'],[eval,['if-decons-expr',_type,_head,_tail,['if-unify',_head,->,[return,'True'],[return,'False']],[return,['Error',['is-function',_type],"is-function non-empty expression as an argument"]]]]],[[_type,_meta],[return,'False']]]]]]]).


'mi__1_1_is-function'(B,C) :- 
  D='mc__1_1_is-function'(B,C) , 
  ci(true,'is-function',1,['is-function',B],C,true,D).




'me__1_1_is-function'(B,C):-'mc__1_1_is-function'(B,C).




'mc__1_1_is-function'(A,B) :-  
  catch( 
     ( 'mi__1_1_get-metatype'(A,C)  ,
       D=C , 
       transpiler_apply( mc__1_1_, 
         A, 
         [A,D], 
         E, 
         [D], 
         [D], 
         [x(noeval,eager,[])], 
         [true], 
         [true]) , 
       F =  
         ispuU( G, 
           ( throw(metta_return('True'))  ,
             throw(metta_return('False')) , 
             G =  
               [ 'if-unify', H,->,'True','False'])) , 
       I =  
         ispuU( 
            [ 'Error', 
              ['is-function',A], 
              "is-function non-empty expression as an argument"], 
            throw( metta_return( [ 'Error', 
                                   ['is-function',A], 
                                   "is-function non-empty expression as an argument"]))) , 
       J =  
         [ 'if-decons-expr', A,H,_,F, 
           I] , 
       as_p1_expr(J,K) , 
       mi__1_1_eval(K,L) , 
       throw(metta_return('False')) , 
       transpiler_apply( mc__2_1_1_, 
         A, 
         [ [A,D], 
           'False'], 
         M, 
         [D,'False'], 
         [D,'False'], 
         [ x(noeval,eager,[]), 
           x(noeval,eager,[])], 
         [ true, 
           throw(metta_return('False'))], 
         [ true, 
           throw(metta_return('False'))]) , 
       N =  
         [ [ [A,'Expression'], 
             L], 
           M] , 
       O=[switch,E,N] , 
       as_p1_expr(O,P) , 
       mi__1_1_eval(P,B)), metta_return(Q),Q=B).


metta_other_asserted('&corelib',[iz,'type-cast','MeTTa']).
metta_other_asserted('&corelib',['@doc','type-cast',['@desc',"Casts atom passed as a first argument to the type passed as a second argument using space as a context"],['@params',[['@param',"Atom to be casted"],['@param',"Type to cast atom to"],['@param',"Context atomspace"]]],['@return',"Atom if casting is successful, (Error ... BadType) otherwise"]]).
metta_other_asserted('&corelib',[:,'type-cast',[->,'Atom','Atom','Atom','Atom']]).
%  ;; This implementation is old and may not be sufficient.
metta_other_asserted('&corelib',['ALT=',['type-cast',_atom,_type,_space],[function,[chain,[eval,['get-metatype',_atom]],_meta,[eval,['if-equal',_type,_meta,[return,_atom],[chain,[eval,['collapse-bind',[eval,['get-type',_atom,_space]]]],_collapsed,[chain,[eval,['map-atom',_collapsed,_pair,[eval,['first-from-pair',_pair]]]],_actual_types,[chain,[eval,['foldl-atom',_actual_types,'False',_a,_b,[eval,['match-type-or',_a,_b,_type]]]],_is_some_comp,[eval,[if,_is_some_comp,[return,_atom],[return,['Error',_atom,'BadType']]]]]]]]]]]]).
metta_other_asserted('&corelib',[iz,'match-types','MeTTa']).
metta_other_asserted('&corelib',['@doc','match-types',['@desc',"Checks if two types can be unified and returns third argument if so, fourth - otherwise"],['@params',[['@param',"First type"],['@param',"Second type"],['@param',"Atom to be returned if types can be unified"],['@param',"Atom to be returned if types cannot be unified"]]],['@return',"Third or fourth argument"]]).
metta_other_asserted('&corelib',[:,'match-types',[->,'Atom','Atom','Atom','Atom','Atom']]).
metta_function_asserted('&corelib',['match-types',_type1,_type2,_then,_else],[function,[eval,['if-equal',_type1,'%Undefined%',[return,_then],[eval,['if-equal',_type2,'%Undefined%',[return,_then],[eval,['if-equal',_type1,'Atom',[return,_then],[eval,['if-equal',_type2,'Atom',[return,_then],['if-unify',_type1,_type2,[return,_then],[return,_else]]]]]]]]]]]).
transpiler_clause_store('match-types',[4],1,['Atom','Atom','Atom','Atom'],'Atom',[x(noeval,lazy,[]),x(noeval,lazy,[]),x(noeval,lazy,[]),x(noeval,lazy,[])],x(noeval,lazy,[]),['match-types',_type1,_type2,_then,_else],[function,[eval,[if,['metta-equal',_type1,'%Undefined%'],[return,_then],[eval,[if,['metta-equal',_type2,'%Undefined%'],[return,_then],[eval,[if,['metta-equal',_type1,'Atom'],[return,_then],[eval,[if,['metta-equal',_type2,'Atom'],[return,_then],[if,['metta-unify',_type1,_type2],[return,_then],[return,_else]]]]]]]]]]]).


'mi__1_4_match-types'(B,C,D,E,F) :- 
  G =  
    'mc__1_4_match-types'(B,C,D,E,F) , 
  ci( true, 
    'match-types', 
    4, 
    [ 'match-types', B,C,D,E], F,true,G).




'me__1_4_match-types'(B,C,D,E,F) :-  
  'mc__1_4_match-types'(B,C,D,E,F).




'mc__1_4_match-types'(A,B,C,D,E) :-  
  E =  
    ispeEnN( F, 
      catch( (   ((
                  (  ((
                      (( as_p1_exec(A,G)  ,
                         H=['metta-equal',G,'%Undefined%'] , 
                         is_True(H)))*->
                      (throw(metta_return(C)),I=C)  )));
                  ((   ((
                        (  ((
                            (( as_p1_exec(B,J)  ,
                               K=['metta-equal',J,'%Undefined%'] , 
                               is_True(K)))*->
                            (throw(metta_return(C)),L=C)  )));
                        ((   ((
                              (  ((
                                  (( as_p1_exec(A,M)  ,
                                     N=['metta-equal',M,'Atom'] , 
                                     is_True(N)))*->
                                  (throw(metta_return(C)),O=C)  )));
                              ((   ((
                                    (  ((
                                        (( as_p1_exec(B,P)  ,
                                           Q=['metta-equal',P,'Atom'] , 
                                           is_True(Q)))*->
                                        (throw(metta_return(C)),R=C)  )));
                                    (  ((
                                        (  ((
                                            (  ((
                                                (( as_p1_expr(A,S)  ,
                                                   as_p1_expr(B,T) , 
                                                   'mi__1_2_metta-unify'(S,T,U) , 
                                                   is_True(U)))*->
                                                (throw(metta_return(C)),V=C)  )));
                                            (throw(metta_return(D)),V=D)  ))),
                                        (R=V)  )))  ))  ,
                                 as_p1_expr(R,W) , 
                                 X=[eval,W] , 
                                 O=X))  ))  ,
                           as_p1_expr(O,Y) , 
                           Z=[eval,Y] , 
                           L=Z))  ))  ,
                     as_p1_expr(L,A1) , 
                     B1=[eval,A1] , 
                     I=B1))  ))  ,
               as_p1_expr(I,C1) , 
               mi__1_1_eval(C1,F)), 
        metta_return(D1), 
        D1=F), 
      E1, 
      catch( (   ((
                  (  ((
                      (( as_p1_exec(A,G)  ,
                         H=['metta-equal',G,'%Undefined%'] , 
                         is_True(H)))*->
                      (throw(metta_return(C)),I=C)  )));
                  ((   ((
                        (  ((
                            (( as_p1_exec(B,J)  ,
                               K=['metta-equal',J,'%Undefined%'] , 
                               is_True(K)))*->
                            (throw(metta_return(C)),L=C)  )));
                        ((   ((
                              (  ((
                                  (( as_p1_exec(A,M)  ,
                                     N=['metta-equal',M,'Atom'] , 
                                     is_True(N)))*->
                                  (throw(metta_return(C)),O=C)  )));
                              ((   ((
                                    (  ((
                                        (( as_p1_exec(B,P)  ,
                                           Q=['metta-equal',P,'Atom'] , 
                                           is_True(Q)))*->
                                        (throw(metta_return(C)),R=C)  )));
                                    (  ((
                                        (  ((
                                            (  ((
                                                (( as_p1_expr(A,S)  ,
                                                   as_p1_expr(B,T) , 
                                                   'mi__1_2_metta-unify'(S,T,U) , 
                                                   is_True(U)))*->
                                                (throw(metta_return(C)),V=C)  )));
                                            (throw(metta_return(D)),V=D)  ))),
                                        (R=V)  )))  ))  ,
                                 as_p1_expr(R,W) , 
                                 X=[eval,W] , 
                                 O=X))  ))  ,
                           as_p1_expr(O,Y) , 
                           Z=[eval,Y] , 
                           L=Z))  ))  ,
                     as_p1_expr(L,A1) , 
                     B1=[eval,A1] , 
                     I=B1))  ))  ,
               as_p1_expr(I,C1) , 
               E1=[eval,C1]), 
        metta_return(F1), 
        F1=E1)).


%  ; Helper MinimalMeTTa
metta_other_asserted('&corelib',[iz,'first-from-pair','MinimalMeTTaHelper']).
metta_other_asserted('&corelib',['@doc','first-from-pair',['@desc',"Gets a pair as a first argument and returns first atom from pair"],['@params',[['@param',"Pair"]]],['@return',"First atom from a pair"]]).
metta_function_asserted('&corelib',['first-from-pair',_pair],[function,['if-unify',_pair,[_first,_second],[return,_first],[return,['Error',['first-from-pair',_pair],"incorrect pair format"]]]]).
transpiler_clause_store('first-from-pair',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['first-from-pair',_pair],[function,[if,['metta-unify',_pair,[_first,_second]],[return,_first],[return,['Error',['first-from-pair',_pair],"incorrect pair format"]]]]).


'mi__1_1_first-from-pair'(B,C) :- 
  D='mc__1_1_first-from-pair'(B,C) , 
  ci(true,'first-from-pair',1,['first-from-pair',B],C,true,D).




'me__1_1_first-from-pair'(B,C):-'mc__1_1_first-from-pair'(B,C).




'mc__1_1_first-from-pair'(A,B) :-  
  catch( 
       ((
        (  ((
            (( C=[D,_]  ,
               'mi__1_2_metta-unify'(A,C,E) , 
               is_True(E)))*->
            (throw(metta_return(D)),B=D)  )));
        (  ((
            (throw( metta_return(['Error',['first-from-pair',A],"incorrect pair format"]))),
            (B =  
               [ 'Error', 
                 ['first-from-pair',A], 
                 "incorrect pair format"])  )))  )), metta_return(F),F=B).


metta_other_asserted('&corelib',[iz,'match-type-or','HelperMM']).
metta_other_asserted('&corelib',['@doc','match-type-or',['@desc',"Checks if two types (second and third arguments) can be unified and returns result of OR operation between first argument and type checking result"],['@params',[['@param',"Boolean value"],['@param',"First type"],['@param',"Second type"]]],['@return',"True or False"]]).
metta_function_asserted('&corelib',['match-type-or',_folded,_next,_type],[function,[chain,[eval,['match-types',_next,_type,'True','False']],_matched,[chain,[eval,[or,_folded,_matched]],_or,[return,_or]]]]).
transpiler_clause_store('match-type-or',[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['match-type-or',_folded,_next,_type],[function,[let,_matched,['match-types',_next,_type,'True','False'],[let,_or,[or,_folded,_matched],[return,_or]]]]).


'mi__1_3_match-type-or'(B,C,D,E) :- 
  F='mc__1_3_match-type-or'(B,C,D,E) , 
  ci( true, 
    'match-type-or', 
    3, 
    ['match-type-or',B,C,D], E,true,F).




'me__1_3_match-type-or'(B,C,D,E) :-  
  'mc__1_3_match-type-or'(B,C,D,E).




'mc__1_3_match-type-or'(A,B,C,D) :-  
  catch( 
     ( E=ispu(B)  ,
       F=ispu(C) , 
       G=ispu('True') , 
       H=ispu('False') , 
       'mi__1_4_match-types'(E,F,G,H,I) , 
       as_p1_exec(I,J) , 
       K=J , 
       is_True(A)*->L='True';L=K , 
       D=L , 
       throw(metta_return(D))), metta_return(M),M=D).


metta_other_asserted('&corelib',[iz,'filter-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','filter-atom',['@desc',"Function takes list of atoms (first argument), variable (second argument) and filter predicate (third argument) and returns list with items which passed filter. E.g. (filter-atom (1 2 3 4) $v (eval (> $v 2))) will give (3 4)"],['@params',[['@param',"List of atoms"],['@param',"Variable"],['@param',"Filter predicate"]]],['@return',"Filtered list"]]).
metta_other_asserted('&corelib',[:,'filter-atom',[->,'Expression','Variable','Atom','Expression']]).
metta_function_asserted('&corelib',['filter-atom',_list,_var,_filter],[function,[eval,['if-decons-expr',_list,_head,_tail,[chain,[eval,['filter-atom',_tail,_var,_filter]],_tail_filtered,[chain,[eval,['atom-subst',_head,_var,_filter]],_filter_expr,[chain,_filter_expr,_is_filtered,[eval,[if,_is_filtered,[chain,['cons-atom',_head,_tail_filtered],_res,[return,_res]],[return,_tail_filtered]]]]]],[return,[]]]]]).
transpiler_clause_store('filter-atom',[3],1,['Expression','Variable','Atom'],'Expression',[x(noeval,eager,[]),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,eager,[]),['filter-atom',_list,_var,_filter],[function,[eval,[if,['decons-ht',_list,_head,_tail],[let,_tail_filtered,['filter-atom',_tail,_var,_filter],[let,_filter_expr,['atom-subst',_head,_var,_filter],[let,_is_filtered,_filter_expr,[eval,[if,_is_filtered,[let,_res,['cons-atom',_head,_tail_filtered],[return,_res]],[return,_tail_filtered]]]]]],[return,[]]]]]).


'mi__1_3_filter-atom'(B,C,D,E) :- 
  F='mc__1_3_filter-atom'(B,C,D,E) , 
  ci( true, 
    'filter-atom', 
    3, 
    ['filter-atom',B,C,D], E,true,F).




'me__1_3_filter-atom'(B,C,D,E) :-  
  'mc__1_3_filter-atom'(B,C,D,E).




'mc__1_3_filter-atom'(A,B,C,D) :-  
  catch(   ((
            (  ((
                (  ((
                    ('mi__1_3_decons-ht'(A,E,F,G),is_True(G))*->
                    (( 'mi__1_3_filter-atom'(F,B,C,H)  ,
                       I=H , 
                       J=ispu(E) , 
                       'mi__1_3_atom-subst'(J,B,C,K) , 
                       as_p1_exec(K,L) , 
                       M=L , 
                       N=M , 
                         ((
                          (  ((
                              (is_True(N))*->
                              (( 'mi__1_2_cons-atom'(E,I,O)  ,
                                 P=O , 
                                 throw(metta_return(P)) , 
                                 Q=P))  )));
                          (throw(metta_return(I)),Q=I)  )) , 
                       R=[eval,Q] , 
                       S=R))  )));
                (throw(metta_return([])),S=[])  ))),
            (mi__1_1_eval(S,D))  )), 
    metta_return(T), 
    T=D).


metta_other_asserted('&corelib',[iz,'map-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','map-atom',['@desc',"Function takes list of atoms (first argument), variable to be used inside (second variable) and an expression which will be evaluated for each atom in list (third argument). Expression should contain variable. So e.g. (map-atom (1 2 3 4) $v (eval (+ $v 1))) will give (2 3 4 5)"],['@params',[['@param',"List of atoms"],['@param',"Variable name"],['@param',"Template using variable"]]],['@return',"Result of evaluating template for each atom in a list"]]).
metta_other_asserted('&corelib',[:,'map-atom',[->,'Expression','Variable','Atom','Expression']]).
metta_function_asserted('&corelib',['map-atom',_list,_var,_map],[function,[eval,['if-decons-expr',_list,_head,_tail,[chain,[eval,['map-atom',_tail,_var,_map]],_tail_mapped,[chain,[eval,['atom-subst',_head,_var,_map]],_map_expr,[chain,_map_expr,_head_mapped,[chain,['cons-atom',_head_mapped,_tail_mapped],_res,[return,_res]]]]],[return,[]]]]]).
transpiler_clause_store('map-atom',[3],1,['Expression','Variable','Atom'],'Expression',[x(noeval,eager,[]),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,eager,[]),['map-atom',_list,_var,_map],[function,[eval,[if,['decons-ht',_list,_head,_tail],[let,_tail_mapped,['map-atom',_tail,_var,_map],[let,_map_expr,['atom-subst',_head,_var,_map],[let,_head_mapped,_map_expr,[let,_res,['cons-atom',_head_mapped,_tail_mapped],[return,_res]]]]],[return,[]]]]]).


'mi__1_3_map-atom'(B,C,D,E) :- 
  F='mc__1_3_map-atom'(B,C,D,E) , 
  ci( true, 
    'map-atom', 
    3, 
    ['map-atom',B,C,D], E,true,F).




'me__1_3_map-atom'(B,C,D,E) :-  
  'mc__1_3_map-atom'(B,C,D,E).




'mc__1_3_map-atom'(A,B,C,D) :-  
  catch(   ((
            (  ((
                (  ((
                    ('mi__1_3_decons-ht'(A,E,F,G),is_True(G))*->
                    (( 'mi__1_3_map-atom'(F,B,C,H)  ,
                       I=H , 
                       J=ispu(E) , 
                       'mi__1_3_atom-subst'(J,B,C,K) , 
                       as_p1_exec(K,L) , 
                       M=L , 
                       N=M , 
                       'mi__1_2_cons-atom'(N,I,O) , 
                       P=O , 
                       throw(metta_return(P)) , 
                       Q=P))  )));
                (throw(metta_return([])),Q=[])  ))),
            (mi__1_1_eval(Q,D))  )), 
    metta_return(R), 
    R=D).


metta_other_asserted('&corelib',[iz,'foldl-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','foldl-atom',['@desc',"Function takes list of values (first argument), initial value (second argument) and operation (fifth argument) and applies it consequently to the list of values, using init value as a start. It also takes two variables (third and fourth argument) to use them inside"],['@params',[['@param',"List of values"],['@param',"Init value"],['@param',"Variable"],['@param',"Variable"],['@param',"Operation"]]],['@return',"Result of applying operation to the list of values"]]).
metta_other_asserted('&corelib',[:,'foldl-atom',[->,'Expression','Atom','Variable','Variable','Atom','Atom']]).
metta_function_asserted('&corelib',['foldl-atom',_list,_init,_a,_b,_op],[function,[eval,['if-decons-expr',_list,_head,_tail,[chain,[eval,['atom-subst',_init,_a,_op]],_op_init,[chain,[eval,['atom-subst',_head,_b,_op_init]],_op_head,[chain,_op_head,_head_folded,[chain,[eval,['foldl-atom',_tail,_head_folded,_a,_b,_op]],_res,[return,_res]]]]],[return,_init]]]]).
transpiler_clause_store('foldl-atom',[5],1,['Expression','Atom','Variable','Variable','Atom'],'Atom',[x(noeval,eager,[]),x(noeval,lazy,[]),x(doeval,eager,['Variable']),x(doeval,eager,['Variable']),x(noeval,lazy,[])],x(noeval,lazy,[]),['foldl-atom',_list,_init,_a,_b,_op],[function,[eval,[if,['decons-ht',_list,_head,_tail],[let,_op_init,['atom-subst',_init,_a,_op],[let,_op_head,['atom-subst',_head,_b,_op_init],[let,_head_folded,_op_head,[let,_res,['foldl-atom',_tail,_head_folded,_a,_b,_op],[return,_res]]]]],[return,_init]]]]).


'mi__1_5_foldl-atom'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_foldl-atom'(B,C,D,E,F,G) , 
  ci( true, 
    'foldl-atom', 
    5, 
    [ 'foldl-atom', B,C,D,E, 
      F], G,true,H).




'me__1_5_foldl-atom'(B,C,D,E,F,G) :-  
  'mc__1_5_foldl-atom'(B,C,D,E,F,G).




'mc__1_5_foldl-atom'(A,B,C,D,E,F) :-  
  F =  
    ispeEnN( G, 
      catch( 
         (   ((
              (  ((
                  ('mi__1_3_decons-ht'(A,H,I,J),is_True(J))*->
                  (( 'mi__1_3_atom-subst'(B,C,E,K)  ,
                     as_p1_exec(K,L) , 
                     M=L , 
                     N=ispu(H) , 
                     O=ispu(M) , 
                     'mi__1_3_atom-subst'(N,D,O,P) , 
                     as_p1_exec(P,Q) , 
                     R=Q , 
                     S=R , 
                     T=ispu(S) , 
                     'mi__1_5_foldl-atom'(I,T,C,D,E,U) , 
                     as_p1_exec(U,V) , 
                     W=V , 
                     throw(metta_return(W)) , 
                     X=W))  )));
              (throw(metta_return(B)),X=B)  ))  ,
           as_p1_expr(X,Y) , 
           mi__1_1_eval(Y,G)), metta_return(Z),Z=G), 
      A1, 
      catch( 
         (   ((
              (  ((
                  ('mi__1_3_decons-ht'(A,H,I,J),is_True(J))*->
                  (( 'mi__1_3_atom-subst'(B,C,E,K)  ,
                     as_p1_exec(K,L) , 
                     M=L , 
                     N=ispu(H) , 
                     O=ispu(M) , 
                     'mi__1_3_atom-subst'(N,D,O,P) , 
                     as_p1_exec(P,Q) , 
                     R=Q , 
                     S=R , 
                     T=ispu(S) , 
                     'mi__1_5_foldl-atom'(I,T,C,D,E,U) , 
                     as_p1_exec(U,V) , 
                     W=V , 
                     throw(metta_return(W)) , 
                     X=W))  )));
              (throw(metta_return(B)),X=B)  ))  ,
           as_p1_expr(X,Y) , 
           A1=[eval,Y]), metta_return(B1),B1=A1)).


metta_other_asserted('&corelib',[iz,'separate-errors','MinimalMeTTaHelper']).
metta_other_asserted('&corelib',[:,'separate-errors',[->,'Expression','Expression','Expression']]).
metta_function_asserted('&corelib',['separate-errors',_succ_err,_res],[function,['if-unify',_succ_err,[_suc,_err],['if-unify',_res,[_a,_b],[eval,['if-error',_a,[chain,['cons-atom',_res,_err],_err_39,[return,[_suc,_err_39]]],[chain,['cons-atom',_res,_suc],_suc_39,[return,[_suc_39,_err]]]]],[return,_succ_err]],[return,_succ_err]]]).
transpiler_clause_store('separate-errors',[2],1,['Expression','Expression'],'Expression',[x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['separate-errors',_succ_err,_res],[function,[if,['metta-unify',_succ_err,[_suc,_err]],[if,['metta-unify',_res,[_a,_b]],[eval,['if-error',_a,[let,_err_39,['cons-atom',_res,_err],[return,[_suc,_err_39]]],[let,_suc_39,['cons-atom',_res,_suc],[return,[_suc_39,_err]]]]],[return,_succ_err]],[return,_succ_err]]]).


'mi__1_2_separate-errors'(B,C,D) :- 
  E='mc__1_2_separate-errors'(B,C,D) , 
  ci(true,'separate-errors',2,['separate-errors',B,C],D,true,E).




'me__1_2_separate-errors'(B,C,D):-'mc__1_2_separate-errors'(B,C,D).




'mc__1_2_separate-errors'(A,B,C) :-  
  catch( 
       ((
        (  ((
            (( D=[E,F]  ,
               'mi__1_2_metta-unify'(A,D,G) , 
               is_True(G)))*->
            (  ((
                (  ((
                    (  ((
                        (( H=[I,_]  ,
                           'mi__1_2_metta-unify'(B,H,J) , 
                           is_True(J)))*->
                        (( K=ispu(I)  ,
                           L =  
                             ispeEnNC( M, 
                                 ((
                                  (transpiler_apply( mc__1_1_, 
                                     E, 
                                     [E,N], 
                                     M, 
                                     [N], 
                                     [N], 
                                     [x(noeval,eager,[])], 
                                     [true], 
                                     [true])),
                                  (throw(metta_return(M)))  )), 
                               O, 
                               O=[E,N],throw(metta_return(O)), 
                               'mi__1_2_cons-atom'(B,F,P),N=P) , 
                           Q =  
                             ispeEnNC( R, 
                                 ((
                                  (transpiler_apply( mc__1_1_, 
                                     S, 
                                     [S,F], 
                                     R, 
                                     [F], 
                                     [F], 
                                     [x(noeval,eager,[])], 
                                     [true], 
                                     [true])),
                                  (throw(metta_return(R)))  )), 
                               T, 
                               T=[S,F],throw(metta_return(T)), 
                               'mi__1_2_cons-atom'(B,E,U),S=U) , 
                           V=['if-error',K,L,Q] , 
                           as_p1_expr(V,W) , 
                           mi__1_1_eval(W,X) , 
                           Y=X))  )));
                    (throw(metta_return(A)),Y=A)  ))),
                (C=Y)  )))  )));
        (throw(metta_return(A)),C=A)  )), metta_return(Z),Z=C).


metta_other_asserted('&corelib',[iz,'check-alternatives','MinimalMeTTaHelper']).
metta_function_asserted('&corelib',['check-alternatives',_atom],[function,[chain,['collapse-bind',_atom],_collapsed,[chain,[eval,['foldl-atom',_collapsed,[[],[]],_succ_err,_res,[eval,['separate-errors',_succ_err,_res]]]],_separated,['if-unify',_separated,[_success,_error],[chain,[eval,['if-equal',_success,[],_error,_success]],_filtered,[chain,['superpose-bind',_filtered],_ret,[return,_ret]]],[return,['Error',['check-alternatives',_atom],"list of results was not filtered correctly"]]]]]]).
transpiler_clause_store('check-alternatives',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['check-alternatives',_atom],[function,[let,_collapsed,['collapse-bind',_atom],[let,_separated,['foldl-atom',_collapsed,[[],[]],_succ_err,_res,[eval,['separate-errors',_succ_err,_res]]],[if,['metta-unify',_separated,[_success,_error]],[let,_filtered,[if,['metta-equal',_success,[]],_error,_success],[let,_ret,['superpose-bind',_filtered],[return,_ret]]],[return,['Error',['check-alternatives',_atom],"list of results was not filtered correctly"]]]]]]).


'mi__1_1_check-alternatives'(B,C) :- 
  D='mc__1_1_check-alternatives'(B,C) , 
  ci(true,'check-alternatives',1,['check-alternatives',B],C,true,D).




'me__1_1_check-alternatives'(B,C):-'mc__1_1_check-alternatives'(B,C).




'mc__1_1_check-alternatives'(A,B) :-  
  catch( 
     ( C=['collapse-bind',A]  ,
       D=C , 
       E =  
         ispuU(F,F=[[],[]]) , 
       G =  
         ispeEnNC( H, 
           mi__1_1_eval(I,H), J,J=[eval,I], 
           I=['separate-errors',K,L]) , 
       'mi__1_5_foldl-atom'(D,E,K,L,G,M) , 
       as_p1_exec(M,N) , 
       O=N , 
         ((
          (  ((
              (( P=[Q,R]  ,
                 'mi__1_2_metta-unify'(O,P,S) , 
                 is_True(S)))*->
              ((   ((
                    (  ((
                        (T=['metta-equal',Q,[]],is_True(T))*->
                        (U=R)  )));
                    (U=Q)  ))  ,
                 V=U , 
                 W=['superpose-bind',V] , 
                 X=W , 
                 throw(metta_return(X)) , 
                 B=X))  )));
          (  ((
              (throw( metta_return( [ 'Error', 
                                      ['check-alternatives',A], 
                                      "list of results was not filtered correctly"]))),
              (B =  
                 [ 'Error', 
                   ['check-alternatives',A], 
                   "list of results was not filtered correctly"])  )))  ))), metta_return(Y),Y=B).


metta_other_asserted('&corelib',[iz,interpret,'MeTTa']).
metta_function_asserted('&corelib',[interpret,_atom,_type,_space],[function,[chain,[eval,['get-metatype',_atom]],_meta,[eval,['if-equal',_type,'Atom',[return,_atom],[eval,['if-equal',_type,_meta,[return,_atom],[eval,[switch,[_type,_meta],[[[_type,'Variable'],[return,_atom]],[[_type,'Symbol'],[chain,[eval,['type-cast',_atom,_type,_space]],_ret,[return,_ret]]],[[_type,'Grounded'],[chain,[eval,['type-cast',_atom,_type,_space]],_ret,[return,_ret]]],[[_type,'Expression'],[chain,[eval,['check-alternatives',[eval,['interpret-expression',_atom,_type,_space]]]],_ret,[return,_ret]]]]]]]]]]]]).
transpiler_clause_store(interpret,[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),[interpret,_atom,_type,_space],[function,[let,_meta,['get-metatype',_atom],[eval,[if,['metta-equal',_type,'Atom'],[return,_atom],[eval,[if,['metta-equal',_type,_meta],[return,_atom],[eval,[switch,[_type,_meta],[[[_type,'Variable'],[return,_atom]],[[_type,'Symbol'],[let,_ret,['type-cast',_atom,_type,_space],[return,_ret]]],[[_type,'Grounded'],[let,_ret,['type-cast',_atom,_type,_space],[return,_ret]]],[[_type,'Expression'],[let,_ret,['check-alternatives',[eval,['interpret-expression',_atom,_type,_space]]],[return,_ret]]]]]]]]]]]]).


mi__1_3_interpret(B,C,D,E) :- 
  F=mc__1_3_interpret(B,C,D,E) , 
  ci(true,interpret,3,[interpret,B,C,D],E,true,F).




me__1_3_interpret(B,C,D,E) :-  
  mc__1_3_interpret(B,C,D,E).




mc__1_3_interpret(A,B,C,D) :-  
  catch( 
     ( 'mi__1_1_get-metatype'(A,E)  ,
       F=E , 
         ((
          (  ((
              (G=['metta-equal',B,'Atom'],is_True(G))*->
              (throw(metta_return(A)),H=A)  )));
          ((   ((
                (  ((
                    (I=['metta-equal',B,F],is_True(I))*->
                    (throw(metta_return(A)),J=A)  )));
                (( transpiler_apply( mc__1_1_, 
                     B, 
                     [B,F], 
                     K, 
                     [F], 
                     [F], 
                     [x(noeval,eager,[])], 
                     [true], 
                     [true])  ,
                   throw(metta_return(A)) , 
                   L=['type-cast',A,B,C] , 
                   M=L , 
                   throw(metta_return(M)) , 
                   transpiler_apply( mc__2_1_1_, 
                     B, 
                     [ [B,'Symbol'], 
                       M], 
                     N, 
                     ['Symbol',M], 
                     ['Symbol',M], 
                     [ x(noeval,eager,[]), 
                       x(noeval,eager,[])], 
                     [ true, 
                       ( L=['type-cast',A,B,C]  ,
                         M=L , 
                         throw(metta_return(M)))], 
                     [ true, 
                       ( L=['type-cast',A,B,C]  ,
                         M=L , 
                         throw(metta_return(M)))]) , 
                   O=['type-cast',A,B,C] , 
                   M=O , 
                   throw(metta_return(M)) , 
                   transpiler_apply( mc__2_1_1_, 
                     B, 
                     [ [B,'Grounded'], 
                       M], 
                     P, 
                     ['Grounded',M], 
                     ['Grounded',M], 
                     [ x(noeval,eager,[]), 
                       x(noeval,eager,[])], 
                     [ true, 
                       ( O=['type-cast',A,B,C]  ,
                         M=O , 
                         throw(metta_return(M)))], 
                     [ true, 
                       ( O=['type-cast',A,B,C]  ,
                         M=O , 
                         throw(metta_return(M)))]) , 
                   Q=['interpret-expression',A,B,C] , 
                   mi__1_1_eval(Q,R) , 
                   'mi__1_1_check-alternatives'(R,S) , 
                   M=S , 
                   throw(metta_return(M)) , 
                   transpiler_apply( mc__2_1_1_, 
                     B, 
                     [ [B,'Expression'], 
                       M], 
                     T, 
                     ['Expression',M], 
                     ['Expression',M], 
                     [ x(noeval,eager,[]), 
                       x(noeval,eager,[])], 
                     [ true, 
                       ( Q=['interpret-expression',A,B,C]  ,
                         mi__1_1_eval(Q,R) , 
                         'mi__1_1_check-alternatives'(R,S) , 
                         M=S , 
                         throw(metta_return(M)))], 
                     [ true, 
                       ( Q=['interpret-expression',A,B,C]  ,
                         mi__1_1_eval(Q,R) , 
                         'mi__1_1_check-alternatives'(R,S) , 
                         M=S , 
                         throw(metta_return(M)))]) , 
                   U =  
                     [ [ [B,'Variable'], 
                         A], N,P,T] , 
                   V=[switch,K,U] , 
                   as_p1_expr(V,W) , 
                   X=[eval,W] , 
                   J=X))  ))  ,
             Y=[eval,J] , 
             H=Y))  )) , 
       mi__1_1_eval(H,D)), metta_return(Z),Z=D).


metta_other_asserted('&corelib',[iz,'interpret-expression','MinimalMeTTaHelper']).
metta_function_asserted('&corelib',['interpret-expression',_atom,_type,_space],[function,[eval,['if-decons',_atom,_op,_args,[chain,[eval,['get-type',_op,_space]],_op_type,[chain,[eval,['is-function',_op_type]],_is_func,['if-unify',_is_func,'True',[chain,[eval,['interpret-func',_atom,_op_type,_type,_space]],_reduced_atom,[chain,[eval,['metta-call',_reduced_atom,_type,_space]],_ret,[return,_ret]]],[chain,[eval,['interpret-tuple',_atom,_space]],_reduced_atom,[chain,[eval,['metta-call',_reduced_atom,_type,_space]],_ret,[return,_ret]]]]]],[chain,[eval,['type-cast',_atom,_type,_space]],_ret,[return,_ret]]]]]).
transpiler_clause_store('interpret-expression',[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-expression',_atom,_type,_space],[function,[eval,[if,['decons-ht',_atom,_op,_args],[let,_op_type,['get-type',_op,_space],[let,_is_func,['is-function',_op_type],[if,['metta-unify',_is_func,'True'],[let,_reduced_atom,['interpret-func',_atom,_op_type,_type,_space],[let,_ret,['metta-call',_reduced_atom,_type,_space],[return,_ret]]],[let,_reduced_atom,['interpret-tuple',_atom,_space],[let,_ret,['metta-call',_reduced_atom,_type,_space],[return,_ret]]]]]],[let,_ret,['type-cast',_atom,_type,_space],[return,_ret]]]]]).


'mi__1_3_interpret-expression'(B,C,D,E) :- 
  F='mc__1_3_interpret-expression'(B,C,D,E) , 
  ci( true, 
    'interpret-expression', 
    3, 
    ['interpret-expression',B,C,D], E,true,F).




'me__1_3_interpret-expression'(B,C,D,E) :-  
  'mc__1_3_interpret-expression'(B,C,D,E).




'mc__1_3_interpret-expression'(A,B,C,D) :-  
  catch( 
       ((
        (  ((
            (  ((
                ('mi__1_3_decons-ht'(A,E,_,F),is_True(F))*->
                (( G=['get-type',E,C]  ,
                   H=G , 
                   'mi__1_1_is-function'(H,I) , 
                   J=I , 
                     ((
                      (  ((
                          ('mi__1_2_metta-unify'(J,'True',K),is_True(K))*->
                          (( 'mi__1_4_interpret-func'(A,H,B,C,L)  ,
                             M=L , 
                             'mi__1_3_metta-call'(M,B,C,N) , 
                             O=N , 
                             throw(metta_return(O)) , 
                             P=O))  )));
                      (( 'mi__1_2_interpret-tuple'(A,C,Q)  ,
                         M=Q , 
                         'mi__1_3_metta-call'(M,B,C,R) , 
                         O=R , 
                         throw(metta_return(O)) , 
                         P=O))  )) , 
                   S=P))  )));
            (( T=['type-cast',A,B,C]  ,
               O=T , 
               throw(metta_return(O)) , 
               S=O))  ))),
        (mi__1_1_eval(S,D))  )), metta_return(U),U=D).


metta_other_asserted('&corelib',[iz,'interpret-func','MinimalMeTTaHelper']).
metta_function_asserted('&corelib',['interpret-func',_expr,_type,_ret_type,_space],[function,[eval,['if-decons',_expr,_op,_args,[chain,[eval,[interpret,_op,_type,_space]],_reduced_op,[eval,['return-on-error',_reduced_op,[eval,['if-decons',_type,_arrow,_arg_types,[chain,[eval,['interpret-args',_expr,_args,_arg_types,_ret_type,_space]],_reduced_args,[eval,['return-on-error',_reduced_args,[chain,['cons-atom',_reduced_op,_reduced_args],_r,[return,_r]]]]],[return,['Error',_type,"Function type expected"]]]]]]],[return,['Error',_expr,"Non-empty expression atom is expected"]]]]]).
transpiler_clause_store('interpret-func',[4],1,['Any','Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-func',_expr,_type,_ret_type,_space],[function,[eval,[if,['decons-ht',_expr,_op,_args],[let,_reduced_op,[interpret,_op,_type,_space],[eval,['return-on-error',_reduced_op,[eval,[if,['decons-ht',_type,_arrow,_arg_types],[let,_reduced_args,['interpret-args',_expr,_args,_arg_types,_ret_type,_space],[eval,['return-on-error',_reduced_args,[let,_r,['cons-atom',_reduced_op,_reduced_args],[return,_r]]]]],[return,['Error',_type,"Function type expected"]]]]]]],[return,['Error',_expr,"Non-empty expression atom is expected"]]]]]).


'mi__1_4_interpret-func'(B,C,D,E,F) :- 
  G =  
    'mc__1_4_interpret-func'(B,C,D,E,F) , 
  ci( true, 
    'interpret-func', 
    4, 
    [ 'interpret-func', B,C,D,E], F,true,G).




'me__1_4_interpret-func'(B,C,D,E,F) :-  
  'mc__1_4_interpret-func'(B,C,D,E,F).




'mc__1_4_interpret-func'(A,B,C,D,E) :-  
  catch( 
       ((
        (  ((
            (  ((
                ('mi__1_3_decons-ht'(A,F,G,H),is_True(H))*->
                (( mi__1_3_interpret(F,B,D,I)  ,
                   J=I , 
                   K=ispu(J) , 
                   L =  
                     ispeEnNC( M, 
                       mi__1_1_eval(N,M), O,O=[eval,N], 
                         ((
                          (  ((
                              ('mi__1_3_decons-ht'(B,_,P,Q),is_True(Q))*->
                              (( 'mi__1_5_interpret-args'(A,G,P,C,D,R)  ,
                                 S=R , 
                                 T=ispu(S) , 
                                 U =  
                                   ispuU( V, 
                                     'mi__1_2_cons-atom'(J,S,W),V=W,throw(metta_return(V))) , 
                                 X=['return-on-error',T,U] , 
                                 as_p1_expr(X,Y) , 
                                 Z=[eval,Y] , 
                                 N=Z))  )));
                          (  ((
                              (throw(metta_return(['Error',B,"Function type expected"]))),
                              (N =  
                                 [ 'Error', B,"Function type expected"])  )))  ))) , 
                   A1=['return-on-error',K,L] , 
                   as_p1_expr(A1,B1) , 
                   C1=[eval,B1] , 
                   D1=C1))  )));
            (  ((
                (throw(metta_return(['Error',A,"Non-empty expression atom is expected"]))),
                (D1 =  
                   [ 'Error', A,"Non-empty expression atom is expected"])  )))  ))),
        (mi__1_1_eval(D1,E))  )), metta_return(E1),E1=E).


metta_other_asserted('&corelib',[iz,'interpret-args','MinimalMeTTaHelper']).
metta_function_asserted('&corelib',['interpret-args',_atom,_args,_arg_types,_ret_type,_space],[function,['if-unify',_args,[],[eval,['if-decons',_arg_types,_actual_ret_type,_type_tail,[chain,[eval,[==,[],_type_tail]],_correct_type_len,[eval,[if,_correct_type_len,[eval,['match-types',_actual_ret_type,_ret_type,[return,[]],[return,['Error',_atom,'BadType']]]],[return,['Error',_atom,'BadType']]]]],[return,['Error',_atom,"Too many arguments"]]]],[eval,['if-decons',_args,_head,_tail,[eval,['if-decons',_arg_types,_head_type,_tail_types,[chain,[eval,[interpret,_head,_head_type,_space]],_reduced_head,[eval,['if-equal',_reduced_head,_head,[chain,[eval,['interpret-args-tail',_atom,_reduced_head,_tail,_tail_types,_ret_type,_space]],_ret,[return,_ret]],[eval,['return-on-error',_reduced_head,[chain,[eval,['interpret-args-tail',_atom,_reduced_head,_tail,_tail_types,_ret_type,_space]],_ret,[return,_ret]]]]]]],[return,['Error',_atom,'BadType']]]],[return,['Error',['interpret-atom',_atom,_args,_arg_types,_space],"Non-empty expression atom is expected"]]]]]]).
transpiler_clause_store('interpret-args',[5],1,['Any','Any','Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-args',_atom,_args,_arg_types,_ret_type,_space],[function,[if,['metta-unify',_args,[]],[eval,[if,['decons-ht',_arg_types,_actual_ret_type,_type_tail],[let,_correct_type_len,[==,[],_type_tail],[eval,[if,_correct_type_len,[eval,['match-types',_actual_ret_type,_ret_type,[return,[]],[return,['Error',_atom,'BadType']]]],[return,['Error',_atom,'BadType']]]]],[return,['Error',_atom,"Too many arguments"]]]],[eval,[if,['decons-ht',_args,_head,_tail],[eval,[if,['decons-ht',_arg_types,_head_type,_tail_types],[let,_reduced_head,[interpret,_head,_head_type,_space],[eval,[if,['metta-equal',_reduced_head,_head],[let,_ret,['interpret-args-tail',_atom,_reduced_head,_tail,_tail_types,_ret_type,_space],[return,_ret]],[eval,['return-on-error',_reduced_head,[let,_ret,['interpret-args-tail',_atom,_reduced_head,_tail,_tail_types,_ret_type,_space],[return,_ret]]]]]]],[return,['Error',_atom,'BadType']]]],[return,['Error',['interpret-atom',_atom,_args,_arg_types,_space],"Non-empty expression atom is expected"]]]]]]).


'mi__1_5_interpret-args'(B,C,D,E,F,G) :- 
  H =  
    'mc__1_5_interpret-args'(B,C,D,E,F,G) , 
  ci( true, 
    'interpret-args', 
    5, 
    [ 'interpret-args', B,C,D,E, 
      F], G,true,H).




'me__1_5_interpret-args'(B,C,D,E,F,G) :-  
  'mc__1_5_interpret-args'(B,C,D,E,F,G).




'mc__1_5_interpret-args'(A,B,C,D,E,F) :-  
  catch( 
       ((
        (  ((
            ('mi__1_2_metta-unify'(B,[],G),is_True(G))*->
            ((   ((
                  (  ((
                      ('mi__1_3_decons-ht'(C,H,I,J),is_True(J))*->
                      (( 'mi__1_2_=='([],I,K)  ,
                         L=K , 
                           ((
                            (  ((
                                (is_True(L))*->
                                (( M=ispu(H)  ,
                                   N=ispu(D) , 
                                   O =  
                                     ispuU([],throw(metta_return([]))) , 
                                   P =  
                                     ispuU( ['Error',A,'BadType'], 
                                       throw(metta_return(['Error',A,'BadType']))) , 
                                   Q =  
                                     [ 'match-types', M,N,O,P] , 
                                   as_p1_expr(Q,R) , 
                                   S=[eval,R] , 
                                   T=S))  )));
                            (  ((
                                (throw(metta_return(['Error',A,'BadType']))),
                                (T=['Error',A,'BadType'])  )))  )) , 
                         U=[eval,T] , 
                         V=U))  )));
                  (  ((
                      (throw(metta_return(['Error',A,"Too many arguments"]))),
                      (V =  
                         [ 'Error', A,"Too many arguments"])  )))  ))  ,
               mi__1_1_eval(V,W) , 
               F=W))  )));
        ((   ((
              (  ((
                  ('mi__1_3_decons-ht'(B,X,Y,Z),is_True(Z))*->
                  ((   ((
                        (  ((
                            ('mi__1_3_decons-ht'(C,A1,B1,C1),is_True(C1))*->
                            (( mi__1_3_interpret(X,A1,E,D1)  ,
                               E1=D1 , 
                                 ((
                                  (  ((
                                      (F1=['metta-equal',E1,X],is_True(F1))*->
                                      (( 'mi__1_6_interpret-args-tail'(A,E1,Y,B1,D,E,G1)  ,
                                         H1=G1 , 
                                         throw(metta_return(H1)) , 
                                         I1=H1))  )));
                                  (( J1=ispu(E1)  ,
                                     K1 =  
                                       ispuU( H1, 
                                         ( 'mi__1_6_interpret-args-tail'(A,E1,Y,B1,D,E,L1)  ,
                                           H1=L1 , 
                                           throw(metta_return(H1)))) , 
                                     M1=['return-on-error',J1,K1] , 
                                     as_p1_expr(M1,N1) , 
                                     O1=[eval,N1] , 
                                     I1=O1))  )) , 
                               P1=[eval,I1] , 
                               Q1=P1))  )));
                        (  ((
                            (throw(metta_return(['Error',A,'BadType']))),
                            (Q1=['Error',A,'BadType'])  )))  ))  ,
                     R1=[eval,Q1] , 
                     S1=R1))  )));
              (  ((
                  (throw( metta_return( [ 'Error', 
                                          [ 'interpret-atom', A,B,C,E], 
                                          "Non-empty expression atom is expected"]))),
                  (S1 =  
                     [ 'Error', 
                       [ 'interpret-atom', A,B,C,E], 
                       "Non-empty expression atom is expected"])  )))  ))  ,
           mi__1_1_eval(S1,T1) , 
           F=T1))  )), metta_return(U1),U1=F).


metta_other_asserted('&corelib',[iz,'interpret-args-tail','MinimalMeTTaHelper']).
metta_function_asserted('&corelib',['interpret-args-tail',_atom,_head,_args_tail,_args_tail_types,_ret_type,_space],[function,[chain,[eval,['interpret-args',_atom,_args_tail,_args_tail_types,_ret_type,_space]],_reduced_tail,[eval,['return-on-error',_reduced_tail,[chain,['cons-atom',_head,_reduced_tail],_ret,[return,_ret]]]]]]).
transpiler_clause_store('interpret-args-tail',[6],1,['Any','Any','Any','Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-args-tail',_atom,_head,_args_tail,_args_tail_types,_ret_type,_space],[function,[let,_reduced_tail,['interpret-args',_atom,_args_tail,_args_tail_types,_ret_type,_space],[eval,['return-on-error',_reduced_tail,[let,_ret,['cons-atom',_head,_reduced_tail],[return,_ret]]]]]]).


'mi__1_6_interpret-args-tail'(B,C,D,E,F,G,H) :- 
  I =  
    'mc__1_6_interpret-args-tail'(B,C,D,E,F,G,H) , 
  ci( true, 
    'interpret-args-tail', 
    6, 
    [ 'interpret-args-tail', B,C,D,E, 
      F,G], H,true,I).




'me__1_6_interpret-args-tail'(B,C,D,E,F,G,H) :-  
  'mc__1_6_interpret-args-tail'(B,C,D,E,F,G,H).




'mc__1_6_interpret-args-tail'(A,B,C,D,E,F,G) :-  
  catch( 
     ( 'mi__1_5_interpret-args'(A,C,D,E,F,H)  ,
       I=H , 
       J=ispu(I) , 
       K =  
         ispuU( L, 
           'mi__1_2_cons-atom'(B,I,M),L=M,throw(metta_return(L))) , 
       N=['return-on-error',J,K] , 
       as_p1_expr(N,O) , 
       mi__1_1_eval(O,G)), metta_return(P),P=G).


metta_other_asserted('&corelib',[iz,'interpret-tuple','MinimalMeTTaHelper']).
metta_function_asserted('&corelib',['interpret-tuple',_atom,_space],[function,['if-unify',_atom,[],[return,_atom],[eval,['if-decons',_atom,_head,_tail,[chain,[eval,[interpret,_head,'%Undefined%',_space]],_rhead,[eval,['if-equal',_rhead,'Empty',[return,'Empty'],[chain,[eval,['interpret-tuple',_tail,_space]],_rtail,[eval,['if-equal',_rtail,'Empty',[return,'Empty'],[chain,['cons-atom',_rhead,_rtail],_ret,[return,_ret]]]]]]]],[return,['Error',['interpret-tuple',_atom,_space],"Non-empty expression atom is expected as an argument"]]]]]]).
transpiler_clause_store('interpret-tuple',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['interpret-tuple',_atom,_space],[function,[if,['metta-unify',_atom,[]],[return,_atom],[eval,[if,['decons-ht',_atom,_head,_tail],[let,_rhead,[interpret,_head,'%Undefined%',_space],[eval,[if,['metta-equal',_rhead,'Empty'],[return,'Empty'],[let,_rtail,['interpret-tuple',_tail,_space],[eval,[if,['metta-equal',_rtail,'Empty'],[return,'Empty'],[let,_ret,['cons-atom',_rhead,_rtail],[return,_ret]]]]]]]],[return,['Error',['interpret-tuple',_atom,_space],"Non-empty expression atom is expected as an argument"]]]]]]).


'mi__1_2_interpret-tuple'(B,C,D) :- 
  E='mc__1_2_interpret-tuple'(B,C,D) , 
  ci(true,'interpret-tuple',2,['interpret-tuple',B,C],D,true,E).




'me__1_2_interpret-tuple'(B,C,D):-'mc__1_2_interpret-tuple'(B,C,D).




'mc__1_2_interpret-tuple'(A,B,C) :-  
  catch( 
       ((
        (  ((
            ('mi__1_2_metta-unify'(A,[],D),is_True(D))*->
            (throw(metta_return(A)),C=A)  )));
        ((   ((
              (  ((
                  ('mi__1_3_decons-ht'(A,E,F,G),is_True(G))*->
                  (( mi__1_3_interpret(E,'%Undefined%',B,H)  ,
                     I=H , 
                       ((
                        (  ((
                            (J=['metta-equal',I,'Empty'],is_True(J))*->
                            (throw(metta_return('Empty')),K='Empty')  )));
                        (( 'mi__1_2_interpret-tuple'(F,B,L)  ,
                           M=L , 
                             ((
                              (  ((
                                  (N=['metta-equal',M,'Empty'],is_True(N))*->
                                  (throw(metta_return('Empty')),O='Empty')  )));
                              (( 'mi__1_2_cons-atom'(I,M,P)  ,
                                 Q=P , 
                                 throw(metta_return(Q)) , 
                                 O=Q))  )) , 
                           R=[eval,O] , 
                           K=R))  )) , 
                     S=[eval,K] , 
                     T=S))  )));
              (  ((
                  (throw( metta_return( [ 'Error', 
                                          ['interpret-tuple',A,B], 
                                          "Non-empty expression atom is expected as an argument"]))),
                  (T =  
                     [ 'Error', 
                       ['interpret-tuple',A,B], 
                       "Non-empty expression atom is expected as an argument"])  )))  ))  ,
           mi__1_1_eval(T,U) , 
           C=U))  )), metta_return(V),V=C).


metta_other_asserted('&corelib',[iz,'metta-call','MinimalMeTTaHelper']).
metta_function_asserted('&corelib',['metta-call',_atom,_type,_space],[function,[eval,['if-error',_atom,[return,_atom],[chain,[eval,_atom],_result,[eval,['if-equal',_result,'NotReducible',[return,_atom],[eval,['if-equal',_result,'Empty',[return,'Empty'],[eval,['if-error',_result,[return,_result],[chain,[eval,[interpret,_result,_type,_space]],_ret,[return,_ret]]]]]]]]]]]]).
transpiler_clause_store('metta-call',[3],1,['Any','Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['metta-call',_atom,_type,_space],[function,[eval,['if-error',_atom,[return,_atom],[let,_result,_atom,[eval,[if,['metta-equal',_result,'NotReducible'],[return,_atom],[eval,[if,['metta-equal',_result,'Empty'],[return,'Empty'],[eval,['if-error',_result,[return,_result],[let,_ret,[interpret,_result,_type,_space],[return,_ret]]]]]]]]]]]]).


'mi__1_3_metta-call'(B,C,D,E) :- 
  F='mc__1_3_metta-call'(B,C,D,E) , 
  ci( true, 
    'metta-call', 
    3, 
    ['metta-call',B,C,D], E,true,F).




'me__1_3_metta-call'(B,C,D,E) :-  
  'mc__1_3_metta-call'(B,C,D,E).




'mc__1_3_metta-call'(A,B,C,D) :-  
  catch( 
     ( E=ispu(A)  ,
       F =  
         ispuU(A,throw(metta_return(A))) , 
       G =  
         ispeEnNC( H, 
           mi__1_1_eval(I,H), J,J=[eval,I], 
             ((
              (K=A),
              (  ((
                  (  ((
                      (L=['metta-equal',K,'NotReducible'],is_True(L))*->
                      (throw(metta_return(A)),I=A)  )));
                  ((   ((
                        (  ((
                            (M=['metta-equal',K,'Empty'],is_True(M))*->
                            (throw(metta_return('Empty')),N='Empty')  )));
                        (( O=ispu(K)  ,
                           P =  
                             ispuU(K,throw(metta_return(K))) , 
                           Q =  
                             ispuU( R, 
                               ( mi__1_3_interpret(K,B,C,S)  ,
                                 R=S , 
                                 throw(metta_return(R)))) , 
                           T=['if-error',O,P,Q] , 
                           as_p1_expr(T,U) , 
                           V=[eval,U] , 
                           N=V))  ))  ,
                     W=[eval,N] , 
                     I=W))  )))  ))) , 
       X=['if-error',E,F,G] , 
       as_p1_expr(X,Y) , 
       mi__1_1_eval(Y,D)), metta_return(Z),Z=D).


%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Standard library written in MeTTa ;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; TODO: Type is used here, but there is no definition for the -> type
%  ; constructor for instance, thus in practice it matches because -> has
%  ; %Undefined% type. We need to assign proper type to -> and other type
%  ; constructors but it is not possible until we support vararg types.
metta_other_asserted('&corelib',[iz,'is-function-type','MeTTa']).
%  ; or Helper?
metta_other_asserted('&corelib',['@doc','is-function-type',['@desc',"Function checks if input type is a function type"],['@params',[['@param',"Type notation"]]],['@return',"True if input type notation is a function type, False - otherwise"]]).
metta_other_asserted('&corelib',[:,'is-function-type',[->,'Type','Bool']]).
metta_function_asserted('&corelib',['is-function-type',_type],[let,_type_meta,['get-metatype',_type],[case,_type_meta,[['Expression',[let,_first,['car-atom',_type],[if,[==,_first,->],'True','False']]],[_,'False']]]]).
transpiler_clause_store('is-function-type',[1],1,['Type'],'Bool',[x(doeval,eager,['Type'])],x(doeval,eager,[boolean]),['is-function-type',_type],[let,_type_meta,['get-metatype',_type],[case,_type_meta,[['Expression',[let,_first,['car-atom',_type],[if,[==,_first,->],'True','False']]],[_,'False']]]]).


'mi__1_1_is-function-type'(B,C) :- 
  D='mc__1_1_is-function-type'(B,C) , 
  ci(true,'is-function-type',1,['is-function-type',B],C,true,D).




'me__1_1_is-function-type'(B,C):-'mc__1_1_is-function-type'(B,C).




'mc__1_1_is-function-type'(A,B) :- 
  'mi__1_1_get-metatype'(A,C) , 
  D=C , 
  true*->E=D;E='Empty' , 
    ((
     (  ((
         (E='Expression')*->
         (( 'mi__1_1_car-atom'(A,F)  ,
            G=F , 
              ((
               ('mi__1_2_=='(G,->,H),is_True(H)*->I='True');
               (I='False')  )) , 
            B=I))  )));
     (  ((
         (E=_*->J='False';mc__1_0_empty(K),J=K),
         (B=J)  )))  )).


%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; MeTTa interpreter implementation ;
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_other_asserted('&corelib',[iz,if,'MeTTa']).
metta_other_asserted('&corelib',['@doc',if,['@desc',"Replace itself by one of the arguments depending on condition."],['@params',[['@param',"Boolean condition"],['@param',"Result when condition is True"],['@param',"Result when condition is False"]]],['@return',"Second or third argument"]]).
metta_other_asserted('&corelib',[:,if,[->,'Bool','Atom','Atom',_t]]).
metta_other_asserted('&corelib',['ALT=',[if,'True',_then,_else],_then]).
metta_other_asserted('&corelib',['ALT=',[if,'False',_then,_else],_else]).
%  ;`$then`, `$else` should be of `Atom` type to avoid evaluation
%  ; and infinite cycle in inference
metta_other_asserted('&corelib',[iz,or,'MeTTa']).
metta_other_asserted('&corelib',['@doc',or,['@desc',"Logical disjunction of two arguments"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"True if any of input arguments is True, False - otherwise"]]).
metta_other_asserted('&corelib',[:,or,[->,'Bool','LazyBool','Bool']]).
metta_other_asserted('&corelib',['ALT=',[or,'False','False'],'False']).
metta_other_asserted('&corelib',['ALT=',[or,'False','True'],'True']).
metta_other_asserted('&corelib',['ALT=',[or,'True','False'],'True']).
metta_other_asserted('&corelib',['ALT=',[or,'True','True'],'True']).
metta_other_asserted('&corelib',[iz,and,'MeTTa']).
metta_other_asserted('&corelib',['@doc',and,['@desc',"Logical conjunction of two arguments"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"Returns True if both arguments are True, False - otherwise"]]).
metta_other_asserted('&corelib',[:,and,[->,'Bool','LazyBool','Bool']]).
metta_other_asserted('&corelib',['ALT=',[and,'False','False'],'False']).
metta_other_asserted('&corelib',['ALT=',[and,'False','True'],'False']).
metta_other_asserted('&corelib',['ALT=',[and,'True','False'],'False']).
metta_other_asserted('&corelib',['ALT=',[and,'True','True'],'True']).
metta_other_asserted('&corelib',[iz,not,'MeTTa']).
metta_other_asserted('&corelib',['@doc',not,['@desc',"Logical negation"],['@params',[['@param',"Argument"]]],['@return',"Negates boolean input argument (False -> True, True -> False)"]]).
metta_other_asserted('&corelib',[:,not,[->,'Bool','Bool']]).
metta_other_asserted('&corelib',['ALT=',[not,'True'],'False']).
metta_other_asserted('&corelib',['ALT=',[not,'False'],'True']).
metta_other_asserted('&corelib',[iz,let,'MeTTa']).
metta_other_asserted('&corelib',['@doc',let,['@desc',"Let function is utilized to establish temporary variable bindings within an expression. It allows introducing variables (first argument), assign values to them (second argument), and then use these values within the scope of the let block"],['@params',[['@param',"Variable name (or several variables inside brackets ())"],['@param',"Expression to be bound to variable (it is being reduced before bind)"],['@param',"Expression which will be reduced and in which variable (first argument) could be used"]]],['@return',"Result of third argument's evaluation"]]).
metta_other_asserted('&corelib',[:,let,[->,'Atom','%Undefined%','Atom','Atom']]).
metta_other_asserted('&corelib',['ALT=',[let,_pattern,_atom,_template],['if-unify',_atom,_pattern,_template,'Empty']]).
metta_other_asserted('&corelib',[iz,'let*','MeTTa']).
metta_other_asserted('&corelib',['@doc','let*',['@desc',"Same as let, but first argument is a tuple containing tuples of variables and their bindings, e.g. (($v (+ 1 2)) ($v2 (* 5 6)))"],['@params',[['@param',"Tuple of tuples with variables and their bindings"],['@param',"Expression which will be evaluated if each pair can be unified"]]],['@return',"Second argument or Empty"]]).
metta_other_asserted('&corelib',[:,'let*',[->,'Expression','Atom','%Undefined%']]).
metta_other_asserted('&corelib',['ALT=',['let*',_pairs,_template],[eval,['if-decons-expr',_pairs,[_pattern,_atom],_tail,[let,_pattern,_atom,['let*',_tail,_template]],_template]]]).
metta_other_asserted('&corelib',[iz,'add-reduct','MeTTa']).
metta_other_asserted('&corelib',['@doc','add-reduct',['@desc',"Prevents atom from being reduced"],['@params',[['@param',"Atom"]]],['@return',"Quoted atom"]]).
metta_other_asserted('&corelib',['@doc','add-reduct-rust1',['@desc',"Adds atom into the atomspace reducing it first"],['@params',[['@param',"Atomspace to add atom into"],['@param',"Atom to add"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'add-reduct-rust1',[->,'hyperon::space::DynSpace','%Undefined%',[->]]]).
metta_function_asserted('&corelib',['add-reduct-minimal',_dst,_atom],['add-atom',_dst,_atom]).
transpiler_clause_store('add-reduct-minimal',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-reduct-minimal',_dst,_atom],['add-atom',_dst,_atom]).


'mi__1_2_add-reduct-minimal'(B,C,D) :- 
  E='mc__1_2_add-reduct-minimal'(B,C,D) , 
  ci( true, 
    'add-reduct-minimal', 
    2, 
    ['add-reduct-minimal',B,C], D,true,E).




'me__1_2_add-reduct-minimal'(B,C,D) :-  
  'mc__1_2_add-reduct-minimal'(B,C,D).




'mc__1_2_add-reduct-minimal'(A,B,C):-'mi__1_2_add-atom'(A,B,C).


metta_other_asserted('&corelib',[iz,'add-reduct','MeTTa']).
metta_other_asserted('&corelib',[:,'add-reduct',[->,'Grounded','%Undefined%',[->]]]).
metta_function_asserted('&corelib',['add-reduct',_dst,_atom],['add-atom',_dst,_atom]).
transpiler_clause_store('add-reduct',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-reduct',_dst,_atom],['add-atom',_dst,_atom]).


'mi__1_2_add-reduct'(B,C,D) :- 
  E='mc__1_2_add-reduct'(B,C,D) , 
  ci(true,'add-reduct',2,['add-reduct',B,C],D,true,E).




'me__1_2_add-reduct'(B,C,D):-'mc__1_2_add-reduct'(B,C,D).




'mc__1_2_add-reduct'(A,B,C):-'mi__1_2_add-atom'(A,B,C).


metta_other_asserted('&corelib',[iz,stringToChars,'MeTTa']).
metta_other_asserted('&corelib',['@doc',stringToChars,['@desc',"Converts a string into a list expression of characters."],['@params',[['@param',"String to be converted."]]],['@return',"Expression representing the list of characters."]]).
metta_other_asserted('&corelib',[:,stringToChars,[->,'String','Expression']]).
metta_other_asserted('&corelib',[iz,charsToString,'MeTTa']).
metta_other_asserted('&corelib',['@doc',charsToString,['@desc',"Converts a list expression of characters into a string."],['@params',[['@param',"Expression representing the list of characters."]]],['@return',"Converted string."]]).
metta_other_asserted('&corelib',[:,charsToString,[->,'Expression','String']]).
metta_other_asserted('&corelib',[iz,parse,'MeTTa']).
metta_other_asserted('&corelib',['@doc',parse,['@desc',"Parses MeTTa code from a string and returns the corresponding atom."],['@params',[['@param',"String containing MeTTa code."]]],['@return',"Parsed atom."]]).
metta_other_asserted('&corelib',[:,parse,[->,'String','Atom']]).
metta_other_asserted('&corelib',[iz,repr,'MeTTa']).
metta_other_asserted('&corelib',['@doc',repr,['@desc',"Creates a string representation of an atom."],['@params',[['@param',"Atom to be represented as a string."]]],['@return',"String representation of the atom."]]).
metta_other_asserted('&corelib',[:,repr,[->,'Atom','String']]).
metta_other_asserted('&corelib',[iz,'car-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','car-atom',['@desc',"Extracts the first atom of an expression as a tuple"],['@params',[['@param',"Expression"]]],['@return',"First atom of an expression"]]).
metta_other_asserted('&corelib',[:,'car-atom',[->,'Expression','Atom']]).
metta_other_asserted('&corelib',['ALT=',['car-atom',_atom],[eval,['if-decons-expr',_atom,_head,_,_head,['Error',['car-atom',_atom],"car-atom expects a non-empty expression as an argument"]]]]).
metta_other_asserted('&corelib',[iz,'cdr-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','cdr-atom',['@desc',"Extracts the tail of an expression (all except first atom)"],['@params',[['@param',"Expression"]]],['@return',"Tail of an expression"]]).
metta_other_asserted('&corelib',[:,'cdr-atom',[->,'Expression','Expression']]).
metta_function_asserted('&corelib',['cdr-atom',_atom],[eval,['if-decons-expr',_atom,_,_tail,_tail,['Error',['cdr-atom',_atom],"cdr-atom expects a non-empty expression as an argument"]]]).
transpiler_clause_store('cdr-atom',[1],1,['Expression'],'Expression',[x(noeval,eager,[])],x(noeval,eager,[]),['cdr-atom',_atom],[eval,[if,['decons-ht',_atom,_,_tail],_tail,['Error',['cdr-atom',_atom],"cdr-atom expects a non-empty expression as an argument"]]]).


'mi__1_1_cdr-atom'(B,C) :- 
  D='mc__1_1_cdr-atom'(B,C) , 
  ci(true,'cdr-atom',1,['cdr-atom',B],C,true,D).




'me__1_1_cdr-atom'(B,C):-'mc__1_1_cdr-atom'(B,C).




'mc__1_1_cdr-atom'(A,B) :- 
    ((
     (  ((
         ('mi__1_3_decons-ht'(A,_,C,D),is_True(D))*->
         (E=C)  )));
     (E =  
        [ 'Error', 
          ['cdr-atom',A], 
          "cdr-atom expects a non-empty expression as an argument"])  )) , 
  mi__1_1_eval(E,B).


metta_other_asserted('&corelib',[iz,quote,'MeTTa']).
metta_other_asserted('&corelib',['@doc',quote,['@desc',"Prevents atom from being reduced"],['@params',[['@param',"Atom"]]],['@return',"Quoted atom"]]).
metta_other_asserted('&corelib',[:,quote,[->,'Atom','Atom']]).
metta_function_asserted('&corelib',[quote,_atom],'NotReducible').
transpiler_clause_store(quote,[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),[quote,_atom],'NotReducible').


mi__1_1_quote(B,C) :- 
  D=mc__1_1_quote(B,C) , 
  ci(true,quote,1,[quote,B],C,true,D).




me__1_1_quote(B,C):-mc__1_1_quote(B,C).




mc__1_1_quote(_,A):-A=ispu('NotReducible').


metta_other_asserted('&corelib',[iz,unquote,'MeTTa']).
metta_other_asserted('&corelib',['@doc',unquote,['@desc',"Unquotes quoted atom, e.g. (unquote (quote $x)) returns $x"],['@params',[['@param',"Quoted atom"]]],['@return',"Unquoted atom"]]).
metta_other_asserted('&corelib',[:,unquote,[->,'%Undefined%','%Undefined%']]).
metta_function_asserted('&corelib',[unquote,[quote,_atom]],_atom).
transpiler_clause_store(unquote,[1],1,['%Undefined%'],'%Undefined%',[x(doeval,eager,['%Undefined%'])],x(doeval,eager,['%Undefined%']),[unquote,[quote,_atom]],_atom).


mi__1_1_unquote(B,C) :- 
  D=mc__1_1_unquote(B,C) , 
  ci(true,unquote,1,[unquote,B],C,true,D).




me__1_1_unquote(B,C):-mc__1_1_unquote(B,C).




mc__1_1_unquote([quote,A],A) :-  
  true.


%  ; TODO: there is no way to define operation which consumes any number of
%  ; arguments  and returns unit
metta_other_asserted('&corelib',[iz,nop,'MeTTa']).
metta_other_asserted('&corelib',['@doc',nop,['@desc',"Outputs unit atom"],['@params',[]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,nop,[->,'EmptyType']]).
metta_other_asserted('&corelib',['ALT=',[nop],[]]).
metta_other_asserted('&corelib',[iz,nop,'MeTTa']).
metta_other_asserted('&corelib',['@doc',nop,['@desc',"Outputs unit atom for any input"],['@params',[['@param',"Anything"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,nop,[->,'Atom',[->]]]).
metta_other_asserted('&corelib',['ALT=',[nop,_x],[]]).
metta_other_asserted('&corelib',[iz,empty,'MeTTa']).
%  ; TODO: can be replaced by Empty atom and removed, kept for compatibility
metta_other_asserted('&corelib',['@doc',empty,['@desc',"Cuts evaluation of the non-deterministic branch and removes it from the result"],['@params',[]],['@return',"Nothing"]]).
metta_other_asserted('&corelib',[:,empty,[->,'%Undefined%']]).
metta_other_asserted('&corelib',['ALT=',[empty],[let,a,b,'never-happens']]).
%  ;For testing
%  ;(= (empty) Empty)
%  ;(= (empty-rust1) (let a b never-happens))
%  ; TODO: MINIMAL added for compatibility, remove after migration
%  ;(= (empty-minimal) Empty)
metta_other_asserted('&corelib',[iz,unique,'MeTTa']).
metta_other_asserted('&corelib',['@doc',unique,['@desc',"Function takes non-deterministic input (first argument) and returns only unique entities. E.g. (unique (superpose (a b c d d))) -> [a, b, c, d]"],['@params',[['@param',"Non-deterministic set of values"]]],['@return',"Unique non-deterministic values from input set"]]).
metta_other_asserted('&corelib',[:,unique,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,union,'MeTTa']).
metta_other_asserted('&corelib',['@doc',union,['@desc',"Function takes two non-deterministic inputs (first and second argument) and returns their union. E.g. (union (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]"],['@params',[['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Union of sets"]]).
metta_other_asserted('&corelib',[:,union,[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,intersection,'MeTTa']).
metta_other_asserted('&corelib',['@doc',intersection,['@desc',"Function takes two non-deterministic inputs (first and second argument) and returns their intersection. E.g. (intersection (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]"],['@params',[['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Intersection of sets"]]).
metta_other_asserted('&corelib',[:,intersection,[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,subtraction,'MeTTa']).
metta_other_asserted('&corelib',['@doc',subtraction,['@desc',"Function takes two non-deterministic inputs (first and second argument) and returns their subtraction. E.g. !(subtraction (superpose (a b b c)) (superpose (b c c d))) -> [a, b]"],['@params',[['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Subtraction of sets"]]).
metta_other_asserted('&corelib',[:,subtraction,[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',['@doc','add-reducts',['@desc',"Function takes space and expression, evaluates atoms in it and adds them into given space"],['@params',[['@param',"Space"],['@param',"Expression"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'add-reducts',[->,'Grounded','%Undefined%',[->]]]).
metta_function_asserted('&corelib',['add-reducts',_space,_tuple],['foldl-atom',_tuple,[],_a,_b,[eval,['add-atom',_space,_b]]]).
transpiler_clause_store('add-reducts',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-reducts',_space,_tuple],['foldl-atom',_tuple,[],_a,_b,[eval,['add-atom',_space,_b]]]).


'mi__1_2_add-reducts'(B,C,D) :- 
  E='mc__1_2_add-reducts'(B,C,D) , 
  ci(true,'add-reducts',2,['add-reducts',B,C],D,true,E).




'me__1_2_add-reducts'(B,C,D):-'mc__1_2_add-reducts'(B,C,D).




'mc__1_2_add-reducts'(A,B,C) :- 
  D=ispu([]) , 
  E =  
    ispeEnNC( F, 
      mi__1_1_eval(G,F), H,H=[eval,G], 
      G=['add-atom',A,I]) , 
  'mi__1_5_foldl-atom'(B,D,_,I,E,J) , 
  as_p1_exec(J,C).


metta_other_asserted('&corelib',['@doc','add-atoms',['@desc',"Function takes space and expression and adds atoms in Expression into given space without reducing them"],['@params',[['@param',"Space"],['@param',"Expression"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'add-atoms',[->,'Grounded','Expression',[->]]]).
metta_function_asserted('&corelib',['add-atoms',_space,_tuple],['foldl-atom',_tuple,[],_a,_b,[eval,['add-atom',_space,_b]]]).
transpiler_clause_store('add-atoms',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['add-atoms',_space,_tuple],['foldl-atom',_tuple,[],_a,_b,[eval,['add-atom',_space,_b]]]).


'mi__1_2_add-atoms'(B,C,D) :- 
  E='mc__1_2_add-atoms'(B,C,D) , 
  ci(true,'add-atoms',2,['add-atoms',B,C],D,true,E).




'me__1_2_add-atoms'(B,C,D):-'mc__1_2_add-atoms'(B,C,D).




'mc__1_2_add-atoms'(A,B,C) :- 
  D=ispu([]) , 
  E =  
    ispeEnNC( F, 
      mi__1_1_eval(G,F), H,H=[eval,G], 
      G=['add-atom',A,I]) , 
  'mi__1_5_foldl-atom'(B,D,_,I,E,J) , 
  as_p1_exec(J,C).


%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Documentation formatting functions
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_other_asserted('&corelib',[iz,'@doc','MeTTa']).
metta_other_asserted('&corelib',['@doc','@doc',['@desc',"Used for documentation purposes. Function documentation starts with @doc"],['@params',[['@param',"Function name"],['@param',"Function description. Starts with @desc"],['@param',"(Optional) parameters description starting with @params which should contain one or more @param symbols"],['@param',"(Optional) description of what function will return. Starts with @return"]]],['@return',"Function documentation using @doc-formal"]]).
metta_other_asserted('&corelib',[:,'@doc',[->,'Atom','DocDescription','DocInformal']]).
metta_other_asserted('&corelib',[:,'@doc',[->,'Atom','DocDescription','DocParameters','DocReturnInformal','DocInformal']]).
metta_other_asserted('&corelib',[iz,'@doc','DataFunctor']).
metta_other_asserted('&corelib',[iz,'@desc','MeTTa']).
metta_other_asserted('&corelib',['@doc','@desc',['@desc',"Used for documentation purposes. Description of function starts with @desc as a part of @doc"],['@params',[['@param',"String containing function description"]]],['@return',"Function description"]]).
metta_other_asserted('&corelib',[:,'@desc',[->,'String','DocDescription']]).
metta_other_asserted('&corelib',[iz,'@desc','DataFunctor']).
metta_other_asserted('&corelib',[iz,'@param','MeTTa']).
metta_other_asserted('&corelib',['@doc','@param',['@desc',"Used for documentation purposes. Description of function parameter starts with @param as a part of @params which is a part of @doc"],['@params',[['@param',"String containing parameter description"]]],['@return',"Parameter description"]]).
metta_other_asserted('&corelib',[:,'@param',[->,'String','DocParameterInformal']]).
metta_other_asserted('&corelib',[:,'@param',[->,'DocType','DocDescription','DocParameter']]).
metta_other_asserted('&corelib',[iz,'@param','DataFunctor']).
metta_other_asserted('&corelib',[iz,'@return','MeTTa']).
metta_other_asserted('&corelib',['@doc','@return',['@desc',"Used for documentation purposes. Description of function return value starts with @return as a part of @doc"],['@params',[['@param',"String containing return value description"]]],['@return',"Return value description"]]).
metta_other_asserted('&corelib',[:,'@return',[->,'String','DocReturnInformal']]).
metta_other_asserted('&corelib',[:,'@return',[->,'DocType','DocDescription','DocReturn']]).
metta_other_asserted('&corelib',[iz,'@return','DataFunctor']).
metta_other_asserted('&corelib',[iz,'@doc-formal','MeTTa']).
metta_other_asserted('&corelib',['@doc','@doc-formal',['@desc',"Used for documentation purposes. get-doc returns documentation starting with @doc-formal symbol. @doc-formal contains 6 or 4 parameters depending on the entity being described (functions being described using 6 parameters, atoms - 4 parameters)"],['@params',[['@param',"Function/Atom name for which documentation is to be displayed. Format (@item name)"],['@param',"Contains (@kind function) or (@kind atom) depends on entity which documentation is displayed"],['@param',"Contains type notation of function/atom"],['@param',"Function/atom description"],['@param',"(Functions only). Description of function parameters"],['@param',"(Functions only). Description of function's return value"]]],['@return',"Expression containing full documentation on function"]]).
metta_other_asserted('&corelib',[:,'@doc-formal',[->,'DocItem','DocKindFunction','DocType','DocDescription','DocParameters','DocReturn','DocFormal']]).
metta_other_asserted('&corelib',[:,'@doc-formal',[->,'DocItem','DocKindAtom','DocType','DocDescription','DocFormal']]).
metta_other_asserted('&corelib',[iz,'@doc-formal','DataFunctor']).
metta_other_asserted('&corelib',[iz,'@item','MeTTa']).
metta_other_asserted('&corelib',['@doc','@item',['@desc',"Used for documentation purposes. Converts atom/function's name to DocItem"],['@params',[['@param',"Atom/Function name to be documented"]]],['@return',"(@item Atom) entity"]]).
metta_other_asserted('&corelib',[:,'@item',[->,'Atom','DocItem']]).
metta_other_asserted('&corelib',[iz,'@item','DataFunctor']).
%  ; TODO: help! gives two outputs
%  ;Atom (@kind function): (%Undefined% (-> Atom Atom)) Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
%  ;Atom (@kind function): DocKindFunction Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case
metta_other_asserted('&corelib',[iz,['@kind',function],'MeTTa']).
metta_other_asserted('&corelib',['@doc',['@kind',function],['@desc',"Used for documentation purposes. Shows type of entity to be documented. (@kind function) in this case"]]).
metta_other_asserted('&corelib',[:,['@kind',function],'DocKindFunction']).
metta_other_asserted('&corelib',[iz,['@kind',function],'DataFunctor']).
metta_other_asserted('&corelib',[iz,['@kind',atom],'MeTTa']).
metta_other_asserted('&corelib',['@doc',['@kind',atom],['@desc',"Used for documentation purposes. Shows type of entity to be documented. (@kind atom) in this case"]]).
metta_other_asserted('&corelib',[:,['@kind',atom],'DocKindAtom']).
metta_other_asserted('&corelib',[iz,['@kind',atom],'DataFunctor']).
metta_other_asserted('&corelib',[iz,'@type','MeTTa']).
metta_other_asserted('&corelib',['@doc','@type',['@desc',"Used for documentation purposes. Converts atom/function's type to DocType"],['@params',[['@param',"Atom/Function type to be documented"]]],['@return',"(@type Type) entity"]]).
metta_other_asserted('&corelib',[:,'@type',[->,'Type','DocType']]).
metta_other_asserted('&corelib',[iz,'@type','DataFunctor']).
metta_other_asserted('&corelib',[iz,'@params','MeTTa']).
metta_other_asserted('&corelib',['@doc','@params',['@desc',"Used for function documentation purposes. Contains several @param entities with description of each @param"],['@params',[['@param',"Several (@param ...) entities"]]],['@return',"DocParameters containing description of all parameters of function in form of (@params ((@param ...) (@param ...) ...))"]]).
metta_other_asserted('&corelib',[:,'@params',[->,'Expression','DocParameters']]).
metta_other_asserted('&corelib',[iz,'@params','DataFunctor']).
metta_other_asserted('&corelib',[iz,'get-doc','MeTTa']).
metta_other_asserted('&corelib',['@doc','get-doc',['@desc',"Returns documentation for the given Atom/Function"],['@params',[['@param',"Atom/Function name for which documentation is needed"]]],['@return',"Documentation for the given atom/function"]]).
metta_other_asserted('&corelib',[:,'get-doc',[->,'Atom','Atom']]).
metta_function_asserted('&corelib',['get-doc',_atom],[let,_meta_type,['get-metatype',_atom],[case,_meta_type,[['Expression',['get-doc-atom',_atom]],[_,['get-doc-single-atom',_atom]]]]]).
transpiler_clause_store('get-doc',[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),['get-doc',_atom],[let,_meta_type,['get-metatype',_atom],[case,_meta_type,[['Expression',['get-doc-atom',_atom]],[_,['get-doc-single-atom',_atom]]]]]).


'mi__1_1_get-doc'(B,C) :- 
  D='mc__1_1_get-doc'(B,C) , 
  ci(true,'get-doc',1,['get-doc',B],C,true,D).




'me__1_1_get-doc'(B,C):-'mc__1_1_get-doc'(B,C).




'mc__1_1_get-doc'(A,B) :-  
  B =  
    ispeEnNC( C, 
        ((
         (  ((
             (  ((
                 (D='Expression')*->
                 (  ((
                     (E =  
                        ispeEnN( F, 
                          'mi__1_1_get-doc-atom'(A,G),as_p1_exec(G,F), 
                          H, 
                          I=['get-doc-atom',A],as_p1_expr(I,H))),
                     (J=E)  )))  )));
             (  ((
                 (K =  
                    ispeEnN( L, 
                        ((
                         (  ((
                             (  ((
                                 (D=M)*->
                                 (  ((
                                     (N =  
                                        ispeEnN( O, 
                                          'mi__1_1_get-doc-single-atom'(A,P),as_p1_exec(P,O), 
                                          Q, 
                                          R=['get-doc-single-atom',A],as_p1_expr(R,Q))),
                                     (S=N)  )))  )));
                             (T=ispuU(U,mc__1_0_empty(U)),S=T)  ))),
                         (as_p1_exec(S,L))  )), 
                      V, 
                        ((
                         (  ((
                             (  ((
                                 (D=M)*->
                                 (R=['get-doc-single-atom',A],S=R)  )));
                             (mc__1_0_empty(U),S=U)  ))),
                         (as_p1_expr(S,V))  )))),
                 (J=K)  )))  ))),
         (as_p1_exec(J,C))  )), 
      W, 
        ((
         (  ((
             (  ((
                 (D='Expression')*->
                 (I=['get-doc-atom',A],J=I)  )));
             (  ((
                 (  ((
                     (  ((
                         (D=M)*->
                         (R=['get-doc-single-atom',A],S=R)  )));
                     (mc__1_0_empty(U),S=U)  ))),
                 (J=S)  )))  ))),
         (as_p1_expr(J,W))  )), 
      ( as_p1_expr(A,X)  ,
        'mi__1_1_get-metatype'(X,Y) , 
        Z=Y , 
        true*->D=Z;D='Empty')).


metta_other_asserted('&corelib',[iz,'get-doc-single-atom','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','get-doc-single-atom',['@desc',"Function used by get-doc to get documentation on either function or atom. It checks if input name is the name of function or atom and calls correspondent function"],['@params',[['@param',"Atom/Function name for which documentation is needed"]]],['@return',"Documentation for the given atom/function"]]).
metta_other_asserted('&corelib',[:,'get-doc-single-atom',[->,'Atom','Atom']]).
metta_function_asserted('&corelib',['get-doc-single-atom',_atom],[let,_top_space,['mod-space!',top],[let,_type,['get-type-space',_top_space,_atom],[if,['is-function-type',_type],['get-doc-function',_atom,_type],['get-doc-atom',_atom]]]]).
transpiler_clause_store('get-doc-single-atom',[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),['get-doc-single-atom',_atom],[let,_top_space,['mod-space!',top],[let,_type,['get-type-space',_top_space,_atom],[if,['is-function-type',_type],['get-doc-function',_atom,_type],['get-doc-atom',_atom]]]]).


'mi__1_1_get-doc-single-atom'(B,C) :- 
  D='mc__1_1_get-doc-single-atom'(B,C) , 
  ci(true,'get-doc-single-atom',1,['get-doc-single-atom',B],C,true,D).




'me__1_1_get-doc-single-atom'(B,C):-'mc__1_1_get-doc-single-atom'(B,C).




'mc__1_1_get-doc-single-atom'(A,B) :-  
  B =  
    ispeEnNC( C, 
        ((
         (  ((
             (  ((
                 ('mi__1_1_is-function-type'(D,E),is_True(E))*->
                 ('mi__1_2_get-doc-function'(A,D,F),G=F)  )));
             ('mi__1_1_get-doc-atom'(A,H),G=H)  ))),
         (as_p1_exec(G,C))  )), 
      I, 
        ((
         (  ((
             (  ((
                 ('mi__1_1_is-function-type'(D,E),is_True(E))*->
                 (J=['get-doc-function',A,D],K=J)  )));
             (L=['get-doc-atom',A],K=L)  ))),
         (as_p1_expr(K,I))  )), 
      ( M=ispu(top)  ,
        'mi__1_1_mod-space!'(M,N) , 
        O=N , 
        'mi__1_2_get-type-space'(O,A,P) , 
        as_p1_exec(P,Q) , 
        D=Q)).


metta_other_asserted('&corelib',[iz,'get-doc-function','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','get-doc-function',['@desc',"Function used by get-doc-single-atom to get documentation on a function. It returns documentation on a function if it exists or default documentation with no description otherwise"],['@params',[['@param',"Function name for which documentation is needed"],['@param',"Type notation for this function"]]],['@return',"Documentation for the given function"]]).
metta_other_asserted('&corelib',[:,'get-doc-function',[->,'Atom','Type','Atom']]).
metta_function_asserted('&corelib',['get-doc-function',_name,_type],[let,_top_space,['mod-space!',top],[unify,_top_space,['@doc',_name,_desc,['@params',_params],_ret],[let,_type_39,[if,[==,_type,'%Undefined%'],['undefined-doc-function-type',_params],['cdr-atom',_type]],[let,[_params_39,_ret_39],['get-doc-params',_params,_ret,_type_39],['@doc-formal',['@item',_name],['@kind',function],['@type',_type],_desc,['@params',_params_39],_ret_39]]],['@doc-formal',['@item',_name],['@kind',function],['@type',_type],['@desc',"No documentation"]]]]).
transpiler_clause_store('get-doc-function',[2],1,['Atom','Type'],'Atom',[x(noeval,lazy,[]),x(doeval,eager,['Type'])],x(noeval,lazy,[]),['get-doc-function',_name,_type],[let,_top_space,['mod-space!',top],[unify,_top_space,['@doc',_name,_desc,['@params',_params],_ret],[let,_type_39,[if,[==,_type,'%Undefined%'],['undefined-doc-function-type',_params],['cdr-atom',_type]],[let,[_params_39,_ret_39],['get-doc-params',_params,_ret,_type_39],['@doc-formal',['@item',_name],['@kind',function],['@type',_type],_desc,['@params',_params_39],_ret_39]]],['@doc-formal',['@item',_name],['@kind',function],['@type',_type],['@desc',"No documentation"]]]]).


'mi__1_2_get-doc-function'(B,C,D) :- 
  E='mc__1_2_get-doc-function'(B,C,D) , 
  ci(true,'get-doc-function',2,['get-doc-function',B,C],D,true,E).




'me__1_2_get-doc-function'(B,C,D):-'mc__1_2_get-doc-function'(B,C,D).




'mc__1_2_get-doc-function'(A,B,C) :-  
  C =  
    ispeEnNC( D, 
      mi__1_4_unify(E,F,G,H,D), 
      I, 
      I =  
        [ unify, E,F,G,H], 
      ( J=ispu(top)  ,
        'mi__1_1_mod-space!'(J,K) , 
        E=K , 
        as_p1_exec(A,L) , 
        M=['@params',N] , 
        F =  
          [ '@doc', L,O,M,P] , 
        G =  
          ispeEnNC( Q, 
            ( as_p1_exec(A,R)  ,
              S=['@item',R] , 
              T=['@kind',function] , 
              U=['@type',B] , 
              V=['@params',W] , 
              Q =  
                [ '@doc-formal', S,T,U,O, 
                  V,X]), 
            Y, 
            ( as_p1_expr(A,Z)  ,
              A1=['@item',Z] , 
              B1=['@kind',function] , 
              C1=['@type',B] , 
              D1=['@params',W] , 
              Y =  
                [ '@doc-formal', A1,B1,C1,O, 
                  D1,X]), 
            (   ((
                 (  ((
                     ('mi__1_2_=='(B,'%Undefined%',E1),is_True(E1))*->
                     ('mi__1_1_undefined-doc-function-type'(N,F1),G1=F1)  )));
                 ('mi__1_1_cdr-atom'(B,H1),G1=H1)  ))  ,
              I1=G1 , 
              J1=ispu(P) , 
              'mi__1_3_get-doc-params'(N,J1,I1,K1) , 
              [W,X]=K1)) , 
        H =  
          ispeEnN( L1, 
            ( as_p1_exec(A,M1)  ,
              N1=['@item',M1] , 
              O1=['@kind',function] , 
              P1=['@type',B] , 
              Q1 =  
                [ '@desc', 
                  "No documentation"] , 
              L1 =  
                [ '@doc-formal', N1,O1,P1,Q1]), 
            R1, 
            ( as_p1_expr(A,S1)  ,
              T1=['@item',S1] , 
              U1=['@kind',function] , 
              V1=['@type',B] , 
              W1 =  
                [ '@desc', 
                  "No documentation"] , 
              R1 =  
                [ '@doc-formal', T1,U1,V1,W1])))).


metta_other_asserted('&corelib',[iz,'undefined-doc-function-type','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','undefined-doc-function-type',['@desc',"Function used by get-doc-single-atom in case of absence of function's type notation"],['@params',[['@param',"List of parameters for the function we want to get documentation for"]]],['@return',"List of %Undefined% number of which depends on input list size. So for two parameters function will return (%Undefined% %Undefined% %Undefined%)"]]).
metta_other_asserted('&corelib',[:,'undefined-doc-function-type',[->,'Expression','Type']]).
metta_function_asserted('&corelib',['undefined-doc-function-type',_params],[if,[==,[],_params],['%Undefined%'],[let,_params_tail,['cdr-atom',_params],[let,_tail,['undefined-doc-function-type',_params_tail],['cons-atom','%Undefined%',_tail]]]]).
transpiler_clause_store('undefined-doc-function-type',[1],1,['Expression'],'Type',[x(noeval,eager,[])],x(doeval,eager,['Type']),['undefined-doc-function-type',_params],[if,[==,[],_params],['%Undefined%'],[let,_params_tail,['cdr-atom',_params],[let,_tail,['undefined-doc-function-type',_params_tail],['cons-atom','%Undefined%',_tail]]]]).


'mi__1_1_undefined-doc-function-type'(B,C) :- 
  D='mc__1_1_undefined-doc-function-type'(B,C) , 
  ci( true, 
    'undefined-doc-function-type', 
    1, 
    ['undefined-doc-function-type',B], C,true,D).




'me__1_1_undefined-doc-function-type'(B,C) :-  
  'mc__1_1_undefined-doc-function-type'(B,C).




'mc__1_1_undefined-doc-function-type'(A,B) :-  
    ((
     (  ((
         ('mi__1_2_=='([],A,C),is_True(C))*->
         (D=['%Undefined%'],B=D)  )));
     (( 'mi__1_1_cdr-atom'(A,E)  ,
        F=E , 
        'mi__1_1_undefined-doc-function-type'(F,G) , 
        H=G , 
        'mi__1_2_cons-atom'('%Undefined%',H,I) , 
        B=I))  )).


metta_other_asserted('&corelib',[iz,'get-doc-params','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','get-doc-params',['@desc',"Function used by get-doc-function to get function's parameters documentation (including return value)"],['@params',[['@param',"List of parameters in form of ((@param Description) (@param Description)...)"],['@param',"Return value's description in form of (@return Description)"],['@param',"Type notation without -> starting symbol e.g. (Atom Atom Atom)"]]],['@return',"United list of params and return value each augmented with its type. E.g. (((@param (@type Atom) (@desc Description)) (@param (@type Atom) (@desc Description2))) (@return (@type Atom) (@desc Description)))"]]).
metta_other_asserted('&corelib',[:,'get-doc-params',[->,'Expression','Atom','Expression',['Expression','Atom']]]).
metta_function_asserted('&corelib',['get-doc-params',_params,_ret,_types],[let,_head_type,['car-atom',_types],[let,_tail_types,['cdr-atom',_types],[if,[==,[],_params],[let,['@return',_ret_desc],_ret,[[],['@return',['@type',_head_type],['@desc',_ret_desc]]]],[let,['@param',_param_desc],['car-atom',_params],[let,_tail_params,['cdr-atom',_params],[let,[_params_39,_result_ret],['get-doc-params',_tail_params,_ret,_tail_types],[let,_result_params,['cons-atom',['@param',['@type',_head_type],['@desc',_param_desc]],_params_39],[_result_params,_result_ret]]]]]]]]).
transpiler_clause_store('get-doc-params',[3],1,['Expression','Atom','Expression'],['Expression','Atom'],[x(noeval,eager,[]),x(noeval,lazy,[]),x(noeval,eager,[])],x(doeval,eager,[['Expression','Atom']]),['get-doc-params',_params,_ret,_types],[let,_head_type,['car-atom',_types],[let,_tail_types,['cdr-atom',_types],[if,[==,[],_params],[let,['@return',_ret_desc],_ret,[[],['@return',['@type',_head_type],['@desc',_ret_desc]]]],[let,['@param',_param_desc],['car-atom',_params],[let,_tail_params,['cdr-atom',_params],[let,[_params_39,_result_ret],['get-doc-params',_tail_params,_ret,_tail_types],[let,_result_params,['cons-atom',['@param',['@type',_head_type],['@desc',_param_desc]],_params_39],[_result_params,_result_ret]]]]]]]]).


'mi__1_3_get-doc-params'(B,C,D,E) :- 
  F='mc__1_3_get-doc-params'(B,C,D,E) , 
  ci( true, 
    'get-doc-params', 
    3, 
    ['get-doc-params',B,C,D], E,true,F).




'me__1_3_get-doc-params'(B,C,D,E) :-  
  'mc__1_3_get-doc-params'(B,C,D,E).




'mc__1_3_get-doc-params'(A,B,C,D) :- 
  'mi__1_1_car-atom'(C,E) , 
  F=E , 
  'mi__1_1_cdr-atom'(C,G) , 
  H=G , 
    ((
     (  ((
         ('mi__1_2_=='([],A,I),is_True(I))*->
         (( as_p1_exec(B,J)  ,
            ['@return',K]=J , 
            L=['@type',F] , 
            M=['@desc',K] , 
            N=['@return',L,M] , 
            O=[[],N] , 
            D=O))  )));
     (( 'mi__1_1_car-atom'(A,P)  ,
        ['@param',Q]=P , 
        'mi__1_1_cdr-atom'(A,R) , 
        S=R , 
        'mi__1_3_get-doc-params'(S,B,H,T) , 
        [U,V]=T , 
        W=['@type',F] , 
        X=['@desc',Q] , 
        Y=['@param',W,X] , 
        'mi__1_2_cons-atom'(Y,U,Z) , 
        A1=Z , 
        transpiler_apply( mc__1_1_, 
          A1, 
          [A1,V], 
          B1, 
          [V], 
          [V], 
          [x(noeval,eager,[])], 
          [true], 
          [true]) , 
        D=B1))  )).


metta_other_asserted('&corelib',[iz,'get-doc-atom','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','get-doc-atom',['@desc',"Function used by get-doc (in case of input type Expression) and get-doc-single-atom (in case input value is not a function) to get documentation on input value"],['@params',[['@param',"Atom's name to get documentation for"]]],['@return',"Documentation on input Atom"]]).
metta_other_asserted('&corelib',[:,'get-doc-atom',[->,'Atom','Atom']]).
metta_function_asserted('&corelib',['get-doc-atom',_atom],[let,_top_space,['mod-space!',top],[let,_type,['get-type-space',_top_space,_atom],[unify,_top_space,['@doc',_atom,_desc],['@doc-formal',['@item',_atom],['@kind',atom],['@type',_type],_desc],[unify,_top_space,['@doc',_atom,_desc_39,['@params',_params],_ret],['get-doc-function',_atom,'%Undefined%'],['@doc-formal',['@item',_atom],['@kind',atom],['@type',_type],['@desc',"No documentation"]]]]]]).
transpiler_clause_store('get-doc-atom',[1],1,['Atom'],'Atom',[x(noeval,lazy,[])],x(noeval,lazy,[]),['get-doc-atom',_atom],[let,_top_space,['mod-space!',top],[let,_type,['get-type-space',_top_space,_atom],[unify,_top_space,['@doc',_atom,_desc],['@doc-formal',['@item',_atom],['@kind',atom],['@type',_type],_desc],[unify,_top_space,['@doc',_atom,_desc_39,['@params',_params],_ret],['get-doc-function',_atom,'%Undefined%'],['@doc-formal',['@item',_atom],['@kind',atom],['@type',_type],['@desc',"No documentation"]]]]]]).


'mi__1_1_get-doc-atom'(B,C) :- 
  D='mc__1_1_get-doc-atom'(B,C) , 
  ci(true,'get-doc-atom',1,['get-doc-atom',B],C,true,D).




'me__1_1_get-doc-atom'(B,C):-'mc__1_1_get-doc-atom'(B,C).




'mc__1_1_get-doc-atom'(A,B) :-  
  B =  
    ispeEnNC( C, 
      mi__1_4_unify(D,E,F,G,C), 
      H, 
      H =  
        [ unify, D,E,F,G], 
      ( I=ispu(top)  ,
        'mi__1_1_mod-space!'(I,J) , 
        D=J , 
        'mi__1_2_get-type-space'(D,A,K) , 
        as_p1_exec(K,L) , 
        M=L , 
        as_p1_exec(A,N) , 
        E=['@doc',N,O] , 
        F =  
          ispeEnN( P, 
            ( as_p1_exec(A,Q)  ,
              R=['@item',Q] , 
              S=['@kind',atom] , 
              T=['@type',M] , 
              P =  
                [ '@doc-formal', R,S,T,O]), 
            U, 
            ( as_p1_expr(A,V)  ,
              W=['@item',V] , 
              X=['@kind',atom] , 
              Y=['@type',M] , 
              U =  
                [ '@doc-formal', W,X,Y,O])) , 
        G =  
          ispeEnNC( Z, 
            mi__1_4_unify(D,A1,B1,C1,Z), 
            D1, 
            D1 =  
              [ unify, D,A1,B1,C1], 
            ( as_p1_exec(A,E1)  ,
              F1=['@params',_] , 
              A1 =  
                [ '@doc', E1,_,F1,_] , 
              B1 =  
                ispeEnN( G1, 
                  'mi__1_2_get-doc-function'(A,'%Undefined%',H1),as_p1_exec(H1,G1), 
                  I1, 
                    ((
                     (J1=['get-doc-function',A,'%Undefined%']),
                     (as_p1_expr(J1,I1))  ))) , 
              C1 =  
                ispeEnN( K1, 
                  ( as_p1_exec(A,L1)  ,
                    M1=['@item',L1] , 
                    N1=['@kind',atom] , 
                    O1=['@type',M] , 
                    P1 =  
                      [ '@desc', 
                        "No documentation"] , 
                    K1 =  
                      [ '@doc-formal', M1,N1,O1,P1]), 
                  Q1, 
                  ( as_p1_expr(A,R1)  ,
                    S1=['@item',R1] , 
                    T1=['@kind',atom] , 
                    U1=['@type',M] , 
                    V1 =  
                      [ '@desc', 
                        "No documentation"] , 
                    Q1 =  
                      [ '@doc-formal', S1,T1,U1,V1])))))).


metta_other_asserted('&corelib',[iz,'help!','MeTTa']).
metta_other_asserted('&corelib',['@doc','help!',['@desc',"Function prints documentation for the input atom. Without parameters prints the list of the stdlib functions."],['@params',[['@param',"Input to get documentation for"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'help!',[->,'Atom',[->]]]).
metta_function_asserted('&corelib',['help!',_atom],[case,['get-doc',_atom],[[['@doc-formal',['@item',_item],['@kind',function],['@type',_type],['@desc',_descr],['@params',_params],['@return',['@type',_ret_type],['@desc',_ret_desc]]],[let,[],['println!',['format-args',"Function {}: {} {}",[_item,_type,_descr]]],[let,[],['println!',['format-args',"Parameters:",[]]],[let,[],['for-each-in-atom',_params,'help-param!'],[let,[],['println!',['format-args',"Return: (type {}) {}",[_ret_type,_ret_desc]]],[]]]]]],[['@doc-formal',['@item',_item],['@kind',function],['@type',_type],['@desc',_descr]],[let,[],['println!',['format-args',"Function {} (type {}) {}",[_item,_type,_descr]]],[]]],[['@doc-formal',['@item',_item],['@kind',atom],['@type',_type],['@desc',_descr]],[let,[],['println!',['format-args',"Atom {}: {} {}",[_item,_type,_descr]]],[]]],[_other,['Error',_other,"Cannot match @doc-formal structure"]]]]).
transpiler_clause_store('help!',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['help!',_atom],[case,['get-doc',_atom],[[['@doc-formal',['@item',_item],['@kind',function],['@type',_type],['@desc',_descr],['@params',_params],['@return',['@type',_ret_type],['@desc',_ret_desc]]],[let,[],['println!',['format-args',"Function {}: {} {}",[_item,_type,_descr]]],[let,[],['println!',['format-args',"Parameters:",[]]],[let,[],['for-each-in-atom',_params,'help-param!'],[let,[],['println!',['format-args',"Return: (type {}) {}",[_ret_type,_ret_desc]]],[]]]]]],[['@doc-formal',['@item',_item],['@kind',function],['@type',_type],['@desc',_descr]],[let,[],['println!',['format-args',"Function {} (type {}) {}",[_item,_type,_descr]]],[]]],[['@doc-formal',['@item',_item],['@kind',atom],['@type',_type],['@desc',_descr]],[let,[],['println!',['format-args',"Atom {}: {} {}",[_item,_type,_descr]]],[]]],[_other,['Error',_other,"Cannot match @doc-formal structure"]]]]).


'mi__1_1_help!'(B,C) :- 
  D='mc__1_1_help!'(B,C) , 
  ci(true,'help!',1,['help!',B],C,true,D).




'me__1_1_help!'(B,C):-'mc__1_1_help!'(B,C).




'mc__1_1_help!'(A,B) :- 
    ((
     (  ((
         (C=ispu(A),'mi__1_1_get-doc'(C,D),as_p1_exec(D,E))*->
         (F=E)  )));
     (F='Empty')  )) , 
  G=['@item',H] , 
  I=['@kind',function] , 
  J=['@type',K] , 
  L=['@desc',M] , 
  N=['@params',O] , 
  P=['@type',Q] , 
  R=['@desc',S] , 
  T=['@return',P,R] , 
  U =  
    [ '@doc-formal', G,I,J,L, 
      N,T] , 
    ((
     (  ((
         (F=U)*->
         (( V=[H,K,M]  ,
            'mi__1_2_format-args'("Function {}: {} {}",V,W) , 
            'mi__1_1_println!'(W,X) , 
            []=X , 
            'mi__1_2_format-args'("Parameters:",[],Y) , 
            'mi__1_1_println!'(Y,Z) , 
            []=Z , 
            'mi__1_2_for-each-in-atom'(O,'help-param!',A1) , 
            []=A1 , 
            B1=[Q,S] , 
            'mi__1_2_format-args'("Return: (type {}) {}",B1,C1) , 
            'mi__1_1_println!'(C1,D1) , 
            []=D1 , 
            B=[]))  )));
     (( E1=['@item',H]  ,
        F1=['@kind',function] , 
        G1=['@type',K] , 
        H1=['@desc',M] , 
        I1 =  
          [ '@doc-formal', E1,F1,G1,H1] , 
          ((
           (  ((
               (F=I1)*->
               (( J1=[H,K,M]  ,
                  'mi__1_2_format-args'("Function {} (type {}) {}",J1,K1) , 
                  'mi__1_1_println!'(K1,L1) , 
                  []=L1 , 
                  M1=[]))  )));
           (( N1=['@item',H]  ,
              O1=['@kind',atom] , 
              P1=['@type',K] , 
              Q1=['@desc',M] , 
              R1 =  
                [ '@doc-formal', N1,O1,P1,Q1] , 
                ((
                 (  ((
                     (F=R1)*->
                     (( S1=[H,K,M]  ,
                        'mi__1_2_format-args'("Atom {}: {} {}",S1,T1) , 
                        'mi__1_1_println!'(T1,U1) , 
                        []=U1 , 
                        V1=[]))  )));
                 (  ((
                     (  ((
                         (  ((
                             (F=W1)*->
                             (X1 =  
                                [ 'Error', W1,"Cannot match @doc-formal structure"])  )));
                         (mc__1_0_empty(Y1),X1=Y1)  ))),
                     (V1=X1)  )))  )) , 
              M1=V1))  )) , 
        B=M1))  )).


metta_other_asserted('&corelib',['@doc','help!',['@desc',"Without parameters prints the list of the stdlib functions."],['@params',[]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'help!',[->,[->]]]).
metta_function_asserted('&corelib',['help!'],[let,_top_space,['mod-space!',top],[unify,_top_space,['@doc',_name,['@desc',_desc]],[let,[],['println!',['format-args',"{}\n\t{}",[_name,_desc]]],'Empty'],'Empty']]).
transpiler_clause_store('help!',[0],1,[],'Any',[],x(doeval,eager,[]),['help!'],[let,_top_space,['mod-space!',top],[unify,_top_space,['@doc',_name,['@desc',_desc]],[let,[],['println!',['format-args',"{}\n\t{}",[_name,_desc]]],'Empty'],'Empty']]).


'mi__1_0_help!'(B) :- 
  C='mc__1_0_help!'(B) , 
  ci(true,'help!',0,['help!'],B,true,C).




'me__1_0_help!'(B):-'mc__1_0_help!'(B).




'mc__1_0_help!'(A) :- 
  B=ispu(top) , 
  'mi__1_1_mod-space!'(B,C) , 
  D=C , 
  E=['@desc',F] , 
  G=['@doc',H,E] , 
  I =  
    ispuU( 'Empty', 
      ( J=[H,F]  ,
        'mi__1_2_format-args'("{}\n\t{}",J,K) , 
        'mi__1_1_println!'(K,L) , 
        []=L)) , 
  M=ispu('Empty') , 
  mi__1_4_unify(D,G,I,M,A).


metta_other_asserted('&corelib',[iz,'help-param!','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','help-param!',['@desc',"Function used by function help! to output parameters using println!"],['@params',[['@param',"Parameters list"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'help-param!',[->,'Atom',[->]]]).
metta_function_asserted('&corelib',['help-param!',_param],[let,['@param',['@type',_type],['@desc',_desc]],_param,['println!',['format-args',"  {} {}",[[type,_type],_desc]]]]).
transpiler_clause_store('help-param!',[1],1,['Any'],'Any',[x(doeval,eager,[])],x(doeval,eager,[]),['help-param!',_param],[let,['@param',['@type',_type],['@desc',_desc]],_param,['println!',['format-args',"  {} {}",[[type,_type],_desc]]]]).


'mi__1_1_help-param!'(B,C) :- 
  D='mc__1_1_help-param!'(B,C) , 
  ci(true,'help-param!',1,['help-param!',B],C,true,D).




'me__1_1_help-param!'(B,C):-'mc__1_1_help-param!'(B,C).




'mc__1_1_help-param!'(A,B) :- 
  [ '@param', 
    ['@type',C], 
    ['@desc',D]] =  
    A , 
  E=[type,C] , 
  F=[E,D] , 
  'mi__1_2_format-args'("  {} {}",F,G) , 
  'mi__1_1_println!'(G,B).


metta_other_asserted('&corelib',[iz,'for-each-in-atom','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','for-each-in-atom',['@desc',"Applies function passed as a second argument to each atom inside first argument"],['@params',[['@param',"Expression to each atom in which function will be applied"],['@param',"Function to apply"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'for-each-in-atom',[->,'Expression','Atom',[->]]]).
metta_function_asserted('&corelib',['for-each-in-atom',_expr,_func],[if,['noreduce-eq',_expr,[]],[],[let,_head,['car-atom',_expr],[let,_tail,['cdr-atom',_expr],[let,_,[_func,_head],['for-each-in-atom',_tail,_func]]]]]).
transpiler_clause_store('for-each-in-atom',[2],1,['Any','Any'],'Any',[x(doeval,eager,[]),x(doeval,eager,[])],x(doeval,eager,[]),['for-each-in-atom',_expr,_func],[if,['noreduce-eq',_expr,[]],[],[let,_head,['car-atom',_expr],[let,_tail,['cdr-atom',_expr],[let,_,[_func,_head],['for-each-in-atom',_tail,_func]]]]]).


'mi__1_2_for-each-in-atom'(B,C,D) :- 
  E='mc__1_2_for-each-in-atom'(B,C,D) , 
  ci(true,'for-each-in-atom',2,['for-each-in-atom',B,C],D,true,E).




'me__1_2_for-each-in-atom'(B,C,D):-'mc__1_2_for-each-in-atom'(B,C,D).




'mc__1_2_for-each-in-atom'(A,B,C) :-  
    ((
     (  ((
         (( D=ispu(A)  ,
            E=ispu([]) , 
            'mi__1_2_noreduce-eq'(D,E,F) , 
            is_True(F)))*->
         (C=[])  )));
     (( 'mi__1_1_car-atom'(A,G)  ,
        H=G , 
        'mi__1_1_cdr-atom'(A,I) , 
        J=I , 
        transpiler_apply( mc__1_1_, 
          B, 
          [B,H], 
          K, 
          [H], 
          [H], 
          [x(noeval,eager,[])], 
          [true], 
          [true]) , 
        _=K , 
        'mi__1_2_for-each-in-atom'(J,B,L) , 
        C=L))  )).


metta_other_asserted('&corelib',[iz,'noreduce-eq','PrivateRelation']).
metta_other_asserted('&corelib',['@doc','noreduce-eq',['@desc',"Checks equality of two atoms without reducing them"],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if not reduced atoms are equal, False - otherwise"]]).
metta_other_asserted('&corelib',[:,'noreduce-eq',[->,'Atom','Atom','Bool']]).
metta_function_asserted('&corelib',['noreduce-eq',_a,_b],[==,[quote,_a],[quote,_b]]).
transpiler_clause_store('noreduce-eq',[2],1,['Atom','Atom'],'Bool',[x(noeval,lazy,[]),x(noeval,lazy,[])],x(doeval,eager,[boolean]),['noreduce-eq',_a,_b],[==,[quote,_a],[quote,_b]]).


'mi__1_2_noreduce-eq'(B,C,D) :- 
  E='mc__1_2_noreduce-eq'(B,C,D) , 
  ci(true,'noreduce-eq',2,['noreduce-eq',B,C],D,true,E).




'me__1_2_noreduce-eq'(B,C,D):-'mc__1_2_noreduce-eq'(B,C,D).




'mc__1_2_noreduce-eq'(A,B,C) :-  
  'mi__1_2_=='([quote,A],[quote,B],C).


%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%  ; Grounded function's documentation
%  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
metta_other_asserted('&corelib',[iz,'add-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','add-atom',['@desc',"Adds atom into the atomspace without reducing it"],['@params',[['@param',"Atomspace to add atom into"],['@param',"Atom to add"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'add-atom',[->,'hyperon::space::DynSpace','Atom',[->]]]).
metta_other_asserted('&corelib',[iz,'get-type','MeTTa']).
metta_other_asserted('&corelib',['@doc','get-type',['@desc',"Returns type notation of input atom"],['@params',[['@param',"Atom to get type for"]]],['@return',"Type notation or %Undefined% if there is no type for input Atom"]]).
metta_other_asserted('&corelib',[:,'get-type',[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[:,'get-type',[->,'Atom','hyperon::space::DynSpace','Atom']]).
metta_other_asserted('&corelib',[iz,'get-type-space','MeTTa']).
metta_other_asserted('&corelib',['@doc','get-type-space',['@desc',"Returns type notation of input Atom (second argument) relative to a specified atomspace (first argument)"],['@params',[['@param',"Atomspace where type notation for input atom will be searched"],['@param',"Atom to get type for"]]],['@return',"Type notation or %Undefined% if there is no type for input Atom in provided atomspace"]]).
metta_other_asserted('&corelib',[iz,'get-type-space','MeTTa']).
metta_other_asserted('&corelib',[:,'get-type-space',[->,'hyperon::space::DynSpace','Atom','Atom']]).
metta_function_asserted('&corelib',['get-type-space',_space,_atom],['get-type',_atom,_space]).
transpiler_clause_store('get-type-space',[2],1,['hyperon::space::DynSpace','Atom'],'Atom',[x(doeval,eager,['hyperon::space::DynSpace']),x(noeval,lazy,[])],x(noeval,lazy,[]),['get-type-space',_space,_atom],['get-type',_atom,_space]).


'mi__1_2_get-type-space'(B,C,D) :- 
  E='mc__1_2_get-type-space'(B,C,D) , 
  ci(true,'get-type-space',2,['get-type-space',B,C],D,true,E).




'me__1_2_get-type-space'(B,C,D):-'mc__1_2_get-type-space'(B,C,D).




'mc__1_2_get-type-space'(A,B,C) :-  
  C =  
    ispeEnN( D, 
      as_p1_exec(B,E),D=['get-type',E,A], 
      F, 
      as_p1_expr(B,G),F=['get-type',G,A]).


metta_other_asserted('&corelib',[iz,'get-metatype','MeTTa']).
metta_other_asserted('&corelib',['@doc','get-metatype',['@desc',"Returns metatype of the input atom"],['@params',[['@param',"Atom to get metatype for"]]],['@return',"Metatype of input atom"]]).
metta_other_asserted('&corelib',[:,'get-metatype',[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'if-equal','MinimalMeTTa']).
metta_other_asserted('&corelib',['@doc','if-equal',['@desc',"Checks if first two arguments are equal and evaluates third argument if equal, fourth argument - otherwise"],['@params',[['@param',"First argument"],['@param',"Second argument"],['@param',"Atom to be evaluated if arguments are equal"],['@param',"Atom to be evaluated if arguments are not equal"]]],['@return',"Evaluated third or fourth argument"]]).
metta_other_asserted('&corelib',[iz,'new-space','MeTTa']).
metta_other_asserted('&corelib',['@doc','new-space',['@desc',"Creates new Atomspace which could be used further in the program as a separate from &self Atomspace"],['@params',[]],['@return',"Reference to a new space"]]).
metta_other_asserted('&corelib',[:,'new-space',[->,'hyperon::space::DynSpace']]).
metta_other_asserted('&corelib',[iz,'remove-atom','MeTTa']).
metta_other_asserted('&corelib',['@doc','remove-atom',['@desc',"Removes atom from the input Atomspace"],['@params',[['@param',"Reference to the space from which the Atom needs to be removed"],['@param',"Atom to be removed"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'remove-atom',[->,'hyperon::space::DynSpace','Atom',[->]]]).
metta_other_asserted('&corelib',[iz,'get-atoms','MeTTa']).
metta_other_asserted('&corelib',['@doc','get-atoms',['@desc',"Shows all atoms in the input Atomspace"],['@params',[['@param',"Reference to the space"]]],['@return',"List of all atoms in the input space"]]).
metta_other_asserted('&corelib',[:,'get-atoms',[->,'hyperon::space::DynSpace','Atom']]).
metta_other_asserted('&corelib',[iz,'new-state','MeTTa']).
metta_other_asserted('&corelib',['@doc','new-state',['@desc',"Creates a new state atom wrapping its argument"],['@params',[['@param',"Atom to be wrapped"]]],['@return',"Returns (State $value) where $value is an argument to a new-state"]]).
metta_other_asserted('&corelib',[:,'new-state',[->,_tnso,['StateMonad',_tnso]]]).
metta_other_asserted('&corelib',[iz,'change-state!','MeTTa']).
metta_other_asserted('&corelib',['@doc','change-state!',['@desc',"Changes input state's wrapped atom to another value (second argument). E.g. (change-state! (State 5) 6) -> (State 6)"],['@params',[['@param',"State created by new-state function"],['@param',"Atom which will replace wrapped atom in the input state"]]],['@return',"State with replaced wrapped atom"]]).
metta_other_asserted('&corelib',[:,'change-state!',[->,['StateMonad',_tcso],_tcso,['StateMonad',_tcso]]]).
metta_other_asserted('&corelib',[iz,'get-state','MeTTa']).
metta_other_asserted('&corelib',['@doc','get-state',['@desc',"Gets a state as an argument and returns its wrapped atom. E.g. (get-state (State 5)) -> 5"],['@params',[['@param',"State"]]],['@return',"Atom wrapped by state"]]).
metta_other_asserted('&corelib',[:,'get-state',[->,['StateMonad',_tgso],_tgso]]).
metta_other_asserted('&corelib',[iz,match,'MeTTa']).
metta_other_asserted('&corelib',['@doc',match,['@desc',"Searches for all declared atoms corresponding to the given pattern (second argument) and produces the output pattern (third argument)"],['@params',[['@param',"Atomspace to search pattern"],['@param',"Pattern atom to be searched"],['@param',"Output template typically containing variables from the input pattern"]]],['@return',"If match was successful it outputs template (third argument) with filled variables (if any were present in pattern) using matched pattern (second argument). Empty - otherwise"]]).
metta_other_asserted('&corelib',[:,match,[->,'Atom','Atom','Atom','%Undefined%']]).
%  ;(ALT= (match $space $pattern $template)
%  ;  (unify $space $pattern $template Empty))
metta_other_asserted('&corelib',[iz,'register-module!','MeTTa']).
metta_other_asserted('&corelib',['@doc','register-module!',['@desc',"Takes a file system path (first argument) and loads the module into the runner"],['@params',[['@param',"File system path"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'register-module!',[->,'Atom',[->]]]).
metta_other_asserted('&corelib',[iz,'mod-space!','MeTTa']).
metta_other_asserted('&corelib',['@doc','mod-space!',['@desc',"Returns the space of the module (first argument) and tries to load the module if it is not loaded into the module system"],['@params',[['@param',"Module name"]]],['@return',"Space name"]]).
metta_other_asserted('&corelib',[:,'mod-space!',[->,'Atom','hyperon::space::DynSpace']]).
metta_function_asserted('&corelib',['mod-space!',self],'&self').
metta_function_asserted('&corelib',['mod-space!',top],'&self').
metta_function_asserted('&corelib',['mod-space!',corelib],'&corelib').
metta_function_asserted('&corelib',['mod-space!',stdlib],'&stdlib').
metta_function_asserted('&corelib',['mod-space!',catalog],'&catalog').
metta_other_asserted('&corelib',[iz,'print-mods!','MeTTa']).
metta_other_asserted('&corelib',['@doc','print-mods!',['@desc',"Prints all modules with their correspondent spaces"],['@params',[]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'print-mods!',[->,[->]]]).
metta_other_asserted('&corelib',['@doc','=alpha',['@desc',"Checks alpha equality of two expressions"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"True if both expressions are alpha equal, False - otherwise"]]).
metta_other_asserted('&corelib',[iz,assertEqual,'MeTTa']).
metta_other_asserted('&corelib',['@doc',assertEqual,['@desc',"Compares (sets of) results of evaluation of two expressions"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"Unit atom if both expression after evaluation is equal, error - otherwise"]]).
metta_other_asserted('&corelib',[:,assertEqual,[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,assertNotEqual,'MeTTaLog']).
metta_other_asserted('&corelib',['@doc',assertNotEqual,['@desc',"Compares (sets of) results of evaluation of two expressions"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"Unit atom if both expressions after evaluation are not equal, error - otherwise"]]).
metta_other_asserted('&corelib',[:,assertNotEqual,[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,assertEqualToResult,'MeTTa']).
metta_other_asserted('&corelib',['@doc',assertEqualToResult,['@desc',"Same as assertEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation"],['@params',[['@param',"First expression (it will be evaluated)"],['@param',"Second expression (it won't be evaluated)"]]],['@return',"Unit atom if both expression after evaluation is equal, error - otherwise"]]).
metta_other_asserted('&corelib',[:,assertEqualToResult,[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,assertAlphaEqual,'MeTTa']).
metta_other_asserted('&corelib',['@doc',assertAlphaEqual,['@desc',"Compares (sets of) results of evaluation of two expressions using alpha equality"],['@params',[['@param',"First expression"],['@param',"Second expression"]]],['@return',"Unit atom if both expressions after evaluation are alpha equal, error - otherwise"]]).
metta_other_asserted('&corelib',[iz,assertNotEqualToResult,'MeTTaLog']).
metta_other_asserted('&corelib',['@doc',assertNotEqualToResult,['@desc',"Same as assertNotEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation"],['@params',[['@param',"First expression (it will be evaluated)"],['@param',"Second expression (it won't be evaluated)"]]],['@return',"Unit atom if both expressions after evaluation are not equal, error - otherwise"]]).
metta_other_asserted('&corelib',[:,assertNotEqualToResult,[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',['@doc',assertAlphaEqualToResult,['@desc',"Same as assertAlphaEqual but it doesn't evaluate second argument. Second argument is considered as a set of values of the first argument's evaluation"],['@params',[['@param',"First expression (it will be evaluated)"],['@param',"Second expression (it won't be evaluated)"]]],['@return',"Unit atom if both expressions after evaluation of the first argument are alpha equal, error - otherwise"]]).
metta_other_asserted('&corelib',[iz,superpose,'MeTTa']).
metta_other_asserted('&corelib',['@doc',superpose,['@desc',"Turns a tuple (first argument) into a nondeterministic result"],['@params',[['@param',"Tuple to be converted"]]],['@return',"Argument converted to nondeterministic result"]]).
metta_other_asserted('&corelib',[:,superpose,[->,'Expression','%Undefined%']]).
metta_other_asserted('&corelib',[iz,collapse,'MeTTa']).
metta_other_asserted('&corelib',['@doc',collapse,['@desc',"Converts a nondeterministic result into a tuple"],['@params',[['@param',"Atom which will be evaluated"]]],['@return',"Tuple"]]).
metta_other_asserted('&corelib',[:,collapse,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,case,'MeTTa']).
metta_other_asserted('&corelib',['@doc',case,['@desc',"Subsequently tests multiple pattern-matching conditions (second argument) for the given value (first argument)"],['@params',[['@param',"Atom (it will be evaluated)"],['@param',"Tuple of pairs mapping condition patterns to results"]]],['@return',"Result of evaluating Atom bound to met condition"]]).
metta_other_asserted('&corelib',[:,case,[->,'Atom','Expression','Atom']]).
metta_other_asserted('&corelib',[iz,capture,'MeTTa']).
metta_other_asserted('&corelib',['@doc',capture,['@desc',"Wraps an atom and captures the current space"],['@params',[['@param',"Function name which space needs to be captured"]]],['@return',"Function"]]).
metta_other_asserted('&corelib',[:,capture,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,hyperpose,'MeTTa']).
metta_other_asserted('&corelib',['@doc',hyperpose,['@desc',"Turns a tuple (first argument) into a nondeterministic result, as superpose, but is explicitly concurrent. Each item of the tuple may be processed in parallel, depending on the number of threads available (which is the minimum of the tuple size and the number of cores available)."],['@params',[['@param',"Tuple to be converted"]]],['@return',"Argument converted to nondeterministic result"]]).
metta_other_asserted('&corelib',[:,hyperpose,[->,'Expression','%Undefined%']]).
metta_other_asserted('&corelib',[iz,sequential,'MeTTa']).
metta_other_asserted('&corelib',['@doc',sequential,['@desc',"Turns a tuple (first argument) into a nondeterministic result, as superpose, but evaluation order of the elements of the tuple is fixed left to right. In that sense the result order is deterministic iff evaluating the tuple elements is deterministic."],['@params',[['@param',"Tuple to be evaluated"]]],['@return',"Sequential results of the tuple's elements."]]).
metta_other_asserted('&corelib',[:,sequential,[->,'Expression','%Undefined%']]).
metta_other_asserted('&corelib',[iz,do,'MeTTa']).
metta_other_asserted('&corelib',['@doc',do,['@desc',"Completely evaluates form, returning nothing. Typically used for side-effects. A common pattern is (sequential ((do <side-effect-form>) <form-that-needs-side-effect>))."],['@params',[['@param',"Form"]]],['@return',"None"]]).
metta_other_asserted('&corelib',[:,do,[->,'Expression','%Undefined%']]).
metta_other_asserted('&corelib',[iz,'pragma!','MeTTa']).
metta_other_asserted('&corelib',['@doc','pragma!',['@desc',"Changes global key's (first argument) value to a new one (second argument)"],['@params',[['@param',"Key's name"],['@param',"New value"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'pragma!',[->,'Symbol','%Undefined%',[->]]]).
metta_other_asserted('&corelib',[iz,'import!','MeTTa']).
metta_other_asserted('&corelib',['@doc','import!',['@desc',"Imports module using its relative path (second argument) and binds it to the token (first argument) which will represent imported atomspace. If first argument is &self then everything will be imported to current atomspace. The source is imported only the first time import! is called. Can load Python code (.py) or MeTTa (.metta); if ambiguous, assumes Python."],['@params',[['@param',"Symbol, which is turned into the token for accessing the imported module"],['@param',"Module name"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'import!',[->,'Atom','Atom',[->]]]).
metta_other_asserted('&corelib',[iz,include,'MeTTa']).
metta_other_asserted('&corelib',['@doc',include,['@desc',"Works just like include! but with &self as a first argument. So everything from input file will be included in the current atomspace and evaluated"],['@params',[['@param',"Name of metta script to import"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,include,[->,'Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'include!','MeTTa']).
metta_other_asserted('&corelib',['@doc','include!',['@desc',"Everything from input file will be included in the current atomspace and evaluated, as if it were being evaluated at the REPL. Unlike import!, the source is evaluated every time include! is called."],['@params',[['@param',"Space"],['@param',"Filename"]]],['@return',"Expression"]]).
metta_other_asserted('&corelib',[:,'include!',[->,'hyperon::space::DynSpace','String','Expression']]).
metta_other_asserted('&corelib',[iz,'bind!','MeTTa']).
metta_other_asserted('&corelib',['@doc','bind!',['@desc',"Registers a new token which is replaced with an atom during the parsing of the rest of the program"],['@params',[['@param',"Token name"],['@param',"Atom, which is associated with the token after reduction"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'bind!',[->,'Symbol','%Undefined%',[->]]]).
metta_other_asserted('&corelib',[iz,'trace!','MeTTa']).
metta_other_asserted('&corelib',['@doc','trace!',['@desc',"Prints its first argument and returns second. Both arguments will be evaluated before processing"],['@params',[['@param',"Atom to print"],['@param',"Atom to return"]]],['@return',"Evaluated second input"]]).
metta_other_asserted('&corelib',[:,'trace!',[->,'%Undefined%',_a,_a]]).
metta_other_asserted('&corelib',[iz,'println!','MeTTa']).
metta_other_asserted('&corelib',['@doc','println!',['@desc',"Prints a line of text to the console"],['@params',[['@param',"Expression/atom to be printed out"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'println!',[->,'%Undefined%',[->]]]).
metta_other_asserted('&corelib',[iz,'format-args','MeTTa']).
metta_other_asserted('&corelib',['@doc','format-args',['@desc',"Fills {} symbols in the input expression with atoms from the second expression. E.g. (format-args (Probability of {} is {}%) (head 50)) gives [(Probability of head is 50%)]. Atoms in the second input value could be variables"],['@params',[['@param',"Expression with {} symbols to be replaced"],['@param',"Atoms to be placed inside expression instead of {}"]]],['@return',"Expression with replaced {} with atoms"]]).
metta_other_asserted('&corelib',[:,'format-args',[->,'String','Atom','String']]).
metta_other_asserted('&corelib',[iz,sealed,'MeTTa']).
metta_other_asserted('&corelib',['@doc',sealed,['@desc',"Replaces all occurrences of any var from var list (first argument) inside atom (second argument) by unique variable. Can be used to create a locally scoped variables"],['@params',[['@param',"Variable list e.g. ($x $y)"],['@param',"Atom which uses those variables"]]],['@return',"Second argument but with variables being replaced with unique variables"]]).
metta_other_asserted('&corelib',[:,sealed,[->,'Expression','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'&self','MeTTa']).
metta_other_asserted('&corelib',['@doc','&self',['@desc',"Returns reference to the current atomspace"],['@params',[]],['@return',"Reference to the current atomspace"]]).
%  ; TODO: help! not working for operations which are defined in both Python and
%  ; Rust standard library: +, -, *, /, %, <, >, <=, >=, ==
metta_other_asserted('&corelib',[iz,+,'MeTTa']).
metta_other_asserted('&corelib',['@doc',+,['@desc',"Sums two numbers"],['@params',[['@param',"Addend"],['@param',"Augend"]]],['@return',"Sum"]]).
metta_other_asserted('&corelib',[:,+,[->,'Number','Number','Number']]).
metta_other_asserted('&corelib',[iz,-,'MeTTa']).
metta_other_asserted('&corelib',['@doc',-,['@desc',"Subtracts second argument from first one"],['@params',[['@param',"Minuend"],['@param',"Deductible"]]],['@return',"Difference"]]).
metta_other_asserted('&corelib',[:,-,[->,'Number','Number','Number']]).
metta_other_asserted('&corelib',[iz,*,'MeTTa']).
metta_other_asserted('&corelib',['@doc',*,['@desc',"Multiplies two numbers"],['@params',[['@param',"Multiplier"],['@param',"Multiplicand"]]],['@return',"Product"]]).
metta_other_asserted('&corelib',[:,*,[->,'Number','Number','Number']]).
metta_other_asserted('&corelib',[iz,/,'MeTTa']).
metta_other_asserted('&corelib',['@doc',/,['@desc',"Divides first argument by second one"],['@params',[['@param',"Dividend"],['@param',"Divisor"]]],['@return',"Fraction"]]).
metta_other_asserted('&corelib',[:,/,[->,'Number','Number','Number']]).
metta_other_asserted('&corelib',[iz,'%','MeTTa']).
metta_other_asserted('&corelib',['@doc','%',['@desc',"Modulo operator. It returns remainder of dividing first argument by second argument"],['@params',[['@param',"Dividend"],['@param',"Divisor"]]],['@return',"Remainder"]]).
metta_other_asserted('&corelib',[:,'%',[->,'Number','Number','Number']]).
metta_other_asserted('&corelib',[iz,<,'MeTTa']).
metta_other_asserted('&corelib',['@doc',<,['@desc',"Less than. Checks if first argument is less than second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is less than second, False - otherwise"]]).
metta_other_asserted('&corelib',[:,<,[->,'Number','Number','Bool']]).
metta_other_asserted('&corelib',[iz,>,'MeTTa']).
metta_other_asserted('&corelib',['@doc',>,['@desc',"Greater than. Checks if first argument is greater than second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is greater than second, False - otherwise"]]).
metta_other_asserted('&corelib',[:,>,[->,'Number','Number','Bool']]).
metta_other_asserted('&corelib',[iz,<=,'MeTTa']).
metta_other_asserted('&corelib',['@doc',<=,['@desc',"Less than or equal. Checks if first argument is less than or equal to second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is less than or equal to second, False - otherwise"]]).
metta_other_asserted('&corelib',[:,<=,[->,'Number','Number','Bool']]).
metta_other_asserted('&corelib',[iz,>=,'MeTTa']).
metta_other_asserted('&corelib',['@doc',>=,['@desc',"Greater than or equal. Checks if first argument is greater than or equal to second one"],['@params',[['@param',"First number"],['@param',"Second number"]]],['@return',"True if first argument is greater than or equal to second, False - otherwise"]]).
metta_other_asserted('&corelib',[:,>=,[->,'Number','Number','Bool']]).
metta_other_asserted('&corelib',[iz,==,'MeTTa']).
metta_other_asserted('&corelib',['@doc',==,['@desc',"Checks equality for two arguments of the same type"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"Returns True if two arguments are equal, False - otherwise. If arguments are of different type function returns Error currently"]]).
metta_other_asserted('&corelib',[:,==,[->,_t,_t,'Bool']]).
metta_other_asserted('&corelib',[iz,xor,'MeTTa']).
metta_other_asserted('&corelib',['@doc',xor,['@desc',"Exclusive disjunction of two arguments"],['@params',[['@param',"First argument"],['@param',"Second argument"]]],['@return',"Return values are the same as logical disjunction, but when both arguments are True xor will return False"]]).
metta_other_asserted('&corelib',[:,xor,[->,'Bool','Bool','Bool']]).
metta_other_asserted('&corelib',[iz,flip,'MeTTa']).
metta_other_asserted('&corelib',['@doc',flip,['@desc',"Produces random boolean value"],['@params',[]],['@return',"Returns uniformly distributed random boolean value"]]).
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
metta_other_asserted('&corelib',[iz,'unique-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','unique-by',['@desc',"Function takes a comparator predicate and a non-deterministic input (second argument) and returns only unique entities according to the predicate. \n  E.g. (unique-by =alpha (superpose (a b c d d))) -> [a, b, c, d]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"]]],['@return',"Unique non-deterministic values from input set based on predicate"]]).
metta_other_asserted('&corelib',[:,'unique-by',[->,'Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'union-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','union-by',['@desc',"Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their union. \n  E.g. (union-by =u= (superpose (a b b c)) (superpose (b c c d))) -> [a, b, b, c, b, c, c, d]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Union of sets based on predicate"]]).
metta_other_asserted('&corelib',[:,'union-by',[->,'Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'intersection-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','intersection-by',['@desc',"Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their intersection. \n  E.g. (intersection-by =will (superpose (a b c c)) (superpose (b c c c d))) -> [b, c, c]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Intersection of sets based on predicate"]]).
metta_other_asserted('&corelib',[:,'intersection-by',[->,'Atom','Atom','Atom','Atom']]).
metta_other_asserted('&corelib',[iz,'subtraction-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','subtraction-by',['@desc',"Function takes a comparator predicate and two non-deterministic inputs (second and third arguments) and returns their subtraction. \n  E.g. !(subtraction-by =identical (superpose (a b b c)) (superpose (b c c d))) -> [a, b]"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"Non-deterministic set of values"],['@param',"Another non-deterministic set of values"]]],['@return',"Non-deterministic Subtraction of sets based on predicate"]]).
metta_other_asserted('&corelib',[:,'subtraction-by',[->,'Atom','Atom','Atom','Atom']]).
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
metta_other_asserted('&corelib',[iz,'=alpha','MeTTa']).
metta_other_asserted('&corelib',['@doc','=alpha',['@desc',"Predicate that **checks** if two atoms are structurally equivalent, allowing variable renaming.\n  Unlike `=alpha-unify`, this predicate does **not** perform unification or modify variables—it only verifies whether the two atoms *could* be equivalent with renaming."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms are structurally equivalent with renaming, false otherwise."]]).
metta_other_asserted('&corelib',[:,'=alpha',[->,'Atom','Atom','Bool']]).
metta_other_asserted('&corelib',[iz,'=will','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','=will',['@desc',"Predicate that **checks** if two atoms *could* unify in theory but does **not** perform unification.\n  Unlike `=u=`, this only verifies whether unification is **possible** without actually binding variables."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if unification is theoretically possible, false otherwise."]]).
metta_other_asserted('&corelib',[:,'=will',[->,'Atom','Atom','Bool']]).
metta_other_asserted('&corelib',[iz,'=u=','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','=u=',['@desc',"Predicate that determines if two atoms can be unified under standard unification rules, meaning they can be made identical by substituting variables appropriately. \n  We do **not** use `==` because `(== $x $y)` would be untrue and have **no side effects**, whereas `=u=` performs unification where possible."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms can be unified, false otherwise."]]).
metta_other_asserted('&corelib',[:,'=u=',[->,'Atom','Atom','Bool']]).
metta_other_asserted('&corelib',[iz,'=alpha-unify','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','=alpha-unify',['@desc',"Predicate that **performs** unification while considering alpha-equivalence. \n  It allows variable renaming where necessary to achieve successful unification."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms unify successfully, considering alpha-equivalence."]]).
metta_other_asserted('&corelib',[:,'=alpha-unify',[->,'Atom','Atom','Bool']]).
metta_other_asserted('&corelib',[iz,'=identical','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','=identical',['@desc',"Predicate that determines if two atoms are completely identical in structure, including variable names and their bindings. \n  This is a stricter comparison than `=alpha` or `=u=`, ensuring that variables and their values match exactly."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms are strictly identical, false otherwise."]]).
metta_other_asserted('&corelib',[:,'=identical',[->,'Atom','Atom','Bool']]).
metta_other_asserted('&corelib',[iz,'=references','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','=references',['@desc',"Predicate that determines if two atoms reference the **same** underlying entity in memory. \n  This is the strictest form of equality, ensuring the atoms are not just identical in structure but are literally the **same instance**."],['@params',[['@param',"First atom"],['@param',"Second atom"]]],['@return',"True if the atoms reference the same object, false otherwise."]]).
metta_other_asserted('&corelib',[:,'=references',[->,'Atom','Atom','Bool']]).
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
metta_other_asserted('&corelib',[iz,'unique-atom-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','unique-atom-by',['@desc',"Function takes a comparator predicate and a tuple and returns only unique entities according to the predicate. \n  E.g. (unique-atom-by =alpha (a b c d d)) -> (a b c d)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"]]],['@return',"Unique values from input set based on predicate"]]).
metta_other_asserted('&corelib',[:,'unique-atom-by',[->,'Atom','Expression','Expression']]).
metta_function_asserted('&corelib',['unique-atom-by',_Pred,_L1],[collapse,['unique-by',_Pred,[superpose,_L1]]]).
transpiler_clause_store('unique-atom-by',[2],1,['Atom','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[])],x(noeval,eager,[]),['unique-atom-by',_Pred,_L1],[collapse,['unique-by',_Pred,[superpose,_L1]]]).


'mi__1_2_unique-atom-by'(B,C,D) :- 
  E='mc__1_2_unique-atom-by'(B,C,D) , 
  ci(true,'unique-atom-by',2,['unique-atom-by',B,C],D,true,E).




'me__1_2_unique-atom-by'(B,C,D):-'mc__1_2_unique-atom-by'(B,C,D).




'mc__1_2_unique-atom-by'(A,B,C) :- 
  D =  
    ispeEnN( E, 
      ( as_p1_exec(A,F)  ,
        mi__1_1_superpose(B,G) , 
        E=['unique-by',F,G]), 
      H, 
      ( as_p1_expr(A,I)  ,
        J=[superpose,B] , 
        H=['unique-by',I,J])) , 
  mi__1_1_collapse(D,C).


metta_other_asserted('&corelib',[iz,'union-atom-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','union-atom-by',['@desc',"Function takes a comparator predicate and two tuples and returns their union. \n  E.g. (union-atom-by =u= (a b b c) (b c c d)) -> (a b b c b c c d)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"],['@param',"List of values"]]],['@return',"Union of sets based on predicate"]]).
metta_function_asserted('&corelib',['union-atom-by',_Pred,_L1,_L2],[collapse,['union-by',_Pred,[superpose,_L1],[superpose,_L2]]]).
transpiler_clause_store('union-atom-by',[3],1,['Atom','Expression','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['union-atom-by',_Pred,_L1,_L2],[collapse,['union-by',_Pred,[superpose,_L1],[superpose,_L2]]]).


'mi__1_3_union-atom-by'(B,C,D,E) :- 
  F='mc__1_3_union-atom-by'(B,C,D,E) , 
  ci( true, 
    'union-atom-by', 
    3, 
    ['union-atom-by',B,C,D], E,true,F).




'me__1_3_union-atom-by'(B,C,D,E) :-  
  'mc__1_3_union-atom-by'(B,C,D,E).




'mc__1_3_union-atom-by'(A,B,C,D) :- 
  E =  
    ispeEnN( F, 
      ( as_p1_exec(A,G)  ,
        mi__1_1_superpose(B,H) , 
        mi__1_1_superpose(C,I) , 
        F=['union-by',G,H,I]), 
      J, 
      ( as_p1_expr(A,K)  ,
        L=[superpose,B] , 
        M=[superpose,C] , 
        J=['union-by',K,L,M])) , 
  mi__1_1_collapse(E,D).


metta_other_asserted('&corelib',[:,'union-atom-by',[->,'Atom','Expression','Expression','Expression']]).
metta_other_asserted('&corelib',[iz,'intersection-atom-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','intersection-atom-by',['@desc',"Function takes a comparator predicate and two tuples and returns their intersection. \n  E.g. (intersection-atom-by =will (a b c c) (b c c c d)) -> (b c c)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"],['@param',"List of values"]]],['@return',"Intersection of sets based on predicate"]]).
metta_function_asserted('&corelib',['intersection-atom-by',_Pred,_L1,_L2],[collapse,['intersection-by',_Pred,[superpose,_L1],[superpose,_L2]]]).
transpiler_clause_store('intersection-atom-by',[3],1,['Atom','Expression','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['intersection-atom-by',_Pred,_L1,_L2],[collapse,['intersection-by',_Pred,[superpose,_L1],[superpose,_L2]]]).


'mi__1_3_intersection-atom-by'(B,C,D,E) :- 
  F='mc__1_3_intersection-atom-by'(B,C,D,E) , 
  ci( true, 
    'intersection-atom-by', 
    3, 
    ['intersection-atom-by',B,C,D], E,true,F).




'me__1_3_intersection-atom-by'(B,C,D,E) :-  
  'mc__1_3_intersection-atom-by'(B,C,D,E).




'mc__1_3_intersection-atom-by'(A,B,C,D) :- 
  E =  
    ispeEnN( F, 
      ( as_p1_exec(A,G)  ,
        mi__1_1_superpose(B,H) , 
        mi__1_1_superpose(C,I) , 
        F=['intersection-by',G,H,I]), 
      J, 
      ( as_p1_expr(A,K)  ,
        L=[superpose,B] , 
        M=[superpose,C] , 
        J=['intersection-by',K,L,M])) , 
  mi__1_1_collapse(E,D).


metta_other_asserted('&corelib',[:,'intersection-atom-by',[->,'Atom','Expression','Expression','Expression']]).
metta_other_asserted('&corelib',[iz,'subtraction-atom-by','MeTTaLog']).
metta_other_asserted('&corelib',['@doc','subtraction-atom-by',['@desc',"Function takes a comparator predicate and two tuples and returns their subtraction. \n  E.g. (subtraction-atom-by =identical (a b b c) (b c c d)) -> (a b)"],['@params',[['@param',"Comparator predicate (e.g., =alpha, =identical, =will, =u=, =references)"],['@param',"List of values"],['@param',"List of values"]]],['@return',"Subtraction of sets based on predicate"]]).
metta_function_asserted('&corelib',['subtraction-atom-by',_Pred,_L1,_L2],[collapse,['subtraction-by',_Pred,[superpose,_L1],[superpose,_L2]]]).
transpiler_clause_store('subtraction-atom-by',[3],1,['Atom','Expression','Expression'],'Expression',[x(noeval,lazy,[]),x(noeval,eager,[]),x(noeval,eager,[])],x(noeval,eager,[]),['subtraction-atom-by',_Pred,_L1,_L2],[collapse,['subtraction-by',_Pred,[superpose,_L1],[superpose,_L2]]]).


'mi__1_3_subtraction-atom-by'(B,C,D,E) :- 
  F='mc__1_3_subtraction-atom-by'(B,C,D,E) , 
  ci( true, 
    'subtraction-atom-by', 
    3, 
    ['subtraction-atom-by',B,C,D], E,true,F).




'me__1_3_subtraction-atom-by'(B,C,D,E) :-  
  'mc__1_3_subtraction-atom-by'(B,C,D,E).




'mc__1_3_subtraction-atom-by'(A,B,C,D) :- 
  E =  
    ispeEnN( F, 
      ( as_p1_exec(A,G)  ,
        mi__1_1_superpose(B,H) , 
        mi__1_1_superpose(C,I) , 
        F=['subtraction-by',G,H,I]), 
      J, 
      ( as_p1_expr(A,K)  ,
        L=[superpose,B] , 
        M=[superpose,C] , 
        J=['subtraction-by',K,L,M])) , 
  mi__1_1_collapse(E,D).


metta_other_asserted('&corelib',[:,'subtraction-atom-by',[->,'Atom','Expression','Expression','Expression']]).
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
metta_other_asserted('&corelib',['@doc','unique-atom',['@desc',"Function takes tuple and returns only unique entities. E.g. (unique-atom (a b c d d)) -> (a b c d)"],['@params',[['@param',"List of values"]]],['@return',"Unique values from input set"]]).
metta_other_asserted('&corelib',['ALT=',['unique-atom',_L1],[collapse,[unique,[superpose,_L1]]]]).
metta_other_asserted('&corelib',[:,'unique-atom',[->,'Expression','Expression']]).
metta_other_asserted('&corelib',['@doc','union-atom',['@desc',"Function takes two tuples and returns their union. E.g. (union-atom (a b b c) (b c c d)) -> (a b b c b c c d)"],['@params',[['@param',"List of values"],['@param',"List of values"]]],['@return',"Union of sets"]]).
metta_other_asserted('&corelib',['ALT=',['union-atom',_L1,_L2],[collapse,[union,[superpose,_L1],[superpose,_L2]]]]).
metta_other_asserted('&corelib',[:,'union-atom',[->,'Expression','Expression','Expression']]).
metta_other_asserted('&corelib',['@doc','intersection-atom',['@desc',"Function takes two tuples and returns their intersection. E.g. (intersection-atom (a b c c) (b c c c d)) -> (b c c)"],['@params',[['@param',"List of values"],['@param',"List of values"]]],['@return',"Intersection of sets"]]).
metta_other_asserted('&corelib',['ALT=',['intersection-atom',_L1,_L2],[collapse,[intersection,[superpose,_L1],[superpose,_L2]]]]).
metta_other_asserted('&corelib',[:,'intersection-atom',[->,'Expression','Expression','Expression']]).
metta_other_asserted('&corelib',['@doc','subtraction-atom',['@desc',"Function takes two tuples and returns their subtraction. E.g. !(subtraction-atom (a b b c) (b c c d)) -> (a b)"],['@params',[['@param',"List of values"],['@param',"List of values"]]],['@return',"Subtraction of sets"]]).
metta_other_asserted('&corelib',['ALT=',['subtraction-atom',_L1,_L2],[collapse,[subtraction,[superpose,_L1],[superpose,_L2]]]]).
metta_other_asserted('&corelib',[:,'subtraction-atom',[->,'Expression','Expression','Expression']]).
metta_other_asserted('&corelib',[iz,'git-module!','MeTTa']).
metta_other_asserted('&corelib',['@doc','git-module!',['@desc',"Provides access to module in a remote git repo, from within MeTTa code. Similar to `register-module!`, this op will bypass the catalog search"],['@params',[['@param',"URL to github repo"]]],['@return',"Unit atom"]]).
metta_other_asserted('&corelib',[:,'git-module!',[->,'Atom',[->]]]).
