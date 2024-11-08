
:- dynamic(can_compile/2).  % Declares `can_compile/2` as a dynamic predicate, meaning it can be modified at runtime.

/** src_code_for(+KB, +F, +Len) 
  * Attempts to compile source code for a specific knowledge base (KB),
  * predicate (F), and argument length (Len).
  * 
  * @param KB Knowledge base to compile for.
  * @param F Predicate name.
  * @param Len Length of arguments.
  * @example 
  *  ?- src_code_for('example_kb', my_predicate, 2).
  */
src_code_for(KB,F,Len) ==>  
   (\+ metta_compiled_predicate(KB,F,Len) ==> do_compile(KB,F,Len)).  % If the predicate is not yet compiled, invoke `do_compile/3`.

 
/** do_compile_space(+KB)
  * Initiates the compilation process for all predicates within the given knowledge base (KB).
  * @param KB The knowledge base to compile.
  * @example 
  *  ?- do_compile_space('my_kb').
  */
do_compile_space(KB) ==>  
   (src_code_for(KB,F,Len) ==> do_compile(KB,F,Len)).  % For each predicate in the KB, ensure it is compiled.

/*previously: The '&self' knowledge base is intended to be compiled here, 
   but this line is commented out, possibly due to it being unused or for debugging. */
%do_compile_space('&self').  

/** do_compile(+KB, +F, +Len)
  * Compiles the given predicate `F` of length `Len` within the knowledge base `KB`.
  * Calls `really_compile/3` after invoking `src_code_for/3`.
  * @param KB Knowledge base to compile.
  * @param F Predicate name.
  * @param Len Length of arguments.
  */
do_compile(KB,F,Len), src_code_for(KB,F,Len) ==> 
   really_compile(KB,F,Len).  % Ensures predicates are fully compiled with the actual compiler step.

/** metta_defn(+KB, +[F|Args], +BodyFn)
  * Defines a predicate `F` with a list of arguments in `Args` and a body function `BodyFn`.
  * Calls `really_compile_src/4` if the length of `Args` is `Len`.
  * @param KB The knowledge base where the definition resides.
  * @param F The predicate name.
  * @param Args The arguments for the predicate.
  * @param BodyFn The function representing the predicate's body.
  */
metta_defn(KB,[F|Args],BodyFn), really_compile(KB,F,Len) / length(Args,Len) ==> 
   really_compile_src(KB,F,Len,Args,BodyFn), 
   {dedupe_ls(F)}.  % Dedupe ensures no duplicate logical structures in compiled output.

/** really_compile_src(+KB, +F, +Len, +Args, +BodyFn)
  * Core compilation process for a meta-predicate, involving arguments and body function.
  * Uses `compile_metta_defn/5` to generate clauses for the knowledge base.
  * @param KB The knowledge base.
  * @param F The predicate.
  * @param Len The length of arguments.
  * @param Args Arguments for the predicate.
  * @param BodyFn The body of the predicate function.
  */
really_compile_src(KB,F,Len,Args,BodyFn),
   {compile_metta_defn(KB,F,Len,Args,BodyFn,Clause)}  % This predicate calls the actual compilation of the clause.
       ==> (compiled_clauses(KB,F,Clause)).  % Once compiled, the clause is added to the knowledge base as `compiled_clauses`.

       
/* Directive to load another file (commented out), likely to load a higher-level ontology.
   This line was probably commented out because it is not necessary at this stage of development. */
%:- ensure_loaded('metta_ontology_level_1.pfc').


/** Test rules a==>b and b==>bb 
  * These are forward-chaining rules for simple inference, often used for debugging or testing purposes.
  */
a ==> b.  % When `a` is true, infer `b`.
b ==> bb.  % When `b` is true, infer `bb`.

a.  % Fact: `a` is true.
:- b.  % Asserts that `b` must be true.
:- bb.  % Asserts that `bb` must be true.

/*previously: These commented out lines were used for tracing inference explanations (`pfcWhy1`), but they are
   disabled, possibly to avoid unnecessary output or to streamline debugging. */
%:- pfcWhy1(a).  
%:- pfcWhy1(b).

:- set_prolog_flag(expect_pfc_file,never).
:- set_prolog_flag(pfc_term_expansion,false).



/** test_fwc
  * A test suite that demonstrates the usage of the PFC forward-chaining inference engine.
  * It tests several forward-chaining operations and queries their justifications.
  */
test_fwc:-  
  pfcAdd_Now(c(X)==>d(X)),  % Adds the rule `c(X) ==> d(X)` to the knowledge base.
  pfcAdd_Now(c(1)),  % Adds the fact `c(1)` to the knowledge base.
  c(_),  % Queries for any `c(_)` fact.
  d(_),  % Queries for any `d(_)` fact.
  pfcWhy1(c(_)),  % Provides an explanation for why `c(_)` holds.
  pfcWhy1(d(_)),  % Provides an explanation for why `d(_)` holds.
  pfcAdd(e(2)),  % Adds the fact `e(2)` to the knowledge base.
  e(_),  % Queries for any `e(_)` fact.
  pfcAdd(e(X) <==> f(X)),  % Adds a bi-directional rule between `e(X)` and `f(X)`.
  f(_),  % Queries for any `f(_)` fact.
  pfcWhy1(e(_)),  % Explains why `e(_)` holds.
  pfcWhy1(f(_)).  % Explains why `f(_)` holds.


end_of_file.


/*previously: These commented-out blocks likely attempt to trigger forward-chaining for all rules and include debugging commands.
   They are likely disabled to prevent performance issues during early stages of debugging or to avoid triggering unnecessary operations. */
%:- forall(==>(X,Y),pfcFwd(==>(X,Y))).  % Forward chains all `==> (X,Y)` rules.
%:- break.  % Breakpoint for debugging purposes.
%:- must_det_ll(property('length',list_operations)).  % Ensures that the `length` property is well-defined for list operations.

/** equivalentTypes(+PredType, +FunctType)
  * Asserts the equivalency between predicate types and function types, ensuring consistency
  * in the logical framework for the knowledge base.
  * 
  * @param PredType Predicate type (e.g., UnaryPredicate).
  * @param FunctType Function type (e.g., UnaryFunction).
  */
(equivalentTypes(PredType,FunctType) ==>  
  (property(KB,FunctorObject,PredType) <==> property(KB,FunctorObject,FunctType))).  % Links the two types logically.

/** Automatically generates equivalency rules based on the arity of predicates and functions. */
(((p_arity(PredType,PA), {plus(KB,FA,1,PA), FA>=0}, f_arity(KB,FunctType,FA))) ==> 
   equivalentTypes(PredType,FunctType)).  % If the arities align, the types are declared equivalent.

/** p_arity(+PredType, -Arity)
  * Defines the arity (number of arguments) for different types of predicates.
  * @param PredType The type of predicate (e.g., UnaryPredicate).
  * @param Arity The arity corresponding to the predicate type.
  */
p_arity('NullaryPredicate', 0).  % No arguments.
p_arity('UnaryPredicate', 1).    % One argument.
p_arity('BinaryPredicate', 2).   % Two arguments.
p_arity('TernaryPredicate', 3).  % Three arguments.
p_arity('QuaternaryPredicate', 4).  % Four arguments.
p_arity('QuinaryPredicate', 5).  % Five arguments.

/* previously: This commented-out block might have been an earlier, more complex version of the `really_compile` logic. 
   It is now replaced by the simplified and modular approach above. */
 /*
    really_compile(KB,F,Len)==>
      ((metta_defn(KB,[F|Args],BodyFn)/compile_metta_defn(KB,F,Len,Args,BodyFn,Clause))
        ==> (compiled_clauses(KB,F,Clause))).
 */
 
