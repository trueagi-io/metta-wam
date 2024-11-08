/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 */

% Uncommented out lines define the agent action queue, with multifile and dynamic predicates
% These declarations allow predicates to be defined in multiple files and modified dynamically at runtime.
% :- multifile(baseKB:agent_action_queue/3).
% :- dynamic(baseKB:agent_action_queue/3).

% Set garbage collection flag to true. This allows Prolog to automatically reclaim unused memory during execution.
:- set_prolog_flag(gc, true).

% Declare a thread-local predicate that can be modified per thread, used to disable PX (predicate expansion).
:- thread_local(t_l:disable_px/0).
% Ensure that no thread has PX disabled at startup by removing any existing clauses for the predicate.
:- retractall(t_l:disable_px).

% Ensure that the predicate `t_l:disable_px` is not active in any thread.
:- must(\+ t_l:disable_px).

% Define custom operators with specified precedence and associativity.
% These operators enable symbolic syntax in Prolog, useful for logical expressions in MeTTa.
:- op(500, fx, '~').        % Unary negation operator (~P for NOT P)
:- op(1050, xfx, '=>').     % Implication operator (A => B for A implies B)
:- op(1050, xfx, '<==>').   % Equivalence operator (A <==> B for A if and only if B)
:- op(1050, xfx, '<-').     % Reverse implication (A <- B for A is implied by B)
:- op(1100, fx, '==>').     % Unary forward chaining rule operator (==> A)
:- op(1150, xfx, '::::').   % Custom operator used for specific MeTTa logic

% Temporarily elevate access level to system to allow modification of certain system settings.
:- current_prolog_flag(access_level, Was),
   set_prolog_flag(access_level, system),
   % Define additional operators for logical constructs and arithmetic operations.
   op(1190, xfx, '::::'),
   op(1180, xfx, '==>'),
   op(1170, xfx, '<==>'),
   op(1160, xfx, '<-'),
   op(1150, xfx, '=>'),
   op(1140, xfx, '<='),
   op(1130, xfx, '<=>'),
   op(600, yfx, '&'),       % AND operator
   op(600, yfx, 'v'),       % OR operator
   op(350, xfx, 'xor'),     % XOR operator
   op(300, fx, '~'),        % Negation
   op(300, fx, '-'),        % Unary minus (negative numbers)
   op(1199, fx, '==>'),     % Forward chaining rule
   % Reset access level to its previous state.
   set_prolog_flag(access_level, Was).

% Style checks, dialect settings, and macro expansion control (currently commented out).
% :- style_check(-discontiguous). % Disables warnings for non-contiguous predicate definitions.
% :- enable_mpred_expansion.      % Enables certain predicate expansions specific to the logicmoo framework.
% :- expects_dialect(pfc).        % Set the expected Prolog dialect to PFC (Prolog Forward Chaining).

/*
 * Dynamic predicate declarations. These predicates represent sessions and agents
 * and allow Prolog to track and manage interactions between them.
 */
% :- dynamic lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2, telnet_fmt_shown/3, agent_action_queue/3).

% Uncommenting the next line would set the source module to `baseKB`, a knowledge base module.
% :- nop('$set_source_module'(baseKB)).

% Set flags to optimize runtime safety and debugging rather than speed.
:- set_prolog_flag(runtime_speed, 0).     % Disable speed optimizations.
:- set_prolog_flag(runtime_safety, 2).    % Maximize safety checks.
:- set_prolog_flag(runtime_debug, 2).     % Maximize debug information.
:- set_prolog_flag(unsafe_speedups, false). % Disable potentially unsafe optimizations.
:- set_prolog_flag(expect_pfc_file, always). % Expect PFC file loading.

% Disable the PFC term expansion during certain phases.
:- set_prolog_flag(pfc_term_expansion, false).

% Predicate to handle function parameter and return type information.
% It separates the return type from the list of parameters.
params_and_return_type([->|TypeList], Len, Params, Ret):-
   append(Params, [Ret], TypeList),  % Splits the list into Params and Ret (return type).
   length(Params, Len).              % Returns the number of parameters.

% Merges two function predicates by comparing their arguments and ensuring they match.
merge_fp(_, _, N) :- N < 1.          % Base case: stop when N is less than 1.
merge_fp(T1, T2, N) :-
  N > 0,
  arg(N, T1, X),                      % Get the N-th argument of T1.
  arg(N, T2, X),                      % Ensure the N-th argument of T2 matches.
  N1 is N - 1,                        % Decrease N and recursively merge the remaining arguments.
  merge_fp(T1, T2, N1).

% Re-enable PFC term expansion after processing is complete.
:- set_prolog_flag(pfc_term_expansion, true).

% Define a "functional-predicate" as a Prolog term.
% This code generates two function terms (P1 and P2) with the same functor (Name and Arity),
% and binds their final arguments.
'functional-predicate'(Name, Arity) ==>
  {functor(P1, Name, Arity),          % Create a functor term P1.
   functor(P2, Name, Arity),          % Create another functor term P2.
   arg(Arity, P1, PV1),               % Get the last argument of P1.
/** 
 * arg/3
 *
 * Extracts the argument from a term at a specific position (Arity).
 *
 * @param Arity Position of the argument in the term.
 * @param P2 Term from which the argument is extracted.
 * @param PV2 Extracted argument value.
 * @example arg(3, f(a, b, c), PV2). % PV2 will be 'c'
 */
% Extract argument PV2 from term P2 at position Arity
arg(Arity, P2, PV2),

% Calculate N as Arity-1
N is Arity-1,

/** 
 * merge_fp/3
 * 
 * Merges two functional predicates P1 and P2 up to the Nth argument.
 * 
 * @param P1 First functional predicate.
 * @param P2 Second functional predicate.
 * @param N Number of arguments to merge.
 */
% Merge functional predicates P1 and P2 up to N arguments
merge_fp(P1, P2, N)},

% Add a rule that expresses that P2 holds when P1 holds, and PV1 differs from PV2
(P1, {P2, PV1 \== PV2} ==> ~P2).

% Declare functional predicates with arity 1
% These directives define predicates as "functional-predicate" and their arity
==> 'functional-predicate'('next-operation', 1).
==> 'functional-predicate'('previous-operation', 1).

% Declare dynamic predicate to allow modifications at runtime
:- dynamic('op-complete'/1).

% Define the initial state of 'previous-operation' as 'none'
'previous-operation'(none).


('next-operation'(Current),
   {
    if_t( retract('previous-operation'(Previous)),
      (if_t(Previous==Current,
             nop(wdmsg(continue(Previous)))),
       if_t(Previous\=@=Current,
        if_t( \+ 'op-complete'(Previous),
           (nop(wdmsg(begun(op_complete(Previous)))),
            pfcAdd('op-complete'(Previous)),
            nop(wdmsg(ended(op_complete(Previous))))))))),
    nop(wdmsg(op_next(Current))),
    assert('previous-operation'(Current))}
   ==>
   'seen-operation'(Current)).




% ==> 'seen-operation'(Current)).

/* previously: This was commented out likely because it's an incomplete or alternative rule for 'next-operation'. It's kept here for potential future use or testing. */
% ==> 'next-operation'(next).

/** 
 * property/3
 *
 * Infers properties from a knowledge base (KB).
 * 
 * @param KB The knowledge base.
 * @param A The subject.
 * @param B A list of properties, one of which (E) is non-variable.
 * @example properties(kb, obj, [color(red), size(large)]). % Infers obj has color red or size large
 */
% Match the property from the list and enforce that the element is non-variable
((properties(KB, A, B), {member(E, B), nonvar(E)}) ==> property(KB, A, E)).

% Property of Op and E implies a valid operation and property structure
property(_, Op, E) ==> (form_op(Op), form_prop(E)).

/**
 * predicate_arity/3
 * 
 * Infers the arity of predicates from knowledge base properties.
 * 
 * @param KB Knowledge base.
 * @param F Function/predicate.
 * @param A Arity.
 */
% Infer arity of predicates and functional predicates based on their properties
((property(KB, F, PA), p_arity(PA, A)) ==> (predicate_arity(KB, F, A))).
((property(KB, F, FA), f_arity(FA, A)) ==> (functional_arity(KB, F, A))).

/* previously: This rule is likely skipped because it's either redundant or part of a refactoring effort.
   It's kept here in case the rule might need revisiting for ensuring compiled predicates have arity. */
% (metta_compiled_predicate(KB, F, A)==>predicate_arity(KB, F, A)).

/**
 * metta_atom_asserted/3
 * 
 * Infers types and definitions from atoms.
 * 
 * @param KB Knowledge base.
 * @param C Operation.
 * @param H Head of the atom.
 * @param T Tail (optional).
 */
% Infer the type for ':-' expressions
(metta_atom_asserted(KB, [C, H, T])/(C==':')) ==> metta_type(KB, H, T).

% Handle '=' expressions for metta definitions
(metta_atom_asserted(KB, [C, H, T|Nil])/(Nil==[], C == '=', H = II)) ==> metta_defn(KB, II, T).
(metta_atom_asserted(KB, [C, H, A1, A2 | AL])/(C == '=')) ==> metta_defn(KB, H, [A1, A2 | AL]).

% Process ':-' for metta definitions that include WAM (Warren Abstract Machine) body instructions
(metta_atom_asserted(KB, [C, H | AL])/(C==':-')) ==> metta_defn(KB, H, ['wam-body' | AL]).

/**
 * src_code_for/3
 * 
 * Provides source code information for metta definitions.
 * 
 * @param KB Knowledge base.
 * @param F Function/predicate.
 * @param Len Number of arguments.
 */
% Infer the source code information for a function F with Len arguments
metta_defn(KB, [F | Args], _)/length(Args, Len) ==> src_code_for(KB, F, Len).

/**
 * op-complete/1
 * 
 * Marks operations as complete.
 * 
 * @param Operation An operation to be marked as complete.
 */
% Ensure an operation is marked complete and deduplicate function F
'op-complete'(op(+, '=', F)),
  metta_defn(KB, [F | Args], _)/length(Args, Len)
  ==> src_code_for(KB, F, Len), {nop(dedupe_cl(/*'&self':*/ F))}.

/**
 * function_arity/3
 * 
 * Associates functions with their arity.
 * 
 * @param KB Knowledge base.
 * @param F Function.
 * @param Len Number of arguments.
 */
% Assert that a function F has arity Len
(src_code_for(KB, F, Len) ==> function_arity(KB, F, Len)).

/**
 * metta_params_and_return_type/5
 * 
 * Infers the parameter and return types for a metta definition.
 * 
 * @param KB Knowledge base.
 * @param F Function.
 * @param Len Length of parameters.
 * @param Params List of parameters.
 * @param Ret Return type.
 */

('op-complete'(op(+,':',F))
 ==>
 (( metta_type(KB,F,TypeList)/is_list(TypeList),
  {params_and_return_type(TypeList,Len,Params,Ret)}) ==>
  metta_params_and_return_type(KB,F,Len,Params,Ret),{do_once(show_deds_w(F))})).


/**
 * Absorbed and non-absorbed return types
 * 
 * Associates arity and return type information.
 */
% Handle absorbed return types
metta_params_and_return_type(KB, F, Len, Params, Ret),
  {is_absorbed_return_type(Params, Ret)}
  ==> (function_arity(KB, F, Len), is_absorbed_return(KB, F, Len, Ret), predicate_arity(KB, F, Len)).

% Handle non-absorbed return types with Len+1
metta_params_and_return_type(KB, F, Len, Params, Ret),
  { is_non_absorbed_return_type(Params, Ret), Len1 is Len + 1 }
  ==> (function_arity(KB, F, Len), is_non_absorbed_return(KB, F, Len, Ret), predicate_arity(KB, F, Len1)).

/**
 * metta_atom/2
 * 
 * Asserts core library atoms when needed.
 */
% Declare corelib types when necessary
(need_corelib_types,op_decl(F,Params,Ret),{nonvar(Ret),length(Params,Len)})==>
   metta_params_and_return_type('&corelib',F,Len,Params,Ret).

% Ensure core library types are added when requested
ensure_corelib_types:- pfcAdd(please_do_corelib_types).



ensure_corelib_types:- pfcAdd(please_do_corelib_types).
%(need_corelib_types, metta_atom_corelib(Term)) ==> metta_atom_asserted('&corelib', Term).
(need_corelib_types, metta_atom(KB,Atom)) ==> metta_atom_asserted(KB, Atom).
:- dynamic(need_corelib_types/0).
(please_do_corelib_types, { \+ need_corelib_types }) ==> need_corelib_types.
'ensure-compiler!':- ensure_corelib_types.
if(Cond,Then,Else,Result):- eval_true(Cond)*-> eval(Then,Result); eval(Else,Result).

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
 

/* 
  File: predicates_functions_props.pl
  Description: This file defines various predicates related to arity (number of arguments), 
               function properties, and control flow mechanisms. It provides declarative information 
               about functions and predicates, including their arities and types.
*/

% The p_arity/2 predicate defines the arity (number of arguments) for predicates with special names.
% @param PredicateName The name of the predicate (e.g., 'SenaryPredicate').
% @param Arity The number of arguments the predicate takes.
% @example p_arity('SenaryPredicate', 6). % True, this predicate takes 6 arguments.
p_arity('SenaryPredicate', 6).
p_arity('SeptenaryPredicate', 7).
p_arity('OctaryPredicate', 8).
p_arity('NonaryPredicate', 9).
p_arity('DenaryPredicate', 10).

% The f_arity/2 predicate defines the arity (number of arguments) for functions.
% @param FunctionName The name of the function (e.g., 'NullaryFunction').
% @param Arity The number of arguments the function takes.
% @example f_arity('UnaryFunction', 1). % True, this function takes 1 argument.
f_arity('NullaryFunction', 0).   % No return value, essentially a procedure.
f_arity('UnaryFunction', 1).     % Returns a single value, and so on.
f_arity('BinaryFunction', 2).
f_arity('TernaryFunction', 3).
f_arity('QuaternaryFunction', 4).
f_arity('QuinaryFunction', 5).
f_arity('SenaryFunction', 6).
f_arity('SeptenaryFunction', 7).
f_arity('OctaryFunction', 8).
f_arity('NonaryFunction', 9).

% form_prop/1 and form_prop/2 predicates describe various properties of functions and predicates.

% The form_prop/1 predicate declares properties that apply to certain forms of execution.
% @param PropertyName The name of the property (e.g., 'Nondeterministic').
% @example form_prop('Deterministic'). % Declares deterministic functions
form_prop('Nondeterministic'). % "Nondeterministic" - Can produce more than one result for the same inputs.
form_prop('Deterministic').    % "Deterministic" - Always produces the same output for the same input.
form_prop('DirectTranspilation'). % "IdiomaticTranspilation" - Converts code to a more idiomatic form in another language.
form_prop('Compiled').         % "FunCompiled" - Functions are compiled to machine code for performance.
form_prop('Interpreted').      % "FunInterpreted" - Functions are executed by an interpreter, without compilation.
form_prop('BooleanFunction').  % "Boolean" - Maps success/failure in Prolog to True/False.

% form_prop/2 adds an additional parameter, providing further details about the property.
% @param PropertyName The name of the property (e.g., 'CoerceArgsToTypes').
% @param Argument The specific value or type related to the property (e.g., 'List').
% @example form_prop('CoerceArgsToTypes', 'List'). % Arguments are automatically coerced to a list.
form_prop('EvalNoArgs').           % "EvalNoArgs" - Don't evaluate or type check arguments.
form_prop('CoerceArgsToTypes', 'List'). % "CoerceArgsToTypes" - Arguments are coerced to a 'List'.
form_prop('TypeConstructor').      % Return the entire value unevaluated, check for EvalNoArgs/CoerceArgsToTypes first.
form_prop('OnFailReturnSelf').     % Return the function itself on failure, a default for MeTTa in Rust.
form_prop('OnFailBacktrack').      % Functions backtrack on failure, except for flow control instructions.

% Additional properties related to function arity and behavior:

% @example form_prop('FixedArityFunction'). % Declares a function with a fixed number of arguments.
form_prop('FixedArityFunction').   % "FixedArityFunction" - Functions or predicates with a fixed number of arguments.
form_prop('ReturnNthArg', 'Integer'). % "ReturnNthArg" - Functions return the Nth argument passed to them.
form_prop('FunctionArity', 'Integer'). % "FunctionArity" - The number of arguments a function takes.
form_prop('PredicateArity', 'Integer'). % "PredicateArity" - Number of arguments a predicate has after conversion to a function.
form_prop('ArityMinMax', 'Integer', 'Integer'). % "VariableArity" - Functions or predicates with variable arguments (Min, Max).

/* previously: The following block defines function types using custom Prolog syntax for types.
   It is skipped in this version of the code, potentially because this type system is not in active use
   or conflicts with the main logic. Leaving it here might signify future integration of a formal type system. */
% (: Z Nat)
% (: S (-> Nat Nat))
% (: S TypeConstructor)

% Control flow and conditional execution properties for core library functions.
% These predicates describe control flow operations such as conditionals, assignments, and function blocks.

% The properties/3 predicate defines properties for core library functions.
% @param Library The name of the library (e.g., '&corelib').
% @param FunctionName The function in the library to which the properties apply.
% @param Properties A list of properties that describe the behavior of the function.
% @example properties('&corelib', 'if', [flow_control, qhelp("Conditional execution."), conditional_execution]).
properties('&corelib', 'if', [flow_control, qhelp("Conditional execution."), conditional_execution]). % Conditional execution (if-else logic).
properties('&corelib', 'case', [flow_control, qhelp("Case selection."), conditional_execution]).     % Case selection (similar to switch statements).
properties('&corelib', 'let', [variable_assignment, qhelp("Variable assignment.")]).                  % Variable assignment (let).
properties('&corelib', 'let*', [variable_assignment, qhelp("Sequential variable assignment."), sequential]). % Sequential variable assignment (let*).
properties('&corelib', 'function', [function_definition, qhelp("Function block.")]).                  % Function block definition (defining functions).
properties('&corelib', 'return', [function_definition, qhelp("Return value of a function block."), return_value]). % Returning a value from a function.
/**
 * properties(+Library, +Predicate, +Attributes).
 * 
 * Defines the properties of predicates in a given library.
 * 
 * @param Library The library where the predicate belongs.
 * @param Predicate The name of the predicate.
 * @param Attributes A list of attributes that describe the predicate's properties.
 * 
 * @example properties('&corelib', 'Error', [error_handling, qhelp("Defines or triggers an error.")]).
 * 
 * This predicate associates a core library predicate with its attributes.
 */

% --- Error Handling and Advanced Control Flow ---
% Describes the 'Error' predicate, which is used for error handling.
properties('&corelib','Error', [error_handling, qhelp("Defines or triggers an error.")]).

% 'catch' predicate is used for handling exceptions in Prolog.
properties('&corelib','catch', [error_handling, qhelp("Catches exceptions."), exception_handling]).

% 'throw' predicate is used for throwing exceptions.
properties('&corelib','throw', [error_handling, qhelp("Throws exceptions."), exception_handling]).

% --- Data Structures and Manipulation ---
% 'collapse' collapses a structure into a simplified form.
properties('&corelib','collapse', [data_structures, qhelp("Collapses a structure."), manipulation]).

% 'sequential' applies operations in sequence to a given structure or data set.
properties('&corelib','sequential', [data_structures, qhelp("Sequentially applies operations."), sequential_operations]).

% 'superpose' superposes or overlays two data structures for manipulation or comparison.
properties('&corelib','superpose', [data_structures, qhelp("Superposes data structures."), manipulation]).

% 'repr' converts a given expression into its string representation.
properties('&corelib','repr', [data_structures, qhelp("Represent an expression as string."), repr ]).

% 'parse' parses a string and converts it into an expression or structured data.
properties('&corelib','parse', [data_structures, qhelp("Parse a string to an expression."), parse ]).

% --- Iteration and Loop Control ---
% 'dedup!' removes duplicates during iterations to avoid repetition of results.
properties('&corelib','dedup!', [iteration_control, qhelp("Removes duplicate elements from iteration."), manipulation]).

% 'nth!' limits or filters the iteration to only process the Nth element.
properties('&corelib','nth!', [iteration_control, qhelp("Allows only the Nth iteration."), manipulation]).

% 'limit!' restricts the number of iterations to a specified count.
properties('&corelib','limit!', [iteration_control, qhelp("Limits the number of iterations.")]).
 
% 'time-limit!' imposes a time restriction on the execution of iterations or processes.
properties('&corelib','time-limit!', [iteration_control, qhelp("Sets a time limit for operations."), time_management]).

% 'offset!' adjusts the starting point of an iteration process, skipping a certain number of initial elements.
properties('&corelib','offset!', [iteration_control, qhelp("Adjusts the starting point of iteration.")]).
 
% 'number-of' returns the number of iterations or counts the number of processed elements.
properties('&corelib','number-of', [iteration_control, qhelp("Returns iteration count.")]).
 
% 'nop' suppresses the result of an iteration; it is a specialty predicate for controlling unwanted outputs.
properties('&corelib','nop', [iteration_control, qhelp("Suppresses iteration result."), suppression]).

% 'do' appears to function similarly to 'nop' in suppressing the result, though may differ in specific control contexts.
properties('&corelib','do', [iteration_control, qhelp("Suppresses iteration result."), suppression]).

% --- Compiler Directives and Optimization ---
% 'pragma!' is a compiler directive to provide hints or settings for optimization.
properties('&corelib','pragma!', [compiler_directive, qhelp("Compiler directive for optimizations/settings."), optimization]).

% 'include!' is used for code inclusion from another file or module.
properties('&corelib','include!', [code_inclusion, qhelp("Includes code from another file or context.")]).

% 'load-ascii' reads the contents of an ASCII file, likely for text processing purposes.
properties('&corelib','load-ascii', [file_handling, qhelp("Loads ASCII file content.")]).
 
% 'extend-py!' extends the system's capabilities by integrating with Python.
properties('&corelib','extend-py!', [integration, qhelp("Extends integration with Python."), python]).

% 'registered-python-function' refers to a Python function registered for interaction with the Prolog environment.
properties('&corelib','registered-python-function', [integration, qhelp("Interacts with Python functions."), python]).

% 'import!' is used to import an external file, module, or package into the current context.
properties('&corelib','import!', [module_import, qhelp("Imports an external module or file.")]).

% --- Evaluation and Dynamic Calls ---
% 'eval' dynamically evaluates an expression during runtime.
properties('&corelib','eval', [evaluation, qhelp("Evaluates an expression.")]).
 
% 'eval-for' is used to evaluate an expression with an assumed return type, which may help in type checking or inference.
properties('&corelib','eval-for', [evaluation, qhelp("Evaluates assuming a return type."), type_assumption]).

% 'call!' dynamically decides whether to treat a term as a predicate or function and attempts to call it.
properties('&corelib','call!', [dynamic_call, qhelp("Tries to dynamically guess if predicate or function.")]).
 
% 'call-p!' specifically calls a term as a predicate, ensuring correct behavior for Prolog predicates.
properties('&corelib','call-p!', [dynamic_call, qhelp("Dynamically calls a predicate."), predicate]).

/* previously: There might be dead code here where additional properties could have been defined. 
It appears this section was intentionally left out to be filled in later for expansion, 
likely for more specialized predicates in corelib or additional use cases not yet supported. */
% --- PLDoc Header for `properties/3` predicate ---
/** 
 * properties(+Library, +FunctionName, +Attributes)
 * 
 * Defines properties related to core library functions, such as their behavior, help documentation, 
 * and other attributes.
 * 
 * @param Library The name of the library, typically '&corelib'.
 * @param FunctionName The name of the function or predicate whose properties are being defined.
 * @param Attributes A list that specifies various attributes about the function, such as its 
 *        category (e.g., 'arithmetic') and a help message for documentation.
 *
 * @example
 * ?- properties('&corelib', '+', [arithmetic, qhelp("Addition."), addition]).
 * This defines the '+' operator with an arithmetic attribute and provides a help description for addition.
 */
 
% --- Defining properties for predicates and functions in the core library ---
% Defining the arity of predicates/functions in the core library.
properties('&corelib','predicate-arity', 
    [function_definition, qhelp("Defines the arity of predicates/functions."), arity]).

% Dynamically calling a function within the core library.
properties('&corelib','call-fn!', 
    [dynamic_call, qhelp("Calls a function dynamically."), function]).

% Call a Python function through Prolog integration.
properties('&corelib','pyr!', 
    [integration, qhelp("Call python."), python]).

% Evaluates a string of Prolog code dynamically.
properties('&corelib','call-string!', 
    [evaluation, qhelp("Evaluates a string of Prolog code."), prolog_code]).

% --- Miscellaneous and Newly Included Properties ---
% Matches patterns within structures or data in the core library.
properties('&corelib','match', 
    [pattern_matching, qhelp("Matches patterns within structures or data.")]).
    
% Retrieves atoms from a given structure.
properties('&corelib','get-atoms', 
    [data_retrieval, qhelp("Retrieves atoms from a structure.")]).
    
% Allocates new space or memory region for internal use.
properties('&corelib','new-space', 
    [memory_allocation, qhelp("Allocates new space or memory region.")]).
    
% Removes an atom from a structure or container.
properties('&corelib','remove-atom', 
    [manipulation, qhelp("Removes an atom from a structure.")]).
    
% Adds or replaces an atom within a structure or memory location.
properties('&corelib','add-atom', 
    [manipulation, qhelp("Replaces an atom within a structure.")]).
    
% Conjunction operation; logical AND in Prolog terms.
properties('&corelib',',', 
    [logical_operation, qhelp("Conjunction; and."), conjunction]).
    
% Disjunction operation; logical OR in Prolog terms.
properties('&corelib',';', 
    [logical_operation, qhelp("Disjunction; or."), disjunction]).
    
% Replace an atom within a structure or memory space.
properties('&corelib','replace-atom', 
    [manipulation, qhelp("Replaces an atom within a structure.")]).
    
% Transfer content from one memory space to another in the core library.
properties('&corelib','transfer!', 
    [memory_management, qhelp("Transfers space content to another space.")]).

% --- Symbolic Arithmetic and Type Conversion ---
% Successor operation in Peano arithmetic (S(n) where n is a natural number).
properties('&corelib','S', 
    [arithmetic, qhelp("Successor in Peano arithmetic."), peano_arithmetic]).

% Zero element in Peano arithmetic (Z in Peano arithmetic).
properties('&corelib','Z', 
    [arithmetic, qhelp("Zero in Peano arithmetic."), peano_arithmetic]).

% Convert from one numeric type to another within the core library.
properties('&corelib','fromNumber', 
    [type_conversion, qhelp("Converts from a numeric type to another type.")]).
    
% Forces argument types for compatibility in type conversion.
properties('&corelib','coerce', 
    [type_conversion, qhelp("Forces argument types for compatibility."), compatibility]).

% --- Arithmetic Operations ---
% Addition operation in arithmetic.
properties('&corelib','+', 
    [arithmetic, qhelp("Addition."), addition]).

% Subtraction operation in arithmetic.
properties('&corelib','-', 
    [arithmetic, qhelp("Subtraction."), subtraction]).

% Multiplication operation in arithmetic.
properties('&corelib','*', 
    [arithmetic, qhelp("Multiplication."), multiplication]).

% Modulus operation in arithmetic, returning the remainder of division.
properties('&corelib','mod', 
    [arithmetic, qhelp("Modulus operation."), modulus]).

% --- Comparison Operators ---
% Less than comparison operator.
properties('&corelib','<', 
    [comparison, qhelp("Less than."), less_than]).

% Greater than or equal comparison operator.
properties('&corelib','>=', 
    [comparison, qhelp("Greater than or equal to."), greater_than_or_equal]).

% Alternate syntax for greater than or equal comparison operator.
properties('&corelib','=>', 
    [comparison, qhelp("Greater than or equal to."), greater_than_or_equal]).

% Less than or equal comparison operator.
properties('&corelib','<=', 
    [comparison, qhelp("Less than or equal to."), less_than_or_equal]).

% Alternate syntax for less than or equal comparison operator.
properties('&corelib','=<', 
    [comparison, qhelp("Less than or equal to."), less_than_or_equal]).

% Greater than comparison operator.
properties('&corelib','>', 
    [comparison, qhelp("Greater than."), greater_than]).

% --- Logic Comparison and Evaluation Control ---
% Equality or unification operator in Prolog.
properties('&corelib','=', 
    [logic, qhelp("Equality/unification operator."), equality]).

% Inequality test in Prolog logic.
properties('&corelib','\\=', 
    [logic, qhelp("Inequality test."), inequality]).
% File Directives:
% These `properties/3` facts are defining various predicates with different categories, 
% providing additional metadata (e.g., category, help text, and tags).
% The 'properties' predicate is commonly used for associating attributes with various 
% predicates or operations in systems that rely on meta-information.

% @param '&corelib': The core library of predicates.
% @param PredicateName: The name of the predicate being described.
% @param Properties: A list containing the predicate category, a brief explanation (`qhelp`), and related tags.

% Each `properties/3` declaration gives helpful descriptions and classifications for the core library predicates.

% --- Equality and Logic Predicates ---

/**
 * @predicate properties/3
 * @description Defines properties for the '==' (equality test).
 * @param '&corelib' The core library for the predicate.
 * @param '==' The equality test operator.
 * @param [logic, qhelp("Equality test."), equality_test] Metadata for the predicate.
 * @example 
 * ?- X == Y.
 * True if X and Y are identical.
 */
properties('&corelib', '==', [logic, qhelp("Equality test."), equality_test]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'or' (logical OR) operation.
 * @param '&corelib' The core library for the predicate.
 * @param 'or' The logical OR operator.
 * @param [logic, qhelp("Logical OR."), logical_or] Metadata for the predicate.
 * @example 
 * ?- true; false.
 * true.
 */
properties('&corelib', 'or', [logic, qhelp("Logical OR."), logical_or]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'xor' (logical XOR) operation.
 * @param '&corelib' The core library for the predicate.
 * @param 'xor' The logical XOR operator.
 * @param [logic, qhelp("Logical XOR."), logical_xor] Metadata for the predicate.
 * @example 
 * ?- xor(true, false).
 * true.
 */
properties('&corelib', 'xor', [logic, qhelp("Logical XOR."), logical_xor]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'and' (logical AND) operation.
 * @param '&corelib' The core library for the predicate.
 * @param 'and' The logical AND operator.
 * @param [logic, qhelp("Logical AND."), logical_and] Metadata for the predicate.
 * @example 
 * ?- true, true.
 * true.
 */
properties('&corelib', 'and', [logic, qhelp("Logical AND."), logical_and]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'not' (logical NOT) operation.
 * @param '&corelib' The core library for the predicate.
 * @param 'not' The logical NOT operator.
 * @param [logic, qhelp("Logical NOT."), logical_not] Metadata for the predicate.
 * @example 
 * ?- not(true).
 * false.
 */
properties('&corelib', 'not', [logic, qhelp("Logical NOT."), logical_not]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'quote' operation to prevent evaluation.
 * @param '&corelib' The core library for the predicate.
 * @param 'quote' Prevents evaluation, treating input as literal.
 * @param [evaluation_control, qhelp("Prevents evaluation, treating input as literal.")] Metadata for the predicate.
 * @example 
 * ?- quote(X).
 * X.
 */
properties('&corelib', 'quote', [evaluation_control, qhelp("Prevents evaluation, treating input as literal.")]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'unquote' operation to retrieve the value of a quoted expression.
 * @param '&corelib' The core library for the predicate.
 * @param 'unquote' The operation to retrieve value from a quoted expression.
 * @param [evaluation_control, qhelp("Retrieves value of a quote."), retrieval] Metadata for the predicate.
 * @example 
 * ?- unquote(quote(X)).
 * X.
 */
properties('&corelib', 'unquote', [evaluation_control, qhelp("Retrieves value of a quote."), retrieval]).

% --- Debugging, Output, and Assertions ---

/**
 * @predicate properties/3
 * @description Defines properties for the 'repl!' operation to enter an interactive REPL environment.
 * @param '&corelib' The core library for the predicate.
 * @param 'repl!' Enters a read-eval-print-loop for interactive evaluation.
 * @param [debugging, qhelp("Interactive read-eval-print loop."), interactive] Metadata for the predicate.
 */
properties('&corelib', 'repl!', [debugging, qhelp("Interactive read-eval-print loop."), interactive]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'time!' operation to measure execution time.
 * @param '&corelib' The core library for the predicate.
 * @param 'time!' Measures execution time for a given operation.
 * @param [execution_timing, qhelp("Execution timing.")] Metadata for the predicate.
 */
properties('&corelib', 'time!', [execution_timing, qhelp("Execution timing.")]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'trace!' operation to print debug information.
 * @param '&corelib' The core library for the predicate.
 * @param 'trace!' Enables printing of debug information.
 * @param [debugging, qhelp("Prints some debug information."), information_printing] Metadata for the predicate.
 */
properties('&corelib', 'trace!', [debugging, qhelp("Prints some debug information."), information_printing]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'no-rtrace!' operation to disable tracing.
 * @param '&corelib' The core library for the predicate.
 * @param 'no-rtrace!' Disables tracing for debugging purposes.
 * @param [debugging, qhelp("Disables tracing for debugging."), trace_control] Metadata for the predicate.
 */
properties('&corelib', 'no-rtrace!', [debugging, qhelp("Disables tracing for debugging."), trace_control]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'rtrace!' operation to enable tracing.
 * @param '&corelib' The core library for the predicate.
 * @param 'rtrace!' Enables tracing for debugging purposes.
 * @param [debugging, qhelp("Enables tracing for debugging."), trace_control] Metadata for the predicate.
 */
properties('&corelib', 'rtrace!', [debugging, qhelp("Enables tracing for debugging."), trace_control]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'println!' operation to print text with a newline.
 * @param '&corelib' The core library for the predicate.
 * @param 'println!' Prints text followed by a newline character.
 * @param [output, qhelp("Prints text with newline to output."), text_printing] Metadata for the predicate.
 */
properties('&corelib', 'println!', [output, qhelp("Prints text with newline to output."), text_printing]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'with-output-to!' operation to redirect output.
 * @param '&corelib' The core library for the predicate.
 * @param 'with-output-to!' Redirects output to a specified target.
 * @param [output, qhelp("Redirects output to a specified target."), redirection] Metadata for the predicate.
 */
properties('&corelib', 'with-output-to!', [output, qhelp("Redirects output to a specified target."), redirection]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'print' operation to print text without a newline.
 * @param '&corelib' The core library for the predicate.
 * @param 'print' Prints text to the output without appending a newline.
 * @param [output, qhelp("Prints text to output."), text_printing] Metadata for the predicate.
 */
properties('&corelib', 'print', [output, qhelp("Prints text to output."), text_printing]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'assertEqual' predicate to assert equality.
 * @param '&corelib' The core library for the predicate.
 * @param 'assertEqual' Asserts that two values are equal.
 * @param [testing, qhelp("Asserts a condition is true."), assertion] Metadata for the predicate.
 */
properties('&corelib', 'assertEqual', [testing, qhelp("Asserts a condition is true."), assertion]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'assertFalse' predicate to assert that a condition is false.
 * @param '&corelib' The core library for the predicate.
 * @param 'assertFalse' Asserts that a condition is false.
 * @param [testing, qhelp("Asserts a condition is false."), assertion] Metadata for the predicate.
 */
properties('&corelib', 'assertFalse', [testing, qhelp("Asserts a condition is false."), assertion]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'assertNotEqual' predicate to assert that two values are not equal.
 * @param '&corelib' The core library for the predicate.
 * @param 'assertNotEqual' Asserts that two values are not equal.
 * @param [testing, qhelp("Asserts two values are not equal."), assertion] Metadata for the predicate.
 */
properties('&corelib', 'assertNotEqual', [testing, qhelp("Asserts two values are not equal."), assertion]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'assertEqualToResult' predicate to assert equality to a result.
 * @param '&corelib' The core library for the predicate.
 * @param 'assertEqualToResult' Asserts equality to a result.
 * @param [testing, qhelp("Asserts equality to a result."), assertion] Metadata for the predicate.
 */
properties('&corelib', 'assertEqualToResult', [testing, qhelp("Asserts equality to a result."), assertion]).

% --- System Integration and State Management ---

/**
 * @predicate properties/3
 * @description Defines properties for the 'change-state!' predicate to modify system state.
 * @param '&corelib' The core library for the predicate.
 * @param 'change-state!' Changes the state of a system component.
 * @param [state_management, qhelp("Changes the state of a system component."), system_integration] Metadata for the predicate.
 */
properties('&corelib', 'change-state!', [state_management, qhelp("Changes the state of a system component."), system_integration]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'set-state' predicate to set a component's state.
 * @param '&corelib' The core library for the predicate.
 * @param 'set-state' Sets the state of a system or component.
 * @param [state_management, qhelp("Sets the state of a component or system.")] Metadata for the predicate.
 */
properties('&corelib', 'set-state', [state_management, qhelp("Sets the state of a component or system.")]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'get-state' predicate to retrieve system or component state.
 * @param '&corelib' The core library for the predicate.
 * @param 'get-state' Retrieves the state of a component or system.
 * @param [state_management, qhelp("Gets the state of a component or system."), data_retrieval] Metadata for the predicate.
 */
properties('&corelib', 'get-state', [state_management, qhelp("Gets the state of a component or system."), data_retrieval]).

% --- List Operations ---

/**
 * @predicate properties/3
 * @description Defines properties for the 'car-atom' predicate to retrieve the head of a list.
 * @param '&corelib' The core library for the predicate.
 * @param 'car-atom' Retrieves the head element of a list.
 * @param [list_operations, qhelp("Retrieves the head of a list."), head_retrieval] Metadata for the predicate.
 */
properties('&corelib', 'car-atom', [list_operations, qhelp("Retrieves the head of a list."), head_retrieval]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'cdr-atom' predicate to retrieve the tail of a list.
 * @param '&corelib' The core library for the predicate.
 * @param 'cdr-atom' Retrieves the tail of a list (everything except the head).
 * @param [list_operations, qhelp("Retrieves the tail of a list."), tail_retrieval] Metadata for the predicate.
 */
properties('&corelib', 'cdr-atom', [list_operations, qhelp("Retrieves the tail of a list."), tail_retrieval]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'range' predicate to generate a range of numbers.
 * @param '&corelib' The core library for the predicate.
 * @param 'range' Generates a range of numbers from a start to end value.
 * @param [list_operations, qhelp("Generates a range of numbers."), range_generation] Metadata for the predicate.
 */
properties('&corelib', 'range', [list_operations, qhelp("Generates a range of numbers."), range_generation]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'make_list' predicate to create a list with specified elements.
 * @param '&corelib' The core library for the predicate.
 * @param 'make_list' Creates a list from given elements.
 * @param [list_operations, qhelp("Creates a list with specified elements."), creation] Metadata for the predicate.
 */
properties('&corelib', 'make_list', [list_operations, qhelp("Creates a list with specified elements."), creation]).

/**
 * @predicate properties/3
 * @description Defines properties for the 'Cons' predicate to construct a list.
 * @param '&corelib' The core library for the predicate.
 * @param 'Cons' Constructs a new list by adding an element to the front.
 * @param [list_operations, qhelp("Constructs a list."), construction] Metadata for the predicate.
 */
properties('&corelib', 'Cons', [list_operations, qhelp("Constructs a list."), construction]).
/** 
 * properties/3
 * 
 * @param '&corelib' Identifier for core library predicates.
 * @param PredicateName The name of the predicate in the core library.
 * @param Attributes A list of attributes that include the category, a helper description, and any additional tags.
 * 
 * @details This is a fact representing various properties of predicates in the `&corelib` module. Each predicate is associated 
 * with its category, a description (qhelp), and relevant tags to describe its functionality. 
 * 
 * @examples 
 *   ?- properties('&corelib', 'length', Attrs).
 *   Attrs = [list_operations, qhelp("Determines the length of a list."), length_determination].
 */

% Predicate describing the properties of 'length', which determines the length of a list.
properties('&corelib', 'length', 
           [list_operations, 
            qhelp("Determines the length of a list."), 
            length_determination]).

% Predicate describing the properties of 'countElement', which counts occurrences of a given element in a list.
properties('&corelib', 'countElement', 
           [list_operations, 
            qhelp("Counts occurrences of an element."), 
            element_counting]).

% Predicate describing the properties of 'tuple-count', which counts the number of tuples in a given data structure.
properties('&corelib', 'tuple-count', 
           [data_structures, 
            qhelp("Counts tuples within a structure."), 
            counting]).

/* previously: properties('&corelib', 'TupleConcat', [data_structures, qhelp("Concatenates tuples."), concatenation]). */
/* Skipped Code: 'TupleConcat' might be intentionally skipped as tuple concatenation could be pending further development or is not required in current use cases. */

% Predicate describing the properties of 'unique', which ensures the uniqueness of nondeterministic results.
properties('&corelib', 'unique', 
           [nondet_sets, 
            qhelp("Makes nondet results unique."), 
            no_repeats_var]).

% Predicate describing the properties of 'subtraction', which lazily subtracts elements from two nondeterministic sets.
properties('&corelib', 'subtraction', 
           [nondet_sets, 
            qhelp("It subtracts elements generated by Call2 from those generated by Call1."), 
            lazy_subtraction]).

% Predicate describing the properties of 'intersection', which lazily computes the intersection of two nondeterministic sets, preserving duplicates.
properties('&corelib', 'intersection', 
           [nondet_sets, 
            qhelp("It gives the intersection duplicates are not removed."), 
            lazy_intersection]).

% Predicate describing the properties of 'union', which lazily computes the union of two nondeterministic sets.
properties('&corelib', 'union', 
           [nondet_sets, 
            qhelp("It gives the union of 2 lists."), 
            lazy_union]).

/* previously: properties('&corelib', 'collapseCardinality', [data_structures, qhelp("Collapses structures with cardinality consideration."), manipulation, cardinality]). */
/* Skipped Code: 'collapseCardinality' may have been deferred for now, as it likely involves complex structure manipulations based on cardinality, which could require additional development or refinement. */

% --- String and Character manipulation predicates ---

% Predicate describing the properties of 'stringToChars', which converts a string into a list of characters.
properties('&corelib', 'stringToChars', 
           [string_operations, 
            qhelp("Convert a string to a list of chars."), 
            string_to_chars]).

% Predicate describing the properties of 'charsToString', which converts a list of characters back into a string.
properties('&corelib', 'charsToString', 
           [string_operations, 
            qhelp("Convert a list of chars to a string."), 
            chars_to_string]).

% Predicate describing the properties of 'format-args', which formats a string using a given format specifier and arguments.
properties('&corelib', 'format-args', 
           [string_operations, 
            qhelp("Generate a formatted string using a format specifier."), 
            format_args]).

% Predicate describing the properties of 'flip', which returns a random boolean value.
properties('&corelib', 'flip', 
           [random, 
            qhelp("Return a random boolean."), 
            random_boolean]).
