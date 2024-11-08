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