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


%:- multifile(baseKB:agent_action_queue/3).
%:- dynamic(baseKB:agent_action_queue/3).

%:- set_prolog_flag(gc,true).

:- thread_local(t_l:disable_px/0).
:- retractall(t_l:disable_px).

:- must(\+ t_l:disable_px).

:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).
:-
 current_prolog_flag(access_level,Was),
 set_prolog_flag(access_level,system),
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'),
 op(600,yfx,'&'),
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'),
 op(1199,fx,('==>')),
 set_prolog_flag(access_level,Was).

%:- style_check(-discontiguous).
%:- enable_mpred_expansion.
%:- expects_dialect(pfc).

/*
:- dynamic   lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3).
:- dynamic lmcache:session_io/4, lmcache:session_agent/2, lmcache:agent_session/2,   telnet_fmt_shown/3,   agent_action_queue/3).

*/
%:- nop('$set_source_module'( baseKB)).
:- set_prolog_flag(runtime_speed, 0).
:- set_prolog_flag(runtime_safety, 2).
:- set_prolog_flag(runtime_debug, 2).
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(expect_pfc_file,always).



:- set_prolog_flag(pfc_term_expansion,false).


params_and_return_type([->|TypeList],Len,Params,Ret):-
   append(Params,[Ret], TypeList),
   length(Params,Len).

merge_fp(_,_,N) :- N<1.
merge_fp(T1,T2,N) :-
  N>0,
  arg(N,T1,X),
  arg(N,T2,X),
  N1 is N-1,
  merge_fp(T1,T2,N1).

:- set_prolog_flag(pfc_term_expansion,true).

((metta_atom_asserted(KB,['==>',X,Y])/nonvar(KB)),
  metta_atom_asserted(KB2,X)) ==>
  metta_atom_asserted(KB2,Y).
/*

'functional-predicate'(Name,Arity) ==>
  {functor(P1,Name,Arity),
   functor(P2,Name,Arity),
   arg(Arity,P1,PV1),
   arg(Arity,P2,PV2),
   N is Arity-1,
   merge_fp(P1,P2,N)},
  (P1,{P2,PV1\==PV2} ==> ~P2).


==> 'functional-predicate'('next-operation',1).
==> 'functional-predicate'('previous-operation',1).

:- dynamic('op-complete'/1).

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


% ==> 'next-operation'(next).


((properties(KB,A,B),{member(E,B),nonvar(E)})==>property(KB,A,E)).
property(_,Op,E) ==> (form_op(Op),form_prop(E)).

((property(KB,F,PA),p_arity(PA,A)) ==> (predicate_arity(KB,F,A))).
((property(KB,F,FA),f_arity(FA,A)) ==> (functional_arity(KB,F,A))).


% (metta_compiled_predicate(KB,F,A)==>predicate_arity(KB,F,A)).

(metta_atom_asserted(KB,[C,H,T])/(C==':')) ==> metta_type(KB,H,T).
(metta_atom_asserted(KB,[C,H,T|Nil])/(Nil==[],C=='=',H=II)) ==> metta_defn(KB,II,T).
(metta_atom_asserted(KB,[C,H,A1,A2|AL])/(C=='=')) ==> metta_defn(KB,H,[A1,A2|AL]).
(metta_atom_asserted(KB,[C,H|AL])/(C==':-')) ==> metta_defn(KB,H,['wam-body'|AL]).

metta_defn(KB,[F|Args],_)/length(Args,Len)
  ==>src_code_for(KB,F,Len).

'op-complete'(op(+,'=',F)),
  metta_defn(KB,[F|Args],_)/length(Args,Len)
  ==>src_code_for(KB,F,Len),{nop(dedupe_cl(F))}.

(src_code_for(KB,F,Len)==>function_arity(KB,F,Len)).

('op-complete'(op(+,':',F))
 ==>
 (( metta_type(KB,F,TypeList)/is_list(TypeList),
  {params_and_return_type(TypeList,Len,Params,Ret)}) ==>
  metta_params_and_return_type(KB,F,Len,Params,Ret),{do_once(show_deds_w(F))})).

metta_params_and_return_type(KB,F,Len,Params,Ret),
  {is_absorbed_return_type(Params,Ret)}
   ==>(function_arity(KB,F,Len),is_absorbed_return(KB,F,Len,Ret),predicate_arity(KB,F,Len)).

metta_params_and_return_type(KB,F,Len,Params,Ret),
 { is_non_absorbed_return_type(Params,Ret),  Len1 is Len+1}
  ==>(function_arity(KB,F,Len),is_non_absorbed_return(KB,F,Len,Ret),predicate_arity(KB,F,Len1)).

(need_corelib_types,op_decl(F,Params,Ret),{nonvar(Ret),length(Params,Len)})==>
   metta_params_and_return_type('&corelib',F,Len,Params,Ret).


ensure_corelib_types:- pfcAdd(please_do_corelib_types).
%(need_corelib_types, metta_atom_corelib(Term)) ==> metta_atom_asserted('&corelib', Term).
(need_corelib_types, metta_atom(KB,Atom)) ==> metta_atom_asserted(KB, Atom).
:- dynamic(need_corelib_types/0).
(please_do_corelib_types, { \+ need_corelib_types }) ==> need_corelib_types.
'ensure-compiler!':- ensure_corelib_types.
% if(Cond,Then,Else,Result):- eval_true(Cond)*-> eval(Then,Result); eval(Else,Result).



:- dynamic(can_compile/2).

src_code_for(KB,F,Len) ==>  ( \+ metta_compiled_predicate(KB,F,Len) ==> do_compile(KB,F,Len)).

do_compile_space(KB) ==> (src_code_for(KB,F,Len) ==> do_compile(KB,F,Len)).

%do_compile_space('&self').

do_compile(KB,F,Len),src_code_for(KB,F,Len) ==> really_compile(KB,F,Len).


metta_defn(KB,[F|Args],BodyFn),really_compile(KB,F,Len)/length(Args,Len)==>
   really_compile_src(KB,F,Len,Args,BodyFn),{dedupe_ls(F)}.

really_compile_src(KB,F,Len,Args,BodyFn),
   {compile_metta_defn(KB,F,Len,Args,BodyFn,Clause)}
       ==> (compiled_clauses(KB,F,Clause)).



%:- ensure_loaded('metta_ontology_level_1.pfc').


:- endif.
*/
:- if(false).
a==>b.
b==>bb.

a.
:- b.
:- bb.
:- endif.

%:- pfcWhy1(a).
%:- pfcWhy1(b).

:- set_prolog_flag(expect_pfc_file,never).
:- set_prolog_flag(pfc_term_expansion,false).


test_fwc:-
  pfcAdd_Now(c(X)==>d(X)),
  pfcAdd_Now(c(1)),
  c(_),
  d(_),
  pfcWhy1(c(_)),
  pfcWhy1(d(_)),
  pfcAdd(e(2)),
  e(_),
  pfcAdd(e(X)<==>f(X)),
  f(_),
  pfcWhy1(e(_)),
  pfcWhy1(f(_)).


%:- forall(==>(X,Y),pfcFwd(==>(X,Y))).

%:- break.

%:- must_det_ll(property('length',list_operations)).




end_of_file.



/*
    really_compile(KB,F,Len)==>
      ((metta_defn(KB,[F|Args],BodyFn)/compile_metta_defn(KB,F,Len,Args,BodyFn,Clause))
        ==> (compiled_clauses(KB,F,Clause))).
*/




% Predicate and Function Arity Definitions:
% Specifies the number of arguments (arity) for predicates and functions, which is fundamental
% for understanding the complexity and capabilities of various logical constructs. Predicates are defined
% from Nullary (no arguments) up to Denary (ten arguments), reflecting a range of logical conditions or assertions.
% Functions are similarly defined but focus on operations that return a value, extending up to Nonary (nine arguments).
% Enforcing Equivalency Between Predicates and Functions:
% Establishes a logical framework to equate the conceptual roles of predicates and functions based on arity.
% This equivalence bridges the functional programming and logical (declarative) paradigms within Prolog,
% allowing a unified approach to defining operations and assertions.

(equivalentTypes(PredType,FunctType) ==>
  ( property(KB,FunctorObject,PredType)
    <==>
    property(KB,FunctorObject,FunctType))).
% Automatically generating equivalency rules based on the arity of predicates and functions.
% This facilitates a dynamic and flexible understanding of function and predicate equivalences,
% enhancing Prolog's expressive power and semantic richness.
(((p_arity(PredType,PA), {plus(KB,FA,1,PA), FA>=0}, f_arity(KB,FunctType,FA)))
  ==> equivalentTypes(PredType,FunctType)).

p_arity('NullaryPredicate', 0).  % No arguments.
p_arity('UnaryPredicate', 1).    % One argument.
p_arity('BinaryPredicate', 2).   % Two arguments.
p_arity('TernaryPredicate', 3).  % Three arguments, and so on.
p_arity('QuaternaryPredicate', 4).
p_arity('QuinaryPredicate', 5).
p_arity('SenaryPredicate', 6).
p_arity('SeptenaryPredicate', 7).
p_arity('OctaryPredicate', 8).
p_arity('NonaryPredicate', 9).
p_arity('DenaryPredicate', 10).

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


% "Nondeterministic" - Can produce more than one result for the same inputs.
form_prop('Nondeterministic').
% "Deterministic" - Always produces the same output for the same input.
form_prop('Deterministic').
% "IdiomaticTranspilation" - Converts code to a more idiomatic form in another language.
form_prop('DirectTranspilation').
% "FunCompiled" - Functions are compiled to machine code for performance.
form_prop('Compiled').
% "FunInterpreted" - Functions are executed by an interpreter, without compilation.
form_prop('Interpreted').

% "Boolean" - Maps success/failure in Prolog to True/False.
form_prop('BooleanFunction').

% "EvalNoArgs" - dont evaluate or type check args
form_prop('EvalNoArgs').
% "CoerceArgsToTypes" - Arguments are automatically coerced to specified types.
form_prop('CoerceArgsToTypes', 'List').
 % check EvalNoArgs/CoerceArgsToTypes then return the whole value unevaluated
form_prop('TypeConstructor').
% this is the default for MeTTa in rust
form_prop('OnFailReturnSelf').
% except for flow control instuctructions functions
form_prop('OnFailBacktrack').


% "FixedArityFunction" - Functions or predicates with a fixed number of arguments.
form_prop('FixedArityFunction').
% "ReturnNthArg" - Functions return the Nth argument passed to them.
form_prop('ReturnNthArg', 'Integer').
% "FunctionArity" - The number of arguments a function takes (2 here).
form_prop('FunctionArity', 'Integer').
% "PredicateArity" - The number of arguments a predicate has after being converted to a function
form_prop('PredicateArity', 'Integer').
% "VariableArity" - Functions or predicates with a variable number of arguments.
form_prop('ArityMinMax', 'Integer', 'Integer'). % Min Max


%(: Z Nat)
%(: S (-> Nat Nat))
%(: S TypeConstructor)

% --- Control Flow and Conditional Execution ---
properties('&corelib','if', [flow_control, qhelp("Conditional execution."), conditional_execution]).
properties('&corelib','case', [flow_control, qhelp("Case selection."), conditional_execution]).
properties('&corelib','let', [variable_assignment, qhelp("Variable assignment.")]).
properties('&corelib','let*', [variable_assignment, qhelp("Sequential variable assignment."), sequential]).
properties('&corelib','sealed', [variable_scoping, qhelp("Variable scoping.")]).
properties('&corelib','function', [function_definition, qhelp("Function block.")]).
properties('&corelib','return', [function_definition, qhelp("Return value of a function block."), return_value]).
properties('&corelib','Error', [error_handling, qhelp("Defines or triggers an error.")]).

% --- Error Handling and Advanced Control Flow ---
properties('&corelib','catch', [error_handling, qhelp("Catches exceptions."), exception_handling]).
properties('&corelib','throw', [error_handling, qhelp("Throws exceptions."), exception_handling]).

% --- Data Structures and Manipulation ---
properties('&corelib','collapse', [data_structures, qhelp("Collapses a structure."), manipulation]).
properties('&corelib','sequential', [data_structures, qhelp("Sequentially applies operations."), sequential_operations]).
properties('&corelib','superpose', [data_structures, qhelp("Superposes data structures."), manipulation]).

properties('&corelib','repr', [data_structures, qhelp("Represent an expression as string."), repr ]).
properties('&corelib','parse', [data_structures, qhelp("Parse a string to an expression."), parse ]).


% --- Iteration and Loop Control ---
properties('&corelib','dedup!', [iteration_control, qhelp("Removes duplicate elements from iteration."), manipulation]).
properties('&corelib','nth!', [iteration_control, qhelp("Allows only the Nth iteration."), manipulation]).
properties('&corelib','limit!', [iteration_control, qhelp("Limits the number of iterations.")]).
properties('&corelib','time-limit!', [iteration_control, qhelp("Sets a time limit for operations."), time_management]).
properties('&corelib','offset!', [iteration_control, qhelp("Adjusts the starting point of iteration.")]).
properties('&corelib','number-of', [iteration_control, qhelp("Returns iteration count.")]).
properties('&corelib','nop', [iteration_control, qhelp("Suppresses iteration result."), suppression]).
properties('&corelib','do', [iteration_control, qhelp("Suppresses iteration result."), suppression]).

% --- Compiler Directives and Optimization ---
properties('&corelib','pragma!', [compiler_directive, qhelp("Compiler directive for optimizations/settings."), optimization]).
properties('&corelib','include!', [code_inclusion, qhelp("Includes code from another file or context.")]).
properties('&corelib','load-ascii', [file_handling, qhelp("Loads ASCII file content.")]).
properties('&corelib','extend-py!', [integration, qhelp("Extends integration with Python."), python]).
properties('&corelib','registered-python-function', [integration, qhelp("Interacts with Python functions."), python]).
properties('&corelib','import!', [module_import, qhelp("Imports an external module or file.")]).

% --- Evaluation and Dynamic Calls ---
properties('&corelib','eval', [evaluation, qhelp("Evaluates an expression.")]).
properties('&corelib','eval-for', [evaluation, qhelp("Evaluates assuming a return type."), type_assumption]).
properties('&corelib','call!', [dynamic_call, qhelp("Tries to dynamically guess if predicate or function.")]).
properties('&corelib','call-p!', [dynamic_call, qhelp("Dynamically calls a predicate."), predicate]).
properties('&corelib','predicate-arity', [function_definition, qhelp("Defines the arity of predicates/functions."), arity]).
properties('&corelib','call-fn!', [dynamic_call, qhelp("Calls a function dynamically."), function]).
properties('&corelib','pyr!', [integration, qhelp("Call python."), python]).
properties('&corelib','call-string!', [evaluation, qhelp("Evaluates a string of Prolog code."), prolog_code]).

% --- Miscellaneous and Newly Included Properties ---
properties('&corelib','match', [pattern_matching, qhelp("Matches patterns within structures or data.")]).
properties('&corelib','get-atoms', [data_retrieval, qhelp("Retrieves atoms from a structure.")]).
properties('&corelib','new-space', [memory_allocation, qhelp("Allocates new space or memory region.")]).
properties('&corelib','remove-atom', [manipulation, qhelp("Removes an atom from a structure.")]).
properties('&corelib','add-atom', [manipulation, qhelp("Replaces an atom within a structure.")]).
properties('&corelib',',', [logical_operation, qhelp("Conjunction; and."), conjunction]).
properties('&corelib',';', [logical_operation, qhelp("Disjunction; or."), disjunction]).
properties('&corelib','replace-atom', [manipulation, qhelp("Replaces an atom within a structure.")]).
properties('&corelib','transfer!', [memory_management, qhelp("Transfers space content to another space.")]).

% --- Symbolic Arithmetic and Type Conversion ---
properties('&corelib','S', [arithmetic, qhelp("Successor in Peano arithmetic."), peano_arithmetic]).
properties('&corelib','Z', [arithmetic, qhelp("Zero in Peano arithmetic."), peano_arithmetic]).
properties('&corelib','fromNumber', [type_conversion, qhelp("Converts from a numeric type to another type.")]).
properties('&corelib','coerce', [type_conversion, qhelp("Forces argument types for compatibility."), compatibility]).

% --- Arithmetic Operations ---
properties('&corelib','+', [arithmetic, qhelp("Addition."), addition]).
properties('&corelib','-', [arithmetic, qhelp("Subtraction."), subtraction]).
properties('&corelib','*', [arithmetic, qhelp("Multiplication."), multiplication]).
properties('&corelib','mod', [arithmetic, qhelp("Modulus operation."), modulus]).
properties('&corelib','<', [comparison, qhelp("Less than."), less_than]).
properties('&corelib','>=', [comparison, qhelp("Greater than or equal to."), greater_than_or_equal]).
properties('&corelib','=>', [comparison, qhelp("Greater than or equal to."), greater_than_or_equal]).
properties('&corelib','<=', [comparison, qhelp("Less than or equal to."), less_than_or_equal]).
properties('&corelib','=<', [comparison, qhelp("Less than or equal to."), less_than_or_equal]).
properties('&corelib','>', [comparison, qhelp("Greater than."), greater_than]).

% --- Logic Comparison and Evaluation Control ---
properties('&corelib','=', [logic, qhelp("Equality/unification operator."), equality]).
properties('&corelib','\\=', [logic, qhelp("Inequality test."), inequality]).
properties('&corelib','==', [logic, qhelp("Equality test."), equality_test]).
properties('&corelib','or', [logic, qhelp("Logical OR."), logical_or]).
properties('&corelib','xor', [logic, qhelp("Logical XOR."), logical_xor])
properties('&corelib','and', [logic, qhelp("Logical AND."), logical_and]).
properties('&corelib','not', [logic, qhelp("Logical NOT."), logical_not]).
properties('&corelib','quote', [evaluation_control, qhelp("Prevents evaluation, treating input as literal.")]).
properties('&corelib','unquote', [evaluation_control, qhelp("Retrieves value of a quote."), retrieval]).

% --- Debugging, Output, and Assertions ---
properties('&corelib','repl!', [debugging, qhelp("Interactive read-eval-print loop."), interactive]).
properties('&corelib','time!', [execution_timing, qhelp("Execution timing.")]).
properties('&corelib','trace!', [debugging, qhelp("Prints some debug information."), information_printing]).
properties('&corelib','no-rtrace!', [debugging, qhelp("Disables tracing for debugging."), trace_control]).
properties('&corelib','rtrace!', [debugging, qhelp("Enables tracing for debugging."), trace_control]).
properties('&corelib','println!', [output, qhelp("Prints text with newline to output."), text_printing]).
properties('&corelib','with-output-to!', [output, qhelp("Redirects output to a specified target."), redirection]).
properties('&corelib','print', [output, qhelp("Prints text to output."), text_printing]).
properties('&corelib','assertEqual', [testing, qhelp("Asserts a condition is true."), assertion]).
properties('&corelib','assertFalse', [testing, qhelp("Asserts a condition is false."), assertion]).
properties('&corelib','assertEqual', [testing, qhelp("Asserts two values are equal."), assertion]).
properties('&corelib','assertNotEqual', [testing, qhelp("Asserts two values are not equal."), assertion]).
properties('&corelib','assertEqualToResult', [testing, qhelp("Asserts equality to a result."), assertion]).

% --- System Integration and State Management ---
properties('&corelib','change-state!', [state_management, qhelp("Changes the state of a system component."), system_integration]).
properties('&corelib','set-state', [state_management, qhelp("Sets the state of a component or system.")]).
properties('&corelib','get-state', [state_management, qhelp("Gets the state of a component or system."), data_retrieval]).

% --- List Operations ---
properties('&corelib','car-atom', [list_operations, qhelp("Retrieves the head of a list."), head_retrieval]).
properties('&corelib','cdr-atom', [list_operations, qhelp("Retrieves the tail of a list."), tail_retrieval]).
properties('&corelib','range', [list_operations, qhelp("Generates a range of numbers."), range_generation]).
properties('&corelib','make_list', [list_operations, qhelp("Creates a list with specified elements."), creation]).
properties('&corelib','Cons', [list_operations, qhelp("Constructs a list."), construction]).
properties('&corelib','length', [list_operations, qhelp("Determines the length of a list."), length_determination]).
properties('&corelib','countElement', [list_operations, qhelp("Counts occurrences of an element."), element_counting]).
properties('&corelib','tuple-count', [data_structures, qhelp("Counts tuples within a structure."), counting]).
%properties('&corelib','TupleConcat', [data_structures, qhelp("Concatenates tuples."), concatenation]).
%properties('&corelib','collapseCardinality', [data_structures, qhelp("Collapses structures with cardinality consideration."), manipulation, cardinality]).

% --- Nondet unique,union,intersection,subtraction Operations ---
properties('&corelib','unique', [nondet_sets, qhelp("Makes nondet results unique."), no_repeats_var]).
properties('&corelib','subtraction', [nondet_sets, qhelp("It subtracts elements generated by Call2 from those generated by Call1."), lazy_subtraction]).
properties('&corelib','intersection', [nondet_sets, qhelp("It gives the intersection duplicates are not removed ."), lazy_intersection]).
properties('&corelib','union', [nondet_sets, qhelp("It gives the union of 2 list ."), lazy_union ]).


% --- String and Character manipulation ---
properties('&corelib','stringToChars', [string_operations, qhelp("Convert a string to a list of chars."), string_to_chars]).
properties('&corelib','charsToString', [string_operations, qhelp("Convert a list of chars to a string."), chars_to_string]).
properties('&corelib','format-args', [string_operations, qhelp("Generate a formatted string using a format specifier."), format_args]).
properties('&corelib','flip', [random, qhelp("Return a random boolean."), random_boolean]).



