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
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%:- multifile(baseKB:agent_action_queue/3).
%:- dynamic(baseKB:agent_action_queue/3).

:- set_prolog_flag(gc,true).

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

:- style_check(-discontiguous).
%:- enable_mpred_expansion.
:- expects_dialect(pfc).

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

:- set_prolog_flag(pfc_term_expansion,true).



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
  (in(FunctorObject,PredType)
    <==>
   in(FunctorObject,FunctType))).
% Automatically generating equivalency rules based on the arity of predicates and functions.
% This facilitates a dynamic and flexible understanding of function and predicate equivalences,
% enhancing Prolog's expressive power and semantic richness.
(((p_arity(PredType,PA), plus(FA,1,PA), f_arity(FunctType,FA)))
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




property(Op,E) ==> (form_op(Op),form_prop(E)).

((properties(A,B),{member(E,B)})==>property(A,E)).





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
properties('if', [flow_control, qhelp("Conditional execution."), conditional_execution]).
properties('case', [flow_control, qhelp("Case selection."), conditional_execution]).
properties('let', [variable_assignment, qhelp("Variable assignment.")]).
properties('let*', [variable_assignment, qhelp("Sequential variable assignment."), sequential]).
properties('function', [function_definition, qhelp("Function block.")]).
properties('return', [function_definition, qhelp("Return value of a function block."), return_value]).
properties('Error', [error_handling, qhelp("Defines or triggers an error.")]).

% --- Error Handling and Advanced Control Flow ---
properties('catch', [error_handling, qhelp("Catches exceptions."), exception_handling]).
properties('throw', [error_handling, qhelp("Throws exceptions."), exception_handling]).

% --- Data Structures and Manipulation ---
properties('collapse', [data_structures, qhelp("Collapses a structure."), manipulation]).
properties('sequential', [data_structures, qhelp("Sequentially applies operations."), sequential_operations]).
properties('superpose', [data_structures, qhelp("Superposes data structures."), manipulation]).

% --- Iteration and Loop Control ---
properties('dedup!', [iteration_control, qhelp("Removes duplicate elements from iteration."), manipulation]).
properties('nth!', [iteration_control, qhelp("Allows only the Nth iteration."), manipulation]).
properties('limit!', [iteration_control, qhelp("Limits the number of iterations.")]).
properties('time-limit!', [iteration_control, qhelp("Sets a time limit for operations."), time_management]).
properties('offset!', [iteration_control, qhelp("Adjusts the starting point of iteration.")]).
properties('number-of', [iteration_control, qhelp("Returns iteration count.")]).
properties('nop', [iteration_control, qhelp("Suppresses iteration result."), suppression]).
properties('do', [iteration_control, qhelp("Suppresses iteration result."), suppression]).

% --- Compiler Directives and Optimization ---
properties('pragma!', [compiler_directive, qhelp("Compiler directive for optimizations/settings."), optimization]).
properties('include!', [code_inclusion, qhelp("Includes code from another file or context.")]).
properties('load-ascii', [file_handling, qhelp("Loads ASCII file content.")]).
properties('extend-py!', [integration, qhelp("Extends integration with Python."), python]).
properties('registered-python-function', [integration, qhelp("Interacts with Python functions."), python]).
properties('import!', [module_import, qhelp("Imports an external module or file.")]).

% --- Evaluation and Dynamic Calls ---
properties('eval', [evaluation, qhelp("Evaluates an expression.")]).
properties('eval-for', [evaluation, qhelp("Evaluates assuming a return type."), type_assumption]).
properties('call!', [dynamic_call, qhelp("Tries to dynamically guess if predicate or function.")]).
properties('call-p!', [dynamic_call, qhelp("Dynamically calls a predicate."), predicate]).
properties('predicate-arity', [function_definition, qhelp("Defines the arity of predicates/functions."), arity]).
properties('call-fn!', [dynamic_call, qhelp("Calls a function dynamically."), function]).
properties('pyr!', [integration, qhelp("Call python."), python]).
properties('call-string!', [evaluation, qhelp("Evaluates a string of Prolog code."), prolog_code]).

% --- Miscellaneous and Newly Included Properties ---
properties('match', [pattern_matching, qhelp("Matches patterns within structures or data.")]).
properties('get-atoms', [data_retrieval, qhelp("Retrieves atoms from a structure.")]).
properties('new-space', [memory_allocation, qhelp("Allocates new space or memory region.")]).
properties('remove-atom', [manipulation, qhelp("Removes an atom from a structure.")]).
properties('add-atom', [manipulation, qhelp("Replaces an atom within a structure.")]).
properties(',', [logical_operation, qhelp("Conjunction; and."), conjunction]).
properties(';', [logical_operation, qhelp("Disjunction; or."), disjunction]).
properties('replace-atom', [manipulation, qhelp("Replaces an atom within a structure.")]).
properties('transfer!', [memory_management, qhelp("Transfers space content to another space.")]).

% --- Symbolic Arithmetic and Type Conversion ---
properties('S', [arithmetic, qhelp("Successor in Peano arithmetic."), peano_arithmetic]).
properties('Z', [arithmetic, qhelp("Zero in Peano arithmetic."), peano_arithmetic]).
properties('fromNumber', [type_conversion, qhelp("Converts from a numeric type to another type.")]).
properties('coerce', [type_conversion, qhelp("Forces argument types for compatibility."), compatibility]).

% --- Arithmetic Operations ---
properties('+', [arithmetic, qhelp("Addition."), addition]).
properties('-', [arithmetic, qhelp("Subtraction."), subtraction]).
properties('*', [arithmetic, qhelp("Multiplication."), multiplication]).
properties('mod', [arithmetic, qhelp("Modulus operation."), modulus]).
properties('<', [comparison, qhelp("Less than."), less_than]).
properties('>=', [comparison, qhelp("Greater than or equal to."), greater_than_or_equal]).
properties('=>', [comparison, qhelp("Greater than or equal to."), greater_than_or_equal]).
properties('<=', [comparison, qhelp("Less than or equal to."), less_than_or_equal]).
properties('=<', [comparison, qhelp("Less than or equal to."), less_than_or_equal]).
properties('>', [comparison, qhelp("Greater than."), greater_than]).

% --- Logic Comparison and Evaluation Control ---
properties('=', [logic, qhelp("Equality/unification operator."), equality]).
properties('\\=', [logic, qhelp("Inequality test."), inequality]).
properties('==', [logic, qhelp("Equality test."), equality_test]).
properties('or', [logic, qhelp("Logical OR."), logical_or]).
properties('and', [logic, qhelp("Logical AND."), logical_and]).
properties('not', [logic, qhelp("Logical NOT."), logical_not]).
properties('quote', [evaluation_control, qhelp("Prevents evaluation, treating input as literal.")]).
properties('unquote', [evaluation_control, qhelp("Retrieves value of a quote."), retrieval]).

% --- Debugging, Output, and Assertions ---
properties('repl!', [debugging, qhelp("Interactive read-eval-print loop."), interactive]).
properties('time!', [execution_timing, qhelp("Execution timing.")]).
properties('trace!', [debugging, qhelp("Prints some debug information."), information_printing]).
properties('no-rtrace!', [debugging, qhelp("Disables tracing for debugging."), trace_control]).
properties('rtrace!', [debugging, qhelp("Enables tracing for debugging."), trace_control]).
properties('println!', [output, qhelp("Prints text with newline to output."), text_printing]).
properties('with-output-to!', [output, qhelp("Redirects output to a specified target."), redirection]).
properties('print', [output, qhelp("Prints text to output."), text_printing]).
properties('assertTrue', [testing, qhelp("Asserts a condition is true."), assertion]).
properties('assertFalse', [testing, qhelp("Asserts a condition is false."), assertion]).
properties('assertEqual', [testing, qhelp("Asserts two values are equal."), assertion]).
properties('assertNotEqual', [testing, qhelp("Asserts two values are not equal."), assertion]).
properties('assertEqualToResult', [testing, qhelp("Asserts equality to a result."), assertion]).

% --- System Integration and State Management ---
properties('change-state!', [state_management, qhelp("Changes the state of a system component."), system_integration]).
properties('set-state', [state_management, qhelp("Sets the state of a component or system.")]).
properties('get-state', [state_management, qhelp("Gets the state of a component or system."), data_retrieval]).

% --- List Operations ---
properties('car-atom', [list_operations, qhelp("Retrieves the head of a list."), head_retrieval]).
properties('cdr-atom', [list_operations, qhelp("Retrieves the tail of a list."), tail_retrieval]).
properties('range', [list_operations, qhelp("Generates a range of numbers."), range_generation]).
properties('make_list', [list_operations, qhelp("Creates a list with specified elements."), creation]).
properties('Cons', [list_operations, qhelp("Constructs a list."), construction]).
properties('length', [list_operations, qhelp("Determines the length of a list."), length_determination]).
properties('countElement', [list_operations, qhelp("Counts occurrences of an element."), element_counting]).
properties('tuple-count', [data_structures, qhelp("Counts tuples within a structure."), counting]).
%properties('TupleConcat', [data_structures, qhelp("Concatenates tuples."), concatenation]).
%properties('collapseCardinality', [data_structures, qhelp("Collapses structures with cardinality consideration."), manipulation, cardinality]).


:- ensure_loaded('metta_ontology_level_1.pfc').


:- set_prolog_flag(expect_pfc_file,never).
:- set_prolog_flag(pfc_term_expansion,false).

