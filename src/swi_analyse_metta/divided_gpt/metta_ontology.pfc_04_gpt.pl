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