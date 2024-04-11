% Predicate and Function Arity Definitions:
% Specifies the number of arguments (arity) for predicates and functions, which is fundamental
% for understanding the complexity and capabilities of various logical constructs. Predicates are defined
% from Nullary (no arguments) up to Denary (ten arguments), reflecting a range of logical conditions or assertions.
% Functions are similarly defined but focus on operations that return a value, extending up to Nonary (nine arguments).
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
((p_arity(PredType,PA), plus(FA,1,PA), f_arity(FunctType,FA))) 
  ==> equivalentTypes(PredType,FunctType)).

% Flow Control Structures:
% Control structures are essential for directing the execution flow of a program. They enable conditional execution,
% looping, and choice between different paths of execution based on logical conditions or external inputs.
property('if', flow_control).          % Conditional execution based on a boolean expression.
property('case', flow_control).        % Choice between multiple paths.
property('let', flow_control).         % Variable binding in a local scope.
property('let*', flow_control).        % Sequential variable binding with dependency.
property('do', flow_control).          % Executes a block of code.
property('limit', flow_control_modification).   % Limits the number of solutions.
property('offset', flow_control_modification).  % Skips a number of solutions.
property('max-time', execution_time_control).   % Limits execution time.
% Inferring backtracking behavior in flow control structures. This indicates that certain paths
% of execution might lead to backtracking, a core concept in Prolog for exploring alternative solutions.
property(P, flow_control) ==> property(P, 'OnFailBacktrack').

% Assertions and Testing Mechanisms:
% Assertions provide a powerful tool for validating expected conditions or outcomes within a program.
% They are critical for debugging and verifying the correctness of logic under various conditions.
property('assertTrue', assertions_testing).     % Asserts a condition is true.
property('assertFalse', assertions_testing).    % Asserts a condition is false.
property('assertEqual', assertions_testing).    % Asserts equality between two values.
property('assertNotEqual', assertions_testing). % Asserts inequality.
property('assertEqualToResult', assertions_testing). % Asserts a value equals an expected result.
% Asserting deterministic outcomes for testing mechanisms. These properties ensure that assertions
% yield predictable, binary outcomes (pass or fail) based on the conditions they test.
property(P, assertions_testing) ==> property(P, 'Deterministic').

% Special Operators and System Interaction:
% Special operators and functionalities enhance Prolog's interaction with its execution environment and system,
% enabling dynamic control flows, system-level operations, and interaction with external processes or data.
property('!', special_operators).               % Cut operator, controls backtracking.
property('call!', special_operators).           % Dynamically calls a predicate.
property('call-fn!', special_operators).        % Calls a function dynamically.
property('repl!', system_interaction).          % Interactive read-eval-print loop.
property('pyr!', special_operators).            % Example of an extension or plugin call.
property('call-cleanup!', resource_management). % Ensures cleanup after execution.
property('setup-call-cleanup!', resource_management). % Setup, call, and cleanup pattern.
property('with-output-to!', output_redirection). % Redirects output to a different stream.
% Deterministic behavior is noted for operations that have predictable outcomes,
% while nondeterministic behavior is acknowledged for operations whose results may vary.
property('call!', 'Deterministic').
property('call-fn!', 'Nondeterministic').
property('!', 'Deterministic').

% Data Structures and Manipulation:
% The definition, organization, and manipulation of data structures are foundational aspects of programming.
% These operations facilitate the storage, retrieval, and modification of data in structured forms.
property('Cons', data_structures).             % Constructs a pair or list.
property('collapse', data_manipulation).       % Flattens nested structures.
property('superpose', data_manipulation).      % Overlays data structures.
property('sequential', data_manipulation).     % Ensures sequential execution.
property('TupleConcat', data_structures).      % Concatenates tuples.
% Operations on data structures are generally deterministic, yielding predictable outcomes based on the inputs and operations.
property(P, data_manipulation) ==> property(P, 'Deterministic').

% Logic and Comparison:
% Logical and comparison operations are fundamental in programming, enabling decision making
% and data comparison. This includes basic logical operations and comparisons between values.
property('and', logic_comparison).
property('or', logic_comparison).
property('not', logic_comparison).
% Logical operations result in deterministic outcomes, directly derived from their input values.
property(P, logic_comparison) ==> property(P, 'Deterministic').

% Additional and Miscellaneous:
% This section covers a variety of functionalities not classified under the previous categories.
% It includes system interaction, functional programming utilities, arithmetic operations,
% and more, providing a wide range of capabilities.
property('atom-replace', data_manipulation).
property('fb-member', list_operations).
property('nop', control_structure).
property('empty', data_validation).
property('function', functional_programming).
property('return', functional_programming).
property('number-of', quantitative_analysis).
property('new-space', system_interaction).
property('bind!', system_interaction).
property('pragma!', system_interaction).
property('transfer!', system_interaction).
property('registered-python-function', interlanguage_integration).
property('S', symbolic_arithmetic).
property('Z', symbolic_arithmetic).
property('bc-base', recursion_control).
property('bc-base-ground', recursion_control).
property('bc-rec', recursion_control).

% Rules for Automatic Property Inference:
% These rules allow for automatic inference of certain properties based on categories,
% simplifying the property assignment process and ensuring consistency.
property('function', 'VariableArity').
property('return', 'Deterministic').
property(P, system_interaction) ==> property(P, 'Deterministic').
property('fb-member', 'Nondeterministic').
property(P, symbolic_arithmetic) ==> property(P, 'Deterministic').
property(P, recursion_control) ==> property(P, 'Deterministic').
property('bc-rec', 'Nondeterministic').


% Evaluation and Execution:
% Evaluation and execution properties pertain to how expressions, commands, or functions are processed and run.
% This includes interpreting code, printing output, and compiling expressions.
% 'eval' allows for the execution of dynamically constructed code, which could lead to nondeterministic outcomes
% depending on the runtime environment and input data.
property('eval', 'Nondeterministic').
property('eval-for', evaluation_execution).
property('echo', evaluation_execution).
property('print', evaluation_execution).
property('println!', evaluation_execution).
property('compile-easy!', evaluation_execution).
property('time!', evaluation_execution).
% The 'eval' operation could lead to different outcomes based on the input, thus considered nondeterministic.
property('eval', 'Nondeterministic').
% Conversely, 'echo' simply reflects its input without alteration, making it deterministic.
property('echo', 'Deterministic').

% Enhanced System Interaction and Dynamic Execution:
% Dynamic execution features and enhanced system interaction capabilities extend Prolog's utility,
% enabling runtime evaluation of code and interaction with the system or external environments.
% 'call-string!' executes Prolog code provided as a string, potentially introducing nondeterminism
% based on the dynamic nature of the executed code and external state.
property('call-string!', external_integration).
% 'call-string!' allows for dynamic execution of Prolog code provided as a string,
% which might be nondeterministic depending on the runtime environment and the code being executed.
property('call-string!', 'Nondeterministic').
% Registering and invoking Python functions from Prolog illustrates interlanguage integration,
% enabling deterministic interoperability with Python codebases.
property('registered-python-function', 'Deterministic').
% Error Handling and Advanced Control Flow Mechanisms:
% Proper error handling is crucial for robust programs, allowing for graceful recovery
% from unexpected states or inputs. Advanced control flow mechanisms provide more complex
% patterns of execution beyond simple conditional checks and loops.
property('catch', error_handling_advanced).
property('throw', error_handling_advanced).
% Error handling operations like 'catch' and 'throw' can influence the control flow based on runtime conditions,
% potentially introducing nondeterminism if the error states or exceptions are not predictable.
property('catch', 'Nondeterministic').
property('throw', 'Nondeterministic').

% Arithmetic and Logical Operations:
% Arithmetic operations form the basis of mathematical computations in programming,
% including basic operations like addition, subtraction, multiplication, and division.
property('+', arithmetic_operations).
property('-', arithmetic_operations).
property('*', arithmetic_operations).
property('mod', arithmetic_operations).
% These operations are deterministic, yielding specific results from given numeric inputs.
property(P, arithmetic_operations) ==> property(P, 'Deterministic').

% List Operations and Data Validation:
% Operations on lists and validation of data are fundamental in many programming tasks,
% allowing for the manipulation, examination, and assurance of data integrity.
property('fb-member', list_operations).
% 'fb-member' checks for membership in a list, which could have nondeterministic outcomes based on list contents.
property('fb-member', 'Nondeterministic').
property('nop', control_structure).
% 'nop' represents a no-operation, effectively serving as a placeholder or for timing.
property('nop', 'Deterministic').
property('empty', data_validation).
% 'empty' checks for or represents an empty structure or condition, a deterministic operation.
property('empty', 'Deterministic').

% Advanced List Operations and Utilities:
% Advanced operations on lists and utility functions provide powerful mechanisms for data manipulation and analysis,
% extending the core capabilities for handling lists and collections.
property('dedup!', list_utilities).
% 'dedup!' removes duplicate elements from a list, providing a deterministic way to ensure unique elements.
property('dedup!', 'Deterministic').

% Arithmetic and Logic Enhancements:
% Enhancements to arithmetic and logic functionalities support more complex mathematical operations and logical reasoning,
% broadening the scope of computational tasks that can be addressed.
property('hyperpose', arithmetic_enhancements).
% 'hyperpose' could be involved in advanced arithmetic or matrix operations, assumed to be deterministic
% for well-defined mathematical transformations.
property('hyperpose', 'Deterministic').

% Functional Programming Enhancements:
% Enhancements and utilities for functional programming emphasize the use of functions as first-class citizens,
% promoting immutability, statelessness, and higher-order functions for more declarative programming approaches.
property('maplist!', functional_enhancements).
% 'maplist!' applies a function to each element of a list in a deterministic manner, preserving list structure.
property('maplist!', 'Deterministic').
property('concurrent-maplist!', functional_programming).
% 'concurrent-maplist!' might introduce nondeterminism due to concurrent execution.
property('concurrent-maplist!', 'Nondeterministic').


% Interactivity and Debugging Tools:
% Tools and functionalities that facilitate interactivity with the user or debugging capabilities
% enhance the development experience by providing insights into program execution and allowing for real-time interaction.
property('trace!', debugging_tools).
property('notrace!', debugging_tools).
property('rtrace!', debugging_tools).
% Debugging commands like 'trace!', 'notrace!', and 'rtrace!' offer deterministic control over tracing and debugging states,
% allowing developers to enable or disable debugging modes as needed.
property(P, debugging_tools) ==> property(P, 'Deterministic').

% Quantitative Analysis and Symbolic Representation:
% Quantitative analysis involves operations that measure or quantify aspects of data,
% while symbolic representation deals with abstract symbols rather than explicit values.
property('number-of', quantitative_analysis).
% 'number-of' provides a count or measure, yielding deterministic results.
property('number-of', 'Deterministic').
property('S', symbolic_arithmetic).
property('Z', symbolic_arithmetic).
% 'S' (successor) and 'Z' (zero) are used in Peano arithmetic, representing numbers symbolically.
property('S', 'Deterministic').
property('Z', 'Deterministic').

