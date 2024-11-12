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