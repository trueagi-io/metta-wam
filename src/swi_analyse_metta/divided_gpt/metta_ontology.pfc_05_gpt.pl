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