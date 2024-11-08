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