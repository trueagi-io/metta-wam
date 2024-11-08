/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming functional/logic programs. It handles different
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
 */

:- encoding(iso_latin_1). % Set encoding to iso_latin_1, ensuring text is handled appropriately.
:- flush_output. % Flushes output after every write to ensure it is printed immediately.
:- setenv('RUST_BACKTRACE', full). % Sets the RUST_BACKTRACE environment variable to 'full' for detailed debugging.
:- op(700, xfx, '=~'). % Defines a new infix operator '=~' with precedence 700, used later in custom logic.
:- ensure_loaded(metta_interp). % Loads the metta_interp file to make its predicates available.

/* previously: This block contained directives setting the environment and preparing the interpreter,
   ensuring that debugging tools, character encoding, and necessary files are in place. */

% ===============================
%    TESTING
% ===============================

/**
 * fb/0
 * 
 * The predicate `fb/0` compiles the program and iterates over all clauses
 * of `fb0`, printing the goals and calling them.
 * 
 * @example
 * ?- fb.
 * ;; Output of fb0 clauses and execution results will be printed.
 */
fb :-
    make,  % Calls 'make', which compiles the program or updates files.
    writeln(';; ==========================================='),  % Prints separator line for readability.
    forall(
        (clause(fb0, Goal),  % Finds all clauses for the predicate 'fb0' and binds 'Goal' to their body.
         write(';; '), writeq(?- Goal), nl,  % Prints the goal in a readable format.
         call(Goal)  % Calls (executes) the goal.
        ),
        writeln(';; ===========================================')  % Prints another separator.
    ).

/**
 * fb0/0
 *
 * Defines mettalog rules for the transpiler, showing MeTTa sources with specific examples.
 * The clauses here provide a glimpse into the transpiler logic, like `two_pi` and `factorial_tail_basic`.
 *
 * @example
 * ?- fb0.
 * ;; Specific examples like two_pi and factorial_tail_basic will be processed.
 */
fb0 :- show_mettalog_src((two_pi(R) :- (pi(A), +(A, A, B), R is B))).  % Displays source for calculating two_pi using pi.
fb0 :- show_mettalog_src(factorial_tail_basic).  % Displays source for factorial tail recursion.
fb0 :- show_mettalog_src(funct).  % Displays function-specific mettalog source.

/* previously: fb0 clauses were simply defined as individual rules, without clear comments on the purpose of the examples shown. */

/**
 * print_metta_src/0
 *
 * Calls `show_mettalog_src/0` to print all available MeTTa source code.
 */
print_metta_src :- show_mettalog_src.  % Calls the main show_mettalog_src predicate to display all relevant source files.

/**
 * show_mettalog_src/0
 *
 * Compiles the program and iterates over all source files containing 'metta'.
 * For each such file, it shows the associated predicate by invoking `show_mettalog_src/1`.
 *
 * @example
 * ?- show_mettalog_src.
 * ;; Shows all MeTTa source files and their Prolog equivalents.
 */
show_mettalog_src :-
    make,  % Compiles the program.
    forall(
        (source_file(AsPred, File),  % Finds all source files and binds the predicate and file name.
         symbol_contains(File, metta)  % Checks if the file contains the string 'metta'.
        ),
        show_mettalog_src(AsPred)  % Calls show_mettalog_src for the predicate found in each MeTTa-related file.
    ).

/* previously: This section aimed to compile and show sources but lacked detailed explanations on the file checking mechanism. */

/**
 * show_mettalog_src/1
 * 
 * Handles different cases for displaying source code based on the input parameters.
 * If the input is of the form F/A (functor/arity), it checks all current predicates with this arity.
 *
 * @param F The functor of the predicate.
 * @param A The arity of the predicate.
 * 
 * @example
 * ?- show_mettalog_src(two_pi/1).
 * ;; Displays the source for the two_pi/1 predicate.
 */
show_mettalog_src(F/A) :-
    nonvar(F),  % Ensure that F is instantiated (not a variable).
    !,  % Cut to prevent backtracking after nonvar check.
    forall(current_predicate(F/A), show_mettalog_src(F, A)).  % For all predicates with functor F and arity A, display their source.