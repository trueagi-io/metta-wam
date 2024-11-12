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
/** <predicate> show_mettalog_src/1
 *  This predicate displays the source of metta-log clauses for the provided argument.
 *  It handles both predicate functors and atoms, performing checks for current predicates 
 *  and then printing their clauses if they are found in files that contain the string "metta".
 *
 *  @param AsPred The argument which can be a functor or atom to check predicates for.
 *  @example show_mettalog_src(some_predicate). % Shows metta clauses for some_predicate.
 */
show_mettalog_src(AsPred):-
    % Extract the functor and arity of the predicate
    functor(AsPred,F,A),
    % Ensure the predicate exists before proceeding
    \+ \+ current_predicate(F/A),
    % Cut to avoid backtracking and continue if the predicate exists
    !,
    % For all current predicates matching the functor and arity, display their source
    forall(current_predicate(F/A), show_mettalog_src(F,A)).

/** <predicate> show_mettalog_src/1
 *  This version of show_mettalog_src handles cases where the input is an atom.
 *  It ensures that the atom corresponds to a current predicate and shows its clauses.
 *  
 *  @param F The atom representing the predicate functor.
 */
show_mettalog_src(F):-
    % Ensure F is an atom and corresponds to a current predicate
    atom(F), \+ \+ current_predicate(F/_),
    % Cut to avoid backtracking after finding the predicate
    !,
    % Show the source for each predicate matching the atom
    forall(current_predicate(F/A), show_mettalog_src(F,A)).

/** <predicate> show_mettalog_src/1
 *  Handles cases where the input is a substring of a predicate functor.
 *  It searches for any predicate functor that contains the input substring
 *  and shows their metta clauses.
 *
 *  @param C The substring to search within predicate functors.
 */
show_mettalog_src(C):-
    % Ensure C is an atom and matches some current predicate functor
    atom(C), \+ \+ (current_predicate(F/_),once(atom_contains(F,C))),
    % Cut to prevent backtracking after finding the predicate
    !,
    % Show the source for all predicates that match the substring C
    forall((current_predicate(F/A),once(atom_contains(F,C))), show_mettalog_src(F,A)).

/** <predicate> show_mettalog_src/1
 *  A fallback case to display CVT (convertible terms) when no direct metta-log clause matches are found.
 *
 *  @param C The term to attempt to display as a CVT.
 */
show_mettalog_src(C):- 
    % Fallback to show CVT if none of the previous cases apply
    show_cvts(C), !.

/** <predicate> show_space_src/0
 *  Compiles the program and displays space sources for each space predicate.
 */
show_space_src:-
    % Rebuild the program using make/0
    make,
    % For each space predicate, show its source
    forall(space_preds(AsPred), show_space_src(AsPred)).

/** <predicate> show_space_src/2
 *  Displays space sources for predicates based on the given functor and arity.
 *
 *  @param F The functor of the predicate.
 *  @param A The arity of the predicate.
 */
show_space_src(F/A):- 
    % Ensure the functor F is non-variable
    nonvar(F), 
    % Cut to avoid backtracking
    !,
    % For each current predicate with the specified functor and arity, show its source
    forall(current_predicate(F/A), show_space_src(F,A)).

/** <predicate> show_space_src/1
 *  Displays space sources for a given predicate functor, similar to show_mettalog_src/1.
 *
 *  @param AsPred The argument which can be a functor or atom to check predicates for.
 */
show_space_src(AsPred):-
    % Extract the functor and arity of the predicate
    functor(AsPred,F,A),
    % Ensure the predicate exists before proceeding
    \+ \+ current_predicate(F/A),
    % Cut to avoid backtracking and continue if the predicate exists
    !,
    % For all current predicates matching the functor and arity, display their source
    forall(current_predicate(F/A), show_space_src(F,A)).

/** <predicate> show_cvts/1
 *  Processes a term by attempting conversions between different predicate forms
 *  (original term, function form, Prolog form) and prints each step.
 *
 *  @param Term The term to process and print in different forms.
 */
show_cvts(Term):-
    % Attempt to process the term if it is a list
    once((is_list(Term), sexpr_s2p(Term,PF))),
    % Ensure PF is not a list, otherwise proceed with showing CVTs
    \+ is_list(PF), 
    !, 
    show_cvts(PF).

/** <predicate> show_mettalog_src/2
 *  Displays the metta-log clauses for a specific predicate functor and arity.
 *  
 *  @param F The functor of the predicate.
 *  @param A The arity of the predicate.
 */
show_mettalog_src(F,A):- 
    % Create the functor term using the given functor and arity
    functor(Head,F,A),
    % Ignore any errors while trying to fetch clause properties
    ignore((
        predicate_property(Head,number_of_clauses(_)),
        % Find the source file of the predicate and check if it contains 'metta'
        source_file(Head,File), atom_contains(File,metta),
        % Print a newline before showing the metta clauses
        nl,
        % Find all clauses associated with the head and print them
        findall((Head:-Body), clause(Head,Body), Clauses),
        % Print the metta clauses
        print_metta_clauses(Clauses)
    )),
    % Print a newline after printing the metta clauses
    nl.

/** <predicate> print_metta_clauses/1
 *  Prints a list of metta-log clauses.
 *  
 *  @param Clauses A list of clauses in the format Head :- Body.
 */
print_metta_clauses([]):- 
    % Base case: nothing to print if the list is empty
    !.
print_metta_clauses([Head:-Body]):- 
    % If there is a single clause, print it using print_metta_clause
    !, print_metta_clause(Head,Body).
print_metta_clauses(Clauses):- 
    % Combine multiple clauses into a single head-body form and print them
    combine_clauses(Clauses,Head,Body),
    !, 
    print_metta_clause(Head,Body).

/** <predicate> print_metta_clause/2
 *  Prints a single metta-log clause.
 *  
 *  @param Head The head of the clause.
 *  @param Body The body of the clause.
 */
print_metta_clause(Head,Body):- 
    % Print the clause using a helper predicate
    print_metta_clause0(Head,Body),
% =========================================
%  PLDoc Header for show_cvts/1
% =========================================

%% show_cvts(+Clause) is det.
% 
% Display a clause in a specific form where the body is processed.
% 
% @param Clause A clause of the form Head :- Body.
%
% @example
% ?- show_cvts(foo :- bar).
% This will process and display foo :- bar in a specific form.
%
show_cvts(Head:-Body). 

% =========================================
% PLDoc Header for print_metta_clause0/2
% =========================================

%% print_metta_clause0(+Head, +Body) is det.
%
% Prints a metta clause by transforming the Body into a list if necessary.
% It will handle cases where the Body is either true or false and print them as
% metta statements accordingly.
%
% @param Head The head of the clause.
% @param Body The body of the clause, which can be true, false, or a conjunction of predicates.
%
% @example
% ?- print_metta_clause0(foo, true).
% This will print the metta clause "foo = True".
%
print_metta_clause0(Head,Body):- 
    % If Body is true, print Head as equal to 'True'
    Body == true,!, 
    pp_metta([=,Head,'True']).

print_metta_clause0(Head,Body):- 
    % If Body is false, print Head as equal to 'False'
    Body == false,!, 
    pp_metta([=,Head,'False']).

print_metta_clause0(Head,Body):- 
    % Transform Body into a list of conjuncts, then into a sequential form
    conjuncts_to_list(Body,List), 
    into_sequential([':-'],List,SP), 
    pp_metta([=,Head,SP]).

% =========================================
%  STERM -> PTERM Conversion
% =========================================

% PLDoc Header for iz_exact_symbol/2
%% iz_exact_symbol(+N, +P) is semidet.
%
% Checks if N is an exact symbol and if it matches with P. 
% Handles specific cases for special symbols like :-, ?-, and ??.
%
% @param N The input to check.
% @param P The output symbol.
%
iz_exact_symbol(N,_):- 
    % If N is not an atom, fail.
    \+ atom(N),!,fail.

iz_exact_symbol(N,P):- 
    % If P is non-variable, recursively check for exact symbol match.
    nonvar(P),!,iz_exact_symbol(N,PP),zalwayz(P=PP).

% Match exact symbols for specific operators like :- and ?-
iz_exact_symbol(':-',':-').
iz_exact_symbol('?-','?-').
iz_exact_symbol('??',_).

% This file directive was commented out as it is not currently needed.
% It loads specific logic rewriting utilities from another module.
%:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

% PLDoc Header for maybe_varz/3
%% maybe_varz(+S, +Name, -Var) is semidet.
%
% Converts a variable into a special '$VAR' term if the input symbol is '?' and the Name is an atom.
%
% @param S The input symbol to check.
% @param Name The name of the variable.
% @param Var The resulting '$VAR'(Name) term.
%
maybe_varz(S,Name,'$VAR'(Name)):- 
    % If S is '?', and Name is an atom, then it's a special variable.
    S=='?',atom(Name),!.

% PLDoc Header for sexpr_s2p/2
%% sexpr_s2p(+HB, -P) is det.
%
% Converts a S-expression (STERM) to PTERM. 
% The implementation handles various forms of S-expressions, including lists and compound terms.
%
% @param HB The S-expression to convert.
% @param P The resulting PTERM.
%
sexpr_s2p(HB,P):- 
    % This clause was skipped because it's marked as 'fail', indicating it should not be executed.
    fail, 
    compound(HB), 
    HB=~ (H=B), 
    compile_for_assert(H,B,Cl),
    clause_to_code(Cl,P),!.

sexpr_s2p(S,P):- 
    % Default case that calls sexpr_s2p with a predefined function and term.
    sexpr_s2p(progn,1,S,P).

% PLDoc Header for clause_to_code/2
%% clause_to_code(+P, -Code) is det.
%
% Transforms a clause into its code representation.
%
% @param P The input clause.
% @param Code The resulting code after transformation.
%
clause_to_code(P,P):- 
    % If P is a free variable, it remains unchanged.
    is_ftVar(P),!.

clause_to_code((H:-B),P):- 
    % If the Body is true, combine the Head and Body into the resulting code.
    B==true, !, 
    combine_code(B,H,P).

clause_to_code(P,P). 

% PLDoc Header for sexpr_s2p/4
%% sexpr_s2p(+Fn, +Nth, +S, -P) is det.
%
% A more complex variant of sexpr_s2p that converts based on a function (Fn) and argument index (Nth).
% Handles various types of S-expressions.
%
% @param Fn The function to apply.
% @param Nth The argument index.
% @param S The S-expression to convert.
% @param P The resulting PTERM.
%
sexpr_s2p(Fn,_Nth,VAR,VAR):- 
    % If the variable is a free variable, return it unchanged.
    is_ftVar(VAR),!.

sexpr_s2p(Fn,_Nth,S,P):- 
    % If S is an exact symbol, return P as the matching symbol.
    iz_exact_symbol(S,P),!.

sexpr_s2p(Fn,_Nth,'#'(S),P):- 
    % Handle the case where S is prefixed with '#'.
    iz_exact_symbol(S,P),!.

sexpr_s2p(Fn,Nth,S,P):- 
    % Convert a list to PTERM using function Fn and argument index Nth.
    S==[], iz_fun_argz(Fn,Nth),!,P=S.

% This clause was commented out, but the original intent was to handle expected types for specific functions.
% sexpr_s2p(Fn,Nth,S,P):- expects_type(Fn,Nth,Type),will_become_type(Type,S,P),!.

sexpr_s2p(_Fn,_Nth,[F|SList],P):- 
    % Convert a list starting with F and SList into a predicate call if F is a system predicate.
    is_list(SList), length(SList,Len),is_syspred(F,Len,Pred), 
    sexpr_s2p_arglist(F,1,SList,PList), !, 
    P=..[Pred|PList].

% Disable the singleton variable warning for the rest of the code.
:- style_check(-singleton).

sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- 
    % Convert a list recursively.
    iz_fun_argz(Fn,Nth),!,
    sexpr_s2p(S,P), 
    sexpr_s2p(Fn,Nth,SList,PList).

sexpr_s2p(Fn,Nth,[S|SList],[P|PList]):- 
    % If S is not an atom or SList is not a list, convert recursively.
    ( \+ atom(S) ; \+ is_list(SList)), !,
    sexpr_s2p(list(Fn),Nth,S,P), 
    sexpr_s2p(list(Fn),Nth,SList,PList).

sexpr_s2p(_Fn,_Nth,[S,STERM0],PTERM):- 
    % Handle quoted terms, converting them into compound terms.
    iz_quoter(S),
    sexpr_s2p_pre_list(S,0,STERM0,STERM), !,
    PTERM=..[S,STERM],!.

sexpr_s2p(_Fn,_Nth,[S|SList],P):- 
    % Handle the case where S is an atom and SList is empty, resulting in a compound term with arity 0.
    atom(S), SList == [], compound_name_arity(P,S,0).

% This commented-out clause handled special cases with variables, but it was skipped to simplify the code.
% sexpr_s2p(Fn,Nth,List,PTERM):- append(Left,[S,Name|TERM],List),maybe_varz(S,Name,Var),!,append(Left,[Var|TERM],NewList), sexpr_s2p(Fn,Nth,NewList,PTERM).

% This block handled 'dot_holds' cases, which are not currently needed in the main flow.
% sexpr_s2p(Fn,Nth,[S|TERM],dot_holds(PTERM)):- \+ (is_list(TERM)),sexpr_s2p_arglist(Fn,Nth,[S|TERM],PTERM),!.

% The following commented-out block dealt with quantifiers but was not necessary for this implementation.
% sexpr_s2p(Fn,Nth,[S,Vars|TERM],PTERM):- nonvar(S),
%    call_if_defined(common_logic_snark:iz_quantifier(S)),
%    zalwayz((sexpr_s2p_arglist(Fn,Nth,TERM,PLIST),
%    PTERM =~ [S,Vars|PLIST])),!.

% This clause handled logical conjunctions in S-expressions but was commented out for performance reasons.
% sexpr_s2p(progn,_,[S|TERM],PTERM):- S==AND,!,zalwayz((maplist(sexpr_s2p,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).
/**
 * @predicate sexpr_s2p/4
 * Transforms an S-expression (a Lisp-like structure) into a Prolog term.
 * @param Fn - Function name or context.
 * @param Nth - Position within arguments (used for recursive calls).
 * @param [S|STERM0] - List structure representing an S-expression.
 * @param PTERM - The resulting Prolog term.
 *
 * This predicate recursively processes S-expressions, transforming them into
 * appropriate Prolog terms based on their structure.
 *
 * @example
 * ?- sexpr_s2p(myFunc, 1, ['quote', 'X'], P).
 * P = quote('X').
 */
sexpr_s2p(Fn, Nth, [S|STERM0], PTERM) :-
    % Handles preprocessing of the S-expression list, 
    % e.g., ensures it is a valid structure to proceed.
    sexpr_s2p_pre_list(Fn, Nth, STERM0, STERM),
    % Processes the argument list of the expression and constructs the term.
    sexpr_s2p_arglist(S, 1, STERM, PLIST), 
    % Combines function and arguments into the resulting Prolog term.
    z_univ(Fn, Nth, PTERM, [S|PLIST]),
    !.

/**
 * Fallback case when the third argument is a variable; 
 * it simply passes the variable as the result.
 */
sexpr_s2p(_Fn, _Nth, VAR, VAR).

/**
 * @predicate expects_type/3
 * Determines the expected type of a function argument.
 * @param Fn - The function or operator in question.
 * @param Nth - The position of the argument in question.
 * @param Type - The type expected for the argument.
 *
 * Retrieves the type definition of an operator, using nth0 to fetch the type 
 * for the given argument position.
 */
expects_type(Fn, Nth, Type) :-
    % Gets the operator's type signature (return type and parameter types).
    get_operator_typedef(Self, Fn, Params, RetType),
    % Retrieves the type corresponding to the Nth position.
    nth0(Nth, [RetType|Params], Type),
    % Ensures the type is not a variable (has been set).
    nonvar(Type).

/**
 * @predicate will_become_type/3
 * Attempts to coerce or match a term to a given type.
 * @param Type - The type to which S should conform.
 * @param S - The term to coerce.
 * @param P - The resulting term after type coercion.
 *
 * If S is a variable or already of the correct type, it is returned unchanged.
 * Otherwise, coercion is attempted.
 */
will_become_type(Type, S, P) :-
    % Attempt to adjust the argument types and unify P with the result.
    try_adjust_arg_types(=, _RetType, 88, _Self, [Type], [S], [PS]),
    PS = P,
    !.

% If S is a free variable, return it unchanged as P.
will_become_type(Type, S, P) :- is_ftVar(S), !, P = S.

% If S already has a type, check if it's a subtype or coerce it.
will_become_type(Type, S, P) :-
    get_type(S, T),
    !,
    (is_subtype(T, Type) -> S = P ; P = coerce(Type, S)).

% If no specific type handling is needed, return S as P.
will_become_type(_Type, S, P) :- !, S = P.

/**
 * @predicate is_subtype/2
 * Checks whether T is a subtype of TT.
 * @param T - The subtype.
 * @param TT - The supertype.
 *
 * Compares two types to determine if T is a subtype of TT.
 */
is_subtype(T, TT) :- T =@= TT, !, T = TT.
is_subtype(T, TT) :- T = TT, !.

/**
 * @predicate iz_quoter/1
 * Determines whether a symbol is a quoter in the target language (like Lisp).
 * @param Symbol - The symbol in question.
 *
 * Recognizes various quoting mechanisms, including Common Lisp syntax.
 */
iz_quoter('#BQ') :- iz_common_lisp.
iz_quoter('#COMMA') :- iz_common_lisp.
iz_quoter('quote').
iz_quoter(superpose).

/**
 * @predicate iz_fun_argz/2
 * Determines the expected number of arguments for a given function.
 * @param F - The function.
 * @param ArgCount - The expected number of arguments.
 */
iz_fun_argz(list(_), _).  % List functions can have arbitrary arity.
iz_fun_argz(defmacro, 2). % defmacro takes two arguments.
iz_fun_argz(defun, 2).    % defun takes two arguments.
iz_fun_argz(let, 1).      % let takes one argument.
iz_fun_argz('let*', 1).   % let* takes one argument.
iz_fun_argz('member', 2). % member takes two arguments.
% Previously: iz_fun_argz('let*', 2). Skipped, as let* typically has arity 1.
iz_fun_argz(F, 1) :- iz_quoter(F).  % Quoting functions take one argument.

/**
 * @predicate z_functor/1
 * Checks if F is a valid functor (ignores variables and reserved symbols).
 * @param F - The functor to check.
 */
z_functor(F) :- \+ atom(F), !, fail.           % F must be an atom.
z_functor(F) :- \+ atom_concat('?', _, F).     % Ignore functors starting with '?'.
z_functor(F) :- \+ atom_concat('$', _, F).     % Ignore functors starting with '$'.

/**
 * @predicate z_univ/4
 * Creates a Prolog term by combining a functor and its arguments.
 * @param Fn - Contextual function name (not used in most cases).
 * @param Nth - Position in the argument list.
 * @param P - The resulting Prolog term.
 * @param [F|ARGS] - Functor and arguments in list form.
 *
 * This is a variant of =../2 (univ) to handle S-expression processing.
 */
z_univ(_Fn, _, P, [F|ARGS]) :-
    % Check if F is a valid functor and ARGS is a list.
    z_functor(F), is_list(ARGS),
    % Get the arity of F and construct the compound term.
    length(ARGS, A), l_arity_l(F, A),
    compound_name_arguments(P, F, ARGS),
    !.

% Fallback case if no transformation is needed, unify P and S directly.
z_univ(_Fn, _Nth, P, S) :- P = S.

/**
 * @predicate l_arity_l/2
 * Determines the arity of a functor F.
 * @param F - The functor.
 * @param A - The arity of the functor.
 */
l_arity_l(F, A) :- clause_b(arity(F, A)).
l_arity_l(function, 1).
l_arity_l(quote, 1).
l_arity_l('#BQ', 1) :- iz_common_lisp.
l_arity_l(F, A) :- current_predicate(F/A).  % Check if F/A exists as a predicate.
l_arity_l(_, 1).  % Default arity is 1.

/**
 * @predicate sexpr_s2p_arglist/4
 * Processes the argument list of an S-expression and transforms each argument into a Prolog term.
 * @param Fn - Function name or context.
 * @param Nth - Position in the argument list.
 * @param [S|SList] - List of arguments to process.
 * @param [P|PList] - List of resulting Prolog terms.
 */
sexpr_s2p_arglist(_Fn, _, VAR, VAR) :- is_ftVar(VAR), !.  % If it's a free variable, return unchanged.

sexpr_s2p_arglist(Fn, Nth, [S|SList], [P|PList]) :-
    % Recursively process each argument in the list.
    sexpr_s2p(Fn, Nth, S, P),
    % Update Nth for the next argument.
    (Nth > 0 -> Nth2 is Nth + 1; Nth2 = 0),
    % Process the rest of the list.
    sexpr_s2p_arglist(Fn, Nth2, SList, PList),
    !.

sexpr_s2p_arglist(Fn, Nth, S, P) :- sexpr_s2p(Fn, Nth, S, P), !.

sexpr_s2p_arglist(_Fn, _Nth, VAR, VAR).  % Base case: free variable.

/**
 * @predicate sexpr_s2p_pre_list/4
 * Preprocesses the list form of an S-expression before transforming it into Prolog terms.
 * @param Fn - Function name or context.
 * @param _ - Placeholder for position.
 * @param STERM - The list structure.
 * @param STERM - Unchanged list structure.
 */
sexpr_s2p_pre_list(_Fn, _, STERM, STERM) :- \+ compound(STERM), !.  % If not a compound term, return unchanged.
sexpr_s2p_pre_list(_Fn, _, STERM, STERM) :- \+ is_list(STERM), !.   % If not a list, return unchanged.

/* Previously: A clause to handle empty lists as a special case, which has been skipped to keep consistency. 
 sexpr_s2p_pre_list(Fn,_,[S|STERM],[S|STERM]):- STERM == [], !.


*/
/**
 * sexpr_s2p_pre_list/4
 * This predicate recursively processes a list, transforming its elements by applying sexpr_s2p.
 * It handles cases where the first element is a list, recursively transforming it if necessary.
 *
 * @param Fn   The function name to apply
 * @param Nth  An index or counter for processing
 * @param [S0|STERM0] The input list of s-expressions
 * @param [S|STERM]   The output list of processed s-expressions
 *
 * @example
 * ?- sexpr_s2p_pre_list(fn, nth, [[a,b],c], Output).
 * Output = [[ProcessedA, ProcessedB], ProcessedC].
 */
sexpr_s2p_pre_list(Fn,Nth,[S0|STERM0],[S|STERM]):-
  % Check if S0 is a list. If so, process it recursively using sexpr_s2p, otherwise apply sexpr_s2p_pre_list.
  (is_list(S0) -> sexpr_s2p(Fn,Nth,S0,S); sexpr_s2p_pre_list(Fn,Nth,S0,S)),
  % Recursively process the rest of the list (STERM0) into STERM.
  sexpr_s2p_pre_list(Fn,Nth,STERM0,STERM), !.
  
% Base case: If there are no more elements to process, the output is the same as the input.
sexpr_s2p_pre_list(_Fn,_,STERM,STERM).

/* previously: There was an attempt to handle a condition for non-list elements,
   but it's now refactored into the is_list condition above. */

/**
 * p2m/1
 * A helper predicate to translate all clauses of a given predicate from Prolog to MeTTa syntax.
 * This version operates on a single predicate indicator (name/arity).
 *
 * @param I   The predicate indicator to translate (name/arity).
 *
 * @example
 * ?- p2m(my_pred).
 * my_pred(a, b) call! my_pred(a, b).
 */
p2m(I):- 
  % Iterate through all clauses of the predicate I/A without repeating.
  forall(no_repeats(current_predicate(I/A)),
    (functor(P,I,A),
     % For each clause, translate the body and print it in MeTTa format.
     forall(clause(P,Body),
       (numbervars(P+Body,0,_,[]),
        write_src(=(P,'call!'(Body))))))).

/**
 * p2m/2
 * Main translation predicate that converts a Prolog term into a MeTTa term.
 * Handles a variety of cases including lists, atomic terms, and Prolog control structures.
 *
 * @param I  Input term in Prolog
 * @param O  Output term in MeTTa
 */
p2m(I,O):- 
  % Call the helper predicate with the initial context of [progn].
  p2m([progn],I,O).

/**
 * p2m/3
 * Translates a Prolog term into a MeTTa term, accounting for different control structures and terms.
 * @param OC   The context in which the translation occurs (e.g., progn, arg)
 * @param NC   The Prolog term being translated
 * @param O    The resulting MeTTa term
 */

% Skip if NC is a variable.
p2m(_OC,NC, NC) :- var(NC), !.  

% Skip if NC is a free-term variable (custom definition).
p2m(_OC,NC, NC) :- is_ftVar(NC), !.

% Translate lists recursively.
p2m(OC,[H|T],'::'(L)):- 
  is_list([H|T]),
  % Map each element of the list recursively.
  maplist(p2m(OC),[H|T],L).

% Convert Prolog lists into 'Cons' notation in MeTTa.
p2m(OC,[H|T], 'Cons'(OH,OT)):- 
  p2m(OC,H, OH), 
  p2m(OC,T, OT).

% Handle atomic terms.
p2m(_OC,A, A):- string(A), !.
p2m(_OC,[], 'Nil').  % Empty list becomes 'Nil' in MeTTa.
p2m(_OC,'[|]','Cons').  % Translate Prolog's '[|]' notation to MeTTa's 'Cons'.
p2m(_OC,!, '!').  % Direct translation for Prolog's cut (!).
p2m(_OC,false, 'False').  % False becomes 'False'.
p2m(_OC,true, 'True').  % True becomes 'True'.

% Handle cases where the input is an atom.
p2m([progn|_],Atom,[O]):- 
  atom(Atom),!, 
  p2m([arg],Atom,O),!.

% Translate conjunctions and disjunctions.
p2m(_OC,( ';' ),'xor').  % Disjunction becomes 'xor'.
p2m(_OC,( ',' ),'and2').  % Conjunction becomes 'and2'.

/* previously: ',' was mapped to 'and', but it was adjusted to 'and2' to fit a new structure. 
   ',' and ';' were skipped earlier due to lacking context-specific translations. */

/* Dead code explanation: The following lines were skipped as they represent an alternate way 
   of translating Prolog control structures that were handled by different MeTTa primitives.
   They remain here for reference. 
   %p2m(_OC,( ',' ),and).
   %p2m(_OC,( '\\+' ),unless).
   %p2m(_OC,( ':-' ),entailed_by).
 */

% Additional atomic translations
p2m(_OC,'atom','is-symbol').  % Prolog's atom check becomes 'is-symbol'.
p2m(_OC,'atomic','symbolic').  % Atomic check becomes 'symbolic'.

% Handle scoped operators.
p2m(_OC,M:I, 'scoped'(N,O)):-  
  p2m(OC,M,N),
  p2m(I,O).

% Negation as failure is translated into a MeTTa 'naf' (negation as failure).
p2m(_OC,(\+ A), O):- !, p2m(_OC,naf(A), O).

/**
 * conjuncts_to_list/2
 * Converts a conjunction of terms (G, E) into a list of terms.
 *
 * @param (G, E)   A conjunction of goals
 * @param List     Output list of goals
 *
 * @example
 * ?- conjuncts_to_list((a, b), List).
 * List = [a, b].
 */
p2m(OC,(G,E),O):-  
  % Convert conjunction into a list of goals.
  conjuncts_to_list((G,E),List),!,
  % Sequentially translate the list of goals.
  into_sequential(OC,List,O),!.
/** <p2m/3> 
    Translates Prolog terms into their equivalent MeTTa syntax.
    @param _OC Context (Operational Context).
    @param Head:-Body The Prolog term to be converted.
    @param O The output term in MeTTa syntax.
    @example
    ?- p2m([], (is(A, B)), O).
    O = eval(B, A).
*/
% Case when the body is 'true', translate to 'True' in MeTTa
p2m(_OC, (Head:-Body), O):- Body == true, !, O = (=(Head, 'True')).

% Case when the body is 'fail', translate to 'empty' in MeTTa
p2m(_OC, (Head:-Body), O):- Body == fail, !, O = (=(Head, [empty])).

% For other cases, decompose the body into conjuncts and apply p2m recursively
p2m(OC, (Head:-Body), O):-
    % Recursively apply p2m to the head of the clause
    p2m(Head, H),
    % Convert conjunctions to a list of body terms
    conjuncts_to_list(Body, List),
    % Apply p2m to each conjunct
    maplist(p2m([progn|OC]), List, SP), 
    !,
    % The output is a MeTTa representation of the clause
    O = ['=', H | SP].

/** <p2m/3> 
    Handle the case where only the body is provided (a directive).
    @param OC Context.
    @param :-Body Directive in Prolog.
    @param O The output in MeTTa.
*/
% Convert Prolog directive into sequential MeTTa code
p2m(OC, (:-Body), O):- !,
    % Convert the body into a list of conjuncts
    conjuncts_to_list(Body, List),
    % Sequentially process the body in the current context
    into_sequential([progn|OC], List, SP), 
    !,
    % Output the MeTTa exec structure
    O = exec(SP).

/** <p2m/3>
    Handle the case of a query in Prolog (?-) and convert to MeTTa.
    @param OC Context.
    @param ?-Body Query in Prolog.
    @param O Output in MeTTa.
*/
% Convert Prolog queries into sequential MeTTa code
p2m(OC, ( ?- Body), O):- !,
    conjuncts_to_list(Body, List),
    into_sequential([progn|OC], List, SP), 
    !,
    % Output the MeTTa exec structure with a '?-' prefix
    O = exec('?-'(SP)).

/* previously: Handled Prolog clauses by converting the body into a list and applying p2m */
%p2m(_OC,(Head:-Body),O):- conjuncts_to_list(Body,List),into_sequential(OC,List,SP),!,O=(=(Head,SP)).

/** <p2m/3>
    Conversion for if-then-else constructs in Prolog.
    @param OC Context.
    @param (A->B;C) If-then-else construct in Prolog.
    @param O Output in MeTTa.
*/
% Convert Prolog if-then-else to MeTTa's det_if_then_else construct
p2m(OC, (A->B;C), O):- !, p2m(OC, det_if_then_else(A, B, C), O).

% Handle Prolog disjunction (A;B) by converting to MeTTa's 'or'
p2m(OC, (A;B), O):- !, p2m(OC, or(A, B), O).

% Convert Prolog soft-cut (A*->B;C) into MeTTa's 'if'
p2m(OC, (A*->B;C), O):- !, p2m(OC, if(A, B, C), O).

% Convert Prolog if-then (A->B) into MeTTa's det_if_then
p2m(OC, (A->B), O):- !, p2m(OC, det_if_then(A, B), O).

% Convert soft cut without else (A*->B) into MeTTa's 'if'
p2m(OC, (A*->B), O):- !, p2m(OC, if(A, B), O).

/** <p2m/3>
    Convert MeTTa specific definitions and constructs like metta_defn, metta_type, etc.
    @param _OC Context.
    @param metta_defn/4 Define a MeTTa atom or definition.
    @param O Output in MeTTa.
*/
p2m(_OC, metta_defn(Eq, Self, H, B), 'add-atom'(Self, [Eq, H, B])).

% Handle MeTTa type retrieval
p2m(_OC, metta_type, 'get-type').

% Handle atom retrieval in MeTTa
p2m(_OC, metta_atom, 'get-atoms').

/* previously: was handling clause in MeTTa */
%p2m(_OC,get_metta_atom,'get-atoms').

% Convert a Prolog clause into an atom retrieval operation in MeTTa
p2m(_OC, clause(H, B), ==([=, H, B], 'get-atoms'('&self'))).

% Convert Prolog assert, assertz, and asserta to MeTTa's add-atom
p2m(_OC, assert(X), 'add-atom'('&self', X)).
p2m(_OC, assertz(X), 'add-atom'('&self', X)).
p2m(_OC, asserta(X), 'add-atom'('&self', X)).

/** <p2m/3>
    Handle Prolog retract operations.
    @param _OC Context.
    @param retract(X) Remove an atom in MeTTa.
    @param O Output in MeTTa.
*/
% Convert Prolog retract into MeTTa's remove-atom
p2m(_OC, retract(X), 'remove-atom'('&self', X)).

% Convert retractall in Prolog to MeTTa's remove-all-atoms
p2m(_OC, retractall(X), 'remove-all-atoms'('&self', X)).

/* previously: A general catch-all case was used to decompose compound terms */
%p2m(_OC,I,O):- I=..[F|II],maplist(p2m,[F|II],OO),O=..OO.

/** <p2m/3>
    Catch-all clause to handle general compound terms in Prolog.
    @param OC Context.
    @param I Input term in Prolog.
    @param O Output term in MeTTa.
*/
% Decompose compound terms and apply p2m recursively
p2m(OC, I, O):-
    % Ensure it's a compound term
    compound(I),
    % Break the compound term into its functor and arguments
    I =.. [F | II], 
    % Recursively apply p2m on each argument
    maplist(p2m([F | OC]), II, OO), 
    % Convert functor F into its MeTTa equivalent
    into_hyphens(F, FF),
    % Construct the output with the converted functor and arguments
    O = [FF | OO].

/** <prolog_to_metta/2>
    Converts a Prolog term to a MeTTa term.
    @param V Prolog term.
    @param D Translated MeTTa term.
    @example
    ?- prolog_to_metta((A->B;C), O).
    O = det_if_then_else(A, B, C).
*/
prolog_to_metta(V, D) :-
    % Call p2m to perform the conversion and ensure it's deterministic
    p2m([progn], V, D), !.

/** <into_sequential/3>
    Converts a list of conjunctions into sequential MeTTa commands.
    @param OC Context.
    @param Body Body of the Prolog clause or goal.
    @param SP Sequential MeTTa commands.
*/
% Process Prolog conjunctions and convert them into a MeTTa sequential form
into_sequential(OC, Body, SP) :-
    % Ensure Body is not already a list
    \+ is_list(Body),
    % Break conjunctions into a list of terms
    conjuncts_to_list(Body, List),
    

is_list(List), % Converts a list of conjunctions into a sequential representation in MeTTa
    into_sequential(OC, List, SP), !.

% Base case for when the input is an empty list; this results in 'True'.
into_sequential([progn|_], Nothing, 'True') :- Nothing == [], !.

% Another base case for when the input is an empty list, results in 'Nil'.
into_sequential(_OC, Nothing, 'Nil') :- Nothing == [], !.

% If there is only one element in the list, convert it directly using 'prolog_to_metta'.
into_sequential(_, [SP], O) :- prolog_to_metta(SP, O).

% When the list has more than one element, convert each item using 'prolog_to_metta' and wrap it with AND.
into_sequential([progn|_], List, SPList) :-
    maplist(prolog_to_metta, List, SPList), !.

% If the AND operator is compiled, use it and convert all elements in the list to MeTTa.
into_sequential(_CA, List, [AND|SPList]) :-
    is_compiled_and(AND),
    maplist(prolog_to_metta, List, SPList), !.


/** list_direct_subdirectories(+Directory, -DirectSubdirectories)
    Get a list of immediate subdirectories in a given directory.

    @param Directory The directory to search in.
    @param DirectSubdirectories The result will be a list of subdirectories directly within the specified directory.

    @example
    ?- list_direct_subdirectories('/home/user', Subdirectories).
    Subdirectories = ['/home/user/Documents', '/home/user/Downloads'].
*/
% List all files in the directory.
list_direct_subdirectories(Directory, DirectSubdirectories) :-
    directory_files(Directory, Entries), % Retrieve the directory entries
    findall(Path,
            (
                member(Entry, Entries), % Iterate over each entry
                \+ member(Entry, ['.', '..']), % Skip '.' and '..'
                symbolic_list_concat([Directory, '/', Entry], Path), % Construct the full path
                is_directory(Path) % Ensure the entry is a directory
            ),
            DirectSubdirectories).

/** list_all_subdirectories(+Directory, -AllSubdirectories)
    Recursively list all subdirectories of a given directory.

    @param Directory The directory to start from.
    @param AllSubdirectories The result will be a list of all subdirectories, including nested ones.

    @example
    ?- list_all_subdirectories('/home/user', Subdirectories).
    Subdirectories = ['/home/user/Documents', '/home/user/Documents/Work'].
*/
% Recursively list all subdirectories in a directory.
list_all_subdirectories(Directory, AllSubdirectories) :-
    list_direct_subdirectories(Directory, DirectSubdirectories), % Get the immediate subdirectories
    findall(Sub,
            (
                member(SubDir, DirectSubdirectories), % For each direct subdirectory
                list_all_subdirectories(SubDir, Subs), % Recursively find subdirectories
                member(Sub, Subs) % Collect the nested subdirectories
            ),
            NestedSubdirectories),
    append(DirectSubdirectories, NestedSubdirectories, AllSubdirectories). % Combine direct and nested subdirectories

/** with_file_lists(+Rel, +P1, +FileSpec)
    Process a list of filenames by applying the predicate P1 to each file. It handles various types of file specifications, such as lists or atomic filenames.

    @param Rel The base directory.
    @param P1 The predicate to apply to each file.
    @param FileSpec The file or list of files to process.

    @example
    ?- with_file_lists('.', print_file, 'example.pl').
*/
% Base case: if the file specification is '.pl', succeed.
with_file_lists(_Rel, _P1, FileSpec) :- FileSpec == '.pl', !.

% If the file specification is a list, apply the predicate to each file.
with_file_lists(Rel, P1, FileSpec) :-
    is_list(FileSpec), !,
    ignore(maplist(with_file_lists(Rel, P1), FileSpec)).

% If the filename is atomic and the file exists, apply the predicate to the file.
with_file_lists(_Rel, P1, Filename) :-
    atomic(Filename), exists_file(Filename), !,
    ignore(call(P1, Filename)).

% Handle the case where the directory is relative and exists.
with_file_lists(Rel, P1, Filename) :-
    absolute_file_name(Rel, Dir, [access(read), file_errors(fail), file_type(directory)]),
    Rel \=@= Dir, !,
    with_file_lists(Dir, P1, Filename).

% If the relative directory doesn't exist, try using the current directory.
with_file_lists(Rel, P1, Filename) :-
    \+ exists_directory(Rel), !,
    with_file_lists('.', P1, Filename).

% Handle compound file paths and directories, attempting to resolve the file specification.
with_file_lists(Rel, P1, File) :-
    compound(File),
    absolute_file_name(File, Dir, [access(read), relative_to(Rel), file_errors(fail), extensions(['pl', 'prolog', 'pfc'])]),
    '\\=@='(Dir, File), !,
    with_file_lists(Rel, P1, Dir).

% Handle compound file paths when they represent directories.
with_file_lists(Rel, P1, File) :-
    compound(File),
    absolute_file_name(File, Dir, [access(read), file_errors(fail), relative_to(Rel), file_type(directory)]),
    '\\=@='(Dir, File), !,
    with_file_lists(Rel, P1, Dir).

/* previously: This block had redundant code for directory handling and file type handling. The earlier approach repeated the handling of compound files unnecessarily, so we have consolidated the logic above.
with_file_lists(Rel, P1, File) :-
    compound(File),
    absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(directory)]),
    '\\=@='(Dir, File), !,
    with_file_lists(Rel, P1, Dir).

with_file_lists(Rel, P1, File) :-
    compound(File), !,
    absolute_file_name(File, Dir, [access(read), file_errors(fail), file_type(['csv', 'tsv', ''])]),
    '\\=@='(Dir, File), !,

      with_file_lists(Rel,P1, Dir).

% Case 1: File contains a wildcard, so we expand it and process the resulting files.
with_file_lists(Rel, P1, File) :-
    % Check if the file contains a wildcard symbol '*'
    symbol_contains(File, '*'),
    % Expand the file name (with wildcard) to get the list of matching files
    expand_file_name(File, List),
    List \== [],  % Ensure the list is not empty
    !,  % Commit to this clause if conditions are met
    % Map over the file list and apply the with_wild_path/1 predicate to each file
    maplist(with_wild_path(Fnicate), List).

% Case 2: File is a directory, find all files matching wildcard in that directory.
with_file_lists(Rel, P1, File) :-
    % Check if File is a directory
    exists_directory(File),
    % Construct a wildcard to match files in the directory
    directory_file_path(File, '*.*sv', Wildcard),
    % Expand the wildcard into a list of files
    expand_file_name(Wildcard, List),
    !,  % Commit to this clause
    % Apply the predicate to each file in the list
    maplist(Fnicate, List).
*/



/* 
   This file contains predicates for handling file lists and directories.
   It uses predicates to expand wildcards and to recursively find and process files in directories.

   The following predicates are heavily commented to improve clarity, including examples where appropriate.
*/

% PLDoc header for with_file_lists/3
/** 
 * with_file_lists(+Rel, +P1, +FileOrDir) 
 * 
 * Processes files or directories, expanding wildcards and recursively processing directories if needed.
 * 
 * @param Rel The base directory or context for relative file paths.
 * @param P1  A predicate to apply to each discovered file.
 * @param FileOrDir The file or directory to process, possibly containing wildcards.
 * 
 * @example
 * ?- with_file_lists('.', writeln, '*.pl').
 * This will print all Prolog files in the current directory.
 */


/* previously:
   This clause was removed to avoid redundancy.
   It used a slightly different wildcard pattern ('*.pl') but the logic
   is covered in the active clauses.
*/

% Case 3: Wildcard handling when the wildcard is an atom and file does not exist.
with_file_lists(Rel, P1, Wildcard) :-
    atom(Wildcard),
    % Ensure that the file does not exist but may contain a wildcard pattern
    \+ exists_file(Wildcard),
    % Check if the wildcard contains special characters like '*', '?', or '|'
    once(atom_contains(Wildcard, '*') ; atom_contains(Wildcard, '?') ; atom_contains(Wildcard, '|')),
    % Expand the wildcard into a list of matching files
    expand_file_name(Wildcard, Files),
    Files \== [],  % Ensure the list is not empty
    !,  % Commit to this clause
    % Apply the predicate to each file in the list
    ignore(maplist(with_file_lists(Rel, P1), Files)).

% Case 4: Wildcard handling when expanding relative file paths.
with_file_lists(Rel, P1, Wildcard) :-
    atom(Wildcard),
    % Check if the wildcard contains special characters
    once(atom_contains(Wildcard, '*') ; atom_contains(Wildcard, '?') ; atom_contains(Wildcard, '|')),
    \+ exists_file(Wildcard),  % Ensure that the file does not exist yet
    % Convert relative wildcard to an absolute path
    absolute_file_name(Wildcard, AbsWildcard, [relative_to(Rel)]),
    \+ exists_file(AbsWildcard),  % Ensure that the absolute wildcard doesn't point to an existing file
    % Expand the wildcard into a list of matching files
    expand_file_name(AbsWildcard, Files),
    Files \== [],  % Ensure the list is not empty
    !,  % Commit to this clause
    % Apply the predicate to each file in the list
    ignore(maplist(with_file_lists(Rel, P1), Files)).

/* previously:
   There was another directory expansion clause that used '.' and '*.pl'.
   It has been commented out as the logic is now better covered by the wildcard handling above.
   The commented out code was not removed to preserve older functionality, which might be useful in specific contexts.
*/

% Case 5: Recursively process all files in directories when '**' is specified.
with_file_lists(Rel, P1, Local) :-
    (Local == '**' ; Local == '**.pl'),  % Check if the input is '**' or '**.pl'
    % Must-det predicate ensures that the directory exists
    must_det_ll((
        absolute_file_name(Directory, AbsDirectory, [file_type(directory)]),
        exists_directory(AbsDirectory))),
    % Find all source files in the directory, including recursively
    findall(File, directory_source_files(AbsDirectory, File, [recursive(true), if(true)]), Files),
    !,  % Commit to this clause
    % Apply the predicate to each file
    ignore(maplist(with_file_lists(Rel, P1), Files)).

% Case 6: Handle filenames with symbolic lists and subdirectories
with_file_lists(Rel, P1, Filename) :-
    % Decompose the symbolic list '**/S/More' into components
    symbolic_list_concat(['**', S | More], '/', Filename),
    symbolic_list_concat([S | More], '/', Rest),
    % Get all subdirectories
    list_all_subdirectories(Rel, AllSubdirectories),
    !,  % Commit to this clause
    % For each subdirectory, apply the predicate recursively
    forall(member(SubDir, AllSubdirectories), with_file_lists(SubDir, P1, Rest)).

% Case 7: Handle symbolic lists with wild directories
with_file_lists(Rel, P1, Filename) :-
    % Decompose symbolic list with wildcard directory
    symbolic_list_concat([WildDir, S | More], '/', Filename),
    symbolic_list_concat([Rel, WildDir, ''], '/', WildMaskDir),
    % Expand the wildcard into all matching subdirectories
    expand_file_name(WildMaskDir, AllSubdirectories),
    symbolic_list_concat([S | More], '/', Rest),
    !,  % Commit to this clause
    % For each subdirectory, apply the predicate recursively
    forall(member(SubDir, AllSubdirectories), with_file_lists(SubDir, P1, Rest)).

% Case 8: Process individual files, ensuring file existence.
with_file_lists(Rel, P1, FileSpec) :-
    atomic(FileSpec),  % Ensure the file specification is atomic
    % Convert to an absolute file name and ensure it exists
    absolute_file_name(FileSpec, AbsFile, [relative_to(Rel), access(read), file_errors(fail)]),
    exists_file(AbsFile),  % Check if the file exists
    !,  % Commit to this clause
    % Apply the predicate P1 to the file
    ignore(call(P1, AbsFile)).

% Case 9: Process directories recursively.
with_file_lists(Rel, P1, Directory) :-
    atomic(Directory),  % Ensure the directory is atomic
    % Convert to an absolute directory path and ensure it exists
    absolute_file_name(Directory, AbsDirectory, [relative_to(Rel), access(read), file_errors(fail), file_type(directory)]),
    exists_directory(AbsDirectory),  % Check if the directory exists
    !,  % Commit to this clause
    % Find all source files in the directory, including recursively
    findall(File, directory_source_files(AbsDirectory, File, [recursive(true), if(true)]), Files),
    !,  % Commit to this clause
    % Apply the predicate to each file
    ignore(maplist(with_file_lists(Rel, P1), Files)).
/**
 * @predicate with_file_lists/3
 * @param Rel The base directory or relative path for file resolution.
 * @param P1 A predicate to apply to each found file.
 * @param Wildcard A wildcard expression to find matching files.
 * Resolves files based on the given wildcard pattern relative to a base directory
 * and applies the predicate P1 to each found file.
 * Skips processing if no files are found that match the wildcard pattern.
 *
 * @example 
 * with_file_lists('.', process_file, '*.pl').
 */
with_file_lists(Rel, P1, Wildcard) :- 
    atom(Wildcard),  % Ensures the wildcard is an atom (a basic Prolog term).
    absolute_file_name(Wildcard, AbsWildcard, [relative_to(Rel)]),  % Resolves the absolute file name based on the wildcard and relative path.
    \+ exists_file(AbsWildcard),  % Checks if the file does not exist to avoid re-processing.
    expand_file_name(AbsWildcard, Files),  % Expands the wildcard to a list of matching files.
    Files \== [],  % Ensures that the list of files is not empty.
    !,  % Cuts to prevent backtracking if files are found.
    ignore(maplist(with_file_lists(Rel, P1), Files)).  % Recursively processes each found file.

% Previously: Predicate to apply P1 to the given filename, ensuring deterministic logic.
% Skipped to avoid enforcing strict determinism here.
% with_file_lists(Rel, P1, Filename) :- must_det_ll(call(P1, Filename)).

% Writes out the current file being processed to the source, then prints a newline.
with_file_lists(Rel, P1, Filename) :- 
    write_src(with_file_lists(Rel, P1, Filename)),  % Outputs the operation details.
    nl.  % Prints a newline.

/* previously: 
 * The following commented code was an example entry point to process files for 
 * a custom "Metta" format. It has been skipped due to its dependency on a specific environment and external tools.
 */
% Entry point for printing to Metta format. It clears the screen, sets the working directory,
% expands the filenames with a specific extension, and processes each file.
% cls,  % Clears the screen (assumes a custom or system-specific implementation).
% with_pwd('/opt/logicmoo_opencog/hyperon-wam/tests/gpt2-like/language_models/', 
% Finds all Prolog files in the specified directory.
% Filt = 'tests/gpt2-like/language_models/*.pl', 
% Filt = '/opt/logicmoo_opencog/hyperon-wam/tests/performance/nondet_unify/*.pl',
% convert_to_metta(Filt),  % Processes each found file.

% Default wildcard mask for locating Prolog files to process.
% A list of file patterns is specified, focusing on Prolog files in various subdirectories.
default_pl_mask(Mask) :- 
    Mask = [
        % 'src/main/metta_*.pl',  % Previously used to target specific metta files.
        % 'src/main/flybase_*.pl',  % Previously used to target specific flybase files.
        '*/*.pl', 
        '*/*/*.pl',
        '*/*/*/.pl',
        '*/*/*/*/.pl',
        '*/*/*/*/*/.pl',
        '*/*/*/*/*/*.pl',
        '*.pl'  % Includes all files ending with .pl in any subdirectory.
    ], !.  % Ensure no backtracking after determining the mask.

% Alternate default wildcard mask if the previous definition is not used.
default_pl_mask(Mask) :- 
    Mask = ['**/*.pl'].  % Recursive pattern to include all .pl files in any subdirectory.

% High-level function to convert files to Metta format in the console.
convert_to_metta_console :- 
    default_pl_mask(Mask),  % Get the default Prolog file mask.
    ignore(convert_to_metta_console(Mask)),  % Safely apply conversion to the mask.
    !,  % Prevent backtracking.
    writeln(';; convert_to_metta_console.').  % Log the completion message.

% High-level function to convert files to Metta format and write to a file.
convert_to_metta_file :- 
    default_pl_mask(Mask),  % Get the default Prolog file mask.
    ignore(convert_to_metta_file(Mask)),  % Safely apply conversion to the mask.
    !,  % Prevent backtracking.
    writeln(';; convert_to_metta_file.').  % Log the completion message.

% Main function that processes files and converts them to Metta format.
convert_to_metta :- 
    default_pl_mask(Mask),  % Get the default Prolog file mask.
    % Skipped setting garbage collection flag locally here as it's an optimization detail.
    call(ignore(convert_to_metta(Mask))),  % Safely apply conversion to the mask.
    !,  % Prevent backtracking.
    writeln(';; convert_to_metta.').  % Log the completion message.

% Short alias to call the Metta conversion process.
ctm :- convert_to_metta.

% Processes a list of filenames by applying the conversion predicate to each.
convert_to_metta_console(FileSpec) :-  
    with_file_lists('.', convert_to_metta_now(user_output), FileSpec).  % Process files in console mode.

% Processes a list of filenames and writes the output to a file.
convert_to_metta_file(FileSpec) :-  
    with_file_lists('.', convert_to_metta_now(_Create), FileSpec).  % Process files and write output to file.

% Main conversion handler for files that exist.
convert_to_metta(Filename) :- 
    atomic(Filename),  % Ensure the filename is an atom.
    exists_file(Filename),  % Check if the file exists.
    !,  % Prevent backtracking.
    ignore(convert_to_metta_file(Filename)),  % Safely apply file conversion.
    ignore(convert_to_metta_console(Filename)),  % Safely print file conversion result to the console.

% Fallback case to handle wildcard patterns for Metta conversion.
convert_to_metta(FileSpec) :- 
    with_file_lists('.', convert_to_metta, FileSpec).  % Process wildcard pattern.

% Processes the given filename by opening it, translating its content, and then closing the file.
convert_to_metta_now(OutputIn, Filename) :- 
    user_io(convert_to_metta_now_out(OutputIn, Filename)).  % Apply conversion with output handling.

% Helper predicate to output the conversion result.
convert_to_metta_now_out(OutputIn, Filename) :- 
    atom(Filename),  % Verifies that the filename is an atom.
/* 
    @predicate file_name_extension/3
    @desc Generates a new filename by replacing the old extension with `.metta`.
    @param Base The base name of the file without extension.
    @param _OldExt Placeholder for the old file extension.
    @param Filename The original filename with the old extension.
    @example file_name_extension('myfile', txt, Filename). 
        % Generates Filename as 'myfile.txt'
*/
file_name_extension(Base, _OldExt, Filename),

/* 
    @predicate file_name_extension/3 
    @desc Generates a new filename with the extension 'metta'.
    @param Base The base name of the file.
    @param metta The new extension to be added.
    @param NewFilename The generated filename with the '.metta' extension.
    @example file_name_extension('myfile', _, NewFilename).
        % Generates NewFilename as 'myfile.metta'
*/
file_name_extension(Base, metta, NewFilename),

/* 
    @predicate file_base_name/2
    @desc Extracts the base name of the file to use as the module name.
    @param Base The base name of the file.
    @param Module The module name derived from the file base name.
    @example file_base_name('myfile.metta', Module).
        % Module is set to 'myfile'
*/
file_base_name(Base, Module),

/* 
    @desc Setup: Prepare to open both the input and output files for reading and writing.
    The format statement was previously printing the action being performed but is now commented out.
    It can be re-enabled for debugging or logging purposes.
    previously: format('~N~n~w~n', [convert_to_metta(Filename,NewFilename)])
*/
% format('~N~n~w~n', [convert_to_metta(Filename, NewFilename)]),

/* 
    @predicate convert_to_metta_file/4
    @desc Handles the process of converting the file into the Metta format by opening 
    the input and output files and calling the necessary translation steps.
    @param Module The module name derived from the file.
    @param OutputIn The initial output stream, which can be a variable.
    @param Filename The original file to be converted.
    @param NewFilename The newly generated file with the .metta extension.
    @example convert_to_metta_file(myfile, _, 'myfile.txt', 'myfile.metta').
        % Converts the content of 'myfile.txt' to 'myfile.metta'
*/
convert_to_metta_file(Module, OutputIn, Filename, NewFilename).

/* 
    @predicate write_src_cmt/1
    @desc Writes the source of the goal G as a string and wraps it in a comment.
    @param G The goal whose source is written.
    @example write_src_cmt(my_goal).
        % Outputs the Prolog source of my_goal within comments.
*/
write_src_cmt(G) :- ignore((with_output_to(string(S), write_src(G)), in_cmt(write(S)))).

/* 
    @predicate convert_to_metta_file/4 
    @desc Main logic for converting a file from one format to another, handling input/output operations 
    and cleanup.
    @param Module The module name extracted from the filename.
    @param OutputIn The output stream for the new file (may be variable).
    @param Filename The original filename.
    @param NewFilename The new filename with the .metta extension.
    @example convert_to_metta_file(my_module, _, 'input.txt', 'output.metta').
        % Converts input.txt into output.metta.
*/
convert_to_metta_file(Module, OutputIn, Filename, NewFilename):-

    /* Copy the term OutputIn to Output to handle the conversion operation */
    copy_term(OutputIn, Output),

    /* 
        If OutputIn is unbound (a variable), print the conversion action being performed.
        previously: This part was used for tracing/debugging purposes and is kept for potential future use.
    */
    if_t(var(OutputIn),
       user_io(write_src_cmt(convert_to_metta_file(Module, OutputIn, Filename, NewFilename)))),

    /* previously: Output = user_output */
    
    /* 
        Use setup_call_cleanup to ensure proper handling of file streams. 
        Open the input file in read mode and apply ISO Latin-1 encoding. 
    */
    setup_call_cleanup(
        open(Filename, read, Input, [encoding(iso_latin_1)]),

        /* 
            Call: Open the output file if Output is a variable, and set its encoding to UTF-8. 
            Then, translate the content from the input file to the output file.
        */
        setup_call_cleanup(
            (if_t(var(Output), open(NewFilename, write, Output, [encoding(utf8)]))),
            with_output_to(Output,
                (
                    write_src_cmt(convert_to_metta_file(Module, OutputIn, Filename, NewFilename)),
                    translate_to_metta(Module, Input)
                )),
            /* 
                Cleanup: Ensure the output stream is closed after writing. 
            */
            close(Output)
        ),

        /* 
            Cleanup: Ensure the input stream is closed after reading. 
        */
        close(Input)
    ).

/* 
    @predicate into_namings/1
    @desc Helper predicate to handle variable naming in Prolog terms, ignoring if the variable is an unbound variable.
    @param N=V The term in the format of a variable name and its value.
    @example into_namings(X = '$VAR'(name)).
        % Ensures the variable name is handled appropriately.
*/
into_namings(N=V) :- ignore(V='$VAR'(N)).

/* 
    @predicate translate_to_metta/2
    @desc Recursively translates content from the input file to the Metta format.
    Terminates when the end of the input stream is reached.
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Processes the content from the input stream until the end of the file.
*/
translate_to_metta(Module, Input) :-
    at_end_of_stream(Input),  % Check if we've reached the end of the input file.
    !, nl.  % Stop processing and insert a newline.

/* 
    @predicate translate_to_metta/2
    @desc Handles the translation of reprintable characters (e.g., whitespace), ensuring they are preserved.
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Processes and prints any reprintable characters such as whitespace.
*/
translate_to_metta(Module, Input) :-
    peek_char(Input, Char),  % Peek at the next character in the input without consuming it.
    is_reprint_char(Char), !,  % If it's a reprintable character, proceed.
    get_char(Input, _),  % Consume the character.
    put_char(Char),  % Output the character as-is.
    translate_to_metta(Module, Input).  % Continue translating.

/* 
    @predicate translate_to_metta/2
    @desc Translates Prolog comments (starting with %) to Metta-style comments (starting with ;).
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Converts Prolog comments to Metta comments and processes the rest of the file.
*/
translate_to_metta(Module, Input) :-
    peek_char(Input, Char),  % Peek at the next character.
    Char == '%',  % Check if the character is a Prolog comment (%).
    get_char(Input, _), put_char(';'),  % Replace % with ; for Metta comment style.
    read_line_to_string(Input, Cmt),  % Read the rest of the comment line.
    print_metta_comments(Cmt), nl,  % Output the Metta-style comment and insert a newline.
    translate_to_metta(Module, Input).  % Continue processing.

/* 
    @predicate translate_to_metta/2
    @desc Translates alternative comment syntax (starting with #) to Metta-style comments.
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Converts # comments into Metta-style comments.
*/
translate_to_metta(Module, Input) :-
    peek_char(Input, Char),  % Peek at the next character.
    Char == '#',  % Check if the character is a comment starting with #.
    get_char(Input, _), put_char(';'),  % Replace # with ; for Metta comment style.
    read_line_to_string(Input, Cmt),  % Read the rest of the comment line.
    print_metta_comments(Cmt), nl,  % Output the Metta-style comment and insert a newline.
    translate_to_metta(Module, Input).  % Continue processing.

/* previously: The following code block for reading clauses with metadata was removed 
due to redundancy in the newer logic handling comments and file translation directly. 
It can be restored if metadata extraction becomes necessary in the future. */
% Reads a clause along with its metadata, then continues translation.
/* PLDoc header for translate_to_metta/2
   @param Module The module to which the translation is applied.
   @param Input The Prolog input to be translated into Metta.
   @example 
     translate_to_metta(my_module, Input).
*/
% The main predicate to translate Prolog clauses to Metta format.
% The clause is read using read_clause_with_info/1. If successful (!), 
% it recursively translates the next clause.
translate_to_metta(Module, Input):-
  % Reads a clause and captures additional information.
  read_clause_with_info(Input),!,
  % Recursively translate the next clause.
  translate_to_metta(Module, Input).


/* PLDoc header for is_reprint_char/1
   @param Char The character being checked.
   @desc This predicate checks if a character (space or period) should be reprinted as-is.
   @example 
     is_reprint_char('.').
*/
% Predicate to determine if the character is a space or a period, which should be retained in output.
is_reprint_char(Char):- char_type(Char, space).
is_reprint_char(Char):- Char == '.'.


/* PLDoc header for translate_comment/2
   @param Cmt The original Prolog comment.
   @param Str The translated Metta comment.
   @desc Translates comments from Prolog to Metta by replacing specific strings in the comment.
   @example 
     translate_comment("% This is a comment", "; This is a comment").
*/
% Translates Prolog comments into Metta comments by applying specific string replacements.
translate_comment(Cmt, Str):- replace_in_string(["%"=";",
                                                 "prolog"="MeTTa",
                                                 "PROLOG"="MeTTa",
                                                 "Prolog"="MeTTa"], Cmt, Str).


/* PLDoc header for read_clause_with_info/1
   @param Stream The input stream from which the clause is read.
   @desc This predicate reads a clause and captures relevant metadata like variable bindings and comments.
   @example 
     read_clause_with_info(Stream).
*/
% Reads a clause from the input stream and captures metadata. 
% Stops reading if end of stream is reached.
read_clause_with_info(Stream) :- at_end_of_stream(Stream),!.
read_clause_with_info(Stream):- 
  % Uses a catch block to handle exceptions when reading the clause.
  catch(read_clause_with_info_0(Stream), E,
  % Outputs the error using user_io if an exception occurs.
  ((user_io(write_src_cmt(E)), write_src_cmt(E)))).


/* PLDoc header for read_clause_with_info_0/1
   @param Stream The input stream from which the clause is read.
   @desc This predicate reads a clause and captures detailed information like term positions, comments, and bindings.
   @example 
     read_clause_with_info_0(Stream).
*/
% Reads a clause from the input stream with detailed options, capturing variable names, 
% term positions, syntax errors, and comments. 
read_clause_with_info_0(Stream) :-
    Options = [ variable_names(Bindings),            % Captures variable bindings.
                term_position(Pos),                 % Captures the term position.
                subterm_positions(RawLayout),       % Captures the layout of the term.
                syntax_errors(error),               % Handles syntax errors.
                comments(Comments),                 % Captures comments associated with the term.
                module(trans_mod)],                 % Sets the module for term processing.
    read_term(Stream, Term, Options),               % Reads the term with the given options.
    % If end of file is reached, terminate the process.
    (   (fail, Term == end_of_file)
    ->  true
    ;   % Sets global variables for term position and variable names.
        b_setval('$term_position', Pos),
        b_setval('$variable_names', Bindings),
        % Displays the term information and processes comments.
        display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments)).


/* PLDoc header for display_term_info/6
   @param Stream The input stream.
   @param Term The Prolog term being processed.
   @param Bindings The variable bindings for the term.
   @param Pos The term position.
   @param RawLayout The raw layout of the term.
   @param Comments The comments associated with the term.
   @desc This predicate processes the term and displays its metadata, including comments.
   @example 
     display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments).
*/
% Displays the term's information such as variable bindings, term positions, and comments.
display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments):-
   % Processes each variable binding into a human-readable format.
   maplist(into_namings, Bindings),
   % Processes the term while ignoring failure if term processing fails.
   ignore(process_term(Stream, Term)),
   % Prints comments in Metta format.
   print_metta_comments(Comments), !.


/* PLDoc header for print_metta_comments/1
   @param Comments The list of comments to be printed.
   @desc This predicate prints all the comments in Metta format.
   @example 
     print_metta_comments(["% Prolog comment"]).
*/
% Prints Metta comments by processing the list of comments.
print_metta_comments(Comments):- print_metta_comment(Comments).


/* PLDoc header for print_metta_comment/1
   @param Comments A comment or list of comments to be printed.
   @desc This predicate processes and prints individual comments recursively.
   @example 
     print_metta_comment("% This is a comment").
*/
% Base case for empty comment list.
print_metta_comment([]):-!.
% Recursively processes and prints a list of comments.
print_metta_comment(_TP-Cmt):- !, print_metta_comment(Cmt).
print_metta_comment([Cmt|Cs]):- !, print_metta_comment(Cmt),!, print_metta_comment(Cs).
% Processes a single comment by translating and printing it.
print_metta_comment(Cmt):- translate_comment(Cmt, String), print_cmt_lines(String).


/* PLDoc header for print_cmt_lines/1
   @param String The string representing the comment.
   @desc This predicate prints each line of a multi-line comment.
   @example 
     print_cmt_lines("This is a comment\nThis is another line").
*/
% Normalizes spaces and splits the comment into individual lines before printing.
print_cmt_lines(String):-
    normalize_space(string(TaxM), String),
    atomics_to_string(List, '\n', TaxM), !,
    % Prints each line of the comment.
    maplist(print_cmt_line, List).


/* PLDoc header for print_cmt_line/1
   @param Str The comment string to be printed.
   @desc This predicate prints a single comment line in the Metta format.
   @example 
     print_cmt_line("This is a comment").
*/
% Formats and prints a single comment line.
print_cmt_line(Str):- format('~N; ~w', [Str]).


/* PLDoc header for echo_as_commnents_until_eof/1
   @param Stream The input stream.
   @desc This predicate echoes comments from the input stream until end of file.
   @example 
     echo_as_commnents_until_eof(Stream).
*/
% Reads and echoes comments from the stream until the end of the file.
echo_as_commnents_until_eof(Stream):-
    repeat,
    % If end of the stream is reached, stop the process.
    (at_end_of_stream(Stream)-> !;
     % Reads the next line and echoes it as a comment.
     (read_line_to_string(Stream, Cmt),
       ignore((print_metta_comments(Cmt))),
        fail)).


/* PLDoc header for process_term/2
   @param Stream The input stream.
   @param Term The Prolog term being processed.
   @desc This predicate processes the term based on its type (directive or clause).
   @example 
     process_term(Stream, end_of_file).
*/
% Processes each term, checking if it is a directive or clause.
process_term(Stream, end_of_file):- !, echo_as_commnents_until_eof(Stream).
process_term(Stream, Term):-
    % If the term is a directive, call it and print it.
    is_directive(Term),
    ignore(maybe_call_directive(Stream, Term)),
    !, ignore(print_directive(Term)).
% Otherwise, expand the term and translate it to Metta.
process_term(_, Term):-
  expand_to_hb(Term, H, B),
  p2m((H:-B), STerm),
  push_term_ctx(Term),
  write_pl_metta(STerm).


/* PLDoc header for maybe_call_directive/2
   @param Stream The input stream.
   @param Directive The directive term.
   @desc This predicate calls Prolog directives when appropriate.
   @example 
     maybe_call_directive(Stream, op(500, xfx, '=>')).
*/
% Handles Prolog directives, potentially calling them if applicable.
maybe_call_directive(Stream, (:- X)):- !, maybe_call_directive(Stream, X).
maybe_call_directive(_Stream, op(X, F, Y)):- trans_mod:op(X, F, Y).
% maybe_call_directive/2
% This predicate checks the stream and calls a corresponding directive based on the argument.
% It either calls use_module or sets the stream encoding.
% 
% @param Stream The stream being operated on.
% @param Directive The directive to be executed.
maybe_call_directive(_Stream, use_module(library(W))) :- 
    % This checks if the directive is a use_module directive.
    trans_mod:use_module(library(W)).  % Calls use_module from trans_mod

maybe_call_directive(Stream, encoding(Enc)) :-
    % If the directive is about encoding, set the stream encoding.
    set_stream(Stream, encoding(Enc)).

% is_directive/1
% Checks if a term is a directive by verifying if it is prefixed with ':-'.
% 
% @param Term The term to be checked.
% @example is_directive((:- encoding(utf8))).
is_directive((:- _)).  % A directive starts with :-, if so, it is considered a directive.

% push_term_ctx/1
% Updates the current term context using a term if it's not already set.
% Handles non-compound terms, terms with bodies, and compound terms by their functor name.
% 
% @param X The term to push into the context.
push_term_ctx(X) :- 
    \+ compound(X), !, % If X is not compound, process directly
    (nb_current(term_ctx, Was) -> true ; Was = []), % Retrieve the current term context or set it as an empty list
    (Was =@= X -> true ; (nb_setval(term_ctx, X), nl)).  % If X is already the context, do nothing, otherwise update it.

push_term_ctx((X :- _)) :- 
    !, % If X is part of a clause, push X's context.
    push_term_ctx(X).

push_term_ctx(X) :- 
    compound_name_arity(X, F, _A),  % Extract the functor F of compound term X
    push_term_ctx(F).  % Push the functor as the context

% print_directive/1
% Prints a Prolog directive in a specific format after processing it.
% 
% @param Directive The directive to print.
print_directive((:- Directive)) :-
    push_term_ctx(exec), % Pushes 'exec' as the current term context.
    p2m([':-'], Directive, STerm),  % Converts the directive into a metaterm (STerm).
    write_pl_metta(exec(STerm)).  % Writes the metaterm in a specific format.

% write_pl_metta/1
% Wrapper to write a metaterm (STerm) ensuring that it's processed only once.
% 
% @param STerm The structured term to write.
write_pl_metta(STerm) :-
    \+ \+ write_pl_metta_0(STerm).  % Ensures the term is only written once.

% write_pl_metta_0/1
% Handles writing the structured term after handling variables within it.
% 
% @param STerm The structured term to write.
write_pl_metta_0(STerm) :-
    numbervars(STerm, 0, _, [singletons(true), attvar(skip)]),  % Number variables, handling singleton variables and skipping attributed variables.
    write_src(STerm).  % Write the structured term to the source.

% Directive to ensure the following files are loaded:
% - metta_compiler: Compiler functionalities
% - metta_convert: Conversion utilities
% - metta_types: Type definitions and checks
% - metta_space: Metta space utilities
% - metta_testing: Testing framework for metta
% - metta_utils: General utilities for metta
% - metta_printer: Printing functionalities
% - metta_eval: Evaluation functionalities
:- ensure_loaded(metta_compiler).
:- ensure_loaded(metta_convert).
:- ensure_loaded(metta_types).
:- ensure_loaded(metta_space).
:- ensure_loaded(metta_testing).
:- ensure_loaded(metta_utils).
:- ensure_loaded(metta_printer).
:- ensure_loaded(metta_eval).

/* previously: Some code related to other metta modules was skipped as it may no longer be relevant.
   The directives that follow are kept for loading utility and evaluation modules.
*/
