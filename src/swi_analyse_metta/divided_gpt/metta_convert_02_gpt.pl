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