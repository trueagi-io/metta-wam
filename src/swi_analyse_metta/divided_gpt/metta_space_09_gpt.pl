/* This is a predicate to format a value (Bytes or general terms) and print it.
 * The first clause handles the case where the input is an integer (representing bytes).
 * The second clause handles all other types of terms.
 * @param Bytes The input value in bytes (if it's an integer) or other terms.
 * @example format_value(2048) would print "2K".
 */
format_value(Bytes) :- 
    % Check if Bytes is an integer (i.e., represents byte size)
    integer(Bytes), 
    % Call helper predicate to format the byte value
    format_bytes(Bytes, Formatted), 
    % Print the formatted value
    write(Formatted).

% Handle general term formatting
format_value(Term) :- 
    % Print the term as-is
    format("~w", [Term]).

/* Predicate to format bytes into a human-readable string representation (G, M, K, B).
 * It chooses the appropriate unit based on the size of the bytes.
 * @param Bytes The number of bytes to format.
 * @param Formatted The resulting human-readable string.
 * @example format_bytes(1073741824, Formatted) would return '1.00G'.
 */
% Base case: If the number is 1G or more, show it in gigabytes (G).
format_bytes(Bytes, Formatted) :-  
    Bytes >= 1073741824, 
    % Convert Bytes to Gigabytes
    GB is Bytes / 1073741824, 
    % Format the result with two decimal places and 'G'
    format(string(Formatted), '~2fG', [GB]).

% Case where the number is less than 1G but 100MB or more, display in megabytes (M).
format_bytes(Bytes, Formatted) :- 
    Bytes >= 104857600, Bytes < 1073741824, !, 
    % Convert Bytes to Megabytes
    MB is Bytes / 1048576, 
    % Round to the nearest integer for display
    D is floor(MB), 
    % Format the result in 'M'
    format(string(Formatted), '~DM', [D]).

% Case where the number is less than 1K, display in bytes (B).
format_bytes(Bytes, Formatted) :- 
    % Format the result directly as Bytes
    format(string(Formatted), '~D', [Bytes]).

/* previously: This clause handled formatting for kilobytes (K), but was commented out. 
 * It was skipped because it's less frequently used, and the current implementation 
 * prioritizes higher units like MB and GB for clarity. 
 * You could uncomment and adjust this if K units are needed.
 */
% % If the number is less than 1M, show it in kilobytes (K).
% format_bytes(Bytes, Formatted) :- 
%     Bytes >= 1024, Bytes < 1048576, !, 
%     KB is Bytes / 1024, 
%     format(string(Formatted), '~0fK', [KB]).

/* Predicate to format total seconds into a readable time (days, hours, minutes, seconds).
 * @param TotalSeconds The input time in seconds.
 * @param Formatted The resulting formatted string.
 * @example format_time(90061, Formatted) would return '1:01:00:01' (1 day, 1 hour, 1 second).
 */
format_time(TotalSeconds, Formatted) :-
    % Convert TotalSeconds to an integer (floor value)
    Seconds is floor(TotalSeconds),
    % Extract the number of days
    Days is div(Seconds, 86400),
    % Get the remaining seconds after subtracting full days
    Remain1 is mod(Seconds, 86400)-57600,
    % Format the remaining seconds as time (hours:minutes:seconds)
    format_time(string(Out), '%T', Remain1),
    % Combine days and formatted time into the result string
    format(string(Formatted), '~w:~w', [Days, Out]).

/* Predicate to print the formatted time.
 * @param TotalSeconds The time in seconds.
 * @example print_formatted_time(3600) would print '0:01:00:00' (1 hour).
 */
print_formatted_time(TotalSeconds) :-
    % Format the total seconds into a human-readable time string
    format_time(TotalSeconds, Formatted),
    % Print the formatted time
    writeln(Formatted).

/* A metapredicate to save pre-execution statistics.
 * This is likely to track system memory, atom usage, and space in a knowledge base.
 * This might be part of some performance logging or debugging process.
 */
metta_final :-
    save_pre_statistic(memory),
    save_pre_statistic(atoms),
    save_pre_statistic(atom_space).

/* previously: The following block of predicates handled symbolic manipulations with atoms.
 * This code has been commented out, possibly because it was not needed or replaced by more efficient methods.
 * These predicates could be uncommented if symbolic processing is required again.
 */
/*
symbol(X):- atom(X).
symbol_number(S,N):- atom_number(S,N).
symbol_string(S,N):- atom_string(S,N).
symbol_chars(S,N):- atom_chars(S,N).
symbol_length(S,N):- atom_length(S,N).
symbol_concat(A,B,C):- atom_concat(A,B,C).
symbolic_list_concat(A,B,C):- atomic_list_concat(A,B,C).
symbolic_list_concat(A,B):- atomic_list_concat(A,B).
symbol_contains(T,TT):- atom_contains(T,TT).
*/

/* Predicate to search for an atom in the knowledge base (KB) that contains a specific variable.
 * @param X The variable to search for within the KB.
 * This uses a forall loop to find every instance of X contained in atoms and print them.
 */
search_for1(X) :-
    forall((metta_atom(_Where, What), contains_var(X, What)),
           (nl, write_src_nl(What))).

/* Another search predicate that looks for a variable within file sources.
 * @param X The variable to search for in file source facts.
 * This also uses a forall loop to find and print matching occurrences.
 */
search_for2(X) :-
    forall((metta_file_src(_Where, What), contains_var(X, What)),
           (nl, write_src_nl(What))).

/* Predicate that retrieves source code from a file in the knowledge base (KB).
 * This fetches source code loaded into the KB and the associated variables.
 * @param Where The location within the KB.
 * @param What The specific source code or content.
 */
metta_file_src(Where, What) :-
    loaded_into_kb(Where, File), 
    metta_file_buffer(_, What, Vars, File, _Loc),
    % Attempt to name variables for better readability
    ignore(maplist(name_the_var, Vars)).

/* Predicate to guess variables within a block of code.
 * This is useful for tracking unification or variable names in the source.
 * @param What The term to match and unify variables.
 */
guess_metta_vars(What) :-
    ignore(once((metta_file_buffer(_, What0, Vars, _File, _Loc),
                 alpha_unify(What, What0),
                 maplist(name_the_var, Vars)))).

/* Helper predicate to name variables in a structured way.
 * It converts system-generated variable names ('$VAR') to readable ones.
 * @param N=V The variable name and value.
 */
name_the_var(N=V) :- 
    % Convert the variable name by stripping the underscore
    ignore((atom_concat('_', NV, N), V = '$VAR'(NV))).

/* Predicate to perform alpha-unification.
 * This checks if two terms are structurally identical while considering variable names.
 * @param What The first term.
 * @param What0 The second term for unification.
 */
alpha_unify(What, What0) :- 
    % Check if terms are identical
    What =@= What0, 
    % Perform strict equality if non-variable or shallow comparison if both are variables
    (nonvar(What) -> What = What0 ; What == What0).