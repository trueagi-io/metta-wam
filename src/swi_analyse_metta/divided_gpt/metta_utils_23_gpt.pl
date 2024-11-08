/* tersifyL/2: This predicate simplifies or shortens lists or terms based on various conditions. 
 * It checks the structure and properties of the input, I, and outputs a simplified version, O.
 * It uses various helper predicates such as `tersify0/2`, `tersify1/2`, and `display_length/2` 
 * to handle specific cases.
 *
 * @param I The input term or list to be simplified.
 * @param O The output term after simplification.
 *
 * @example tersifyL([1,2,3,4,5], O). -> O = [1,2,3,4,5]
 */

/* The first clause checks if the input is not a "cons" (non-list structure) and returns it as-is. */
tersifyL(I, O) :- \+ is_cons(I), !, O = I.

/* The second clause simplifies the head of a list if the tail is not a list, 
   then recursively applies tersify/2 to the head. */
tersifyL([H|I], [HH|I]) :- \+ is_list(I), !, tersify(H, HH).

/* This clause handles cases where the head is non-variable and the tail is not a group. 
   If the list's display length exceeds 170 characters, it truncates the list with '...'. */
tersifyL([H|I], O) :- nonvar(H), \+ is_group(I), display_length(I, N), N > 170,
  length(I, LL), tersify(H, HH), (('...'(HH, LL, '...'(N))) = O), !.

/* Calls a generic simplification helper (tersify0/2). */
tersifyL(I, O) :- tersify0(I, O), !.

/* Recursively simplifies both the head and tail of the list. */
tersifyL([H|TT], [HH|TT]) :- tersify(H, HH), !, tersifyL(TT, TT), !.

/* Uses a different helper (tersify1/2) if other clauses fail. */
tersifyL(I, O) :- tersify1(I, O), !.

/* Default clause that returns the input unchanged if no simplification applies. */
tersifyL(I, I).

/* tersify2/2: Another variant of simplification focusing on compound terms and lists.
 * It attempts to reduce complex terms by recursively applying simplifications to 
 * their components.
 *
 * @param I Input term to be simplified.
 * @param O Output term after simplification.
 */

/* Simplifies equations by recursively simplifying both sides (N=V). */
tersify2(I, O) :- compound(I), (I = (N = V)), tersify2(N, NN), tersify2(V, VV), !, O = (NN = VV).

/* Checks if the term is "simple enough" and doesn't need further simplification. */
tersify2(I, O) :- simple_enough(I), !, I = O.

/* Simplifies compound terms by calling tersify1/2. */
tersify2(I, O) :- compound(I), tersify1(I, O), !.

/* Uses tersify0/2 for other compound terms. */
tersify2(I, O) :- tersify0(I, O), !.

/* Handles lists by recursively applying tersify2/2 to their elements. */
tersify2(I, O) :- is_list(I), !, my_maplist(tersify2, I, O).

/* Simplifies general compound terms by simplifying their arguments. */
tersify2(I, O) :- compound(I), !, compound_name_arguments(I, F, IA), my_maplist(tersify, IA, OA), compound_name_arguments(O, F, OA).

/* Default clause that returns the input unchanged if no simplification applies. */
tersify2(I, I).

/* tersify3/2: Yet another variant of the tersification logic, adding checks for long lists and
 * other compound terms. 
 *
 * @param I Input term to be simplified.
 * @param O Output term after simplification.
 */

/* Handles equations in compound terms and simplifies both sides. */
tersify3(I, O) :- compound(I), (I = (N = V)), tersify3(N, NN), tersify3(V, VV), !, O = (NN = VV).

/* Returns the term unchanged if it's simple enough. */
tersify3(I, O) :- simple_enough(I), !, I = O.

/* Uses tersify1/2 for simplifying compound terms. */
tersify3(I, O) :- compound(I), tersify1(I, O), !.

/* Uses tersify0/2 for compound terms that don't match the above cases. */
tersify3(I, O) :- tersify0(I, O), !.

/* Truncates lists that are too long, keeping the first element and adding '...'. */
tersify3([H|I], O) :- is_list(I), ((display_length(I, N), N > 170) -> 
  (length(I, LL), tersify(H, HH), (('...'(HH, LL, '...'(N))) = O)); I = O), !.

/* Recursively simplifies lists by applying tersify3/2 to each element. */
tersify3(I, O) :- is_list(I), !, my_maplist(tersify3, I, O).

/* Simplifies general compound terms by applying tersify to their arguments. */
tersify3(I, O) :- compound(I), !, compound_name_arguments(I, F, IA), my_maplist(tersify, IA, OA), compound_name_arguments(O, F, OA).

/* Default clause that returns the input unchanged. */
tersify3(I, I).

/* write_map/2: Outputs a mapping based on the input type, such as VM, dictionary, etc.
 * It checks for specific types like virtual machine (VM), dictionaries, and maps.
 *
 * @param G The goal or term being mapped.
 * @param Where A string or term that indicates where the output is being written.
 *
 * @example write_map(my_dict, output). -> ...Dict_output...
 */

/* Handles the case where G is a virtual machine term. */
write_map(G, Where) :- is_vm(G), !, write('...VM_'), write(Where), write('...').

/* Handles the case where G is a VM map. */
write_map(G, Where) :- is_vm_map(G), !, write('...Map_'), write(Where), write('...').

/* Handles the case where G is a dictionary. */
write_map(G, Where) :- is_dict(G), !, write('...Dict_'), write(Where), write('...').

/* Default case where G is not a special type; just write the output. */
write_map(_G, Where) :- write('...'), write(Where), write('...').

/* non_empty_wqs_c/1: Checks if a value is non-empty, considering different empty representations.
 * Empty strings, whitespace, or special HTML characters are considered "empty."
 *
 * @param V The value to check.
 *
 * @example non_empty_wqs_c(" "). -> false
 */

/* Checks if the value is not empty by negating the empty_wqs_c/1 predicate. */
non_empty_wqs_c(V) :- \+ empty_wqs_c(V).

/* empty_wqs_c/1: Determines if a value is "empty."
 * It considers variables, empty strings, HTML space characters, and newline characters as empty.
 *
 * @param V The value to check.
 */

/* Fails if the input is a variable, as it cannot determine if it's empty. */
empty_wqs_c(V) :- var(V), !, fail.

/* Converts atoms to strings for further checks. */
empty_wqs_c(A) :- atom(A), atom_string(A, S), !, empty_wqs_c(S).

/* Empty list is considered empty. */
empty_wqs_c([]).

/* Empty string is considered empty. */
empty_wqs_c("").

/* HTML non-breaking space is considered empty. */
empty_wqs_c("&nbsp;").

/* A single space character is empty. */
empty_wqs_c(" ").

/* A newline character is considered empty. */
empty_wqs_c("\n").

%is_writer_goal((C1,C2)):- !, (is_writer_goal(C1);is_writer_goal(C2)).
/* previously: This clause handled writer goals in compound terms,
   but was skipped, likely due to complexity or inefficiency in its current form. */

/* is_writer_goal/1: Checks if a goal involves writing output.
 * This predicate looks for specific goal structures that indicate writing,
 * such as format/2, nl/0, or specific atoms related to printing.
 *
 * @param H The goal to check.
 */

/* Fails if the input is not callable. */
is_writer_goal(H) :- \+ callable(H), !, fail.

/* Fails if the input is a list. */
is_writer_goal(H) :- is_list(H), !, fail.

/* If the input is an atom, it checks if the atom indicates a writer goal. */
is_writer_goal(A) :- atom(A), !, is_writer_goal_f(A).

/* Fails if the input is neither a compound term nor callable. */
is_writer_goal(H) :- \+ compound(H), !, fail.

/* The commented clause was previously designed to check for writer goals in compound terms,
   but it was disabled, possibly for optimization purposes. */

/* is_writer_goal_f/1: A helper predicate to check if an atom is a writer goal.
 * It handles specific cases like 'wqs_c', 'format', 'nl', and others.
 *
 * @param F The atom to check.
 */

/* Checks if the atom is 'wqs_c', indicating a writer goal. */
is_writer_goal_f(wqs_c).

/* Attempts to match the atom against known writer goal patterns. */
is_writer_goal_f(F) :- is_writer_goal_l(F), !.

/* Converts a non-atom term to an atom for checking if it's a writer goal. */
is_writer_goal_f(F) :- \+ atom(F), !, term_to_atom(F, A), is_writer_goal_f(A).

/* Filters out known non-writer goal patterns using the atom_concat/3 method. */
is_writer_goal_f(F) :- not_writer_goal_r(R), atom_concat(_, R, F), !, fail.

/* Matches atoms that start or end with known writer goal components. */
is_writer_goal_f(F) :- is_writer_goal_l(L), atom_concat(L, _, F), !.
is_writer_goal_f(F) :- is_writer_goal_l(R), atom_concat(_, R, F), !.

/* Helper predicates to define specific patterns of writer goals. */
not_writer_goal_r(test). 
is_writer_goal_l(msg). 
is_writer_goal_l(call).
is_writer_goal_l(nl).  
is_writer_goal_l(format). 
is_writer_goal_l(with_).
is_writer_goal_l(locally).
is_writer_goal_l(html).  
is_writer_goal_l(ptcol).  
is_writer_goal_l(wots).
is_writer_goal_l(print). 
is_writer_goal_l(flush_output).  
is_writer_goal_l(wqs).