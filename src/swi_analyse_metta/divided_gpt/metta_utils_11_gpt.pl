% Predicate to disable arc expansion rules by retracting them and setting a flag
% This predicate disables all arc_term_expansion rules and ensures that they are no longer applied.
% @param None
% @example disable_arc_expansion. % Disables the arc expansion rules
disable_arc_expansion:-
    % For each arc_term_expansions rule, retract it from the database
    forall(arc_term_expansions(Rule), forall(retract(Rule), true)),
    % Set the Prolog flag to false, preventing further arc term expansion
    set_prolog_flag(arc_term_expansion, false).

% Directive to declare a multifile predicate
% The `goal_expansion/4` predicate can be defined in multiple files.
:- multifile(goal_expansion/4).

% Directive to declare a dynamic predicate
% The `goal_expansion/4` predicate can be modified during runtime.
:- dynamic(goal_expansion/4).

% Predicate for expanding goals during execution, specifically handling compound goals
% @param G - The goal to expand
% @param I - Input context for the expansion
% @param GG - Expanded goal output
% @param O - Output context (usually identical to input)
% @example goal_expansion(goal, input, expanded_goal, output).
goal_expansion(G, I, GG, O):-
    % Ensure that the input is non-variable and that the source location is known
    nonvar(I), source_location(_, _),
    % If the goal is a compound term, proceed with expansion
    compound(G),
    % If a must_det (deterministic) condition is removed, expand the goal accordingly
    % If not, check for md_like conditions and try to expand those
    (remove_must_det(MD) -> remove_mds(MD, G, GG)
    ; (md_like(MD), maybe_expand_md(MD, G, GG))),
    % Input and output contexts are set to be equal
    I = O.


/*
:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).

my_assertion(G):- call(G),!.
my_assertion(G):- fbug(my_assertion(G)),writeq(goal(G)),nl,!,break.
must_be_free(AllNew):- plain_var(AllNew),!.
must_be_free(AllNew):- arcST,fbug(must_be_free(AllNew)),break,fail.
must_be_nonvar(AllNew):- nonvar_or_ci(AllNew),!.
must_be_nonvar(AllNew):- arcST,fbug(must_be_nonvar(AllNew)),break,fail.

my_len(X,Y):- var(X),!,length(X,Y).
my_len(X,Y):- is_list(X),!,length(X,Y).
my_len(X,Y):- functor([_|_],F,A),functor(X,F,A),!,length(X,Y).
my_len(X,Y):- arcST,!,ibreak.
*/


% Predicate to check if a goal is a map (using some virtual machine map logic)
% @param G - The goal to check
is_map(G):- 
    % Check if G is a virtual machine map
    is_vm_map(G), !.

% Sort the input list I into output list O, safely handling errors
% @param I - Input list
% @param O - Output list (sorted)
sort_safe(I, O):- 
    % Attempt to sort the list; if sorting fails, return the input as output
    catch(sort(I, O), _, I = O).

% Predicate to append two lists
% @param A - First list
% @param B - Second list
my_append(A, B):- 
    % Append list B to list A
    append(A, B).

% Predicate to append two lists and output the result in a third list
% @param A - First list
% @param B - Second list
% @param C - Resulting list after appending A and B
my_append(A, B, C):- 
    % Append list B to list A, resulting in list C
    append(A, B, C).

% Execute a goal with tty output set to false (for silent execution)
% @param Goal - The goal to execute
with_tty_false(Goal):- 
    % Temporarily set the tty flag to false and execute the goal
    with_set_stream(current_output, tty(false), Goal).

% Execute a goal with tty output set to true (for normal execution)
% @param Goal - The goal to execute
with_tty_true(Goal):- 
    % Temporarily set the tty flag to true and execute the goal
    with_set_stream(current_output, tty(true), Goal).

% Count occurrences of a goal G and store the result in N
% @param G - The goal to count
% @param N - The number of occurrences of G
% @example count_of(member(X, [1,2,3]), N). % N = 3
count_of(G, N):- 
    % Find all variants of G, collect them in a set, and count the length of the set
    findall_vset(G, G, S), length(S, N).

% Find all variants of T where goal G is true, store in set S
% @param T - Term to find
% @param G - Goal to check
% @param S - Set of variants found
findall_vset(T, G, S):- 
    % Find all instances of T where G is true, convert the list to a set
    findall(T, G, L), variant_list_to_set(L, S).

% Flatten a list of objects into a single list
% @param Objs - The list of objects to flatten
% @param ObjsO - The flattened list
flatten_objects(Objs, ObjsO):- 
    % Flatten the list, ensuring that nested lists are merged into one
    flatten([Objs], ObjsO), !.

% Compare if two terms are equal using variable handling rules
% @param E - First term
% @param S - Second term
var_e(E, S):- 
    % If E and S are the same, succeed
    E == S, !.
var_e(E, S):- 
    % If E is non-variable or an attributed variable, compare using unification
    (nonvar(E); attvar(E)), !, E =@= S.

% Convert a list of variants into a set, removing duplicates
% @param List - List of variants
% @param Out - Resulting set of variants
variant_list_to_set([E|List], Out):- 
    % Select a variant from the list, compare it to E, and continue
    select(S, List, Rest), var_e(E, S), !, 
    variant_list_to_set([E|Rest], Out).
variant_list_to_set([E|List], [E|Out]):- 
    % If E is unique, add it to the set and continue
    !, variant_list_to_set(List, Out).
variant_list_to_set(H, H).

% Predicate to substitute terms in a structure using non-backtracking set argument
% @param Obj - The object to modify
% @param New - The new term to insert
% @param Old - The old term to replace
nb_subst(Obj, New, Old):- 
    % Get the position P1 where the substitution needs to happen and apply it
    get_setarg_p1(nb_setarg, Found, Obj, P1), Found =@= Old,
    p1_call(P1, New), !, nb_subst(Obj, New, Old).
nb_subst(_Obj, _New, _Old).

% Predicate to check if any arc-related files are present in a list or term
% @param Some - The list or atom to check
system:any_arc_files(Some):- 
    % If Some is a non-empty list, map over the list and check each element
    is_list(Some), !, Some \== [], maplist(any_arc_files, Some).
system:any_arc_files(Some):- 
    % Check if the atom contains the word 'arc'
    atom_contains(Some, 'arc').

% Directive to declare a thread-local predicate
% The `in_memo_cached/5` predicate is local to each thread.
:- thread_local(in_memo_cached/5).

% Directive to declare a multifile hook for the make process
% The `prolog:make_hook/2` predicate can be defined across multiple files.
:- multifile(prolog:make_hook/2).

% Directive to declare a dynamic hook for the make process
% The `prolog:make_hook/2` predicate can be modified at runtime.
:- dynamic(prolog:make_hook/2).

% Hook to handle actions before the make process, clearing caches if necessary
% @param Some - The files or terms involved in the make process
prolog:make_hook(before, Some):- 
    % If there are any arc-related files, clear all caches before proceeding
    any_arc_files(Some), forall(muarc:clear_all_caches, true).

% Multifile predicate to clear all caches (may be defined in other files)
:- multifile(muarc:clear_all_caches/0).

% Dynamic predicate to clear all caches (modifiable at runtime)
:- dynamic(muarc:clear_all_caches/0).

% Predicate to clear all caches unless extreme caching is enabled
% @param None
muarc:clear_all_caches:- 
    % If extreme caching is not enabled, retract all cached entries
    \+ luser_getval(extreme_caching, true), 
    retractall(in_memo_cached(_, _, _, _, _)), fail.

% Memoization mechanism for caching goals to avoid redundant computations
% @param G - The goal to cache
arc_memoized(G):- 
    % If G is a compound term and ground, attempt memoization
    compound(G), ground(G), functor(G, F, 1), functor(C, F, 1), !, 
    arc_memoized(C), G = C, !.
arc_memoized(G):-
  copy_term(G,C,GT),
  (Key = (C+GT)),
  (in_memo_cached(Key,C,track,started,Info)->throw(already_memoizing(in_memo_cached(Key,C,track,started,Info))) ; true),
  numbervars(Key,0,_,[attvar(bind),singletons(true)]),!,
  setup_call_cleanup((asserta(in_memo_cached(Key,C,track,started,_),Started)),
  catch(
  (in_memo_cached(Key,C,GT,Found,AttGoals)*->(G=Found,maplist(call,AttGoals))