/** 
 * shall_count_as_same(+A, +B)
 *
 * Predicate to determine whether two terms A and B should be considered the same.
 * Uses copy_term to ensure variables are not shared and then checks if A and B are identical
 * both structurally and syntactically using @=.
 * The use of double negation (\+ \+) ensures that the comparison fails gracefully.
 * 
 * @param A First term.
 * @param B Second term.
 * @example 
 * ?- shall_count_as_same(foo(X), foo(_)).
 * true.
 */
shall_count_as_same(A, B) :- 
    copy_term(B, CB),       % Copy term B into CB to ensure no variable sharing.
    copy_term(A, CA),       % Copy term A into CA to ensure no variable sharing.
    \+ \+ (A = B,           % Perform double negation to check if A and B are equal.
           B =@= CB,        % Check if B and its copy CB are syntactically identical.
           A =@= CA),       % Check if A and its copy CA are syntactically identical.
    !.                      % Cut to prevent backtracking.

/* previously:
   The line below was commented out because the alternative approach is less flexible 
   and doesn't account for variable binding in A and B.
   shall_count_as_same(A, B) :- \+ A \= B, !.
*/

/**
 * count_each(+List, +Group, -Result)
 *
 * Predicate to count the occurrence of each distinct element in List within Group.
 * Uses shall_count_as_same/2 to compare elements and length/2 to calculate the count.
 *
 * @param List The list of elements to count.
 * @param Group The group in which elements are counted.
 * @param Result The result as a list of pairs [Count-Element].
 * @example 
 * ?- count_each([a, b, a], [a, a, b, c], Result).
 * Result = [2-a, 1-b].
 */
count_each([C|L], GC, [Len-C|LL]) :- 
    include(shall_count_as_same(C), GC, Lst),   % Include elements in GC that match C.
    length(Lst, Len),                           % Get the length of the matching elements.
    !,                                          % Cut to prevent backtracking.
    count_each(L, GC, LL).                      % Recur for the rest of the list.
count_each([], _, []).                          % Base case: empty list.

/**
 * count_each_inv(+List, +Group, -Result)
 *
 * Similar to count_each/3 but the result pairs are formatted as Element-Count.
 * 
 * @param List The list of elements to count.
 * @param Group The group in which elements are counted.
 * @param Result The result as a list of pairs [Element-Count].
 * @example 
 * ?- count_each_inv([a, b, a], [a, a, b, c], Result).
 * Result = [a-2, b-1].
 */
count_each_inv([C|L], GC, [C-Len|LL]) :- 
    include(shall_count_as_same(C), GC, Lst),   % Include elements in GC that match C.
    length(Lst, Len),                           % Get the length of the matching elements.
    count_each_inv(L, GC, LL).                  % Recur for the rest of the list.
count_each_inv([], _, []).                      % Base case: empty list.

/**
 * maplist_n(+N, :P, +List)
 *
 * Applies the predicate P with the current index N to each element in the list.
 * Increments N on each recursive call.
 *
 * @param N Starting index.
 * @param P Predicate to apply.
 * @param List Input list.
 * @example 
 * ?- maplist_n(1, writeln, [a, b, c]).
 * 1
 * a
 * 2
 * b
 * 3
 * c
 * true.
 */
maplist_n(N, P, [H1|T1]) :- 
    p2_call(P, N, H1),      % Call predicate P with current index N and head H1.
    N1 is N + 1,            % Increment N.
    maplist_n(N1, P, T1).   % Recur with incremented N and rest of the list.
maplist_n(_, _, []).        % Base case: empty list.

/**
 * maplist_n(+N, :P, +List1, -List2)
 *
 * Applies predicate P with the current index N to corresponding elements of List1 and List2.
 * Increments N on each recursive call.
 *
 * @param N Starting index.
 * @param P Predicate to apply.
 * @param List1 Input list.
 * @param List2 Output list with transformed elements.
 * @example 
 * ?- maplist_n(1, writeln, [a, b], [x, y]).
 * 1
 * a
 * x
 * 2
 * b
 * y
 * true.
 */
maplist_n(N, P, [H1|T1], [H2|T2]) :- 
    call(P, N, H1, H2),     % Call predicate P with current index N, head H1 and corresponding head H2.
    N1 is N + 1,            % Increment N.
    maplist_n(N1, P, T1, T2).  % Recur with incremented N, rest of List1 and List2.
maplist_n(_, _, [], []).    % Base case: both lists are empty.

/* previously:
   The following code to print points grid is preserved but not used, 
   possibly because the current scope doesn't require graphical output handling.
*/

/*
print_points_grid(Points):- 
    points_range(Points, LoH, LoV, HiH, HiV, H, V), 
    writeqln(size_range(LoH, LoV, HiH, HiV, H, V)), 
    points_to_grid(Points, Grid), 
    print_grid(Grid).

print_points_grid(Grid):- 
    points_range(Grid, LoH, LoV, HiH, HiV, _H, _V), 
    print_grid(Grid, LoH, LoV, HiH, HiV, Grid).
*/

/* previously:
   The following code related to kaggle_arc_train and kaggle_arc_eval is skipped 
   because it's tied to a specific data set (Kaggle ARC), and may require access to external data.
   However, the structure is preserved as it may be useful for future use.
*/

/*
%print_trainer:- 
%    kaggle_arc_train(Name, Stuff), 
%    atom_json_term(Stuff, JSON, []), 
%    print_arc(Name, JSON).

%print_evaler:- 
%    kaggle_arc_eval(Name, Stuff), 
%    atom_json_term(Stuff, JSON, []), 
%    print_arc(Name, JSON).
*/


/* 
% data looks like

kaggle_arc_train('007bbfb7', trn, [[0, 7, 7], [7, 7, 7], [0, 7, 7]], [[0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [7, 7, 7, 7, 7, 7, 7, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7]]).
kaggle_arc_train('007bbfb7', trn, [[4,0, 4], [0,0,0], [0, 4,0]], [[4,0, 4,0,0,0, 4,0, 4], [0,0,0,0,0,0,0,0,0], [0, 4,0,0,0,0,0, 4,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 4,0, 4,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 4,0,0,0,0]]).
kaggle_arc_train('007bbfb7', trn, [[0,0,0], [0,0, 2], [2,0, 2]], [[0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 2], [0,0,0,0,0,0, 2,0, 2], [0,0,0,0,0,0,0,0,0], [0,0, 2,0,0,0,0,0, 2], [2,0, 2,0,0,0, 2,0, 2]]).
kaggle_arc_train('007bbfb7', trn, [[6, 6,0], [6,0,0], [0, 6, 6]], [[6, 6,0, 6, 6,0,0,0,0], [6,0,0, 6,0,0,0,0,0], [0, 6, 6,0, 6, 6,0,0,0], [6, 6,0,0,0,0,0,0,0], [6,0,0,0,0,0,0,0,0], [0, 6, 6,0,0,0,0,0,0], [0,0,0, 6, 6,0, 6, 6,0], [0,0,0, 6,0,0, 6,0,0], [0,0,0,0, 6, 6,0, 6, 6]]).
kaggle_arc_train('007bbfb7', trn, [[2, 2, 2], [0,0,0], [0, 2, 2]], [[2, 2, 2, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0, 2, 2,0, 2, 2,0, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 2, 2,0, 2, 2]]).
kaggle_arc_train('007bbfb7', tst, [[7,0, 7], [7,0, 7], [7, 7,0]], [[7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7, 7,0, 7,0,0,0], [7,0, 7, 7,0, 7,0,0,0], [7, 7,0, 7, 7,0,0,0,0]]).

kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3,0, 3,0,0], [0,0, 3,0, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]], [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3, 4, 3,0,0], [0,0, 3, 4, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]]).

