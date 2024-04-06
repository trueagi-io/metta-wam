/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
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
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */



% # 1. Length of a List
% %  Normal Recursive
% prolog
len([], 0).
len([_|T], N) :-
    len(T, X),
    N is X + 1.
%

% %  With Accumulator
% prolog
len_acc(L, N) :-
    len_acc(L, 0, N).

len_acc([], Acc, Acc).
len_acc([_|T], Acc, N) :-
    NewAcc is Acc + 1,
    len_acc(T, NewAcc, N).
%

% # 2. Sum of a List
% %  Normal Recursive
% prolog
sum([], 0).
sum([H|T], S) :-
    sum(T, X),
    S is X + H.
%

% %  With Accumulator
% prolog
sum_acc(L, S) :-
    sum_acc(L, 0, S).

sum_acc([], Acc, Acc).
sum_acc([H|T], Acc, S) :-
    NewAcc is Acc + H,
    sum_acc(T, NewAcc, S).
%

% # 3. Factorial
% %  Normal Recursive
% prolog
factorial(0, 1).
factorial(N, F) :-
    N > 0,
    X is N - 1,
    factorial(X, Y),
    F is N * Y.
%

% %  With Accumulator
% prolog
factorial_acc(N, F) :-
    factorial_acc(N, 1, F).

factorial_acc(0, Acc, Acc).
factorial_acc(N, Acc, F) :-
    N > 0,
    NewAcc is Acc * N,
    NewN is N - 1,
    factorial_acc(NewN, NewAcc, F).
%

% # 4. Reverse List
% %  Normal Recursive
% prolog
reverse_list([], []).
reverse_list([H|T], R) :-
    reverse_list(T, RevT),
    append(RevT, [H], R).
%

% %  With Accumulator
% prolog
reverse_list_acc(L, R) :-
    reverse_list_acc(L, [], R).

reverse_list_acc([], Acc, Acc).
reverse_list_acc([H|T], Acc, R) :-
    reverse_list_acc(T, [H|Acc], R).
%

% # 5. Fibonacci
% %  Normal Recursive
% prolog
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.
%

% %  With Accumulator
% prolog
fibonacci_acc(N, F) :-
    fibonacci_acc(N, 0, 1, F).

fibonacci_acc(0, A, _, A).
fibonacci_acc(N, A, B, F) :-
    N > 0,
    NewN is N - 1,
    NewB is A + B,
    fibonacci_acc(NewN, B, NewB, F).
%



%  6. Find an Element in a List
% # Normal Recursive
% prolog
element_in_list(X, [X|_]).
element_in_list(X, [_|T]) :- element_in_list(X, T).
%

% # With Accumulator
% prolog
element_in_list_acc(X, L) :- element_in_list_acc(X, L, false).

element_in_list_acc(X, [], Acc) :- Acc.
element_in_list_acc(X, [X|_], _) :- true.
element_in_list_acc(X, [_|T], Acc) :- element_in_list_acc(X, T, Acc).
%

%  7. Check if a List is a Palindrome
% # Normal Recursive
% prolog
is_palindrome(L) :- reverse(L, L).
%

% # With Accumulator
% prolog
is_palindrome_acc(L) :- reverse_acc(L, [], L).

reverse_acc([], Acc, Acc).
reverse_acc([H|T], Acc, R) :- reverse_acc(T, [H|Acc], R).
%

%  8. Calculate the Product of All Elements in a List
% # Normal Recursive
% prolog
product_list([], 1).
product_list([H|T], P) :-
    product_list(T, Temp),
    P is H * Temp.
%

% # With Accumulator
% prolog
product_list_acc(L, P) :- product_list_acc(L, 1, P).

product_list_acc([], Acc, Acc).
product_list_acc([H|T], Acc, P) :-
    NewAcc is Acc * H,
    product_list_acc(T, NewAcc, P).
%

%  9. Find the Nth Element of a List
% # Normal Recursive
% prolog
nth_element(1, [H|_], H).
nth_element(N, [_|T], X) :-
    N > 1,
    M is N - 1,
    nth_element(M, T, X).
%

% # With Accumulator
% prolog
nth_element_acc(N, L, X) :- nth_element_acc(N, L, 1, X).

nth_element_acc(N, [H|_], N, H).
nth_element_acc(N, [_|T], Acc, X) :-
    NewAcc is Acc + 1,
    nth_element_acc(N, T, NewAcc, X).
%

%  10. Count the Occurrences of an Element in a List
% # Normal Recursive
% prolog
count_occurrences(_, [], 0).
count_occurrences(X, [X|T], N) :-
    count_occurrences(X, T, M),
    N is M + 1.
count_occurrences(X, [Y|T], N) :-
    X \= Y,
    count_occurrences(X, T, N).
%

% # With Accumulator
% prolog
count_occurrences_acc(X, L, N) :- count_occurrences_acc(X, L, 0, N).

count_occurrences_acc(_, [], Acc, Acc).
count_occurrences_acc(X, [X|T], Acc, N) :-
    NewAcc is Acc + 1,
    count_occurrences_acc(X, T, NewAcc, N).
count_occurrences_acc(X, [Y|T], Acc, N) :-
    X \= Y,
    count_occurrences_acc(X, T, Acc, N).
%

%  11. Calculate the Greatest Common Divisor of Two Numbers
% # Normal Recursive
% prolog
gcd(A, 0, A) :- A > 0.
gcd(A, B, GCD) :-
    B > 0,
    R is A mod B,
    gcd(B, R, GCD).
%

% # With Accumulator
% prolog
gcd_acc(A, B, GCD) :- gcd_acc(A, B, 1, GCD).

gcd_acc(A, 0, Acc, Acc) :- A > 0.
gcd_acc(A, B, Acc, GCD) :-
    B > 0,
    R is A mod B,
    NewAcc is B * Acc,
    gcd_acc(B, R, NewAcc, GCD).
%

%  12. Check if a Number is Prime
% # Normal Recursive
% prolog
is_prime(2).
is_prime(N) :-
    N > 2,
    \+ (between(2, sqrt(N), X), N mod X =:= 0).
%

% # With Accumulator
% prolog
is_prime_acc(N) :- is_prime_acc(N, 2).

is_prime_acc(2, 2).
is_prime_acc(N, Acc) :-
    N > 2,
    (
        (Acc * Acc > N, !);
        (N mod Acc =\= 0, NewAcc is Acc + 1, is_prime_acc(N, NewAcc))
    ).
%

%  13. Merge Two Sorted Lists into a Sorted List
% # Normal Recursive
% prolog
merge_sorted([], L, L).
merge_sorted(L, [], L).
merge_sorted([H1|T1], [H2|T2], [H1|M]) :-
    H1 =< H2,
    merge_sorted(T1, [H2|T2], M).
merge_sorted([H1|T1], [H2|T2], [H2|M]) :-
    H1 > H2,
    merge_sorted([H1|T1], T2, M).
%

% # With Accumulator
% prolog
merge_sorted_acc(L1, L2, L) :- merge_sorted_acc(L1, L2, [], L).

merge_sorted_acc([], L, Acc, L) :- reverse(Acc, L), !.
merge_sorted_acc(L, [], Acc, L) :- reverse(Acc, L), !.
merge_sorted_acc([H1|T1], [H2|T2], Acc, [H|M]) :-
    H1 =< H2,
    merge_sorted_acc(T1, [H2|T2], [H1|Acc], M).
merge_sorted_acc([H1|T1], [H2|T2], Acc, [H|M]) :-
    H1 > H2,
    merge_sorted_acc([H1|T1], T2, [H2|Acc], M).

%

%  14. Find the Last Element of a List
% # Normal Recursive
% prolog
last_element([H], H).
last_element([_|T], X) :- last_element(T, X).
%

% # With Accumulator
% prolog
last_element_acc([H|T], X) :- last_element_acc(T, H, X).

last_element_acc([], Acc, Acc).
last_element_acc([H|T], _, X) :- last_element_acc(T, H, X).
%

%  15. Remove Duplicate Elements from a List
% # Normal Recursive
% prolog
remove_duplicates([], []).
remove_duplicates([H|T], [H|T1]) :- \+ member(H, T), remove_duplicates(T, T1).
remove_duplicates([_|T], T1) :- remove_duplicates(T, T1).
%

% # With Accumulator
% prolog
remove_duplicates_acc(L, R) :- remove_duplicates_acc(L, [], R).

remove_duplicates_acc([], Acc, Acc).
remove_duplicates_acc([H|T], Acc, R) :-
    (member(H, Acc) -> remove_duplicates_acc(T, Acc, R);
    remove_duplicates_acc(T, [H|Acc], R)).
%

%  16. Check if a Binary Tree is Balanced
% # Normal Recursive
% prolog
is_balanced(null).
is_balanced(tree(L, _, R)) :-
    height(L, Hl),
    height(R, Hr),
    D is Hl - Hr,
    abs(D) =< 1,
    is_balanced(L),
    is_balanced(R).
%

% # With Accumulator
% prolog
is_balanced_acc(T) :- is_balanced_acc(T, 0).

is_balanced_acc(null, 0).
is_balanced_acc(tree(L, _, R), H) :-
    is_balanced_acc(L, Hl),
    is_balanced_acc(R, Hr),
    D is Hl - Hr,
    abs(D) =< 1,
    H is max(Hl, Hr) + 1.
%

%  17. Calculate the Height of a Binary Tree
% # Normal Recursive
% prolog
height(null, 0).
height(tree(L, _, R), H) :-
    height(L, Hl),
    height(R, Hr),
    H is max(Hl, Hr) + 1.
%

% # With Accumulator
% prolog
height_acc(T, H) :- height_acc(T, 0, H).

height_acc(null, Acc, Acc).
height_acc(tree(L, _, R), Acc, H) :-
    NewAcc is Acc + 1,
    height_acc(L, NewAcc, Hl),
    height_acc(R, NewAcc, Hr),
    H is max(Hl, Hr).
%

%  18. Search for an Element in a Binary Search Tree
% # Normal Recursive
% prolog
search_bst(tree(_, X, _), X).
search_bst(tree(L, Y, _), X) :-
    X < Y,
    search_bst(L, X).
search_bst(tree(_, Y, R), X) :-
    X > Y,
    search_bst(R, X).
%

% # With Accumulator
% prolog
% The accumulator is not very useful here, as the search path is already determined by the BST property.
search_bst_acc(Tree, X) :- search_bst(Tree, X).
%

%  19. Insert an Element into a Binary Search Tree
% # Normal Recursive
% prolog
insert_bst(null, X, tree(null, X, null)).
insert_bst(tree(L, Y, R), X, tree(L1, Y, R)) :-
    X < Y,
    insert_bst(L, X, L1).
insert_bst(tree(L, Y, R), X, tree(L, Y, R1)) :-
    X > Y,
    insert_bst(R, X, R1).
%

% # With Accumulator
% prolog
% The accumulator is not very useful here, as the insertion path is already determined by the BST property.
insert_bst_acc(Tree, X, NewTree) :- insert_bst(Tree, X, NewTree).
%

%  20. Delete an Element from a Binary Search Tree
% # Normal Recursive
% prolog
delete_bst(Tree, X, NewTree) :-
    remove_bst(Tree, X, NewTree).

remove_bst(tree(L, X, R), X, Merged) :- merge_trees(L, R, Merged), !.
remove_bst(tree(L, Y, R), X, tree(L1, Y, R)) :-
    X < Y,
    remove_bst(L, X, L1).
remove_bst(tree(L, Y, R), X, tree(L, Y, R1)) :-
    X > Y,
    remove_bst(R, X, R1).

merge_trees(null, Tree, Tree).
merge_trees(Tree, null, Tree).
merge_trees(tree(L1, X, R1), tree(L2, Y, R2), tree(Merged, Y, R2)) :-
    merge_trees(tree(L1, X, R1), L2, Merged).
%

% # With Accumulator
% prolog
% The accumulator is not very useful here, as the deletion path is already determined by the BST property.
delete_bst_acc(Tree, X, NewTree) :- delete_bst(Tree, X, NewTree).
%

%  21. Find the Lowest Common Ancestor in a Binary Search Tree
% # Normal Recursive
% prolog
lowest_common_ancestor(tree(_, Y, _), X, Z, Y) :-
    X < Y, Z > Y;
    X > Y, Z < Y.
lowest_common_ancestor(tree(L, Y, _), X, Z, LCA) :-
    X < Y, Z < Y,
    lowest_common_ancestor(L, X, Z, LCA).
lowest_common_ancestor(tree(_, Y, R), X, Z, LCA) :-
    X > Y, Z > Y,


    lowest_common_ancestor(R, X, Z, LCA).
%

% # With Accumulator
% prolog
% The accumulator is not very useful here, as the search path is already determined by the BST property.
lowest_common_ancestor_acc(Tree, X, Z, LCA) :- lowest_common_ancestor(Tree, X, Z, LCA).
%

%  22. Check if a Graph is Cyclic
% For graphs, it's better to represent them in a Prolog-friendly format, such as adjacency lists. I will use a representation where each node has a list of its neighbors.
% # Normal Recursive
% prolog
is_cyclic(Graph) :-
    member(Vertex-_, Graph),
    dfs(Vertex, Graph, [Vertex], _), !.

dfs(Vertex, Graph, Visited, [Vertex|Visited]) :-
    member(Vertex-Neighbors, Graph),
    member(Neighbor, Neighbors),
    member(Neighbor, Visited), !.
dfs(Vertex, Graph, Visited, FinalVisited) :-
    member(Vertex-Neighbors, Graph),
    member(Neighbor, Neighbors),
    \+ member(Neighbor, Visited),
    dfs(Neighbor, Graph, [Neighbor|Visited], FinalVisited).
%

% # With Accumulator
% prolog
% Due to the way depth-first search works, a typical accumulator wouldn't be very effective.
% The visited list already acts like an accumulator.
is_cyclic_acc(Graph) :- is_cyclic(Graph).
%

%  23. Perform a Depth-First Search on a Graph
% # Normal Recursive
% prolog
dfs_graph(Vertex, Graph) :- dfs_vertex(Vertex, Graph, []).

dfs_vertex(Vertex, _, Visited) :- member(Vertex, Visited), !.
dfs_vertex(Vertex, Graph, Visited) :-
    write(Vertex), nl,
    member(Vertex-Neighbors, Graph),
    dfs_neighbors(Neighbors, Graph, [Vertex|Visited]).

dfs_neighbors([], _, _).
dfs_neighbors([Neighbor|Neighbors], Graph, Visited) :-
    dfs_vertex(Neighbor, Graph, Visited),
    dfs_neighbors(Neighbors, Graph, Visited).
%

% # With Accumulator
% prolog
% The visited list acts as an accumulator.
dfs_graph_acc(Vertex, Graph) :- dfs_graph(Vertex, Graph).
%

%  24. Perform a Breadth-First Search on a Graph
% # Normal Recursive
% prolog
bfs_graph(Vertex, Graph) :-
    bfs([Vertex], Graph, [Vertex]).

bfs([], _, _).
bfs([Vertex|Vertices], Graph, Visited) :-
    write(Vertex), nl,
    member(Vertex-Neighbors, Graph),
    filter_unvisited(Neighbors, Visited, NewNeighbors, NewVisited),
    append(Vertices, NewNeighbors, NewVertices),
    bfs(NewVertices, Graph, NewVisited).

filter_unvisited([], Visited, [], Visited).
filter_unvisited([Neighbor|Neighbors], Visited, NewNeighbors, NewVisited) :-
    (member(Neighbor, Visited) ->
        filter_unvisited(Neighbors, Visited, NewNeighbors, NewVisited);
        filter_unvisited(Neighbors, [Neighbor|Visited], NewNeighbors, [Neighbor|NewVisited])
    ).
%

% # With Accumulator
% prolog
% The visited list acts as an accumulator.
bfs_graph_acc(Vertex, Graph) :- bfs_graph(Vertex, Graph).
%

%  25. Check if a Graph is Connected
% # Normal Recursive
% prolog
is_connected(Graph) :-
    Graph = [Vertex-_|_],
    dfs_graph(Vertex, Graph),
    \+ (member(OtherVertex-_, Graph), \+ member(OtherVertex, Visited)), !.
%

% # With Accumulator
% prolog
% The visited list acts as an accumulator.
is_connected_acc(Graph) :- is_connected(Graph).
%

%  26. Find the Shortest Path between Two Nodes in a Graph
% # Normal Recursive
% prolog
shortest_path(Start, End, Graph, Path) :-
    shortest_path([Start], End, Graph, [Start], Path).

shortest_path(_, End, _, Visited, ReversePath) :-
    reverse(ReversePath, [End|_]), !.
shortest_path(Vertices, End, Graph, Visited, Path) :-
    adjacent_unvisited(Vertices, Graph, Visited, Adjacent),
    append(Visited, Adjacent, NewVisited),
    append(Vertices, Adjacent, NewVertices),
    shortest_path(NewVertices, End, Graph, NewVisited, Path).
%

% # With Accumulator
% prolog
% The visited list and the list of vertices to explore act as accumulators.
shortest_path_acc(Start, End, Graph, Path) :- shortest_path(Start, End, Graph, Path).
%

%  27. Check if a String is a Palindrome
% # Normal Recursive
% prolog
is_string_palindrome(Str) :- string_chars(Str, Chars), is_palindrome(Chars).
%

% # With Accumulator
% prolog
is_string_pal

indrome_acc(Str) :- string_chars(Str, Chars), is_palindrome_acc(Chars, []).
%

%  28. Compute the Edit Distance between Two Strings
% # Normal Recursive
% prolog
edit_distance([], [], 0).
edit_distance([_|T1], [], D) :-
    edit_distance(T1, [], D1),
    D is D1 + 1.
edit_distance([], [_|T2], D) :-
    edit_distance([], T2, D1),
    D is D1 + 1.
edit_distance([H1|T1], [H2|T2], D) :-
    edit_distance(T1, T2, D1),
    D is D1 + (H1 \= H2).
%

% # With Accumulator
% prolog
edit_distance_acc(S1, S2, D) :- edit_distance_acc(S1, S2, 0, D).

edit_distance_acc([], [], Acc, Acc).
edit_distance_acc([_|T1], [], Acc, D) :- NewAcc is Acc + 1, edit_distance_acc(T1, [], NewAcc, D).
edit_distance_acc([], [_|T2], Acc, D) :- NewAcc is Acc + 1, edit_distance_acc([], T2, NewAcc, D).
edit_distance_acc([H1|T1], [H2|T2], Acc, D) :-
    NewAcc is Acc + (H1 \= H2),
    edit_distance_acc(T1, T2, NewAcc, D).
%

%  29. Find the Longest Common Subsequence of Two Strings
% # Normal Recursive
% prolog
lcs([], _, []).
lcs(_, [], []).
lcs([H|T1], [H|T2], [H|Lcs]) :- lcs(T1, T2, Lcs), !.
lcs(S1, [_|T2], Lcs) :- lcs(S1, T2, Lcs).
lcs([_|T1], S2, Lcs) :- lcs(T1, S2, Lcs).
%

% # With Accumulator
% prolog
lcs_acc(S1, S2, Lcs) :- lcs_acc(S1, S2, [], Lcs).

lcs_acc([], _, Acc, Lcs) :- reverse(Acc, Lcs).
lcs_acc(_, [], Acc, Lcs) :- reverse(Acc, Lcs).
lcs_acc([H|T1], [H|T2], Acc, Lcs) :- lcs_acc(T1, T2, [H|Acc], Lcs).
lcs_acc(S1, [_|T2], Acc, Lcs) :- lcs_acc(S1, T2, Acc, Lcs).
lcs_acc([_|T1], S2, Acc, Lcs) :- lcs_acc(T1, S2, Acc, Lcs).
%

%  30. Find the Longest Common Substring of Two Strings
% # Normal Recursive
% prolog
longest_common_substring(S1, S2, Lcs) :-
    findall(Sub, (substring(S1, Sub), substring(S2, Sub)), Subs),
    longest_string(Subs, Lcs).

substring(Str, Sub) :-
    append(_, Rest, Str),
    append(Sub, _, Rest).

longest_string([H|T], Longest) :-
    longest_string(T, H, Longest).

longest_string([], Acc, Acc).
longest_string([H|T], Acc, Longest) :-
    length(H, LenH),
    length(Acc, LenAcc),
    (LenH > LenAcc -> longest_string(T, H, Longest); longest_string(T, Acc, Longest)).
%

% # With Accumulator
% prolog
longest_common_substring_acc(S1, S2, Lcs) :-
    findall(Sub, (substring(S1, Sub), substring(S2, Sub)), Subs),
    longest_string_acc(Subs, [], Lcs).

longest_string_acc([], Acc, Acc).
longest_string_acc([H|T], Acc, Longest) :-
    length(H, LenH),
    length(Acc, LenAcc),
    (LenH > LenAcc -> longest_string_acc(T, H, Longest); longest_string_acc(T, Acc, Longest)).
%


