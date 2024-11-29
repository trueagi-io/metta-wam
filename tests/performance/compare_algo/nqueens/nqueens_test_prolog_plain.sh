#!/bin/bash

# Create a Prolog program for the N-Queens problem
cat > nqueens.pl <<'EOF'
% Generate a list of integers from X to Y
range(X, X, [X]).
range(X, Y, [X|Rest]) :-
    X < Y,
    X1 is X + 1,
    range(X1, Y, Rest).

% Permute a list (generate all permutations)
permute([], []).
permute([X|Xs], Perm) :-
    permute(Xs, Perm1),
    insert(X, Perm1, Perm).

% Insert element X into list Y at any position
insert(X, List, [X|List]).
insert(X, [Y|Ys], [Y|Zs]) :-
    insert(X, Ys, Zs).

% Check if queens in Solution are placed safely
safe([]).
safe([Q|Qs]) :-
    no_attack(Q, Qs, 1),
    safe(Qs).

% Ensure no queen attacks another
no_attack(_, [], _).
no_attack(Q, [Q1|Qs], Dist) :-
    Q =\= Q1,
    abs(Q - Q1) =\= Dist,
    Dist1 is Dist + 1,
    no_attack(Q, Qs, Dist1).

% Solve the N-Queens problem for a given board size N
nqueens(N, Solution) :-
    range(1, N, Rows),
    permute(Rows, Solution),
    safe(Solution).

% Start the benchmark
benchmark(N) :-
    statistics(walltime, [_ | [_]]),
    nqueens(N, Solution),
    statistics(walltime, [_ | [Elapsed]]),
    length(Solution, Length),
    format("N=~w: ~w solutions found in ~w ms~n", [N, Length, Elapsed]).
EOF

# Benchmark for board sizes 4 through 15
echo "Benchmarking N-Queens (Prolog implementation)"
for n in {4..15}; do
    echo "N=$n:"
    swipl --quiet -g "consult('nqueens.pl'), benchmark($n), halt."
    echo
done

# Clean up
rm -f nqueens.pl

