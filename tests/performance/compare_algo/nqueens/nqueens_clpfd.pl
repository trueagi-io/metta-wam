:- use_module(library(clpfd)).

% Solve the N-Queens problem for a given board size N
nqueens(N, Solution) :-
    length(Solution, N),
    Solution ins 1..N,
    safe(Solution),
    labeling([], Solution).

% Ensure queens are placed safely
safe([]).
safe([Q|Qs]) :-
    safe(Qs, Q, 1),
    safe(Qs).

safe([], _, _).
safe([Q|Qs], Q0, Dist) :-
    Q #\= Q0,
    abs(Q - Q0) #\= Dist,
    Dist1 #= Dist + 1,
    safe(Qs, Q0, Dist1).

% Start the benchmark
benchmark(N) :-
    statistics(walltime, [_ | [_]]),
    findall(Solution, nqueens(N, Solution), Solutions),
    length(Solutions, Count),
    statistics(walltime, [_ | [Elapsed]]),
    format("N=~w: ~w solutions found in ~w ms~n", [N, Count, Elapsed]).
