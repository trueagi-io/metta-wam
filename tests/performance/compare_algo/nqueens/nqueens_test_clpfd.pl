% Define the N-Queens problem

:- use_module(library(clpfd)).

nqueens(N, Solution) :- 
    length(Solution, N),
    Solution ins 1..N, 
    all_distinct(Solution),
    safe(Solution).

% Ensure no queens attack each other
safe([]).
safe([Q|Qs]) :- 
    no_attack(Q, Qs, 1), 
    safe(Qs).

% Check if a queen attacks another
no_attack(_, [], _).
no_attack(Q, [Q1|Qs], Dist) :- 
    Q #\= Q1,
    Q #\= Q1 + Dist,
    Q #\= Q1 - Dist,
    Dist1 #= Dist + 1,
    no_attack(Q, Qs, Dist1).


:- forall(between(4,10,N), time(nqueens(N,_))).


