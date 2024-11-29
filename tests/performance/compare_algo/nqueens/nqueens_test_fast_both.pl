% ; Updated car-atom-or-fail and cdr-atom-or-fail predicates
% (= (car-atom-or-fail $atom)
%    (if (== $atom ())
%        (empty) ; Fail behavior
%        (car-atom $atom)))
car_atom_or_fail(Atom, Result) :-
    (Atom == [] -> fail ; [Result|_] = Atom).  % Extract the head of the list or fail if empty.

% (= (cdr-atom-or-fail $atom)
%    (if (== $atom ())
%        (empty) ; Fail behavior
%        (cdr-atom $atom)))
cdr_atom_or_fail(Atom, Result) :-
    (Atom == [] -> fail ; [_|Result] = Atom).  % Extract the tail of the list or fail if empty.

% ; Select function
% (= (select $x) ((car-atom-or-fail $x) (cdr-atom-or-fail $x)))
% (= (select $x)
%    (let* (($y (car-atom-or-fail $x))
%           ($z (cdr-atom-or-fail $x))
%           (($u $v) (select $z)))
%     ($u (cons-atom $y $v))))
select(List, (Head, Tail)) :-
    car_atom_or_fail(List, Head),  % Extract head of the list.
    cdr_atom_or_fail(List, Tail). % Extract tail of the list.

select(List, (U, [Y|V])) :-
    car_atom_or_fail(List, Y),     % Extract head of the list as Y.
    cdr_atom_or_fail(List, Z),     % Extract tail of the list as Z.
    select(Z, (U, V)).             % Recursive call to process the rest of the list.

% ; Range function
% (= (range $x $y)
%     (if (== $x $y)
%         ($x)
%         (let $z (range (+ $x 1) $y)
%             (cons-atom $x $z))))
range(X, Y, [X]) :- X == Y.         % Base case: if X equals Y, the range is just [X].
range(X, Y, [X|Z]) :-
    X \== Y,                       % Ensure X and Y are different.
    X1 is X + 1,                   % Increment X.
    range(X1, Y, Z).               % Recursive call to build the rest of the range.

% ; N-Queens Problem
% (= (nqueens $n)
%     (let $r (range 1 $n) (nqueens_aux $r ())))
nqueens(N, Result) :-
    range(1, N, R),                % Generate range [1, 2, ..., N].
    nqueens_aux(R, [], Result).    % Solve the N-Queens problem.

% ; N-Queens Auxiliary Function
% (= (nqueens_aux $unplaced $safe)
%     (if (== $unplaced ())
%         $safe
%         (let ($q $r) (select $unplaced)
%             (if (not_attack $q 1 $safe)
%                 (let $safeext (cons-atom $q $safe)
%                   (nqueens_aux $r $safeext))
%                 (empty)))))
nqueens_aux([], Safe, Safe).        % Base case: no unplaced queens, return the safe placements.

nqueens_aux(Unplaced, Safe, Result) :-
    select(Unplaced, (Q, R)),      % Select the first queen and the rest of the list.
    not_attack(Q, 1, Safe),        % Ensure the current queen doesn't attack the safe queens.
    SafeExt = [Q|Safe],            % Add the current queen to the safe list.
    nqueens_aux(R, SafeExt, Result). % Recursive call with the remaining queens.

% ; Not Attack Function
% (= (not_attack $q $d $s)
%     (if (== $s ())
%         True
%         (let* (($h (car-atom $s)) ($t (cdr-atom $s)))
%             (if (or (== $q $h)
%                     (or (== $q (+ $d $h)) (== $h (+ $q $d))))
%                 False
%                 (not_attack $q (+ $d 1) $t))))))
not_attack(_, _, []).               % Base case: no queens left to check, no attacks possible.
not_attack(Q, D, [H|T]) :-
    Q \= H,                        % Ensure Q does not equal H.
    \+ (Q =:= D + H),              % Ensure Q is not diagonally attacking H (Q = D + H).
    \+ (H =:= Q + D),              % Ensure H is not diagonally attacking Q (H = Q + D).
    D1 is D + 1,                   % Increment the diagonal offset.
    not_attack(Q, D1, T).          % Recursive call to check the rest of the list.

% Start the benchmark
benchmark(N) :-
    statistics(walltime, [_ | [_]]),
    nqueens(N, Solution),
    statistics(walltime, [_ | [Elapsed]]),
    length(Solution, Length),
    format("N=~w: ~w solutions found in ~w ms~n", [N, Length, Elapsed]).

:- forall(between(4,15,N), time(benchmark(N))).
