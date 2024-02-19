% ---- Naive Recursion Method ----
% Summary: Simple and direct implementation using the basic definition of Fibonacci numbers.
% Pros: Easy to understand and implement.
% Cons: Highly inefficient for large N due to recalculations of already computed Fibonacci numbers; exponential time complexity.
fib_naive(0, 0).
fib_naive(1, 1).
fib_naive(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_naive(N1, F1),
    fib_naive(N2, F2),
    F is F1 + F2.

% ---- Memoization (Dynamic Programming) Method ----
% Summary: Uses memoization to store previously calculated Fibonacci numbers, avoiding recalculations and improving efficiency.
% Pros: Efficient even for large N due to no recalculations; polynomial time complexity.
% Cons: Uses additional memory to store the previously calculated Fibonacci numbers.
:- dynamic was_fib_memo/2.
fib_memo(0, 0) :- !.
fib_memo(1, 1) :- !.
fib_memo(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    ( was_fib_memo(N1, F1) -> true ; fib_memo(N1, F1), assertz(was_fib_memo(N1, F1)) ),
    ( was_fib_memo(N2, F2) -> true ; fib_memo(N2, F2), assertz(was_fib_memo(N2, F2)) ),
    F is F1 + F2,
    assertz(was_fib_memo(N, F)).


% ---- Tail Recursion with Accumulators Method ----
% Summary: Calculates the Fibonacci sequence using tail recursion with accumulators, maintaining a constant stack size.
% Pros: Efficient and uses a constant amount of stack space.
% Cons: Slightly more complex due to the use of accumulators and a helper predicate.
fib_tail_recursive(N, F) :- fib_tail_recursive(N, 0, 1, F).
fib_tail_recursive(0, A, _, A).
fib_tail_recursive(N, A, B, F) :-
    N > 0,
    N1 is N - 1,
    Sum is A + B,
    fib_tail_recursive(N1, B, Sum, F).



% ---- Tabling Method ----
% Summary: Uses tabling to store intermediate results, avoiding redundant calculations and improving efficiency.
% Pros: Efficient even for large N due to no recalculations; polynomial time complexity.
% Cons: Uses additional memory to store the intermediate results; support for tabling is not available in all Prolog implementations.
:- table fib_tabled/2.
fib_tabled(0, 0).
fib_tabled(1, 1).
fib_tabled(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib_tabled(N1, F1),
    fib_tabled(N2, F2),
    F is F1 + F2.





% List of all the Fibonacci implementations
fibonaccis([
    fib_memo,
    fib_tail_recursive,
    fib_tabled,
    fib_naive
]).

% Utility to run and time each Fibonacci implementation
time_fibonaccis(N) :-
    retractall(was_fib_memo(_,_)), % Clear any memoized results
    fibonaccis(Fibonaccis),
    member(Fib, Fibonaccis),
  format('~N~n% =====================================================~n',[]),
  Goal=..[Fib,N,_],
  statistics(walltime, [Start|_]),
  catch(call(Goal),E,format('~N~nError in Goal: ~q ~q ~n',[Goal,E])),
  statistics(walltime, [End|_]),
  Time is End - Start,
  format('~N~n~w(~w) took \t~w ms~n', [Fib, N, Time]),
  fail.


time_fibonaccis(_):-
    format('~N~n% =====================================================~n',[]),!.

% Running the utility with an example, N=30.

%:- writeln(':- time_fibonaccis(30000)').


