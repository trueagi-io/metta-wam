% Standard Recursive Approach
% Pros: Simple and easy to understand.
% Cons: Not efficient for large N as it is not tail recursive.
factorial_standard(0, 1).
factorial_standard(N, Result) :-
    N > 0,
    M is N - 1,
    factorial_standard(M, SubResult),
    Result is N * SubResult.

% Basic Tail Recursive Approach with Accumulator
% Pros: Tail recursive, efficient in stack frame usage for large N.
% Cons: Requires understanding of accumulators and tail recursion.
factorial_tail_basic2(N, Result) :-
    factorial_tail_basic3(N, 1, Result).
factorial_tail_basic3(0, Accumulator, Accumulator).
factorial_tail_basic3(N, Accumulator, Result) :-
    N > 0,
    NewAccumulator is N * Accumulator,
    M is N - 1,
    factorial_tail_basic(M, NewAccumulator, Result).

% Factorial with between/3 Predicate
% Pros: Utilizes between/3 predicate, providing a clear range.
% Cons: Not tail recursive, can be inefficient for large N.
factorial_between(N, Result) :-
    factorial_between(N, 1, Result).
factorial_between(0, Product, Product) :- !.
factorial_between(N, Product, Result) :-
    N > 0,
    NewProduct is N * Product,
    Next is N - 1,
    factorial_between(Next, NewProduct, Result).

% Factorial using findall/3 and product/2
% Pros: Utilizes findall/3 to generate a list, making it more adaptable.
% Cons: Generates a list of all numbers from 1 to N, can be memory-intensive for large N.
product(List, Product) :-
    foldl(multiply, List, 1, Product).
multiply(X, Y, Z) :-
    Z is X * Y.
factorial_findall(N, Result) :-
    N >= 0,
    findall(X, between(1, N, X), List),
    product(List, Result).

% Using between/3 Predicate in Tail Recursive Manner
% Pros: Combines tail recursion with between/3 for clear range definition.
% Cons: Slightly more complex due to the combination of concepts.
factorial_tail_between(N, Result) :-
    factorial_tail_between(N, 1, Result).
factorial_tail_between(0, Product, Product) :- !.
factorial_tail_between(N, Product, Result) :-
    N > 0,
    NewProduct is N * Product,
    Next is N - 1,
    factorial_tail_between(Next, NewProduct, Result).

% Tail Recursion with Explicit Cut
% Pros: Uses explicit cut to avoid unnecessary backtracking, optimizing performance.
% Cons: The use of cut (!) requires caution as it can affect the logic if misplaced.
factorial_tail_cut(N, Result) :-
    factorial_tail_cut(N, 1, Result).
factorial_tail_cut(0, Accumulator, Accumulator) :- !.
factorial_tail_cut(N, Accumulator, Result) :-
    N > 0,
    NewAccumulator is N * Accumulator,
    M is N - 1,
    factorial_tail_cut(M, NewAccumulator, Result).

% Accumulator Version with Explicit Cut
% Pros: Efficient for large N due to tail recursion and avoids unnecessary backtracking due to cut.
% Cons: Requires understanding of both accumulators and the effect of cut on logic.
factorial_acc_cut(N, Result) :-
    factorial_acc_cut(N, 1, Result).
factorial_acc_cut(0, Accumulator, Accumulator) :- !.
factorial_acc_cut(N, Accumulator, Result) :-
    N > 0,
    NewAccumulator is N * Accumulator,
    M is N - 1,
    factorial_acc_cut(M, NewAccumulator, Result).

% Accumulator Version with Guard Clauses
% Pros: Uses guard clauses for clear distinction between base and recursive cases. Efficient for large N.
% Cons: Slightly more verbose due to the explicit guard clauses.
factorial_acc_guard(N, Result) :-
    factorial_acc_guard(N, 1, Result).
factorial_acc_guard(N, Accumulator, Accumulator) :-
    N =< 0.
factorial_acc_guard(N, Accumulator, Result) :-
    N > 0,
    NewAccumulator is N * Accumulator,
    M is N - 1,
    factorial_acc_guard(M, NewAccumulator, Result).

% Tabling Method
% Summary: Uses tabling to store intermediate results, avoiding redundant calculations and improving efficiency.
% Pros: Efficient even for large N due to no recalculations; polynomial time complexity.
% Cons: Uses additional memory to store the intermediate results; support for tabling is not available in all Prolog implementations.
:- table factorial_tabled/2.
factorial_tabled(0, 1) :- !.
factorial_tabled(N, F) :-
    N > 0,
    N1 is N - 1,
    factorial_tabled(N1, F1),
    F is N * F1.

% Memoization (Dynamic Programming) Method
% Summary: Uses memoization to store previously calculated factorials, improving efficiency.
% Pros: Efficient even for large N due to no recalculations.
% Cons: Uses additional memory to store the previously calculated factorials.
:- dynamic was_factorial_memo/2.
factorial_memo(0, 1) :- !.
factorial_memo(N, F) :-
    N > 0,
    ( was_factorial_memo(N, F) -> true ;
      N1 is N - 1,
      factorial_memo(N1, F1),
      F is N * F1,
      assertz(was_factorial_memo(N, F)) ).



% List of all the factorial implementations
factorials([
    factorial_standard,
    factorial_between,
    factorial_findall,
    factorial_tail_basic2,
    factorial_tail_between,
    factorial_tabled,
    factorial_memo,
    factorial_tail_cut,
    factorial_acc_cut,
    factorial_acc_guard
]).

% Utility to run and time each Factorial implementation
time_factorials(N) :-
  retractall(was_factorial_memo(_,_)), % Clear any memoized results
  factorials(Factorials),
  member(Fib, Factorials),
  format('~N~n% =====================================================~n',[]),
  Goal=..[Fib,N,_],
  statistics(walltime, [Start|_]),
  catch(call(Goal),E,format('~N~nError in Goal: ~q ~q ~n',[Goal,E])),
  statistics(walltime, [End|_]),
  Time is End - Start,
  format('~N~n~w(~w) took \t~w ms~n', [Fib, N, Time]),
  fail.


time_factorials(_):-
    format('~N~n% =====================================================~n',[]),!.

% Running the utility with an example, N=30.
%:- writeln(':- time_factorials(61111).').



