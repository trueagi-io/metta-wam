
:- use_module(library(clpfd)).

%:- table fib/2.

% Define the base cases.
fib(0, 0).
fib(1, 1).
% Define the recursive case using CLP(FD) constraints.
fib(N, F) :-
    N #> 1, % Ensure that N is greater than 1.
    N1 #= N - 1, % Define N1 as N - 1.
    N2 #= N - 2, % Define N2 as N - 2.
    fib(N1, F1), % Recursive call to find the (N-1)th Fibonacci number.
    fib(N2, F2), % Recursive call to find the (N-2)th Fibonacci number.
	F #= F1 + F2. % The Nth Fibonacci number is the sum of the (N-1)th and (N-2)th Fibonacci numbers.

% Example query for finding the Nth Fibonacci number:
% ?- fib(10, F).
% F = 55.

% Example query for finding the position N of a given Fibonacci number:
% ?- fib(N, 55).
% N = 10.
