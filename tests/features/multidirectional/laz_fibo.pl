%!  lazy_list(:Next, +State0, -List)
%
%   Create a lazy list where the next element is defined by
%
%       call(Next, State0, State1, Head)
%
%   The example below uses this  predicate   to  define  a lazy list
%   holding the Fibonacci numbers. Our state  keeps the two previous
%   Fibonacci numbers.
%
%     ```

:- use_module(library(clpfd)).

fibonacci_numbers(L) :-
    lazy_list(fib, state(-,-), L).

fib(state(-,-), state(0,-), 0) :- !.
fib(state(0,-), state(1,0), 1) :- !.
fib(state(P,Q), state(F,P), F):- F #= P+Q.

%     ```
%
%   The above can be used to retrieve   the Nth Fibonacci number. As
%   fib/2 provides no access  to  the   complete  list  of Fibonacci
%   numbers, this can be used to generate large Fibonacci numbers.
%
%    ```

fib(N, F) :-
   fibonacci_numbers(L),
   nth0(N, L, F0), (F=F0 -> !;(F<F0,!,fail)).


%    ```
