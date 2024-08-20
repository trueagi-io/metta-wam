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
fibonacci_numbers(L) :-
    lazy_list(fib, state(-,-), L).

fib(state(-,-), state(0,-), 0) :- !.
fib(state(0,-), state(1,0), 1) :- !.
fib(state(P,Q), state(F,P), F) :-
   F is P+Q.

%     ```
%
%   The above can be used to retrieve   the Nth Fibonacci number. As
%   fib/2 provides no access  to  the   complete  list  of Fibonacci
%   numbers, this can be used to generate large Fibonacci numbers.

fib(N, F) :-
    %fibonacci_numbers(L),
   lazy_list(fib, state(-,-), L),
    nth1(N, L, F).


/*
numbers(N) :-
    lazy_list(next_number, L),

    %fibonacci_numbers(L),
   lazy_list(fib, state(-,-), L),


call(Next, List, Tail)
*/
list_iter(Iter,L) :-
    lazy_iter(Iter,nth0,L).

space_iter(Iter,L) :-
    lazy_iter(Iter,get_next_atom,L).

get_next_atom(_,Space,Atom):- metta_atom(Space,Atom).


lazy_iter(Iter,Next,L) :-
    lazy_list(lazy_iter(Iter,Next), state(0), L).

%lazy_iter(Iter,state(0),state(1),V):- get_item(0,Iter,V).
lazy_iter(Iter,Next,state(N),state(N1),V):- N1 is N +1,call(Next,N,Iter,V).
