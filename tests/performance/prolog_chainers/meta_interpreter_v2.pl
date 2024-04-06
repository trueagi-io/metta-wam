:- op(500, xfy, =>).
:- use_module(library(clpfd)).
:- dynamic(natnum/1).

natnum(0).
natnum(s(X)) :-
    natnum(X).

list_length(Ls, L) :-
    list_length_(Ls, 0, L).

list_length_([], L, L).
list_length_([_|Ls], L0, L) :-
    L1 #= L0 + 1,
    list_length_(Ls, L1, L).

mi(true, true).
mi((A, B), (PA, PB)) :-
    mi(A, PA), mi(B, PB).

mi((A; B), (PA; PB)) :-
    mi(A, PA); mi(B, PB).

mi(Goal, built_in(Goal)) :- % Check if the goal is a built-in predicate.
    predicate_property(Goal, built_in),!,
    call(Goal). % Directly call the built-in predicate.

mi(G, P => G) :-
    G \= true,
    G \=  (_,_),
    clause(G, Body), mi(Body, P).

mi2(true, {name: true, children: []}).
mi2((A, B), {name: and, children: [PA, PB]}) :-
    mi2(A, PA), mi2(B, PB).
mi2((A; B), {name: or, children: [PA, PB]}) :-
    mi2(A, PA); mi2(B, PB).

mi2(G, {name: G, children: []}) :- % Check if the goal is a built-in predicate.
    predicate_property(G, built_in),
    call(G). % Directly call the built-in predicate.


mi2(G, {name: G, children: [P]}) :-
    G \= true,
    G \=  (_,_),
    clause(G, Body), mi2(Body, P).

:- use_module(library(logicmoo_utils)).

ppp(Proof):-
 % Use 'format' to structure the output, introducing the proof with a newline and indent.
 format('~N~nProof=~n\t'),
 % Employ double negation to ensure 'Proof' variables are universally quantified, making the output cleaner.
 % 'numbervars' attributes unique numbers to variables, enhancing readability.
 \+ \+ (numbervars(Proof,0,_,[attvars(skip), singletons(true)]),pppt(Proof)).

pppt(Proof):-
  % 'print_tree' is called to visually represent 'Proof' in a tree structure, followed by newlines for separation.
  print_tree(Proof),nl,nl.