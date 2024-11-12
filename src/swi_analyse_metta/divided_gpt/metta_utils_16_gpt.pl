/** <module> Map predicates and grid processing utility

This module contains predicates that map over lists or grid structures
and apply predicates to them. It also includes utility predicates for
handling compound terms, lists, and variable grids.

The predicates make use of some advanced techniques such as `maplist/3`
and custom mapping predicates for handling complex data structures. The code 
also contains legacy, commented-out sections for reference, showing 
previous iterations and dead code explanations.

*/

% File directive: specifies the module containing the utility for mapping predicates
% :- module(map_predicates, [map_pred1/4, maybe_mapgrid/3, mapgrid/5]).

/** 
 * map_pred1(+NoCycles, +Pred, +Input, -Output)
 * 
 * This predicate maps the predicate `Pred` over the input `Input` to produce the output `Output`.
 * It supports both lists and compound terms and ensures no cycles are created by keeping track of 
 * visited elements in the `NoCycles` list.
 *
 * @param NoCycles A list to prevent cycles.
 * @param Pred The predicate to apply.
 * @param Input The input data (either list or compound term).
 * @param Output The transformed output.
 *
 * @example
 * ?- map_pred1([], some_predicate, [1, 2, 3], Output).
 * 
 * This applies `some_predicate` to each element of the list `[1, 2, 3]`.
 */

% If the input is a list, apply the predicate to each element using maplist
map_pred1(NoCycles,Pred, IO, OO) :- 
    is_list(IO),   % Check if the input is a list
    !,             % Cut to prevent backtracking
    maplist(map_pred(NoCycles,Pred), IO, OO).  % Apply map_pred for each element in the list

% If the input is a compound term, recursively map the predicate over each argument
map_pred1(NoCycles,Pred, IO, [O|ArgS]) :-  
    IO = [I|Args],  % Destructure the input into head (I) and tail (Args)
    !,              
    map_pred([IO,ArgS|NoCycles],Pred, I, O),  % Apply the predicate to the head
    map_pred0([IO,I|NoCycles],Pred, Args, ArgS).  % Recursively apply to the tail

% Handling compound terms, preserving the functor while applying the predicate to each argument
map_pred1(NoCycles,Pred, P, P1) :-
    compound_name_arguments(P, F, Args),  % Extract the functor and arguments from the compound term
    maplist(map_pred([P|NoCycles],Pred), Args, ArgS),  % Apply predicate to each argument
    compound_name_arguments(P1, F, ArgS).  % Reconstruct the term with the transformed arguments

/* previously: map_pred(_Pred, P, P).
 * This was a base case that was previously used, but now replaced
 * with a more generic implementation.
*/

/*
:- meta_predicate map_pred(2, ?, ?, ?, ?).
map_pred(Pred, P, X, Sk, P1) :- 
    must_be_free(X),  % Ensure X is a free variable
    p2_call(Pred, P, X),  % Call the predicate Pred with P and X
    !, 
    must(Sk = P1),  % Ensure Sk unifies with P1
    !.
map_pred(_Pred, P, _, _, P1) :- 
    is_ftVar(P),  % Check if P is a flexible variable
    !, 
    must(P1 = P),  % Ensure P1 is equal to P
    !.
map_pred(Pred, [P|Args], X, Sk, [P1|ArgS]) :- 
    !, 
    map_pred(Pred, P, X, Sk, P1),  % Apply predicate to head
    !, 
    must(map_pred(Pred, Args, X, Sk, ArgS)),  % Recursively apply to the tail
    !.
map_pred(Pred, P, X, Sk, P1) :- 
    compound(P),  % Check if P is a compound term
    !, 
    compound_name_arguments(P, F, Args),  % Extract functor and arguments
    map_pred(Pred, [F|Args], X, Sk, [Fs|ArgS]),  % Apply predicate to functor and arguments
    !, 
    compound_name_arguments(P1, Fs, ArgS),  % Reconstruct the compound term
    !.
map_pred(_Pred, P, _, _, P).  % Base case, no-op
*/

% Predicate to check if a term is a cons list
is_cons(A) :- 
    compound(A),  % Check if A is a compound term
    A = [_|_].    % Ensure it has the form of a cons (non-empty list)

into_grid_or_var(G,G):- is_cons(G),!.
into_grid_or_var(G,G):- var(G),!.
into_grid_or_var(O,G):- cast_to_grid(O,G,_Uncast),!.

maybe_mapgrid(P2,I,O):- is_grid(I),!,mapgrid(P2,I,O).
maybe_mapgrid(P3,I,O,M):- is_grid(I),!,mapgrid(P3,I,O,M).
maybe_mapgrid(P4,I,O,M,N):- is_grid(I),!,mapgrid(P4,I,O,M,N).

mapgrid(P4,Grid,GridM,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridM,G2),into_grid_or_var(GridN,G3),into_grid_or_var(GridO,G4),mapg_list(P4,G1,G2,G3,G4).
mapg_list(P4,Grid,GridM,GridN,GridO):- is_list(Grid),!,maplist(mapg_list(P4),Grid,GridM,GridN,GridO).
mapg_list(P4,Grid,GridM,GridN,GridO):- call(P4,Grid,GridM,GridN,GridO),!.

mapgrid(P3,Grid,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),into_grid_or_var(GridO,G3),mapg_list(P3,G1,G2,G3).
mapg_list(P3,Grid,GridN,GridO):- is_list(Grid),!,maplist(mapg_list(P3),Grid,GridN,GridO).
mapg_list(P3,Grid,GridN,GridO):- call(P3,Grid,GridN,GridO),!.

mapgrid(P2, Grid,GridN):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),!,mapg_list(P2, G1,G2).
mapg_list(P2, Grid,GridN):- is_list(Grid),!,maplist(mapg_list(P2),Grid,GridN).
mapg_list(P2, Grid,GridN):- p2_call(P2, Grid,GridN),!.

mapgrid(P1,Grid):- into_grid_or_var(Grid,G1),mapg_list(P1,G1).
mapg_list(P1,Grid):- is_list(Grid),!,maplist(mapg_list(P1),Grid).
mapg_list(P1,Grid):- p1_call(P1,Grid),!.


maplist_ignore(_3,H,I,J):- (H==[];I==[],J==[]),!,(ignore(H=[]),ignore(I=[]),ignore(J=[])).
maplist_ignore(P3,H,I,J):- \+ is_list(H),!, ignore(p2_call(call(P3,H),I,J)).
maplist_ignore(P3,[H|Grid],[I|GridN],[J|GridO]):- maplist_ignore(P3,H,I,J), !,maplist_ignore(P3,Grid,GridN,GridO).

maplist_ignore(_2,H,I):- (H==[];I==[]),!,(ignore(H=[]),ignore(I=[])).
maplist_ignore(P2, H,I):- \+ is_list(H),!, ignore(p2_call(P2, H,I)).
maplist_ignore(P2, [H|Grid],[I|GridN]):- maplist_ignore(P2, H,I), !,maplist_ignore(P2, Grid,GridN).

%p1_or(P1,Q1,E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).
