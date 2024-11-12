
    ; ((call(G),copy_term(G,CG,GG)) *->asserta(in_memo_cached(Key,C,GT,CG,GG))
                  ;asserta(in_memo_cached(Key,C,GT,failed,_)))),
  E, (retractall(in_memo_cached(Key,C,GT,_,_)),throw(E))),erase(Started)).




/**
 * Set the nth element in a list.
 *
 * Replaces the element at position `N` in the list with element `E`.
 * 
 * @param N Index of the element to replace.
 * @param Row List in which to replace the element.
 * @param E Element to insert.
 * @param RowMod Modified list.
 * @example 
 * ?- set_nth1(2, [a, b, c], x, L).
 * L = [a, x, c].
 */
set_nth1(1, [_|Row], E, [E|Row]) :- !.
set_nth1(N, [W|Row], E, [W|RowMod]) :-
    /* Decrement the index and recurse until N = 1 */
    Nm1 is N - 1,
    set_nth1(Nm1, Row, E, RowMod).

/**
 * Find all solutions and count them.
 *
 * Uses `findall_set/3` to find the set of all solutions and then counts the 
 * number of solutions found.
 * 
 * @param T Template of the solutions.
 * @param G Goal to be satisfied.
 * @param N Number of solutions.
 * @example 
 * ?- findall_count(X, member(X, [a, b, b, c]), N).
 * N = 3.
 */
findall_count(T, G, N) :- 
    findall_set(T, G, S),
    length(S, N).

/**
 * Find all solutions and return as a set.
 *
 * Uses `findall/3` to collect solutions, then converts the list into a set
 * to remove duplicates.
 * 
 * @param T Template of the solutions.
 * @param G Goal to be satisfied.
 * @param S Set of unique solutions.
 * @example 
 * ?- findall_set(X, member(X, [a, b, b, c]), S).
 * S = [a, b, c].
 */
findall_set(T, G, S) :- 
    findall(T, G, L),
    list_to_set(L, S).

/**
 * Create a list initialized with a specified element.
 *
 * Generates a list of length `N`, where every element is `E`.
 * 
 * @param N Number of elements in the list.
 * @param E Element to initialize the list with.
 * @param List Resulting list.
 * @example 
 * ?- make_list_inited(3, x, L).
 * L = [x, x, x].
 */
make_list_inited(0, _, []) :- !.
make_list_inited(1, E, [E]) :- !.
make_list_inited(N, E, [E|List]) :-
    /* Decrement the counter and recurse */
    Nm1 is N - 1,
    make_list_inited(Nm1, E, List).

/**
 * Retrieve the nth clause of a predicate.
 *
 * @param P Predicate to search for.
 * @param I Index of the clause to retrieve.
 * @example 
 * ?- nth_fact(my_predicate, 2).
 */
nth_fact(P, I) :- 
    clause(P, true, Ref),
    nth_clause(P, I, Ref).

/**
 * Check if a term is nonvar or a compound term.
 *
 * @param C The term to check.
 * @example 
 * ?- nonvar_or_ci(X).
 * false.
 */
nonvar_or_ci(C) :- 
    (nonvar(C); attvar(C)), 
    !.

/**
 * Add information to both test and pair rule sets.
 *
 * @param Info The information to add.
 * @example 
 * ?- add_i('info').
 */
add_i(Info) :-
    quietly((
        tersify(Info, InfoT),
        luser_getval(test_rules, TRules),
        luser_getval(pair_rules, PRules),
        /* Add the tersified Info to both sets */
        nb_set_add(TRules, InfoT),
        nb_set_add(PRules, InfoT),
        nop(pp(cyan, +InfoT))
    )).

/**
 * Append a term `i(F)` to the info and add it.
 *
 * @param F The term to append.
 * @param Info The information to append to.
 * @example 
 * ?- add_i(fact, 'info').
 */
add_i(F, Info) :-
    append_term(i(F), Info, FInfo),
    add_i(FInfo).

/* Skipped old do_action code with explanation */
/* previously: do_action(Info):- guess_pretty(Info),add_i(action,Info),call(Info).
   Explanation: This version was removed because it included a guess_pretty step,
   which is now handled in a different context elsewhere in the program. */

% Proceeding with the new, simpler implementation
do_action(Call) :- 
    !, 
    copy_term(Call, Info), 
    call(Call), 
    add_i(action, Info).

/* Remaining predicates follow similar patterns */

add_action(Info):- add_i(action,Info).
add_note(Info):- add_i(note,Info).
add_indiv(W,Info):- add_i(indiv(W),Info).
add_comparitor(Info):- add_i(comparitor,Info).
show_rules:-
 luser_getval(pair_rules,PRules), maplist(pp(cyan),PRules),
 luser_getval(test_rules,TRules), maplist(pp(blue),TRules),
 !.


sub_atom_value(TestID,A):- sub_term(A,TestID),(atom(A);string(A)).

my_list_to_set(List, Set):- my_list_to_set(List, (=) ,Set).
my_list_to_set_variant(List, Set):- my_list_to_set(List, (=@=) ,Set).
my_list_to_set_cmp(List, Set):- my_list_to_set(List, (=@=) ,Set).

my_list_to_set([E|List],P2, Set):- select(C,List,Rest), p2_call(P2, E,C), !, my_list_to_set([E|Rest],P2, Set).
my_list_to_set([E|List],P2, [E|Set]):-!, my_list_to_set(List,P2, Set).
my_list_to_set([],_,[]).

my_list_to_set_cmp([E|List],C3, Set):- select(C,List,Rest), call(C3,R,E,C),
   R== (=), my_list_to_set_cmp([C|Rest],C3, Set),!.
  my_list_to_set_cmp([E|List],C3, [E|Set]):-!, my_list_to_set_cmp(List,C3, Set).
my_list_to_set_cmp([],_,[]).


contains_nonvar(N,Info):- sub_term(E,Info),nonvar_or_ci(E),E=N,!.

max_min(A,B,C,D):- must_be_free(C),must_be_free(D),max_min0(A,B,C,D).
max_min0(A,B,B,B):- plain_var(A).
max_min0(A,B,A,A):- plain_var(B),!.
max_min0(A,B,C,D):- number(A),number(B), !, ((A > B) -> (C=A, D=B) ; (C=B, D=A)).
max_min0(_,A,A,A):- number(A),!.
max_min0(A,_,A,A):- number(A),!.
max_min0(_,_,_,_).

as_debug(L,G):- as_debug(L,true,G).
as_debug(9,_,_):- !.
as_debug(_,C,G):- ignore(catch((call(C)->wots(S,G),format('~NDEBUG: ~w~N',[S]);true),_,true)).

shall_count_as_same(A,B):- same_term(A,B),!. % unify ok_ok cmatch
shall_count_as_same(A,B):- plain_var(A),!,A==B.
shall_count_as_same(A,B):- atomic(A),!, A=@=B.
shall_count_as_same(A,B):- var(B),!,A=@=B.
shall_count_as_same(A,B):- A=@=B,!.


