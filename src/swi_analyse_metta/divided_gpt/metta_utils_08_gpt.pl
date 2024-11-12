/** <predicate> is_setter_syntax(+Input, -Object, -Member, -Var, -How)
    @brief Determines if the Input matches the setter syntax for object-member assignments.
    @param Input The input to analyze (set, gset, hset, etc.)
    @param Object The object part of the setter syntax.
    @param Member The member part of the setter syntax.
    @param Var Unused variable that holds placeholder values.
    @param How Specifies whether the setter is 'b' (basic), 'nb' (non-basic), or other methods.
    
    @example
    ?- is_setter_syntax(set(my_object,my_member),Obj,Member,_,How).
    Obj = my_object,
    Member = my_member,
    How = b.
*/

/* Checks if the input is not compound (e.g., not a complex term), if true, fails immediately */
is_setter_syntax(I, _Obj, _Member, _Var, _) :- \+ compound(I), !, fail.

/* Matches a basic setter syntax: set(Object, Member) */
is_setter_syntax(set(Obj, Member), Obj, Member, _Var, b).

/* Matches a non-basic setter syntax: gset(Object, Member) */
is_setter_syntax(gset(Obj, Member), Obj, Member, _Var, nb).

/* Matches a customizable setter syntax: hset(How, Object, Member) */
is_setter_syntax(hset(How, Obj, Member), Obj, Member, _Var, How).

/* Matches a dot syntax variant of set: set(Object.Member) */
is_setter_syntax(set(ObjMember), Obj, Member, _Var, b) :-
    obj_member_syntax(ObjMember, Obj, Member).

/* Matches a dot syntax variant of gset: gset(Object.Member) */
is_setter_syntax(gset(ObjMember), Obj, Member, _Var, nb) :-
    obj_member_syntax(ObjMember, Obj, Member).

/* Matches a dot syntax variant of hset: hset(How, Object.Member) */
is_setter_syntax(hset(How, ObjMember), Obj, Member, _Var, How) :-
    obj_member_syntax(ObjMember, Obj, Member).

/** <predicate> obj_member_syntax(+CompoundObjectMember, -Object, -Member)
    @brief Decomposes Object.Member syntax into Object and Member.
    @param CompoundObjectMember The term representing Object.Member.
    @param Object The object extracted from the compound.
    @param Member The member extracted from the compound.
*/
obj_member_syntax(ObjMember, Obj, Member) :-
    compound(ObjMember), 
    compound_name_arguments(ObjMember, '.', [Obj, Member]), 
    !.

/** <predicate> maybe_expand_md(+MD, +Input, -Output)
    @brief Attempts to expand a goal if it matches the MD pattern.
    @param MD The meta-predicate that defines the mapping.
    @param Input The input term to potentially expand.
    @param Output The expanded version of the input, if applicable.
*/
maybe_expand_md(_MD, I, _) :- \+ compound(I), !, fail.

/* The following lines were previously disabled to avoid certain expansions */
/* previously:
maybe_expand_md(MD, must_det_ll(GoalL), GoalL) :- !.
*/

/* Expands meta goals based on MD pattern */
maybe_expand_md(MD, MDGoal, GoalLO) :- 
    compound_name_arg(MDGoal, MD, Goal), !, 
    expand_md(MD, Goal, GoalLO).

/* Handles maplist expansions for MD */
maybe_expand_md(MD, maplist(P1, GoalL), GoalLO) :- 
    P1 == MD, !, 
    expand_md(MD, GoalL, GoalLO).

/* Skipping duplicate maplist handling case */
maybe_expand_md(MD, maplist(P1, GoalL), GoalLO) :- 
    P1 == MD, !, 
    expand_md(MD, GoalL, GoalLO).

/* Handles sub-term expansion within compound terms */
maybe_expand_md(MD, I, O) :- 
    sub_term(C, I), 
    compound(C), 
    compound_name_arg(C, MD, Goal), 
    compound(Goal), 
    Goal = (_, _),
    once((expand_md(MD, Goal, GoalO), substM(I, C, GoalO, O))), 
    I \=@= O.

/* previously: Skipped this expansion rule due to redundant handling */
/*maybe_expand_md(MD, I, O) :- sub_term(S, I), compound(S), S = must_det_ll(G),
  once(expand_md(MD, S, M)), M \= S,
*/

/** <predicate> expand_md(+MD, +Term, -Expanded)
    @brief Expands a term based on the meta-predicate MD.
    @param MD The meta-predicate identifier.
    @param Term The term to expand.
    @param Expanded The expanded version of the term.
*/
expand_md(_MD, Nil, true) :- Nil == [], !.

/* If the term is not callable, it cannot be expanded */
expand_md(_MD, Var, Var) :- \+ callable(Var), !.

/* Expands a list of terms, recursively expanding head and tail */
expand_md(MD, [A|B], (AA, BB)) :- 
    assertion(callable(A)), 
    assertion(is_list(B)), 
    !, 
    expand_md1(MD, A, AA), 
    expand_md(MD, B, BB).

/* Expands non-list terms */
expand_md(MD, A, AA) :- !, expand_md1(MD, A, AA).

/** <predicate> prevents_expansion(+Term)
    @brief Predicate to check if a term should not be expanded due to trace conditions.
*/
prevents_expansion(A) :- is_trace_call(A).

/** <predicate> is_trace_call(+Term)
    @brief Checks if a term is related to trace debugging, which prevents expansion.
*/
is_trace_call(A) :- A == trace.
is_trace_call(A) :- A == itrace.

/** <predicate> skip_expansion(+Term)
    @brief Skips certain expansions, such as cuts or control structures.
*/
skip_expansion(A) :- var(A), !, fail.
skip_expansion(!).
skip_expansion(false).
skip_expansion(true).

/* Skips expansion for specific compound terms based on functor/arity */
skip_expansion(C) :- compound(C), functor(C, F, A), skip_fa_expansion(F, A).

/** <predicate> skip_fa_expansion(+Functor, +Arity)
    @brief Predicate to skip expansion for certain functor/arity pairs.
*/
skip_fa_expansion(once, 1).
skip_fa_expansion(call, _).
skip_fa_expansion(if_t, 2).

/** <predicate> expand_md1(+MD, +Term, -ExpandedTerm)
    @brief Expands an individual term based on the meta-predicate MD.
*/
expand_md1(_MD, Var, Var) :- \+ callable(Var), !.

/* Skips expansion for cuts or other skipped constructs */
expand_md1(_MD, Cut, Cut) :- skip_expansion(Cut), !.

/* Expands a compound term where the functor matches MD */
expand_md1(MD, MDAB, AABB) :- 
    compound(MDAB), 
    compound_name_arg(MDAB, MD, AB), 
    !, 
    expand_md(MD, AB, AABB).

/* Expands maplist constructs */
expand_md1(MD, maplist(P1, A), md_maplist(MD, P1, A)) :- !.
expand_md1(MD, maplist(P2, A, B), md_maplist(MD, P2, A, B)) :- !.
expand_md1(MD, maplist(P3, A, B, C), md_maplist(MD, P3, A, B, C)) :- !.

/* Expands user-defined maplist constructs */
expand_md1(MD, my_maplist(P1, A), md_maplist(MD, P1, A)) :- !.
expand_md1(MD, my_maplist(P2, A, B), md_maplist(MD, P2, A, B)) :- !.
expand_md1(MD, my_maplist(P3, A, B, C), md_maplist(MD, P3, A, B, C)) :- !.

/* previously:
expand_md1(MD, Goal, O) :- \+ compound(Goal), !, O = must_det_ll(Goal).
expand_md1(MD, (A, B), ((A, B))) :- remove_must_det(MD), prevents_expansion(A), !.
expand_md1(MD, (A, B), must_det_ll((A, B))) :- prevents_expansion(A), !.
*/

/* Expands conjunctions (A, B) into (AA, BB) by recursively expanding both */
expand_md1(MD, (A, B), (AA, BB)) :- !, expand_md(MD, A, AA), expand_md(MD, B, BB).

/* Expands conditional statements (C*->A;B) */
expand_md1(MD, (C*->A;B), (CC*->AA;BB)) :- 
    !, 
    expand_md(MD, A, AA), 
    expand_md(MD, B, BB), 
    expand_must_not_error(C, CC).