
  include(not_sub_var(RSGoals),SinglesC,KSingles),
  length(KSingles,SL),length(VSingles,SL),my_maplist(=('$VAR'('__')),VSingles),
  subst_2L(KSingles,VSingles,[RTermC,RSGoals],[SRTermC,SRSGoals]),
  subst(SRTermC,{cbg('_')},cbg,SSRTermC),!.




/* number_vars_calc_goals/3 calculates variables and goals for a given term */
% @param Term Input term.
% @param TermC Copy of the term after variable numbering.
% @param [4|SGoals] Numbered goals starting with a 4.
% Uses numbervars with the option singletons(true) to ensure unique variable names.
number_vars_calc_goals(Term, TermC, [4|SGoals]) :-
    /* term_variables/2 extracts the free variables in Term */
    term_variables(Term, Vars),

    /* term_attvars/2 extracts attributed variables in Term */
    term_attvars(Term, Attvars),

    /* copy_term/3 creates a copy of the term and variables, including goals */
    copy_term(Term+Vars+Attvars, TermC+VarsC+AttvarsC, Goals),

    /* numbervars/3 numbers variables in TermC and Goals */
    notrace(catch(numbervars(TermC+Goals, 0, _Ten1, [singletons(true)]), _, fail)),

    /* append/3 appends lists of attributed variables and free variables */
    append([AttvarsC, VarsC, AttvarsC, Vars], Sorted),

    /* sort_goals/3 sorts the goals based on variable ordering */
    sort_goals(Goals, Sorted, SGoals), !.

/* Another variant of number_vars_calc_goals, differing in options */
% @param [5|SGoals] Numbered goals starting with a 5.
number_vars_calc_goals(Term, TermC, [5|SGoals]) :-
    term_variables(Term, Vars),
    term_attvars(Term, Attvars),
    copy_term(Term+Vars+Attvars, TermC+VarsC+AttvarsC, Goals),
    /* numbervars with singletons(false) and attvar(skip) options */
    numbervars(TermC+Goals, 0, _Ten1, [singletons(false), attvar(skip)]),
    append([AttvarsC, VarsC, Attvars, Vars], Sorted),
    sort_goals(Goals, Sorted, SGoals), !.

/* writeg/1 tries to write a term with extra handling */
% @param Term The term to write.
% Uses writeg0 or fallback to ppa for error handling.
writeg(Term) :- 
    ignore(\+ notrace(catch(once(writeg0(Term); ppa(Term)), E, (pp(E), ppa(Term))))), !.

/* writeg0/1 handles writing a term, including attributed variables */
% @param Term The term to write.
% Writes attributed variables and goals if applicable.
writeg0(Term) :-
    term_attvars(Term, Attvars),
    Attvars \== [], !,
    must_det_ll((
        number_vars_calc_goals(Term, TermC, Goals),
        writeg5(TermC), !,
        if_t(Goals \== [], (
            nl_if_needed,
            write(' goals='), 
            call_w_pad_prev(3, az_ansi(print_tree_no_nl(Goals)))
        ))
    )), !.

/* Writes ground terms or invokes numbering for variables */
writeg0(Term) :- 
    \+ ground(Term), 
    \+ \+ must_det_ll((
        numbervars(Term, 0, _Ten1, [singletons(true), attvar(skip)]), 
        writeg5(Term)
    )).

/* If no special handling is needed, just write the term */
writeg0(Term) :- writeg5(Term), !.

/* writeg5 handles specific types of terms for formatted output */
writeg5(X) :- 
    is_ftVar(X), !, write_nbsp, write_nbsp, print(X), write_nbsp.

/* Special handling for 2x2 grids */
writeg5(N=V) :- 
    is_simple_2x2(V), !, 
    print_grid(N, V), 
    writeln(' = '), 
    call_w_pad_prev(2, writeg9(V)).

/* Special handling for grid-like structures */
writeg5(N=V) :- 
    is_gridoid(V), !, 
    print_grid(N, V), 
    writeln(' = '), 
    call_w_pad_prev(2, writeg9(V)).

/* Generic handling of non-variable terms */
writeg5(N=V) :- 
    nl_if_needed, 
    nonvar(N), 
    pp_no_nl(N), 
    writeln(' = '), 
    !, 
    call_w_pad_prev(2, writeg5(V)).

/* Default failure case */
writeg5(_) :- write_nbsp, fail.

/* Recursive term handler */
writeg5(V) :- writeg9(V).

/* writeg8 is a helper function for printing ftVars or variables */
writeg8(X) :- is_ftVar(X), !, print(X).
writeg8(X) :- var(X), !, print(X).
writeg8(X) :- writeq(X).

/* writeg9 handles lists and structured outputs */
writeg9(V) :- is_simple_2x2(V), !, print_simple_2x2(writeg8, V).
writeg9(V) :- is_list(V), nl_if_needed, write('['), !, my_maplist(writeg5, V), write(']').
writeg9(_) :- write_nbsp, write(' \t '), fail.
writeg9(X) :- is_ftVar(X), !, write_nbsp, write_nbsp, print(X).
writeg9(V) :- pp_no_nl(V).

/* previously: alternative writeg5 implementation, now skipped for clarity */
% Kept for legacy reasons, but not used in the current implementation.
/*
writeg5(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg5(V):- is_gridoid(V),!,call_w_pad_prev(2,writeg9(V)).
writeg5(V):- is_list(V),nl_if_needed,write('['),my_maplist(writeg5,V),write(']').
*/

/* arg1_near checks if the first argument of a goal matches a specific variable */
% @param Vars List of variables.
% @param Goal Goal to match against.
% @param Nth Position of the variable in Vars.
arg1_near(Vars, Goal, Nth) :- 
    tc_arg(1, Goal, PreSort), 
    nth1(Nth, Vars, E), 
    E == PreSort, !.

arg1_near(_VarsC, Goal, PreSort) :- 
    tc_arg(1, Goal, PreSort), !.

arg1_near(_VarsC, Goal, Goal).

/* sort_goals uses predsort to order goals based on their variables */
% @param Goals The list of goals.
% @param VarsC The variables for sorting.
% @param SGoals Sorted list of goals.
sort_goals(Goals, VarsC, SGoals) :- 
    predsort(sort_on(arg1_near(VarsC)), Goals, SGoals).

/*

writeg0(Obj):- is_object(Obj),pp(Obj),!.
writeg0(O):- writeg00(O).

writeg00(Term):-
  maybe_term_goals(Term,TermC,Goals),
  writeg00(TermC), call_w_pad(2,writeg00(Goals)),!.
writeg00(N=V):- nl_if_needed,nonvar(N), pp_no_nl(N),writeln(' = '), !, call_w_pad(2,writeg00(V)).