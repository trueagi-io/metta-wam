
%fbug(P):- format(user_error,'~N~p~n',[P]).
:- endif.

substM(T, F, R, R) :- T==F, !. % If the term is exactly the one to replace, return the replacement.
substM(T, _, _, R) :- \+ compound(T), !, R = T. % If it's not a compound term, no replacement is needed.
substM([H1|T1], F, R, [H2|T2]) :- % If it's a list, recursively substitute in the head and tail.
    !, substM(H1, F, R, H2), substM(T1, F, R, T2).
substM(C1, F, R, C2) :- % If it's a compound term, decompose and substitute in the arguments.
    C1 =.. [Fn|A1], substM_l(A1, F, R, A2), !, C2 =.. [Fn|A2].

/* Helper predicate: Substitute within a list of arguments */
substM_l([], _, _, []). % Base case: empty list.
substM_l([H1|T1], F, R, [H2|T2]) :- % Recursive case: substitute in head and tail of the list.
    substM(H1, F, R, H2), substM_l(T1, F, R, T2).

/**
 * @predicate pp_m(+Clause)
 * Pretty-print the clause.
 * This predicate writes the clause `Cl` to the source in a formatted way.
 *
 * @param Clause The Prolog clause to pretty-print.
 */
pp_m(Cl) :- write_src(Cl), !. % Calls a helper to write the clause to the source.
pp_m(C, Cl) :- color_g_mesg(C, write_src(Cl)), !. % Writes the clause with some colored messages.

/* previously: unused code for tracing, now commented out for clarity */
%  notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).

/**
 * @predicate pp_q(+Clause)
 * Pretty-print the clause for queries.
 * Outputs the clause with query-specific formatting and ensures no singleton variables.
 *
 * @param Clause The Prolog clause to pretty-print.
 */
pp_q(Cl) :-
    notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).

/**
 * @predicate ncatch(+Goal, +Error, +Handler)
 * Wrapper around catch/3 for error handling.
 * This predicate catches exceptions and handles them with `Handler`.
 *
 * @param Goal The goal to execute.
 * @param Error The error to catch.
 * @param Handler The handler for the caught error.
 */
ncatch(G, E, F) :- catch(G, E, F).

/**
 * @predicate mcatch(+Goal, +Error, +Handler)
 * Another wrapper for catch/3, used for modular error handling.
 *
 * @param Goal The goal to execute.
 * @param Error The error to catch.
 * @param Handler The handler for the caught error.
 */
mcatch(G, E, F) :- catch(G, E, F).

/* previously: error handling was extended with debugging hooks, removed for clarity */
%mcatch(G,E,F):- catch(G,E,(fbug(G=E),catch(bt,_,fail),fbug(G=E),ignore(call(F)),throw(E))).

/* Directive: Conditional inclusion if predicate `if_t/2` does not exist */
:- if(\+ current_predicate(if_t/2)).
:- meta_predicate(if_t(0, 0)).
/**
 * @predicate if_t(+IfGoal, +ThenGoal)
 * Conditional execution of Prolog goals.
 * If `IfGoal` succeeds, `ThenGoal` is executed.
 *
 * @param IfGoal The condition goal.
 * @param ThenGoal The goal to execute if `IfGoal` succeeds.
 */
if_t(IF, THEN) :- call(call, ignore((IF, THEN))).
:- endif.

/* Directive: Conditional inclusion if predicate `must_ll/1` does not exist */
:- if(\+ current_predicate(must_ll/1)).
:- meta_predicate(must_ll(0)).
/**
 * @predicate must_ll(+Goal)
 * Ensures that the goal succeeds at least once.
 * If the goal fails, it throws an error.
 *
 * @param Goal The goal to execute.
 */
must_ll(G) :- md(call, G) *-> true ; throw(not_at_least_once(G)).
:- endif.

/* Directive: Conditional inclusion if predicate `at_least_once/1` does not exist */
:- if(\+ current_predicate(at_least_once/1)).
:- meta_predicate(at_least_once(0)).
/**
 * @predicate at_least_once(+Goal)
 * Ensures that the goal succeeds at least once, throwing an error if it fails.
 *
 * @param Goal The goal to execute.
 */
at_least_once(G) :- call(G) *-> true ; throw(not_at_least_once(G)).
:- endif.

/* previously: wrapping mechanisms were used, preserved for future refactoring */
%wraps_each(must_ll,call).

/* Skipping `remove_must_det/1` because its functionality is now disabled */
remove_must_det(_) :- !, fail. % Always fails as it's not intended for use anymore.

/* Skipped code for conditionally removing `must_det_ll` calls, commented for clarity */
%remove_must_det(MD):- !.
%remove_must_det(MD):- nb_current(remove_must_det(MD),TF),!,TF==true.

/* This block handles removal of specific terms in compound structures. 
   However, it's not currently in use, but preserved for future needs */
remove_mds(MD, GG, GO) :-
    sub_term(G, GG), compound(G), compound_name_arg(G, MD, GGGG),
    subst001(GG, G, GGGG, GGG), remove_mds(MD, GGG, GO).
remove_mds(_, GG, GG).

/**
 * @predicate never_rrtrace/0
 * Prevent tracing if in CGI environment.
 * Ensures that tracing doesn't interfere when in CGI mode.
 */
never_rrtrace :- nb_current(cant_rrtrace, t), !, notrace.
never_rrtrace :- is_cgi, notrace.

/* Skipped tracing directives, preserved for potential debugging reactivation */
%itrace:- !.
%itrace:- \+ current_prolog_flag(debug,true),!.

/* Trace the main thread, used for debugging */
itrace :- if_thread_main(trace), !.
ibreak :- if_thread_main(((trace, break))).

/* Catch an argument by position, with error handling */
tc_arg(N, C, E) :- 
    catch(arg(N, C, E), Err,
    /* previously: detailed error reporting for debugging, now simplified */
    (bt, fbug(tc_arg(N, C, E) = Err), 
    ((tracing -> true ; trace), break, arg(N, C, E)))).

/**
 * @predicate compound_name_arg(+Compound, +Name, -Arg)
 * Extract the argument of a compound term.
 * If the term is not instantiated, it unifies `Compound` with the functor `Name` and argument `Arg`.
 *
 * @param Compound A compound term or variable.
 * @param Name The functor name.
 * @param Arg The argument to extract.
 */
compound_name_arg(G, MD, Goal) :- var(G), !, atom(MD), G =.. [MD, Goal]. % Case: G is uninstantiated.
compound_name_arg(G, MD, Goal) :- compound(G), !, compound_name_arguments(G, MD, [Goal]). % Case: G is a compound term.

/* File directives for handling user-defined message hooks */
:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).

/* Disabled error hook for debugging, can be re-enabled for more granular error handling */
%user:message_hook(Term, Kind, Lines):- error==Kind, itrace,fbug(user:message_hook(Term, Kind, Lines)),trace,fail.

/**
 * @predicate user:message_hook(+Term, +Kind, +Lines)
 * Custom handler for messages.
 * Can be used to intercept and process specific message types, but currently fails on all messages.
 *
 * @param Term The message term.
 * @param Kind The kind of message (e.g., error, warning).
 * @param Lines The message content.
 */
user:message_hook(Term, Kind, Lines) :-
    fail, error == Kind. % Always fails, not currently used.