 ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)),!,
  ((term_singletons(O,SS),numbervars(SS,999999999999,_,[attvar(skip),singletons(true)]))).


:- dynamic(pretty_clauses:pp_hook/3). % Declare pp_hook/3 as a dynamic predicate, allowing runtime modifications
:- multifile(pretty_clauses:pp_hook/3). % Declare pp_hook/3 as a multifile predicate, allowing it to be defined across multiple files
:- module_transparent(pretty_clauses:pp_hook/3). % Ensure that pp_hook/3 is transparent, maintaining module encapsulation

pretty_clauses:pp_hook(FS,Tab,S):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),  notrace(catch(arc_pp_hook(FS,Tab,S),_,fail)).


/**
 * arc_pp_hook(+FS, +Tab, +S) is semidet.
 *
 * Pretty printing hook for terms. This predicate handles various types of terms,
 * including ANSI-formatted and grouped terms.
 *
 * @param FS The term being processed.
 * @param Tab The current indentation level.
 * @param S The stream or term to print.
 */
arc_pp_hook(_, Tab, S) :- term_is_ansi(S), !, prefix_spaces(Tab), write_keeping_ansi_mb(S).  % Handle ANSI-formatted terms

/* Previously: arc_pp_hook(_,Tab,S):- is_vm(S),!,prefix_spaces(Tab),!,write('..VM..').
   This code was skipped as it seems to be a specific condition related to a VM term, but is no longer relevant. */

/* Previously: arc_pp_hook(_,  _,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
   Skipped because it explicitly checks for conditions that are handled more efficiently in other parts of the code. */

arc_pp_hook(FS, _, G) :-
    \+ current_prolog_flag(never_pp_hook, true),  % Check if the pp_hook is enabled
    nb_current(arc_can_portray, t),  % Ensure arc_can_portray is active
    current_predicate(is_group/1),  % Check if is_group/1 predicate is defined
    locally(b_setval(pp_parent, FS), print_with_pad(pp_hook_g(G))), !.  % Locally set pp_parent and print the group

/** pp_parent(-PP) is semidet.
 *
 * Retrieves the current parent of a pretty printing process.
 *
 * @param PP The parent term, or an empty list if none is set.
 */
pp_parent(PP) :- nb_current(pp_parent, PP), !.  % Retrieve the current parent
pp_parent([]) :- !.  % Default to an empty list if no parent is set

/* Previously: :- meta_predicate(lock_doing(+,+,0)).
   Modified to support a module-based predicate specifier. */

/**
 * lock_doing(+Lock, +G, :Goal) is det.
 *
 * Prevents recursive execution of the same goal within a locked section.
 * If the goal has already been executed, it won't be re-executed in the same context.
 *
 * @param Lock The lock variable, used to track goals.
 * @param G The goal to lock.
 * @param Goal The actual goal to execute.
 */
:- meta_predicate(lock_doing(+,+,:)).  % Declare lock_doing as a meta-predicate

lock_doing(Lock, G, Goal) :-
    (nb_current(Lock, Was); Was = []), !,  % Check if the lock already has a value
    \+ ((member(E, Was), E == G)),  % Ensure G hasn't been executed before
    locally(nb_setval(Lock, [G | Was]), Goal).  % Execute the goal in a locked section

/** never_let_arc_portray_again is det.
 *
 * Sets a Prolog flag to prevent further portrayal by the arc system.
 */
never_let_arc_portray_again :- set_prolog_flag(never_pp_hook, true), !.  % Disable the pp_hook globally

/** arc_can_portray is semidet.
 *
 * Checks if the arc portrayal system is allowed to portray terms.
 */
arc_can_portray :- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray, t).  % Check if portrayal is allowed

/** arcp:will_arc_portray is semidet.
 *
 * Determines whether the arc system should portray a term.
 * Conditions include ensuring debug mode is off and tracing is disabled.
 */
arcp:will_arc_portray :-
    \+ current_prolog_flag(never_pp_hook, true),
    \+ nb_current(arc_can_portray, f),
    current_prolog_flag(debug, false),  % Ensure debugging is off
    \+ tracing,  % Ensure tracing is off
    flag(arc_portray_current_depth, X, X), X < 3,  % Ensure portrayal depth is within limits
    current_predicate(bfly_startup/0).  % Check if the predicate bfly_startup/0 exists

/** user:portray(+Grid) is semidet.
 *
 * Custom portrayal for grid structures.
 *
 * @param Grid The grid to portray.
 */
user:portray(Grid) :-
    arcp:will_arc_portray,  % Check if portrayal is allowed
    \+ \+ catch(quietly(arc_portray(Grid)), _, fail), !,  % Attempt to portray the grid quietly, catching any errors
    flush_output.

/** pp_hook_g(+S) is semidet.
 *
 * Pretty print hook for a given term. It supports various term types and
 * ensures that ANSI formatting and other conditions are handled appropriately.
 *
 * @param S The term to be pretty printed.
 */
pp_hook_g(S) :- term_is_ansi(S), !, write_keeping_ansi_mb(S).  % Handle ANSI-formatted terms
pp_hook_g(_) :- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray, t), !, fail.  % Fail if portrayal is disabled
pp_hook_g(S) :- term_contains_ansi(S), !, write_nbsp, pp_hook_g0(S).  % Handle terms containing ANSI sequences
pp_hook_g(G) :- \+ plain_var(G), lock_doing(in_pp_hook_g, G, pp_hook_g0(G)).  % Lock and execute pp_hook_g0

/** pp_hook_g0(+S) is semidet.
 *
 * Secondary pretty print hook for ANSI-formatted terms.
 *
 * @param S The term to print.
 */
pp_hook_g0(S) :- term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).  % Handle ANSI-formatted terms
pp_hook_g0(_) :- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray, t), !, fail.  % Fail if portrayal is disabled
pp_hook_g0(_) :- in_pp(bfly), !, fail.  % Skip if in the context of 'bfly'
pp_hook_g0(G) :- wots_hs(S, in_bfly(f, pp_hook_g10(G))), write(S).  % Execute further pretty printing in a 'butterfly' context

/** mass_gt1(+O1) is semidet.
 *
 * Check if the mass of an object is greater than 1.
 *
 * @param O1 The object to check.
 */
mass_gt1(O1) :- into_obj(O1, O2), mass(O2, M), !, M > 1.  % Convert O1 into O2 and check if its mass is greater than 1

/** pp_hook_g10(+G) is semidet.
 *
 * Tertiary pretty print hook that handles more complex structures.
 *
 * @param G The term to print.
 */
pp_hook_g10(G) :- \+ plain_var(G), current_predicate(pp_hook_g1/1), lock_doing(in_pp_hook_g10, G, pp_hook_g1(G)).  % Lock and execute pp_hook_g1

/* Previously: as_grid_string(O,SSS):- is_grid(O),wots_vs(S,print_grid(O)), sformat(SSS,'{  ~w}',[S]).
   Commented out as it may be an alternative approach for grid formatting. */

/** as_grid_string(+O, -SSS) is semidet.
 *
 * Convert an object to a grid-formatted string.
 *
 * @param O The object to convert.
 * @param SSS The resulting string.
 */
as_grid_string(O, SSS) :- wots_vs(S, show_indiv(O)), sformat(SSS, '{  ~w}', [S]).  % Convert object O into a formatted string

/** as_pre_string(+O, -SS) is semidet.
 *
 * Convert an object to a pretty-printed string.
 *
 * @param O The object to convert.
 * @param SS The resulting string.
 */
as_pre_string(O, SS) :- wots_hs(S, show_indiv(O)), strip_vspace(S, SS).  % Convert object O into a pretty-printed string

/** pretty_grid(+O) is semidet.
 *
 * Pretty print a grid structure.
 *
 * @param O The grid to print.
 */
pretty_grid(O) :-
    catch(
        (wots_hs(S, print_grid(O)), strip_vspace(S, SS), ptc(orange, (format('"  ~w  "', [SS])))),
        _, fail), !.  % Try to pretty-print the grid, catching any errors