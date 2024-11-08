/* 
  expand_md1/3: Expands meta-data (MD) for various Prolog constructs.
  This predicate dispatches based on different forms of Prolog terms (like `->`, `;`, `call_cleanup`, etc.).
  It applies expansions to both the "then" and "else" branches of conditional expressions, 
  as well as ensuring certain conditions are met for meta-data.
  
  @param MD The meta-data to expand.
  @param InTerm The input term to expand.
  @param OutTerm The resulting expanded term.
*/
expand_md1(MD, (C->A; B), (CC->AA; BB)) :- 
  !, 
  expand_md(MD, A, AA), % Expand the meta-data for the "then" part of the condition
  expand_md(MD, B, BB), % Expand the meta-data for the "else" part of the condition
  expand_must_not_error(C, CC). % Ensure that "C" is not skipped or handled incorrectly

/* 
  Handles disjunctions (C;B). Expands the metadata of both the first and second parts of the disjunction.
*/
expand_md1(MD, (C; B), (CC; BB)) :- 
  !, 
  expand_md(MD, B, BB), % Expand the second part
  expand_must_not_error(C, CC). % Ensure correctness for the first part

/* 
  Handles `locally(C, A)` constructs. Expands `A` inside the locally construct while leaving `C` untouched.
*/
expand_md1(MD, locally(C, A), locally(C, AA)) :- 
  !, 
  expand_md(MD, A, AA).

/* 
  Handles the call_cleanup/2 construct. Ensures both the main goal and the cleanup part are expanded.
*/
expand_md1(MD, call_cleanup(A, B), call_cleanup(AA, BB)) :- 
  !, 
  expand_md(MD, A, AA), 
  expand_md(MD, B, BB).

/* 
  Handles setup_call_cleanup/3. Expands each part: setup, main goal, and cleanup.
*/
expand_md1(MD, setup_call_cleanup(C, A, B), setup_call_cleanup(CC, AA, BB)) :- 
  !, 
  expand_md(MD, C, CC), 
  expand_md(MD, A, AA), 
  expand_md(MD, B, BB).

/* 
  Handles module-qualified predicates (M:P). Expands the predicate part.
*/
expand_md1(MD, M:P, M:AABB) :- 
  !, 
  expand_md(MD, P, AABB).

/* 
  Expands predicates with meta-predicate properties.
  This clause checks if the predicate is meta, and if so, applies the correct argument expansion.
*/
expand_md1(MD, P, AABB) :- 
  predicate_property(P, (meta_predicate(MP))), % Check if it's a meta-predicate
  strip_module(P, _, SP), 
  strip_module(MP, _, SMP), 
  kaggle_arc_1_pred(_, SP), % Check if it belongs to a specific context (e.g., kaggle)
  \+ skippable_built_in(P), % Skip built-in predicates if necessary
  SP =.. [F | Args], 
  SMP =.. [F | Margs], 
  !,
  maplist(expand_meta_predicate_arg(MD), Margs, Args, EArgs), % Expand arguments
  AABB =.. [F | EArgs].

/* 
  Expands goals without removing the must-det condition (which ensures determinism).
*/
expand_md1(MD, A, MDAA) :- 
  \+ remove_must_det(MD), 
  !, 
  expand_goal(A, AA), % Expand the goal
  compound_name_arg(MDAA, MD, AA). % Reconstruct the expanded term with meta-data

/* 
  Default case for goal expansion if other cases don't apply.
*/
expand_md1(_MD, A, AA) :- 
  expand_goal(A, AA).

/* 
  Ensures that certain goals are expanded as "must_not_error". 
  If the must-not-error flag is set, no expansion is done.
*/
expand_must_not_error(C, C) :- 
  remove_must_det(must_not_error), 
  !.

/* 
  Wraps goals with the must_not_error/1 construct if they are not meta-predicates.
*/
expand_must_not_error(C, CC) :- 
  \+ predicate_property(C, meta_predicate(_)), 
  !, 
  CC = must_not_error(C), 
  !.

/* 
  Otherwise, applies meta-data expansion to ensure the goal is correctly handled with must_not_error.
*/
expand_must_not_error(C, CC) :- 
  expand_md(must_not_error, C, CC).

/* 
  kaggle_arc_1_pred/2: Checks if a predicate belongs to a Kaggle-related file and is not imported from another module.
  This is a filtering mechanism to avoid processing predicates from non-relevant files.

  @param M The module.
  @param P The predicate.
*/
kaggle_arc_1_pred(M, P) :- 
  predicate_property(M:P, file(F)), % Check if the predicate has an associated file
  \+ predicate_property(M:P, imported_from(_)), % Ensure it's not imported
  \+ \+ atom_contains(F, 'arc_'), % File name should contain 'arc_'
  \+ atom_contains(F, '_pfc'), % Skip certain files
  \+ atom_contains(F, '_afc'), 
  true. % Always succeeds if the checks are passed

/* 
  skippable_built_in/1: Identifies system built-in predicates that can be skipped during processing.
  Built-in predicates that are ISO or marked notrace are skipped.

  @param MP The meta-predicate to check.
*/
skippable_built_in(MP) :- 
  strip_module(MP, _, P), 
  predicate_property(system:P, built_in), % Check if it's a system built-in
  once(predicate_property(system:P, iso); predicate_property(system:P, notrace)).

% Goal expansion logic follows below...

/* 
  expand_meta_predicate_arg/4: Expands meta-predicate arguments based on their mode ('?', '+', '-', ':', etc.).
  The mode dictates how the argument should be handled.

  @param MD The meta-data.
  @param Mode The argument mode.
  @param Arg The argument to expand.
  @param ExpandedArg The resulting expanded argument.
*/
expand_meta_predicate_arg(_MD, '?', A, A) :- !.
expand_meta_predicate_arg(_MD, '+', A, A) :- !.
expand_meta_predicate_arg(_MD, '-', A, A) :- !.
expand_meta_predicate_arg(MD, ':', A, AA) :- !, expand_md1(MD, A, AA).
expand_meta_predicate_arg(MD, 0, A, AA) :- !, expand_md1(MD, A, AA).
expand_meta_predicate_arg(_MD, _, A, A).

/* previously: ... 
   This dead code was likely related to older goal expansion mechanisms.
   It was skipped as it no longer fits the current architecture.
*/

/* 
  goal_expansion_getter/2: Attempts to retrieve an expanded version of a goal. 
  It handles simple cases directly, and more complex ones by recursively expanding arguments.

  @param Goal The original goal.
  @param Out The expanded goal.
*/
goal_expansion_getter(Goal, O) :- 
  \+ compound(Goal), % If it's not a compound term, return as is
  !, 
  O = Goal.

goal_expansion_getter(I, O) :- 
  md_like(MD), 
  maybe_expand_md(MD, I, O), 
  I \=@= O, % Ensure the expanded goal is different
  !.

goal_expansion_getter(Goal, get_kov(Func, Self, Value)) :- 
  compound(Goal), 
  compound_name_arguments(Goal, '.', [Self, Func, Value]), 
  var(Value), % Check for partially instantiated terms
  !.

goal_expansion_getter(Goal, Out) :- 
  compound_name_arguments(Goal, F, Args), 
  maplist(goal_expansion_getter, Args, ArgsOut), % Recursively expand arguments
  compound_name_arguments(Out, F, ArgsOut).

/* 
  Exports the goal_expansion_getter/2 predicate to be accessible externally.
*/
:- export(goal_expansion_getter/2).

/* 
  Imports the goal_expansion_getter/2 predicate to the system namespace for internal use.
*/
:- system:import(goal_expansion_getter/2).

/* 
  goal_expansion_setter/2: This predicate seems intended to handle "setter" goals, 
  but it fails on non-compound terms.

  @param Goal The goal to set.
  @param O The output goal after setting.
*/
goal_expansion_setter(Goal, _) :- 
  \+ compound(Goal), 
  !, 
  fail. % Non-compound goals cannot be processed

goal_expansion_setter(I,O):- md_like(MD),maybe_expand_md(MD,I,O),I\=@=O,!.
