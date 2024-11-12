/**
 * goal_expansion_setter(+Goal, -ExpandedGoal)
 *
 * Expands the given Goal into its corresponding ExpandedGoal.
 * This predicate applies specific transformations based on the structure of the goal.
 * It handles setters (goal expansion for setting values).
 *
 * @param Goal The input goal.
 * @param ExpandedGoal The transformed goal after expansion.
 *
 * @example
 * ?- goal_expansion_setter(set_omember(a, b, c), ExpandedGoal).
 * ExpandedGoal = set_omember(b, a, b, c).
 */
goal_expansion_setter(G, GO) :-
    % Removes must_det condition from the goal G.
    remove_must_det(MD), 
    !,  % Cut to prevent backtracking.
    % Removes must_det set and rewrites the goal G to GG.
    remove_mds(MD, G, GG),
    % Recursively process the new goal GG.
    goal_expansion_setter(GG, GO).

/* previously: 
%goal_expansion_setter(GG, GO):- remove_must_det(MD), sub_term(G,GG),compound(G),
% G = must_det_ll(GGGG),subst001(GG,G,GGGG,GGG),!,goal_expansion_setter(GGG,GO).
The above code was skipped because it seems redundant with existing logic,
or has been replaced by better abstractions. */

% Expands a goal involving multiple terms (conjunction of two goals).
% Skipped because it's a simplified case of goal expansion.
%goal_expansion_setter((G1, G2), (O1, O2)):- !, expand_goal(G1,O1), expand_goal(G2,O2),!.

% Handles the case where the goal is set_omember with 4 arguments.
goal_expansion_setter(set_omember(A, B, C, D), set_omember(A, B, C, D)) :- !.

% Handles the case where the goal is set_omember with 3 arguments and adds a default 'b'.
goal_expansion_setter(set_omember(A, B, C), set_omember(b, A, B, C)) :- !.

% Transforms a '.' predicate into a get_kov structure if Value is uninstantiated.
goal_expansion_setter(Goal, get_kov(Func, Self, Value)) :-
    compound(Goal),  % Ensure Goal is a compound term.
    compound_name_arguments(Goal, '.', [Self, Func, Value]),  % Deconstruct '.' predicate.
    var(Value).  % Only proceed if Value is a variable.

% Attempt to expand MD-like patterns.
goal_expansion_setter(I, O) :-
    md_like(MD),  % Check if there's a must_det pattern.
    maybe_expand_md(MD, I, M),  % Attempt to expand it.
    I \=@= M,  % Ensure the new goal M is different from I.
    !,  % Cut to avoid backtracking.
    goal_expansion_setter(M, O).  % Recursively expand the new goal M.

% Expand meta-predicate goals (skipped due to a fail condition and might be handled elsewhere).
goal_expansion_setter(Goal, Out) :-
    predicate_property(Goal, meta_predicate(_)),  % Check if it's a meta-predicate.
    !, fail,  % Fail and skip this clause.
    tc_arg(N1, Goal, P),  % Get the argument at position N1.
    goal_expansion_setter(P, MOut),  % Recursively expand P.
    setarg(N1, Goal, MOut),  % Set the expanded argument.
    !,  % Cut to avoid backtracking.
    expand_goal(Goal, Out).  % Expand the goal.

% Expands goals that follow setter syntax.
goal_expansion_setter(Goal, Out) :-
    tc_arg(N1, Goal, P),  % Get the N1th argument of the goal.
    is_setter_syntax(P, Obj, Member, Var, How),  % Check if it's setter syntax.
    setarg(N1, Goal, Var),  % Modify the argument in the original goal.
    !,  % Cut to avoid backtracking.
    expand_goal((Goal, set_omember(How, Member, Obj, Var)), Out).  % Expand into a set_omember.

% Handles a specific setter case with get_kov.
goal_expansion_setter(Goal, Out) :-
    get_setarg_p1(setarg, I, Goal, P1),  % Extract setarg data.
    compound(I),  % Ensure I is a compound term.
    compound_name_arguments(I, '.', [Self, Func, Value]),  % Deconstruct '.' predicate.
    call(P1, get_kov(Func, Self, Value)),  % Call P1 with the get_kov predicate.
    !,  % Cut to avoid backtracking.
    expand_goal(Goal, Out).  % Expand the goal.

% Another case handling setter syntax expansion.
goal_expansion_setter(Goal, Out) :-
    get_setarg_p1(setarg, I, Goal, P1),  % Extract setarg data.
    is_setter_syntax(I, Obj, Member, Var, How),  % Check setter syntax.
    call(P1, Var),  % Call P1 with Var.
    !,  % Cut to avoid backtracking.
    expand_goal((Goal, set_omember(How, Member, Obj, Var)), Out).  % Expand the goal.

% Export the goal_expansion_setter predicate for use in other modules.
:- export(goal_expansion_setter/2).

% Import the goal_expansion_setter predicate into the system module.
:- system:import(goal_expansion_setter/2).

/* previously: 
system:term_expansion((Head:-Goal),I,(Head:-Out),O):- nonvar(I),  compound(Goal),
goal_expansion_setter(Goal,Out),Goal\=@=Out,I=O,!,nop((print(goal_expansion_getter(Goal-->Out)),nl)).
The above code was skipped because it's commented out, and its logic may have been refactored into more abstract forms elsewhere.
*/

% Handles term expansions for system goals involving setters.
arc_term_expansion1((system:term_expansion((Head:-Body), I, Out, O):-
    nonvar(I),  % Ensure I is not a variable.
    compound(Head),  % Ensure Head is a compound term.
    term_expansion_setter((Head:-Body), Out),  % Apply term expansion setter.
    (Head:-Body) = In, In \== Out,  % Ensure the expanded term is different.
    I = O,  % Set I to O.
    !,  % Cut to avoid backtracking.
    nop((print(term_expansion_setter(In-->Out)), nl)))).  % Optionally print debug information.

/* previously: 
%system:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),
% Goal\==Out,I=O,!,(print(goal_expansion_getter(Goal-->Out)),nl)).
This block was skipped because it deals with goal expansion getter logic, which is handled by the setter logic above.
*/

% Handles goal expansion for user-defined goals (similar to system goals).
arc_term_expansion1((goal_expansion(Goal, I, Out, O):-
    goal_expansion_setter(Goal, Out),  % Apply the setter expansion.
    Goal \== Out,  % Ensure the goal has changed.
    I = O,  % Set I to O.
    !,  % Cut to avoid backtracking.
    nop((print(goal_expansion_setter(Goal-->Out)), nl)))).  % Optionally print debug information.

% Exporting arc_term_expansions/1 predicate.
:- export(arc_term_expansions/1).

% Expands rules for arc_term_expansion when enabled.
arc_term_expansions(H:- (current_prolog_flag(arc_term_expansion, true), B)):-
    arc_term_expansion1(H:-B).  % Apply arc term expansion.

% Exporting enable_arc_expansion/0 predicate.
:- export(enable_arc_expansion/0).

% Enables arc term expansion by asserting new rules.
enable_arc_expansion :-
    forall(arc_term_expansions(Rule),  % For all arc term expansions,
        (strip_module(Rule, M, Rule0),  % Strip the module from the rule.
        nop(u_dmsg(asserta_if_new(Rule, M, Rule0))),  % Optionally print debug info.
        asserta_if_new(Rule))),  % Assert the rule if it's new.
    set_prolog_flag(arc_term_expansion, true).  % Enable the arc term expansion flag.

% Exporting disable_arc_expansion/0 predicate.
:- export(disable_arc_expansion/0).