/**
 * trigger_trigger1/2
 * 
 * @param Trigger Term that triggers the body evaluation
 * @param Body The body to evaluate when the trigger is met
 * 
 * This predicate copies the Trigger term, then attempts to evaluate it.
 * It also associates the Trigger with its original form through `'$pt$'`.
 * The `fail/0` predicate ensures backtracking, so the process can be repeated if needed.
 *
 * @example
 * ?- trigger_trigger1(my_trigger, my_body).
 */
trigger_trigger1(Trigger, Body) :-
  % Copy the Trigger term into TriggerCopy to preserve the original.
  copy_term(Trigger, TriggerCopy),
  
  % Call the pfc_call/1 predicate to attempt proving Trigger.
  pfc_call(Trigger),
  
  % Associate Trigger with its copy and call fcEvalLHS.
  with_current_why(Trigger, fcEvalLHS(Body, (Trigger, '$pt$'(TriggerCopy, Body)))),
  
  % Force backtracking with fail/0.
  fail.

/**
 * pfc_call/1
 * 
 * @param P Term to evaluate.
 * 
 * This predicate handles various cases for P. It attempts to prove facts, handle logic operators,
 * and invoke system predicates. This ensures compatibility with forward chaining logic.
 * 
 * @example
 * ?- pfc_call(my_predicate).
 */
% Check if P is a variable; if so, call pfcFact to attempt proving it as a fact.
pfc_call(P) :- var(P), !, pfcFact(P).

% Ensure P is callable; otherwise, throw an error.
pfc_call(P) :- \+ callable(P), throw(pfc_call(P)).

% Handle the cut operator (!).
pfc_call((!)) :- !, cut_c.

% Handle the truth case.
pfc_call(true) :- !.

% Handle conditional logic with "if-then-else".
pfc_call((A -> B; C)) :- !, pfc_call(A) -> pfc_call(B); pfc_call(C).

% Handle a form of conditional logic using soft cut.
pfc_call((A *-> B; C)) :- !, pfc_call(A) *-> pfc_call(B); pfc_call(C).

% Handle if-then without the else case.
pfc_call((A -> B)) :- !, pfc_call(A) -> pfc_call(B).

% Handle soft cut without the else case.
pfc_call((A *-> B)) :- !, pfc_call(A) *-> pfc_call(B).

% Handle conjunction (A, B).
pfc_call((A, B)) :- !, pfc_call(A), pfc_call(B).

% Handle disjunction (A; B).
pfc_call((A; B)) :- !, pfc_call(A); pfc_call(B).

% Handle negation (\+ A).
pfc_call(\+ (A)) :- !, \+ pfc_call(A).

% Handle arithmetic expressions.
pfc_call((A is B)) :- !, A is B.

% Handle clause lookup.
pfc_call(clause(A, B)) :- !, clause(A, B).
pfc_call(clause(A, B, Ref)) :- !, clause(A, B, Ref).

% Handle the triggering of backward chaining rules (via '$bt$').
pfc_call(P) :-
  % Attempt to trigger backward chaining (bc) rules for P.
  '$bt$'(P, Trigger),
  
  % Get support for the rule that was triggered.
  pfcGetSupport('$bt$'(P, Trigger), S),
  
  % Evaluate the left-hand side of the rule using the trigger and support.
  fcEvalLHS(Trigger, S),
  
  % Fail to backtrack.
  fail.

% Check if P is a system predicate; if so, call it directly.
pfc_call(P) :- predicate_property(P, imported_from(system)), !, call(P).
pfc_call(P) :- predicate_property(P, built_in), !, call(P).

% Handle dynamic predicates by calling them directly.
pfc_call(P) :- \+ predicate_property(P, _), functor(P, F, A), dynamic(F/A), !, call(P).

% Call predicates that have no clauses, directly.
pfc_call(P) :- \+ predicate_property(P, number_of_clauses(_)), !, call(P).

% Handle the current choice point and retrieve clauses to evaluate.
pfc_call(P) :-
  setup_call_cleanup(
    % Save the current choice point for backtracking.
    nb_current('$pfc_current_choice', Was),
    
    % Capture the current prolog choice point and call clauses for P.
    (prolog_current_choice(CP), push_current_choice(CP), clause(P, Condition), pfc_call(Condition)),
    
    % Restore the saved choice point after evaluation.
    nb_setval('$pfc_current_choice', Was)).

/* previously: This version of pfc_call was skipped because the new version is more efficient
 * and handles dynamic predicates better. It was replaced with a more streamlined version above.
 *
 * pfc_call(P) :-
 *   clause(P,true)*-> true ; (clause(P,Condition), Condition\==true,
 *      pfc_call(Condition)).
 */

/**
 * undoable/1
 * 
 * @param A Action to check for undoability.
 * 
 * This predicate succeeds if there is a defined method for undoing action A.
 */
undoable(A) :- fcUndoMethod(A, _).

/**
 * pfc_cache_bc/1
 * 
 * @param P Term to cache backward chaining results for.
 * 
 * This predicate ensures backward chaining rules for P are evaluated and cached.
 * It iterates through all backward chaining triggers and their supports.
 */
pfc_cache_bc(P) :-
  % Iterate over all backward chaining triggers for P.
  forall('$bt$'(P, Trigger),
         % For each trigger, evaluate and cache the left-hand side of the rule.
         forall(pfcGetSupport('$bt$'(P, Trigger), S),
                fcEvalLHS(Trigger, S))).

/**
 * pfc_nf/2
 * 
 * @param In The left-hand side of the rule to normalize.
 * @param Out The normalized form of the rule.
 * 
 * This predicate converts the left-hand side of a PFC rule into a normal form.
 * It may backtrack to produce additional clauses as needed.
 */
pfc_nf(LHS, List) :-
  % Convert the LHS to its first normal form.
  pfc_nf1(LHS, List2),
  
  % Handle negations to produce the final normalized list.
  pfc_nf_negations(List2, List).

/**
 * pfc_nf1/2
 * 
 * @param In The left-hand side of the rule to normalize.
 * @param Out The intermediate normalized form.
 * 
 * This predicate handles specific forms of LHS expressions in PFC rules.
 * It may backtrack into additional clauses.
 */

% Handle variables by returning them as-is.
pfc_nf1(P, [P]) :- var(P), !.

% The next rule handles ... (upward compatibility reasons).