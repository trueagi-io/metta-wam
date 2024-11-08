/**
 * @predicate triggerSupports/3
 * Triggers the support of a fact if an axiomatic supporter exists or recursively finds
 * support through other triggers.
 * @param FactIn - The initial fact.
 * @param Trigger - The trigger to check for support.
 * @param OUT - The list of supported facts.
 * @example
 * ?- triggerSupports(someFact, someTrigger, Result).
 * Result = [list_of_facts].
 */
triggerSupports(_, U, []) :- 
    % If U is an axiomatic supporter, terminate the search immediately.
    axiomatic_supporter(U), !.
triggerSupports(FactIn, Trigger, OUT) :-
    % Try to find the support for the Trigger using pfcGetSupport.
    % If a support is found, recursively search for more support.
    pfcGetSupport(Trigger, (Fact, AnotherTrigger)) *->
    ( 
        % Recursively find more support facts.
        triggerSupports(Fact, AnotherTrigger, MoreFacts),
        % Collect the facts into the output list.
        OUT = [Fact | MoreFacts]
    );
    % If no support found with pfcGetSupport, proceed with another method.
    triggerSupports1(FactIn, Trigger, OUT).

/**
 * @predicate triggerSupports1/3
 * Triggers the support for a fact, using the `may_cheat` flag if available.
 * @param FactIn - The initial fact (unused here).
 * @param X - The trigger.
 * @param OUT - The output list containing the trigger.
 */
triggerSupports1(_, X, [X]) :- 
    % If may_cheat flag is true, cheat and return the trigger as the support.
    may_cheat.

/**
 * @predicate may_cheat/0
 * A helper predicate used to allow "cheating" in the support checking.
 */
may_cheat :- true_flag.


% File directive:
% Set occurs_check flag to true when running forward chaining

/**
 * @predicate pfcFwd/1
 * Forward chains from a fact or a list of facts.
 * @param Fact - The fact or list of facts to forward chain from.
 */
pfcFwd(Fact) :- 
    % Fix the control argument types for Fact.
    control_arg_types(Fact, Fixed), 
    !,
    % Forward chain with the fixed argument.
    pfcFwd(Fixed).
pfcFwd(Fact) :-
    % Temporarily set the Prolog flag `occurs_check` to true during forward chaining.
    locally(set_prolog_flag(occurs_check, true), 
    % Forward chain with the original fact.
    pfcFwd0(Fact)).

/**
 * @predicate pfcFwd0/1
 * A helper predicate for pfcFwd/1.
 * If the input is a list, recursively forward chain for each item; otherwise, forward chain for a single fact.
 * @param Fact - A fact or list of facts.
 */
pfcFwd0(Fact) :- 
    % If the input is a list, apply forward chaining to each fact in the list.
    is_list(List) -> 
    my_maplist(pfcFwd0, List); 
    % If not a list, forward chain a single fact.
    pfcFwd1(Fact).

/**
 * @predicate pfcFwd1/1
 * Forward chains for a single fact, checking positive and negative triggers.
 * @param Fact - The fact to forward chain from.
 */
pfcFwd1(Fact) :-
    % Check if the fact triggers a forward chain rule.
    (fc_rule_check(Fact) *-> true; true),
    % Create a copy of the fact for further processing.
    copy_term(Fact, F),
    % Check positive triggers for forward chaining.
    ignore(fcpt(Fact, F)),
    % Check negative triggers for forward chaining.
    ignore(fcnt(Fact, F)).


/**
 * @predicate fc_rule_check/1
 * Special built-in forward chaining for rules.
 * Processes rule patterns such as P==>Q or P<==>Q.
 * @param P - The rule to check.
 */
fc_rule_check((Name::::P==>Q)) :-
    !,
    % Process the rule for forward chaining.
    processRule(P, Q, (Name::::P==>Q)).
fc_rule_check((Name::::P<==>Q)) :-
    !,
    % Process both directions of the bi-directional rule.
    processRule(P, Q, ((Name::::P<==>Q))),
    processRule(Q, P, ((Name::::P<==>Q))).
fc_rule_check((P==>Q)) :-
    !,
    % Process a standard forward chaining rule.
    processRule(P, Q, (P==>Q)).
fc_rule_check((P<==>Q)) :-
    !,
    % Process a bi-directional rule.
    processRule(P, Q, (P<==>Q)),
    processRule(Q, P, (P<==>Q)).
fc_rule_check(('<-'(P, Q))) :-
    !,
    % Define a backward chaining rule.
    pfcDefineBcRule(P, Q, ('<-'(P, Q))).
fc_rule_check(_).


/**
 * @predicate fcpt/2
 * Checks for positive triggers during forward chaining.
 * @param Fact - The fact to be checked.
 * @param F - A copy of the fact.
 */
fcpt(Fact, F) :-
    % Get a quick trigger for the fact.
    pfcGetTriggerQuick('$pt$'(F, Body)),
    % Trace the positive trigger.
    pfcTraceMsg('\n Found positive trigger(+):\n    ~p~n       body: ~p~n', [F, Body]),
    % Get the support for the positive trigger.
    pfcGetSupport('$pt$'(F, Body), Support),
    % Evaluate the left-hand side of the body of the trigger.
    with_current_why(Support, with_current_why(Fact, fcEvalLHS(Body, (Fact, '$pt$'(F, Body))))),
    % Fail to allow backtracking and finding more triggers.
    fail.

/* previously:  commented out variant of fcpt that handles 'presently' predicates, 
   not used due to specific scenario requirements */
% fcpt(Fact, F) :-
%   pfcGetTriggerQuick('$pt$'(presently(F), Body)),
%   fcEvalLHS(Body, (presently(Fact), '$pt$'(presently(F), Body))),
%   fail.

% A fallback to succeed if no more positive triggers are found.
fcpt(_, _).

/**
 * @predicate fcnt/2
 * Checks for negative triggers during forward chaining.
 * @param Fact - The fact to be checked.
 * @param F - A copy of the fact.
 */
fcnt(_Fact, F) :-
    % Check if there is a negative trigger.
    pfc_spft(X, _, '$nt$'(F, Condition, Body)),
    % Call the system to evaluate the condition.
    pfcCallSystem(Condition),
    % Remove the negative trigger support.
    pfcRem_S(X, (_, '$nt$'(F, Condition, Body))),
    % Fail to allow backtracking and finding more negative triggers.
    fail.
fcnt(_, _).

/**
 * @predicate pfcRem_S/2
 * Removes support for a fact and retracts it if no longer supported.
 * @param P - The fact.
 * @param S - The support to be removed.
 */
pfcRem_S(P, S) :-
    % Trace message indicating support removal.
    pfcTraceMsg('    Removing support: ~p from ~p~n', [S, P]),
    % Attempt to remove one support and check if fact is unsupported.
    pfcRemOneSupport(P, S) 
        -> removeIfUnsupported(P)
        ; % If support cannot be removed, issue a warning.
          pfcWarn("pfcRem_S/2 Could not find support ~p to remove from fact ~p", [S, P]).


/**
 * @predicate pfcDefineBcRule/3
 * Defines a backward chaining rule and adds the corresponding '$bt$' triggers.
 * @param Head - The head of the rule.
 * @param Body - The body of the rule.
 * @param ParentRule - The parent rule to which the backward chaining rule belongs.
 */
pfcDefineBcRule(Head, _Body, ParentRule) :-
    % Check if the head of the rule is not an atomic literal.
    (\+ pfcLiteral(Head)),
    % Warn if the backward chaining rule is malformed.
    pfcWarn("Malformed backward chaining rule.  ~p not atomic literal.", [Head]),
    pfcError("caused by rule: ~p", [ParentRule]),
    % Fail if rule is malformed.
    !, fail.

pfcDefineBcRule(Head, Body, ParentRule) :-
    % Add the backward chaining rule to the system (details omitted).
    %...