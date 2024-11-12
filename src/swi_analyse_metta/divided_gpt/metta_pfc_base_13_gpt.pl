/** <module> PFC Evaluation and Backward Chaining Module
    This module handles PFC (Prolog Forward Chaining) evaluation
    for both the LHS (Left Hand Side) and RHS (Right Hand Side) of rules, 
    including backward chaining and triggers.
*/

/* Copy the parent rule for further modifications */
% @param ParentRule The original rule.
% @param ParentRuleCopy A copy of the parent rule.
copy_term(ParentRule, ParentRuleCopy),

/* Build the right-hand side (RHS) of a rule */
% @param Head The head of the rule.
% @param Rhs The right-hand side of the rule constructed from Head.
buildRhs(Head, Rhs),

/* Get the current user for tracking purposes */
% @param USER The current user of the system.
% This line uses a custom predicate current_why_U to fetch the user. 
% @TODO: Review why '_U' is used here and confirm its purpose.
current_why_U(USER), 

/* Iterate over each LHS in the PFC rule and create a trigger */
% pfcForEach is used to find all matching facts (using lazy_findall)
% for the given LHS, and a trigger is constructed for each one.
pfcForEach(pfc_nf(Body, Lhs),
    (buildTrigger(Lhs, rhs(Rhs), Trigger),
    /* Add the trigger and its associated backward chaining */
    pfcAdd('$bt$'(Head, Trigger), (ParentRuleCopy, USER)))).

/** get_bc_clause(+Head, -Clause)
    Retrieve the backward chaining clause for the given head.
    @param Head The head of the clause to retrieve.
    @param Clause The corresponding clause (HeadC :- BodyC).
*/
get_bc_clause(Head, (HeadC :- BodyC)) :- get_bc_clause(Head, HeadC, BodyC).

/** get_bc_clause(+HeadIn, -HeadC, -Body)
    Recursive form of get_bc_clause to handle negation.
    @param HeadIn The input head, potentially negated (~Head).
    @param HeadC The resulting (possibly negated) head.
    @param Body The body of the backward chaining clause.
*/
get_bc_clause(HeadIn, ~HeadC, Body) :- 
    compound(HeadIn), HeadIn = ~Head, !,
    /* Establish the body for the negated head */
    Body = (awc, 
           (nonvar(HeadC) -> (HeadC = Head, !) ; (HeadC = Head)), 
           pfc_bc_and_with_pfc(~Head)).

get_bc_clause(Head, Head, Body) :-  
    /* Handle the case where Head is not negated */
    Body = (awc, !, pfc_bc_and_with_pfc(Head)).

/* Thread initialization directive */
% This directive initializes a named variable to hold current PFC choices.
% It runs once at thread startup.
:- thread_initialization(nb_setval('$pfc_current_choice', [])).

/** push_current_choice/0
    Push the current Prolog choice point onto the stack.
    This handles cases where PFC cuts are supported.
*/
push_current_choice :- current_prolog_flag(pfc_support_cut, false), !.
push_current_choice :- prolog_current_choice(CP), push_current_choice(CP), !.
push_current_choice(CP) :- 
    nb_current('$pfc_current_choice', Was) -> 
    b_setval('$pfc_current_choice', [CP | Was]) ; 
    b_setval('$pfc_current_choice', [CP]).

/** cut_c/0
    Perform a Prolog cut to the most recent choice point, 
    provided by PFC.
*/
cut_c :- current_prolog_flag(pfc_support_cut, false), !.
cut_c :- must_ex(nb_current('$pfc_current_choice', [CP | _WAS])), prolog_cut_to(CP).

/** fcEvalLHS(+LHS, +Support)
    Evaluate the Left-Hand Side (LHS) of a rule.
    Depending on the form of LHS, different actions are triggered.
    @param LHS The LHS of the rule to evaluate.
    @param Support The supporting context for this evaluation.
*/
fcEvalLHS((Test -> Body), Support) :-
    /* If-then construct: evaluate Test, then proceed with Body */
    !,
    pfcDoAll(pfcCallSystem(Test) -> fcEvalLHS(Body, Support)),
    !.

fcEvalLHS((Test *-> Body), Support) :-
    /* Soft-cut construct: Test, then Body if Test succeeds */
    !,
    pfcDoAll(pfcCallSystem(Test) *-> fcEvalLHS(Body, Support)).

fcEvalLHS(rhs(X), Support) :-
    /* Directly evaluate the RHS */
    !,
    pfcDoAll(pfc_eval_rhs(X, Support)),
    !.

fcEvalLHS(X, Support) :-
    /* If X is a trigger, add it */
    pfcType(X, trigger(_Pos)),
    !,
    pfcAddTrigger(X, Support),
    !.

/* Dead code: commented out snippet of code for snip triggers */
/* previously: 
fcEvalLHS(snip(X), Support) :-
    snip(Support),
    fcEvalLHS(X, Support).
*/

fcEvalLHS(X, _) :-
    /* Unrecognized LHS */
    pfcWarn("Unrecognized item found in trigger body, namely ~p.", [X]).

/** pfc_eval_rhs(+RHS, +Support)
    Evaluate the Right-Hand Side (RHS) of a rule.
    @param RHS The RHS to be evaluated, may consist of multiple parts.
    @param Support The supporting context for this evaluation.
*/
pfc_eval_rhs([], _) :- !.
pfc_eval_rhs([Head | Tail], Support) :-
    /* Recursively evaluate each part of the RHS */
    pfc_eval_rhs1(Head, Support),
    pfc_eval_rhs(Tail, Support).

/** pfc_eval_rhs1(+Fact, +Support)
    Helper to evaluate individual RHS facts or actions.
    @param Fact The fact or action to evaluate.
    @param Support The supporting context.
*/
pfc_eval_rhs1(Fact, S) :- 
    control_arg_types(Fact, Fixed), 
    !, 
    pfc_eval_rhs1(Fixed, S).

pfc_eval_rhs1({Action}, Support) :-
    /* Evaluate a Prolog action */
    !,
    fcEvalAction(Action, Support).

pfc_eval_rhs1(P, _Support) :-
    /* Remove a negated literal */
    pfcNegatedLiteral(P),
    !,
    pfcWithdraw(P).

pfc_eval_rhs1([X | Xrest], Support) :-
    /* Embedded sublist */
    !,
    pfc_eval_rhs([X | Xrest], Support).

pfc_eval_rhs1(Assertion, Support) :-
    /* Assert a fact */
    once_writeq_nl(pfcRHS(Assertion)),
    (must_ex(pfcPost1(Assertion, Support)) *-> true ; 
    pfcWarn("Malformed RHS of a rule: ~p", [Assertion])).

/** fcEvalAction(+Action, +Support)
    Evaluate an action found on the RHS of a rule.
    @param Action The action to evaluate.
    @param Support The supporting context.
*/
fcEvalAction(Action, Support) :-
    pfcCallSystem(Action),
    (undoable(Action)
    -> pfcAddActionTrace(Action, Support)
    ; true).

/** trigger_trigger(+Trigger, +Body, +Support)
    Handle triggers in PFC rules.
    @param Trigger The trigger condition.
    @param Body The body to evaluate if the trigger fires.
    @param Support The supporting context.
*/
trigger_trigger(Trigger, Body, _Support) :-
    trigger_trigger1(Trigger, Body).
trigger_trigger(_, _, _).

/* Dead code: previously used for "presently" triggers */
/* previously: 
trigger_trigger1(presently(Trigger), Body) :-
    !,
    copy_term(Trigger, TriggerCopy),
    pfc_call(Trigger).
*/