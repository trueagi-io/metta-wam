  % %  db
  pfcAddDbToHead(X,X2) ->  retract(X2) ; retract(X).


% pfcRetractType/2
% @desc This predicate handles retracting different types of database entries, specifically triggers and actions.
% It first tries to retract X and if successful, calls unFc/1 on it. If X can't be found, it issues a warning.
% @param trigger(Pos) - A trigger identified by Pos.
% @param action - An action to be removed.
% @example pfcRetractType(trigger(pos1), fact_to_retract).
pfcRetractType(trigger(Pos),X) :-
    % Try to retract X. If successful, call unFc/1.
    retract(X)
    -> unFc(X)
    % If the retraction fails, issue a warning.
    ; pfcWarn("Trigger(~p) not found to retract: ~p",[Pos,X]).

% pfcRetractType/2 (special case for action)
% @desc Removes an action trace from the database.
% @param action - The action to be removed.
pfcRetractType(action,X) :-
    % Remove the action trace associated with X.
    pfcRemActionTrace(X).

/* previously: General rule retraction was more flexible, but now this specializes for actions and triggers. */

% pfcAddType1/1
% @desc Adds an item X to the database after determining its type.
% It first checks the type of X, then adds it to the database accordingly.
% @param X - The item to add to the database (could be a fact, rule, trigger, or action).
% @example pfcAddType1(fact(some_fact)).
pfcAddType1(X) :-
    % Determine the type of X.
    pfcType(X,Type),
    % Add the entry to the database (or retract and re-add if necessary).
    pfcAddDbToHead(X,X2),
    % Call the appropriate predicate based on the type.
    pfcAddType(Type,X2).

% pfcAddType/2
% @desc Adds facts, rules, or triggers based on their type to ensure uniqueness.
% @param fact(Type) - A fact of some specific type.
% @param rule(Type) - A rule of some specific type.
% @param trigger(Pos) - A trigger at a specific position.
pfcAddType(fact(Type),X) :-
    % Ensure the fact is unique before asserting it into the database.
    pfcUnique(fact(Type),X),
    % Add the fact to the database.
    assert(X),!.

pfcAddType(rule(Type),X) :-
    % Ensure the rule is unique before asserting it.
    pfcUnique(rule(Type),X),
    % Add the rule to the database.
    assert(X),!.

pfcAddType(trigger(Pos),X) :-
    % If the trigger is unique, add it to the database.
    pfcUnique(trigger(Pos),X)
    -> assert(X)
    % Otherwise, warn that the trigger is not unique but still add it.
    ; (pfcWarn(not_pfcUnique(X)),assert(X)).

% pfcAddType/2 (special case for actions)
% @desc Actions are added without uniqueness checks.
% @param action - The action to be added.
pfcAddType(action,_Action) :-
    % Simply succeed; no further action needed for action types.
    !.

/* previously: Actions were checked for uniqueness, but it was determined unnecessary for performance reasons. */

% pfcWithdraw/1
% @desc Withdraws support for a fact or a list of facts. If a list is provided, it recursively withdraws support for each element.
% @param P - The fact or list of facts to withdraw support for.
% @example pfcWithdraw([fact1, fact2]).
pfcWithdraw(P) :-
    % If P is a list, recursively withdraw support for each element.
    is_list(P),!,
    my_maplist(pfcWithdraw,P).

pfcWithdraw(P) :-
    % Try to match the support mechanism.
    matches_why_UU(UU),
    % Withdraw support for P, using UU as context.
    pfcWithdraw(P,UU).

% pfcWithdraw/2
% @desc Removes support for a fact P, checks if it's still supported, and retracts it if not.
% @param P - The fact to withdraw support for.
% @param S - The specific support term being removed.
pfcWithdraw(P,S) :-
    % Get the support relationship between P and S.
    pfcGetSupport(P,S),
    % Ensure the support term is materialized.
    matterialize_support_term(S,Sup),
    % Trace message for debugging support withdrawal.
    pfcTraceMsg('    Withdrawing direct support: ~p   \n   From: ~p~n',[Sup,P]),
    % Attempt to remove one unit of support.
    (pfcRemOneSupportOrQuietlyFail(P,S)
        % If successful, trace the success.
        -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
        % If not, issue a warning.
        ; pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not pfcRemOneSupport: ~p",[Sup,P])),
    % Check if P is now unsupported and remove it if so.
    removeIfUnsupported(P).

pfcWithdraw(P,S) :-
    % If no matching support was found, trace the failure.
    matterialize_support_term(S,Sup),
    pfcTraceMsg('    No support matching: ~p   \n   For: ~p~n',[Sup,P]),!,
    % Remove P if unsupported.
    removeIfUnsupported(P).

% pfcRetractAll/1
% @desc Withdraws both direct and indirect support for a fact (or list of facts).
% @param P - The fact or list of facts to fully retract support for.
% @example pfcRetractAll([fact1, fact2]).
pfcRetractAll(P) :-
    % If P is a list, iterate over all elements and retract support for each.
    is_list(P),!,
    my_maplist(pfcRetractAll,P).

pfcRetractAll(P) :-
    % Try to match the support mechanism.
    matches_why_UU(UU),
    % Fully retract all support for P using UU as context.
    pfcRetractAll(P,UU).

% pfcRetractAll/2
% @desc Removes all support for a fact and checks if it can still be supported.
% If not, it retracts the fact from the database.
% @param Fact - The fact for which to retract support.
% @param S - The specific support term being removed.
pfcRetractAll(Fact,S) :-
    % Control the argument types and ensure they are fixed for the retraction.
    control_arg_types(Fact,Fixed),!,
    % Perform the full retraction with fixed arguments.
    pfcRetractAll(Fixed,S).

pfcRetractAll(P,S) :-
    % Withdraw support for P. If successful, fail to stop further retractions.
    \+ \+ pfcWithdraw(P,S),
    fail.

pfcRetractAll(P,S) :-
  pfcGetSupport(P,(P2,_)),
  pfcType(P2,fact(_)),
  pfcSupportedBy(P2,S,_How),
   pfcRetractAll(P2),
    \+ fcSupported(P),!,
    fcUndo(P).
pfcRetractAll(P,S) :-
  pfcGetSupport( P,(_,T)),
    pfcGetSupport(T,(P2,_)),
    pfcSupportedBy(P2,S,_How),
    pfcType(P2,fact(_)),
   pfcRetractAll(P2),
    \+ fcSupported(P),!,
    fcUndo(P).
pfcRetractAll(P,S) :-
  fcSupported(P),
  pfcGetSupport(P,(P2,_)),
  pfcSupportedBy(P2,S,_How),
  pfcType(P2,rule(_)),
   pfcRetractAll(P2),