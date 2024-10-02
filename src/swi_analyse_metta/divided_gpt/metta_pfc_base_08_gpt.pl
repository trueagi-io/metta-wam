
pfcRemoveSupportsQuietly(pfcQueue(P)),
% Ensures no further backtracking once the fact is processed.
!.

/* remove_selection/1 handles cases where a fact is selected but is not on the PFC queue. */
% PLDoc header
% @param P - The fact that is being checked for removal.
% If the fact P is not found in the PFC queue, it prints a warning message.
remove_selection(P) :-
  brake(pfcPrintf("pfc:get_next_fact - selected fact not on Queue: ~p",
               [P])).

/* select_next_fact/1 is used to identify the next fact to process in the forward-chaining mechanism. */
% PLDoc header
% @param P - The fact to be selected.
% First attempts to use a user-defined predicate to select the next fact.
select_next_fact(P) :-
  pfcSelect(P),
  % If a user-defined selection succeeds, it stops here.
  !.
% If user-defined selection fails, it falls back to the default fact selection mechanism.
select_next_fact(P) :-
  defaultpfcSelect(P),
  % Ensures no backtracking after selection.
  !.

/* defaultpfcSelect/1 is the fallback mechanism to select a fact from the PFC queue. */
% PLDoc header
% @param P - The fact that is selected from the front of the queue.
% Selects a fact from the PFC queue using a system-level call.
defaultpfcSelect(P) :- 
  pfcCallSystem(pfcQueue(P)),
  % Ensures no further backtracking once the selection is done.
  !.

/* pfcHalt/0 stops the forward chaining mechanism and halts the process. */
% PLDoc header
% Halts the forward chaining mechanism due to an unspecified reason.
pfcHalt :-  
  pfcHalt("unknown_reason", []).

/* pfcHalt/1 halts the system with a specific message format. */
% PLDoc header
% @param Format - The format of the halt message.
pfcHalt(Format) :-  
  pfcHalt(Format, []).

/* pfcHalt/2 stops the process with a formatted message and arguments. */
% PLDoc header
% @param Format - The format of the message.
% @param Args - Additional arguments for the formatted message.
pfcHalt(Format, Args) :-
  % Create a formatted halt message.
  format(string(Msg), Format, Args),
  % If a halt signal is already set, log a warning.
  (pfcHaltSignal(Msg) ->
       pfcWarn("pfcHalt finds pfcHaltSignal(~w) already set", [Msg])
     % Otherwise, assert a new halt signal.
     ; assert(pfcHaltSignal(Msg))).

% %
% %  Predicates for manipulating triggers.

% PLDoc header for pfcAddTrigger/2
% @param Trigger - The trigger structure.
% @param Support - The support structure.
% Add a positive trigger ('$pt$') with its body and support.
pfcAddTrigger('$pt$'(Trigger, Body), Support) :-
  % Add a positive trigger and trace the message.
  !,
  pfcTraceMsg('      Adding positive trigger(+) ~p~n', ['$pt$'(Trigger, Body)]),
  % Assert the positive trigger with support.
  pfcAssert('$pt$'(Trigger, Body), Support),
  % Create a copy of the term for further evaluation.
  copy_term('$pt$'(Trigger, Body), Tcopy),
  % Evaluate the trigger.
  pfc_call(Trigger),
  % Evaluate the body of the trigger with the current why explanation.
  with_current_why(Trigger, fcEvalLHS(Body, (Trigger, Tcopy))),
  % Fail to force backtracking to evaluate all triggers.
  fail.

/* Adding negative triggers with body and support */
% PLDoc header for pfcAddTrigger/3 for negative triggers.
% @param Trigger - The trigger structure.
% @param Test - The test condition for the negative trigger.
% @param Body - The body of the negative trigger.
% @param Support - The support structure.
pfcAddTrigger('$nt$'(Trigger, Test, Body), Support) :-
  !,
  % Trace the message for adding a negative trigger.
  pfcTraceMsg('      Adding negative trigger(-): ~p~n       test: ~p~n       body: ~p~n', [Trigger, Test, Body]),
  % Create a copy of the trigger for further evaluation.
  copy_term(Trigger, TriggerCopy),
  % Assert the negative trigger with support.
  pfcAssert('$nt$'(TriggerCopy, Test, Body), Support),
  % If the test condition fails, evaluate the body.
  \+ pfc_call(Test),
  % Evaluate the body with the current why explanation.
  with_current_why(\+ pfc_call(Test), fcEvalLHS(Body, ((\+Trigger), '$nt$'(TriggerCopy, Test, Body)))).

/* Adding backward triggers */
% PLDoc header for pfcAddTrigger/3 for backward triggers.
% @param Trigger - The trigger structure.
% @param Body - The body of the backward trigger.
% @param Support - The support structure.
pfcAddTrigger('$bt$'(Trigger, Body), Support) :-
  !,
  % Assert the backward trigger with support.
  pfcAssert('$bt$'(Trigger, Body), Support),
  % Combine backward and positive triggers for evaluation.
  pfcBtPtCombine(Trigger, Body, Support).

/* Handling unrecognized triggers */
% PLDoc header for pfcAddTrigger/2 when the trigger type is not recognized.
% @param X - The unrecognized trigger.
% @param _Support - The support structure (ignored).
pfcAddTrigger(X, _Support) :-
  % Log a warning for unrecognized trigger.
  pfcWarn("Unrecognized trigger(?) to pfcAddTrigger: ~p", [X]).

/* pfcBtPtCombine/3 combines backward and positive triggers for evaluation. */
% PLDoc header
% @param Head - The head of the trigger.
% @param Body - The body of the trigger.
% @param Support - The support structure.
pfcBtPtCombine(Head, Body, Support) :-
  % Find any positive triggers ('$pt$') with unifying heads and add the instantiated '$bt$' body.
  pfcGetTriggerQuick('$pt$'(Head, _PtBody)),
  % Evaluate the left-hand side of the body.
  fcEvalLHS(Body, Support),
  % Fail to ensure backtracking.
  fail.
pfcBtPtCombine(_, _, _) :- 
  % Succeed if no further backtracking is required.
  !.

/* pfcGetTriggerQuick/1 fetches triggers quickly using clauses or pfc_call. */
% PLDoc header
% @param Trigger - The trigger to fetch.
pfcGetTriggerQuick(Trigger) :-  
  % Attempt to fetch the clause directly, or fall back to pfc_call.
  clause(Trigger, true) *-> true ; pfc_call(Trigger).

/* pfcCallSystem/1 invokes system-level PFC calls. */
% PLDoc header
% @param Trigger - The trigger to call.
pfcCallSystem(Trigger) :-  
  pfc_call(Trigger).

% %
% %  Predicates for manipulating action traces.

% PLDoc header for pfcAddActionTrace/2
% @param Action - The action being traced.
% @param Support - The support structure for the action.
% Add an action trace along with its support.
pfcAddActionTrace(Action, Support) :-
  % Adds the action trace with support.
  pfcAddSupport(pfcAction(Action), Support).

% PLDoc header for pfcRemActionTrace/1
% @param pfcAction(A) - The action to remove from the action trace.
pfcRemActionTrace(pfcAction(A)) :-
  % Find the undo method for the action and call it to remove the action trace.
  fcUndoMethod(A, UndoMethod),
  pfcCallSystem(UndoMethod),
  % Succeed after removing the trace.
  !.

% %
% %  Predicates to remove PFC facts, triggers, action traces, and queue items from the database.

% PLDoc header for pfcRetract/1
% @param X - The fact, trigger, or action trace to retract.
% Retract an arbitrary item (fact, trigger, etc.) from the database.
pfcRetract(X) :-
  % Determine the type of the item (fact, trigger, action, etc.).
  pfcType(X, Type),
  % Retract the item based on its type.
  pfcRetractType(Type, X),
  % Succeed after retracting.
  !.

/* pfcRetractType/2 retracts facts specifically. */
% PLDoc header for pfcRetractType/2 for facts.
% @param Type - The type of item (fact, rule, etc.).
% @param X - The fact to be retracted.
pfcRetractType(fact(_), X) :-
  % Add the database head for the fact and retract it.
  pfcAddDbToHead(X, X2) -> retract(X2) ; retract(X).

pfcRetractType(rule(_),X) :-