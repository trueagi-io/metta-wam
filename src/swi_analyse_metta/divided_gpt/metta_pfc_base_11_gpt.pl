% % File Directive: This directive runs the PFC engine.
% It initiates the process of forward chaining rules.
pfcRun.

% % Predicate: pfcUnFcCheckTriggers/1
% This predicate checks triggers for a fact and attempts to 
% evaluate the associated action if the condition fails.
%
% @param F The fact to check triggers for.
% @example pfcUnFcCheckTriggers(myFact).
pfcUnFcCheckTriggers(F) :-
  % Check if F is of the type 'fact'.
  pfcType(F,fact(_)),
  
  % Make a copy of the fact for safe evaluation.
  copy_term(F,Fcopy),
  
  % Find a trigger associated with the fact.
  pfcCallSystem('$nt$'(Fcopy, Condition, Action)),
  
  % If the condition fails, evaluate the action.
  (\+ pfcCallSystem(Condition)),
  
  % Evaluate the action and provide a context for negation.
  fcEvalLHS(Action, ((\+ F), '$nt$'(F, Condition, Action))),
  
  % Force backtracking to continue checking other triggers.
  fail.

% Base case for pfcUnFcCheckTriggers/1 when no more triggers are left.
pfcUnFcCheckTriggers(_).

% % Predicate: pfcRetractDependantRelations/1
% This predicate removes any dependent relations for a given fact.
%
% @param Fact The fact whose dependent relations are retracted.
% @example pfcRetractDependantRelations(myFact).
pfcRetractDependantRelations(Fact) :-
  % Check the type of the fact.
  pfcType(Fact, Type),
  
  % If the fact is a trigger, remove its support.
  (Type = trigger(_Pos) -> pfcRemOneSupport(P, (_, Fact))
  
  % Otherwise, remove support for a regular fact.
  ; pfcRemOneSupportOrQuietlyFail(P, (Fact, _))),
  
  % Remove the fact if it is no longer supported.
  removeIfUnsupported(P),
  
  % Force backtracking to continue retracting other relations.
  fail.

% Base case for pfcRetractDependantRelations/1 when no more relations are left.
pfcRetractDependantRelations(_).

% % Predicate: removeIfUnsupported/1
% This predicate checks if a given fact is unsupported and removes 
% it if it is not.
%
% @param P The fact to check for support.
% @example removeIfUnsupported(myFact).
removeIfUnsupported(P) :-
   % If P is supported, print a trace message; otherwise, undo it.
   fcSupported(P) -> pfcTraceMsg(fcSupported(P)) ; fcUndo(P).

% % Predicate: fcSupported/1
% This predicate succeeds if the given fact is supported based on the 
% current TMS (Truth Maintenance System) mode.
%
% @param P The fact to check for support.
% @example fcSupported(myFact).
fcSupported(P) :-
  % Ensure the TMS mode is available.
  must_ex(fcTmsMode(Mode)),
  
  % Check if the fact is supported based on the mode.
  supported(Mode, P).

% The supported/2 predicate defines how support is determined
% based on the TMS mode.
supported(local, P) :- !, pfcGetSupport(P, _).
supported(cycles, P) :- !, wellFounded(P).
supported(_, _P) :- true.

% % Predicate: wellFounded/1
% A fact is well-founded if it is supported by the user or by a set of facts 
% and rules, all of which are well-founded.
%
% @param Fact The fact to check for well-foundedness.
% @example wellFounded(myFact).
wellFounded(Fact) :-
  % Start the check with an empty list of descendants.
  wf(Fact, []).

% % Predicate: wf/2
% The main recursive predicate to check if a fact is well-founded.
%
% @param F The fact to check.
% @param Descendants The list of facts that have already been checked.
wf(F, _) :-
  % If the fact is an axiom or an assumption, it is well-founded.
  (axiom(F); assumption(F)),
  !.

wf(F, Descendants) :-
  % Ensure there is no cyclic dependency.
  (\+ memberchk(F, Descendants)),
  
  % Find the facts that support this fact.
  supports(F, Supporters),
  
  % Ensure all the supporters are well-founded.
  wflist(Supporters, [F | Descendants]),
  !.

% % Predicate: wflist/2
% Applies wf/2 to each fact in a list to ensure all are well-founded.
%
% @param L The list of facts to check.
% @param Descendants The current list of descendants.
wflist([], _).
wflist([X | Rest], L) :-
  wf(X, L),
  wflist(Rest, L).

% % Predicate: supports/2
% Finds a justification for a fact by returning the list of supporters.
% Typically, one of the supporters is a rule.
%
% @param F The fact to find supporters for.
% @param ListOfSupporters The list of supporters.
% @example supports(myFact, Supporters).
supports(F, [Fact | MoreFacts]) :-
  % Get the support for the fact and trigger.
  pfcGetSupport(F, (Fact, Trigger)),
  
  % Find the supporters of the trigger.
  triggerSupports(Trigger, MoreFacts).

% % Predicate: triggerSupports/2
% Finds the supporters for a trigger. 
% If the trigger is axiomatic, there are no further supporters.
%
% @param Trigger The trigger to check.
% @param AllSupport The list of supporting facts.
triggerSupports(U, []) :- 
  axiomatic_supporter(U), !.

% triggerSupports/2 attempts to find the supporters of a trigger,
% first using triggerSupports1/2 and falling back to triggerSupports2/2.
triggerSupports(Trigger, AllSupport) :-
  % Attempt to find the supporters with triggerSupports1/2.
  triggerSupports1(Trigger, AllSupport) *-> true ; triggerSupports2(Trigger, AllSupport).

% % Predicate: triggerSupports1/2
% Finds the first level of support for a trigger.
%
% @param Trigger The trigger to check.
% @param AllSupport The list of supporting facts.
triggerSupports1(Trigger, AllSupport) :-
  % Get the support for the trigger.
  pfcGetSupport(Trigger, (Fact, AnotherTrigger)),
  
  % Recursively find supporters for the other triggers.
  (triggerSupports(AnotherTrigger, MoreFacts) *-> true ; MoreFacts = [AnotherTrigger]),
  
  % Combine the facts into a support list.
  [Fact | MoreFacts] = AllSupport.

% % Predicate: triggerSupports2/2
% This is the second method to find support for a trigger, but it is currently disabled.
% The reason for skipping this code is unclear, but the predicate fails immediately.
% This is likely an optimization or a fallback that is no longer needed.
% It remains here for historical purposes.
triggerSupports2(Trigger, AllSupport) :- fail,
  pfcGetSupport(Trigger, (Fact, AnotherTrigger)),
  (triggerSupports(AnotherTrigger, MoreFacts) *-> true ; MoreFacts = [AnotherTrigger]),
  [Fact | MoreFacts] = AllSupport.

% % Predicate: axiomatic_supporter/1
% Defines the types of axiomatic supporters.
%
% @param Var The potential axiomatic supporter.
axiomatic_supporter(Var) :- is_ftVar(Var), !, fail.
axiomatic_supporter(is_ftVar(_)).
axiomatic_supporter(clause_u(_)).
axiomatic_supporter(user(_)).
axiomatic_supporter(U) :- is_file_ref(U), !.
axiomatic_supporter(ax) :- !.

% % Predicate: is_file_ref/1
% Checks if a term is a file reference.
%
% @param A The term to check.
is_file_ref(A) :- compound(A), A = mfl4(_VarNameZ, _, _, _).

% A special case of triggerSupports where the variable is a free term variable (is_ftVar).
triggerSupports(_, Var, [is_ftVar(Var)]) :- is_ftVar(Var), !.