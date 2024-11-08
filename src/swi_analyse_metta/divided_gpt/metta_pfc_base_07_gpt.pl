/**
 *  pfcUnique(+Type, +P) is semidet.
 *
 *  Ensures the uniqueness of an assertion of a given Type (e.g., post, pre).
 *  This is used to prevent duplication of assertions, checking if a fact has already been asserted.
 *  Delegates to a helper predicate that includes a default value for the third argument.
 *
 *  @param Type Type of assertion (e.g., post, pre).
 *  @param P Assertion term to check for uniqueness.
 */
pfcUnique(Type, P) :- pfcUnique(Type, P, true).

/* previously: pfcUnique(post,Head,Tail):- !, \+ is_clause_asserted(Head,Tail).
   This code was used to prevent asserting a clause that already exists but was commented out due to performance improvements in the current implementation.
*/

/**
 *  pfcUnique(+_, +Head, +Tail) is semidet.
 *
 *  Checks if the exact assertion of Head and Tail is already present.
 *  This prevents the assertion of duplicate facts or rules.
 *
 *  @param _ Placeholder for unused Type argument.
 *  @param Head Head of the rule to check.
 *  @param Tail Body of the rule to check.
 */
pfcUnique(_, Head, Tail) :- \+ is_asserted_exact(Head, Tail), !.

/*
   previously: This block attempted to verify if an assertion is already made by examining 
   the internal structure of clauses. It was commented out due to its inefficiency and complexity.
   pfcUnique(_,H,B):- \+ is_asserted(H,B),!.
   pfcUnique(_,H,B):- \+ (
       clause(H, B, Ref),
       clause(HH, BB, Ref),
       strip_m(HH, HHH),
       HHH=@=H,
       strip_m(BB, BBB),
       BBB=@=B).
*/

/**
 *  pfcSetSearch(+Mode) is det.
 *
 *  Sets the current search mode for forward-chaining inferences.
 *
 *  @param Mode The search mode (e.g., direct, depth, breadth, etc.).
 */
pfcSetSearch(Mode) :- pfcSetVal(pfcSearch(Mode)).

/**
 *  pfcGetSearch(-Mode) is det.
 *
 *  Retrieves the current search mode for forward-chaining inferences.
 *
 *  @param Mode Variable to unify with the current search mode.
 */
pfcGetSearch(Mode) :- (t_l:pfcSearchTL(ModeT) -> true ; pfcSearch(ModeT)) -> Mode = ModeT.

/**
 *  pfcEnqueue(+P, +S) is det.
 *
 *  Adds a fact or rule to the processing queue based on the current search mode.
 *
 *  @param P The fact or rule to enqueue.
 *  @param S Optional source or context of the rule.
 */
pfcEnqueue(P, S) :- pfcGetSearch(Mode), !,
    pfcEnqueue(Mode, P, S).

pfcEnqueue(P, S) :- pfcWarn("No pfcSearch mode"),
    pfcEnqueue(direct, P, S).

/**
 *  pfcEnqueue(+Mode, +P, +S) is det.
 *
 *  Enqueues a fact or rule depending on the search mode specified.
 *
 *  @param Mode The search mode (direct, thread, depth, breadth).
 *  @param P Fact or rule to be enqueued.
 *  @param S Optional source/context.
 */
pfcEnqueue(Mode, P, S) :-
    Mode = direct  -> pfcFwd(P) ;
    Mode = thread  -> pfcThreadFwd(P, S) ;
    Mode = depth   -> pfcAsserta(pfcQueue(P), S) ;
    Mode = breadth -> pfcAssert(pfcQueue(P), S) ;
    true           -> pfcWarn("Unrecognized pfcSearch mode: ~p", Mode), pfcEnqueue(direct, P, S).

/**
 *  pfcRemoveOldVersion(+Rule) is det.
 *
 *  Removes any previous version of the rule if it exists.
 *  This helps to prevent conflicts or redundancy in the database.
 *
 *  @param Rule The rule that may have older versions to remove.
 */
pfcRemoveOldVersion((Identifier::::Body)) :-
  % Warn if Identifier is a variable, as this is unexpected.
  (var(Identifier)
  ->
  pfcWarn("Variable used as a rule name in ~p :::: ~p",
          [Identifier, Body]);
  pfcRemoveOldVersion0(Identifier::::Body)).

/**
 *  pfcRemoveOldVersion0(+Rule) is det.
 *
 *  Helper predicate that removes the old version of a rule.
 *
 *  @param Rule The rule for which older versions may be removed.
 */
pfcRemoveOldVersion0((Identifier::::Body)) :-
  nonvar(Identifier),
  clause((Identifier::::OldBody), _),
  \+(Body = OldBody),
  pfcWithdraw((Identifier::::OldBody)),
  !.
pfcRemoveOldVersion0(_).

/**
 *  with_fc_mode(+Mode, :Goal) is semidet.
 *
 *  Temporarily changes the forward-chaining propagation mode while running the Goal.
 *
 *  @param Mode The forward-chaining mode to use (e.g., direct, thread).
 *  @param Goal The goal to execute under the specified mode.
 */
with_fc_mode(Mode, Goal) :- locally(t_l:pfcSearchTL(Mode), Goal).

/**
 *  pfcThreadFwd(+S, +P) is det.
 *
 *  Forwards a fact/rule P in thread mode, encapsulating the reasoning within the specified source/context S.
 *
 *  @param S Source/context for the forward propagation.
 *  @param P Fact/rule to be forwarded.
 */
pfcThreadFwd(S, P) :-
    with_only_current_why(S,
        % Optionally, keep 'thread' mode active
        call_in_thread(with_fc_mode(thread, (pfcFwd(P))))).

/*
   previously: There were multiple implementations for in_fc_call/1,
   commented out as the code evolved to streamline the selection of the forward-chaining mode.
   These older versions were removed for simplicity.
   in_fc_call(Goal):- with_fc_mode(thread, Goal).
   in_fc_call(Goal):- with_fc_mode(direct, Goal).
*/

/**
 *  pfcRun is det.
 *
 *  Computes the deductive closure of the current database.
 *  The process depends on the search mode, either direct, depth, or breadth.
 */
pfcRun :-
  (\+ pfcGetSearch(direct)),
  pfcStep,
  pfcRun.
pfcRun.

/**
 *  pfcStep is det.
 *
 *  Processes one fact/rule from the forward-chaining queue.
 *  Stops if a halt signal is encountered or if the queue is empty.
 */
pfcStep :-
  % If a halt signal is present, remove it and stop processing.
  pfcRetract(pfcHaltSignal(Msg)),
  pfcTraceMsg(removing(pfcHaltSignal(Msg))),
  !,
  fail.

pfcStep :-
  % Draw conclusions from the next fact in the queue.
  get_next_fact(P),
  pfcdo(pfcFwd(P)),
  !.

/**
 *  get_next_fact(-P) is det.
 *
 *  Retrieves the next fact from the queue for forward-chaining reasoning.
 *
 *  @param P The fact to process.
 */
get_next_fact(P) :-
  select_next_fact(P),
  remove_selection(P).

/**
 *  remove_selection(+P) is det.
 *
 *  Removes a selected fact from the queue after it has been processed.
 *
 *  @param P The fact to remove from the queue.
 */
remove_selection(P) :-
  pfcRetract(pfcQueue(P)),