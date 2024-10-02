    \+ fcSupported(P),
    fcUndo(P),!.

pfcRetractAll(P,_S0) :-
  /* removeIfUnsupported(P): If fact P is unsupported, remove it. */
  removeIfUnsupported(P),
  /* The fail predicate forces backtracking to ensure all instances are removed. */
  fail.

/**
 * This clause succeeds once all instances of P have been removed.
 */
pfcRetractAll(_,_).

/**
 * pfcSupportedBy(+P, +S, -How) 
 * @param P The fact being checked.
 * @param S The support fact.
 * @param How The fact that provides the support.
 * This predicate determines how fact P is supported by another fact S, considering both forwards and backwards links.
 */
pfcSupportedBy(P,S,How) :-
   /* Get the support for P in terms of facts F and T. */
   pfcGetSupport(P,(F,T)),
   /* Check if fact F supports P. If not, check if T supports P. */
   (pfcSupportedBy(F,S,_) -> How = F;
    pfcSupportedBy(T,S,How)).

/**
 * pfcSupportedBy(+P, +S, -How) 
 * If P is equal to S, the support is directly S.
 */
pfcSupportedBy(P,S,How) :- P = S, How = S.

/**
 * pfcRetractAll_v2(+P, +S0) 
 * @param P The fact to retract.
 * @param S0 The original support state.
 * A more complex version of pfcRetractAll, this version also logs support removals and handles 
 * dependencies between facts more thoroughly.
 */
pfcRetractAll_v2(P,S0) :-
  /* Double negation ensures P is handled without committing to side effects too early. */
  \+ \+ pfcWithdraw(P,S0),
  /* Get the support and potential trigger for removal. */
  pfcGetSupport(P,(S,RemoveIfTrigger)),
  /* previously: pfcDebug(pfcPrintf("removing support ~p from ~p",[S,P])) */
  /* Generate a materialized support term for logging purposes. */
  matterialize_support_term((S,RemoveIfTrigger),Sup),
  /* Log the removal of support with a message. */
  pfcTraceMsg('    Removing support: ~p   \n   From: ~p~n',[Sup,P]),
  /* Attempt to remove one instance of support, logging success or failure. */
  (pfcRemOneSupportOrQuietlyFail(P,(S,RemoveIfTrigger))
     -> pfcTraceMsg('    Success removing support: ~p   \n   From: ~p~n',[Sup,P])
     ; (pfcWarn("pfcRemOneSupport/2 Could not find support ~p thus\n    Did not yet pfcRetractAll_v2: ~p",
                [Sup,P]))),
  /* Recursively retract all support for S. */
  pfcRetractAll_v2(S, S0),
  /* Force failure to continue backtracking and processing. */
  fail.

/**
 * When no more instances of P exist, remove it if unsupported.
 */
pfcRetractAll_v2(P,_) :- removeIfUnsupported(P).

/**
 * pfcRemove(+Fact) 
 * @param Fact The fact to remove.
 * User-facing predicate for retracting support for a fact P. It behaves like pfcRetractAll but may apply additional 
 * "forceful" measures like pfcBlast to ensure retraction.
 */
pfcRemove(Fact) :-
  /* Validate and fix the control argument types. */
  control_arg_types(Fact,Fixed), !, pfcRemove(Fixed).

pfcRemove(P) :-
  /* Retract all instances of P. */
  pfcRetractAll(P),
  /* If P still exists, apply pfcBlast to force retraction. */
  pfc_call(P) -> pfcBlast(P) ; true.

/**
 * pfcBlast(+F) 
 * @param F The fact to retract forcefully.
 * Retract fact F and remove any dependent facts, ensuring complete removal from the database.
 */
pfcBlast(F) :-
  /* Remove any remaining supports for F. */
  pfcRemoveSupports(F),
  /* Undo F by performing any necessary retraction or action cleanup. */
  fcUndo(F).

/**
 * pfcRemoveSupports(+F) 
 * @param F The fact to remove supports for.
 * This predicate removes all remaining supports for a fact F, issuing warnings as necessary.
 */
pfcRemoveSupports(F) :-
  /* Attempt to remove one support at a time, logging warnings if supports still exist. */
  pfcRemOneSupport(F,S),
  pfcWarn("~p was still supported by ~p (but no longer)",[F,S]),
  /* Fail to trigger backtracking and handle all supports. */
  fail.

pfcRemoveSupports(_).

/**
 * pfcRemoveSupportsQuietly(+F) 
 * @param F The fact to remove supports for.
 * Similar to pfcRemoveSupports but operates quietly, without issuing warnings.
 */
pfcRemoveSupportsQuietly(F) :-
  pfcRemOneSupport(F,_),
  fail.
pfcRemoveSupportsQuietly(_).

/**
 * fcUndo(+X) 
 * @param X The action or fact to undo.
 * This predicate undoes the fact X, either by reversing an action or retracting a fact from the database.
 */
fcUndo(pfcAction(A)) :-
  /* previously: Find and execute a method to undo the action A. */
  /* Remove action trace and undo. */
  !, pfcRemActionTrace(pfcAction(A)).

fcUndo('$pt$'(/*Key,*/Head,Body)) :-
  /* Undo a positive trigger by retracting it from the database. */
  !,
  (retract('$pt$'(/*Key,*/Head,Body))
    -> unFc('$pt$'(Head,Body))
     ; pfcWarn("Trigger not found to retract: ~p",['$pt$'(Head,Body)])).

fcUndo('$nt$'(Head,Condition,Body)) :-
  /* Undo a negative trigger by retracting it from the database. */
  !,
  (retract('$nt$'(Head,Condition,Body))
    -> unFc('$nt$'(Head,Condition,Body))
     ; pfcWarn("Trigger not found to retract: ~p",['$nt$'(Head,Condition,Body)])).

fcUndo(Fact) :-
  /* Undo a general fact by retracting it and logging the removal. */
  retract(Fact),
  pfcTraceRem(Fact),
  unFc(Fact).

/**
 * unFc(+P) 
 * @param P The fact to "un-forward-chain" from.
 * This predicate undoes the forward chaining effects of a fact P, removing dependent relations and 
 * cleaning up associated triggers.
 */
unFc(F) :-
  /* Retract all dependent relations of F. */
  pfcRetractDependantRelations(F),
  /* Perform additional cleanup checks. */
  unFc1(F).

/**
 * unFc1(+P) 
 * @param P The fact to check for triggers.
 * This helper predicate checks triggers and ensures consistency after a fact has been removed.
 */
unFc1(F) :-
  pfcUnFcCheckTriggers(F),
  % is this really the right place for pfcRun<?
  