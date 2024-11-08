/* 
  PLDoc header for pfcDefault/2 predicate
  pfcDefault(+GeneralTerm, +Default)
  Ensures that if a clause for the GeneralTerm is present, it remains unchanged.
  Otherwise, it asserts the Default value for the term.
  @param GeneralTerm - The term to check in the database.
  @param Default - The default term to assert if GeneralTerm is not found.
*/
pfcDefault(GeneralTerm,Default) :-
  /* Checks if the GeneralTerm clause exists and is true. If so, do nothing. */
  clause(GeneralTerm,true) -> true 
  /* Otherwise, assert the Default term into the database. */
  ; assert(Default).

/* 
  File directive for setting default mode for forward chaining system.
  It checks if there is a set mode, if not, defaults to 'cycles' mode.
  PLDoc header for the directive below.
*/
:- pfcDefault(fcTmsMode(_), fcTmsMode(cycles)).

/* previously: Set the truth maintenance system (TMS) mode to 'cycles'. */

/* 
  File directive for setting default search strategy for PFC (Prolog Forward Chaining).
  PLDoc header for this directive.
*/
:- pfcDefault(pfcSearch(_), pfcSearch(direct)).

/* previously: Default search strategy for forward chaining is 'direct'. */

/* 
  PLDoc header for pfcAdd/1 predicate
  pfcAdd(+P)
  Adds the fact P to the database with support.
  @param P - The fact to be added.
  @example pfcAdd(fact1).
*/
pfcAdd(P) :- 
  /* Retrieves the current "why" context, used for forward reasoning. */
  must_ex(current_why_UU(UU)),
  /* Adds the fact with its support (the reason for it being added). */
  pfcAdd(P, UU).

/* previously: pfcAdd predicate with unused commented code about 'with_current_why'. */

/* previously: Alternate form of pfcAdd, specifically for cases where P is of the form ==>P */

/* 
  PLDoc header for pfcAdd/2 predicate
  pfcAdd(+P, +S)
  Adds the fact P to the database with support S, triggers forward reasoning, 
  and logs warnings if the add fails.
  @param P - The fact to be added.
  @param S - The support for this fact.
  @example pfcAdd(fact1, some_support).
*/
pfcAdd(P,S) :-
  /* Posts the fact P with support S to the database. */
  pfcPost(P,S),
  /* Runs the forward reasoning process (triggers new inferences). */
  pfcRun, !.

/* previously: Catch-all to handle failed adds, with warnings. */
pfcAdd(P,S) :- 
  /* Issues a warning if adding P with support S fails. */
  pfcWarn("pfcAdd(~p,~p) failed", [P,S]).

/* 
  PLDoc header for pfcPost/2 predicate
  pfcPost(+Ps, +S)
  Attempts to post a set of facts to the database.
  @param Ps - The list of facts to be posted.
  @param S - The support for these facts.
  @example pfcPost([fact1, fact2], some_support).
*/
pfcPost(List,S):- 
  /* Calls pfcPost_rev with reversed order to process the facts. */
  pfcPost_rev(S, List).

/* 
  PLDoc header for pfcPost_rev/2 predicate
  pfcPost_rev(+S, +Term)
  Recursively posts facts from a list.
  @param S - The support for the facts.
  @param Term - A list or singleton fact to be processed.
  @example pfcPost_rev(support, [fact1, fact2]).
*/
pfcPost_rev(S,Term) :-
  /* If Term is a list, map over the list and post each fact recursively. */
  is_list(Term)
  /* Maps over each element in the list. */
  -> my_maplist(pfcPost_rev(S), Term)
  /* Otherwise, post the singleton fact. */
  ; pfcPost1(Term, S).

/* 
  PLDoc header for pfcPost1/2 predicate
  pfcPost1(+P, +S)
  Posts a fact and adds it to the forward chaining queue.
  @param P - The fact to be posted.
  @param S - The support for the fact.
*/
pfcPost1(Fact,S) :- 
  /* Ensures that the argument types are correct and fixed before posting. */
  control_arg_types(Fact,Fixed),
  /* Recursively calls itself with the fixed arguments. */
  !, pfcPost1(Fixed, S).

/* previously: Error handling during fact posting using Prolog's occurs_check flag. */

pfcPost1(P,S):- 
  /* Temporarily sets occurs_check flag to true and catches any errors. */
  locally(set_prolog_flag(occurs_check, true),
    catch(pfcPost11(P,S), E, 
      (notrace, wdmsg(P => E), trace))).

/* previously: Handles assertion and uniqueness checking during fact posting. */

pfcPost11(P,S) :-
  /* Adds support for the fact. */
  must_ex(pfcAddSupport(P,S)),
  /* If the fact is unique, it proceeds with further posting; otherwise, issues a warning. */
  (pfcUnique(post, P)
  /* Posts the fact to the database. */
  -> pfcPost2(P,S) 
  /* Issues a warning if the fact is not unique. */
  ; nop(pfcWarn(not_pfcUnique(post, P)))).


pfcPost2(P,S):-
  must_ex(once(\+ \+ is_asserted_exact(P);assert(P))),
  must_ex(pfcTraceAdd(P,S)),
  !,
  must_ex(pfcEnqueue(P,S)),
  !.

is_asserted_exact(MH,B):-
  strip_module(MH,M,H),
  is_asserted_exact(M,H,B).
is_asserted_exact(MHB):-
  strip_module(MHB,M,HB),
  expand_to_hb(HB,H,B),
  is_asserted_exact(M,H,B).
is_asserted_exact(M,H,B):-
    M=MM,
    (MM:clause(M:H,B,Ref)*->true; M:clause(MM:H,B,Ref)),
    %clause_ref_module(Ref),
    clause_property(Ref,module(MM)),
  %module_checks_out
   is_asserted_exact(MM,H,B,Ref).
is_asserted_exact(_,H,B,Ref):-
    clause(CH,CB,Ref),strip_m(CH,HH),HH=@=H,strip_m(CB,BB),cl(HH,BB)=@=cl(H,B).



%pfcPost1(_,_).
%pfcPost1(P,S) :-
 %pfcWarn("pfcPost1: ~p\n (support: ~p) failed",[P,S]).

% %   pfcAddDbToHead(+P,-NewP) is semidet.
% talkes a fact P or a conditioned fact
% (P:-C) and adds the Db context.
%

pfcAddDbToHead(P,NewP) :-
  pfcCallSystem(pfcCurrentDb(Db)),
  (Db=true        -> NewP = P;
   P=(Head:-Body) -> NewP = (Head :- (Db,Body));
   true      -> NewP = (P :- Db)).

:- dynamic(pfcCurrentDb/1).
pfcCurrentDb(true).

% %  pfcUnique(X) is det.
%
% is true if there is no assertion X in the prolog db.
%

pfcUnique(Type,(Head:-Tail)) :- !,pfcUnique(Type,Head,Tail).

