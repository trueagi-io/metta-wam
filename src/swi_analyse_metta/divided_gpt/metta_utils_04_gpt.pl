
   fbug(message_hook(Term, Kind, Lines)),fail.


% PLDoc header for meta_predicates
/** 
 * must_det_ll(:Goal) is det.
 * 
 * This predicate ensures that Goal is deterministic and that no choice points
 * are left after it succeeds. It is a meta-predicate as it operates on other goals.
 *
 * @param Goal The goal to be executed deterministically.
 */
:- meta_predicate(must_det_ll(0)).

/** 
 * must_det_ll1(:P1, :Goal) is det.
 * 
 * This predicate is similar to `must_det_ll/1` but uses an additional predicate P1 
 * during its execution.
 *
 * @param P1 A predicate used as a helper in execution.
 * @param Goal The goal to be executed.
 */
:- meta_predicate(must_det_ll1(1, 0)).

/** 
 * md_failed(:P1, :Goal) is det.
 * 
 * This predicate handles the failure of a goal within a meta-predicate context.
 * It executes P1 and then Goal, managing failure appropriately.
 *
 * @param P1 The first predicate to be executed.
 * @param Goal The goal that might fail.
 */
:- meta_predicate(md_failed(1, 0)).

/** 
 * must_not_error(:Goal) is det.
 * 
 * Ensures that the given Goal does not raise an error. It catches errors and 
 * prevents them from propagating.
 *
 * @param Goal The goal that should not result in an error.
 */
:- meta_predicate(must_not_error(0)).

% This line is commented out because the predicate was likely removed or deprecated.
% % :- meta_predicate(must_det_l(0)).

/* previously: 
   % This flag was disabled. Uncommenting this line would prevent certain debugging features.
   % :- no_xdbg_flags.
*/

/** 
 * wno_must(:Goal) is det.
 * 
 * Runs the given goal but with certain debugging and tracing flags disabled.
 * 
 * @param Goal The goal to execute with modified local settings.
 */
:- meta_predicate(wno_must(0)).

% Set no_must_det_ll and cant_rrtrace flags locally, then call the goal G.
wno_must(G) :- 
    locally(nb_setval(no_must_det_ll, t), % Disable must_det_ll checks locally
    locally(nb_setval(cant_rrtrace, t),   % Disable rrtrace locally
    call(G))).                            % Call the goal

% PLDoc header for md_maplist/3
/** 
 * md_maplist(:MD, :P1, +List) is det.
 * 
 * This predicate maps the meta-predicate MD over a list, applying P1 to each element.
 * 
 * @param MD The meta-predicate to apply to each element of the list.
 * @param P1 A predicate to be applied to each element of the list.
 * @param List The list of elements to process.
 */
md_maplist(_MD, _, []) :- 
    % Base case: empty list, do nothing.
    !.

% Recursive case: Apply MD and P1 to the head of the list, then recurse on the tail.
md_maplist(MD, P1, [H | T]) :- 
    call(MD, call(P1, H)),       % Apply MD and P1 to the head element
    md_maplist(MD, P1, T).       % Recur on the tail

% PLDoc header for md_maplist/4
/** 
 * md_maplist(:MD, :P2, +ListA, +ListB) is det.
 * 
 * Maps a predicate over two lists element-wise.
 * 
 * @param MD The meta-predicate to apply to each pair of elements.
 * @param P2 A predicate to be applied to each pair of elements.
 * @param ListA The first list.
 * @param ListB The second list.
 */
md_maplist(_MD, _, [], []) :- 
    % Base case: both lists are empty.
    !.

% Recursive case: Apply MD and P2 to the heads of the lists, then recurse.
md_maplist(MD, P2, [HA | TA], [HB | TB]) :- 
    call(MD, call(P2, HA, HB)),  % Apply MD and P2 to the head elements of both lists
    md_maplist(MD, P2, TA, TB).  % Recur on the tails

% PLDoc header for md_maplist/5
/** 
 * md_maplist(:MD, :P3, +ListA, +ListB, +ListC) is det.
 * 
 * Maps a predicate over three lists element-wise.
 * 
 * @param MD The meta-predicate to apply to each triplet of elements.
 * @param P3 A predicate to be applied to each triplet of elements.
 * @param ListA The first list.
 * @param ListB The second list.
 * @param ListC The third list.
 */
md_maplist(_MD, _, [], [], []) :- 
    % Base case: all three lists are empty.
    !.

% Recursive case: Apply MD and P3 to the heads of the lists, then recurse.
md_maplist(MD, P3, [HA | TA], [HB | TB], [HC | TC]) :- 
    call(MD, call(P3, HA, HB, HC)),  % Apply MD and P3 to the head elements of all three lists
    md_maplist(MD, P3, TA, TB, TC).  % Recur on the tails of the lists

% The following code is commented out as it was likely part of a debugging process that is no longer needed.
% % must_det_ll(G):- !, once((/*notrace*/(G)*->true;md_failed(P1,G))).

/* previously:
   % This was an old directive to check if the must_det_ll/1 predicate exists before defining it.
   % It was removed because the condition is either unnecessary or handled elsewhere.
   % :- if( \+ current_predicate(must_det_ll/1)).
*/

% If tracing is active, run X once and ensure it's deterministic.
must_det_ll(X) :- 
    tracing, 
    !, 
    once(X).

% Otherwise, call md/2 to execute X within the context of the meta-predicate.
must_det_ll(X) :- 
    md(once, X).

/* previously:
   % This directive ensured that the predicate definition only occurred once.
   % It has been removed as the condition is no longer relevant.
   % :- endif.
*/


md(P1,G):- tracing,!, call(P1,G). % once((call(G)*->true;md_failed(P1,G))).
md(P1,G):- remove_must_det(MD), wraps_each(MD,P1),!,call(G).
md(P1,G):- never_rrtrace,!, call(P1,G).
md(P1,G):- /*notrace*/(arc_html),!, ignore(/*notrace*/(call(P1,G))),!.
%md(P1,X):- !,must_not_error(X).
md(P1,(X,Goal)):- is_trace_call(X),!,call((itrace,call(P1,Goal))).
md(_, X):- is_trace_call(X),!,itrace.
md(P1, X):- nb_current(no_must_det_ll,t),!,call(P1,X).
md(P1,X):- \+ callable(X), !, throw(md_not_callable(P1,X)).
md(P1,(A*->X;Y)):- !,(must_not_error(A)*->md(P1,X);md(P1,Y)).
md(P1,(A->X;Y)):- !,(must_not_error(A)->md(P1,X);md(P1,Y)).
md(P1,(X,Cut)):- (Cut==(!)),md(P1,X),!.
md(MD,maplist(P1,List)):- !, call(MD,md_maplist(MD,P1,List)).
md(MD,maplist(P2,ListA,ListB)):- !, call(MD,md_maplist(MD,P2,ListA,ListB)).
md(MD,maplist(P3,ListA,ListB,ListC)):- !, call(MD,md_maplist(MD,P3,ListA,ListB,ListC)).
md(P1,(X,Cut,Y)):- (Cut==(!)), !, (md(P1,X),!,md(P1,Y)).
md(P1,(X,Y)):- !, (md(P1,X),md(P1,Y)).
%md(P1,X):- /*notrace*/(ncatch(X,_,fail)),!.
%md(P1,X):- conjuncts_to_list(X,List),List\=[_],!,maplist(must_det_ll,List).
md(_,must_det_ll(X)):- !, must_det_ll(X).
md(_,grid_call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,(X,Y,Z)):- !, (md(P1,X)->md(P1,Y)->md(P1,Z)).
%md(P1,(X,Y)):- !, (md(P1,X)->md(P1,Y)).
%md(P1,if_t(X,Y)):- !, if_t(must_not_error(X),md(P1,Y)).
md(P1,forall(X,Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).
md(P1,\+ (X, \+ Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).

md(P1,(X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;md_failed(P1,X;Y)).
md(P1,\+ (X)):- !, (\+ must_not_error(X) -> true ; md_failed(P1,\+ X)).
%md(P1,(M:Y)):- nonvar(M), !, M:md(P1,Y).
md(P1,X):-
  ncatch(must_det_ll1(P1,X),
  md_failed(P1,G,N), % <- ExceptionTerm
   % bubble up and start running
  ((M is N -1, M>0)->throw(md_failed(P1,G,M));(ugtrace(md_failed(P1,G,M),X),throw('$aborted')))),!.
%must_det_ll(X):- must_det_ll1(P1,X),!.