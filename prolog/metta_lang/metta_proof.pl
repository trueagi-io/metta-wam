
interpC(Goal):-
  interpC(Goal, Proof),
  once(ppt(Proof)).

interpC(Goal, Proof):-
  save_module_pred_info,!,
  interpC(everything_is_good,  Goal, [], _Caller, _Callee, _DetInfo, Proof).

% predicate P1 as the first argument.
%
% Helper predicate to test that everything_is_good (that goal is callable).
everything_is_good(Goal) :- callable(Goal).


% -------------------------------
% interpB/7: Meta-interpreter for a code block in which cuts do not affect the outer code.
%
% This predicate wraps the interpretation of a code block by calling interpD/7.
% It isolates the determinacy effects of cuts and failures within the block.
%
% DetInfo (Determinacy Info) is a structure of the form:
%     detinfo(Cutted, Failed)
% where:
%   - Cutted is bound to 'cutted' if a cut (!) is executed within this block.
%   - Failed is bound to 'failed' if a failure occurs after such a cut.
%
% By isolating DetInfo within interpB/7, any cut inside the block (for example, inside a call)
% is recorded and handled locally. Once interpD/7 returns, interpB/7 checks DetInfo:
%   - If Cutted == cutted, it performs a local cut (using !) to prune alternatives in the block.
%   - If Failed == failed, it forces the block to fail.
%
% Thus, a cut inside a call does not affect the outer code because its effect is contained in DetInfo.
%
% Usage:
%   interpB(P1, Test, Visited, Caller, Callee, _OldDetInfo, Proof).
%
interpB(P1, Test, Visited, Caller, Callee, _OldDetInfo, Proof) :-
    interpD(P1, Test, Visited, Caller, Callee, DetInfo, Proof),
    % DetInfo is expected to be of the form detinfo(Cutted, Failed)
    DetInfo = detinfo(Cutted, Failed),
    (Cutted == cutted -> ! ; true),
    (Failed == failed -> fail ; true).

interpD(P1, Test, Visited, Caller, _OldCallee, DetInfo, Proof) :-
    interpC(P1, Test, Visited, Caller, _Callee, DetInfo, Proof).

% -------------------------------
% interpD/7: Core meta-interpreter for code that may contain cuts.
%
% This predicate performs the actual meta-interpretation of a goal or code block.
% It threads along the Determinacy Info (DetInfo), a structure used to record local
% effects of cuts and subsequent failures.
%
% DetInfo is structured as:
%     detinfo(Cutted, Failed)
% where:
%   - Cutted: is set to 'cutted' if a cut (!) is encountered during interpretation.
%   - Failed: is set to 'failed' if a failure occurs after a cut.
%
% The use of DetInfo lets interpD/7 safely interpret code containing cuts without letting
% those cuts propagate outside the current block. For instance, if a cut is executed inside
% a call, DetInfo records it; then, when control returns to interpB/7, the outer code remains
% unaffected because only the local DetInfo is examined.
%
% The following clauses handle various constructs (e.g., true, !, fail, conjunction, etc.)
% and pass along DetInfo appropriately.
%
% Note: The first clause below ensures that if an uninstantiated variable is encountered,
% an instantiation error is thrown.
%
% Caller and Callee are passed along (and may be updated for atomic goals).
% Visited is a list of goals already processed (to prevent loops).

:- discontiguous(interpC/7).

% If the goal is a variable, throw an instantiation error.
% (Updated error message: previously referred to interpD/6; now updated to interpD/7.)
interpC(_P1, Goal, _Visited, _Caller, _Callee, _DetInfo, _NoProofReturned) :-
    var(Goal), !,
    throw(error(instantiation_error, interpD/7)).

% P1 is our little sanity test that we call before continuing.
% If call(P1, Goal) succeeds, we force a failure (to trigger backtracking)
% else we break out (using break) and continue.
interpC(P1, Goal, _Visited, _Caller, _Callee, _DetInfo, _Proof) :-
    (call(P1, Goal) -> fail ; (!, break)).

% Base case: 'true' succeeds.
interpC(_P1, true, _Visited, _Caller, _Callee, _DetInfo, true) :- !.

% If a failure is bound then start skipping goals.
interpC(_P1, Goal, _Visited, _Caller, _Callee, DetInfo, fail_skipping(DetInfo, Goal)) :-
    DetInfo = detinfo(_Cutted, Failed), Failed == failed, !.
    % We are skipping over Goal now because a failure was signaled.

% Handling the cut (!) operator:
% When a cut is encountered, update DetInfo by binding its first component to 'cutted'.
% We use copy_term/2 to preserve the previous DetInfo state.
interpC(_P1, !, _Visited, _Caller, _Callee, DetInfo, cutted(PrevDetInfo)) :-
    copy_term(DetInfo, PrevDetInfo),
    !,  % commit to this clause immediately
    DetInfo = detinfo(Cutted, _),
    Cutted = cutted.

% Handling fail/0:
% If no cut has occurred (i.e. Cutted is still a variable), simply fail.
interpC(_P1, fail, _Visited, _Caller, _Callee, DetInfo, fail) :-
    DetInfo = detinfo(Cutted, _),
    var(Cutted), !,
    fail.
% Otherwise, record the current state (via copy_term) and then fail.
interpC(_P1, fail, _Visited, _Caller, _Callee, DetInfo, fail(PrevDetInfo)) :-
    copy_term(DetInfo, PrevDetInfo),
    DetInfo = detinfo(_, Failed), !,
    Failed = failed.


interpC(P1, Goal, Visited, Caller, Callee, DetInfo, Proof) :- var(Caller), !,
  strip_module(Goal,Module,_BareGoal),Module = Caller,
  interpC(P1, Goal, Visited, Caller, Callee, DetInfo, Proof).

% Built-in: call/1
% Note: This clause calls interpB/7 to isolate the call block.
interpC(P1, call(Goal), Visited, Caller, Callee, DetInfo, call(Proof)) :- !,
    once(interpC(P1, Goal, Visited, Caller, Callee, DetInfo, Proof)).

% Built-in: once/1 (alternative clause)
interpC(P1, notrace(Goal), Visited, Caller, Callee, DetInfo, notrace(Proof)) :- !,
    notrace(interpC(P1, Goal, Visited, Caller, Callee, DetInfo, Proof)).

interpC(P1, once(Goal), Visited, Caller, Callee, DetInfo, once(Proof)) :- !,
    once(interpC(P1, Goal, Visited, Caller, Callee, DetInfo, Proof)).

% Special handling for @/2.
% When a goal is of the form @(Goal, CallerModule),
% where CallerModule is a module name (and not a goal), recursively meta-interpret Goal using
% CallerModule as the new Caller. The resulting proof is built as @(ProofA, CallerModule),
% which is equivalent to the module-qualified form CallerModule:ProofA.
interpC(P1, @(Goal, CallerModule), Visited, _OldCaller, Callee, DetInfo, @(ProofA, CallerModule)) :- !,
    if_t(var(CallerModule), throw(error(module_instantiation_error, interpD/7))),
    interpC(P1, Goal, Visited, CallerModule, Callee, DetInfo, ProofA).

% Module qualification using the colon operator.
interpC(P1, Caller:Goal, Visited, _OldCaller, Callee, DetInfo, Caller:Proof) :- !,
    if_t(var(Caller), throw(error(module_instantiation_error, interpD/7))),
    interpC(P1, Goal, Visited, Caller, Callee, DetInfo, Proof).

% Conjunction: (A, B)
% Recursively interpret A and B, threading the same Visited list and DetInfo.
interpC(P1, (A, B), Visited, Caller, Callee, DetInfo, and(ProofA, ProofB)) :- !,
    interpD(P1, A, Visited, Caller, Callee, DetInfo, ProofA),
    interpD(P1, B, Visited, Caller, _Callee, DetInfo, ProofB).

% Disjunction: left branch.
interpC(P1, (A; _B), Visited, Caller, Callee, DetInfo, or(left, ProofA)) :-
    interpD(P1, A, Visited, Caller, Callee, DetInfo, ProofA).

% Disjunction: right branch.
interpC(P1, (_A; B), Visited, Caller, Callee, DetInfo, or(right, ProofB)) :- !,
    interpD(P1, B, Visited, Caller, Callee, DetInfo, ProofB).

% Negation: \+ Goal
% Succeeds if Goal cannot be interpreted.
interpC(P1, \+ Goal, Visited, Caller, Callee, DetInfo, naf(Goal)) :- !,
    \+ interpC(P1, Goal, Visited, Caller, Callee, DetInfo, _).

% If-then-else with hard cut: (If -> Then ; Else)
interpC(P1, (If -> Then ; Else), Visited, Caller, Callee, DetInfo, if_then_else(ProofIf, ProofThen, ProofElse)) :- !,
    interpD(P1, If, Visited, Caller, Callee, DetInfo, ProofIf),
    (   interpD(P1, Then, Visited, Caller, Callee, DetInfo, ProofThen)
    ->  true
    ;   ProofThen = failed
    ),
    (   interpD(P1, Else, Visited, Caller, Callee, DetInfo, ProofElse)
    ->  true
    ;   ProofElse = failed
    ).

% If-then-else with soft cut: (If *-> Then ; Else)
interpC(P1, (If *-> Then ; Else), Visited, Caller, Callee, DetInfo, soft_if_then_else(ProofIf, ProofThen, ProofElse)) :- !,
    interpD(P1, If, Visited, Caller, Callee, DetInfo, ProofIf),
    (   interpD(P1, Then, Visited, Caller, Callee, DetInfo, ProofThen)
    ->  true
    ;   ProofThen = failed
    ),
    (   interpD(P1, Else, Visited, Caller, Callee, DetInfo, ProofElse)
    ->  true
    ;   ProofElse = failed
    ).

% If-then without else (hard cut): (If -> Then)
interpC(P1, (If -> Then), Visited, Caller, Callee, DetInfo, if_then(ProofIf, ProofThen)) :- !,
    interpD(P1, If, Visited, Caller, Callee, DetInfo, ProofIf),
    interpD(P1, Then, Visited, Caller, Callee, DetInfo, ProofThen).

% If-then without else (soft cut): (If *-> Then)
interpC(P1, (If *-> Then), Visited, Caller, Callee, DetInfo, soft_if_then(ProofIf, ProofThen)) :- !,
    interpD(P1, If, Visited, Caller, Callee, DetInfo, ProofIf),
    interpD(P1, Then, Visited, Caller, Callee, DetInfo, ProofThen).

% Built-in: findall/3.
% Collect all solutions (each paired with its proof) into lists.
interpC(P1, findall(Template, Goal, List), Visited, Caller, Callee, DetInfo,
         findall(Template, Proofs, List)) :- !,
    findall((Template, P),
            interpB(P1, Goal, Visited, Caller, Callee, DetInfo, P),
            Pairs),
    interpD_p2l(Pairs, Templates, Proofs),
    List = Templates.

% Built-in: forall/2.
% For every solution of Generator, interpret Test and collect the proofs.
interpC(P1, forall(Generator, Test), Visited, Caller, Callee, DetInfo, forall(ProofList)) :- !,
    findall((PG, PT),
            ( interpC(P1, Generator, Visited, Caller, Callee, DetInfo, PG),
              interpC(P1, Test, Visited, Caller, Callee, DetInfo, PT)
            ),
            Found),
    ( Found = [] ->
         ProofList = vacuous
    ;   ProofList = Found
    ).

% --- New SWI-Prolog Meta-Predicates ---

% Built-in: call_cleanup/2.
interpC(P1, call_cleanup(Goal, Cleanup), Visited, Caller, Callee, DetInfo, call_cleanup(ProofGoal, ProofCleanup)) :- !,
    call_cleanup(
        interpB(P1, Goal, Visited, Caller, Callee, DetInfo, ProofGoal),
        interpB(P1, Cleanup, Visited, Caller, Callee, DetInfo, ProofCleanup)
    ).

% Built-in: setup_call_cleanup/3.
interpC(P1, setup_call_cleanup(Setup, Goal, Cleanup), Visited, Caller, Callee, DetInfo, setup_call_cleanup(ProofSetup, ProofGoal, ProofCleanup)) :- !,
    setup_call_cleanup(
        interpB(P1, Setup, Visited, Caller, Callee, DetInfo, ProofSetup),
        interpB(P1, Goal, Visited, Caller, Callee, DetInfo, ProofGoal),
        interpB(P1, Cleanup, Visited, Caller, Callee, DetInfo, ProofCleanup)
    ).

% Built-in: catch/3.
interpC(P1, catch(Goal, Catcher, Recovery), Visited, Caller, Callee, DetInfo, catch(ProofGoal, Catcher, ProofRecovery)) :- !,
    catch(
        interpB(P1, Goal, Visited, Caller, Callee, DetInfo, ProofGoal),
        Exception,
        (   (Catcher = Exception ->
                interpB(P1, Recovery, Visited, Caller, Callee, DetInfo, ProofRecovery)
            ;   throw(Exception)
            ))
    ).

% --- New clause for handling predicates with mod_meta declarations ---
% If Goal's functor and arity match a mod_meta declaration, then
% meta_spec_matches/4 extracts the positions of meta-arguments (those marked '?'),
% and meta_args/7 recursively computes proofs for those arguments.
interpC(P1, Goal, Visited, Caller, Callee, DetInfo, meta_decl(Caller, Callee, DetInfo, Goal, MetaProof)) :- fail,
    functor(Goal, Fun, Arity),
    % Look for a mod_meta declaration for this predicate.
    mod_meta(Module, Spec),
    predicate_property(Caller:Goal, module(Module)),
    meta_spec_matches(Fun, Arity, Spec, MetaPositions),  % e.g., for call(3, ?, ?, ?) -> MetaPositions = [2,3,4]
    !,
    meta_args(P1, Goal, MetaPositions, Visited, Caller, Callee, DetInfo, MetaArgProofs),
    MetaProof = meta(Fun, MetaPositions, MetaArgProofs),
    call(Goal).

% --- Helper: meta_spec_matches(+Fun, +Arity, +Spec, -MetaPositions) ---
%
% We assume Spec is of the form:
%     Fun(ArgSpec1, ArgSpec2, ..., ArgSpecN)
% and that the marker ? indicates a meta argument.
% For example, if Spec is call(3, ?, ?, ?), then:
%   - Fun is call
%   - ArgSpecs = [3, ?, ?, ?]
%   - We consider positions 2,3,4 as meta.
meta_spec_matches(Fun, Arity, Spec, MetaPositions) :-
    Spec =.. [Fun | ArgSpecs],
    length(ArgSpecs, L),
    % For simplicity, assume that the effective arity of the spec equals L.
    Arity = L,
    findall(Pos, (nth1(Pos, ArgSpecs, SpecVal), SpecVal == '?'), MetaPositions).

% --- Helper: meta_args(P1, +Goal, +MetaPositions, +Visited, +Caller, +Callee, +DetInfo, -MetaProofs) ---
%
% For each position in MetaPositions, extract the corresponding argument from Goal and
% recursively call interpD/7 to obtain its proof.
meta_args(_P1, _Goal, [], _Visited, _Caller, _Callee, _DetInfo, []).
meta_args(P1, Goal, [Pos|Rest], Visited, Caller, Callee, DetInfo, [Proof|Proofs]) :-
    arg(Pos, Goal, Arg),
    interpD(P1, Arg, Visited, Caller, Callee, DetInfo, Proof),
    meta_args(P1, Goal, Rest, Visited, Caller, Callee, DetInfo, Proofs).

% Non-built-in call (foreign predicates) using pre_call wrapper.
interpC(_OldP1, pre_call(P1, Goal), Visited, Caller, Callee, DetInfo, Proof) :- !,
   interpD(P1, Goal, Visited, Caller, Callee, DetInfo, Proof).

% Built-in predicate call:
% If the goal is built-in, record it as such and execute it.
interpC(_P1, Goal, _Visited, Caller, Callee, DetInfo, builtin(Caller, Callee, DetInfo, Goal)) :-
    predicate_property(Caller:Goal, built_in),
    Caller:call(Goal).

% Non-built-in call (foreign predicates):
% If the goal is not defined via clauses (i.e. has no clauses), treat it as a foreign predicate.
interpC(_P1, Goal, _Visited, Caller, _OldCallee, DetInfo, non_builtin_call(Caller, Callee, DetInfo, Goal)) :-
    \+ predicate_property(Caller:Goal, number_of_clauses(_)), !,
    imported_from(Caller:Goal, Callee),!,
    call(Callee:Goal).

% Undefined predicate detection:
% If the goal is not built-in and is_undefined/1 succeeds, then simply call the goal.
interpC(_P1, Goal, _Visited, Caller, _Callee, _DetInfo, called(Goal)) :-
    is_undefined(Caller:Goal), !,
    %throw(error(existence_error(procedure, Caller:Goal), interpD/6)).
    % if it true is undefined (that is clause/2 whiuch is all we will try next is not going to make it work we may as well use call and let it do the throwing
    trace,Caller:call(Goal).

% --- Atomic goals ---
% For user-defined atomic goals, use clause/2 for a defining clause, then recursively
% interpret its body. Record the module (Callee) and capture the determinacy info (DetInfo)
% for cuts and failures.
interpC(P1, Goal, Visited, Caller, _OldCallee, _OldDetInfo,
        proof(Caller, Callee, DetInfo, Goal, (Goal :- Body), ProofBody)) :- !,
    % Ensure Goal is not already visited to prevent infinite loops.
    \+ check_identical_goal_member(Goal, Visited),
    % Retrieve a clause defining Goal; Body is the clause body.
    Caller:clause(Goal, Body, ClauseInfo),
    % Determine the module in which Goal is defined and bind it to Callee.
    clause_property(ClauseInfo, module(Callee)),
    % Recursively interpret the clause body, adding Goal to the Visited list.
    interpD(P1, Body, [Goal|Visited], Caller, Callee, DetInfo, ProofBody),
    % Bind DetInfo to record the effects of cuts and failures.
    DetInfo = detinfo(Cutted, Failed),
    % If a cut was executed (Cutted equals 'cutted'), perform a local cut here.
    (Cutted == cutted -> ! ; true),
    % If failure was signalled (Failed equals 'failed'), force failure of this goal.
    (Failed == failed -> fail ; true).

check_identical_goal_member(Goal, Visited):- member(V, Visited),V==Goal,!,trace.

% -------------------------------------------------------------------------------
% Helper predicate: interpD_p2l/3
%
% This predicate splits a list of (Template, Proof) pairs into two separate lists:
% one for the Templates and one for the corresponding Proofs.
interpC_p2l([], [], []).
interpC_p2l([(T,P)|Rest], [T|Ts], [P|Ps]) :- !,
    interpD_p2l(Rest, Ts, Ps).


:- dynamic(mod_pub/2).
:- dynamic(mod_priv/2).
:- dynamic(mod_meta/2).
:- dynamic(mod_norm/2).
:- dynamic(mod_pred/2).

% -------------------------------------------------------------------------------
% smp/0: Sample predicate to save module predicate information and list them.
%
% This predicate saves module predicate information (via save_module_pred_info)
% and then lists various asserted predicate information for debugging.
smp :-
    save_module_pred_info,
    cls,
    listing(mod_priv),
    listing(mod_pub),
    listing([mod_norm, mod_meta]).

% -------------------------------------------------------------------------------
% save_module_pred_info/0: Save module predicate information.
%
% For all module predicates (determined by module_pred/2), save their information
% using save_module_pred_info/2.
save_module_pred_info :- mod_meta(_,_),!.
save_module_pred_info :-
  forall(module_pred(M, G), assrtnew(mod_pred(M, G))),
  forall(mod_pred(M, G), save_module_pred_info(M, G)).




% -------------------------------------------------------------------------------
% module_pred/2: Retrieve a predicate from a module.
%
% This predicate finds a predicate G in the current module M such that:
%   - G is not imported from another module.
%   - If G is defined in module RM, then M must equal RM; otherwise, RM is set to M.
% There is also an alternative clause that handles private predicates.
module_pred(M, G) :-
   current_module(M),
   current_predicate(_, M:G),
   \+ predicate_property(M:G, imported_from(_)),
   predicate_property(M:G, _).
module_pred(M, G) :-
   module_private(M, G).

imported_from(Caller:Goal, Callee):- predicate_property(Caller:Goal, imported_from(Callee)), current_predicate(_,Callee:Goal).
imported_from(_Caller:Goal, Callee):- module_pred(Callee, Goal), current_predicate(_,Callee:Goal),!.
imported_from(Caller:Goal, Callee):- current_predicate(_,Caller:Goal),!,Callee=Caller.
% -------------------------------------------------------------------------------
% module_private/2: Check if a predicate G is private in module M.
%
% A predicate is considered private if it is defined in M and does not have any
% imported properties.
module_private(M, G) :-
   current_module(M),
   current_predicate(_, M:G),
   \+ predicate_property(G, _).

% -------------------------------------------------------------------------------
% save_module_pred_info/2: Save information about a module predicate.
%
% This predicate asserts new facts about module predicates:
%   - mod_pred(M, G) is asserted for any predicate G in module M.
%   - If G is a meta-predicate, mod_meta(M, G) is asserted; otherwise, mod_norm(M, G) is asserted.
%   - If G is private, mod_priv(M, G) is asserted; otherwise, mod_pub(M, G) is asserted.
save_module_pred_info(M, G) :-
   assrtnew(mod_pred(M, G)),!,
   once(if_t(predicate_property(M:G, meta_predicate(G)), assrtnew(mod_meta(M, G)))),
   once(if_t(\+ predicate_property(M:G, meta_predicate(G)), assrtnew(mod_norm(M, G)))),
   once(if_t(module_private(M, G), assrtnew(mod_priv(M, G)))),
   if_t(\+ module_private(M, G), assrtnew(mod_pub(M, G))),!.

% -------------------------------------------------------------------------------
% assrtnew/1: Assert a fact if it is not already asserted.
%
% This predicate checks if a clause is already asserted (using is_clause_asserted/1).
% If not, it asserts the fact.

assrtnew(G) :- \+ \+ clause_asserted(G),!.
assrtnew(G) :-  assertz(G).

% -------------------------------------------------------------------------------
% is_undefined/1: Check if a goal is undefined.
%
% This predicate succeeds if no predicate property exists for the given goal, indicating that
% the goal is undefined.
is_undefined(Goal) :-
    \+ predicate_property(Goal, _).


