%! buildTrigger(+Triggers, +Consequent, -X) is det.
% This predicate processes a list of triggers, and constructs a 
% Prolog term that represents a conjunction of triggers.
% The '$pt$' functor is used to represent the processed trigger.
% 
% The first argument is a list of triggers, the second argument is the 
% consequent (the result of the rule firing), and the third argument is the
% output, which is the processed trigger list.
% 
% @example
% ?- buildTrigger([trigger1, trigger2], consequent, X).
% X = '$pt$'(trigger1, '$pt$'(trigger2, consequent)).

!,
buildTest(Test, Test2),  % Call buildTest to process Test and get Test2.
buildTrigger([{Test2}|Triggers], Consequent, X).  % Recursively build triggers.

% buildTrigger([snip|Triggers], Consequent, snip(X)) :-
%   !,
%   buildTrigger(Triggers, Consequent, X).
%
% This piece of code was commented out, potentially because it's
% an alternative form that deals with 'snip' trigger formats, which 
% was not needed in the current context.

%! buildTrigger(+T, +Consequent, -X) is det.
% This predicate constructs a trigger by recursively conjoining
% elements using the '$pt$' functor.
% 
% @example
% ?- buildTrigger([t1, t2], consequent, X).
% X = '$pt$'(t1, '$pt$'(t2, consequent)).

buildTrigger([T|Triggers], Consequent, '$pt$'(T, X)) :-
  !,
  buildTrigger(Triggers, Consequent, X).  % Recursively build triggers.

% buildNtTest(+Trigger, +Testin, -Testout) is det.
% This predicate builds a test for negative triggers ('$nt$'/3).
% It ensures that no matching facts are in the database and any
% additional test specified in the rule is included.
% 
% @example
% ?- buildNtTest(trigger, test_in, Test_out).
% Test_out = (pfc_call(trigger), test_in).
%
buildNtTest(T, Testin, Testout) :-
  buildTest(Testin, Testmid),  % Process Testin to get Testmid.
  pfcConjoin((pfc_call(T)), Testmid, Testout).  % Conjoin the tests.

% buildTest(+TestIn, -TestOut) is det.
% This predicate simplifies curly bracketed terms.
% If the input is wrapped in curly brackets, it removes them.
% Otherwise, it leaves the term unchanged.
%
% @example
% ?- buildTest({a}, X).
% X = a.
%
% @example
% ?- buildTest(a, X).
% X = a.

buildTest({Test}, Test) :- !.  % If Test is wrapped in curly brackets, remove them.
buildTest(Test, Test).  % Otherwise, leave Test unchanged.

% pfcType(+Value, -Type) is semidet.
% This predicate identifies the type of a PFC (Prolog Forward Chaining)
% object. It distinguishes between facts, rules, and triggers.
% 
% @example
% ?- pfcType(==>(a,b), Type).
% Type = rule(fwd).

pfcType(Var, Type):- 
  var(Var), !,  % If the input is a variable, it's assumed to be a fact.
  Type = fact(_FT).

pfcType(_:X, Type):- 
  !, pfcType(X, Type).  % If the input is module-prefixed, recurse into the term.

pfcType(~_, Type):- 
  !, Type = fact(_FT).  % If the term starts with '~', it's a fact.

pfcType(('==>'(_,_)), Type):- 
  !, Type = rule(fwd).  % If the term is a forward chaining rule, mark it as such.

pfcType('==>'(X), Type):- 
  !, pfcType(X, Type), 
  pfcWarn(pfcType('==>'(X), Type)).  % Handle warnings for forward chaining rules.

pfcType(('<==>'(_,_)), Type):- 
  !, Type = rule(<==>).  % Bidirectional rule (equivalence).

pfcType(('<-'(_,_)), Type):- 
  !, Type = rule(bwc).  % Backward chaining rule.

pfcType((':-'(_,_)), Type):- 
  !, Type = rule(cwc).  % Classical Prolog rule.

pfcType('$pt$'(_,_,_), Type):- 
  !, Type = trigger(+).  % Positive trigger with three arguments.

pfcType('$pt$'(_, _), Type):- 
  !, Type = trigger(+).  % Positive trigger with two arguments.

pfcType('$nt$'(_, _, _), Type):- 
  !, Type = trigger(-).  % Negative trigger.

pfcType('$bt$'(_, _), Type):- 
  !, Type = trigger(?).  % Bidirectional trigger.

pfcType(pfcAction(_), Type):- 
  !, Type = action.  % Action term.

pfcType(('::::'(_,X)), Type):- 
  !, pfcType(X, Type).  % Handle ':'-prefixed terms.

pfcType(_, fact(_FT)):- 
  % If none of the above patterns match, assume it's a fact.
  !.

% pfcAssert(+P, +Support) is det.
% This predicate asserts a fact or rule P if it doesn't already exist,
% and adds the provided support information.
% 
% @example
% ?- pfcAssert(fact, support).
% true.
%
pfcAssert(P, Support) :-
  (pfc_clause(P) ; assert(P)),  % Assert P if it's not already in the database.
  !,
  pfcAddSupport(P, Support).  % Add support information for P.

% pfcAsserta(+P, +Support) is det.
% Like pfcAssert/2, but uses asserta/1 to add P to the front of the database.
pfcAsserta(P, Support) :-
  (pfc_clause(P) ; asserta(P)),
  !,
  pfcAddSupport(P, Support).

% pfcAssertz(+P, +Support) is det.
% Like pfcAssert/2, but uses assertz/1 to add P to the end of the database.
pfcAssertz(P, Support) :-
  (pfc_clause(P) ; assertz(P)),
  !,
  pfcAddSupport(P, Support).

% pfc_clause(+Head) is semidet.
% This predicate checks if a clause with a given head exists in the database.
% It ensures the clause is exactly the same by using variant/2.
% This helps avoid duplicate clauses with the same meaning but different variables.
%
pfc_clause((Head :- Body)) :- 
  !,
  copy_term(Head, Head_copy),
  copy_term(Body, Body_copy),
  clause(Head, Body),
  variant(Head, Head_copy),
  variant(Body, Body_copy).

pfc_clause(Head) :- 
  % Find unit clauses that are exactly identical to Head.
  copy_term(Head, Head_copy),
  clause(Head_copy, true),
  variant(Head, Head_copy).

% pfcForEach(+Binder, +Body) is det.
% This predicate iterates over Binder and applies Body for each result.
% After failing once, it succeeds (because of fail/0).
% 
% @example
% ?- pfcForEach((X=1; X=2), writeln(X)).
% 1
% 2
% true.

pfcForEach(Binder, Body) :- 
  Binder, pfcdo(Body), fail.  % Execute Body for each solution to Binder.
pfcForEach(_, _).  % End recursion when no more solutions exist.

% pfcdo(+X) is det.
% Executes X once and always succeeds. This ensures predicates 
% like pfcForEach don't fail prematurely.
pfcdo(X) :- 
  X, !.  % Execute X and succeed if it succeeds.
pfcdo(_).  % If X fails, succeed anyway.

% pfcUnion(+L1, +L2, -L3) is det.
% This predicate creates a union of two sets, L1 and L2, where sets
% are represented as simple lists.
% 
% @example
% ?- pfcUnion([a,b,c], [b,d], L3).
% L3 = [a, c, d].
%
pfcUnion([], L, L).  % The union of an empty set and L is L.
pfcUnion([Head|Tail], L, Tail2) :-
  memberchk(Head, L),  % If Head is already in L, skip it.
  !,
  pfcUnion(Tail, L, Tail2).  % Recursively process the tail.
pfcUnion([Head|Tail], L, [Head|Tail2]) :-
  pfcUnion(Tail, L, Tail2).  % Add Head to the result and process the tail.

% pfcConjoin(+Conjunct1, +Conjunct2, -Conjunction) is det.
% This predicate conjoins two terms, producing a simplified conjunction.
% It's used to build composite tests from individual conditions.