%  File   : pfcdb.pl
%  Author : Tim Finin, finin@prc.unisys.com
%  Author : Dave Matuszek, dave@prc.unisys.com
%  Author : Dan Corpron
%  Updated: 10/11/87, ...
%  Purpose: This file defines predicates to manipulate a PFC (Prolog Forward Chaining) database 
%           by saving, restoring, resetting the database, and handling database terms.

% The directive for loading or processing the file is assumed to be handled elsewhere.
% No file directives here explicitly.

% Predicate: pfcConjoin/3
% Combines two terms into a conjunction.
% The first two clauses handle cases where one of the terms is 'true' to avoid redundant conjunctions.
% If neither is 'true', the terms are combined into a conjunction (C1, C2).
%
% @example
% ?- pfcConjoin(true, X, Result).   % Result = X.
% ?- pfcConjoin(X, true, Result).   % Result = X.
% ?- pfcConjoin(a, b, Result).      % Result = (a, b).
pfcConjoin(true, X, X) :- !.  % If the first argument is true, the result is simply X.
pfcConjoin(X, true, X) :- !.  % If the second argument is true, the result is X.
pfcConjoin(C1, C2, (C1,C2)).  % Combine both arguments into a conjunction.

% Predicate: pfcDatabaseTerm/1
% Describes what kinds of terms are considered part of the PFC database.
% These terms are part of forward chaining logic in PFC and shouldn't appear in an empty database.
%
% @example
% ?- pfcDatabaseTerm('==>'/2).  % True if the term is part of the PFC database.
pfcDatabaseTerm('$spft$'/3).  % A special PFC term for forward chaining.
pfcDatabaseTerm('$pt$'/2).    % Another PFC term (predicates).
pfcDatabaseTerm('$bt$'/2).    % Base term (backward chaining triggers).
pfcDatabaseTerm('$nt$'/3).    % Negation trigger term.
pfcDatabaseTerm('==>'/2).     % Forward implication.
pfcDatabaseTerm('<==>'/2).    % Bidirectional implication.
pfcDatabaseTerm('<-'/2).      % Reverse implication.
pfcDatabaseTerm(pfcQueue/1).  % The queue used for managing PFC operations.

% Predicate: pfcReset/0
% Resets the PFC database by retracting all forward chaining rules and justifications.
% It checks whether any PFC-related items remain in the database, issuing a warning if so.
% 
% @example
% ?- pfcReset.  % Cleans the PFC database by retracting all relevant terms.
pfcReset :-
  pfc_spft(P, F, Trigger),  % Iterate through all forward chaining rules in the form of $spft$.
  pfcRetractOrWarn(P),      % Attempt to retract the rule's predicate P.
  pfcRetractOrWarn('$spft$'(P, F, Trigger)),  % Attempt to retract the $spft$ term itself.
  fail.  % Fail to force backtracking and ensure all relevant terms are retracted.
pfcReset :-  % After all retracts, verify the database is empty.
  (pfcDatabaseItem(T) ->  % Check if there are any remaining database items.
   (pfcError("Pfc database not empty after pfcReset, e.g., ~p.~n", [T]), fail)  % Warn if there are.
    ; true).  % Succeed if no items remain.

% Predicate: pfcDatabaseItem/1
% True if there is still some PFC-related data (crud) in the database.
% It checks if any terms match the patterns defined by pfcDatabaseTerm/1.
%
% @example
% ?- pfcDatabaseItem(Term).  % Term will match any PFC-related database item.
pfcDatabaseItem(Term:-Body) :-
  pfcDatabaseTerm(P/A),  % Check if the term's functor matches a PFC database term.
  functor(Term, P, A),   % Extract the functor and arity of the term.
  clause(Term, Body).    % Check if the term exists in the database with a body.

% Predicate: pfcRetractOrWarn/1
% Retracts a term if it exists in the database, otherwise issues a warning.
% 
% @example
% ?- pfcRetractOrWarn(myFact).  % Retract 'myFact' if it exists, warn otherwise.
pfcRetractOrWarn(X) :-  retract(X), !.  % Retract the term if possible.
pfcRetractOrWarn(X) :-
  pfcWarn("Couldn't retract ~p.", [X]),  % Issue a warning if retract fails.
  nop((dumpST, pfcWarn("Couldn't retract ~p.", [X]))), !.  % Further logging (no operation in nop).

% Predicate: pfcRetractOrQuietlyFail/1
% Similar to pfcRetractOrWarn/1, but fails quietly instead of issuing a warning when retract fails.
% 
% @example
% ?- pfcRetractOrQuietlyFail(myFact).  % Retract 'myFact' if possible, fail otherwise without warning.
pfcRetractOrQuietlyFail(X) :-  retract(X), !.  % Retract the term if possible.
pfcRetractOrQuietlyFail(X) :-
  nop((pfcTraceMsg("Trace: Couldn't retract ~p.", [X]),  % Log the failure quietly.
      nop((dumpST, pfcWarn("Couldn't retract ~p.", [X]))))),  % Further logging.
  !, fail.  % Fail after attempting to log the failure quietly.

%   Previously commented out code and dead code for future reference:
%   It seems some logging or debugging-related code has been omitted or replaced by nop operations.
%   They may be useful for debugging purposes in specific situations, thus preserved as references.
%   /* previously:  pfcReset_with_logging ...  */