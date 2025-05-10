/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
  LogicMOO Base FOL/PFC Setup
% Dec 13, 2035
% Douglas Miles

*/

%*********************************************************************************************
% PROGRAM FUNCTION:  Implements forward chaining, tracks changes, and provides proofs of safety.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- if( \+ current_predicate(set_fileAssertMt/1)).
% %
% %
% %  predicates for manipulating support relationships
% %

%!  pfcAddSupport(+P, +Support) is det.
%
%   Adds support for a given proposition by asserting the support fact.
%
%   This predicate is used to record the support for a proposition 'P'. It takes
%   a pair consisting of a 'Fact' and a 'Trigger', and asserts that this support
%   exists by storing it in the form of a special fact.
%
%   @arg P       The proposition being supported.
%   @arg Support A pair '(Fact, Trigger)' representing the support for 'P'. The
%                'Fact' is the actual support fact, and the 'Trigger' is the condition
%                or event that supports the proposition.
%
%   @example
%     % Add support for a fact 'some_fact' with the trigger 'some_trigger'.
%     ?- pfcAddSupport(some_fact, (some_support, some_trigger)).
%
%     % This will assert that the fact 'some_fact' is supported by 'some_support' and
%     % 'some_trigger'.
%
pfcAddSupport(P, (Fact, Trigger)) :-
    % Assert the support fact using the '$spft$' predicate.
    assert('$spft$'(P, Fact, Trigger)).

%!  pfcGetSupport(+P, -Support) is semidet.
%
%   Retrieves the support information for a given proof P.
%   The support is provided in the form of a tuple '(Fact, Trigger)',
%   where 'Fact' is a fact involved in the proof, and 'Trigger' is
%   an event or condition that triggered the proof.
%
%   @arg P       The proof for which the support information is being retrieved.
%   @arg Support A tuple '(Fact, Trigger)' where 'Fact' is a fact and 'Trigger'
%                is the corresponding trigger event or condition.
%
%   @example
%     % Retrieve support information for a given proof:
%     ?- pfcGetSupport(P, Support).
%     Support = (Fact, Trigger).
%
pfcGetSupport(P, (Fact, Trigger)) :-
    % Retrieves the support using pfc_spft/3, which extracts the Fact and Trigger
    % for the given proof P.
    pfc_spft(P, Fact, Trigger).

%!  pfc_spft(+P, -Fact, -Trigger) is semidet.
%
%   Retrieves the fact and trigger that support the given proof P.
%   This predicate internally calls the system predicate '$spft$' to get the
%   fact ('F') and trigger ('T') associated with the proof.
%
%   @arg P    The proof for which the fact and trigger are being retrieved.
%   @arg Fact The fact involved in the proof.
%   @arg Trigger The trigger event or condition related to the proof.
%
%   @example
%     % Retrieve the fact and trigger for a proof P:
%     ?- pfc_spft(P, Fact, Trigger).
%     Fact = some_fact,
%     Trigger = some_trigger.
%
pfc_spft(P, F, T) :-
    % Calls the system predicate '$spft$'/3 to retrieve the fact (F) and trigger (T)
    % for the given proof P.
    pfcCallSystem('$spft$'(P, F, T)).

% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

%!  pfcRemOneSupport(+P, +FactTriggerPair) is semidet.
%
%   Removes one support relationship between a fact and a trigger.
%   This predicate ensures that at least one of P, Fact, or Trigger is callable,
%   and then attempts to retract the support fact associated with them.
%
%   The support fact being retracted is represented as '$spft$'(P, Fact, Trigger).
%
%   @arg P The primary predicate or fact that is being checked for support.
%   @arg FactTriggerPair A tuple (Fact, Trigger) representing the fact and the trigger
%                        that together form the support relationship for P.
%
%   @example
%     % Suppose P is some predicate, Fact is a fact, and Trigger is a condition
%     % that supports the fact P. The following retracts this support relationship:
%     ?- pfcRemOneSupport(P, (Fact, Trigger)).
%
pfcRemOneSupport(P, (Fact, Trigger)) :-
    % Ensure that at least one of P, Fact, or Trigger is callable.
    must_ex(callable(P); callable(Fact); callable(Trigger)),
    % Attempt to retract the support fact '$spft$'(P, Fact, Trigger).
    pfcRetractOrWarn('$spft$'(P, Fact, Trigger)).

%!  pfcRemOneSupportOrQuietlyFail(+P, +Support) is det.
%
%   Removes one support fact or fails quietly if the support fact cannot be retracted.
%   This predicate ensures that the arguments are callable and attempts to retract
%   the support fact represented as '$spft$'(P, Fact, Trigger). It will silently fail
%   if the retract operation does not succeed, ensuring that no exceptions are thrown.
%
%   @arg P       The primary fact or rule that was supported.
%   @arg Support A tuple of the form (Fact, Trigger) representing the fact and trigger
%                that provided support for the primary fact P.
%
%   @example Remove a specific support for a fact:
%     ?- pfcRemOneSupportOrQuietlyFail(rule1, (fact1, trigger1)).
%
pfcRemOneSupportOrQuietlyFail(P, (Fact, Trigger)) :-
    % Ensure that P, Fact, or Trigger are callable terms.
    must_ex(callable(P); callable(Fact); callable(Trigger)),
    % Attempt to retract the support fact represented as '$spft$'(P, Fact, Trigger).
    pfcRetractOrQuietlyFail('$spft$'(P, Fact, Trigger)).

%!  pfc_collect_supports(-Triples) is det.
%
%   Collects all triples that are supported by the PFC (Prolog Forward Chaining) system.
%   This predicate gathers all support triples from the PFC system using the 'pfc_support_relation/1' predicate.
%   If there are any support triples, they are returned in the 'Triples' list.
%   If no triples are found, an empty list is returned.
%
%   @arg Triples A list of support triples collected from the PFC system.
%
%   @example Collect support triples:
%     ?- pfc_collect_supports(Triples).
%     Triples = [triple1, triple2, triple3].
%
pfc_collect_supports(Triples) :-
  % Use bagof/3 to collect all triples that satisfy pfc_support_relation/1.
  bagof(Triple, pfc_support_relation(Triple), Triples),!.  % Cut to ensure deterministic behavior
% If bagof/3 fails (i.e., no triples are found), return an empty list.
pfc_collect_supports([]).

%!  pfc_support_relation(?Triple) is det.
%
%   Defines the relationship between a Prolog Forward Chaining (PFC) predicate, fact, and trigger.
%   This predicate is used to structure support relationships in the PFC system by packaging
%   the predicate 'P', fact 'F', and trigger 'T' into a triple '(P,F,T)'.
%   It relies on the 'pfc_spft/3' predicate to obtain the individual components.
%
%   @arg Triple A tuple '(P, F, T)' where 'P' is the predicate, 'F' is the fact,
%               and 'T' is the trigger in the PFC system.
%
%   @example Querying for a specific support triple:
%     ?- pfc_support_relation((P, F, T)).
%     P = some_predicate,
%     F = some_fact,
%     T = some_trigger.
%
pfc_support_relation((P, F, T)) :-
  % Use pfc_spft/3 to unify P, F, and T as components of the support triple.
  pfc_spft(P, F, T).

%!  pfc_make_supports(+PSupportTuple) is det.
%
%   Creates support for a fact by adding it to the PFC (Prolog Forward Chaining) support
%   structure. The support tuple consists of a fact 'P' and two support sources 'S1' and 'S2'.
%
%   The predicate first adds the support for the fact using 'pfcAddSupport/2', associating
%   the fact 'P' with the pair '(S1, S2)'. It then attempts to classify the fact 'P' by calling
%   'pfcAddType1/1', though failure to classify does not cause the operation to fail due to the
%   use of the disjunction with 'true'. A cut ('!') is placed to prevent unnecessary backtracking.
%
%   @arg PSupportTuple A tuple '(P, S1, S2)' where 'P' is the fact to support and 'S1', 'S2' are
%                      the support sources.
%
%   @example
%     % Add support for a fact 'foo' with support sources 'a' and 'b'.
%     ?- pfc_make_supports((foo, a, b)).
%
pfc_make_supports((P,S1,S2)) :-
  % Add the support for fact P, with S1 and S2 as the sources.
  pfcAddSupport(P,(S1,S2)),
  % Try to classify the fact P using pfcAddType1/1, or succeed with true if classification fails.
  (pfcAddType1(P); true),
  % Cut to prevent backtracking after adding support and classification.
  !.


%!  pfcTriggerKey(+Trigger, -Key) is det.
%
%   Extracts the best indexing key from a given Trigger.
%
%   This predicate determines a key for indexing the trigger (first argument).
%   Different types of triggers have different structures, and this predicate
%   attempts to extract the most appropriate key term for efficient indexing.
%
%   @arg Trigger The trigger term, which can take various forms.
%   @arg Key The term used for indexing the trigger.
%
%   @examples
%     ?- pfcTriggerKey('$pt$'(foo, bar), Key).
%     Key = foo.
%
%     ?- pfcTriggerKey('$nt$'(baz, qux, quux), Key).
%     Key = baz.
%
pfcTriggerKey('$pt$'(Key,_), Key).
pfcTriggerKey('$pt$'(Key,_,_), Key).
pfcTriggerKey('$nt$'(Key,_,_), Key).
pfcTriggerKey(Key, Key).

%!  pfc_trigger_key(+Trigger, -Key) is det.
%
%   Retrieves the key from the trigger for use as the first argument
%   of the trigger base clause that stores the trigger.
%
%   This predicate attempts to extract a key from a given trigger. Depending on
%   the form of the trigger, different extraction rules are applied. If the trigger
%   is a variable, it will unify with itself. For more complex structures like 'chart/2',
%   the specific elements inside the structure are extracted as the key.
%
%   @arg Trigger The trigger term, which can have different forms like variables,
%        or structures such as 'chart/2'.
%   @arg Key The term extracted as the key from the trigger.
%
%   @examples
%     ?- pfc_trigger_key(chart(word('apple'), some_list), Key).
%     Key = apple.
%
%     ?- pfc_trigger_key(chart(stem(['a', 'b', 'c']), some_list), Key).
%     Key = 'a'.
%
%     ?- pfc_trigger_key(foo, Key).
%     Key = foo.
%
pfc_trigger_key(X, X) :- var(X), !.
pfc_trigger_key(chart(word(W), _L), W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]), _L), Char1) :- !.
pfc_trigger_key(chart(Concept, _L), Concept) :- !.
pfc_trigger_key(X, X).

%!  nb_pushval(+Name, +Value) is det.
%
%   Push a new Value onto the named non-backtrackable variable.
%
%   If a value is already associated with the given Name, the new Value is added
%   to the front of the list. If no value is currently associated with Name, a new
%   list containing Value is created and stored under Name.
%
%   @arg Name  The name of the non-backtrackable variable.
%   @arg Value The value to be pushed onto the list stored in the non-backtrackable variable.
%
%   @example
%     % Push values to a non-backtrackable variable 'foo':
%     ?- nb_pushval(foo, 1).
%     ?- nb_pushval(foo, 2).
%     ?- nb_current(foo, X).
%     X = [2, 1].
%
nb_pushval(Name, Value) :-
    % If a value is already associated with Name, add the new Value to the front of the list.
    nb_current(Name, Before) ->
        nb_setval(Name, [Value | Before])
    ;
    % Otherwise, create a new list with the Value.
    nb_setval(Name, [Value]).

%!  nb_peekval(+Name, -Value) is semidet.
%
%   Peek at the top value of the list associated with the non-backtrackable variable.
%
%   This predicate retrieves the first value from the list stored in the non-backtrackable
%   variable without modifying the list.
%
%   @arg Name  The name of the non-backtrackable variable.
%   @arg Value The value retrieved from the top of the list.
%
%   @example
%     % Peek at the top value in the list for 'foo':
%     ?- nb_pushval(foo, 1), nb_peekval(foo, X).
%     X = 1.
%
nb_peekval(Name, Value) :-
    % Retrieve the first value from the list associated with Name.
    nb_current(Name, [Value | _Before]).

%!  nb_hasval(+Name, +Value) is semidet.
%
%   Check if a Value is present in the list stored in the non-backtrackable variable.
%
%   This predicate succeeds if the specified Value is present in the list associated with
%   the non-backtrackable variable.
%
%   @arg Name  The name of the non-backtrackable variable.
%   @arg Value The value to check for in the list.
%
%   @example
%     % Check if '1' is in the list associated with 'foo':
%     ?- nb_pushval(foo, 1), nb_hasval(foo, 1).
%     true.
%
nb_hasval(Name, Value) :-
    % Check if Value is present in the list associated with Name.
    nb_current(Name, List),member(Value, List).

%!  nb_popval(+Name, -Value) is semidet.
%
%   Pop the top value from the list stored in the non-backtrackable variable.
%
%   This predicate removes the first value from the list associated with the non-backtrackable
%   variable and unifies it with Value.
%
%   @arg Name  The name of the non-backtrackable variable.
%   @arg Value The value popped from the top of the list.
%
%   @example
%     % Pop a value from the list associated with 'foo':
%     ?- nb_pushval(foo, 1), nb_pushval(foo, 2), nb_popval(foo, X).
%     X = 2.
%
nb_popval(Name, Value) :-
    % Remove the first value from the list associated with Name and update the list.
    nb_current(Name, [Value | Before]) ->
        nb_setval(Name, Before).

%!  reset_shown_justs is det.
%
%   Resets the tracking of shown justifications in the system.
%   This predicate removes all existing entries from the 't_l:shown_why/1' predicate,
%   effectively clearing any stored justifications.
%   Additionally, it performs a no-operation call to 'color_line/2' with
%   red color and line number 1, which may be for diagnostic or logging purposes.
%
%   @example
%   % Reset the shown justifications:
%   ?- reset_shown_justs.
%
reset_shown_justs :-
    % Remove all entries of shown justifications from the system.
    retractall(t_l:shown_why(_)),
    % Call a no-op function to potentially color a line for diagnostic purposes.
    nop(color_line(red, 1)).

%!  clear_proofs is det.
%
%   Clears all stored proof data from the 't_l:whybuffer/2' predicate and optionally
%   logs the action using 'color_line/2'.
%
%   This predicate retracts all facts related to 't_l:whybuffer/2', effectively
%   clearing the stored proofs. After clearing, it may call 'color_line/2' with the
%   arguments 'cyan' and '1' for potential logging or visual feedback.
%
%   @example Clear all stored proofs:
%       ?- clear_proofs.
%       true.
%
clear_proofs :-
    % Retract all facts from the 't_l:whybuffer/2' predicate, clearing the proof data.
    retractall(t_l:whybuffer(_P, _Js)),
    % Optionally log or provide visual feedback by calling 'color_line/2'.
    nop(color_line(cyan, 1)).

%!  lookup_spft_match(+A, +B, -C) is nondet.
%
%   Matches a specific pattern by ensuring that 'A' is unchanged after a lookup operation.
%
%   This predicate first creates a copy of 'A' (as 'AA') to ensure the original term is preserved.
%   Then, it calls 'lookup_spft/3' with the arguments 'A', 'B', and 'C'. After the lookup, it checks
%   whether the original term 'A' is still structurally identical to its copy 'AA' using '=@=/2'.
%
%   This can be useful for ensuring that the lookup operation does not modify the input term 'A'.
%
%   @arg A The term to be matched and checked for preservation.
%   @arg B The second argument passed to 'lookup_spft/3', typically used for lookup purposes.
%   @arg C The output variable to hold the result of the lookup operation.
%
%   @example Lookup a match while ensuring that 'A' is unchanged:
%       ?- lookup_spft_match(some_term, some_key, Result).
%       Result = ...
%
lookup_spft_match(A, B, C) :-
    % Create a copy of 'A' as 'AA' to preserve the original term.
    copy_term(A, AA),
    % Perform the lookup using 'lookup_spft/3'.
    lookup_spft(A, B, C),
    % Ensure that 'A' is still structurally identical to 'AA'.
    A =@= AA.

%!  lookup_spft_match_deeper(+H, +Fact, -Trigger) is nondet.
%
%   Performs a deeper lookup operation by ensuring the head term 'H' is unchanged after the lookup.
%
%   This predicate creates a copy of the term 'H' (as 'HH') to ensure the original term is preserved.
%   It then performs a lookup using 'lookup_spft/3' with the pattern '(H :- _B)', where 'H' is matched
%   with the head of a clause, and the body is ignored. After the lookup, the predicate checks if the
%   original term 'H' is still structurally identical to its copy 'HH' using '=@=/2'.
%
%   This is particularly useful for cases where the lookup involves clauses, and you want to ensure that
%   the head of the clause remains unchanged during the operation.
%
%   @arg H The head term to be matched and checked for preservation.
%   @arg Fact The second argument passed to 'lookup_spft/3', typically representing a fact to be looked up.
%   @arg Trigger The output variable to hold the result of the lookup operation.
%
%   @example Lookup a match while ensuring that the head 'H' is unchanged:
%       ?- lookup_spft_match_deeper(head_term, some_fact, Trigger).
%       Trigger = ...
%
lookup_spft_match_deeper(H, Fact, Trigger) :-
    % Create a copy of 'H' as 'HH' to preserve the original term.
    copy_term(H, HH),
    % Perform the lookup using 'lookup_spft/3' with the pattern '(H :- _B)'.
    lookup_spft((H :- _B), Fact, Trigger),
    % Ensure that 'H' is still structurally identical to 'HH'.
    H =@= HH.

%!  lookup_spft_match_first(+A, +B, -C) is nondet.
%
%   Performs a lookup operation, preferring a match where 'A' remains unchanged, but falling back to a regular lookup if needed.
%
%   If 'A' is instantiated (nonvar), the predicate first tries to find a match using 'lookup_spft_match/3', which ensures 'A' is preserved.
%   If this fails, it falls back to a standard lookup with 'lookup_spft/3'.
%   The 'no_repeats/1' predicate is used to ensure that duplicate results are removed.
%
%   If 'A' is uninstantiated (var), the predicate directly performs a standard lookup using 'lookup_spft/3'.
%
%   @arg A The term to be matched. If instantiated, it will first attempt to ensure that 'A' is unchanged.
%   @arg B The second argument passed to 'lookup_spft/3', typically used for lookup purposes.
%   @arg C The output variable to hold the result of the lookup operation.
%
%   @example Perform a lookup, preferring a match that preserves 'A':
%       ?- lookup_spft_match_first(some_term, some_key, Result).
%       Result = ...
%
%   @example Perform a fallback lookup if no match-preserving result is found:
%       ?- lookup_spft_match_first(var_term, some_key, Result).
%       Result = ...
%
lookup_spft_match_first(A, B, C) :-
    % If 'A' is instantiated (nonvar), prioritize finding a match where 'A' remains unchanged.
    nonvar(A), !,
    % Use 'no_repeats/1' to ensure duplicate results are avoided. Try 'lookup_spft_match/3' first,
    % and if it fails, fall back to 'lookup_spft/3'.
    no_repeats(((lookup_spft_match(A, B, C); lookup_spft(A, B, C)))).
% If 'A' is not instantiated, simply perform a lookup using 'lookup_spft/3'.
lookup_spft_match_first(A, B, C) :-
    lookup_spft(A, B, C).

%!  pfc_is_info(:TermC) is semidet.
%
%   Determines if a term is classified as PFC information.
%
%   This predicate checks various patterns to determine whether the given term should be
%   considered as PFC (Prolog Forward Chaining) information. It handles specific forms
%   such as pairs, facts with certain functors, and specific patterns. The predicate
%   succeeds if the term matches one of these patterns, indicating that it is PFC information.
%
%   @arg TermC The term to be checked. It can be a pair of a control word ('CWC') and some information,
%        or it may follow other specific structures defined below.
%
%   @example Check if a term is PFC information:
%       ?- pfc_is_info((atom_info, some_info)).
%       true.
%
%       ?- pfc_is_info(pfc_bc_only(some_fact)).
%       true.
%
pfc_is_info((CWC, Info)) :-
    % If 'CWC' is an atom and recognized as PFC info, or recurse on the second part ('Info').
    (atom(CWC), is_a_info(CWC)) ;
    pfc_is_info(Info).
pfc_is_info(pfc_bc_only(C)) :-
    % Check if 'C' is a non-variable term.
    is_ftNonvar(C), !.
pfc_is_info(infoF(C)) :-
    % Check if 'C' is a non-variable term in the 'infoF/1' structure.
    is_ftNonvar(C), !.
pfc_is_info(inherit_above(_, _)).
    % The pattern 'inherit_above/2' is always considered PFC information.

%!  is_a_info(+CWC) is semidet.
%
%   Determines if the given control word ('CWC') is classified as PFC information.
%
%   This predicate checks if the given control word ('CWC') is either explicitly identified as "fail"
%   or if it follows the 'is_pfc_chained/1' rule. The predicate succeeds if either of these conditions
%   is met, classifying the control word as PFC information.
%
%   @arg CWC The control word to be checked. It can either be the atom 'fail' or some term that satisfies
%        the 'is_pfc_chained/1' rule.
%
%   @example Check if a control word is PFC information:
%       ?- is_a_info(fail).
%       true.
%
%       ?- is_a_info(some_control_word).
%       true.
%
is_a_info(fail).
is_a_info(CWC) :-
    % Succeeds if 'CWC' is a term that satisfies 'is_pfc_chained/1'.
    is_pfc_chained(CWC).

%!  is_pfc_chained(+CWC) is semidet.
%
%   Determines if the given control word ('CWC') is part of a predefined set of PFC-chained control words.
%
%   This predicate checks if the control word ('CWC') belongs to a predefined set of control words
%   that are considered to be "PFC-chained". These control words include 'cwc', 'awc', 'zwc', 'fwc',
%   'bwc', and 'wac'. The predicate succeeds if 'CWC' matches one of these control words.
%
%   @arg CWC The control word to be checked. It must match one of the predefined values.
%
%   @example Check if a control word is PFC-chained:
%       ?- is_pfc_chained(cwc).
%       true.
%
%       ?- is_pfc_chained(nonexistent).
%       false.
%
is_pfc_chained(cwc).
is_pfc_chained(awc).
is_pfc_chained(zwc).
is_pfc_chained(fwc).
is_pfc_chained(bwc).
is_pfc_chained(wac).

% The following line uses forall/2 to process all operations that meet the
% condition 'is_pfc_chained/1' and asserts them into the Prolog database using
% 'assert_if_new/1'.
%
% - 'is_pfc_chained(Op)' is the condition being checked.
% - 'assert_if_new(Op)'' ensures that the operation 'Op' is added to the database
%   only if it hasn't already been asserted.
%
% In essence, this line ensures that for every operation that is PFC chained,
% it is asserted as a new fact if it does not already exist, preventing duplicates.
:- forall(is_pfc_chained(Op),assert_if_new(Op)).

%!  reserved_body(+B) is semidet.
%
%   Determines if the given term 'B' is considered a "reserved body" in the context
%   of this Prolog program. The predicate succeeds if 'B' matches one of the reserved
%   patterns, otherwise it fails.
%
%   @arg B The term to check.
%
%   The predicate works as follows:
%   - If 'B' is a variable, it immediately fails ('var(B), !, fail.').
%   - If 'B' matches the predicate 'attr_bind/1' or 'attr_bind/2', it succeeds.
%   - Otherwise, it checks if 'B' satisfies the helper predicate 'reserved_body_helper/1'.
%
%   @example
%   % Example of a reserved body:
%   ?- reserved_body(attr_bind(X)).
%   true.
%
%   @example
%   % This will fail because a variable is not allowed:
%   ?- reserved_body(X).
%   false.
%
reserved_body(B) :-
    % If B is a variable, immediately fail.
    var(B), !, fail.
% If B matches the reserved pattern attr_bind/1, succeed.
reserved_body(attr_bind(_)).
% If B matches the reserved pattern attr_bind/2, succeed.
reserved_body(attr_bind(_, _)).
% Otherwise, check if B satisfies the reserved_body_helper/1 predicate.
reserved_body(B) :- reserved_body_helper(B).

%!  reserved_body_helper(+B) is semidet.
%
%   This helper predicate checks whether a given term 'B' is a reserved body, based on
%   additional conditions. It succeeds if 'B' is a compound term that meets specific
%   criteria, otherwise it fails.
%
%   @arg B The term to check.
%
%   The predicate works as follows:
%   - If 'B' is not a compound term, it fails ('\+ compound(B), !, fail.').
%   - If 'B' is a tuple of the form '(ZAWC, _)' where 'ZAWC' is an atom and
%     satisfies 'is_pfc_chained/1', it succeeds.
%
%   @example
%   % This will succeed because 'my_atom' is an atom and is chained:
%   ?- is_pfc_chained(my_atom), reserved_body_helper((my_atom, something_else)).
%   true.
%
%   @example
%   % This will fail because B is not a compound term:
%   ?- reserved_body_helper(some_atom).
%   false.
%
reserved_body_helper(B) :-
    % If B is not a compound term, fail.
    \+ compound(B), !, fail.
% If B is a tuple (ZAWC, _), where ZAWC is an atom and chained, succeed.
reserved_body_helper((ZAWC, _)) :-
    atom(ZAWC),is_pfc_chained(ZAWC).

%!  call_only_based_mfl(+H, -MFL) is det.
%
%   Retrieves the module, file, and line number information for a given predicate 'H',
%   storing it in the structure 'mfl4(_, M, F, L)'. This predicate is used to gather
%   metadata about the location of the predicate 'H' in the source code.
%
%   @arg H   The predicate for which information is retrieved.
%   @arg MFL A structure 'mfl4(_, M, F, L)' where:
%            - 'M' is the module in which 'H' is defined.
%            - 'F' is the file where the definition of 'H' is located.
%            - 'L' is the line number of 'H' in the source file.
%
%   The predicate works by checking multiple properties of 'H':
%   - It tries to determine the module 'M' where 'H' is imported from or defined.
%   - It tries to extract the line number 'L' of the predicate definition.
%   - It attempts to find the file 'F' where the predicate 'H' is defined, including handling
%     cases where 'H' is a foreign predicate.
%
call_only_based_mfl(H, mfl4(_VarNameZ, M, F, L)) :-
    % Attempt to find the module 'M' where the predicate 'H' is imported from or defined.
    ignore(predicate_property(H, imported_from(M)); predicate_property(H, module(M))),
    % Attempt to retrieve the line number 'L' of the predicate 'H'.
    ignore(predicate_property(H, line_count(L))),
    % Try to determine the file 'F' where the predicate 'H' is defined. Handle foreign predicates.
    ignore(source_file(M:H, F); predicate_property(H, file(F));
           (predicate_property(H, foreign), F = foreign)).

%!  uses_call_only(+H) is semidet.
%
%   Determines if the given predicate 'H' can be classified as "call-only," meaning it is
%   either a foreign predicate or a compiled predicate (not interpreted).
%
%   @arg H The predicate to check.
%
%   The predicate works as follows:
%   - If 'H' is a foreign predicate, it succeeds immediately.
%   - If 'H' has any properties but is not interpreted, it also succeeds.
%   - Otherwise, the predicate fails.
%
%   @example
%   % Check if a predicate is call-only:
%   ?- uses_call_only(my_foreign_predicate).
%   true.
%
uses_call_only(H) :-
    % Succeed immediately if the predicate 'H' is foreign.
    predicate_property(H, foreign), !.
uses_call_only(H) :-
    % Succeed if the predicate 'H' has any properties but is not interpreted.
    predicate_property(H, _),
    \+ predicate_property(H, interpreted), !.

%!  clause_match(+H, +B, -Ref) is semidet.
%
%   Matches a clause for a given head 'H' and body 'B', and retrieves the reference 'Ref'.
%   This predicate determines how to match clauses or call-only predicates.
%
%   @arg H   The head of the clause or predicate.
%   @arg B   The body of the clause.
%   @arg Ref A reference to the clause that matched, or an indication that the predicate
%            is call-only.
%
%   The predicate works as follows:
%   - If 'H' is a call-only predicate, it succeeds immediately, returning 'uses_call_only(H)' as 'Ref'.
%   - If the clause is already asserted, it succeeds and returns the reference to that clause.
%   - Otherwise, it checks for clause matching using 'clause/3' and compares the head with '=@=/2'.
%     It ensures the body 'B' is not a reserved body by using 'reserved_body_helper/1'.
%
clause_match(H, _B, uses_call_only(H)) :-
    % If 'H' is a call-only predicate, succeed immediately with 'uses_call_only(H)' as 'Ref'.
    uses_call_only(H), !.
clause_match(H, B, Ref) :-
    % Match an asserted clause for head 'H' and body 'B', retrieving 'Ref' as the clause reference.
    locally_clause_asserted(H, B, Ref), !.
clause_match(H, B, Ref) :-
    % Copy the head 'H' to a temporary variable 'HH' and check for clause matching.
    % Ensure that 'H' is structurally equal to 'HH' (using '=@=') and that the body is not reserved.
    ((copy_term(H, HH), clause(H,B,Ref),H =@= HH)*->true ;clause(H,B,Ref)),\+ reserved_body_helper(B).

%!  find_mfl(+C, -MFL) is det.
%
%   Finds the module, file, and line number (MFL) for a given clause 'C'. The result is returned
%   in the form of a 'mfl4/4' structure.
%
%   @arg C   The clause or term for which metadata (module, file, line) is being sought.
%   @arg MFL The result, a structure 'mfl4(_, M, F, L)' where:
%            - 'M' is the module where 'C' is defined.
%            - 'F' is the file where the definition of 'C' is located.
%            - 'L' is the line number in the source file.
%
%   The predicate works as follows:
%   - It tries to look up the MFL using 'lookup_spft_match/3'.
%   - If the clause 'C' is wrapped, it unwraps it and recursively finds the MFL for the unwrapped version.
%   - If none of the above succeed, it attempts to expand 'C' into a head 'H' and body 'B' and then
%     find their MFL using 'find_hb_mfl/4'.
%
find_mfl(C, MFL) :-
    % Try to look up the MFL for the clause 'C' using 'lookup_spft_match/3'.
    lookup_spft_match(C, MFL, ax).
find_mfl(C, MFL) :-
    % If 'C' can be unwrapped, recursively find the MFL for the unwrapped version.
    unwrap_litr0(C, UC) -> C \== UC -> find_mfl(UC, MFL).
find_mfl(C, MFL) :-
    % Expand 'C' into head 'H' and body 'B', then find their MFL.
    expand_to_hb(C, H, B),
    find_hb_mfl(H, B, _Ref, MFL) -> true ;
    (clause_match(H, B, Ref), find_hb_mfl(H, B, Ref, MFL)).

%!  find_hb_mfl(+H, +B, +Ref, -MFL) is semidet.
%
%   Finds the module, file, and line number (MFL) for a given head 'H', body 'B', and clause reference 'Ref'.
%   The result is returned in the form of a 'mfl4/4' structure.
%
%   @arg H   The head of the clause or predicate.
%   @arg B   The body of the clause.
%   @arg Ref A reference to the clause (if applicable).
%   @arg MFL The result, a structure 'mfl4(_, M, F, L)' where:
%            - 'M' is the module where 'H' is defined.
%            - 'F' is the file where the definition of 'H' is located.
%            - 'L' is the line number in the source file.
%
%   The predicate works as follows:
%   - If 'Ref' is atomic, it uses clause properties to find the line number and file.
%   - If no reference is available, it tries to find MFL using 'lookup_spft_match_first/3'.
%   - If 'H' is a call-only predicate, it uses 'call_only_based_mfl/2' to find the MFL.
%
find_hb_mfl(_H, _B, Ref, mfl4(_VarNameZ, M, F, L)) :-
    % If 'Ref' is atomic, use clause properties to retrieve the line number, file, and module.
    atomic(Ref),clause_property(Ref, line_count(L)),clause_property(Ref, file(F)),
        clause_property(Ref, module(M)).
find_hb_mfl(H, B, _, mfl4(VarNameZ, M, F, L)) :-
    % Try to find the MFL for the head-body pair using 'lookup_spft_match_first/3'.
    lookup_spft_match_first((H :- B), mfl4(VarNameZ, M, F, L), _), !.
find_hb_mfl(H, B, _Ref, mfl4(VarNameZ, M, F, L)) :-
    % Try to find the MFL for the head 'H' using 'lookup_spft_match_first/3', assuming 'B' is ground.
    lookup_spft_match_first(H, mfl4(VarNameZ, M, F, L), _), ground(B).
find_hb_mfl(H, _B, uses_call_only(H), MFL) :-
    % If 'H' is a call-only predicate, find the MFL using 'call_only_based_mfl/2'.
    !, call_only_based_mfl(H, MFL).

% The following directives deal with fixing up exports of predicates between modules.
% This is used to ensure that the correct predicates are exported from one module to another,
% often necessary for sharing code across different parts of the system.

% This directive fixes up the exports for the current module by adjusting the exports list,
% ensuring that all required predicates are properly exported. It may be used to ensure that
% predicates are available for use in other modules or systems.
%:- fixup_exports.

% The next line, which is commented out, shows a previous approach where exports were fixed
% up specifically for a module identified by the 'pfc_shared_module' flag (such as 'BaseKB').
% This would have ensured that exports are adjusted into 'BaseKB'. It is currently disabled.
%:- current_prolog_flag(pfc_shared_module,BaseKB),fixup_module_exports_into(BaseKB).

% This directive ensures that the exports from the current module are fixed up into the 'system' module.
% This is used to make predicates from the current module available to the 'system' module, which may be
% necessary for interoperability between the system and predicates of this module .
:- fixup_module_exports_into(system).

%!  mpred_rule_hb(+C, -H, -B) is semidet.
%
%   Decomposes a rule or clause 'C' into its head 'H' and body 'B'.
%   This predicate handles several common syntactic forms used in rules:
%   - '(H :- B)' is a standard Prolog rule with head 'H' and body 'B'.
%   - '(H <- B)' is a variant form where the head 'H' and body 'B' are reversed.
%   - '(B ==> H)' is used in certain Prolog extensions for forward chaining rules.
%   - '(==> H)' is a rule with no explicit body, which defaults to 'true' (i.e., always succeeds).
%   - '(HB1 <==> HB2)' represents bidirectional rules, which are handled by decomposing
%     each direction into 'H' and 'B'.
%
%   @arg C   The rule or clause to decompose.
%   @arg H   The head of the rule or clause.
%   @arg B   The body of the rule or clause.
%
%   @example
%   % Decompose a standard Prolog rule:
%   ?- mpred_rule_hb((foo :- bar), H, B).
%   H = foo,
%   B = bar.
%
%   @example
%   % Handle forward chaining rule syntax:
%   ?- mpred_rule_hb((bar ==> foo), H, B).
%   H = foo,
%   B = bar.
%
mpred_rule_hb(C, _) :-
    % Fail if 'C' is not a compound term.
    \+ compound(C), !, fail.
mpred_rule_hb((H :- B), H, B) :- !.
mpred_rule_hb((H <- B), H, B) :- !.
mpred_rule_hb((B ==> H), H, B) :- !.
mpred_rule_hb((==> H), H, true) :- !.
mpred_rule_hb((HB1 <==> HB2), (H1, H2), (B1, B2)) :-
    % Handle bidirectional rules, decomposing each side of the rule.
    !, (mpred_rule_hb((HB1 ==> HB2), H2, B2) ; mpred_rule_hb((HB2 ==> HB1), H1, B1)).

:- module_transparent((get_assertion_head_arg)/3).

%!  get_assertion_head_arg(+N, +P, -E) is det.
%
%   Retrieves the N-th argument 'E' from the unnegated head of the assertion 'P'.
%   This predicate first unnegates the head 'P' using 'get_assertion_head_unnegated/2'
%   and then extracts the N-th argument using 'arg/3'.
%
%   @arg N  The argument position to retrieve (1-based index).
%   @arg P  The predicate or rule from which the argument is to be extracted.
%   @arg E  The extracted argument.
%
get_assertion_head_arg(N, P, E) :-
    % Unnegate the head of 'P', binding it to 'PP'.
    get_assertion_head_unnegated(P, PP), !,
    % Retrieve the N-th argument from 'PP'.
    arg(N, PP, E).

%!  get_assertion_head_unnegated(+P, -PP) is det.
%
%   Retrieves the unnegated head 'PP' from a rule or predicate 'P'.
%   If the head 'H' is negated, it attempts to unnegate it using 'pfc_unnegate/2'.
%   If not negated, the head 'H' is simply returned as 'PP'.
%
%   @arg P  The predicate or rule.
%   @arg PP The unnegated head.
%
get_assertion_head_unnegated(P, PP) :-
    % Decompose 'P' into head 'H' and body '_' using 'mpred_rule_hb/3'.
    mpred_rule_hb(P, H, _),
    % If 'H' can be unnegated, bind it to 'PP'; otherwise, 'PP = H'.
    (pfc_unnegate(H, PP) -> true ; H == PP).

%!  replace_arg(+Q, +N, +NEW, -R) is det.
%
%   Creates a copy 'R' of term 'Q' with the N-th argument replaced by 'NEW'.
%   This is achieved by duplicating the term and using 'nb_setarg/3' to modify the argument.
%
%   @arg Q    The original term.
%   @arg N    The position of the argument to replace.
%   @arg NEW  The new value for the N-th argument.
%   @arg R    The result, a copy of 'Q' with the modified argument.
%
replace_arg(Q, N, NEW, R) :-
    % Create a duplicate of 'Q' in 'R'.
    duplicate_term(Q, R),
    % Ensure 'Q' and 'R' are the same term.
    Q = R,
    % Set the N-th argument in 'R' to 'NEW'.
    nb_setarg(N, R, NEW).

%!  if_missing_mask(+Q, ?R, ?Test) is semidet.
%
%   Applies a "missing mask" to the term 'Q', returning a modified term 'R' and a test 'Test'.
%   This predicate handles various cases of negation or missing arguments in the term 'Q':
%   - If 'Q' is of the form 'M:Q', the mask is applied to 'Q' in the module 'M'.
%   - If 'Q' is a simple (non-compound) term, the mask results in '~Q' and a test of '\+Q'.
%   - More complex terms are handled by decomposing the rule using 'mpred_rule_hb/3'.
%
%   @arg Q    The original term to apply the mask to.
%   @arg R    The result after applying the mask.
%   @arg Test The test condition associated with the mask.
%
if_missing_mask(M:Q, M:R, M:Test) :-
    % If 'Q' is of the form 'M:Q', apply the mask to 'Q' within module 'M'.
    nonvar(Q), !,
    if_missing_mask(Q, R, Test).
if_missing_mask(Q, ~Q, \+Q) :-
    % If 'Q' is not a compound term, result in '~Q' and a test of '\+Q'.
    \+ is_ftCompound(Q), !.
%if_missing_mask(ISA, ~ ISA, \+ ISA):- functor(ISA,F,1),(F==tSwim;call_u(functorDeclares(F))),!.
if_missing_mask(HB, RO, TestO) :-
    % Decompose 'HB' into head 'H' and body 'B'.
    once(mpred_rule_hb(HB, H, B)),B \== true,HB \== H, !,
    % Apply the mask to the head 'H'.
    if_missing_mask(H, R, TestO),
    % Substitute 'H' with 'R' in 'HB', resulting in 'RO'.
    subst(HB, H, R, RO).
if_missing_mask(ISA, ISA, \+ ISA) :-
    % If 'ISA' is a functor with one argument, apply a simple mask.
    functor(ISA, _F, 1), !.
if_missing_mask(Q, R, Test) :-
    % For more complex terms, find the missing argument position.
    which_missing_argnum(Q, N),
    % Apply the mask to the N-th argument.
    if_missing_n_mask(Q, N, R, Test), !.
if_missing_mask(ISA, ~ISA, \+ISA).

%!  if_missing_n_mask(+Q, +N, ?R, ?Test) is semidet.
%
%   Applies a "missing mask" to the N-th argument of the term 'Q'.
%   The modified term is returned in 'R', along with the associated test 'Test'.
%
%   @arg Q    The original term.
%   @arg N    The position of the argument to check for missingness.
%   @arg R    The result after applying the mask.
%   @arg Test The test condition for the masked argument.
%
if_missing_n_mask(Q, N, R, Test) :-
    % Retrieve the N-th argument of 'Q' into 'Was'.
    get_assertion_head_arg(N, Q, Was),
    % Modify the N-th argument, replacing it with 'NEW'.
    (nonvar(R) ->
        % If 'R' is already instantiated, find the missing argument in 'R'.
        (which_missing_argnum(R, RN), get_assertion_head_arg(RN, R, NEW))
    ;
        % Otherwise, replace the N-th argument in 'Q' with 'NEW', yielding 'R'.
        replace_arg(Q, N, NEW, R)
    ), !,
    % The test is a 'dif/2' comparison between 'Was' and 'NEW'.
    Test = dif:dif(Was, NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):-
 must_ex((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) ->
    (get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/

%! which_missing_argnum(+Q, ?N) is semidet.
%
%   Determines which argument of the compound term 'Q' is considered "missing."
%   The argument number is returned in 'N'. A "missing" argument is identified based on
%   whether it satisfies certain conditions (e.g., not a functor with arity 0, single-valued argument, etc.).
%
%   @arg Q The compound term being examined.
%   @arg N The position of the "missing" argument.
%
which_missing_argnum(Q, N) :-
    % Check that 'Q' is a compound term and has non-zero arity.
    compound(Q), \+ compound_name_arity(Q, _, 0),
    % Ensure 'Q' is an acyclic compound term.
    must_ex((acyclic_term(Q), is_ftCompound(Q), get_functor(Q, F, A))),
    F \= t,  % Exclude functors named 't'.
    % Check if the argument is single-valued using 'singleValuedInArg/2'.
    (call_u(singleValuedInArg(F, N)) -> true ; which_missing_argnum(Q, F, A, N)).

%! which_missing_argnum(+Q, +_F, +A, ?N) is semidet.
%
%   Iterates through the arguments of 'Q' and checks if any of them are "missing."
%   A missing argument is identified as a non-variable term.
%
%   @arg Q The term being examined.
%   @arg _F The functor (unused in this clause).
%   @arg A The arity of the term 'Q'.
%   @arg N The argument number of the missing argument.
%
which_missing_argnum(_, _, 1, _) :- !, fail.
which_missing_argnum(Q, _F, A, N) :-
    % Iterate over the arguments from 1 to 'A'.
    between(A, 1, N),
    % Get the N-th argument from 'Q' and check if it is non-variable.
    get_assertion_head_arg(N, Q, Was),
    is_ftNonvar(Was).

:-use_module(library(lists)).

%!  justification(+F, -J) is semidet.
%
%   Retrieves a justification 'J' for a fact 'F'. A justification typically represents
%   the support or reason why a fact is considered true.
%
%   @arg F The fact for which the justification is being sought.
%   @arg J The justification for the fact.
%
justification(F, J) :-
    % Find a justification for fact 'F' using 'supports/2'.
    supports(F, J).

%!  justifications(+F, -Js) is det.
%
%   Retrieves all justifications 'Js' for a fact 'F'. The justifications are collected into
%   a list 'Js' using 'bagof/3', which gathers all possible solutions for 'justification/2'.
%
%   @arg F The fact for which justifications are being sought.
%   @arg Js The list of all justifications for the fact.
%
justifications(F, Js) :-
    % Collect all justifications for fact 'F' into list 'Js'.
    bagof(J, justification(F, J), Js).

%!  base(+P, -L) is semidet.
%
%   True iff 'L' is a list of "base" facts which, when combined, allow
%   us to deduce 'P'. A "base" fact is either an axiom (a fact added by the user or a
%   raw Prolog fact with no support) or an assumption (a failed goal that we assume to be true).
%
%   @arg P The fact for which the base facts are being deduced.
%   @arg L A list of base facts that justify the deduction of 'P'.
%
base(F, [F]) :-
    % A base fact is either an axiom or an assumption.
    (axiom(F) ; assumption(F)), !.
base(F, L) :-
    % If 'F' is not a direct base fact, retrieve its justifications.
    justification(F, Js),
    % Recursively find the base facts for each justification.
    bases(Js, L).

%!  bases(+L1, -L2) is semidet.
%
%   True if 'L2' is the union of all the base facts on which the conclusions
%   in the list 'L1' are based. This recursively breaks down the justifications
%   in 'L1' into their component base facts and combines them into 'L2'.
%
%   @arg L1 The list of conclusions (facts) whose base facts are being sought.
%   @arg L2 The resulting list of base facts.
%
% %  bases(L1,L2) is true if list L2 represents the union of all of the
% %  facts on which some conclusion in list L1 is based.

bases([], []).
bases([X | Rest], L) :-
    % Recursively find the base facts for 'X'.
    base(X, Bx),
    % Recursively find the base facts for the rest of the list.
    bases(Rest, Br),
    % Combine the base facts using 'pfcUnion/3' to avoid duplicates.
    pfcUnion(Bx, Br, L).

%!  axiom(+F) is semidet.
%
%   True if 'F' is an axiom. An axiom is either a fact supported by the special
%   support '(god,god)' or matched by 'matches_why_UU(UU)'. An axiom is a fact that
%   was either added by the user or is a raw Prolog fact with no further support.
%
%   @arg F The fact to check for being an axiom.
%
axiom(F) :-
    % Check if 'F' matches a special support condition 'matches_why_UU(UU)'.
    matches_why_UU(UU),
    % Verify that the support for 'F' is either 'UU' or '(god,god)'.
    pfcGetSupport(F, UU) ; pfcGetSupport(F, (god, god)).


%!  assumption(+P) is semidet.
%
%   Assumptions typically arise from failure to prove a goal, which is
%   treated as an implicit assumption of its negation.
%
%   An assumption is a failed goal, meaning that our inability to prove 'P' is taken as a proof of 'not(P)'.
%
%   True if 'P' is an assumption. An assumption is defined as any goal 'P' that,
%   when unnegated using 'pfc_unnegate/2', does not have a proof, and thus its negation is assumed.
%
assumption(P) :-
    % Unnegate 'P' to check if it represents an assumption.
    pfc_unnegate(P, _).

%!  assumptions(+X, -As) is det.
%
%   True if 'As' is the set of assumptions that underlie the fact 'X'.
%   If 'X' is an assumption, 'As' contains only 'X'. If 'X' is an axiom, there are no assumptions.
%   Otherwise, it recursively collects assumptions from the justifications of 'X'.
%
%   @arg X  The fact whose assumptions are being deduced.
%   @arg As The resulting list of assumptions that underlie 'X'.
%
assumptions(X, [X]) :-
    % If 'X' is an assumption, the result is '[X]'.
    assumption(X).
assumptions(X, []) :-
    % If 'X' is an axiom, there are no assumptions.
    axiom(X).
assumptions(X, L) :-
    % If 'X' has justifications, recursively gather assumptions from them.
    justification(X, Js),assumptions1(Js, L).

%!  assumptions1(+Js, -L) is det.
%
%   Helper predicate to process a list of justifications 'Js' and collect
%   all assumptions into 'L'. This applies the 'assumptions/2' predicate
%   recursively to each justification and unions the results.
%
%   @arg Js The list of justifications.
%   @arg L  The resulting list of assumptions.
%
assumptions1([], []).
assumptions1([X | Rest], L) :-
    % Recursively get assumptions for each justification 'X'.
    assumptions(X, Bx),
    % Get assumptions for the rest of the justifications.
    assumptions1(Rest, Br),
    % Union the assumptions from 'X' and the rest into 'L'.
    pfcUnion(Bx, Br, L).

%!  pfcProofTree(+P, -T) is det.
%
%   Constructs the proof tree 'T' for the fact 'P'. A proof tree is a nested list
%   where the root is 'P', and each child represents a justification for 'P'.
%   The form of a proof tree is:
%   - '[P, J1, J2, ..., Jn]', where each 'Ji' is an independent justifier for 'P'.
%   - Each justifier 'Ji' itself can have its own proof tree in the form '[Ji1, Ji2, ..., Jik]'.
%
%   @arg P The fact for which the proof tree is being constructed.
%   @arg T The resulting proof tree structure.
%

%! pfcChild(+P, ?Q) is semidet.
%
%   True if 'P' is an immediate justifier for 'Q'. This predicate looks up
%   whether 'P' directly supports 'Q' by checking the support data.
%
%   @arg P The potential justifier (parent).
%   @arg Q The fact to check (child).
%
pfcChild(P, Q) :-
    % Check if 'P' directly supports 'Q'.
    pfcGetSupport(Q, (P, _)).
pfcChild(P, Q) :-
    % If 'P' is a trigger for 'Q', recursively check for support.
    pfcGetSupport(Q, (_, Trig)),
    pfcType(Trig, trigger(_Pos)),
    pfcChild(P, Trig).

%! pfcChildren(+P, -L) is det.
%
%   Collects the immediate children of 'P' into a list 'L'. These are all the
%   facts that are directly supported by 'P'.
%
%   @arg P The fact whose children are being collected.
%   @arg L The list of immediate children.
%
pfcChildren(P, L) :-
    % Collect all immediate justifiers (children) of 'P' using 'bagof_or_nil/3'.
    bagof_or_nil(C, pfcChild(P, C), L).

%! pfcDescendant(+P, ?Q) is semidet.
%
%   True if 'P' is an ancestor (justifier) for 'Q'. This predicate recursively
%   checks whether 'P' supports 'Q', either directly or indirectly.
%
%   @arg P The potential ancestor (justifier).
%   @arg Q The fact to check (descendant).
%
pfcDescendant(P, Q) :-
    % Recursively find if 'P' is an ancestor of 'Q'.
    pfcDescendant1(P, Q, []).

%! pfcDescendant1(+P, +Q, +Seen) is semidet.
%
%   Helper predicate that tracks the 'Seen' list to avoid cycles when recursively
%   checking for descendants.
%
%   @arg P The ancestor being checked.
%   @arg Q The fact being checked for being a descendant.
%   @arg Seen The list of previously visited nodes to avoid cycles.
%
pfcDescendant1(P, Q, Seen) :-
    % Check if 'X' is an immediate justifier for 'Q'.
    pfcChild(X, Q),
    % Ensure 'X' has not been visited before.
    (\+ member(X, Seen)),
    % Either 'P' is 'X' or continue recursively checking ancestors.
    (P = X ; pfcDescendant1(P, X, [X | Seen])).

%! pfcDescendants(+P, -L) is det.
%
%   Collects all descendants of 'P' (facts that are directly or indirectly
%   justified by 'P') into a list 'L'.
%
%   @arg P The ancestor being checked.
%   @arg L The list of all descendants of 'P'.
%
pfcDescendants(P, L) :-
    % Collect all descendants of 'P' using 'bagof_or_nil/3'.
    bagof_or_nil(Q, pfcDescendant1(P, Q, []), L).

/*
current_why_U(U):- must_ex(current_why(Why)), U = user(Why).
current_why_UU(UU):- current_why_U(U), UU= (U,U).
matches_why_U(U):-  freeze(U,U=user(_)).
matches_why_UU(UU):- matches_why_U(U1),matches_why_U(U2), freeze(UU,UU=(U1,U2)).
*/

%!   current_why_U(-U) is det.
%
%   Retrieves the current user reason 'U' using 'get_why_uu/1'. This represents
%   a reason why a fact is true, expressed as a user context.
%   In this implementation, 'get_why_uu/1' provides the user context.
%
%   @arg U The current user reason.
%
current_why_U(U) :-
    % Retrieve the first element of the tuple (U, _) from 'get_why_uu/1'.
    get_why_uu((U, _)).

%!  current_why_UU(-UU) is det.
%
%   Retrieves the current pair of user reasons 'UU', where both components
%   of the tuple represent user reasons for a fact. This uses 'get_why_uu/1'
%   to retrieve the current justification pair.
%
%   @arg UU The current pair of user reasons.
%
current_why_UU(UU) :-
    % Retrieve the pair of user reasons using 'get_why_uu/1'.
    get_why_uu(UU).

%!  matches_why_U(+U) is semidet.
%
%   Matches the current user reason with 'U'. This predicate uses 'freeze/2'
%   to ensure that 'U' can be matched lazily, ensuring it does not fail prematurely.
%
%   @arg U The user reason to match.
%
matches_why_U(U) :-
    % Attempt to match 'U' with the current user reason using 'freeze/2'.
    nop((current_why_U(Y), freeze(U, \+ \+ (U = Y; true)))).

%!  matches_why_UU(+UU) is semidet.
%
%   Matches 'UU' to the current pair of user reasons. This predicate ensures
%   that 'UU' is a valid pair of user reasons, delegating to 'only_is_user_reason/1'.
%
%   @arg UU The user reason pair to match.
%
matches_why_UU(UU) :-
    % Ensure 'UU' is a valid pair of user reasons.
    nop(only_is_user_reason(UU)).

%!  matterialize_support_term(+S, -Sup) is det.
%
%   Converts a supported term 'S' into a materialized support term 'Sup'. If 'S' has
%   attached variables (e.g., constraints), those goals are added to the support term.
%   If no attached variables exist, 'S' is returned unchanged.
%
%   @arg S   The original support term.
%   @arg Sup The materialized support term, possibly with goals.
%
matterialize_support_term(S, Sup) :-
    % Check if the term 'S' has any attached variables (constraints).
    term_attvars(S, Atts),Atts \== [] ->
    % If attached variables exist, copy 'S' and gather the associated goals.
    copy_term(S, _, Goals), Sup = S + Goals, !.
%
%   The following predicate is a fallback when the term 'SS' has no attached variables, ensuring
%   that the original term is returned without modification. Note: variation of above predicate.
%
matterialize_support_term(SS, SS).

:- set_prolog_flag(pfc_term_expansion, false).

%!  pfc_system_term_expansion(+I, +S0, -O, -S1) is semidet.
%
%   Handles term expansion in Prolog, enabling or disabling the term expansion mechanism
%   for PFC (Prolog Forward Chaining) based on the current Prolog flags and source file type.
%   This predicate checks whether term expansion is enabled ('pfc_term_expansion' flag),
%   and if the file being loaded is a '.pfc.pl' file or the 'pfc_term_expansion' flag is true.
%
%   @arg I  The input term to be expanded.
%   @arg S0 The initial state before expansion.
%   @arg O  The output term after expansion.
%   @arg S1 The final state after expansion.
%
pfc_system_term_expansion(I, S0, O, S1) :-
    % Check if the 'pfc_term_expansion' flag is not set to false.
    (\+ current_prolog_flag(pfc_term_expansion, false),
    % Check if the file is a '.pfc.pl' file or if 'pfc_term_expansion' is true.
    ( \+ \+ (source_location(File, _), atom_concat(_, '.pfc.pl', File)) ;
      current_prolog_flag(pfc_term_expansion, true))
    ) ->
    % If term expansion is enabled, attempt to expand the input term 'I'.
    once((
        prolog_load_context('term', T),
        nop(writeln(T)),
        T =@= I
    )) ->
    % Perform the term expansion and verify that 'I' and 'O' are not the same.
    (pfc_term_expansion(I, O) -> I \=@= O -> S0 = S1, fbugio(I --> O)).

% Declare 'system:term_expansion/4' as multifile to allow custom term expansion rules.
:- multifile(system:term_expansion/4).

% Asserting a custom term expansion rule into the system 'term_expansion/4'.
% This ensures that 'pfc_system_term_expansion/4' is invoked during the term expansion process.
:- asserta((system:term_expansion(I, S0, O, S1) :-
    pfc_system_term_expansion(I, S0, O, S1))).

% :- listing(term_expansion/4).

end_of_file.




















%! is_fc_body( +P) is semidet.
%
%   Determines if the given predicate P is a forward chaining body.
%
%   This predicate checks whether P represents a forward chaining rule
%   by checking if it contains the atom 'fwc'.
%
%   @arg P The predicate to check.
%
is_fc_body(P):- has_body_atom(fwc,P).

%! is_bc_body( +P) is semidet.
%
%   Determines if the given predicate P is a backchaining body.
%
%   This predicate checks whether P represents a backchaining rule
%   by checking if it contains the atom 'bwc'.
%
%   @arg P The predicate to check.
%
is_bc_body(P):- has_body_atom(bwc,P).

%! is_action_body( +P) is semidet.
%
%   Determines if the given predicate P is an action body.
%
%   This predicate checks whether P represents an action body
%   by checking if it contains the atom 'wac'.
%
%   @arg P The predicate to check.
%
is_action_body(P):- has_body_atom(wac,P).

%! has_body_atom( +WAC, ?P) is semidet.
%
%   Checks if the predicate P has WAC as an atom in its body.
%
%   This predicate succeeds if WAC is exactly equal to P. If P is a compound term,
%   it recursively checks whether WAC is present in the body of P by examining its first argument.
%
%   @arg WAC The atom to check for in the body.
%   @arg P The predicate to inspect.
%
has_body_atom(WAC,P):- call(
   WAC==P -> true ; (is_ftCompound(P), get_assertion_head_arg(1,P,E), has_body_atom(WAC,E))),!.

/*
% Commented-out alternative implementation for has_body_atom.
% This may have been a previous or incomplete approach for handling more complex cases.

% has_body_atom(WAC,P,Rest):- call(WAC==P -> Rest = true ;
%   (is_ftCompound(P), functor(P,F,A), is_atom_body_pfa(WAC,P,F,A,Rest))).
% is_atom_body_pfa(WAC,P,F,2,Rest):- get_assertion_head_arg(1,P,E), E==WAC, get_assertion_head_arg(2,P,Rest), !.
% is_atom_body_pfa(WAC,P,F,2,Rest):- get_assertion_head_arg(2,P,E), E==WAC, get_assertion_head_arg(1,P,Rest), !.
*/

%! same_functors( +Head1, +Head2) is det.
%
%   Checks if two predicate heads have the same functor and arity.
%
%   @arg Head1 The first predicate head to compare.
%   @arg Head2 The second predicate head to compare.
%
same_functors(Head1, Head2):-
    must_det(get_unnegated_functor(Head1, F1, A1)),
    must_det(get_unnegated_functor(Head2, F2, A2)),
    !, F1 = F2, A1 = A2.

%! mpred_update_literal( +P, ?N, ?Q, ?R) is semidet.
%
%   Updates the N-th argument of the predicate P with Q and returns the updated result in R.
%
%   The predicate P is updated by replacing its N-th argument with a new value Q. The new
%   predicate R is returned as a result.
%
%   @arg P The predicate to update.
%   @arg N The position of the argument to update.
%   @arg Q The new value for the N-th argument.
%   @arg R The resulting predicate after the update.
%
mpred_update_literal(P, N, Q, R):- get_assertion_head_arg(N, P, UPDATE),
    call(replace_arg(P, N, Q_SLOT, Q)),must_ex(call_u(Q)),update_value(Q_SLOT, UPDATE, NEW),
    replace_arg(Q, N, NEW, R).

% '$spft'(MZ,5,5,5).

%! update_single_valued_arg(+Module, +P, ?N) is semidet.
%
%   Updates the N-th argument of predicate P in the specified module.
%
%   This predicate is used to ensure that certain arguments in predicates
%   are updated with a single consistent value, particularly in forward-chaining
%   or backchaining logic systems. If the update requires cross-module logic, it
%   handles module transparency as well.
%
%   @arg Module The module where the predicate is defined.
%   @arg P The predicate whose argument needs updating.
%   @arg N The argument position to update.
%
:- module_transparent((update_single_valued_arg)/3).

update_single_valued_arg(M, M:Pred, N):- !, update_single_valued_arg(M, Pred, N).
update_single_valued_arg(_, M:Pred, N):- !, update_single_valued_arg(M, Pred, N).
update_single_valued_arg(world, P, N):-  !, current_prolog_flag(pfc_shared_module, BaseKB),
    update_single_valued_arg(BaseKB, P, N).
update_single_valued_arg(M, P, N):- ibreak,\+ clause_b(mtHybrid(M)),trace,clause_b(mtHybrid(M2)),!,
    update_single_valued_arg(M2, P, N).
update_single_valued_arg(M, P, N):- get_assertion_head_arg(N, P, UPDATE),is_relative(UPDATE),!,dtrace,
    ibreak,replace_arg(P, N, OLD, Q),
    must_det_ll((clause_u(Q), update_value(OLD, UPDATE, NEW), \+ is_relative(NEW), replace_arg(Q, N, NEW, R))),
    !, update_single_valued_arg(M, R, N).
update_single_valued_arg(M, P, N):-
    call_u((must_det_ll((
        call_u(mtHybrid(M)),
        mpred_type_args \= M,
        mpred_kb_ops \= M,
        get_assertion_head_arg(N, P, UPDATE),
        replace_arg(P, N, Q_SLOT, Q),
        var(Q_SLOT),
        same_functors(P, Q),
        must_det_ll((
            assertz(M:P),
            doall((
                lookup_u(M:Q, E),
                UPDATE \== Q_SLOT,
                erase(E),
                mpred_unfwc1(M:Q))))))))).

% =======================
% utils
% =======================

%! map_literals( +P, ?G) is semidet.
%
%   Applies the predicate P to every literal in G.
%
%   This predicate maps the given predicate P over each literal in the structure G.
%   It recursively applies P to all parts of G that are considered literals.
%
%   @arg P The predicate to apply to each literal.
%   @arg G The structure or list of literals to which P is applied.
%
map_literals(P,G):- map_literals(P,G,[]).

%! map_literals( +VALUE1, :TermH, ?VALUE3) is semidet.
%
%   A helper predicate for map_literals/2.
%
%   This variant of map_literals works with additional state or arguments.
%   It skips over variables and non-compound terms.
%
%   @arg VALUE1 The predicate to apply to each literal.
%   @arg TermH The term being processed.
%   @arg VALUE3 The state or list of results after applying the predicate.
%
map_literals(_,H,_):- is_ftVar(H),!.  % Skip variables
map_literals(_,[],_) :- !.  % Handle empty lists
map_literals(Pred,(H,T),S):- !, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):- !, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- mpred_literal(H), must_ex(apply(Pred,[H|S])), !.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H), !.  % Skip non-compound terms
map_literals(Pred,H,S):- H=..List, !, map_literals(Pred,List,S), !.

%! map_unless( :PRED1Test, ?Pred, ?H, ?S) is semidet.
%
%   Applies a predicate to a term unless a test condition is met.
%
%   This predicate applies the given predicate Pred to term H unless the test
%   predicate Test succeeds for H. If the test fails, the mapping proceeds.
%
%   @arg PRED1Test The test predicate that halts mapping if true.
%   @arg Pred The predicate to map to the term.
%   @arg H The head term to be processed.
%   @arg S The state passed to the predicate.
%
map_unless(Test,Pred,H,S):- call(Test,H), ignore(apply(Pred,[H|S])), !.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H), !.  % Skip non-compound terms
map_unless(Test,Pred,(H,T),S):- !, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):- !, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):- H=..List, !, map_unless(Test,Pred,List,S), !.

%! map_first_arg( +Pred, ?List) is semidet.
%
%   Applies the given predicate to the first argument of each element in List.
%
%   This predicate processes a list, applying the predicate Pred to the first argument
%   of each term in the list. It works recursively to handle lists and structures.
%
%   @arg Pred The predicate to apply to each element first argument.
%   @arg List The list of terms to process.
%
:- meta_predicate(map_first_arg(*,+)).
map_first_arg(CMPred,List):- strip_module(CMPred,CM,Pred), map_first_arg(CM,Pred,List,[]).

%! map_first_arg( +Pred, :TermH, ?S) is semidet.
%
%   A variant of map_first_arg/2 that processes terms with additional arguments.
%
%   This variant processes terms by applying Pred to their first argument and then
%   recursively processes the rest of the structure.
%
%   @arg Pred The predicate to apply.
%   @arg TermH The term being processed.
%   @arg S The state or context passed to the predicate.
%
:- meta_predicate(map_first_arg(+,*,+,+)).
map_first_arg(CM,Pred,H,S):- is_ftVar(H),!, CM:apply(Pred,[H|S]).
map_first_arg(_,_,[],_) :- !.
map_first_arg(CM,Pred,(H,T),S):- !, map_first_arg(CM,Pred,H,S), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,(H;T),S):- !, map_first_arg(CM,Pred,H,S); map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,[H|T],S):- !, CM:apply(Pred,[H|S]), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,H,S):- CM:apply(Pred,[H|S]).

%:- fixup_exports.

% % :- ensure_loaded(logicmoo(util/rec_lambda)).

%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});-mpred_isa(I,C))).
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))).

%! pfcVerifyMissing( +GC, ?GO, ?GO) is semidet.
%
%   Verifies the missing rule in forward chaining for predicate GO.
%
%   This predicate checks if a forward-chained rule derived from GC to GO is missing.
%   It constructs the predicate GO by copying the structure of GC and substituting
%   its last argument.
%
%   @arg GC The original predicate structure.
%   @arg GO The generated predicate.
%
pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-
    GC=..[F,A|Args], append(Left,[D],Args), append(Left,[C],NewArgs), GO=..[F,A|NewArgs], !.

%example mpred_freeLastArg(mpred_isa(I,C),~(mpred_isa(I,C))):-is_ftNonvar(C),!.
%example mpred_freeLastArg(mpred_isa(I,C),(mpred_isa(I,F),C\=F)):-!.

%! mpred_freeLastArg( +G, ?GG) is semidet.
%
%   Frees the last argument of a predicate.
%
%   This predicate removes the last argument of the given predicate G and returns
%   the result in GG.
%
%   @arg G The predicate whose last argument is to be freed.
%   @arg GG The resulting predicate after removing the last argument.
%
mpred_freeLastArg(G,GG):-
    G=..[F,A|Args], append(Left,[_],Args), append(Left,[_],NewArgs), GG=..[F,A|NewArgs], !.
mpred_freeLastArg(_G, false).

%! mpred_current_op_support( +VALUE1) is semidet.
%
%   Retrieves the current operator support in PFC.
%
%   This predicate returns a default operator support value '(p,p)' for PFC rules.
%
%   @arg VALUE1 The value representing the current operator support.
%
mpred_current_op_support((p,p)):- !.

%! pfcVersion( +VALUE1) is semidet.
%
% Prolog Forward Chaining Version.
%
%pfcVersion(6.6).

% % :- '$set_source_module'(mpred_kb_ops).

%! correctify_support( +S, ?S) is semidet.
%
%   Corrects or normalizes support for a predicate.
%
%   This predicate ensures that the support for a given predicate S is normalized.
%   It handles various forms of support and returns a standardized format.
%
%   @arg S The support structure to correct.
%
correctify_support(U,(U,ax)):- var(U), !.
correctify_support((U,U),(U,ax)):- !.
correctify_support((S,T),(S,T)):- !.
correctify_support((U,_UU),(U,ax)):- !.
correctify_support([U],S):- correctify_support(U,S).
correctify_support(U,(U,ax)).

%! clause_asserted_local( :TermABOX) is semidet.
%
%   Checks if a clause is locally asserted in the knowledge base.
%
%   This predicate verifies if a clause has been asserted locally, using internal
%   representations of facts and their associated triggers.
%
%   @arg TermABOX The term to check for local assertion.
%
clause_asserted_local(MCL):-
    must_ex(strip_mz(MCL,MZ,CL)),
    must_ex(CL='$spft'(MZ,P,Fact,Trigger )),!,
    clause_u('$spft'(MZ,P,Fact,Trigger),true,Ref),
    clause_u('$spft'(MZ,UP,UFact,UTrigger),true,Ref),
    (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).

%! is_already_supported( +P, ?S, ?UU) is semidet.
%
%   Checks if a predicate P is already supported.
%
%   This predicate determines whether a given predicate P has already been supported
%   by inspecting the knowledge base for existing support clauses.
%
%   @arg P The predicate to check.
%   @arg S The current support.
%   @arg UU The support structure, if found.
%
is_already_supported(P,(S,T),(S,T)):- clause_asserted_local('$spft'(_MZ,P,S,T)),!.
is_already_supported(P,_S,UU):- clause_asserted_local('$spft'(_MZ,P,US,UT)),must_ex(get_source_uu(UU)),
    UU=(US,UT).

%! if_missing1( +Q) is semidet.
%
%   Triggers an action if a certain predicate Q is missing.
%
%   This predicate calls a handler if the literal Q is missing from the knowledge base.
%
%   @arg Q The literal to check.
%
if_missing1(Q):- mpred_literal_nv(Q),call_u(\+ ~Q),if_missing_mask(Q,R,Test),!,lookup_u(R),Test.

%! mpred_run_pause is det.
%
%   Pauses the forward-chaining engine.
%
%   This predicate pauses the forward-chaining reasoning process by asserting a
%   flag indicating the pause.
%
mpred_run_pause:- asserta(t_l:mpred_run_paused).

%! mpred_run_resume is det.
%
%   Resumes the forward-chaining engine.
%
%   This predicate resumes the forward-chaining reasoning process by retracting
%   the pause flag.
%
mpred_run_resume:- retractall(t_l:mpred_run_paused).

%! fwithout_running( +G) is det.
%
%   Runs a goal G without allowing forward chaining to execute.
%
%   This predicate ensures that the given goal G is executed in a context where
%   forward chaining is paused. If it is already paused, G is simply executed.
%
%   @arg G The goal to execute.
%
fwithout_running(G):- (t_l:mpred_run_paused -> G ; locally_tl(mpred_run_pause,G)).

