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
 * Last Modified: $LastChangedDate$  // You will replace this with Git automation
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

/* LogicMOO Base FOL/PFC Setup
% Date: Dec 13, 2035
% Author: Douglas Miles
*/

% Ensuring only one instance of this directive is processed to prevent conflicts.
% :- if( \+ current_predicate(set_fileAssertMt/1)).

%! pfcAddSupport(+P, +Fact_Trigger) is det.
%
%   Asserts a support fact-triple into the Prolog database.
%
%   @arg P The predicate for which the support is being added.
%   @arg Fact_Trigger A tuple of Fact and Trigger used in the reasoning process.
%
%   /* previously: ... */
pfcAddSupport(P,(Fact,Trigger)) :- assert('$spft$'(P,Fact,Trigger)).

%! pfcGetSupport(+P, -Fact_Trigger) is semidet.
%
%   Retrieves a support fact-triple from the Prolog database.
%
%   @arg P The predicate for which the support is being retrieved.
%   @arg Fact_Trigger A tuple of Fact and Trigger used in the reasoning process.
%
%   /* previously: ... */
pfcGetSupport(P,(Fact,Trigger)) :- pfc_spft(P,Fact,Trigger).

%! pfc_spft(+P, ?F, ?T) is semidet.
%
%   A helper predicate to call the system's spft predicate.
%
%   @arg P Predicate concerned.
%   @arg F Fact involved.
%   @arg T Trigger involved.
%
%   /* previously: ... */
pfc_spft(P,F,T) :- pfcCallSystem('$spft$'(P,F,T)).

%! pfcRemOneSupport(+P, +Fact_Trigger) is det.
%
%   Removes a specific support fact from the system, ensuring at least one of the arguments is bound.
%
%   @arg P Predicate from which support is being removed.
%   @arg Fact_Trigger The specific fact and trigger tuple to be removed.
%
%   /* previously: ... */
pfcRemOneSupport(P,(Fact,Trigger)) :-
  must_ex(callable(P);callable(Fact);callable(Trigger)),
  pfcRetractOrWarn('$spft$'(P,Fact,Trigger)).

%! pfcRemOneSupportOrQuietlyFail(+P, +Fact_Trigger) is det.
%
%   Attempts to remove a support fact from the system, failing quietly if not possible.
%
%   @arg P Predicate from which support is being removed.
%   @arg Fact_Trigger The specific fact and trigger tuple targeted for removal.
%
%   /* previously: ... */
pfcRemOneSupportOrQuietlyFail(P,(Fact,Trigger)) :-
  must_ex(callable(P);callable(Fact);callable(Trigger)),
  pfcRetractOrQuietlyFail('$spft$'(P,Fact,Trigger)).

%! pfc_collect_supports(-Tripples) is nondet.
%
%   Collects all support triples from the system.
%
%   @arg Tripples The list of all support triples collected.
%
%   /* previously: ... */
pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

%! pfc_support_relation(-P_F_T) is nondet.
%
%   Describes the relationship between predicates, facts, and triggers in supports.
%
%   @arg P_F_T A tuple representing the predicate, fact, and trigger relationship.
%
%   /* previously: ... */
pfc_support_relation((P,F,T)) :-
  pfc_spft(P,F,T).

%! pfc_make_supports(+P_S1_S2) is nondet.
%
%   Creates support entries combining two supports into one.
%
%   @arg P_S1_S2 A tuple containing the predicate and two support elements to be combined.
%
%   /* previously: ... */
pfc_make_supports((P,S1,S2)) :-
  pfcAddSupport(P,(S1,S2)),
  (pfcAddType1(P); true),
  !.

%! pfcTriggerKey(+Trigger, -Key) is det.
%
%   Retrieves the key best suited for indexing a trigger.
%
%   @arg Trigger The trigger for which the key is being retrieved.
%   @arg Key The retrieved key.
%
%   /* previously: ... */
pfcTriggerKey('$pt$'(Key,_),Key).
pfcTriggerKey('$pt$'(Key,_,_),Key).
pfcTriggerKey('$nt$'(Key,_,_),Key).
pfcTriggerKey(Key,Key).

%! pfc_trigger_key(+X, -X) is det.
%
%   Simplified version of trigger key retrieval; fails if X is not bound.
%
%   @arg X Input which could either be a variable or a compound term.
%
%   /* previously: ... */
pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).

%! nb_pushval(+Name, +Value) is det.
%
%   Pushes a value onto a named stack implemented using non-backtrackable global variables.
%
%   @arg Name The name of the stack.
%   @arg Value The value to push onto the stack.
%
%   /* previously: ... */
nb_pushval(Name,Value):-nb_current(Name,Before)->nb_setval(Name,[Value|Before]);nb_setval(Name,[Value]).

%! nb_peekval(+Name, -Value) is semidet.
%
%   Peeks at the top value of a named stack without removing it.
%
%   @arg Name The name of the stack.
%   @arg Value The top value on the stack.
%
%   /* previously: ... */
nb_peekval(Name,Value):-nb_current(Name,[Value|_Before]).

%! nb_hasval(+Name, ?Value) is semidet.
%
%   Checks if a value is within a named stack.
%
%   @arg Name The name of the stack.
%   @arg Value The value to check for within the stack.
%
%   /* previously: ... */
nb_hasval(Name,Value):-nb_current(Name,List),member(Value,List).

%! nb_popval(+Name, -Value) is semidet.
%
%   Pops the top value from a named stack.
%
%   @arg Name The name of the stack.
%   @arg Value The value popped off the stack.
%
%   /* previously: ... */
nb_popval(Name,Value):-nb_current(Name,[Value|Before])->nb_setval(Name,Before).

%! reset_shown_justs is det.
%
%   Resets the justifications shown to the user.
%
%   /* previously: ... */
reset_shown_justs:- retractall(t_l:shown_why(_)),nop(color_line(red,1)).

%! clear_proofs is det.
%
%   Clears all stored proofs from the system.
%
%   /* previously: ... */
clear_proofs:- retractall(t_l:whybuffer(_P,_Js)),nop(color_line(cyan,1)).

%! lookup_spft_match(+A, +B, +C) is semidet.
%
%   Looks up supports that match a given condition, ensuring the original query remains unchanged.
%
%   @arg A Initial condition or predicate.
%   @arg B Secondary condition related to the fact.
%   @arg C Tertiary condition related to the trigger.
%
%   /* previously: ... */
lookup_spft_match(A,B,C):- copy_term(A,AA),lookup_spft(A,B,C),A=@=AA.

%! lookup_spft_match_deeper(+H, +Fact, +Trigger) is semidet.
%
%   Searches deeper for matches in support entries, ensuring the head remains unchanged after the lookup.
%
%   @arg H The head of the clause being matched.
%   @arg Fact The fact component in the support entry.
%   @arg Trigger The trigger component in the support entry.
%
%   /* previously: ... */
lookup_spft_match_deeper(H,Fact,Trigger):-
  copy_term(H,HH),
  lookup_spft((H:- _B),Fact,Trigger),
  H=@=HH.

%! lookup_spft_match_first(+A, +B, +C) is semidet.
%
%   Performs a first-match search for supports, ensuring no repeats and considering both match and direct lookup strategies.
%
%   @arg A Initial condition or predicate.
%   @arg B Secondary condition related to the fact.
%   @arg C Tertiary condition related to the trigger.
%
%   /* previously: ... */
lookup_spft_match_first(A,B,C):- nonvar(A),!,
  no_repeats(((lookup_spft_match(A,B,C);lookup_spft(A,B,C)))).

lookup_spft_match_first(A,B,C):- lookup_spft(A,B,C).

%! pfc_is_info(:TermC) is semidet.
%
%   Checks if a given term is classified as informational within the PFC context.
%
%   @arg TermC The term being checked for its informational status.
%
%   /* previously: ... */
pfc_is_info((CWC,Info)):- (atom(CWC),is_a_info(CWC));pfc_is_info(Info).
pfc_is_info(pfc_bc_only(C)):-is_ftNonvar(C),!.
pfc_is_info(infoF(C)):-is_ftNonvar(C),!.
pfc_is_info(inherit_above(_,_)).

%! is_a_info(+Term) is semidet.
%
%   Identifies if a term is recognized as an informational keyword within PFC.
%
%   @arg Term The term being checked.
%
%   /* previously: ... */
is_a_info(fail).
is_a_info(CWC):- is_pfc_chained(CWC).

%! is_pfc_chained(+Keyword) is semidet.
%
%   Determines if a keyword is part of the chained commands in PFC.
%
%   @arg Keyword The keyword being evaluated.
%
%   /* previously: ... */
is_pfc_chained(cwc).
is_pfc_chained(awc).
is_pfc_chained(zwc).
is_pfc_chained(fwc).
is_pfc_chained(bwc).
is_pfc_chained(wac).

%! assert_if_new(:Op) is det.
%
%   Asserts an operation if it is not already present in the database.
%
%   @arg Op The operation to potentially assert.
%
%   /* previously: ... */
:- forall(is_pfc_chained(Op),assert_if_new(Op)).

%! reserved_body(+B) is semidet.
%
%   Checks if the body of a clause is reserved or has special handling.
%
%   @arg B The body of the clause being evaluated.
%
%   /* previously: ... */
reserved_body(B):-var(B),!,fail.
reserved_body(attr_bind(_)).
reserved_body(attr_bind(_,_)).
reserved_body(B):-reserved_body_helper(B).

%! reserved_body_helper(+B) is semidet.
%
%   Helper for checking if a body structure has reserved handling.
%
%   @arg B The body structure to check.
%
%   /* previously: ... */
reserved_body_helper(B):- \+ compound(B),!,fail.
reserved_body_helper((ZAWC,_)):- atom(ZAWC),is_pfc_chained(ZAWC).

%! call_only_based_mfl(+H, -mfl4(_VarNameZ,M,F,L)) is semidet.
%
%   Determines the module, file, and line based on the given head of a clause.
%
%   @arg H The head of the clause.
%   @arg mfl4 A structure containing the module, file, and line details.
%
%   /* previously: ... */
call_only_based_mfl(H,mfl4(_VarNameZ,M,F,L)):-
  ignore(predicate_property(H,imported_from(M));predicate_property(H,module(M))),
  ignore(predicate_property(H,line_count(L))),
  ignore(source_file(M:H,F);predicate_property(H,file(F));(predicate_property(H,foreign),F=foreign)).

%! uses_call_only(+H) is semidet.
%
%   Checks if a predicate uses a call-only mechanism, particularly for foreign predicates.
%
%   @arg H The predicate being checked.
%
%   /* previously: ... */
uses_call_only(H):- predicate_property(H,foreign),!.
uses_call_only(H):- predicate_property(H,_), \+ predicate_property(H,interpreted),!.

%! clause_match(+H, ?B, ?Ref) is semidet.
%
%   Attempts to match a clause in the database, considering special handling for call-only predicates.
%
%   @arg H The head of the clause being matched.
%   @arg B The body of the clause.
%   @arg Ref The reference for clause matching.
%
%   /* previously: ... */
clause_match(H,_B,uses_call_only(H)):- uses_call_only(H),!.
clause_match(H,B,Ref):- clause_asserted(H,B,Ref),!.
clause_match(H,B,Ref):- ((copy_term(H,HH),clause(H,B,Ref),H=@=HH)*->true;clause(H,B,Ref)), \+ reserved_body_helper(B).

%! find_mfl(+C, -MFL) is nondet.
%
%   Finds the module, file, and line information for a given clause.
%
%   @arg C The clause being evaluated.
%   @arg MFL The module, file, and line details returned.
%
%   /* previously: ... */
find_mfl(C,MFL):- lookup_spft_match(C,MFL,ax).
find_mfl(C,MFL):- unwrap_litr0(C,UC) -> C\==UC -> find_mfl(UC,MFL).
find_mfl(C,MFL):- expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).

%! find_hb_mfl(+H, +B, +Ref, -mfl4(_VarNameZ,M,F,L)) is semidet.
%
%   Retrieves module, file, and line details based on the head and body of a clause.
%
%   @arg H The head of the clause.
%   @arg B The body of the clause.
%   @arg Ref The reference associated with the clause.
%   @arg mfl4 The structure containing module, file, and line information.
%
%   /* previously: ... */
find_hb_mfl(_H,_B,Ref,mfl4(_VarNameZ,M,F,L)):- atomic(Ref),clause_property(Ref,line_count(L)),
 clause_property(Ref,file(F)),clause_property(Ref,module(M)).
find_hb_mfl(H,B,_,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first( (H:-B),mfl4(VarNameZ,M,F,L),_),!.
find_hb_mfl(H,B,_Ref,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first(H,mfl4(VarNameZ,M,F,L),_),ground(B).
find_hb_mfl(H,_B,uses_call_only(H),MFL):- !,call_only_based_mfl(H,MFL).

%! fixup_exports is det.
%
%   Adjusts exports to ensure that all necessary predicates are available in the module interface.
%
%   /* previously: ... */
:- fixup_exports.

%! mpred_rule_hb(+C, -H, -B) is nondet.
%
%   Deconstructs a rule into its head and body components.
%
%   @arg C The complete rule.
%   @arg H The head of the rule.
%   @arg B The body of the rule.
%
%   /* previously: ... */
mpred_rule_hb(C,_):- \+ compound(C),!,fail.
mpred_rule_hb((H:-B),H,B):- !.
mpred_rule_hb((H<-B),H,B):- !.
mpred_rule_hb((B==>H),H,B):- !.
mpred_rule_hb((==>H),H,true):- !.
mpred_rule_hb((HB1<==>HB2),(H1,H2),(B1,B2)):- !, (mpred_rule_hb((HB1==>HB2),H2,B2);mpred_rule_hb((HB2==>HB1),H1,B1)).

%! get_assertion_head_arg(+N, +P, -E) is det.
%
%   Retrieves the argument at position N from the head of an assertion.
%
%   @arg N The position of the argument to retrieve.
%   @arg P The predicate or assertion containing the argument.
%   @arg E The argument retrieved.
%
%   /* previously: ... */
:- module_transparent( (get_assertion_head_arg)/3).
get_assertion_head_arg(N,P,E):-get_assertion_head_unnegated(P,PP),!,arg(N,PP,E).

%! get_assertion_head_unnegated(+P, -PP) is det.
%
%   Retrieves the unnegated form of the head of an assertion.
%
%   @arg P The predicate or assertion.
%   @arg PP The unnegated head of the assertion.
%
%   /* previously: ... */
get_assertion_head_unnegated(P,PP):- mpred_rule_hb(P,H,_), (pfc_unnegate(H,PP)->true;H==PP).

%! replace_arg(+Q, +N, +NEW, -R) is det.
%
%   Replaces the argument at position N in Q with NEW, resulting in R.
%
%   @arg Q The original term.
%   @arg N The position of the argument to replace.
%   @arg NEW The new argument.
%   @arg R The resulting term after replacement.
%
%   /* previously: ... */
replace_arg(Q,N,NEW,R):- duplicate_term(Q,R),Q=R,nb_setarg(N,R,NEW).

%! if_missing_mask(+Q, ?R, ?Test) is semidet.
%
%   Applies a masking condition to detect missing elements in a query.
%
%   @arg Q The original query.
%   @arg R The masked query result.
%   @arg Test The test condition applied to identify missing elements.
%
%   /* previously: ... */
if_missing_mask(M:Q,M:R,M:Test):- nonvar(Q),!,if_missing_mask(Q,R,Test).
if_missing_mask(Q,~Q,\+Q):- \+ is_ftCompound(Q),!.

%! if_missing_mask(+HB, -RO, -TestO) is semidet.
%
%   Applies a missing mask to a higher-order rule body, modifying it based on the missing elements.
%
%   @arg HB The original high-order rule body.
%   @arg RO The modified rule body after applying the missing mask.
%   @arg TestO The test condition applied to identify missing elements in the rule body.
%
%   /* previously: ... */
if_missing_mask(HB,RO,TestO):- once(mpred_rule_hb(HB,H,B)),B\==true,HB\==H,!,
     if_missing_mask(H,R,TestO),subst(HB,H,R,RO).

%! if_missing_mask(+ISA, -ISA, -Test) is semidet.
%
%   Handles missing masks specifically for terms that are functor-based, typically used in ISA structures.
%
%   @arg ISA The original ISA term.
%   @arg ISA The resulting term after applying the mask (identical due to the specific handling of functors).
%   @arg Test The test condition for the ISA structure.
%
%   /* previously: ... */
if_missing_mask(ISA, ISA, \+ ISA):- functor(ISA, _F,1),!.% (F==tSwim;call_u(functorDeclares(F))),!.

%! if_missing_mask(+Q, +R, -Test) is semidet.
%
%   General handler for applying missing masks, typically used when other specific handlers are not applicable.
%
%   @arg Q The original query or term.
%   @arg R The result after applying the missing mask.
%   @arg Test The test condition applied.
%
%   /* previously: ... */
if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_n_mask(Q,N,R,Test),!.

%! if_missing_mask(+ISA, -~ISA, -\+ISA) is semidet.
%
%   Specifically handles missing masks for functor-based ISA terms, applying negation.
%
%   @arg ISA The original ISA term.
%   @arg ~ISA The negated ISA term as a result of the mask.
%   @arg \+ISA The test condition applied, asserting the negation of ISA.
%
%   /* previously: ... */
if_missing_mask(ISA, ~ ISA, \+ ISA).

%! if_missing_n_mask(+Q, ?N, ?R, ?Test) is semidet.
%
%   Applies a missing mask based on a specific argument position identified within a term.
%
%   @arg Q The original query or term.
%   @arg N The argument position within Q to focus the missing mask on.
%   @arg R The result after applying the missing mask to the specific argument.
%   @arg Test The test condition applied, focusing on the specific argument.
%
%   /* previously: ... */
if_missing_n_mask(Q,N,R,Test):-
  get_assertion_head_arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),get_assertion_head_arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

%! which_missing_argnum(+VALUE1, ?VALUE2) is semidet.
%
%   Identifies the argument number in a term that is considered 'missing' based on specific conditions.
%
%   @arg VALUE1 The term being evaluated.
%   @arg VALUE2 The identified missing argument number.
%
%   /* previously: ... */
which_missing_argnum(Q,N):- compound(Q),\+ compound_name_arity(Q,_,0),
 must_ex((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (call_u(singleValuedInArg(F,N)) -> true; which_missing_argnum(Q,F,A,N)).

%! which_missing_argnum(+Q, +F, +A, -N) is semidet.
%
%   Specifically determines the missing argument number within a term that meets certain conditions.
%
%   @arg Q The term being evaluated.
%   @arg F The functor of Q.
%   @arg A The arity of Q.
%   @arg N The identified missing argument number.
%
%   /* previously: ... */
which_missing_argnum(_,_,1,_):-!,fail.
which_missing_argnum(Q,_F,A,N):- between(A,1,N),get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was).

%   File   : pfcjust.pl
%   Author : Tim Finin, finin@prc.unisys.com
%   Author :  Dave Matuszek, dave@prc.unisys.com
%   Updated:
%   Purpose: predicates for accessing Pfc justifications.
%   Status: more or less working.
%   Bugs:

%= *** predicates for exploring supports of a fact *****

%! justification(+F, -J) is nondet.
%
%   Retrieves the justification for a given fact.
%
%   @arg F The fact for which the justification is being retrieved.
%   @arg J The justification retrieved.
%
%   /* previously: ... */
:- use_module(library(lists)).

justification(F,J) :- supports(F,J).

%! justifications(+F, -Js) is nondet.
%
%   Retrieves all justifications for a given fact.
%
%   @arg F The fact for which justifications are being retrieved.
%   @arg Js The list of all justifications retrieved.
%
%   /* previously: ... */
justifications(F,Js) :- bagof(J,justification(F,J),Js).

%! base(+P, -L) is nondet.
%
%   Determines the base facts from which a given conclusion can be deduced.
%
%   @arg P The conclusion or predicate being analyzed.
%   @arg L The list of base facts deduced.
%
%   /* previously: ... */
base(F,[F]) :- (axiom(F) ; assumption(F)),!.

base(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification(F,Js),
  bases(Js,L).

%! bases(+L1, -L2) is nondet.
%
%   Computes the union of all base facts for a list of conclusions.
%
%   @arg L1 The list of conclusions being analyzed.
%   @arg L2 The resulting list of all base facts.
%
%   /* previously: ... */
bases([],[]).
bases([X|Rest],L) :-
  base(X,Bx),
  bases(Rest,Br),
  pfcUnion(Bx,Br,L).

%! axiom(+F) is semidet.
%
%   Checks if a given fact is an axiom.
%
%   @arg F The fact being checked.
%
%   /* previously: ... */
axiom(F) :-
  matches_why_UU(UU),
  pfcGetSupport(F,UU);
  pfcGetSupport(F,(god,god)).

%! assumption(+P) is semidet.
%
%   Determines if a failed goal can be considered an assumption.
%
%   @arg P The proposition being evaluated as an assumption.
%
%   /* previously: ... */
assumption(P) :- pfc_unnegate(P,_).

%! assumptions(+X, -As) is nondet.
%
%   Identifies all assumptions underlying a given conclusion.
%
%   @arg X The conclusion being analyzed.
%   @arg As The list of assumptions identified.
%
%   /* previously: ... */
assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

%! assumptions1(+X, -L) is nondet.
%
%   Helper predicate for recursively determining assumptions for a list of justifications.
%
%   @arg X The list of justifications being analyzed.
%   @arg L The resulting list of all underlying assumptions.
%
%   /* previously: ... */
assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  pfcUnion(Bx,Br,L).

%! pfcProofTree(+P, -T) is nondet.
%
%   Constructs a proof tree for a given proposition.
%
%   @arg P The proposition for which the proof tree is constructed.
%   @arg T The resulting proof tree.
%
%   /* previously: ... */
pfcProofTree(P,T) the proof tree for P is T where a proof tree is
%      of the form
%      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
%           ^                         and has the form of
%           [J11, J12,... J1n]      a list of proof trees.

%! pfcChild(+P, ?Q) is nondet.
%
%   Determines if P is an immediate justifier for Q.
%
%   @arg P The potential justifier.
%   @arg Q The proposition being justified.
%
%   /* previously: ... */
pfcChild(P,Q) :-
  pfcGetSupport(Q,(P,_)).

pfcChild(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfcType(Trig,trigger(_Pos)),
  pfcChild(P,Trig).

%! pfcChildren(+P, -L) is nondet.
%
%   Retrieves all immediate children or justifications directly linked to a given proposition.
%
%   @arg P The proposition being analyzed.
%   @arg L The list of all children or direct justifications.
%
%   /* previously: ... */
pfcChildren(P,L) :- bagof_or_nil(C,pfcChild(P,C),L).

%! pfcDescendant(+P, ?Q) is nondet.
%
%   Determines if P is a descendant justifier for Q.
%
%   @arg P The potential ancestor justifier.
%   @arg Q The proposition being justified.
%
%   /* previously: ... */
pfcDescendant(P,Q) :-
   pfcDescendant1(P,Q,[]).

%! pfcDescendant1(+P, +Q, +Seen) is nondet.
%
%   Helper for recursively determining descendant justifiers, tracking already seen justifiers to prevent loops.
%
%   @arg P The potential ancestor justifier.
%   @arg Q The proposition being justified.
%   @arg Seen The list of already seen justifiers to prevent circular reasoning.
%
%   /* previously: ... */
pfcDescendant1(P,Q,Seen) :-
  pfcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).

%! pfcDescendants(+P, -L) is nondet.
%
%   Retrieves all descendant justifiers for a given proposition.
%
%   @arg P The proposition being analyzed.
%   @arg L The list of all descendant justifiers.
%
%   /* previously: ... */
pfcDescendants(P,L) :-
  bagof_or_nil(Q,pfcDescendant1(P,Q,[]),L).

%! current_why_U(+U) is det.
%
%   Retrieves the current user associated with a justification.
%
%   @arg U The user identifier.
%
%   /* previously: ... */
current_why_U(U):-  get_why_uu((U,_)).% must_ex(current_why(Why)), U = user(Why).

%! current_why_UU(+UU) is det.
%
%   Retrieves the current user tuple associated with a justification.
%
%   @arg UU The user tuple identifier.
%
%   /* previously: ... */
current_why_UU(UU):- get_why_uu(UU). % current_why_U(U), UU= (U,U).

%! matches_why_U(+U) is semidet.
%
%   Checks if the given user matches the current justification user, freezing the match until a determination can be made.
%
%   @arg U The user identifier being checked.
%
%   /* previously: ... */
matches_why_U(U):-  nop((current_why_U(Y), freeze(U,\+ \+ (U=Y;true)))).

%! matches_why_UU(+UU) is semidet.
%
%   Checks if the given user tuple matches the current justification user tuple, applying freezing to delay evaluation.
%
%   @arg UU The user tuple identifier being checked.
%
%   /* previously: ... */
matches_why_UU(UU):- nop(only_is_user_reason(UU)). % matches_why_U(U1),matches_why_U(U2),freeze(UU,UU=(U1,U2)).

%! matterialize_support_term(+S, -Sup) is det.
%
%   Materializes a support term by ensuring any variables are instantiated based on the support term's context.
%
%   @arg S The original support term possibly containing variables.
%   @arg Sup The fully instantiated support term.
%
%   /* previously: ... */
matterialize_support_term(S,Sup):- term_attvars(S,Atts), Atts\==[] -> copy_term(S,_,Goals),Sup= S+Goals,!.
matterialize_support_term(SS,SS).

%! set_prolog_flag(+pfc_term_expansion, +false) is det.
%
%   Sets the Prolog flag for PFC term expansion to false.
%
%   /* previously: ... */
:- set_prolog_flag(pfc_term_expansion,false).

%! pfc_system_term_expansion(+I, +S0, -O, -S1) is nondet.
%
%   Manages the system term expansion within PFC, enabling or disabling it based on the context.
%
%   @arg I The input term.
%   @arg S0 The original state.
%   @arg O The output term after expansion.
%   @arg S1 The state after expansion.
%
%   /* previously: ... */
pfc_system_term_expansion(I,S0,O,S1):- %use_pfc_term_expansion, % trace,
 ( \+ current_prolog_flag(pfc_term_expansion,false),
  ( \+ \+ (source_location(File,_), atom_concat(_,'.pfc.pl',File))
    ; current_prolog_flag(pfc_term_expansion,true))) ->
       once((prolog_load_context('term',T),nop(writeln(T)),T=@=I))
         ->(pfc_term_expansion(I,O)-> I\=@=O->S0=S1, fbugio(I-->O)).

%! system:term_expansion(+I, +S0, -O, -S1) is det.
%
%   Declares a multifile and dynamic predicate for term expansion within the system, specifically for PFC.
%
%   @arg I The input term.
%   @arg S0 The original state.
%   @arg O The output term after expansion.
%   @arg S1 The state after expansion.
%
%   /* previously: ... */
:- multifile(system:term_expansion/4).
:- asserta((system:term_expansion(I,S0,O,S1):-
           pfc_system_term_expansion(I,S0,O,S1))).
%:- listing(term_expansion/4).

%:- endif.

%! end_of_file is semidet.
%
%   Marks the end of the file for clarity and potential parsing requirements.
%
%   /* previously: ... */
end_of_file.

%! is_fc_body(+P) is semidet.
%
%   Determines if the body of a predicate is forward chaining.
%
%   @arg P The predicate being analyzed.
%
%   /* previously: ... */
is_fc_body(P):- has_body_atom(fwc,P).

%! is_bc_body(+P) is semidet.
%
%   Determines if the body of a predicate is backchaining.
%
%   @arg P The predicate being analyzed.
%
%   /* previously: ... */
is_bc_body(P):- has_body_atom(bwc,P).

%! is_action_body(+P) is semidet.
%
%   Determines if the body of a predicate is action-oriented.
%
%   @arg P The predicate being analyzed.
%
%   /* previously: ... */
is_action_body(P):- has_body_atom(wac,P).

%! has_body_atom(+WAC, ?P) is semidet.
%
%   Checks if a given predicate contains a specific atom within its body.
%
%   @arg WAC The atom being checked.
%   @arg P The predicate being analyzed.
%
%   /* previously: ... */
has_body_atom(WAC,P):- call(
   WAC==P -> true ; (is_ftCompound(P),get_assertion_head_arg(1,P,E),has_body_atom(WAC,E))),!.

%! same_functors(+Head1, +Head2) is semidet.
%
%   Checks if two heads of clauses have the same functor and arity.
%
%   @arg Head1 The first head being compared.
%   @arg Head2 The second head being compared.
%
%   /* previously: ... */
same_functors(Head1,Head2):-must_det(get_unnegated_functor(Head1,F1,A1)),must_det(get_unnegated_functor(Head2,F2,A2)),!,F1=F2,A1=A2.

%! mpred_update_literal(+P, ?N, ?Q, ?R) is semidet.
%
%   Updates a literal based on its position and the current state of the system.
%
%   @arg P The original predicate.
%   @arg N The position of the literal to update.
%   @arg Q The updated literal.
%   @arg R The result after updating the literal.
%
%   /* previously: ... */
mpred_update_literal(P,N,Q,R):-
    get_assertion_head_arg(N,P,UPDATE),call(replace_arg(P,N,Q_SLOT,Q)),
    must_ex(call_u(Q)),update_value(Q_SLOT,UPDATE,NEW),
    replace_arg(Q,N,NEW,R).

%! update_single_valued_arg(+Module, +P, ?N) is semidet.
%
%   Updates a single valued argument within a module's context.
%
%   @arg Module The module where the update is taking place.
%   @arg P The predicate containing the argument to update.
%   @arg N The position of the argument within the predicate.
%
%   /* previously: ... */
:- module_transparent( (update_single_valued_arg)/3).

update_single_valued_arg(M,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).
update_single_valued_arg(_,M:Pred,N):-!,update_single_valued_arg(M,Pred,N).

update_single_valued_arg(world,P,N):- !, current_prolog_flag(pfc_shared_module,BaseKB), update_single_valued_arg(BaseKB,P,N).
update_single_valued_arg(M,P,N):- ibreak, \+ clause_b(mtHybrid(M)), trace, clause_b(mtHybrid(M2)),!,
   update_single_valued_arg(M2,P,N).

update_single_valued_arg(M,P,N):-
  get_assertion_head_arg(N,P,UPDATE),
  is_relative(UPDATE),!,
  dtrace,
  ibreak,
  replace_arg(P,N,OLD,Q),
  must_det_l((clause_u(Q),update_value(OLD,UPDATE,NEW),\+ is_relative(NEW), replace_arg(Q,N,NEW,R))),!,
  update_single_valued_arg(M,R,N).

%! map_literals(+P, ?G) is semidet.
%
%   Applies a mapping function to literals within a predicate.
%
%   @arg P The predicate containing the literals.
%   @arg G The mapping function being applied.
%
%   /* previously: ... */
map_literals(P,G):-map_literals(P,G,[]).

%! map_literals(+VALUE1, :TermH, ?VALUE3) is semidet.
%
%   Maps literals within a structure using a specified function.
%
%   @arg VALUE1 The predicate or structure containing the literals.
%   @arg TermH The specific term or literal being mapped.
%   @arg VALUE3 Additional parameters for the mapping function.
%
%   /* previously: ... */
map_literals(_,H,_):-is_ftVar(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- mpred_literal(H),must_ex(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.

%! map_unless(:PRED1Test, ?Pred, ?H, ?S) is semidet.
%
%   Maps a function to elements unless a specified test fails.
%
%   @arg Test The test function to determine if mapping should proceed.
%   @arg Pred The mapping function to apply.
%   @arg H The elements to potentially map.
%   @arg S Additional parameters for the mapping function.
%
%   /* previously: ... */
map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.

%! map_first_arg(+Pred, ?List) is semidet.
%
%   Applies a mapping function to the first argument of each element in a list.
%
%   @arg Pred The mapping function to apply.
%   @arg List The list containing the elements to be mapped.
%
%   /* previously: ... */
:- meta_predicate(map_first_arg(*,+)).
map_first_arg(CMPred,List):- strip_module(CMPred,CM,Pred), map_first_arg(CM,Pred,List,[]).

%! map_first_arg(+Pred, :TermH, ?S) is semidet.
%
%   Specifically applies a mapping function to the first argument of a compound term.
%
%   @arg Pred The mapping function.
%   @arg TermH The term being mapped.
%   @arg S Additional parameters for the mapping function.
%
%   /* previously: ... */
:- meta_predicate(map_first_arg(+,*,+,+)).
map_first_arg(CM,Pred,H,S):-is_ftVar(H),!,CM:apply(Pred,[H|S]).
map_first_arg(_,_,[],_) :- !.
map_first_arg(CM,Pred,(H,T),S):-!, map_first_arg(CM,Pred,H,S), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,(H;T),S):-!, map_first_arg(CM,Pred,H,S) ; map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,[H|T],S):-!, CM:apply(Pred,[H|S]), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,H,S):- CM:apply(Pred,[H|S]).

%! pfcVerifyMissing(+GC, ?GO, ?GO) is semidet.
%
%   Verifies and handles missing elements in forward chaining rules, ensuring consistency.
%
%   @arg GC The original rule being checked.
%   @arg GO The outcome after handling missing elements.
%   @arg GO The test condition applied to identify missing elements.
%
%   /* previously: ... */
pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%! mpred_freeLastArg(+G, ?GG) is semidet.
%
%   Frees the last argument of a given predicate, potentially modifying it based on conditions.
%
%   @arg G The original predicate.
%   @arg GG The modified predicate after freeing the last argument.
%
%   /* previously: ... */
mpred_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
mpred_freeLastArg(_G,false).

%! mpred_current_op_support(+VALUE1) is semidet.
%
%   Identifies current operational support within the system for a given operation.
%
%   @arg VALUE1 The operation being checked.
%
%   /* previously: ... */
mpred_current_op_support((p,p)):-!.

%! pfcVersion(+VALUE1) is semidet.
%
%   Provides the current version of the PFC system in use.
%
%   @arg VALUE1 The value representing the version.
%
%   /* previously: ... */
%pfcVersion(6.6).

%! correctify_support(+S, ?S) is semidet.
%
%   Corrects a support entry, ensuring it meets system specifications.
%
%   @arg S The original support entry.
%   @arg S The corrected support entry.
%
%   /* previously: ... */
correctify_support(U,(U,ax)):-var(U),!.
correctify_support((U,U),(U,ax)):-!.
correctify_support((S,T),(S,T)):-!.
correctify_support((U,_UU),(U,ax)):-!.
correctify_support([U],S):-correctify_support(U,S).
correctify_support(U,(U,ax)).

%! clause_asserted_local(:TermABOX) is semidet.
%
%   Checks if a clause is locally asserted within a specific context.
%
%   @arg ABOX The clause being checked.
%
%   /* previously: ... */
clause_asserted_local(MCL):-
  must_ex(strip_mz(MCL,MZ,CL)),
  must_ex(CL='$spft'(MZ,P,Fact,Trigger )),!,
  clause_u('$spft'(MZ,P,Fact,Trigger),true,Ref),
  clause_u('$spft'(MZ,UP,UFact,UTrigger),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).

%! is_already_supported(+P, ?S, ?UU) is semidet.
%
%   Checks if a predicate is already supported by an existing clause.
%
%   @arg P The predicate being checked.
%   @arg S The specific support tuple being verified.
%   @arg UU The user tuple associated with the support.
%
%   /* previously: ... */
is_already_supported(P,(S,T),(S,T)):- clause_asserted_local('$spft'(_MZ,P,S,T)),!.
is_already_supported(P,_S,UU):- clause_asserted_local('$spft'(_MZ,P,US,UT)),must_ex(get_source_uu(UU)),UU=(US,UT).

%! if_missing1(+Q) is semidet.
%
%   Handles cases where a query Q is missing by applying specific conditions and checks.
%
%   @arg Q The query being evaluated for missing conditions.
%
%   /* previously: ... */
if_missing1(Q):- mpred_literal_nv(Q), call_u( \+ ~ Q), if_missing_mask(Q,R,Test),!, lookup_u(R), Test.

%! mpred_run_pause is det.
%
%   Pauses the running of the mpred engine to allow for controlled evaluation.
%
%   /* previously: ... */
mpred_run_pause:- asserta(t_l:mpred_run_paused).

%! mpred_run_resume is det.
%
%   Resumes the running of the mpred engine after a pause.
%
%   /* previously: ... */
mpred_run_resume:- retractall(t_l:mpred_run_paused).

%! fwithout_running(+G) is det.
%
%   Executes a goal G without the interference of the running mpred engine.
%
%   @arg G The goal to be executed.
%
%   /* previously: ... */
fwithout_running(G):- (t_l:mpred_run_paused->G;locally_tl(mpred_run_pause,G)).
