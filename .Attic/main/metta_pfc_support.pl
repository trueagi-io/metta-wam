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
% :- if( \+ current_predicate(set_fileAssertMt/1)).
% % 
% %
% %  predicates for manipulating support relationships
% %

% %  pfcAddSupport(+Fact,+Support)

pfcAddSupport(P,(Fact,Trigger)) :- assert('$spft$'(P,Fact,Trigger)).

pfcGetSupport(P,(Fact,Trigger)) :- pfc_spft(P,Fact,Trigger).

pfc_spft(P,F,T) :- pfcCallSystem('$spft$'(P,F,T)).

% There are three of these to try to efficiently handle the cases
% where some of the arguments are not bound but at least one is.

pfcRemOneSupport(P,(Fact,Trigger)) :-
  must_ex(callable(P);callable(Fact);callable(Trigger)),
  pfcRetractOrWarn('$spft$'(P,Fact,Trigger)).

pfcRemOneSupportOrQuietlyFail(P,(Fact,Trigger)) :-
  must_ex(callable(P);callable(Fact);callable(Trigger)),
  pfcRetractOrQuietlyFail('$spft$'(P,Fact,Trigger)).


pfc_collect_supports(Tripples) :-
  bagof(Tripple, pfc_support_relation(Tripple), Tripples),
  !.
pfc_collect_supports([]).

pfc_support_relation((P,F,T)) :-
  pfc_spft(P,F,T).



pfc_make_supports((P,S1,S2)) :-
  pfcAddSupport(P,(S1,S2)),
  (pfcAddType1(P); true),
  !.

% %  pfcTriggerKey(+Trigger,-Key)
% %
% %  Arg1 is a trigger.  Key is the best term to index it on.

pfcTriggerKey('$pt$'(Key,_),Key).
pfcTriggerKey('$pt$'(Key,_,_),Key).
pfcTriggerKey('$nt$'(Key,_,_),Key).
pfcTriggerKey(Key,Key).


% % ^L
% %  Get a key from the trigger that will be used as the first argument of
% %  the trigger base clause that stores the trigger.
% %

pfc_trigger_key(X,X) :- var(X), !.
pfc_trigger_key(chart(word(W),_L),W) :- !.
pfc_trigger_key(chart(stem([Char1|_Rest]),_L),Char1) :- !.
pfc_trigger_key(chart(Concept,_L),Concept) :- !.
pfc_trigger_key(X,X).


nb_pushval(Name,Value):-nb_current(Name,Before)->nb_setval(Name,[Value|Before]);nb_setval(Name,[Value]).
nb_peekval(Name,Value):-nb_current(Name,[Value|_Before]).
nb_hasval(Name,Value):-nb_current(Name,List),member(Value,List).
nb_popval(Name,Value):-nb_current(Name,[Value|Before])->nb_setval(Name,Before).

reset_shown_justs:- retractall(t_l:shown_why(_)),nop(color_line(red,1)).
clear_proofs:- retractall(t_l:whybuffer(_P,_Js)),nop(color_line(cyan,1)).


lookup_spft_match(A,B,C):- copy_term(A,AA),lookup_spft(A,B,C),A=@=AA.

lookup_spft_match_deeper(H,Fact,Trigger):-
  copy_term(H,HH),
  lookup_spft((H:- _B),Fact,Trigger),
  H=@=HH.

lookup_spft_match_first(A,B,C):- nonvar(A),!,
  no_repeats(((lookup_spft_match(A,B,C);lookup_spft(A,B,C)))).

lookup_spft_match_first(A,B,C):- lookup_spft(A,B,C).


% %  pfc_is_info( :TermC) is semidet.
%
% PFC If Is A Info.
%
pfc_is_info((CWC,Info)):- (atom(CWC),is_a_info(CWC));pfc_is_info(Info).
pfc_is_info(pfc_bc_only(C)):-is_ftNonvar(C),!.
pfc_is_info(infoF(C)):-is_ftNonvar(C),!.
pfc_is_info(inherit_above(_,_)).


is_a_info(fail).
is_a_info(CWC):- is_pfc_chained(CWC).

is_pfc_chained(cwc).
is_pfc_chained(awc).
is_pfc_chained(zwc).
is_pfc_chained(fwc).
is_pfc_chained(bwc).
is_pfc_chained(wac).

:- forall(is_pfc_chained(Op),assert_if_new(Op)).

reserved_body(B):-var(B),!,fail.
reserved_body(attr_bind(_)).
reserved_body(attr_bind(_,_)).
reserved_body(B):-reserved_body_helper(B).

reserved_body_helper(B):- \+ compound(B),!,fail.
reserved_body_helper((ZAWC,_)):- atom(ZAWC),is_pfc_chained(ZAWC).

call_only_based_mfl(H,mfl4(_VarNameZ,M,F,L)):-
  ignore(predicate_property(H,imported_from(M));predicate_property(H,module(M))),
  ignore(predicate_property(H,line_count(L))),
  ignore(source_file(M:H,F);predicate_property(H,file(F));(predicate_property(H,foreign),F=foreign)).

uses_call_only(H):- predicate_property(H,foreign),!.
uses_call_only(H):- predicate_property(H,_), \+ predicate_property(H,interpreted),!.

clause_match(H,_B,uses_call_only(H)):- uses_call_only(H),!.
clause_match(H,B,Ref):- clause_asserted(H,B,Ref),!.
clause_match(H,B,Ref):- ((copy_term(H,HH),clause(H,B,Ref),H=@=HH)*->true;clause(H,B,Ref)), \+ reserved_body_helper(B).

find_mfl(C,MFL):- lookup_spft_match(C,MFL,ax).
find_mfl(C,MFL):- unwrap_litr0(C,UC) -> C\==UC -> find_mfl(UC,MFL).
find_mfl(C,MFL):- expand_to_hb(C,H,B),
   find_hb_mfl(H,B,_Ref,MFL)->true; (clause_match(H,B,Ref),find_hb_mfl(H,B,Ref,MFL)).

find_hb_mfl(_H,_B,Ref,mfl4(_VarNameZ,M,F,L)):- atomic(Ref),clause_property(Ref,line_count(L)),
 clause_property(Ref,file(F)),clause_property(Ref,module(M)).
find_hb_mfl(H,B,_,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first( (H:-B),mfl4(VarNameZ,M,F,L),_),!.
find_hb_mfl(H,B,_Ref,mfl4(VarNameZ,M,F,L)):- lookup_spft_match_first(H,mfl4(VarNameZ,M,F,L),_),ground(B).
find_hb_mfl(H,_B,uses_call_only(H),MFL):- !,call_only_based_mfl(H,MFL).

:- fixup_exports.
%:- current_prolog_flag(pfc_shared_module,BaseKB),fixup_module_exports_into(BaseKB).
:- fixup_module_exports_into(system).

mpred_rule_hb(C,_):- \+ compound(C),!,fail.
mpred_rule_hb((H:-B),H,B):- !.
mpred_rule_hb((H<-B),H,B):- !.
mpred_rule_hb((B==>H),H,B):- !.
mpred_rule_hb((==>H),H,true):- !.
mpred_rule_hb((HB1<==>HB2),(H1,H2),(B1,B2)):- !, (mpred_rule_hb((HB1==>HB2),H2,B2);mpred_rule_hb((HB2==>HB1),H1,B1)).

:- module_transparent( (get_assertion_head_arg)/3).
get_assertion_head_arg(N,P,E):-get_assertion_head_unnegated(P,PP),!,arg(N,PP,E).

get_assertion_head_unnegated(P,PP):- mpred_rule_hb(P,H,_), (pfc_unnegate(H,PP)->true;H==PP).
replace_arg(Q,N,NEW,R):- duplicate_term(Q,R),Q=R,nb_setarg(N,R,NEW).

%% if_missing_mask( +Q, ?R, ?Test) is semidet.
%
% If Missing Mask.
%

if_missing_mask(M:Q,M:R,M:Test):- nonvar(Q),!,if_missing_mask(Q,R,Test).
if_missing_mask(Q,~Q,\+Q):- \+ is_ftCompound(Q),!.

%if_missing_mask(ISA, ~ ISA, \+ ISA):- functor(ISA,F,1),(F==tSwim;call_u(functorDeclares(F))),!.
if_missing_mask(HB,RO,TestO):- once(mpred_rule_hb(HB,H,B)),B\==true,HB\==H,!,
     if_missing_mask(H,R,TestO),subst(HB,H,R,RO).

if_missing_mask(ISA, ISA, \+ ISA):- functor(ISA, _F,1),!.% (F==tSwim;call_u(functorDeclares(F))),!.

if_missing_mask(Q,R,Test):-
   which_missing_argnum(Q,N),
   if_missing_n_mask(Q,N,R,Test),!.

if_missing_mask(ISA, ~ ISA, \+ ISA).

%% if_missing_n_mask( +Q, ?N, ?R, ?Test) is semidet.
%
% If Missing Mask.
%
if_missing_n_mask(Q,N,R,Test):-
  get_assertion_head_arg(N,Q,Was),
  (nonvar(R)-> (which_missing_argnum(R,RN),get_assertion_head_arg(RN,R,NEW));replace_arg(Q,N,NEW,R)),!,
   Test=dif:dif(Was,NEW).

/*
Old version
if_missing_mask(Q,N,R,dif:dif(Was,NEW)):-
 must_ex((is_ftNonvar(Q),acyclic_term(Q),acyclic_term(R),functor(Q,F,A),functor(R,F,A))),
  (singleValuedInArg(F,N) ->
    (get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R));
    ((get_assertion_head_arg(N,Q,Was),is_ftNonvar(Was)) -> replace_arg(Q,N,NEW,R);
        (N=A,get_assertion_head_arg(N,Q,Was),replace_arg(Q,N,NEW,R)))).
*/


%% which_missing_argnum( +VALUE1, ?VALUE2) is semidet.
%
% Which Missing Argnum.
%
which_missing_argnum(Q,N):- compound(Q),\+ compound_name_arity(Q,_,0),
 must_ex((acyclic_term(Q),is_ftCompound(Q),get_functor(Q,F,A))),
 F\=t,
  (call_u(singleValuedInArg(F,N)) -> true; which_missing_argnum(Q,F,A,N)).

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


:- use_module(library(lists)).

justification(F,J) :- supports(F,J).

justifications(F,Js) :- bagof(J,justification(F,J),Js).



% %  base(P,L) - is true iff L is a list of "base" facts which, taken
% %  together, allows us to deduce P.  A base fact is an axiom (a fact
% %  added by the user or a raw Prolog fact (i.e. one w/o any support))
% %  or an assumption.

base(F,[F]) :- (axiom(F) ; assumption(F)),!.

base(F,L) :-
  % i.e. (reduce 'append (map 'base (justification f)))
  justification(F,Js),
  bases(Js,L).


% %  bases(L1,L2) is true if list L2 represents the union of all of the
% %  facts on which some conclusion in list L1 is based.

bases([],[]).
bases([X|Rest],L) :-
  base(X,Bx),
  bases(Rest,Br),
  pfcUnion(Bx,Br,L).

axiom(F) :-
  matches_why_UU(UU),
  pfcGetSupport(F,UU);
  pfcGetSupport(F,(god,god)).

% %  an assumption is a failed goal, i.e. were assuming that our failure to
% %  prove P is a proof of not(P)

assumption(P) :- pfc_unnegate(P,_).

% %  assumptions(X,As) if As is a set of assumptions which underly X.

assumptions(X,[X]) :- assumption(X).
assumptions(X,[]) :- axiom(X).
assumptions(X,L) :-
  justification(X,Js),
  assumptions1(Js,L).

assumptions1([],[]).
assumptions1([X|Rest],L) :-
  assumptions(X,Bx),
  assumptions1(Rest,Br),
  pfcUnion(Bx,Br,L).


% %  pfcProofTree(P,T) the proof tree for P is T where a proof tree is
% %  of the form
% %
% %      [P , J1, J2, ;;; Jn]         each Ji is an independent P justifier.
% %           ^                         and has the form of
% %           [J11, J12,... J1n]      a list of proof trees.


% pfcChild(P,Q) is true iff P is an immediate justifier for Q.
% mode: pfcChild(+,?)

pfcChild(P,Q) :-
  pfcGetSupport(Q,(P,_)).

pfcChild(P,Q) :-
  pfcGetSupport(Q,(_,Trig)),
  pfcType(Trig,trigger(_Pos)),
  pfcChild(P,Trig).

pfcChildren(P,L) :- bagof_or_nil(C,pfcChild(P,C),L).

% pfcDescendant(P,Q) is true iff P is a justifier for Q.

pfcDescendant(P,Q) :-
   pfcDescendant1(P,Q,[]).

pfcDescendant1(P,Q,Seen) :-
  pfcChild(X,Q),
  (\+ member(X,Seen)),
  (P=X ; pfcDescendant1(P,X,[X|Seen])).

pfcDescendants(P,L) :-
  bagof_or_nil(Q,pfcDescendant1(P,Q,[]),L).



/*
current_why_U(U):- must_ex(current_why(Why)), U = user(Why).
current_why_UU(UU):- current_why_U(U), UU= (U,U).
matches_why_U(U):-  freeze(U,U=user(_)).
matches_why_UU(UU):- matches_why_U(U1),matches_why_U(U2), freeze(UU,UU=(U1,U2)).
*/
current_why_U(U):-  get_why_uu((U,_)).% must_ex(current_why(Why)), U = user(Why).
current_why_UU(UU):- get_why_uu(UU). % current_why_U(U), UU= (U,U).
matches_why_U(U):-  nop((current_why_U(Y), freeze(U,\+ \+ (U=Y;true)))).
matches_why_UU(UU):- nop(only_is_user_reason(UU)). % matches_why_U(U1),matches_why_U(U2),freeze(UU,UU=(U1,U2)).


matterialize_support_term(S,Sup):- term_attvars(S,Atts), Atts\==[] -> copy_term(S,_,Goals),Sup= S+Goals,!.
matterialize_support_term(SS,SS).





:- set_prolog_flag(pfc_term_expansion,false).

pfc_system_term_expansion(I,S0,O,S1):- %use_pfc_term_expansion, % trace,
 ( \+ current_prolog_flag(pfc_term_expansion,false),
  ( \+ \+ (source_location(File,_), atom_concat(_,'.pfc.pl',File))
    ; current_prolog_flag(pfc_term_expansion,true))) ->
       once((prolog_load_context('term',T),nop(writeln(T)),T=@=I))
         ->(pfc_term_expansion(I,O)-> I\=@=O->S0=S1, fbugio(I-->O)).


:- multifile(system:term_expansion/4).
:- asserta((system:term_expansion(I,S0,O,S1):-
           pfc_system_term_expansion(I,S0,O,S1))).
%:- listing(term_expansion/4).

% :- endif.



end_of_file.





















%% is_fc_body( +P) is semidet.
%
% If Is A Forward Chaining Body.
%
is_fc_body(P):- has_body_atom(fwc,P).

%% is_bc_body( +P) is semidet.
%
% If Is A Backchaining Body.
%
is_bc_body(P):- has_body_atom(bwc,P).

%% is_action_body( +P) is semidet.
%
% If Is A Action Body.
%
is_action_body(P):- has_body_atom(wac,P).



%% has_body_atom( +WAC, ?P) is semidet.
%
% Has Body Atom.
%
has_body_atom(WAC,P):- call(
   WAC==P -> true ; (is_ftCompound(P),get_assertion_head_arg(1,P,E),has_body_atom(WAC,E))),!.

/*
has_body_atom(WAC,P,Rest):- call(WAC==P -> Rest = true ; (is_ftCompound(P),functor(P,F,A),is_atom_body_pfa(WAC,P,F,A,Rest))).
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(1,P,E),E==WAC,get_assertion_head_arg(2,P,Rest),!.
is_atom_body_pfa(WAC,P,F,2,Rest):-get_assertion_head_arg(2,P,E),E==WAC,get_assertion_head_arg(1,P,Rest),!.
*/


same_functors(Head1,Head2):-must_det(get_unnegated_functor(Head1,F1,A1)),must_det(get_unnegated_functor(Head2,F2,A2)),!,F1=F2,A1=A2.

%% mpred_update_literal( +P, ?N, ?Q, ?R) is semidet.
%
% PFC Update Literal.
%
mpred_update_literal(P,N,Q,R):-
    get_assertion_head_arg(N,P,UPDATE),call(replace_arg(P,N,Q_SLOT,Q)),
    must_ex(call_u(Q)),update_value(Q_SLOT,UPDATE,NEW),
    replace_arg(Q,N,NEW,R).


% '$spft'(MZ,5,5,5).

%% update_single_valued_arg(+Module, +P, ?N) is semidet.
%
% Update Single Valued Argument.
%
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


update_single_valued_arg(M,P,N):-
 call_u((must_det_l((

  call_u(mtHybrid(M)),
  mpred_type_args \= M,
  mpred_kb_ops \= M,
  get_assertion_head_arg(N,P,UPDATE),
  replace_arg(P,N,Q_SLOT,Q),
  var(Q_SLOT),
  same_functors(P,Q),
  % current_why(U),
  must_det_l((
     % rtrace(attvar_op(assert_if_new,M:'$spft'(MZ,P,U,ax))),
     % (call_u(P)->true;(assertz_mu(P))),
     assertz(M:P),
     doall((
          lookup_u(M:Q,E),
          UPDATE \== Q_SLOT,
          erase(E),
          mpred_unfwc1(M:Q))))))))).

% =======================
% utils
% =======================

%% map_literals( +P, ?G) is semidet.
%
% Map Literals.
%
map_literals(P,G):-map_literals(P,G,[]).


%% map_literals( +VALUE1, :TermH, ?VALUE3) is semidet.
%
% Map Literals.
%
map_literals(_,H,_):-is_ftVar(H),!. % skip over it
map_literals(_,[],_) :- !.
map_literals(Pred,(H,T),S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,[H|T],S):-!, apply(Pred,[H|S]), map_literals(Pred,T,S).
map_literals(Pred,H,S):- mpred_literal(H),must_ex(apply(Pred,[H|S])),!.
map_literals(_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_literals(Pred,H,S):-H=..List,!,map_literals(Pred,List,S),!.



%% map_unless( :PRED1Test, ?Pred, ?H, ?S) is semidet.
%
% Map Unless.
%
map_unless(Test,Pred,H,S):- call(Test,H),ignore(apply(Pred,[H|S])),!.
map_unless(_Test,_,[],_) :- !.
map_unless(_Test,_Pred,H,_S):- \+ is_ftCompound(H),!. % skip over it
map_unless(Test,Pred,(H,T),S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,[H|T],S):-!, apply(Pred,[H|S]), map_unless(Test,Pred,T,S).
map_unless(Test,Pred,H,S):-H=..List,!,map_unless(Test,Pred,List,S),!.


:- meta_predicate(map_first_arg(*,+)).
%% map_first_arg( +Pred, ?List) is semidet.
%
% PFC Maptree.
%
map_first_arg(CMPred,List):- strip_module(CMPred,CM,Pred), map_first_arg(CM,Pred,List,[]).

:- meta_predicate(map_first_arg(+,*,+,+)).
%% map_first_arg( +Pred, :TermH, ?S) is semidet.
%
% PFC Maptree.
%
map_first_arg(CM,Pred,H,S):-is_ftVar(H),!,CM:apply(Pred,[H|S]).
map_first_arg(_,_,[],_) :- !.
map_first_arg(CM,Pred,(H,T),S):-!, map_first_arg(CM,Pred,H,S), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,(H;T),S):-!, map_first_arg(CM,Pred,H,S) ; map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,[H|T],S):-!, CM:apply(Pred,[H|S]), map_first_arg(CM,Pred,T,S).
map_first_arg(CM,Pred,H,S):- CM:apply(Pred,[H|S]).

%:- fixup_exports.

% % :- ensure_loaded(logicmoo(util/rec_lambda)).

%example pfcVerifyMissing(mpred_isa(I,D), mpred_isa(I,C), ((mpred_isa(I,C), {D==C});-mpred_isa(I,C))).
%example pfcVerifyMissing(mudColor(I,D), mudColor(I,C), ((mudColor(I,C), {D==C});-mudColor(I,C))).


%% pfcVerifyMissing( +GC, ?GO, ?GO) is semidet.
%
% Prolog Forward Chaining Verify Missing.
%
pfcVerifyMissing(GC, GO, ((GO, {D==C});\+ GO) ):-  GC=..[F,A|Args],append(Left,[D],Args),append(Left,[C],NewArgs),GO=..[F,A|NewArgs],!.

%example mpred_freeLastArg(mpred_isa(I,C),~(mpred_isa(I,C))):-is_ftNonvar(C),!.
%example mpred_freeLastArg(mpred_isa(I,C),(mpred_isa(I,F),C\=F)):-!.

%% mpred_freeLastArg( +G, ?GG) is semidet.
%
% PFC Free Last Argument.
%
mpred_freeLastArg(G,GG):- G=..[F,A|Args],append(Left,[_],Args),append(Left,[_],NewArgs),GG=..[F,A|NewArgs],!.
mpred_freeLastArg(_G,false).


%% mpred_current_op_support( +VALUE1) is semidet.
%
% PFC Current Oper. Support.
%
mpred_current_op_support((p,p)):-!.


%% pfcVersion( +VALUE1) is semidet.
%
% Prolog Forward Chaining Version.
%
%pfcVersion(6.6).


% % :- '$set_source_module'(mpred_kb_ops).

%% correctify_support( +S, ?S) is semidet.
%
% Correctify Support.
%
correctify_support(U,(U,ax)):-var(U),!.
correctify_support((U,U),(U,ax)):-!.
correctify_support((S,T),(S,T)):-!.
correctify_support((U,_UU),(U,ax)):-!.
correctify_support([U],S):-correctify_support(U,S).
correctify_support(U,(U,ax)).


%% clause_asserted_local( :TermABOX) is semidet.
%
% Clause Asserted Local.
%
clause_asserted_local(MCL):-
  must_ex(strip_mz(MCL,MZ,CL)),
  must_ex(CL='$spft'(MZ,P,Fact,Trigger )),!,
  clause_u('$spft'(MZ,P,Fact,Trigger),true,Ref),
  clause_u('$spft'(MZ,UP,UFact,UTrigger),true,Ref),
  (((UP=@=P,UFact=@=Fact,UTrigger=@=Trigger))).



%% is_already_supported( +P, ?S, ?UU) is semidet.
%
% If Is A Already Supported.
%
is_already_supported(P,(S,T),(S,T)):- clause_asserted_local('$spft'(_MZ,P,S,T)),!.
is_already_supported(P,_S,UU):- clause_asserted_local('$spft'(_MZ,P,US,UT)),must_ex(get_source_uu(UU)),UU=(US,UT).

% TOO UNSAFE
% is_already_supported(P,_S):- copy_term_and_varnames(P,PC),sp ftY(PC,_,_),P=@=PC,!.


if_missing1(Q):- mpred_literal_nv(Q), call_u( \+ ~ Q), if_missing_mask(Q,R,Test),!, lookup_u(R), Test.


mpred_run_pause:- asserta(t_l:mpred_run_paused).
mpred_run_resume:- retractall(t_l:mpred_run_paused).

fwithout_running(G):- (t_l:mpred_run_paused->G;locally_tl(mpred_run_pause,G)).


