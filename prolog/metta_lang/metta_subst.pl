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

%self_subst(X):- var(X),!.
%self_subst(X):- string(X),!.
%self_subst(X):- number(X),!.
%self_subst([]).
self_subst(X):- \+ callable(X),!.
self_subst(X):- !, self_eval(X),!.
self_subst(X):- is_valid_nb_state(X),!.
self_subst(X):- is_list(X),!,fail.
%self_subst(X):- compound(X),!.
%self_subst(X):- is_ref(X),!,fail.
self_subst(X):- atom(X),!, \+ nb_bound(X,_),!.
self_subst('True'). self_subst('False'). self_subst('F'). %'


%:- nb_setval(self_space, '&self'). % '
substs_to(XX,Y):- Y==XX,!.
substs_to(XX,Y):- Y=='True',!, is_True(XX),!. %'

%current_self(Space):- nb_bound(self_space,Space).
/*
subst_args(Eq,RetType,A,AA):-
  current_self(Space),
  subst_args(Eq,RetType,11,Space,A,AA).

%subst_args(Eq,RetType,Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(subst_args(Eq,RetType,X)),fail.
*/

subst_args(Eq,RetType,Depth,Self,X,Y):- atom(Eq),  ( Eq \== ('=')),  ( Eq \== ('match')) ,!,
   call(Eq,'=',RetType,Depth,Self,X,Y). % '

 :- style_check(-singleton).



%subst_args(Eq,RetType,_Dpth,_Slf,X,Y):- nonvar(Y),X=Y,!.
%subst_args(Eq,RetType,Depth,Self,X,Y):- nonvar(Y),!,subst_args(Eq,RetType,Depth,Self,X,XX),substs_to(XX,Y).
subst_args(Eq,RetType,_Dpth,_Slf,X,Y):- var(X),!,Y=X.
% subst_args(Eq,RetType,Depth,_Slf,X,Y):- Depth<1,bt,trace,break.
subst_args(Eq,RetType,_Dpth,_Slf,X,Y):- self_subst(X),!,Y=X.
subst_args(Eq,RetType,_Dpth,_Slf,[X|T],Y):-
  % !, fail,
  T==[], \+ callable(X),!,Y=[X].

subst_args(Eq,RetType,Depth,Self,[F|X],Y):-
 % (F=='superpose' ; ( option_value(no_repeats,false))),  %'
  notrace((D1 is Depth-1)),!,
  subst_args0(Eq,RetType,D1,Self,[F|X],Y).

subst_args(Eq,RetType,Depth,Self,X,Y):- subst_args0(Eq,RetType,Depth,Self,X,Y).
/*
subst_args(Eq,RetType,Depth,Self,X,Y):-
  mnotrace((no_repeats_var(YY),
  D1 is Depth-1)),
  subst_args0(Eq,RetType,D1,Self,X,Y),
   mnotrace(( \+ (Y\=YY))).
*/

subst_args(X,Y):- subst_args('&self',X,Y). %'
subst_args(Space,X,Y):- subst_args(100,Space,X,Y).

subst_args(Depth,Space,X,Y):-subst_args('=',_RetType,
  Depth,Space,X,Y).

:- nodebug(metta(eval)).


%subst_args0(Eq,RetType,Depth,_Slf,X,Y):- Depth<1,!,X=Y, (\+ trace_on_overflow-> true; reset_eval_num,debug(metta(eval))).
subst_args0(Eq,RetType,_Dpth,_Slf,X,Y):- self_subst(X),!,Y=X.
subst_args0(Eq,RetType,Depth,Self,X,Y):-
  Depth2 is Depth-1,
  trace_eval(subst_args1(Eq,RetType),(false,(e2;e)),Depth,Self,X,M),
  (M\=@=X ->subst_args0(Eq,RetType,Depth2,Self,M,Y);Y=X).

:- discontiguous subst_args1/6.
:- discontiguous subst_args2/6.

subst_args1(Eq,RetType,Depth,Self,X,Y):-
  var(Eq) -> (!,subst_args1('=',RetType,Depth,Self,X,Y));
    (atom(Eq),  ( Eq \== ('='), Eq \== ('match')) ,!, call(Eq,'=',RetType,Depth,Self,X,Y)).

subst_args1(Eq,RetType,_Dpth,_Slf,Name,Value):- atom(Name), nb_bound(Name,Value),!.

subst_args1(Eq,RetType,Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 subst_args(Eq,RetType,Depth,Self,VI,VM),
  ( VM\==VI -> subst_args(Eq,RetType,Depth,Self,[V|VM],VVO) ;
    (subst_args(Eq,RetType,Depth,Self,V,VV), (V\==VV -> subst_args(Eq,RetType,Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

subst_args1(Eq,RetType,_Dpth,_Slf,X,Y):- \+ is_list(X),!,Y=X.

subst_args1(Eq,RetType,Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(subst_args(Eq,RetType,Depth,Self),VI,VO).

subst_args1(Eq,RetType,Depth,Self,['let',A,A5,AA],OO):- !,
  %(var(A)->true;trace),
  ((subst_args(Eq,RetType,Depth,Self,A5,AE), AE=A)),
  subst_args(Eq,RetType,Depth,Self,AA,OO).
%subst_args1(Eq,RetType,Depth,Self,['let',A,A5,AA],AAO):- !,subst_args(Eq,RetType,Depth,Self,A5,A),subst_args(Eq,RetType,Depth,Self,AA,AAO).
subst_args1(Eq,RetType,Depth,Self,['let*',[],Body],RetVal):- !, subst_args(Eq,RetType,Depth,Self,Body,RetVal).
subst_args1(Eq,RetType,Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
    subst_args1(Eq,RetType,Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).

    is_sl_op('>').  is_sl_op('<'). %  is_sl_op('>').
    is_sl_op('\\=@=').

subst_args1(Eq,RetType,Depth,Self,[OP,N1,N2],TF):-
  fail,
  is_sl_op(OP), !,
  ((subst_args(Eq,RetType,Depth,Self,N1,N1Res),subst_args(Eq,RetType,Depth,Self,N2,N2Res),
     ((N1,N2)\=@=(N1Res,N2Res)),subst_args1(Eq,RetType,Depth,Self,[OP,N1Res,N2Res],TF))
     *->true;
      subst_selfless([OP,N1,N2],TF)).

%subst_args1(Eq,RetType,Depth,Self,O,O):-!.

subst_args1(Eq,RetType,_Dpth,_Slf,['repl!'],'True'):- !, repl.
subst_args1(Eq,RetType,Depth,Self,['!',Cond],Res):- !, call(subst_args(Eq,RetType,Depth,Self,Cond,Res)).
subst_args1(Eq,RetType,Depth,Self,['rtrace',Cond],Res):- !, rtrace(subst_args(Eq,RetType,Depth,Self,Cond,Res)).
subst_args1(Eq,RetType,Depth,Self,['time',Cond],Res):- !, time(subst_args(Eq,RetType,Depth,Self,Cond,Res)).
%subst_args1(Eq,RetType,Depth,Self,['print',Cond],Res):- !, subst_args(Eq,RetType,Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
% !(println! $1)
subst_args1(Eq,RetType,Depth,Self,['println!',Cond],[]):- !, subst_args(Eq,RetType,Depth,Self,Cond,Res),format('~N'),write_src(Res),format('~N').

subst_args1(Eq,RetType,_Dpth,_Slf,List,Y):- is_list(List),maplist(self_subst,List),List=[H|_], \+ atom(H), !,Y=List.

subst_args1(Eq,RetType,Depth,Self,['assertTrue', X],TF):- !, subst_args(Eq,RetType,Depth,Self,['assertEqual',X,'True'],TF).
subst_args1(Eq,RetType,Depth,Self,['assertFalse',X],TF):- !, subst_args(Eq,RetType,Depth,Self,['assertEqual',X,'False'],TF).

subst_args1(Eq,RetType,Depth,Self,['assertEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   l1t_loonit_assert_source_tf(
        ['assertEqual',X0,Y0],
        (bagof_subst(Depth,Self,X,XX),
         bagof_subst(Depth,Self,Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->make_nop(RetVal);RetVal=[got,XX,expected,YY]).

subst_args1(Eq,RetType,Depth,Self,['assertNotEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   l1t_loonit_assert_source_tf(
        ['assertNotEqual',X0,Y0],
        (setof_subst(Depth,Self,X,XX), setof_subst(Depth,Self,Y,YY)),
         \+ equal_enough(XX,YY), TF),
  (TF=='True'->make_nop(RetVal);RetVal=[got,XX,expected,not,YY]).

subst_args1(Eq,RetType,Depth,Self,['assertEqualToResult',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   l1t_loonit_assert_source_tf(
        ['assertEqualToResult',X0,Y0],
        (bagof_subst(Depth,Self,X,XX), =(Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->make_nop(RetVal);RetVal=[got,XX,expected,YY]),!.


l1t_loonit_assert_source_tf(Src,Goal,Check,TF):-
   copy_term(Goal,OrigGoal),
   l1t_loonit_asserts(Src, time_eval('\n; EVAL TEST\n;',Goal), Check),
   as_tf(Check,TF),!,
  ignore((
          once((TF='True', is_debugging(pass));(TF='False', is_debugging(fail))),
     with_debug((eval),time_eval('Trace',OrigGoal)))).

l1t_loonit_asserts(Src,Goal,Check):-
                 loonit_asserts(Src,Goal,Check).


/*
sort_result(Res,Res):- \+ compound(Res),!.
sort_result([And|Res1],Res):- is_and(And),!,sort_result(Res1,Res).
sort_result([T,And|Res1],Res):- is_and(And),!,sort_result([T|Res1],Res).
sort_result([H|T],[HH|TT]):- !, sort_result(H,HH),sort_result(T,TT).
sort_result(Res,Res).

unify_enough(L,L):-!.
unify_enough(L,C):- is_list(L),into_list_args(C,CC),!,unify_lists(CC,L).
unify_enough(C,L):- is_list(L),into_list_args(C,CC),!,unify_lists(CC,L).
unify_enough(C,L):- \+ compound(C),!,L=C.
unify_enough(L,C):- \+ compound(C),!,L=C.
unify_enough(L,C):- into_list_args(L,LL),into_list_args(C,CC),!,unify_lists(CC,LL).

unify_lists(C,L):- \+ compound(C),!,L=C.
unify_lists(L,C):- \+ compound(C),!,L=C.
unify_lists([C|CC],[L|LL]):- unify_enough(L,C),!,unify_lists(CC,LL).

equal_enough(R,V):- is_list(R),is_list(V),sort(R,RR),sort(V,VV),!,equal_enouf(RR,VV),!.
equal_enough(R,V):- copy_term(R,RR),copy_term(V,VV),equal_enouf(R,V),!,R=@=RR,V=@=VV.

equal_enough_for_test(X,Y):- must_det_ll((subst_vars(X,XX),subst_vars(Y,YY))),!,equal_enough(XX,YY),!.

equal_enouf(R,V):- R=@=V, !.
equal_enouf(_,V):- V=@='...',!.
equal_enouf(L,C):- is_list(L),into_list_args(C,CC),!,equal_enouf_l(CC,L).
equal_enouf(C,L):- is_list(L),into_list_args(C,CC),!,equal_enouf_l(CC,L).
%equal_enouf(R,V):- (var(R),var(V)),!, R=V.
equal_enouf(R,V):- (var(R);var(V)),!, R==V.
equal_enouf(R,V):- number(R),number(V),!, RV is abs(R-V), RV < 0.03 .
equal_enouf(R,V):- atom(R),!,atom(V), has_unicode(R),has_unicode(V).
equal_enouf(R,V):- (\+ compound(R) ; \+ compound(V)),!, R==V.
equal_enouf(L,C):- into_list_args(L,LL),into_list_args(C,CC),!,equal_enouf_l(CC,LL).

equal_enouf_l(C,L):- \+ compound(C),!,L=@=C.
equal_enouf_l(L,C):- \+ compound(C),!,L=@=C.
equal_enouf_l([C|CC],[L|LL]):- !, equal_enouf(L,C),!,equal_enouf_l(CC,LL).


has_unicode(A):- atom_codes(A,Cs),member(N,Cs),N>127,!.
set_last_error(_).

*/
subst_args1(Eq,RetType,Depth, Self, [OP|ARGS], Template):-
    is_space_op(OP),  !,
    subst_args_as(Depth, Self, [OP|ARGS], Template).

% Definition of uses_op to validate operations
is_space_op('match').
is_space_op('get-atoms').
is_space_op('add-atom').
is_space_op('remove-atom').
%is_space_op('replace-atom').
is_space_op('atom-count').
is_space_op('atom-replace').

subst_args_as(Depth, Self, [OP|ARGS], Template):- !, eval_10('=',_,Depth, Self, [OP|ARGS], Template).

subst_args_as(Depth,Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter_l1t(Eq,Depth,Space,Goal).
subst_args_as(Depth,Self,['match',Other,Goal,Template,Else],Template):-
  (subst_args_as(Depth,Self,['match',Other,Goal,Template],Template)*->true;Template=Else).
subst_args_as(Depth,Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter_l1t(Eq,Depth,Space,PredDecl).
subst_args_as(_Dpth,Self,['add-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF).
subst_args_as(_Dpth,Self,['remove-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF).
subst_args_as(_Dpth,Self,['atom-count',Other],Count):- !, into_space(Self,Other,Space), findall(_,metta_eq_def(Eq,Other,_,_),L_as),length(L_as,C_as),findall(_,get_metta_atom(Eq,Space,_),L2),length(L2,C2),Count is C_as+C2.
subst_args_as(_Dpth,Self,['atom-replace',Other,Rem,Add],TF):- !, into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_l1t_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF).

subst_args1(Eq,RetType,Depth,Self,X,Res):-
   X= [CaseSym|_],CaseSym == 'case',  !, eval_10('=',_,Depth, Self, X,Res).

% Macro: case
subst_args1_hide(Depth,Self,X,Res):-
   X= [CaseSym,A,CL],CaseSym == 'case', !,
   into_case_l1t_list(CL,CASES),
   findall(Key-Value,
     (nth0(Nth,CASES,Case0),
       (is_case_l1t(Key,Case0,Value),
        if_trace((case),(format('~N'),
           writeqln(c(Nth,Key)=Value))))),KVs),!,
   ((subst_args(Eq,RetType,Depth,Self,A,AA),         if_trace((case),writeqln(switch=AA)),
    (select_case_l1t(Depth,Self,AA,KVs,Value)->true;(member(Void -Value,KVs),Void=='%void%')))
     *->true;(member(Void -Value,KVs),Void=='%void%')),
    subst_args(Eq,RetType,Depth,Self,Value,Res).

  select_case_l1t(Depth,Self,AA,Cases,Value):-
     (best_key_l1t(AA,Cases,Value) -> true ;
      (maybe_special_key_l1ts(Depth,Self,Cases,CasES),
       (best_key_l1t(AA,CasES,Value) -> true ;
        (member(Void -Value,CasES),Void=='%void%')))).

  best_key_l1t(AA,Cases,Value):-
    ((member(Match-Value,Cases),unify_enough(AA,Match))->true;
     ((member(Match-Value,Cases),AA ==Match)->true;
      ((member(Match-Value,Cases),AA=@=Match)->true;
        (member(Match-Value,Cases),AA = Match)))).

        %into_case_l1t_list([[C|ASES0]],CASES):-  is_list(C),!, into_case_l1t_list([C|ASES0],CASES),!.
    into_case_l1t_list(CASES,CASES):- is_list(CASES),!.
        is_case_l1t(AA,[AA,Value],Value):-!.
        is_case_l1t(AA,[AA|Value],Value).

   maybe_special_key_l1ts(Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     subst_args(Eq,RetType,Depth,Self,K,AK), K\=@=AK,!,
     maybe_special_key_l1ts(Depth,Self,KVI,KVO).
   maybe_special_key_l1ts(Depth,Self,[_|KVI],KVO):-
     maybe_special_key_l1ts(Depth,Self,KVI,KVO).
   maybe_special_key_l1ts(_Depth,_Self,[],[]).


%[collapse,[1,2,3]]
subst_args1(Eq,RetType,Depth,Self,['collapse',List],Res):-!, bagof_subst(Depth,Self,List,Res).
%[superpose,[1,2,3]]
subst_args1(Eq,RetType,Depth,Self,['superpose',List],Res):- !, member(E,List),subst_args(Eq,RetType,Depth,Self,E,Res).

get_l1t_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_l1t_sa_p2(P3,E,Cmpd,SA).
get_l1t_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_l1t_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_l1t_sa_p1(P3,E,Arg,SA).
subst_args1(Eq,RetType,Depth,Self, Term, Res):- fail,
  mnotrace(( get_l1t_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   subst_args(Eq,RetType,Depth,Self, Term, Res).

/*

sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- sub_sterm1(Sub,Term).
sub_sterm1(_  ,List):- \+ compound(List),!,fail.
sub_sterm1(Sub,List):- is_list(List),!,member(SL,List),sub_sterm(Sub,SL).
sub_sterm1(_  ,[_|_]):-!,fail.
sub_sterm1(Sub,Term):- arg(_,Term,SL),sub_sterm(Sub,SL).
*/
% =================================================================
% =================================================================
% =================================================================
%  NOP/EQUALITU/DO
% =================================================================
% =================================================================
% ================================================================
subst_args1(Eq,RetType,_Depth,_Self,['nop'],                 _ ):- !, fail.
subst_args1(Eq,RetType,Depth,Self,['nop',Expr], Empty):- !,
  ignore(subst_args(Eq,RetType,Depth,Self,Expr,_)),
  make_nop([], Empty).

subst_args1(Eq,RetType,Depth,Self,['do',Expr], Empty):- !,
  forall(subst_args(Eq,RetType,Depth,Self,Expr,_),true),
  make_nop([],Empty).

subst_args1(Eq,RetType,_Depth,_Self,['call',S], TF):- !, eval_call(S,TF).

% =================================================================
% =================================================================
% =================================================================
%  if/If
% =================================================================
% =================================================================
% =================================================================


subst_args1(Eq,RetType,Depth,Self,PredDecl,Res):-
  Do_more_defs = do_more_defs(true),
  clause(eval_21(subst_args,RetType,Depth,Self,PredDecl,Res),Body),
  Do_more_defs == do_more_defs(true),
  (call(Body), nb_setarg(1,Do_more_defs,false),
   deterministic(TF), (TF==true -> ! ; true)).


subst_args1(Eq,RetType,Depth,Self,['if',Cond,Then,Else],Res):- fail, !,
   subst_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> subst_args(Eq,RetType,Depth,Self,Then,Res)
     ;  subst_args(Eq,RetType,Depth,Self,Else,Res)).

subst_args1(Eq,RetType,Depth,Self,['If',Cond,Then,Else],Res):- fail, !,
   subst_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> subst_args(Eq,RetType,Depth,Self,Then,Res)
     ;  subst_args(Eq,RetType,Depth,Self,Else,Res)).

subst_args1(Eq,RetType,Depth,Self,['If',Cond,Then],Res):- fail, !,
   subst_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> subst_args(Eq,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).

subst_args1(Eq,RetType,Depth,Self,['if',Cond,Then],Res):- fail, !,
   subst_args(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> subst_args(Eq,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).


subst_args1(Eq,RetType,_Dpth,_Slf,[_,Nothing],NothingO):-
   'Nothing'==Nothing,!,do_expander(Eq,RetType,Nothing,NothingO).



subst_args1(Eq,RetType,Depth,Self, Term, Res):- fail,
   mnotrace(( get_l1t_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, setof_subst(Depth,Self,List,Var),
   subst_args(Eq,RetType,Depth,Self, Term, Res).


%max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).


subst_args1(Eq,RetType,_Dpth,_Slf,[_,Nothing],Nothing):- 'Nothing'==Nothing,!.



subst_args1(Eq,RetType,Depth,Self,['colapse'|List], Flat):- !, maplist(subst_args(Eq,RetType,Depth,Self),List,Res),flatten(Res,Flat).
subst_args1(Eq,RetType,_Dpth,_Slf,['car-atom',Atom],CAR):- !, Atom=[CAR|_],!.
subst_args1(Eq,RetType,_Dpth,_Slf,['cdr-atom',Atom],CDR):- !, Atom=[_|CDR],!.

subst_args1(Eq,RetType,Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  subst_args(Eq,RetType,Depth,Self,A,AA), subst_args(Eq,RetType,Depth,Self,B,BB).

subst_args1(Eq,RetType,Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
   subst_args(Eq,RetType,Depth,Self,A,AA), subst_args(Eq,RetType,Depth,Self,B,BB).


subst_args1(Eq,RetType,Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !, subst_args(Eq,RetType,Depth,Self,StateExpr,StateMonad),
  subst_args(Eq,RetType,Depth,Self,UpdatedValue,Value),  'change-state!'(Depth,Self,StateMonad, Value, Ret).
subst_args1(Eq,RetType,Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  subst_args(Eq,RetType,Depth,Self,UpdatedValue,Value),  'new-state'(Depth,Self,Value,StateMonad).
subst_args1(Eq,RetType,Depth,Self,['get-state',StateExpr],Value):- !,
  subst_args(Eq,RetType,Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value).



% subst_args1(Eq,RetType,Depth,Self,['get-state',Expr],Value):- !, subst_args(Eq,RetType,Depth,Self,Expr,State), arg(1,State,Value).



% check_type:- option_else(typecheck,TF,'False'), TF=='True'.

:- dynamic is_registered_state/1.
:- flush_output.
:- setenv('RUST_BACKTRACE',full).

/*
% Function to check if an value is registered as a state name
:- dynamic(is_registered_state/1).

is_nb_state(G):-  is_valid_nb_state(G) -> true ;
                 is_registered_state(G),nb_bound(G,S),is_valid_nb_state(S).


:- multifile(state_type_method/3).
:- dynamic(state_type_method/3).
space_type_method(is_nb_state,new_space,init_state).
space_type_method(is_nb_state,clear_space,clear_nb_values).
space_type_method(is_nb_state,add_atom,add_nb_value).
space_type_method(is_nb_state,remove_atom,'change-state!').
space_type_method(is_nb_state,replace_atom,replace_nb_value).
space_type_method(is_nb_state,atom_count,value_nb_count).
space_type_method(is_nb_state,get_atoms,'get-state').
space_type_method(is_nb_state,atom_iter,value_nb_iter).

state_type_method(is_nb_state,new_state,init_state).
state_type_method(is_nb_state,clear_state,clear_nb_values).
state_type_method(is_nb_state,add_value,add_nb_value).
state_type_method(is_nb_state,remove_value,'change-state!').
state_type_method(is_nb_state,replace_value,replace_nb_value).
state_type_method(is_nb_state,value_count,value_nb_count).
state_type_method(is_nb_state,'get-state','get-state').
state_type_method(is_nb_state,value_iter,value_nb_iter).
%state_type_method(is_nb_state,query,state_nb_query).

% Clear all values from a state
clear_nb_values(StateNameOrInstance) :-
    fetch_or_create_state(StateNameOrInstance, State),
    nb_setarg(1, State, []).



% Function to confirm if a term represents a state
is_valid_nb_state(State):- compound(State),functor(State,'State',_).

% Find the original name of a given state
state_original_name(State, Name) :-
    is_registered_state(Name),
    nb_bound(Name, State).

% Register and initialize a new state
init_state(Name) :-
    State = 'State'(_,_),
    asserta(is_registered_state(Name)),
    nb_setval(Name, State).

% Change a value in a state
'change-state!'(Depth,Self,StateNameOrInstance, UpdatedValue, Out) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(2, State, Type),
    ( (check_type,\+ get_type_l1t(Depth,Self,UpdatedValue,Type))
     -> (Out = ['Error', UpdatedValue, 'BadType'])
     ; (nb_setarg(1, State, UpdatedValue), Out = State) ).

% Fetch all values from a state
'get-state'(StateNameOrInstance, Values) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(1, State, Values).

'new-state'(Depth,Self,Init,'State'(Init, Type)):- check_type->get_type_l1t(Depth,Self,Init,Type);true.

'new-state'(Init,'State'(Init, Type)):- check_type->get_type_l1t(10,'&self',Init,Type);true.

fetch_or_create_state(Name):- fetch_or_create_state(Name,_).
% Fetch an existing state or create a new one

fetch_or_create_state(State, State) :- is_valid_nb_state(State),!.
fetch_or_create_state(NameOrInstance, State) :-
    (   atom(NameOrInstance)
    ->  (is_registered_state(NameOrInstance)
        ->  nb_bound(NameOrInstance, State)
        ;   init_state(NameOrInstance),
            nb_bound(NameOrInstance, State))
    ;   is_valid_nb_state(NameOrInstance)
    ->  State = NameOrInstance
    ;   writeln('Error: Invalid input.')
    ),
    is_valid_nb_state(State).

*/

subst_args1(Eq,RetType,Depth,Self,['get-type',Val],Type):- !, get_type_l1t(Depth,Self,Val,Type),ground(Type),Type\==[], Type\==Val,!.

% mnotrace(G):- once(G).
/*
is_decl_type(ST):- metta_type(_,_,Type),sub_term_safely(T,Type),T=@=ST, \+ nontype(ST).
is_type(Type):- nontype(Type),!,fail.
is_type(Type):- is_decl_type(Type).
is_type(Type):- atom(Type).

nontype(Type):- var(Type),!.
nontype('->').
nontype(N):- number(N).

*/

needs_subst(EvalMe):- is_list(EvalMe),!.


get_type_l1t(_Dpth,_Slf,Var,'%Undefined%'):- var(Var),!.
get_type_l1t(_Dpth,_Slf,Val,'Number'):- number(Val),!.
get_type_l1t(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_type_l1t(Depth,Self,Expr,['StateMonad',Type]):- is_valid_nb_state(Expr),'get-state'(Expr,Val),!,
   get_type_l1t(Depth,Self,Val,Type).


get_type_l1t(Depth,Self,EvalMe,Type):- needs_subst(EvalMe),subst_args(Eq,RetType,Depth,Self,EvalMe,Val), \+ needs_subst(Val),!,
   get_type_l1t(Depth,Self,Val,Type).

get_type_l1t(_Dpth,Self,[Fn|_],Type):- symbol(Fn),metta_type(Self,Fn,List),last_element(List,Type), nonvar(Type),
   is_type(Type).
get_type_l1t(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,List,LType),last_element(LType,Type), nonvar(Type),
   is_type(Type).

get_type_l1t(Depth,_Slf,Type,Type):- Depth<1,!.
get_type_l1t(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,Type,['->'|List]).
get_type_l1t(Depth,Self,List,Types):- List\==[], is_list(List),Depth2 is Depth-1,maplist(get_type_l1t(Depth2,Self),List,Types).
get_type_l1t(_Dpth,Self,Fn,Type):- symbol(Fn),metta_type(Self,Fn,Type),!.
%get_type_l1t(Depth,Self,Fn,Type):- nonvar(Fn),metta_type(Self,Fn,Type2),Depth2 is Depth-1,get_type_l1t(Depth2,Self,Type2,Type).
%get_type_l1t(Depth,Self,Fn,Type):- Depth>0,nonvar(Fn),metta_type(Self,Type,Fn),!. %,!,last_element(List,Type).

get_type_l1t(Depth,Self,Expr,Type):-Depth2 is Depth-1, subst_args(Eq,RetType,Depth2,Self,Expr,Val),Expr\=@=Val,get_type_l1t(Depth2,Self,Val,Type).


get_type_l1t(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_type_l1t(_Dpth,_Slf,Val,Type):- is_decl_type(Val),Type=Val.
get_type_l1t(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
get_type_l1t(_Dpth,_Slf,Val,'Symbol'):- symbol(Val).
%get_type_l1t(Depth,Self,[T|List],['List',Type]):- Depth2 is Depth-1,  is_list(List),get_type_l1t(Depth2,Self,T,Type),!,
%  forall((member(Ele,List),nonvar(Ele)),get_type_l1t(Depth2,Self,Ele,Type)),!.
%get_type_l1t(Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
get_type_l1t(_Dpth,_Slf,Cmpd,Type):- \+ ground(Cmpd),!,Type=[].
get_type_l1t(_Dpth,_Slf,_,'%Undefined%'):- fail.


subst_args1(Eq,RetType,Depth,Self,['length',L],Res):- !, subst_args(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
subst_args1(Eq,RetType,Depth,Self,['CountElement',L],Res):- !, subst_args(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).

/*

is_feo_f('Cons').

is_seo_f('{...}').
is_seo_f('[...]').
is_seo_f('{}').
is_seo_f('[]').
is_seo_f('StateMonad').
is_seo_f('State').
is_seo_f('Event').
is_seo_f('Concept').
is_seo_f(N):- number(N),!.

*/

/*
subst_args1(Eq,RetType,Depth,Self,[F,A|Args],Res):-
   \+ self_subst(A),
   subst_args(Eq,RetType,Depth,Self,A,AA),AA\==A,
   subst_args(Eq,RetType,Depth,Self,[F,AA|Args],Res).


subst_args1(Eq,RetType,Depth,Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_subst(A),
   subst_args(Eq,RetType,Depth,Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), subst_args(Eq,RetType,Depth,Self,[F,A1|NewArgs],Res)))).
*/

/* %%

% !(assertEqualToResult ((inc) 2) (3))
subst_args1(Eq,RetType,Depth,Self,[F|Args],Res):- is_list(F),
  metta_atom_iter_l1t(Eq,Depth,Self,['=',F,R]), subst_args(Eq,RetType,Depth,Self,[R|Args],Res).

subst_args1(Eq,RetType,Depth,Self,[F|Args],Res):- is_list(F), Args\==[],
  append(F,Args,FArgs),!,subst_args(Eq,RetType,Depth,Self,FArgs,Res).
*/
subst_args1(Eq,RetType,_Dpth,Self,['import!',Other,File],RetVal):- into_space(Self,Other,Space),!, include_metta(Space,File),!,make_nop(Space,RetVal). %RetVal=[].
subst_args1(Eq,RetType,Depth,Self,['bind!',Other,Expr],RetVal):-
   into_name(Self,Other,Name),!,subst_args(Eq,RetType,Depth,Self,Expr,Value),nb_setval(Name,Value),  make_nop(Value,RetVal).
subst_args1(Eq,RetType,Depth,Self,['pragma!',Other,Expr],RetVal):-
   into_name(Self,Other,Name),!,subst_args(Eq,RetType,Depth,Self,Expr,Value),set_option_value(Name,Value),  make_nop(Value,RetVal).
subst_args1(Eq,RetType,_Dpth,Self,['transfer!',File],RetVal):- !, include_metta(Self,File),  make_nop(Self,RetVal).



%l_l1t_args1(Depth,Self,['nop',Expr],Empty):- !,  subst_args(Eq,RetType,Depth,Self,Expr,_), make_nop([],Empty).

/*
is_True(T):- T\=='False',T\=='F',T\==[].

is_and(S):- \+ atom(S),!,fail.
is_and('#COMMA'). is_and(','). is_and('and'). is_and('And').
*/
subst_args1(Eq,RetType,_Dpth,_Slf,[And],'True'):- is_and(And),!.
subst_args1(Eq,RetType,Depth,Self,['and',X,Y],TF):- !, as_tf((subst_args(Eq,RetType,Depth,Self,X,'True'),subst_args(Eq,RetType,Depth,Self,Y,'True')),TF).
subst_args1(Eq,RetType,Depth,Self,[And,X|Y],TF):- is_and(And),!,subst_args(Eq,RetType,Depth,Self,X,TF1),
  is_True(TF1),subst_args1(Eq,RetType,Depth,Self,[And|Y],TF).
%subst_args2(Eq,Depth,Self,[H|T],_):- \+ is_list(T),!,fail.
subst_args1(Eq,RetType,Depth,Self,['or',X,Y],TF):- !, as_tf((subst_args(Eq,RetType,Depth,Self,X,'True');subst_args(Eq,RetType,Depth,Self,Y,'True')),TF).




subst_args1(Eq,RetType,Depth,Self,['+',N1,N2],N):- number(N1),!,
   subst_args(Eq,RetType,Depth,Self,N2,N2Res), catch_err(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).
subst_args1(Eq,RetType,Depth,Self,['-',N1,N2],N):- number(N1),!,
   subst_args(Eq,RetType,Depth,Self,N2,N2Res), catch_err(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).

subst_args1(Eq,RetType,Depth,Self,[V|VI],[V|VO]):- nonvar(V), fail, is_metta_data_functor(Eq,V),is_list(VI),!,maplist(subst_args(Eq,RetType,Depth,Self),VI,VO).

subst_args1(Eq,RetType,Depth,Self,X,Y):-
  (subst_args2(Eq,Depth,Self,X,Y)*->true;
    (subst_args2_failed(Depth,Self,X,Y)*->true;X=Y)).


subst_args2_failed(_Dpth,_Slf,T,TT):- T==[],!,TT=[].
subst_args2_failed(_Dpth,_Slf,T,TT):- var(T),!,TT=T.
subst_args2_failed(_Dpth,_Slf,[F|LESS],Res):- once(subst_selfless([F|LESS],Res)),mnotrace([F|LESS]\==Res),!.
%subst_args2_failed(Depth,Self,[V|Nil],[O]):- Nil==[], once(subst_args(Eq,RetType,Depth,Self,V,O)),V\=@=O,!.
subst_args2_failed(Depth,Self,[H|T],[HH|TT]):- !,
  subst_args(Eq,RetType,Depth,Self,H,HH),
  subst_args2_failed(Depth,Self,T,TT).
subst_args2_failed(Depth,Self,T,T):-!.
%subst_args2_failed(Depth,Self,T,TT):- subst_args(Eq,RetType,Depth,Self,T,TT).

   %subst_args(Eq,RetType,Depth,Self,X,Y):- subst_args1(Eq,RetType,Depth,Self,X,Y)*->true;Y=[].

%subst_args1(Eq,RetType,Depth,_,_,_):- Depth<1,!,fail.
%subst_args1(Eq,RetType,Depth,_,X,Y):- Depth<3, !, ground(X), (Y=X).
%subst_args1(Eq,RetType,_Dpth,_Slf,X,Y):- self_subst(X),!,Y=X.

% Kills zero arity functions subst_args1(Eq,RetType,Depth,Self,[X|Nil],[Y]):- Nil ==[],!,subst_args(Eq,RetType,Depth,Self,X,Y).


/*
into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).
subst_args2(Eq,_Dpth,_Slf,Name,Value):- atom(Name), nb_bound(Name,Value),!.
*/
% Macro Functions
%subst_args1(Eq,RetType,Depth,_,_,_):- Depth<1,!,fail.
subst_args2(Eq,Depth,_,X,Y):- Depth<3, !, fail, ground(X), (Y=X).
subst_args2(Eq,Depth,Self,[F|PredDecl],Res):- fail,
   Depth>1,
   mnotrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   subst_args(Eq,RetType,Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl, subst(PredDecl,SSub,Repl,Temp))),
   subst_args(Eq,RetType,Depth,Self,[F|Temp],Res).



% user defined function
subst_args2(Eq,Depth,Self,[H|PredDecl],Res):- mnotrace(is_user_defined_head(Eq,Self,H)),!,
   subst_args30(Eq,Depth,Self,[H|PredDecl],Res).

% function inherited by system
%subst_args2(Eq,Depth,Self,PredDecl,Res):-  subst_args40(Eq,Depth,Self,PredDecl,Res).

/*
last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.




%catch_warn(G):- notrace(catch_err(G,E,(wdmsg(catch_warn(G)-->E),fail))).
%catch_nowarn(G):- notrace(catch_err(G,error(_,_),fail)).

%as_tf(G,TF):- catch_nowarn((call(G)*->TF='True';TF='False')).
*/
%subst_selfless([O|_],_):- var(O),!,fail.
%subst_selfless(['==',X,Y],TF):- (number(X);number(Y)), as_tf(X=:=Y,TF),!.
%subst_selfless(['==',X,Y],TF):- as_tf(X=Y,TF),!.
subst_selfless(X,Y):- !,eval_selfless(_,_,_,_,X,Y).
/*subst_selfless(['=',X,Y],TF):-!,as_tf(X=Y,TF).
subst_selfless(['>',X,Y],TF):-!,as_tf(X>Y,TF).
subst_selfless(['<',X,Y],TF):-!,as_tf(X<Y,TF).
subst_selfless(['=>',X,Y],TF):-!,as_tf(X>=Y,TF).
subst_selfless(['<=',X,Y],TF):-!,as_tf(X=<Y,TF).
subst_selfless(['%',X,Y],TF):-!,subst_selfless(['mod',X,Y],TF).

subst_selfless(LIS,Y):-  mnotrace((
   LIS=[F,_,_], atom(F), catch_warn(current_op(_,yfx,F)),
   catch_err((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.
*/
% less Macro-ey Functions




/*
; Bind &kb22 to a new empty Space
!(bind! &kb22 (new-space))

; Some knowledge
(= (frog $x)
   (and (croaks $x)
        (eat_flies $x)))
(= (croaks Fritz) True)
(= (eat_flies Fritz) True)
(= (croaks Sam) True)
(= (eat_flies Sam) True)
(= (green $x)
   (frog $x))

; Define conditional
(: ift (-> Bool Atom Atom))
(= (ift True $then) $then)

; For anything that is green, assert it is Green in &kb22
!(ift (green $x)
      (add-atom &kb22 (Green $x)))

; Retrieve the inferred Green things: Fritz and Sam.
!(assertEqualToResult
  (match &kb22 (Green $x) $x)
  (Fritz Sam))
*/
:- discontiguous subst_args3/4.
%subst_args2(Eq,Depth,Self,PredDecl,Res):- subst_args3(Depth,Self,PredDecl,Res).

%subst_args2(Eq,_Dpth,_Slf,L1,Res):- is_list(L1),maplist(self_subst,L1),!,Res=L1.
%subst_args2(Eq,_Depth,_Self,X,X).

/*
is_user_defined_head(Eq,Other,H):- mnotrace(is_user_defined_head0(Eq,Other,H)).
is_user_defined_head0(Eq,Other,[H|_]):- !, nonvar(H),!, is_user_defined_head_f(Eq,Other,H).
is_user_defined_head0(Eq,Other,H):- callable(H),!,functor(H,F,_), is_user_defined_head_f(Eq,Other,F).
is_user_defined_head0(Eq,Other,H):- is_user_defined_head_f(Eq,Other,H).

is_user_defined_head_f(Eq,Other,H):- is_user_defined_head_f1(Eq,Other,H).
is_user_defined_head_f(Eq,Other,H):- is_user_defined_head_f1(Eq,Other,[H|_]).

%is_user_defined_head_f1(Eq,Other,H):- metta_type(Other,H,_).
%is_user_defined_head_f1(Eq,Other,H):- get_metta_atom(Eq,Other,[H|_]).
is_user_defined_head_f1(Eq,Other,H):- metta_eq_def(Eq,Other,[H|_],_).
%is_user_defined_head_f(Eq,_,H):- is_metta_builtin(H).


is_special_op(F):- \+ atom(F), \+ var(F), !, fail.
is_special_op('case').
is_special_op(':').
is_special_op('=').
is_special_op('->').
is_special_op('let').
is_special_op('let*').
is_special_op('if').
is_special_op('rtrace').
is_special_op('or').
is_special_op('and').
is_special_op('not').
is_special_op('match').
is_special_op('call').
is_special_op('let').
is_special_op('let*').
is_special_op('nop').
is_special_op('assertEqual').
is_special_op('assertEqualToResult').

is_metta_builtin(Special):- is_special_op(Special).
is_metta_builtin('==').
is_metta_builtin(F):- once(atom(F);var(F)), current_op(_,yfx,F).
is_metta_builtin('println!').
is_metta_builtin('transfer!').
is_metta_builtin('collapse').
is_metta_builtin('superpose').
is_metta_builtin('+').
is_metta_builtin('-').
is_metta_builtin('*').
is_metta_builtin('/').
is_metta_builtin('%').
is_metta_builtin('==').
is_metta_builtin('<').
is_metta_builtin('>').
is_metta_builtin('all').
is_metta_builtin('import!').
is_metta_builtin('pragma!').
*/


subst_args30(Eq,Depth,Self,H,B):- if_or_else(subst_args34(Depth,Self,H,B),subst_args37(Eq,Depth,Self,H,B)).

subst_args34(_Dpth,Self,H,B):-
  metta_eq_def(Eq,Self,H,B).

% Has argument that is headed by the same function
subst_args37(Eq,Depth,Self,[H1|Args],Res):-
   mnotrace((append(Left,[[H2|H2Args]|Rest],Args), H2==H1)),!,
   subst_args(Eq,RetType,Depth,Self,[H2|H2Args],ArgRes),
   mnotrace((ArgRes\==[H2|H2Args], append(Left,[ArgRes|Rest],NewArgs))),
   subst_args30(Eq,Depth,Self,[H1|NewArgs],Res).

subst_args37(Eq,Depth,Self,[[H|Start]|T1],Y):- !,
   mnotrace((is_user_defined_head_f(Eq,Self,H),is_list(Start))),
   metta_eq_def(Eq,Self,[H|Start],Left),
   subst_args(Eq,RetType,Depth,Self,[Left|T1],Y).

% Has subterm to subst
subst_args37(Eq,Depth,Self,[F|PredDecl],Res):-
   Depth>1, fail,
   quietly(sub_sterm1(SSub,PredDecl)),
   mnotrace((ground(SSub),SSub=[_|Sub], is_list(Sub),maplist(atomic,SSub))),
   subst_args(Eq,RetType,Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   subst_args30(Eq,Depth,Self,[F|Temp],Res).

%subst_args37(Eq,Depth,Self,X,Y):- (subst_args38(Eq,Depth,Self,X,Y)*->true;metta_atom_iter_l1t(Eq,Depth,Self,[=,X,Y])).

subst_args37(Eq,Depth,Self,PredDecl,Res):- fail,
 ((term_variables(PredDecl,Vars),
  (get_metta_atom(Eq,Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (subst_args(Eq,RetType,Depth,Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))))),
 PredDecl\=@=Res.

subst_args38(Eq,_Dpth,Self,[H|_],_):- mnotrace( \+ is_user_defined_head_f(Eq,Self,H) ), !,fail.
subst_args38(Eq,_Dpth,Self,[H|T1],Y):- metta_eq_def(Eq,Self,[H|T1],Y).
%subst_args38(Eq,_Dpth,Self,[H|T1],'True'):- get_metta_atom(Eq,Self,[H|T1]).
subst_args38(Eq,_Dpth,Self,CALL,Y):- fail,append(Left,[Y],CALL),metta_eq_def(Eq,Self,Left,Y).


%subst_args3(Depth,Self,['ift',CR,Then],RO):- fail, !, %fail, % trace,
%   metta_eq_def(Eq,Self,['ift',R,Then],Become),subst_args(Eq,RetType,Depth,Self,CR,R),subst_args(Eq,RetType,Depth,Self,Then,_True),subst_args(Eq,RetType,Depth,Self,Become,RO).

metta_atom_iter_l1t(Eq,_Dpth,Other,[Equal,H,B]):- Eq == Equal,!,
  metta_eq_def(Eq,Other,H,B).

metta_atom_iter_l1t(Eq,Depth,_,_):- Depth<3,!,fail.
metta_atom_iter_l1t(Eq,_Dpth,_Slf,[]):-!.
%metta_atom_iter_l1t(Eq,_Dpth,Other,H):- get_metta_atom(Eq,Other,H).
metta_atom_iter_l1t(Eq,Depth,Other,H):- D2 is Depth -1, metta_eq_def(Eq,Other,H,B),metta_atom_iter_l1t(Eq,D2,Other,B).
metta_atom_iter_l1t(Eq,_Dpth,_Slf,[And]):- is_and(And),!.
metta_atom_iter_l1t(Eq,Depth,Self,[And,X|Y]):- is_and(And),!,D2 is Depth -1, metta_atom_iter_l1t(Eq,D2,Self,X),metta_atom_iter_l1t(Eq,D2,Self,[And|Y]).
/*
%metta_atom_iter_l1t2(_,Self,[=,X,Y]):- metta_eq_def(Eq,Self,X,Y).
%metta_atom_iter_l1t2(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!, metta_eq_def(Eq,Other,H,B).
%metta_atom_iter_l1t2(_Dpth,Self,X,Y):- metta_eq_def(Eq,Self,X,Y). %, Y\=='True'.
%metta_atom_iter_l1t2(_Dpth,Self,X,Y):- get_metta_atom(Eq,Self,[=,X,Y]). %, Y\=='True'.

*/
metta_atom_iter_l1t_ref(Other,['=',H,B],Ref):-metta_eq_def(Eq,Other,H,B).
%metta_atom_iter_l1t_ref(Other,H,Ref):-clause(get_metta_atom(Eq,Other,H),true,Ref).

%not_compound(Term):- \+ is_list(Term),!.
%subst_args2(Eq,Depth,Self,Term,Res):- maplist(not_compound,Term),!,subst_args345(Depth,Self,Term,Res).


% function inherited by system
subst_args40(Eq,Depth,Self,[F|X],FY):- is_function(F), \+ is_special_op(F), is_list(X),
  maplist(subst_args(Eq,RetType,Depth,Self),X,Y),!,subst_args5(Depth,Self,[F|Y],FY).
subst_args40(Eq,Depth,Self,FX,FY):- subst_args5(Depth,Self,FX,FY).

%subst_args5(_Dpth,_Slf,[F|LESS],Res):- once(subst_selfless([F|LESS],Res)),mnotrace(([F|LESS]\==Res)),!.
subst_args5(Depth,Self,[AE|More],TF):- eval_selfless(_,_,Depth,Self,[AE|More],TF),!.
subst_args5(Depth,Self,[AE|More],TF):- is_system_pred(AE), length(More,Len),
  (is_syspred(AE,Len,Pred),catch_warn(as_tf(apply(Pred,More),TF)))*->true;
 subst_args6(Depth,Self,[AE|More],TF).
subst_args6(_Dpth,_Slf,[AE|More],TF):- is_system_pred(AE),length([AE|More],Len),
 is_syspred(AE,Len,Pred),append(More,[TF],Args),!,catch_warn(apply(Pred,Args)).

%subst_args40(Eq,Depth,Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,subst_args(Eq,RetType,Depth,Self,[F2|X2],Y2),subst_args(Eq,RetType,Depth,Self,X1,Y1).


%cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
bagof_subst(Depth,Self,X,L):- !,findall(E,subst_args(Eq,RetType,Depth,Self,X,E),L).
setof_subst(Depth,Self,X,S):- !,findall(E,subst_args(Eq,RetType,Depth,Self,X,E),L),sort(L,S).
%setof_subst(Depth,Self,X,S):- setof(E,subst_args(Eq,RetType,Depth,Self,X,E),S)*->true;S=[].


:- find_missing_cuts.
