:- style_check(-singleton).

self_eval(X):- \+ callable(X),!.
self_eval(X):- is_valid_nb_state(X),!.
%self_eval(X):- string(X),!.
%self_eval(X):- number(X),!.
%self_eval([]).
self_eval(X):- is_list(X),!,fail.
self_eval(X):- typed_list(X,_,_),!.
%self_eval(X):- compound(X),!.
%self_eval(X):- is_ref(X),!,fail.
self_eval(X):- atom(X),!, \+ nb_current(X,_),!.
self_eval('True'). self_eval('False'). % self_eval('F').
self_eval('Empty').


:-  set_prolog_flag(access_level,system).
hyde(F/A):- functor(P,F,A), redefine_system_predicate(P),'$hide'(F/A), '$iso'(F/A).
:- 'hyde'(option_else/2).
:- 'hyde'(atom/1).
:- 'hyde'(quietly/1).
:- 'hyde'(notrace/1).
:- 'hyde'(var/1).
:- 'hyde'(is_list/1).
:- 'hyde'(copy_term/2).
:- 'hyde'(nonvar/1).
%:- 'hyde'(option_value/2).



:- nb_setval(self_space, '&self').
evals_to(XX,Y):- Y==XX,!.
evals_to(XX,Y):- Y=='True',!, is_True(XX),!.

current_self(Space):- nb_current(self_space,Space).
eval_args(A,AA):- eval_args0(_RetType,A,AA).
eval_args0(RetType,A,AA):-
  current_self(Space),
  eval_args0(RetType,100,Space,A,AA).

%eval_args0(RetType,Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(eval_args0(RetType,X)),fail.
eval_args(A,AA,X,Y):- eval_args0(_RetType,A,AA,X,Y).
eval_args(RetType,A,AA,X,Y):- eval_args0(RetType,A,AA,X,Y).

eval_args0(RetType,_Dpth,_Slf,X,Y):- nonvar(Y),X=Y,!.

eval_args0(RetType,Depth,Self,X,Y):- nonvar(Y),!,eval_args0(RetType,Depth,Self,X,XX),evals_to(XX,Y).

eval_args0(RetType,_Dpth,_Slf,X,Y):- var(X),!,Y=X.

eval_args0(RetType,_Dpth,_Slf,[X|T],Y):- T==[], number(X),!,Y=[X].


eval_args0(RetType,Depth,Self,X,Y):-   eval_args00(RetType,Depth,Self,X,Y).

/*
eval_args0(RetType,Depth,Self,[F|X],Y):-
  (F=='superpose' ; ( option_value(no_repeats,false))),
  mnotrace((D1 is Depth-1)),!,
  eval_args00(RetType,D1,Self,[F|X],Y).

eval_args0(RetType,Depth,Self,X,Y):-
  mnotrace((no_repeats_var(YY),
  D1 is Depth-1)),
  eval_args00(RetType,D1,Self,X,Y),
   mnotrace(( \+ (Y\=YY))).
*/

debugging_metta(G):- notrace((is_debugging((eval))->ignore(G);true)).


:- nodebug(metta(eval)).


w_indent(Depth,Goal):-
  \+ \+ mnotrace(ignore(((
    format('~N'),
    setup_call_cleanup(forall(between(Depth,101,_),write('  ')),Goal, format('~N')))))).
indentq(Depth,Term):-
  \+ \+ mnotrace(ignore(((
    format('~N'),
    setup_call_cleanup(forall(between(Depth,101,_),write('  ')),format('~q',[Term]),
    format('~N')))))).


with_debug(Flag,Goal):- is_debugging(Flag),!, call(Goal).
with_debug(Flag,Goal):- flag(eval_num,_,0),
  setup_call_cleanup(set_debug(Flag,true),call(Goal),set_debug(Flag,flase)).

flag_to_var(Flag,Var):- atom(Flag), \+ atom_concat('trace-on-',_,Flag),!,atom_concat('trace-on-',Flag,Var).
flag_to_var(metta(Flag),Var):- !, nonvar(Flag), flag_to_var(Flag,Var).
flag_to_var(Flag,Var):- Flag=Var.

set_debug(Flag,Val):- \+ atom(Flag), flag_to_var(Flag,Var), atom(Var),!,set_debug(Var,Val).
set_debug(Flag,true):- !, debug(metta(Flag)),flag_to_var(Flag,Var),set_option_value(Var,true).
set_debug(Flag,false):- nodebug(metta(Flag)),flag_to_var(Flag,Var),set_option_value(Var,false).
if_trace((Flag;true),Goal):- !, notrace(( catch(ignore((Goal)),E,wdmsg(E-->if_trace((Flag;true),Goal))))).
if_trace(Flag,Goal):- notrace((catch(ignore((is_debugging(Flag),Goal)),E,wdmsg(E-->if_trace(Flag,Goal))))).


%maybe_efbug(SS,G):- efbug(SS,G)*-> if_trace(eval,wdmsg(SS=G)) ; fail.
maybe_efbug(_,G):- call(G).
%efbug(P1,G):- call(P1,G).
efbug(_,G):- call(G).



is_debugging(Flag):- var(Flag),!,fail.
is_debugging((A;B)):- !, (is_debugging(A) ; is_debugging(B) ).
is_debugging((A,B)):- !, (is_debugging(A) , is_debugging(B) ).
is_debugging(not(Flag)):- !,  \+ is_debugging(Flag).
is_debugging(Flag):- Flag== false,!,fail.
is_debugging(Flag):- Flag== true,!.
is_debugging(Flag):- debugging(metta(Flag),TF),!,TF==true.
is_debugging(Flag):- debugging(Flag,TF),!,TF==true.
is_debugging(Flag):- flag_to_var(Flag,Var),
   (option_value(Var,true)->true;(Flag\==Var -> is_debugging(Var))).

:- nodebug(metta(overflow)).


eval_args99(RetType,Depth,Self,X,Y):- eval_args(RetType,Depth,Self,X,Y)*->true;eval_args_failed(Depth,Self,X,Y).

eval_args00(RetType,Depth,_Slf,X,Y):- Depth<1,!,X=Y, (\+ trace_on_overflow-> true; flag(eval_num,_,0),debug(metta(eval))).
%eval_args00(RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args00(RetType,Depth,Self,X,Y):-
  Depth2 is Depth-1,
  eval_args11(RetType,Depth,Self,X,M),
  (M\=@=X ->eval_args00(RetType,Depth2,Self,M,Y);Y=X).



eval_args11(RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args11(RetType,Depth,Self,X,Y):- \+ is_debugging((eval)),!, 
  D1 is Depth-1,
  eval_args20(RetType,D1,Self,X,Y).
eval_args11(RetType,Depth,Self,X,Y):- flag(eval_num,EX,EX+1),
  option_else(traclen,Max,100),
  if_trace(eval, (EX>Max->(set_debug(eval,false), %set_debug(overflow,false),
                       write('Switched off tracing. For a longer trace !(pragma! tracelen 101))'));true)),
  nop(notrace(no_repeats_var(YY))),
  notrace(D1 is Depth-1),
  DR is 99-D1,
  if_trace((eval),indentq(Depth,'-->'(EX,Self,X,depth(DR)))),
  Ret=retval(fail),
  call_cleanup((
    eval_args20(RetType,D1,Self,X,Y),
    notrace(( \+ (Y\=YY), nb_setarg(1,Ret,Y)))),
    mnotrace(ignore(((Y\=@=X,if_trace((eval),indentq(Depth,'<--'(EX,Ret)))))))),
  (Ret\=@=retval(fail)->true;(rtrace(eval_args00(RetType,D1,Self,X,Y)),fail)).





















:- discontiguous eval_args20/5.
:- discontiguous eval_args30/5.
:- discontiguous eval_args40/5.
:- discontiguous eval_args30fz/5.
:- discontiguous eval_args31/5.
:- discontiguous eval_args60/5.

eval_args20(RetType,_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.












% =================================================================
% =================================================================
% =================================================================
%  VAR HEADS/ NON-LISTS
% =================================================================
% =================================================================
% =================================================================





eval_args20(RetType,Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval_args0(RetType,Depth,Self,VI,VM),
  ( VM\==VI -> eval_args0(RetType,Depth,Self,[V|VM],VVO) ;
    (eval_args0(RetType,Depth,Self,V,VV), (V\==VV -> eval_args0(RetType,Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_args20(RetType,_Dpth,_Slf,X,Y):- \+ is_list(X),!,Y=X.










% =================================================================
% =================================================================
% =================================================================
%  CASE/SWITCH
% =================================================================
% =================================================================
% =================================================================

% Macro: case
eval_args20(RetType,Depth,Self,X,Res):-
   X= [CaseSym,A,CL],CaseSym == 'case', !,
   into_case_list(CL,CASES),
   findall(Key-Value,
     (nth0(Nth,CASES,Case0),
       (is_case(Key,Case0,Value),
        if_trace(metta(case),(format('~N'),writeqln(c(Nth,Key)=Value))))),KVs),!,
   ((eval_args0(RetType,Depth,Self,A,AA),        if_trace(metta(case),writeqln(switch=AA)),
    (select_case(Depth,Self,AA,KVs,Value)->true;(member(Void -Value,KVs),Void=='%void%')))
     *->true;(member(Void -Value,KVs),Void=='%void%')),
    eval_args0(RetType,Depth,Self,Value,Res).

  select_case(Depth,Self,AA,Cases,Value):-
     (best_key(AA,Cases,Value) -> true ;
      (maybe_special_keys(Depth,Self,Cases,CasES),
       (best_key(AA,CasES,Value) -> true ;
        (member(Void -Value,CasES),Void=='%void%')))).

  best_key(AA,Cases,Value):-
     ((member(Match-Value,Cases),AA ==Match)->true;
      ((member(Match-Value,Cases),AA=@=Match)->true;
        (member(Match-Value,Cases),AA = Match))).

		%into_case_list([[C|ASES0]],CASES):-  is_list(C),!, into_case_list([C|ASES0],CASES),!.
	into_case_list(CASES,CASES):- is_list(CASES),!.
		is_case(AA,[AA,Value],Value):-!.
		is_case(AA,[AA|Value],Value).

   maybe_special_keys(Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     eval_args0(RetType,Depth,Self,K,AK), K\=@=AK,!,
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(Depth,Self,[_|KVI],KVO):-
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(_Depth,_Self,[],[]).


% =================================================================
% =================================================================
% =================================================================
%  COLLAPSE/SUPERPOSE
% =================================================================
% =================================================================
% =================================================================



%[collapse,[1,2,3]]
eval_args20(RetType,Depth,Self,['collapse',List],Res):-!,
 bagof_eval(RetType,Depth,Self,List,Res).

%[superpose,[1,2,3]]
eval_args20(RetType,Depth,Self,['superpose',List],Res):- !,
  member(E,List),eval_args0(RetType,Depth,Self,E,Res).

%[sequential,[1,2,3]]
eval_args20(RetType,Depth,Self,['sequential',List],Res):- !,
  member(E,List),eval_args0(RetType,Depth,Self,E,Res).

get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).
eval_args20(RetType,Depth,Self, Term, Res):- fail,
  mnotrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   eval_args0(RetType,Depth,Self, Term, Res).


sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- sub_sterm1(Sub,Term).
sub_sterm1(_  ,List):- \+ compound(List),!,fail.
sub_sterm1(Sub,List):- is_list(List),!,member(SL,List),sub_sterm(Sub,SL).
sub_sterm1(_  ,[_|_]):-!,fail.
sub_sterm1(Sub,Term):- arg(_,Term,SL),sub_sterm(Sub,SL).


eval_args20(RetType,Depth,Self, Term, Res):- fail,
   mnotrace(( get_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, bagof_eval(RetType,Depth,Self,List,Var),
   eval_args0(RetType,Depth,Self, Term, Res).


max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).
% =================================================================
% =================================================================
% =================================================================
%  if/If
% =================================================================
% =================================================================
% =================================================================




eval_args20(RetType,Depth,Self,['if',Cond,Then],Res):- !,
   eval_args0('Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args0(RetType,Depth,Self,Then,Res) ; Res = []).

eval_args20(RetType,Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval_args0('Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args0(RetType,Depth,Self,Then,Res);eval_args0(RetType,Depth,Self,Else,Res)).

eval_args20(RetType,_Dpth,_Slf,[_,Nothing],Nothing):- 'Nothing'==Nothing,!.



% =================================================================
% =================================================================
% =================================================================
%  NOP/EQUALITU/DO
% =================================================================
% =================================================================
% ================================================================
eval_args20(_RetType1,Depth,Self,['nop',Expr], Empty):- !,
  eval_args0(_RetType2,Depth,Self,Expr,_),
  return_empty([],_Empty).

eval_args20(_RetType1,Depth,Self,['do',Expr], Empty):- !,
  eval_args0(_RetType2,Depth,Self,Expr,_),
  return_empty([],_Empty).
/*
eval_args20(_RetType1,Depth,Self,['do',Expr], Empty):- !,
  forall(eval_args0(_RetType2,Depth,Self,Expr,_),true),
  return_empty([],_Empty).
*/
eval_args20(RetType,Depth,Self,['nop'],_ ):- !, fail.

% =================================================================
% =================================================================
% =================================================================
%  AND/OR
% =================================================================
% =================================================================
% =================================================================

is_True(T):- T\=='False',T\=='F',T\==[].

is_and(S):- \+ atom(S),!,fail.
is_and('#COMMA'). is_and(','). is_and('and'). is_and('And').
eval_args20(RetType,_Dpth,_Slf,[And],'True'):- is_and(And),!.
eval_args20(RetType,Depth,Self,['and',X,Y],TF):- !, as_tf((eval_args0(RetType,Depth,Self,X,'True'),eval_args0(RetType,Depth,Self,Y,'True')),TF).
eval_args20(RetType,Depth,Self,[And,X|Y],TF):- is_and(And),!,eval_args0(RetType,Depth,Self,X,TF1),
  is_True(TF1),eval_args20(RetType,Depth,Self,[And|Y],TF).
%eval_args40(RetType,Depth,Self,[H|T],_):- \+ is_list(T),!,fail.
eval_args20(RetType,Depth,Self,['or',X,Y],TF):- !, as_tf((eval_args0(RetType,Depth,Self,X,'True');eval_args0(RetType,Depth,Self,Y,'True')),TF).




% =================================================================
% =================================================================
% =================================================================
%  LET/LET*
% =================================================================
% =================================================================
% =================================================================

eval_args20(RetType,Depth,Self,['let',A,A5,AA],OO):- !,
  %(var(A)->true;trace),
  ((eval_args0(RetType,Depth,Self,A5,AE), AE=A)),
  eval_args0(RetType,Depth,Self,AA,OO).
%eval_args20(RetType,Depth,Self,['let',A,A5,AA],AAO):- !,eval_args0(RetType,Depth,Self,A5,A),eval_args0(RetType,Depth,Self,AA,AAO).
eval_args20(RetType,Depth,Self,['let*',[],Body],RetVal):- !, eval_args0(RetType,Depth,Self,Body,RetVal).
eval_args20(RetType,Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
    eval_args20(RetType,Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).

eval_args20(RetType,Depth,Self,['flatten'|List], Flat):- !, maplist(eval_args0(RetType,Depth,Self),List,Res),flatten(Res,Flat).
eval_args20(RetType,Depth,Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter(Depth,Space,PredDecl).


eval_args20(RetType,Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval_args0(RetType,Depth,Self),VI,VO).
% =================================================================
% =================================================================
% =================================================================
%  TRACE/PRINT
% =================================================================
% =================================================================
% =================================================================
eval_args20(RetType,_Dpth,_Slf,['repl!'],Y):- !, repl,check_returnval(RetType,Y).
eval_args20(RetType,Depth,Self,['!',Cond],Res):- !, call(eval_args(RetType,Depth,Self,Cond,Res)).
eval_args20(RetType,Depth,Self,['rtrace',Cond],Res):- !, rtrace(eval_args(RetType,Depth,Self,Cond,Res)).
eval_args20(RetType,Depth,Self,['trace',Cond],Res):- !, with_debug(eval,eval_args(RetType,Depth,Self,Cond,Res)).
eval_args20(RetType,Depth,Self,['time',Cond],Res):- !, time_eval(eval(Cond),eval_args(RetType,Depth,Self,Cond,Res)).
eval_args20(RetType,Depth,Self,['print',Cond],Res):- !, eval_args(RetType,Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
% !(println! $1)
eval_args20(RetType,Depth,Self,['println!'|Cond],Res):- !, maplist(eval_args(RetType,Depth,Self),Cond,[Res|Out]),
   format('~N'),maplist(write_src,[Res|Out]),format('~N').
eval_args20(RetType,Depth,Self,['trace!',A|Cond],Res):- !, maplist(eval_args(RetType,Depth,Self),[A|Cond],[AA|Result]),
   last(Result,Res), format('~N'),maplist(write_src,[AA]),format('~N').

%eval_args20(RetType,Depth,Self,['trace!',A,B],C):- !,eval_args(RetType,Depth,Self,B,C),format('~N'),wdmsg(['trace!',A,B]=C),format('~N').
%eval_args20(RetType,_Dpth,_Slf,['trace!',A],A):- !, format('~N'),wdmsg(A),format('~N').

eval_args20(RetType,_Dpth,_Slf,List,Y):- is_list(List),maplist(self_eval,List),List=[H|_], \+ atom(H), !,Y=List.

% =================================================================
% =================================================================
% =================================================================
%  UNIT TESTING/assert<STAR>
% =================================================================
% =================================================================
% =================================================================


eval_args20(RetType,Depth,Self,['assertTrue', X],TF):- !, eval_args0(RetType,Depth,Self,['assertEqual',X,'True'],TF).
eval_args20(RetType,Depth,Self,['assertFalse',X],TF):- !, eval_args0(RetType,Depth,Self,['assertEqual',X,'False'],TF).

eval_args20(RetType,Depth,Self,['assertEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf(
        ['assertEqual',X,Y],
        (bagof_eval(RetType,Depth,Self,X,XX), bagof_eval(RetType,Depth,Self,Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]).

eval_args20(RetType,Depth,Self,['assertNotEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf(
        ['assertEqual',X,Y],
        (bagof_eval(RetType,Depth,Self,X,XX), bagof_eval(RetType,Depth,Self,Y,YY)),
         \+ equal_enough(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]).

eval_args20(RetType,Depth,Self,['assertEqualToResult',X,Y],RetVal):- !,
   loonit_assert_source_tf(
        ['assertEqualToResult',X,Y],
        (bagof_eval(RetType,Depth,Self,X,XX), sort(Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]).


loonit_assert_source_tf(Src,Goal,Check,TF):-
   copy_term(Goal,OrigGoal),
   loonit_asserts(Src, time_eval('\n; EVAL TEST\n;',Goal), Check),
   as_tf(Check,TF),!,
  ignore((
          once((TF='True', trace_on_pass);(TF='False', trace_on_fail)),
     with_debug(metta(eval),time_eval('Trace',OrigGoal)))).

sort_result(Res,Res):- \+ compound(Res),!.
sort_result([And|Res1],Res):- is_and(And),!,sort_result(Res1,Res).
sort_result([T,And|Res1],Res):- is_and(And),!,sort_result([T|Res1],Res).
sort_result([H|T],[HH|TT]):- !, sort_result(H,HH),sort_result(T,TT).
sort_result(Res,Res).

unify_enough(L,L).
%unify_enough(L,C):- is_list(L),into_list_args(C,CC),!,unify_lists(CC,L).
%unify_enough(C,L):- is_list(L),into_list_args(C,CC),!,unify_lists(CC,L).
%unify_enough(C,L):- \+ compound(C),!,L=C.
%unify_enough(L,C):- \+ compound(C),!,L=C.
unify_enough(L,C):- into_list_args(L,LL),into_list_args(C,CC),unify_lists(CC,LL).

%unify_lists(C,L):- \+ compound(C),!,L=C.
%unify_lists(L,C):- \+ compound(C),!,L=C.
unify_lists(L,L):-!.
unify_lists([C|CC],[L|LL]):- unify_enough(L,C),!,unify_lists(CC,LL).

equal_enough(R,V):- is_list(R),is_list(V),sort(R,RR),sort(V,VV),!,equal_enouf(RR,VV),!.
equal_enough(R,V):- copy_term(R,RR),copy_term(V,VV),equal_enouf(R,V),!,R=@=RR,V=@=VV.

%s_empty(X):- var(X),!.
s_empty(X):- var(X),!,fail.
is_empty('Empty').
is_empty([]).
is_empty([X]):-!,is_empty(X).
equal_enough_for_test(X,Y):- is_empty(X),!,is_empty(Y).
equal_enough_for_test(X,Y):- must_det_ll((subst_vars(X,XX),subst_vars(Y,YY))),!,equal_enough_for_test2(XX,YY),!.
equal_enough_for_test2(X,Y):- equal_enough(X,Y).

equal_enouf(R,V):- is_ftVar(R), is_ftVar(V), R=V,!.
equal_enouf(X,Y):- is_empty(X),!,is_empty(Y).
equal_enouf(R,V):- R=@=V, R=V, !.
equal_enouf(_,V):- V=@='...',!.
equal_enouf(L,C):- is_list(L),into_list_args(C,CC),!,equal_enouf_l(CC,L).
equal_enouf(C,L):- is_list(L),into_list_args(C,CC),!,equal_enouf_l(CC,L).
%equal_enouf(R,V):- (var(R),var(V)),!, R=V.
equal_enouf(R,V):- (var(R);var(V)),!, R==V.
equal_enouf(R,V):- number(R),number(V),!, RV is abs(R-V), RV < 0.03 .
equal_enouf(R,V):- atom(R),!,atom(V), has_unicode(R),has_unicode(V).
equal_enouf(R,V):- (\+ compound(R) ; \+ compound(V)),!, R==V.
equal_enouf(L,C):- into_list_args(L,LL),into_list_args(C,CC),!,equal_enouf_l(CC,LL).

equal_enouf_l([S1,V1|_],[S2,V2|_]):- S1 == 'State',S2 == 'State',!, equal_enouf(V1,V2).
equal_enouf_l(C,L):- \+ compound(C),!,L=@=C.
equal_enouf_l(L,C):- \+ compound(C),!,L=@=C.
equal_enouf_l([C|CC],[L|LL]):- !, equal_enouf(L,C),!,equal_enouf_l(CC,LL).


has_unicode(A):- atom_codes(A,Cs),member(N,Cs),N>127,!.
set_last_error(_).

eval_args20(_ListOfRetType,Depth,Self,['TupleConcat',A,B],OO):- !,
    eval_args0(RetType,Depth,Self,A,AA),
    eval_args0(RetType,Depth,Self,B,BB),
    append(AA,BB,OO).
eval_args20(_RetType,Depth,Self,['range',A,B],OO):- (is_list(A);is_list(B)),
  ((eval_args0(RetType,Depth,Self,A,AA),
    eval_args0(RetType,Depth,Self,B,BB))),
    ((AA+BB)\=@=(A+B)),
    eval_args20(_RetType,Depth,Self,['range',AA,BB],OO),!.


%eval_args20(RetType,Depth,Self,['colapse'|List], Flat):- !, maplist(eval_args(RetType,Depth,Self),List,Res),flatten(Res,Flat).

% =================================================================
% =================================================================
% =================================================================
%  SPACE EDITING
% =================================================================
% =================================================================
% =================================================================
% do_metta(_Who,What,Where,PredDecl,_TF):-   do_metta(Where,What, PredDecl).
/*
eval_args30(RetType,_Dpth,Self,['add-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF).
eval_args30(RetType,_Dpth,Self,['remove-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF).
eval_args30(RetType,_Dpth,Self,['atom-count',Other],Count):- !, into_space(Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2.
eval_args30(RetType,_Dpth,Self,['atom-replace',Other,Rem,Add],TF):- !, into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF).
*/

eval_args20(RetType,Depth,Self,['add-atom',Other,PredDecl],Res):- !,
   into_space(Depth,Self,Other,Space),
   do_metta(python,load,Space,PredDecl,TF),return_empty([],Res),check_returnval(RetType,TF).
eval_args20(RetType,Depth,Self,['remove-atom',Other,PredDecl],Res):- !,   into_space(Depth,Self,Other,Space),
   do_metta(python,unload,Space,PredDecl,TF),return_empty([],Res),check_returnval(RetType,TF).
eval_args20(RetType,Depth,Self,['atom-count',Other],Count):- !,   (( into_space(Depth,Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),
    findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2)),check_returnval(RetType,Count).
eval_args20(RetType,Depth,Self,['atom-replace',Other,Rem,Add],TF):- !,
 ((into_space(Depth,Self,Other,Space), copy_term(Rem,RCopy),
   as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF))),
 check_returnval(RetType,TF).
eval_args20(RetType,Depth,Self,['get-atoms',Other],Atom):- !,
  ignore(RetType='Atom'),
  get_atoms(Depth,Self,Other,Atom), check_returnval(RetType,Atom).

get_atoms(Depth,Self,Other,Atom):- Other=='&self',!,metta_atom(Other,Atom).
get_atoms(Depth,Self,Other,Atom):- fail,
  is_asserted_space(Other),!,
  metta_atom(Other,Atom).
get_atoms(Depth,Self,Other,AtomO):-
  into_space(Depth,Self,Other,Space),
  space_to_Space(Depth,Self,Space,SpaceC),
  into_listoid(SpaceC,AtomsL),
  %no_repeat_var(NRAtom),
  member(Atom,AtomsL),
  %Atom = NRAtom, 
  AtomO=Atom.

space_to_Space(_Dpth,_Slf,Space,SpaceC):- compound(Space),functor(Space,_,1),arg(1,Space,L),is_list(L),!,SpaceC=Space.
space_to_Space(Depth,Self,Space,SpaceC):- findall(Atom, metta_atom_iter(Depth,Self,Space,Atom),Atoms),
   SpaceC = 'hyperon::space::DynSpace'(Atoms).

%eval_args20(RetType,Depth,Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter(Depth,Space,Goal).
%eval_args20(RetType,Depth,Self,['match',Other,Goal,Template,Else],Template):- into_space(Self,Other,Space),!,  (metta_atom_iter(Depth,Space,Goal)*->true;Else=Template).

% Match-ELSE
eval_args20(RetType,Depth,Self,['match',Other,Goal,Template,Else],Template):- !,
  ((eval_args20(RetType,Depth,Self,['match',Other,Goal,Template],Template),
       \+ return_empty([],Template))*->true;Template=Else).
% Match-TEMPLATE
eval_args20(RetType,Depth,Self,['match',Other,Goal,Template],Res):- !,
  catch(try_match(RetType,Depth,Self,Other,Goal,Template,Res),E,
   (wdmsg(catch(try_match(RetType,Depth,Self,Other,Goal,Template,Res)=E)),
     rtrace(try_match(RetType,Depth,Self,Other,Goal,Template,Res)))).

  /*
try_match(RetType,Depth,Self,Other,Goal,Template,Res):- fail,
  into_space(Depth,Self,Other,Space),
  metta_atom_iter(Depth,Self,Space,Goal),
  eval_args99(RetType,Depth,Self,Template,Res).
*/

try_match(RetType,Depth,Self,Other,GoalM,Template,Res):-
  get_atoms(Depth,Self,Other,Goal),once(GoalM=Goal),
  %eval_args99(RetType,Depth,Self,Template,Res).
  Template=Res.

/*

metta_atom_iter(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!,
  (metta_defn(Other,H,B)*->true;(metta_atom(Other,H),B='True')).

metta_atom_iter(Depth,_,_):- Depth<3,!,fail.
metta_atom_iter(_Dpth,_Slf,[]):-!.
metta_atom_iter(_Dpth,Other,H):- metta_atom(Other,H).
metta_atom_iter(Depth,Other,H):- D2 is Depth -1, metta_defn(Other,H,B),metta_atom_iter(D2,Other,B).
metta_atom_iter(_Dpth,_Slf,[And]):- is_and(And),!.
metta_atom_iter(Depth,Self,[And,X|Y]):- is_and(And),!,D2 is Depth -1, metta_atom_iter(D2,Self,X),metta_atom_iter(D2,Self,[And|Y]).
*/
metta_atom_iter(Depth,Other,H):-
   current_self(Self),
   metta_atom_iter(Depth,Self,Other,H).

metta_atom_iter(Depth,_Slf,Other,[Equal,[F|H],B]):- fail, '=' == Equal,!,  % trace,
   metta_defn(Other,[F|HH],BB),
   once(eval_until_unify(Depth,Other,H,HH)),
   once(eval_until_unify(Depth,Other,B,BB)).
metta_atom_iter(Depth,_Slf,Other,[Equal,[F|H],B]):- '=' == Equal,!,  % trace,
   metta_defn(Other,[F|HH],B), once(eval_until_unify(Depth,Other,H,HH)).

metta_atom_iter(Depth,_,_,_):- Depth<3,!,fail.
metta_atom_iter(Depth,Self,Other,[And|Y]):- atom(And), is_and(And),!,
  (Y==[] -> true ;  ( D2 is Depth -1, Y = [H|T], metta_atom_iter(D2,Self,Other,H),metta_atom_iter(D2,Self,Other,[And|T]))).
metta_atom_iter(_Dpth,_Slf,Other,H):- metta_atom(Other,H).
metta_atom_iter(Depth,Self,Other,H):- metta_defn(Other,H,B), D2 is Depth -1, metta_atom_iter(D2,Self,Other,B).
%metta_atom_iter(Depth,Other,H):- D2 is Depth -1, metta_defn(Other,H,B),metta_atom_iter(D2,Other,B).
%metta_atom_iter_l2(Depth,Self,Other,H):- metta_atom_iter(Depth,Self,Other,H).
%$metta_atom_iter(_Dpth,_Slf,[]):-!.

eval_args20(RetType,_Dpth,_Slf,['new-space'],Space):- !, 'new-space'(Space),check_returnval(RetType,Space).

/*
metta_atom_iter2(_,Self,[=,X,Y]):- metta_defn(Self,X,Y).
metta_atom_iter2(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!, metta_defn(Other,H,B).
metta_atom_iter2(_Dpth,Self,X,Y):- metta_defn(Self,X,Y). %, Y\=='True'.
metta_atom_iter2(_Dpth,Self,X,Y):- metta_atom(Self,[=,X,Y]). %, Y\=='True'.

*/
/*
metta_atom_iter2(_,Self,[=,X,Y]):- metta_defn(Self,X,Y).
metta_atom_iter2(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!, metta_defn(Other,H,B).
metta_atom_iter2(_Dpth,Self,X,Y):- metta_defn(Self,X,Y). %, Y\=='True'.
metta_atom_iter2(_Dpth,Self,X,Y):- metta_atom(Self,[=,X,Y]). %, Y\=='True'.
*/
metta_atom_iter_ref(Other,['=',H,B],Ref):-clause(metta_defn(Other,H,B),true,Ref).
metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom(Other,H),true,Ref).



eval_args20(RetType,Depth,Self,X,Y):- eval_args30(RetType,Depth,Self,X,Y).

% =================================================================
% =================================================================
% =================================================================
%  CONS/CAR/CDR
% =================================================================
% =================================================================
% =================================================================



into_pl_list(Var,Var):- var(Var),!.
into_pl_list(Nil,[]):- Nil == 'Nil',!.
into_pl_list([Cons,H,T],[HH|TT]):- Cons == 'Cons', !, into_pl_list(H,HH),into_pl_list(T,TT),!.
into_pl_list(X,X).

into_metta_cons(Var,Var):- var(Var),!.
into_metta_cons([],'Nil'):-!.
into_metta_cons([Cons, A, B ],['Cons', AA, BB]):- 'Cons'==Cons, no_cons_reduce, !,
  into_metta_cons(A,AA), into_metta_cons(B,BB).
into_metta_cons([H|T],['Cons',HH,TT]):- into_metta_cons(H,HH),into_metta_cons(T,TT),!.
into_metta_cons(X,X).

into_listoid(AtomC,Atom):- AtomC = [Cons,H,T],Cons=='Cons',!, Atom=[H,[T]].
into_listoid(AtomC,Atom):- is_list(AtomC),!,Atom=AtomC.
into_listoid(AtomC,Atom):- typed_list(AtomC,_,Atom),!.

:- if( \+  current_predicate( typed_list / 3 )).
typed_list(Cmpd,Type,List):-  compound(Cmpd), Cmpd\=[_|_], compound_name_arguments(Cmpd,Type,[List|_]),is_list(List).
:- endif.

eval_args30(RetType,_Dpth,_Slf,['car-atom',Atom],CAR):- !, Atom=[CAR|_],!.
eval_args30(RetType,_Dpth,_Slf,['cdr-atom',Atom],CDR):- !, Atom=[_|CDR],!.

eval_args30(RetType,Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  eval_args0(RetType,Depth,Self,A,AA), eval_args0(RetType,Depth,Self,B,BB).

eval_args30(RetType,Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
   eval_args0(RetType,Depth,Self,A,AA), eval_args0(RetType,Depth,Self,B,BB).


% =================================================================
% =================================================================
% =================================================================
%  STATE EDITING
% =================================================================
% =================================================================
% =================================================================

eval_args30(RetType,Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !, eval_args0(RetType,Depth,Self,StateExpr,StateMonad),
  eval_args0(RetType,Depth,Self,UpdatedValue,Value),  'change-state!'(Depth,Self,StateMonad, Value, Ret).
eval_args30(RetType,Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  eval_args0(RetType,Depth,Self,UpdatedValue,Value),  'new-state'(Depth,Self,Value,StateMonad).
eval_args30(RetType,Depth,Self,['get-state',StateExpr],Value):- !,
  eval_args0(RetType,Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value).



% eval_args30(RetType,Depth,Self,['get-state',Expr],Value):- !, eval_args0(RetType,Depth,Self,Expr,State), arg(1,State,Value).



check_type:- option_else(typecheck,TF,'False'), TF=='True'.

:- dynamic is_registered_state/1.
:- flush_output.
:- setenv('RUST_BACKTRACE',full).

% Function to check if an value is registered as a state name
:- dynamic(is_registered_state/1).
is_nb_state(G):- is_valid_nb_state(G) -> true ;
                 is_registered_state(G),nb_current(G,S),is_valid_nb_state(S).


:- multifile(space_type_method/3).
:- dynamic(space_type_method/3).
space_type_method(is_nb_space,new_space,init_space).
space_type_method(is_nb_space,clear_space,clear_nb_atoms).
space_type_method(is_nb_space,add_atom,add_nb_atom).
space_type_method(is_nb_space,remove_atom,'change-space!').
space_type_method(is_nb_space,replace_atom,replace_nb_atom).
space_type_method(is_nb_space,atom_count,atom_nb_count).
space_type_method(is_nb_space,get_atoms,'get-space').
space_type_method(is_nb_space,atom_iter,atom_nb_iter).

:- multifile(state_type_method/3).
:- dynamic(state_type_method/3).
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
    nb_current(Name, State).

% Register and initialize a new state
init_state(Name) :-
    State = 'State'(_,_),
    asserta(is_registered_state(Name)),
    nb_setval(Name, State).

% Change a value in a state
'change-state!'(Depth,Self,StateNameOrInstance, UpdatedValue, Out) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(2, State, Type),
    ( (check_type,\+ get_type(Depth,Self,UpdatedValue,Type))
     -> (Out = ['Error', UpdatedValue, 'BadType'])
     ; (nb_setarg(1, State, UpdatedValue), Out = State) ).

% Fetch all values from a state
'get-state'(StateNameOrInstance, Values) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(1, State, Values).

'new-state'(Depth,Self,Init,'State'(Init, Type)):- check_type->get_type(Depth,Self,Init,Type);true.

'new-state'(Init,'State'(Init, Type)):- check_type->get_type(10,'&self',Init,Type);true.

fetch_or_create_state(Name):- fetch_or_create_state(Name,_).
% Fetch an existing state or create a new one

fetch_or_create_state(State, State) :- is_valid_nb_state(State),!.
fetch_or_create_state(NameOrInstance, State) :-
    (   atom(NameOrInstance)
    ->  (is_registered_state(NameOrInstance)
        ->  nb_current(NameOrInstance, State)
        ;   init_state(NameOrInstance),
            nb_current(NameOrInstance, State))
    ;   is_valid_nb_state(NameOrInstance)
    ->  State = NameOrInstance
    ;   writeln('Error: Invalid input.')
    ),
    is_valid_nb_state(State).

% =================================================================
% =================================================================
% =================================================================
%  GET-TYPE
% =================================================================
% =================================================================
% =================================================================

eval_args30(RetType,Depth,Self,['get-type',Val],Type):- !, get_type(Depth,Self,Val,Type),ground(Type),Type\==[], Type\==Val,!.


/*
mnotrace(G):- once(G).

is_decl_type(ST):- metta_type(_,_,Type),sub_term(T,Type),T=@=ST, \+ nontype(ST).
is_type(Type):- nontype(Type),!,fail.
is_type(Type):- is_decl_type(Type).
is_type(Type):- atom(Type).

nontype(Type):- var(Type),!.
nontype('->').
nontype(N):- number(N).

needs_eval(EvalMe):- is_list(EvalMe),!.
get_type(_Dpth,_Slf,Var,'%Undefined%'):- var(Var),!.
get_type(_Dpth,_Slf,Val,'Number'):- number(Val),!.
get_type(Depth,Self,Expr,['StateMonad',Type]):- is_valid_nb_state(Expr),'get-state'(Expr,Val),!,
   get_type(Depth,Self,Val,Type).


get_type(Depth,Self,EvalMe,Type):- needs_eval(EvalMe),eval_args0(RetType,Depth,Self,EvalMe,Val), \+ needs_eval(Val),!,
   get_type(Depth,Self,Val,Type).

get_type(_Dpth,Self,[Fn|_],Type):- symbol(Fn),metta_type(Self,Fn,List),last_element(List,Type), nonvar(Type),
   is_type(Type).
get_type(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,List,LType),last_element(LType,Type), nonvar(Type),
   is_type(Type).

get_type(Depth,_Slf,Type,Type):- Depth<1,!.
get_type(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,Type,['->'|List]).
get_type(Depth,Self,List,Types):- List\==[], is_list(List),Depth2 is Depth-1,maplist(get_type(Depth2,Self),List,Types).
get_type(_Dpth,Self,Fn,Type):- symbol(Fn),metta_type(Self,Fn,Type),!.
%get_type(Depth,Self,Fn,Type):- nonvar(Fn),metta_type(Self,Fn,Type2),Depth2 is Depth-1,get_type(Depth2,Self,Type2,Type).
%get_type(Depth,Self,Fn,Type):- Depth>0,nonvar(Fn),metta_type(Self,Type,Fn),!. %,!,last_element(List,Type).

get_type(Depth,Self,Expr,Type):-Depth2 is Depth-1, eval_args0(RetType,Depth2,Self,Expr,Val),Expr\=@=Val,get_type(Depth2,Self,Val,Type).


get_type(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_type(_Dpth,_Slf,Val,Type):- is_decl_type(Val),Type=Val.
get_type(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
get_type(_Dpth,_Slf,Val,'Symbol'):- symbol(Val).
%get_type(Depth,Self,[T|List],['List',Type]):- Depth2 is Depth-1,  is_list(List),get_type(Depth2,Self,T,Type),!,
%  forall((member(Ele,List),nonvar(Ele)),get_type(Depth2,Self,Ele,Type)),!.
%get_type(Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
get_type(_Dpth,_Slf,Cmpd,Type):- \+ ground(Cmpd),!,Type=[].
get_type(_Dpth,_Slf,_,'%Undefined%'):- fail.


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








eval_args30(RetType,Depth,Self,['length',L],Res):- !, eval_args0(RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_args30(RetType,Depth,Self,['CountElement',L],Res):- !, eval_args0(RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).


/*
eval_args30(RetType,Depth,Self,[F,A|Args],Res):-
   \+ self_eval(A),
   eval_args0(RetType,Depth,Self,A,AA),AA\==A,
   eval_args0(RetType,Depth,Self,[F,AA|Args],Res).


eval_args30(RetType,Depth,Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_eval(A),
   eval_args0(RetType,Depth,Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), eval_args0(RetType,Depth,Self,[F,A1|NewArgs],Res)))).
*/

/* %%

% !(assertEqualToResult ((inc) 2) (3))
eval_args30(RetType,Depth,Self,[F|Args],Res):- is_list(F),
  metta_atom_iter(Depth,Self,['=',F,R]), eval_args0(RetType,Depth,Self,[R|Args],Res).

eval_args30(RetType,Depth,Self,[F|Args],Res):- is_list(F), Args\==[],
  append(F,Args,FArgs),!,eval_args0(RetType,Depth,Self,FArgs,Res).
*/


% =================================================================
% =================================================================
% =================================================================
%  IMPORT/BIND
% =================================================================
% =================================================================
% =================================================================
nb_bind(Name,Value):- nb_current(Name,Was),same_term(Value,Was),!.
nb_bind(Name,Value):- nb_setval(Name,Value),!.
eval_args30(RetType,Depth,Self,['import!',Other,File],RetVal):-
     (( into_space(Depth,Self,Other,Space),!, include_metta(Space,File),!,return_empty(Space,RetVal))),
     check_returnval(RetType,RetVal). %RetVal=[].
eval_args30(RetType,Depth,Self,['bind!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,eval_args(RetType,Depth,Self,Expr,Value),
    nb_bind(Name,Value),  return_empty(Value,RetVal))),
   check_returnval(RetType,RetVal).
eval_args30(RetType,Depth,Self,['pragma!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,nd_ignore((eval_args(RetType,Depth,Self,Expr,Value),set_option_value(Name,Value))),  return_empty(Value,RetVal),check_returnval(RetType,RetVal))).
eval_args30(RetType,_Dpth,Self,['transfer!',File],RetVal):- !, must_det_ll((include_metta(Self,File),  return_empty(Self,RetVal),check_returnval(RetType,RetVal))).


nd_ignore(Goal):- call(Goal)*->true;true.


eval_args30(RetType,Depth,Self,['+',N1,N2],N):- number(N1),!,
   eval_args0(RetType,Depth,Self,N2,N2Res), catch(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).
eval_args30(RetType,Depth,Self,['-',N1,N2],N):- number(N1),!,
   eval_args0(RetType,Depth,Self,N2,N2Res), catch(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).

eval_args30(RetType,Depth,Self,[V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(V),is_list(VI),!,maplist(eval_args0(RetType,Depth,Self),VI,VO).

eval_args30(RetType,Depth,Self,X,Y):-
  (eval_args40(RetType,Depth,Self,X,Y)*->true;
    (eval_args_failed(Depth,Self,X,Y)*->true;X=Y)).


















% =================================================================
% =================================================================
% =================================================================
%  EVAL FAILED
% =================================================================
% =================================================================
% =================================================================










eval_args_failed(_Dpth,_Slf,T,TT):- T==[],!,TT=[].
eval_args_failed(_Dpth,_Slf,T,TT):- var(T),!,TT=T.
eval_args_failed(_Dpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace([F|LESS]\==Res),!.
%eval_args_failed(Depth,Self,[V|Nil],[O]):- Nil==[], once(eval_args0(RetType,Depth,Self,V,O)),V\=@=O,!.
eval_args_failed(Depth,Self,[H|T],[HH|TT]):- !,
  eval_args0(RetType,Depth,Self,H,HH),
  eval_args_failed(Depth,Self,T,TT).

eval_args_failed(Depth,Self,T,TT):- eval_args0(RetType,Depth,Self,T,TT).

   %eval_args0(RetType,Depth,Self,X,Y):- eval_args20(RetType,Depth,Self,X,Y)*->true;Y=[].

%eval_args30(RetType,Depth,_,_,_):- Depth<1,!,fail.
%eval_args30(RetType,Depth,_,X,Y):- Depth<3, !, ground(X), (Y=X).
%eval_args30(RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.

% Kills zero arity functions eval_args30(RetType,Depth,Self,[X|Nil],[Y]):- Nil ==[],!,eval_args0(RetType,Depth,Self,X,Y).


/*
into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).
eval_args40(RetType,_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.
*/
% Macro Functions
%eval_args30(RetType,Depth,_,_,_):- Depth<1,!,fail.
%eval_args40(RetType,Depth,_,X,Y):- Depth<3, !, fail, ground(X), (Y=X).



% user defined function
eval_args40(RetType,Depth,Self,[H|PredDecl],Res):-
  mnotrace(is_user_defined_head(Self,H)),!,
   eval_args60(RetType,Depth,Self,[H|PredDecl],Res).


eval_args40(RetType,Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   mnotrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   eval_args0(RetType,Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl, subst(PredDecl,SSub,Repl,Temp))),
   Temp\=@=PredDecl,
   eval_args0(RetType,Depth,Self,[F|Temp],Res).

% =================================================================
% =================================================================
% =================================================================
% function inherited by system
% =================================================================
% =================================================================
% =================================================================

% predicate inherited by system
eval_args40(RetType,Depth,Self,[AE|More],TF):-
  length(More,Len),
  is_syspred(AE,Len,Pred),
  mnotrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  adjust_args(Depth,Self,AE,More,Adjusted),!,
  catch_warn(efbug(show_call,as_tf(apply(Pred,Adjusted),TF))),
  check_returnval(RetType,TF).

:- if( \+  current_predicate( adjust_args / 2 )).

is_user_defined_goal(Self,Head):-
  is_user_defined_head(Self,Head).

:- endif.


% function inherited by system
eval_args40(RetType,Depth,Self,[AE|More],TF):-
  length([AE|More],Len),
  is_syspred(AE,Len,Pred),
  mnotrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  adjust_args(Depth,Self,AE,More,Adjusted),!,
  append(Adjusted,[TF],Args),!,
  efbug(show_call,catch_warn(apply(Pred,Args))),
  check_returnval(RetType,TF).

:- if( \+  current_predicate( check_returnval / 2 )).
check_returnval(_RetType,_TF).
:- endif.

:- if( \+  current_predicate( adjust_args / 5 )).
adjust_args(_Depth,_Self,_V,VI,VI).
:- endif.


last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.




catch_warn(G):- notrace(catch(G,E,(wdmsg(catch_warn(G)-->E),fail))).
catch_nowarn(G):- notrace(catch(G,error(_,_),fail)).

as_tf(G,TF):- catch_nowarn((call(G)*->TF='True';TF='False')).
eval_selfless(['==',X,Y],TF):- as_tf(X=:=Y,TF),!.
eval_selfless(['==',X,Y],TF):- as_tf(X=@=Y,TF),!.
eval_selfless(['=',X,Y],TF):-!,as_tf(X=Y,TF).
eval_selfless(['>',X,Y],TF):-!,as_tf(X>Y,TF).
eval_selfless(['<',X,Y],TF):-!,as_tf(X<Y,TF).
eval_selfless(['=>',X,Y],TF):-!,as_tf(X>=Y,TF).
eval_selfless(['<=',X,Y],TF):-!,as_tf(X=<Y,TF).
eval_selfless(['%',X,Y],TF):-!,eval_selfless(['mod',X,Y],TF).

eval_selfless(LIS,Y):-  mnotrace((

   LIS=[F,_,_], atom(F), catch_warn(current_op(_,yfx,F)),
   catch((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.

% less Macro-ey Functions




:- discontiguous eval_args60/4.
%eval_args40(RetType,Depth,Self,PredDecl,Res):-  eval_args60(RetType,Depth,Self,PredDecl,Res).

%eval_args40(RetType,_Dpth,_Slf,L1,Res):- is_list(L1),maplist(self_eval,L1),!,Res=L1.
%eval_args40(RetType,_Depth,_Self,X,X).




% =================================================================
% =================================================================
% =================================================================
%  USER DEFINED FUNCTIONS
% =================================================================
% =================================================================
% =================================================================
eval_args40(RetType,Depth,Self,H,B):-
  eval_args60(RetType,Depth,Self,H,B).


eval_args60(_RetType,_Depth,Self,H,B):-
  metta_defn(Self,H,B).
/*
eval_args60(RetType,Depth,Self,H,B):-
    (eval_args64(RetType,Depth,Self,H,B)*->true;eval_args67(RetType,Depth,Self,H,B)).

eval_args64(RetType,_Dpth,Self,H,B):-
   (metta_defn(Self,H,B);(metta_atom(Self,H),B='True')).
*/

% Has argument that is headed by the same function
eval_args67(RetType,Depth,Self,[H1|Args],Res):-
   mnotrace((append(Left,[[H2|H2Args]|Rest],Args), H2==H1)),!,
   eval_args0(RetType,Depth,Self,[H2|H2Args],ArgRes),
   mnotrace((ArgRes\==[H2|H2Args], append(Left,[ArgRes|Rest],NewArgs))),
   eval_args20(RetType,Depth,Self,[H1|NewArgs],Res).

eval_args67(RetType,Depth,Self,[[H|Start]|T1],Y):-
   mnotrace((is_user_defined_head_f(Self,H),is_list(Start))),
   metta_defn(Self,[H|Start],Left),
   eval_args0(RetType,Depth,Self,[Left|T1],Y).

% Has subterm to eval
eval_args67(RetType,Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   quietly(sub_sterm1(SSub,PredDecl)),
   mnotrace((ground(SSub),SSub=[_|Sub], is_list(Sub),maplist(atomic,SSub))),
   eval_args0(RetType,Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   Temp\=@=PredDecl,
   eval_args20(RetType,Depth,Self,[F|Temp],Res).

%eval_args67(RetType,Depth,Self,X,Y):- (eval_args68(RetType,Depth,Self,X,Y)*->true;metta_atom_iter(Depth,Self,[=,X,Y])).

eval_args67(RetType,Depth,Self,PredDecl,Res):- fail,
 ((term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (eval_args0(RetType,Depth,Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))))),
 PredDecl\=@=Res.

eval_args68(RetType,_Dpth,Self,[H|_],_):- mnotrace( \+ is_user_defined_head_f(Self,H) ), !,fail.
eval_args68(RetType,_Dpth,Self,[H|T1],Y):- metta_defn(Self,[H|T1],Y).
eval_args68(RetType,_Dpth,Self,[H|T1],'True'):- metta_atom(Self,[H|T1]).
eval_args68(RetType,_Dpth,Self,CALL,Y):- fail,append(Left,[Y],CALL),metta_defn(Self,Left,Y).


%eval_args60(RetType,Depth,Self,['ift',CR,Then],RO):- fail, !, %fail, % trace,
%   metta_defn(Self,['ift',R,Then],Become),eval_args0(RetType,Depth,Self,CR,R),eval_args0(RetType,Depth,Self,Then,_True),eval_args0(RetType,Depth,Self,Become,RO).


%not_compound(Term):- \+ is_list(Term),!.
%eval_args40(RetType,Depth,Self,Term,Res):- maplist(not_compound,Term),!,eval_args345(RetType,Depth,Self,Term,Res).


% function inherited by system
eval_args40(RetType,Depth,Self,[F|X],FY):- is_function(F), \+ is_special_op(F), is_list(X),
  maplist(eval_args0(RetType,Depth,Self),X,Y),!,eval_args5(RetType,Depth,Self,[F|Y],FY).
eval_args40(RetType,Depth,Self,FX,FY):- eval_args5(RetType,Depth,Self,FX,FY).

eval_args5(RetType,_Dpth,_Slf,[F|LESS],Res):- once((ground(LESS),eval_selfless([F|LESS],Res),mnotrace(([F|LESS]\==Res)))),!.
eval_args5(RetType,Depth,Self,[AE|More],TF):- length(More,Len),
  (is_syspred(AE,Len,Pred),catch_warn(as_tf(apply(Pred,More),TF)))*->true;eval_args6(RetType,Depth,Self,[AE|More],TF).
eval_args6(RetType,_Dpth,_Slf,[AE|More],TF):- length([AE|More],Len), is_syspred(AE,Len,Pred),append(More,[TF],Args),!,catch_warn(apply(Pred,Args)).

%eval_args40(RetType,Depth,Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval_args0(RetType,Depth,Self,[F2|X2],Y2),eval_args0(RetType,Depth,Self,X1,Y1).

% =================================================================
% =================================================================
% =================================================================
%  AGREGATES
% =================================================================
% =================================================================
% =================================================================

cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
bagof_eval(RetType,Depth,Self,X,L):- !,
   findall(E,(eval_args0(RetType,Depth,Self,X,E), \+ var(E), \+ is_empty(E)),L).
setof_eval(RetType,Depth,Self,X,S):- !,findall(E,eval_args0(RetType,Depth,Self,X,E),L),sort(L,S).
%bagof_eval(RetType,Depth,Self,X,S):- setof(E,eval_args0(RetType,Depth,Self,X,E),S)*->true;S=[].


