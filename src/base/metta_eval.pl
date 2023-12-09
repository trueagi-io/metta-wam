%
% post match modew
%:- style_check(-singleton).

self_eval0(X):- \+ callable(X),!.
self_eval0(X):- is_valid_nb_state(X),!.
%self_eval0(X):- string(X),!.
%self_eval0(X):- number(X),!.
%self_eval0([]).
self_eval0(X):- is_metta_declaration(X),!.
self_eval0(X):- is_list(X),!,fail.
self_eval0(X):- typed_list(X,_,_),!.
%self_eval0(X):- compound(X),!.
%self_eval0(X):- is_ref(X),!,fail.
self_eval0('True'). self_eval0('False'). % self_eval0('F').
self_eval0('Empty').
self_eval0(X):- atom(X),!, \+ nb_current(X,_),!.

self_eval(X):- notrace(self_eval0(X)).

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
:- 'hyde'(quietly/1).
%:- 'hyde'(option_value/2).


is_metta_declaration([F|_]):- F == '->',!.
is_metta_declaration([F,_,_|T]):- T ==[], is_metta_declaration_f(F).

is_metta_declaration_f(F):- F == ':', !.
is_metta_declaration_f(F):- F == '=', !,
   \+ (current_self(Space), is_user_defined_head_f(Space,F)).

(F==':';
  (F=='=',  \+
   \+ (current_self(Space), is_user_defined_head_f(Space,F)))).
% is_metta_declaration([F|T]):- is_list(T), is_user_defined_head([F]),!.

:- nb_setval(self_space, '&self').
evals_to(XX,Y):- Y=@=XX,!.
evals_to(XX,Y):- Y=='True',!, is_True(XX),!.

current_self(Space):- nb_current(self_space,Space).

do_expander('=',_,X,X):-!.
do_expander(':',_,X,Y):- !, get_type(X,Y)*->X=Y.

'get_type'(Arg,Type):- 'get-type'(Arg,Type).




eval_args(X,Y):- current_self(Space),
  rtrace_on_existence_error(eval(100,Space,X,Y)).
eval_args(Depth,Self,X,Y):- locally(set_prolog_flag(gc,false),rtrace_on_existence_error(eval(Depth,Self,X,Y))).
eval_args(Expander,RetType,Depth,Self,X,Y):-
     rtrace_on_existence_error(eval(Expander,RetType,Depth,Self,X,Y)).

%eval(Expander,RetType,Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(eval(Expander,RetType,X)),fail.
eval(Depth,Self,X,Y):- eval('=',_RetType,Depth,Self,X,Y).

%eval(Expander,RetType,_Dpth,_Slf,X,Y):- nonvar(Y),X=Y,!.

eval(Expander,RetType,Depth,Self,X,Y):- nonvar(Y),!,
   get_type(Depth,Self,Y,RetType), !,
   eval(Expander,RetType,Depth,Self,X,XX),evals_to(XX,Y).

eval(_Expander,_RetType,_Dpth,_Slf,X,Y):- var(X),!,Y=X.

eval(Expander,RetType,_Dpth,_Slf,[X|T],Y):- T==[], number(X),!, do_expander(Expander,RetType,X,YY),Y=[YY].

eval(Expander,RetType,Depth,Self,[F|X],Y):-
  (F=='superpose' ; ( option_value(no_repeats,false))),
  notrace((D1 is Depth-1)),!,
  eval_11(Expander,RetType,D1,Self,[F|X],Y).

eval(Expander,RetType,Depth,Self,X,Y):-
  notrace(allow_repeats_eval_(X)),
  !,
  eval_11(Expander,RetType,Depth,Self,X,Y).
eval(Expander,RetType,Depth,Self,X,Y):-
  notrace((no_repeats_var(YY),
  D1 is Depth-1)),
  eval_11(Expander,RetType,D1,Self,X,Y),
   notrace(( \+ (Y\=YY))).

allow_repeats_eval_(_):- !.
allow_repeats_eval_(_):- option_value(no_repeats,false),!.
allow_repeats_eval_(X):- \+ is_list(X),!,fail.
allow_repeats_eval_([F|_]):- atom(F),allow_repeats_eval_f(F).
allow_repeats_eval_f('superpose').
allow_repeats_eval_f('collapse').

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
  setup_call_cleanup(set_debug(Flag,true),call(Goal),set_debug(Flag,false)).

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


eval_99(Expander,RetType,Depth,Self,X,Y):-
  eval_20(Expander,RetType,Depth,Self,X,Y)*->true;eval_failed(Expander,RetType,Depth,Self,X,Y).



eval_00(_Expander,_RetType,Depth,_Slf,X,Y):- Depth<1,!,X=Y, (\+ trace_on_overflow-> true; flag(eval_num,_,0),debug(metta(eval))).
eval_00(_Expander,_RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_00(Expander,RetType,Depth,Self,X,YO):-
  Depth2 is Depth-1,
  copy_term(X, XX),
  eval_15(Expander,RetType,Depth,Self,X,M),
  ((M\=@=XX, \+ self_eval(M))->
      eval_00(Expander,RetType,Depth2,Self,M,Y);Y=M),
  finish_eval(Expander,RetType,Depth2,Self,Y,YO).



%eval_11(Expander,RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_11(Expander,RetType,Depth,Self,X,Y):- \+ is_debugging((eval)),!,
  D1 is Depth-1,
  eval_00(Expander,RetType,D1,Self,X,Y).
eval_11(Expander,RetType,Depth,Self,X,Y):-
 notrace((

  flag(eval_num,EX,EX+1),
  option_else(traclen,Max,100),
  if_trace(eval, (EX>Max->(set_debug(eval,false), %set_debug(overflow,false),
                       write('Switched off tracing. For a longer trace !(pragma! tracelen 101))'));true)),
  nop(notrace(no_repeats_var(YY))),
  notrace(D1 is Depth-1),
  DR is 99-D1,
  if_t(EX<10,if_trace((eval),indentq(Depth,'-->'(EX,eval(Self,X,'$VAR'('RET')),depth(DR))))),
  Ret=retval(fail))),
  call_cleanup((
    eval_00(Expander,RetType,D1,Self,X,Y),
    notrace(( \+ (Y\=YY), nb_setarg(1,Ret,Y)))),
    mnotrace(ignore(((Y\=@=X,if_t(EX<10,if_trace((eval),indentq(Depth,'<--'(EX,Ret))))))))),
  (Ret\=@=retval(fail)->true;(rtrace(eval_00(Expander,RetType,D1,Self,X,Y)),fail)).


eval_15(Expander,RetType,Depth,Self,[Empty|X],Y):-
    Empty=='Empty',!,(X==[]->Y=Empty ; eval_15(Expander,RetType,Depth,Self,X,Y)).
eval_15(Expander,RetType,Depth,Self,X,Y):- !,
  eval_20(Expander,RetType,Depth,Self,X,Y).

eval_15(Expander,RetType,Depth,Self,X,Y):-
  ((eval_20(Expander,RetType,Depth,Self,X,Y),
   if_t(var(Y),dmsg((eval_20(Expander,RetType,Depth,Self,X,Y),var(Y)))),
   nonvar(Y))*->true;(eval_failed(Expander,RetType,Depth,Self,X,Y),fail)).







:- discontiguous eval_20/6.
:- discontiguous eval_40/6.
%:- discontiguous eval_30fz/5.
%:- discontiguous eval_31/5.
%:- discontiguous eval_60/5.

eval_20(Expander,RetType,_Dpth,_Slf,Name,Y):-
    atom(Name), !,
      (nb_current(Name,X)->do_expander(Expander,RetType,X,Y);
       Y = Name).


eval_20(Expander,RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,do_expander(Expander,RetType,X,Y).

% =================================================================
% =================================================================
% =================================================================
%  VAR HEADS/ NON-LISTS
% =================================================================
% =================================================================
% =================================================================

eval_20(Expander,RetType,_Dpth,_Slf,[X|T],Y):- T==[], \+ callable(X),!, do_expander(Expander,RetType,X,YY),Y=[YY].
%eval_20(Expander,RetType,_Dpth,Self,[X|T],Y):- T==[],  atom(X),
%   \+ is_user_defined_head_f(Self,X),
%   do_expander(Expander,RetType,X,YY),!,Y=[YY].

eval_20(Expander,RetType,Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval(Expander,RetType,Depth,Self,VI,VM),
  ( VM\==VI -> eval(Expander,RetType,Depth,Self,[V|VM],VVO) ;
    (eval(Expander,RetType,Depth,Self,V,VV), (V\==VV -> eval(Expander,RetType,Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_20(Expander,RetType,Depthpth,_Slf,X,Y):- \+ is_list(X),!,Y=X.

eval_20(Expander,RetType,Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval(Expander,RetType,Depth,Self),VI,VO).

eval_20(Expander,RetType,Depthpth,_Slf,['repl!'],'True'):- !, repl.
eval_20(Expander,RetType,Depth,Self,['!',Cond],Res):- !, call(eval(Expander,RetType,Depth,Self,Cond,Res)).
eval_20(Expander,RetType,Depth,Self,['rtrace',Cond],Res):- !, rtrace(eval(Expander,RetType,Depth,Self,Cond,Res)).
eval_20(Expander,RetType,Depth,Self,['time',Cond],Res):- !, time(eval(Expander,RetType,Depth,Self,Cond,Res)).
eval_20(Expander,RetType,Depth,Self,['print',Cond],Res):- !, eval(Expander,RetType,Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
% !(println! $1)
eval_20(Expander,RetType,Depth,Self,['println!',Cond],Res):- !, eval(Expander,RetType,Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').

eval_20(Expander,RetType,Depthpth,_Slf,List,Y):- is_list(List),maplist(self_eval,List),List=[H|_], \+ atom(H), !,Y=List.

eval_20(Expander,RetType,Depth,Self,['assertTrue', X],TF):- !, eval(Expander,RetType,Depth,Self,['assertEqual',X,'True'],TF).
eval_20(Expander,RetType,Depth,Self,['assertFalse',X],TF):- !, eval(Expander,RetType,Depth,Self,['assertEqual',X,'False'],TF).

eval_20(Expander,RetType,Depth,Self,['assertEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertEqual',X0,Y0],
        (bagof_eval(Expander,RetType,Depth,Self,X,XX),
         bagof_eval(Expander,RetType,Depth,Self,Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]).

eval_20(Expander,RetType,Depth,Self,['assertNotEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertNotEqual',X0,Y0],
        (setof_eval(Expander,RetType,Depth,Self,X,XX), setof_eval(Expander,RetType,Depth,Self,Y,YY)),
         \+ equal_enough(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,not,YY]).

eval_20(Expander,RetType,Depth,Self,['assertEqualToResult',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertEqualToResult',X0,Y0],
        (bagof_eval(Expander,RetType,Depth,Self,X,XX), =(Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]),!.


loonit_assert_source_tf(Src,Goal,Check,TF):-
   copy_term(Goal,OrigGoal),
   loonit_asserts(Src, time_eval('\n; EVAL TEST\n;',Goal), Check),
   as_tf(Check,TF),!,
  ignore((
          once((TF='True', is_debugging(pass));(TF='False', is_debugging(fail))),
     with_debug((eval),time_eval('Trace',OrigGoal)))).

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




eval_20(Expander,RetType,Depth,Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter(Expander,RetType,Depth,Space,Goal).
eval_20(Expander,RetType,Depth,Self,['match',Other,Goal,Template,Else],Template):-
  (eval_20(Expander,RetType,Depth,Self,['match',Other,Goal,Template],Template)*->true;Template=Else).

% Macro: case
eval_20(Expander,RetType,Depth,Self,X,Res):-
   X= [CaseSym,A,CL],CaseSym == 'case', !,
   into_case_list(CL,CASES),
   findall(Key-Value,
     (nth0(Nth,CASES,Case0),
       (is_case(Key,Case0,Value),
        if_trace((case),(format('~N'),
           writeqln(c(Nth,Key)=Value))))),KVs),!,
   ((eval(Expander,RetType,Depth,Self,A,AA),        if_trace((case),writeqln(switch=AA)),
    (select_case(Expander,RetType,Depth,Self,AA,KVs,Value)->true;(member(Void -Value,KVs),Void=='%void%')))
     *->true;(member(Void -Value,KVs),Void=='%void%')),
    eval(Expander,RetType,Depth,Self,Value,Res).

  select_case(Expander,RetType,Depth,Self,AA,Cases,Value):-
     (best_key(AA,Cases,Value) -> true ;
      (maybe_special_keys(Expander,RetType,Depth,Self,Cases,CasES),
       (best_key(AA,CasES,Value) -> true ;
        (member(Void -Value,CasES),Void=='%void%')))).

  best_key(AA,Cases,Value):-
    ((member(Match-Value,Cases),unify_enough(AA,Match))->true;
     ((member(Match-Value,Cases),AA ==Match)->true;
      ((member(Match-Value,Cases),AA=@=Match)->true;
        (member(Match-Value,Cases),AA = Match)))).

		%into_case_list([[C|ASES0]],CASES):-  is_list(C),!, into_case_list([C|ASES0],CASES),!.
	into_case_list(CASES,CASES):- is_list(CASES),!.
		is_case(AA,[AA,Value],Value):-!.
		is_case(AA,[AA|Value],Value).

   maybe_special_keys(Expander,RetType,Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     eval(Expander,RetType,Depth,Self,K,AK), K\=@=AK,!,
     maybe_special_keys(Expander,RetType,Depth,Self,KVI,KVO).
   maybe_special_keys(Expander,RetType,Depth,Self,[_|KVI],KVO):-
     maybe_special_keys(Expander,RetType,Depth,Self,KVI,KVO).
   maybe_special_keys(_Expander,RetType,Depth,_Self,[],[]).


%[collapse,[1,2,3]]
eval_20(Expander,RetType,Depth,Self,['collapse',List],Res):-!, bagof_eval(Expander,RetType,Depth,Self,List,Res).
%[superpose,[1,2,3]]
eval_20(Expander,RetType,Depth,Self,['superpose',List],Res):- !, member(E,List),eval(Expander,RetType,Depth,Self,E,Res).
get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).
eval_20(Expander,RetType,Depth,Self, Term, Res):-
  mnotrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   eval(Expander,RetType,Depth,Self, Term, Res).


sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- sub_sterm1(Sub,Term).
sub_sterm1(_  ,List):- \+ compound(List),!,fail.
sub_sterm1(Sub,List):- is_list(List),!,member(SL,List),sub_sterm(Sub,SL).
sub_sterm1(_  ,[_|_]):-!,fail.
sub_sterm1(Sub,Term):- arg(_,Term,SL),sub_sterm(Sub,SL).


eval_20(Expander,RetType,Depth,Self, Term, Res):-
   mnotrace(( get_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, setof_eval(Expander,RetType,Depth,Self,List,Var),
   eval(Expander,RetType,Depth,Self, Term, Res).


max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).

eval_20(Expander,RetType,Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval(Expander,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> eval(Expander,RetType,Depth,Self,Then,Res)
     ;  eval(Expander,RetType,Depth,Self,Else,Res)).

eval_20(Expander,RetType,Depth,Self,['If',Cond,Then,Else],Res):- !,
   eval(Expander,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> eval(Expander,RetType,Depth,Self,Then,Res)
     ;  eval(Expander,RetType,Depth,Self,Else,Res)).

eval_20(Expander,RetType,Depth,Self,['If',Cond,Then],Res):- !,
   eval(Expander,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval(Expander,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).

eval_20(Expander,RetType,Depth,Self,['if',Cond,Then],Res):- !,
   eval(Expander,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval(Expander,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).


eval_20(Expander,RetType,Depthpth,_Slf,[_,Nothing],Nothing):- 'Nothing'==Nothing,!.

eval_20(Expander,RetType,Depth,Self,['let',A,A5,AA],OO):- !,
  %(var(A)->true;trace),
  ((eval(Expander,RetType,Depth,Self,A5,AE), AE=A)),
  eval(Expander,RetType,Depth,Self,AA,OO).
%eval_20(Expander,RetType,Depth,Self,['let',A,A5,AA],AAO):- !,eval(Expander,RetType,Depth,Self,A5,A),eval(Expander,RetType,Depth,Self,AA,AAO).
eval_20(Expander,RetType,Depth,Self,['let*',[],Body],RetVal):- !, eval(Expander,RetType,Depth,Self,Body,RetVal).
eval_20(Expander,RetType,Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
    eval_20(Expander,RetType,Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).

eval_20(Expander,RetType,Depth,Self,['colapse'|List], Flat):- !, maplist(eval(Expander,RetType,Depth,Self),List,Res),flatten(Res,Flat).
eval_20(Expander,RetType,Depth,Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter(Expander,RetType,Depth,Space,PredDecl).
eval_20(Expander,RetType,Depthpth,_Slf,['car-atom',Atom],CAR):- !, Atom=[CAR|_],!.
eval_20(Expander,RetType,Depthpth,_Slf,['cdr-atom',Atom],CDR):- !, Atom=[_|CDR],!.

eval_20(Expander,RetType,Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  eval(Expander,RetType,Depth,Self,A,AA), eval(Expander,RetType,Depth,Self,B,BB).

eval_20(Expander,RetType,Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
   eval(Expander,RetType,Depth,Self,A,AA), eval(Expander,RetType,Depth,Self,B,BB).


eval_20(Expander,RetType,Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !, eval(Expander,RetType,Depth,Self,StateExpr,StateMonad),
  eval(Expander,RetType,Depth,Self,UpdatedValue,Value),  'change-state!'(Expander,RetType,Depth,Self,StateMonad, Value, Ret).
eval_20(Expander,RetType,Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  eval(Expander,RetType,Depth,Self,UpdatedValue,Value),  'new-state'(Expander,RetType,Depth,Self,Value,StateMonad).
eval_20(Expander,RetType,Depth,Self,['get-state',StateExpr],Value):- !,
  eval(Expander,RetType,Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value).



% eval_20(Expander,RetType,Depth,Self,['get-state',Expr],Value):- !, eval(Expander,RetType,Depth,Self,Expr,State), arg(1,State,Value).



check_type:- option_else(typecheck,TF,'False'), TF=='True'.

:- dynamic is_registered_state/1.
:- flush_output.
:- setenv('RUST_BACKTRACE',full).

% Function to check if an value is registered as a state name
:- dynamic(is_registered_state/1).
is_nb_state(G):- is_valid_nb_state(G) -> true ;
                 is_registered_state(G),nb_current(G,S),is_valid_nb_state(S).


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
    nb_current(Name, State).

% Register and initialize a new state
init_state(Name) :-
    State = 'State'(_,_),
    asserta(is_registered_state(Name)),
    nb_setval(Name, State).

% Change a value in a state
'change-state!'(Expander,RetType,Depth,Self,StateNameOrInstance, UpdatedValue, Out) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(2, State, Type),
    ( (check_type,\+ get_type(Expander,RetType,Depth,Self,UpdatedValue,Type))
     -> (Out = ['Error', UpdatedValue, 'BadType'])
     ; (nb_setarg(1, State, UpdatedValue), Out = State) ).

% Fetch all values from a state
'get-state'(StateNameOrInstance, Values) :-
    fetch_or_create_state(StateNameOrInstance, State),
    arg(1, State, Values).

'new-state'(Expander,RetType,Depth,Self,Init,'State'(Init, Type)):- check_type->get_type(Expander,RetType,Depth,Self,Init,Type);true.

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


eval_20(Expander,RetType,Depth,Self,['get-type',Val],Type):- !, get_type(Expander,RetType,Depth,Self,Val,Type),ground(Type),Type\==[], Type\==Val,!.


mnotrace(G):- once(G).

is_decl_type(ST):- metta_type(_,_,Type),sub_term(T,Type),T=@=ST, \+ nontype(ST).
is_type(Type):- nontype(Type),!,fail.
is_type(Type):- is_decl_type(Type).
is_type(Type):- atom(Type).

nontype(Type):- var(Type),!.
nontype('->').
nontype(N):- number(N).

needs_eval(EvalMe):- is_list(EvalMe),!.

get_type(Expander,RetType,Depthpth,_Slf,Var,'%Undefined%'):- var(Var),!.
get_type(Expander,RetType,Depthpth,_Slf,Val,'Number'):- number(Val),!.
get_type(Expander,RetType,Depth,Self,Expr,['StateMonad',Type]):- is_valid_nb_state(Expr),'get-state'(Expr,Val),!,
   get_type(Expander,RetType,Depth,Self,Val,Type).


get_type(Expander,RetType,Depth,Self,EvalMe,Type):- needs_eval(EvalMe),eval(Expander,RetType,Depth,Self,EvalMe,Val), \+ needs_eval(Val),!,
   get_type(Expander,RetType,Depth,Self,Val,Type).

get_type(Expander,RetType,Depthpth,Self,[Fn|_],Type):- symbol(Fn),metta_type(Self,Fn,List),last_element(List,Type), nonvar(Type),
   is_type(Type).
get_type(Expander,RetType,Depthpth,Self,List,Type):- is_list(List),metta_type(Self,List,LType),last_element(LType,Type), nonvar(Type),
   is_type(Type).

get_type(Expander,RetType,Depth,_Slf,Type,Type):- Depth<1,!.
get_type(Expander,RetType,Depthpth,Self,List,Type):- is_list(List),metta_type(Self,Type,['->'|List]).
get_type(Expander,RetType,Depth,Self,List,Types):- List\==[], is_list(List),Expander,RetType,Depth2 is Depth-1,maplist(get_type(Expander,RetType,Depth2,Self),List,Types).
get_type(Expander,RetType,Depthpth,Self,Fn,Type):- symbol(Fn),metta_type(Self,Fn,Type),!.
%get_type(Expander,RetType,Depth,Self,Fn,Type):- nonvar(Fn),metta_type(Self,Fn,Type2),Expander,RetType,Depth2 is Depth-1,get_type(Expander,RetType,Depth2,Self,Type2,Type).
%get_type(Expander,RetType,Depth,Self,Fn,Type):- Depth>0,nonvar(Fn),metta_type(Self,Type,Fn),!. %,!,last_element(List,Type).

get_type(Expander,RetType,Depth,Self,Expr,Type):-Expander,RetType,Depth2 is Depth-1, eval(Expander,RetType,Depth2,Self,Expr,Val),Expr\=@=Val,get_type(Expander,RetType,Depth2,Self,Val,Type).


get_type(Expander,RetType,Depthpth,_Slf,Val,'String'):- string(Val),!.
get_type(Expander,RetType,Depthpth,_Slf,Val,Type):- is_decl_type(Val),Type=Val.
get_type(Expander,RetType,Depthpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
get_type(Expander,RetType,Depthpth,_Slf,Val,'Symbol'):- symbol(Val).
%get_type(Expander,RetType,Depth,Self,[T|List],['List',Type]):- Depth2 is Depth-1,  is_list(List),get_type(Expander,RetType,Depth2,Self,T,Type),!,
%  forall((member(Ele,List),nonvar(Ele)),get_type(Expander,RetType,Depth2,Self,Ele,Type)),!.
%get_type(Expander,RetType,Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
get_type(Expander,RetType,Depthpth,_Slf,Cmpd,Type):- \+ ground(Cmpd),!,Type=[].
get_type(Expander,RetType,Depthpth,_Slf,_,'%Undefined%'):- fail.
eval_20(Expander,RetType,Depth,Self,['length',L],Res):- !, eval(Expander,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_20(Expander,RetType,Depth,Self,['CountElement',L],Res):- !, eval(Expander,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).


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



/*
eval_20(Expander,RetType,Depth,Self,[F,A|Args],Res):-
   \+ self_eval(A),
   eval(Expander,RetType,Depth,Self,A,AA),AA\==A,
   eval(Expander,RetType,Depth,Self,[F,AA|Args],Res).


eval_20(Expander,RetType,Depth,Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_eval(A),
   eval(Expander,RetType,Depth,Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), eval(Expander,RetType,Depth,Self,[F,A1|NewArgs],Res)))).
*/

/* %%

% !(assertEqualToResult ((inc) 2) (3))
eval_20(Expander,RetType,Depth,Self,[F|Args],Res):- is_list(F),
  metta_atom_iter(Expander,RetType,Depth,Self,['=',F,R]), eval(Expander,RetType,Depth,Self,[R|Args],Res).

eval_20(Expander,RetType,Depth,Self,[F|Args],Res):- is_list(F), Args\==[],
  append(F,Args,FArgs),!,eval(Expander,RetType,Depth,Self,FArgs,Res).
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
eval_20(Expander,RetType,Depth,Self,['import!',Other,File],RetVal):-
     (( into_space(Depth,Self,Other,Space),!, include_metta(Space,File),!,return_empty(Space,RetVal))),
     check_returnval(Expander,RetType,RetVal). %RetVal=[].
eval_20(Expander,_RtType,_Depth,Self,['bind!',Other,['new-space']],RetVal):- atom(Other),!,assert(was_asserted_space(Other)).
eval_20(Expander,RetType,Depth,Self,['bind!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,eval(Expander,RetType,Depth,Self,Expr,Value),
    nb_bind(Name,Value),  return_empty(Value,RetVal))),
   check_returnval(Expander,RetType,RetVal).
eval_20(Expander,RetType,Depth,Self,['pragma!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,nd_ignore((eval(Expander,RetType,Depth,Self,Expr,Value),
   set_option_value(Name,Value))),  return_empty(Value,RetVal),
   check_returnval(Expander,RetType,RetVal))).
eval_20(Expander,RetType,_Dpth,Self,['transfer!',File],RetVal):- !, must_det_ll((include_metta(Self,File),  return_empty(Self,RetVal),check_returnval(Expander,RetType,RetVal))).


nd_ignore(Goal):- call(Goal)*->true;true.

eval_20(Expander,RetType,Depth,Self,['nop',Expr],Empty):- !,  eval(Expander,RetType,Depth,Self,Expr,_), return_empty([],Empty).

is_True(T):- T\=='False',T\=='F',T\==[].

is_and(S):- \+ atom(S),!,fail.
is_and('#COMMA'). is_and(','). is_and('and'). is_and('And').

eval_20(Expander,RetType,Depthpth,_Slf,[And],'True'):- is_and(And),!.
eval_20(Expander,RetType,Depth,Self,['and',X,Y],TF):- !, as_tf((eval(Expander,RetType,Depth,Self,X,'True'),eval(Expander,RetType,Depth,Self,Y,'True')),TF).
eval_20(Expander,RetType,Depth,Self,[And,X|Y],TF):- is_and(And),!,eval(Expander,RetType,Depth,Self,X,TF1),
  is_True(TF1),eval_20(Expander,RetType,Depth,Self,[And|Y],TF).
%eval_2(Expander,RetType,Depth,Self,[H|T],_):- \+ is_list(T),!,fail.
eval_20(Expander,RetType,Depth,Self,['or',X,Y],TF):- !, as_tf((eval(Expander,RetType,Depth,Self,X,'True');eval(Expander,RetType,Depth,Self,Y,'True')),TF).



eval_20(Expander,RetType,Depthpth,Self,['add-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF).
eval_20(Expander,RetType,Depthpth,Self,['remove-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF).
eval_20(Expander,RetType,Depthpth,Self,['atom-count',Other],Count):- !, into_space(Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2.
eval_20(Expander,RetType,Depthpth,Self,['atom-replace',Other,Rem,Add],TF):- !, into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF).


eval_20(Expander,RetType,Depth,Self,['+',N1,N2],N):- number(N1),!,
   eval(Expander,RetType,Depth,Self,N2,N2Res), catch(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).
eval_20(Expander,RetType,Depth,Self,['-',N1,N2],N):- number(N1),!,
   eval(Expander,RetType,Depth,Self,N2,N2Res), catch(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).

eval_20(Expander,RetType,Depth,Self,[V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(V),is_list(VI),!,maplist(eval(Expander,RetType,Depth,Self),VI,VO).

eval_20(Expander,RetType,Depth,Self,X,Y):-
  (eval_2(Expander,RetType,Depth,Self,X,Y)*->true;
    (finish_eval(Expander,RetType,Depth,Self,X,Y)*->true;X=Y)).


finish_eval(Expander,RetType,Depthpth,_Slf,T,TT):- T==[],!,TT=[].
finish_eval(Expander,RetType,Depthpth,_Slf,T,TT):- var(T),!,TT=T.
finish_eval(Expander,RetType,Depthpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace([F|LESS]\==Res),!.
%finish_eval(Expander,RetType,Depth,Self,[V|Nil],[O]):- Nil==[], once(eval(Expander,RetType,Depth,Self,V,O)),V\=@=O,!.
finish_eval(Expander,RetType,Depth,Self,[H|T],[HH|TT]):- !,
  eval(Expander,RetType,Depth,Self,H,HH),
  finish_eval(Expander,RetType,Depth,Self,T,TT).

finish_eval(Expander,RetType,Depth,Self,T,TT):- eval(Expander,RetType,Depth,Self,T,TT).

   %eval(Expander,RetType,Depth,Self,X,Y):- eval_20(Expander,RetType,Depth,Self,X,Y)*->true;Y=[].

%eval_20(Expander,RetType,Depth,_,_,_):- Depth<1,!,fail.
%eval_20(Expander,RetType,Depth,_,X,Y):-  Depth<3, !, ground(X), (Y=X).
%eval_20(Expander,RetType,Depthpth,_Slf,X,Y):- self_eval(X),!,Y=X.

% Kills zero arity functions eval_20(Expander,RetType,Depth,Self,[X|Nil],[Y]):- Nil ==[],!,eval(Expander,RetType,Depth,Self,X,Y).


/*
into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).
eval_2(Expander,RetType,Depthpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.
*/
% Macro Functions
%eval_20(Expander,RetType,Depth,_,_,_):- Depth<1,!,fail.
eval_2(Expander,RetType,Depth,_,X,Y):- Depth<3, !, fail, ground(X), (Y=X).
eval_2(Expander,RetType,Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   mnotrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   eval(Expander,RetType,Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   eval(Expander,RetType,Depth,Self,[F|Temp],Res).


   
% user defined function
eval_2(Expander,RetType,Depth,Self,[H|PredDecl],Res):- mnotrace(is_user_defined_head(Self,H)),!,
   eval_30(Expander,RetType,Depth,Self,[H|PredDecl],Res).

% function inherited by system
eval_2(Expander,RetType,Depth,Self,PredDecl,Res):- eval_40(Expander,RetType,Depth,Self,PredDecl,Res).



:- if( \+  current_predicate( check_returnval / 3 )).
check_returnval(_,_RetType,_TF).
:- endif.

:- if( \+  current_predicate( check_returnval / 2 )).
check_returnval(_Re,_TF).

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

eval_selfless(LIS,Y):-  notrace((
   LIS=[F,_,_], atom(F), catch_warn(current_op(_,yfx,F)),
   catch((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.

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
:- discontiguous eval_3/4.
%eval_2(Expander,RetType,Depth,Self,PredDecl,Res):- eval_3(Expander,RetType,Depth,Self,PredDecl,Res).

%eval_2(Expander,RetType,Depthpth,_Slf,L1,Res):- is_list(L1),maplist(self_eval,L1),!,Res=L1.
%eval_2(_Expander,RetType,Depth,_Self,X,X).


is_user_defined_head(Other,H):- mnotrace(is_user_defined_head0(Other,H)).
is_user_defined_head0(Other,[H|_]):- !, nonvar(H),!, is_user_defined_head_f(Other,H).
is_user_defined_head0(Other,H):- callable(H),!,functor(H,F,_), is_user_defined_head_f(Other,F).
is_user_defined_head0(Other,H):- is_user_defined_head_f(Other,H).

is_user_defined_head_f(Other,H):- is_user_defined_head_f1(Other,H).
is_user_defined_head_f(Other,H):- is_user_defined_head_f1(Other,[H|_]).

%is_user_defined_head_f1(Other,H):- metta_type(Other,H,_).
is_user_defined_head_f1(Other,H):- metta_atom(Other,[H|_]).
is_user_defined_head_f1(Other,H):- metta_defn(Other,[H|_],_).
%is_user_defined_head_f(_,H):- is_metta_builtin(H).


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



eval_30(Expander,RetType,Depth,Self,H,B):-  (eval_34(Expander,RetType,Depth,Self,H,B)*->true;eval_37(Expander,RetType,Depth,Self,H,B)).

eval_34(Expander,RetType,Depthpth,Self,H,B):-  (metta_defn(Self,H,B);(metta_atom(Self,H),B='True')).

% Has argument that is headed by the same function
eval_37(Expander,RetType,Depth,Self,[H1|Args],Res):-
   mnotrace((append(Left,[[H2|H2Args]|Rest],Args), H2==H1)),!,
   eval(Expander,RetType,Depth,Self,[H2|H2Args],ArgRes),
   mnotrace((ArgRes\==[H2|H2Args], append(Left,[ArgRes|Rest],NewArgs))),
   eval_30(Expander,RetType,Depth,Self,[H1|NewArgs],Res).

eval_37(Expander,RetType,Depth,Self,[[H|Start]|T1],Y):-
   mnotrace((is_user_defined_head_f(Self,H),is_list(Start))),
   metta_defn(Self,[H|Start],Left),
   eval(Expander,RetType,Depth,Self,[Left|T1],Y).

% Has subterm to eval
eval_37(Expander,RetType,Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   quietly(sub_sterm1(SSub,PredDecl)),
   mnotrace((ground(SSub),SSub=[_|Sub], is_list(Sub),maplist(atomic,SSub))),
   eval(Expander,RetType,Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   eval_30(Expander,RetType,Depth,Self,[F|Temp],Res).

%eval_37(Expander,RetType,Depth,Self,X,Y):- (eval_38(Expander,RetType,Depth,Self,X,Y)*->true;metta_atom_iter(Expander,RetType,Depth,Self,[=,X,Y])).

eval_37(Expander,RetType,Depth,Self,PredDecl,Res):- fail,
 ((term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (eval(Expander,RetType,Depth,Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))))),
 PredDecl\=@=Res.

eval_38(Expander,RetType,Depthpth,Self,[H|_],_):- mnotrace( \+ is_user_defined_head_f(Self,H) ), !,fail.
eval_38(Expander,RetType,Depthpth,Self,[H|T1],Y):- metta_defn(Self,[H|T1],Y).
eval_38(Expander,RetType,Depthpth,Self,[H|T1],'True'):- metta_atom(Self,[H|T1]).
eval_38(Expander,RetType,Depthpth,Self,CALL,Y):- fail,append(Left,[Y],CALL),metta_defn(Self,Left,Y).


%eval_3(Expander,RetType,Depth,Self,['ift',CR,Then],RO):- fail, !, %fail, % trace,
%   metta_defn(Self,['ift',R,Then],Become),eval(Expander,RetType,Depth,Self,CR,R),eval(Expander,RetType,Depth,Self,Then,_True),eval(Expander,RetType,Depth,Self,Become,RO).

metta_atom_iter(Expander,RetType,Depthpth,Other,[Equal,H,B]):- '=' == Equal,!,
  (metta_defn(Other,H,B)*->true;(metta_atom(Other,H),B='True')).

metta_atom_iter(Expander,RetType,Depth,_,_):- Depth<3,!,fail.
metta_atom_iter(Expander,RetType,Depthpth,_Slf,[]):-!.
metta_atom_iter(Expander,RetType,Depthpth,Other,H):- metta_atom(Other,H).
metta_atom_iter(Expander,RetType,Depth,Other,H):- D2 is Depth -1, metta_defn(Other,H,B),metta_atom_iter(D2,Other,B).
metta_atom_iter(Expander,RetType,Depthpth,_Slf,[And]):- is_and(And),!.
metta_atom_iter(Expander,RetType,Depth,Self,[And,X|Y]):- is_and(And),!,D2 is Depth -1, metta_atom_iter(D2,Self,X),metta_atom_iter(D2,Self,[And|Y]).
/*
metta_atom_iter2(_,Self,[=,X,Y]):- metta_defn(Self,X,Y).
metta_atom_iter2(Expander,RetType,Depthpth,Other,[Equal,H,B]):- '=' == Equal,!, metta_defn(Other,H,B).
metta_atom_iter2(Expander,RetType,Depthpth,Self,X,Y):- metta_defn(Self,X,Y). %, Y\=='True'.
metta_atom_iter2(Expander,RetType,Depthpth,Self,X,Y):- metta_atom(Self,[=,X,Y]). %, Y\=='True'.

*/
metta_atom_iter_ref(Other,['=',H,B],Ref):-clause(metta_defn(Other,H,B),true,Ref).
metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom(Other,H),true,Ref).

%not_compound(Term):- \+ is_list(Term),!.
%eval_2(Expander,RetType,Depth,Self,Term,Res):- maplist(not_compound,Term),!,eval_345(Expander,RetType,Depth,Self,Term,Res).


% function inherited by system
eval_40(Expander,RetType,Depth,Self,[F|X],FY):- is_function(F), \+ is_special_op(F), is_list(X),
  maplist(eval(Expander,RetType,Depth,Self),X,Y),!,eval_5(Expander,RetType,Depth,Self,[F|Y],FY).
eval_40(Expander,RetType,Depth,Self,FX,FY):- eval_5(Expander,RetType,Depth,Self,FX,FY).

eval_5(Expander,RetType,Depthpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace(([F|LESS]\==Res)),!.
eval_5(Expander,RetType,Depth,Self,[AE|More],TF):- length(More,Len),
  (is_syspred(AE,Len,Pred),catch_warn(as_tf(apply(Pred,More),TF)))*->true;eval_6(Expander,RetType,Depth,Self,[AE|More],TF).
eval_6(Expander,RetType,Depthpth,_Slf,[AE|More],TF):- length([AE|More],Len), is_syspred(AE,Len,Pred),append(More,[TF],Args),!,catch_warn(apply(Pred,Args)).

%eval_40(Expander,RetType,Depth,Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval(Expander,RetType,Depth,Self,[F2|X2],Y2),eval(Expander,RetType,Depth,Self,X1,Y1).


cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
bagof_eval(Expander,RetType,Depth,Self,X,L):- !,findall(E,eval(Expander,RetType,Depth,Self,X,E),L).
setof_eval(Expander,RetType,Depth,Self,X,S):- !,findall(E,eval(Expander,RetType,Depth,Self,X,E),L),sort(L,S).
%setof_eval(Expander,RetType,Depth,Self,X,S):- setof(E,eval(Expander,RetType,Depth,Self,X,E),S)*->true;S=[].

