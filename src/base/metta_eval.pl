%self_eval(X):- var(X),!.
%self_eval(X):- string(X),!.
%self_eval(X):- number(X),!.
%self_eval([]).
self_eval(X):- \+ callable(X),!.
self_eval(X):- is_metta_declaration(X),!.
self_eval(X):- is_valid_nb_state(X),!.
self_eval(X):- is_list(X),!,fail.
%self_eval(X):- compound(X),!.
%self_eval(X):- is_ref(X),!,fail.
self_eval(X):- atom(X),!, \+ nb_current(X,_),!.
self_eval('True'). self_eval('False'). self_eval('F').

is_metta_declaration([F,_,_|T]):- T ==[], (F=='=';F==':'),!.
% is_metta_declaration([F|T]):- is_list(T), is_user_defined_head([F]),!.

:- nb_setval(self_space, '&self').
evals_to(XX,Y):- Y==XX,!.
evals_to(XX,Y):- Y=='True',!, is_True(XX),!.

current_self(Space):- nb_current(self_space,Space).
eval_args(A,AA):-
  current_self(Space),
  eval_args(11,Space,A,AA).

%eval_args(Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(eval_args(X)),fail.

eval_args(_Dpth,_Slf,X,Y):- nonvar(Y),X=Y,!.
eval_args(_Dpth,_Slf,X,Y):- var(X),!,X=Y.
eval_args(Depth,Self,X,Y):- nonvar(Y),!,eval_args(Depth,Self,X,XX),evals_to(XX,Y).
eval_args(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args(_Dpth,_Slf,[X|T],Y):- T==[], \+ callable(X),!,Y=[X].

eval_args(Depth,Self,X,Y):-
  allow_repeats_eval(X),
  !,
  eval_args0000(Depth,Self,X,Y).
eval_args(Depth,Self,X,Y):-
  notrace((no_repeats_var(YY),
  D1 is Depth-1)),
  eval_args0000(D1,Self,X,Y),
   notrace(( \+ (Y\=YY))).

allow_repeats_eval(_):- option_value(no_repeats,false),!.
allow_repeats_eval(X):- \+ is_list(X),!,fail.
allow_repeats_eval([F|_]):- atom(F),allow_repeats_eval_f(F).
allow_repeats_eval_f('superpose').
allow_repeats_eval_f('collapse').



debugging_metta(G):-is_debugging((eval))->ignore(G);true.


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
if_trace((Flag;true),Goal):- !, catch(ignore((Goal)),E,wdmsg(E-->if_trace((Flag;true),Goal))).
if_trace(Flag,Goal):- catch(ignore((is_debugging(Flag),Goal)),E,wdmsg(E-->if_trace(Flag,Goal))).

is_debugging(Flag):- var(Flag),!,fail.
is_debugging(Flag):- debugging(metta(Flag),TF),!,TF==true.
is_debugging(Flag):- debugging(Flag,TF),!,TF==true.
is_debugging(Flag):- flag_to_var(Flag,Var),
   (option_value(Var,true)->true;(Flag\==Var -> is_debugging(Var))).

:- nodebug(metta(overflow)).

eval_args0000(Depth,_Slf,X,Y):- Depth<1,!,X=Y, (\+ is_debugging(overflow)-> true; flag(eval_num,_,0),set_debug((eval),true)).
%eval_args0000(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args0000(Depth,Self,X,Y):-
  copy_term(X,XX),
  Depth2 is Depth-1,
  call_nth(eval_args11(Depth,Self,X,M),Nth),
  (is_bool_or_same(XX,M)
          -> (!, (Nth=1->Y=M;fail) )
            ;  eval_args0000(Depth2,Self,M,Y)),
  nop(nonvar(Y)).

is_bool_or_same( X,M):- X=@=M,!.
%is_bool_or_same( _,_):- !, fail.
is_bool_or_same( X,_):- \+ ground(X),!,fail.
is_bool_or_same(_X,M):- \+ atomic(M),!,fail.
is_bool_or_same( X,'True'):- !, ground(X).
is_bool_or_same(_X,M):- is_bool_or(M).
% if `True` is not commented, we fail two tests in examples/compat/test_scripts/b4_nondeterm.metta
%is_bool_or('True').
is_bool_or('False').
is_bool_or([]).



%eval_args11(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
%eval_args11(Depth,Self,X,Y):- \+ debugging(metta(eval)),!, eval_args20(Depth,Self,X,Y).

eval_args11(Depth,Self,X,Y):- flag(eval_num,EX,EX+1),
  option_else(traclen,Max,100),
  (EX>Max->(set_debug(eval,false),set_debug(exec,false),set_debug(overflow,false),
    skip(write('Switched off tracing. For a longer trace !(pragma! tracelen 101))')));true),
  notrace((D1 is Depth-1)),
  DR is 99-D1,
  if_trace((eval;true),indentq(Depth,'-->'(EX,Self,X,depth(DR)))),
  Ret=retval(fail),
  call_cleanup(
   (eval_args20(D1,Self,X,Y),
    notrace(( nb_setarg(1,Ret,Y)))),
   notrace(ignore(((Y\=@=X,flag(eval_num,_,EX),if_trace((eval;true),indentq(Depth,'<--'(EX,Ret)))))))),
  (Ret\=@=retval(fail)->true;(rtrace(eval_args20(D1,Self,X,Y)),fail)).


%:- discontiguous eval_args20/4.
:- discontiguous eval_args30/4.
:- discontiguous eval_args40/4.
% eval_args20 is "1 step"
eval_args20(_Dpth,_Slf,Name,Value):- atom(Name), !, (nb_current(Name,Value) -> true ; Value=Name).
eval_args20(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.

eval_args20(Depth,_,_,_):- Depth<1,!,fail.
eval_args20(Depth,_,X,Y):- Depth<3, !, ground(X), (Y=X).

eval_args20(Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval_args(Depth,Self,VI,VM),
  ( VM\==VI -> eval_args(Depth,Self,[V|VM],VVO) ;
    (eval_args(Depth,Self,V,VV), (V\==VV -> eval_args(Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_args20(_Dpth,_Slf,X,Y):- \+ is_list(X),!,Y=X.
eval_args20(_Dpth,_Slf,List,Y):- List=[H|_], \+ atom(H), maplist(self_eval,List), !,Y=List.
eval_args20(Depth,Self,[V|VI],[V|VO]):- var(V),!,maplist(eval_args(Depth,Self),VI,VO).
eval_args20(_Dpth,_Slf,[V,H,B|T],[V,H,B|T]):- T==[], V=='=',!.


/*
eval_args20(Depth,Self,[F,A|Args],Res):-
   \+ self_eval(A),
   eval_args(Depth,Self,A,AA),AA\==A,
   eval_args(Depth,Self,[F,AA|Args],Res).


eval_args20(Depth,Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_eval(A),
   eval_args(Depth,Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), eval_args(Depth,Self,[F,A1|NewArgs],Res)))).
*/

/* %%

% !(assertEqualToResult ((inc) 2) (3))
eval_args20(Depth,Self,[F|Args],Res):- is_list(F),
  metta_atom_iter(Depth,Self,['=',F,R]), eval_args(Depth,Self,[R|Args],Res).

eval_args20(Depth,Self,[F|Args],Res):- is_list(F), Args\==[],
  append(F,Args,FArgs),!,eval_args(Depth,Self,FArgs,Res).
*/

eval_args20(Depth,Self,X,Y):- eval_args30(Depth,Self,X,Y).

eval_args30(_Dpth,_Slf,['repl!'],'True'):- !, repl.
eval_args30(Depth,Self,['!',Cond],Res):- !, call(eval_args(Depth,Self,Cond,Res)).
eval_args30(Depth,Self,['rtrace',Cond],Res):- !, rtrace(eval_args(Depth,Self,Cond,Res)).
eval_args30(Depth,Self,['trace',Cond],Res):- !, with_debug(eval,eval_args(Depth,Self,Cond,Res)).
eval_args30(Depth,Self,['time',Cond],Res):- !, time_eval(eval(Cond),eval_args(Depth,Self,Cond,Res)).
eval_args30(Depth,Self,['print',Cond],Res):- !, eval_args(Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
% !(println! $1)
eval_args30(Depth,Self,['println!'|Cond],Res):- !, maplist(eval_args(Depth,Self),Cond,[Res|Out]),
   format('~N'),maplist(write_src,[Res|Out]),format('~N').
eval_args30(Depth,Self,['trace!',A|Cond],Res):- !, maplist(eval_args(Depth,Self),[A|Cond],[AA|Result]),
   last(Result,Res), format('~N'),maplist(write_src,[AA]),format('~N').

%eval_args30(Depth,Self,['trace!',A,B],C):- !,eval_args(Depth,Self,B,C),format('~N'),wdmsg(['trace!',A,B]=C),format('~N').
%eval_args30(_Dpth,_Slf,['trace!',A],A):- !, format('~N'),wdmsg(A),format('~N').

eval_args30(Depth,Self,['assertTrue', X],TF):- !, eval_args(Depth,Self,['assertEqual',X,'True'],TF).
eval_args30(Depth,Self,['assertFalse',X],TF):- !, eval_args(Depth,Self,['assertEqual',X,'False'],TF).

eval_args30(Depth,Self,['assertEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertEqual',X0,Y0],
        (bagof_eval(Depth,Self,X,XX),
         bagof_eval(Depth,Self,Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]).

eval_args30(Depth,Self,['assertNotEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertNotEqual',X0,Y0],
        (bagof_eval(Depth,Self,X,XX), bagof_eval(Depth,Self,Y,YY)),
         \+ equal_enough(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,not,YY]).

eval_args30(Depth,Self,['assertEqualToResult',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertEqualToResult',X0,Y0],
        (bagof_eval(Depth,Self,X,XX), =(Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]),!.


loonit_assert_source_tf(Src,Goal,Check,TF):-
   copy_term(Goal,OrigGoal),
   color_g_mesg('#114411',(write_src_nl(Src),writeq(Src))),
   loonit_asserts(Src, time_eval('\n; EVAL TEST\n;',Goal), Check),
   as_tf(Check,TF),!,
  ignore((
          once((TF=='True', is_debugging(pass));(TF=='False', is_debugging(fail))),
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


eval_args30(Depth,Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter(Depth,Space,Goal).
eval_args30(Depth,Self,['match',Other,Goal,Template,Else],Template):-
  (eval_args30(Depth,Self,['match',Other,Goal,Template],Template)*->true;Template=Else).

% Macro: case
eval_args30(Depth,Self,['case',A,CL],Res):-
   into_case_list(CL,CASES),
   findall(Key-Value,
     (nth0(Nth,CASES,Case0),
       (is_case(Key,Case0,Value),
        if_trace((case),(format('~N'),
           writeqln(c(Nth,Key)=Value))))),KVs),!,
   ((eval_args(Depth,Self,A,AA),        if_trace((case),writeqln(switch=AA)),
    (select_case(Depth,Self,AA,KVs,Value)->true;(member(Void -Value,KVs),Void=='%void%')))
     *->true;(member(Void -Value,KVs),Void=='%void%')),
    eval_args(Depth,Self,Value,Res).

  select_case(Depth,Self,AA,Cases,Value):-
     (best_key(AA,Cases,Value) -> true ;
      (maybe_special_keys(Depth,Self,Cases,CasES),
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

   maybe_special_keys(Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     eval_args(Depth,Self,K,AK), K\=@=AK,!,
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(Depth,Self,[_|KVI],KVO):-
     maybe_special_keys(Depth,Self,KVI,KVO).
   maybe_special_keys(_Depth,_Self,[],[]).

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

eval_args30(_Dpth,_Slf,['car-atom',Atom],CAR):- !, Atom=[CAR|_],!.
eval_args30(_Dpth,_Slf,['cdr-atom',Atom],CDR):- !, Atom=[_|CDR],!.
eval_args30(Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  eval_args(Depth,Self,A,AA), eval_args(Depth,Self,B,BB).
eval_args30(Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
   eval_args(Depth,Self,A,AA), eval_args(Depth,Self,B,BB).

eval_until_eq(Depth,Self,X,Y):- var(Y),!,eval_args(Depth,Self,X,Y).
eval_until_eq(Depth,Self,Y,X):- var(Y),!,eval_args(Depth,Self,X,Y).
eval_until_eq(Depth,Self,Y,X):- eval_until_eq1(Depth,Self,Y,X).
eval_until_eq1(_Dpth,_Slf,X,Y):- X=Y,!.
eval_until_eq1(Depth,Self,X,Y):- eval_args20(Depth,Self,X,XX),X\=@=XX,!,eval_until_eq1(Depth,Self,Y,XX).


%eval_args30(Depth,Self,['eq',X,Y],Res):- !, as_tf(eval_until_eq(Depth,Self,X,Y),Res).
eval_args30(_Dpth,_Slf,['memb',E,List],Res):- !, into_pl_list(List,PLList),as_tf(member(E,PLList), Res).
eval_args30(_Dpth,_Slf,['make_list',List],MettaList):- !, into_metta_cons(List,MettaList).

%[collapse,[1,2,3]]
eval_args30(Depth,Self,['collapse',List],Res):-!, bagof_eval(Depth,Self,List,Res).
%[superpose,[1,2,3]]
eval_args30(Depth,Self,['superpose',List],Res):- !, member(E,List),
  eval_args(Depth,Self,E,Res).
get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).

eval_args30(Depth,Self, Term, Res):- fail,
  mnotrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,30),
   member(Var,List),
   eval_args(Depth,Self, Term, Res).

eval_args30(Depth,Self, Term, Res):- fail,
   mnotrace(( get_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, setof_eval(Depth,Self,List,Var),
   eval_args(Depth,Self, Term, Res).


max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).


eval_args30(Depth,Self,['If',Cond,Then],Res):- is_user_defined_head_f(Self,'If'), !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res) ; Res = []).

eval_args30(Depth,Self,['If',Cond,Then,Else],Res):- is_user_defined_head_f(Self,'If'), !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res);eval_args(Depth,Self,Else,Res)).

eval_args30(Depth,Self,['if',Cond,Then],Res):- !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res) ; Res = []).

eval_args30(Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res);eval_args(Depth,Self,Else,Res)).

% @TODO: This should not be here
eval_args30(_Dpth,_Slf,['ift',Cond,Then],Then):- Cond == 'True', !.

eval_args30(_Dpth,_Slf,[_,Nothing],Nothing):- 'Nothing'==Nothing,!.

eval_args30(Depth,Self,['let',A,A5,AA],OO):- !,
  %(var(A)->true;trace),
  ((eval_args(Depth,Self,A5,AE), AE=A)),
  eval_args(Depth,Self,AA,OO).
%eval_args30(Depth,Self,['let',A,A5,AA],AAO):- !,eval_args(Depth,Self,A5,A),eval_args(Depth,Self,AA,AAO).
eval_args30(Depth,Self,['let*',[],Body],RetVal):- !, eval_args(Depth,Self,Body,RetVal).
eval_args30(Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
    eval_args30(Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).

%eval_args30(Depth,Self,['colapse'|List], Flat):- !, maplist(eval_args(Depth,Self),List,Res),flatten(Res,Flat).
eval_args30(Depth,Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter(Depth,Space,PredDecl).


eval_args30(Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !, eval_args(Depth,Self,StateExpr,StateMonad),
  eval_args(Depth,Self,UpdatedValue,Value),  'change-state!'(Depth,Self,StateMonad, Value, Ret).
eval_args30(Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  eval_args(Depth,Self,UpdatedValue,Value),  'new-state'(Depth,Self,Value,StateMonad).
eval_args30(Depth,Self,['get-state',StateExpr],Value):- !,
  eval_args(Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value).


eval_args30(Depth,Self,[V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(V),
  is_list(VI),!,maplist(eval_args(Depth,Self),VI,VO).

adjust_args(_Dpth,Self,F,X,X):- (is_special_op(Self,F); \+ iz_conz(X)),!.
adjust_args(Depth,Self,Op,X,Y):-
  get_operator_typedef(Self,Op,Params,_RetType),
  as_prolog(Depth,Self,X,M),
  args_conform(Depth,Self,M,Params),!,
  into_typed_args(Depth,Self,Params,M,Y).
adjust_args(Depth,Self,F,X,Y):- is_list(X), is_function(F), !, maplist(eval_args(Depth,Self),X,Y).
adjust_args(Depth,Self,_,X,Y):- is_list(X), !, maplist(as_prolog(Depth,Self),X,Y),!.
adjust_args(Depth,Self,_,X,Y):- as_prolog(Depth,Self,X,Y).

into_typed_args(_Dpth,_Slf,T,M,Y):- (\+ iz_conz(T); \+ iz_conz(M)),!, M=Y.
into_typed_args(Depth,Self,[T|TT],[M|MM],[Y|YY]):-
  into_typed_arg(Depth,Self,T,M,Y),
  into_typed_args(Depth,Self,TT,MM,YY).

into_typed_arg(_Dpth,_Slf,T,M,Y):- var(M),!, add_argtype(T,M),Y=M.
into_typed_arg(Depth,Self,T,M,Y):- into_typed_arg0(Depth,Self,T,M,Y)*->true;M=Y.

into_typed_arg0(Depth,Self,T,M,Y):- var(T), !, get_type(Depth,Self,M,T),
 (wants_eval_kind(T)->eval_args(Depth,Self,M,Y);Y=M).

into_typed_arg0(Depth,Self,T,M,Y):- is_pro_eval_kind(T),!,eval_args(Depth,Self,M,Y).
into_typed_arg0(Depth,Self,T,M,Y):- ground(M),!, \+ arg_violation(Depth,Self,M,T),Y=M.
into_typed_arg0(_Dpth,_Slf,T,M,Y):- is_non_eval_kind(T),!,M=Y.
into_typed_arg0(Depth,Self,_,M,Y):- eval_args(Depth,Self,M,Y).
add_argtype(_,_).

is_non_eval_kind(Type):- is_nonspecific_type(Type),!.
is_non_eval_kind('Atom').

is_pro_eval_kind('Number').
is_pro_eval_kind('Symbol').
is_pro_eval_kind('Bool').


as_prolog(_Dpth,_Slf,I,O):- \+ iz_conz(I),!,I=O.
as_prolog(Depth,Self,[H|T],O):- H=='::',!,as_prolog(Depth,Self,T,O).
as_prolog(Depth,Self,[H|T],[HH|TT]):- as_prolog(Depth,Self,H,HH),as_prolog(Depth,Self,T,TT).



% eval_args30(Depth,Self,['get-state',Expr],Value):- !, eval_args(Depth,Self,Expr,State), arg(1,State,Value).



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

eval_args30(Depth,Self,['length',L],Res):- !, eval_args(Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_args30(Depth,Self,['CountElement',L],Res):- !, eval_args(Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_args30(Depth,Self,['get-type',Val],Type):- !,
   get_type12(Depth,Self,Val,Type).
   
   
   
%get_type12(Depth,Self,Val,Type):- 
%  get_type1(Depth,Self,Val,Type), ground(Type),Type\==[], Type\==Val,!.
%get_type12(Depth,Self,Val,Type):- 
%  get_type2(Depth,Self,Val,Type), ground(Type),Type\==[], Type\==Val,!.
get_type12(Depth,Self,Val,Type):- 
  get_type(Depth,Self,Val,Type),nonvar(Type),!.
get_type12(_Dpth,_Slf,_Vl,[]).

mnotrace(G):- once(G).

is_decl_type(ST):- metta_type(_,_,Type),sub_sterm(T,Type),T=@=ST, \+ nontype(ST).
is_decl_type([ST|_]):- !, atom(ST),is_decl_type_l(ST).
is_decl_type(ST):- \+ atom(ST),!,fail.
is_decl_type('%Undefined%').  is_decl_type('Number').
is_decl_type('String').       is_decl_type('Bool').
is_decl_type('Type').         is_decl_type('Symbol').
is_decl_type('Any').          is_decl_type('Atom').
is_decl_type(Type):-          is_decl_type_l(Type).
is_decl_type_l('StateMonad'). is_decl_type_l('List').


last_type(List,Type):- is_list(List),last(List,Type),is_type(Type).
last_type(Type,Type):- is_type(Type),!.

is_type(Type):- nontype(Type),!,fail.
is_type(Type):- is_decl_type(Type).
is_type(Type):- atom(Type).

nontype(Type):- var(Type),!.
nontype('->').
nontype(N):- number(N).

needs_eval(EvalMe):- is_list(EvalMe),!.


args_violation(_Dpth,_Slf,Args,List):- ( \+ iz_conz(Args); \+ iz_conz(List)), !, fail.
args_violation(Depth,Self,[A|Args],[L|List]):- once(arg_violation(Depth,Self,A,L) ; args_violation(Depth,Self,Args,List)).
arg_violation(Depth,Self,A,L):- \+ (get_type0(Depth,Self,A,T), \+ type_violation(T,L)).
%arg_violation(Depth,Self,A,_):- get_type(Depth,Self,A,_),!.

type_violation(T,L):- \+ \+ (is_nonspecific_type(T);is_nonspecific_type(L)),!,fail.
type_violation(T,L):- T\=L.



args_conform(_Dpth,_Slf,Args,List):- ( \+ iz_conz(Args); \+ iz_conz(List)), !.
args_conform(Depth,Self,[A|Args],[L|List]):- arg_conform(Depth,Self,A,L) , args_conform(Depth,Self,Args,List).
arg_conform(Depth,Self,A,L):- get_type0(Depth,Self,A,T), type_conform(T,L),!.
arg_conform(_Dpth,_Slf,_,_).
%arg_conform(Depth,Self,A,_):- get_type(Depth,Self,A,_),!.

type_conform(T,L):- T=L,!.
type_conform(T,L):- \+ \+ (is_nonspecific_type(T);is_nonspecific_type(L)),!.

is_nonspecific_type(Var):- var(Var),!.
is_nonspecific_type('%Undefined%').
is_nonspecific_type([]).
is_nonspecific_type('Atom').
is_nonspecific_type('Any').

get_type(Depth,_Slf,Type,Type):- Depth<1,!.
%get_type(Depth,Self,Val,Type):- is_debugging(eval), !,
% ftrace(get_type0(Depth,Self,Val,Type)).
get_type(Depth,Self,Val,Type):-
  show_call(get_type0(Depth,Self,Val,Type)).
get_type(Depth,Self,Val,Type):-
  show_call(get_type1(Depth,Self,Val,Type)).


get_type0(_Dpth,_Slf,Var,'%Undefined%'):- var(Var),!.

/*
(: Left
  (-> %Undefined% Either))

(: (Left %Undefined%) Either)

*/

get_type0(Depth,Self,[Op|Args],Type):- symbol(Op),
  get_operator_typedef(Self,Op,Params,RetType),
  % Fills in type variables when possible
  ignore(args_conform(Depth,Self,Args,Params)),
  % unitests:  arg violations should return ()
  (\+ args_violation(Depth,Self,Args,Params) -> Type=RetType ; (Type=[],!)).

get_type0(_Dpth,Self,List,Type):- % is_list(List),
  metta_type(Self,Params,Type), List == Params.
  %last(LType,Type), nonvar(Type), is_type(Type).

get_type0(Depth,Self,EvalMe,Type):- needs_eval(EvalMe),eval_args(Depth,Self,EvalMe,Val), \+ needs_eval(Val),!,
   get_type(Depth,Self,Val,Type).

%get_type0(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,Type,['->'|List]).
get_type0(Depth,Self,List,Types):- List\==[], is_list(List),Depth2 is Depth-1,maplist(get_type(Depth2,Self),List,Types).
%get_type(Depth,Self,Op,Type):- nonvar(Op),metta_type(Self,Op,Type2),Depth2 is Depth-1,get_type(Depth2,Self,Type2,Type).
%get_type(Depth,Self,Op,Type):- Depth>0,nonvar(Op),metta_type(Self,Type,Op),!. %,!,last_element(List,Type).

get_type0(Depth,Self,Expr,Type):-Depth2 is Depth-1, eval_args(Depth2,Self,Expr,Val),Expr\=@=Val,get_type(Depth2,Self,Val,Type).


get_type0(Depth,Self,Expr,['StateMonad',Type]):- is_valid_nb_state(Expr),'get-state'(Expr,Val),!,
   ((state_decltype(Expr,Type),nonvar(Type)); get_type(Depth,Self,Val,Type)).
get_type0(_Dpth,_Slf,Val,Type):- is_decl_type(Val),(Type=Val;Type='Type').

get_type0(_Dpth,_Slf,Cmpd,Type):- compound(Cmpd),!, \+ ground(Cmpd),!,Type=[].


get_type0(_Dpth,_Slf,Val,'Number'):- number(Val).
get_type0(_Dpth,_Slf,Val,'Integer'):- integer(Val).
get_type0(_Dpth,_Slf,Val,'Decimal'):- float(Val).
get_type0(_Dpth,_Slf,Val,'Rational'):- rational(Val).
get_type0(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
get_type0(_Dpth,Self,Op,Type):- symbol(Op), metta_type(Self,Op,Type).
get_type0(_Dpth,_Slf,Val,'Symbol'):- symbol(Val).
get_type0(_Dpth,_Slf,Val,Type):- string(Val),!,(Type='String';Type='Symbol').
%get_type(Depth,Self,[T|List],['List',Type]):- Depth2 is Depth-1,  is_list(List),get_type(Depth2,Self,T,Type),!,
%  forall((member(Ele,List),nonvar(Ele)),get_type(Depth2,Self,Ele,Type)),!.
%get_type(Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
%get_type0(_Dpth,_Slf,_,'%Undefined%'):- fail.

state_decltype(Expr,Type):- functor(Expr,_,A),arg(A,Expr,Type),once(var(Type);is_decl_type(Type)).

get_type1(_Dpth,_Slf,Var,'%Undefined%'):- var(Var),!.
get_type1(_Dpth,_Slf,Val,'Number'):- number(Val),!.
get_type1(Depth,Self,Expr,['StateMonad',Type]):- is_valid_nb_state(Expr),'get-state'(Expr,Val),!,
   get_type1(Depth,Self,Val,Type).
get_type1(Depth,Self,EvalMe,Type):- needs_eval(EvalMe),eval_args(Depth,Self,EvalMe,Val), \+ needs_eval(Val),!,
   get_type1(Depth,Self,Val,Type).
get_type1(_Dpth,Self,[Fn|_],Type):- symbol(Fn),metta_type(Self,Fn,List),last_element(List,Type), nonvar(Type),
   is_type(Type).
get_type1(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,List,LType),last_element(LType,Type), nonvar(Type),
   is_type(Type).
get_type1(Depth,_Slf,Type,Type):- Depth<1,!.
get_type1(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,Type,['->'|List]).
get_type1(Depth,Self,List,Types):- List\==[], is_list(List),Depth2 is Depth-1,maplist(get_type1(Depth2,Self),List,Types).
get_type1(_Dpth,Self,Fn,Type):- symbol(Fn),metta_type(Self,Fn,Type),!.
%get_type1(Depth,Self,Fn,Type):- nonvar(Fn),metta_type(Self,Fn,Type2),Depth2 is Depth-1,get_type1(Depth2,Self,Type2,Type).
%get_type1(Depth,Self,Fn,Type):- Depth>0,nonvar(Fn),metta_type(Self,Type,Fn),!. %,!,last_element(List,Type).

get_type1(Depth,Self,Expr,Type):-Depth2 is Depth-1, eval_args(Depth2,Self,Expr,Val),Expr\=@=Val,get_type1(Depth2,Self,Val,Type).
get_type1(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_type1(_Dpth,_Slf,Val,Type):- is_decl_type(Val),Type=Val.
get_type1(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
get_type1(_Dpth,_Slf,Val,'Symbol'):- symbol(Val).
%get_type1(Depth,Self,[T|List],['List',Type]):- Depth2 is Depth-1,  is_list(List),get_type1(Depth2,Self,T,Type),!,
%  forall((member(Ele,List),nonvar(Ele)),get_type1(Depth2,Self,Ele,Type)),!.
%get_type1(Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
get_type1(_Dpth,_Slf,Cmpd,Type):- \+ ground(Cmpd),!,Type=[].
get_type1(_Dpth,_Slf,_,'%Undefined%'):- fail.
get_type1(Depth,Self,Val,Type):- Depth2 is Depth-1, get_type0(Depth2,Self,Val,Type).

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



eval_args30(_Dpth,Self,['import!',Other,File],RetVal):-

  must_det_ll(( into_space(Self,Other,Space),!, include_metta(Space,File),!,return_empty(Space,RetVal))). %RetVal=[].
eval_args30(Depth,Self,['bind!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,eval_args(Depth,Self,Expr,Value),nb_setval(Name,Value),  return_empty(Value,RetVal))).
eval_args30(Depth,Self,['pragma!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,nd_ignore((eval_args(Depth,Self,Expr,Value),set_option_value(Name,Value))),  return_empty(Value,RetVal))).
eval_args30(_Dpth,Self,['transfer!',File],RetVal):- !, must_det_ll((include_metta(Self,File),  return_empty(Self,RetVal))).

nd_ignore(Goal):- call(Goal)*->true;true.

eval_args30(_Dpth,_Slf,['::'|Expr],Expr):- !.
eval_args30(Depth,Self,['nop',Expr],Empty):- !,  eval_args(Depth,Self,Expr,_), return_empty([],Empty).
eval_args30(_Dpth,_Slf,['nop'],Empty):- !, return_empty([],Empty).
eval_args30(Depth,Self,['do',Expr],Empty):- !,  eval_args(Depth,Self,Expr,_), return_empty([],Empty).

is_True(T):- T\=='False',T\=='F',T\==[].

is_and(S):- \+ atom(S),!,fail.
is_and('and2','True').
is_and('#COMMA','True'). is_and(',','True'). is_and('and','True'). % is_and('And').

eval_args30(_Dpth,_Slf,[And],True):- is_and(And,True),!.
eval_args30(Depth,Self,[And,X,Y],TF):-  is_and(And,True),!, as_tf((
   eval_args(Depth,Self,X,True),eval_args(Depth,Self,Y,True)),TF).
eval_args30(Depth,Self,[And,X],True):- is_and(And,True),!,
 eval_args(Depth,Self,X,True).
eval_args30(Depth,Self,[And,X|Y],TF):- is_and(And,_True),!,
  eval_args(Depth,Self,X,TF1), \+ \+ is_True(TF1),
  eval_args(Depth,Self,[And|Y],TF).
%eval_args40(Depth,Self,[H|T],_):- \+ is_list(T),!,fail.
eval_args30(Depth,Self,['or',X,Y],TF):- !, as_tf((eval_args(Depth,Self,X,'True');eval_args(Depth,Self,Y,'True')),TF).


eval_args30(_Dpth,_Slf,['arity',F,A],TF):- !,as_tf(current_predicate(F/A),TF).

eval_args30(_Dpth,Self,['add-atom',Other,PredDecl],TF):- !,must_det_ll(( into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF))).
eval_args30(_Dpth,Self,['remove-atom',Other,PredDecl],TF):- !,must_det_ll(( into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF))).
eval_args30(_Dpth,Self,['atom-count',Other],Count):- !,must_det_ll(( into_space(Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2)).
eval_args30(_Dpth,Self,['atom-replace',Other,Rem,Add],TF):- !, must_det_ll((into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF))).


eval_args30(Depth,Self,['+',N1,N2],N):- number(N1),!,
   eval_args(Depth,Self,N2,N2Res), catch(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).
eval_args30(Depth,Self,['-',N1,N2],N):- number(N1),!,
   eval_args(Depth,Self,N2,N2Res), catch(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).

%
%
%
%
%
%
%
%
eval_args30(Depth,Self,[V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(V), !,
  adjust_args(Depth,Self,V,VI,VO).


eval_args30(Depth,Self,X,Y):-
  (efbug(show_success,eval_args40(Depth,Self,X,Y))*->true;
    (efbug(show_failure,eval_args40_failed(Depth,Self,X,Y)*->true;X=Y))).

%maybe_efbug(SS,G):- efbug(SS,G)*-> if_trace(eval,wdmsg(SS=G)) ; fail.
maybe_efbug(_,G):- call(G).
%efbug(P1,G):- call(P1,G).
efbug(_,G):- call(G).


eval_args40_failed(_Dpth,_Slf,T,T).
/*

eval_args40_failed(Depth,Self,[X|XX],[Y]):- XX == [],!, eval_args20(Depth,Self,X,Y).

%eval_args40_failed(_Dpth,_Slf,T,TT):- T==[],!,TT=[].
%eval_args40_failed(_Dpth,_Slf,T,TT):- var(T),!,TT=T.
%eval_args40_failed(_Dpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace([F|LESS]\==Res),!.
%eval_args40_failed(Depth,Self,[V|Nil],[O]):- Nil==[], once(eval_args(Depth,Self,V,O)),V\=@=O,!.
eval_args40_failed(Depth,Self,[H|T],[HH|TT]):- !,
  eval_args(Depth,Self,H,HH),
  eval_args40_failed(Depth,Self,T,TT).

eval_args40_failed(Depth,Self,T,TT):- eval_args(Depth,Self,T,TT).

   %eval_args(Depth,Self,X,Y):- eval_args20(Depth,Self,X,Y)*->true;Y=[].
*/
%eval_args20(Depth,_,_,_):- Depth<1,!,fail.
%eval_args20(Depth,_,X,Y):- Depth<3, !, ground(X), (Y=X).
%eval_args20(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.

% Kills zero arity functions
% eval_args20(Depth,Self,[X|Nil],[Y]):- Nil ==[],!,eval_args(Depth,Self,X,Y).


/*
into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).
eval_args40(_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.
*/
% Macro Functions
eval_args40(Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   mnotrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   eval_args(Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl, subst(PredDecl,SSub,Repl,Temp))),
   eval_args(Depth,Self,[F|Temp],Res).



% user defined function
eval_args40(Depth,Self,[H|PredDecl],Res):- 
   mnotrace(is_user_defined_head(Self,H)),!,
   eval_args60(Depth,Self,[H|PredDecl],Res).

% function inherited by system
eval_args40(Depth,Self,PredDecl,Res):-
  eval_args80(Depth,Self,PredDecl,Res).


last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.




catch_warn(G):- quietly(catch(G,E,(wdmsg(catch_warn(G)-->E),fail))).
catch_nowarn(G):- quietly(catch(G,error(_,_),fail)).

as_tf(G,TF):- catch_nowarn((call(G)*->TF='True';TF='False')).
eval_selfless(['==',X,Y],TF):- as_tf(X=:=Y,TF),!.
eval_selfless(['==',X,Y],TF):- as_tf(X=@=Y,TF),!.
%eval_selfless(['=',X,Y],TF):-!,as_tf(X #= Y,TF).
eval_selfless(['>',X,Y],TF):-!,as_tf(X>Y,TF).
eval_selfless(['<',X,Y],TF):-!,as_tf(X<Y,TF).
eval_selfless(['=>',X,Y],TF):-!,as_tf(X>=Y,TF).
eval_selfless(['<=',X,Y],TF):-!,as_tf(X=<Y,TF).
eval_selfless(['%',X,Y],TF):-!,eval_selfless(['mod',X,Y],TF).

eval_selfless(LIS,Y):-  mnotrace((
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
:- discontiguous eval_args6/4.
%eval_args40(Depth,Self,PredDecl,Res):- eval_args6(Depth,Self,PredDecl,Res).

%eval_args40(_Dpth,_Slf,L1,Res):- is_list(L1),maplist(self_eval,L1),!,Res=L1.
%eval_args40(_Depth,_Self,X,X).



eval_args60(Depth,Self,H,B):-  ( no_repeats(H+B,eval_args64(Depth,Self,H,B))*->true;eval_args67(Depth,Self,H,B)).

eval_args64(_Dpth,Self,H,B):- metta_defn(Self,H,B).
eval_args64(_Dpth,Self,H,B):- metta_atom(Self,H),B=H.

% Has argument that is headed by the same function
eval_args67(Depth,Self,[H1|Args],Res):-
   mnotrace((append(Left,[[H2|H2Args]|Rest],Args), H2==H1)),!,
   eval_args(Depth,Self,[H2|H2Args],ArgRes),
   mnotrace((ArgRes\==[H2|H2Args], append(Left,[ArgRes|Rest],NewArgs))),
   eval_args60(Depth,Self,[H1|NewArgs],Res).

eval_args67(Depth,Self,[[H|Start]|T1],Y):-
   mnotrace((is_user_defined_head_f(Self,H),is_list(Start))),
   metta_defn(Self,[H|Start],Left),
   eval_args(Depth,Self,[Left|T1],Y).

% Has subterm to eval
eval_args67(Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   quietly(sub_sterm1(SSub,PredDecl)),
   mnotrace((ground(SSub),SSub=[_|Sub], is_list(Sub),maplist(atomic,SSub))),
   eval_args(Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   eval_args60(Depth,Self,[F|Temp],Res).

%eval_args67(Depth,Self,X,Y):- (eval_args68(Depth,Self,X,Y)*->true;metta_atom_iter(Depth,Self,[=,X,Y])).

eval_args67(Depth,Self,PredDecl,Res):- fail,
 ((term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (eval_args(Depth,Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))))),
 PredDecl\=@=Res.

eval_args68(_Dpth,Self,[H|_],_):- mnotrace( \+ is_user_defined_head_f(Self,H) ), !,fail.
eval_args68(_Dpth,Self,[H|T1],Y):- metta_defn(Self,[H|T1],Y).
eval_args68(_Dpth,Self,[H|T1],'True'):- metta_atom(Self,[H|T1]).
eval_args68(_Dpth,Self,CALL,Y):- fail,append(Left,[Y],CALL),metta_defn(Self,Left,Y).


%eval_args6(Depth,Self,['ift',CR,Then],RO):- fail, !, %fail, % trace,
%   metta_defn(Self,['ift',R,Then],Become),eval_args(Depth,Self,CR,R),eval_args(Depth,Self,Then,_True),eval_args(Depth,Self,Become,RO).

metta_atom_iter(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!,
  (metta_defn(Other,H,B)*->true;(metta_atom(Other,H),B='True')).

metta_atom_iter(Depth,_,_):- Depth<3,!,fail.
metta_atom_iter(_Dpth,_Slf,[]):-!.
metta_atom_iter(_Dpth,Other,H):- metta_atom(Other,H).
metta_atom_iter(Depth,Other,H):- D2 is Depth -1, metta_defn(Other,H,B),metta_atom_iter(D2,Other,B).
metta_atom_iter(_Dpth,_Slf,[And]):- is_and(And),!.
metta_atom_iter(Depth,Self,[And,X|Y]):- is_and(And),!,D2 is Depth -1, metta_atom_iter(D2,Self,X),metta_atom_iter(D2,Self,[And|Y]).
/*
metta_atom_iter2(_,Self,[=,X,Y]):- metta_defn(Self,X,Y).
metta_atom_iter2(_Dpth,Other,[Equal,H,B]):- '=' == Equal,!, metta_defn(Other,H,B).
metta_atom_iter2(_Dpth,Self,X,Y):- metta_defn(Self,X,Y). %, Y\=='True'.
metta_atom_iter2(_Dpth,Self,X,Y):- metta_atom(Self,[=,X,Y]). %, Y\=='True'.

*/
metta_atom_iter_ref(Other,['=',H,B],Ref):-clause(metta_defn(Other,H,B),true,Ref).
metta_atom_iter_ref(Other,H,Ref):-clause(metta_atom(Other,H),true,Ref).

sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- sub_sterm1(Sub,Term).
sub_sterm1(_  ,List):- \+ compound(List),!,fail.
sub_sterm1(Sub,List):- is_list(List),!,member(SL,List),sub_sterm(Sub,SL).
sub_sterm1(_  ,[_|_]):-!,fail.
sub_sterm1(Sub,Term):- arg(_,Term,SL),sub_sterm(Sub,SL).

%not_compound(Term):- \+ is_list(Term),!.
%eval_args40(Depth,Self,Term,Res):- maplist(not_compound,Term),!,eval_args645(Depth,Self,Term,Res).

eval_args80(Depth,Self,FX,FY):- eval_args84(Depth,Self,FX,FY).
% function inherited by system
eval_args80(Depth,Self,[F|X],FY):- % adjust_args(Depth,Self,F,X,Eval),
  eval_args85(Depth,Self,[F|X],FY).


eval_args84(_Dpth,_Slf,[F|LESS],Res):- once(maybe_efbug(show_success,eval_selfless([F|LESS],Res))),mnotrace(([F|LESS]\==Res)),!.
eval_args85(Depth,Self,[AE|More],TF):- length(More,Len),
  (is_syspred(AE,Len,Pred),catch_warn(efbug(show_call,as_tf(apply(Pred,More),TF))))*->true;eval_args86(Depth,Self,[AE|More],TF).
eval_args86(_Dpth,_Slf,[AE|More],TF):- length([AE|More],Len), is_syspred(AE,Len,Pred),append(More,[TF],Args),!,
  efbug(show_call,catch_warn(apply(Pred,Args))).

%eval_args80(Depth,Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval_args(Depth,Self,[F2|X2],Y2),eval_args(Depth,Self,X1,Y1).


cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
findall_eval(Depth,Self,X,L):- !,   findall(E,eval_args(Depth,Self,X,E),L).
bagof_eval(Depth,Self,X,L):- !,bagof_or_nil(E,eval_args(Depth,Self,X,E),L).
setof_eval(Depth,Self,X,S):- !,bagof_or_nil(E,eval_args(Depth,Self,X,E),L),sort(L,S).
%setof_eval(Depth,Self,X,S):- setof(E,eval_args(Depth,Self,X,E),S)*->true;S=[].
bagof_or_nil(T,G,L):- findall(T,G,L).
%bagof_or_nil(T,G,L):- bagof(T,G,L)*->true;L=[].

