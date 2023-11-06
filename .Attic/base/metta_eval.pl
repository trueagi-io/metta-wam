%self_eval(X):- var(X),!.
%self_eval(X):- string(X),!.
%self_eval(X):- number(X),!.
%self_eval([]).
self_eval(X):- \+ callable(X),!.
self_eval(X):- is_valid_nb_state(X),!.
self_eval(X):- is_list(X),!,fail.
%self_eval(X):- compound(X),!.
%self_eval(X):- is_ref(X),!,fail.
self_eval(X):- atom(X),!, \+ nb_current(X,_),!.
self_eval('True'). self_eval('False'). self_eval('F').


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
if_trace(Flag,Goal):- catch(ignore((is_debugging(Flag),Goal)),E,wdmsg(E-->if_trace(Flag,Goal))).

is_debugging(Flag):- var(Flag),!,fail.
is_debugging(Flag):- \+ atom(Flag), flag_to_var(Flag,Var), atom(Var),!,is_debugging(Var).
is_debugging(Flag):- debugging(Flag),!.
is_debugging(Flag):- debugging(metta(Flag)),!.
is_debugging(Flag):- flag_to_var(Flag,Var),!,option_value(Var,true).


eval_args0000(Depth,_Slf,X,Y):- Depth<1,!,X=Y, (\+ is_debugging(overflow)-> true; flag(eval_num,_,0),set_debug((eval),true)).
%eval_args0000(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args0000(Depth,Self,X,Y):-
  copy_term(X,XX),
  Depth2 is Depth-1,
  call_nth(eval_args11(Depth,Self,X,M),Nth),
  ((is_bool_or_same(XX,M))
          -> (!, (Nth=1->Y=M;fail) )
            ;  eval_args0000(Depth2,Self,M,Y)),
  nonvar(Y).

is_bool_or_same( X,M):- X=@=M,!.
is_bool_or_same( _,_):- !, fail.
is_bool_or_same( _,_):- !, fail.
is_bool_or_same( X,_):- \+ ground(X),!,fail.
is_bool_or_same(_X,M):- \+ atomic(M),!,fail.
%is_bool_or_same( X,'True'):- ground(X),!.
is_bool_or_same(_X,M):- is_bool_or(M).
% if `True` is not commented, we fail two tests in examples/compat/test_scripts/b4_nondeterm.metta
%is_bool_or('True').
is_bool_or('False').
is_bool_or([]).



%eval_args11(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
%eval_args11(Depth,Self,X,Y):- \+ debugging(metta(eval)),!, eval_args1(Depth,Self,X,Y).
eval_args11(Depth,Self,X,Y):- flag(eval_num,EX,EX+1),
  option_else(traclen,Max,100),
  (EX>Max->(set_debug(eval,false),set_debug(exec,false),set_debug(overflow,false),
    skip(write('Switched off tracing. For a longer trace !(pragma! tracelen 101))')));true),
  notrace((D1 is Depth-1)),
  DR is 99-D1,
  if_trace((eval),indentq(Depth,'-->'(EX,Self,X,depth(DR)))),
  Ret=retval(fail),
  call_cleanup(
   (eval_args1(D1,Self,X,Y),
    notrace(( nb_setarg(1,Ret,Y)))),
   notrace(ignore(((Y\=@=X,flag(eval_num,_,EX),EXm1 is EX -1, if_trace((eval),indentq(Depth,'<--'(EXm1,Ret)))))))),
  (Ret\=@=retval(fail)->true;(rtrace(eval_args1(D1,Self,X,Y)),fail)).


:- discontiguous eval_args1/4.
:- discontiguous eval_args2/4.

eval_args1(_Dpth,_Slf,Name,Value):- atom(Name), !, (nb_current(Name,Value) -> true ; Value=Name).
eval_args1(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_args1(Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval_args(Depth,Self,VI,VM),
  ( VM\==VI -> eval_args(Depth,Self,[V|VM],VVO) ;
    (eval_args(Depth,Self,V,VV), (V\==VV -> eval_args(Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_args1(_Dpth,_Slf,X,Y):- \+ is_list(X),!,Y=X.

eval_args1(Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval_args(Depth,Self),VI,VO).

eval_args1(_Dpth,_Slf,['repl!'],'True'):- !, repl.
eval_args1(Depth,Self,['!',Cond],Res):- !, call(eval_args(Depth,Self,Cond,Res)).
eval_args1(Depth,Self,['rtrace',Cond],Res):- !, rtrace(eval_args(Depth,Self,Cond,Res)).
eval_args1(Depth,Self,['trace',Cond],Res):- !, with_debug(eval,eval_args(Depth,Self,Cond,Res)).
eval_args1(Depth,Self,['time',Cond],Res):- !, time_eval(eval(Cond),eval_args(Depth,Self,Cond,Res)).
eval_args1(Depth,Self,['print',Cond],Res):- !, eval_args(Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
% !(println! $1)
eval_args1(Depth,Self,['println!'|Cond],Res):- !, maplist(eval_args(Depth,Self),Cond,[Res|Out]),
   format('~N'),maplist(write_src,[Res|Out]),format('~N').
eval_args1(Depth,Self,['trace!',A|Cond],Res):- !, maplist(eval_args(Depth,Self),[A|Cond],[AA|Result]),
   last(Result,Res), format('~N'),maplist(write_src,[AA]),format('~N').

%eval_args1(Depth,Self,['trace!',A,B],C):- !,eval_args(Depth,Self,B,C),format('~N'),wdmsg(['trace!',A,B]=C),format('~N').
%eval_args1(_Dpth,_Slf,['trace!',A],A):- !, format('~N'),wdmsg(A),format('~N').

eval_args1(_Dpth,_Slf,List,Y):- is_list(List),maplist(self_eval,List),List=[H|_], \+ atom(H), !,Y=List.

eval_args1(Depth,Self,['assertTrue', X],TF):- !, eval_args(Depth,Self,['assertEqual',X,'True'],TF).
eval_args1(Depth,Self,['assertFalse',X],TF):- !, eval_args(Depth,Self,['assertEqual',X,'False'],TF).

eval_args1(Depth,Self,['assertEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertEqual',X0,Y0],
        (bagof_eval(Depth,Self,X,XX),
         bagof_eval(Depth,Self,Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]).

eval_args1(Depth,Self,['assertNotEqual',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertNotEqual',X0,Y0],
        (bagof_eval(Depth,Self,X,XX), bagof_eval(Depth,Self,Y,YY)),
         \+ equal_enough(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,not,YY]).

eval_args1(Depth,Self,['assertEqualToResult',X0,Y0],RetVal):- !,
  subst_vars(X0,X),subst_vars(Y0,Y),
   loonit_assert_source_tf(
        ['assertEqualToResult',X0,Y0],
        (bagof_eval(Depth,Self,X,XX), =(Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]),!.


loonit_assert_source_tf(Src,Goal,Check,TF):-
   copy_term(Goal,OrigGoal),
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


eval_args1(Depth,Self,['match',Other,Goal,Template],Template):- into_space(Self,Other,Space),!, metta_atom_iter(Depth,Space,Goal).
eval_args1(Depth,Self,['match',Other,Goal,Template,Else],Template):-
  (eval_args1(Depth,Self,['match',Other,Goal,Template],Template)*->true;Template=Else).

% Macro: case
eval_args1(Depth,Self,X,Res):-
   X= [CaseSym,A,CL],CaseSym == 'case', !,
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


%[collapse,[1,2,3]]
eval_args1(Depth,Self,['collapse',List],Res):-!, bagof_eval(Depth,Self,List,Res).
%[superpose,[1,2,3]]
eval_args1(Depth,Self,['superpose',List],Res):- !, member(E,List),
  eval_args(Depth,Self,E,Res).
get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).
eval_args1(Depth,Self, Term, Res):- fail,
  mnotrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   eval_args(Depth,Self, Term, Res).

eval_args1(Depth,Self, Term, Res):- fail,
   mnotrace(( get_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, setof_eval(Depth,Self,List,Var),
   eval_args(Depth,Self, Term, Res).


max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).


eval_args1(Depth,Self,['if',Cond,Then],Res):- !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res) ; Res = []).

eval_args1(Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval_args(Depth,Self,Cond,TF),
   (is_True(TF) -> eval_args(Depth,Self,Then,Res);eval_args(Depth,Self,Else,Res)).

eval_args1(_Dpth,_Slf,[_,Nothing],Nothing):- 'Nothing'==Nothing,!.

eval_args1(Depth,Self,['let',A,A5,AA],OO):- !,
  %(var(A)->true;trace),
  ((eval_args(Depth,Self,A5,AE), AE=A)),
  eval_args(Depth,Self,AA,OO).
%eval_args1(Depth,Self,['let',A,A5,AA],AAO):- !,eval_args(Depth,Self,A5,A),eval_args(Depth,Self,AA,AAO).
eval_args1(Depth,Self,['let*',[],Body],RetVal):- !, eval_args(Depth,Self,Body,RetVal).
eval_args1(Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
    eval_args1(Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).

%eval_args1(Depth,Self,['colapse'|List], Flat):- !, maplist(eval_args(Depth,Self),List,Res),flatten(Res,Flat).
eval_args1(Depth,Self,['get-atoms',Other],PredDecl):- !,into_space(Self,Other,Space), metta_atom_iter(Depth,Space,PredDecl).
eval_args1(_Dpth,_Slf,['car-atom',Atom],CAR):- !, Atom=[CAR|_],!.
eval_args1(_Dpth,_Slf,['cdr-atom',Atom],CDR):- !, Atom=[_|CDR],!.

eval_args1(Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  eval_args(Depth,Self,A,AA), eval_args(Depth,Self,B,BB).

eval_args1(Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
   eval_args(Depth,Self,A,AA), eval_args(Depth,Self,B,BB).


eval_args1(Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !, eval_args(Depth,Self,StateExpr,StateMonad),
  eval_args(Depth,Self,UpdatedValue,Value),  'change-state!'(Depth,Self,StateMonad, Value, Ret).
eval_args1(Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  eval_args(Depth,Self,UpdatedValue,Value),  'new-state'(Depth,Self,Value,StateMonad).
eval_args1(Depth,Self,['get-state',StateExpr],Value):- !,
  eval_args(Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value).



% eval_args1(Depth,Self,['get-state',Expr],Value):- !, eval_args(Depth,Self,Expr,State), arg(1,State,Value).



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


eval_args1(Depth,Self,['get-type',Val],Type):- !, get_type(Depth,Self,Val,Type),ground(Type),Type\==[], Type\==Val,!.


mnotrace(G):- once(G).

is_decl_type(ST):- metta_type(_,_,Type),sub_sterm(T,Type),T=@=ST, \+ nontype(ST).
is_decl_type([ST|_]):- !, atom(ST),is_decl_type_l(ST).
is_decl_type(ST):- \+ atom(ST),!,fail.
is_decl_type('%Undefined%').  is_decl_type('Number').
is_decl_type('String').       is_decl_type('Bool').
is_decl_type('Type').         is_decl_type('Symbol').
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

get_type(_Dpth,_Slf,Var,'%Undefined%'):- var(Var),!.
get_type(_Dpth,_Slf,Val,'Number'):- number(Val),!.
get_type(Depth,Self,Expr,['StateMonad',Type]):- is_valid_nb_state(Expr),'get-state'(Expr,Val),!,
   get_type(Depth,Self,Val,Type).
get_type(Depth,Self,EvalMe,Type):- needs_eval(EvalMe),eval_args(Depth,Self,EvalMe,Val), \+ needs_eval(Val),!,
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

get_type(Depth,Self,Expr,Type):-Depth2 is Depth-1, eval_args(Depth2,Self,Expr,Val),Expr\=@=Val,get_type(Depth2,Self,Val,Type).
get_type(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_type(_Dpth,_Slf,Val,Type):- is_decl_type(Val),Type=Val.
get_type(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
get_type(_Dpth,_Slf,Val,'Symbol'):- symbol(Val).
%get_type(Depth,Self,[T|List],['List',Type]):- Depth2 is Depth-1,  is_list(List),get_type(Depth2,Self,T,Type),!,
%  forall((member(Ele,List),nonvar(Ele)),get_type(Depth2,Self,Ele,Type)),!.
%get_type(Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
get_type(_Dpth,_Slf,Cmpd,Type):- \+ ground(Cmpd),!,Type=[].
get_type(_Dpth,_Slf,_,'%Undefined%'):- fail.
eval_args1(Depth,Self,['length',L],Res):- !, eval_args(Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_args1(Depth,Self,['CountElement',L],Res):- !, eval_args(Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).


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
eval_args1(Depth,Self,[F,A|Args],Res):-
   \+ self_eval(A),
   eval_args(Depth,Self,A,AA),AA\==A,
   eval_args(Depth,Self,[F,AA|Args],Res).


eval_args1(Depth,Self,[F,A1|AArgs],Res):- fail, member(F,['+']),
 cwdl(40,((
   append(L,[A|R],AArgs),
   \+ self_eval(A),
   eval_args(Depth,Self,A,AA),AA\==A,!,
   append(L,[AA|R],NewArgs), eval_args(Depth,Self,[F,A1|NewArgs],Res)))).
*/

/* %%

% !(assertEqualToResult ((inc) 2) (3))
eval_args1(Depth,Self,[F|Args],Res):- is_list(F),
  metta_atom_iter(Depth,Self,['=',F,R]), eval_args(Depth,Self,[R|Args],Res).

eval_args1(Depth,Self,[F|Args],Res):- is_list(F), Args\==[],
  append(F,Args,FArgs),!,eval_args(Depth,Self,FArgs,Res).
*/
eval_args1(_Dpth,Self,['import!',Other,File],RetVal):- into_space(Self,Other,Space),!, include_metta(Space,File),!,return_empty(Space,RetVal). %RetVal=[].
eval_args1(Depth,Self,['bind!',Other,Expr],RetVal):-
   into_name(Self,Other,Name),!,eval_args(Depth,Self,Expr,Value),nb_setval(Name,Value),  return_empty(Value,RetVal).
eval_args1(Depth,Self,['pragma!',Other,Expr],RetVal):-
   into_name(Self,Other,Name),!,nd_ignore((eval_args(Depth,Self,Expr,Value),set_option_value(Name,Value))),  return_empty(Value,RetVal).
eval_args1(_Dpth,Self,['transfer!',File],RetVal):- !, include_metta(Self,File),  return_empty(Self,RetVal).

nd_ignore(Goal):- call(Goal)*->true;true.

eval_args1(Depth,Self,['nop',Expr],Empty):- !,  eval_args(Depth,Self,Expr,_), return_empty([],Empty).
eval_args1(_Dpth,_Slf,['nop'],Empty):- !, return_empty([],Empty).
eval_args1(Depth,Self,['do',Expr],Empty):- !,  eval_args(Depth,Self,Expr,_), return_empty([],Empty).

is_True(T):- T\=='False',T\=='F',T\==[].

is_and(S):- \+ atom(S),!,fail.
is_and('#COMMA'). is_and(','). is_and('and'). % is_and('And').

eval_args1(_Dpth,_Slf,[And],'True'):- is_and(And),!.
eval_args1(Depth,Self,[And,X,Y],TF):-  is_and(And),!, as_tf((
   eval_args(Depth,Self,X,'True'),eval_args(Depth,Self,Y,'True')),TF).
eval_args1(Depth,Self,[And,X],TF):- is_and(And),!,
 eval_args(Depth,Self,X,TF).
eval_args1(Depth,Self,[And,X|Y],TF):- is_and(And),!,
  eval_args(Depth,Self,X,TF1), \+ \+ is_True(TF1),
  eval_args(Depth,Self,[And|Y],TF).
%eval_args2(Depth,Self,[H|T],_):- \+ is_list(T),!,fail.
eval_args1(Depth,Self,['or',X,Y],TF):- !, as_tf((eval_args(Depth,Self,X,'True');eval_args(Depth,Self,Y,'True')),TF).



eval_args1(_Dpth,Self,['add-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,load,PredDecl),TF).
eval_args1(_Dpth,Self,['remove-atom',Other,PredDecl],TF):- !, into_space(Self,Other,Space), as_tf(do_metta(Space,unload,PredDecl),TF).
eval_args1(_Dpth,Self,['atom-count',Other],Count):- !, into_space(Self,Other,Space), findall(_,metta_defn(Other,_,_),L1),length(L1,C1),findall(_,metta_atom(Space,_),L2),length(L2,C2),Count is C1+C2.
eval_args1(_Dpth,Self,['atom-replace',Other,Rem,Add],TF):- !, into_space(Self,Other,Space), copy_term(Rem,RCopy),
  as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Other,load,Add)),TF).


eval_args1(Depth,Self,['+',N1,N2],N):- number(N1),!,
   eval_args(Depth,Self,N2,N2Res), catch(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).
eval_args1(Depth,Self,['-',N1,N2],N):- number(N1),!,
   eval_args(Depth,Self,N2,N2Res), catch(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail)).

eval_args1(Depth,Self,[V|VI],[V|VO]):- nonvar(V),is_metta_data_functor(V),is_list(VI),!,maplist(eval_args(Depth,Self),VI,VO).

eval_args1(Depth,Self,X,Y):-
  (eval_args2(Depth,Self,X,Y)*->true;
    (eval_args2_failed(Depth,Self,X,Y)*->true;X=Y)).

eval_args2_failed(Depth,Self,[X|XX],[Y]):- XX == [],!, eval_args1(Depth,Self,X,Y).

eval_args2_failed(_Dpth,_Slf,T,TT):- T==[],!,TT=[].
eval_args2_failed(_Dpth,_Slf,T,TT):- var(T),!,TT=T.
eval_args2_failed(_Dpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace([F|LESS]\==Res),!.
%eval_args2_failed(Depth,Self,[V|Nil],[O]):- Nil==[], once(eval_args(Depth,Self,V,O)),V\=@=O,!.
eval_args2_failed(Depth,Self,[H|T],[HH|TT]):- !,
  eval_args(Depth,Self,H,HH),
  eval_args2_failed(Depth,Self,T,TT).

eval_args2_failed(Depth,Self,T,TT):- eval_args(Depth,Self,T,TT).

   %eval_args(Depth,Self,X,Y):- eval_args1(Depth,Self,X,Y)*->true;Y=[].

%eval_args1(Depth,_,_,_):- Depth<1,!,fail.
%eval_args1(Depth,_,X,Y):- Depth<3, !, ground(X), (Y=X).
%eval_args1(_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.

% Kills zero arity functions eval_args1(Depth,Self,[X|Nil],[Y]):- Nil ==[],!,eval_args(Depth,Self,X,Y).


/*
into_values(List,Many):- List==[],!,Many=[].
into_values([X|List],Many):- List==[],is_list(X),!,Many=X.
into_values(Many,Many).
eval_args2(_Dpth,_Slf,Name,Value):- atom(Name), nb_current(Name,Value),!.
*/
% Macro Functions
%eval_args1(Depth,_,_,_):- Depth<1,!,fail.
eval_args2(Depth,_,X,Y):- Depth<3, !, fail, ground(X), (Y=X).
eval_args2(Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   mnotrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   eval_args(Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl, subst(PredDecl,SSub,Repl,Temp))),
   eval_args(Depth,Self,[F|Temp],Res).



% user defined function
eval_args2(Depth,Self,[H|PredDecl],Res):- 
   mnotrace(is_user_defined_head(Self,H)),!,
   eval_args30(Depth,Self,[H|PredDecl],Res).

% function inherited by system
eval_args2(Depth,Self,PredDecl,Res):- eval_args40(Depth,Self,PredDecl,Res).


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
:- discontiguous eval_args3/4.
%eval_args2(Depth,Self,PredDecl,Res):- eval_args3(Depth,Self,PredDecl,Res).

%eval_args2(_Dpth,_Slf,L1,Res):- is_list(L1),maplist(self_eval,L1),!,Res=L1.
%eval_args2(_Depth,_Self,X,X).


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
is_special_op('let*').
%is_special_op('nop').
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



eval_args30(Depth,Self,H,B):-  ( no_repeats(H+B,eval_args34(Depth,Self,H,B))*->true;eval_args37(Depth,Self,H,B)).

eval_args34(_Dpth,Self,H,B):- metta_defn(Self,H,B).
eval_args34(_Dpth,Self,H,B):- metta_atom(Self,H),B=H.

% Has argument that is headed by the same function
eval_args37(Depth,Self,[H1|Args],Res):-
   mnotrace((append(Left,[[H2|H2Args]|Rest],Args), H2==H1)),!,
   eval_args(Depth,Self,[H2|H2Args],ArgRes),
   mnotrace((ArgRes\==[H2|H2Args], append(Left,[ArgRes|Rest],NewArgs))),
   eval_args30(Depth,Self,[H1|NewArgs],Res).

eval_args37(Depth,Self,[[H|Start]|T1],Y):-
   mnotrace((is_user_defined_head_f(Self,H),is_list(Start))),
   metta_defn(Self,[H|Start],Left),
   eval_args(Depth,Self,[Left|T1],Y).

% Has subterm to eval
eval_args37(Depth,Self,[F|PredDecl],Res):-
   Depth>1,
   quietly(sub_sterm1(SSub,PredDecl)),
   mnotrace((ground(SSub),SSub=[_|Sub], is_list(Sub),maplist(atomic,SSub))),
   eval_args(Depth,Self,SSub,Repl),
   mnotrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   eval_args30(Depth,Self,[F|Temp],Res).

%eval_args37(Depth,Self,X,Y):- (eval_args38(Depth,Self,X,Y)*->true;metta_atom_iter(Depth,Self,[=,X,Y])).

eval_args37(Depth,Self,PredDecl,Res):- fail,
 ((term_variables(PredDecl,Vars),
  (metta_atom(Self,PredDecl) *-> (Vars ==[]->Res='True';Vars=Res);
   (eval_args(Depth,Self,PredDecl,Res),ignore(Vars ==[]->Res='True';Vars=Res))))),
 PredDecl\=@=Res.

eval_args38(_Dpth,Self,[H|_],_):- mnotrace( \+ is_user_defined_head_f(Self,H) ), !,fail.
eval_args38(_Dpth,Self,[H|T1],Y):- metta_defn(Self,[H|T1],Y).
eval_args38(_Dpth,Self,[H|T1],'True'):- metta_atom(Self,[H|T1]).
eval_args38(_Dpth,Self,CALL,Y):- fail,append(Left,[Y],CALL),metta_defn(Self,Left,Y).


%eval_args3(Depth,Self,['ift',CR,Then],RO):- fail, !, %fail, % trace,
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
%eval_args2(Depth,Self,Term,Res):- maplist(not_compound,Term),!,eval_args345(Depth,Self,Term,Res).


% function inherited by system
eval_args40(Depth,Self,[F|X],FY):- is_function(F), \+ is_special_op(F), is_list(X),
  maplist(eval_args(Depth,Self),X,Y),!,eval_args5(Depth,Self,[F|Y],FY).
eval_args40(Depth,Self,FX,FY):- eval_args5(Depth,Self,FX,FY).

eval_args5(_Dpth,_Slf,[F|LESS],Res):- once(eval_selfless([F|LESS],Res)),mnotrace(([F|LESS]\==Res)),!.
eval_args5(Depth,Self,[AE|More],TF):- length(More,Len),
  (is_syspred(AE,Len,Pred),catch_warn(as_tf(apply(Pred,More),TF)))*->true;eval_args6(Depth,Self,[AE|More],TF).
eval_args6(_Dpth,_Slf,[AE|More],TF):- length([AE|More],Len), is_syspred(AE,Len,Pred),append(More,[TF],Args),!,catch_warn(apply(Pred,Args)).

%eval_args40(Depth,Self,[X1|[F2|X2]],[Y1|Y2]):- is_function(F2),!,eval_args(Depth,Self,[F2|X2],Y2),eval_args(Depth,Self,X1,Y1).


cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).
findall_eval(Depth,Self,X,L):- !,   findall(E,eval_args(Depth,Self,X,E),L).
bagof_eval(Depth,Self,X,L):- !,bagof_or_nil(E,eval_args(Depth,Self,X,E),L).
setof_eval(Depth,Self,X,S):- !,bagof_or_nil(E,eval_args(Depth,Self,X,E),L),sort(L,S).
%setof_eval(Depth,Self,X,S):- setof(E,eval_args(Depth,Self,X,E),S)*->true;S=[].
bagof_or_nil(T,G,L):- findall(T,G,L).
%bagof_or_nil(T,G,L):- bagof(T,G,L)*->true;L=[].
