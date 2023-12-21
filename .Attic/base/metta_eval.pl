%
% post match modew
%:- style_check(-singleton).

self_eval0(X):- \+ callable(X),!.
self_eval0(X):- is_valid_nb_state(X),!.

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
%:- 'hyde'(notrace/1).
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

evals_to(XX,Y):- Y=@=XX,!.
evals_to(XX,Y):- Y=='True',!, is_True(XX),!.

current_space(Space):- current_self(Space).

do_expander('=',_,X,X):-!.
do_expander(':',_,X,Y):- !, get_type(X,Y)*->X=Y.

'get_type'(Arg,Type):- 'get-type'(Arg,Type).




eval_args(X,Y):- current_self(Self),
  eval_args(100,Self,X,Y).
eval_args(Depth,Self,X,Y):- eval_args('=',_,Depth,Self,X,Y).
eval_args(Eq,RetType,Depth,Self,X,Y):- eval(Eq,RetType,Depth,Self,X,Y).
/*
eval_args(Eq,RetType,Depth,Self,X,Y):-
   locally(set_prolog_flag(gc,true),
      rtrace_on_existence_error(
     eval(Eq,RetType,Depth,Self,X,Y))).
*/


%eval(Eq,RetType,Depth,_Self,X,_Y):- forall(between(6,Depth,_),write(' ')),writeqln(eval(Eq,RetType,X)),fail.
eval(Depth,Self,X,Y):- eval('=',_RetType,Depth,Self,X,Y).

%eval(Eq,RetType,_Dpth,_Slf,X,Y):- nonvar(Y),X=Y,!.

eval(_Eq,_RetType,_Dpth,_Slf,X,Y):- notrace(var(X)),!,Y=X.

eval(Eq,RetType,Depth,Self,X,Y):- notrace(nonvar(Y)),!,
   get_type(Depth,Self,Y,RetType), !,
   eval(Eq,RetType,Depth,Self,X,XX),evals_to(XX,Y).


eval(Eq,RetType,_Dpth,_Slf,[X|T],Y):- notrace((T==[], number(X))),!, do_expander(Eq,RetType,X,YY),Y=[YY].

/*
eval(Eq,RetType,Depth,Self,[F|X],Y):-
  (F=='superpose' ; ( option_value(no_repeats,false))),
  notrace((D1 is Depth-1)),!,
  eval_11(Eq,RetType,D1,Self,[F|X],Y).
*/

eval(Eq,RetType,Depth,Self,X,Y):- atom(Eq),  ( Eq \== ('=')) ,!,
   call(Eq,'=',RetType,Depth,Self,X,Y).

eval(Eq,RetType,Depth,Self,X,Y):-
  %notrace(allow_repeats_eval_(X)),
  !,
  eval_11(Eq,RetType,Depth,Self,X,Y).
/*
eval(Eq,RetType,Depth,Self,X,Y):-
  nop(notrace((no_repeats_var(YY)),
  D1 is Depth-1)),!,
  eval_11(Eq,RetType,D1,Self,X,Y),
   notrace(( \+ (Y\=YY))).

allow_repeats_eval_(_):- !.
allow_repeats_eval_(_):- option_value(no_repeats,false),!.
allow_repeats_eval_(X):- \+ is_list(X),!,fail.
allow_repeats_eval_([F|_]):- atom(F),allow_repeats_eval_f(F).
allow_repeats_eval_f('superpose').
allow_repeats_eval_f('collapse').
*/
debugging_metta(G):- notrace((is_debugging((eval))->ignore(G);true)).


:- nodebug(metta(eval)).


w_indent(Depth,Goal):-
  \+ \+ notrace(ignore(((
    format('~N'),
    setup_call_cleanup(forall(between(Depth,101,_),write('  ')),Goal, format('~N')))))).
indentq(Depth,Term):-
  \+ \+ notrace(ignore(((
    format('~N'),
    setup_call_cleanup(forall(between(Depth,101,_),write('  ')),format('~q',[Term]),
    format('~N')))))).


indentq(DR,EX,AR,retval(Term)):-nonvar(Term),!,indentq(DR,EX,AR,Term).
indentq(DR,EX,AR,Term):-
  \+ \+
   color_g_mesg('#2f2f2f',
      notrace(ignore((( format('~N;'),
      format('~` t~d~5|:', [EX]),
      format('~` t~d~8|', [DR]),
      forall(between(1,DR,_),write('     |')),write('-'),write(AR),with_indents(false,write_src(Term)),
    format('~N')))))).


with_debug(Flag,Goal):- is_debugging(Flag),!, call(Goal).
with_debug(Flag,Goal):- flag(eval_num,_,0),
  setup_call_cleanup(set_debug(Flag,true),call(Goal),set_debug(Flag,false)).

flag_to_var(Flag,Var):- atom(Flag), \+ atom_concat('trace-on-',_,Flag),!,atom_concat('trace-on-',Flag,Var).
flag_to_var(metta(Flag),Var):- !, nonvar(Flag), flag_to_var(Flag,Var).
flag_to_var(Flag,Var):- Flag=Var.

set_debug(Flag,Val):- \+ atom(Flag), flag_to_var(Flag,Var), atom(Var),!,set_debug(Var,Val).
set_debug(Flag,true):- !, debug(metta(Flag)),flag_to_var(Flag,Var),set_option_value(Var,rtrace).
set_debug(Flag,false):- nodebug(metta(Flag)),flag_to_var(Flag,Var),set_option_value(Var,true).
if_trace((Flag;true),Goal):- !, notrace(( catch_err(ignore((Goal)),E,fbug(E-->if_trace((Flag;true),Goal))))).
if_trace(Flag,Goal):- notrace((catch_err(ignore((is_debugging(Flag),Goal)),E,fbug(E-->if_trace(Flag,Goal))))).


%maybe_efbug(SS,G):- efbug(SS,G)*-> if_trace(eval,fbug(SS=G)) ; fail.
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
   (option_value(Var,rtrace)->true;(Flag\==Var -> is_debugging(Var))).

:- nodebug(metta(overflow)).


eval_99(Eq,RetType,Depth,Self,X,Y):-
  eval_20(_Nth,Eq,RetType,Depth,Self,X,Y)*->true;eval_failed(Depth,Self,X,Y).



eval_00(_Eq,_RetType,Depth,_Slf,X,Y):- Depth<1,!,X=Y, (\+ trace_on_overflow-> true; flag(eval_num,_,0),
  debug(metta(eval))).
eval_00(_Eq,_RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_00(Eq,RetType,Depth,Self,X,YO):-
  Depth2 is Depth-1,
  Nth = nth(0),
  copy_term(X, XX),!,
  eval_20(Nth,Eq,RetType,Depth2,Self,X,M),
  eval_01(Eq,RetType,Depth2,Self,XX,M,Y),
  notrace(eval_03(Nth,Eq,RetType,Depth2,Self,_XX,Y,YO)),
  arg(1,Nth,Was), Was1 is Was+1, nb_setarg(1,Nth,Was1).

eval_01(Eq,RetType,Depth,Self,XX,M,YO):- notrace(((M\=@=XX,  \+ self_eval(M)))),!,eval_02(Eq,RetType,Depth,Self,XX,M,YO).
eval_01(_Eq,_RetType,_Depth,_Self,_,M,M).


eval_02(Eq,RetType,Depth,Self,_XX,M,YO):- eval_00(Eq,RetType,Depth,Self,M,Y)*-> Y=YO; YO=M.


eval_03(_Nth,Eq,RetType,Depth2,Self,_XX,Y,YO):-
  once(if_or_else((subst_args(Eq,RetType,Depth2,Self,Y,YO)),
     if_or_else(finish_eval(Eq,RetType,Depth2,Self,Y,YO),
          Y=YO))).



eval_11(_Eq,_RetType,_Dpth,_Slf,X,Y):- self_eval(X),!,Y=X.
eval_11(Eq,RetType,Depth,Self,X,Y):- \+ is_debugging((eval)),!,
  D1 is Depth-1,
  eval_00(Eq,RetType,D1,Self,X,Y).
eval_11(Eq,RetType,Depth,Self,X,Y):-
 notrace((

  flag(eval_num,EX,EX+1),
  D1 is Depth-1,
  DR is 99-D1,
  PrintRet = _,
  option_else('trace-length',Max,100),
  if_t((EX>Max), (set_debug(eval,false),MaxP1 is Max+1, set_debug(overflow,false),
      format('; Switched off tracing. For a longer trace: !(pragma! trace-length ~w)',[MaxP1]))),
  nop(notrace(no_repeats_var(YY))),

  if_t(DR<20,if_trace((eval),(PrintRet=1, indentq(DR,EX, '-->',eval(X))))),
  Ret=retval(fail))),

  call_cleanup((
    (eval_00(Eq,RetType,D1,Self,X,Y)),
    notrace(( \+ (Y\=YY), nb_setarg(1,Ret,Y)))),

    (PrintRet==1 -> indentq(DR,EX,'<--',Ret) ;
    notrace(ignore(((Y\=@=X,
      if_t(DR<20,if_trace((eval),indentq(DR,EX,'<--',Ret))))))))),

  (Ret\=@=retval(fail)->true;(rtrace(eval_00(Eq,RetType,D1,Self,X,Y)),fail)).


:- discontiguous eval_20/7.
:- discontiguous eval_40/6.

% =================================================================
% =================================================================
% =================================================================
%  VAR HEADS/ NON-LISTS
% =================================================================
% =================================================================
% =================================================================

eval_20(_Nth,Eq,RetType,Depth,Self,X,Y):- maybe_redirect(Eq,RetType,Depth,Self,X,Y),
  fail,
   eval_50(Eq,RetType,Depth,Self,X,Y).

maybe_redirect(_Eq,_RetType,_Dpth,_Slf,X,_Y):-  \+ is_list(X).
maybe_redirect(_Eq,_RetType,_Dpth,_Slf,[X|_],_Y):-  \+ atom(X).
maybe_redirect(_Eq,_RetType,_Dpth,_Slf,X,_Y):- self_eval(X).

eval_20(_Nth,Eq,RetType,_Dpth,_Slf,[X|T],Y):- T==[], \+ callable(X),!, do_expander(Eq,RetType,X,YY),Y=[YY].
%eval_20(_Nth,Eq,RetType,_Dpth,Self,[X|T],Y):- T==[],  atom(X),
%   \+ is_user_defined_head_f(Self,X),
%   do_expander(Eq,RetType,X,YY),!,Y=[YY].

eval_20(_Nth,Eq,RetType,Depth,Self,X,Y):- atom(Eq),  ( Eq \== ('=')) ,!,
   call(Eq,'=',RetType,Depth,Self,X,Y).


eval_20(_Nth,Eq,RetType,Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval(Eq,RetType,Depth,Self,VI,VM),
  ( VM\==VI -> eval(Eq,RetType,Depth,Self,[V|VM],VVO) ;
    (eval(Eq,RetType,Depth,Self,V,VV), (V\==VV -> eval(Eq,RetType,Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_20(_Nth,Eq,RetType,_Dpth,_Slf,X,Y):- \+ is_list(X),!,do_expander(Eq,RetType,X,Y).

eval_20(_Nth,Eq,_RetType,Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval(Eq,_ArgRetType,Depth,Self),VI,VO).

eval_20(_Nth,_Eq,_RetType,_Depth,_Self,[ConceptNode,X],X):-notrace(( atom(ConceptNode),atom_concat(_Concept,'Node',ConceptNode))),!.


% =================================================================
% =================================================================
% =================================================================
%  TRACE/PRINT
% =================================================================
% =================================================================
% =================================================================

eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['repl!'],Y):- !,  repl,check_returnval(Eq,RetType,Y).
eval_20(_Nth,Eq,RetType,Depth,Self,['!',Cond],Res):- !, call(eval(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(_Nth,Eq,RetType,Depth,Self,['rtrace!',Cond],Res):- !, rtrace(eval(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(_Nth,Eq,RetType,Depth,Self,['trace',Cond],Res):- !, with_debug(eval,eval(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(_Nth,Eq,RetType,Depth,Self,['time',Cond],Res):- !, time_eval(eval(Cond),eval(Eq,RetType,Depth,Self,Cond,Res)).
eval_20(_Nth,Eq,RetType,Depth,Self,['print',Cond],Res):- !, eval(Eq,RetType,Depth,Self,Cond,Res),format('~N'),print(Res),format('~N').
% !(println! $1)
eval_20(_Nth,Eq,RetType,Depth,Self,['println!'|Cond],Res):- !, maplist(eval(Eq,RetType,Depth,Self),Cond,[Res|Out]),
   format('~N'),maplist(write_src,[Res|Out]),format('~N').
eval_20(_Nth,Eq,RetType,Depth,Self,['trace!',A|Cond],Res):- !, maplist(eval(Eq,RetType,Depth,Self),[A|Cond],[AA|Result]),
   last(Result,Res), format('~N'),maplist(write_src,[AA]),format('~N').

%eval_20(_Nth,Eq,RetType,Depth,Self,['trace!',A,B],C):- !,eval(Eq,RetType,Depth,Self,B,C),format('~N'),fbug(['trace!',A,B]=C),format('~N').
%eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['trace!',A],A):- !, format('~N'),fbug(A),format('~N').

eval_20(_Nth,Eq,RetType,_Dpth,_Slf,List,YY):-  notrace((is_list(List),maplist(self_eval,List),List=[H|_], \+ atom(H))), !,Y=List,do_expander(Eq,RetType,Y,YY).



% =================================================================
% =================================================================
% =================================================================
%  UNIT TESTING/assert<STAR>
% =================================================================
% =================================================================
% =================================================================


eval_20(_Nth,Eq,RetType,Depth,Self,['assertTrue', X],TF):- !, eval(Eq,RetType,Depth,Self,['assertEqual',X,'True'],TF).
eval_20(_Nth,Eq,RetType,Depth,Self,['assertFalse',X],TF):- !, eval(Eq,RetType,Depth,Self,['assertEqual',X,'False'],TF).

eval_20(_Nth,Eq,RetType,Depth,Self,['assertEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf(
        ['assertEqual',X,Y],
        (bagof_eval(Eq,RetType,Depth,Self,X,XX), bagof_eval(Eq,RetType,Depth,Self,Y,YY)),
         equal_enough_for_test(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,[expected(_)],YY]).

eval_20(_Nth,Eq,RetType,Depth,Self,['assertNotEqual',X,Y],RetVal):- !,
   loonit_assert_source_tf(
        ['assertEqual',X,Y],
        (bagof_eval(Eq,RetType,Depth,Self,X,XX), bagof_eval(Eq,RetType,Depth,Self,Y,YY)),
         \+ equal_enough(XX,YY), TF),
  (TF=='True'->return_empty(RetVal);RetVal=[got,XX,expected,YY]).

eval_20(_Nth,Eq,RetType,Depth,Self,['assertEqualToResult',X,Y],RetVal):- !,
   loonit_assert_source_tf(
        ['assertEqualToResult',X,Y],
        (bagof_eval(Eq,RetType,Depth,Self,X,XX), sort(Y,YY)),
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
has_let_star(Y):- sub_var('let*',Y).

equal_enough_for_test(X,Y):- is_empty(X),!,is_empty(Y).
equal_enough_for_test(X,Y):- has_let_star(Y),!,\+ is_empty(X).
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

% =================================================================
% =================================================================
% =================================================================
%  SPACE EDITING
% =================================================================
% =================================================================
% =================================================================

eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['new-space'],Space):- !, 'new-space'(Space),check_returnval(Eq,RetType,Space).

eval_20(_Nth,Eq,RetType,Depth,Self,[Op,Space|Args],Res):- is_space_op(Op),!,
  eval_space_start(Eq,RetType,Depth,Self,[Op,Space|Args],Res).


eval_space_start(Eq,RetType,_Depth,_Self,[_Op,_Other,Atom],Res):-
  (Atom == [] ;  Atom =='Empty';  Atom =='Nil'),!,return_empty('False',Res),check_returnval(Eq,RetType,Res).

eval_space_start(Eq,RetType,Depth,Self,[Op,Other|Args],Res):-
  into_space(Depth,Self,Other,Space),
  eval_space(Eq,RetType,Depth,Self,[Op,Space|Args],Res).


eval_space(Eq,RetType,_Dpth,_Slf,['add-atom',Space,PredDecl],Res):- !,
   do_metta(python,load,Space,PredDecl,TF),return_empty(TF,Res),check_returnval(Eq,RetType,Res).

eval_space(Eq,RetType,_Dpth,_Slf,['remove-atom',Space,PredDecl],Res):- !,
   do_metta(python,unload,Space,PredDecl,TF),return_empty(TF,Res),check_returnval(Eq,RetType,Res).

eval_space(Eq,RetType,_Dpth,_Slf,['atom-count',Space],Count):- !,
    ignore(RetType='Number'),ignore(Eq='='),
    findall(Atom, get_metta_atom_from(Space, Atom),Atoms),
    length(Atoms,Count).

eval_space(Eq,RetType,_Dpth,_Slf,['atom-replace',Space,Rem,Add],TF):- !,
   copy_term(Rem,RCopy), as_tf((metta_atom_iter_ref(Space,RCopy,Ref), RCopy=@=Rem,erase(Ref), do_metta(Space,load,Add)),TF),
 check_returnval(Eq,RetType,TF).

eval_space(Eq,RetType,_Dpth,_Slf,['get-atoms',Space],Atom):- !,
  ignore(RetType='Atom'), get_metta_atom_from(Space, Atom), check_returnval(Eq,RetType,Atom).

% Match-ELSE
eval_space(Eq,RetType,Depth,Self,['match',Other,Goal,Template,Else],Template):- !,
  ((eval_space(Eq,RetType,Depth,Self,['match',Other,Goal,Template],Template),
       \+ return_empty([],Template))*->true;Template=Else).

%Match-TEMPLATE
eval_space(Eq,_RetType,Depth,Self,['match',Other,Goal,Template],Res):- !,
   metta_atom_iter(Eq,Depth,Self,Other,Goal),
   Template=Res.

metta_atom_iter(Eq,_Depth,_Slf,Other,[Equal,[F|H],B]):- Eq == Equal,!,  % trace,
   (metta_defn(Eq,Other,[F|H],B)). % once(eval_until_unify(Eq,RetType,Depth,Other,H,HH)).

metta_atom_iter(_Eq,Depth,_,_,_):- Depth<3,!,fail.
metta_atom_iter(Eq,Depth,Self,Other,[And|Y]):- atom(And), is_and(And),!,
  (Y==[] -> true ;  ( D2 is Depth -1, Y = [H|T], metta_atom_iter(Eq,D2,Self,Other,H),metta_atom_iter(Eq,D2,Self,Other,[And|T]))).

metta_atom_iter(Eq,Depth,_Slf,Other,X):- dcall0000000000(eval_args_true(Eq,_RetType,Depth,Other,X)).



metta_atom_iter_ref(Other,H,Ref):-clause(asserted_metta_atom(Other,H),true,Ref).


% =================================================================
% =================================================================
% =================================================================
%  CASE/SWITCH
% =================================================================
% =================================================================
% =================================================================
% Macro: case
:- nodebug(metta(case)).

% if there is only a void then always return nothing for each Case
eval_20(_Nth,Eq,_RetType,Depth,Self,['case',A,[[Void,_]]],Res):-
   '%void%' == Void,
   eval(Eq,_UnkRetType,Depth,Self,A,_),!,Res =[].

% if there is nothing for case just treat like a collapse
eval_20(_Nth,Eq,_RetType,Depth,Self,['case',A,[]],Empty):-
  %forall(eval(Eq,_RetType2,Depth,Self,Expr,_),true),
  once(eval(Eq,_RetType2,Depth,Self,A,_)),
  return_empty([],Empty).

% Macro: case
eval_20(_Nth,Eq,RetType,Depth,Self,['case',A,CL|T],Res):-
   must_det_ll(T==[]),
   into_case_list(CL,CASES),
   findall(Key-Value,
     (nth0(Nth,CASES,Case0),
       (is_case(Key,Case0,Value),
        if_trace(metta(case),(format('~N'),writeqln(c(Nth,Key)=Value))))),KVs),!,
   eval_case(Eq,RetType,Depth,Self,A,KVs,Res).

eval_case(Eq,CaseRetType,Depth,Self,A,KVs,Res):-
   ((eval(Eq,_UnkRetType,Depth,Self,A,AA),
         if_trace((case),(writeqln('case'=A))),
         if_trace(metta(case),writeqln('switch'=AA)),
    (select_case(Depth,Self,AA,KVs,Value)->true;(member(Void -Value,KVs),Void=='%void%')))
     *->true;(member(Void -Value,KVs),Void=='%void%')),
    eval(Eq,CaseRetType,Depth,Self,Value,Res).

  select_case(Depth,Self,AA,Cases,Value):-
     (best_key(AA,Cases,Value) -> true ;
      (maybe_special_keys(Depth,Self,Cases,CasES),
       (best_key(AA,CasES,Value) -> true ;
        (member(Void -Value,CasES),Void=='%void%')))).

  best_key(AA,Cases,Value):-
     ((member(Match-Value,Cases),AA ==Match)->true;
      ((member(Match-Value,Cases),AA=@=Match)->true;
        (member(Match-Value,Cases),AA = Match))).

	into_case_list(CASES,CASES):- is_list(CASES),!.
		is_case(AA,[AA,Value],Value):-!.
		is_case(AA,[AA|Value],Value).

   maybe_special_keys(Depth,Self,[K-V|KVI],[AK-V|KVO]):-
     eval(Depth,Self,K,AK), K\=@=AK,!,
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
eval_20(_Nth,Eq,RetType,Depth,Self,['collapse',List],Res):-!,
 bagof_eval(Eq,RetType,Depth,Self,List,Res).



%[superpose,[1,2,3]]
eval_20(Nth,Eq,RetType,Depth,Self,['superpose',List],Res):- !,
  (((is_user_defined_head(Eq,Self,List) ,eval(Eq,RetType,Depth,Self,List,UList), List\=@=UList)
    *->  eval_20(Nth,Eq,RetType,Depth,Self,['superpose',UList],Res)
       ; ((member(E,List),eval(Eq,RetType,Depth,Self,E,Res))*->true;return_empty([],Res)))),
  \+ Res = 'Empty'.

%[sequential,[1,2,3]]
eval_20(Nth,Eq,RetType,Depth,Self,['sequential',List],Res):- !,
  (((fail,is_user_defined_head(Eq,Self,List) ,eval(Eq,RetType,Depth,Self,List,UList), List\=@=UList)
    *->  eval_20(Nth,Eq,RetType,Depth,Self,['sequential',UList],Res)
       ; ((member(E,List),eval_ne(Eq,RetType,Depth,Self,E,Res))*->true;return_empty([],Res)))).


get_sa_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_sa_p2(P3,E,Cmpd,SA).
get_sa_p2(P3,E,Cmpd,call(P3,N1,Cmpd)):- arg(N1,Cmpd,E).
get_sa_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_sa_p1(P3,E,Arg,SA).


sub_sterm(Sub,Sub).
sub_sterm(Sub,Term):- sub_sterm1(Sub,Term).
sub_sterm1(_  ,List):- \+ compound(List),!,fail.
sub_sterm1(Sub,List):- is_list(List),!,member(SL,List),sub_sterm(Sub,SL).
sub_sterm1(_  ,[_|_]):-!,fail.
sub_sterm1(Sub,Term):- arg(_,Term,SL),sub_sterm(Sub,SL).


% =================================================================
% =================================================================
% =================================================================
%  NOP/EQUALITU/DO
% =================================================================
% =================================================================
% ================================================================
eval_20(_Nth,_Eq,_RetType,_Depth,_Self,['nop'],                 _ ):- !, fail.
eval_20(_Nth,_Eq,_RetType1,Depth,Self,['nop',Expr], Empty):- !,
  ignore(eval('=',_RetType2,Depth,Self,Expr,_)),
  return_empty([], Empty).

eval_20(_Nth,Eq,_RetType1,Depth,Self,['do',Expr], Empty):- !,
  forall(eval(Eq,_RetType2,Depth,Self,Expr,_),true),
  %eval_ne(Eq,_RetType2,Depth,Self,Expr,_),!,
  return_empty([],Empty).


max_counting(F,Max):- flag(F,X,X+1),  X<Max ->  true; (flag(F,_,10),!,fail).
% =================================================================
% =================================================================
% =================================================================
%  if/If
% =================================================================
% =================================================================
% =================================================================



eval_20(_Nth,Eq,RetType,Depth,Self,['if',Cond,Then,Else],Res):- !,
   eval(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> eval(Eq,RetType,Depth,Self,Then,Res)
     ;  eval(Eq,RetType,Depth,Self,Else,Res)).

eval_20(_Nth,Eq,RetType,Depth,Self,['If',Cond,Then,Else],Res):- !,
   eval(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF)
     -> eval(Eq,RetType,Depth,Self,Then,Res)
     ;  eval(Eq,RetType,Depth,Self,Else,Res)).

eval_20(_Nth,Eq,RetType,Depth,Self,['If',Cond,Then],Res):- !,
   eval(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval(Eq,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).

eval_20(_Nth,Eq,RetType,Depth,Self,['if',Cond,Then],Res):- !,
   eval(Eq,'Bool',Depth,Self,Cond,TF),
   (is_True(TF) -> eval(Eq,RetType,Depth,Self,Then,Res) ;
      (!, fail,Res = [],!)).


eval_20(_Nth,Eq,RetType,_Dpth,_Slf,[_,Nothing],NothingO):-
   'Nothing'==Nothing,!,do_expander(Eq,RetType,Nothing,NothingO).

% =================================================================
% =================================================================
% =================================================================
%  LET/LET*
% =================================================================
% =================================================================
% =================================================================



eval_until_unify(_Eq,_RetType,_Dpth,_Slf,X,X):- !.
eval_until_unify(Eq,RetType,Depth,Self,X,Y):- eval_until_eq(Eq,RetType,Depth,Self,X,Y).

eval_until_eq(Eq,RetType,_Dpth,_Slf,X,Y):-  X=Y,check_returnval(Eq,RetType,Y).
%eval_until_eq(Eq,RetType,Depth,Self,X,Y):- var(Y),!,eval_in_steps_or_same(Eq,RetType,Depth,Self,X,XX),Y=XX.
%eval_until_eq(Eq,RetType,Depth,Self,Y,X):- var(Y),!,eval_in_steps_or_same(Eq,RetType,Depth,Self,X,XX),Y=XX.
eval_until_eq(Eq,RetType,Depth,Self,X,Y):- \+is_list(Y),!,eval_in_steps_some_change(Eq,RetType,Depth,Self,X,XX),Y=XX.
eval_until_eq(Eq,RetType,Depth,Self,Y,X):- \+is_list(Y),!,eval_in_steps_some_change(Eq,RetType,Depth,Self,X,XX),Y=XX.
eval_until_eq(Eq,RetType,Depth,Self,X,Y):- eval_in_steps_some_change(Eq,RetType,Depth,Self,X,XX),eval_until_eq(Eq,RetType,Depth,Self,Y,XX).
eval_until_eq(_Eq,_RetType,_Dpth,_Slf,X,Y):- length(X,Len), \+ length(Y,Len),!,fail.
eval_until_eq(Eq,RetType,Depth,Self,X,Y):-  nth1(N,X,EX,RX), nth1(N,Y,EY,RY),
  EX=EY,!, maplist(eval_until_eq(Eq,RetType,Depth,Self),RX,RY).
eval_until_eq(Eq,RetType,Depth,Self,X,Y):-  nth1(N,X,EX,RX), nth1(N,Y,EY,RY),
  ((var(EX);var(EY)),eval_until_eq(Eq,RetType,Depth,Self,EX,EY)),
  maplist(eval_until_eq(Eq,RetType,Depth,Self),RX,RY).
eval_until_eq(Eq,RetType,Depth,Self,X,Y):-  nth1(N,X,EX,RX), nth1(N,Y,EY,RY),
  h((is_list(EX);is_list(EY)),eval_until_eq(Eq,RetType,Depth,Self,EX,EY)),
  maplist(eval_until_eq(Eq,RetType,Depth,Self),RX,RY).

 eval_1change(Eq,RetType,Depth,Self,EX,EXX):-
    eval_20(_Nth,Eq,RetType,Depth,Self,EX,EXX),  EX \=@= EXX.

eval_complete_change(Eq,RetType,Depth,Self,EX,EXX):-
   eval(Eq,RetType,Depth,Self,EX,EXX),  EX \=@= EXX.

eval_in_steps_some_change(_Eq,_RetType,_Dpth,_Slf,EX,_):- \+ is_list(EX),!,fail.
eval_in_steps_some_change(Eq,RetType,Depth,Self,EX,EXX):- eval_1change(Eq,RetType,Depth,Self,EX,EXX).
eval_in_steps_some_change(Eq,RetType,Depth,Self,X,Y):- append(L,[EX|R],X),is_list(EX),
    eval_in_steps_some_change(Eq,RetType,Depth,Self,EX,EXX), EX\=@=EXX,
    append(L,[EXX|R],XX),eval_in_steps_or_same(Eq,RetType,Depth,Self,XX,Y).

eval_in_steps_or_same(Eq,RetType,Depth,Self,X,Y):-eval_in_steps_some_change(Eq,RetType,Depth,Self,X,Y).
eval_in_steps_or_same(Eq,RetType,_Dpth,_Slf,X,Y):- X=Y,check_returnval(Eq,RetType,Y).

  % (fail,return_empty([],Template))).


eval_20(_Nth,Eq,RetType,Depth,Self,['let',A,A5,AA],OO):- !,
  %(var(A)->true;trace),
  ((eval(Eq,RetType,Depth,Self,A5,AE), AE=A)),
    eval(Eq,RetType,Depth,Self,AA,OO).
%eval_20(_Nth,Eq,RetType,Depth,Self,['let',A,A5,AA],AAO):- !,eval(Eq,RetType,Depth,Self,A5,A),eval(Eq,RetType,Depth,Self,AA,AAO).
eval_20(_Nth,Eq,RetType,Depth,Self,['let*',[],Body],RetVal):- !, eval(Eq,RetType,Depth,Self,Body,RetVal).
eval_20(Nth,Eq,RetType,Depth,Self,['let*',[[Var,Val]|LetRest],Body],RetVal):- !,
     eval_20(Nth,Eq,RetType,Depth,Self,['let',Var,Val,['let*',LetRest,Body]],RetVal).


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

%eval_20(_Nth,Eq,RetType,Depth,Self,['colapse'|List], Flat):- !, maplist(eval(Eq,RetType,Depth,Self),List,Res),flatten(Res,Flat).

%eval_20(_Nth,Eq,RetType,Depth,Self,['flatten'|List], Flat):- !, maplist(eval(Eq,RetType,Depth,Self),List,Res),flatten(Res,Flat).


eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['car-atom',Atom],CAR_Y):- !, Atom=[CAR|_],!,do_expander(Eq,RetType,CAR,CAR_Y).
eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['cdr-atom',Atom],CDR_Y):- !, Atom=[_|CDR],!,do_expander(Eq,RetType,CDR,CDR_Y).

eval_20(_Nth,Eq,RetType,Depth,Self,['Cons', A, B ],['Cons', AA, BB]):- no_cons_reduce, !,
  eval(Eq,RetType,Depth,Self,A,AA), eval(Eq,RetType,Depth,Self,B,BB).

eval_20(_Nth,Eq,RetType,Depth,Self,['Cons', A, B ],[AA|BB]):- \+ no_cons_reduce, !,
   eval(Eq,RetType,Depth,Self,A,AA), eval(Eq,RetType,Depth,Self,B,BB).



% =================================================================
% =================================================================
% =================================================================
%  STATE EDITING
% =================================================================
% =================================================================
% =================================================================

eval_20(_Nth,Eq,RetType,Depth,Self,['change-state!',StateExpr, UpdatedValue], Ret):- !,
 call_in_shared_space(((eval(Eq,RetType,Depth,Self,StateExpr,StateMonad),
  eval(Eq,RetType,Depth,Self,UpdatedValue,Value),  'change-state!'(Depth,Self,StateMonad, Value, Ret)))).
eval_20(_Nth,Eq,RetType,Depth,Self,['new-state',UpdatedValue],StateMonad):- !,
  call_in_shared_space(((eval(Eq,RetType,Depth,Self,UpdatedValue,Value),  'new-state'(Depth,Self,Value,StateMonad)))).
eval_20(_Nth,Eq,RetType,Depth,Self,['get-state',StateExpr],Value):- !,
  call_in_shared_space((eval(Eq,RetType,Depth,Self,StateExpr,StateMonad), 'get-state'(StateMonad,Value))).

call_in_shared_space(G):- call_in_thread(main,G).

% eval_20(_Nth,Eq,RetType,Depth,Self,['get-state',Expr],Value):- !, eval(Eq,RetType,Depth,Self,Expr,State), arg(1,State,Value).



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
    call_in_shared_space(nb_current(Name, State)).

% Register and initialize a new state
init_state(Name) :-
    State = 'State'(_,_),
    asserta(is_registered_state(Name)),
    call_in_shared_space(nb_setval(Name, State)).

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

eval_20(_Nth,Eq,RetType,Depth,Self,['get-type',Val],TypeO):- !, get_type(Depth,Self,Val,Type),ground(Type),Type\==[], Type\==Val,!,
  do_expander(Eq,RetType,Type,TypeO).








% =================================================================
% =================================================================
% =================================================================
%  IMPORT/BIND
% =================================================================
% =================================================================
% =================================================================
nb_bind(Name,Value):- nb_current(Name,Was),same_term(Value,Was),!.
nb_bind(Name,Value):- call_in_shared_space(nb_setval(Name,Value)),!.
eval_20(_Nth,Eq,RetType,Depth,Self,['import!',Other,File],RetVal):-
     (( into_space(Depth,Self,Other,Space),!, include_metta(Space,File),!,return_empty(Space,RetVal))),
     check_returnval(Eq,RetType,RetVal). %RetVal=[].
eval_20(_Nth,Eq,RetType,_Depth,_Slf,['bind!',Other,['new-space']],RetVal):- atom(Other),!,assert(was_asserted_space(Other)),
  return_empty([],RetVal), check_returnval(Eq,RetType,RetVal).
eval_20(_Nth,Eq,RetType,Depth,Self,['bind!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,eval(Eq,RetType,Depth,Self,Expr,Value),
    nb_bind(Name,Value),  return_empty(Value,RetVal))),
   check_returnval(Eq,RetType,RetVal).
eval_20(_Nth,Eq,RetType,Depth,Self,['pragma!',Other,Expr],RetVal):-
   must_det_ll((into_name(Self,Other,Name),!,nd_ignore((eval(Eq,RetType,Depth,Self,Expr,Value),
   set_option_value(Name,Value))),  return_empty(Value,RetVal),
    check_returnval(Eq,RetType,RetVal))).
eval_20(_Nth,Eq,RetType,_Dpth,Self,['transfer!',File],RetVal):- !, must_det_ll((include_metta(Self,File),
   return_empty(Self,RetVal),check_returnval(Eq,RetType,RetVal))).


nd_ignore(Goal):- call(Goal)*->true;true.









% =================================================================
% =================================================================
% =================================================================
%  AND/OR
% =================================================================
% =================================================================
% =================================================================

is_True(T):- T\=='False',T\==[].
%is_False(T):- once(T=='False';T==[]).

is_and(S):- \+ atom(S),!,fail.
is_and(',').
is_and(S):- is_and(S,_).

is_and(S,_):- \+ atom(S),!,fail.
is_and('and','True').
is_and('and2','True').
is_and('#COMMA','True').
is_and(',' , 'EmptyType').  % is_and('And').

%eval_20(_Nth,Eq,RetType,Depth,Self,[And,X,Y],TF):-  is_and(And,True),!,
  %as_tf((eval_args(Eq,RetType,Depth,Self,X,True),
   %         eval_args(Eq,RetType,Depth,Self,Y,True)),TF).

%eval_20(_Nth,Eq,RetType,Depth,Self,[And,X],True):- is_and(And,True),!, eval_args(Eq,RetType,Depth,Self,X,True).

eval_20(_Nth,Eq,RetType,_Dpth,_Slf,[And],TF):- is_and(And,_), !, \+ is_False(TF), check_returnval(Eq,RetType,TF), ignore(TF='True').
eval_20(_Nth,Eq,RetType,Depth,Self,[And,X|Y],TF):- is_and(And,_True),!,
  eval_args_true(Eq,RetType,Depth,Self,X,TF),
  eval_args(Eq,RetType,Depth,Self,[And|Y],_TF).

eval_20(_Nth,Eq,RetType,Depth,Self,['or',X,Y],TF):- !,
   as_tf((eval_args_true(Eq,RetType,Depth,Self,X);eval_args_true(Eq,RetType,Depth,Self,Y)),TF).

eval_20(_Nth,Eq,RetType,Depth,Self,['not',X],TF):- !,
   as_tf(( \+ eval_args_true(Eq,RetType,Depth,Self,X)), TF).

eval_20(_Nth,Eq,RetType,Depth,Self,['number-of!',X],N):- !,
   bagof_eval(Eq,RetType,Depth,Self,X,ResL),
   length(ResL,N), ignore(RetType='Number').

eval_20(_Nth,Eq,RetType,Depth,Self,['number-of!',X,N],TF):- !,
   bagof_eval(Eq,RetType,Depth,Self,X,ResL),
   length(ResL,N), true_type(Eq,RetType,TF).

eval_args_true(Eq,RetType,Depth,Self,X,TF1):-
  ((eval_ne(Eq,RetType,Depth,Self,X,TF1),  \+  is_False(TF1));
     ( \+  is_False(TF1),metta_atom_true(Eq,Depth,Self,Self,X))).

eval_args_true(Eq,RetType,Depth,Self,X):-
  metta_atom_true(Eq,Depth,Self,Self,X);  (eval_ne(Eq,RetType,Depth,Self,X,TF1),  \+  is_False(TF1)).

% is this OK?
metta_atom_true(Eq,Depth,Self,Other,H):- metta_defn(Eq,Other,H,B), D2 is Depth -1, metta_atom_true(Eq,D2,Self,Other,B).
metta_atom_true(Eq,_Dpth,_Slf,Other,H):- get_metta_atom(Eq,Other,H).

% =================================================================
% =================================================================
% =================================================================
%  DATA FUNCTOR
% =================================================================
% =================================================================
% =================================================================
eval20_failked(Eq,RetType,Depth,Self,[V|VI],[V|VO]):-
    nonvar(V),is_metta_data_functor(V),is_list(VI),!,
    maplist(eval(Eq,RetType,Depth,Self),VI,VO).
eval20_failed(Eq,RetType,Depth,Self, Term, Res):-
  notrace(( get_sa_p1(setarg,ST,Term,P1), % ST\==Term,
   compound(ST), ST = [F,List],F=='superpose',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !,
   %max_counting(F,20),
   member(Var,List),
   eval(Eq,RetType,Depth,Self, Term, Res).
eval20_failed_2(Eq,RetType,Depth,Self, Term, Res):-
   notrace(( get_sa_p1(setarg,ST,Term,P1),
   compound(ST), ST = [F,List],F=='collapse',nonvar(List), %maplist(atomic,List),
   call(P1,Var))), !, bagof_eval(Eq,RetType,Depth,Self,List,Var),
   eval(Eq,RetType,Depth,Self, Term, Res).


% =================================================================
% =================================================================
% =================================================================
%  MeTTaLog Extras
% =================================================================
% =================================================================
% =================================================================

eval_20(_Nth,_Eq,_RetType1,_Depth,_Self,['call',S], TF):- !, eval_call(S,TF).
eval_20(_Nth,Eq,RetType,Depth,Self,['eval',S], Res):-  !, eval(Eq,RetType,Depth,Self,S, Res).

eval_20(_Nth,Eq,RetType,Depth,Self,PredDecl,Res):-
  Do_more_defs = do_more_defs(true),
  clause(eval_21(Eq,RetType,Depth,Self,PredDecl,Res),Body),
  Do_more_defs == do_more_defs(true),
  call_ndet(Body,DET),
  nb_setarg(1,Do_more_defs,false),
 (DET==true -> ! ; true).


eval_21(Eq,RetType,Depth,Self,['CollapseCardinality',List],Len):-!,
 bagof_eval(Eq,RetType,Depth,Self,List,Res),
 length(Res,Len).
/*
eval_21(_Eq,_RetType,_Depth,_Self,['TupleCount', [N]],N):- number(N),!.


eval_21(Eq,RetType,Depth,Self,['TupleCount',List],Len):-!,
 bagof_eval(Eq,RetType,Depth,Self,List,Res),
 length(Res,Len).
*/


eval_20(_Nth,Eq,RetType,Depth,Self,['length',L],Res):- !, eval(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).
eval_20(_Nth,Eq,RetType,Depth,Self,['CountElement',L],Res):- !, eval(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1).

eval_20(_Nth,Eq,_ListOfRetType,Depth,Self,['TupleConcat',A,B],OO):- fail, !,
    eval(Eq,RetType,Depth,Self,A,AA),
    eval(Eq,RetType,Depth,Self,B,BB),
    append(AA,BB,OO).
eval_20(Nth,Eq,OuterRetType,Depth,Self,['range',A,B],OO):- (is_list(A);is_list(B)),
  ((eval(Eq,RetType,Depth,Self,A,AA),
    eval(Eq,RetType,Depth,Self,B,BB))),
    ((AA+BB)\=@=(A+B)),
    eval_20(Nth,Eq,OuterRetType,Depth,Self,['range',AA,BB],OO),!.

%eval_20(_Nth,Eq,RetType,Depth,Self,['colapse'|List], Flat):- !, maplist(eval(Eq,RetType,Depth,Self),List,Res),flatten(Res,Flat).



% =================================================================
% =================================================================
% =================================================================
%  METTLOG PREDEFS
% =================================================================
% =================================================================
% =================================================================

eval_20(_Nth,Eq,RetType,Depth,Self,['length',L],Res):- !, eval(Depth,Self,L,LL),
   (is_list(LL)->length(LL,Res);Res=1),
   check_returnval(Eq,RetType,Res).

eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['arity',F,A],TF):- !,as_tf(current_predicate(F/A),TF),check_returnval(Eq,RetType,TF).
eval_20(_Nth,Eq,RetType,Depth,Self,['CountElement',L],Res):- !, eval(Eq,RetType,Depth,Self,L,LL), !, (is_list(LL)->length(LL,Res);Res=1),check_returnval(Eq,RetType,Res).
eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['make_list',List],MettaList):- !, into_metta_cons(List,MettaList),check_returnval(Eq,RetType,MettaList).




eval_20(_Nth,Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ResL).
eval_20(_Nth,Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1,ArgL2],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ResL).
eval_20(_Nth,Eq,RetType,Depth,Self,['maplist!',Pred,ArgL1,ArgL2,ArgL3],ResL):- !,
      maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ResL).

  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Res):-
      eval(Eq,RetType,Depth,Self,[Pred,Arg1],Res).
  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Arg2,Res):-
      eval(Eq,RetType,Depth,Self,[Pred,Arg1,Arg2],Res).
  eval_pred(Eq,RetType,Depth,Self,Pred,Arg1,Arg2,Arg3,Res):-
      eval(Eq,RetType,Depth,Self,[Pred,Arg1,Arg2,Arg3],Res).

eval_20(_Nth,Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1],ResL):- !,
      concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ResL).
eval_20(_Nth,Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1,ArgL2],ResL):- !,
      concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ResL).
eval_20(_Nth,Eq,RetType,Depth,Self,['concurrent-maplist!',Pred,ArgL1,ArgL2,ArgL3],ResL):- !,
      concurrent_maplist(eval_pred(Eq,RetType,Depth,Self,Pred),ArgL1,ArgL2,ArgL3,ResL).
eval_20(_Nth,Eq,RetType,Depth,Self,['concurrent-forall!',Gen,Test|Options],Empty):- !,
      maplist(s2p,Options,POptions),
      call(thread:concurrent_forall(
            user:eval_ne(Eq,RetType,Depth,Self,Gen,_),
            user:forall(eval(Eq,RetType,Depth,Self,Test,_),true),
            POptions)),
     return_empty([],Empty).
eval_20(_Nth,Eq,RetType,Depth,Self,['hyperpose',ArgL1],ResL):- !, concurrent_maplist(eval(Eq,RetType,Depth,Self),ArgL1,ResL).


eval_20(_Nth,Eq,RetType,_Dpth,_Slf,['==',X,Y],Res):-  !,
    suggest_type(RetType,'Bool'),
    eq_unify(Eq,_SharedType, X, Y, Res).

eq_unify(_Eq,_SharedType, X, Y, TF):- as_tf(X=:=Y,TF),!.
eq_unify(_Eq,_SharedType, X, Y, TF):- as_tf( '#='(X,Y),TF),!.
eq_unify( Eq,  SharedType, X, Y, TF):- as_tf(eval_until_unify(Eq,SharedType, X, Y), TF).


suggest_type(_RetType,_Bool).


true_type(_,_,_).

eval_20(Nth,Eq,RetType,Depth,Self,PredDecl,Res):-
  if_or_else((eval_30(Eq,RetType,Depth,Self,PredDecl,Res)),
       if_or_else(eval_40(Eq,RetType,Depth,Self,PredDecl,Res),
            if_or_else(eval_60(Eq,RetType,Depth,Self,PredDecl,Res),
                 (Nth==nth(0),fail,eval_failed(Eq,RetType,Depth,Self,PredDecl,Res))))).

eval_30(Eq,RetType,Depth,Self,PredDecl,Res):-
  Do_more_defs = do_more_defs(true),
  clause(eval_80(Eq,RetType,Depth,Self,PredDecl,Res),Body),
  (arg(1,Do_more_defs,true)-> true ; (!,fail)),
  call_ndet(show_ndet(Body),DET),
  (DET==true -> ! ; true),
  nb_setarg(1,Do_more_defs,false).

  %trace,

%eval_80(Eq,RetType,Depth,Self,PredDecl,Res):- fail.
eval_80(Eq,RetType,Depth,Self,PredDecl,Res):- eval_math(Eq,RetType,Depth,Self,PredDecl,Res).
eval_80(Eq,RetType,Depth,Self,PredDecl,Res):- eval_sys_predicate(Eq,RetType,Depth,Self,PredDecl,Res).
eval_80(Eq,RetType,Depth,Self,PredDecl,Res):- eval_sys_function(Eq,RetType,Depth,Self,PredDecl,Res).
%eval_80(Eq,RetType,Depth,Self,PredDecl,Res):- fail.

% =================================================================
% =================================================================
% =================================================================
%  EVAL FAILED
% =================================================================
% =================================================================
% =================================================================


eval_failed(Eq,RetVal,Depth,Self,T,TT):-
  finish_eval(Eq,RetVal,Depth,Self,T,TT),!.

finish_eval(_Eq,_RetVal,_Dpth,_Slf,T,TT):- \+ compound(T),!,TT=T.
%finish_eval(Eq,RetVal,_Dpth,_Slf,[],[]):-!.
%finish_eval(Eq,RetVal,_Dpth,_Slf,[F|LESS],Res):- once(Eq,RetVal,eval_selfless(Eq,RetVal,[F|LESS],Res)),notrace(Eq,RetVal,[F|LESS]\==Res),!.
%finish_eval(Eq,RetVal,Depth,Self,[V|Nil],[O]):- Nil==[], once(Eq,RetVal,eval(Eq,RetVal,Eq,RetType,Depth,Self,V,O)),V\=@=O,!.
finish_eval(Eq,RetVal,Depth,Self,[H|T],[HH|TT]):- !,
  eval(Eq,RetVal,Depth,Self,H,HH),
  finish_eval(Eq,RetVal,Depth,Self,T,TT).
finish_eval(Eq,RetVal,Depth,Self,T,TT):- eval(Eq,RetVal,Depth,Self,T,TT).


 :- discontiguous eval_40/6.

% Macro Functions
%eval_40(Eq,RetType,Depth,_,_,_):- Depth<1,!,fail.
eval_40(_Eq,_RetType,Depth,_,X,Y):- Depth<3, !, fail, ground(X), (Y=X).
eval_40(Eq,RetType,Depth,Self,[F|PredDecl],Res):- fail,
   Depth>1,
   notrace((sub_sterm1(SSub,PredDecl), ground(SSub),SSub=[_|Sub], is_list(Sub), maplist(atomic,SSub))),
   eval(Eq,RetType,Depth,Self,SSub,Repl),
   notrace((SSub\=Repl, subst(PredDecl,SSub,Repl,Temp))),
   eval(Eq,RetType,Depth,Self,[F|Temp],Res).
% =================================================================
% =================================================================
% =================================================================
%  PLUS/MINUS
% =================================================================
% =================================================================
% =================================================================
eval_40(Eq,RetType,Depth,Self,['+',N1,N2],N):- number(N1),!,
   eval(Eq,RetType,Depth,Self,N2,N2Res), notrace(catch_err(N is N1+N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).
eval_40(Eq,RetType,Depth,Self,['-',N1,N2],N):- number(N1),!,
   eval(Eq,RetType,Depth,Self,N2,N2Res), notrace(catch_err(N is N1-N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).
eval_40(Eq,RetType,Depth,Self,['*',N1,N2],N):- number(N1),!,
   eval(Eq,RetType,Depth,Self,N2,N2Res), notrace(catch_err(N is N1*N2Res,_E,(set_last_error(['Error',N2Res,'Number']),fail))).

eval_40(Eq,RetType,Depth,Self,X,Y):-
   fail,
   eval_50(Eq,RetType,Depth,Self,X,Y).

must_eval_args(Eq,RetType,Depth,Self,More,Adjusted):-
   (eval_args(Eq,RetType,Depth,Self,More,Adjusted)*->true;
      (with_debug(eval,eval_args(Eq,RetType,Depth,Self,More,Adjusted))*-> true;
         (
           nl,writeq(eval_args(Eq,RetType,Depth,Self,More,Adjusted)),writeln('.'),
             (More=Adjusted -> true ;
                (trace, throw(must_eval_args(Eq,RetType,Depth,Self,More,Adjusted))))))).


% user defined function
eval_40(Eq,RetType,Depth,Self,[H|PredDecl],Res):- fail,
   %notrace(is_user_defined_head(Self,H)),!,
   eval_60(Eq,RetType,Depth,Self,[H|PredDecl],Res).


eval_50(Eq,RetType,_Dpth,_Slf,Name,Y):-  atom(Name), !,
      (nb_current(Name,X)-> do_expander(Eq,RetType,X,Y); Y = Name).

eval_50(Eq,RetType,_Dpth,_Slf,[X|T],Y):- T==[], \+ callable(X),!, do_expander(Eq,RetType,X,YY),Y=[YY].

eval_50(Eq,RetType,Depth,Self,X,Y):- atom(Eq),  ( (Eq \== ('='), Eq \== ('match'))) ,!,
   if_or_else(call(Eq,'=',RetType,Depth,Self,X,Y),eval_51(Eq,RetType,Depth,Self,X,Y)).

eval_50(Eq,RetType,Depth,Self,X,Y):- eval_51(Eq,RetType,Depth,Self,X,Y).

eval_51(Eq,RetType,Depth,Self,[V|VI],VVO):-  \+ is_list(VI),!,
 eval(Eq,RetType,Depth,Self,VI,VM),
  ( VM\==VI -> eval(Eq,RetType,Depth,Self,[V|VM],VVO) ;
    (eval(Eq,RetType,Depth,Self,V,VV), (V\==VV -> eval(Eq,RetType,Depth,Self,[VV|VI],VVO) ; VVO = [V|VI]))).

eval_51(Eq,RetType,_Dpth,_Slf,X,Y):- \+ is_list(X),!,do_expander(Eq,RetType,X,Y).

eval_51(Eq,_RetType,Depth,Self,[V|VI],[V|VO]):- var(V),is_list(VI),!,maplist(eval(Eq,_ArgRetType,Depth,Self),VI,VO).

% =================================================================
% =================================================================
% =================================================================
% inherited by system
% =================================================================
% =================================================================
% =================================================================
is_system_pred(S):- atom(S),atom_concat(_,'!',S).

eval_math(_Eq,_RetType,_Dpth,_Slf,LESS,Res):-
   notrace((ground(LESS),once((eval_selfless(LESS,Res),notrace(LESS\==Res))))),!.


% predicate inherited by system
eval_sys_predicate(Eq,RetType,_Depth,Self,[AE|More],TF):-
  once((is_system_pred(AE),
  length(More,Len),
  is_syspred(AE,Len,Pred))),
  %trace,  ls,length(More,Len),
  notrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  %show_ndet(adjust_args_mp(Eq,RetType,Depth,Self,Pred,Len,AE,More,Adjusted)),
  %
  More=Adjusted,
  show_ndet(efbug(show_call,eval_call(catch_warn(apply(Pred,Adjusted)),TF))),
  show_ndet(check_returnval(Eq,RetType,TF)),!.

show_ndet(G):- call(G).
%show_ndet(G):- call_ndet(G,DET),(DET==true -> ! ; dmsg(show_ndet(G))).

:- if( \+  current_predicate( adjust_args / 2 )).

   :- discontiguous eval_80/6.

is_user_defined_goal(Self,Head):-
  is_user_defined_head(Self,Head).

:- endif.

adjust_args_mp(_Eq,_RetType,_Depth,_Self,_Pred,_Len,_AE,Args,Adjusted):- Args==[],!,Adjusted=Args.
adjust_args_mp(Eq,RetType,Depth,Self,Pred,Len,AE,Args,Adjusted):-
   functor(P,Pred,Len), predicate_property(P,meta_predicate(Needs)),
   account_needs(1,Needs,Args,More),!,
   adjust_args(Eq,RetType,Depth,Self,AE,More,Adjusted).
adjust_args_mp(Eq,RetType,Depth,Self,_Pred,_Len,AE,Args,Adjusted):-
   adjust_args(Eq,RetType,Depth,Self,AE,Args,Adjusted).

acct(0,A,call(eval(A,_))).
acct(':',A,call(eval(A,_))).
acct(_,A,A).
account_needs(_,_,[],[]).
account_needs(N,Needs,[A|Args],[M|More]):- arg(N,Needs,What),!,
   acct(What,A,M),plus(1,N,NP1),
   account_needs(NP1,Needs,Args,More).

:- nodebug(metta(call)).

eval_call(S,TF):-
  s2p(S,P), !,
    %if_trace(call,dmsg(eval_call_S(S,'$VAR'('TF')))),!,
    if_trace(call,dmsg(eval_call_P(P,'$VAR'('TF')))),!,
  as_tf(P,TF).

eval_sys_function(Eq,RetType,_Depth,Self,[AE|More],Res):-
  is_system_pred(AE),
  length([AE|More],Len),
  is_syspred(AE,Len,Pred),
  notrace( \+ is_user_defined_goal(Self,[AE|More])),!,
  %adjust_args(Depth,Self,AE,More,Adjusted),!,
  %adjust_args_mp(Eq,RetType,Depth,Self,Pred,Len,AE,More,Adjusted),
   More=Adjusted,
  append(Adjusted,[Res],Args),
  efbug(show_call,catch_warn(apply(Pred,Args))),
  check_returnval(Eq,RetType,Res).

:- if( \+  current_predicate( check_returnval / 3 )).
check_returnval(_,_RetType,_TF).
:- endif.

:- if( \+  current_predicate( adjust_args / 5 )).
adjust_args(_Depth,_Self,_V,VI,VI).
:- endif.

%eval_80(Eq,RetType,Depth,Self,PredDecl,Res):- eval_67(Eq,RetType,Depth,Self,PredDecl,Res).



last_element(T,E):- \+ compound(T),!,E=T.
last_element(T,E):- is_list(T),last(T,L),last_element(L,E),!.
last_element(T,E):- compound_name_arguments(T,_,List),last_element(List,E),!.




catch_warn(G):- quietly(catch(G,E,(fbug(catch_warn(G)-->E),fail))).
catch_nowarn(G):- quietly(catch(G,error(_,_),fail)).


as_tf(G,TF):-  G\=[_|_], catch_nowarn((call(G)*->TF='True';TF='False')).
%eval_selfless(['==',X,Y],TF):- as_tf(X=:=Y,TF),!.
%eval_selfless(['==',X,Y],TF):- as_tf(X=@=Y,TF),!.
eval_selfless(['>',X,Y],TF):-!,as_tf(X>Y,TF).
eval_selfless(['<',X,Y],TF):-!,as_tf(X<Y,TF).
eval_selfless(['=>',X,Y],TF):-!,as_tf(X>=Y,TF).
eval_selfless(['<=',X,Y],TF):-!,as_tf(X=<Y,TF).
eval_selfless(['%',X,Y],TF):-!,eval_selfless(['mod',X,Y],TF).

eval_selfless(LIS,Y):-  notrace(( ground(LIS),
   LIS=[F,_,_], atom(F), catch_warn(current_op(_,yfx,F)),
   catch_err((LIS\=[_], s2p(LIS,IS), Y is IS),_,fail))),!.

% less Macro-ey Functions


% =================================================================
% =================================================================
% =================================================================
%  USER DEFINED FUNCTIONS
% =================================================================
% =================================================================
% =================================================================

call_ndet(Goal,DET):- call(Goal),deterministic(DET),(DET==true->!;true).

eval_60(Eq,RetType,Depth,Self,H,B):-
   (eval_64(Eq,RetType,Depth,Self,H,B)*->true;
     (fail,eval_67(Eq,RetType,Depth,Self,H,B))).


%eval_64(Eq,_RetType,_Dpth,Self,H,B):-  Eq='=',!, metta_defn(Eq,Self,H,B).
eval_64(Eq,_RetType,_Dpth,Self,H,B):-
   Eq='match', call(metta_atom(Self,H)),B=H.


eval_64(Eq,RetType,Depth,Self,[[H|Start]|T1],Y):-
   notrace((is_user_defined_head_f(Self,H),is_list(Start))),
   metta_defn(Eq,Self,[H|Start],Left),
   [Left|T1] \=@= [[H|Start]|T1],
   eval(Eq,RetType,Depth,Self,[Left|T1],Y).

eval_64(Eq,_RetType,Depth,Self,[H|Args],B):- % no weird template matchers
  % forall(metta_defn(Eq,Self,[H|Template],_),
  %    maplist(not_template_arg,Template)),
   Eq='=',
   (metta_defn(Eq,Self,[H|Args],B0)*->true;(fail,[H|Args]=B0)),
   light_eval(Depth,Self,B0,B).
    %(eval(Eq,RetType,Depth,Self,B,Y);eval_match(Eq,RetType,Depth,Self,Y)).
% Use the first template match
eval_64(Eq,_RetType,Depth,Self,[H|Args],B):- fail,
   Eq='=',
  (metta_defn(Eq,Self,[H|Template],B0),Args=Template),
  light_eval(Depth,Self,B0,B).


light_eval(_Depth,_Self,B,B).

not_template_arg(TArg):- var(TArg), !, \+ attvar(TArg).
not_template_arg(TArg):- atomic(TArg),!.
%not_template_arg(TArg):- is_list(TArg),!,fail.


% Has argument that is headed by the same function
eval_67(Eq,RetType,Depth,Self,[H1|Args],Res):-
   notrace((append(Left,[[H2|H2Args]|Rest],Args), H2==H1)),!,
   eval(Eq,RetType,Depth,Self,[H2|H2Args],ArgRes),
   notrace((ArgRes\==[H2|H2Args], append(Left,[ArgRes|Rest],NewArgs))),
   eval_60(Eq,RetType,Depth,Self,[H1|NewArgs],Res).

eval_67(Eq,RetType,Depth,Self,[[H|Start]|T1],Y):-
   notrace((is_user_defined_head_f(Self,H),is_list(Start))),
   metta_defn(Eq,Self,[H|Start],Left),
   eval(Eq,RetType,Depth,Self,[Left|T1],Y).

% Has subterm to eval
eval_67(Eq,RetType,Depth,Self,[F|PredDecl],Res):- fail,
   Depth>1,
   quietly(sub_sterm1(SSub,PredDecl)),
   notrace((ground(SSub),SSub=[_|Sub], is_list(Sub),maplist(atomic,SSub))),
   eval(Eq,RetType,Depth,Self,SSub,Repl),
   notrace((SSub\=Repl,subst(PredDecl,SSub,Repl,Temp))),
   eval_60(Eq,RetType,Depth,Self,[F|Temp],Res).



% =================================================================
% =================================================================
% =================================================================
%  AGREGATES
% =================================================================
% =================================================================
% =================================================================

cwdl(DL,Goal):- call_with_depth_limit(Goal,DL,R), (R==depth_limit_exceeded->(!,fail);true).

%bagof_eval(Eq,RetType,Depth,Self,X,L):- bagof_eval(Eq,RetType,_RT,Depth,Self,X,L).


%bagof_eval(Eq,RetType,Depth,Self,X,S):- bagof(E,eval_ne(Eq,RetType,Depth,Self,X,E),S)*->true;S=[].
bagof_eval(_Eq,_RetType,_Dpth,_Slf,X,L):- typed_list(X,_Type,L),!.
bagof_eval(Eq,RetType,Depth,Self,X,L):-
   findall(E,eval_ne(Eq,RetType,Depth,Self,X,E),L).

setof_eval(Depth,Self,X,L):- setof_eval('=',_RT,Depth,Self,X,L).
setof_eval(Eq,RetType,Depth,Self,X,S):- bagof_eval(Eq,RetType,Depth,Self,X,L),
   sort(L,S).


eval_ne(Eq,RetType,Depth,Self,X,O):-
  eval(Eq,RetType,Depth,Self,X,E), \+ var(E), \+ is_empty(E), O=E.




:- ensure_loaded(metta_subst).
