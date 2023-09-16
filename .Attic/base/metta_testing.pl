/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/
:- encoding(iso_latin_1).

:- ensure_loaded(library(occurs)).
%:- ensure_loaded(library(logicmoo_utils)).

% Reset loonit counters
loonit_reset :-
    flag(loonit_failure, _, 0),
    flag(loonit_success, _, 0).

% Increment loonit counters based on goal evaluation
loonit_asserts(G) :-
    call(G), !,
    flag(loonit_success, X, X+1).

loonit_asserts(G) :-
    flag(loonit_failure, X, X+1),
    itrace, G.

% ANSI escape codes for colors
ansi_green(Fmt, Args) :-
    format('\e[32m~w\e[0m', [Fmt]),
    format(Fmt, Args).

ansi_red(Fmt, Args) :-
    format('\e[31m~w\e[0m', [Fmt]),
    format(Fmt, Args).

% Generate loonit report with colorized output
loonit_report :-
    flag(loonit_success, Successes, 0),
    flag(loonit_failure, Failures, 0),
    ansi_format([bold], 'LoonIt Report~n',[]),
    format('------------~n'),
    ansi_format([fg(green)], 'Successes: ~w~n', [Successes]),
    ansi_format([fg(red)], 'Failures: ~w~n', [Failures]).


quick_test:-
  set_prolog_flag(encoding,iso_latin_1),
   forall(quick_test(Test),
                  forall(open_string(Test,Stream),
                    load_metta_stream('&self',Stream))).


:- if(\+ current_predicate(wdmsg/1)).
%wdmsg(P):- format(user_error,'~N~p~n',[P]).
:- endif.



substM(T, F, R, R):- T==F,!.
substM(T, _, _, R):- \+ compound(T),!,R=T.
substM([H1|T1], F, R, [H2|T2]) :- !, substM(H1, F, R, H2), substM(T1, F, R, T2).
substM(C1, F, R, C2) :- C1 =.. [Fn|A1], substM_l(A1,F,R,A2),!, C2 =.. [Fn|A2].
substM_l([], _, _, []).  substM_l([H1|T1], F, R, [H2|T2]) :- substM(H1, F, R, H2), substM_l(T1, F, R, T2).


ppm(Cl):-
  notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).


ncatch(G,E,F):- catch(G,E,F).
mcatch(G,E,F):- catch(G,E,F).
%mcatch(G,E,F):- catch(G,E,(wdmsg(G=E),catch(bt,_,fail),wdmsg(G=E),ignore(call(F)),throw(E))).
%ncatch(G,E,F):- catch(G,E,(wdmsg(G=E),catch(bt,_,fail),wdmsg(G=E),call(G))).
%ncatch(G,E,(F)).



:- meta_predicate(if_t(0,0)).
if_t(IF, THEN) :- call(call,ignore((((IF,THEN))))).
:- meta_predicate(must_ll(0)).
must_ll(G):- md(call,G)*->true;throw(not_at_least_once(G)).
:- meta_predicate(at_least_once(0)).
at_least_once(G):- call(G)*->true;throw(not_at_least_once(G)).

%wraps_each(must_ll,call).
wraps_each(must_det_ll,once).
md_like(MD):- wraps_each(MD,_).

remove_must_det(_):- !,fail.
%remove_must_det(MD):- !.
%remove_must_det(MD):- nb_current(remove_must_det(MD),TF),!,TF==true.
%remove_must_det(MD):- \+ false.

%remove_mds(MD,G,GGG):- compound(G), G = must_det_ll(GG),!,expand_goal(GG,GGG),!.
%remove_mds(MD,G,GGG):- compound(G), G = must_det_l(GG),!,expand_goal(GG,GGG),!.
remove_mds(MD,GG,GO):- sub_term(G,GG),compound(G),compound_name_arg(G,MD,GGGG),subst001(GG,G,GGGG,GGG),remove_mds(MD,GGG,GO).
remove_mds(_,GG,GG).
%remove_mds(MD,G,GG):- compound(G), G = ..[MD,AA], compound(G),removed_term(G,GO),expand_goal(GO,GG).

%never_rrtrace:-!.
never_rrtrace:- nb_current(cant_rrtrace,t),!,notrace.
never_rrtrace:- is_cgi,notrace.


%itrace:- !.
%itrace:- \+ current_prolog_flag(debug,true),!.
itrace:- if_thread_main(trace),!.
ibreak:- if_thread_main(((trace,break))).
%recolor(_,_):- ibreak.

%tc_arg(N,C,E):- compound(C),!,arg(N,C,E).
tc_arg(N,C,E):- catch(arg(N,C,E),Err,
  unrepress_output((bt,wdmsg(tc_arg(N,C,E)=Err),((tracing->true;trace),break,arg(N,C,E))))).






compound_name_arg(G,MD,Goal):- var(G),!,atom(MD),G=..[MD,Goal].
compound_name_arg(G,MD,Goal):- compound(G),!, compound_name_arguments(G,MD,[Goal]).


:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
%user:message_hook(Term, Kind, Lines):- error==Kind, itrace,wdmsg(user:message_hook(Term, Kind, Lines)),trace,fail.
user:message_hook(Term, Kind, Lines):- error==Kind,  wdmsg(user:message_hook(Term, Kind, Lines)),fail.

:- meta_predicate(must_det_ll(0)).
:- meta_predicate(must_det_ll1(1,0)).
:- meta_predicate(md_failed(1,0)).
:- meta_predicate(must_not_error(0)).
%:- meta_predicate(must_det_l(0)).

%:- no_xdbg_flags.
:- meta_predicate(wno_must(0)).

wno_must(G):- locally(nb_setval(no_must_det_ll,t),locally(nb_setval(cant_rrtrace,t),call(G))).

md_maplist(_MD,_,[]):-!.
md_maplist(MD,P1,[H|T]):- call(MD,call(P1,H)), md_maplist(MD,P1,T).

md_maplist(_MD,_,[],[]):-!.
md_maplist(MD,P2,[HA|TA],[HB|TB]):- call(MD,call(P2,HA,HB)), md_maplist(MD,P2,TA,TB).

md_maplist(_MD,_,[],[],[]):-!.
md_maplist(MD,P3,[HA|TA],[HB|TB],[HC|TC]):- call(MD,call(P3,HA,HB,HC)), md_maplist(MD,P3,TA,TB,TC).

%must_det_ll(G):- !, once((/*notrace*/(G)*->true;md_failed(P1,G))).

must_det_ll(X):- md(once,X).


md(P1,G):- remove_must_det(MD), wraps_each(MD,P1),!,call(G).
md(P1,G):- never_rrtrace,!, call(P1,G).
md(P1,G):- /*notrace*/(arc_html),!, ignore(/*notrace*/(call(P1,G))),!.
md(P1,G):- tracing,!, call(P1,G). % once((call(G)*->true;md_failed(P1,G))).
%md(P1,X):- !,must_not_error(X).
md(P1,(X,Goal)):- is_trace_call(X),!,call((itrace,call(P1,Goal))).
md(_, X):- is_trace_call(X),!,itrace.
md(P1, X):- nb_current(no_must_det_ll,t),!,call(P1,X).
md(P1,X):- \+ callable(X), !, throw(md_not_callable(P1,X)).
md(P1,(A*->X;Y)):- !,(must_not_error(A)*->md(P1,X);md(P1,Y)).
md(P1,(A->X;Y)):- !,(must_not_error(A)->md(P1,X);md(P1,Y)).
md(P1,(X,Cut)):- (Cut==(!)),md(P1,X),!.
md(MD,maplist(P1,List)):- !, call(MD,md_maplist(MD,P1,List)).
md(MD,maplist(P2,ListA,ListB)):- !, call(MD,md_maplist(MD,P2,ListA,ListB)).
md(MD,maplist(P3,ListA,ListB,ListC)):- !, call(MD,md_maplist(MD,P3,ListA,ListB,ListC)).
md(P1,(X,Cut,Y)):- (Cut==(!)), !, (md(P1,X),!,md(P1,Y)).
md(P1,(X,Y)):- !, (md(P1,X),md(P1,Y)).
%md(P1,X):- /*notrace*/(ncatch(X,_,fail)),!.
%md(P1,X):- conjuncts_to_list(X,List),List\=[_],!,maplist(must_det_ll,List).
md(_,must_det_ll(X)):- !, must_det_ll(X).
md(_,grid_call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,call(P2,I,O)):- !, must_grid_call(P2,I,O).
%md(P1,(X,Y,Z)):- !, (md(P1,X)->md(P1,Y)->md(P1,Z)).
%md(P1,(X,Y)):- !, (md(P1,X)->md(P1,Y)).
%md(P1,if_t(X,Y)):- !, if_t(must_not_error(X),md(P1,Y)).
md(P1,forall(X,Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).
md(P1,\+ (X, \+ Y)):- !, md(P1,forall(must_not_error(X),must_not_error(Y))).

md(P1,(X;Y)):- !, ((must_not_error(X);must_not_error(Y))->true;md_failed(P1,X;Y)).
md(P1,\+ (X)):- !, (\+ must_not_error(X) -> true ; md_failed(P1,\+ X)).
%md(P1,(M:Y)):- nonvar(M), !, M:md(P1,Y).
md(P1,X):-
  ncatch(must_det_ll1(P1,X),
  md_failed(P1,G,N), % <- ExceptionTerm
   % bubble up and start running
  ((M is N -1, M>0)->throw(md_failed(P1,G,M));(ftrace(X),throw('$aborted')))),!.
%must_det_ll(X):- must_det_ll1(P1,X),!.

must_det_ll1(P1,X):- tracing,!,must_not_error(call(P1,X)),!.
must_det_ll1(P1,once(A)):- !, once(md(P1,A)).
must_det_ll1(P1,X):-
  strip_module(X,M,P),functor(P,F,A),setup_call_cleanup(nop(trace(M:F/A,+fail)),(must_not_error(call(P1,X))*->true;md_failed(P1,X)),
    nop(trace(M:F/A,-fail))),!.

%must_not_error(G):- must(once(G)).

must_not_error(G):- (tracing;never_rrtrace),!,call(G).
must_not_error(G):- notrace(is_cgi),!, ncatch((G),E,((u_dmsg(E=G)))).
%must_not_error(X):- is_guitracer,!, call(X).
%must_not_error(G):- !, call(G).
must_not_error(X):- !,ncatch(X,E,(wdmsg(E=X),trace,ftrace(X))).
must_not_error(X):- ncatch(X,E,(rethrow_abort(E);(/*arcST,*/writeq(E=X),pp(etrace=X),
  trace,
  rrtrace(visible_rtrace([-all,+exception]),X)))).

always_rethrow(E):- never_rrtrace,!,throw(E).
always_rethrow('$aborted').
always_rethrow(md_failed(_,_,_)).

%catch_non_abort(Goal):- cant_rrtrace(Goal).
catch_non_abort(Goal):- catch(cant_rrtrace(Goal),E,rethrow_abort(E)),!.
rethrow_abort(E):- format(user_error,'~N~q~n',[catch_non_abort_or_abort(E)]),fail.
%rethrow_abort(time_limit_exceeded):-!.
rethrow_abort('$aborted'):- !, throw('$aborted'),!,forall(between(1,700,_),sleep(0.01)),writeln(timeout),!,fail.
rethrow_abort(E):- ds,!,format(user_error,'~N~q~n',[catch_non_abort(E)]),!.

cant_rrtrace(Goal):- never_rrtrace,!,call(Goal).
cant_rrtrace(Goal):- setup_call_cleanup(cant_rrtrace,Goal,can_rrtrace).

main_debug:- main_thread,current_prolog_flag(debug,true).
cant_rrtrace:- nb_setval(cant_rrtrace,t).
can_rrtrace:- nb_setval(cant_rrtrace,f).
%md_failed(P1,X):- predicate_property(X,number_of_clauses(1)),clause(X,(A,B,C,Body)), (B\==!),!,must_det_ll(A),must_det_ll((B,C,Body)).
md_failed(P1,G):- never_rrtrace,!,notrace,/*notrace*/(u_dmsg(md_failed(P1,G))),!,throw(md_failed(P1,G,2)).
md_failed(_,_):- never_rrtrace,!,fail.
md_failed(P1,G):- tracing,/*notrace*/(u_dmsg(md_failed(P1,G))),!,fail.
md_failed(P1,G):- main_debug,/*notrace*/(u_dmsg(md_failed(P1,G))),!,throw(md_failed(P1,G,2)).
md_failed(P1,G):- is_cgi,!, u_dmsg(arc_html(md_failed(P1,G))).
md_failed(P1,X):- notrace,is_guitracer,u_dmsg(failed(X))/*,arcST*/,nortrace,atrace, call(P1,X).
md_failed(P1,X):-  u_dmsg(failed(P1,X))/*,arcST*/,nortrace,atrace,
 trace,visible_rtrace([-all,+fail,+call,+exception],X).
% must_det_ll(X):- must_det_ll(X),!.

:- meta_predicate(rrtrace(0)).
rrtrace(X):- rrtrace(etrace,X).

is_guitracer:- getenv('DISPLAY',_), current_prolog_flag(gui_tracer,true).
:- meta_predicate(rrtrace(1,0)).
rrtrace(P1,X):- never_rrtrace,!,nop((u_dmsg(cant_rrtrace(P1,X)))),!,fail.
rrtrace(P1,G):- is_cgi,!, u_dmsg(arc_html(rrtrace(P1,G))),call(P1,G).
rrtrace(P1,X):- notrace, \+ is_guitracer,!,nortrace, /*arcST, sleep(0.5), trace,*/
   (notrace(\+ current_prolog_flag(gui_tracer,true)) -> call(P1,X); (itrace,call(P1,X))).
%rrtrace(_,X):- is_guitracer,!,notrace,nortrace,ncatch(call(call,gtrace),_,true),atrace,call(X).
rrtrace(P1,X):- itrace,!, call(P1,X).

:- meta_predicate(arc_wote(0)).
arc_wote(G):- with_pp(ansi,wote(G)).
arcST:- itrace,arc_wote(bts),itrace.
atrace:- arc_wote(bts).
%atrace:- ignore((stream_property(X,file_no(2)), with_output_to(X,dumpST))),!.

:- meta_predicate(odd_failure(0)).
odd_failure(G):- never_rrtrace,!,call(G).
odd_failure(G):- wno_must(G)*->true;fail_odd_failure(G).

:- meta_predicate(fail_odd_failure(0)).
fail_odd_failure(G):- u_dmsg(odd_failure(G)),rtrace(G), fail.
%fail_odd_failure(G):- call(G)*->true;(u_dmsg(odd_failure(G)),fail,rrtrace(G)).


bts:-
 ensure_loaded(library(prolog_stack)),
 prolog_stack:export(prolog_stack:get_prolog_backtrace_lc/3),
 use_module(library(prolog_stack),[print_prolog_backtrace/2,get_prolog_backtrace_lc/3]),
  /*notrace*/(prolog_stack:call(call,get_prolog_backtrace_lc,8000, Stack, [goal_depth(600)])),
  stream_property(S,file_no(1)), prolog_stack:print_prolog_backtrace(S, Stack),
  ignore((fail, current_output(Out), \+ stream_property(Out,file_no(1)), print_prolog_backtrace(Out, Stack))),!.

my_assertion(G):- my_assertion(call(G),G).

my_assertion(_,G):- call(G),!.
my_assertion(Why,G):- u_dmsg(my_assertion(Why,G)),writeq(Why=goal(G)),nl,!,ibreak.

must_be_free(Free):- plain_var(Free),!.
must_be_free(Free):- \+ nonvar_or_ci(Free),!.
must_be_free(Nonfree):- arcST,u_dmsg(must_be_free(Nonfree)),
  ignore((attvar(Nonfree),get_attrs(Nonfree,ATTS),pp(ATTS))),ibreak,fail.

must_be_nonvar(Nonvar):- nonvar_or_ci(Nonvar),!.
must_be_nonvar(IsVar):- arcST,u_dmsg(must_be_nonvar(IsVar)),ibreak,fail.


% goal_expansion(must_det_l(G),I,must_det_ll(G),O):- nonvar(I),source_location(_,_), nonvar(G),I=O.

%goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_), compound(G), remove_mds(MD,G,GG),I=O.

%:- system:ensure_loaded(library(pfc_lib)).
%:- expects_dialect(pfc).
/*
goal_expansion(Goal,Out):- compound(Goal), tc_arg(N1,Goal,E),
   compound(E), E = set(Obj,Member), setarg(N1,Goal,Var),
   expand_goal((Goal,b_set_dict(Member,Obj,Var)),Out).
*/
get_setarg_p1(P3,E,Cmpd,SA):-  compound(Cmpd), get_setarg_p2(P3,E,Cmpd,SA).
get_setarg_p2(P3,E,Cmpd,SA):- arg(N1,Cmpd,E), SA=call(P3,N1,Cmpd).
get_setarg_p2(P3,E,Cmpd,SA):- arg(_,Cmpd,Arg),get_setarg_p1(P3,E,Arg,SA).

my_b_set_dict(Member,Obj,Var):- set_omemberh(b,Member,Obj,Var).
%nb_set_dict(Member,Obj,Var),
set_omemberh(_,Member,Obj,Var):- !, arc_setval(Obj,Member,Var).
%nb_link_dict(Member,Obj,Var),
%set_omemberh(nb,Member,Obj,Var):- !, nb_set_dict(Member,Obj,Var).
%set_omemberh(link,Member,Obj,Var):- !, nb_link_dict(Member,Obj,Var).
%set_omemberh(How,Member,Obj,Var):- call(call,How,Member,Obj,Var),!.

set_omember(Member,Obj,Var):-  set_omember(b,Member,Obj,Var).

set_omember(How,Member,Obj,Var):-
  must_be_nonvar(Member), must_be_nonvar(Obj),  must_be_nonvar(How),  !,
  set_omemberh(How,Member,Obj,Var),!.



get_kov(K,O,V):- dictoo:is_dot_hook(user,O,K,V),!,o_m_v(O,K,V).
get_kov(K,O,V):- ((get_kov1(K,O,V)*->true;(get_kov1(props,O,VV),get_kov1(K,VV,V)))).

get_kov1(K,O,V):- (is_hooked_obj(O),o_m_v(O,K,V))*->true;get_kov2(K,O,V).
% (get_kov(Prop,VM,Value) -> true ; (get_kov(props,VM,Hashmap),nonvar(Hashmap),must_not_error(nb_get_value(Hashmap,Prop,ValueOOV)),get_oov_value(ValueOOV,Value))).
get_kov2(K,O,V):- is_dict(O),!,get_dict(K,O,OOV),get_oov_value(OOV,V).
get_kov2(K,O,V):- nonvar(K),is_rbtree(O),!,rb_lookup(K,V,O).
get_kov2(K,O,V):- is_rbtree(O),!,rb_in(K,V,OOV),get_oov_value(OOV,V).
%get_kov(K,O,V):- is_rbtree(O),!,nb_rb_get_node(K,O,Node),nb_rb_node_value(Node,V).

get_oov_value(ValueOOV,Value):- compound(ValueOOV),ValueOOV=oov(Value),!.
get_oov_value(Value,Value).


term_expansion_setter(I,O):- maybe_expand_md(must_det_ll,I,O),I\=@=O,!.
term_expansion_setter(I,O):- maybe_expand_md(must_det_ll,I,M),I\=@=M,!,term_expansion_setter(M,O).
term_expansion_setter(Goal,get_kov(Func,Self,Value)):- compound(Goal),
  compound_name_arguments(Goal,'.',[ Self, Func, Value]),var(Value).

term_expansion_setter((Head:-Body),Out):-
   get_setarg_p1(setarg,I,Head,P1), is_setter_syntax(I,Obj,Member,Var,How),
   call(P1,Var),
   BodyCode = (Body, set_omember(How,Member,Obj,Var)),
   % goal_expansion_setter(BodyCode,Goal),
   expand_term((Head:- BodyCode),Out),!.

%term_expansion_setter((Head:-Body),(Head:-GBody)):- goal_expansion_setter(Body,GBody),!.

:- export(term_expansion_setter/2).
:- system:import(term_expansion_setter/2).

%goal_expansion(Goal,'.'(Training, Objs, Obj)):- Goal = ('.'(Training, Objs, A), Obj = V),  var(Obj).

is_setter_syntax(I,_Obj,_Member,_Var,_):- \+ compound(I),!,fail.
is_setter_syntax(set(Obj,Member),Obj,Member,_Var,b).
is_setter_syntax(gset(Obj,Member),Obj,Member,_Var,nb).
is_setter_syntax(hset(How,Obj,Member),Obj,Member,_Var,How).
is_setter_syntax(set(ObjMember),Obj,Member,_Var,b):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(gset(ObjMember),Obj,Member,_Var,nb):- obj_member_syntax(ObjMember,Obj,Member).
is_setter_syntax(hset(How,ObjMember),Obj,Member,_Var,How):- obj_member_syntax(ObjMember,Obj,Member).

obj_member_syntax(ObjMember,Obj,Member):-compound(ObjMember), compound_name_arguments(ObjMember,'.',[Obj,Member]),!.

maybe_expand_md(_MD,I,_):- \+ compound(I),!,fail.
%maybe_expand_md(MD,I,_):- compound(I),!,fail. % THIS DISABLES
% THIS DISABLES
%maybe_expand_md(MD,must_det_ll(GoalL),GoalL):-!.
maybe_expand_md(MD,MDGoal,GoalLO):- compound_name_arg(MDGoal,MD,Goal),!, expand_md(MD,Goal,GoalLO).
maybe_expand_md(MD,maplist(P1,GoalL),GoalLO):- P1 ==MD,!,
  expand_md(MD,GoalL,GoalLO).
maybe_expand_md(MD,maplist(P1,GoalL),GoalLO):- P1 ==MD,!,
  expand_md(MD,GoalL,GoalLO).
maybe_expand_md(MD,I,O):- sub_term(C,I),compound(C), compound_name_arg(C,MD,Goal),
   compound(Goal),Goal=(_,_),
   once((expand_md(MD,Goal,GoalO),substM(I,C,GoalO,O))),I\=@=O.


%maybe_expand_md(MD,I,O):- sub_term(S,I),compound(S),S=must_det_ll(G),
%  once(expand_md(MD,S,M)),M\=S,



expand_md(_MD,Nil,true):- Nil==[],!.
expand_md(_MD,Var,Var):- \+ callable(Var),!.
expand_md(MD,[A|B],(AA,BB)):- assertion(callable(A)), assertion(is_list(B)), !,
  expand_md1(MD,A,AA), expand_md(MD,B,BB).
expand_md(MD,A,AA):- !, expand_md1(MD,A,AA).

prevents_expansion(A):- is_trace_call(A).
is_trace_call(A):- A == trace.
is_trace_call(A):- A == itrace.

skip_expansion(A):- var(A),!,fail.
skip_expansion(!).
skip_expansion(false).
skip_expansion(true).
skip_expansion(C):- compound(C),functor(C,F,A),skip_fa_expansion(F,A).
skip_fa_expansion(once,1).
skip_fa_expansion(call,_).
skip_fa_expansion(if_t,2).

expand_md1(_MD,Var,Var):- \+ callable(Var),!.
expand_md1(_MD,Cut,Cut):-  skip_expansion(Cut),!.
expand_md1(MD,MDAB, AABB):- compound(MDAB), compound_name_arg(MDAB,MD,AB),!, expand_md(MD,AB,AABB).
expand_md1(MD,maplist(P1,A),md_maplist(MD,P1,A)):-!.
expand_md1(MD,maplist(P2,A,B),md_maplist(MD,P2,A,B)):-!.
expand_md1(MD,maplist(P3,A,B,C),md_maplist(MD,P3,A,B,C)):-!.
expand_md1(MD,my_maplist(P1,A),md_maplist(MD,P1,A)):-!.
expand_md1(MD,my_maplist(P2,A,B),md_maplist(MD,P2,A,B)):-!.
expand_md1(MD,my_maplist(P3,A,B,C),md_maplist(MD,P3,A,B,C)):-!.
%expand_md1(MD,Goal,O):- \+ compound(Goal), !,O = must_det_ll(Goal).
%expand_md1(MD,(A,B),((A,B))):- remove_must_det(MD), prevents_expansion(A),!.
%expand_md1(MD,(A,B),must_det_ll((A,B))):- prevents_expansion(A),!.
expand_md1(MD,(A,B),(AA,BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB).
expand_md1(MD,(C*->A;B),(CC*->AA;BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB), expand_must_not_error(C,CC).
expand_md1(MD,(C->A;B),(CC->AA;BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB), expand_must_not_error(C,CC).
expand_md1(MD,(C;B),(CC;BB)):- !, expand_md(MD,B,BB), expand_must_not_error(C,CC).

expand_md1(MD,locally(C,A),locally(C,AA)):- !, expand_md(MD,A,AA).

expand_md1(MD,call_cleanup(A,B),call_cleanup(AA,BB)):- !, expand_md(MD,A,AA), expand_md(MD,B,BB).
expand_md1(MD,setup_call_cleanup(C,A,B),setup_call_cleanup(CC,AA,BB)):- !,
  expand_md(MD,C,CC),expand_md(MD,A,AA), expand_md(MD,B,BB).

expand_md1(MD,M:P, M:AABB):-!,expand_md(MD,P, AABB).

expand_md1(MD,P, AABB) :- predicate_property(P,(meta_predicate( MP ))),
   strip_module(P,_,SP),strip_module(MP,_,SMP), kaggle_arc_1_pred(_,SP),
   \+ skippable_built_in(P),
   SP=..[F|Args],SMP=..[F|Margs],!,
   maplist(expand_meta_predicate_arg(MD),Margs,Args,EArgs),
   AABB=..[F|EArgs].

expand_md1(MD, A, MDAA):- \+ remove_must_det(MD), !, expand_goal(A,AA),!,compound_name_arg(MDAA,MD,AA).
expand_md1(_MD, A, AA):- expand_goal(A,AA),!.

expand_must_not_error(C,C):- remove_must_det(must_not_error),!.
expand_must_not_error(C,CC):- \+ predicate_property(C,meta_predicate(_)),!, CC = must_not_error(C),!.
expand_must_not_error(C,CC):- expand_md(must_not_error, C, CC).

kaggle_arc_1_pred(M,P):-
  predicate_property(M:P,file(F)),
  \+ predicate_property(M:P,imported_from(_)),
  \+ \+ atom_contains(F,'arc_'),
  \+ atom_contains(F,'_pfc'),
  \+ atom_contains(F,'_afc'),
  % \+ atom_contains(F,'_ui_'),
  true.

%meta_builtin(P):- var(P),meta_builtin(P).
%meta_builtin(P):- predicate_property(P,interpreted),predicate_property(P,static).
skippable_built_in(MP):- strip_module(MP,_,P), predicate_property(system:P,built_in),
  once(predicate_property(system:P,iso);predicate_property(system:P,notrace)).
%meta_builtin(P):- predicate_property(P,/*notrace*/), \+ predicate_property(P,nodebug).

expand_meta_predicate_arg(_MD,'?',A,A):-!.
expand_meta_predicate_arg(_MD,'+',A,A):-!.
expand_meta_predicate_arg(_MD,'-',A,A):-!.
expand_meta_predicate_arg(MD, ':',A,AA):- !,expand_md1(MD,A,AA).
expand_meta_predicate_arg(MD,  0,A,AA):- !,expand_md1(MD,A,AA).
%expand_meta_predicate_arg(MD,*,A,AA):- !,expand_md1(MD,A,AA).
expand_meta_predicate_arg(_MD,_,A,A).

goal_expansion_getter(Goal,O):- \+ compound(Goal), !,O = Goal.
goal_expansion_getter(I,O):- md_like(MD),maybe_expand_md(MD,I,O),I\=@=O,!.
goal_expansion_getter(I,O):- md_like(MD),maybe_expand_md(MD,I,M),I\=@=M,!,goal_expansion_getter(M,O).
goal_expansion_getter(Goal,get_kov(Func,Self,Value)):- compound(Goal),
  compound_name_arguments(Goal,'.',[ Self, Func, Value]),var(Value).
goal_expansion_getter(Goal,Out):-
 compound_name_arguments(Goal,F,Args),
 maplist(goal_expansion_getter,Args,ArgsOut),
 compound_name_arguments(Out,F,ArgsOut).

:- export(goal_expansion_getter/2).
:- system:import(goal_expansion_getter/2).


goal_expansion_setter(Goal,_):- \+ compound(Goal), !, fail.


goal_expansion_setter(I,O):- md_like(MD),maybe_expand_md(MD,I,O),I\=@=O,!.
goal_expansion_setter(G,GO):- remove_must_det(MD), !,remove_mds(MD,G,GG),goal_expansion_setter(GG,GO).
%goal_expansion_setter(GG,GO):- remove_must_det(MD), sub_term(G,GG),compound(G),G = must_det_ll(GGGG),subst001(GG,G,GGGG,GGG),!,goal_expansion_setter(GGG,GO).
%goal_expansion_setter((G1,G2),(O1,O2)):- !, expand_goal(G1,O1), expand_goal(G2,O2),!.
goal_expansion_setter(set_omember(A,B,C,D),set_omember(A,B,C,D)):-!.
goal_expansion_setter(set_omember(A,B,C),set_omember(b,A,B,C)):-!.
goal_expansion_setter(Goal,get_kov(Func,Self,Value)):- compound(Goal),
  compound_name_arguments(Goal,'.',[ Self, Func, Value]),var(Value).
goal_expansion_setter(I,O):- md_like(MD),maybe_expand_md(MD,I,M),I\=@=M,!,goal_expansion_setter(M,O).


goal_expansion_setter(Goal,Out):-
   predicate_property(Goal,meta_predicate(_)),!,fail,
   tc_arg(N1,Goal,P), goal_expansion_setter(P,MOut),
   setarg(N1,Goal,MOut), !, expand_goal(Goal, Out).

goal_expansion_setter(Goal,Out):-
   tc_arg(N1,Goal,P),  is_setter_syntax(P,Obj,Member,Var,How),
   setarg(N1,Goal,Var), !, expand_goal((Goal,set_omember(How,Member,Obj,Var)), Out).

goal_expansion_setter(Goal,Out):-
   get_setarg_p1(setarg,I,Goal,P1), compound(I), compound_name_arguments(I,'.',[ Self, Func, Value]),
   call(P1,get_kov(Func,Self,Value)),!,
   expand_goal(Goal,Out).

goal_expansion_setter(Goal,Out):-
   get_setarg_p1(setarg,I,Goal,P1), is_setter_syntax(I,Obj,Member,Var,How),
   call(P1,Var),!,
   expand_goal((Goal,set_omember(How,Member,Obj,Var)),Out).

:- export(goal_expansion_setter/2).
:- system:import(goal_expansion_setter/2).


/*
system:term_expansion((Head:-Goal),I,(Head:-Out),O):- nonvar(I),  compound(Goal),
 goal_expansion_setter(Goal,Out),Goal\=@=Out,I=O,!,
 nop((print(goal_expansion_getter(Goal-->Out)),nl)).
*/
arc_term_expansion1((system:term_expansion((Head:-Body),I,Out,O):-
   nonvar(I),  compound(Head),
     term_expansion_setter((Head:-Body),Out),(Head:-Body)=In,In\==Out,I=O,!,
     nop((print(term_expansion_setter(In-->Out)),nl)))).


%system:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),Goal\==Out,I=O,!,
%  ((print(goal_expansion_getter(Goal-->Out)),nl)).

%user:goal_expansion(Goal,I,Out,O):- compound(Goal),goal_expansion_getter(Goal,Out),Goal\==Out,I=O,!,
%  ((print(goal_expansion_getter(Goal-->Out)),nl)).

arc_term_expansion1((goal_expansion(Goal,I,Out,O):-
   goal_expansion_setter(Goal,Out),Goal\==Out,I=O,!,
  nop((print(goal_expansion_setter(Goal-->Out)),nl)))).

:- export(arc_term_expansions/1).
arc_term_expansions(H:- (current_prolog_flag(arc_term_expansion, true), B)):-
  arc_term_expansion1(H:-B).

:- export(enable_arc_expansion/0).
enable_arc_expansion:-
 forall(arc_term_expansions(Rule),
   (strip_module(Rule,M,Rule0),
     nop(u_dmsg(asserta_if_new(Rule,M,Rule0))),
     asserta_if_new(Rule))),
 set_prolog_flag(arc_term_expansion, true).

:- export(disable_arc_expansion/0).
disable_arc_expansion:-
 forall(arc_term_expansions(Rule),forall(retract(Rule),true)),
 set_prolog_flag(arc_term_expansion, false).

:- multifile(goal_expansion/4).
:- dynamic(goal_expansion/4).

goal_expansion(G,I,GG,O):- nonvar(I),source_location(_,_),
    compound(G),
     (remove_must_det(MD)->remove_mds(MD,G,GG);(md_like(MD),maybe_expand_md(MD,G,GG))),I=O.


/*
 tests for term expander


*/
:- if(debugging(term_expansion)).
:- enable_arc_expansion.
:- style_check(-singleton).
dte:- set(_X.local) = val.
dte:- gset(_X.global) = gval.
dte:- must_det_ll((set(_X.a) = b)).
dte:- must_det_ll(locally(nb_setval(e,X.locally),dte([foo|set(X.tail)]))).
dte:- member(set(V.element),set(V.list)).
dte(set(E.v)):- set(E.that)=v.
:- style_check(+singleton).
:- disable_arc_expansion.
:- listing(dte).
:- endif.






end_of_file. % comment this out once to get these files in your readline history
mf('./1-VSpaceTest.metta').
mf('./2-VSpaceTest.metta').
mf('./3-Learn-Rules.metta').
mf('./4-VSpaceTest.metta').
mf('./5-Learn-Flybase.metta').
mf('./6-Learn-Flybase-Full.metta').
mf('./8-VSpaceTest.metta').
mf('./autoexec.metta').
mf('./data/OBO-Metta/export/Alliance_of_Genome_Resources.metta').
mf('./data/OBO-Metta/export/biosapiens.metta').
mf('./data/OBO-Metta/export/chebi_fb_2023_04.metta').
mf('./data/OBO-Metta/export/DBVAR.metta').
mf('./data/OBO-Metta/export/doid.metta').
mf('./data/OBO-Metta/export/flybase_controlled_vocabulary.metta').
mf('./data/OBO-Metta/export/flybase_stock_vocabulary.metta').
mf('./data/OBO-Metta/export/fly_anatomy.metta').
mf('./data/OBO-Metta/export/fly_development.metta').
mf('./data/OBO-Metta/export/gene_group_FB2023_04.metta').
mf('./data/OBO-Metta/export/go-basic.metta').
mf('./data/OBO-Metta/export/image.metta').
mf('./data/OBO-Metta/export/psi-mi.metta').
mf('./data/OBO-Metta/export/slice.chebi.metta').
mf('./data/OBO-Metta/export/so-simple.metta').
mf('./data/OBO-Metta/export/so.metta').
mf('./data/OBO-Metta/export/SOFA.metta').
mf('./examples/compat/common/BelieveMe.metta').
mf('./examples/compat/common/EqualityType.metta').
mf('./examples/compat/common/EqualityTypeTest.metta').
mf('./examples/compat/common/formula/DeductionFormula.metta').
mf('./examples/compat/common/formula/DeductionFormulaTest.metta').
mf('./examples/compat/common/formula/ImplicationDirectIntroductionFormula.metta').
mf('./examples/compat/common/formula/ModusPonensFormula.metta').
mf('./examples/compat/common/In.metta').
mf('./examples/compat/common/InTest.metta').
mf('./examples/compat/common/List.metta').
mf('./examples/compat/common/ListTest.metta').
mf('./examples/compat/common/Maybe.metta').
mf('./examples/compat/common/MaybeTest.metta').
mf('./examples/compat/common/Num.metta').
mf('./examples/compat/common/NumTest.metta').
mf('./examples/compat/common/OrderedSet.metta').
mf('./examples/compat/common/OrderedSetTest.metta').
mf('./examples/compat/common/Record.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValue.metta').
mf('./examples/compat/common/truthvalue/EvidentialTruthValueTest.metta').
mf('./examples/compat/common/truthvalue/MeasEq.metta').
mf('./examples/compat/common/truthvalue/TemporalTruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValue.metta').
mf('./examples/compat/common/truthvalue/TruthValueTest.metta').
mf('./examples/compat/dependent-types/DeductionDTL.metta').
mf('./examples/compat/dependent-types/DeductionDTLTest.metta').
mf('./examples/compat/dependent-types/DeductionImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTL.metta').
mf('./examples/compat/dependent-types/ImplicationDirectIntroductionDTLTest.metta').
mf('./examples/compat/dependent-types/ModusPonensDTL.metta').
mf('./examples/compat/dependent-types/ModusPonensDTLTest.metta').
mf('./examples/compat/entail/DeductionEntail.metta').
mf('./examples/compat/entail/DeductionEntailTest.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntail.metta').
mf('./examples/compat/entail/ImplicationDirectIntroductionEntailTest.metta').
mf('./examples/compat/equal/DeductionEqual.metta').
mf('./examples/compat/equal/DeductionEqualTest.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqual.metta').
mf('./examples/compat/equal/ImplicationDirectIntroductionEqualTest.metta').
mf('./examples/compat/match/DeductionImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/match/DeductionMatch.metta').
mf('./examples/compat/match/DeductionMatchTest.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatch.metta').
mf('./examples/compat/match/ImplicationDirectIntroductionMatchTest.metta').
mf('./examples/compat/prob-dep-types/inf_order_probs.metta').
mf('./examples/compat/prob-dep-types/prob_dep_types.metta').
mf('./examples/compat/recursion-schemes/src/base.metta').
mf('./examples/compat/recursion-schemes/src/examples/benchmark.metta').
mf('./examples/compat/recursion-schemes/src/examples/expression.metta').
mf('./examples/compat/recursion-schemes/src/schemes.metta').
mf('./examples/compat/synthesis/experiments/non-determinism.metta').
mf('./examples/compat/synthesis/experiments/self-contained-synthesize.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-case.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-let.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-superpose.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-type-checking.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify-test.metta').
mf('./examples/compat/synthesis/experiments/synthesize-via-unify.metta').
mf('./examples/compat/synthesis/experiments/unify-via-case.metta').
mf('./examples/compat/synthesis/experiments/unify-via-let.metta').
mf('./examples/compat/synthesis/Synthesize.metta').
mf('./examples/compat/synthesis/SynthesizeTest.metta').
mf('./examples/compat/synthesis/Unify.metta').
mf('./examples/compat/synthesis/UnifyTest.metta').
mf('./examples/compat/test_scripts/a1_symbols.metta').
mf('./examples/compat/test_scripts/a2_opencoggy.metta').
mf('./examples/compat/test_scripts/a3_twoside.metta').
mf('./examples/compat/test_scripts/b0_chaining_prelim.metta').
mf('./examples/compat/test_scripts/b1_equal_chain.metta').
mf('./examples/compat/test_scripts/b2_backchain.metta').
mf('./examples/compat/test_scripts/b3_direct.metta').
mf('./examples/compat/test_scripts/b4_nondeterm.metta').
mf('./examples/compat/test_scripts/b5_types_prelim.metta').
mf('./examples/compat/test_scripts/c1_grounded_basic.metta').
mf('./examples/compat/test_scripts/c2_spaces.metta').
mf('./examples/compat/test_scripts/c2_spaces_kb.metta').
mf('./examples/compat/test_scripts/c3_pln_stv.metta').
mf('./examples/compat/test_scripts/d1_gadt.metta').
mf('./examples/compat/test_scripts/d2_higherfunc.metta').
mf('./examples/compat/test_scripts/d3_deptypes.metta').
mf('./examples/compat/test_scripts/d4_type_prop.metta').
mf('./examples/compat/test_scripts/d5_auto_types.metta').
mf('./examples/compat/test_scripts/e1_kb_write.metta').
mf('./examples/compat/test_scripts/e2_states.metta').
mf('./examples/compat/test_scripts/e3_match_states.metta').
mf('./examples/compat/test_scripts/f1_imports.metta').
mf('./examples/compat/test_scripts/f1_moduleA.metta').
mf('./examples/compat/test_scripts/f1_moduleB.metta').
mf('./examples/compat/test_scripts/f1_moduleC.metta').
mf('./examples/compat/test_scripts/_e2_states_dia.metta').
mf('./examples/fibo.metta').
mf('./examples/fwgc.metta').
mf('./examples/httpclient.metta').
mf('./examples/NARS.metta').
mf('./examples/NARS_listing.metta').
mf('./examples/RUN_minnars.metta').
mf('./examples/RUN_tests0.metta').
mf('./examples/RUN_tests1.metta').
mf('./examples/RUN_tests2.metta').
mf('./examples/RUN_tests3.metta').
mf('./examples/send-more.metta').
mf('./examples/talk80.metta').
mf('./examples/VRUN_tests0.metta').
mf('./examples/VRUN_tests1.metta').
mf('./examples/VRUN_tests2.metta').
mf('./examples/VRUN_tests3.metta').
mf('./metta_vspace/nm_test.metta').
mf('./metta_vspace/r.metta').
mf('./metta_vspace/test_nspace.metta').
:- forall(mf(H),add_history1(load_metta(H))).
%:- load_metta





end_of_file.



parsing(String, Expr) :- string(String),!,string_codes(String,Codes),phrase(expressions(Expr), Codes).
parsing(String, Expr) :- phrase(expressions(Expr), String).

expressions([E|Es]) -->
    ws, expression(E), ws,
    !, % single solution: longest input match
    expressions(Es).
expressions([]) --> [].

% ws --> ";",until_eol,
ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% A number N is represented as n(N), a symbol S as s(S).

expression(s(A))         --> symbol(Cs), { atom_codes(A, Cs) }.
expression(n(N))         --> number(Cs), { number_codes(N, Cs) }.
expression(List)         --> [L],{is_bracket_lr(L,R)},expressions(List), [R].
expression([s(quote),Q]) --> "'", expression(Q).

number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

digit(D) --> [D], { code_type(D, digit) }.

symbol([A|As]) -->
    [A],
    { is_ok_symbolchar(A) },
    symbolr(As).

symbolr([A|As]) -->
    [A],
    { is_ok_symbolchar(A) ; code_type(A, alnum) },
    symbolr(As).
symbolr([]) --> [].

is_bracket_lr(L,R):- member(LR,["()","{}","[]","\"\""]), nth0(0,LR,L),nth0(1,LR,R).
is_ok_symbolchar(A):- \+ code_type(A, space), \+ code_type(A, white), \+ is_bracket_lr(A,_), \+ is_bracket_lr(_,A).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

codelist_to_forms_i(AsciiCodesList,FormsOut):-
    parsing(AsciiCodesList, Forms0),
    compile_all(Forms0, FormsOut),!.

run(Program, Values) :-
    parsing(Program, Forms0),
    empty_assoc(E),
    compile_all(Forms0, Forms),
    writeq(seeingFormas(Forms)),nl,
    phrase(eval_all(Forms, Values0), [E-E], _),
    maplist(unfunc, Values0, Values).

unfunc(s(S), S).
unfunc(t, t).
unfunc(n(N), N).
unfunc([], []).
unfunc([Q0|Qs0], [Q|Qs]) :- unfunc(Q0, Q), unfunc(Qs0, Qs).

fold([], _, V, n(V)).
fold([n(F)|Fs], Op, V0, V) :- E =.. [Op,V0,F], V1 is E, fold(Fs, Op, V1, V).

compile_all(Fs0, Fs) :- maplist(compile, Fs0, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    compile/2 marks (with 'user/1') calls of user-defined functions.
    This eliminates an otherwise defaulty representation of function
    calls and thus allows for first argument indexing in eval//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

compile(F0, F) :-
    (   F0 = n(_)   -> F = F0
    ;   F0 = s(t)   -> F = t
    ;   F0 = s(nil) -> F = []
    ;   F0 = s(_)   -> F = F0
    ;   F0 = [] -> F = []
    ;   F0 = [s(quote),Arg] -> F = [quote,Arg]
    ;   F0 = [s(setq),s(Var),Val0] -> compile(Val0, Val), F = [setq,Var,Val]
    ;   F0 = [s(Op)|Args0],
        memberchk(Op, [+,-,*,equal,if,>,<,=,progn,eval,list,car,cons,
                       cdr,while,not]) ->
        compile_all(Args0, Args),
        F = [Op|Args]
    ;   F0 = [s(defun),s(Name),Args0|Body0] ->
        compile_all(Body0, Body),
        maplist(arg(1), Args0, Args),
        F = [defun,Name,Args|Body]
    ;   F0 = [s(Op)|Args0] -> compile_all(Args0, Args), F = [user(Op)|Args]
    ).

eval_all([], [])         --> [].
eval_all([A|As], [B|Bs]) --> eval(A, B), eval_all(As, Bs).

eval(n(N), n(N))       --> [].
eval(t, t)             --> [].
eval([], [])           --> [].
eval(s(A), V), [Fs-Vs] --> [Fs-Vs], { get_assoc(A, Vs, V) }.
eval([L|Ls], Value)    --> eval(L, Ls, Value).

eval(quote, [Q], Q) --> [].
eval(+, As0, V)     --> eval_all(As0, As), { fold(As, +, 0, V) }.
eval(-, As0, V)     --> eval_all(As0, [n(V0)|Vs0]), { fold(Vs0, -, V0, V) }.
eval(*, As0, V)     --> eval_all(As0, Vs), { fold(Vs, *, 1, V) }.
eval(car, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [C|_] }.
eval(cdr, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [_|C] }.
eval(list, Ls0, Ls) --> eval_all(Ls0, Ls).
eval(not, [A], V)   --> eval(A, V0), goal_truth(V0=[], V).
eval(>, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1>V2, V).
eval(<, [A,B], V)   --> eval(>, [B,A], V).
eval(=, [A,B], V)   --> eval(A, n(V1)), eval(B, n(V2)), goal_truth(V1=:=V2, V).
eval(progn, Ps, V)  --> eval_all(Ps, Vs), { last(Vs, V) }.
eval(eval, [A], V)  --> eval(A, F0), { compile(F0, F1) }, eval(F1, V).
eval(equal, [A,B], V) --> eval(A, V1), eval(B, V2), goal_truth(V1=V2, V).
eval(cons, [A,B], [V0|V1])  --> eval(A, V0), eval(B, V1).
eval(while, [Cond|Bs], [])  -->
    (   eval(Cond, []) -> []
    ;   eval_all(Bs, _),
        eval(while, [Cond|Bs], _)
    ).
eval(defun, [F,As|Body], s(F)), [Fs-Vs0] -->
    [Fs0-Vs0],
    { put_assoc(F, Fs0, As-Body, Fs) }.
eval(user(F), As0, V), [Fs-Vs] -->
    eval_all(As0, As1),
    [Fs-Vs],
    { empty_assoc(E),
      get_assoc(F, Fs, As-Body),
      bind_arguments(As, As1, E, Bindings),
      phrase(eval_all(Body, Results), [Fs-Bindings], _),
      last(Results, V) }.
eval('bind!', [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(setq, [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(if, [Cond,Then|Else], Value) -->
    (   eval(Cond, []) -> eval_all(Else, Values), { last(Values, Value) }
    ;   eval(Then, Value)
    ).

:- meta_predicate goal_truth(0,*,//,//).
goal_truth(Goal, T) --> { Goal -> T = t ; T = [] }.

bind_arguments([], [], Bs, Bs).
bind_arguments([A|As], [V|Vs], Bs0, Bs) :-
    put_assoc(A, Bs0, V, Bs1),
    bind_arguments(As, Vs, Bs1, Bs).

run(S):-'format'('~n~s~n',[S]),run(S,V),writeq(V).

%if_script_file_time(X):-if_startup_script(time(X)).
if_script_file_time(_):-!.
%if_script_file_time(X):- nop(time(X)).

% Append:
    :- if_script_file_time(run("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].


% Fibonacci, naive version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].


% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].


% Fibonacci, iterative version:
    :- if_script_file_time(run("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 30,794 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 12831368 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].



% Fibonacci, accumulating version:
    :- if_script_file_time(run("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 350)")).

    %@ % 44,595 inferences, 0.003 CPU in 0.003 seconds (100% CPU, 14526532 Lips)
    %@ V = [fib, fib1, 6254449428820551641549772190170184190608177514674331726439961915653414425].


% Higher-order programming and eval:
    :- if_script_file_time(run("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].

%:- ensure_loaded(metta_reader).



#[test]
fn test_case_operation() {
    let metta = new_metta_rust();
    let result = metta.run(&mut SExprParser::new("
    "));

    let expected = metta.run(&mut SExprParser::new("
        ! OK
        ! 7
        ! (superpose (OK-3 OK-4))
        ! (superpose (3 4 5))
        ! (superpose ())
    "));
    assert_eq!(result, expected);

    let metta = new_metta_rust();
    let result = metta.run(&mut SExprParser::new("
        (Rel-P A B)
        (Rel-Q A C)

        ; cases can be used for deconstruction
        !(case (match &self ($rel A $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))))

        ; %void% can be used to capture empty results
        !(case (match &self ($rel B $x) ($rel $x))
            (((Rel-P $y) (P $y))
            ((Rel-Q $y) (Q $y))
            (%void% no-match)))

        ; a functional example
        (= (maybe-inc $x)
            (case $x
            (((Just $v) (Just (+ 1 $v)))
                (Nothing Nothing)))
        )
        !(maybe-inc Nothing)
        !(maybe-inc (Just 2))
    "));
    let expected = metta.run(&mut SExprParser::new("
        ! (superpose ((Q C) (P B)))
        ! no-match
        ! Nothing
        ! (Just 3)
    "));
    assert_eq_metta_results!(result, expected);
}



use hyperon::metta::text::*;
use hyperon::metta::runner::new_metta_rust;

#[test]
fn test_reduce_higher_order() {
    let program = "
        ; Curried plus
        (: plus (-> Number (-> Number Number)))
        (= ((plus $x) $y) (+ $x $y))
        ; Define inc as partial evaluation of plus
        (: inc (-> (-> Number Number)))
        (= (inc) (plus 1))

        !(assertEqualToResult ((inc) 2) (3))
    ";
    let metta = new_metta_rust();

    let result = metta.run(&mut SExprParser::new(program));

    assert_eq!(result, Ok(vec![vec![]]));
}



use hyperon::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_custom_match_with_space() {
    let mut main_space = GroundingSpace::new();
    let mut inserted_space = GroundingSpace::new();
    inserted_space.add(expr!("implies" ("B" x) ("C" x)));
    inserted_space.add(expr!("implies" ("A" x) ("B" x)));
    inserted_space.add(expr!("A" "Sam"));
    main_space.add(Atom::gnd(inserted_space));
    let result = main_space.query(&expr!("," ("implies" ("B" x) z) ("implies" ("A" x) y) ("A" x)));
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].resolve(&VariableAtom::new("y")), Some(expr!("B" "Sam")));
    assert_eq!(result[0].resolve(&VariableAtom::new("z")), Some(expr!("C" "Sam")));
}



use hyperon::*;
use hyperon::common::*;
use hyperon::metta::interpreter::*;
use hyperon::space::grounding::GroundingSpace;

#[test]
fn test_types_in_metta() {
    let mut space = GroundingSpace::new();
    space.add(expr!("=" ("check" (":" n "Int")) ({IS_INT} n)));
    space.add(expr!("=" ("check" (":" n "Nat")) ({AND} ("check" (":" n "Int")) ({GT} n {0}))));
    space.add(expr!("=" ("if" {true} then else) then));
    space.add(expr!("=" ("if" {false} then else) else));
    space.add(expr!(":" "if" ("->" "bool" "Atom" "Atom" "Atom")));
    space.add(expr!("=" ("fac" n) ("if" ("check" (":" n "Nat")) ("if" ({EQ} n {1}) {1} ({MUL} n ("fac" ({SUB} n {1})))) ({ERR}))));

    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Int"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {3} "Nat"))), Ok(vec![expr!({true})]));
    assert_eq!(interpret(&space, &expr!("check" (":" {(-3)} "Nat"))), Ok(vec![expr!({false})]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(3)} "Nat")) "ok" "nok")), Ok(vec![expr!("ok")]));
    assert_eq!(interpret(&space, &expr!("if" ("check" (":" {(-3)} "Nat")) "ok" "nok")), Ok(vec![expr!("nok")]));
    assert_eq!(interpret(&space, &expr!("fac" {1})), Ok(vec![expr!({1})]));
    assert_eq!(interpret(&space, &expr!("fac" {3})), Ok(vec![expr!({6})]));
}








    #[test]
    fn test_match_expression_with_variables() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" b c))),
        bind_set![{a: expr!("A"), b: expr!("B"), c: expr!("C") }]);
    }

    #[test]
    fn test_match_different_value_for_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!("+" "A" ("*" "B" "C")));
        assert_eq!(space.query(&expr!("+" a ("*" a c))), BindingsSet::empty());
    }

    #[test]
    fn test_match_query_variable_has_priority() {
        let mut space = GroundingSpace::new();
        space.add(expr!("equals" x x));

        let result = space.query(&expr!("equals" y z));
        assert_eq!(result, bind_set![{ y: expr!(z) }]);
    }

    #[test]
    fn test_match_query_variable_via_data_variable() {
        let mut space = GroundingSpace::new();
        space.add(expr!(x x));
        assert_eq!(space.query(&expr!(y (z))), bind_set![{y: expr!((z))}]);
    }

    #[test]
    fn test_match_if_then_with_x() {
        let mut space = GroundingSpace::new();
        space.add(expr!("=" ("if" "True" then) then));
        assert_eq!(space.query(&expr!("=" ("if" "True" "42") X)),
        bind_set![{X: expr!("42")}]);
    }

    #[test]
    fn test_match_combined_query() {
        let mut space = GroundingSpace::new();
        space.add(expr!("posesses" "Sam" "baloon"));
        space.add(expr!("likes" "Sam" ("blue" "stuff")));
        space.add(expr!("has-color" "baloon" "blue"));

        let result = space.query(&expr!("," ("posesses" "Sam" object)
        ("likes" "Sam" (color "stuff"))
        ("has-color" object color)));
        assert_eq!(result, bind_set![{object: expr!("baloon"), color: expr!("blue")}]);
    }

