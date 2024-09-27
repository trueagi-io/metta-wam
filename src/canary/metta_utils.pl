
:- set_prolog_flag(verbose_autoload, false).
:- set_prolog_flag(verbose, silent).
:- set_prolog_flag(verbose_load, silent).
:- ensure_loaded(library(logicmoo_utils)).
:- assert((user:'$exported_op'(_,_,_):- fail)).
:- abolish((system:'$exported_op'/3)).
:- assert((system:'$exported_op'(_,_,_):- fail)).

:- if(exists_source(library(logicmoo_utils))).
:- ensure_loaded(library(logicmoo_utils)).
:- endif.
:- if(exists_source(library(dictoo))).
%:- ensure_loaded(library(dictoo)).
:- endif.



:- dynamic(done_once/1).
do_once(G):-
  ((done_once(GG),GG=@=G) -> true
  ;(assert(done_once(G)),(once(@(G,user))->true;retract(done_once(G))))).

cleanup_debug:-
  forall(
    (clause(prolog_debug:debugging(A1,B,C),Body,Cl1),
     clause(prolog_debug:debugging(A2,B,C),Body,Cl2),
     A1=@=A2,Cl1\==Cl2),
     erase(Cl2)).

:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).
catch_nolog(G):- ignore(catch(notrace(G),E,once(true;nop(u_dmsg(E=G))))).
catch_log(G):- ignore(catch((G),E,((u_dmsg(E=G),ugtrace(E,G))))).
% catch_log(G):- ignore(catch(notrace(G),E,((writeln(E=G),catch_nolog(ds))))).

get_user_error(UE):- stream_property(UE,file_no(2)),!.
get_user_error(UE):- stream_property(UE,alias(user_error)),!.

ufmt(G):- notrace((fbug(G)->true;ufmt0(G))).
ufmt0(G):- fmt(G)->true;writeln(G).
u_dmsg(G):- is_list(G),!,my_maplist(u_dmsg,G).
u_dmsg(M):- get_user_error(UE), \+ current_predicate(with_toplevel_pp/2),!, with_output_to(UE,ufmt(M)).
u_dmsg(M):- get_user_error(UE),!, with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))).
u_dmsg(M):- get_user_error(UE),  stream_property(UO,file_no(1)), current_output(CO),!,
  (UO==CO ->  fbug(M) ;
   (with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))), with_output_to(CO,pp(M)))).
u_dmsg(G):-ufmt(G),!.


:- multifile(is_cgi/0).
:- dynamic(is_cgi/0).
:- multifile(arc_html/0).
:- dynamic(arc_html/0).


logicmoo_use_swish:-
  set_prolog_flag(use_arc_swish,true),
  ld_logicmoo_webui,call(call,webui_start_swish_and_clio),
  http_handler('/swish', http_redirect(moved, '/swish/'), []).

arc_user(Nonvar):- nonvar(Nonvar),!,arc_user(Var),!,Nonvar=Var.
arc_user(main):- main_thread, !. %\+ if_thread_main(fail),!.
arc_user(ID):- catch((pengine:pengine_user(ID)),_,fail),!.
arc_user(ID):- catch((xlisting_web:is_cgi_stream,xlisting_web:find_http_session(User),http_session:session_data(User,username(ID))),_,fail),!.
arc_user(ID):- catch((is_cgi, (xlisting_web:find_http_session(ID))),_,fail),!.
arc_user(ID):- is_cgi,!,ID=web_user.
arc_user(ID):- thread_self(ID).

:- dynamic(arc_user_prop/3).

%luser_setval(N,V):- nb_setval(N,V),!.
luser_setval(N,V):- arc_user(ID),luser_setval(ID,N,V),!.
luser_setval(ID,N,V):- \+ (arc_sensical_term(N),arc_sensical_term(V)),
  warn_skip(not_arc_sensical_term(luser_setval(ID,N,V))).
luser_setval(ID,N,V):-
  (atom(N)->nb_setval(N,V);true),
  retractall(arc_user_prop(ID,N,_)),asserta(arc_user_prop(ID,N,V)).


luser_unsetval(N):- ignore(nb_delete(N)), arc_user(ID),luser_unsetval(ID,N),!.
luser_unsetval(ID,N):- retractall(arc_user_prop(ID,N,_)).

set_luser_default(N,V):- luser_setval(global,N,V).
luser_default(N,V):- var(V),!,luser_getval(N,V).
luser_default(N,V):- set_luser_default(N,V).

luser_linkval(N,V):- arc_user(ID),luser_linkval(ID,N,V),!.
luser_linkval(ID,N,V):- \+ var(V), \+ (arc_sensical_term(N),arc_sensical_term(V)),
 trace,
 warn_skip(not_arc_sensical_term(luser_linkval(ID,N,V))).
luser_linkval(ID,N,V):-
  (atom(N)->nb_linkval(N,V);true),
  retractall(arc_user_prop(ID,N,_)),asserta(arc_user_prop(ID,N,V)).

arc_sensical_term(O):- nonvar(O), O\==[], O\=='', O \= (_ - _), O\==end_of_file.
arc_sensical_term(V,O):- arc_sensical_term(V), !, O=V.

%arc_option(grid_size_only):- !,fail.
arc_option(O):- luser_getval(O,t).
if_arc_option(O,G):- (arc_option(O)->must_det_ll(G); true).

with_luser(N,V,Goal):-
  (luser_getval(N,OV);OV=[]),
  setup_call_cleanup(
    luser_setval(N,V),
    once(Goal),
    luser_setval(N,OV)).

%luser_getval(N,V):- nb_current(N,VVV),arc_sensical_term(VVV,VV),!,V=VV.
% caches the valuetemp on this thread
luser_getval(N,V):-  luser_getval_0(N,VV),VV=V,arc_sensical_term(V),!.

luser_getval_0(arc_user,V):- arc_user(V).
luser_getval_0(N,V):- luser_getval_1(N,V).

luser_getval_1(N,V):- luser_getval_2(N,V).
luser_getval_1(N,V):- luser_getval_3(N,V), \+ (luser_getval_2(N,VV), nop(VV\=V)).
luser_getval_1(N,V):- get_luser_default(N,V), \+ (luser_getval_3(N,VV), nop(VV\=V)), \+ (luser_getval_2(N,VV), nop(VV\=V)).

%luser_getval_0(N,V):- luser_getval_2(N,V), \+ luser_getval_1(N,_).
%luser_getval_0(N,V):- luser_getval_3(N,V), \+ luser_getval_2(N,_), \+ luser_getval_1(N,_).
%luser_getval_3(N,V):- is_cgi, current_predicate(get_param_req/2),get_param_req(N,M),url_decode_term(M,V).
luser_getval_2(N,V):- \+ main_thread, atom(N), httpd_wrapper:http_current_request(Request), member(search(List),Request),member(N=VV,List),url_decode_term(VV,V),arc_sensical_term(V),!.
luser_getval_2(N,V):- atom(N), nb_current(N,ValV),arc_sensical_term(ValV,Val),Val=V.

luser_getval_3(N,V):- arc_user(ID), arc_user_prop(ID,N,V).
luser_getval_3(_,_):- \+ is_cgi, !, fail.
luser_getval_3(N,V):-  \+ main_thread, atom(N), current_predicate(get_param_sess/2),get_param_sess(N,M),url_decode_term(M,V),arc_sensical_term(V).
%luser_getval_3(N,V):- atom(N), nb_current(N,ValV),arc_sensical_term(ValV,Val),Val=V.


get_luser_default(N,V):- arc_user_prop(global,N,VV),VV=V,arc_sensical_term(V),!.
get_luser_default(N,V):- atom(N), current_prolog_flag(N,VV),VV=V,arc_sensical_term(V),!.
%luser_getval(ID,N,V):- thread_self(ID),nb_current(N,V),!.
%luser_getval(ID,N,V):- !, ((arc_user_prop(ID,N,V);nb_current(N,V))*->true;arc_user_prop(global,N,V)).


ansi_main:- thread_self(main),nop(is_cgi),!.

main_thread:- thread_self(main),!.
if_thread_main(G):- main_thread->call(G);true.




:- if(\+ current_predicate(fbug/1)).
%fbug(P):- format(user_error,'~N~p~n',[P]).
:- endif.



substM(T, F, R, R):- T==F,!.
substM(T, _, _, R):- \+ compound(T),!,R=T.
substM([H1|T1], F, R, [H2|T2]) :- !, substM(H1, F, R, H2), substM(T1, F, R, T2).
substM(C1, F, R, C2) :- C1 =.. [Fn|A1], substM_l(A1,F,R,A2),!, C2 =.. [Fn|A2].
substM_l([], _, _, []).  substM_l([H1|T1], F, R, [H2|T2]) :- substM(H1, F, R, H2), substM_l(T1, F, R, T2).


pp_m(Cl):- write_src(Cl),!.
pp_m(C,Cl):- color_g_mesg(C,write_src(Cl)),!.
%  notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).
pp_q(Cl):-
  notrace((format('~N'), ignore(( \+ ((numbervars(Cl,0,_,[singletons(true)]), print_tree_with_final(Cl,"."))))))).


ncatch(G,E,F):- catch(G,E,F).
mcatch(G,E,F):- catch(G,E,F).
%mcatch(G,E,F):- catch(G,E,(fbug(G=E),catch(bt,_,fail),fbug(G=E),ignore(call(F)),throw(E))).
%ncatch(G,E,F):- catch(G,E,(fbug(G=E),catch(bt,_,fail),fbug(G=E),call(G))).
%ncatch(G,E,(F)).


:- if( \+ current_predicate(if_t/2)).
:- meta_predicate(if_t(0,0)).
if_t(IF, THEN) :- call(call,ignore((((IF,THEN))))).
:- endif.

:- if( \+ current_predicate(must_ll/1)).
:- meta_predicate(must_ll(0)).
must_ll(G):- md(call,G)*->true;throw(not_at_least_once(G)).
:- endif.

:- if( \+ current_predicate(at_least_once/1)).
:- meta_predicate(at_least_once(0)).
at_least_once(G):- call(G)*->true;throw(not_at_least_once(G)).
:- endif.

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
  /*unrepress_output*/((bt,fbug(tc_arg(N,C,E)=Err),((tracing->true;trace),break,arg(N,C,E))))).






compound_name_arg(G,MD,Goal):- var(G),!,atom(MD),G=..[MD,Goal].
compound_name_arg(G,MD,Goal):- compound(G),!, compound_name_arguments(G,MD,[Goal]).


:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
%user:message_hook(Term, Kind, Lines):- error==Kind, itrace,fbug(user:message_hook(Term, Kind, Lines)),trace,fail.
user:message_hook(Term, Kind, Lines):-
   fail, error==Kind,
   fbug(message_hook(Term, Kind, Lines)),fail.

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

%:- if( \+ current_predicate(must_det_ll/1)).
must_det_ll(X):- tracing,!,once(X).
must_det_ll(X):- md(once,X).
%:- endif.

md(P1,G):- tracing,!, call(P1,G). % once((call(G)*->true;md_failed(P1,G))).
md(P1,G):- remove_must_det(MD), wraps_each(MD,P1),!,call(G).
md(P1,G):- never_rrtrace,!, call(P1,G).
md(P1,G):- /*notrace*/(arc_html),!, ignore(/*notrace*/(call(P1,G))),!.
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
  ((M is N -1, M>0)->throw(md_failed(P1,G,M));(ugtrace(md_failed(P1,G,M),X),throw('$aborted')))),!.
%must_det_ll(X):- must_det_ll1(P1,X),!.

must_det_ll1(P1,X):- tracing,!,must_not_error(call(P1,X)),!.
must_det_ll1(P1,once(A)):- !, once(md(P1,A)).
must_det_ll1(P1,X):-
  strip_module(X,M,P),functor(P,F,A),
  setup_call_cleanup(nop(trace(M:F/A,+fail)),(must_not_error(call(P1,X))*->true;md_failed(P1,X)),
    nop(trace(M:F/A,-fail))),!.


%must_not_error(G):- must(once(G)).

must_not_error(G):- (tracing;never_rrtrace),!,call(G).
must_not_error(G):- notrace(is_cgi),!, ncatch((G),E,((u_dmsg(E=G)))).
%must_not_error(X):- is_guitracer,!, call(X).
%must_not_error(G):- !, call(G).
must_not_error(X):- !,ncatch(X,E,(fbug(E=X),ugtrace(error(E),X))).
must_not_error(X):- ncatch(X,E,(rethrow_abort(E);(/*arcST,*/writeq(E=X),pp(etrace=X),
  trace,
  rrtrace(visible_rtrace([-all,+exception]),X)))).


always_rethrow('$aborted').
always_rethrow(md_failed(_,_,_)).
always_rethrow(return(_)).
always_rethrow(metta_return(_)).
always_rethrow(give_up(_)).
always_rethrow(time_limit_exceeded(_)).
always_rethrow(depth_limit_exceeded).
always_rethrow(restart_reading).
always_rethrow(E):- never_rrtrace,!,throw(E).

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

md_failed(P1,X):- notrace((write_src_uo(failed(P1,X)),fail)).
md_failed(P1,X):- tracing,visible_rtrace([-all,+fail,+call,+exception],call(P1,X)).
md_failed(P1,X):- \+ tracing, !, visible_rtrace([-all,+fail,+exit,+call,+exception],call(P1,X)).
md_failed(P1,G):- is_cgi, \+ main_debug, !, u_dmsg(arc_html(md_failed(P1,G))),fail.
md_failed(_P1,G):- option_value(testing,true),!,
      T='FAILEDDDDDDDDDDDDDDDDDDDDDDDDDD!!!!!!!!!!!!!'(G),
      write_src_uo(T), give_up(T,G).
md_failed(P1,G):- never_rrtrace,!,notrace,/*notrace*/(u_dmsg(md_failed(P1,G))),!,throw(md_failed(P1,G,2)).
%md_failed(P1,G):- tracing,call(P1,G).
md_failed(_,_):- never_rrtrace,!,fail.
md_failed(P1,X):- notrace,is_guitracer,u_dmsg(failed(X))/*,arcST*/,nortrace,atrace,call(P1,X).
md_failed(P1,G):- main_debug,/*notrace*/(write_src_uo(md_failed(P1,G))),!,throw(md_failed(P1,G,2)).
% must_det_ll(X):- must_det_ll(X),!.

write_src_uo(G):-
 stream_property(S,file_no(1)),
    with_output_to(S,
     (format('~N~n~n',[]),
      write_src(G),
      format('~N~n~n'))),!,
 %stack_dump,
 stream_property(S2,file_no(2)),
    with_output_to(S2,
     (format('~N~n~n',[]),
      write_src(G),
      format('~N~n~n'))),!.

:- meta_predicate(rrtrace(0)).
rrtrace(X):- rrtrace(etrace,X).

stack_dump:-  ignore(catch(bt,_,true)). %,ignore(catch(dumpST,_,true)),ignore(catch(bts,_,true)).
ugtrace(error(Why),G):- !, notrace,write_src_uo(Why),stack_dump,write_src_uo(Why),rtrace(G).
ugtrace(Why,G):-  tracing,!,notrace,write_src(Why),rtrace(G).
ugtrace(Why,_):-  is_testing, !, ignore(give_up(Why,5)),throw('$aborted').
ugtrace(Why,G):-  wdmsg(Why),ggtrace(G),throw('$aborted').
%ugtrace(Why,G):- ggtrace(G).

give_up(Why,_):- is_testing,!,write_src_uo(Why),!, throw(give_up(Why)).
give_up(Why,N):- is_testing,!,write_src_uo(Why),!, halt(N).
give_up(Why,_):- write_src_uo(Why),throw('$aborted').

is_guitracer:- getenv('DISPLAY',_), current_prolog_flag(gui_tracer,true).
:- meta_predicate(rrtrace(1,0)).
rrtrace(P1,X):- never_rrtrace,!,nop((u_dmsg(cant_rrtrace(P1,X)))),!,fail.
rrtrace(P1,G):- is_cgi,!, u_dmsg(arc_html(rrtrace(P1,G))),call(P1,G).
rrtrace(P1,X):- notrace, \+ is_guitracer,!,nortrace, /*arcST, sleep(0.5), trace,*/
   (notrace(\+ current_prolog_flag(gui_tracer,true)) -> call(P1,X); (itrace,call(P1,X))).
%rrtrace(_,X):- is_guitracer,!,notrace,nortrace,ncatch(call(call,ugtrace),_,true),atrace,call(X).
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
:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).

my_assertion(G):- call(G),!.
my_assertion(G):- fbug(my_assertion(G)),writeq(goal(G)),nl,!,break.
must_be_free(AllNew):- plain_var(AllNew),!.
must_be_free(AllNew):- arcST,fbug(must_be_free(AllNew)),break,fail.
must_be_nonvar(AllNew):- nonvar_or_ci(AllNew),!.
must_be_nonvar(AllNew):- arcST,fbug(must_be_nonvar(AllNew)),break,fail.

my_len(X,Y):- var(X),!,length(X,Y).
my_len(X,Y):- is_list(X),!,length(X,Y).
my_len(X,Y):- functor([_|_],F,A),functor(X,F,A),!,length(X,Y).
my_len(X,Y):- arcST,!,ibreak.
*/
is_map(G):- is_vm_map(G),!.
%arc_webui:- false.
sort_safe(I,O):- catch(sort(I,O),_,I=O).
my_append(A,B):- append(A,B).
my_append(A,B,C):- append(A,B,C).
with_tty_false(Goal):- with_set_stream(current_output,tty(false),Goal).
with_tty_true(Goal):- with_set_stream(current_output,tty(true),Goal).

% Count occurrences of G and store the result in N
count_of(G,N):- findall_vset(G,G,S),length(S,N).
findall_vset(T,G,S):- findall(T,G,L),variant_list_to_set(L,S).
flatten_objects(Objs,ObjsO):- flatten([Objs],ObjsO),!.


var_e(E,S):- E==S,!.
var_e(E,S):- (nonvar(E);attvar(E)),!,E=@=S.

variant_list_to_set([E|List],Out):- select(S,List,Rest),var_e(E,S),!, variant_list_to_set([E|Rest],Out).
variant_list_to_set([E|List],[E|Out]):- !, variant_list_to_set(List,Out).
variant_list_to_set(H,H).

nb_subst(Obj,New,Old):-
  get_setarg_p1(nb_setarg,Found,Obj,P1),Found=@=Old,
  p1_call(P1,New),!,nb_subst(Obj,New,Old).
nb_subst(_Obj,_New,_Old).

system:any_arc_files(Some):- is_list(Some),!, Some\==[],maplist(any_arc_files,Some).
system:any_arc_files(Some):- atom_contains(Some,'arc').

:- thread_local(in_memo_cached/5).
:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).
prolog:make_hook(before, Some):- any_arc_files(Some), forall(muarc:clear_all_caches,true).

:- multifile(muarc:clear_all_caches/0).
:- dynamic(muarc:clear_all_caches/0).
muarc:clear_all_caches:-  \+ luser_getval(extreme_caching,true), retractall(in_memo_cached(_,_,_,_,_)), fail.
%arc_memoized(G):- !, call(G).

arc_memoized(G):- compound(G),ground(G),functor(G,F,1),functor(C,F,1),!,arc_memoized(C),G=C,!.
arc_memoized(G):-
  copy_term(G,C,GT),
  (Key = (C+GT)),
  (in_memo_cached(Key,C,track,started,Info)->throw(already_memoizing(in_memo_cached(Key,C,track,started,Info))) ; true),
  numbervars(Key,0,_,[attvar(bind),singletons(true)]),!,
  setup_call_cleanup((asserta(in_memo_cached(Key,C,track,started,_),Started)),
  catch(
  (in_memo_cached(Key,C,GT,Found,AttGoals)*->(G=Found,maplist(call,AttGoals))
    ; ((call(G),copy_term(G,CG,GG)) *->asserta(in_memo_cached(Key,C,GT,CG,GG))
                  ;asserta(in_memo_cached(Key,C,GT,failed,_)))),
  E, (retractall(in_memo_cached(Key,C,GT,_,_)),throw(E))),erase(Started)).

set_nth1(1,[_|Row],E,[E|Row]):-!.
set_nth1(N,[W|Row],E,[W|RowMod]):- Nm1 is N-1, set_nth1(Nm1,Row,E,RowMod).

findall_count(T,G,N):- findall_set(T,G,S),length(S,N).

findall_set(T,G,S):- findall(T,G,L),list_to_set(L,S).

make_list_inited(0,_,[]):-!.
make_list_inited(1,E,[E]):-!.
make_list_inited(N,E,[E|List]):- Nm1 is N -1,make_list_inited(Nm1,E,List).

nth_fact(P,I):- clause(P,true,Ref),nth_clause(P,I,Ref).

nonvar_or_ci(C):- (nonvar(C);attvar(C)),!.

add_i(Info):-
 quietly((tersify(Info,InfoT),
 luser_getval(test_rules,TRules),
 luser_getval(pair_rules,PRules),
  nb_set_add(TRules,InfoT),
  nb_set_add(PRules,InfoT),
 nop(pp(cyan,+InfoT)))).

add_i(F,Info):-
 append_term(i(F),Info,FInfo),
 add_i(FInfo).

add_rule(Info):- add_i(rule,Info).
add_cond(Info):- add_i(cond,Info).
%do_action(Info):- guess_pretty(Info),add_i(action,Info),call(Info).
do_action(Call):- !, copy_term(Call,Info),call(Call),add_i(action,Info).
add_action(Info):- add_i(action,Info).
add_note(Info):- add_i(note,Info).
add_indiv(W,Info):- add_i(indiv(W),Info).
add_comparitor(Info):- add_i(comparitor,Info).
show_rules:-
 luser_getval(pair_rules,PRules), maplist(pp(cyan),PRules),
 luser_getval(test_rules,TRules), maplist(pp(blue),TRules),
 !.


sub_atom_value(TestID,A):- sub_term(A,TestID),(atom(A);string(A)).

my_list_to_set(List, Set):- my_list_to_set(List, (=) ,Set).
my_list_to_set_variant(List, Set):- my_list_to_set(List, (=@=) ,Set).
my_list_to_set_cmp(List, Set):- my_list_to_set(List, (=@=) ,Set).

my_list_to_set([E|List],P2, Set):- select(C,List,Rest), p2_call(P2, E,C), !, my_list_to_set([E|Rest],P2, Set).
my_list_to_set([E|List],P2, [E|Set]):-!, my_list_to_set(List,P2, Set).
my_list_to_set([],_,[]).

my_list_to_set_cmp([E|List],C3, Set):- select(C,List,Rest), call(C3,R,E,C),
   R== (=), my_list_to_set_cmp([C|Rest],C3, Set),!.
  my_list_to_set_cmp([E|List],C3, [E|Set]):-!, my_list_to_set_cmp(List,C3, Set).
my_list_to_set_cmp([],_,[]).


contains_nonvar(N,Info):- sub_term(E,Info),nonvar_or_ci(E),E=N,!.

max_min(A,B,C,D):- must_be_free(C),must_be_free(D),max_min0(A,B,C,D).
max_min0(A,B,B,B):- plain_var(A).
max_min0(A,B,A,A):- plain_var(B),!.
max_min0(A,B,C,D):- number(A),number(B), !, ((A > B) -> (C=A, D=B) ; (C=B, D=A)).
max_min0(_,A,A,A):- number(A),!.
max_min0(A,_,A,A):- number(A),!.
max_min0(_,_,_,_).

as_debug(L,G):- as_debug(L,true,G).
as_debug(9,_,_):- !.
as_debug(_,C,G):- ignore(catch((call(C)->wots(S,G),format('~NDEBUG: ~w~N',[S]);true),_,true)).

shall_count_as_same(A,B):- same_term(A,B),!. % unify ok_ok cmatch
shall_count_as_same(A,B):- plain_var(A),!,A==B.
shall_count_as_same(A,B):- atomic(A),!, A=@=B.
shall_count_as_same(A,B):- var(B),!,A=@=B.
shall_count_as_same(A,B):- A=@=B,!.
shall_count_as_same(A,B):- copy_term(B,CB),copy_term(A,CA),\+ \+ ( A=B, B=@=CB, A=@=CA),!.
%shall_count_as_same(A,B):- \+ A \= B, !.

count_each([C|L],GC,[Len-C|LL]):- include(shall_count_as_same(C),GC,Lst),length(Lst,Len),!,count_each(L,GC,LL).
count_each([],_,[]).

count_each_inv([C|L],GC,[C-Len|LL]):- include(shall_count_as_same(C),GC,Lst),length(Lst,Len),count_each_inv(L,GC,LL).
count_each_inv([],_,[]).

maplist_n(N,P,[H1|T1]):-
  p2_call(P,N,H1), N1 is N+1,
  maplist_n(N1,P,T1).
maplist_n(_N,_P,[]).

maplist_n(N,P,[H1|T1],[H2|T2]):-
  call(P,N,H1,H2), N1 is N+1,
  maplist_n(N1,P,T1,T2).
maplist_n(_N,_P,[],[]).

/*
print_points_grid(Points):-
 points_range(Points, LoH, LoV, HiH, HiV, H, V), writeqln(size_range(LoH, LoV, HiH, HiV, H, V)), points_to_grid(Points, Grid), print_grid(Grid).

print_points_grid(Grid):-
 points_range(Grid, LoH, LoV, HiH, HiV, _H, _V), print_grid(Grid, LoH, LoV, HiH, HiV, Grid).
*/


%print_trainer:- kaggle_arc_train(Name, Stuff), atom_json_term(Stuff, JSON, []), print_arc(Name, JSON).
%print_evaler:- kaggle_arc_eval(Name, Stuff), atom_json_term(Stuff, JSON, []), print_arc(Name, JSON).

 /*
% data looks like

kaggle_arc_train('007bbfb7', trn, [[0, 7, 7], [7, 7, 7], [0, 7, 7]], [[0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [7, 7, 7, 7, 7, 7, 7, 7, 7], [0, 7, 7,0, 7, 7,0, 7, 7], [0,0,0,0, 7, 7,0, 7, 7], [0,0,0, 7, 7, 7, 7, 7, 7], [0,0,0,0, 7, 7,0, 7, 7]]).
kaggle_arc_train('007bbfb7', trn, [[4,0, 4], [0,0,0], [0, 4,0]], [[4,0, 4,0,0,0, 4,0, 4], [0,0,0,0,0,0,0,0,0], [0, 4,0,0,0,0,0, 4,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 4,0, 4,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 4,0,0,0,0]]).
kaggle_arc_train('007bbfb7', trn, [[0,0,0], [0,0, 2], [2,0, 2]], [[0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 2], [0,0,0,0,0,0, 2,0, 2], [0,0,0,0,0,0,0,0,0], [0,0, 2,0,0,0,0,0, 2], [2,0, 2,0,0,0, 2,0, 2]]).
kaggle_arc_train('007bbfb7', trn, [[6, 6,0], [6,0,0], [0, 6, 6]], [[6, 6,0, 6, 6,0,0,0,0], [6,0,0, 6,0,0,0,0,0], [0, 6, 6,0, 6, 6,0,0,0], [6, 6,0,0,0,0,0,0,0], [6,0,0,0,0,0,0,0,0], [0, 6, 6,0,0,0,0,0,0], [0,0,0, 6, 6,0, 6, 6,0], [0,0,0, 6,0,0, 6,0,0], [0,0,0,0, 6, 6,0, 6, 6]]).
kaggle_arc_train('007bbfb7', trn, [[2, 2, 2], [0,0,0], [0, 2, 2]], [[2, 2, 2, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0, 2, 2,0, 2, 2,0, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0], [0,0,0, 2, 2, 2, 2, 2, 2], [0,0,0,0,0,0,0,0,0], [0,0,0,0, 2, 2,0, 2, 2]]).
kaggle_arc_train('007bbfb7', tst, [[7,0, 7], [7,0, 7], [7, 7,0]], [[7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7,0,0,0, 7,0, 7], [7,0, 7,0,0,0, 7,0, 7], [7, 7,0,0,0,0, 7, 7,0], [7,0, 7, 7,0, 7,0,0,0], [7,0, 7, 7,0, 7,0,0,0], [7, 7,0, 7, 7,0,0,0,0]]).

kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3,0, 3,0,0], [0,0, 3,0, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]], [[0,0,0,0,0,0], [0,0, 3,0,0,0], [0, 3, 4, 3,0,0], [0,0, 3, 4, 3,0], [0,0,0, 3,0,0], [0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0,0,0,0,0], [0,0,0, 3,0, 3,0,0,0,0], [0,0, 3,0,0,0, 3,0,0,0], [0,0,0,0,0, 3,0, 3,0,0], [0,0,0, 3,0, 3, 3,0,0,0], [0,0, 3, 3, 3,0,0,0,0,0], [0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0,0,0,0,0], [0,0,0, 3,0, 3,0,0,0,0], [0,0, 3,0,0,0, 3,0,0,0], [0,0,0,0,0, 3, 4, 3,0,0], [0,0,0, 3,0, 3, 3,0,0,0], [0,0, 3, 3, 3,0,0,0,0,0], [0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0,0,0], [0, 3, 3,0, 3, 3,0, 3,0,0], [3,0,0, 3,0,0, 3,0, 3,0], [0,0,0, 3,0,0, 3, 3,0,0], [0,0,0, 3,0,0, 3,0,0,0], [0,0,0, 3,0,0, 3,0,0,0], [0,0,0,0, 3, 3,0, 3,0,0], [0,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0,0,0], [0, 3, 3,0, 3, 3,0, 3,0,0], [3,0,0, 3, 4, 4, 3, 4, 3,0], [0,0,0, 3, 4, 4, 3, 3,0,0], [0,0,0, 3, 4, 4, 3,0,0,0], [0,0,0, 3, 4, 4, 3,0,0,0], [0,0,0,0, 3, 3,0, 3,0,0], [0,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0], [0,0, 3, 3, 3, 3,0,0,0,0], [0,0, 3,0,0, 3,0,0,0,0], [0,0, 3,0,0, 3,0, 3,0,0], [0,0, 3, 3, 3, 3, 3, 3, 3,0], [0,0,0, 3,0,0,0,0, 3,0], [0,0,0, 3,0,0,0, 3, 3,0], [0,0,0, 3, 3,0,0, 3,0, 3], [0,0,0, 3,0, 3,0,0, 3,0], [0,0,0,0, 3,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0], [0,0, 3, 3, 3, 3,0,0,0,0], [0,0, 3, 4, 4, 3,0,0,0,0], [0,0, 3, 4, 4, 3,0, 3,0,0], [0,0, 3, 3, 3, 3, 3, 3, 3,0], [0,0,0, 3,0,0,0,0, 3,0], [0,0,0, 3,0,0,0, 3, 3,0], [0,0,0, 3, 3,0,0, 3, 4, 3], [0,0,0, 3, 4, 3,0,0, 3,0], [0,0,0,0, 3,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', trn, [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0, 3,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0, 3,0,0,0,0], [0,0, 3,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0, 3, 3,0,0, 3,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0, 3, 3,0,0, 3,0,0, 3,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 4, 3, 3,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 3,0,0,0,0,0,0,0, 3,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0, 3,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 4, 4, 4, 4, 4, 4, 3,0,0,0,0], [0,0, 3,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 3, 3,0,0,0,0], [0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3,0,0,0,0, 3,0, 3,0,0], [0,0,0,0,0,0, 3, 3, 4, 4, 3,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 3, 3,0,0, 3,0,0, 3,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0, 3, 4, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
kaggle_arc_train('00d62c1b', tst, [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0, 3,0, 3, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0, 3,0,0,0,0, 3,0,0, 3,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 3,0, 3, 3, 3,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0, 3,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3,0,0,0, 3,0,0], [0,0,0,0,0,0, 3, 3,0, 3,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0, 3,0,0,0,0,0, 3, 3,0,0,0,0,0,0,0,0,0,0], [0, 3,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0, 3, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3,0,0,0, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]], [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0, 3, 4, 3, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3, 3, 3, 3, 3,0, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0, 3, 4, 4, 4, 4, 3, 4, 4, 3,0,0,0,0,0,0,0], [0,0,0,0, 3, 3, 3, 3, 3,0, 3, 3, 3,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0,0, 3, 4, 4, 4, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0,0,0, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3,0,0], [0,0,0,0,0,0, 3, 3, 4, 3,0,0,0, 3, 3, 3, 3, 3,0,0], [0,0, 3,0,0,0,0,0, 3, 3,0,0,0,0,0,0,0,0,0,0], [0, 3, 4, 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], [0,0, 3,0, 3,0, 3, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 4, 4, 4, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0, 3, 3, 3, 3, 3,0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
*/
%tell(s), ignore((nl, nl, task_pairs(Name, ExampleNum, In, Out), format('~N~q.~n', [test_pairs_cache(Name, ExampleNum, In, Out)]), fail)), told.
map_pred(Pred, P, X) :- map_pred([],Pred, P, X).
%map_pred(NoCycles,_Pred, P, X) :- member(E,NoCycles), E==P,!, X = P.
map_pred(NoCycles,Pred, P, X) :- p2_call(Pred, P, X)*->true;map_pred0(NoCycles,Pred, P, X).

map_pred1(Pred, P, P1) :- map_pred1(P, Pred, P, P1).

map_pred0(_NoCycles,_Pred, Args, ArgSO) :- must_be_free(ArgSO), Args==[],!, ArgSO=[].
map_pred0(_NoCycles, Pred, P, P1) :-  p2_call(Pred, P, P1),!. % *->true;fail.
map_pred0(NoCycles,Pred, P, X) :- fail, attvar(P), !, %duplicate_term(P,X),P=X,
  get_attrs(P,VS), map_pred([P|NoCycles],Pred, VS, VSX), P=X, put_attrs(X,VSX),!.
map_pred0(NoCycles,Pred, P, X):- map_pred1(NoCycles,Pred, P, X).

map_pred1(_NoCycles,_Pred, P, P1) :- ( \+ compound(P) ; is_ftVar(P)), !, must_det_ll(P1=P), !.
% map_pred0(NoCycles,Pred, Args, ArgSO) :- is_list(Args), !, maplist(map_pred([Args|NoCycles],Pred), Args, ArgS), ArgS=ArgSO.
map_pred1(NoCycles,Pred, IO, OO) :- is_list(IO),!, maplist(map_pred(NoCycles,Pred), IO, OO).
map_pred1(NoCycles,Pred, IO, [O|ArgS]) :-  IO= [I|Args], !,
  map_pred([IO,ArgS|NoCycles],Pred, I, O), map_pred0([IO,I|NoCycles],Pred, Args, ArgS).
map_pred1(NoCycles,Pred, P, P1) :-
  compound_name_arguments(P, F, Args), maplist(map_pred([P|NoCycles],Pred),Args,ArgS), compound_name_arguments(P1, F, ArgS).
%map_pred(_Pred, P, P).
/*
:- meta_predicate map_pred(2, ?, ?, ?, ?).
map_pred(Pred, P, X, Sk, P1) :- must_be_free(X), p2_call(Pred, P, X), !, must(Sk=P1), !.
map_pred(_Pred, P, _, _, P1) :- is_ftVar(P), !, must(P1=P), !.
map_pred(Pred, [P|Args], X, Sk, [P1|ArgS]) :- !, map_pred(Pred, P, X, Sk, P1), !, must(map_pred(Pred, Args, X, Sk, ArgS)), !.
map_pred(Pred, P, X, Sk, P1) :- compound(P), !, compound_name_arguments(P, F, Args), map_pred(Pred, [F|Args], X, Sk, [Fs|ArgS]), !, compound_name_arguments(P1, Fs, ArgS), !.
map_pred(_Pred, P, _, _, P).
*/
is_cons(A):- compound(A),A=[_|_].

into_grid_or_var(G,G):- is_cons(G),!.
into_grid_or_var(G,G):- var(G),!.
into_grid_or_var(O,G):- cast_to_grid(O,G,_Uncast),!.

maybe_mapgrid(P2,I,O):- is_grid(I),!,mapgrid(P2,I,O).
maybe_mapgrid(P3,I,O,M):- is_grid(I),!,mapgrid(P3,I,O,M).
maybe_mapgrid(P4,I,O,M,N):- is_grid(I),!,mapgrid(P4,I,O,M,N).

mapgrid(P4,Grid,GridM,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridM,G2),into_grid_or_var(GridN,G3),into_grid_or_var(GridO,G4),mapg_list(P4,G1,G2,G3,G4).
mapg_list(P4,Grid,GridM,GridN,GridO):- is_list(Grid),!,maplist(mapg_list(P4),Grid,GridM,GridN,GridO).
mapg_list(P4,Grid,GridM,GridN,GridO):- call(P4,Grid,GridM,GridN,GridO),!.

mapgrid(P3,Grid,GridN,GridO):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),into_grid_or_var(GridO,G3),mapg_list(P3,G1,G2,G3).
mapg_list(P3,Grid,GridN,GridO):- is_list(Grid),!,maplist(mapg_list(P3),Grid,GridN,GridO).
mapg_list(P3,Grid,GridN,GridO):- call(P3,Grid,GridN,GridO),!.

mapgrid(P2, Grid,GridN):- into_grid_or_var(Grid,G1),into_grid_or_var(GridN,G2),!,mapg_list(P2, G1,G2).
mapg_list(P2, Grid,GridN):- is_list(Grid),!,maplist(mapg_list(P2),Grid,GridN).
mapg_list(P2, Grid,GridN):- p2_call(P2, Grid,GridN),!.

mapgrid(P1,Grid):- into_grid_or_var(Grid,G1),mapg_list(P1,G1).
mapg_list(P1,Grid):- is_list(Grid),!,maplist(mapg_list(P1),Grid).
mapg_list(P1,Grid):- p1_call(P1,Grid),!.


maplist_ignore(_3,H,I,J):- (H==[];I==[],J==[]),!,(ignore(H=[]),ignore(I=[]),ignore(J=[])).
maplist_ignore(P3,H,I,J):- \+ is_list(H),!, ignore(p2_call(call(P3,H),I,J)).
maplist_ignore(P3,[H|Grid],[I|GridN],[J|GridO]):- maplist_ignore(P3,H,I,J), !,maplist_ignore(P3,Grid,GridN,GridO).

maplist_ignore(_2,H,I):- (H==[];I==[]),!,(ignore(H=[]),ignore(I=[])).
maplist_ignore(P2, H,I):- \+ is_list(H),!, ignore(p2_call(P2, H,I)).
maplist_ignore(P2, [H|Grid],[I|GridN]):- maplist_ignore(P2, H,I), !,maplist_ignore(P2, Grid,GridN).

%p1_or(P1,Q1,E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).

p1_call((P1;Q1),E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).
p1_call((P1,Q1),E):- must_be(callable,P1),!, (p1_call(P1,E),p1_call(Q1,E)).
p1_call(or(P1,Q1),E):- must_be(callable,P1),!, (p1_call(P1,E);p1_call(Q1,E)).
p1_call(and(P1,Q1),E):- must_be(callable,P1),!, (p1_call(P1,E),p1_call(Q1,E)).
p1_call(not(not(P1)),E):- !, \+ \+ p1_call(P1,E).
p1_call(not(P1),E):- !, not(p1_call(P1,E)).
p1_call(once(P1),E):- !, once(p1_call(P1,E)).
p1_call(ignore(P1),E):- !, ignore(p1_call(P1,E)).
p1_call(chk(P1),E):- !, \+ \+ (p1_call(P1,E)).
p1_call( \+ (P1),E):- !, \+ p1_call(P1,E).
p1_call(P1,E):- !, call(P1,E).

chk(X,E):- \+ \+ call(X,E).

p2_call_p2(P2a,P2b,A,B):- p2_call(P2a,A,M),p2_call(P2b,M,B).

p2_call(P2,A,B):- P2==[],!,A=B.
p2_call(p1_call(P1),E,O):- !, p1_call(P1,E), E=O.
p2_call([P2],Grid,GridN):- !, p2_call(P2, Grid,GridN).
p2_call([P2|P2L],Grid,GridN):- !, p2_call(P2, Grid,GridM),p2_call(P2L,GridM,GridN).
p2_call(ignore(P2),A,B):- p2_call(P2,A,B)*->true;A=B.
p2_call(type(Type,P2),A,B):- into_type(Type,A,AA),p2_call(P2,AA,B).
p2_call(or(P2,Q2),A,B):- nop(must_be(callable,P2)),!, (p2_call(P2,A,B);p2_call(Q2,A,B)).
p2_call(and(P2,Q2),A,B):- nop(must_be(callable,P2)),!, (p2_call(P2,A,AB),p2_call(Q2,AB,B)).
p2_call(P2,A,B):- must_be(callable,P2), call(P2,A,B).


p1_or(P1A,P1B,X):- p1_call(P1A,X)->true;p1_call(P1B,X).
p1_and(P1A,P1B,X):- p1_call(P1A,X),p1_call(P1B,X).
p1_not(P1,E):- \+ p1_call(P1,E).
p1_ignore(P1,E):- ignore(p1_call(P1,E)).
p1_arg(N,P1,E):- tc_arg(N,E,Arg),p1_call(P1,Arg).
p1_subterm(P1,E):- sub_term(Arg,E),p1_call(P1,Arg).

:- meta_predicate my_partition(-, ?, ?, ?).
my_partition(_,[],[],[]):-!.
my_partition(P1,[H|L],[H|I],E):- \+ \+ p1_call(P1,H),!,
  my_partition(P1,L,I,E).
my_partition(P1,[H|L],I,[H|E]):-
   my_partition(P1,L,I,E),!.
my_partition(P1,H,I,HE):- arcST,ibreak,
  my_partition(P1,[H],I,HE).


mapgroup(P2,G1,L2):- into_list(G1,L1),!, with_my_group(L1,maplist(P2,L1,L2)).
mapgroup(P1,G1):- into_list(G1,L1), !, with_my_group(L1,maplist(P1,L1)).

selected_group(Grp):- nb_current('$outer_group',Grp),!.
selected_group([]).

sub_cmpd(_, LF) :- \+ compound(LF), !, fail.
sub_cmpd(X, X).
sub_cmpd(X, Term) :-
    (   is_list(Term)
    ->  member(E, Term),
        sub_cmpd(X, E)
    ;   tc_arg(_, Term, Arg),
        sub_cmpd(X, Arg)
    ).



%with_my_group([O|Grp],Goal):- compound(O),O=obj(_),!, locally(nb_setval('$outer_group',[O|Grp]),Goal).
with_my_group(_,Goal):- call(Goal).

into_mlist(L,L).
my_maplist(P4,G1,L2,L3,L4):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P4,L1,L2,L3,L4)).
my_maplist(P3,G1,L2,L3):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P3,L1,L2,L3)).
my_maplist(P2,G1,L2):- into_mlist(G1,L1),!, with_my_group(L1,maplist(P2,L1,L2)).
my_maplist(P1,G1):- into_mlist(G1,L1), !, with_my_group(L1,maplist(P1,L1)).


my_include(P1,L,I):- include(p1_call(P1),L,I).
%my_include(P1,[H|L],O):- (p2_call(p1_call(P1),H,HH)*->(my_include(P1,L,I),O=[HH|I]);my_include(P1,L,O)).
my_include(_,_,[]).

%my_exclude(P1,I,O):- my_include(not(P1),I,O).
my_exclude(P1,I,O):- my_partition(P1,I,_,O).


subst_1L([],Term,Term):-!.
subst_1L([X-Y|List], Term, NewTerm ) :-
  subst0011(X, Y, Term, MTerm ),
  subst_1L(List, MTerm, NewTerm ).

subst_2L([],_,I,I).
subst_2L([F|FF],[R|RR],I,O):- subst0011(F,R,I,M),subst_2L(FF,RR,M,O).


subst001(I,F,R,O):- subst0011(F,R,I,O),!.


subst0011(X, Y, Term, NewTerm ) :-
  copy_term((X,Y,Term),(CX,CY,Copy),Goals),
  (Goals==[]
   ->subst0011a( X, Y, Term, NewTerm )
   ;(subst0011a(CX, CY, Goals, NewGoals),
     (NewGoals==Goals ->
       subst0011a( X, Y, Term, NewTerm )
       ; (subst0011a(CX, CY, Copy, NewCopy),
          NewTerm = NewCopy, maplist(call,NewGoals))))).



subst0011a(X, Y, Term, NewTerm ) :-
 ((X==Term)-> Y=NewTerm ;
  (is_list(Term)-> maplist(subst0011a(X, Y), Term, NewTerm );
   (( \+ compound(Term); Term='$VAR'(_))->Term=NewTerm;
     ((compound_name_arguments(Term, F, Args),
       maplist(subst0011a(X, Y), Args, ArgsNew),
        compound_name_arguments( NewTerm, F, ArgsNew )))))),!.

subst001C(I,F,R,O):- subst001_p2(same_term,I,F,R,O),!.
subst0011C(F,R,I,O):- subst0011_p2(same_term,F,R,I,O),!.
subst_2LC(F,R,I,O):- subst_2L_p2(same_term,F,R,I,O).

subst_2L_p2(_P2, [],_,I,I):-!.
subst_2L_p2(_P2, _,[],I,I):-!.
subst_2L_p2(P2, [F|FF],[R|RR],I,O):- subst0011_p2(P2, F,R,I,M),subst_2L_p2(P2, FF,RR,M,O).

subst001_p2(P2, I,F,R,O):- subst0011_p2(P2, F,R,I,O),!.

subst_1L_p2(_,  [],Term,Term):-!.
subst_1L_p2(P2, [X-Y|List], Term, NewTerm ) :-
  subst0011_p2(P2, X, Y, Term, MTerm ),
  subst_1L_p2(P2, List, MTerm, NewTerm ).

subst0011_p2(P2, X, Y, Term, NewTerm ) :-
  copy_term((X,Y,Term),(CX,CY,Copy),Goals),
  (Goals==[]
  ->subst0011a_p2(P2, X, Y, Term, NewTerm )
  ;(subst0011a_p2(P2, CX, CY, Goals, NewGoals),
     (NewGoals==Goals ->
       subst0011a_p2(P2, X, Y, Term, NewTerm )
       ; (subst0011a_p2(P2, CX, CY, Copy, NewCopy),
          NewTerm = NewCopy, maplist(call,NewGoals))))).

subst0011a_p2(P2, X, Y, Term, NewTerm ) :-
 (p2_call(P2,X,Term)-> Y=NewTerm ;
  (is_list(Term)-> maplist(subst0011a_p2(P2, X, Y), Term, NewTerm );
   (( \+ compound(Term); Term='$VAR'(_))->Term=NewTerm;
     ((compound_name_arguments(Term, F, Args),
       maplist(subst0011a_p2(P2, X, Y), Args, ArgsNew),
        compound_name_arguments( NewTerm, F, ArgsNew )))))),!.



ppa(FF):-
  copy_term(FF,FA,GF),
  numbervars(FA+GF,0,_,[attvar(bind),singletons(true)]),
  sort_safe(GF,GS),write(' '),
  locally(b_setval(arc_can_portray,nil),
      ppawt(FA)),format('~N'),
  ignore((GS\==[], format('\t'),ppawt(attvars=GS),nl)),nl,!.

ppawt(FA):-
  write_term(FA,[numbervars(false), quoted(true),
   character_escapes(true),cycles(true),dotlists(false),no_lists(false),
    blobs(portray),attributes(dots),
    portray(true), partial(false), fullstop(true),
    %portray(false), partial(true), fullstop(true),
   ignore_ops(false), quoted(true), quote_non_ascii(true), brace_terms(false)]).

intersection(APoints,BPoints,Intersected,LeftOverA,LeftOverB):-
  intersection_univ(APoints,BPoints,Intersected,LeftOverA,LeftOverB),!.

same_univ(A,B):- (plain_var(A)->A==B;(B=@=A->true; (fail, \+ (A \=B )))).

intersection_univ(APoints,BPoints,Intersected):-
  intersection_univ(APoints,BPoints,Intersected,_,_),!.
intersection_univ(APoints,BPoints,Intersected,LeftOverA,LeftOverB):-
  pred_intersection(same_univ,APoints,BPoints,Intersected,_,LeftOverA,LeftOverB).

intersection_eq(APoints,BPoints,Intersected):-
  intersection_eq(APoints,BPoints,Intersected,_,_),!.
intersection_eq(APoints,BPoints,Intersected,LeftOverA,LeftOverB):-
  pred_intersection(same_univ,APoints,BPoints,Intersected,_,LeftOverA,LeftOverB).

/*
intersection_u([],LeftOverB,[],[],LeftOverB):-!.
intersection_u(LeftOverA,[],[],LeftOverA,[]):-!.
intersection_u([A|APoints],BPoints,[A|Intersected],LeftOverA,LeftOverB):-
  select(A,BPoints,BPointsMinusA),!,
  intersection_u(APoints,BPointsMinusA,Intersected,LeftOverA,LeftOverB).
intersection_u([A|APoints],BPoints,Intersected,[A|LeftOverA],LeftOverB):-
  intersection_u(APoints,BPoints,Intersected,LeftOverA,LeftOverB).
*/

:- meta_predicate(each_obj(?,?,0)).
each_obj([],_,_):-!.
each_obj([Obj|List],Obj,Goal):- ignore(Goal), each_obj(List,Obj,Goal).

pred_intersection(_P2, [],LeftOverB, [],[], [],LeftOverB):-!.
pred_intersection(_P2, LeftOverA,[], [],[], LeftOverA,[]):-!.
pred_intersection(P2, [A|APoints],BPoints,[A|IntersectedA],[B|IntersectedB],LeftOverA,LeftOverB):-
  select(B,BPoints,BPointsMinusA),
  \+ \+ p2_call(P2, A,B),!,
  pred_intersection(P2, APoints,BPointsMinusA,IntersectedA,IntersectedB,LeftOverA,LeftOverB).
pred_intersection(P2, [A|APoints],BPoints,IntersectedA,IntersectedB,[A|LeftOverA],LeftOverB):-
  pred_intersection(P2, APoints,BPoints,IntersectedA,IntersectedB,LeftOverA,LeftOverB).



















pp(PP):-pp_m(PP).
pp(Color,PP):- ansi_format([fg(Color)],'~@',[pp(PP)]).


warn_skip(P):- pp(warn_skip(P)).

with_set_stream(_,_,G):- call(G).

fake_impl(M:F/A):- functor(P,F,A), asserta((M:P :- !, fail)).
fake_impl(F/A):- functor(P,F,A), asserta((P :- !, fail)).


:- fake_impl(arc_setval/3).
:- fake_impl(cast_to_grid/3).
:- fake_impl(dot_cfg:dictoo_decl/8).
:- fake_impl(get_param_sess/2).
:- fake_impl(into_list/2).
:- fake_impl(into_type/3).
:- fake_impl(is_grid/1).
:- fake_impl(is_hooked_obj/1).
:- fake_impl(is_vm_map/1).
:- fake_impl(ld_logicmoo_webui/0).
:- fake_impl(must_grid_call/3).
:- fake_impl(o_m_v/3).
:- fake_impl(quick_test/1).
:- fake_impl(url_decode_term/2).
:- fake_impl(xlisting_web:find_http_session/1).
:- fake_impl(xlisting_web:is_cgi_stream/0).


end_of_file.














































































:- encoding(iso_latin_1).
/*
  this is part of (H)MUARC  https://logicmoo.org/xwiki/bin/view/Main/ARC/

  This work may not be copied and used by anyone other than the author Douglas Miles
  unless permission or license is granted (contact at business@logicmoo.org)
*/


:- meta_predicate(print_grid(+,+,+,+)).
:- meta_predicate(print_grid(+,+,+)).


%:- autoload(library(http/html_write),[html/3,print_html/1]).


is_debugging(M):- \+ \+ debugging(M),!.
is_debugging(_):- is_testing,!.
%is_debugging(_):- menu_or_upper('B').

debug_m(_,Tiny):- display_length(Tiny,Len),Len<30,!,pp(Tiny).
debug_m(M,_):- \+ is_debugging(M),!.
%debug_m(_,List):- is_list(List),!,print_ss(List).
debug_m(_,Term):- pp(Term).
debug_c(M,_):- \+ is_debugging(M),!.
debug_c(_,C):- call(C),!.
debug_c(M,C):- wots_hs(S,C),debug_m(M,S),!.

:- meta_predicate(wno(0)).
wno(G):-
 locally(b_setval(print_collapsed,10), G).

:- meta_predicate(print_collapsed(0)).
print_collapsed(Size,G):-
 locally(b_setval(print_collapsed,Size), print_collapsed0(Size,G)).

:- meta_predicate(print_collapsed0(0)).
print_collapsed0(Size,G):- Size<10, !, call(G).
% print_collapsed(Size,G):-  call(G).
print_collapsed0(Size,G):- Size>=10, !, wots_hs(_S,G).
print_collapsed0(_,G):- wots_vs(S,G),write(S).

tersify(I,O):- tracing,!,I=O.
%tersify(I,O):- term_variables(I,Vs), \+ ( member(V,Vs), attvar(V)),!,I=O.
tersify(I,O):- tersify23(I,O),!.
tersify(X,X):-!.

tersify23(I,O):- quietly((tersify2(I,M),tersify3(M,O))),!.

%srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
%srw_arc(I,O):- compound(I),!, wots_hs(O,(write(ppt(I)))).
/*
srw_arc(I,O):- is_grid(I),!, wots_hs(O,(write('"'),print_grid(I),write('"'))).
*/
srw_arc(I,O):- is_vm_map(I),!, O='..vvmm..'.
srw_arc(I,O):- is_grid(I),!, O='..grid..'.
/*
srw_arc(List,O):- current_prolog_flag(dmsg_len,Three),
  is_list(List),length(List,L),L>Three,
   append([A,B,C],[F|_],List),F \='...'(_), !,
  simplify_goal_printed([A,B,C,'....'(L>Three)],O).
*/
%srw_arc(gridFn(_),gridFn):-!.
%srw_arc(I,O):- is_points_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
%srw_arc(I,O):- is_list(I), length(I,N),N>10,!,O='..lo_points..'(N),!.
srw_arc(I,O):- tersify(I,O),!,I\==O,!.

:- multifile(dumpst_hook:simple_rewrite/2).
:- dynamic(dumpst_hook:simple_rewrite/2).

dumpst_hook:simple_rewrite(I,O):- fail, notrace(catch(arc_simple_rewrite(I,O),_,fail)).

arc_simple_rewrite(I,O):-
  \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(bfly_startup/0),
  current_predicate(is_group/1),
  b_setval(arc_can_portray,nil),
  locally(b_setval(arc_can_portray,nil),once((compound(I), lock_doing(srw_arc,I,srw_arc(I,O))))), I\==O, I\=@=O, !, \+ I=O,
  b_setval(arc_can_portray,t).


%:- set_prolog_flag(never_pp_hook, true).


portray_terse:- true,!.

:- discontiguous arc_portray/2.


arc_portray(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
arc_portray(_,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t), !, fail.
arc_portray(Map,TF):- get_map_pairs(Map,Type,Pairs),!, arc_portray_pairs(Type,TF,Pairs).

arc_portray_t(G, _):- is_vm_map(G), !, write_map(G,'arc_portray_t').
arc_portray_t(G, _):- is_grid(G),  !, data_type(G,W),writeq(grid(W)).
arc_portray_t(G, _):- print(G),!.

arc_portray(G, _):- is_vm_map(G),  !, write_map(G,'arc_portray').
arc_portray(G, TF):- TF == true, portray_terse, arc_portray_t(G, TF),!.
arc_portray(G, TF):- catch(arc_portray_nt(G, TF),E,(writeln(E),never_let_arc_portray_again,fail)),!.
%arc_portray(G, _TF):- writeq(G),!.

% Portray In Debugger

arc_portray_nt(G, false):- is_grid(G), print_grid(G),!.
%arc_portray_nt([G|L],_False):- is_object(G), !, pp([G|L]).
%arc_portray_nt(G0, true):- is_group(G0), ppt(G0),!.
%arc_portray_nt(G0, false):- is_group(G0), ppt(G0),!.
arc_portray_nt(G0, Tracing):- is_group(G0), into_list(G0,G), length(G,L),% L>1, !,
   maplist(tersify,G0,GG), write(GG),
   if_t(Tracing==false,
    in_cmt((
     dash_chars,
     once(((why_grouped(_TestID,Why,WG),WG=@=G,fail);(Why = (size2D=L)))),!,
     print_grid(Why,G),nl_now,

     %underline_print(writeln(Why)),
     %print_info_l(G),
     dash_chars))).


arc_portray_nt(G,_False):- is_object(G), wots(S,writeg(G)),
  global_grid(G,GG),!,
  print_grid(GG),
  write(S),!. % show_indiv(S,G).
  %object_grid(G,OG),
  %neighbor_map(OG,NG), !,
  %print_grid(object_grid,NG),nl_now,
  %underline_print(print_info(G)),

arc_portray_nt(G,false):- via_print_grid(G),!, grid_size(G,H,V),!,H>0,V>0, print_grid(H,V,G).

% Portray In tracer
arc_portray_nt(G,true):- is_object(G),underline_print((ppt(G))).
arc_portray_nt(G,true):- via_print_grid(G),write_nbsp,underline_print((ppt(G))),write_nbsp.
arc_portray_nt(G,true):- tersify(G,O),write_nbsp,writeq(O),write_nbsp.
arc_portray_nt(G0, _):- \+ is_gridoid(G0),!,print(G0).


arc_portray_pairs(Type,TF,Pairs):-
  length(Pairs,N),
  writeln(arc_portray_pairs(Type,TF,len(N))),
  swap_kv(Pairs,VKPairs),
  keysort(VKPairs,SVKPairs),
  my_maplist(tc_arg(2),SVKPairs,SVKPairs2),
  arc_portray_type_pairs(TF,SVKPairs2).

arc_portray_type_pairs(TF,Pairs):- append(Left,[K1-V1,K2-V2|Right],Pairs),is_grid(V1),is_grid(V2),!,
  append(Left,[call-print_side_by_side(yellow,V1,K1,_,V2,K2)|Right],PairsM),
  arc_portray_type_pairs(TF,PairsM).
arc_portray_type_pairs(TF,Pairs):-
  forall(member(K-V,Pairs),arc_portray_pair(Pairs,K,V,TF)).

swap_kv([_-V|Pairs],VKPairs):- plain_var(V),!, swap_kv(Pairs,VKPairs).
swap_kv([K-V|Pairs],['-'(Type,K-V)|VKPairs]):-
  data_type(V,Type),
  swap_kv(Pairs,VKPairs).
swap_kv([],[]).


arc_portray_pair(Ps,K,Val,TF):-
 nl_if_needed,
 arc_portray_1_pair(Ps,K,Val,TF),
 nl_if_needed_ansi.

arc_portray_1_pair(_Ps,call,Val,_TF):- !, call(Val).
arc_portray_1_pair(Ps,K,Val,TF):-
 (via_print_grid(Val) -> print_grid(K,Val)
   ;  (print(K),write('= '),once(arc_portray(Val,TF);print(Val)))),
 ignore(arc_portray_pair_optional(Ps,K,Val,TF)),!.

arc_portray_pair_optional(Ps,K,Val,TF):-
 once(( Val\==[], is_list(Val),my_maplist(is_object,Val),
  print_info(Val),
  Val \= [_],
  compare_objects(Val,Diffs),
  color_print(cyan,call(arc_portray_pair(Ps,diffs(K),Diffs,TF))))).


% arc_portray(G):- \+ \+ catch((wots_hs(S,( tracing->arc_portray(G,true);arc_portray(G,false))),write(S),ttyflush),_,fail).
arc_portray(G):- \+ compound(G),fail.
arc_portray(G):- is_vm(G), !, write('..VM..').
arc_portray(G):- \+ nb_current(arc_portray,t),\+ nb_current(arc_portray,f),is_print_collapsed,!,
  locally(nb_setval(arc_portray,t),arc_portray1(G)).
arc_portray(G):- \+ nb_current(arc_portray,f),!, locally(nb_setval(arc_portray,t),arc_portray1(G)).
arc_portray(G):- locally(nb_setval(arc_portray,f),arc_portray1(G)).

arc_portray1(G):-
 flag(arc_portray_current_depth,X,X), X < 3,
 \+ \+
  setup_call_cleanup(flag(arc_portray_current_depth,X,X+1),catch(((tracing->arc_portray(G,true);
  arc_portray(G,false)),ttyflush),E,(fail,format(user_error,"~N~q~n",[E]),fail)),flag(arc_portray_current_depth,_,X)).


%via_print_grid(G):- tracing,!,fail.
via_print_grid(G):- is_points_list(G). %,!,fail,grid_size(G,H,V),number(H),number(V),H>1,V>1.
via_print_grid(G):- is_grid(G).
via_print_grid(G):- is_obj_props(G),!,fail.
via_print_grid(G):- is_object(G).
via_print_grid(G):- is_group(G).
via_print_grid(G):- is_gridoid(G).



terseA(_,[],[]):- !.
terseA(_,L,'... attrs ...'(N)):- is_list(L),length(L,N),N>10,!.
terseA(I,[A|L],[B|LL]):-terseA(I,A,B),terseA(I,L,LL),!.
terseA(I,dif(A,B),B):-A==I,!.
terseA(I,dif(B,A),B):-A==I,!.
terseA(_,put_attr(_,B,A),A):- B==ci,!.
terseA(_,put_attr(_,B,A),B=A):-!.
terseA(_,A,A):-!.


simple_enough(I):- plain_var(I).
simple_enough(I):- atomic(I).
simple_enough(I):- \+ compound(I),!.
simple_enough(_*_):-!.
simple_enough(_+_):-!.
simple_enough(A):- functor(A,_,1),tc_arg(1,A,E),!,simple_enough(E).
%simple_enough(I):- number(I).
%simple_enough(I):- atom(I).

tersify0(I,O):- simple_enough(I),!,I=O.
tersify0(I,av(C,Others)):- attvar(I),copy_term(I,C,Attrs),terseA(C,Attrs,Others),!.
tersify0(I,I):- var(I),!.


%tersifyC(D):- is_vm_map(D),!.
tersifyC(av(_,_)).
tersifyC(objFn(_,_)).
tersifyC(groupFn(_,_)).
tersifyC(objFn(_)).
tersifyC(groupFn(_)).

tersify1(I,O):- simple_enough(I),!,I=O.
tersify1(av(_,Blue), -(Blue)):-!.
tersify1(I,O):- compound(I), tersifyC(I),!,I=O.
tersify1(gridFn(I),gridFn(I)):-!. % tersifyG(I,O).
%tersify1(gridFn(I),gridFn(O)):-tersifyG(I,O).
tersify1(Nil,[]):- Nil == [],!.
tersify1(I,gridFn(S)):- is_grid(I), into_gridnameA(I,O),!,sformat(S,'~w',[O]).
tersify1(I,gridFn(O)):- is_grid(I),tersifyG(I,O),!.
tersify1(I,groupFn(O,List)):- is_group(I), mapgroup(tersify1,I,List),mapgroup(obj_to_oid,I,OIDs),length(List,N), !,ignore((get_current_test(TestID),is_why_grouped(TestID,N,Why,OIDs),!,O=Why)).

tersify1(I,Q):- is_object(I),object_ref_desc(I,Q),!.
tersify1(I,O):- is_vm_map(I), get_kov(objs,I,_),!, O='$VAR'('VM').
tersify1(I,O):- is_vm_map(I), get_kov(pairs,I,_),!, O='$VAR'('Training').


tersifyG(I,O):- tersifyL(I,O),numbervars(O,1,_,[attvar(bind),singletons(false)]),!.

%tersifyL(I,I):- is_ftVar(I),!.
%tersifyL(I,I):- \+ compound(I),!.
tersifyL(I,O):- \+ is_cons(I),!,O=I.
tersifyL([H|I],[HH|I]):- \+ is_list(I),!,tersify(H,HH).
tersifyL([H|I],O):- nonvar(H), \+ is_group(I), display_length(I,N) , N>170,
  length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O),!.
tersifyL(I,O):- tersify0(I,O),!.
tersifyL([H|TT],[HH|TT]):- tersify(H,HH),!,tersifyL(TT,TT),!.
tersifyL(I,O):- tersify1(I,O),!.
tersifyL(I,I).

tersify2(I,O):- compound(I),(I=(N=V)),tersify2(N,NN),tersify2(V,VV),!,O=(NN=VV).
tersify2(I,O):- simple_enough(I),!,I=O.
tersify2(I,O):- compound(I),tersify1(I,O),!.
tersify2(I,O):- tersify0(I,O),!.
tersify2(I,O):- is_list(I), !, my_maplist(tersify2,I,O).
tersify2(I,O):- compound(I), !, compound_name_arguments(I,F,IA), my_maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify2(I,I).

tersify3(I,O):- compound(I),(I=(N=V)),tersify3(N,NN),tersify3(V,VV),!,O=(NN=VV).
tersify3(I,O):- simple_enough(I),!,I=O.
tersify3(I,O):- compound(I),tersify1(I,O),!.
tersify3(I,O):- tersify0(I,O),!.
tersify3([H|I],O):- is_list(I), ((display_length(I,N), N>170) ->
  (length(I,LL),tersify(H,HH),(('...'(HH,LL,'...'(N)))=O)); I=O),!.
tersify3(I,O):- is_list(I), !, my_maplist(tersify3,I,O).
tersify3(I,O):- compound(I), !, compound_name_arguments(I,F,IA), my_maplist(tersify,IA,OA), compound_name_arguments(O,F,OA).
tersify3(I,I).

write_map(G,Where):- is_vm(G), !, write('...VM_'),write(Where),write('...').
write_map(G,Where):- is_vm_map(G), !, write('...Map_'),write(Where),write('...').
write_map(G,Where):- is_dict(G), !, write('...Dict_'),write(Where),write('...').
write_map(_G,Where):- write('...'),write(Where),write('...').



non_empty_wqs_c(V):- \+ empty_wqs_c(V).
empty_wqs_c(V):- var(V),!,fail.
empty_wqs_c(A):- atom(A),atom_string(A,S),!,empty_wqs_c(S).
empty_wqs_c([]).
empty_wqs_c("").
empty_wqs_c("&nbsp;").
empty_wqs_c(" ").
empty_wqs_c("\n").

is_writer_goal(H):- \+ callable(H),!,fail.
is_writer_goal(H):- is_list(H),!,fail.
is_writer_goal(A):- atom(A),!,is_writer_goal_f(A).
is_writer_goal(H):- \+ compound(H),!,fail.
%is_writer_goal((C1,C2)):- !, (is_writer_goal(C1);is_writer_goal(C2)).
is_writer_goal(C):- compound_name_arity(C,F,_),once(is_writer_goal_f(F);(tc_arg(_,C,E),is_writer_goal(E))).


is_writer_goal_f(wqs_c).
is_writer_goal_f(F):- is_writer_goal_l(F),!.
is_writer_goal_f(F):- \+ atom(F),!, term_to_atom(F,A),is_writer_goal_f(A).
is_writer_goal_f(F):- not_writer_goal_r(R),atom_concat(_,R,F),!,fail.
is_writer_goal_f(F):- is_writer_goal_l(L),atom_concat(L,_,F),!.
is_writer_goal_f(F):- is_writer_goal_l(R),atom_concat(_,R,F),!.
not_writer_goal_r(test). is_writer_goal_l(msg). is_writer_goal_l(call).
is_writer_goal_l(nl).  is_writer_goal_l(format). is_writer_goal_l(with_).
is_writer_goal_l(locally).

is_writer_goal_l(html).  is_writer_goal_l(ptcol).  is_writer_goal_l(wots).
is_writer_goal_l(print). is_writer_goal_l(flush_output).  is_writer_goal_l(wqs).
is_writer_goal_l(pp). is_writer_goal_l(write).  is_writer_goal_l(dash_).


maybe_color(SS,_):- term_contains_ansi(SS),!, write_nbsp, write(SS).
maybe_color(SS,P):- term_contains_ansi(P),!, write_nbsp, write(SS).
maybe_color(SS,P):- pp_msg_color(P,C), ansicall(C,is_maybe_bold(P,write(SS))),!.

write_atom(S):- \+ atom(S),!,wqs(S).
write_atom(S):- atom_contains(S,'~'),!,notrace(catch(format(S,[]),_,maybe_write_atom_link(S))).
write_atom(S):- maybe_write_atom_link(S),!.
write_atom(S):- into_title_str(S,TS),write(TS),!.

:- meta_predicate(into_title_str(+,-)).
into_title_str(Term,Str):- string(Term),!,Str=Term.
into_title_str(Term,Str):- plain_var(Term),sformat(Str,'~p',[Term]),!.
into_title_str(Term,Str):- var(Term),tersify0(Term,Terse), sformat(Str,'~p',[Terse]),!.
into_title_str(Term,Str):- term_is_ansi(Term), wots(Str,write_keeping_ansi_mb(Term)),!.
into_title_str(Term,Str):- (is_codelist(Term);is_charlist(Term)),catch(sformat(Str,'~s',[Term]),_,sformat(Str,'~p',[Term])),!.
into_title_str(Term,Str):- is_list(Term),my_maplist(into_title_str,Term,O3),atomics_to_string(O3," ",Str),!.
into_title_str([H|T],Str):- into_title_str(H,A),into_title_str(T,B),atomics_to_string([A,B]," ",Str),!.
into_title_str(Term,Str):- \+ callable(Term),sformat(Str,'~p',[Term]),!.
into_title_str(format(Fmt,Args),Str):- sformat(Str,Fmt,Args),!.
into_title_str(Term,""):- empty_wqs_c(Term),!.
into_title_str(out,"Output").
into_title_str(in,"Input").
into_title_str(i,"IN").
into_title_str(o,"OUT").
into_title_str(Term,Str):- atom(Term),is_valid_linkid(Term,Kind,_),Term\=@=Kind,into_title_str(Kind,KS),sformat(Str,'~w (~w)',[Term,KS]),!.
into_title_str(Term,Str):- atom(Term), atom_contains(Term,'_'), \+ atom_contains(Term,' '),  to_case_breaks(Term,T),
 include(\=(xti(_,punct)),T,O),my_maplist(tc_arg(1),O,O1),my_maplist(toProperCamelAtom,O1,O2),
  atomics_to_string(O2," ",Str),!.
into_title_str(Term,Str):- has_short_id(Term,Kind,ID),Term\=@=Kind,into_title_str(Kind,KS),sformat(Str,'~w (~w)',[ID,KS]),!.

into_title_str(T-U,Str):- into_title_str([some(T),"..to..",some(U)],Str).
into_title_str(T*U,Str):- into_title_str([some(T),"(",some(U),")"],Str).
into_title_str(T+U,Str):- into_title_str(T,S1), number(U), N is U+1, sformat(Str,'~w #~w',[S1,N]).
into_title_str(T+U,Str):- var(U), into_title_str(T,S1), sformat(Str,'~w(s)',[S1]).
into_title_str(title(Term),Str):- !, into_title_str(Term,Str),!.
into_title_str(some(Var),"Some"):- var(Var),!.
into_title_str(some(Var),Str):- !, into_title_str(Var,Str).
into_title_str(User:Term,Str):- User == user, !, into_title_str(Term,Str).
into_title_str(trn,"Training Pair").
into_title_str(tst,"EVALUATION TEST").
%into_title_str(Term,Str):- tersify23(Term,Terse),Term\=@=Terse,!,into_title_str(Terse,Str).
into_title_str(Term,Str):- callable_arity(Term,0),is_writer_goal(Term),catch(notrace(wots(Str,call_e_dmsg(Term))),_,fail),!.
into_title_str(Term,Str):- catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)),nonvar(Str),atom_length(Str,E50),E50<180,!.
into_title_str(Term,Str):- compound(Term), compound_name_arguments(Term,Name,Args),
   %include(not_p1(plain_var),Args,Nonvars),
   Args=Nonvars,
   my_maplist(tersify,Nonvars,ArgsT), into_title_str([Name,"(",ArgsT,")"],Str),!.
into_title_str(Term,Str):- catch(sformat(Str,'~p',[Term]),_,term_string(Term,Str)).

has_short_id(TestID,testid,UUID):- is_valid_testname(TestID),test_id_atom(TestID,UUID).
has_short_id(Obj,object,OID):- is_object(Obj),obj_to_oid(Obj,OID).
has_short_id(Grid,grid,GID):- is_grid(Grid),grid_to_gid(Grid,GID).


is_valid_linkid(ID,testid,TestID):- atom_id(ID,TestID),is_valid_testname(TestID),!.
is_valid_linkid(ID,object,Obj):- known_object(ID,Obj),!.
is_valid_linkid(ID,grid,Grid):- known_grid(ID,Grid),!.
% individuate_3(complete, two(v_1d398264_trn_0_in, v_1d398264_trn_0_out))
is_valid_linkid(ID,group,Grp):- get_current_test(TestID),is_why_grouped_g(TestID,_Count,ID,Grp).


wqs_c(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
wqs_c(S):- (string(S);is_codelist(S);is_charlist(S)),catch(format('~s',[S]),_,writeq(S)).
wqs_c(S):- empty_wqs_c(S),!.
wqs_c(S):- var(S),!,write(var(S)).
wqs_c(S):- atom(S),into_title_str(S,TS),write(TS),!.
wqs_c(S):- atom(S),write_atom(S),!.
%wqs_c(S):- atom(S),write(S),!.
wqs_c(S):- \+compound(S),!,notrace(catch(format('~p',[S]),_,write(S))).
wqs_c(title(S)):- !, wqs_c(S).
wqs_c(H+T):- !, wqs_c(H),write_nbsp,wqs_c(T).
wqs_c(S):- is_grid(S), print_grid(S),!.
wqs_c(S):- is_vm(S), pp(S) ,!.
wqs_c(L):- is_list(L), include(non_empty_wqs_c,L,LL),!,wqs_c_l(LL).
wqs_c([H|T]):- pp([H|T]),!.
wqs_c(H):- callable_arity(H,0),is_writer_goal(H),catch(call_e_dmsg(H),_,fail),!.
%wqs_c(H):- callable_arity(H,0),call(H),!.
wqs_c(H):- locally(t_l:wqs_fb(pp_no_nl),wqs(H)),!.

wqs_c_l([]):-!.
wqs_c_l([H]):- wqs_c(H),!.
wqs_c_l([H|T]):- wqs_c(H),write_nbsp,wqs_c_l(T),!.





ppt(_):- is_print_collapsed,!.
ppt(G):- stack_check_or_call(4000,writeq(G)),!.
ppt(G):- is_vm_map(G), !, write_map(G,'ppt').
ppt(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
%ppt(P):- compound(P),wqs1(P),!.

ppt(P):- \+ ansi_main, wants_html,!,ptcol_html(P),write_br.
ppt(P):- \+ \+ ((tersify(P,Q),!,pp(Q))),!.
ppt(Color,P):- \+ ansi_main, wants_html,!,with_color_span(Color,ptcol_html(P)),write_br.
ppt(Color,P):- \+ \+ ((tersify(P,Q),!,pp(Color,Q))),!.


write_br:- ansi_main,!,nl.
write_br:- write('<br>').

ptc(Color,Call):- pp(Color,call(Call)).

:- meta_predicate(ppnl(+)).
ppnl(Term):- is_list(Term),!,g_out(wqs(Term)).
ppnl(Term):- nl_if_needed,format('~q',[Term]),nl_if_needed_ansi.

:- meta_predicate(pp(+)).
pp(Color,P):- \+ ansi_main, wants_html,!,with_color_span(Color,pp(P)),write_br.
pp(Color,P):- ignore((quietlyd((wots_hs(S,pp(P)),!,color_print(Color,S))))).

pp(_):- is_print_collapsed,!.
%pp(Term):- is_toplevel_printing(Term), !, nl_if_needed, pp_no_nl(Term),!,nl_if_needed_ansi.
pp(_Term):- nl_if_needed, fail.
pp(Term):- \+ ansi_main, wants_html,!, wots_vs(SS,ptcol_html_scrollable(Term)),write(SS),write_br.
pp(Term):- \+ nb_current(arc_can_portray,_),!,locally(nb_setval(arc_can_portray,t),print(Term)).
pp(Term):- az_ansi(pp_no_nl(Term)),!,nl_if_needed_ansi.

/*
ptcol(P):- wants_html,!,ptcol_html(P).
ptcol(call(P)):- callable(P),!,call(P).
ptcol(P):- pp(P).
*/

%ptcol_html(P):- ptcol_html_0(P).
ptcol_html(P):- ptcol_html_scrollable_0(P).
ptcol_html_scrollable(P):- with_tag_ats(div,scrollable,ptcol_html_scrollable_0(P)).


ptcol_html_0(P):- with_tag(pre,ptcol_html_wo_pre(P)).
ptcol_html_wo_pre(call(P)):- callable(P),!, in_pp_html(call(P)).
ptcol_html_wo_pre(P):- in_pp_html(print_tree_no_nl(P)).
ptcol_html_scrollable_0(P):- ptcol_html_wo_pre(P).


pp_wcg(G):- wants_html,!,ptcol_html_scrollable(G).
pp_wcg(G):- pp_safe(call((locally(nb_setval(arc_can_portray,t),print(G))))),!.

wqln(Term):- ppnl(Term).
wqnl(G):- pp_safe(call((locally(nb_setval(arc_can_portray,nil),print(G))))),!.

pp_safe(_):- nb_current(pp_hide,t),!.
pp_safe(call(W)):- !, nl_if_needed,nl_now,call(W),nl_now.
pp_safe(W):- nl_if_needed,nl_now,writeq(W),nl_now.
pp_safe(C,W):- color_print(C,call(pp_safe(W))).


%p_p_t_no_nl(Term):- is_toplevel_printing(Term), !, print_tree_no_nl(Term).

p_p_t_no_nl(P):- \+ ansi_main, wants_html,!,ptcol_html(P).
p_p_t_no_nl(Term):- az_ansi(print_tree_no_nl(Term)).

ppt_no_nl(P):- \+ ansi_main, wants_html,!,ptcol_html(P).
ppt_no_nl(P):- tersify(P,Q),!,pp_no_nl(Q).

is_toplevel_printing(_):- \+ is_string_output, line_position(current_output,N),  N<2, fail.

pp_no_nl(P):- var(P),!,pp(var_pt(P)),nop((dumpST,ibreak)).
pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- atom(P),atom_contains(P,'~'),!,format(P).
pp_no_nl(G):- is_vm_map(G), !, write_map(G,'pp').
%pp_no_nl(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_no_nl(P):- \+ \+ (( pt_guess_pretty(P,GP),ptw(GP))).
%pp(P):-!,writeq(P).
%ptw(P):- quietlyd(p_p_t_nl(P)),!.
%ptw(_):- nl_if_needed,fail.
ptw(P):- var(P),!,ptw(var_ptw(P)),nop((dumpST,ibreak)).
ptw(G):- is_vm_map(G), !, write_map(G,'ptw').
ptw(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
ptw(P):- p_p_t_no_nl(P),!.

%ptw(P):- quietlyd(write_term(P,[blobs(portray),quoted(true),quote_non_ascii(false), portray_goal(print_ansi_tree),portray(true)])),!.
print_ansi_tree(S,_):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
print_ansi_tree(P,_):- catch(arc_portray(P),_,(never_let_arc_portray_again,fail)),!.
print_ansi_tree(P,_OL):- catch(p_p_t_no_nl(P),_,(never_let_arc_portray_again,fail)),!.

%p_p_t_nl(T):- az_ansi(print_tree_nl(T)).
%p_p_t(T):- az_ansi(print_tree(T)).

pt_guess_pretty(P,O):- \+ nb_current(in_pt_guess_pretty,t), locally(nb_setval(in_pt_guess_pretty,t),pt_guess_pretty_1(P,O)).
pt_guess_pretty(O,O).

upcase_atom_var_l(IntL,NameL):- upcase_atom_var(IntL,NameL).
upcase_atom_var_l(IntL,NameL):- is_list(IntL),!,my_maplist(upcase_atom_var_l,IntL,NameL).

pt_guess_pretty_1(P,O):- copy_term(P,O,_),
  ignore((sub_term(Body,O), compound(Body), Body=was_once(InSet,InVars),upcase_atom_var_l(InSet,InVars))),
  ignore(pretty1(O)),ignore(pretty_two(O)),ignore(pretty_three(O)),ignore(pretty_final(O)),!,
  ((term_singletons(O,SS),numbervars(SS,999999999999,_,[attvar(skip),singletons(true)]))).

:- dynamic(pretty_clauses:pp_hook/3).
:- multifile(pretty_clauses:pp_hook/3).
:- module_transparent(pretty_clauses:pp_hook/3).
pretty_clauses:pp_hook(FS,Tab,S):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),  notrace(catch(arc_pp_hook(FS,Tab,S),_,fail)).

arc_pp_hook(_,Tab,S):- term_is_ansi(S), !,prefix_spaces(Tab), write_keeping_ansi_mb(S).
%arc_pp_hook(_,Tab,S):- is_vm(S),!,prefix_spaces(Tab),!,write('..VM..').
%arc_pp_hook(_,  _,_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
arc_pp_hook(FS,_  ,G):- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),
  current_predicate(is_group/1),
   locally(b_setval(pp_parent,FS),
     print_with_pad(pp_hook_g(G))),!.

pp_parent(PP):- nb_current(pp_parent,PP),!.
pp_parent([]):-!.

%:- meta_predicate(lock_doing(+,+,0)).
:- meta_predicate(lock_doing(+,+,:)).
lock_doing(Lock,G,Goal):-
 (nb_current(Lock,Was);Was=[]), !,
  \+ ((member(E,Was),E==G)),
  locally(nb_setval(Lock,[G|Was]),Goal).

never_let_arc_portray_again:- set_prolog_flag(never_pp_hook, true),!.
arc_can_portray:- \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t).

arcp:will_arc_portray:-
   \+ current_prolog_flag(never_pp_hook, true),
   \+ nb_current(arc_can_portray,f),
   %nb_current(arc_can_portray,t),
   current_prolog_flag(debug,false),
   \+ tracing,
   flag(arc_portray_current_depth,X,X),X<3,
   current_predicate(bfly_startup/0).

user:portray(Grid):-
  arcp:will_arc_portray, \+ \+ catch(quietly(arc_portray(Grid)),_,fail),!, flush_output.


pp_hook_g(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
pp_hook_g(_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
pp_hook_g(S):- term_contains_ansi(S), !, write_nbsp, pp_hook_g0(S).
pp_hook_g(G):- \+ plain_var(G), lock_doing(in_pp_hook_g,G,pp_hook_g0(G)).

pp_hook_g0(S):- term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
pp_hook_g0(_):- \+ \+ current_prolog_flag(never_pp_hook, true), nb_current(arc_can_portray,t),!,fail.
pp_hook_g0(_):- in_pp(bfly),!,fail.
pp_hook_g0(G):- wots_hs(S,in_bfly(f,pp_hook_g10(G))),write(S).

mass_gt1(O1):- into_obj(O1,O2),mass(O2,M),!,M>1.

% Pretty printing
pp_hook_g10(G):- \+ plain_var(G), current_predicate(pp_hook_g1/1), lock_doing(in_pp_hook_g10,G,pp_hook_g1(G)).

%as_grid_string(O,SSS):- is_grid(O),wots_vs(S,print_grid(O)), sformat(SSS,'{  ~w}',[S]).
as_grid_string(O,SSS):- wots_vs(S,show_indiv(O)), sformat(SSS,'{  ~w}',[S]).
as_pre_string(O,SS):- wots_hs(S,show_indiv(O)), strip_vspace(S,SS).


pretty_grid(O):-
  catch(
  (wots_hs(S,print_grid(O)),strip_vspace(S,SS),
   ptc(orange,(format('"  ~w  "',[SS])))),
  _,fail),!.
/*
pretty_grid(O):-
  catch(
  (wots_hs(S,print_grid(O)),strip_vspace(S,SS),
   ptc(orange,(format('"  ~w  "',[SS])))),
  _,(never_let_arc_portray_again,fail)).
*/
pp_hook_g1(O):-  plain_var(O), !, fail.

pp_hook_g1(O):-  attvar(O), !, is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.
pp_hook_g1(S):-  term_is_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
%pp_hook_g1(S):-  term_contains_ansi(S), !, fail, write_nbsp, write_keeping_ansi_mb(S).
pp_hook_g1(rhs(O)):- write_nbsp,nl,bold_print(print(r_h_s(O))),!.

pp_hook_g1(iz(O)):- compound(O), O = info(_),underline_print(print(izz(O))),!.
pp_hook_g1(O):-  is_grid(O), /* \+ (sub_term(E,O),compound(E),E='$VAR'(_)), */ pretty_grid(O).


pp_hook_g1(O):- is_object(O), into_solid_grid(O,G), wots(SS,pretty_grid(G)),write(og(SS)),!.

pp_hook_g1(shape_rep(grav,O)):- is_points_list(O), as_grid_string(O,S), wotsq(O,Q), print(shape_rep(grav,S,Q)),!.
pp_hook_g1(vals(O)):- !, writeq(vals(O)),!.
%pp_hook_g1(l2r(O)):- into_solid_grid_strings(l2r(O),Str),Str\=@=l2r(O),print_term_no_nl(Str),!.
pp_hook_g1(localpoints(O)):- is_points_list(O), as_grid_string(O,S), wotsq(O,Q), print(localpoints(S,Q)),!.
pp_hook_g1(C):- compound(C), compound_name_arguments(C,F,[O]),is_points_list(O), length(O,N),N>2, as_grid_string(O,S), compound_name_arguments(CO,F,[S]), print(CO),!.

pp_hook_g1(O):-  is_points_list(O),as_grid_string(O,S),write(S),!.
pp_hook_g1(O):-  is_real_color(O), color_print(O,call(writeq(O))),!.
pp_hook_g1(O):-  is_colorish(O), data_type(O,DT), writeq('...'(DT)),!.

pp_hook_g1(_):-  \+ in_pp(ansi),!, fail.


pp_hook_g1(Grp):- current_predicate(pp_ilp/1),is_rule_mapping(Grp),pp_ilp(Grp),!.

pp_hook_g1(O):- atom(O), atom_contains(O,'o_'), pp_parent([LF|_]), \+ (LF==lf;LF==objFn),
  resolve_reference(O,Var), O\==Var, \+ plain_var(Var),!,
  write_nbsp, writeq(O), write(' /* '), show_indiv(Var), write(' */ ').

pp_hook_g1(O):-  is_object(O),pp_no_nl(O), !.
pp_hook_g1(O):-  is_group(O),pp_no_nl(O), !.

%pp_hook_g1(change_obj(N,O1,O2,Sames,Diffs)):-  showdiff_objects5(N,O1,O2,Sames,Diffs),!.

pp_hook_g1(O):-  is_vm_map(O),data_type(O,DT), writeq('..map.'(DT)),!.
pp_hook_g1(O):-  is_gridoid(O),show_indiv(O), !.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  with_tagged('h5',w_section(object,[O1, O2],pp(O))).
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff), w_section(showdiff_objects(O1,O2)),!.
%pp_hook_g1(O):-  O = change_obj( O1, O2, _Same, _Diff),  w_section(object,[O1, O2],with_tagged('h5',pp(O))).
%pp_hook_g1(O):-  O = diff(A -> B), (is_gridoid(A);is_gridoid(B)),!, p_c_o('diff', [A, '-->', B]),!.
pp_hook_g1(O):-  O = showdiff( O1, O2), !, showdiff(O1, O2).
%pp_hook_g1(O):- compound(O),wqs1(O), !.
pp_hook_g1(O):- \+ compound(O),fail.
pp_hook_g1(G):- '@'(pp_hook_g1a(G),user).

pp_hook_g1a(G):- \+ current_prolog_flag(debug,true),
  current_predicate(pp_hook_g2/1), lock_doing(in_pp_hook_g3,any,pp_hook_g2(G)),!.
pp_hook_g1a(G):- fch(G),!.

%pp_hook_g2(O):- current_predicate(colorize_oterms/2),colorize_oterms(O,C), notrace(catch(fch(C),_,fail)),! .

fch(O):- wqs1(O).
%fch(O):- pp_no_nl(O).
%fch(O):- print(O).
%fch(O):- p_p_t_no_nl(O).

wotsq(O,Q):- wots_hs(Q,wqnl(O)).
has_goals(G):- term_attvars(G,AV),AV\==[].
has_goals(G):- term_variables(G,TV),term_singletons(G,SV),TV\==SV.

maybe_term_goals(Term,TermC,Goals):-
  term_attvars(Term,Attvars), Attvars\==[],!,
  term_variables(Term,Vars),
  include(not_in(Attvars),Vars,PlainVars),
  copy_term((Attvars+PlainVars+Term),(AttvarsC+PlainVarsC+TermC),Goals),
  numbervars(PlainVarsC,10,Ten1,[singletons(true),attvar(skip)]),
  numbervars(AttvarsC+Goals,Ten1,_Ten,[attvar(bind),singletons(false)]).

maybe_replace_vars([],SGoals,TermC,SGoals,TermC):-!.
maybe_replace_vars([V|VarsC],SGoals,TermC,RSGoals,RTermC):-
   my_partition(sub_var(V),SGoals,Withvar,WithoutVar),
   Withvar=[OneGoal],
   freeze(OneGoal,(OneGoal\==null,OneGoal \== @(null))),
   findall(_,sub_var(V,TermC),LL),LL=[_],!,
   subst([WithoutVar,TermC],V,{OneGoal},[SGoalsM,TermCM]),
   maybe_replace_vars(VarsC,SGoalsM,TermCM,RSGoals,RTermC).
maybe_replace_vars([_|VarsC],SGoals,TermC,RSGoals,RTermC):-
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC).


src_sameish(Orig,Find):- copy_term(Orig,COrig),Find=Orig,Orig=@=COrig.

number_vars_calc_goals(Term,SSRTermC,[1|SRSGoals]):-
  term_singletons(Term,Singles),
  term_attvars(Term,Vars),
  copy_term(Term+Vars+Singles,TermC+VarsC+SinglesC,Goals),
  notrace(catch(numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(skip)]),_,fail)),
  sort_goals(Goals,VarsC,SGoals),
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC),
  include(not_sub_var(RSGoals),SinglesC,KSingles),
  length(KSingles,SL),length(VSingles,SL),my_maplist(=('$VAR'('__')),VSingles),
  subst_2L(KSingles,VSingles,[RTermC,RSGoals],[SRTermC,SRSGoals]),
  subst_1L_p2(src_sameish,[
    {dif('$VAR'('__'),RED)}=dif(RED),
    {cbg('$VAR'('__'))}=cbg],
     SRTermC,SSRTermC),!.

number_vars_calc_goals(Term,SRTermC,[2|RSGoals]):-
  term_attvars(Term,AVars),
  copy_term(Term+AVars,TermC+VarsC,GoalsI),
  term_attvars(GoalsI,GAttvars), copy_term(GoalsI+GAttvars,_+GAttvarsC,GoalsGoals),
  append(GoalsI,GoalsGoals,Goals),
  append([VarsC,GAttvarsC,AVars,GAttvars],SortVars),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(bind)]),
  sort_goals(Goals,SortVars,SGoals),
  maybe_replace_vars(SortVars,SGoals,TermC,RSGoals,RTermC),
  subst_1L_p2(src_sameish,[
    {dif('$VAR'('___'),RED)}=dif(RED),
    {cbg('$VAR'('___'))}=cbg],
     RTermC,SRTermC),!.

number_vars_calc_goals(Term,SSRTermC,[3|SRSGoals]):-
  term_singletons(Term,Singles),
  term_attvars(Term,Vars),
  copy_term(Term+Vars+Singles,TermC+VarsC+SinglesC,Goals),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(bind)]),
  sort_goals(Goals,VarsC,SGoals),
  maybe_replace_vars(VarsC,SGoals,TermC,RSGoals,RTermC),
  include(not_sub_var(RSGoals),SinglesC,KSingles),
  length(KSingles,SL),length(VSingles,SL),my_maplist(=('$VAR'('__')),VSingles),
  subst_2L(KSingles,VSingles,[RTermC,RSGoals],[SRTermC,SRSGoals]),
  subst(SRTermC,{cbg('_')},cbg,SSRTermC),!.

number_vars_calc_goals(Term,TermC,[4|SGoals]):-
  term_variables(Term,Vars),
  term_attvars(Term,Attvars),
  copy_term(Term+Vars+Attvars,TermC+VarsC+AttvarsC,Goals),
  notrace(catch(numbervars(TermC+Goals,0,_Ten1,[singletons(true)]),_,fail)),
  append([AttvarsC,VarsC,AttvarsC,Vars],Sorted),
  sort_goals(Goals,Sorted,SGoals),!.

number_vars_calc_goals(Term,TermC,[5|SGoals]):-
  term_variables(Term,Vars),
  term_attvars(Term,Attvars),
  copy_term(Term+Vars+Attvars,TermC+VarsC+AttvarsC,Goals),
  numbervars(TermC+Goals,0,_Ten1,[singletons(false),attvar(skip)]),
  append([AttvarsC,VarsC,Attvars,Vars],Sorted),
  sort_goals(Goals,Sorted,SGoals),!.



writeg(Term):- ignore( \+ notrace(catch(once(writeg0(Term);ppa(Term)),E,(pp(E),ppa(Term))))),!.

writeg0(Term):- term_attvars(Term,Attvars),Attvars\==[],!,
  must_det_ll(((number_vars_calc_goals(Term,TermC,Goals),
  writeg5(TermC)),!,
  if_t(Goals\==[],(nl_if_needed,
    write(' goals='), call_w_pad_prev(3,az_ansi(print_tree_no_nl(Goals))))))),!.

writeg0(Term):- \+ ground(Term),
 \+ \+ must_det_ll((
  numbervars(Term,0,_Ten1,[singletons(true),attvar(skip)]), writeg5(Term))).
writeg0(Term):- writeg5(Term),!.

writeg5(X):- is_ftVar(X),!,write_nbsp,write_nbsp,print(X),write_nbsp.
writeg5(N=V):- is_simple_2x2(V),!,print_grid(N,V),writeln(' = '),call_w_pad_prev(2,writeg9(V)).
writeg5(N=V):- is_gridoid(V),!,print_grid(N,V),writeln(' = '),call_w_pad_prev(2,writeg9(V)).
writeg5(N=V):- nl_if_needed,nonvar(N), pp_no_nl(N),writeln(' = '), !, call_w_pad_prev(2,writeg5(V)).
writeg5(_):- write_nbsp, fail.
writeg5(V):- writeg9(V).

writeg8(X):- is_ftVar(X),!,print(X).
writeg8(X):- var(X),!,print(X).
writeg8(X):- writeq(X).

writeg9(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg9(V):- is_list(V),nl_if_needed,write('['),!,my_maplist(writeg5,V),write(']').
writeg9(_):- write_nbsp,write(' \t '),fail.
writeg9(X):- is_ftVar(X),!,write_nbsp,write_nbsp,print(X).
writeg9(V):- pp_no_nl(V).


/*
writeg5(V):- is_simple_2x2(V),!,print_simple_2x2(writeg8,V).
writeg5(V):- is_gridoid(V),!,call_w_pad_prev(2,writeg9(V)).
writeg5(V):- is_list(V),nl_if_needed,write('['),my_maplist(writeg5,V),write(']').
*/
arg1_near(Vars,Goal,Nth):- tc_arg(1,Goal,PreSort),nth1(Nth,Vars,E),E==PreSort,!.
arg1_near(_VarsC,Goal,PreSort):- tc_arg(1,Goal,PreSort),!.
arg1_near(_VarsC,Goal,Goal).

sort_goals(Goals,VarsC,SGoals):- predsort(sort_on(arg1_near(VarsC)),Goals,SGoals).

/*

writeg0(Obj):- is_object(Obj),pp(Obj),!.
writeg0(O):- writeg00(O).

writeg00(Term):-
  maybe_term_goals(Term,TermC,Goals),
  writeg00(TermC), call_w_pad(2,writeg00(Goals)),!.
writeg00(N=V):- nl_if_needed,nonvar(N), pp_no_nl(N),writeln(' = '), !, call_w_pad(2,writeg00(V)).
writeg00(O):- compound(O),compound_name_arguments(O,F,[A]),!,call_w_pad(2,((writeq(F),write('('),writeg3(A),write(')')))).
writeg00(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg00([H|T]):- compound(H),H=(_=_), my_maplist(writeg0,[H|T]).
writeg00([H|T]):- is_list(T),call_w_pad(2,((nl,write('['),writeg2(H),my_maplist(writeg0,T),write(']'),nl))).
%writeg0(Term):- \+ ground(Term),!, \+ \+ (numbervars(Term,99799,_,[singletons(true)]),
%   subst(Term,'$VAR'('_'),'$VAR'('_____'),TermO), writeg0(TermO)).
%writeg0(V):- \+ is_list(V),!,writeq(V),nl_now.
writeg00(V):- \+ is_list(V),!,pp(V).
writeg00(X):- call_w_pad(2,pp(X)).

writeg1(N=V):- is_gridoid(V),!,print_grid(N,V),call_w_pad(2,(my_maplist(writeg1,V))).
writeg1(X):- nl_if_needed,writeg2(X),!,write_nbsp,!.
writeg2(S):- term_contains_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- is_ftVar(X),!,print(X).
writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(true)]),!.
%writeg2(X):- write_term(X,[quoted(true),quote_non_ascii(true),portrayed(false),nl(false),numbervars(false)]),!.
%writeg1(X):- nl_if_needed,writeg(X).
writeg2(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
writeg2(X):- writeq(X),!.
writeg3(X):- is_list(X),X\==[],X=[_,_|_],!,writeg(X).
writeg3(X):- writeg2(X).
*/

% Nov 9th, 1989
/*
pp_hook_g1(T):-
 nb_current('$portraying',Was)
   ->  ((member(E,Was), T==E) -> ptv2(T) ; locally(b_setval('$portraying',[T|Was]),ptv0(T)))
   ; locally(b_setval('$portraying',[T]),ptv0(T)).
*/

%pp_hook_g(G):- compound(G),ppt(G),!.
%pp_hook_g(G):- ppt(G),!.


strip_vspace(S,Stripped):- string_concat(' ',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,' ',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\n',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\n',S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat('\t',SS,S),!,strip_vspace(SS,Stripped).
strip_vspace(S,Stripped):- string_concat(SS,'\t',S),!,strip_vspace(SS,Stripped).

strip_vspace(S,Stripped):- replace_in_string([" \n"="\n","(   "="(  ","(\n"="( "],S,S2),S2\==S,!,strip_vspace(S2,Stripped).
%strip_vspace(S,Stripped):- split_string(S, "", "\t\r\n", [Stripped]).
strip_vspace(S,S).


print_nl(P):- nl_if_needed, wots_hs(SS,pp_no_nl(P)), maybe_color(SS,P),nl_if_needed.

color_write(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
color_write(P):- wots_hs(SS,write(P)), maybe_color(SS,P).

write_keeping_ansi_mb(P):- is_maybe_bold(P,write_keeping_ansi(P)).

is_maybe_bold(P):- sformat(S,'~w',[P]),atom_contains(S,'stOF').

is_maybe_bold(P,G):- is_maybe_bold(P),!, underline_print(bold_print(G)).
is_maybe_bold(_P,G):- call(G).

pp_msg_color(P,C):- compound(P),pc_msg_color(P,C),!.
pp_msg_color(P,C):- must_det_ll(mesg_color(P,C)).
pc_msg_color(iz(P),C):- pp_msg_color(P,C).
pc_msg_color(link(P,_,_),C):- pp_msg_color(P,C).
pc_msg_color(link(P,_),C):- pp_msg_color(P,C).
pc_msg_color((_->P),C):- pp_msg_color(P,C).
pc_msg_color([P|_],C):- pp_msg_color(P,C).
pc_msg_color(diff(P),C):- pp_msg_color(P,C).

%:- meta_predicate(wots_hs(0)).
%wots_hs(G):- wots_hs(S,G),write(S).

:- meta_predicate(wots_ansi(-,0)).
wots_ansi(S,Goal):- wots(S,woto_ansi(Goal)).
:- meta_predicate(wots_ansi(-,0)).
wots_html(S,Goal):- wots(S,woto_html(Goal)).

:- meta_predicate(woto_ansi(0)).
woto_ansi(Goal):- with_toplevel_pp(ansi,Goal).
:- meta_predicate(woto_html(0)).
woto_html(Goal):- with_toplevel_pp(http,Goal).

:- meta_predicate(wots_hs(-,0)).
%wots_hs(S,G):- \+ wants_html,!,wots(S,G).
%wots_hs(S,G):- wots(S,G),!.
wots_hs(S,G):- wots(SS,G),notrace(remove_huge_spaces(SS,S)).
:- meta_predicate(wots_vs(-,0)).
wots_vs(OOO,G):- wots(S,G),notrace(fix_vspace(S,OOO)).

fix_vspace(S,OOO):-
 strip_vspace(S,SS), (atom_contains(SS,'\n') ->
   wots_hs(SSS,(nl_now,write('   '),write(SS),nl_now));SSS=SS),
   remove_huge_spaces(SSS,OOO).


write_tall(L):- is_list(L),!,my_maplist(write_tall,L).
write_tall(E):- wots_vs(S,wqs_c(E)),writeln(S).
write_wide(L):- is_list(L),!,my_maplist(write_wide,L).
write_wide(E):- wots_vs(S,wqs_c(E)),write(S),write_nbsp.

p_to_br(S,SS):- fix_br_nls(S,S0),
  cr_to_br(S0,SSS),
  replace_in_string(['<p>'='<br>','<br/>'='<br>','</p>'=' ','<p/>'='<br>','<br><br>'='<br>'],SSS,SSSS),
  cr_to_br(SSSS,SS).

cr_to_br(S,SSS):- wants_html,!,cr_to_br_html(S,SSS).
cr_to_br(S,SSS):- cr_to_br_ansi(S,SSS).

cr_to_br_html(S,SSS):- replace_in_string(['\r\n'='<br>','\r'='<br>','\n'='<br>'],S,SSS).
cr_to_br_ansi(S,SSS):- replace_in_string(['<br>'='\n','&nbsp;'=' '],S,SSS).

fix_br_nls(S,O):- replace_in_string(
 ['<br/>\n'='<br/>','<br>\n'='<br>','</p>\n'='</p>','<p/>\n'='<p/>','<p>\n'='<p>',
  '\n<br>'='<br>','\n<br/>'='<br/>','\n</p>'='</p>','\n<p/>'='<p/>','\n<p>'='<p>'],S,O).

remove_huge_spaces(S,O):- notrace((fix_br_nls(S,SS),!,p_to_br(SS,O))),!.
/*
remove_huge_spaces(S,O):- fix_br_nls(S,S0),
  replace_in_string(['          '='     ',
    '                                                                          '='     ',
    '                                                                          '='     ',
    '                                                                                                                                                                                                                                                                                                                                                                                                               '='  ',
    '                                                                                                                                                                                                                                                                                   '='   ',
    '                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        '='    ',
    '                                                                          '='     ',
    '\t'='  ',
    '                         '='     '],S0,SS),p_to_br(SS,O).
*/


wqs_l(H):- \+ is_list(H),!, wqs(H).
wqs_l(H):- wqs(H).

wqs(P):- wots_hs(SS,wqs0(P)), maybe_color(SS,P).
wqs(C,P):- ansicall(C,wqs0(P)),!.

wqs0(X):- plain_var(X), wqs(plain_var(X)),!.
wqs0(X):- plain_var(X), !, wqs(plain_var(X)), ibreak.
wqs0(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).
wqs0(C):- is_colorish(C),color_print(C,C),!.
wqs0(G):- is_vm_map(G), !, write_map(G,'wqs').
wqs0(X):- var(X), !, get_attrs(X,AVs),!,writeq(X),write('/*{'),print(AVs),write('}*/').
wqs0(X):- attvar(X), !, wqs(attvar(X)).
wqs0(nl_now):- !, nl_now. wqs0(X):- X=='', !. wqs0(X):- X==[], !.
wqs0(X):- is_grid(X), !, print_grid(X).
wqs0(G):- compound(G), G = call(C),callable(C),!,call(C).
wqs0([T]):- !, wqs(T).
wqs0([H|T]):- string(H), !, write(H), write_nbsp, wqs(T).
wqs0([H|T]):- compound(H),skip(_)=H, !,wqs(T).
wqs0([H|T]):- wqs(H), need_nl(H,T), wqs(T), !.
wqs0(X):- is_object(X), tersify1(X,Q), X\==Q,!, wqs(Q).
wqs0(X):- is_object(X), show_shape(X),!.
wqs0(X):- string(X), atom_contains(X,'~'), catch((sformat(S,X,[]),color_write(S)),_,fail),!.
wqs0(X):- string(X), !, color_write(X).
%wqs([H1,H2|T]):- string(H1),string(H2),!, write(H1),write_nbsp, wqs([H2|T]).
%wqs([H1|T]):- string(H1),!, write(H1), wqs(T).
%wqs([H|T]):- compound(H),!, writeq(H), wqs(T).

wqs0(call(C)):- !, call(C).
wqs0(X):- \+ compound(X),!, write_nbsp, write(X).
wqs0(C):- compound(C),wqs1(C),!.
wqs0(C):- wqs2(C).
%wqs(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).

wqs2(S):- term_contains_ansi(S), !, write_nbsp, write_keeping_ansi_mb(S).
%wqs2(P):- wants_html,!,pp(P).

:- thread_local(t_l:wqs_fb/1).
wqs2(X):- t_l:wqs_fb(P1),call(P1,X),!.
%wqs2(X):- with_wqs_fb(writeq,X).
wqs2(X):- with_wqs_fb(writeq,print(X)),!.
%wqs2(X):- with_wqs_fb(writeq,((write_nbsp,write_term(X,[quoted(true)])))).

with_wqs_fb(FB,Goal):-
  locally(t_l:wqs_fb(FB),Goal).


as_arg_str(C,S):- wots_vs(S,print(C)).

arg_string(S):- string(S),!.
arg_string(S):- term_contains_ansi(S),!.

wqs1(C):- \+ compound(C),!,wqs0(C).
wqs1(S):- term_is_ansi(S), !, write_keeping_ansi_mb(S).

wqs1(format(C,N)):- catch((sformat(S,C,N),color_write(S)),_,fail),!.
wqs1(writef(C,N)):- !, writef(C,N).
wqs1(q(C)):-  \+ arg_string(C),wots_hs(S,writeq(C)),color_write(S),!.
wqs1(g(C)):-  \+ arg_string(C),wots_vs(S,bold_print(wqs1(C))),print(g(S)),!.
wqs1(print_ss(C)):-  \+ arg_string(C), wots_vs(S,print_ss(C)),wqs1(print_ss(S)),!.
wqs1(b(C)):-  \+ arg_string(C), wots_vs(S,bold_print(wqs1(C))),color_write(S).
wqs1(T):- \+ is_list(T), term_contains_ansi(T),!,write_keeping_ansi_mb(T).
wqs1(norm(C)):- writeq(norm(C)),!.
wqs1(grid_rep(norm,C)):- writeq(grid_rep(norm,C)),!.
wqs1(grid(C)):- writeq(grid(C)),!.
wqs1(rhs(RHS)):- nl_now,wqnl(rhs(RHS)),nl_now.
%wqs1(grid_ops(norm,C)):- writeq(norm(C)),!.
%norm_grid

wqs1(pp(P)):-  wots_vs(S,pp_no_nl(P)),write((S)).
wqs1(ppt(P)):- wots_vs(S,ppt_no_nl(P)),write((S)).
wqs1(wqs(P)):- wots_vs(S,wqs(P)),write((S)).
wqs1(wqs(C,P)):- wots_vs(S,wqs(P)),color_print(C,S).

wqs1(vals(C)):- writeq(vals(C)),!.
%wqs1(colors_cc(C)):- \+ arg_string(C), as_arg_str(C,S),wqs(colorsz(S)).
wqs1(io(C)):-  \+ arg_string(C),wots_vs(S,bold_print(wqs(C))),write(io(S)).

wqs1(uc(C,W)):- !, write_nbsp, color_print(C,call(underline_print(format("\t~@",[wqs(W)])))).
wqs1(cc(C,N)):- is_color(C),!,color_print(C,call(writeq(cc(C,N)))).
wqs1(write_nav_cmd(C,N)):- !, write_nav_cmd(C,N).

wqs1(-(C,N)):- is_color(C),!,color_print(C,call(writeq(C))), write('-'), wqs(N).
wqs1(cc(C,N)):- N\==0,attvar(C), get_attrs(C,PC), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- N\==0,var(C), sformat(PC,"~p",[C]), !, wqs(ccc(PC,N)).
wqs1(cc(C,N)):- \+ arg_string(C), wots_hs(S,color_print(C,C)), wqs(cc(S,N)).
wqs1(color_print(C,X)):- is_color(C), !, write_nbsp, color_print(C,X).
wqs1(color_print(C,X)):- \+ plain_var(C), !, write_nbsp, color_print(C,X).
wqs1(X):- into_f_arg1(X,_,Arg),is_gridoid(Arg),area_or_len(Arg,Area),Area<5,writeq(X),!.
% wqs1(C):- callable(C), is_wqs(C),wots_vs(S,catch(C,_,fail)),write((S)).
wqs1(X):- is_gridoid_arg1(X), print_gridoid_arg1(X).

into_f_arg1(X,F,Arg):- compound(X), compound_name_arguments(X,F,[Arg]), compound(Arg).

is_gridoid_arg1(X):- into_f_arg1(X,_F,Arg),is_gridoid(Arg).
print_gridoid_arg1(X):- into_f_arg1(X,F,Arg),print_gridoid_arg1(F,Arg).

print_gridoid_arg1(F,Arg):- \+ wants_html,!, wots_vs(VS,wqs(Arg)), writeq(F),write('(`'),!, print_with_pad(write(VS)),write('`)').
print_gridoid_arg1(F,Arg):- wots_vs(VS,wqs(Arg)),
 with_tag_style(span,"display: inline; white-space: nowrap",(writeq(F),write('({'),!,write(VS),write('})'))).


nl_needed(N):- line_position(current_output,L1),L1>=N.

nl_now :- wants_html,!,nl_if_needed_ansi.
nl_now :- nl.

ansi_in_pre:- current_predicate(in_pre/0),in_pre.
nl_if_needed :- ansi_main,!, format('~N').
nl_if_needed :- ansi_in_pre,ignore((nl_needed(11),write('<br/>'))),!.
nl_if_needed :- wants_html,!,ignore((nl_needed(11),write('<br/>\n'))).
nl_if_needed :- format('~N').
nl_if_needed_ansi :- \+ ansi_main, wants_html,!.
nl_if_needed_ansi :- nl_if_needed.

write_nbsp:- ansi_main,!,write(' ').
write_nbsp:- wants_html,!,write('&nbsp;').
write_nbsp:- write(' ').

is_breaker(P):- compound(P),functor(P,_,A), A>=3.

last_f(H,F):- \+ compound(H),data_type(H,F).
last_f(H,F/A):- compound(H),!,functor(H,F,A).

need_nl(_,_):- line_position(current_output,L1),L1<40,!.
need_nl(_,_):- line_position(current_output,L1),L1>160,!,nl_if_needed.
need_nl(H0,[H1,H2|_]):- H1\=cc(_,_), last_f(H0,F0),last_f(H1,F1),last_f(H2,F2), F0\==F1, F1==F2,!,format('~N  ').
%need_nl(H0,[H1|_]):- last_f(H0,F0),last_f(H1,F1), F0==F1, !, write_nbsp.
need_nl(_,_).
/*
need_nl(_Last,[H|_]):- last_f(H,F),
 once(nb_current(last_h,cc(LF,C));(LF=F,C=0)),
   (LF==F-> (write_nbsp, plus(C,1,CC), nb_setval(last_h,cc(F,CC))) ; ((C>2 -> nl_now ; write_nbsp), nb_setval(last_h,cc(F,0)))).

need_nl(_,_):- wants_html,!,write_nbsp.
%need_nl(_,_):- !,write_nbsp.
need_nl(H,[P|_]):- \+ is_breaker(H),is_breaker(P),line_position(user_output,L1),L1>80,nl_now,bformatc1('\t\t').
need_nl(_,_):- line_position(user_output,L1),L1>160,nl_now,bformatc1('\t\t').
need_nl(_,_).
*/

dash_chars:- wants_html,!,section_break.
dash_chars:- dash_chars(40),!.

dash_chars(_):- wants_html,!,section_break.
dash_chars(H):- integer(H), dash_border(H).
dash_chars(S):- nl_if_needed,dash_chars(60,S),nl_if_needed_ansi.
dash_chars(_,_):- wants_html,!,section_break.
dash_chars(H,_):- H < 1,!.
dash_chars(H,C):- forall(between(0,H,_),bformatc1(C)).

%section_break:- wants_html,!,write('<p><hr></p>').
section_break.
%dash_uborder_no_nl_1:-  line_position(current_output,0),!, bformatc1('\u00AF\u00AF\u00AF ').
%dash_uborder_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- bformatc1('\u00AF\u00AF\u00AF ').
dash_uborder_no_nl_1:- uborder(Short,Long),!, bformatc1(Short),bformatc1(Long),write_nbsp.
dash_uborder_no_nl(1):- !, dash_uborder_no_nl_1.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, uborder(Short,Long),write_nbsp, write(Short),dash_chars(WidthM1,Long),!.
dash_uborder_no_nl(Width):- WidthM1 is Width-1, write_nbsp, bformat('\u00AF'),dash_chars(WidthM1,'\u00AF\u00AF'),!.
dash_uborder_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' \u00AF'),dash_chars(WidthM1,'\u00AF\u00AF').

dash_uborder(Width):- nl_if_needed,dash_uborder_no_nl(Width),nl_now.

uborder('-','--'):- stream_property(current_output,encoding(utf8)),!.
uborder('\u00AF','\u00AF\u00AF'):- !. %stream_property(current_output,encoding(text)).
%uborder('-','--').

dash_border_no_nl_1:-  line_position(current_output,0),!, bformatc1(' ___ ').
dash_border_no_nl_1:-  line_position(current_output,W),W==1,!, bformatc1('___ ').
dash_border_no_nl_1:- bformatc1(' ___ ').

%dash_border_no_nl(Width):- write(''),dash_chars(Width,'_'),write_nbsp,!.

dash_border_no_nl(Width):- nl_if_needed, WidthM1 is Width-1, bformatc1(' _'),dash_chars(WidthM1,'__').

dash_border(Width):- !, dash_border_no_nl(Width),nl_now,!.

functor_test_color(pass,green).
functor_test_color(fail,red).
functor_test_color(warn,yellow).

arcdbg(G):- is_vm_map(G), !, write_map(G,'arcdbg').
arcdbg(G):- compound(G), compound_name_arity(G,F,_),functor_test_color(F,C),
  wots_hs(S,print(G)),color_print(C,S),!,nl_if_needed_ansi.
arcdbg(G):- u_dmsg(G).


%user:portray(Grid):- ((\+ tracing, is_group(Grid),print_grid(Grid))).
%user:portray(Grid):- quietlyd((is_object(Grid),print_grid(Grid))).
n_times(N,Goal):- forall(between(1,N,_),ignore(Goal)).
banner_lines(Color):- banner_lines(Color,1).
banner_lines(Color,N):- wants_html,!,format('\n<hr style="border: ~wpx solid ~w">\n',[N,Color]),!.
banner_lines(Color,N):-
 must_det_ll((nl_if_needed,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now,
  n_times(N,color_print(Color,'=================================================')),nl_now,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now,
  n_times(N,color_print(Color,'=================================================')),nl_now,
  n_times(N,color_print(Color,'-------------------------------------------------')),nl_now)),!.

print_sso(A):- ( \+ compound(A) ; \+ (sub_term(E,A), is_gridoid(E))),!, u_dmsg(print_sso(A)),!.
print_sso(A):- grid_footer(A,G,W),writeln(print_sso(W)), print_grid(W,G),!.
print_sso(A):- must_det_ll(( nl_if_needed, into_ss_string(A,SS),!,
  SS = ss(L,Lst),
  writeln(print_sso(l(L))),
  forall(member(S,Lst),writeln(S)),nl_if_needed)),!.

var_or_number(V):- var(V),!.
var_or_number(V):- integer(V),!.


find_longest_len(SL,L):- find_longest_len(SL,10,L),!.
find_longest_len([],L,L).
find_longest_len([S|SS],N,L):- print_length(S,N2),max_min(N,N2,NM,_),
  find_longest_len(SS,NM,L).

:- meta_predicate( print_with_pad(0)).
:- export( print_with_pad/1).
/*print_with_pad(Goal):-

  (line_position(current_output,O);O=0),!,
  O1 is O+1,
  call_w_pad(O1,Goal).
*/
print_with_pad(Goal):-(line_position(current_output,O);O=0),!,  O1 is O+1,wots(S,Goal),print_w_pad(O1,S).


into_s(Text,S):- notrace(catch(text_to_string(Text,S),_,fail)),!.
into_s(Obj,S):- wots_hs(S,pp(Obj)),!.

print_w_pad(Pad,Text):- into_s(Text,S), atomics_to_string(L,'\n',S)-> my_maplist(print_w_pad0(Pad),L).
print_w_pad0(Pad,S):- nl_if_needed,dash_chars(Pad,' '), write(S).


:- meta_predicate(call_w_pad_prev(+,0)).
call_w_pad_prev(Pad,Goal):- wots_hs(S,Goal), print_w_pad(Pad,S).

%call_w_pad(N,Goal):- wants_html,!,format('<span style="margin-left:~w0%;">',[N]),call_cleanup(call(Goal),write('</span>')).
:- meta_predicate(call_w_pad(+,0)).
call_w_pad(_N,Goal):- wants_html,!,format('<span style="margin-left:10px;">',[]),call_cleanup(call(Goal),write('</span>')).
call_w_pad(N,Goal):- nl_if_needed,wots_hs(S,dash_chars(N,' ')),!,pre_pend_each_line(S,Goal).
maybe_print_pre_pended(Out,Pre,S):- atomics_to_string(L,'\n',S), maybe_print_pre_pended_L(Out,Pre,L).
maybe_print_pre_pended_L(Out,_,[L]):- write(Out,L),!,flush_output(Out).
maybe_print_pre_pended_L(Out,Pre,[H|L]):- write(Out,H),nl(Out),!,write(Out,Pre),maybe_print_pre_pended_L(Out,Pre,L).

%pre_pend_each_line(_,Goal):- !,ignore(Goal).
:- meta_predicate(pre_pend_each_line(+,0)).
pre_pend_each_line(Pre,Goal):- write(Pre),pre_pend_each_line0(Pre,Goal).
pre_pend_each_line0(Pre,Goal):-
  current_output(Out),
  current_predicate(predicate_streams:new_predicate_output_stream/2),!,
  call(call,predicate_streams:new_predicate_output_stream([Data]>>maybe_print_pre_pended(Out,Pre,Data),Stream)),
  arc_set_stream(Stream,tty(true)),
  %arc_set_stream(Stream,buffer(false)),
  %undo(ignore(catch(close(Stream),_,true))),!,
  setup_call_cleanup(true,
   (with_output_to_each(Stream,once(Goal)),flush_output(Stream)),
    ignore(catch(close(Stream),_,true))),!.
pre_pend_each_line0(Pre,Goal):-
  with_output_to_each(string(Str),Goal)*->once((maybe_print_pre_pended(current_output,Pre,Str),nl_if_needed)).



end_of_file.



run_source_code(ShareVars, SourceCode, Vs, QQ):-
  QQ = source_buffer(SourceCode,Vs),!,
  %print(term=Sourcecode -> vs=Vs),
  maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; maplist(mort,SourceCode)).

run_source_code(ShareVars, Vs, QQ):-
  QQ = source_buffer(SourceCode,Vs),!,
  %print(term=Sourcecode -> vs=Vs),
  maplist(share_vars(Vs),ShareVars),
  (\+ is_list(SourceCode)
    -> mort(SourceCode)
    ; maplist(mort,SourceCode)).


%vars_to_dictation([_=Value|Gotten],TIn,TOut):- is_vm_map(Value),!, vars_to_dictation(Gotten,TIn,TOut).

vars_to_dictation([Name=Value|Gotten],TIn,TOut):- !,
  my_assertion(atom(Name)),
  vars_to_dictation(Gotten,TIn,TMid),
  to_prop_name(Name,UName),
  tio_tersify(Value,ValueT),!,
  put_dict(UName,TMid,ValueT,TOut).

vars_to_dictation([NameValue|Gotten],TIn,TOut):- !,
  vars_to_dictation(Gotten,TIn,TMid),
  to_prop_name(NameValue,UName),
  tio_tersify(NameValue,ValueT),!,
  put_dict(UName,TMid,ValueT,TOut).

vars_to_dictation([NameValue|Gotten],TIn,TOut):- compound(NameValue),compound_name_arguments(NameValue,Name,Value),!,
  vars_to_dictation([Name=Value|Gotten],TIn,TOut).

vars_to_dictation([],T,T).

tio_tersify(Value,ValueT):- is_grid(Value),!,ValueT=_.
tio_tersify(Value,Value).
:- export(copy_qq_//1).

copy_qq_([]) --> [].
copy_qq_([C|Cs]) --> [C], copy_qq_(Cs).

:- export(copy_qq//1).
muarc:copy_qq(A) --> copy_qq_(Cs), {atom_codes(A, Cs)}.

to_prop_name(Name=_,UName):- nonvar(Name),!,to_prop_name(Name,UName).
to_prop_name(Name,UName):- compound(Name),compound_name_arity(Name,F,_),!,to_prop_name(F,UName).
to_prop_name(Name,UName):- to_case_breaks(Name,Breaks),xtis_to_atomic(Breaks,UName).

xtis_to_atomic([xti(Str,upper),xti(StrL,lower)|Breaks],StrO):- string_upper(Str,Str),
   symbol_chars(Str,CharsList),append(Left,[U],CharsList),
   name(S1,Left),symbolic_list_concat([S1,'_',U,StrL],'',StrUL),!,
   xtis_to_atomic([xti(StrUL,lower)|Breaks],StrO).
xtis_to_atomic([],'').
xtis_to_atomic([xti(Str,_)],Lower):- downcase_atom(Str,Lower).
xtis_to_atomic([XTI|Breaks],Atomic):-
  xtis_to_atomic([XTI],S1),xtis_to_atomic(Breaks,S2),!,symbolic_list_concat([S1,S2],'_',Atomic).

share_vars(Vs,Name=Value):- member(VName=VValue,Vs),VName==Name,!,(Value=VValue->true;trace_or_throw(cant(share_vars(Vs,Name=Value)))).
share_vars(_,Name=_):- string_concat('_',_,Name),!. % Hide some vars
share_vars(V,Name=Value):- fbug(missing(share_vars(V,Name=Value))),!.



parse_expansions(_,Vs,Vs,Src,Src):- \+ compound(Src),!.
parse_expansions(_,Vs0,Vs,dont_include(Var),nop(dont_include(Var))):-
  dont_include_var(Vs0,Vs,Var),!.
parse_expansions(F, Vs0,Vs,[Src0|Sourcecode0],[Src|Sourcecode]):- !,
  parse_expansions(F, Vs0, Vs1, Src0, Src),
  parse_expansions(F, Vs1, Vs, Sourcecode0, Sourcecode).
parse_expansions(FF, Vs0, Vs, Cmpd0, Cmpd):-
  compound_name_arguments(Cmpd0,F,Args0),
  parse_expansions([F|FF], Vs0, Vs, Args0,Args),
  compound_name_arguments(Cmpd,F,Args).

dont_include_var(Vs0,Vs,Var):- select(_=VV,Vs0,Vs),VV==Var,!.
dont_include_var(Vs,Vs,_).

append_sets(Sets,Set):- append(Sets,List),list_to_set(List,Set).
append_sets(Set1,Set2,Set):- append(Set1,Set2,List),list_to_set(List,Set).
flatten_sets(Sets,Set):- flatten(Sets,List),list_to_set(List,Set).

print_prop_val(N=V):- to_prop_name(N,P),format('~N\t\t'),print(P=V),nl.


ignore_numvars(Name='$VAR'(Name)).


