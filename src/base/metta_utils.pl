:- ensure_loaded(library(logicmoo_utils)).
:- ensure_loaded(library(dictoo)).
:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).
catch_nolog(G):- ignore(catch(notrace(G),E,once(true;nop(u_dmsg(E=G))))).
catch_log(G):- ignore(catch(notrace(G),E,((u_dmsg(E=G),ftrace(G))))).
% catch_log(G):- ignore(catch(notrace(G),E,((writeln(E=G),catch_nolog(ds))))).

get_user_error(UE):- stream_property(UE,file_no(2)),!.
get_user_error(UE):- stream_property(UE,alias(user_error)),!.

ufmt(G):- fmt(G)->true;writeln(G).
u_dmsg(G):- is_list(G),!,my_maplist(u_dmsg,G).
u_dmsg(M):- get_user_error(UE), \+ current_predicate(with_toplevel_pp/2),!, with_output_to(UE,ufmt(M)).
u_dmsg(M):- get_user_error(UE),!, with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))).
u_dmsg(M):- get_user_error(UE),  stream_property(UO,file_no(1)), current_output(CO),!,
  (UO==CO ->  dmsg(M) ;
   (with_toplevel_pp(ansi, with_output_to(UE,ufmt(M))), with_output_to(CO,pp(M)))).
u_dmsg(G):-ufmt(G),!.


/*
:- export(plain_var/1).
plain_var(V):- notrace((var(V), \+ attvar(V), \+ get_attr(V,ci,_))).

my_assertion(G):- call(G),!.
my_assertion(G):- wdmsg(my_assertion(G)),writeq(goal(G)),nl,!,break.
must_be_free(AllNew):- plain_var(AllNew),!.
must_be_free(AllNew):- arcST,wdmsg(must_be_free(AllNew)),break,fail.
must_be_nonvar(AllNew):- nonvar_or_ci(AllNew),!.
must_be_nonvar(AllNew):- arcST,wdmsg(must_be_nonvar(AllNew)),break,fail.

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
   atom_chars(Str,CharsList),append(Left,[U],CharsList),
   name(S1,Left),atomic_list_concat([S1,'_',U,StrL],'',StrUL),!,
   xtis_to_atomic([xti(StrUL,lower)|Breaks],StrO).
xtis_to_atomic([],'').
xtis_to_atomic([xti(Str,_)],Lower):- downcase_atom(Str,Lower).
xtis_to_atomic([XTI|Breaks],Atomic):-
  xtis_to_atomic([XTI],S1),xtis_to_atomic(Breaks,S2),!,atomic_list_concat([S1,S2],'_',Atomic).

share_vars(Vs,Name=Value):- member(VName=VValue,Vs),VName==Name,!,(Value=VValue->true;trace_or_throw(cant(share_vars(Vs,Name=Value)))).
share_vars(_,Name=_):- string_concat('_',_,Name),!. % Hide some vars
share_vars(V,Name=Value):- dmsg(missing(share_vars(V,Name=Value))),!.



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
  /*unrepress_output*/((bt,wdmsg(tc_arg(N,C,E)=Err),((tracing->true;trace),break,arg(N,C,E))))).






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


