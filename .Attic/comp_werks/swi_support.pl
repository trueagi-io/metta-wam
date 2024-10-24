
:- set_prolog_flag(verbose_autoload, false).
:- set_prolog_flag(verbose, silent).
:- set_prolog_flag(verbose_load, silent).
:- assert((user:'$exported_op'(_,_,_):- fail)).
:- abolish((system:'$exported_op'/3)).
:- assert((system:'$exported_op'(_,_,_):- fail)).

fbug(_):- is_compatio,!.
fbug(P) :- format("~N"), current_predicate(write_src/1),
  with_output_to(user_error,in_cmt(pp_fb(P))),!.
fbug(N=V) :- nonvar(N), !, fbdebug1(N:-V).
fbug(V) :- compound(V),functor(V,F,_A),!,fbdebug1(F:-V).
fbug(V) :- fbdebug1(debug:-V).
fbdebug1(Message) :-
  % ISO Standard: flush_output/1
  flush_output(user_output),
  flush_output(user_error),
  catch(portray_clause(user_error,Message,[]),_,catch_ignore(format(user_error, "~n/* ~q. */~n", [Message]))),
  %format(user_error, "~n/* ~p. */~n", [Message]),
  flush_output(user_error).


swi_only(_):- is_scryer,!,fail.
swi_only(G):- call(G).
is_scryer:- \+  current_prolog_flag(libswipl,_).


:- create_prolog_flag(max_per_file,inf,[keep(true),access(read_write),type(term)]).
:- create_prolog_flag(max_disk_cache,inf,[keep(true),access(read_write),type(term)]).
:- create_prolog_flag(samples_per_million,inf,[keep(true),access(read_write),type(term)]).

with_cwd(Dir,Goal):- Dir == '.',!,setup_call_cleanup(working_directory(X, X), Goal, working_directory(_,X)).
with_cwd(Dir,Goal):- setup_call_cleanup(working_directory(X, Dir), Goal, working_directory(_,X)).

with_option([],G):-!,call(G).
with_option([H|T],G):- !, with_option(H,with_option(T,G)).
with_option(N=V,G):-!,  with_option(N,V,G).
with_option(NV,G):- compound(NV), NV =..[N,V],!,with_option(N,V,G).
with_option(N,G):- with_option(N,true,G).

with_option(N,V,G):-  (was_option_value(N,W)->true;W=[]),
  setup_call_cleanup(set_option_value(N,V),G, set_option_value(N,W)).


was_option_value(N,V):- nb_current(N,VV), VV\==[], !,V=VV.
%was_option_value(N,V):- current_prolog_flag(N,VV),!,V=VV.
was_option_value(N,V):- prolog_load_context(N,VV),!,V=VV.

option_else(N,V,Else):- notrace(option_else0(N,V,Else)).
option_else0( N,V,_Else):- was_option_value(N,VV),!,VV=V.
option_else0(_N,V, Else):- !,V=Else.

%option_value( N,V):- var(V), !, (was_option_value( N,V)->true;trace).
option_value(N,V):- V==true,option_value0(N,'True'),!.
option_value(N,V):- V==false,option_value0(N,'False'),!.
option_value(N,V):- notrace(once(((p2mE(V,VV),option_value0(N,VV))))).


option_value0( N,V):- var(V), !,  was_option_value( N,V).
option_value0( N,V):- nonvar(V), option_value0( N,VV), once((p2m(VV,V2),p2m(V,V1))), V1=V2.
option_value0( N,V):- option_else0( N,V ,[]).

p2mE(NA,NA):- \+ atom(NA),!.
p2mE(false,'False').
p2mE(true,'True').
p2mE(E,E).
set_option_value(N,V):-
  set_option_value0(N,V).
set_option_value0(N,V):-
   p2mE(V,VV),!,
   catch(nb_setval(N,VV),E,fbug(E)),
   catch(create_prolog_flag(N,V,[keep(false),access(read_write), type(term)]),E2,fbug(E2)),
   catch(set_prolog_flag(N,V),E3,fbug(E3)),!.

kaggle_arc:- \+ exists_directory('/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/'), !.
%kaggle_arc:- !.
kaggle_arc:-
   with_option(argv,['--libonly'],
     with_cwd('/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/',
       ensure_loaded(kaggle_arc))).

%:- ensure_loaded((read_obo2)).

%:- kaggle_arc.

symbol(X):- atom(X).
symbolic(X):- atomic(X).
symbol_number(S,N):- atom_number(S,N).
symbol_string(S,N):- atom_string(S,N).
symbol_chars(S,N):- atom_chars(S,N).
symbol_length(S,N):- atom_length(S,N).
symbol_concat(A,B,C):- atom_concat(A,B,C).
symbolic_list_concat(A,B,C):- atomic_list_concat(A,B,C).
symbol_contains(T,TT):- atom_contains(T,TT).

add_flybase:- prolog_load_context(file, File),
    absolute_file_name('../../data/ftp.flybase.org/releases/current/',Dir,[relative_to(File),file_type(directory)]),
    asserta(ftp_data(Dir)).

:- prolog_load_context(file, File),
    absolute_file_name('./',Dir,[relative_to(File),file_type(directory)]),
    asserta(pyswip_dir(Dir)).


:- prolog_load_context(directory, Dir),
    asserta(user:library_directory(Dir)),
    asserta(pyswip_metta_dir(Dir)).

metta_python:- ensure_loaded(library(metta_python)).
:- if( (fail, \+ current_predicate(must_det_ll/1))).
% Calls the given Goal and throws an exception if Goal fails.
% Usage: must_det_ll(+Goal).
must_det_ll(M:Goal) :- !, must_det_ll(M,Goal).
must_det_ll(Goal) :- must_det_ll(user,Goal).

must_det_ll(_M,Goal) :- var(Goal),!,throw(var_must_det_ll(Goal)),!.
must_det_ll(M,Goal) :- var(M),!,strip_module(Goal,M,NewGoal),!,must_det_ll(M,NewGoal).
must_det_ll(M,(GoalA,GoalB)) :- !, must_det_ll(M,GoalA), must_det_ll(M,GoalB).
must_det_ll(M,(GoalA->GoalB;GoalC)) :- !, (call_ll(M,GoalA)-> must_det_ll(M,GoalB) ; must_det_ll(M,GoalC)).
must_det_ll(M,(GoalA*->GoalB;GoalC)) :- !, (call_ll(M,GoalA)*-> must_det_ll(M,GoalB) ; must_det_ll(M,GoalC)).
must_det_ll(M,(GoalA->GoalB)) :- !, (call_ll(M,GoalA)-> must_det_ll(M,GoalB)).
must_det_ll(_,M:Goal) :- !, must_det_ll(M,Goal).
must_det_ll(M,Goal) :-
    % Call Goal, succeed with true if Goal succeeds.
    M:call(Goal) -> true ; % If Goal fails, throw an exception indicating that Goal failed.
      throw(failed(Goal)).

call_ll(_M,Goal):- var(Goal),!,throw(var_call_ll(Goal)),!.
call_ll(M,Goal):- var(M),!,strip_module(Goal,M,NewGoal),!,call_ll(M,NewGoal).
call_ll(M,Goal):- M:call(Goal).

:- endif.


:- if( \+ current_predicate(if_t/2)).
if_t(If,Then):- call(If)->call(Then);true.
:-endif.

:- if( \+ current_predicate(atom_contains/2)).
atom_contains(Atom1, SubAtom) :- sub_atom(Atom1, _Before, _, _After, SubAtom).
:- endif.

:- if( \+ current_predicate(nop/1)).
nop(_).
:- endif.

:- if( \+ current_predicate(catch_ignore/1)).
catch_ignore(G):- ignore(catch(G,E,catch_i((nl,writeq(causes(G,E)),nl)))).
:- endif.

:- if( \+ current_predicate(catch_i/1)).
catch_i(G):- ignore(catch(G,_,true)).
:- endif.

:- if( \+ current_predicate(add_history1/1)).
add_history1(_).
:- endif.

:- if( \+ current_predicate(add_history/1)).
add_history(_).
:- endif.

