
:- set_prolog_flag(verbose_autoload, false).
:- set_prolog_flag(verbose, silent).
:- set_prolog_flag(verbose_load, silent).
:- assert((user:'$exported_op'(_,_,_):- fail)).
:- abolish((system:'$exported_op'/3)).
:- assert((system:'$exported_op'(_,_,_):- fail)).

fbug(_):- is_compatio,!.
fbug(P) :- format("~N"), current_predicate(write_src/1),
  with_output_to(user_error,in_cmt(write_src(P))),!.
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

with_cwd(Dir,Goal):- Dir == '.',!,setup_call_cleanup(working_directory(X, X), Goal,
  working_directory(_,X)).
with_cwd(Dir,Goal):- var(Dir),X=Dir,!,setup_call_cleanup(working_directory(X, X), Goal,
  working_directory(_,X)).

with_cwd(Dir,Goal):- \+ exists_directory(Dir),!,throw(with_cwd(Dir,Goal)),!.
with_cwd(Dir,Goal):- setup_call_cleanup(working_directory(X, Dir), Goal, working_directory(_,X)).

with_option([],G):-!,call(G).
with_option([H|T],G):- !, with_option(H,with_option(T,G)).
with_option(N=V,G):-!,  with_option(N,V,G).
with_option(NV,G):- compound(NV), NV =..[N,V],!,with_option(N,V,G).
with_option(N,G):- with_option(N,true,G).

with_option(N,V,G):-  (was_option_value(N,W)->true;W=[]),
  setup_call_cleanup(set_option_value(N,V),G, set_option_value(N,W)).


was_option_value(N,V):- current_prolog_flag(N,VV),!,V=VV.
was_option_value(N,V):- prolog_load_context(N,VV),!,V=VV.
was_option_value(N,V):- nb_current(N,VV), VV\==[],!,V=VV.

option_else(N,V,Else):- notrace((option_else0(N,VV,Else),p2mE(VV,V))).
option_else0( N,V,_Else):- was_option_value(N,VV),!,VV=V.
option_else0(_N,V, Else):- !,V=Else.

%option_value( N,V):- var(V), !, notrace(once(((option_value0(N,V))))).
option_value(N,V):- var(V), !, was_option_value( N,VV), once((p2mE(VV,V2),p2mE(V,V1))), V1=V2.
option_value(N,V):- V==true,option_value0(N,'True'),!.
option_value(N,V):- V==false,option_value0(N,'False'),!.
option_value(N,V):- notrace(option_value0(N,V)).


option_value0( N,V):- was_option_value( N,VV), once((p2mE(VV,V2),p2mE(V,V1))), V1=V2.
option_value0(_N,[]).

p2mE(NA,NA):- \+ atom(NA),!.
p2mE(false,'False').
p2mE(true,'True').
p2mE(E,E).
set_option_value(N,V):-
  set_option_value0(N,V).
set_option_value0(N,V):-
   p2mE(V,VV),!,
   catch(nb_setval(N,VV),E,fbug(E)),
   p2mE(PV,VV),!,
   catch(create_prolog_flag(N,PV,[keep(false),access(read_write), type(term)]),E2,fbug(E2)),
   catch(set_prolog_flag(N,PV),E3,fbug(E3)),!.

kaggle_arc:- \+ exists_directory('/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/'),
 !.
%kaggle_arc:- !.
kaggle_arc:-
   with_option(argv,['--libonly'],
     with_cwd('/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/',
       ensure_loaded(kaggle_arc))).

%:- ensure_loaded((read_obo2)).

%:- kaggle_arc.


all_upper_symbol(A):-all_upper_atom(A).
any_to_symbol(A,B):-any_to_atom(A,B).
concat_symbol(A,B,C):-concat_atom(A,B,C).
downcase_symbol(A,B):-downcase_atom(A,B).
non_empty_symbol(A):-non_empty_atom(A).
string_to_symbol(A,B):-string_to_atom(A,B).
sub_string_or_symbol(A,B,C,D,E):-sub_string_or_atom(A,B,C,D,E).
sub_symbol(A,B,C,D,E):-sub_atom(A,B,C,D,E).

symbol(A):- atom(A).
symbol_chars(A,B):- atom_chars(A,B).
symbol_codes(A,B):-atom_codes(A,B).
symbol_concat(A,B,C):- atom_concat(A,B,C).
symbol_contains(A,B):- atom_contains(A,B).
symbol_length(A,B):- atom_length(A,B).
symbol_number(A,B):- atom_number(A,B).
symbol_string(A,B):- atom_string(A,B).
symbol_upper(A,B):- upcase_atom(A,B).
symbolic(A):-atomic(A).
symbolic_concat(A,B,C):-atomic_concat(A,B,C).
symbolic_concat(A,B,C,D):-atomic_concat(A,B,C,D).
symbolic_list_concat(A,B):-atomic_list_concat(A,B).
symbolic_list_concat(A,B,C):- atomic_list_concat(A,B,C).
symbolic_to_string(A,B):-atomic_to_string(A,B).
symbolics_to_string(A,B):-atomics_to_string(A,B).
symbolics_to_string(A,B,C):-atomics_to_string(A,B,C).
upcase_symbol(A,B):-upcase_atom(A,B).
:- prolog_load_context(directory, File),
   ignore((
     absolute_file_name('../../data/ftp.flybase.org/releases/current/',Dir,[relative_to(File),
     file_type(directory), file_errors(fail)]),
    asserta(ftp_data(Dir)))).

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

