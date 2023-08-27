
with_prolog_flag(Name,ArgV,Goal):-
  current_prolog_flag(Name,Was),
  setup_call_cleanup(set_prolog_flag(Name,ArgV),Goal,set_prolog_flag(Name,Was)).
    
kaggle_arc:- setup_call_cleanup(
     working_directory(X, '/opt/logicmoo_workspace/packs_sys/logicmoo_agi/prolog/kaggle_arc/'),
     ensure_loaded(kaggle_arc),
     working_directory(_,X)). 

:- with_prolog_flag(argv,['--libonly'],kaggle_arc).


:- if( \+ current_predicate(must_det_ll/1)).
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
catch_ignore(G):- ignore(catch(G,E,catch_i((nl,writeq(G=E),nl)))).
:- endif.

:- if( \+ current_predicate(catch_i/1)).
catch_i(G):- ignore(catch(G,_,true)).
:- endif.

