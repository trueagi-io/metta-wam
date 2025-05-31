

assumed_functor(X, fa(F, A)):- is_list(X), X=[F|Args], atom(F), length(Args, A),!.
assumed_functor(_, fa(unk, 0)):-!.


:- dynamic(use_evaluator/3).

use_evaluator(fa(Fn,_), compiler, disabled):- nb_current(disable_compiler,Was), member(Fn,Was),!.
use_evaluator(fa(Fn,_), interp, disabled):- nb_current(disable_interp,Was), member(Fn,Was),!.
use_evaluator(FA, Type, only):- nonvar(Type),
   use_evaluator(FA, Other, disabled), Type\==Other.
use_evaluator(FA, Type, enabled):- use_evaluator(FA, Type, only).

use_evaluator(fa('help!', 1), compiler, disabled).
use_evaluator(fa('help!', 0), compiler, disabled).
use_evaluator(fa('listing!', 1), interp, disabled).
use_evaluator(fa('compiled-info', 1), interp, disabled).

some_evaluator(interp).
some_evaluator(compiler).

:- meta_predicate(with_evaluator_status(+, +, +, 0)).


test_case_improve([superpose, [1, 2, 3]]).
test_case_improve([get-type, [+, 2, 3]]).
test_case_improve([eval, [+, 1, 2, 3]]).
test_case_improve([eval, [+, [+, 1, 2], 3]]).
test_case_improve(['help!', ['if']]).
test_case_improve(['help!']).



test_drt:-
  forall(test_case_improve(X), test_improve(X)).

test_improve(X):- forall(test_drt(X, Y), writeln(Y)).

test_drt(X, Y):-
   peek_scope(Eq, RetType, Depth, Self),
   must(assumed_functor(X, FA)), !,
   use_right_thing(FA, Eq, RetType, Depth, Self, X, Y).

eval_use_right_thing(Eq,RetType,Depth,Self,X,Y):-
    must(assumed_functor(X, FA)), !,
    use_right_thing(FA, Eq, RetType, Depth, Self, X, Y).

use_right_thing(_FA, Eq,RetType,Depth,Self,X,Y):-
  nb_current('eval_in_only', interp), !,
  eval_09(Eq,RetType,Depth,Self,X,Y).
use_right_thing(_FA, Eq,RetType,Depth,Self,X,Y):-
  nb_current('eval_in_only', compiler), !,
  with_scope(Eq, RetType, Depth, Self, transpile_eval(X,Y)).
use_right_thing(_FA, Eq,RetType,Depth,Self,X,Y):-
  nb_current('eval_in_only', rust), !,
  with_scope(Eq, RetType, Depth, Self, rust_metta_run(exec(X),Y)).
use_right_thing(FA, Eq, RetType, Depth, Self, X, Y):-
   with_scope(Eq, RetType, Depth, Self, use_right_thing(FA, X, Y)).


set_eval_fallback:- nb_setval('eval_fallback', []),nb_setval('eval_in_only', []).

:- initialization(set_eval_fallback).
:- thread_initialization(set_eval_fallback).

use_right_thing(FA, _X, _Y) :-
   findall(Use=Status, (some_evaluator(Use),use_evaluator(FA, Use, Status)), Info),
   (nb_current('eval_in_only', OnlyIn)->true;OnlyIn=[]),
   (nb_current('eval_fallback', FallBack)->true;FallBack=[]),
   (nb_current('disable_compiler', DC)->true;DC=[]),
   (nb_current('disable_interp', DI)->true;DI=[]),
   ppt(use_right_thing(onlyIn=OnlyIn,fallBack=FallBack,dC=DC,dI=DI,FA=Info)),
   fail.


use_right_thing(_FA, X,Y):-
  nb_current('eval_in_only', interp), !,
  peek_scope(Eq,RetType,Depth,Self),!,
  eval_09(Eq,RetType,Depth,Self,X,Y).
use_right_thing(_FA, X,Y):-
  nb_current('eval_in_only', compiler), !,
  transpile_eval(X,Y).
use_right_thing(_FA, X,Y):-
  nb_current('eval_in_only', rust), !,
  rust_metta_run(exec(X),Y).
use_right_thing( FA, X, Y) :-
    use_evaluator(FA, compiler, disabled), !,
    eval_in_only(interp, X, Y).

use_right_thing(FA, X, Y) :-
    use_evaluator(FA, interp, disabled), !,
    eval_in_only(compiler, X, Y).

% If no evaluator explicitly enabled: compare both, decide, and cache decision
use_right_thing(_FA, X, Y) :-
   nb_current('eval_in_only', OnlyIn), OnlyIn \== [], !, eval_in_only(OnlyIn, X, Y).

% Main dynamic evaluator switch
use_right_thing(FA, X, Y) :-
    use_evaluator(FA, hyperon, enabled), !,
    run_in_rust( X, Y).

use_right_thing(FA, X, Y) :-
    use_evaluator(FA, interp, enabled), !,
    eval_in(interp, X, Y).

use_right_thing(FA, X, Y) :-
    use_evaluator(FA, compiler, enabled), !,
    eval_in(compiler, X, Y).

use_right_thing(FA, X, Y) :-
    \+ use_evaluator(FA, _, enabled),
    compare_impls(
        eval_in_only(interp, X, Y1), Y1,
        eval_in_only(compiler, X, Y2), Y2,
        Answers, Status),
    ( Status == ok ->
        set_use_evaluator(FA, compiler, enabled)
    ;   set_use_evaluator(FA, compiler, disabled)
    ),
    !, member(Y, Answers).



% --- Evaluator registry system ---

set_use_evaluator(FA, Flag, Status) :-
    retractall(use_evaluator(FA, Flag, _)),
    asserta(use_evaluator(FA, Flag, Status)).

get_use_evaluator(FA, Flag, Status) :-
    use_evaluator(FA, Flag, Status), !.
get_use_evaluator(_, _, _, unset).

% enable_evaluator(FA, Flag) :- set_use_evaluator(FA, Flag, enabled).
% disable_evaluator(FA, Flag) :- set_use_evaluator(FA, Flag, disabled).

list_registered_evaluators :-
    forall(use_evaluator(FA, Flag, Status),
           format('~q/~w => ~w~n', [FA, Flag, Status])).

% --- Evaluator comparison logic ---

compare_impls(Goal1, X1, Goal2, X2, List1, Status) :-
    findall(X1, call(Goal1), List1),
    findall(X2, call(Goal2), List2),
    (   List1 =@= List2
    ->  Status = ok
    ;   Status = mismatch(List1, List2)
    ).

% --- Evaluation modes (with one evaluator disabled) ---

only_compiled_eval(FA, X, Y) :-
    with_evaluator_status(interp=disabled, FA,
        with_scope(
            transpile_eval(X, Y))).

only_interpreted_eval(FA, X, Y) :-
    with_evaluator_status(compiler=disabled, FA,
        woc(eval_10( X, Y))).

% --- Temporarily override evaluator status ---

with_evaluator_status(Flag=Status, FA, Goal) :-
    ( use_evaluator(FA, Flag, OldStatus) -> true ; OldStatus = unset ),
    set_use_evaluator(FA, Flag, Status),
    setup_call_cleanup(
        true,
        Goal,
        ( OldStatus == unset ->
              retractall(use_evaluator(FA, Flag, _))
          ;   set_use_evaluator(FA, Flag, OldStatus)
        )
    ).


eval_in_only(OnlyIn, Goal):-
  locally(nb_setval('eval_in_only', OnlyIn),
   Goal).

eval_in_only(OnlyIn, X, Y):-
   eval_in_only(OnlyIn,
   eval_in(OnlyIn, X, Y)).

eval_in(interp, X, Y):-
   peek_scope(Eq, RetType, Depth, Self),
    woc(eval_10(Eq, RetType, Depth, Self, X, Y)).
eval_in(compiler, X, Y):-
    transpile_eval(X, Y).
eval_in(rust, X, Y):-
  rust_metta_run(exec(X),Y).






print_mtest(subtest_ans_count(0,X)):- !, write_exec_test(no_results,[assertEmpty,X]).
print_mtest(subtest_ans_count(N,X)):- !, write_exec_test(count,[assertCounted,N,X]).
print_mtest(subtest_xform(_,X,Y)):- !, write_exec_test(xform,[assertEqual,X,Y]).
print_mtest(subtest_xxform(_,X,Y)):- !, write_exec_test(xxform,[assertEqual,X,Y]).
print_mtest(subtest_same(_,X)):- !, write_exec_test(same,[assertSame,X]).
print_mtest(_).
%print_mtest(subtest_ans_count(1,X)):- subtest_xform(_,X,Y), !, write_exec_test(xform,[assertEqual,X,Y]).
%print_mtest(subtest_ans_count(1,X)):- subtest_xxform(_,X,Y), !, write_exec_test(xxform,[assertEqual,X,Y]).
%print_mtest(subtest_ans_count(1,X)):- subtest_same(_,X), !, write_exec_test(same,[assertEqual,X,X]).
%print_mtest(subtest_ans_count(1,X)):- subtest_xxsame(_,X), !, write_exec_test(xxsame,[xxSame,X]).

write_exec_test(Why,X):- nl, format('; ~w ~n',[Why]),write_src_wi(exec(X)),nl.

subtest_data(subtest_ans_count/2).
subtest_data(subtest_xform/3).
subtest_data(subtest_xxform/3).
subtest_data(subtest_xxsame/2).
subtest_data(subtest_same/2).
subtest_data(subtest_simular/2).
subtest_data(x_not_xx/3).
subtest_data(subtest_failed_x/2).
:- forall(subtest_data(F/A),dynamic(F/A)).

subtest_save_file:- count_tests, setup_call_cleanip(tell(subtest_save), subtest_save, told).
subtest_save:- forall(subtest_data(F/A),ignore(listing(F/A))).

count_tests(F,Count):- subtest_data(F/A), functor(P,F,A), predicate_property(P,number_of_clauses(Count)).
count_tests(Total):- findall(C,count_tests(_,C),L),sumlist(L,Total).
count_tests:- forall(count_tests(F,C),writeln(F=C)).


create_tests_file:- count_tests, setup_call_cleanup(tell('compiler_tests.metta'), create_tests, told).

create_tests_file_for(_):- count_tests(Total),Total==0,!.
create_tests_file_for(Filename):- count_tests,
   atom_concat(Filename,'~compiler_tests.metta',SaveTo),
   setup_call_cleanup(tell(SaveTo), create_tests, told).

create_tests:- forall(subtest_data(F/A),create_tests(F/A)).
create_tests(F/A):- functor(P,F,A), forall(call(P),forall(print_mtest(P),nl)).

:- dynamic(user:on_finish_load_metta/1).
:- multifile(user:on_finish_load_metta/1).

on_finish_load_metta(Filename):-
   option_value(subtests,true), create_tests_file_for(Filename).



%record_subchain(Goal,_X,_Y):-!, call(Goal).
record_subchain(Goal,_X,_Y):- \+ option_value(subtests,true), !, call(Goal).
record_subchain(Goal,X,Y):-
   term_variables(X,XVs),
   user_io(record_subchain(Goal,X,XVs,Y)).


record_subchain(_Goal,X,[],_Y):- subtest_ans_count(0,X),!,fail.
%record_subchain( Goal,X,[], Y):- subtest_same(1,X),!,X=Y.
record_subchain( Goal,X,[], Y):- !,
    ( woct(call_nth(Goal,Nth))
      *-> ignore( \+ maybe_record_subtest(X,[],X,[],X,[],Nth,Y)) ; (ignore( \+ maybe_record_subtest(X,[],X,[],X,[],0,'Empty')),!,fail)).

record_subchain(Goal,X,XVs,Y):-
   copy_term(X+XVs,XX+XXVs), copy_term(X+XVs,XC+XCVs), numbervars(XCVs,0,_,[attvar(skip)]),
   ( woct(call_nth(Goal,Nth))
     *-> ignore( \+ maybe_record_subtest(X,XVs,XX,XXVs,XC,XCVs,Nth,Y)) ; (ignore( \+ maybe_record_subtest(X,XVs,XX,XXVs,XC,XCVs,0,'Empty')),!,fail)).

maybe_record_subtest(_X,_XVs,_XX,_XXVs,XC,_XCVs,Nth,_Y):- Nth\==1,!,maybe_nth_x(XC,Nth),!.
maybe_record_subtest(X,XVs,XX,XXVs,XC,XCVs,Nth,Y):- maybe_record_subtest_1(X,XVs,XX,XXVs,XC,XCVs,Nth,Y).


maybe_record_subtest_1(X,_XVs,XX,_XXVs,_XC,_XCVs,Nth,Y):- X==XX,!,maybe_record_subtest_same_xx(X,Nth,Y).
maybe_record_subtest_1(X,XVs,XX,XXVs,_XC,XCVs,Nth,Y):- X=@=XX,XVs=XXVs,XXVs=XCVs,maybe_record_subtest_same_xx(X,Nth,Y),!.
maybe_record_subtest_1(X,XVs,XX,XXVs,_XC,XCVs,Nth,Y):-
  ignore( \+ ((maplist(if_both_vars_unify,XCVs,XVs,XXVs),
               maybe_record_subtest_r(X,XX,Nth,Y)))),!.


if_both_vars_unify(XC,X,XX):- ignore(XX=XC),ignore(X=XC).


maybe_record_subtest_r(X,XX,Nth,Y):- X\=@=XX,!,maybe_record_subtest_r_xx(X,XX,Nth,Y),!.
maybe_record_subtest_r(X,_,Nth,Y):- maybe_record_subtest_same_xx(X,Nth,Y),!.

maybe_record_subtest_same_xx(X,_,_):- subtest_ans_count(_,X),!.
maybe_record_subtest_same_xx(X,Nth,Y):- maybe_nth_x(X,Nth),
   if_t( X\=@=Y, subtest_assertz(subtest_xform(Nth,X,Y))),
   if_t(  X==Y, subtest_assertz(subtest_same(Nth,X))),
   if_t( (X\==Y,X=@=Y), subtest_assertz(subtest_simular(Nth,X))), !.

maybe_record_subtest_r_xx(_,XX,_,_):- subtest_ans_count(_,XX),!.
maybe_record_subtest_r_xx(X,XX,Nth,Y):- maybe_nth_x(XX,Nth),
   if_t( X\=@=XX, subtest_assertz(x_not_xx(Nth,X,XX))),
   if_t( X\=@=Y, subtest_assertz(subtest_xxform(Nth,XX,Y))),
   if_t( X=@=Y, subtest_assertz(subtest_xxsame(Nth,X))),
   !.

maybe_nth_x(XX,Nth):- subtest_ans_count(Was,XX), Nth =< Was, !.
%maybe_nth_x(XX,  _):- compound(XX), compound_name_arguments(XX,F,[_,X|_]),nonvar(X),skip_record_subtest(F,X),!.
maybe_nth_x(XX,Nth):- retractall(subtest_ans_count(_,XX)), assert(subtest_ans_count(Nth,XX)).

subtest_assertz(G):- \+ compound(G), throw(\+ compound(G)).
subtest_assertz(G):- compound_name_arguments(G,F,[_,X|_]),skip_record_subtest(F,X),!.
subtest_assertz(G):- \+ \+ clause(G,true),!.
subtest_assertz(G):- debug_info(subtest_assertz,G), assertz(G),!.


skip_record_subtest(_F,X):- \+ compound(X),!.
skip_record_subtest(_F,[X|_]):- dont_subtest_function(X),!.

dont_subtest_function('TupleConcat').
dont_subtest_function('superpose').
dont_subtest_function('collapse').
%dont_subtest_function(_).
