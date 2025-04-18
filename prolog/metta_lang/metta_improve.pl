

:- dynamic use_evaluator/3.

use_evaluator(FA, Type, only):-
   use_evaluator(FA, Other, disabled), Type\==Other.
use_evaluator(FA, Type, enabled):- use_evaluator(FA, Type, only).

use_evaluator(fa('help!',1),compiler,disabled).
use_evaluator(fa('help!',0),compiler,disabled).


:- meta_predicate with_evaluator_status(+, +, +, 0).


test_case_improve([superpose,[1,2,3]]).
test_case_improve([get-type,[+,2,3]]).
test_case_improve([eval,[+,1,2,3]]).
test_case_improve([eval,[+,[+,1,2],3]]).
test_case_improve(['help!',['if']]).
test_case_improve(['help!']).



test_drt:-
  forall(test_case_improve(X), test_improve(X)).

test_improve(X):- forall(test_drt(X,Y),writeln(Y)).

test_drt(X,Y):-
   peek_scope(Eq,RetType,Depth,Self),
   must(assumed_functor(X,FA)),!,
   use_right_thing(FA,Eq,RetType,Depth,Self,X,Y).

assumed_functor(X,fa(F,A)):- is_list(X),X=[F|Args],atom(F),length(Args,A).

% If no evaluator explicitly enabled: compare both, decide, and cache decision
use_right_thing(FA, Eq, RetType, Depth, Self, X, Y) :-
    \+ use_evaluator(FA, _, enabled),
    compare_impls(
        only_interpreted_eval(FA, Eq, RetType, Depth, Self, X, Y1), Y1,
        only_compiled_eval(FA, Eq, RetType, Depth, Self, X, Y2), Y2,
        Answers, Status),
    ( Status == ok ->
        set_use_evaluator(FA, compiler, enabled)
    ;   set_use_evaluator(FA, interp, enabled)
    ),
    !, member(Y, Answers).


% Main dynamic evaluator switch
use_right_thing(FA, Eq, RetType, Depth, Self, X, Y) :-
    use_evaluator(FA, hyperon, enabled), !,
    run_in_rust(Eq, RetType, Depth, Self, X, Y).

use_right_thing(FA, Eq, RetType, Depth, Self, X, Y) :-
    use_evaluator(FA, interp, enabled), !,
    woc(eval_10(Eq, RetType, Depth, Self, X, Y)).

use_right_thing(FA, Eq, RetType, Depth, Self, X, Y) :-
    use_evaluator(FA, compiler, enabled), !,
    with_scope(Eq,RetType,Depth,Self,transpile_eval(X, Y)).

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

only_compiled_eval(FA, Eq, RetType, Depth, Self, X, Y) :-
    with_evaluator_status(interp=disabled, FA,
        with_scope(Eq, RetType, Depth, Self,
            transpile_eval(X, Y))).

only_interpreted_eval(FA, Eq, RetType, Depth, Self, X, Y) :-
    with_evaluator_status(compiler=disabled, FA,
        woc(eval_10(Eq, RetType, Depth, Self, X, Y))).


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

