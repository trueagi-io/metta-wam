% Disables the optimizer. This predicate always fails, effectively serving as a no-op.
% disable_optimizer :- false.
disable_optimizer.

% Operator definitions for pattern matching
% Defines custom operator '=~' with precedence level 700.
:- op(700,xfx,'=~').

% Defines custom operator '=~' with precedence level 690.
:- op(690,xfx, =~ ).

%! assumed_true(+HB, +B2) is semidet.
%
%  Verifies that certain conditions hold true, taking into account dynamic disabling of the optimizer.
%  If the optimizer is disabled, or if B2 does not meet specific criteria, the predicate fails.
%  Otherwise, it verifies the condition based on the structure of B2.
%
%  @param HB Context or helper structure used in the optimization process.
%  @param B2 The condition to be verified.
%

assumed_true(_,_):- disable_optimizer, !, fail.

% Fails if the second argument is unbound.
assumed_true(_ ,B2):- var(B2),!,fail.

% Recursively checks truth of embedded evaluations.
assumed_true(HB,eval_true(B2)):-!,assumed_true(HB,B2).

% Checks if B2 is explicitly the term 'is_True('True')'.
assumed_true(_ ,B2):- B2==is_True('True').
%assumed_true(_ ,A=B):- A==B,!.
% Checks if B2 is the string 'True'.
assumed_true(_ ,B2):- B2=='True'.

% Checks if B2 is the boolean true.
assumed_true(_ ,B2):- B2== true,!.

% Evaluates to true if both A and B are equal and Atom is 'Atom'.
assumed_true(_ ,eval_for(b_5,Atom,A,B)):- 'Atom' == Atom, A=B.

% Evaluates to true if both A and B are equal and Atom is 'Any'.
assumed_true(_ ,eval_for(b_5,Atom,A,B)):- 'Any' == Atom, A=B.
%assumed_true(_ ,eval_for(b_1,Atom,A,B)):- 'Atom' == Atom, A=B.

% Fails if B2 is a user assignment with 'True'.
assumed_true(_ ,B2):- B2==u_assign('True', '$VAR'('_')),!.

% Recursively checks equality by evaluating X = Y.
assumed_true(HB,X==Y):- !, assumed_true(HB,X=Y).
%assumed_true( _,X=Y):- X==Y,!.
% Evaluates equality between X and Y if both are namespace variables and counts are appropriate.
assumed_true(HB, X = Y) :-
 is_nsVar(X),is_nsVar(Y),
  ( \+ (X\=Y)),
  (count_var_gte(HB,Y,2);count_var_gte(HB,X,2)),
  X=Y,!.

% Optimizes variable assignment with respect to unary functions.
%Failsimmediatelyifoptimizerisdisabled.
 optimize_u_assign_1(_,_):- disable_optimizer,!, fail.

% Fails if Var is a namespace variable.
optimize_u_assign_1(_,Var,_,_):- is_nsVar(Var),!,fail.

% Checks symbol arity and generates code if matching.
optimize_u_assign_1(_HB, [H|T], R, Code) :-
    symbol(H),
    length([H|T], Arity),
    predicate_arity(F, A),
    Arity == A,
    \+ (predicate_arity(F, A2), A2 \= A),
    append([H|T], [R], ArgsR),
    Code =.. ArgsR, !.

% Optimizes non-compound terms.
optimize_u_assign_1(HB,Compound,R,Code):- \+ compound(Compound),!, optimize_u_assign(HB,Compound,R,Code).

% Continues optimization for lists.
optimize_u_assign_1(HB,[H|T],R,Code):- !, optimize_u_assign(HB,[H|T],R,Code).

% Handles the case of unbound compound and list in R.
optimize_u_assign_1(_ ,Compound,R,Code):-
   is_list(R),var(Compound),
   into_u_assign(R,Compound,Code),!.

%optimize_u_assign_1(_,Compound,R,Code):- f2p(Compound,R,Code),!.

% Additional pattern matching and handling for optimizations.
optimize_u_assign_1(_,Compound,R,Code):-
  compound(Compound),
  as_functor_args(Compound,F,N0), N is N0 +1,
  (predicate_arity(F,N); functional_predicate_arg(F, N, N)),
   append_term_or_call(Compound,R,Code).

% Optimizes match operations involving queries and templates.
optimize_u_assign_1(HB,Compound,R,Code):- p2s(Compound,MeTTa),   optimize_u_assign(HB,MeTTa,R,Code).

% Fallback for assigning a result using patterns.
%optimize_u_assign_1(_,[Pred| ArgsL], R, u_assign([Pred| ArgsL],R)).



% disables
%append_term_or_call(F,R,call(F,R)):- disable_optimizer, !.

% Handles symbols directly in conjunction with appending.
append_term_or_call([F|Compound],R,Code):- symbol(F),
      is_list(Compound),append(Compound,[R],CodeL), Code=..[F|CodeL],!.
% Handles symbols directly.
append_term_or_call(F,R,Code):- symbol(F),!, Code=..[F,R].

% General append for term and result.
append_term_or_call(F,R,Code):- append_term(F,R,Code),!.

% Default case calls function with R.
append_term_or_call(F,R,call(F,R)).




% Disables optimization for the specified unit. Always fails the first clause.
optimize_unit11(_,_):- !, fail.

% Optimizes when the first argument is true.
optimize_unit11(True,true):-True==true,!.
/*
optimize_unit11(B1,true):- B1 = eval_for(b_1,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.
optimize_unit11(B1,true):- B1 = eval_for(b_5,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.

optimize_unit11(B1,true):- B1 = eval_for(b_6,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.
*/
optimize_unit11(eval_true([GM, Val, Eval]), call(GM, Val, Eval)):-
    symbol(GM),  \+ iz_conz(Val), \+ iz_conz(Eval),
    GM = '==',!.

optimize_unit11(eval_true([GM0, [GM, Eval], Val]), call(GM,Eval,Val)):-
    GM0 = '==',
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    nonvar(Val),var(Eval),!.
optimize_unit11(I,true):- I=eval_for(_,'%Undefined%', A, C), \+ iz_conz(A),\+ iz_conz(C), A=C.


% disables
optimize_unit1(_,_):- disable_optimizer, !, fail.
optimize_unit1(Var,_):- var(Var),!,fail.
optimize_unit1(true,true):-!.
optimize_unit1(I,O):- fail, \+ is_list(I), I\=(_,_), compound(I),
  predicate_property(I,number_of_rule(1)),predicate_property(I,number_of_causes(1)),
  clause(I,O), O\==true, O\=(_,_).


optimize_unit1(eval_for(b_6,'Atom', A,B), A=B):- \+ iz_conz(A),\+ iz_conz(B),  \+ \+ (A=B).
optimize_unit1(B1,eval_true(A)):- B1 = eval_for(_,NonEval, A, B),NonEval=='Bool', B=='True',!.

% disables
optimize_unit1(_,_):- disable_optimizer, !, fail.
optimize_unit1(eval_for(b_6,Atom,A,B),eval(A,B)):- 'Atom' == Atom,!.
optimize_unit1(eval_for(_,Atom,A,B),print(A=B)):- 'Atom' == Atom, freeze(A, A=B),freeze(B, A=B), \+ \+ (A=B).
optimize_unit1(B=True, B=True):- B='True','True'==True.
optimize_unit1(ISTRUE,true):- assumed_true(_ ,ISTRUE),!.
optimize_unit1(((A,B),C),(A,B,C)).
optimize_unit1(=(Const,Var),true):- is_nsVar(Var),symbol(Const),=(Const,Var).
optimize_unit1(=(Const,Var),=(Var,Const)):- fail, is_nsVar(Var),symbol(Const),!.
optimize_unit1(
  ==(['get-metatype', A], Sym, _B),
  call('get-metatype',A,Sym)).

optimize_unit1(
  eval_true([==, ['get-metatype', A], 'Expression']),
  call('get-metatype',A,'Expression')).

optimize_unit1( eval_true([GM, Val, Eval]), call(GM, Val, Eval)):-
    symbol(GM), \+ iz_conz(Val), \+ iz_conz(Eval),
    GM = '==',!.


optimize_unit1( eval_true([GM, Eval]), call(GM,Eval)):-
    symbol(GM), predicate_arity(GM,1), \+ predicate_arity(GM,2),
    var(Eval),!.

optimize_unit1( ==([GM,Eval],Val,C), call(GM,Eval,Val)):- C==Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.





% disables
optimize_u_assign(_,_,_,_):- disable_optimizer, !, fail.

optimize_u_assign(_,[Var|_],_,_):- is_nsVar(Var),!,fail.
optimize_u_assign(_,[Empty], _, (!,fail)):-  Empty == empty,!.
optimize_u_assign(_,[EqEq,[GM,Eval],Val],C, call(GM,Eval,Val)):-
    EqEq == '==',C==Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.

optimize_u_assign(_,[+, A, B], C, plus(A , B, C)):- number_wang(A,B,C), !.
optimize_u_assign(_,[-, A, B], C, plus(B , C, A)):- number_wang(A,B,C), !.
optimize_u_assign(_,[+, A, B], C, +(A , B, C)):- !.
optimize_u_assign(_,[-, A, B], C, +(B , C, A)):- !.
optimize_u_assign(_,[*, A, B], C, *(A , B, C)):- number_wang(A,B,C), !.
optimize_u_assign(_,['/', A, B], C, *(B , C, A)):- number_wang(A,B,C), !.
optimize_u_assign(_,[*, A, B], C, *(A , B, C)):- !.
optimize_u_assign(_,['/', A, B], C, *(B , C, A)):- !.
optimize_u_assign(_,[fib, B], C, fib(B, C)):- !.
optimize_u_assign(_,[fib1, A,B,C,D], R, fib1(A, B, C, D, R)):- !.
optimize_u_assign(_,['pragma!',N,V],Empty,set_option_value_interp(N,V)):-
   nonvar(N),ignore((fail,Empty='Empty')), !.
optimize_u_assign((H:-_),Filter,A,filter_head_arg(A,Filter)):- fail, compound(H), arg(_,H,HV),
  HV==A, is_list(Filter),!.
optimize_u_assign(_,[+, A, B], C, '#='(C , A + B)):- number_wang(A,B,C), !.
optimize_u_assign(_,[-, A, B], C, '#='(C , A - B)):- number_wang(A,B,C), !.
optimize_u_assign(_,[match,KB,Query,Template], R, Code):-  match(KB,Query,Template,R) = Code.

optimize_u_assign(HB,MeTTaEvalP, R, Code):- \+ is_nsVar(MeTTaEvalP),
  compound_non_cons(MeTTaEvalP), p2s(MeTTaEvalP,MeTTa),
  MeTTa\=@=MeTTaEvalP,!, optimize_body(HB, u_assign(MeTTa, R), Code).

% optimize_u_assign(_,_,_,_):- !,fail.
optimize_u_assign((H:-_),[Pred| ArgsL], R, Code):- var(R), symbol(Pred), ok_to_append(Pred),
  append([Pred| ArgsL],[R], PrednArgs),Code=..PrednArgs,
  (H=..[Pred|_] -> nop(set_option_value('tabling',true)) ; current_predicate(_,Code)),!.


%!  optimize_conj(+Head, +Body1, +Body2, -Result) is det.
%
%   Attempts to optimize conjunctions in the given Prolog goal.
%   The optimization tries to simplify the conjunction by matching patterns
%   and making transformations if specific conditions are met.
%
%   @arg Head The head of the clause being optimized.
%   @arg Body1 The first part of the body.
%   @arg Body2 The second part of the body.
%   @arg Result The optimized result of the conjunction.
%
optimize_conj(_, _, _, _):- disable_optimizer, !, fail.

optimize_conj(_Head, B1,B2,eval_true(E)):-
        % If B2 evaluates to is_True and B1 is an evaluation of the same value, simplify.
        B2 = is_True(True_Eval),
        B1 = eval(E,True_Eval1),
        True_Eval1 == True_Eval,!.

optimize_conj(HB, RR, C=A, RR):-
    % Optimization based on argument structures and variable occurrence counts.
  compound(RR),is_nsVar(C),is_nsVar(A),
  as_functor_args(RR,_,_,Args),is_list(Args), member(CC,Args),var(CC), CC==C,
    count_var(HB,C,N),N=2,C=A,!.

optimize_conj(_, u_assign(Term, C), u_assign(True,CC), eval_true(Term)):-
    % Simplify when the assignment matches the expected true condition.
   'True'==True, CC==C.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(HB, u_assign(Term, C), C=A, u_assign(Term,A)):-
%Simplify variable assignments.
  is_ftVar(C),is_ftVar(A),count_var(HB,C,N),N=2,!.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(HB, B1,BT,B1):- assumed_true(HB,BT),!.
optimize_conj(HB, BT,B1,B1):- assumed_true(HB,BT),!.

% Commented-out optimization rules for potential future use.
% optimize_conj(Head, u_assign(Term, C), u_assign(True, CC), Term):- 'True' == True.
%     optimize_conj(Head, u_assign(Term, C), is_True(CC), CTerm).
%optimize_conj(Head,B1,BT,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
%optimize_conj(Head,BT,B1,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
optimize_conj(Head,B1,B2,(BN1,BN2)):-
    % Recursively optimize both parts of the body.
   optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).

%!  optimize_head_and_body(+Head, +Body, -HeadNewest, -BodyNewest) is det.
%
%   Optimizes both the head and body of a clause by first labeling singletons,
%   then attempting optimizations on the head and body, and finally recursively
%   optimizing until a fixed point is reached.
%
%   @arg Head The original head of the clause.
%   @arg Body The original body of the clause.
%   @arg HeadNewest The optimized head of the clause.
%   @arg BodyNewest The optimized body of the clause.
%
optimize_head_and_body(Head,Body,HeadNewest,BodyNewest):-
   label_body_singles(Head,Body),
   (merge_and_optimize_head_and_body(Head,Body,HeadNew,BodyNew),
      (((Head,Body)=@=(HeadNew,BodyNew))
      ->  (HeadNew=HeadNewest,BodyNew=BodyNewest)
      ;

  (color_g_mesg('#404064',print_pl_source(( HeadNew :- BodyNew))),
    optimize_head_and_body(HeadNew,BodyNew,HeadNewest,BodyNewest)))),!.

%!  continue_optimize(+HB, +Clause) is det.
%
%   Continues the optimization process for the given clause by expanding
%   the head and body and performing body optimization.
%
%   @arg HB The head-body structure before optimization.
%   @arg Clause The clause to be optimized.
%
continue_optimize(HB,(H:-BB)):- expand_to_hb(HB,H,B), must_optimize_body(HB,B,BB),!.

% Uncommented version in case further work is needed.
%continue_optimize(Converted,Converted).

%!  merge_and_optimize_head_and_body(+Head, +Body, -HeadO, -BodyNew) is det.
%
%   Merges and optimizes the head and body of a clause. This process includes
%   handling compound heads and recursively optimizing the body.
%
merge_and_optimize_head_and_body(Head,Converted,HeadO,Body):- nonvar(Head),
   Head = (PreHead,True),!,
   merge_and_optimize_head_and_body(PreHead,(True,Converted),HeadO,Body),!.
merge_and_optimize_head_and_body(AHead,Body,Head,BodyNew):-
    assertable_head(AHead,Head),
   must_optimize_body(Head,Body,BodyNew),!.

%!  assertable_head(+PotentialHead, -Head) is det.
%
%   Ensures the provided head is assertable. This can involve restructuring
%   arguments into a functor form when necessary.
%
assertable_head(u_assign(FList,R),Head):- FList =~ [F|List],
   append(List,[R],NewArgs), symbol(F), Head=..[F|NewArgs],!.
assertable_head(Head,Head).

%!  label_body_singles(+Head, +Body) is det.
%
%   Labels singleton variables in the body that are also present in the head.
%
label_body_singles(Head,Body):-
   term_singletons(Body+Head,BodyS),
   maplist(label_body_singles_2(Head),BodyS).
label_body_singles_2(Head,Var):- sub_var_safely(Var,Head),!.
label_body_singles_2(_,Var):- ignore(Var='$VAR'('_')).



metta_predicate(u_assign(evaluable,eachvar)).
metta_predicate(eval_true(matchable)).
metta_predicate(with_space(space,matchable)).
metta_predicate(limit(number,matchable)).
metta_predicate(findall(template,matchable,listvar)).
metta_predicate(match(space,matchable,template,eachvar)).


must_optimize_body(A,B,CC):- once(optimize_body(A,B,C)), C \=@= B,!, must_optimize_body(A,C,CC).
must_optimize_body(_,B,C):- B =C.

%!  optimize_body(+Head, +Body, -OptimizedBody) is det.
%
%   Optimizes the body of a clause. This predicate handles various body constructs
%   like conjunctions, disjunctions, and other structures that may benefit from optimization.
%
optimize_body(_HB,Body,BodyNew):- is_nsVar(Body),!,Body=BodyNew.
%optimize_body( HB,u_assign(VT,R),u_assign(VT,R)):-!, must_optimize_body(HB,VT,VTT).
optimize_body( HB,with_space(V,T),with_space(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,call(T),call(TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,rtrace_on_error(T),rtrace_on_error(TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,limit(V,T),limit(V,TT)):-!, must_optimize_body(HB,T,TT).
optimize_body( HB,findall_ne(V,T,R),findall_ne(V,TT,R)):-!,
 expand_to_hb(HB,H,_), must_optimize_body((H:-findall_ne(V,T,R)),T,TT).
optimize_body( HB,findall(V,T,R),findall(V,TT,R)):-!,
 expand_to_hb(HB,H,_),
 must_optimize_body((H:-findall(V,T,R)),T,TT).
optimize_body( HB,loonit_assert_source_tf(V,T,R3,R4),
  loonit_assert_source_tf(V,TT,R3,R4)):-!,
  must_optimize_body(HB,T,TT).
optimize_body( HB,loonit_assert_source_empty(V,X,Y,T,R3,R4),
  loonit_assert_source_empty(V,X,Y,TT,R3,R4)):-!,
  must_optimize_body(HB,T,TT).

% Handles conjunction and conditional constructs.
optimize_body( HB,(B1*->B2;B3),(BN1*->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2), optimize_body(HB,B3,BN3).
optimize_body( HB,(B1->B2;B3),(BN1->BN2;BN3)):-!, must_optimize_body(HB,B1,BN1), must_optimize_body(HB,B2,BN2), must_optimize_body(HB,B3,BN3).
optimize_body( HB,(B1:-B2),(BN1:-BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1*->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1->B2),(BN1*->BN2)):-!, must_optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1;B2),(BN1;BN2)):-!, optimize_body(HB,B1,BN1), optimize_body(HB,B2,BN2).
optimize_body( HB,(B1,B2),(BN1)):- optimize_conjuncts(HB,(B1,B2),BN1).
%optimize_body(_HB,==(Var, C), Var=C):- self_eval(C),!.
optimize_body( HB,u_assign(A,B),R):- optimize_u_assign_1(HB,A,B,R),!.
optimize_body( HB,eval(A,B),R):- optimize_u_assign_1(HB,A,B,R),!.
%optimize_body(_HB,u_assign(A,B),u_assign(AA,B)):- p2s(A,AA),!.
optimize_body(_HB,Body,BodyNew):- optimize_body_unit(Body,BodyNew).

%!  optimize_body_unit(+Input, -Output) is det.
%
%   Performs basic unit-level optimizations for specific cases like 'true'.
%
optimize_body_unit(I,O):- I==true,!,I=O.
optimize_body_unit(I,O):- I==('True'='True'),!,O=true.
optimize_body_unit(I,O):- fail,
   copy_term(I,II),
   optimize_unit1(I,Opt),I=@=II,!,
   optimize_body_unit(Opt,O).
optimize_body_unit(I,O):- fail,
   optimize_unit11(I,Opt),
   optimize_body_unit(Opt,O).
optimize_body_unit(O,O).


ok_to_append('$VAR'):- !, fail.
ok_to_append(_).

number_wang(A,B,C):-
  (numeric(C);numeric(A);numeric(B)),!,
  maplist(numeric_or_var,[A,B,C]),
  maplist(decl_numeric,[A,B,C]),!.

p2s(P,S):- into_list_args(P,S).

get_decl_type(N,DT):- attvar(N),get_atts(N,AV),sub_term_safely(DT,AV),symbol(DT).

numeric(N):- number(N),!.
numeric(N):- get_attr(N,'Number','Number').
numeric(N):- get_decl_type(N,DT),(DT=='Int',DT=='Number').
decl_numeric(N):- numeric(N),!.
decl_numeric(N):- ignore((var(N),put_attr(N,'Number','Number'))).
numeric_or_var(N):- var(N),!.
numeric_or_var(N):- numeric(N),!.
numeric_or_var(N):- \+ compound(N),!,fail.
numeric_or_var('$VAR'(_)).

non_compound(S):- \+ compound(S).

did_optimize_conj(Head,B1,B2,B12):- once(optimize_conj(Head,B1,B2,B12)), B12\=@=(B1,B2),!.

optimize_conjuncts(Head,(B1,B2,B3),BN):- B3\=(_,_),
  did_optimize_conj(Head,B2,B3,B23),
  must_optimize_body(Head,(B1,B23),BN), !.
optimize_conjuncts(Head,(B1,B2,B3),BN):-
  did_optimize_conj(Head,B1,B2,B12),
  must_optimize_body(Head,(B12,B3),BN),!.
%optimize_conjuncts(Head,(B1,B2),BN1):- optimize_conj(Head,B1,B2,BN1).
optimize_conjuncts(Head,(B1,B2),BN1):- did_optimize_conj(Head,B1,B2,BN1),!.
optimize_conjuncts(Head,(B1*->B2),(BN1*->BN2)):- !,
  optimize_conjuncts(Head,B1,BN1),
  optimize_conjuncts(Head,B2,BN2).
optimize_conjuncts(Head,(B1->B2),(BN1->BN2)):- !,
  optimize_conjuncts(Head,B1,BN1),
  optimize_conjuncts(Head,B2,BN2).
optimize_conjuncts(Head,(B1;B2),(BN1;BN2)):- !,
  optimize_conjuncts(Head,B1,BN1),
  optimize_conjuncts(Head,B2,BN2).
optimize_conjuncts(Head,(B1,B2),(BN1,BN2)):- !,
   must_optimize_body(Head,B1,BN1), must_optimize_body(Head,B2,BN2).
optimize_conjuncts(_,A,A).

count_var_gte(HB,V,Ct):- count_var(HB,V,CtE),Ct>=CtE.







