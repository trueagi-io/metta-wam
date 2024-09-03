% Disables the optimizer
% Always evaluates to false, effectively a no-op.
disable_optimizer:- false.
% Base clause for disabling optimizer; never succeeds.
disable_optimizer.

% Operator definitions for pattern matching
% Defines custom operator '=~' with precedence level 700.
:- op(700,xfx,'=~').
% Defines custom operator '=~' with precedence level 690.
:- op(690,xfx, =~ ).

%% assumed_true(+HB, +B2) is semidet.
%  Verifies that certain conditions hold true, taking into account dynamic disabling of the optimizer.
%  Arguments:
%  - HB: Context or helper structure used in the optimization process.
%  - B2: The condition to be verified.
%  If the optimizer is disabled, or if B2 does not meet specific criteria, the predicate fails.
%  Otherwise, it verifies the condition based on the structure of B2.
%
/* previous: % disables*/
assumed_true(_,_):- disable_optimizer, !, fail.
% Fails if the second argument is unbound.
assumed_true(_ ,B2):- var(B2),!,fail.
% Recursively checks truth of embedded evaluations.
assumed_true(HB,eval_true(B2)):-!,assumed_true(HB,B2).
% Checks if B2 is explicitly the term 'is_True('True')'.
assumed_true(_ ,B2):- B2==is_True('True').
% Checks if B2 is the string 'True'.
%assumed_true(_ ,A=B):- A==B,!.
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
% Recursively checks equality by evaluating X=Y.
assumed_true(HB,X==Y):- !, assumed_true(HB,X=Y).
%assumed_true( _,X=Y):- X==Y,!.
% Evaluates equality between X and Y if both are namespace variables and counts are appropriate.
assumed_true(HB,X=Y):- is_nsVar(X),is_nsVar(Y),
  ( \+ (X\=Y)),
    % Ensure X and Y are not explicitly different
  (count_var_gte(HB,Y,2);count_var_gte(HB,X,2)),
   % Ensure variable occurs enough times
  X=Y,!.

% Optimizes variable assignment with respect to unary functions.
% Fails immediately if optimizer is disabled.
 optimize_u_assign_1(_,_):- disable_optimizer,!, fail.
% Fails if Var is a namespace variable.
optimize_u_assign_1(_,Var,_,_):- is_nsVar(Var),!,fail.
% Checks symbol arity and generates code if matching.
optimize_u_assign_1(_HB,[H|T],R,Code):- symbol(H),length([H|T],Arity),
   predicate_arity(F,A),Arity==A, \+ (predicate_arity(F,A2),A2\=A),
    append([H|T],[R],ArgsR),Code=..ArgsR,!.
% Optimizes non-compound terms.
optimize_u_assign_1(HB,Compound,R,Code):- \+ compound(Compound),!, optimize_u_assign(HB,Compound,R,Code).
% Continues optimization for lists.
optimize_u_assign_1(HB,[H|T],R,Code):- !, optimize_u_assign(HB,[H|T],R,Code).
% Handles the case of unbound compound and list in R.
optimize_u_assign_1(_ ,Compound,R,Code):-
   is_list(R),var(Compound),
   into_u_assign(R,Compound,Code),!.

%optimize_u_assign_1(_,Compound,R,Code):- f2p(Compound,R,Code),!.
optimize_u_assign_1(_,Compound,R,Code):-
  compound(Compound),
  as_functor_args(Compound,F,N0), N is N0 +1,
  (predicate_arity(F,N); functional_predicate_arg(F, N, N)),
   append_term_or_call(Compound,R,Code).
% Translates MeTTa to optimized code.
optimize_u_assign_1(HB,Compound,R,Code):- p2s(Compound,MeTTa),   optimize_u_assign(HB,MeTTa,R,Code).
%optimize_u_assign_1(_,[Pred| ArgsL], R, u_assign([Pred| ArgsL],R)).



% disables
%append_term_or_call(F,R,call(F,R)):- disable_optimizer, !.
% Appends terms or calls to generate code.
% Appends terms for lists with symbol F.
append_term_or_call([F|Compound],R,Code):- symbol(F),
      is_list(Compound),append(Compound,[R],CodeL), Code=..[F|CodeL],!.
% Handles symbols directly.
append_term_or_call(F,R,Code):- symbol(F),!, Code=..[F,R].
% General append for term and result.
append_term_or_call(F,R,Code):- append_term(F,R,Code),!.
% Default case calls function with R.
append_term_or_call(F,R,call(F,R)).


%% optimize_unit1(+Input, +Output) is semidet.
%  Performs optimization on the given `Input` and produces an optimized `Output`.
%  This predicate applies various optimization strategies depending on the structure of `Input`.
%  Arguments:
%  - Input: The term or structure to be optimized.
%  - Output: The result after applying optimization techniques.
%  This predicate fails if the optimizer is disabled or if specific conditions are not met.
%

% Optimization unit for specific true evaluations.
% Fails by default, preventing unintended evaluations.
optimize_unit11(_,_):- !, fail.
% Matches true directly for optimization.
optimize_unit11(True,true):-True==true,!.
/*
optimize_unit11(B1,true):- B1 = eval_for(b_1,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.
optimize_unit11(B1,true):- B1 = eval_for(b_5,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.

optimize_unit11(B1,true):- B1 = eval_for(b_6,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.
*/


% Handles specific structured evaluations.
optimize_unit11(eval_true([GM, Val, Eval]), call(GM, Val, Eval)):-
    symbol(GM),  \+ iz_conz(Val), \+ iz_conz(Eval),
    GM = '==',!.

optimize_unit11(eval_true([GM0, [GM, Eval], Val]), call(GM,Eval,Val)):-
    GM0 = '==',
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    nonvar(Val),var(Eval),!.
% Handles undefined evaluations that resolve to true.
optimize_unit11(I,true):- I=eval_for(_,'%Undefined%', A, C), \+ iz_conz(A),\+ iz_conz(C), A=C.


% disables
optimize_unit1(_,_):- disable_optimizer, !, fail.  % Disable the optimizer, failing immediately.
optimize_unit1(Var,_):- var(Var),!,fail.  % Fail if the first argument is unbound.
optimize_unit1(true,true):-!.  % Succeed immediately if the first argument is `true`.

optimize_unit1(I,O):- fail, \+ is_list(I), I\=(_,_), compound(I),
  predicate_property(I,number_of_rule(1)),predicate_property(I,number_of_causes(1)),
  clause(I,O), O\==true, O\=(_,_).

% Optimize the case where `eval_for/4` checks for equality of `A` and `B`.
optimize_unit1(eval_for(b_6,'Atom', A,B), A=B):- \+ iz_conz(A),\+ iz_conz(B),  \+ \+ (A=B).
% Optimize when `NonEval` is 'Bool' and `B` is 'True'.
optimize_unit1(B1,eval_true(A)):- B1 = eval_for(_,NonEval, A, B),NonEval=='Bool', B=='True',!.

%% optimize_unit1(+Input, +Output) is semidet.
%  Continues optimization based on specific patterns in the `Input`.
%  Optimizations include equality checks, freezing variables, and pattern matching.
%  This version preserves existing predicate definitions for specialized cases.
%
optimize_unit1(_,_):- disable_optimizer, !, fail.  % Ensure the optimizer can be disabled.
optimize_unit1(eval_for(b_6,Atom,A,B),eval(A,B)):- 'Atom' == Atom,!.  % Specific case optimization for `Atom`.
% Optimize `eval_for/4` by freezing variables and checking equality.
optimize_unit1(eval_for(_,Atom,A,B),print(A=B)):- 'Atom' == Atom, freeze(A, A=B),freeze(B, A=B), \+ \+ (A=B).
% Handle boolean comparisons where both `B` and `True` are 'True'.
optimize_unit1(B=True, B=True):- B='True','True'==True.
% General optimization for evaluations known to be true.
optimize_unit1(ISTRUE,true):- assumed_true(_ ,ISTRUE),!.
% Flatten nested conjunctions.
optimize_unit1(((A,B),C),(A,B,C)).
% Optimize equality of constants and variables.
optimize_unit1(=(Const,Var),true):- is_nsVar(Var),symbol(Const),=(Const,Var).
% Attempt to further optimize equality, though this path fails by default.
optimize_unit1(=(Const,Var),=(Var,Const)):- fail, is_nsVar(Var),symbol(Const),!.

% Optimize calls to `get-metatype/2` with specific patterns.
optimize_unit1(
  ==(['get-metatype', A], Sym, _B),
  call('get-metatype',A,Sym)).

% Handle specific cases where `eval_true/1` is checking a metatype.
optimize_unit1(
  eval_true([==, ['get-metatype', A], 'Expression']),
  call('get-metatype',A,'Expression')).

% General optimization for binary operations, particularly `==/2`.
optimize_unit1( eval_true([GM, Val, Eval]), call(GM, Val, Eval)):-
    symbol(GM), \+ iz_conz(Val), \+ iz_conz(Eval),
    GM = '==',!.

% Optimize unary operations that match specific criteria.
optimize_unit1( eval_true([GM, Eval]), call(GM,Eval)):-
    symbol(GM), predicate_arity(GM,1), \+ predicate_arity(GM,2),
    var(Eval),!.

% Optimize match operations involving equality and argument unification.
optimize_unit1( ==([GM,Eval],Val,C), call(GM,Eval,Val)):- C==Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.

%% optimize_u_assign(+Head, +Args, +Result, -Code) is semidet.
%  Optimizes variable assignments and function calls in a MeTTa program.
%  Arguments:
%  - Head: The head of the clause, typically a compound term.
%  - Args: The list of arguments to be optimized.
%  - Result: The result of the optimization, typically a variable.
%  - Code: The generated code after optimization.
%  This predicate includes specific patterns for arithmetic, logical operations, and function calls.
%

optimize_u_assign(_,_,_,_):- disable_optimizer, !, fail.
% Disable optimizer if necessary.



% Fail if the first argument in the list is a namespace variable.
optimize_u_assign(_,[Var|_],_,_):- is_nsVar(Var),!,fail.
% Optimize empty lists to a fail predicate.
optimize_u_assign(_,[Empty], _, (!,fail)):-  Empty == empty,!.
% Optimize binary equality checks in specific contexts.
optimize_u_assign(_,[EqEq,[GM,Eval],Val],C, call(GM,Eval,Val)):-
    EqEq == '==',C==Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.

% Optimize arithmetic operations with basic operations like addition and subtraction.
optimize_u_assign(_,[+, A, B], C, plus(A , B, C)):- number_wang(A,B,C), !.
optimize_u_assign(_,[-, A, B], C, plus(B , C, A)):- number_wang(A,B,C), !.
optimize_u_assign(_,[+, A, B], C, +(A , B, C)):- !.
optimize_u_assign(_,[-, A, B], C, +(B , C, A)):- !.
optimize_u_assign(_,[*, A, B], C, *(A , B, C)):- number_wang(A,B,C), !.
optimize_u_assign(_,['/', A, B], C, *(B , C, A)):- number_wang(A,B,C), !.
optimize_u_assign(_,[*, A, B], C, *(A , B, C)):- !.
optimize_u_assign(_,['/', A, B], C, *(B , C, A)):- !.
% Optimize Fibonacci calculations.
optimize_u_assign(_,[fib, B], C, fib(B, C)):- !.
optimize_u_assign(_,[fib1, A,B,C,D], R, fib1(A, B, C, D, R)):- !.
% Optimize pragma settings in MeTTa.
optimize_u_assign(_,['pragma!',N,V],Empty,set_option_value_interp(N,V)):-
   nonvar(N),ignore((fail,Empty='Empty')), !.
% Optimize filter operations, matching against specific patterns in the head.
optimize_u_assign((H:-_),Filter,A,filter_head_arg(A,Filter)):- fail, compound(H), arg(_,H,HV),
  HV==A, is_list(Filter),!.
% Optimize arithmetic operations using CLP(FD) constraints.
optimize_u_assign(_,[+, A, B], C, '#='(C , A + B)):- number_wang(A,B,C), !.
optimize_u_assign(_,[-, A, B], C, '#='(C , A - B)):- number_wang(A,B,C), !.
% Optimize match operations involving queries and templates.
optimize_u_assign(_,[match,KB,Query,Template], R, Code):-  match(KB,Query,Template,R) = Code.

% Further optimize MeTTa code after translation into an intermediate form.
optimize_u_assign(HB,MeTTaEvalP, R, Code):- \+ is_nsVar(MeTTaEvalP),
  compound_non_cons(MeTTaEvalP), p2s(MeTTaEvalP,MeTTa),
  MeTTa\=@=MeTTaEvalP,!, optimize_body(HB, u_assign(MeTTa, R), Code).

/*% optimize_u_assign(_,_,_,_):- !,fail.*/

% Default case for function application optimization.
optimize_u_assign((H:-_),[Pred| ArgsL], R, Code):- var(R), symbol(Pred), ok_to_append(Pred),
  append([Pred| ArgsL],[R], PrednArgs),Code=..PrednArgs,
  (H=..[Pred|_] -> nop(set_option_value('tabling',true)) ; current_predicate(_,Code)),!.

%% optimize_conj(+Head, +B1, +B2, -Optimized) is semidet.
%  Optimizes conjunctions within a clause body.
%  This involves combining or transforming predicates to improve efficiency.
%  Arguments:
%  - Head: The head of the clause.
%  - B1, B2: The conjunctions within the body.
%  - Optimized: The resulting optimized conjunction.
%  This predicate also handles special cases for true evaluations.
%

optimize_conj(_, _, _, _):- disable_optimizer, !, fail.  % Disable optimization if needed.

% Optimize evaluation of true statements.
optimize_conj(_Head, B1,B2,eval_true(E)):-
        B2 = is_True(True_Eval),
        B1 = eval(E,True_Eval1),
        True_Eval1 == True_Eval,!.

% Optimize conjunctions involving variable assignments.
optimize_conj(HB, RR, C=A, RR):- compound(RR),is_nsVar(C),is_nsVar(A),
  as_functor_args(RR,_,_,Args),is_list(Args), member(CC,Args),var(CC), CC==C,
    count_var(HB,C,N),N=2,C=A,!.

% Optimize u_assign for true evaluations.
optimize_conj(_, u_assign(Term, C), u_assign(True,CC), eval_true(Term)):-
   'True'==True, CC==C.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(HB, u_assign(Term, C), C=A, u_assign(Term,A)):- is_ftVar(C),is_ftVar(A),count_var(HB,C,N),N=2,!.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
% Optimize by verifying assumptions.
optimize_conj(HB, B1,BT,B1):- assumed_true(HB,BT),!.
optimize_conj(HB, BT,B1,B1):- assumed_true(HB,BT),!.
%optimize_conj(Head, u_assign(Term, C), u_assign(True,CC), Term):- 'True'==True,
%     optimize_conj(Head, u_assign(Term, C), is_True(CC), CTerm).
%optimize_conj(Head,B1,BT,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
%optimize_conj(Head,BT,B1,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
% Optimize conjunctions within the body of a clause.
optimize_conj(Head,B1,B2,(BN1,BN2)):-
   optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).

% Preserve the following commented-out code for future reference or extended use cases.

%% optimize_head_and_body(+Head, +Body, -HeadNew, -BodyNew) is det.
%  Optimizes both the head and body of a clause.
%  This includes labeling, merging, and recursively optimizing the body.
%  Arguments:
%  - Head: The original head of the clause.
%  - Body: The original body of the clause.
%  - HeadNew: The optimized head.
%  - BodyNew: The optimized body.
%
optimize_head_and_body(Head,Body,HeadNewest,BodyNewest):-
   label_body_singles(Head,Body),
     % Label single occurrences in the body.
   (merge_and_optimize_head_and_body(Head,Body,HeadNew,BodyNew),
      (((Head,Body)=@=(HeadNew,BodyNew))
      ->  (HeadNew=HeadNewest,BodyNew=BodyNewest)
      ;

  (color_g_mesg('#404064',print_pl_source(( HeadNew :- BodyNew))),
    optimize_head_and_body(HeadNew,BodyNew,HeadNewest,BodyNewest)))),!.

%% continue_opimize(+HeadBody, -OptimizedClause) is det.
%  Continues the optimization process on a given head-body clause.
%  Arguments:
%  - HeadBody: The original head-body pair.
%  - OptimizedClause: The resulting optimized clause.
%
continue_opimize(HB,(H:-BB)):- expand_to_hb(HB,H,B), must_optimize_body(HB,B,BB),!.
/*%continue_opimize(Converted,Converted).*/


% Further optimization continues below, including merging heads and optimizing bodies.

%% merge_and_optimize_head_and_body(+AHead, +Body, -Head, -BodyNew) is det.
%  Merges and optimizes the head and body of a clause.
%  This includes handling special cases for head structures and optimizing the body.
%  Arguments:
%  - AHead: The head of the clause before optimization.
%  - Body: The body of the clause.
%  - Head: The optimized head.
%  - BodyNew: The optimized body.
%
merge_and_optimize_head_and_body(Head,Converted,HeadO,Body):- nonvar(Head),
   Head = (PreHead,True),!,
   merge_and_optimize_head_and_body(PreHead,(True,Converted),HeadO,Body),!.
merge_and_optimize_head_and_body(AHead,Body,Head,BodyNew):-
    assertable_head(AHead,Head),
     % Convert the head to an assertable form if needed.
   must_optimize_body(Head,Body,BodyNew),!.

%% assertable_head(+FListR, -Head) is det.
%  Converts specific patterns in the head to a more assertable form.
%  This is used to transform functional heads into a predicate form.
%  Arguments:
%  - FListR: The original function list and result.
%  - Head: The transformed, assertable head.
%
assertable_head(u_assign(FList,R),Head):- FList =~ [F|List],
   append(List,[R],NewArgs), symbol(F), Head=..[F|NewArgs],!.
assertable_head(Head,Head).
% Default case, the head is already assertable.

%% label_body_singles(+Head, +Body) is det.
%  Labels single occurrences of variables in the body for optimization purposes.
%  This is necessary for certain optimizations that rely on variable occurrences.
%  Arguments:
%  - Head: The head of the clause.
%  - Body: The body of the clause.
%
label_body_singles(Head,Body):-
   term_singletons(Body+Head,BodyS),
   % Find singletons in the body relative to the head.
   maplist(label_body_singles_2(Head),BodyS).

% Helper predicate to label single variables if not already in the head.
label_body_singles_2(Head,Var):- sub_var(Var,Head),!.
label_body_singles_2(_,Var):- ignore(Var='$VAR'('_')).

%! metta_predicate(+Signature) is det.
%  Declares various MeTTa predicates used in optimizations.
%ThesedeclarationsassistinpatternmatchingandoptimizationsinMeTTa.
%Arguments:
%-Signature:ThesignatureoftheMeTTapredicate.
%
metta_predicate(u_assign(evaluable,eachvar)).
metta_predicate(eval_true(matchable)).
metta_predicate(with_space(space,matchable)).
metta_predicate(limit(number,matchable)).
metta_predicate(findall(template,matchable,listvar)).
metta_predicate(match(space,matchable,template,eachvar)).

%% must_optimize_body(+Head, +Body, -OptimizedBody) is det.
%  Recursively optimizes the body of a clause.
%  It applies optimizations iteratively until no further optimizations can be made.
%  Arguments:
%  - Head: The head of the clause.
%  - Body: The body of the clause.
%  - OptimizedBody: The final optimized version of the body.
%
must_optimize_body(A,B,CC):- once(optimize_body(A,B,C)), C \=@= B,!, must_optimize_body(A,C,CC).
must_optimize_body(_,B,C):- B =C.
% If no further optimization is possible, return the body as is.
%!  optimize_body(+HB, +Body, -BodyNew) is det.
%
%   Core optimization logic for a clause body.
%   This predicate optimizes various constructs within the body of a clause, including function calls, conditional statements, and more.
%
%   @arg HB The head-body context or clause being optimized.
%   @arg Body The original body of the clause.
%   @arg BodyNew The resulting optimized body.
%
optimize_body(_HB, Body, BodyNew) :- 
    % If the body is a namespace variable, return it as is.
    is_nsVar(Body), !, Body = BodyNew.  

/* previously:  
% optimize_body( HB, u_assign(VT,R), u_assign(VT,R)) :- 
% This optimization was commented out, possibly because it was redundant or unnecessary.
% must_optimize_body(HB, VT, VTT).
*/

optimize_body(HB, with_space(V, T), with_space(V, TT)) :- 
    % Optimize the body within the with_space construct.
    !, must_optimize_body(HB, T, TT).

optimize_body(HB, call(T), call(TT)) :- 
    % Optimize the body within a call construct.
    !, must_optimize_body(HB, T, TT).

optimize_body(HB, rtrace_on_error(T), rtrace_on_error(TT)) :- 
    % Optimize the body within rtrace_on_error for error tracing.
    !, must_optimize_body(HB, T, TT).

optimize_body(HB, limit(V, T), limit(V, TT)) :- 
    % Optimize the body within a limit construct.
    !, must_optimize_body(HB, T, TT).

optimize_body(HB, findall_ne(V, T, R), findall_ne(V, TT, R)) :- 
    % Optimize within a findall_ne construct, expanding the head-body if necessary.
    !, expand_to_hb(HB, H, _), must_optimize_body((H :- findall_ne(V, T, R)), T, TT).

optimize_body(HB, findall(V, T, R), findall(V, TT, R)) :- 
    % Optimize within a findall construct, expanding the head-body if necessary.
    !, expand_to_hb(HB, H, _), must_optimize_body((H :- findall(V, T, R)), T, TT).

optimize_body(HB, loonit_assert_source_tf(V, T, R3, R4), loonit_assert_source_tf(V, TT, R3, R4)) :- 
    % Optimize within a loonit_assert_source_tf construct.
    !, must_optimize_body(HB, T, TT).

optimize_body(HB, loonit_assert_source_empty(V, X, Y, T, R3, R4), loonit_assert_source_empty(V, X, Y, TT, R3, R4)) :- 
    % Optimize within a loonit_assert_source_empty construct.
    !, must_optimize_body(HB, T, TT).

optimize_body(HB, (B1 *-> B2 ; B3), (BN1 *-> BN2 ; BN3)) :- 
    % Optimize conditional constructs with potential non-determinism.
    !, must_optimize_body(HB, B1, BN1), optimize_body(HB, B2, BN2), optimize_body(HB, B3, BN3).

optimize_body(HB, (B1 -> B2 ; B3), (BN1 -> BN2 ; BN3)) :- 
    % Optimize conditional constructs with determinism.
    !, must_optimize_body(HB, B1, BN1), must_optimize_body(HB, B2, BN2), must_optimize_body(HB, B3, BN3).

optimize_body(HB, (B1 :- B2), (BN1 :- BN2)) :- 
    % Optimize body in the context of a clause definition.
    !, optimize_body(HB, B1, BN1), optimize_body(HB, B2, BN2).

optimize_body(HB, (B1 *-> B2), (BN1 *-> BN2)) :- 
    % Optimize a soft-cut conditional construct.
    !, must_optimize_body(HB, B1, BN1), optimize_body(HB, B2, BN2).

optimize_body(HB, (B1 -> B2), (BN1 -> BN2)) :- 
    % Optimize a hard-cut conditional construct.
    !, must_optimize_body(HB, B1, BN1), optimize_body(HB, B2, BN2).

optimize_body(HB, (B1 ; B2), (BN1 ; BN2)) :- 
    % Optimize disjunction constructs.
    !, optimize_body(HB, B1, BN1), optimize_body(HB, B2, BN2).

optimize_body(HB, (B1, B2), (BN1)) :- 
    % Optimize conjunctions, ensuring optimization of both parts.
    optimize_conjuncts(HB, (B1, B2), BN1).

/* previously:  
% optimize_body(_HB, ==(Var, C), Var=C):- self_eval(C), !.
% This code was commented out, possibly because it relied on a specific evaluation context or was redundant.
*/

optimize_body(HB, u_assign(A, B), R) :- 
    % Optimize assignments, possibly using an optimized version of u_assign.
    optimize_u_assign_1(HB, A, B, R), !.

optimize_body(HB, eval(A, B), R) :- 
    % Optimize evaluation constructs.
    optimize_u_assign_1(HB, A, B, R), !.

/* previously:  
% optimize_body(_HB, u_assign(A, B), u_assign(AA, B)) :- 
% This code was commented out, possibly because the optimization logic was handled elsewhere.
% p2s(A, AA), !.
*/

optimize_body(_HB, Body, BodyNew) :- 
    % Fall back to unit-level optimization if no other rules apply.
    optimize_body_unit(Body, BodyNew).

%!  optimize_body_unit(+I, -O) is det.
%
%   Unit-level optimization of body elements.
%   This predicate handles simple transformations and straightforward optimizations within the body.
%
%   @arg I The original body element to be optimized.
%   @arg O The optimized body element.
%
optimize_body_unit(I, O) :- 
    % If the body element is 'true', return it as is.
    I == true, !, I = O.

optimize_body_unit(I, O) :- 
    % If the body element is a trivial equality, simplify it to 'true'.
    I == ('True' = 'True'), !, O = true.

optimize_body_unit(I, O) :- 
    % This branch was intended for more complex optimizations but was commented out.
    fail, copy_term(I, II), optimize_unit1(I, Opt), I =@= II, !, optimize_body_unit(Opt, O).

optimize_body_unit(I, O) :- 
    % This branch was intended for another level of optimization but was commented out.
    fail, optimize_unit11(I, Opt), optimize_body_unit(Opt, O).

optimize_body_unit(O, O).

%!  ok_to_append(+Symbol) is semidet.
%
%   Predicate to determine if appending is allowed for a given symbol.
%
%   @arg Symbol The symbol to check.
%
ok_to_append('$VAR') :- 
    % '$VAR' cannot be appended.
    !, fail.
ok_to_append(_).

%!  number_wang(+A, +B, +C) is det.
%
%   Helper predicate to handle numeric operations in optimization.
%   Ensures that the arguments are numeric or variables, and declares them as numeric if necessary.
%
%   @arg A The first numeric value or variable.
%   @arg B The second numeric value or variable.
%   @arg C The third numeric value or variable.
%
number_wang(A, B, C) :- 
    % Ensure that the arguments are numeric or variables, and declare them as numeric if necessary.
    (numeric(C) ; numeric(A) ; numeric(B)), !,
    maplist(numeric_or_var, [A, B, C]),
    maplist(decl_numeric, [A, B, C]), !.

%!  p2s(+P, -S) is det.
%
%   Converts a Prolog term into a list of arguments for further processing.
%
%   @arg P The Prolog term to be converted.
%   @arg S The resulting list of arguments.
%
p2s(P, S) :- 
    % Convert the term P into a list of its arguments.
    into_list_args(P, S).

%!  get_decl_type(+N, -DT) is semidet.
%
%   Retrieves the declared type of a variable based on attributes.
%
%   @arg N The variable whose declared type is being retrieved.
%   @arg DT The declared type of the variable.
%
get_decl_type(N, DT) :- 
    % If N is an attributed variable, retrieve its declared type.
    attvar(N), get_atts(N, AV), sub_term(DT, AV), symbol(DT).

%!  numeric(+N) is semidet.
%
%   Checks if a term is numeric, either by being a number or having the 'Number' attribute.
%
%   @arg N The term to check.
%
numeric(N) :- 
    % Check if N is a number.
    number(N), !.
numeric(N) :- 
    % Check if N has the 'Number' attribute.
    get_attr(N, 'Number', 'Number').
numeric(N) :- 
    % Check if N's declared type is numeric.
    get_decl_type(N, DT), (DT == 'Int', DT == 'Number').

%!  decl_numeric(+N) is det.
%
%   Declares a variable as numeric if it is not already numeric.
%
%   @arg N The variable to be declared as numeric.
%
decl_numeric(N) :- 
    % Declare N as numeric if it is already numeric.
    numeric(N), !.
decl_numeric(N) :- 
    % If N is a variable, assign it the 'Number' attribute.
    ignore((var(N), put_attr(N, 'Number', 'Number'))).

%!  numeric_or_var(+N) is semidet.
%
%   Checks if a term is either numeric or a variable.
%
%   @arg N The term to check.
%
numeric_or_var(N) :- 
    % If N is a variable, it passes the check.
    var(N), !.
numeric_or_var(N) :- 
    % If N is numeric, it passes the check.
    numeric(N), !.
numeric_or_var(N) :- 
    % Fail if N is neither numeric nor a variable.
    \+ compound(N), !, fail.
numeric_or_var('$VAR'(_)).

%!  non_compound(+S) is semidet.
%
%   Helper to check if a term is non-compound.
%
%   @arg S The term to check.
%
non_compound(S) :- 
    % Check if S is not a compound term.
    \+ compound(S).

%!  did_optimize_conj(+Head, +B1, +B2, -B12) is semidet.
%
%   Attempts to optimize conjunctions and evaluate results.
%
%   @arg Head The head-body context.
%   @arg B1 The first term in the conjunction.
%   @arg B2 The second term in the conjunction.
%   @arg B12 The optimized conjunction of B1 and B2.
%
did_optimize_conj(Head, B1, B2, B12) :- 
    % Attempt to optimize a conjunction and verify the result differs from the original.
    once(optimize_conj(Head, B1, B2, B12)), B12 \=@= (B1, B2), !.

%!  optimize_conjuncts(+Head, +Conj, -BN) is det.
%
%   Optimizes conjunctions of three terms, with special handling for compound terms.
%
%   @arg Head The head-body context.
%   @arg Conj The conjunction to be optimized.
%   @arg BN The resulting optimized conjunction.
%
optimize_conjuncts(Head, (B1, B2, B3), BN) :- 
    % Optimize a conjunction of three terms, with special consideration for B3 being a simple term.
    B3 \= (_, _),
    did_optimize_conj(Head, B2, B3, B23),
    must_optimize_body(Head, (B1, B23), BN), !.

optimize_conjuncts(Head, (B1, B2, B3), BN) :- 
    % Optimize a conjunction of three terms, with special consideration for B1 being a simple term.
    did_optimize_conj(Head, B1, B2, B12),
    must_optimize_body(Head, (B12, B3), BN), !.

/* previously:  
% optimize_conjuncts(Head, (B1, B2), BN1) :- optimize_conj(Head, B1, B2, BN1).
% This was likely commented out due to redundancy with did_optimize_conj.
*/

optimize_conjuncts(Head, (B1, B2), BN1) :- 
    % Optimize a conjunction of two terms.
    did_optimize_conj(Head, B1, B2, BN1), !.

optimize_conjuncts(Head, (B1 *-> B2), (BN1 *-> BN2)) :- 
    % Optimize a soft-cut conditional conjunction.
    !, optimize_conjuncts(Head, B1, BN1), optimize_conjuncts(Head, B2, BN2).

optimize_conjuncts(Head, (B1 -> B2), (BN1 -> BN2)) :- 
    % Optimize a hard-cut conditional conjunction.
    !, optimize_conjuncts(Head, B1, BN1), optimize_conjuncts(Head, B2, BN2).

optimize_conjuncts(Head, (B1 ; B2), (BN1 ; BN2)) :- 
    % Optimize a disjunction of two terms.
    !, optimize_conjuncts(Head, B1, BN1), optimize_conjuncts(Head, B2, BN2).

optimize_conjuncts(Head, (B1, B2), (BN1, BN2)) :- 
    % Optimize a conjunction of two terms.
    !, must_optimize_body(Head, B1, BN1), must_optimize_body(Head, B2, BN2).

optimize_conjuncts(_, A, A).

%!  count_var_gte(+HB, +V, +Ct) is det.
%
%   Counts the occurrences of a variable in a term, ensuring it meets or exceeds a threshold.
%
%   @arg HB The head-body context.
%   @arg V The variable to be counted.
%   @arg Ct The minimum count threshold.
%
count_var_gte(HB, V, Ct) :- 
    % Count the occurrences of V in HB and compare with the threshold Ct.
    count_var(HB, V, CtE), Ct >= CtE.
