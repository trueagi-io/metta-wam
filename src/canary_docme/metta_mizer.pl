% Disables the optimizer. This predicate always fails, effectively serving as a no-op.
disable_optimizer :- false.

% Base clause for disabling optimizer; never succeeds.
disable_optimizer.

% Operator definitions for pattern matching
% Defines custom operator '=~' with precedence level 700.
:- op(700, xfx, '=~').

% Defines custom operator '=~' with precedence level 690.
:- op(690, xfx, =~).

%! assumed_true(+HB, +B2) is semidet.
%
%  Verifies that certain conditions hold true, taking into account dynamic disabling of the optimizer.
%  If the optimizer is disabled, or if B2 does not meet specific criteria, the predicate fails.
%  Otherwise, it verifies the condition based on the structure of B2.
%  
%  @param HB Context or helper structure used in the optimization process.
%  @param B2 The condition to be verified.
%
assumed_true(_, _) :- disable_optimizer, !, fail.

% Fails if the second argument is unbound.
assumed_true(_, B2) :- var(B2), !, fail.

% Recursively checks truth of embedded evaluations.
assumed_true(HB, eval_true(B2)) :- !, assumed_true(HB, B2).

% Checks if B2 is explicitly the term 'is_True('True')'.
assumed_true(_, B2) :- B2 == is_True('True').
%assumed_true(_ ,A=B):- A==B,!.
% Checks if B2 is the string 'True'.
assumed_true(_, B2) :- B2 == 'True'.

% Checks if B2 is the boolean true.
assumed_true(_, B2) :- B2 == true, !.

% Evaluates to true if both A and B are equal and Atom is 'Atom'.
assumed_true(_, eval_for(b_5, Atom, A, B)) :- 'Atom' == Atom, A = B.

% Evaluates to true if both A and B are equal and Atom is 'Any'.
assumed_true(_, eval_for(b_5, Atom, A, B)) :- 'Any' == Atom, A = B.

% Fails if B2 is a user assignment with 'True'.
assumed_true(_, B2) :- B2 == u_assign('True', '$VAR'('_')), !.

% Recursively checks equality by evaluating X = Y.
assumed_true(HB, X == Y) :- !, assumed_true(HB, X = Y).
%assumed_true( _,X=Y):- X==Y,!.
% Evaluates equality between X and Y if both are namespace variables and counts are appropriate.
assumed_true(HB, X = Y) :-
  is_nsVar(X), is_nsVar(Y),
  ( \+ (X \= Y)),
  (count_var_gte(HB, Y, 2); count_var_gte(HB, X, 2)),
  X = Y, !.

% Optimizes variable assignment with respect to unary functions.
% Fails immediately if optimizer is disabled.
optimize_u_assign_1(_, _) :- disable_optimizer, !, fail.

% Fails if Var is a namespace variable.
optimize_u_assign_1(_, Var, _, _) :- is_nsVar(Var), !, fail.

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
optimize_u_assign_1(HB, Compound, R, Code) :- \+ compound(Compound), !, optimize_u_assign(HB, Compound, R, Code).

% Continues optimization for lists.
optimize_u_assign_1(HB, [H|T], R, Code) :- !, optimize_u_assign(HB, [H|T], R, Code).

% Handles the case of unbound compound and list in R.
optimize_u_assign_1(_, Compound, R, Code) :-
   is_list(R), var(Compound),
   into_u_assign(R, Compound, Code), !.

%optimize_u_assign_1(_,Compound,R,Code):- f2p(Compound,R,Code),!.

% Additional pattern matching and handling for optimizations.
optimize_u_assign_1(_, Compound, R, Code) :-
    compound(Compound),
    as_functor_args(Compound, F, N0), N is N0 + 1,
    (predicate_arity(F, N); functional_predicate_arg(F, N, N)),
    append_term_or_call(Compound, R, Code).

% Optimizes match operations involving queries and templates.
optimize_u_assign_1(HB, Compound, R, Code) :- p2s(Compound, MeTTa), optimize_u_assign(HB, MeTTa, R, Code).

% Fallback for assigning a result using patterns.
% optimize_u_assign_1(_, [Pred|ArgsL], R, u_assign([Pred|ArgsL], R)).



% disables
%append_term_or_call(F,R,call(F,R)):- disable_optimizer, !.

% Handles symbols directly in conjunction with appending.
append_term_or_call([F|Compound],R,Code):- symbol(F),
      is_list(Compound),append(Compound,[R],CodeL), Code=..[F|CodeL],!.
% Handles symbols directly.
append_term_or_call(F, R, Code) :- symbol(F), !, Code =.. [F, R].

% General append for term and result.
append_term_or_call(F, R, Code) :- append_term(F, R, Code), !.

% Default case calls function with R.
append_term_or_call(F, R, call(F, R)).




% Disables optimization for the specified unit. Always fails the first clause.
optimize_unit11(_,_):- !, fail.

% Optimizes when the first argument is true.
optimize_unit11(True,true):- True == true, !.

/*
% The following commented-out clauses perform optimizations for eval_for predicates 
% with specific parameters. They are disabled but might be re-enabled later.
optimize_unit11(B1,true):- B1 = eval_for(b_1,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.
optimize_unit11(B1,true):- B1 = eval_for(b_5,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.
optimize_unit11(B1,true):- B1 = eval_for(b_6,NonEval, A, B), A=B, is_non_eval_kind(NonEval),!.
*/

% Optimizes 'eval_true' predicate for equality ('==') checks. Only proceeds if the
% value and evaluation are not constants.
optimize_unit11(eval_true([GM, Val, Eval]), call(GM, Val, Eval)):-
    symbol(GM),  \+ iz_conz(Val), \+ iz_conz(Eval),
    GM = '==',!.

% Optimizes nested 'eval_true' predicates when they contain an equality ('==') check.
optimize_unit11(eval_true([GM0, [GM, Eval], Val]), call(GM,Eval,Val)):-
    GM0 = '==',
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    nonvar(Val),var(Eval),!.

% Optimizes 'eval_for' predicates where the operator is '%Undefined%'.
optimize_unit11(I,true):- I = eval_for(_,'%Undefined%', A, C), \+ iz_conz(A), \+ iz_conz(C), A=C.

% Disable optimizer if necessary.
optimize_unit1(_,_):- disable_optimizer, !, fail.

% Fails if the variable is uninstantiated.
optimize_unit1(Var,_):- var(Var), !, fail.

% Optimizes when the first argument is true.
optimize_unit1(true,true):- !.

% Various specific optimization cases for eval_for predicates with different arguments.
optimize_unit1(eval_for(b_6,'Atom', A, B), A=B):- \+ iz_conz(A), \+ iz_conz(B), \+ \+ (A=B).
optimize_unit1(B1,eval_true(A)):- B1 = eval_for(_,NonEval, A, B), NonEval == 'Bool', B == 'True',!.

% Handle specific cases where `eval_for/4` is checking an atomic value.
optimize_unit1(eval_for(b_6,Atom,A,B),eval(A,B)):- 'Atom' == Atom,!.
optimize_unit1(eval_for(_,Atom,A,B),print(A=B)):- 'Atom' == Atom, freeze(A, A=B), freeze(B, A=B), \+ \+ (A=B).
optimize_unit1(B=True, B=True):- B = 'True', 'True' == True.
optimize_unit1(ISTRUE,true):- assumed_true(_, ISTRUE), !.

% Simplifies conjunctions by merging them into a single conjunction term.
optimize_unit1(((A,B),C),(A,B,C)).

% Handles cases where an assignment matches specific conditions.
optimize_unit1(=(Const,Var),true):- is_nsVar(Var), symbol(Const), =(Const,Var).
optimize_unit1(=(Const,Var),=(Var,Const)):- fail, is_nsVar(Var), symbol(Const), !.

% Optimization cases for matching metatypes.
optimize_unit1(==(GM, Eval, Val, C), call(GM, Eval, Val)) :-
    C == Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val), var(Eval), !.

%! optimize_u_assign(+Head, +Args, +Result, -Code) is semidet.
%
% Optimizes variable assignments and function calls.
% - Head: The head of the clause, typically a compound term.
% - Args: The list of arguments to be optimized.
% - Result: The result of the optimization.
% - Code: The generated code after optimization.
%
optimize_u_assign(_, _, _, _) :- disable_optimizer, !, fail.  % Disable optimizer if needed.

% Fails if the first argument is a namespace variable.
optimize_u_assign(_,[Var|_],_,_):- is_nsVar(Var), !, fail.

% Simplifies an empty list to fail.
optimize_u_assign(_,[Empty], _, (!,fail)):- Empty == empty, !.

% Optimizes binary equality checks for specific cases.
optimize_u_assign(_,[EqEq,[GM,Eval],Val],C, call(GM,Eval,Val)):-
    EqEq == '==', C == Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val), var(Eval), !.

% Arithmetic optimizations (e.g., addition, subtraction).
optimize_u_assign(_,[+, A, B], C, plus(A, B, C)):- number_wang(A,B,C), !.
optimize_u_assign(_,[-, A, B], C, plus(B, C, A)):- number_wang(A,B,C), !.
optimize_u_assign(_,[*, A, B], C, *(A , B, C)):- number_wang(A,B,C), !.

% Optimizes Fibonacci calculations.
optimize_u_assign(_,[fib, B], C, fib(B, C)):- !.

% Optimizes pragma settings.
optimize_u_assign(_,['pragma!',N,V],Empty,set_option_value_interp(N,V)):- 
   nonvar(N), ignore((fail, Empty = 'Empty')), !.

% Optimizes filter operations.
optimize_u_assign((H:-_),Filter,A,filter_head_arg(A,Filter)):- 
   fail, compound(H), arg(_,H,HV),
   HV==A, is_list(Filter),!.

% More optimizations for arithmetic using constraints.
optimize_u_assign(_,[+, A, B], C, '#='(C , A + B)):- number_wang(A,B,C),

