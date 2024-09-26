/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%*********************************************************************************************% 
% PROGRAM FUNCTION:  Translate functional expressions into Prolog predicates.
%*********************************************************************************************%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  eval_for(+Why, +Var, +B, -C) is det.
%
%   Evaluates the input `B` and attempts to unify it with `C`. The evaluation
%   depends on the value of `Why` and the type of the variable `Var`.
%
%   The first clause ensures that if `Var` is an unbound variable, `B` is unified
%   directly with `C`. The second clause handles the case where `B` is already 
%   equal to `C`, and no further evaluation is needed.
%
%   The third clause applies when the reason is `b_C`, where `eval_for1/4` is used
%   to handle the evaluation, and a check is made to ensure the type of `C` can 
%   be assigned to `A`.
%
%   The fourth clause calls `eval_for1/4` with the given parameters to handle other
%   reasons for the evaluation.
%
%   @arg Why The reason or context for evaluation, which influences how the predicate behaves.
%   @arg Var The variable being evaluated. If it is unbound, it unifies directly with `B`.
%   @arg B The initial value or goal that is evaluated.
%   @arg C The result of the evaluation or unification process.
%
eval_for(_,Var,B,C):- 
    % If Var is unbound, unify B with C.
    var(Var),!,B = C.
eval_for(_, _, B, C):- 
    % If B and C are already equal, no need to proceed further.
    B == C, !.
eval_for(b_C, A, B, C):- 
    % If the reason is b_C, perform evaluation with eval_for1 and ensure type compatibility.
    !,eval_for1(b_C, A, B, C),\+ \+ ((get_type(C, CT), can_assign(CT, A))).
eval_for(Why, A, B, C):- 
    % For other reasons, evaluate using eval_for1.
    eval_for1(Why, A, B, C).

%!  eval_for1(+Why, +Type, +B, -C) is det.
%
%   A helper predicate that performs evaluation depending on the `Type` and `Why` parameters.
%   It checks if `B` is callable, compound, or requires further evaluation based on the type.
%
%   @arg Why The reason or context for evaluation.
%   @arg Type The type of the value being evaluated.
%   @arg B The initial value or goal.
%   @arg C The result after evaluation.
%
eval_for1(_Why, _, B, C):- 
    % If B is not callable, directly unify B with C.
    \+ callable(B), !,B = C.
eval_for1(_Why, _, B, C):- 
    % If B and C are compound terms, unify them.
    compound(B), compound(C), B = C, !.
eval_for1(_Why, 'Any', B, C):- 
    % For 'Any' type, evaluate B and unify the result with C.
    !,eval(B, C).
eval_for1(_Why, 'AnyRet', B, C):- 
    % For 'AnyRet' type, evaluate B and unify the result with C.
    !,eval(B, C).
eval_for1(b_6, 'Atom', B, C):- 
    % For 'Atom' type in the b_6 context, evaluate B and unify the result with C.
    !,eval(B, C).
eval_for1(_, 'Atom', B, C):- 
    % For 'Atom' type, unify B directly with C.
    !, B = C.
eval_for1(_Why, A, B, C):- 
    % Fallback clause that delegates evaluation to eval_for/4.
    eval_for(A, B, C).

%!  why_call(+Context, +Goal) is det.
%
%   Calls the given `Goal` within a certain `Context`. The `Context` is not used in this
%   version but could influence the behavior in more complex setups. Currently, it simply
%   calls the `Goal`.
%
%   @arg Context The context in which the goal is executed (currently unused).
%   @arg Goal The goal to be executed.
%
why_call(_, Goal):- 
    % Execute the provided Goal.
    call(Goal).

%!  u_assign1(+B, -C) is det.
%!  u_assign2(+B, -C) is det.
%!  u_assign3(+B, -C) is det.
%!  u_assign4(+B, -C) is det.
%!  u_assign6(+B, -C) is det.
%!  u_assign7(+B, -C) is det.
%!  u_assign8(+B, -C) is det.
%!  u_assign9(+B, -C) is det.
%!  u_assignA(+B, -C) is det.
%!  u_assignB(+B, -C) is det.
%!  u_assignC(+B, -C) is det.
%
%   These predicates all serve as wrappers around either `u_assign5/2` or `u_assignI/2`,
%   performing assignment-like operations. The exact behavior is delegated to either
%   of these two predicates.
%
%   @arg B The input value to be checked and possibly assigned.
%   @arg C The output result, possibly modified or assigned based on `B`.

u_assign1(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assign2(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assign3(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assign4(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assign6(B, C):- 
    % Delegate assignment to u_assignI/2.
    u_assignI(B, C).
u_assign7(B, C):- 
    % Delegate assignment to u_assignI/2.
    u_assignI(B, C).
u_assign8(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assign9(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assignA(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assignB(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).
u_assignC(B, C):- 
    % Delegate assignment to u_assign5/2.
    u_assign5(B, C).

%!  u_assign5(+B, -C) is det.
%
%   Performs the assignment operation by first checking if `B` is a compound term. 
%   If `B` is not compound, it directly unifies `B` with `C`. Otherwise, it delegates 
%   the operation to `u_assignI/2`.
%
%   @arg B The input value to be checked and possibly assigned.
%   @arg C The output result, possibly modified or assigned based on `B`.
u_assign5(B, C):- 
    % If B is not a compound term, unify B with C.
    \+ compound(B), !, 
    B = C.
u_assign5(B, C):- 
    % Delegate to u_assignI/2 for further processing.
    u_assignI(B, C).

%!  u_assignI(+B, -C) is det.
%
%   Handles the assignment when `B` is a variable or requires further processing.
%   If `B` is unbound, it directly unifies `B` with `C`. Otherwise, it calls the
%   `u_assign/2` predicate for more complex assignment behavior.
%
%   @arg B The input value to be assigned or evaluated.
%   @arg C The output result, possibly assigned or modified based on `B`.
u_assignI(B, C):- 
    % If B is unbound (a variable), unify it with C.
    var(B),!,B = C.
u_assignI(B, C):- 
    % Call u_assign/2 for further processing.
    u_assign(B, C).

% Define an operator `=~` with precedence 700 and type xfx (infix, non-associative).
:- op(700, xfx, '=~').

% Declare that the predicate `f2q/6` may appear non-consecutively in the file.
:- discontiguous f2q/6.

%!  f2p(-RetResult, +Convert, -Converted) is det.
%
%   A wrapper predicate for `f2p/5` that uses a default `HeadIs` value of `my_head`.
%   This predicate is used to initiate a conversion process where the result of the 
%   conversion is stored in `Converted`.
%
%   @arg RetResult The result to be returned after processing.
%   @arg Convert The input to be converted.
%   @arg Converted The output after conversion.
%
f2p(RetResult, Convert, Converted):- 
    % Call the main f2p/5 predicate with default HeadIs and RetType `_ANY_`.
    f2p(my_head, _ANY_, RetResult, Convert, Converted).

%!  f2p(+HeadIs, -RetResult, +Convert, -Converted) is det.
%
%   A variant of `f2p/5` with a default `RetType` value of `_ANY_`. This predicate 
%   uses the provided `HeadIs` value, and performs the same conversion process.
%
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetResult The result to be returned after processing.
%   @arg Convert The input to be converted.
%   @arg Converted The output after conversion.
%
f2p(HeadIs, RetResult, Convert, Converted):- 
    % Call the main f2p/5 predicate with default RetType `_ANY_`.
    f2p(HeadIs, _ANY_, RetResult, Convert, Converted), !.

%!  f2p(+HeadIs, +RetType, -RetResult, +Convert, -Converted) is det.
%
%   Calls the main `f2p/6` with the default depth value of 40. The `HeadIs` and 
%   `RetType` are used to guide the conversion process.
%
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The type of the result.
%   @arg RetResult The result to be returned after processing.
%   @arg Convert The input to be converted.
%   @arg Converted The output after conversion.
%
f2p(HeadIs, RetType, RetResult, Convert, Converted):- 
    % Call f2p/6 with depth 40 and the provided HeadIs, RetType, RetResult, Convert, and Converted.
    f2p(40, HeadIs, RetType, RetResult, Convert, Converted).

%!  f2p(+Depth, +HeadIs, +RetType, -RetResult, +Convert, -Converted) is det.
%
%   The main `f2p/6` predicate which handles the conversion by first reducing the depth,
%   then calling `f2q/6` to process the input. The result is then converted using 
%   `convert_fromi/3`.
%
%   @arg Depth The depth limit for the recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The type of the result.
%   @arg RetResult The result to be returned after processing.
%   @arg Convert The input to be converted.
%   @arg Converted The output after conversion.
%
f2p(Depth, HeadIs, RetType, RetResult, Convert, Converted):- 
    % Decrease the depth for the next call.
    Depth2 is Depth - 1,
    % Call f2q/6 with the reduced depth and perform the necessary conversion.
    f2q(Depth2, HeadIs, RetType, RetResult, Convert, Converting),
    % Convert the intermediate result to the final result.
    convert_fromi(Depth2, Converting, Converted), !.

%f2p(_Depth,_HeadIs,_RetType,RetResult,Convert, eval(Convert,RetResult)).

%!  convert_fromi(+Depth, +Converting, -Converted) is det.
%
%   This predicate converts the `Converting` term into the `Converted` term by 
%   recursively processing its structure. It handles different types of terms 
%   including variables, compounds, lists, and compound terms with arguments.
%
%   The conversion halts when `Converting` is a simple term, a variable, or 
%   when the `Depth` reaches a certain limit (if defined externally).
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg Converting The input term to be converted.
%   @arg Converted The output term after conversion.
%
convert_fromi(_Depth, Converted, Converted):- 
    % If `Converted` is already a final result (no further processing needed), succeed.
    !.
convert_fromi(_Depth, Converted, Converted):- 
    % If `Converted` is a flexible variable (using some custom ftVar check), succeed.
    is_ftVar(Converted), !.
convert_fromi(_Depth, Converted, Converted):- 
    % If `Converted` is not a compound term, succeed without further processing.
    \+ compound(Converted), !.
% Commented out to allow future re-enabling of this clause for `u_assign` handling.
% convert_fromi(_Depth, u_assign(E,R), UA):-  
%     !, 
%     u_assign(E, R) = UA.
convert_fromi(Depth, (A, B), (AA, BB)):- 
    % If `Converting` is a pair (A, B), recursively convert both elements.
    !,convert_fromi(Depth, A, AA),convert_fromi(Depth, B, BB).
convert_fromi(Depth, Converting, Converted):- 
    % If `Converting` is a list, map the conversion over each element.
    is_list(Converting), !, maplist(convert_fromi(Depth), Converting, Converted).
convert_fromi(Depth, Converting, Converted):- 
    % If `Converting` is a compound term, extract the functor and arguments.
    compound_name_arguments(Converting, F, Args),!,
    % Recursively convert each argument of the compound term.
    maplist(convert_fromi(Depth), Args, NewArgs),!,
    % Rebuild the compound term with the converted arguments.
    compound_name_arguments(Converted, F, NewArgs).
%convert_fromi(Depth,Converting, Converted):- f2q(Depth,Converting, Converted).

%!  is_fqVar(+Var2) is nondet.
%
%   Determines whether `Var2` is an "fqVar" by checking two conditions:
%   1. It first checks if `Var2` is an "ftVar" using `is_ftVar/1`.
%   2. If not, it then checks if `Var2` is a symbol using `symbol/1`.
%   If either of these conditions is satisfied, `is_fqVar/1` succeeds.
%
%   @arg Var2 The variable or term being checked.
%
is_fqVar(Var2):- 
    % If Var2 satisfies the ftVar check, succeed.
    is_ftVar(Var2), !.
is_fqVar(Var2):- 
    % If Var2 is a symbol, succeed.
    symbol(Var2), !.
%f2q(_Depth,_HeadIs,RetType,Var1, Var2,  ((Var1=Var2))):-
%   is_fqVar(Var1),is_fqVar(Var2),!.

%!  f2q(+Depth, +HeadIs, +RetType, -RetVar, +Convert, -Converted) is det.
%
%   The `f2q/6` predicate performs various operations based on the `Convert` term.
%   It processes terms differently depending on whether they are variables, lists, 
%   compound terms, or arity-0 functions. In cases where the conversion involves 
%   variables or specific constructs, it uses `eval_for/4` or `u_assign/2` predicates 
%   to handle the logic.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The type of the result being processed.
%   @arg RetVar The variable that will store the result.
%   @arg Convert The input term or value to be converted.
%   @arg Converted The resulting output after conversion.
%
f2q(_Depth, _HeadIs, RetType, RetVar, Convert, true) :- 
    % If RetVar, RetType, and Convert are variables, unify RetVar with Convert.
    is_ftVar(RetVar), is_ftVar(RetType), is_ftVar(Convert),
    RetVar = Convert, !.
f2q(_Depth, _HeadIs, RetType, RetVar, Convert, eval_for(b_C, RetType, Convert, RetVar)) :- 
    % If Convert is a flexible variable, use eval_for/4 to evaluate.
    is_ftVar(Convert), !.
f2q(_Depth, _HeadIs, RetType, RetVar, [C|Convert], eval_for(b_B, RetType, [C|Convert], RetVar)) :- 
    % If the head of Convert is a flexible variable, use eval_for/4 for evaluation.
    is_ftVar(C), !.
f2q(Depth, HeadIs, RetType, RetResult, eval(Convert), Code):-  
    % Recursively process the Convert term when it needs evaluation.
    !,DepthM1 is Depth - 1,f2q(DepthM1, HeadIs, RetType, RetResult, Convert, Code).
f2q(_Depth, _HeadIs, _RetType, _RetResult, u_assign(E, R), UA):-  
    % Process u_assign/2, using u_assign2 for assignment.
    !,u_assign2(E, R) = UA.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, Converted) :- 
    % If Convert is an arity-0 function, assign it using u_assign3.
    is_arity_0(Convert, F), !, Converted = u_assign3([F], RetResult), !.
f2q(_Depth, _HeadIs, RetType, RetResult, Convert, true) :- 
    % If Convert is an if-then clause (:-), copy it directly to RetResult.
    ignore(RetType = 'Atom'),(Convert = (H :- B)),(RetResult = (H :- B)).

%!  get_ret_type(+Function, -RetType) is det.
%
%   Determines the return type (`RetType`) of a given function or operator (`Function`). 
%   The first clause handles the case when `Function` is a list with arguments (likely
%   representing a function or operator with parameters), while the second clause handles
%   simpler cases where `Function` is not a list.
%
%   If the function has arguments, the predicate first calculates the appropriate number 
%   of parameters, then calls `get_operator_typedef1/4` to retrieve the return type.
%   If a valid return type is found, it is unified with `RetType`; otherwise, a default 
%   or undefined type is assigned to `RetType`.
%
%   @arg Function The function or operator whose return type is being determined.
%   @arg RetType The resulting return type of the function or operator.
%
get_ret_type([F|Args], RetType):- is_list(Args),!,
    % If the function has a list of arguments, process it.
    % Create a list of parameters of length PL.
    ((length(Args,Len),(PL = Len ; PL is Len + 1 ; PL is Len - 1),
      PL>=0,
      length(Params,PL),
      % Get the type definition for the operator and its return type.
      get_operator_typedef1(_Self,F,Params,RetType),
      % Ensure the return type is not 'RetAny'.
      %  use the found RetType, otherwise assign a default.
      RetType \== 'RetAny')*->true;RetType=_/*'%Undefined%'*/).    
get_ret_type(F, RetType):- 
    % For simpler cases, directly get the type of F.
    get_type(F, RetType).

%!  f2q(+Depth, +HeadIs, +RetType, -RetVar, +Data, -CodeOut) is det.
%
%   The `f2q/6` predicate performs various conversions and operations based on the type of data provided.
%   It processes different cases such as when `RetType` or `RetVar` is a variable, compound terms, and specific 
%   data forms like numbers or symbols. The final result is returned as `CodeOut`.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type (can be determined during execution).
%   @arg RetVar The variable that will store the result.
%   @arg Data The input data to be processed.
%   @arg CodeOut The resulting output code after processing.
%
f2q(Depth, HeadIs, RetType, RetVar, Data, CodeOut):- 
    % If RetType is unbound and Data is bound, infer the return type from Data.
    var(RetType), nonvar(Data),get_ret_type(Data, PRT),nonvar(PRT),!,RetType = PRT,
    f2q(Depth, HeadIs, RetType, RetVar, Data, CodeOut).
f2q(Depth, HeadIs, RetType, RetVar, Data, CodeOut):- 
    % If RetType is unbound and RetVar is bound, infer the return type from RetVar.
    var(RetType), nonvar(RetVar),get_ret_type(RetVar, PRT),nonvar(PRT),!,RetType = PRT,
    f2q(Depth, HeadIs, RetType, RetVar, Data, CodeOut).
f2q(Depth, HeadIs, RetType, RetVal, Convert, Code):- 
    % If Convert is a compound term, break it into a list of arguments and process them.
    compound_non_cons(Convert),into_list_args(Convert, ConvertL),
    f2q(Depth, HeadIs, RetType, RetVal, ConvertL, Code).
   
%!  data_term(+Convert) is nondet.
%
%   Succeeds if `Convert` is a valid data term. A data term is considered a non-compound
%   term that can be self-evaluated and either satisfies the `iz_conz/1` predicate or
%   remains a non-compound term.
%
%   The `self_eval/1` predicate is used to determine if `Convert` can evaluate itself,
%   and `iz_conz/1` provides additional checks for specific conditions.
%
%   @arg Convert The term being checked to determine if it is a valid data term.
%
data_term(Convert):- 
    % Check if Convert is not a compound term and can self-evaluate.
    \+ compound(Convert),self_eval(Convert),!,
    % Either Convert satisfies iz_conz/1 or it is not a compound term.
    (iz_conz(Convert) ; \+ compound(Convert)).

%!  f2q(+Depth, +HeadIs, +RetType, -RetResult, +Convert, -CodeOut) is det.
%
%   The `f2q/6` predicate handles different conversions and transformations based on the input
%   `Convert`. It processes data terms, variables, compound terms, and logical operators like
%   "and" and "or". The result of the conversion or transformation is returned as `CodeOut`.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type (can be determined during execution).
%   @arg RetResult The variable that will store the result.
%   @arg Convert The input data or expression to be converted or evaluated.
%   @arg CodeOut The resulting output code after processing.
%

% If Convert is a data term, unify RetResult with Convert using `=~`.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, (RetResult =~ Convert)) :-
    data_term(Convert), !.
% If Convert is a data term, directly unify RetResult with Convert.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, true) :-
    data_term(Convert),
    RetResult = Convert, !.
% If Convert is a flexible variable (ftVar), generate code using `into_equals`.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, RetResultConverted) :-
    is_ftVar(Convert), !, % Check if Convert is a variable.
    into_equals(RetResult, Convert, RetResultConverted).
% If Convert is a number, generate code using `into_equals`.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, RetResultConverted) :-
    number(Convert), !,
    into_equals(RetResult, Convert, RetResultConverted).
% If Convert is a conjunction (','), recursively process both sides.
f2q(Depth, HeadIs, _RetType, RetResult, SOR, CC) :-
    SOR =~ [LogOp, AsPredI, Convert], ',' == LogOp,
    RetResult = [LogOp, RetResult1, RetResult2],
    must_det_ll((
        f2p(Depth, HeadIs, _RetType1, RetResult1, AsPredI, AsPredO),
        f2p(Depth, HeadIs, _RetType2, RetResult2, Convert, Converted)
    )), !,
    combine_code(AsPredO, Converted, CC).
% If Convert is a logical 'and' operation, process both sides.
f2q(Depth, HeadIs, RetType, RetResult, SOR, (AsPredO, Converted)) :-
    SOR =~ [LogOp, AsPredI, Convert], 'and' == LogOp, !,
    must_det_ll((
        f2p(Depth, HeadIs, 'Bool', 'True', AsPredI, AsPredO),
        f2p(Depth, HeadIs, RetType, RetResult, Convert, Converted)
    )), !.
% If Convert is a logical 'or' operation, process both sides.
f2q(Depth, HeadIs, RetType, RetResult, SOR, CC) :-
    SOR =~ [LogOp, AsPredI, Convert], 'or' == LogOp,
    must_det_ll((
        f2p(Depth, HeadIs, RetTypeA, RetResultA, AsPredI, AsPredO),
        f2p(Depth, HeadIs, RetTypeB, RetResultB, Convert, Converted)
    )), !,
    combine_code(
        (AsPredO, RetResult = RetResultA, RetType = RetTypeA);
        (Converted, RetResult = RetResultB, RetType = RetTypeB),
        CC
    ).
% If Convert is a match expression, extract atoms.
f2q(Depth, HeadIs, RetType, Atom, Convert, Converted) :-
    Convert =~ match(Space, Q, T),
    Q == T,
    Atom = Q, !,
    f2p(Depth, HeadIs, RetType, Atom, 'get-atoms'(Space), Converted).
% If Convert is a 'get-atoms' expression, compile the pattern.
f2q(Depth, HeadIs, _RetType, AtomsVar, Convert, Converted) :-
    Convert =~ 'get-atoms'(Space),
    Pattern = AtomsVar,
    compile_pattern(Depth, HeadIs, Space, Pattern, Converted).
% If Convert is a 'match' expression, compile the pattern and evaluate it.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ 'match'(ESpace, Pattern, Template), !,
    must_det_ll((
        f2p(Depth, HeadIs, _SpaceT, SpaceV, ESpace, Code),
        compile_pattern(Depth, HeadIs, SpaceV, Pattern, SpacePatternCode),
        f2p(Depth, HeadIs, RetType, RetResult, Template, TemplateCode),
        combine_code((Code, SpacePatternCode), TemplateCode, Converted)
    )).
% Helper to compile patterns in a space with a pattern.
compile_pattern(_Depth, _HeadIs, Space, Pattern, SpaceMatchCode) :-
    SpaceMatchCode = metta_atom_iter(Space, Pattern).

/*
  f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ 'match'(_Space,Match,Template),!,
   must_det_ll((
    f2p(Depth,HeadIs,RetType,_,Match,MatchCode),
    into_equals(RetResult,Template,TemplateCode),
    combine_code(MatchCode,TemplateCode,Converted))).
*/


%!  interpet_this(+Convert) is nondet.
%
%   Attempts to interpret the given `Convert` expression. The predicate first checks if
%   the term can be broken down into its functor, arity, and arguments, and then applies
%   interpretation rules based on the functor.
%
%   If the functor is symbolic or can be compiled, the predicate either fails or proceeds
%   with further interpretation. Special handling is used to avoid processing certain cases
%   (like when the functor is already compiled).
%
%   @arg Convert The term or expression to be interpreted.
%
interpet_this(_Convert):- 
    % General fail clause to prevent unintended interpretation unless a specific rule matches.
    !, fail.
interpet_this(Convert):- 
    % Decompose Convert into its functor, arity, and arguments, and attempt interpretation.
    as_functor_args(Convert, F, A, Args), interpet_this(Convert, F, A, Args).

%!  interpet_this(+Convert, +F, +A, +Args) is nondet.
%
%   Applies interpretation logic based on the functor `F`. Checks if `F` is symbolic, if it can
%   be compiled, or if it matches interpretation rules. If no matching rule applies, it fails.
%
%   @arg Convert The original term to be interpreted.
%   @arg F The functor of the term.
%   @arg A The arity of the term.
%   @arg Args The arguments of the term.
%
interpet_this(_, F, _, _) :- 
    % If F is not symbolic, fail.
    \+ symbolic(F), !.
interpet_this(_, F, _, _) :- 
    % If F can be compiled, fail (indicating that it should not be interpreted).
    compile_this_s(F), 
    !, fail.
interpet_this(_, F, _, _) :- 
    % If F matches an interpretation rule, succeed.
    interpet_this_f(F), !.
% Stable workaround for issues with '=~' operator for numbers.
interpet_this(Convert, F, A, Args) :- 
    % If Convert can be compiled, fail to avoid further interpretation.
    compile_this(Convert, F, A, Args), !, fail.
% Catch-all interpretation case.
interpet_this(_, _, _, _).

%!  interpet_this_f(+F) is nondet.
%
%   Helper predicate to handle specific functors `F` during interpretation. It fails for
%   certain functors that are compiled and succeeds for functors marked as interpreted.
%
%   @arg F The functor being checked.
%
interpet_this_f(_Convert) :- 
    % General fail clause to prevent interpretation unless a specific rule matches.
    !, fail.
interpet_this_f(F) :- 
    % If F is marked as 'Compiled' in the metta_atom_file_buffer, fail (do not interpret).
    metta_atom_file_buffer_isa(F, 'Compiled'), !, fail.
interpet_this_f(F) :- 
    % If F is marked as 'Interpreted', succeed.
    metta_atom_file_buffer_isa(F, 'Interpreted'), !.
interpet_this_f(F) :- 
    % If F is an operator declared for numbers, interpret it.
    op_decl(F, ['Number', 'Number'], 'Number').

%!  compile_this(+Convert) is nondet.
%
%   Attempts to compile the given `Convert` expression. The predicate first checks
%   if the term can be broken down into its functor, arity, and arguments, and then applies
%   compilation rules based on the functor. It will avoid compilation if certain conditions
%   are met, such as if the functor is already compiled or interpreted.
%
%   @arg Convert The term or expression to be compiled.
%
compile_this(_) :- 
    % General success clause to ensure the predicate does not fail unless a specific rule matches.
    !.
compile_this(Convert) :- 
    % Decompose Convert into its functor, arity, and arguments, and attempt compilation.
    as_functor_args(Convert, F, A, Args), compile_this(Convert, F, A, Args).

%!  compile_this(+Convert, +F, +A, +Args) is nondet.
%
%   Applies compilation logic based on the functor `F`. It checks if `F` is symbolic,
%   and if not, it fails. If `F` matches certain compilation rules, it proceeds.
%
%   @arg Convert The original term to be compiled.
%   @arg F The functor of the term.
%   @arg A The arity of the term.
%   @arg Args The arguments of the term.
%
compile_this(_, F, _, _) :- 
    % If F is not symbolic, fail to avoid compilation.
    \+ symbolic(F), !, fail.
compile_this(_, F, _, _) :- 
    % If F matches a compilation rule, succeed.
    compile_this_f(F), !.

%!  compile_this_f(+F) is nondet.
%
%   Helper predicate to handle specific functors `F` during compilation. If `F` is
%   already compiled or matches certain conditions, it succeeds.
%
%   @arg F The functor being checked for compilation.
%
compile_this_f(_) :- 
    % General success clause for most functors.
    !.
compile_this_f(F) :- 
    % If F is already marked as 'Compiled' in the metta_atom_file_buffer, succeed.
    metta_atom_file_buffer_isa(F, 'Compiled').
compile_this_f(F) :- 
    % If F is marked as 'Interpreted', fail to prevent compilation.
    interpet_this_f(F), !, fail.
compile_this_f(F) :- 
    % If F matches specific compilation rules, succeed.
    compile_this_s(F), !.
compile_this_f(F) :- 
    % If F is declared with '->' in the metta_atom_file_buffer, succeed.
    metta_atom_file_buffer([':', F, [Ar | _]]), Ar == '->', !.

%!  compile_this_s(+F) is nondet.
%
%   Handles specific functors `F` that are always considered valid for compilation.
%   These include logical operations and control flow predicates.
%
%   @arg F The functor being checked.
%
compile_this_s('superpose').
compile_this_s('match').
compile_this_s('do').
compile_this_s('do-all').

%!  f2q(+Depth, +HeadIs, +RetType, -RetResult, +Convert, -Converted) is det.
%
%   The `f2q/6` predicate is responsible for handling various forms of expressions
%   and transformations like inline definitions, "do", "do-all", and "let" constructs.
%   It takes an expression `Convert` and processes it recursively based on its structure,
%   producing a transformed or compiled result in `Converted`.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg RetResult The variable that will store the result.
%   @arg Convert The input data or expression to be processed.
%   @arg Converted The resulting output code after processing.
%

% Handle inline definitions by retrieving the inline definition and processing it.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :- 
    fail,  % Currently set to fail, this can be enabled when needed.
    dif_functors(HeadIs, Convert),get_inline_def(Convert, NewDef), !,
    must_det_ll((f2p(Depth, HeadIs, RetType, RetResult, NewDef, Converted))).
% Handle 'do' constructs by processing the body of the 'do'.
f2q(Depth, HeadIs, RetType, RetResult, Convert, do(Converted)) :- 
    Convert =~ ['do', Body], !,ignore(RetResult = 'Empty'),  % Default RetResult to 'Empty'.
    f2p(Depth, HeadIs, RetType, _RetResult, Body, Converted).
% Handle 'do-all' constructs by processing the body of 'do-all' and returning 'Empty' as RetResult.
f2q(Depth, HeadIs, _RetTypeD, RetResult, Convert, (doall(Converted), RetResult = 'Empty')) :- 
    Convert =~ ['do-all', Body], !,f2p(Depth, HeadIs, _RetTypeB, _RetResultB, Body, Converted).
% Handle 'let' expressions by binding a variable to a value and then processing the body.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-  
    Convert =~ ['let', Var, Value1, Body], !,f2p(Depth, HeadIs, _, ResValue1, Value1, CodeForValue1),
    into_equals(Var, ResValue1, CodeEquals),  % Bind Var to ResValue1.
    f2p(Depth, HeadIs, RetType, RetResult, Body, BodyCode),
    combine_code([CodeForValue1, CodeEquals, BodyCode], Converted).
% Another variation of handling 'let' expressions by processing the variable binding and body.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :- 
    Convert =~ ['let', Var, Value1, Body], !,f2p(Depth, HeadIs, _, Var, Value1, BindingCode),
    f2p(Depth, HeadIs, RetType, RetResult, Body, BodyCode),combine_code(BindingCode, BodyCode, Converted).

%!  is_Nil(+Nil) is nondet.
%
%   Succeeds if `Nil` represents an empty list or other forms of "nil" representations.
%   The `Nil` argument is checked against common representations of an empty or "nil" value.
%
%   @arg Nil The value to be checked for being an empty list or "nil".
%
is_Nil(Nil):- 
    % Check if Nil is the empty list [].
    Nil == [], !.
is_Nil(Nil):- 
    % Check if Nil is represented as the atom 'Nil'.
    Nil == 'Nil', !.
is_Nil(Nil):- 
    % Check if Nil is represented as the atom '()'.
    Nil == '()', !.

%!  f2q(+Depth, +HeadIs, +RetType, -RetResult, +Convert, -Converted) is det.
%
%   The `f2q/6` predicate handles various forms of control flow and expressions, such as 
%   `let*`, `superpose`, `sequential`, and conditional constructs (`if`, `if-error`, etc.).
%   It processes and transforms the input `Convert` into corresponding logical or code structures,
%   producing the output as `Converted`.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg RetResult The variable that will store the result.
%   @arg Convert The input data or expression to be processed.
%   @arg Converted The resulting output code after processing.
%

% Handle 'let*' expressions with an empty list of bindings.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['let*', Nil, Body], is_Nil(Nil), !,
    must_det_ll((f2p(Depth, HeadIs, RetType, RetResult, Body, Converted))).
% Handle 'let*' expressions with bindings.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['let*', AAAA, Body], AAAA =~ [VE | Bindings], VE =~ [V, E],
    f2q(Depth, HeadIs, RetType, RetResult, ['let', V, E, ['let*', Bindings, Body]], Converted).
% Handle 'let*' expressions using `compile_let_star/4` for each binding.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :- 
    fail, 
    dif_functors(HeadIs,Convert),
    Convert =~ ['let*', Bindings, Body], !,
    must_det_ll((
        maplist(compile_let_star(Depth, HeadIs, RetType), Bindings, CodeList),
        combine_code(CodeList, BindingCode),
        f2p(Depth, HeadIs, RetType, RetResult, Body, BodyCode),
        combine_code(BindingCode, BodyCode, Converted)
    )).

%!  compile_let_star(+Depth, +HeadIs, +RetType, +NV, -Converted) is det.
%
%   Helper predicate to compile individual bindings in a 'let*' expression.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg NV The binding to be compiled, consisting of an expression and variable.
%   @arg Converted The resulting output code for the binding.
%
compile_let_star(Depth, HeadIs, RetType, NV, Converted) :-
    must_det_ll((
        NV =~ [Expression, Var],
        (var(Var) -> 
            f2p(Depth, HeadIs, RetType, Var, Expression, Converted)
        ; (var(Expression) -> 
            f2p(Depth, HeadIs, RetType, Expression, Var, Converted)
        ; (f2p(Depth, HeadIs, RetType, Eval1Result, Expression, Code),
            into_equals(Eval1Result, Var, Eval1ResultVar),
            combine_code(Code, Eval1ResultVar, Converted)))
        )
    )), !.

% Handle 'superpose' with a 'collapse' in the collection.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['superpose', COL], compound_equals(COL, 'collapse'(Value1)), !,
    f2p(Depth, HeadIs, RetType, ResValue1, Value1, CodeForValue1),
    Converted = (find_ne(ResValue1, CodeForValue1, Gathered), member(RetResult, Gathered)).
% Handle 'sequential' by transforming it into a 'superpose' expression.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['sequential' | ValueL], ReConvert =~ ['superpose' | ValueL], !,
    f2q(Depth, HeadIs, RetType, RetResult, ReConvert, Converted).
% Handle 'sequential' expressions with a list of values.
f2q(Depth, HeadIs, RetType, RetResult, Convert, (Converted)) :-
    Convert =~ ['sequential', ValueL], is_list(ValueL), !,
    maplist(f2p_assign(Depth, HeadIs, RetType), RetResultL, ValueL, CodeForValueL),
    last(RetResultL, RetResult),
    combine_code(CodeForValueL, Converted), !.
% Handle 'superpose' expressions with a list of values.
f2q(Depth, HeadIs, RetType, RetResult, Convert, (Converted)) :-
    Convert =~ ['superpose', ValueL], is_list(ValueL), !,
    must_det_ll((
        ignore(cname_var('SP_Ret', RetResult)),
        maplist(f2p(Depth, HeadIs, RetType, RetResult), ValueL, CodeForValueL),
        list_to_disjuncts(CodeForValueL, Converted)
    )), !.
% Handle 'superpose' expressions with non-standard variables.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, (Converted)) :-
    Convert =~ ['superpose', ValueL], is_nsVar(ValueL), !,
    Converted = call('superpose'(ValueL, RetResult)),
    cname_var('MeTTa_SP_', ValueL).

:- op(700,xfx,=~).

% Handle 'chain' expressions.
f2q(Depth, HeadIs, RetType, RetResult, Convert, (Code1, Eval1Result = Result, Converted)) :-
    Convert =~ 'chain'(Eval1, Result, Eval2), !,
    f2p(Depth, HeadIs, RetType, Eval1Result, Eval1, Code1),
    f2p(Depth, HeadIs, RetType, RetResult, Eval2, Converted).
% Handle 'eval-in-space' expressions.
f2q(Depth, HeadIs, RetType, ResValue2, Convert, (CodeForValue1, Converted)) :-
    Convert =~ ['eval-in-space', Value1, Value2], 
    f2p(Depth, HeadIs, RetType, ResValue1, Value1, CodeForValue1),
    f2p(Depth, HeadIs, RetType, ResValue2, Value2, CodeForValue2),
    Converted = with_space(ResValue1, CodeForValue2).
% Handle 'if' or 'If' conditional expressions.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    once(Convert =~ 'if'(Cond, Then, Else); Convert =~ 'If'(Cond, Then, Else)), !,
    Test = is_True(CondResult),f2p(Depth, HeadIs, RetType, CondResult, Cond, CondCode),
    compile_test_then_else(Depth, RetResult, (CondCode, Test), Then, Else, Converted).
% Handle 'if' or 'If' conditional expressions with no Else branch.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    dif_functors(HeadIs,Convert),
    once(Convert =~ 'if'(Cond, Then); Convert =~ 'If'(Cond, Then)), 
    f2p(Depth, HeadIs, RetType, CondResult, Cond, CondCode),
    f2p(Depth, HeadIs, RetType, RetResult, Then, ThenCode),
    combine_code([CondCode, is_True(CondResult), ThenCode], Converted).
% Handle 'if-error' conditional expressions.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ 'if-error'(Value, Then, Else), !, Test = is_Error(ValueResult),
    f2p(Depth, HeadIs, RetType, ValueResult, Value, ValueCode),
    combine_code(ValueCode, Test, ValueCodeTest),
    compile_test_then_else(Depth, RetResult, ValueCodeTest, Then, Else, Converted).
% Handle 'if-empty' conditional expressions.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ 'if-empty'(Value, Then, Else), !, Test = is_Empty(ValueResult),
    f2p(Depth, HeadIs, RetType, ValueResult, Value, ValueCode),
    compile_test_then_else(Depth, RetResult, (ValueCode, Test), Then, Else, Converted).
% Handle 'if-non-empty-expression' conditional expressions.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    (Convert =~ 'if-non-empty-expression'(Value, Then, Else)), !,
    (Test = (\+ is_Empty(ValueResult))),
    f2p(Depth, HeadIs, RetType, ValueResult, Value, ValueCode),
    compile_test_then_else(Depth, RetResult, (ValueCode, Test), Then, Else, Converted).
% Handle 'if-equals' conditional expressions.
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ ['if-equals',Value1,Value2,Then,Else],!,Test = equal_enough(ResValue1,ResValue2),
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
    compile_test_then_else(Depth,RetResult,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).

%!  cname_var(+Sym, +Src) is det.
%
%   The `cname_var/2` predicate generates or handles variable names (`Sym`) based on the
%   input `Src`. It handles cases where `Src` is a variable, a `$VAR` term, or already
%   bound. It uses `gensym/2` to generate new variable names if necessary.
%
%   @arg Sym The symbolic name or prefix used for generating variable names.
%   @arg Src The source variable that might be bound or used to generate a name.
%
cname_var(Sym, _Src) :- 
    % If Sym is a variable, succeed (do nothing).
    var(Sym), !.
cname_var(Sym, Src) :- 
    % If Src is a variable, generate a new name using gensym and bind it to Src.
    var(Src), !,
    must_det_ll((
        gensym(Sym, SrcV), 
        Src = '$VAR'(SrcV)
    )).
cname_var(Sym, Src) :- 
    % If Src is a '$VAR' term, generate a new name using gensym and update Src.
    Src = '$VAR'(_), !,
    must_det_ll((
        gensym(Sym, SrcV), 
        nb_setarg(1, Src, SrcV)
    )).
cname_var(_Sym, _Src).   % Catch-all case: do nothing for other cases.

cname_var(Name = Var) :- cname_var(Name, Var).

%!  f2q(+Depth, +HeadIs, +RetType, -RetResult, +Convert, -Converted) is det.
%
%   Handles `assertEqual` and `assertEqualToResult` expressions by generating code
%   that compares two values for equality, using `findall_ne/3` to evaluate both values.
%   Assertions are processed using `loonit_assert_source_tf_empty/5`, which ensures that
%   the source is asserted correctly and compares the evaluated results.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg RetResult The variable that will store the result.
%   @arg Convert The input data or expression to be processed.
%   @arg Converted The resulting output code after processing.
%

% Handle 'assertEqual' expressions that compare two values.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['assertEqual', Value1, Value2], !,
    % Generate unique variable names for Src, ResValue1, ResValue2, L1, and L2.
    cname_var('Src_', Src),
    cname_var('FA_', ResValue1),
    cname_var('FA_', ResValue2),
    cname_var('FARL_', L1),
    cname_var('FARL_', L2),
    % Process Value1 and Value2, generating corresponding code for each.
    f2p(Depth, HeadIs, RetType, ResValue1, Value1, CodeForValue1),
    f2p(Depth, HeadIs, RetType, ResValue2, Value2, CodeForValue2),
    % Combine the generated code for equality assertion.
    Converted = (
        Src = Convert,
        loonit_assert_source_tf_empty(Src, L1, L2,
            (findall_ne(ResValue1, CodeForValue1, L1),
             findall_ne(ResValue2, CodeForValue2, L2)),
            equal_enough_for_test(L1, L2), RetResult)
    ).
% Handle 'assertEqualToResult' expressions that compare a value to a result.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['assertEqualToResult', Value1, Value2], !,
    % Process Value1, generating corresponding code for it.
    f2p(Depth, HeadIs, RetType, ResValue1, Value1, CodeForValue1),
    % Set the source and goal for the assertion.
    Src = Convert,
    Goal = findall_ne(ResValue1, CodeForValue1, L1),
    % Combine the generated code for the assertion.
    Converted = (
        loonit_assert_source_tf_empty(Src, L1, Value2,
            Goal,
            equal_enough_for_test(L1, Value2), RetResult)
    ).

%!  maybe_unlistify(+UValueL, +ValueL, +RetResult, +URetResult) is det.
%
%   This predicate is used to ensure that the structure of lists and variables remains consistent.
%   It attempts to unlistify a list, ensuring that single-element lists are properly handled by
%   recursing through them. If the list has multiple elements, it simply unifies the values.
%
%   @arg UValueL A possibly nested list.
%   @arg ValueL The corresponding list value.
%   @arg RetResult The result value that may correspond to the unlisted or original structure.
%   @arg URetResult The unlisted result.
%
% Currently set to fail, can be enabled later.
maybe_unlistify([UValueL], ValueL, RetResult, [URetResult]) :- 
    fail,  
    is_list(UValueL), !,
    % Recurse to handle nested lists.
    maybe_unlistify(UValueL, ValueL, RetResult, URetResult).
% If the structure is already in its simplest form, unify the values.    
maybe_unlistify(ValueL, ValueL, RetResult, RetResult).  

%!  list_to_disjuncts(+List, -Disjunction) is det.
%
%   Converts a list of elements into a disjunction (a sequence of `;` operators). 
%   The base case for an empty list is `false`, and single elements are returned directly.
%   Multiple elements are combined into a disjunction (e.g., `(A; B; C)`).
%
%   @arg List The list of elements to be converted.
%   @arg Disjunction The resulting disjunction (logical OR).
%
list_to_disjuncts([], false).  % Base case: an empty list becomes false.
list_to_disjuncts([A], A) :- 
    % Base case: a single element remains as it is.
    !.
list_to_disjuncts([A | L], (A; D)) :- 
    % Recursively convert the list into a disjunction of elements.
    list_to_disjuncts(L, D).

%f2p_assign(Depth,_HeadIs,_RetType,V,Value,is_True(V)):- Value=='True'.

%!  f2p_assign(+Depth, +HeadIs, +RetType, -ValueResult, +Value, -Converted) is det.
%
%   The `f2p_assign/6` predicate handles the assignment or processing of values, generating
%   the appropriate code for assigning or transforming `Value` into `ValueResult`. It accounts
%   for variables, simple values, and compound terms.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg ValueResult The result variable that will store the processed value.
%   @arg Value The input value to be assigned or processed.
%   @arg Converted The resulting code after processing.
%
f2p_assign(_Depth, _HeadIs, _RetType, ValueR, Value, ValueR = Value) :- 
    % If Value is a non-standard variable (nsVar), directly assign ValueR = Value.
    is_nsVar(Value), !.
f2p_assign(_Depth, _HeadIs, _RetType, ValueR, Value, ValueR = Value) :- 
    % If Value is not a compound term, directly assign ValueR = Value.
    \+ compound(Value), !.
f2p_assign(_Depth, _HeadIs, _RetType, ValueResult, Value, Converted) :- 
    % If there is a direct conversion using f2p for the value, use it.
    f2p(Value, ValueResult, Converted), !.
f2p_assign(Depth, HeadIs, RetType, ValueResult, Value, Converted) :- 
    % For compound terms, generate code to process the value and assign the result.
    f2p(Depth, HeadIs, RetType, ValueResultR, Value, CodeForValue),
    ValueResultRValueResult = (ValueResultR = ValueResult),
    combine_code(CodeForValue, ValueResultRValueResult, Converted).

%!  f2p_arg(+Depth, +HeadIs, +RetType, -ValueResult, +Value, -Converted) is det.
%
%   The `f2p_arg/6` predicate processes an argument `Value`, generating code for
%   handling it and assigning the result to `ValueResult`. It handles both simple
%   and compound values, as well as non-standard variables.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg ValueResult The result variable that will store the processed value.
%   @arg Value The input value to be processed.
%   @arg Converted The resulting code after processing.
%
f2p_arg(_Depth, _HeadIs, _RetType, Value, Value, true) :- 
    % If Value is a non-standard variable, no further processing is needed.
    is_nsVar(Value), !.
f2p_arg(_Depth, _HeadIs, _RetType, Value, Value, true) :- 
    % If Value is not a compound term, no further processing is needed.
    \+ compound(Value), !.
f2p_arg(_Depth, _HeadIs, _RetType, ValueResult, Value, Converted) :- 
    % If there is a direct conversion using h2p for the value, use it.
    h2p(Value, ValueResult, Converted), !.
f2p_arg(Depth, HeadIs, RetType, ValueResult, Value, Converted) :- 
    % If none of the above, delegate to f2p_assign/6 for further processing.
    f2p_assign(Depth, HeadIs, RetType, ValueResult, Value, Converted).
f2q(Depth,HeadIs,RetType,RetResult,Convert, keep(Converted)) :-
  Convert =~ ['case',Value,PNil],[]==PNil,!,Converted = (ValueCode,RetResult=[]),
      f2p(Depth,HeadIs,RetType,_ValueResult,Value,ValueCode).

%!  f2q(+Depth, +HeadIs, +RetType, -RetResult, +Convert, -Converted) is det.
%
%   This `f2q/6` predicate handles `case` expressions by processing the case value
%   and its options (branches). It uses helper predicates to generate the corresponding
%   code for case matching and branching.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg RetResult The variable that will store the result.
%   @arg Convert The input data or expression to be processed.
%   @arg Converted The resulting output code after processing.
%

% Case handling where the case value is not a variable and needs evaluation.
f2q(Depth, HeadIs, RetType, RetResult, Convert, (ValueCode, Converted)) :-
    Convert =~ ['case', Value | Options], \+ is_nsVar(Value), !,
    % Generate a variable for the case value.
    cname_var('CASE_VAR_', ValueResult),
    % Recursively process the 'case' with ValueResult and Options.
    f2q(Depth, HeadIs, RetType, RetResult, ['case', ValueResult | Options], Converted),
    % Generate code for evaluating the case value.
    f2p(Depth, HeadIs, RetType, ValueResult, Value, ValueCode).

% Main case handling clause with options.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['case', Value, Options], !,
    % Generate the code for processing the case options.
    must_det_ll((
        maplist(compile_case_bodies(Depth, HeadIs, RetType), Options, Cases),
        cname_var('SWITCH_', AllCases),
        cname_var('CASE_RESULT_', RetResult),
        % Combine the code for all cases and match them against the case value.
        Converted = (
            AllCases = Cases,
            select_case(AllCases, Value, RetResult)
        )
    )).

%!  select_case(+AllCases, +Value, -BodyResult) is det.
%
%   Helper predicate to select the appropriate case option based on the match value.
%
%   @arg AllCases The list of all case options.
%   @arg Value The value to be matched against.
%   @arg BodyResult The result of the body corresponding to the matched case.
%
select_case(AllCases, Value, BodyResult) :-
    % Find the case option where the match value matches, then execute its code.
    once((
        member(caseOption(MatchVar, MatchCode, BodyResult, BodyCode), AllCases),
        rtrace_on_error(MatchCode),
        unify_case(Value, MatchVar)
    )), !,
    rtrace_on_error(BodyCode).

% Case handling with specific options.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['case', Value, [Opt | Options]], nonvar(Opt), !,
    % Compile the first case option.
    must_det_ll((
        compile_case_bodies(Depth, HeadIs, RetType, Opt, caseOption(Value, If, RetResult, Then)),
        % Combine the first option with the rest of the cases.
        Converted = (If -> Then ; Else),
        ConvertCases =~ ['case', Value, Options],
        f2q(Depth, HeadIs, RetType, RetResult, ConvertCases, Else)
    )).

/*
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,Options],!,
   must_det_ll((
    maplist(compile_case_bodies(Depth,HeadIs,RetType),Options,Cases),
    Converted =
        (( AllCases = Cases,
           once((member(caseOption(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
                 (MatchCode,unify_enough(Value,MatchVar)))),
           (BodyCode),
           BodyResult=RetResult)))).

f2q(Depth,HeadIs,RetType,_,Convert, Converted) :-
  Convert =~ ['case',Value,Options,RetResult],!,
   must_det_ll((
    f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
    maplist(compile_case_bodies(Depth,HeadIs,RetType),Options,Cases),
    Converted =
        (( AllCases = Cases,
           call(ValueCode),
           once((member(caseOption(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
                 both_of(ValueResult,MatchCode,unify_enough(ValueResult,MatchVar)))),
           call(BodyCode),
           BodyResult=RetResult)))).


both_of(Var,G1,G2):- nonvar(Var),!,call(G2),call(G1).
both_of(_Var,G1,G2):- call(G1),call(G2).

*/

%!  compile_case_bodies(+Depth, +HeadIs, +RetType, +MatchBody, -CaseOption) is det.
%
%   Compiles individual case bodies into structured case options. The case options consist
%   of the match condition (`If`), the result of matching (`MatchResult`), and the body
%   code (`BodyCode`). Special handling is applied if the match is '%void%', which always
%   succeeds with `true` for the match condition.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg MatchBody The case match and body pair to be processed.
%   @arg CaseOption The resulting compiled case option.
%
compile_case_bodies(Depth, HeadIs, RetType, [Match, Body], caseOption(_, true, BodyResult, BodyCode)) :- 
    % Special case: if Match is '%void%', the condition is always true.
    Match == '%void%', !,
    f2p(Depth, HeadIs, RetType, BodyResult, Body, BodyCode).
compile_case_bodies(Depth, HeadIs, RetType, [Match, Body], caseOption(MatchResult, If, BodyResult, BodyCode)) :- !,
    % General case: compile the match and the body.
    f2p(Depth, HeadIs, RetType, MatchResultV, Match, MatchCode),
    % Combine match code with the condition to unify case variables.
    combine_code(MatchCode, unify_case(MatchResult, MatchResultV), If),
    f2p(Depth, HeadIs, RetType, BodyResult, Body, BodyCode).
compile_case_bodies(Depth, HeadIs, RetType, MatchBody, CS) :- 
    % If the match body is compound, decompose it and process further.
    compound(MatchBody), MatchBody =~ MB, 
    compile_case_bodies(Depth, HeadIs, RetType, MB, CS).

%!  compound_equals(+COL1, +COL2) is det.
%
%   This predicate checks whether two compound terms are equal. It first checks for
%   syntactic equality (`@=`), and if that fails, it proceeds with deeper comparisons.
%   Special handling is applied if the terms are non-standard variables (`nsVar`).
%
%   @arg COL1 The first compound term or variable.
%   @arg COL2 The second compound term or variable.
%
compound_equals(COL1, COL2) :- 
    % If COL1 and COL2 are syntactically equal, unify them.
    COL1 =@= COL2, !, COL1 = COL2.
compound_equals(COL1, COL2) :- 
    % Fall back to more complex comparison if syntactic equality fails.
    compound_equals1(COL1, COL2).

%!  compound_equals1(+COL1, +COL2) is det.
%
%   Helper predicate for `compound_equals/2`. It handles specific cases for
%   non-standard variables and compound terms.
%
%   @arg COL1 The first term or variable.
%   @arg COL2 The second term or variable.
%
compound_equals1(COL1, COL2) :- 
    % If COL1 is a non-standard variable, ensure COL2 is also a non-standard variable, and unify them.
    is_nsVar(COL1), !, is_nsVar(COL2), ignore(COL1 = COL2), !.
compound_equals1(COL1, COL2) :- 
    % If both COL1 and COL2 are compound terms, unify them.
    compound(COL1), !, compound(COL2), COL1 = COL2.

%!  f2q(+Depth, +HeadIs, +RetType, -RetResult, +Convert, -Converted) is det.
%
%   The `f2q/6` predicate handles various expressions such as 'collapse', 'compose',
%   'unify', 'if-decons', logical 'and', and 'or' operations. It processes the given
%   expression (`Convert`) and transforms it into corresponding logical or code structures.
%
%   @arg Depth The current depth level for recursion or processing.
%   @arg HeadIs The head of the structure or process being used.
%   @arg RetType The expected return type.
%   @arg RetResult The variable that will store the result.
%   @arg Convert The input data or expression to be processed.
%   @arg Converted The resulting output code after processing.
%

% Handle 'collapse' expressions by evaluating and collapsing the result.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['collapse', Value1], !,
    f2p(Depth, HeadIs, RetType, ResValue1, Value1, CodeForValue1),
    Converted = (findall_ne(ResValue1, CodeForValue1, RetResult)).
% Handle 'compose' expressions by treating them as 'collapse'.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['compose', Value1], !,
    Convert2 =~ ['collapse', Value1], !,
    f2q(Depth, HeadIs, RetType, RetResult, Convert2, Converted).
% Handle 'unify' expressions that check unification and execute 'Then' or 'Else'.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['unify', Value1, Value2, Then, Else], !,
    Test = metta_unify(ResValue1, ResValue2),
    f2p(Depth, HeadIs, RetType, ResValue1, Value1, CodeForValue1),
    f2p(Depth, HeadIs, RetType, ResValue2, Value2, CodeForValue2),
    compile_test_then_else(Depth, RetResult, (CodeForValue1, CodeForValue2, Test), Then, Else, Converted).
% Handle 'if-decons' expressions that deconstruct lists and execute 'Then' or 'Else'.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    dif_functors(HeadIs,Convert),
    Convert =~ ['if-decons', Atom, Head, Tail, Then, Else], !,
    Test = unify_cons(AtomResult, ResHead, ResTail),
    f2p(Depth, HeadIs, RetType, AtomResult, Atom, AtomCode),
    f2p(Depth, HeadIs, RetType, ResHead, Head, CodeForHead),
    f2p(Depth, HeadIs, RetType, ResTail, Tail, CodeForTail),
    compile_test_then_else(Depth, RetResult, (AtomCode, CodeForHead, CodeForTail, Test), Then, Else, Converted).
% Handle logical 'and' with no body, which always evaluates to 'True'.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, was_True(RetResult)) :-
    is_compiled_and(AND),
    Convert =~ [AND], !.
% Handle logical 'and' with a single body.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    is_compiled_and(AND),
    Convert =~ [AND, Body], !,
    f2p(Depth, HeadIs, RetType, RetResult, Body, BodyCode),
    compile_test_then_else(Depth, RetResult, BodyCode, 'True', 'False', Converted).
% Handle logical 'and' with two bodies.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    is_compiled_and(AND),
    Convert =~ [AND, Body1, Body2], !,
    f2p(Depth, HeadIs, RetType, B1Res, Body1, Body1Code),
    f2p(Depth, HeadIs, RetType, RetResult, Body2, Body2Code),
    into_equals(B1Res, 'True', AE),
    Converted = (Body1Code, AE, Body2Code), !.
% Handle logical 'and' with two bodies, returning 'True' or 'False'.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    is_compiled_and(AND),
    Convert =~ [AND, Body1, Body2], !,
    f2p(Depth, HeadIs, RetType, B1Res, Body1, Body1Code),
    f2p(Depth, HeadIs, RetType, _, Body2, Body2Code),
    into_equals(B1Res, 'True', AE),
    compile_test_then_else(Depth, RetResult, (Body1Code, AE, Body2Code), 'True', 'False', Converted).
% Handle logical 'and' with more than two bodies by chaining the remaining bodies.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    is_compiled_and(AND),
    Convert =~ [AND, Body1, Body2 | BodyMore], !,
    And2 =~ [AND, Body2 | BodyMore],
    Next =~ [AND, Body1, And2],
    f2q(Depth, HeadIs, RetType, RetResult, Next, Converted).
% Handle logical 'or' expressions by converting to ";" (disjunction).
f2q(Depth,HeadIs,RetType,RetResult,SOR,or(AsPredO, Converted)) :-
  SOR =~ or(AsPredI, Convert),
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))),!.

f2q(Depth,HeadIs,RetType,RetResult,or(AsPredI,Convert), (AsPredO *-> true; Converted)) :- fail, !,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))).
% Handle logical 'or' with conditional disjunction.
f2q(Depth, HeadIs, RetType, RetResult, (AsPredI; Convert), (AsPredO; Converted)) :- !,
    must_det_ll((
        f2p(Depth, HeadIs, RetType, RetResult, AsPredI, AsPredO),
        f2p(Depth, HeadIs, RetType, RetResult, Convert, Converted)
    )).

%!  'True'(+X) is det.
%
%   This predicate checks if X is true. It uses the helper predicate `is_True/1` to
%   determine the truth value of X. If `is_True(X)` fails, it is ignored, meaning the
%   predicate will still succeed even if `is_True(X)` does not hold.
%
%   @arg X The value to be checked for truth.
%
%   @example
%     ?- 'True'(some_value).
%
'True'(X):- 
    % Call the predicate `is_True/1`, ignoring any failures.
    ignore(is_True(X)).

%!  'False'(+X) is det.
%
%   This predicate checks if X is false. It succeeds only if the helper predicate 
%   `is_False/1` succeeds. Unlike `True/1`, no failure is ignored here.
%
%   @arg X The value to be checked for falsity.
%
%   @example
%     ?- 'False'(some_value).
%
'False'(X):- 
    % Call the predicate `is_False/1` to check if X is false.
    is_False(X).

%!  get_inline_case_list(+HeadDef, +Quot, -CaseList) is det.
%
%   Retrieves a list of inline cases for a given head definition (`HeadDef`) and a quoted
%   element (`Quot`). This is done by finding all definitions inline with `HeadDef` and pairing
%   them with `Quot`.
%
%   The predicate first finds all possible inline definitions (`NewDef`) that can be
%   derived from `HeadDef` and ensures the list of definitions (`DefList`) is not empty.
%   Then, it matches each inline definition with the quoted element `Quot` to create the
%   final `CaseList`.
%
%   @arg HeadDef The original head definition to find inline cases for.
%   @arg Quot    A quoted element to be paired with the inline definitions.
%   @arg CaseList The resulting list of cases, each being a pair of `Quot` and `NewDef`.
%
%   @example
%     ?- get_inline_case_list(some_head, 'Q', CaseList).
%
get_inline_case_list(HeadDef,Quot,CaseList):-
   % Find all inline definitions for the given HeadDef and ensure the list is not empty.
   findall([HeadDef,NewDef],get_inline_def1(HeadDef,NewDef),DefList), DefList \== [],
   % Create a case list by pairing Quot with each inline definition in DefList.
   findall([Quot,NewDef],member([HeadDef,NewDef],DefList),CaseList).

%!  get_inline_def(+HeadDef, -NewDef) is det.
%
%   Retrieves a disjunction of all possible inline definitions for a given head definition (`HeadDef`).
%   This is done by collecting all definitions that match `HeadDef` using `get_inline_def1/2` and then
%   combining them into a disjunction.
%
%   The predicate first gathers all matching inline definitions (`EachDef`) and ensures the list is
%   not empty. Then, it calls `disj_def/2` to combine them into a single disjunctive definition (`NewDef`).
%
%   @arg HeadDef The original head definition for which inline definitions are sought.
%   @arg NewDef  The resulting disjunctive definition combining all inline definitions.
%
%   @example
%     ?- get_inline_def(some_head, NewDef).
%
get_inline_def(HeadDef,NewDef):-
   % Find all inline definitions for the given HeadDef and ensure the list is not empty.
   findall(NewDef,get_inline_def1(HeadDef,NewDef),EachDef), EachDef \== [],
   % Combine all inline definitions into a disjunction.
   disj_def(EachDef,NewDef).

%!  get_inline_def1(+HeadDef, -NewDef) is det.
%
%   Finds a single inline definition (`NewDef`) for a given head definition (`HeadDef`).
%   This is done by transforming the body of a clause that matches `HeadDef`.
%
%   The predicate first converts `HeadDef` into a list of arguments (`UHeadDef`), then
%   makes a copy of it to preserve the original term (`CopyUHeadDef`). It extracts the
%   head (`UHead`) and ensures it is a non-variable term. After that, it retrieves a clause
%   from the metta_atom_file_buffer and transforms its body using `xform_body/2`. The process
%   succeeds only if the transformed clause is equivalent to the original.
%
%   @arg HeadDef The original head definition to transform.
%   @arg NewDef  The transformed inline definition.
%
%   @example
%     ?- get_inline_def1(some_head, NewDef).
%
get_inline_def1(HeadDef,NewDef):-
   % Convert HeadDef into a list of arguments.
   into_list_args(HeadDef,UHeadDef),
   % Make a copy of UHeadDef to preserve the original structure.
   copy_term(UHeadDef,CopyUHeadDef),
   % Extract the head (UHead) and ensure it is not a variable.
   [UHead|_UArgs] = UHeadDef, nonvar(UHead),
   % Retrieve a clause from the buffer and ensure it is an equality (=) clause.
   metta_atom_file_buffer([Eq,UHeadDef|Body]), Eq == '=',
   % Transform the body into NewDef.
   once(xform_body(Body,NewDef)),
   % Ensure the transformed definition is equivalent to the original.
   (UHeadDef =@= CopyUHeadDef).

%xform_body([Body],Body):-!.
%xform_body(Items,[progn|Body]).

%!  xform_body(+Input, -TransformedBody) is det.
%
%   Transforms a list of goals into a compound goal (conjunction). This predicate handles
%   different cases depending on the structure of the input:
%   - If the input is a variable and satisfies `is_ftVar/1`, the input is returned unchanged.
%   - If the input is an empty list, it returns `call(true)` (indicating an empty body).
%   - For a list with a single element, it returns the element itself.
%   - For two elements, it creates a conjunction `(Body1, Body2)`.
%   - For more than two elements, it recursively constructs a conjunction.
%
%   @arg Input The input term (variable, list, etc.) to be transformed.
%   @arg TransformedBody The resulting transformed body as a conjunction.
%
%   @example
%     ?- xform_body([goal1, goal2, goal3], Result).
%     Result = (goal1, (goal2, goal3)).
%
xform_body(Var,Var):- 
    % If Var is a free-term variable, return it unchanged.
    is_ftVar(Var), !.
xform_body([],call(true)):- 
    % If the input is an empty list, return call(true).
    !.
xform_body([Body],Body):- 
    % If there is only one body element, return it directly.
    !.
xform_body([Body1,Body2],(Body1,Body2)):- 
    % If there are exactly two body elements, create a conjunction.
    !.
xform_body([Body1|Body2L],(Body1,Body2)):- 
    % For a list of more than two elements, recursively create a conjunction.
    xform_body(Body2L,Body2).

%!  disj_def(+Input, -Disjunction) is det.
%
%   Transforms a list of goals into a disjunction. Similar to `xform_body/2`, but the result
%   is a disjunction `(Body1; Body2)` instead of a conjunction. This predicate handles:
%   - Variables satisfying `is_ftVar/1` are returned unchanged.
%   - An empty list results in `call(fail)` (indicating no solutions).
%   - A single element is returned as-is.
%   - Two elements are combined into a disjunction `(Body1; Body2)`.
%   - For more than two elements, it recursively constructs a disjunction.
%
%   @arg Input The input term (variable, list, etc.) to be transformed into a disjunction.
%   @arg Disjunction The resulting disjunction.
%
%   @example
%     ?- disj_def([goal1, goal2, goal3], Result).
%     Result = (goal1; (goal2; goal3)).
%
disj_def(Var,Var):- 
    % If Var is a free-term variable, return it unchanged.
    is_ftVar(Var), !.
disj_def([],call(fail)):- 
    % If the input is an empty list, return call(fail).
    !.
disj_def([Body],Body):- 
    % If there is only one body element, return it directly.
    !.
disj_def([Body1,Body2],(Body1;Body2)):- 
    % If there are exactly two body elements, create a disjunction.
    !.
disj_def([Body1|Body2L],(Body1;Body2)):- 
    % For a list of more than two elements, recursively create a disjunction.
    disj_def(Body2L,Body2).

/*
f2q(Depth,HeadIs,RetType,RetResult,transpose(Convert), Converted,Code) :- !,
   maplist(each_result(Depth,HeadIs,RetType,RetResult),Convert, Converted),
   list_to_disjuncts(Converted,Code).

each_result(Depth,HeadIs,RetType,RetResult,Convert,Converted):-
   f2p(Depth,HeadIs,RetType,OneResult,Convert,Code1),
   into_equals(OneResult,RetResult,Code2),
   combine_code(Code1,Code2,Converted).

*/
/*
f2q(Depth,HeadIs,RetType,RetResult,Convert, once(u_assign(Body,RetResult))) :-
  Convert=~ first_of(Body), is_ftVar(Body),!.
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert=~ first_of(Body),
  must_det_ll((as_functor_args(Body,F,A,Args),
  as_functor_args(Quot,quot,A,NewArgs),
  as_functor_args(QConvert,quot,A,Args))),
  get_inline_case_list([F|NewArgs],Quot,DefList),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,[case,QConvert,DefList],Converted))).*/

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is det.
%
%   This predicate handles various types of conversions of a `Convert` term into a `Converted`
%   term, based on specific patterns. It works with constructs like `once/1`, `catch/3`,
%   `call_cleanup/2`, and `not/1`. The `Depth`, `HeadIs`, `RetType`, and `RetResult` arguments
%   are passed down to helper predicates such as `f2p/6` and `s2p/2`.
%
%   @arg Depth     The current depth level of the transformation.
%   @arg HeadIs    The current head of the transformation process.
%   @arg RetType   The return type of the predicate being transformed.
%   @arg RetResult The result of the transformation.
%   @arg Convert   The term to be converted.
%   @arg Converted The resulting term after conversion.
%
%   @example
%     ?- f2q(1, some_head, some_ret_type, some_ret_result, ConvertTerm, ConvertedTerm).
%

% Handle the case where Convert is '~ first_of(Body)' and convert it using f2p.
f2q(Depth,HeadIs,RetType,RetResult,Convert, once(Converted)) :-
  % Pattern match for first_of(Body) in Convert.
  Convert =~ first_of(Body), 
  !,
  % Call f2p to process the Body and convert it.
  f2p(Depth,HeadIs,RetType,RetResult,Body,Converted).
% Handle the case where Convert is '~ catch(Body, E, Handler)' and convert it using f2p and s2p.
f2q(Depth,HeadIs,RetType,RetResult,Convert, catch(BodyCode,Ex,HandlerCode)) :-
    % Pattern match for catch(Body, E, Handler) in Convert.
    Convert =~ catch(Body,E,Handler), 
    !,
    % Convert the exception term using s2p.
    s2p(E,Ex),
    % Convert the body and handler using f2p.
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    f2p(Depth,HeadIs,RetType,RetResult,Handler,HandlerCode).
% Handle the case where Convert is '~ finally(Body, Handler)' and convert it using f2p.
f2q(Depth,HeadIs,RetType,RetResult,Convert, call_cleanup(BodyCode,HandlerCode)) :-
    % Pattern match for finally(Body, Handler) in Convert.
    Convert =~ finally(Body,Handler), 
    !,
    % Convert the body and handler using f2p.
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    f2p(Depth,HeadIs,RetType,RetResult,Handler,HandlerCode).
% Handle the case where Convert is an inline definition.
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    % Fail first to force backtracking if this branch does not match.
    fail,
    % Ensure Convert has a different functor from HeadIs.
    dif_functors(HeadIs,Convert),
    % Get an inline definition for Convert.
    get_inline_def(Convert,InlineDef),
    !,
    % Convert the inline definition using f2p.
    must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,InlineDef,Converted))).
% Handle the case where Convert is '~ not(AsPred)' and convert it to \+ eval_true(AsPred).
f2q(Depth,HeadIs,RetType,RetResult,Convert, \+ eval_true(AsPredO)) :-
    % Pattern match for not(AsPred) in Convert.
    '=~'(Convert, (not(AsPredI))),
    % Convert the AsPredI term using f2p.
    must_det_ll(f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO)).

%!  get_first_p1(+E, +Cmpd, -Result) is nondet.
%
%   Retrieves the first occurrence of element `E` from the compound term `Cmpd` and provides
%   a description of how to access it (`Result`). This predicate handles both lists and 
%   general compound terms. It fails if `E` is not found in `Cmpd`.
%
%   @arg E      The element to find within the compound term.
%   @arg Cmpd   The compound term or list to search through.
%   @arg Result A term describing how to access the element, such as `set_nth1/3` for lists or
%               `set_arg/3` for compound terms.
%
%   @example
%     % Search in a list:
%     ?- get_first_p1(a, [a,b,c], Result).
%     Result = set_nth1(1, [a, b, c]).
%
%     % Search in a compound term:
%     ?- get_first_p1(a, f(a,b,c), Result).
%     Result = set_arg(1, f(a, b, c)).
%
get_first_p1(_, Cmpd, _) :- 
    % Fail if Cmpd is not a compound term.
    \+ compound(Cmpd), !, fail.
% Handle the case where Cmpd is a list, and E is found as an element.
get_first_p1(E, Cmpd, set_nth1(N1, Cmpd)) :- 
    % Check if Cmpd is a list and find E at position N1.
    is_list(Cmpd),nth1(N1, Cmpd, E).
% Handle the case where Cmpd is a list, and we need to recursively search its elements.
get_first_p1(E, Cmpd, Result) :- 
    % If Cmpd is a list, iterate through its elements.
    is_list(Cmpd), !, member(Ele, Cmpd), 
    % Recursively search within each element.
    get_first_p1(E, Ele, Result).
% Fail if Cmpd satisfies the `is_conz/1` condition.
get_first_p1(_, Cmpd, _) :- 
    % If Cmpd satisfies is_conz/1, fail.
    is_conz(Cmpd), !, fail.
% Handle the case where Cmpd is a compound term, and E is found as an argument.
get_first_p1(E, Cmpd, set_arg(N1, Cmpd)) :- 
    % Find E as the N1-th argument in the compound term Cmpd.
    arg(N1, Cmpd, E).
% Recursively search the arguments of a compound term.
get_first_p1(E, Cmpd, Result) :- 
    % Recursively search within the arguments of Cmpd.
    arg(_, Cmpd, Ele), !, get_first_p1(E, Ele, Result).

%!  non_simple_arg(+E) is nondet.
%
%   Succeeds if `E` is a compound term but not a free-term variable.
%   This is used to distinguish between simple arguments and more complex terms.
%
%   @arg E The term to check.
%
%   @example
%     ?- non_simple_arg(f(a)).
%     true.
%     ?- non_simple_arg(X).
%     false.
%
non_simple_arg(E):- 
    % Succeeds if E is a compound and not a free-term variable.
    compound(E), !, \+ is_ftVar(E).

%%%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Converting, -(PreArgs, Converted)) is nondet.
%
%   This predicate attempts to convert a functor into another representation while 
%   maintaining its arguments and structure, and verifying type compatibility. 
%   The goal fails if there are non-simple arguments that cannot be handled.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Converting   The functor being processed and converted.
%   @arg PreArgs      Pre-existing arguments from previous conversions.
%   @arg Converted    The resulting functor after conversion with new arguments.
%
%   @example
%     % Example usage of f2q predicate
%     ?- f2q(1, head, int, RetResult, some_functor(a, b), ([], Converted)).
%     Converted = some_functor(a, b).
%
f2q(Depth, HeadIs, RetType, RetResult, Converting, (PreArgs, Converted)) :-
    % Initial failure condition that may indicate incomplete or specialized code.
    fail,
    % Extract the functor name, arity, and arguments from the term being converted.
    as_functor_args(Converting, F, A, Args),
    % Check if there is at least one non-simple argument.
    % This double negation ensures the condition holds without immediate failure.
    \+ \+ (member(E, Args), non_simple_arg(E)),
    % Create a variable named 'Self' for further usage in type fitting.
    cname_var('Self', Self),
    % Map over the arguments to verify type compatibility and generate new arguments.
    maplist(type_fit_childs('=', Depth, Self), _RetTypes1, ArgsCode, Args, NewArgs),
    % Combine any generated argument code with pre-existing arguments.
    combine_code(ArgsCode, PreArgs),
    % Logging for debugging type fitting (commented out as no-op).
    nop(non_compat_io(color_g_mesg('magenta', (
        (write_src(type_fit_childs('=', Depth, F, _RetTypes2, PreArgs, Args, NewArgs)), nl)
    )))),
    % Rebuild the functor with the new arguments.
    as_functor_args(Convert, F, A, NewArgs),
    % Ensure that none of the new arguments are non-simple, enforcing compatibility.
    \+ (member(E, NewArgs), non_simple_arg(E)), !,
    % Proceed to the next phase of conversion using f2p.
    f2p(Depth, HeadIs, RetType, RetResult, Convert, Converted).

 /*
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ if(Cond,Then),!,
   f2p(Depth,HeadIs,RetType,CondResult,Cond,CondCode),
   f2p(Depth,HeadIs,RetType,RetResult,Then,ThenCode),
   Converted = ((CondCode,is_True(CondResult)),ThenCode).

f2q(Depth,HeadIs,RetType,RetResult,Converter, Converted):-
   de_eval(Converter,Convert),!,
   f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted).

f2q(Depth,HeadIs,RetType,_Result,Convert, Converted)
  :- fail,
  as_functor_args(Convert,Func,PA),
   functional_predicate_arg(Func,PA,Nth),
   Convert =~ [Func|PredArgs],
   nth1(Nth,PredArgs,Result,FuncArgs),
   RetResult = Result,
   AsFunct =~ [Func|FuncArgs],
  f2p(Depth,HeadIs,RetType,RetResult,AsFunct, Converted).

  */

%   f2q(_Depth,_HeadIs,_RetType, _RetVal, Convert, Convert) :- compound(Convert), (Convert= (_,_)),!.

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   This predicate handles various cases of functor conversion and code transformation.
%   The conversion aims to match specific patterns and produce corresponding code, handling
%   non-evaluatable terms, bindings, and specific operations like `println!`.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Convert      The term or functor being processed and converted.
%   @arg Converted    The resulting code or term after conversion.
%
%   @example
%     % Example usage of f2q for a functor conversion with addition
%     ?- f2q(1, head, int, RetResult, 'println!'(msg), Converted).
%     Converted = (eval(['println!', ValueResult], RetResult)).
%

f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :- 
    % Initial failure condition that may indicate incomplete or specialized code.
    fail,
    % Ensure functor extraction for both the current term and new quoted arguments.
    must_det_ll((
        as_functor_args(Convert, F, A, Args),
        as_functor_args(Quot, quot, A, NewArgs),
        as_functor_args(QConvert, quot, A, Args)
    )),
    % Retrieve the inline case list for further processing.
    get_inline_case_list([F | NewArgs], Quot, DefList), !,
    % Proceed with further processing using f2p for the case list.
    must_det_ll((
        f2p(Depth, HeadIs, RetType, RetResult, case(QConvert, DefList), Converted)
    )).

%!  is_non_evaluatable(+S) is nondet.
%
%   Determines whether a given term is non-evaluatable.
%   This predicate checks if a term is a variable, non-compound, or has non-evaluatable components.
%
%   @arg S The term to be checked.
is_non_evaluatable(S) :- 
    % Simple case: non-compound terms are non-evaluatable.
    \+ compound(S), !.
is_non_evaluatable(S) :- 
    % Check if the term is a free variable (ftVar).
    is_ftVar(S), !.
is_non_evaluatable([H|_]) :- 
    % Check if the first element is neither a symbol nor non-evaluatable.
    \+ symbol(H), \+ is_non_evaluatable(H).

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles the conversion of non-evaluatable terms into a call to `call_why/2`.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, Converted) :-
    % This branch deals with non-evaluatable terms.
    fail, 
    is_non_evaluatable(Convert),
    Converted = call_why(non_eval, Convert = RetResult), !.

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles 'bind!' expressions where the second argument is a variable.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, Converted) :-
    % Match 'bind!' expressions with a variable in the second position.
    Convert =~ 'bind!'(Var, Value),
    is_ftVar(Value), !,
    Converted = u_assign8('bind!'(Var, Value), RetResult).
%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles 'bind!' expressions where the second argument is 'new-space'.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, Converted) :-
    Convert =~ 'bind!'(Var, Value),
    Value =~ 'new-space'(), !,
    Converted = eval('bind!'(Var, Value), RetResult).
%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles general 'bind!' expressions and recursively processes the second argument.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ 'bind!'(Var, Value), !,
    f2p(Depth, HeadIs, RetType, ValueResult, Value, ValueCode),
    Eval = eval_args(['bind!', Var, ValueResult], RetResult),
    combine_code(ValueCode, Eval, Converted).

%!  returns_empty(+Function) is nondet.
%
%   Checks if the provided function is one that returns an empty result.
returns_empty('add-atom').
returns_empty('remove-atom').

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles conversion of specific functions like 'add-atom' and 'remove-atom' which return empty results.
f2q(_Depth, _HeadIs, _RetType, RetResult, Convert, Converted) :-
    (Convert =~ [EmptyResultFunction, Where, What, RetResult] ;
     Convert =~ [EmptyResultFunction, Where, What]),
    nonvar(EmptyResultFunction),
    returns_empty(EmptyResultFunction),
    current_predicate(EmptyResultFunction/2),
    =(What, WhatP), !,
    Converted = as_nop(call(EmptyResultFunction, Where, WhatP), RetResult).
%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles 'println!' expressions by generating evaluation code for the result.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    Convert =~ ['println!', Value], !,
    Converted = (ValueCode, eval(['println!', ValueResult], RetResult)),
    f2p(Depth, HeadIs, RetType, ValueResult, Value, ValueCode).
%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles arithmetic expressions like addition where the second argument is not a number or a variable.
f2q(Depth, HeadIs, RetType, RetResult, Convert, CodeForValueConverted) :- 
    fail,
    Convert =~ [Plus, N, Value], 
    symbol(Plus), 
    current_predicate(Plus/3), 
    number(N),
    \+ number(Value), \+ is_nsVar(Value), !,
    f2p(Depth, HeadIs, RetType, ValueResult, Value, CodeForValue), !,
    Converted =.. [Plus, N, ValueResult, RetResult],
    combine_code(CodeForValue, Converted, CodeForValueConverted).

/*
% match(Space,f(1)=Y,Y)
f2q(Depth,HeadIs,RetType,Y,Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert=~ match(Space,AsFunctionY,YY),
    nonvar(AsFunctionY),( AsFunctionY =~ (AsFunction=Y)), nonvar(AsFunction),
    !, Y==YY,
    f2p(Depth,HeadIs,RetType,Y,AsFunction,Converted),!.
*/

%!  metta_atom_iter(+Space, -Match) is nondet.
%
%   Iterates over atoms in the given Space, matching each one to the provided Match.
%   This predicate delegates to `metta_atom_iter/5`, setting a default equality 
%   operator (`=`) and recursion depth (10).
%
%   @arg Space The space in which the atoms reside.
%   @arg Match The pattern or value to match against.
metta_atom_iter(Space, Match) :- metta_atom_iter('=', 10, Space, Space, Match).

%!  make_with_space(+Space, +MatchCode, -ResultCode) is det.
%
%   Ensures that a functor is wrapped with `with_space/2` unless the Space is `&self`.
%   If the Space is `&self`, the MatchCode is returned unchanged.
%
%   @arg Space The space in which the functor operates.
%   @arg MatchCode The code to be wrapped or returned.
%   @arg ResultCode The final code, potentially wrapped in `with_space/2`.
make_with_space(Space, MatchCode, MatchCode) :- Space == '&self', !.
make_with_space(Space, MatchCode, with_space(Space, MatchCode)) :- Space \== '&self'.

%%%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +(A =~ B), -(A =~ B)) is det.
%
%   If `Convert` is a simple `=~` comparison, this is treated as a direct match
%   and no further conversion or processing is necessary.
%
%   @arg Depth       The current depth of recursion or transformation.
%   @arg HeadIs      Describes the head of the predicate for type conversion.
%   @arg RetType     The expected return type of the functor.
%   @arg RetResult   The result of the conversion after processing.
%   @arg A =~ B      The comparison to be evaluated.
f2q(_Depth, _HeadIs, _RetType, _RetResult, (A =~ B), (A =~ B)) :- !.

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +EvalConvert, -Converted) is nondet.
%
%   Converts an `eval` functor into its internal representation. The actual 
%   conversion logic for the functor is handled by a recursive call to `f2p/6`.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg EvalConvert  The functor to be evaluated.
%   @arg Converted    The resulting code after evaluation.
f2q(Depth, HeadIs, RetType, RetResult, EvalConvert, Converted) :-
    EvalConvert =~ eval(Convert), !,
    must_det_ll((
        f2p(Depth, HeadIs, RetType, RetResult, Convert, Converted)
    )).

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Converts `u_assign` functors where the first argument is compound. 
%   The conversion flattens the first argument into a list of arguments.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Convert      The `u_assign` functor to be converted.
%   @arg Converted    The resulting code after conversion.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    fail,compound(Convert),Convert = u_assign(C, Var),compound_non_cons(C),into_list_args(C,CC),!,
    f2p(Depth, HeadIs, RetType, RetResult, u_assign(CC, Var), Converted).

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles `u_assign` functors where the first argument is a list.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Convert      The `u_assign` functor to be converted.
%   @arg Converted    The resulting code after conversion.
f2q(_Depth, _HeadIs, _RetType, _RetResult, Convert, Converted) :-
    fail,compound(Convert),Convert = u_assign(C, _Var),is_list(C),Converted = Convert, !.

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Converts simple symbols into canonical forms. This process converts functional 
%   predicates based on their argument positions.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Convert      The symbol to be processed.
%   @arg Converted    The resulting code after conversion.
f2q(_Depth, HeadIs, _RetType, RetResult, Convert, Converted) :- 
    fail,
    symbol(Convert), 
    functional_predicate_arg(Convert, Nth, Nth2),Nth == 1, Nth2 == 1,HeadIs \=@= Convert,Convert = F, !,
    must_det_ll((
        do_predicate_function_canonical(FP, F),
        compound_name_list(Converted, FP, [RetResult])
    )).

%%%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +(is(Convert)), -(Converted, is(RetResult, Result))) is nondet.
%
%   Converts `is` expressions into the equivalent `is` predicate in Prolog.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Convert      The term to be evaluated by `is`.
%   @arg Converted    The resulting code after conversion.
f2q(Depth, HeadIs, RetType, RetResult, is(Convert), (Converted, is(RetResult, Result))) :- 
    !,
    must_det_ll((
        f2p(Depth, HeadIs, RetType, Result, Convert, Converted)
    )).

%!  into_equals(+Eval, +Result, -Code) is det.
%
%   Converts an evaluation term into an `equals` statement, delegating to 
%   `into_u_assign/3` when necessary.
%
%   @arg Eval     The term being evaluated.
%   @arg Result   The result of the evaluation.
%   @arg Code     The resulting code after conversion.
into_equals(Eval, Result, Code) :-
    into_u_assign(Eval, Result, Code).

%!  into_u_assign(+Eval, +Result, -Code) is det.
%
%   Converts an evaluation term into a `u_assign` statement, with special handling for 
%   variables and constants.
%
%   @arg Eval     The term being evaluated.
%   @arg Result   The result of the evaluation.
%   @arg Code     The resulting code after conversion.
into_u_assign(Eval, Result, true) :- is_nsVar(Eval),is_nsVar(Result), Eval = Result, !.
into_u_assign(Eval, Result, Code) :- Result == 'True', !,f2p(Eval, _Result, Code).
into_u_assign(Eval, Result, Code) :- var(Eval), \+ var(Result), !,into_u_assign(Result, Eval, Code).
into_u_assign(Eval, Result, Code) :- f2p(Eval, Result, Code), !.
into_u_assign(Eval, Result, Code) :- Code = u_assign5(Eval, Result).

% check if this is a flow control operation
%f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted):-
%  compound(Convert), \+ compound_name_arity(Convert,_,0),
%  f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted),!.
%!  f2q(+Depth, +HeadIs, +RetType, +RetResultL, +ConvertL, -Converted) is nondet.
%
%   Handles the conversion of a list containing a single sublist. The sublist is processed
%   by `f2p/6`, and the result is converted into an `equals` statement.
%
%   @arg Depth       The current depth of recursion or transformation.
%   @arg HeadIs      Describes the head of the predicate for type conversion.
%   @arg RetType     The expected return type of the functor.
%   @arg RetResultL  The result of the conversion for the outer list.
%   @arg ConvertL    The list containing the functor to be processed.
%   @arg Converted   The resulting code after conversion.
f2q(Depth, HeadIs, RetType, RetResultL, ConvertL, Converted) :- 
    is_list(ConvertL),
    ConvertL = [Convert],  % Check if the list contains a single element (sublist).
    is_list(Convert),      % Ensure the single element is itself a list.
    f2p(Depth, HeadIs, RetType, RetResult, Convert, Code), !,  % Process the sublist.
    into_equals(RetResultL, [RetResult], Equals),             % Create an equals statement for the result.
    combine_code(Code, Equals, Converted).

%%%!  f2q(+Depth, +HeadIs, +RetType, +ResultVar, +'cdr-atom'(Atom), +'cdr-atom'(Atom, ResultVar)) is det.
%
%   Handles `cdr-atom` conversion, generating the appropriate Prolog structure with `cdr-atom` functor.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg ResultVar    The result variable to unify with the `cdr-atom` result.
%   @arg Atom         The atom whose `cdr` is being accessed.
f2q(_Depth, _HeadIs, _RetType, ResultVar, 'cdr-atom'(Atom), 'cdr-atom'(Atom, ResultVar)) :- !.

%%%!  f2q(+Depth, +HeadIs, +RetType, +ResultVar, +'car-atom'(Atom), +'car-atom'(Atom, ResultVar)) is det.
%
%   Handles `car-atom` conversion, generating the appropriate Prolog structure with `car-atom` functor.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg ResultVar    The result variable to unify with the `car-atom` result.
%   @arg Atom         The atom whose `car` is being accessed.
f2q(_Depth, _HeadIs, _RetType, ResultVar, 'car-atom'(Atom), 'car-atom'(Atom, ResultVar)) :- !.

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is nondet.
%
%   Handles the conversion of a list into a predicate form, then proceeds with converting
%   the list representation into predicate form using `f2p/6`.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Convert      The list to be converted.
%   @arg Converted    The resulting code after conversion.
f2q(Depth, HeadIs, RetType, RetResult, Convert, Converted) :-
    fail, 
    is_list(Convert),
    once((sexpr_s2p(Convert, IS), \+ IS =@= Convert)), !,  % Convert the list if not already in predicate form.
    must_det_ll((
        f2p(Depth, HeadIs, RetType, RetResult, IS, Converted)
    )).  % Proceed with the conversion using the predicate form.

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +ConvertL, -Converted) is nondet.
%
%   Handles the conversion of a list of functors by recursively processing each functor with `f2p_assign/6`.
%   The results are combined and finally assigned to the result using `u_assign`.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg ConvertL     The list of functors to be processed.
%   @arg Converted    The resulting code after conversion.
f2q(Depth, HeadIs, RetType, RetResult, ConvertL, Converted) :-
    fail, 
    is_list(ConvertL),
    maplist(f2p_assign(Depth, HeadIs, RetType), RetResultL, ConvertL, ConvertedL),  % Recursively process the list of functors.
    combine_code(ConvertedL, Conjs),  % Combine the resulting code.
    into_u_assign(RetResultL, RetResult, Code),  % Create the final assignment code.
    combine_code(Conjs, Code, Converted).  % Combine the assignment with the code.

/* MAYBE USE ?
% If Convert is a compound term, we need to recursively convert its arguments.
f2q(Depth,HeadIs,RetType,RetResult, Convert, Converted) :- fail,
    compound(Convert), !,
    Convert =~ [Functor|Args],  % Deconstruct Convert to as_functor_args and arguments
    maplist(convert_argument, Args, ConvertedArgs),  % Recursively convert each argument
    Converted =~ [Functor|ConvertedArgs],  % Reconstruct Converted with the converted arguments
    (callable(Converted) -> f2p(Depth,HeadIs,RetType,RetResult, Converted, _); true).  % If Converted is callable, proceed with its conversion
% Helper predicate to convert an argument of a compound term
convert_argument(Arg, ConvertedArg) :-
    (callable(Arg) -> ftp(_, _, Arg, ConvertedArg); ConvertedArg = Arg).
*/

% convert Funtion
% f2q(Depth,HeadIs,RetType,ResultVar,Convert, Converted) :- h2p(Convert, ResultVar, Converted).


/*
f2q(Depth,_HeadIs,_RetType,RetResult,AsPred,Converted):-
   compound(AsPred),
   as_functor_args(AsPred,F,A,Args),
   no_lists(Args),
   always_predicate_in_src(F,A),
   was_predicate(AsPred,RetResult,Converted).

f2q(Depth,_HeadIs,_RetType,RetResult,AsPred,Converted):-
   compound(AsPred),
   as_functor_args(AsPred,F,A,Args),
   no_lists(Args),
   always_function_in_src(F,A),
   was_predicate(AsPred,RetResult,Converted).
*/

%!  f2q(+Depth, +HeadIs, +RetType, +RetResult, +Convert, -Converted) is det.
%
%   Converts a `u_assign` expression into a custom `u_assignA` form for further processing.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetResult    The result of the conversion after processing.
%   @arg Convert      The `u_assign` term to be converted.
%   @arg Converted    The resulting code after conversion.
f2q(_Depth, _HeadIs, _RetType, _RetResult, u_assign(Convert, Res), u_assignA(Convert, Res)) :- !.

%!  f2q(+Depth, +HeadIs, +RetType, +RetVar, +Data, -CodeOut) is nondet.
%
%   Converts a functor `Data` by extracting its arguments, assigning types, and generating
%   the appropriate `eval_for` structure. This version checks the operator type definition
%   and assigns types based on the self context.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetVar       The result variable for the conversion.
%   @arg Data         The functor data to be processed.
%   @arg CodeOut      The resulting code after conversion.
f2q(Depth, _HeadIs, RetType, RetVar, Data, CodeOut) :-
    % Extract the functor name and arguments from the term.
    as_functor_args(Data, F, A, Args),
    current_self(Self),  % Get the current context (Self).
    length(NewArgs, A),  % Create a list of new arguments with the same arity.
    length(ParamTypes, A),  % Create a list of parameter types.
    % Ensure the operator and type assignment are valid.
    most_true([get_operator_typedef(Self, F, ParamTypes, RetTypeF), can_assign(RetTypeF, RetType)]),
    % Debug output for specific function.
    if_t(F == (fL), println(Data)),
    % Narrow down the types to a more specific one.
    narrow_types(RetTypeF, RetType, NarrowType),
    % Prepare the call for the functor.
    Call = [F | NewArgs],
    % Append parameter types and the return type.
    append(ParamTypes, [RetType | _], ParamTypesO),
    % Generate the evaluation code for the arguments.
    into_eval_for_l(Depth, Call, Self, F, 1, ParamTypesO, Args, NewArgs, ParamCode),
    % Combine the parameter code with the final eval_for call.
    combine_code(ParamCode, eval_for(b_6, NarrowType, Call, RetVar), CodeOut).

%%%!  f2q(+Depth, +HeadIs, +RetType, +RetVar, +Data, -eval_for(b_8, RetType, Data, RetVar)) is det.
%
%   Converts `Data` directly into an `eval_for(b_8)` structure.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg RetType      The expected return type of the functor.
%   @arg RetVar       The result variable for the conversion.
%   @arg Data         The functor data to be processed.
%   @arg CodeOut      The resulting eval_for structure.
f2q(_Depth, _HeadIs, RetType, RetVar, Data, eval_for(b_8, RetType, Data, RetVar)).

%!  most_true(+Conditions) is nondet.
%
%   Checks if at least one condition in the list is true, succeeding as soon as
%   one succeeds.
%
%   @arg Conditions The list of conditions to check.
most_true([]) :- !.
most_true([A | List]) :- call(A), !, most_true(List).
most_true([A | List]):- most_true(List),ignore(A).

%!  into_eval_for_l(+Depth, +HeadIs, +Self, +F, +Nth, +ParamTypes, +Args, +NewArgs, -CCode) is det.
%
%   Processes a list of arguments recursively, generating code for each argument
%   using `into_eval_for/8`.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg Self         The current context (Self).
%   @arg F            The functor name being processed.
%   @arg Nth          The current argument index.
%   @arg ParamTypes   The list of parameter types.
%   @arg Args         The list of arguments to process.
%   @arg NewArgs      The resulting list of new arguments.
%   @arg CCode        The combined code after processing.
into_eval_for_l(Depth, HeadIs, Self, F, Nth, [PT | ParamTypes], [A | Args], [N | NewArgs], CCode) :- !,
    % Generate evaluation code for the current argument.
    into_eval_for(Depth, HeadIs, Self, F, Nth, PT, A, N, C),
    % Increment the argument index.
    Nth1 is Nth + 1,
    % Recursively process the remaining arguments.
    into_eval_for_l(Depth, HeadIs, Self, F, Nth1, ParamTypes, Args, NewArgs, Code),
    % Combine the generated code.
    combine_code(C, Code, CCode).
into_eval_for_l(_Depth,_HeadIs,_Slf,_F,_Nth,[],Args,Args,true).
into_eval_for_l(_Depth,_HeadIs,_Slf,_F,_Nth,_ParamTypes,[],[],true).

%!  into_eval_for(+Depth, +HeadIs, +Self, +F, +Nth, +PT, +A, -N, -CodeOut) is det.
%
%   Generates evaluation code for individual arguments, handling numbers, variables,
%   and functors with various parameter types.
%
%   @arg Depth        The current depth of recursion or transformation.
%   @arg HeadIs       Describes the head of the predicate for type conversion.
%   @arg Self         The current context (Self).
%   @arg F            The functor name being processed.
%   @arg Nth          The current argument index.
%   @arg PT           The parameter type for the current argument.
%   @arg A            The argument to process.
%   @arg N            The resulting argument after processing.
%   @arg CodeOut      The code generated for this argument.
into_eval_for(_Depth, _HeadIs, _Slf, _F, _Nth, PT, A, A, true) :- 
    number(A), !, ignore(PT = 'Number').
into_eval_for(_Depth, _HeadIs, _Slf, _F, _Nth, PT, A, N, eval_for(b_5, PT, A, N)) :- 
    nonvar(PT), PT \== 'Atom', var(A), !.
into_eval_for(_Depth, _HeadIs, _Slf, F, Nth, PT, A, N, eval_for(b_4(Nth, F), PT, A, N)) :- 
    var(PT), var(A), !.
into_eval_for(Depth, HeadIs, _Slf, _F, _Nth, RetType, Arg, NewArg, CodeOut) :- 
    is_list(Arg),f2p(Depth, HeadIs, RetType, NewArg, Arg, CodeOut), !.
into_eval_for(Depth, HeadIs, Slf, F, Nth, RetType, Arg, NewArgO, CodeOut) :-
    compound(Arg),as_functor_args(Arg, AF, _A, Args),Compile = [AF | Args], !,
    into_eval_for(Depth, HeadIs, Slf, F, Nth, RetType, Compile, NewArgO, CodeOut), !.
into_eval_for(_Depth, _HeadIs, _Slf, _F, _Nth, PT, A, N, eval_for(b_3, PT, A, N)) :- 
    var(PT), get_type(A, PT), nonvar(PT), !.
into_eval_for(_Depth, _HeadIs, _Slf, F, Nth, PT, A, N, eval_for(b_2(Nth, F), PT, A, N)) :- 
    var(PT), !.
into_eval_for(_Depth, _HeadIs, _Slf, _F, _Nth, PT, A, N, eval_for(b_1, PT, A, N)) :- 
    nonvar(PT), PT \== 'Atom', !.
into_eval_for(_Depth, _HeadIs, _Slf, _F, _Nth, _PT, A, A, true).
