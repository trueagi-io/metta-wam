/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter/Runtime
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

%*********************************************************************************************
% PROGRAM FUNCTION: Provides Prolog predicates and rules for function type checking
% of MeTTa expressions and data structures.
%*********************************************************************************************


:- discontiguous default_isa/2.
:- discontiguous desc_aka/2.
:- discontiguous explicit_isa/2.
:- discontiguous subtype/2.

% MismatchBehavior
% Handles type mismatches or argument mismatches during evaluation.
% MismatchOriginal = return_original_on_mismatch.
% MismatchError = throw_type_error_on_mismatch.
% MismatchFail = fail_on_mismatch.

subtype('MismatchEnum', 'FunctionTypeEnum').
desc_aka('MismatchOriginal', return_original_on_mismatch).
desc_aka('MismatchError', throw_type_error_on_mismatch).
desc_aka('MismatchFail', fail_on_mismatch).
default_isa('MismatchOriginal', 'MismatchEnum').
explicit_isa('MismatchOriginal', 'MismatchEnum').
explicit_isa('MismatchError', 'MismatchEnum').
explicit_isa('MismatchFail', 'MismatchEnum').

% NoMatchBehavior
% Handles cases where the head of a clause fails to match the call.
% NoMatchFail = fail_on_head_mismatch.
% NoMatchOriginal = return_original_on_head_mismatch.

subtype('NoMatchEnum', 'FunctionTypeEnum').
default_isa('NoMatchFail', 'NoMatchEnum').
desc_aka('NoMatchFail', fail_on_head_mismatch).
desc_aka('NoMatchOriginal', return_original_on_head_mismatch).
explicit_isa('NoMatchFail', 'NoMatchEnum').
explicit_isa('NoMatchOriginal', 'NoMatchEnum').

% EvaluationOrder
% Determines the priority order during evaluation.
% OrderClause = clause_order_priority.
% OrderFittest = fittest_first_priority.

subtype('EvaluationOrderEnum', 'FunctionTypeEnum').
default_isa('OrderClause', 'EvaluationOrderEnum').
desc_aka('OrderClause', clause_order_priority).
desc_aka('OrderFittest', fittest_first_priority).
explicit_isa('OrderClause', 'EvaluationOrderEnum').
explicit_isa('OrderFittest', 'EvaluationOrderEnum').

% FunctionResultBehavior
% Determines the outcome after successful evaluation.
% Nondeterministic = continue_on_success.
% Deterministic = cut_on_first_success.

subtype('FunctionResultEnum', 'FunctionTypeEnum').
default_isa('Nondeterministic', 'FunctionResultEnum').
desc_aka('Nondeterministic', continue_on_success).
desc_aka('Deterministic', cut_on_first_success).
explicit_isa('Nondeterministic', 'FunctionResultEnum').
explicit_isa('Deterministic', 'FunctionResultEnum').

% OnClauseFailed
% Handles behavior when a clause fails to match.
% ClauseFailNonDet = continue_on_failure.
% ClauseFailDet = cut_on_first_failure.

subtype('ClauseFailedEnum', 'FunctionTypeEnum').
default_isa('ClauseFailNonDet', 'ClauseFailedEnum').
desc_aka('ClauseFailNonDet', continue_on_failure).
desc_aka('ClauseFailDet', cut_on_first_failure).
explicit_isa('ClauseFailNonDet', 'ClauseFailedEnum').
explicit_isa('ClauseFailDet', 'ClauseFailedEnum').

% OutOfClausesBehavior
% Handles behavior when no clauses remain for evaluation.
% FailureEmpty = fail_on_final_failure.
% FailureOriginal = return_original_on_final_failure.

subtype('OutOfClausesEnum', 'Enum').
default_isa('FailureEmpty', 'OutOfClausesEnum').
desc_aka('FailureEmpty', fail_on_final_failure).
desc_aka('FailureOriginal', return_original_on_final_failure).
explicit_isa('FailureEmpty', 'OutOfClausesEnum').
explicit_isa('FailureOriginal', 'OutOfClausesEnum').


% predicate_behavior(Op, Len, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior)
predicate_behavior(Op, Len, MisMatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior) :-
    predicate_behavior_impl(Op, Len, MisMatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior)
    *-> true ; predicate_behavior_fallback(Op, Len, MisMatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior).

% default
predicate_behavior_fallback(_, _, 'MismatchOriginal', 'NoMatchOriginal', 'OrderClause', 'Nondeterministic', 'ClauseFailNonDet', 'FailureOriginal').

predicate_behavior_impl('get-type', 1, 'MismatchFail', 'NoMatchFail', 'OrderClause', 'Nondeterministic', 'ClauseFailNonDet', 'FailureEmpty').
predicate_behavior_impl('foo', 2, 'MismatchOriginal', 'NoMatchOriginal', 'OrderFittest', 'Nondeterministic', 'ClauseFailNonDet', 'FailureOriginal').
predicate_behavior_impl('match', 4, 'MismatchFail', 'NoMatchFail', 'OrderClause', 'Nondeterministic', 'ClauseFailNonDet', 'FailureEmpty').


get_ftype(Eq,RetType,Depth,Self,Val,TypeO):-
  if_or_else(get_ftype_decl(Eq,RetType,Depth,Self,Val,TypeO),get_ftype_fallback(Eq,RetType,Depth,Self,Val,TypeO)).

get_ftype_decl(_Eq,_RetType,Depth,Self,Val,TypeO):-
    get_type(Depth,Self,Val,TypeO),is_list(TypeO),[Type|_]=TypeO,Type=='->'.

get_ftype_fallback(_Eq,_Type,_Depth,Self,[Op|Args],TypeO):- nonvar(Op), len_or_unbound(Args,Len),!,get_operator_ftypedef(Self, Op, Len, TypeO).
get_ftype_fallback(_Eq,_Type,_Depth,Self,Op,TypeO):- get_operator_ftypedef(Self, Op, _Len, TypeO).

op_farity(Op,Len):- no_repeats_var(Len), if_or_else(op_farity_decl(Op,Len),op_farity_fallback(Op,Len)).

op_farity_decl(Op,Len):- metta_defn(_Self, [Op | Args], _Body), len_or_unbound(Args,Len).
op_farity_decl(Op,Len):- metta_type(_Self, Op, [Ar,_|Types]), Ar=='->', len_or_unbound(Types,Len).
op_farity_fallback(_Op,Len):- between(0,8,Len).

get_operator_ftypedef(Self, Op, Len, TypeO):- (var(Len)->op_farity(Op,Len);true),
   get_operator_typedef(Self, Op, Len, ParamTypes, RetType), append(['->'|ParamTypes],[RetType],TypeO).

/*
metta_defn('&self', ['double-it', 'Z'], 'Z').
metta_defn('&self', ['double-it', ['S', X], ['S', ['S', ['double-it', X]]]).
get_operator_typedef('&self', 'double-it', 1, ['List'], 'List').

-->

function_declaration('double-it', 1, ['Z'], ['List'], 'List', ['let', ReturnVal, 'Z', ReturnVal], 'Z').
function_declaration('double-it', 1, [['S', X]], ['List'], 'List', ['let', ReturnVal, ['S', ['S', ['double-it', X]]], ReturnVal], ['S', ['S', ['double-it', X]]]).

*/

fake_body([Op | Parameters], [Op | Parameters]).


metta_defn_return(Self, Original, Body, DeclBody, ReturnVal):-
  if_or_else(metta_defn_decl(Self,Original, Body, DeclBody, ReturnVal),metta_defn_fallback(Self, Original, Body, DeclBody, ReturnVal)).

metta_defn_decl(Self, [Op | Parameters], Body, [let, ReturnVal, Body, ReturnVal], ReturnVal):- metta_defn(Self, [Op | Parameters], Body).
metta_defn_fallback(_Self, [Op | Parameters], Body, Body, ReturnVal):-
   Body = [let, [quote,ReturnVal], [quote,[Op | Parameters]], ReturnVal].


function_declaration(Op, Len, Parameters, ParamTypes, RetType, DeclBody, ReturnVal) :-
    Self = '&self',
    len_or_unbound(Parameters, Len),
    metta_defn_return(Self, [Op | Parameters], Body, DeclBody, ReturnVal),
    len_or_unbound(Parameters, Len),
    NR = ([Op | Parameters] + Body),
    copy_term(NR, NRR),
    no_repeats_var(NRR),
    get_operator_typedef_near(Self, Op, Len, ParamTypes, RetType, [Op | Parameters], Body),
    NR = NRR,
    nop(write_src_nl(metta_defn(Self, [Op | Parameters], Body))).

get_operator_typedef_near(Self, Op, Len, ParamTypes, RetType, Head, Body):-
    src_data_ordinal([=,Head,Body],FileLineClause),
    length(ParamTypes,Len),
    findall(FileLineType-pr(ParamTypes, RetType),(src_data_ordinal([:,Op,[Ar|Type]],FileLineType),Ar=='->',append(ParamTypes,[RetType],Type)),LocTypes),
    get_operator_typedef_near(Self, Op, Len, ParamTypes, RetType, Head, Body, FileLineClause,LocTypes).

src_data_ordinal(Data,Ordinal):-
    user:metta_file_buffer(0,Ordinal,_TypeNameCompound,Data,_NamedVarsListC,_Context,_Range).

get_operator_typedef_near(_Self, _Op, _Len, ParamTypes, RetType, _Head, _Body, FileLineClause,LocTypes):-
    select(FileLineType-pr(ParamTypes, RetType),LocTypes,Rest),
    Space is FileLineType-FileLineClause,
    Space<0,
    \+ (member(FileLineTypeOther-_,Rest),
       SpaceOther is FileLineTypeOther-FileLineClause,
       SpaceOther<0, SpaceOther>Space),!.

get_operator_typedef_near(_Self, _Op, _Len, ParamTypes, RetType, _Head, _Body, FileLineClause,LocTypes):-
    select(FileLineType-pr(ParamTypes, RetType),LocTypes,Rest),
    Space is FileLineType-FileLineClause,
    Space>0,
    \+ (member(FileLineTypeOther-_,Rest),
       SpaceOther is FileLineTypeOther-FileLineClause,
       SpaceOther>0, SpaceOther<Space),!.

finfo([Op|Args]):- is_list(Args),!,length(Args,Len),finfo(Op, Len).
finfo(Op):- atomic(Op), !, finfo(Op, _, _).
finfo(Op, Len):- finfo(Op, Len, _).

finfo(Op, Len, Head) :-
    % length(Parameters, Len),
    op_farity(Op,Len),
    show_pall(predicate_behavior(Op, Len, _MisMatchBehavior, _NoMatchBehavior, _EvaluationOrder, _SuccessBehavior, _FailureBehavior, _OutOfClausesBehavior)),
    length(Args,Len),
    [Op|Args] = Head,
    show_pall(get_ftype('=',_RetType1,20,'&self',Head, TypeO),get_ftype('&self',Head, TypeO)),
    %show_pall(metta_atom(_,[iz,Op,_])),
    %show_pall(metta_atom(_,[':',Op,_])),
    show_pall(function_declaration_scores(Op, Len, _Parameters, _ParamTypes, _RetType, _Body, _ReturnVal, _Scores)),
    show_pall((metta_atom(KB,[A,B|Out]),sub_var(Op,[A,B])),ist(KB,[A,B|Out])),
    true.

show_pall(Var):- \+ callable(Var),!.
show_pall(Atom):- atom(Atom),!,current_predicate(Atom/_,SHOWP),!,show_pall(SHOWP,SHOWP).
show_pall(Op/Len):- !,current_predicate(Op/Len,SHOWP),!,show_pall(SHOWP,SHOWP).
show_pall(SHOWP):- show_pall(SHOWP,SHOWP),!.

show_pall(SHOWP,Template):- current_predicate(_,SHOWP),!,
    no_repeats_var(TemplateNR),
    findall(Template, (SHOWP,TemplateNR=Template), ScoredBodies),
    maplist(write_src_nl, ScoredBodies), !.
show_pall(SHOWP,_Template):- write_src_nl(unknown(SHOWP)).

function_declaration_scores(Op, Len, Parameters, ParamTypes, RetType, Body, ReturnVal, Score + HScore):-
   function_declaration(Op, Len, Parameters, ParamTypes, RetType, Body, ReturnVal),
   score_term(ParamTypes, Score), score_term(Parameters, HScore).

score_term(Types, Score):- term_to_list(Types, XX), maplist(nc_weight, XX, XXL), sumlist(XXL, Score).

% Main Entry Point
implement_predicate([Op | Parameters], ReturnVal) :-
    % Safely execute the main logic, falling back on a default behavior if needed.
    catch(implement_predicate_nr([Op | Parameters], ReturnVal), metta_notreducable(Original), ReturnVal = Original).

% Main Logic
implement_predicate_nr([Op | Parameters], ReturnVal) :-

    Original = [Op | Parameters],

    % Determine the expected arity of the predicate
    len_or_unbound(Parameters, Len),
    % Retrieve the behavior configuration for the predicate
    predicate_behavior(Op, Len, MismatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior),

    % Validate enums dynamically using explicit_isa to ensure valid inputs
    validate_function_type_enums(MismatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior),

    % Retrieve all clauses for the predicate
    findall(thbr(ParamTypes, Params, Body, ReturnVal, RetType), function_declaration(Op, Len, Params, ParamTypes, RetType, Body, ReturnVal), Clauses),

    % Extract parameter types and group them by index across all clauses
    findall(Types, (member(thbr(Types, _, _, _, RetType), Clauses)), ParamTypesPerClause),
    group_types_by_param_index(ParamTypesPerClause, Grouped),
    convert_to_unique_sets(Grouped, GroupedParamTypes),

    % Generate a coercion table mapping parameters to their possible coerced types
    parameter_coercion_table(Parameters, GroupedParamTypes, CoercionTable),

    % Phase 1: Filter and Score Type Matching
    findall(TypeScore - (MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType),
        (member(thbr(Types, Params, Body, ReturnVal, RetType), Clauses),
         score_type_match(Types, Parameters, CoercionTable, MinimizedTypes, ReducedParams, TypeScore)),
        TypeMatchedBodies),
    % Handle type mismatch based on MismatchBehavior
    (TypeMatchedBodies \== [] -> true ;
        (MismatchBehavior == 'MismatchFail' -> (!, fail)
        ; MismatchBehavior == 'MismatchError' -> throw(metta_notreducable(['Error',Original,'Incorrect Parameter Types']))
        ; MismatchBehavior == 'MismatchOriginal' -> throw(metta_notreducable(Original)))),

    % Phase 2: Filter and Score Params Matching
    findall((TypeScore + HeadScore) - (MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType),
        (member(TypeScore - (MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType), TypeMatchedBodies),
         score_head_match(Params, Parameters, HeadScore)),
        FullyMatchedBodies),
    % Handle NoMatch Params based on NoMatchBehavior
    (FullyMatchedBodies \== [] -> true ;
        (NoMatchBehavior == 'NoMatchFail' -> (!, fail)
        ; NoMatchBehavior == 'NoMatchOriginal' -> throw(metta_notreducable(Original)))),

    % Maybe change the evaluation order
    (EvaluationOrder == 'OrderClause' ->
        OrderedBodies = FullyMatchedBodies ;
        keysort(FullyMatchedBodies, OrderedBodies)), % 'OrderFittest' sorts by score

    % Process Ordered Bodies
    (((member(_TypeScore - (_MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType), OrderedBodies), match_head(Params, ReducedParams)) *->
        (call(Body) *->
            (SuccessBehavior == 'Deterministic' -> ! ; true) % vs Nondeterministic
        ;
            (FailureBehavior == 'ClauseFailDet' -> % vs ClauseFailNonDet
                (OutOfClausesBehavior == 'FailureOriginal' -> throw(metta_notreducable([Op| ReducedParams])) ; (!, fail));
             fail)))
    *-> true ;
    (OutOfClausesBehavior == 'FailureOriginal' -> throw(metta_notreducable(Original)) ; (!, fail))).

% Group Types by Parameter Index
group_types_by_param_index([], []).
group_types_by_param_index([Types | Rest], Grouped) :-
    group_types_by_param_index(Rest, SubGrouped),
    merge_types(Types, SubGrouped, Grouped).

% Merge Types for Grouping
merge_types([], Grouped, Grouped).
merge_types([Type | RestTypes], [], [[Type] | RestGrouped]) :-
    merge_types(RestTypes, [], RestGrouped).
merge_types([Type | RestTypes], [SubList | RestGrouped], [[Type | SubList] | MergedRest]) :-
    merge_types(RestTypes, RestGrouped, MergedRest).

% Convert Lists to Unique Sets
convert_to_unique_sets([], []).
convert_to_unique_sets([List | Rest], [UniqueList | UniqueRest]) :-
    list_to_set(List, UniqueList),
    convert_to_unique_sets(Rest, UniqueRest).

% Coerce Parameter to Each Type
coerce_parameter_to_types(_, [], []).
coerce_parameter_to_types(Parameter, [Type | RestTypes], [Type-CoercedValue | RestCoercions]) :-
    % Coerce the parameter to the given type
    coerce(Parameter, Type, CoercedValue),
    coerce_parameter_to_types(Parameter, RestTypes, RestCoercions).

% Coerce Parameter List to Each Grouped Type
coerce_parameters([], [], []).
coerce_parameters([Param | RestParams], [Types | RestTypesGroups], [Param-Coercions | RestCoercions]) :-
    % Coerce each parameter based on its grouped types
    coerce_parameter_to_types(Param, Types, Coercions),
    coerce_parameters(RestParams, RestTypesGroups, RestCoercions).

% Generate the Coercion Table
parameter_coercion_table(Params, GroupedTypes, CoercionTable) :-
    coerce_parameters(Params, GroupedTypes, CoercionTable).

% Type and Params Matching
score_type_match(Types, Parameters, CoercionTable, MinimizedTypes, ReducedParams, Score) :-
    % Evaluate the compatibility of each parameter with the expected type
    maplist(type_match_score(CoercionTable), CoercionTable, Types, Parameters, MinimizedTypes, ReducedParams, Scores),
    % Sum the scores to determine the overall type match score
    sumlist(Scores, Score).

% Type Compatibility Scoring
type_match_score(CoercionTableWhole, CoercionTableE, ExpectedType, ActualParam, MinimizedType, ReducedParam, Score) :-
    % Determine the actual type of the parameter
    (get_type(ActualParam, ActualType),
     % Perfect match: The actual type matches the expected type
     (ActualType == ExpectedType ->
        (MinimizedType = ExpectedType,
         Score = 10) ;
      % Compatible match: Expected type is a supertype of the actual type
      sub_type(ExpectedType, ActualType) ->
        (MinimizedType = ExpectedType,
         Score = 5) ;
      % Compatible match: Actual type is a supertype of the expected type
      sub_type(ActualType, ExpectedType) ->
        (MinimizedType = ActualType,
         Score = 9)),
     % Retrieve the reduced parameter based on the coercion table
     (key_member(CoercionTableE, MinimizedType, ReducedParam) -> true ;
         find_type_fit(CoercionTableWhole, ActualParam, ExpectedType, ReducedParam))).


find_type_fit(CoercionTable, ActualParam, MinimizedType, ReducedParam):-
     key_member(CoercionTable, ActualParam, TypeList), key_member(TypeList, MinimizedType, ReducedParam).

key_member(L, K, V):- member(KK - V, L), ((var(KK);var(K)) -> KK == K ; KK = K).

% Type Definitions
type_of('Z', 'List').
type_of(['S', _], 'List').

% Subtype Relationships
sub_type(_, _).

% Enums Validation
validate_function_type_enums(MismatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior) :-
    % Validate each behavior configuration against allowed enum values
    validate_enum_value('MismatchEnum', MismatchBehavior),
    validate_enum_value('NoMatchEnum', NoMatchBehavior),
    validate_enum_value('EvaluationOrderEnum', EvaluationOrder),
    validate_enum_value('FunctionResultEnum', SuccessBehavior),
    validate_enum_value('ClauseFailedEnum', FailureBehavior),
    validate_enum_value('OutOfClausesEnum', OutOfClausesBehavior).

validate_enum_value(EnumType, Value) :-
    % Ensure the value belongs to the allowed enum set
    (explicit_isa(Value, EnumType) -> true ;
     throw(error(invalid_enum_value(EnumType, Value), _))).

% Utility: Length Check
% len_or_unbound(Params, Len) :- (var(Len) -> true ; length(Params, Len)).

% Match Params
match_head(Params, Parameters) :-
    Params = Parameters.



% ------------------------------------------------------------------------------
% Core Logic with Type Guards
% ------------------------------------------------------------------------------

% Helper to check type guards.
guard_match(X, number) :- number(X).
guard_match(X, atom) :- atom(X).
guard_match(X, list) :- is_list(X).
guard_match(X, complex) :- is_list(X), length(X, N), N > 5.
guard_match(X, simple) :- is_list(X), length(X, N), N =< 5.
guard_match(_, generic).

% Define what happens inside the guarded body.
guarded_body(X, Result, success) :-
    writeln(successful_guard(X)),
    Result = processed(X).

guarded_body(X, Result, failure) :-
    writeln(failed_guard(X)),
    Result = return_original(X).

% Fallback logic if no guards match.
fallback_logic(X, Result) :-
    writeln('No type guard matched. Executing fallback.'),
    Result = default_value(X).

% Nested guard logic.
nested_guard(X, Result) :-
    (   X = hello ->
        Result = special_case_handled
    ;   Result = default_atom_result
    ).

% ------------------------------------------------------------------------------
% Tests
% ------------------------------------------------------------------------------

% Test 1: Simple Type Guard Matching
test_simple_guard :-
    function(42, Result1), writeln(Result1),
    function(hello, Result2), writeln(Result2),
    function([], Result3), writeln(Result3),
    function(foo, Result4), writeln(Result4).

% Test 2: Fallback Behavior
test_fallback :-
    function_with_fallback([], Result), writeln(Result).

% Test 3: Prioritized Type Guard Evaluation
test_prioritized :-
    prioritized_function([1, 2, 3], Result1), writeln(Result1),
    prioritized_function([1, 2, 3, 4, 5, 6], Result2), writeln(Result2),
    prioritized_function(hello, Result3), writeln(Result3).

% Test 4: Nested Guarded Logic with Errors
test_nested :-
    nested_function(42, Result1), writeln(Result1),
    nested_function(hello, Result2), writeln(Result2),
    nested_function(world, Result3), writeln(Result3),
    nested_function([], Result4), writeln(Result4).

% ------------------------------------------------------------------------------
% Function Definitions
% ------------------------------------------------------------------------------

% Function with basic guards.
function(X, Result) :-
    (   guard_match(X, number) ->
        guarded_body(X, Result, success)
    ;   guard_match(X, atom) ->
        guarded_body(X, Result, success)
    ;   guard_match(X, list) ->
        guarded_body(X, Result, success)
    ;   guarded_body(X, Result, failure)
    ).

% Function with a fallback mechanism.
function_with_fallback(X, Result) :-
    (   guard_match(X, number) ->
        guarded_body(X, Result, success)
    ;   guard_match(X, atom) ->
        guarded_body(X, Result, success)
    ;   fallback_logic(X, Result)
    ).

% Function with prioritized guards.
prioritized_function(X, Result) :-
    evaluation_order(fittest_first), % Assume we process most specific guards first.
    (   guard_match(X, complex) ->
        guarded_body(X, Result, success)
    ;   guard_match(X, simple) ->
        guarded_body(X, Result, success)
    ;   guard_match(X, generic) ->
        guarded_body(X, Result, success)
    ;   guarded_body(X, Result, failure)
    ).

% Function with nested guards and error handling.
nested_function(X, Result) :-
    (   guard_match(X, number) ->
        guarded_body(X, Result, success)
    ;   guard_match(X, atom) ->
        nested_guard(X, Result)
    ;   fallback_logic(X, Result)
    ).

ffffff:- writeln('
    ?- test_simple_guard.
    ?- test_fallback.
    ?- test_prioritized.
    ?- test_nested.
').


%! freeist(+X, +Y, -Result) is det.
%
%  A comparison predicate for `predsort/3` that sorts terms by freeness.
%
%  Terms are sorted based on the following criteria:
%  - Variables are considered the "most free" and are sorted first.
%  - Partially instantiated terms come next.
%  - Fully ground terms are sorted last. Among them, they are further sorted by
%    their complexity (the total number of functors and arguments in the term).
%
%  If two terms have the same degree of freeness and complexity, a lexicographic comparison
%  is used as a final fallback.
%
%  Example usage with `predsort/3`:
%  ==
%  ?- predsort(freeist, [X, f(Y), g(a), Z, b, h(1, 2, 3)], Sorted).
%  % Sorted = [X, Z, f(Y), b, g(a), h(1, 2, 3)].
%
%  ?- predsort(freeist, [a, f(a), h(a, b, c), g(a), b], Sorted).
%  % Sorted = [a, b, g(a), f(a), h(a, b, c)].
%
%  ?- predsort(freeist, [X, Z, f(X, Y), b, h(a), g(a)], Sorted).
%  % Sorted = [X, Z, f(X, Y), b, g(a), h(a)].
%
%  ?- predsort(freeist, [g(a), g(b), f(a, b), a, h(a, b, c), X, Z], Sorted).
%  % Sorted = [X, Z, a, g(a), g(b), f(a, b), h(a, b, c)].
%  ==
%
%  @param Result Comparison result: `<`, `=`, or `>`.
%  @param Y Second term to compare.
%  @param X First term to compare.

%freeist(Result, X, Y):- X == Y, !, Result = (=).
freeist(Result, X, Y):- X =@= Y, !, compare(Result, Y, X).
freeist(Result, Y, X) :- compound(Y), Y=(YY-_), !, freeist(Result, YY, X).
freeist(Result, Y, X) :- compound(X), X=(XX-_), !, freeist(Result, Y, XX).
freeist(Result, Y, X) :-
    term_freeness(Y, FX),
    term_freeness(X, FY),
    ( FX = FY ->
        ( FX = 2 -> % If both terms are ground
            term_arity(Y, AX),
            term_arity(X, AY),
            ( AX = AY ->
                term_complexity(Y, CX),
                term_complexity(X, CY),
                ( CX = CY ->
                    (compound_term_compare(ResultNE, X, Y), (ResultNE \= (=) -> ResultNE=Result ; compare(Result, Y, X))) % Compare compound terms argument by argument
                ; compare(Result, CX, CY) )
            ; compare(Result, AX, AY) )
        ; compare(Result, Y, X) ) % Fallback for other types if freeness is the same
    ; compare(Result, FX, FY) % Compare by freeness
    ), !.

% Calculate term freeness
term_freeness(Term, 1) :- attvar(Term), !.
term_freeness(Term, 0) :- var(Term), !.
%term_freeness(Term, 1) :- term_variables(Term, Vars), Vars \= [], !.
term_freeness(_, 2).

% Calculate term arity (number of arguments)
term_arity(Term, Arity) :-
    %ground(Term), % Only applies to ground terms
    Term =.. [_|Args],
    length(Args, Arity).
term_arity([_|Args], Arity):-length(Args, Arity).

% Calculate term complexity (total number of functors and arguments in the term)
term_complexity(Term, Complexity) :- fail,
    ground(Term), % Only applies to ground terms
    term_complexity_acc(Term, 0, Complexity).
term_complexity(_, 1).

term_complexity_acc(Term, Acc, Complexity) :-
    Term =.. [_|Args],
    length(Args, ArgCount),
    NewAcc is Acc + 1 + ArgCount,
    foldl(term_complexity_acc, Args, NewAcc, Complexity).

term_to_list(L, [L]):- \+ compound(L), !.
term_to_list(L, L):- is_list(L), !.
term_to_list(C, [F|Args]):- C \=[_|_], !, compound_name_arguments(C, F, Args).
term_to_list(L, [L]).

% Compare compound terms argument by argument
compound_term_compare(Result, X, Y) :-
    term_to_list(X, XX),
    term_to_list(Y, YY),
    maplist(nc_weight, XX, XXL), sumlist(XXL, SX),
    maplist(nc_weight, YY, YYL), sumlist(YYL, SY),
    compare(FunctorResult, SY, SX), % Compare functors lexicographically
    ( FunctorResult = (=) ->
        compare_args(Result, XX, YY)  % Compare arguments recursively
    ; Result = FunctorResult ).

% Recursively compare lists of arguments
compare_args(Result, [A1|Rest1], [A2|Rest2]) :- !,
    non_compound_compare(ArgResult, A1, A2), % Compare individual arguments using the custom predicate
    ( ArgResult = (=) ->
        compare_args(Result, Rest1, Rest2) % Continue with the remaining arguments
    ; Result = ArgResult ).
compare_args(Result, A, B) :- A==B, Result = (=). % Both lists are empty
compare_args(Result, A, _) :- A==[], Result = (<). % First list is shorter
compare_args(Result, _, B) :- B==[], Result = (>). % Second list is shorter

% Example custom comparison for individual atoms or non-compound terms
non_compound_compare(Result, A, B) :-
    % Example: Comparing atoms by custom weights
    nc_weight(A, WA),
    nc_weight(B, WB),
    (WA==WB-> compare(Result, WB, WA); compare(Result, A, B)).

% Example weight mapping for atomics
nc_weight(Attvar, 7):- attvar(Attvar), !.
nc_weight(Var, 8):- var(Var), !.
nc_weight(T, N):- is_decl_mtype(T, N), !.
nc_weight(T, N):- is_decl_utype(T, N), !.
nc_weight(T, 6):- atomic(T), !.
nc_weight(T, Sum):- term_to_list(T, XX), maplist(nc_weight, XX, XXL), sumlist(XXL, Sum).




