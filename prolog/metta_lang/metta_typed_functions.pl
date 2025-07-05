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


%calc_non_evalation_args:- forall()

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

/*
;(: a (-> Number Number ) )
(= (a $x) (+ $x $x))
(= (a 1) 1)

;(: a (-> String Number ) )
(= (a $x ) (repr (a (parse $x))))

(a "3") > "6"

(a 1) -> 1
*/

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


% predicate_behavior(Self, Op, Len, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior)

predicate_behavior(Self, Op, Len, ObjList) :-
    if_or_else(predicate_behavior_impl(Self, Op, Len, TypeList),
           predicate_behavior_fallback(Self, Op, Len, TypeList)),
    findall(Patch,get_type(Op,Patch),Patches),
    update_types(Patches,TypeList,Updated),
    length(ObjList,10),
    maplist(=(Updated),ObjList).

% default
predicate_behavior_fallback(_, _, _, ['MismatchOriginal', 'NoMatchOriginal', 'OrderClause', 'Nondeterministic', 'ClauseFailNonDet', 'FailureOriginal']).

does_conflict(One,Conflict):- explicit_isa(One,This),explicit_isa(Conflict,This).
update_type(One,Before,After):-
  select(Conflict,Before,Middle),
  does_conflict(One,Conflict),!,
  update_type(One,Middle,After).
update_type(One,Before,[One|Before]).

update_types([],List,List):-!.
update_types([One|Patches],Before,After):- !, update_type(One,Before,Middle), update_types(Patches,Middle,After).



predicate_behavior_impl(_, 'get-type', 1, ['MismatchFail', 'NoMatchFail', 'OrderClause', 'Nondeterministic', 'ClauseFailNonDet', 'FailureEmpty']).
predicate_behavior_impl('&self', 'foo', 2,['MismatchOriginal', 'NoMatchOriginal', 'OrderFittest', 'Nondeterministic', 'ClauseFailNonDet', 'FailureOriginal']).
predicate_behavior_impl(_, 'match', 4,    ['MismatchFail', 'NoMatchFail', 'OrderClause', 'Nondeterministic', 'ClauseFailNonDet', 'FailureEmpty']).

predicate_behavior_impl(_, 'case', 2,     ['MismatchOriginal', 'NoMatchFail', 'OrderClause', 'Nondeterministic', 'ClauseFailNonDet', 'FailureEmpty']).

%(case (empty) ((Empty ())))

%(if (== (+ 2 3) 5) True False)
%(assert 0)

get_ftype(Eq, RetType, Depth, Self, Val, TypeO):-
  if_or_else(get_ftype_decl(Eq, RetType, Depth, Self, Val, TypeO),
             get_ftype_decl2(Eq, RetType, Depth, Self, Val, TypeO),
             get_ftype_fallback(Eq, RetType, Depth, Self, Val, TypeO)).

get_ftype_decl(_Eq, _RetType, Depth, Self, Val, TypeO):- get_type(Depth, Self, Val, TypeO), arrow_type(TypeO,_,_).
get_ftype_decl2(Eq, RetType, Depth, Self, [Val|_], TypeO):- is_list(Val), get_ftype_decl(Eq, RetType, Depth, Self, Val, TypeO).

get_ftype_fallback(_Eq, _Type, _Depth, Self, [Op|Args], TypeO):- nonvar(Op), len_or_unbound(Args, Len), !, get_operator_ftypedef(Self, Op, Len, TypeO).
get_ftype_fallback(_Eq, _Type, _Depth, Self, Op, TypeO):- get_operator_ftypedef(Self, Op, _Len, TypeO).

op_farity(Op, Len):- no_repeats_var(Len),
   if_or_else(op_farity_decl(Op, Len),op_farity_fallback(Op, Len)).

op_farity_decl(Op, Len):- metta_type(_Self, Op, [Ar, _|Types]), Ar=='->', len_or_unbound(Types, Len).
op_farity_decl(Op, Len):- metta_defn(_Self, [Op1 | Args], _Body),Op==Op1, len_or_unbound(Args, Len).
op_farity_decl(Op, Len):- metta_defn(_Self, [[Op1|Args]|_], _Body),Op==Op1, len_or_unbound(Args, Len).
op_farity_fallback(_Op, Len):- between(0, 8, Len).

get_operator_ftypedef(Self, Op, Len, TypeO):- (var(Len)->op_farity(Op, Len);true),
   get_operator_typedef(Self, Op, Len, ParamTypes, RetType), append(['->'|ParamTypes], [RetType], TypeO).

/*
metta_defn('&self', ['double-it', 'Z'], 'Z').
metta_defn('&self', ['double-it', ['S', X], ['S', ['S', ['double-it', X]]]).
get_operator_typedef('&self', 'double-it', 1, ['List'], 'List').

-->


*/

fake_body([Op | Parameters], [Op | Parameters]).

must_length(List,Len):- is_list(List),!,must_det_lls(length(List,Len)).
must_length(List,Len):- integer(Len),!,must_det_lls(length(List,Len)).
must_length(List,Len):- trace,length(List,Len).

metta_defn_return(Self, Original, Body, WrappedBody, ReturnVal):-
  if_or_else(metta_defn_decl(Self, Original, Body, WrappedBody, ReturnVal),
         metta_defn_fallback(Self, Original, Body, WrappedBody, ReturnVal)).

metta_defn_decl(Self, [Op | Args], Body, [let, ReturnVal, Body, ReturnVal], ReturnVal):- metta_defn(Self, [Op | Args], Body).
%metta_defn_decl(Self, [[Op | Args] | Apply], [ apply, Body | Apply], [let, ReturnVal, [ Body | Apply], ReturnVal], ReturnVal):- is_list( Args), metta_defn(Self, [Op | Args] , Body).
%metta_defn_decl(Self, [Op | Args], [ [do_apply ,[Op | Args], Body] | Apply], [let, ReturnVal, [Body|Apply], ReturnVal], ReturnVal):- is_list( Args),
%   metta_defn(Self, [[Op | Args] | Apply], Body).

metta_defn_fallback(_Self, [Op | Parameters], [let, ReturnVal, Body, ReturnVal], Body, ReturnVal):- fail, is_list(Parameters),
   must_length(Parameters, Len),
   format(atom(Fn),'mc_~w__~w',[Len,Op]),
   current_predicate_fast(Fn/_),
   Body = ['call-fn',Fn|Parameters],!.
metta_defn_fallback(_Self, [Op | Parameters], Body, Body, ReturnVal):- fail,
   Body = [let, [quote, ReturnVal], [quote, ['interp!', Op | Parameters]], ReturnVal], Op \=='interp!'.


metta_typed_defn(Self, ParamTypes, RetType, Head, WrappedBody, ReturnVal):-  Head = [Op | Parameters],
   must_length(Parameters, Len), must_length(CParameters, Len), must_length(ParamTypes, Len), must_length(CParamTypes, Len),
   function_declaration_impl(Self, Op, _Len, CParameters, CParamTypes, CRetType, WrappedBody, CReturnVal),
   ParamTypes = CParamTypes, Parameters = CParameters, CRetType = RetType, CReturnVal = ReturnVal.

:- dynamic(function_declaration_cache/9).
function_declaration_cache('&self', 'double-it', 1, ['Z'], ['List'], 'List', ['let', ReturnVal, 'Z', ReturnVal], 'Z').
function_declaration_cache('&self', 'double-it', 1, [['S', X]], ['List'], 'List', ['let', ReturnVal, ['S', ['S', ['double-it', X]]], ReturnVal], ['S', ['S', ['double-it', X]]]).


function_declaration(Self,  Op, Len, Parameters, ParamTypes, RetType, Body, WrappedBody, ReturnVal):-
   must_length(Parameters, Len), must_length(CParameters, Len), must_length(ParamTypes, Len), must_length(CParamTypes, Len),
   function_declaration_impl(Self, Op, Len, CParameters, CParamTypes, CRetType, CBody, CWrappedBody, CReturnVal),
   ParamTypes = CParamTypes, Parameters = CParameters,  CRetType = RetType, CBody = Body, CWrappedBody = WrappedBody, CReturnVal = ReturnVal.


function_declaration_impl(Self, Op, Len, Parameters, ParamTypes, RetType, Body, WrappedBody, ReturnVal):-
  if_or_else(function_declaration_impl1(Self, Op, Len, Parameters, ParamTypes, RetType, Body, WrappedBody, ReturnVal),
             function_declaration_impl2(Self, Op, Len, Parameters, ParamTypes, RetType, Body, WrappedBody, ReturnVal)).

function_declaration_impl2(Self, Op, Len, Parameters, ParamTypes, RetType, Body, WrappedBody, ReturnVal):-
  if_trace(guards,wdmg(failed(function_declaration_impl(Self, Op, Len, Parameters, ParamTypes, RetType, Body, WrappedBody, ReturnVal)))),fail.

function_declaration_impl1(Self, Op, Len, Parameters, ParamTypes, RetType, Body, WrappedBody, ReturnVal) :-
   call((
    %Self = '&self',
    len_or_unbound(Parameters, Len),
    %(var(Len)->op_farity(Op, Len);true),
    %len_or_unbound(Parameters, Len),
    nop(no_repeats_var(NRR)),
    metta_defn_return(Self, [Op | Parameters], Body, WrappedBody, ReturnVal),
    must_length(Parameters, Len),
    NR = ([Op | Parameters] + Body),
    once(head_body_typedef(Self, Op, Len, ParamTypes, RetType, [Op | Parameters], Body)),
    NR = NRR)). %nop(write_src_nl(metta_defn(Self, [Op | Parameters], Body).

head_body_typedef(Self, Op, Len, ParamTypes, RetType, Head, Body):-
    if_or_else(src_data_ordinal(Self, [=, Head, Body], ClauseOrdinal),
               src_data_ordinal(Self, [=, Head, _], ClauseOrdinal),
               ClauseOrdinal = -1),
    must_length(ParamTypes, Len),
    SrcObject = pr(ParamTypes, RetType),
    findall(TypeDeclLoc-SrcObject, (src_data_ordinal(Self, [:, Op, [Ar|Type]], TypeDeclLoc), Ar=='->', append(ParamTypes, [RetType], Type)), SrcObjectList),
    SrcObjectList\==[],
    nearest_src_object(SrcObject, ClauseOrdinal, SrcObjectList),!.
head_body_typedef(Self, Op, Len, ParamTypes, RetType, _Head, _Body):- %trace,
    get_operator_typedef(Self, Op, Len, ParamTypes, RetType).

src_data_ordinal(Self, Data, Ordinal):-
   if_or_else(src_data_ordinal_exact(Self, Data, Ordinal),src_data_ordinal_unifable(Self, Data, Ordinal)).
src_data_ordinal_exact(Self, Data, Ordinal):- copy_term(Data,DataC),
    user:metta_file_buffer(0, Ordinal, _TypeNameCompound, DataC, _NamedVarsListC, Context, _Range), DataC=@=Data,
    \+ \+ visible_from(Context, Self).
src_data_ordinal_unifable(Self, Data, Ordinal):- copy_term(Data,DataC),
    user:metta_file_buffer(0, Ordinal, _TypeNameCompound, DataC, _NamedVarsListC, Context, _Range),
    Data=DataC,
    \+ \+ visible_from(Context, Self).

visible_from(_Context, _Self).

nearest_src_object(SrcObject, ClauseOrdinal, SrcObjectList):- ClauseOrdinal > 0,
    select(TypeDeclLoc-SrcObject, SrcObjectList, Rest),
    Distance is TypeDeclLoc-ClauseOrdinal,
    Distance<0,
    \+ (member(LocOther-_, Rest),
       DistanceOther is LocOther-ClauseOrdinal,
       DistanceOther<0, DistanceOther>Distance), !.

nearest_src_object(SrcObject, ClauseOrdinal, SrcObjectList):-
    if_trace(nearest_src_object,wdmsg(failing(nearest_src_object(SrcObject, ClauseOrdinal, SrcObjectList)))),fail.

nearest_src_object(SrcObject, ClauseOrdinal, SrcObjectList):- ClauseOrdinal > 0,
    select(TypeDeclLoc-SrcObject, SrcObjectList, Rest),
    Distance is TypeDeclLoc-ClauseOrdinal,
    Distance>0,
    \+ (member(LocOther-_, Rest),
       DistanceOther is LocOther-ClauseOrdinal,
       DistanceOther>0, DistanceOther<Distance), !.

nearest_src_object(SrcObject, _, SrcObjectList):- last(_ - SrcObject, SrcObjectList),!.



heads_of_op(Op,Head):- metta_type(_Self,Op,ArTypeDecl),arrow_type(ArTypeDecl,ParamTypes,RetType),arrow_type(RetType,LT,_LR),
   same_len_copy(ParamTypes,PTL),same_len_copy(LT,LTL),
   Head=[[Op|PTL]|LTL].
heads_of_op(Op,Head):- metta_type(_Self,Op,ArTypeDecl),arrow_type(ArTypeDecl,ParamTypes,RetType), \+ arrow_type(RetType,_LT,_LR),
   same_len_copy(ParamTypes,PTL),
   Head=[Op|PTL].

arrow_type([Ar|TypeDecl],ParamTypes,RetType):- Ar=='->',append(ParamTypes,[RetType],TypeDecl).


finfo_atomic(Op):-
   if_or_else(  (heads_of_op(Op,Head), finfo_head(Head)),
                (op_farity(Op, Len), finfo_arity(Op, Len))).

finfo_head([[Op|LA]|Args]):- atom(Op), is_list(LA), is_list(Args), !, length(LA, Len), finfo(Op,Len,[[Op|LA]|Args]).
finfo_head([Op|Args]):- atom(Op), is_list(Args), !, length(Args, Len), finfo(Op, Len, [Op|Args]).
% curried

finfo(Op):- atomic(Op), !, finfo_atomic(Op).
finfo(Op):- is_list(Op), !, finfo_head(Op).
finfo(Op, Len):- atomic(Op), finfo_arity(Op, Len).
finfo_arity(Op, Len):- if_t(var(Len),op_farity(Op, Len)), length(Args, Len), finfo([Op|Args]).
finfo(Op, Len, Head):-
    % length(Parameters, Len),
    if_t(var(Len),op_farity(Op, Len)),
    succ(Len,LenP1),
    call_showing(predicate_behavior(Self, Op, Len, [List|_]), predicate_behavior(Self, Op, Len, List)),
    %length(Parameters, Len),
    %length(ParamTypes, Len),
    call_showing(get_ftype('=', _RetType1, 20, Self, Head, TypeO), [['get-ftype', Head, Self],==>,TypeO]),
    %call_showing(metta_atom(_, [iz, Op, _])),
    %call_showing(metta_atom(_, [':', Op, _])),
    ReturnVal = '$VAR'('_returnVal'),
    call_showing(transpiler_predicate_store(Op, LenP1, _, _)),
    call_showing(transpiler_clause_store(Op, LenP1, _, _, _, _, _, _, _)),
    format(atom(Fn),'mc_~w__~w',[Len,Op]), % forall(current_predicate_fast(Fn/LenP1),listing(Fn/LenP1)),
    call_showing(Fn/LenP1),
    call_showing(function_declaration_scores(Self, Op, Len, Parameters, ParamTypes, _RetType, Body, ReturnVal,_)),
    if_t(\+ function_declaration_scores(Self, Op, Len, Parameters, ParamTypes, __RetType, Body, ReturnVal,_),
        call_showing(function_declaration_impl(_, Op, Len, Parameters, ParamTypes, ___RetType, Body, __WrappedBody, ReturnVal))),
    if_t(\+ metta_defn(Self, [Op | Parameters], Body), call_showing(metta_defn_return(Self, [Op | Parameters], Body, _, ReturnVal),[=,[Op | Parameters],Body])),
    call_showing((metta_atom(KB, [A, B|Out]), sub_var_safely(Op, [A, B])), [ist,KB, [A, B|Out]]),
    true.

call_showing(Var):- \+ callable(Var), !, write_src_nl(not(callable(Var))).
call_showing(Atom):- atom(Atom), \+ current_predicate_fast(Atom/_, _), !, write_src_nl(unknown(Atom)).
call_showing(Atom):- atom(Atom), !, forall(current_predicate_fast(Atom/N),call_showing(Atom/N)).
call_showing(Op/Len):- \+ current_predicate_fast(Op/Len), !, write_src_nl(unknown(Op/Len)).
call_showing(Op/Len):- !, forall(current_predicate_fast(Op/Len, SHOWP), call_showing(clause(SHOWP,Body), (SHOWP:-Body))).
call_showing(SHOWP):- \+ current_predicate_fast(_, SHOWP), !, write_src_nl(unknown(SHOWP)).
call_showing(SHOWP):- call_showing(SHOWP, SHOWP).

call_showing(SHOWP, Template):-
    no_repeats_var(TemplateNR),
    findall(Template, (SHOWP, TemplateNR=Template), ScoredBodies),
    maplist(output_showing, ScoredBodies),
    ignore((ScoredBodies==[], functor(Template,F,A),output_showing(Template=missing(F/A)))).

output_showing(List):- is_list(List),!,write_src_nl(List).
output_showing(Info):- w_color(white,write_w_attvars(Info)),nl, !.
output_showing(Info):- write_src_nl(Info).



function_declaration_scores(Self, Op, Len, Parameters, ParamTypes, RetType, Body, ReturnVal, Score + HScore):-
   function_declaration(Self, Op, Len, Parameters, ParamTypes, RetType, Body, _WrappedBody, ReturnVal),
   score_term(ParamTypes, Score), score_term(Parameters, HScore).

score_term(Types, Score):- term_to_list(Types, XX), maplist(nc_weight, XX, XXL), sumlist(XXL, Score).

% Main Entry Point
implement_predicate(Self, [Op | Parameters], ReturnVal) :-
    % Safely execute the main logic, falling back on a default behavior if needed.
    catch(implement_predicate_nr(Self, [Op | Parameters], ReturnVal), metta_notreducable(Original), ReturnVal = Original).

:- op(700,xfx,('haz_value')).
'haz_value'(List,E):- member(EE,List),EE==E.

% Main Logic
implement_predicate_nr(Self, [Op | Parameters], ReturnVal) :-

    Original = [Op | Parameters],

    % Determine the expected arity of the predicate
    len_or_unbound(Parameters, Len),
    % Retrieve the behavior configuration for the predicate
    predicate_behavior(Self, Op, Len, [MismatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior]),

    % Validate enums dynamically using explicit_isa to ensure valid inputs
    %validate_function_type_enums(MismatchBehavior, NoMatchBehavior, EvaluationOrder, SuccessBehavior, FailureBehavior, OutOfClausesBehavior),

    % Retrieve all clauses for the predicate
    findall(thbr(ParamTypes, Params, Body, ReturnVal, RetType), function_declaration(Self, Op, Len, Params, ParamTypes, RetType, Body, _WrappedBody, ReturnVal), Clauses),

    % Extract parameter types and group them by index across all clauses
    findall(Types, (member(thbr(Types, _, _, _, RetType), Clauses)), ParamTypesPerClause),
    group_types_by_param_index(ParamTypesPerClause, Grouped),
    convert_to_unique_sets(Grouped, ParamTypeSets),

    % Generate a coercion table mapping parameters to their possible coerced types
    parameter_coercion_table(Parameters, ParamTypeSets, CoercionTable),

    % Phase 1: Filter and Score Type Matching
    findall(TypeScore - (MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType),
        (member(thbr(Types, Params, Body, ReturnVal, RetType), Clauses),
         score_type_match(Types, Parameters, CoercionTable, MinimizedTypes, ReducedParams, TypeScore)),
        TypeMatchedBodies),

    % Handle type mismatch based on MismatchBehavior
    (TypeMatchedBodies \== [] -> true ;
        (MismatchBehavior haz_value 'MismatchFail' -> (!, fail)
        ; MismatchBehavior haz_value 'MismatchError' -> throw(metta_notreducable(['Error', Original, 'Incorrect Parameter Types']))
        ; MismatchBehavior haz_value 'MismatchOriginal' -> throw(metta_notreducable(Original)))),

    % Phase 2: Filter and Score Params Matching
    findall((TypeScore + HeadScore) - (MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType),
        (member(TypeScore - (MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType), TypeMatchedBodies),
         score_head_match(Params, Parameters, HeadScore)),
        FullyMatchedBodies),

    % Handle NoMatch Params based on NoMatchBehavior
    ( FullyMatchedBodies \== [] -> true ;
        (NoMatchBehavior haz_value 'NoMatchFail' -> (!, fail)
        ; NoMatchBehavior haz_value 'NoMatchOriginal' -> throw(metta_notreducable(Original)))),

    % Maybe change the evaluation order
    (EvaluationOrder haz_value 'OrderFittest' ->
        keysort(FullyMatchedBodies, OrderedBodies);
        OrderedBodies = FullyMatchedBodies), % 'OrderFittest' sorts by score

    % Process Ordered Bodies
    (((member(_TypeScore - (_MinimizedTypes, ReducedParams, Params, Body, ReturnVal, RetType), OrderedBodies), match_head(Params, ReducedParams)) *->
        (eval_args(Body,ReturnVal) *->
            (SuccessBehavior haz_value 'Deterministic' -> ! ; true) % vs Nondeterministic
        ;
            (FailureBehavior haz_value 'ClauseFailDet' -> % vs ClauseFailNonDet
                (OutOfClausesBehavior haz_value 'FailureOriginal' -> throw(metta_notreducable([Op| ReducedParams])) ; (!, fail));
             fail)))
    *-> true ;
    (OutOfClausesBehavior haz_value 'FailureOriginal' -> throw(metta_notreducable(Original)) ; (!, fail))).

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
      % Compatible match: Actual type is a supertype of the expected type
      assignable_to(ActualType, ExpectedType) ->
        (MinimizedType = ActualType,
         Score = 8) ;
      % Compatible match: Expected type is a supertype of the actual type
      assignable_to(ExpectedType, ActualType) ->
        (MinimizedType = ExpectedType,
         Score = 5)),
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
assignable_to(Was, _):- Was = '%Undefined%', !, fail.
assignable_to(From,To):- can_assign(From,To).
%assignable_to(_, _).

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




