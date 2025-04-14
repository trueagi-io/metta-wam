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
% PROGRAM FUNCTION: Provides Prolog predicates and rules for type checking, evaluation, and manipulation
% of MeTTa expressions and data structures.
%*********************************************************************************************

% Ensure that the `metta_interp` library is loaded,
% That loads all the predicates called from this file
:- ensure_loaded(metta_interp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  typed_list(+Cmpd, -Type, -List) is nondet.
%
%   Verifies that Cmpd is a compound term with functor Type and a list as its
%   first argument, while ensuring it is not a standard Prolog list. It uses
%   `compound_name_arguments/3` to extract the functor and arguments.
%
%   @arg Cmpd The compound term to be checked.
%   @arg Type Unifies with the functor (name) of the compound term Cmpd.
%   @arg List Unifies with the first argument of Cmpd if it is a valid list.
%
%   @example
%     ?- typed_list(foo([1, 2, 3], bar), Type, List).
%     Type = foo,
%     List = [1, 2, 3].
%
typed_list(Cmpd, Type, List) :-
    % Ensure that Cmpd is a compound term.
    compound(Cmpd),
    % Ensure that Cmpd is not a list (lists are compound terms too in Prolog).
    Cmpd \= [_|_],
    % Extract the functor (Type) and arguments from the compound term.
    compound_name_arguments(Cmpd, Type, [List|_]),
    % Verify that the first argument is a list.
    is_list(List).

%!  is_syspred(+H, +Len, -Pred) is nondet.
%
%   Checks if H with arity Len corresponds to a system predicate, unifying
%   the result with Pred. This predicate wraps `is_syspred0/3` using `notrace/1`
%   to suppress debugging during execution.
%
%   @arg H The head of the predicate being checked.
%   @arg Len The arity of the predicate.
%   @arg Pred The result indicating if the predicate is a system predicate.
%
%   @example
%     ?- is_syspred(write, 1, Pred).
%     Pred = true.
%
is_syspred(H, Len, Pred) :-
    % Suppress debugging with notrace while calling is_syspred0/3.
    notrace(is_syspred0(H, Len, Pred)).

%!  is_syspred0(+H, +Len, -Pred) is nondet.
%
%   Determines whether H with arity Len is a system predicate, using several checks and transformations.
%   If successful, it unifies Pred with the appropriate predicate name.
%
%   @arg H    The predicate name to be checked.
%   @arg Len  The arity of the predicate.
%   @arg Pred The resulting system predicate, if found.
%
%   @example
%     ?- is_syspred0(write, 1, Pred).
%     Pred = write.
%
is_syspred0(H, _Ln, _Prd) :-
    % Check if H is not an atom; if so, cut and fail.
    \+ atom(H),!,fail.
is_syspred0(H, _Ln, _Prd) :-
    % Fail if H is the same when converted to both uppercase and lowercase.
    upcase_atom(H, U), downcase_atom(H, U), !, fail.
is_syspred0(H, Len, Pred) :-
    % Check if the predicate with the given name and arity exists.
    current_predicate(H/Len), !, Pred = H.
is_syspred0(H, Len, Pred) :-
    % Check for predicates with a '!' suffix.
    atom_concat(Mid, '!', H), H \== Mid, is_syspred0(Mid, Len, Pred), !.
is_syspred0(H, Len, Pred) :-
    % Check for predicates with a '-p' suffix.
    atom_concat(Mid, '-p', H), H \== Mid, is_syspred0(Mid, Len, Pred), !.
is_syspred0(H, Len, Pred) :-
    % Check for predicates with a '-fn' suffix.
    atom_concat(Mid, '-fn', H), H \== Mid, is_syspred0(Mid, Len, Pred), !.
is_syspred0(H, Len, Pred) :-
    % Check if H can be transformed by replacing certain characters with underscores.
    into_underscores(H, Mid), H \== Mid, is_syspred0(Mid, Len, Pred), !.

%is_function(F):- atom(F).

%!  is_metta_data_functor(+Eq, +Other, +H) is nondet.
%
%   Determines whether H is a valid "metta data functor" by checking several conditions.
%   The first clause traces and checks for a clause of `is_data_functor/1`.
%   The second clause ensures H is not one of the excluded functors and performs
%   a series of negated checks to confirm it is not a built-in or operator.
%
%   @arg Eq    The equality predicate or term being evaluated.
%   @arg Other Another term used in the evaluation.
%   @arg H     The functor being checked.
%
%   @example
%     ?- is_metta_data_functor(_, _, functor_name).
%     true.
%
is_metta_data_functor(_Eq, _Othr, H) :-
    % Trace and check if there is a clause for is_data_functor/1.
    bt, trace, clause(is_data_functor(H), _).
is_metta_data_functor(Eq, Other, H) :-
    % Exclude specific functors.
    H \== 'Right', H \== 'Something',
    % \+ metta_type(Other, H, _), % fail,
    % Perform various negated checks to confirm the validity of the functor.
    %\+ get_metta_atom(Eq, Other, [H|_]),
    \+ metta_eq_def(Eq, Other, [H|_], _),
    \+ is_metta_builtin(H),
    \+ is_comp_op(H, _),
    \+ is_math_op(H, _, _).

%!  mnotrace(:Goal) is nondet.
%
%   Executes the given Goal once if `mnotrace/1` is not already defined.
%   This is used as a fallback to ensure the predicate exists, enabling
%   conditional tracing behavior.
%
%   @arg Goal The goal to be executed.
%
%   @example
%     ?- mnotrace(write('Hello')).
%     Hello
%
:- if(\+ current_predicate(mnotrace/1)).
mnotrace(G) :-
    % Execute the goal exactly once.
    once(G).
:- endif.

%!  'Number':attr_unify_hook(+Attr, +NewValue) is nondet.
%
%   Ensures that the attribute associated with the atom 'Number' can only unify
%   with numeric values. This hook is invoked when an attribute needs to unify
%   with a new value during constraint handling.
%
%   @arg Attr      The current attribute value (unused in this clause).
%   @arg NewValue  The value to be unified, which must be numeric.
%
%   @example
%     ?- put_attr(X, 'Number', _), X = 42.
%     X = 42.
%
'Number':attr_unify_hook(_, NewValue) :-
    % Ensure the new value is numeric.
    numeric(NewValue).

dont_put_attr(V,M,T):- nop(put_attr(V,M,T)),!.

%is_decl_type(ST):- metta_type(_,_,[_|Type]),is_list(Type),sub_sterm(T,Type),nonvar(T),T=@=ST, \+ nontype(ST).

%!  is_decl_utype(+Type) is nondet.
%
%   Succeeds if Type is a declared "utype" (user-defined type). The predicate
%   checks for predefined types such as 'Number', 'Symbol', 'String', etc.
%
%   @arg Type The type being checked.
%
is_decl_utype(U):- is_decl_utype(U,_).
is_decl_utype('Atom',1).
is_decl_utype('Expression',5).
is_decl_utype('Any',3).
is_decl_utype('%Undefined%',3).
is_decl_utype('AnyRet',3).
is_decl_utype('Type',5).
is_decl_utype('Number',5).
is_decl_utype('Symbol',5).
is_decl_utype('String',5).
is_decl_utype('Bool',5).
% is_decl_utype(Type) :- is_decl_type_l(Type).

%!  is_decl_mtype(+Type) is nondet.
%
%   @arg Type The type being checked.
%
is_decl_mtype(U):- is_decl_mtype(U,_).
is_decl_mtype('Variable',5).
is_decl_mtype('Number',5).
is_decl_mtype('Symbol',5).
is_decl_mtype('Expression',5).
is_decl_mtype('Grounded',4).
is_decl_mtype('PyObject',4).

% is_decl_type([ST|_]) :- !, atom(ST), is_decl_type_l(ST).
% is_decl_type(ST) :- \+ atom(ST), !, fail.

%!  is_decl_type(+Type) is nondet.
%
%   Succeeds if Type is a valid declared type.
%
%   @arg Type The type to be checked, which can be a utype, mtype, or a compound type.
%
is_decl_type(Type) :-
    % Check if Type is a declared utype.
    is_decl_utype(Type).
is_decl_type(Type) :-
    % Check if Type is a declared type from is_decl_type_l/1.
    is_decl_type_l(Type).
is_decl_type([Type, SType]) :-
    % Handle compound types by checking both main and secondary types.
    is_decl_type_l(Type),
    is_decl_utype(SType).

%!  is_decl_type_l(+Type) is nondet.
%
%   Checks if Type is a member of specific predefined types, such as 'StateMonad' or 'List'.
%
%   @arg Type The type being checked.
%
is_decl_type_l('StateMonad').
is_decl_type_l('List').

%!  last_type(+List, -Type) is nondet.
%
%   Determines the last element of a given list and checks if it is a valid type.
%   If the input is already a valid type, it succeeds directly.
%
%   @arg List A list whose last element is to be checked as a type.
%   @arg Type The last element or the provided type to be validated.
%
%   @example
%     ?- last_type([1, 'Number', 'String'], Type).
%     Type = 'String'.
%
last_type(List, Type) :-
    % Ensure the input is a list and check the last element.
    is_list(List),
    last(List, Type),
    is_type(Type).
last_type(Type, Type) :-
    % Succeed directly if the provided value is a valid type.
    is_type(Type), !.

%!  is_type(+Type) is nondet.
%
%   Succeeds if Type is a valid declared type. This predicate ensures that
%   non-types (as defined by `nontype/1`) are excluded.
%
%   @arg Type The type to be checked.
%
is_type(Type) :-
    % Fail if Type is identified as a non-type.
    nontype(Type), !, fail.
is_type(Type) :-
    % Check if Type is a declared type.
    is_decl_type(Type).
% is_type(Type) :- atom(Type).

%!  nontype(+Type) is nondet.
%
%   Succeeds if Type is not considered a valid type. This includes variables,
%   numbers, and certain special symbols.
%
%   @arg Type The type to be validated as a non-type.
%
nontype(Type) :-
    % A variable is considered a non-type.
    var(Type), !.
nontype('->').
nontype(N) :-
    % Numbers are treated as non-types.
    number(N).

%!  needs_eval(+EvalMe) is nondet.
%
%   Succeeds if EvalMe is a list, indicating that it requires evaluation.
%
%   @arg EvalMe The term to be checked for evaluation.
%
%   @example
%     ?- needs_eval([1, 2, 3]).
%     true.
%
needs_eval(EvalMe) :- fail,
    % Check if EvalMe is a list.
    is_list(EvalMe).

%!  args_violation(+Depth, +Self, +Args, +List) is nondet.
%
%   Succeeds if there is a violation between the elements in Args and List.
%   A violation occurs if an argument does not conform to its expected type.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Args  The list of arguments to check.
%   @arg List  The list of expected types or values.
%
args_violation(_Dpth, _Slf, Args, List) :-
    % Fail if either Args or List is not a non-empty list (conz structure).
    (\+ iz_conz(Args); \+ iz_conz(List)),
    !, fail.

args_violation(Depth, Self, [A|Args], [L|List]) :-
    % Check for a violation in the head or recursively in the tail.
    once(arg_violation(Depth, Self, A, L) ;
         args_violation(Depth, Self, Args, List)).

%!  arg_violation(+Depth, +Self, +Arg, +Expected) is nondet.
%
%   Succeeds if the given Arg violates the expected type L.
%
%   @arg Depth    The current recursion depth.
%   @arg Self     The context or structure being evaluated.
%   @arg Arg      The actual argument to be checked.
%   @arg Expected The expected type or value.
%
arg_violation(Depth, Self, A, L) :-
    % Check if the argument type matches the expected type.
    \+ (get_type_equals(Depth, Self, A, T), \+ type_violation(T, L)).
% arg_violation(Depth, Self, A, _) :- get_type(Depth, Self, A, _), !.

%!  type_violation(+Type, +Value) is nondet.
%
%   Succeeds if there is a type mismatch between Type and Value.
%
%   @arg Type  The type to be checked.
%   @arg Value The value or type to compare against.
%
type_violation(T, L) :-
    % Fail if both are non-specific types.
    \+ \+ (is_nonspecific_type(T); is_nonspecific_type(L)),
    !, fail.
type_violation(T, L) :-
    % Fail if the types are not equal.
    T \= L.

%!  not_arg_violation(+Depth, +Self, +Arg, +Type) is nondet.
%
%   Succeeds if the Arg conforms to the expected Type and does not cause a violation.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Arg   The actual argument to check.
%   @arg Type  The expected type.
%
not_arg_violation(Depth, Self, Arg, Type) :-
    % Ensure the argument conforms to the type and does not violate it.
    arg_conform(not_arg_violation, Depth, Self, Arg, Type),
    \+ arg_violation(Depth, Self, Arg, Type).

%!  get_types(+Depth, +Self, +Var, -TypeSet) is det.
%
%   Collects all possible types for a variable into a set.
%
%   @arg Depth   The current recursion depth.
%   @arg Self    The context or structure being evaluated.
%   @arg Var     The variable to retrieve types for.
%   @arg TypeSet The set of possible types for the variable.
%
get_types(Depth, Self, Var, TypeSet) :-
    % Collect all types for the variable using setof/3.
    quietly(setof(Type, get_type_each(Depth, Self, Var, Type), TypeSet)).

%!  get_type_equals(+Depth, +Self, +Var, -Type) is nondet.
%
%   Checks if a variable and a type are equivalent.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Var   The variable to check.
%   @arg Type  The type to compare against.
%
get_type_equals(_Depth, _Self, Var, TypeO) :-
    % Succeed if both Var and TypeO are unbound variables.
    var(Var), var(TypeO), !.
get_type_equals(Depth, Self, Var, TypeO) :-
    % Retrieve the type of the variable.
    get_type(Depth, Self, Var, TypeO).

%if_or_else(get_type(Depth,Self,Val,Type),Type='%Undefined%'),

%!  get_type(+Depth, +Self, +Val, -Type) is nondet.
%
%   Retrieves the type of a given value, ensuring there are no repeated types.
%   If `return_only_first_type` is enabled, only the first matching type is returned.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Val   The value for which the type is being retrieved.
%   @arg Type  The determined type of the value.
%
get_type(Depth, Self, Val, TypeO):- quietly(get_type0(Depth, Self, Val, TypeO)).
get_type0(Depth, Self, Val, TypeO) :-
    % Ensure no repeated types using no_repeats_var/1.
    no_repeats_var(NoRepeatType),
    % Retrieve the type of the value.
    call_nth(get_type_each(Depth, Self, Val, Type),Nth), Type\=='',
    ((Nth > 1) -> Type\== 'Atom' ; true),
    % Ensure the type matches the expected no-repeat type.
    \+ \+ (NoRepeatType = Type),
    Type = TypeO,
    % If only the first type should be returned, cut; otherwise, proceed.
    (return_only_first_type -> !; true).

%!  return_only_first_type is nondet.
%
%   Succeeds if only the first matching type should be returned.
%
return_only_first_type :- fail,
    % Check if the flag is set to true.
    true_flag.

%!  is_space_type(+Space, -Type) is nondet.
%
%   Determines the type of a given space, distinguishing between asserted spaces
%   and other types using various space type methods.
%
%   @arg Space The space to be checked.
%   @arg Type  The type of the space (e.g., `is_asserted_space`).
%
is_space_type(Space, is_asserted_space) :-
    % Check if the space was asserted.
    was_asserted_space(Space), !.
is_space_type(Space, Test) :-
    % Use no_repeats to ensure uniqueness in space type methods.
    no_repeats(Test, space_type_method(Test, _, _)),
    % Call the test to determine the space type.
    is_not_prolog_space \== Test,
    call(Test, Space), !.

%!  is_state_type(+State, -Type) is nondet.
%
%   Determines the type of a given state using state type methods.
%
%   @arg State The state to be checked.
%   @arg Type  The type determined by the relevant state type method.
%
is_state_type(State, Test) :-
    % Use no_repeats to ensure uniqueness in state type methods.
    no_repeats(Test, state_type_method(Test, _, _)),
    % Call the test to determine the state type.
    call(Test, State), !.

%!  is_dynaspace(+Space) is nondet.
%
%   Succeeds if the given Space is recognized as a dynamic space.
%   It checks if the space is either asserted, a named Python space,
%   or matches a specific typed list.
%
%   @arg Space The space to be evaluated.
%
%   @example
%     ?- is_dynaspace(Space).
%     false.  % If Space is unbound or invalid.
%
%     ?- was_asserted_space(my_space), is_dynaspace(my_space).
%     true.
%
%   @note The commented-out clause checks for callable expressions,
%   which is left in place for future reference.
%
% is_dynaspace(Expr) :- \+ is_list(Expr), callable(Expr), is_space_type(Expr, _).
is_dynaspace(S) :-
    % Fail if the space is a variable.
    var(S), !, fail.
is_dynaspace(S) :-
    % Succeeds if the space was asserted.
    was_asserted_space(S).
is_dynaspace(S) :-
    % Succeeds if the space is a named Python space.
    py_named_space(S).
is_dynaspace(S) :-
    % Succeeds if the space matches a specific typed list.
    typed_list(S, 'hyperon::space::DynSpace', _).

%  fake_notrace( is_space_type(Expr,_)),!.

%!  is_PyObject(+Obj) is nondet.
%
%   Succeeds if the given Obj is recognized as a Python object.
%   It checks if the object is a Python object, a known Python constant,
%   or fails if the object is an unbound variable.
%
%   @arg Obj The object to be evaluated.
%
%   @example
%     ?- is_PyObject('None').
%     true.
%
%     ?- is_PyObject(X).
%     false.  % Unbound variables are not considered Python objects.
%
is_PyObject(S) :-
    % Succeeds if S is recognized as a Python object.
    py_is_py(S).

%!  get_type_each(+Depth, +Self, +Val, -Type) is nondet.
%
%   Determines the type of a given value, using multiple checks and recursion.
%   Handles special cases for Python objects, dynamic spaces, variables with attributes,
%   and state monads.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Val   The value whose type is being determined.
%   @arg Type  The determined type of the value.
%
%   @example
%     ?- get_type_each(10, _, [], Type).
%     Type = '%Undefined%'.
%
get_type_each(_, _, Nil, UD) :-
    % If the value is an empty list, the type is '%Undefined%'.
    Nil == [], !, UD = '%Undefined%'.
get_type_each(Depth, Self, Val, Type) :-
    % Default depth of 10 if Depth is not an integer.
    default_depth(DEFAULT_DEPTH),
    \+ integer(Depth), !, get_type_each(DEFAULT_DEPTH, Self, Val, Type).
get_type_each(_Depth, _Slf, Val, PyObject) :-
    % If the value is a Python object, its type is 'PyObject'.
    is_PyObject(Val), !, 'PyObject' = PyObject.
get_type_each(Depth, _Slf, _Type, _) :-
    % Fail if recursion depth is exhausted.
    Depth < 1, !, fail.
% get_type(Depth, Self, Val, Type) :- is_debugging(eval),
%     ftrace(get_type_each(Depth, Self, Val, Type)),
%     fail.
get_type_each(Depth, Self, Var, Type) :- var(Var), get_attr(Var,cns, _ = Set),is_list(Set),member(Type,Set).

get_type_each(Depth, Self, Expr, ['StateMonad', Type]) :-
    % Handle state monad expressions.
    notrace(is_valid_nb_state(Expr)), !,
    if_or_else(state_decltype(Expr, Type), nonvar(Type)),
    ('get-state'(Expr, Val), !, Depth2 is Depth - 1,
     get_value_type(Depth2, Self, Val, Type)).
get_type_each(_Dpth, Self, Var, Type) :-
    % Retrieve type from variable attributes.
    var(Var), !,
    get_attr(Var, cns, Self = TypeList),is_list(TypeList),
    member(Type, TypeList).
get_type_each(_Dpth, _Slf, Expr, 'hyperon::space::DynSpace') :-
    % Check if the expression is a dynamic space.
    is_dynaspace(Expr), !.
get_type_each(Depth, Self, Val, Type) :-
    % If the value is not compound, use non-compound type checking.
    \+ compound(Val), !, get_type_nc(Depth, Self, Val, Type).
get_type_each(Depth, Self, Val, Type) :-
    % Perform type checking for compound values.
    if_t(option_value('type-check', auto), check_bad_type(Depth, Self, Val)),
    if_or_else(
        (get_type_cmpd_2nd_non_nil(Depth, Self, Val, Type, How),
         trace_get_type(How, Type, gt(Val))),
        (trace_get_type('FAILED', '', gt(Val)), fail)).


%!  get_type_cmpd_2nd_non_nil(+Depth, +Self, +Val, -Type, -How) is nondet.
%
%   Determines the type of a compound value, ensuring that if multiple types
%   are found, the type is not an empty list (`[]`). It invokes
%   `get_type_cmpd/5` and checks if the result is not the first occurrence.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Val   The compound value whose type is being determined.
%   @arg Type  The determined type of the value.
%   @arg How   The method or context used to determine the type.
%
%   @example
%     ?- get_type_cmpd_2nd_non_nil(10, _, my_val, Type, How).
%     false.  % Example when no valid second type is found.
%
get_type_cmpd_2nd_non_nil(Depth, Self, Val, Type, How) :-
    % Call get_type_cmpd/5 and retrieve the occurrence number (Nth).
    call_nth(get_type_cmpd(Depth, Self, Val, Type, How), Nth),
    % If this is a second or later occurrence, ensure Type is not an empty list.
    (Nth > 1 -> Type \== [] ; true).

have_some_defs(Depth,Self,Val):-
  \+ \+
 ([H|Args] = Val,
  metta_type(Eq,H,[Ar|ArgTypes]),Ar=='->',
  append(ParamTypes,[_RType],ArgTypes),
  length(ParamTypes,Len),
  len_or_unbound(Args,ALen),
  Len = ALen).

check_bad_type(_Depth,_Self,Val):- \+ is_list(Val),!.
check_bad_type(Depth,Self,Val):- \+ have_some_defs(Depth,Self,Val),!,
  trace_get_type(checking_childs,Val,check),!,
  maplist(check_bad_type(Depth,Self),Val).
check_bad_type(Depth,Self,Val):-
  maplist(check_bad_type(Depth,Self),Val),
  check_bad_type2(Depth,Self,Val).

check_bad_type2(Depth,Self,Val):- Val= [Op|Args],
  typed_expression(Depth,Self,[Op|Args],ArgTypes,RType),
   trace_get_type(type_sig(Op),ArgTypes,RType),
   args_conform(check, Depth,Self,Args,ArgTypes),
   (args_violation(Depth,Self,Args,ArgTypes) ->
    (trace_get_type(bad_type,args_violation(Args,ArgTypes),check),fail);
    (trace_get_type(conformed,no_args_violation(Args,ArgTypes),check),true)).

%!  typed_expression(+Depth, +Self, +Expr, -ArgTypes, -RType) is nondet.
%
%   Determines the argument types and return type for a given operator
%   expression. It extracts the operator and arguments from the expression
%   and retrieves their types using `get_operator_typedef1/5`.
%
%   @arg Depth    The current recursion depth.
%   @arg Self     The context or structure being evaluated.
%   @arg Expr     The operator expression in the form [Op | Args].
%   @arg ArgTypes The list of argument types.
%   @arg RType    The return type of the expression.
%
typed_expression(Depth, Self, [Op | Args], ArgTypes, RType) :-
    % Get the length of arguments or determine if unbound.
    len_or_unbound(Args, Len),
    % Retrieve the operator type definition.
    quietly(get_operator_typedef_R(Self, Op, Len, ArgTypes, RType)).

%!  badly_typed_expression(+Depth, +Self, +Expr) is nondet.
%
%   Succeeds if the expression is badly typed. It checks the operator
%   expression and verifies if the argument types are consistent with
%   the expected types, including type assignment checks.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Expr  The operator expression in the form [Op | Args].
%
badly_typed_expression(Depth, Self, [Op | Args]) :-
    % Check if the expression is typed and consistent.
    typed_expression(Depth, Self, [Op | Args], ArgTypes, RType),
    % Verify if the return type can be assigned.
    % can_assign(RetType, RType),
    % Check for argument violations.
    args_violation(Depth, Self, Args, ArgTypes),
    !.

% Disable debugging for specific modules.
:- nodebug(metta(types)).
:- nodebug(types).

%!  trace_get_type(+How, +Type, +Val) is det.
%
%   Traces the type evaluation process if tracing is enabled for `types`.
%   It logs messages formatted with color and indentation.
%
%   @arg How  The method or process used to determine the type.
%   @arg Type The determined type of the value.
%   @arg Val  The value being evaluated.
%
trace_get_type(How, Type, Val) :-
    % Log the type evaluation if tracing for 'types' is enabled.
    if_trace(
        types,
        color_g_mesg(
            '#7f2f2f',
            w_indent(3, format('<-- ~@ <- ~@ < ~@', [wsf(How), wsf(Type), wsf(Val)])))),
    !.

%!  wsf(+T) is det.
%
%   Writes the source form of a term T with indentation settings.
%
%   @arg T The term to be written.
%
wsf(T) :- with_indents(false, write_src(T)).

%!  get_type_nc(+Depth, +Self, +Val, -Type) is nondet.
%
%   Determines the non-compound type of a value (Val) based on its characteristics.
%   This predicate handles various primitive types such as numbers, strings, and symbols.
%
%   @arg Depth The current recursion depth (unused in some clauses).
%   @arg Self  The context or structure being evaluated.
%   @arg Val   The value whose type is being determined.
%   @arg Type  The determined type of the value.
%
%   @example
%     ?- get_type_nc(_, _, "hello", Type).
%     Type = 'String'.
%
%     ?- get_type_nc(_, _, 42, Type).
%     Type = 'Integer'.
%
get_type_nc(_Dpth, Self, Op, Type) :-
    % Retrieve the type using metta_type/3.
    metta_type(Self, Op, Type).
get_type_nc(Dpth, Slf, Val, Type) :-
    % Handle symbolic values.
    symbol(Val), !, get_type_symb(Dpth, Slf, Val, Type).
get_type_nc(_Dpth, _Slf, Val, 'String') :-
    % Succeed if the value is a string.
    string(Val), !.
% get_type_nc(_Dpth, _Slf, Val, Type) :- py_is_object(Val), py_type(Val, Type).
get_type_nc(_Dpth, _Slf, Val, 'Number') :-
    % Succeed if the value is a number.
    number(Val).
get_type_nc(_Dpth, _Slf, Val, 'Integer') :-
    % Succeed if the value is an integer and specialize numbers.
    integer(Val), !, specialize_number.
get_type_nc(_Dpth, _Slf, Val, 'Decimal') :-
    % Succeed if the value is a float and specialize numbers.
    float(Val), !, specialize_number.
get_type_nc(_Dpth, _Slf, Val, 'Rational') :-
    % Succeed if the value is a rational number.
    rational(Val), !.

%!  specialize_number is nondet.
%
%   This predicate controls whether further specialization of numbers (e.g.,
%   distinguishing between integers, floats, and rational numbers) is performed.
%   It succeeds if the `false_flag` is not set, indicating that specialization is disabled.
%
%   @example
%     ?- specialize_number.
%     false.  % Always fails because the flag is set to false.
%
specialize_number :-
    % Fail if the `false_flag` is active.
    false_flag.

%!  get_type_symb(+Depth, +Self, +Val, -Type) is nondet.
%
%   Determines the type of symbolic values based on specific patterns or evaluations.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Val   The symbolic value whose type is being determined.
%   @arg Type  The determined type of the symbolic value.
%
%   @example
%     ?- get_type_symb(_, _, 'True', Type).
%     Type = 'Bool'.
%
get_type_symb(_Dpth, _Slf, Val, 'Bool') :-
    % Check if the value is 'True' or 'False'.
    (Val == 'False'; Val == 'True').
get_type_symb(_Dpth, _Slf, Val, 'Type') :-
    % Check if the value is a declared type.
    is_decl_type(Val).
get_type_symb(_Dpth, _Slf, Val, Type) :-
    % Handle values concatenated with '@'.
    symbolic_list_concat([Type, _ | _], '@', Val).
get_type_symb(_Dpth, _Slf, Val, Type) :-
    % Handle values concatenated with ':'.
    symbolic_list_concat([Type, _ | _], ':', Val).
get_type_symb(Depth, Self, Op, Type) :-
    % Evaluate arguments if the operator is defined.
    Depth2 is Depth - 1,
    eval_args(Depth2, Self, Op, Val),
    Op \=@= Val, !,
    get_type(Depth2, Self, Val, Type).

%!  get_dict_type(+Val, +Type, -TypeO) is nondet.
%
%   Determines the type of a value by retrieving it from its dictionary attributes.
%
%   @arg Val   The value whose type is being retrieved.
%   @arg Type  The current type, if non-variable.
%   @arg TypeO The determined type from the dictionary.
%
%   @example
%     ?- get_dict_type(my_dict, _, Type).
%     Type = 'MyClass'.
%
get_dict_type(_Vl, Type, TypeO) :-
    % If Type is non-variable, unify TypeO with it.
    nonvar(Type), TypeO = Type.
get_dict_type(Val, _, Type) :-
    % Retrieve the type from the dictionary 'type' field.
    get_dict(Val, type, Type).
get_dict_type(Val, _, Type) :-
    % Retrieve the type from the dictionary 'class' field.
    get_dict(Val, class, Type).
get_dict_type(Val, _, Type) :-
    % Retrieve the type from the dictionary 'types' field if it is a list.
    get_dict(Val, types, TypeL),
    is_list(TypeL),
    member(Type, TypeL).

%!  get_type_cmpd(+Depth, +Self, +Val, -Type, -How) is nondet.
%
%   Determines the type of a compound value (Val) based on its structure.
%   This predicate handles dictionaries, variables, characters, curried operators,
%   and typed lists. The method used to determine the type is unified with How.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Val   The compound value whose type is being determined.
%   @arg Type  The determined type of the value.
%   @arg How   The method used to determine the type.
%
%get_type_cmpd(_Dpth,Self,Op,Type):- copy_term(Op,Copy),
%  metta_type(Self,Op,Type), Op=@=Copy.

type_by_functor('#\\',1,'Char').
type_by_functor('rng',_,'RandomGenerator').


get_type_cmpd(_Dpth,_Slf,Val,Type,dict):- is_dict(Val,Type),!,
  get_dict_type(Val,Type,TypeO).
get_type_cmpd(_Dpth,_Slf,'$VAR'(_),'Var',functorV):- !.
get_type_cmpd(_Dpth,_Slf,Cmpd,Type,type_by_functor(F,A,Type)):- functor(Cmpd,F,A),type_by_functor(F,A,Type),!.
% Curried Op
get_type_cmpd(Depth,Self,[[Op|Args]|Arg],Type,curried(W)):-
 symbol(Op),
 Depth2 is Depth-1,
 get_type_cmpd(Depth2,Self,[Op|Args],Type1,W),
 get_type(Depth2,Self,Arg,ArgType),
 ignore(sub_var_safely(ArgType,Type1)->true;
   (sub_term_safely(ST,Type1),var(ST),ST=ArgType)),
 last(Type1,Type).
get_type_cmpd(Depth,Self,[Op|Args],Type,ac(Op,[P|Arams],RetType)):- symbol(Op),
  len_or_unbound(Args,Len),
  get_operator_typedef(Self,Op,Len,[P|Arams],RetType),
  % Fills in type variables when possible
  args_conform(get_type, Depth,Self,Args,[P|Arams]),
  % \+ maplist(var,Arams),
  % unitests:  arg violations should return ()
  (\+ args_violation(Depth,Self,Args,[P|Arams])),
  Type=RetType.

get_type_cmpd(Depth,Self,ArTypeDecl,Type,arrow_type(ParamTypes,RetType)):-
  arrow_type(ArTypeDecl,ParamTypes,RetType), Type='%Undefined%',!.

get_type_cmpd(_Dpth,_Slf,Cmpd,Type,typed_list):-
  typed_list(Cmpd,Type,_List).
% commenting this fails two tests
get_type_cmpd(_Dpth,_Slf,_Cmpd,[],unknown):-!.
/*
get_type_cmpd(Depth,Self,[Op|Expr],Type,not_bat):-
  symbol(Op),
  maplist(get_type(Depth,Self),Expr,Types),
  [Op|Types]\=@=[Op|Expr],
  \+ badly_typed_expression(Depth,Self,[Op|Expr]),
  metta_type(Self,[Op|Types],Type).

get_type_cmpd(Depth,Self,List,Types,maplist(get_type)):-
  List\==[],
  \+ badly_typed_expression(Depth,Self,List),
  is_list(List),
  Depth2 is Depth-1,
  maplist(get_type(Depth2,Self),List,Types),
  \+ badly_typed_expression(Depth,Self,Types).

*/
get_type_cmpd(Depth,Self,EvalMe,Type,Eval_First):-
    needs_eval(EvalMe),
    Depth2 is Depth-1,
    eval_args_for_type(Depth2,Self,EvalMe,Val),
    get_type_cmpd_eval(Depth2,Self,EvalMe,Val,Type,Eval_First).
get_type_cmpd(_Dpth,_Slf,_Cmpd,[],unknown).


eval_args_for_type(_Depth2,Self,EvalMe,Val):-
      EvalMe=Val.
      %trace, break,
      %eval_args(3,Self,EvalMe,Val).
eval_args_carefully(Depth2,Self,EvalMe,Val):-
     % trace,
     eval_args(Depth2,Self,EvalMe,Val).


%!  get_type_cmpd_eval(+Depth, +Self, +EvalMe, +Val, -Type, -How) is nondet.
%
%   Evaluates the type of a compound value (EvalMe) based on different strategies.
%   It uses various methods to determine the type, such as `maplist/2` and first-evaluation.
%
%   @arg Depth  The current recursion depth.
%   @arg Self   The context or structure being evaluated.
%   @arg EvalMe The value to be evaluated for its type.
%   @arg Val    The evaluated value.
%   @arg Type   The determined type of the evaluated value.
%   @arg How    The strategy used to evaluate the type.
%
%   @example
%     ?- get_type_cmpd_eval(10, _, my_val, my_val, Type, eval_first).
%     Type = ...  % Result based on the type of my_val.
%
get_type_cmpd_eval(Depth2, Self, EvalMe, Val, Type, maplist(get_type)) :-
    % Handle maplist-based type evaluation.
    !,
    EvalMe =@= Val,
    maplist(get_type(Depth2, Self), List, Type),
    % Ensure the expression is not badly typed.
    \+ badly_typed_expression(Depth, Self, Type).
get_type_cmpd_eval(Depth2, Self, _EvalMe, Val, Type, eval_first) :-
    % Handle first-evaluation strategy.
    !,
    \+ needs_eval(Val),
    get_type(Depth2, Self, Val, Type).
get_type_cmpd_eval(Depth2, Self, _EvalMe, Val, Type, eval_first_reduced) :-
    % Handle reduced first-evaluation strategy.
    !,
    get_type(Depth2, Self, Val, Type).

state_decltype(Expr,Type):- functor(Expr,_,A),
  arg(A,Expr,Type),once(var(Type);is_decl_type(Type)).

%!  get_value_type(+Depth, +Self, +Val, -Type) is nondet.
%
%   Determines the type of a given value (Val). This predicate handles variables,
%   numbers, strings, and invokes additional type-checking logic if needed.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Val   The value whose type is being determined.
%   @arg Type  The determined type of the value.
%
%   @example
%     ?- get_value_type(_, _, 42, Type).
%     Type = 'Number'.
%
%     ?- get_value_type(_, _, X, Type).
%     Type = '%Undefined%'.
%
get_value_type(_Dpth,_Slf,Val,'Number'):- number(Val),!.
get_value_type(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_value_type(Depth,Self,Val,T):- get_type(Depth,Self,Val,T), T\==[], T\=='%Undefined%',!.
get_value_type(_Dpth,_Slf,Val,T):- 'get-metatype'(Val,T).

/*

get_value_type(Depth,Self,EvalMe,Type):- needs_eval(EvalMe),
     eval_args_for_type(Depth,Self,EvalMe,Val), \+ needs_eval(Val),!,
   get_value_type(Depth,Self,Val,Type).

get_value_type(_Dpth,Self,[Fn|_],Type):- symbol(Fn),metta_type(Self,Fn,List),last_element(List,Type), nonvar(Type),
   is_type(Type).
get_value_type(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,List,LType),last_element(LType,Type), nonvar(Type),
   is_type(Type).

get_value_type(Depth,_Slf,Type,Type):- Depth<1,!.
get_value_type(_Dpth,Self,List,Type):- is_list(List),metta_type(Self,Type,['->'|List]).
get_value_type(Depth,Self,List,Types):- List\==[], is_list(List),Depth2 is Depth-1,maplist(get_value_type(Depth2,Self),List,Types).
get_value_type(_Dpth,Self,Fn,Type):- symbol(Fn),metta_type(Self,Fn,Type),!.
%get_value_type(Depth,Self,Fn,Type):- nonvar(Fn),metta_type(Self,Fn,Type2),Depth2 is Depth-1,get_value_type(Depth2,Self,Type2,Type).
%get_value_type(Depth,Self,Fn,Type):- Depth>0,nonvar(Fn),metta_type(Self,Type,Fn),!. %,!,last_element(List,Type).

%get_value_type(Depth,Self,Expr,Type):-Depth2 is Depth-1,
% eval_args(Depth2,Self,Expr,Val),
%  Expr\=@=Val,get_value_type(Depth2,Self,Val,Type).

get_value_type(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_value_type(_Dpth,_Slf,Val,Type):- is_decl_type(Val),Type=Val.
get_value_type(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True'),!.
% get_value_type(_Dpth,_Slf,Val,'Symbol'):- symbol(Val).
%get_value_type(Depth,_Slf,Cmpd,Type):- compound(Cmpd), functor(Cmpd,Type,1),!.
%get_value_type(_Dpth,_Slf,Cmpd,Type):- \+ ground(Cmpd),!,Type=[].
%get_value_type(_Dpth,_Slf,_,'%Undefined%'):- fail.
%get_value_type(Depth,Self,Val,Type):- Depth2 is Depth-1, get_type_equals(Depth2,Self,Val,Type).
*/

%!  as_prolog(+I, -O) is det.
%
%   Converts a structured input (I) into a Prolog-friendly output (O). This predicate
%   handles various patterns, such as lists, `Cons` structures, `::` operators, and `@` symbols.
%
%   @arg I The input to be converted.
%   @arg O The converted Prolog-friendly output.
%
%   @example
%     ?- as_prolog(['Cons', 1, ['Cons', 2, []]], O).
%     O = [1, 2].
%
as_prolog(I, O) :-
    % Use default recursion depth of 10 and '&self' as context.
    as_prolog(0, '&self', I, O).

%!  as_prolog(+Depth, +Self, +I, -O) is det.
%
%   Converts a structured input (I) into a Prolog-friendly output (O). This predicate
%   handles various structures, such as lists, 'Cons', '::' operators, and '@' symbols.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg I     The input structure to be converted.
%   @arg O     The resulting Prolog-friendly output.
%
%   @example
%     ?- as_prolog(_, _, ['Cons', 1, ['Cons', 2, []]], O).
%     O = [1, 2].
%
%     ?- as_prolog(_, _, ['@', foo, a, b], O).
%     O = foo(a, b).
%

:- dynamic(dont_de_Cons/0).
dont_de_Cons.


acyclic_term_nat(I):- copy_term(I,O,_),acyclic_term(O).

as_prolog(0, _Slf, S, P):- S=='Nil', \+ dont_de_Cons, !,P=[].
as_prolog(_Dpth, _Slf, I, O) :- \+ compound(I), !, O = I.

as_prolog(0, Self, [Eval, Metta], Prolog) :- Eval == '!', !,
    Prolog = (eval(Metta,TF),is_true(TF)).

as_prolog(0, Self, [Cons, H, T | Nil], [HH | TT]) :-
    % Handle 'Cons' structures as lists.
    Cons=='Cons',
    Nil == [], \+ dont_de_Cons, !,
    as_prolog(0, Self, H, HH),
    as_prolog(0, Self, T, TT).
as_prolog(0, Self, [CC | List], O) :-
    % Handle '::' operator by mapping elements to Prolog terms.
    CC =='::',
    is_list(List), \+ dont_de_Cons, !,
    maplist(as_prolog(0, Self), List, L),
    !, O = L.

as_prolog(_, Self, exec(Eval), O) :- default_depth(DEFAULT_DEPTH),!,eval_args(DEFAULT_DEPTH, Self, Eval, O).
as_prolog(_, Self, quote(O), O) :- !.
as_prolog(_Dpth, _Slf, I, O) :-
    % If I is not a 'conz' structure, unify it directly with O.
    \+ iz_conz(I), !, I = O.

as_prolog(N, Self, [At| List], O) :-
    % Handle '@' symbol by constructing compound terms.
    At=='@', \+ dont_de_Cons,
    is_list(List), \+ dont_de_Cons,
    maplist(as_prolog(N, Self), List, [HH | L]),
    atom(HH), !,
    compound_name_arguments(O, HH, L).

as_prolog(_Depth, _Self, I, O) :- \+ acyclic_term_nat(I),!,nl,writeq(cyclic_I_term_BUG(I)),nl,nl,!,fail, I=O.
as_prolog(_Depth, _Self, I, O) :- \+ acyclic_term_nat(O),!,nl,writeq(cyclic_O_term_BUG(O)),nl,nl, I=O.
as_prolog(Depth, Self, I, O) :-
    % If I is a list, map each element to Prolog terms.
    is_list(I), !,
    maplist(as_prolog(Depth, Self), I, O).
as_prolog(Depth, Self, [H|T], [HH|TT]) :-
    % If is a list, map each element to Prolog terms.
    as_prolog(Depth, Self, H, HH),
    as_prolog(1, Self, T, TT), !.
as_prolog(_Dpth, _Slf, I, I).

%!  try_adjust_arg_types(+Eq, +RetType, +Depth, +Self, +Params, +X, -Y) is nondet.
%
%   Attempts to adjust argument types by converting the input (X) to a Prolog-friendly
%   format, ensuring the adjusted arguments conform to the expected types, and setting
%   the return type.
%
%   @arg Eq      The equation or relation being evaluated.
%   @arg RetType The expected return type.
%   @arg Depth   The current recursion depth.
%   @arg Self    The context or structure being evaluated.
%   @arg Params  The list of expected parameter types.
%   @arg X       The input arguments.
%   @arg Y       The adjusted arguments with the appropriate types.
%
%   @example
%     ?- try_adjust_arg_types(_, 'Number', 10, '&self', [int, int], ['Cons', 1, 2], Y).
%     Y = [1, 2].
%
try_adjust_arg_types(_Eq, RetType, Depth, Self, Params, X, Y) :-
    % Convert the input (X) to a Prolog-friendly format (M).
    as_prolog(Depth, Self, X, M),
    % Ensure the adjusted arguments conform to the expected parameter types.
    args_conform(try_adjust_arg_types, Depth, Self, M, Params), !,
    % Set the return type.
    set_type(Depth, Self, Y, RetType),
    % Convert the typed arguments back into the final output (Y).
    into_typed_args(Depth, Self, Params, M, Y).

%adjust_args(Else,Eq,RetType,Depth,Self,_,X,Y):- is_list(X), !, maplist(eval_args(Depth,Self),X,Y).
%adjust_args(Else,Eq,RetType,Depth,Self,_,X,Y):- is_list(X), !, maplist(as_prolog(Depth,Self),X,Y),!.

%!  adjust_args_9(+Eq, +RetType, +ResIn, -ResOut, +Depth, +Self, +AE, +More, -Adjusted) is det.
%
%   Wrapper around `adjust_args/10` that performs argument adjustments using the `eval` strategy.
%
%   @arg Eq        The equation or relation being evaluated.
%   @arg RetType   The expected return type.
%   @arg ResIn     The input result to be adjusted.
%   @arg ResOut    The output result after adjustments.
%   @arg Depth     The current recursion depth.
%   @arg Self      The context or structure being evaluated.
%   @arg AE        Additional input arguments.
%   @arg More      Additional output arguments.
%   @arg Adjusted  The final adjusted arguments.
%
adjust_args_9(Eq, RetType, ResIn, ResOut, Depth, Self, AE, More, Adjusted) :-
    show_failure_when(argtypes,
       rtrace_when(argtypes,adjust_args(eval, Eq, RetType, ResIn, ResOut, Depth, Self, AE, More, Adjusted))).


%!  adjust_args(+Else, +Eq, +RetType, +Res, -NewRes, +Depth, +Self, +Op, +X, -Y) is det.
%
%   Adjusts arguments for an operation using the provided strategy. If the arguments
%   conform to specific operator types, it applies transformations or falls back to alternatives.
%
%   @arg Else     The alternative strategy if the primary adjustment fails.
%   @arg Eq       The equation or relation being evaluated.
%   @arg RetType  The expected return type.
%   @arg Res      The input result to be adjusted.
%   @arg NewRes   The result after adjustment.
%   @arg Depth    The current recursion depth.
%   @arg Self     The context or structure being evaluated.
%   @arg Op       The operation being applied.
%   @arg X        The input arguments.
%   @arg Y        The final adjusted arguments.
%

adjust_args(_Else, _Eq, _RetType, Res, Res, _Dpth, Self, F, X, Y) :-
    % If the input is empty, or if it uses a special operator, or if X is not a conz structure.
    (X == [] ; is_special_op(Self, F) ; \+ iz_conz(X)), !,
    Y = X.
adjust_args( Else, Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y) :-
    % Attempt primary adjustment, and fall back if necessary.
    if_or_else(
        adjust_argsA(Else, Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y),
        adjust_argsB(Else, Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y)
    ).

%!  adjust_argsA(+Else, +Eq, +RetType, +Res, -NewRes, +Depth, +Self, +Op, +X, -Y) is nondet.
%
%   Primary strategy for adjusting arguments. It checks if the arguments conform
%   to the operator’s type definition and transforms them accordingly.
%
%   @arg Else      The alternative strategy if this adjustment fails.
%   @arg Eq        The equation or relation being evaluated.
%   @arg RetType   The expected return type.
%   @arg Res       The input result to be adjusted.
%   @arg NewRes    The adjusted result.
%   @arg Depth     The current recursion depth.
%   @arg Self      The context or structure being evaluated.
%   @arg Op        The operation being applied.
%   @arg X         The input arguments.
%   @arg Y         The final adjusted arguments.
%
adjust_argsA(Else, Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y):-
   if_or_else(adjust_argsA1(Else, Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y),
              adjust_argsA2(Else, Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y)).

adjust_argsA1(_Else,_Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y) :-
    len_or_unbound(X, Len),
    get_operator_typedef(Self, Op, Len, ParamTypes, RRetType),
    (nonvar(NewRes) -> CRes = NewRes ; CRes = Res),
    RRetType = RetType,
    args_conform(adjust_argsA1, Depth, Self, [CRes | X], [RRetType | ParamTypes]),
    trace_if_debug(Op,Len),
    into_typed_args(Depth, Self, [RRetType | ParamTypes], [Res | X], [NewRes | Y]).
adjust_argsA2(_Else,_Eq, RetType, Res, NewRes, Depth, Self, Op, X, Y) :-
    len_or_unbound(X, Len),
    get_operator_typedef(Self, Op, Len, ParamTypes, RRetType),
    (nonvar(NewRes) -> CRes = NewRes ; CRes = Res),
    RRetType = RetType,
    trace_if_debug(Op,Len),
    args_conform(adjust_argsA2, Depth, Self, [CRes | X], [RRetType | ParamTypes]),
    into_typed_args(Depth, Self, [RRetType | ParamTypes], [Res | X], [NewRes | Y]).

%!  adjust_argsB(+Else, +Eq, +RetType, +Res, -Res, +Depth, +Self, +Op, +Args, -Adjusted) is nondet.
%
%   Secondary adjustment strategy that evaluates each argument individually if possible.
%
%   @arg Else      The alternative strategy if needed.
%   @arg Eq        The equation or relation being evaluated.
%   @arg RetType   The expected return type.
%   @arg Res       The input result.
%   @arg Res       The result after adjustments.
%   @arg Depth     The current recursion depth.
%   @arg Self      The context or structure being evaluated.
%   @arg Op        The operation being applied.
%   @arg Args      The list of arguments to adjust.
%   @arg Adjusted  The final adjusted arguments.
%
adjust_argsB(Else, Eq, _RetType, Res, Res, Depth, Self, _, Args, Adjusted) :-
    % If Args is a list, evaluate each element.
    is_list(Args), !,
    maplist(eval_1_arg(Else, Eq, _, Depth, Self), Args, Adjusted).
adjust_argsB(Else, _Eq, _RetType, Res, Res, Depth, Self, _, X, Y) :-
    % Fallback to the alternative strategy.
    call(Else, X, Y).

%!  eval_1_arg(+Else, +Eq, +ReturnType, +Depth, +Self, +Arg, -Adjusted) is det.
%
%   Evaluates a single argument using the given equation and strategy.
%
%   @arg Else        The fallback strategy if evaluation fails.
%   @arg Eq          The equation or relation being evaluated.
%   @arg ReturnType  The expected return type of the evaluation.
%   @arg Depth       The current recursion depth.
%   @arg Self        The context or structure being evaluated.
%   @arg Arg         The argument to be evaluated.
%   @arg Adjusted    The result after evaluation.
%
eval_1_arg(Else, Eq, ReturnType, Depth, Self, Arg, Adjusted) :-
    (
        if_or_else(
            eval(Eq, ReturnType, Depth, Self, Arg, Adjusted),
            call(Else, Arg, Adjusted))).

%!  get_operator_typedef(+Self, +Op, +ParamTypes, -RetType) is det.
%
%   Retrieves the type definition of an operator, including its parameter types and return type.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg ParamTypes  The list of parameter types for the operator.
%   @arg RetType     The return type of the operator.
%
get_operator_typedef(Self, Op, ParamTypes, RetType) :-
    len_or_unbound(ParamTypes, Len),
    get_operator_typedef(Self, Op, Len, ParamTypes, RetType).


%!  reset_cache is det.
%
%   Clears the cached operator type definitions by retracting all facts
%   of `get_operator_typedef0/5`. This is used to reset the dynamic predicate
%   and ensure that new type definitions can be loaded or updated.
%
%   @example
%     ?- reset_cache.
%     true.  % All cached operator type definitions are cleared.
%
reset_cache :-
    % Retract all facts of the dynamic predicate get_operator_typedef0/5.
    retractall(get_operator_typedef0(_, _, _, _, _)).

% Declare get_operator_typedef0/5 as a dynamic predicate to allow modification at runtime.
:- dynamic(get_operator_typedef0/5).

%!  get_operator_typedef(+Self, +Op, +Len, -ParamTypes, -RetType) is nondet.
%
%   Retrieves the operator type definition for a given operator (Op). It first checks
%   if the type definition exists in the cached predicate `get_operator_typedef0/5`.
%   If not found, it attempts to retrieve it using `get_operator_typedef1/5` or
%   `get_operator_typedef2/5`.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg Len         The arity of the operator.
%   @arg ParamTypes  The list of parameter types.
%   @arg RetType     The return type of the operator.
%

get_operator_typedef(Self, Op, Len, ParamTypes, RetType):-
    quietly(get_operator_typedef_NR(Self, Op, Len, ParamTypes, RetType)).

get_operator_typedef_NR(Self, Op, Len, ParamTypes, RetType) :-
    no_repeats_var(NoRepeatType),
    % Ensure the length matches the parameter types or is unbound.
    len_or_unbound(ParamTypes, Len),
    % Try to retrieve the type definition from cache, or fallback to other strategies.
    if_or_else(
        get_operator_typedef0(Self, Op, Len, ParamTypes, RetType),
        get_operator_typedef1(Self, Op, Len, ParamTypes, RetType),
        get_operator_typedef2(Self, Op, Len, ParamTypes, RetType)),
    NoRepeatType = ParamTypes+RetType.

%!  get_operator_typedef_R(+Self, +Op, +Len, -ParamTypes, -RetType) is nondet.
%
%   _REALLY_ Retrieves the operator type definition based on the `'->'` type structure
%   and caches the result in `get_operator_typedef0/5`.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg Len         The arity of the operator.
%   @arg ParamTypes  The list of parameter types.
%   @arg RetType     The return type of the operator.
%
get_operator_typedef_R(Self, Op, Len, ParamTypes, RetType) :-
    % Ensure the length matches the parameter types or is unbound.
    len_or_unbound(ParamTypes, Len),
    % Build the type list for metta_type lookup.
    if_t(nonvar(ParamTypes), append(ParamTypes, [RetType], List)),
    % Check the type definition in metta_type/3.
    metta_type(Self, Op, ['->' | List]),
    % Cache the result for future lookups.
    if_t(var(ParamTypes), append(ParamTypes, [RetType], List)).

%!  get_operator_typedef1(+Self, +Op, +Len, -ParamTypes, -RetType) is nondet.
%
%   Retrieves the operator type definition based on the `'->'` type structure
%   and caches the result in `get_operator_typedef0/5`.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg Len         The arity of the operator.
%   @arg ParamTypes  The list of parameter types.
%   @arg RetType     The return type of the operator.
%

get_operator_typedef1(Self, Op, Len, ParamTypes, RetType) :-
    get_operator_typedef_R(Self, Op, Len, ParamTypes, RetType),
    assert(get_operator_typedef0(Self, Op, Len, ParamTypes, RetType)).

%!  get_operator_typedef2(+Self, +Op, +Len, -ParamTypes, -RetType) is nondet.
%
%   Retrieves the operator type definition by ensuring that all parameter types
%   are evaluation kinds and assigns a default return type `'AnyRet'`. The result
%   is cached in `get_operator_typedef0/5`.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg Len         The arity of the operator.
%   @arg ParamTypes  The list of parameter types.
%   @arg RetType     The return type of the operator (default is `'AnyRet'`).
%
get_operator_typedef2(Self, Op, Len, ParamTypes, RetType) :- symbol(Op),(symbol_concat(_,'!',Op);symbol_concat(_,'@',Op)),!,
    % Default return type is 'AnyRet'.
    ignore('Atom' = RetType),
    % Ensure all parameter types are valid evaluation kinds.
    maplist(=('Atom'), ParamTypes),
    % Cache the result for future lookups.
    assert(get_operator_typedef0(Self, Op, Len, ParamTypes, RetType)).
    % nop(wdmsg(missing(get_operator_typedef2(Self, Op, ParamTypes, RetType)))), !, fail.

get_operator_typedef2(Self, Op, Len, ParamTypes, RetType) :-
    % Default return type is 'AnyRet'.
    nop(ignore('AnyRet' = RetType)),
    % Ensure all parameter types are valid evaluation kinds.
    maplist(is_eval_kind, ParamTypes),
    % Cache the result for future lookups.
    assert(get_operator_typedef0(Self, Op, Len, ParamTypes, RetType)).
    % nop(wdmsg(missing(get_operator_typedef2(Self, Op, ParamTypes, RetType)))), !, fail.

%!  ignored_args_conform(+Why, +Depth, +Self, +Args, +List) is det.
%
%   Checks if the arguments (Args) conform to the expected types (List), but allows
%   non-specific types and ignores certain conditions.
%
%   @arg Why      The current reason calling this code
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Args  The list of arguments.
%   @arg List  The list of expected types or values.
%
ignored_args_conform(_Why, Depth, Self, A, L) :-
    % If either Args or List is not a conz structure, succeed without further checks.
    (\+ iz_conz(A); \+ iz_conz(L)), !.
ignored_args_conform(Why, Depth, Self, A, L) :-
    % Check if each argument conforms to its corresponding expected type.
    maplist(ignored_arg_conform(Why, Depth, Self), A, L).

%!  ignored_arg_conform(Why, +Depth, +Self, +Arg, +Expected) is det.
%
%   Checks if a single argument (Arg) conforms to the expected type (Expected),
%   allowing for non-specific types.
%
%   @arg Why      The current reason calling this code
%   @arg Depth    The current recursion depth.
%   @arg Self     The context or structure being evaluated.
%   @arg Arg      The argument to be checked.
%   @arg Expected The expected type or value.
%
ignored_arg_conform_1(_Why, Depth, Self, A, L) :-
    % Succeed if the expected type is a non-specific type.
    nonvar(L), is_nonspecific_type(L), !.
ignored_arg_conform_1(_Why, Depth, Self, A, L) :-
    % Check the argument type and verify it conforms to the expected type.
    get_type(Depth, Self, A, T),
    can_assign(T, L), !.

ignored_arg_conform(Why, Depth, Self, A, L):- show_failure_when(argtypes,ignored_arg_conform_1(Why, Depth, Self, A, L)),!.
ignored_arg_conform(_Why, Depth, Self, _, _) :- !.

%!  args_conform(+Why, +Depth, +Self, +Args, +List) is det.
%
%   Checks if the arguments (Args) conform to the expected types (List).
%
%   @arg Why      The current reason calling this code
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Args  The list of arguments.
%   @arg List  The list of expected types or values.
%
args_conform(_Why, _Depth, _Self, _, Nil):- Nil==[],!.
args_conform(_Why, _Depth, _Self, Nil, _):- Nil==[],!.
args_conform(_Why, _Dpth, _Slf, Args, List) :-
    % If either Args or List is not a conz structure, succeed without further checks.
    (\+ iz_conz(Args); \+ iz_conz(List)), !.
args_conform(Why, Depth, Self, [A | Args], [L | List]) :- !,
    % Check if the argument conforms and proceed with the rest of the list.
    show_failure_when(argtypes, arg_conform(Why, Depth, Self, A, L)),
    args_conform(Why, Depth, Self, Args, List).
%!  arg_conform(Why, +Depth, +Self, +Arg, +Expected) is det.
%
%   Checks if a single argument (Arg) conforms to the expected type (Expected).
%
%   @arg Why      The current reason calling this code
%   @arg Depth    The current recursion depth.
%   @arg Self     The context or structure being evaluated.
%   @arg Arg      The argument to be checked.
%   @arg Expected The expected type or value.
%

%arg_conform(Why, _Depth, Self, A, ParamType):- !, non_arg_violation_each(Self,ParamType, A).
arg_conform(_Why, _Dpth, _Slf, A, _L) :- var(A), !.
arg_conform(_Why, _Dpth, _Slf, _A, L) :-
    % Succeed if the expected type is a non-specific type.
    nonvar(L), is_nonspecific_type(L), !.
arg_conform( Why, Depth, Self,  A, L) :- var(L), Why == get_type, !, get_type(Depth, Self, A, L).
arg_conform(_Why, _Dpth, _Slf, _A, L) :- var(L), !.
arg_conform(Why, Depth, Self, A, L) :-
    % Check the argument type and verify it conforms to the expected type.
    get_type_each(Depth, Self, A, T), T \== 'Var',
    type_conform(T, L), !.
arg_conform(Why, _Dpth, _Slf, _, _):- Why\== get_type, !.
arg_conform(Why, Depth, Self, A, _) :- get_type(Depth, Self, A, _), !.

%!  type_conform(+Type, +Expected) is nondet.
%
%   Checks if a type (Type) conforms to the expected type (Expected).
%
%   @arg Type     The type to be checked.
%   @arg Expected The expected type.
%
type_conform(T, L) :-
    % Succeed if the types are equal.
    T = L, !.
type_conform(T, L) :- \+ is_nonspecific_type(T), \+ is_nonspecific_type(L), !, show_failure_when(argtypes,can_assign(T, L)).
type_conform(T, L) :- fail,
    % Succeed if either type is non-specific.
    \+ \+ (is_nonspecific_type(T); is_nonspecific_type(L)), !.
type_conform(T, L) :-
    % Succeed if the type can be assigned.
    show_failure_when(argtypes,can_assign(T, L)).


% Declare `thrown_metta_return/1` as dynamic to allow runtime modifications.
:- dynamic(thrown_metta_return/1).

%!  throw_metta_return(+L) is det.
%
%   Stores the given value (L) in a dynamic predicate and throws a `metta_return/1` exception.
%   This is used to handle specific return values in Metta logic by throwing an exception.
%
%   @arg L The value to be stored and thrown as an exception.
%
%   @example
%     ?- throw_metta_return(result).
%     ERROR: metta_return(result)
%
throw_metta_return(L) :-
    % Store the value in the dynamic predicate.
    asserta(thrown_metta_return(L)),
    % Throw an exception with the stored value.
    (throw(metta_return(L))).

%!  into_typed_args(+Depth, +Self, +Types, +Values, -TypedValues) is det.
%
%   Converts a list of values into typed arguments based on their corresponding types.
%   If either the list of types or values is not a conz structure, the values are unified directly.
%
%   @arg Depth        The current recursion depth.
%   @arg Self         The context or structure being evaluated.
%   @arg Types        The list of expected types.
%   @arg Values       The list of input values.
%   @arg TypedValues  The resulting list of typed values.
%
into_typed_args(_Dpth, _Slf, T, M, Y) :-
    % If either list is not a conz structure, unify the values directly.
    (\+ iz_conz(T); \+ iz_conz(M)), !, show_failure_when(argtypes,M = Y).

into_typed_args(Depth, Self, [T | TT], [M | MM], [Y | YY]) :-
    % Process each type-value pair.
    show_failure_when(argtypes,into_typed_arg(Depth, Self, T, M, Y)),
    into_typed_args(Depth, Self, TT, MM, YY).

:- nodebug(metta(argtypes)).
:- initialization(nodebug(metta(argtypes))).

%!  into_typed_arg(+Depth, +Self, +Type, +Value, -TypedValue) is det.
%
%   Converts a single value into a typed argument based on the given type.
%
%   @arg Depth       The current recursion depth.
%   @arg Self        The context or structure being evaluated.
%   @arg Type        The expected type of the value.
%   @arg Value       The input value to be typed.
%   @arg TypedValue  The resulting typed value.
%
into_typed_arg(_Dpth, Self, T, M, Y) :-
    % If the value is a variable, assign the type attribute and unify it.
    var(M),  !, show_failure_when(argtypes,(Y = M, dont_put_attr(M, cns, Self = [T]))).
into_typed_arg(Depth, Self, T, M, Y) :-
    % Use into_typed_arg0 for further evaluation or fallback to direct unification.
    if_or_else(show_failure_when(argtypes,into_typed_arg0(Depth, Self, T, M, Y)),
               show_failure_when(argtypes,(M = Y))).

%!  into_typed_arg0(+Depth, +Self, +Type, +Value, -TypedValue) is nondet.
%
%   Helper predicate for `into_typed_arg/5`. It evaluates the value based on its type.
%
%   @arg Depth       The current recursion depth.
%   @arg Self        The context or structure being evaluated.
%   @arg Type        The expected type.
%   @arg Value       The value to be evaluated.
%   @arg TypedValue  The resulting typed value.
%

into_typed_arg0(Depth, Self, T, M, Y) :- T=='Atom',!,M=Y.
into_typed_arg0(_Dpth, _Slf, T, M, Y) :-
    % If the type does not require evaluation, use the value directly.
    nonvar(T), is_non_eval_kind(T), !, M = Y.


into_typed_arg0(Depth, Self, T, M, Y):-
    if_or_else(into_typed_argA(Depth, Self, T, M, Y),
               into_typed_argB(Depth, Self, T, M, Y)).


into_typed_argA(Depth, Self, T, M, Y) :-
    % If the type is a variable, determine the value type and evaluate if needed.
    var(T),
    %no_repeats_var(NoRepeatY),
        ((  get_type(Depth, Self, M, T),
            (wants_eval_kind(T) -> eval_args_carefully(Depth, Self, M, Y) ; Y = M))).
    %NoRepeatY = Y.


into_typed_argB(Depth, Self, T, M, Y) :- nonvar(T),
    % If the type requires evaluation, evaluate the value.
    is_pro_eval_kind(T), !, eval_args_carefully(Depth, Self, M, Y),
    nop(( \+ arg_violation(Depth, Self, Y, T) )).

into_typed_argB(Depth, Self, T, M, Y) :- nonvar(T),
    % If the value is ground and conforms to the type, use it directly.
    ground(M), !, \+ arg_violation(Depth, Self, M, T), Y = M.

into_typed_argB(Depth, Self, _, M, Y) :-
    % Default case: evaluate the value.
    eval_args_carefully(Depth, Self, M, Y).

%!  wants_eval_kind(+Type) is nondet.
%
%   Determines if a type requires evaluation.
%
%   @arg Type The type to check.
%
wants_eval_kind(T) :-
    % If the type is a pro-eval kind, it requires evaluation.
    nonvar(T), is_pro_eval_kind(T), !.
wants_eval_kind(_) :- true.

%!  cns:attr_unify_hook(+Input, +NewValue) is det.
%
%   Handles the unification of a value with a `cns` attribute.
%
%   @arg Input     The context or structure being evaluated.
%   @arg NewValue  The value being unified with the attribute.
%
prevent_type_violations(Self, BecomingValue,RequireType):- non_arg_violation(Self, RequireType, BecomingValue).
%type_list_violations(BecomingValue,RequireType):- (RType,RequireType),non_arg_violation(_Self, RequireType, BecomingValue).

% TODO make sure it is inclusive rather than exclusive

cns:attr_unify_hook(_Slf=_TypeList,_NewValue):- nb_current(suspend_type_unificaton, true),!.
cns:attr_unify_hook(Self= TypeList, NewValue) :-
  show_failure_when(argtypes,maplist(prevent_type_violations(Self,NewValue),TypeList)),
  show_failure_when(argtypes,cns_attr_unify_hook(Self,TypeList, NewValue)).

cns_attr_unify_hook(Self,TypeList,NewValue) :-
    % If the new value is an attributed variable, assign the same attribute.
    attvar(NewValue), !, dont_put_attr(NewValue, cns, Self = TypeList).
cns_attr_unify_hook(Self , TypeList, NewValue) :-
    % Retrieve the type of the new value and check if it can be assigned.
    show_failure_when(argtypes,can_assign_value_typelist(Self, NewValue, TypeList)).

can_assign_value_typelist(Self, NewValue, TypeList):-
    default_depth(DEFAULT_DEPTH),get_type(DEFAULT_DEPTH, Self, NewValue, Was),
    must_det_lls(can_assign_value_typelist_4(Self, NewValue, Was, TypeList)).

can_assign_value_typelist_4(_Self, _NewValue, _Was, Nil):- Nil==[],!.
can_assign_value_typelist_4(_Self, _NewValue, Was, TypeList):-
    member(Type,TypeList),can_assign(Was,Type),!.
can_assign_value_typelist_4(Self, NewValue, _Was, TypeList):-
    with_output_to(user_error,(nl,display(var(NewValue)),nl)),
    if_t(debugging(metta(argtypes)),trace),
    var(NewValue), dont_put_attr(NewValue, cns, Self = TypeList).


%!  set_type(+Depth, +Self, +Var, +Type) is det.
%
%   Sets the type of a variable (Var) based on the given type (Type).
%   It ensures that the type is added to the list of known types if not already present.
%
%   @arg Depth The current recursion depth.
%   @arg Self  The context or structure being evaluated.
%   @arg Var   The variable whose type is being set.
%   @arg Type  The type to assign to the variable.
%
%   @example
%     ?- set_type(10, '&self', X, 'Number').
%     true.
%
%   @note The commented-out duplicate clause remains for reference.
%
% set_type(Depth, Self, Var, Type) :- nop(set_type(Depth, Self, Var, Type)), !.
% set_type(Depth, Self, Var, Type) :- freeze(Obj, get_type(D, Self, Obj,Type)),!.
set_type(Depth, Self, Var, Type) :-
    % Retrieve the current types of the variable.
    (get_types(Depth, Self, Var, TypeL) -> true ; TypeL = []),
    % Add the new type to the list if necessary.
    add_type(Depth, Self, Var, TypeL, Type).

%!  add_type(+Depth, +Self, +Var, +TypeList, +Type) is det.
%
%   Adds a new type to the list of known types for a variable, ensuring no duplicates.
%
%   @arg Depth    The current recursion depth.
%   @arg Self     The context or structure being evaluated.
%   @arg Var      The variable to update.
%   @arg TypeList The current list of types for the variable.
%   @arg Type     The new type to add.
%
add_type(_Depth, _Self, _Var, TypeL, Type) :-
    is_list(TypeL),
    % If the type is already in the list, do nothing.
    \+ \+ (member(E, TypeL), E == Type), !.
add_type(_Depth, Self, Var, TypeL, Type) :- var(Var), !,
    % Add the new type to the list and set it as an attribute.
    is_list(TypeL),
    append([Type], TypeL, TypeList),
    dont_put_attr(Var, cns, Self = TypeList).
add_type(_Depth, _Self, Var, TypeL, Type) :-
    ignore(append(_,[Type|_], TypeL)),!.
    % If the variable is not bound, do nothing.



%!  can_assign(+Was, +Type) is nondet.
%
%   Checks if a value of type `Was` can be assigned to a variable of type `Type`.
%   Assignment is allowed if either type is non-specific or if the two types are identical.
%
%   @arg Was  The actual type of the value.
%   @arg Type The expected type for assignment.
%
%   @example
%     ?- can_assign('Number', 'Number').
%     true.
%
can_assign(T, L) :-
    % Succeed if the types are equal.
    T == L, !.
can_assign(T, L) :- fail,
    % Succeed if either type is non-specific.
    \+ \+ (is_nonspecific_type(T); is_nonspecific_type(L)), !.
% If the types are identical, assignment is allowed.
can_assign(Was, Type) :- nonvar(Was),nonvar(Type), formated_data_type(Was),formated_data_type(Type),!,Type==Was.
% If the types are identical, assignment is allowed.
can_assign(Was, Type) :- Was = Type, !.
% If either type is non-specific, assignment is allowed.
can_assign(Was, Type) :- nonvar(Was),nonvar(Type), (is_nonspecific_type(Was); is_nonspecific_type(Type)), !.

%can_assign(Was,Type):- (Was=='Nat';Type=='Nat'),!,fail.
%can_assign(Was,Type):- \+ cant_assign_to(Was,Type).
%can_assign(_Ws,_Typ).
/*
cant_assign_to(Was,Type):- cant_assign(Was,Type),!.
cant_assign_to(Type,Was):- cant_assign(Was,Type),!.
cant_assign(A,B):- \+ A \= B, !, fail.
cant_assign(Number,String):- formated_data_type(Number),formated_data_type(String), Number\==String.
cant_assign(Number,Other):- formated_data_type(Number), symbol(Other), Number\==Other.
*/

%!  is_non_eval_kind(+Type) is nondet.
%
%   Checks if the given type (Type) does not require evaluation. A non-eval kind
%   includes specific non-variable types or the 'Atom' type.
%
%   @arg Type The type to check.
%
is_non_eval_kind(Var) :-
    % If the input is a variable, succeed.
    var(Var), !, fail.
is_non_eval_kind('Atom').
is_non_eval_kind('Expression').
% is_non_eval_kind('Variable').
is_non_eval_kind(Type) :-
    % If the type is non-variable, not 'Any', and non-specific, succeed.
    nonvar(Type), Type \== 'Any', is_nonspecific_type(Type), !.

%!  is_nonspecific_type(+Type) is nondet.
%
%   Succeeds if the given type (Type) is considered non-specific.
%
%   @arg Type The type to check.
%
is_nonspecific_type(Any) :-
    notrace(is_nonspecific_type0(Any)), !.

%!  is_nonspecific_type0(+Type) is nondet.
%
%   Helper predicate that defines non-specific types.
%
is_nonspecific_type0(Var) :-
    % Fail if the input is a variable.
    var(Var), !, fail.

is_nonspecific_type0('%Undefined%').
is_nonspecific_type0('ErrorType').
is_nonspecific_type0('Expression').
% is_nonspecific_type([]).
is_nonspecific_type0('Atom').
is_nonspecific_type0(Any) :-
    is_nonspecific_any(Any).

%!  formated_data_type(+Type) is nondet.
%
%   Checks if the given type is a recognized formatted data type.
%
%   @arg Type The type to check.
%
formated_data_type('Number').
formated_data_type('Symbol').
formated_data_type('Bool').
formated_data_type('Char').
formated_data_type('String').
formated_data_type([List | _]) :-
    List == 'List'.

%!  is_nonspecific_any(+Type) is nondet.
%
%   Succeeds if the given type is considered a non-specific "any" type.
%
%   @arg Type The type to check.
%
is_nonspecific_any(Any) :-
    notrace(is_nonspecific_any0(Any)), !.

%!  is_nonspecific_any0(+Type) is nondet.
%
%   Helper predicate that defines non-specific "any" types.
%
is_nonspecific_any0(Any) :-
    Any == 'Any'.
is_nonspecific_any0(Any) :-
    Any == '%Undefined%'.
% is_nonspecific_any0(Any) :- Any == 'Type'.
is_nonspecific_any0(Any) :-
    Any == 'AnyRet'.

%!  is_nonspecific_type_na(+Type) is nondet.
%
%   Checks if a type is non-specific but not 'Atom'.
%
%   @arg Type The type to check.
%
is_nonspecific_type_na(NotAtom) :-
    NotAtom \== 'Atom', is_nonspecific_type(NotAtom).

%!  narrow_types(+InputType, +ExpectedType, -ResultType) is det.
%
%   Narrows down the result type based on the input and expected types.
%
%   @arg InputType    The input type.
%   @arg ExpectedType The expected type.
%   @arg ResultType   The resulting narrowed type.
%
narrow_types(RetType, RetType, RetType) :- !.
narrow_types(Any, RetType, RetType) :-
    nonvar(Any), is_nonspecific_any(Any), !.
narrow_types(Any, RetType, RetType) :-
    nonvar(Any), is_nonspecific_any(Any), !.
narrow_types(Any, RetType, RetType) :-
    nonvar(Any), is_nonspecific_type_na(Any), !.
narrow_types(RetType, Any, RetType) :-
    nonvar(Any), is_nonspecific_type_na(Any), !.
narrow_types(RetType, Any, RetType) :-
    is_type_list(Any, List), !, narrow_types([RetType | List], Out).
narrow_types(Any, RetType, RetType) :-
    is_type_list(Any, List), !, narrow_types([RetType | List], Out).
narrow_types(Fmt, Fmt1, Fmt) :-
    formated_data_type(Fmt), formated_data_type(Fmt1).
narrow_types(Fmt, Fmt1, Fmt) :-
    formated_data_type(Fmt), !.
narrow_types(Fmt1, Fmt, Fmt) :-
    formated_data_type(Fmt), !.
narrow_types(Fmt1, Fmt2, 'NarrowTypeFn'(Fmt1, Fmt2)).

%!  is_type_list(+TypeFn, -List) is nondet.
%
%   Succeeds if the given type function (TypeFn) corresponds to a list of types.
%
%   @arg TypeFn The input type function.
%   @arg List   The resulting list of types.
%
is_type_list('NarrowTypeFn'(Fmt1, Fmt2), List) :-
    % Retrieve the type list for the type function.
    get_type_list('NarrowTypeFn'(Fmt1, Fmt2), List).

%!  get_type_list(+TypeFn, -List) is det.
%
%   Retrieves the list of types represented by the type function (TypeFn).
%
%   @arg TypeFn The input type function.
%   @arg List   The resulting list of types.
%
get_type_list('NarrowTypeFn'(Fmt1, Fmt2), List) :-
    !,
    % Recursively collect the types from both components.
    get_type_list(Fmt1, List1),
    get_type_list(Fmt2, List2),
    append(List1, List2, List).
get_type_list(A, [A]).

%!  narrow_types(+NL, -Out) is det.
%
%   Narrows down a list of types to a single type function or list of types.
%
%   @arg NL   The input list of types.
%   @arg Out  The narrowed type or type function.
%
narrow_types(NL, Out) :-
    % If NL is not a list, return it as a single-element list.
    \+ is_list(NL), !, Out = [NL].
narrow_types([A | List], Out) :-
    % Handle variables in the type list.
    var(A), !, narrow_types(List, LT), Out = 'NarrowTypeFn'(A, LT).
narrow_types([A, B | List], Out) :-
    % Recursively narrow down the remaining list.
    narrow_types([B | List], BL),
    narrow_types(A, BL, Out).
narrow_types([A], A).

%!  is_pro_eval_kind(+Type) is nondet.
%
%   Succeeds if the given type (Type) requires evaluation.
%
%   @arg Type The type to check.
%
is_pro_eval_kind(Var) :-
    % If the input is a variable, succeed.
    var(Var), !.
is_pro_eval_kind(A) :- is_non_eval_kind(A),!,fail.
is_pro_eval_kind(SDT) :- % Check if the type is a formatted data type.
    formated_data_type(SDT), !.
is_pro_eval_kind(A) :- % Fail for certain types.
    A == 'Atom', !, fail.
is_pro_eval_kind(A) :-
    A == '%Undefined%', !.
is_pro_eval_kind(A) :-
    % Check for non-specific "any" types.
    is_nonspecific_any(A), !.
is_pro_eval_kind(_A) :- !.

%!  is_feo_f(+F) is nondet.
%
%   Succeeds if the given function (F) is recognized as a first-order operator.
%
%   @arg F The function to check.
%
is_feo_f('Cons').

%!  is_seo_f(+F) is nondet.
%
%   Succeeds if the given function (F) is recognized as a second-order operator.
%
%   @arg F The function to check.
%
is_seo_f('{...}').
is_seo_f('[...]').
is_seo_f('{}').
is_seo_f('[]').
is_seo_f('StateMonad').
is_seo_f('State').
is_seo_f('Event').
is_seo_f('Concept').
is_seo_f(N) :-
    % Succeed if the input is a number.
    number(N), !.

:- if(\+ current_predicate(is_absorbed_return_type/2)).

%!  is_absorbed_return_type(+Params, +Var) is nondet.
%
%   Succeeds if the given return type (Var) is considered an absorbed return type.
%   Absorbed return types are those that do not propagate as valid return types.
%
%   @arg Params The parameters associated with the return type check.
%   @arg Var    The return type to be checked.
%
is_absorbed_return_type(Params, Var) :-
    % If Var is a variable, succeed if it is not a sub-variable of Params.
    var(Var), !, \+ sub_var_safely(Var, Params).
is_absorbed_return_type(_, 'Bool').
is_absorbed_return_type(_, [Ar]) :-
    % Succeed if the type is a single-element list containing '->'.
    !, Ar == (->).
is_absorbed_return_type(_, 'EmptyType').
is_absorbed_return_type(_, 'ReturnType').
is_absorbed_return_type(_, X) :-
    % Check if the type is recognized as a self-return type.
    is_self_return(X).

%!  is_self_return(+Type) is nondet.
%
%   Succeeds if the given type is a recognized self-return type.
%
%   @arg Type The type to check.
%
is_self_return('ErrorType').

%!  is_non_absorbed_return_type(+Params, +Var) is nondet.
%
%   Succeeds if the given return type (Var) is not an absorbed return type.
%
%   @arg Params The parameters associated with the return type check.
%   @arg Var    The return type to be checked.
%
is_non_absorbed_return_type(Params, Var) :-
    % Fail if the type is an absorbed return type.
    \+ is_absorbed_return_type(Params, Var).

:- endif.

%is_user_defined_goal(Self,[H|_]):- is_user_defined_head(Eq,Self,H).

%!  is_user_defined_head(+Other, +Head) is nondet.
%
%   Checks if the given head (H) is a user-defined head. This predicate uses `=` as the default
%   equality comparison.
%
%   @arg Other The context or structure being evaluated.
%   @arg Head  The head to be checked.
%
is_user_defined_head(Other, H) :-
    % Use `=` as the default equality comparison.
    is_user_defined_head(=, Other, H).

%!  is_user_defined_head(+Eq, +Other, +Head) is nondet.
%
%   Checks if the given head (H) is a user-defined head using the specified equality (Eq).
%
%   @arg Eq    The equality comparison to use.
%   @arg Other The context or structure being evaluated.
%   @arg Head  The head to be checked.
%
is_user_defined_head(Eq, Other, H) :-
    mnotrace(is_user_defined_head0(Eq, Other, H)).

%!  is_user_defined_head0(+Eq, +Other, +Head) is nondet.
%
%   Internal predicate that performs the user-defined head check with pattern matching.
%
%   @arg Eq    The equality comparison to use.
%   @arg Other The context or structure being evaluated.
%   @arg Head  The head to be checked.
%
is_user_defined_head0(Eq, Other, [H | _]) :-
    % If the head is in list form, check its first element.
    !, nonvar(H), !,
    is_user_defined_head_f(Eq, Other, H).
is_user_defined_head0(Eq, Other, H) :-
    % If the head is callable, extract its functor and check it.
    callable(H), !,
    functor(H, F, _, _),
    is_user_defined_head_f(Eq, Other, F).
is_user_defined_head0(Eq, Other, H) :-
    % Default case: directly check the head.
    is_user_defined_head_f(Eq, Other, H).

%!  is_user_defined_head_f(+Eq, +Other, +Head) is nondet.
%
%   Helper predicate that checks if the head is user-defined using the specified equality (Eq).
%
%   @arg Eq    The equality comparison to use.
%   @arg Other The context or structure being evaluated.
%   @arg Head  The head to be checked.
%
is_user_defined_head_f(Other, H) :-
    % Use `=` as the default equality comparison.
    is_user_defined_head_f(=, Other, H).
is_user_defined_head_f(Eq, Other, H) :-
    % Perform the user-defined head check.
    is_user_defined_head_f1(Eq, Other, H).
is_user_defined_head_f(Eq, Other, H) :-
    % If the head is in list form, check the first element.
    is_user_defined_head_f1(Eq, Other, [H | _]).
% is_user_defined_head_f1(Eq, Other, H) :- metta_type(Other, H, _).
% s_user_defined_head_f1(Other, H) :- get_metta_atom(Eq, Other, [H | _]).

%!  is_user_defined_head_f1(+Eq, +Other, +Head) is nondet.
%
%   Performs the core user-defined head check using equality (Eq).
%
%   @arg Eq    The equality comparison to use.
%   @arg Other The context or structure being evaluated.
%   @arg Head  The head to be checked.
%
is_user_defined_head_f1(Other, H) :-
    % Use `=` as the default equality comparison.
    is_user_defined_head_f1(=, Other, H).
is_user_defined_head_f1(Eq, Other, H) :-
    % Perform the equality-based user-defined head check.
    metta_eq_def(Eq, Other, [H | _], _).
% is_user_defined_head_f(Eq, _, H) :- is_metta_builtin(H).

%!  is_special_op(+Op) is nondet.
%
%   Checks if the given operator (Op) is a special operator in the current context.
%
%   @arg Op The operator to check.
%
is_special_op(Op) :-
    % Retrieve the current self context and check if the operator is special.
    current_self(Self),
    is_special_op(Self, Op).

%!  is_special_op(+Self, +Op) is nondet.
%
%   Checks if the given operator (Op) is a special operator based on the context (Self).
%
%   @arg Self The context or structure being evaluated.
%   @arg Op   The operator to check.
%
is_special_op(_Slf, F) :-
    % Fail if the operator is not an atom and not a variable.
    \+ atom(F), \+ var(F), !, fail.

% is_special_op(Self, Op) :-
%     % Check if the operator parameter types are non-evaluation kinds.
%     get_operator_typedef(Self, Op, Params, _RetType),
%     maplist(is_non_eval_kind, Params).
%
% is_special_op(_Self, Op) :-
%     % Check if the operator is a special built-in.
%     is_special_builtin(Op).

%!  is_eval_kind(+ParamType) is det.
%
%   Determines if the given parameter type (ParamType) requires evaluation.
%   It defaults to 'Any' if the parameter type is not provided.
%
%   @arg ParamType The parameter type to check.
%
is_eval_kind(ParamType) :-
    % Ignore unbound parameter types and assume 'Any' as default.
    ignore(ParamType = 'Any').

%!  is_metta_data_functor(+Eq, +F) is nondet.
%
%   Checks if the given functor (F) is a Metta data functor using the provided equality (Eq).
%
%   @arg Eq The equality relation to use.
%   @arg F  The functor to check.
%
is_metta_data_functor(Eq, F) :-
    % Retrieve the current self context and perform the functor check.
    current_self(Self),
    is_metta_data_functor(Eq, Self, F).

%!  get_operator_typedef(+Self, +Op, +ParamTypes, -RetType) is det.
%
%   Retrieves the type definition of an operator (Op) with its parameter types and return type.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg ParamTypes  The list of parameter types.
%   @arg RetType     The return type of the operator.
%
:- if(\+ current_predicate(get_operator_typedef/4)).
get_operator_typedef(Self, Op, ParamTypes, RetType) :-
    % Retrieve the operator type definition.
    quietly(get_operator_typedef(Self, Op, _, ParamTypes, RetType)).
:- endif.

%!  get_operator_typedef1(+Self, +Op, +ParamTypes, -RetType) is det.
%
%   Retrieves the first-level type definition of an operator (Op) with its parameter types
%   and return type.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg ParamTypes  The list of parameter types.
%   @arg RetType     The return type of the operator.
%
:- if(\+ current_predicate(get_operator_typedef1/4)).
get_operator_typedef1(Self, Op, ParamTypes, RetType) :-
    % Retrieve the first-level operator type definition.
    get_operator_typedef1(Self, Op, _, ParamTypes, RetType).
:- endif.

%!  get_operator_typedef(+Self, +Op, -Arity, +ParamTypes, -RetType) is det.
%
%   Retrieves the full type definition of an operator (Op) with its arity, parameter types,
%   and return type.
%
%   @arg Self        The context or structure being evaluated.
%   @arg Op          The operator whose type is being retrieved.
%   @arg Arity       The arity of the operator (can be ignored).
%   @arg ParamTypes  The list of parameter types.
%   @arg RetType     The return type of the operator.
%
:- if(\+ current_predicate(get_operator_typedef/5)).
get_operator_typedef(Self, Op, _, ParamTypes, RetType) :-
    % Retrieve the full operator type definition.
    quietly(get_operator_typedef(Self, Op, ParamTypes, RetType)).
:- endif.

%!  is_special_builtin(+Builtin) is nondet.
%
%   Checks if the given Builtin is a recognized special built-in MeTTa predicate.
%   These built-ins represent various control structures and operations
%   that extend beyond standard Prolog predicates.
%
%   @arg Builtin The name of the special built-in predicate to be checked.
%
%   @example
%     ?- is_special_builtin('if').
%     true.
%
is_special_builtin('case').
%is_special_builtin(':').
%is_special_builtin('=').
%is_special_builtin('->').
is_special_builtin('bind!').
%is_special_builtin('new-space').
is_special_builtin('let').
is_special_builtin('let*').
is_special_builtin('if').
is_special_builtin('rtrace').
is_special_builtin('or').
is_special_builtin('and').
is_special_builtin('not').
is_special_builtin('match').
is_special_builtin('call').
is_special_builtin('let').
is_special_builtin('let*').
is_special_builtin('nop').
is_special_builtin('assertEqual').
is_special_builtin('assertEqualToResult').
is_special_builtin('collapse').
is_special_builtin('superpose').
%is_special_builtin('==').

%!  is_metta_builtin(+Builtin) is nondet.
%
%   Succeeds if the given Builtin is recognized as a MeTTa built-in predicate or operator.
%   This includes both special built-ins and common operators used in metta logic.
%
%   @arg Builtin The name of the metta built-in predicate or operator to be checked.
%
%   @example
%     ?- is_metta_builtin('+').
%     true.
%
%     ?- is_metta_builtin('unknown_op').
%     false.
%
is_metta_builtin(Special):- is_special_builtin(Special).
is_metta_builtin('==').
is_metta_builtin(F):- once(atom(F);var(F)), current_op(_,yfx,F).
is_metta_builtin('println!').
is_metta_builtin('transfer!').
is_metta_builtin('compile!').
is_metta_builtin('+').
is_metta_builtin('-').
is_metta_builtin('*').
is_metta_builtin('/').
is_metta_builtin('%').
is_metta_builtin('==').
is_metta_builtin('<').
is_metta_builtin('>').
is_metta_builtin('all').
is_metta_builtin('import!').
is_metta_builtin('pragma!').

%!  is_comp_op(+Operator, +Arity) is nondet.
%
%   Checks if the given Operator is a valid comparison operator in Prolog with
%   the specified arity. These operators are used for comparing terms and
%   evaluating arithmetic expressions.
%
%   @arg Operator The comparison operator to be checked.
%   @arg Arity    The arity (number of arguments) of the operator.
%
%   @example
%     ?- is_comp_op('\\==', 2).
%     true.
%
% is_comp_op('=', 2).          % Unification
is_comp_op('\\=', 2).        % Not unifiable
is_comp_op('==', 2).         % Strict equality
is_comp_op('\\==', 2).       % Strict inequality
is_comp_op('@<', 2).         % Term is before
is_comp_op('@=<', 2).        % Term is before or equal
is_comp_op('@>', 2).         % Term is after
is_comp_op('@>=', 2).        % Term is after or equal
is_comp_op('=<', 2).         % Less than or equal
is_comp_op('<', 2).          % Less than
is_comp_op('>=', 2).         % Greater than or equal
is_comp_op('>', 2).          % Greater than
is_comp_op('is', 2).         % Arithmetic equality
is_comp_op('=:=', 2).        % Arithmetic exact equality
is_comp_op('=\\=', 2).       % Arithmetic inequality

%!  is_math_op(+Operator, +Arity, -Status) is nondet.
%
%   Checks if the given Operator is a recognized arithmetic or mathematical operation
%   with the specified arity. The status `exists` indicates that the operation is supported.
%
%   @arg Operator The arithmetic operator or function to be checked.
%   @arg Arity    The arity (number of arguments) of the operator or function.
%   @arg Status   The status of the operation, typically unified with `exists`.
%
%   @example
%     ?- is_math_op('+', 2, Status).
%     Status = exists.
%
%     ?- is_math_op('sqrt', 1, Status).
%     Status = exists.
%
is_math_op('*', 2, exists).         % Multiplication
is_math_op('**', 2, exists).        % Exponentiation
is_math_op('+', 1, exists).         % Unary Plus
is_math_op('+', 2, exists).         % Addition
is_math_op('-', 1, exists).         % Unary Minus
is_math_op('-', 2, exists).         % Subtraction
is_math_op('.', 2, exists).         % Array Indexing or Member Access (Depends on Context)
is_math_op('/', 2, exists).         % Division
is_math_op('//', 2, exists).        % Floor Division
is_math_op('///', 2, exists).       % Alternative Division Operator (Language Specific)
is_math_op('/\\', 2, exists).       % Bitwise AND
is_math_op('<<', 2, exists).        % Bitwise Left Shift
is_math_op('>>', 2, exists).        % Bitwise Right Shift
is_math_op('\\', 1, exists).        % Bitwise NOT
is_math_op('\\/', 2, exists).       % Bitwise OR
is_math_op('^', 2, exists).         % Bitwise XOR
is_math_op('abs', 1, exists).       % Absolute Value
is_math_op('acos', 1, exists).      % Arc Cosine
is_math_op('acosh', 1, exists).     % Hyperbolic Arc Cosine
is_math_op('asin', 1, exists).      % Arc Sine
is_math_op('asinh', 1, exists).     % Hyperbolic Arc Sine
is_math_op('atan', 1, exists).      % Arc Tangent
is_math_op('atan2', 2, exists).     % Two-Argument Arc Tangent
is_math_op('atanh', 1, exists).     % Hyperbolic Arc Tangent
is_math_op('cbrt', 1, exists).      % Cube Root
is_math_op('ceil', 1, exists).      % Ceiling Function
is_math_op('ceiling', 1, exists).   % Ceiling Value
is_math_op('cmpr', 2, exists).      % Compare Two Values (Language Specific)
is_math_op('copysign', 2, exists).  % Copy the Sign of a Number
is_math_op('cos', 1, exists).       % Cosine Function
is_math_op('cosh', 1, exists).      % Hyperbolic Cosine
is_math_op('cputime', 0, exists).   % CPU Time
is_math_op('degrees', 1, exists).   % Convert Radians to Degrees
is_math_op('denominator', 1, exists). % Get Denominator of Rational Number
is_math_op('div', 2, exists).       % Integer Division
is_math_op('e', 0, exists).         % Euler Number
is_math_op('epsilon', 0, exists).   % Machine Epsilon
is_math_op('erf', 1, exists).       % Error Function
is_math_op('erfc', 1, exists).      % Complementary Error Function
is_math_op('eval', 1, exists).      % Evaluate Expression
is_math_op('exp', 1, exists).       % Exponential Function
is_math_op('expm1', 1, exists).     % exp(x) - 1
is_math_op('fabs', 1, exists).      % Absolute Value (Floating-Point)
is_math_op('float', 1, exists).     % Convert Rational to Float
is_math_op('float_fractional_part', 1, exists). % Fractional Part of Float
is_math_op('float_integer_part', 1, exists).    % Integer Part of Float
is_math_op('floor', 1, exists).     % Floor Value
is_math_op('fmod', 2, exists).      % Floating-Point Modulo Operation
is_math_op('frexp', 2, exists).     % Get Mantissa and Exponent
is_math_op('fsum', 1, exists).      % Accurate Floating Point Sum
is_math_op('gamma', 1, exists).     % Gamma Function
is_math_op('gcd', 2, exists).       % Greatest Common Divisor
is_math_op('getbit', 2, exists).    % Get Bit at Position
is_math_op('hypot', 2, exists).     % Euclidean Norm, Square Root of Sum of Squares
is_math_op('inf', 0, exists).       % Positive Infinity
is_math_op('integer', 1, exists).   % Convert Float to Integer
is_math_op('isinf', 1, exists).     % Check for Infinity
is_math_op('isnan', 1, exists).     % Check for Not a Number
is_math_op('lcm', 2, exists).       % Least Common Multiple
is_math_op('ldexp', 2, exists).     % Load Exponent of a Floating Point Number
is_math_op('lgamma', 1, exists).    % Log Gamma
is_math_op('log', 1, exists).       % Logarithm Base e
is_math_op('log10', 1, exists).     % Base 10 Logarithm
is_math_op('log1p', 1, exists).     % log(1 + x)
is_math_op('log2', 1, exists).      % Base 2 Logarithm
is_math_op('lsb', 1, exists).       % Least Significant Bit
is_math_op('max', 2, exists).       % Maximum of Two Values
is_math_op('maxr', 2, exists).      % Maximum Rational Number (Language Specific)
is_math_op('min', 2, exists).       % Minimum of Two Values
is_math_op('minr', 2, exists).      % Minimum Rational Number (Language Specific)
is_math_op('mod', 2, exists).       % Modulo Operation
is_math_op('modf', 2, exists).      % Return Fractional and Integer Parts
is_math_op('msb', 1, exists).       % Most Significant Bit
is_math_op('nan', 0, exists).       % Not a Number
is_math_op('nexttoward', 2, exists). % Next Representable Floating-Point Value
is_math_op('numerator', 1, exists). % Get Numerator of Rational Number
is_math_op('pi', 0, exists).        % Pi
is_math_op('popcount', 1, exists).  % Count of Set Bits
is_math_op('pow', 2, exists).       % Exponentiation
is_math_op('powm', 3, exists).      % Modulo Exponentiation
is_math_op('radians', 1, exists).   % Convert Degrees to Radians
is_math_op('remainder', 2, exists). % Floating-Point Remainder
is_math_op('remquo', 3, exists).    % Remainder and Part of Quotient
is_math_op('round', 1, exists).     % Round to Nearest Integer
is_math_op('roundeven', 1, exists). % Round to Nearest Even Integer
is_math_op('setbit', 2, exists).    % Set Bit at Position
is_math_op('signbit', 1, exists).   % Sign Bit of Number
is_math_op('sin', 1, exists).       % Sine Function
is_math_op('sinh', 1, exists).      % Hyperbolic Sine
is_math_op('sqrt', 1, exists).      % Square Root
is_math_op('tan', 1, exists).       % Tangent Function
is_math_op('tanh', 1, exists).      % Hyperbolic Tangent
is_math_op('testbit', 2, exists).   % Test Bit at Position
is_math_op('trunc', 1, exists).     % Truncate Decimal to Integer
is_math_op('ulogb', 1, exists).     % Unbiased Exponent of a Floating-Point Value
is_math_op('xor', 2, exists).       % Exclusive OR
is_math_op('zerop', 1, exists).     % Test for Zero

% :- load_pfc_file('metta_ontology.pl.pfc').

:- ensure_loaded(metta_typed_functions).

:- find_missing_cuts.

