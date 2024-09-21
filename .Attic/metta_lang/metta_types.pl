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
typed_list(Cmpd,Type,List):-  compound(Cmpd), Cmpd\=[_|_], compound_name_arguments(Cmpd,Type,[List|_]),is_list(List).
is_syspred(H,Len,Pred):- notrace(is_syspred0(H,Len,Pred)).
is_syspred0(H,_Ln,_Prd):- \+ atom(H),!,fail.
is_syspred0(H,_Ln,_Prd):- upcase_atom(H,U),downcase_atom(H,U),!,fail.
is_syspred0(H,Len,Pred):- current_predicate(H/Len),!,Pred=H.
is_syspred0(H,Len,Pred):- atom_concat(Mid,'!',H), H\==Mid, is_syspred0(Mid,Len,Pred),!.
is_syspred0(H,Len,Pred):- atom_concat(Mid,'-p',H), H\==Mid, is_syspred0(Mid,Len,Pred),!.
is_syspred0(H,Len,Pred):- atom_concat(Mid,'-fn',H), H\==Mid, is_syspred0(Mid,Len,Pred),!.
is_syspred0(H,Len,Pred):- into_underscores(H,Mid), H\==Mid, is_syspred0(Mid,Len,Pred),!.
%is_function(F):- atom(F).
is_metta_data_functor(_Eq,_Othr,H):-
 bt,trace, clause(is_data_functor(H),_).
is_metta_data_functor(Eq,Other,H):- H\=='Right', H\=='Something',
 % metta_type(Other,H,_), % fail,
  \+ get_metta_atom(Eq,Other,[H|_]),
  \+ metta_eq_def(Eq,Other,[H|_],_),
  \+ is_metta_builtin(H),
  \+ is_comp_op(H,_),
  \+ is_math_op(H,_,_).


:- if( \+ current_predicate(mnotrace/1) ).
 mnotrace(G):- once(G).
:- endif.

'Number':attr_unify_hook(_,NewValue):- numeric(NewValue).

%is_decl_type(ST):- metta_type(_,_,[_|Type]),is_list(Type),sub_sterm(T,Type),nonvar(T),T=@=ST, \+ nontype(ST).

is_decl_utype('%Undefined%').
is_decl_utype('Number').
is_decl_utype('Symbol').
is_decl_utype('Expression').
is_decl_utype('String').
is_decl_utype('Bool').
is_decl_utype('Type').
is_decl_utype('Any').
is_decl_utype('Atom').
%is_decl_utype(Type):- is_decl_type_l(Type).
is_decl_mtype('Variable').
is_decl_mtype('Number').
is_decl_mtype('Symbol').
is_decl_mtype('Expression').
is_decl_mtype('Grounded').
is_decl_mtype('PyObject').

%is_decl_type([ST|_]):- !, atom(ST),is_decl_type_l(ST).
%is_decl_type(ST):- \+ atom(ST),!,fail.
is_decl_type(Type):- is_decl_utype(Type).
is_decl_type(Type):- is_decl_type_l(Type).
is_decl_type([Type,SType]):- is_decl_type_l(Type),is_decl_utype(SType).

is_decl_type_l('StateMonad'). is_decl_type_l('List').

last_type(List,Type):- is_list(List),last(List,Type),is_type(Type).
last_type(Type,Type):- is_type(Type),!.

is_type(Type):- nontype(Type),!,fail.
is_type(Type):- is_decl_type(Type).
%is_type(Type):- atom(Type).

nontype(Type):- var(Type),!.
nontype('->').
nontype(N):- number(N).

needs_eval(EvalMe):- is_list(EvalMe).


args_violation(_Dpth,_Slf,Args,List):- ( \+ iz_conz(Args); \+ iz_conz(List)), !, fail.
args_violation(Depth,Self,[A|Args],[L|List]):-
 once( arg_violation(Depth,Self,A,L) ;
  args_violation(Depth,Self,Args,List)).

arg_violation(Depth,Self,A,L):-
   \+ (get_type_equals(Depth,Self,A,T), \+ type_violation(T,L)).
%arg_violation(Depth,Self,A,_):- get_type(Depth,Self,A,_),!.

type_violation(T,L):- \+ \+ (is_nonspecific_type(T);is_nonspecific_type(L)),!,fail.
type_violation(T,L):- T\=L.


not_arg_violation(Depth,Self,Arg,Type):-
   arg_conform(Depth,Self,Arg,Type),
   \+ arg_violation(Depth,Self,Arg,Type).


get_types(Depth,Self,Var,TypeSet):-
   setof(Type,get_type_each(Depth,Self,Var,Type),TypeSet).

get_type_equals(_Depth,_Self,Var,TypeO):- var(Var),var(TypeO),!.
get_type_equals(Depth,Self,Var,TypeO):- get_type(Depth,Self,Var,TypeO).

%if_or_else(get_type(Depth,Self,Val,Type),Type='%Undefined%'),

get_type(Depth,Self,Val,TypeO):-
   no_repeats_var(NoRepeatType),
   get_type_each(Depth,Self,Val,Type),
   NoRepeatType=Type,
   Type=TypeO,
   (return_only_first_type->!;true).

return_only_first_type:- true_flag.

is_space_type(Space,is_asserted_space):- was_asserted_space(Space),!.
is_space_type(Space,Test):- no_repeats(Test,space_type_method(Test,_,_)),call(Test,Space),!.

is_state_type(State,Test):- no_repeats(Test,state_type_method(Test,_,_)),call(Test,State),!.

%is_dynaspace(Expr):- \+ is_list(Expr), callable(Expr), is_space_type(Expr,_).
is_dynaspace(S):- var(S),!,fail.
is_dynaspace(S):- was_asserted_space(S).
is_dynaspace(S):- py_named_space(S).
is_dynaspace(S):- typed_list(S,'hyperon::space::DynSpace',_).
%  fake_notrace( is_space_type(Expr,_)),!.

is_PyObject(S):- py_is_py(S).
is_PyObject(S):- var(S),!,fail.
is_PyObject('@'(S)):- !, nonvar(S), is_py_const(S).
is_py_const('None').
is_py_const('False').
is_py_const('True').


get_type_each(_, _, Nil, UD):- Nil==[],!,UD='%Undefined%'.
get_type_each(Depth,Self,Val,Type):- \+ integer(Depth),!,get_type_each(10,Self,Val,Type).
get_type_each(_Depth,_Slf,Val,PyObject):- is_PyObject(Val),!,'PyObject'=PyObject.
get_type_each(Depth,_Slf,_Type,_):- Depth<1,!, fail.
%get_type(Depth,Self,Val,Type):- is_debugging(eval),
% ftrace(get_type_each(Depth,Self,Val,Type)),
% fail.
get_type_each(Depth,Self,Expr,['StateMonad',Type]):-
  notrace( is_valid_nb_state(Expr)),!,
  if_or_else(state_decltype(Expr,Type),nonvar(Type)),
   ('get-state'(Expr,Val),!,Depth2 is Depth-1,
      get_value_type(Depth2,Self,Val,Type)).

get_type_each(_Dpth,Self,Var,Type):- var(Var),!,
 get_attr(Var,metta_type,Self=TypeList),member(Type,TypeList).

get_type_each(_Dpth,_Slf,Expr,'hyperon::space::DynSpace'):- is_dynaspace(Expr),!.
get_type_each(Depth,Self,Val,Type):- \+ compound(Val),!, get_type_nc(Depth,Self,Val,Type).

get_type_each(Depth,Self,Val,Type):-
 if_t(option_value('type-check',auto),check_bad_type(Depth,Self,Val)),
 if_or_else((get_type_cmpd_2nd_non_nil(Depth,Self,Val,Type,How),trace_get_type(How,Type,gt(Val))),
    (trace_get_type('FAILED','',gt(Val)),fail)).

get_type_cmpd_2nd_non_nil(Depth,Self,Val,Type,How):-
  call_nth(get_type_cmpd(Depth,Self,Val,Type,How),Nth),
  (Nth>1 -> Type\==[] ; true).
/*
have_some_defs(Depth,Self,Val):-
  \+ \+
 ([H|Args] = Val,
  metta_type(Eq,H,[Ar|ArgTypes]),Ar=='->',
  append(ParamTypes,[RType],ArgTypes),
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
   args_conform(Depth,Self,Args,ArgTypes),
   (args_violation(Depth,Self,Args,ArgTypes) ->
    (trace_get_type(bad_type,args_violation(Args,ArgTypes),check),fail);
    (trace_get_type(conformed,no_args_violation(Args,ArgTypes),check),true)).
*/
typed_expression(Depth,Self,[Op|Args],ArgTypes,RType):-
   len_or_unbound(Args,Len),
   get_operator_typedef1(Self,Op,Len,ArgTypes,RType).

badly_typed_expression(Depth,Self,[Op|Args]):-
  typed_expression(Depth,Self,[Op|Args],ArgTypes,RType),
  can_assign(RetType,RType),
  args_violation(Depth,Self,Args,ArgTypes),
  !.

:- nodebug(metta(types)).
:- nodebug(types).
trace_get_type(How,Type,Val):-
  if_trace(types,
    color_g_mesg('#7f2f2f',
       w_indent(3,format('<-- ~@ <- ~@ < ~@',[wsf(How),wsf(Type),wsf(Val)])))),!.
wsf(T):- with_indents(false,write_src(T)).

get_type_nc(_Dpth,Self,Op,Type):- metta_type(Self,Op,Type).
get_type_nc(Dpth,Slf,Val,Type):- symbol(Val),!,get_type_symb(Dpth,Slf,Val,Type).
get_type_nc(_Dpth,_Slf,Val,'String'):- string(Val),!.
%get_type_nc(_Dpth,_Slf,Val,Type):- py_is_object(Val),py_type(Val,Type).
get_type_nc(_Dpth,_Slf,Val,'Number'):- number(Val).
get_type_nc(_Dpth,_Slf,Val,'Integer'):- integer(Val),!, specialize_number.
get_type_nc(_Dpth,_Slf,Val,'Decimal'):- float(Val),!, specialize_number.
get_type_nc(_Dpth,_Slf,Val,'Rational'):- rational(Val),!.

specialize_number:- false_flag.

get_type_symb(_Dpth,_Slf,Val,'Bool'):- (Val=='False';Val=='True').
get_type_symb(_Dpth,_Slf,Val,'Type'):- is_decl_type(Val).
get_type_symb(_Dpth,_Slf,Val,Type):- symbolic_list_concat([Type,_|_],'@',Val).
get_type_symb(_Dpth,_Slf,Val,Type):- symbolic_list_concat([Type,_|_],':',Val).
get_type_symb(Depth,Self,Op,Type):- % defined symbol
  Depth2 is Depth-1, eval_args(Depth2,Self,Op,Val),Op\=@=Val,!,
  get_type(Depth2,Self,Val,Type).

get_dict_type(_Vl,Type,TypeO):- nonvar(Type),TypeO=Type.
get_dict_type(Val,_,Type):-  get_dict(Val,type,Type).
get_dict_type(Val,_,TypeO):- get_dict(Val,class,Type).
get_dict_type(Val,_,TypeO):- get_dict(Val,types,TypeL),
   is_list(TypeL),member(Type,TypeL).


%get_type_cmpd(_Dpth,Self,Op,Type):- copy_term(Op,Copy),
%  metta_type(Self,Op,Type), Op=@=Copy.

get_type_cmpd(_Dpth,_Slf,Val,Type,dict):- is_dict(Val,Type),!,
  get_dict_type(Val,Type,TypeO).

get_type_cmpd(_Dpth,_Slf,'$VAR'(_),'Var',functorV):- !.
get_type_cmpd(_Dpth,_Slf,'#\\'(_),'Char',functor):- !.

% Curried Op
get_type_cmpd(Depth,Self,[[Op|Args]|Arg],Type,curried(W)):-
 symbol(Op),
 Depth2 is Depth-1,
 get_type_cmpd(Depth2,Self,[Op|Args],Type1,W),
 get_type(Depth2,Self,Arg,ArgType),
 ignore(sub_var(ArgType,Type1)->true;
   (sub_term(ST,Type1),var(ST),ST=ArgType)),
 last(Type1,Type).



get_type_cmpd(Depth,Self,[Op|Args],Type,ac(Op,[P|Arams],RetType)):- symbol(Op),
  len_or_unbound(Args,Len),
  get_operator_typedef1(Self,Op,Len,[P|Arams],RetType),
  % Fills in type variables when possible
  args_conform(Depth,Self,Args,[P|Arams]),
  % \+ maplist(var,Arams),
  % unitests:  arg violations should return ()
  (\+ args_violation(Depth,Self,Args,[P|Arams])),
  Type=RetType.


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
    eval_args(Depth2,Self,EvalMe,Val),
    get_type_cmpd_eval(Depth2,Self,EvalMe,Val,Type,Eval_First).

get_type_cmpd(_Dpth,_Slf,_Cmpd,[],unknown).


get_type_cmpd_eval(Depth2,Self,EvalMe,Val,Type,maplist(get_type)):- !,
    EvalMe =@=Val,
    maplist(get_type(Depth2,Self),List,Type),
    \+ badly_typed_expression(Depth,Self,Type).

get_type_cmpd_eval(Depth2,Self,EvalMe,Val,Type,eval_first):- !,
    \+ needs_eval(Val), get_type(Depth2,Self,Val,Type).

get_type_cmpd_eval(Depth2,Self,_EvalMe,Val,Type,eval_first_reduced):- !,
    get_type(Depth2,Self,Val,Type).


state_decltype(Expr,Type):- functor(Expr,_,A),
  arg(A,Expr,Type),once(var(Type);is_decl_type(Type)).


get_value_type(_Dpth,_Slf,Var,'%Undefined%'):- var(Var),!.
get_value_type(_Dpth,_Slf,Val,'Number'):- number(Val),!.
get_value_type(_Dpth,_Slf,Val,'String'):- string(Val),!.
get_value_type(Depth,Self,Val,T):- get_type(Depth,Self,Val,T), T\==[], T\=='%Undefined%',!.
get_value_type(_Dpth,_Slf,Val,T):- 'get-metatype'(Val,T).

/*

get_value_type(Depth,Self,EvalMe,Type):- needs_eval(EvalMe),
     eval_args(Depth,Self,EvalMe,Val), \+ needs_eval(Val),!,
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


as_prolog(I,O):- as_prolog(10,'&self',I,O).
as_prolog(_Dpth,_Slf,I,O):- \+ iz_conz(I),!,I=O.
as_prolog(Depth,Self,[Cons,H,T],[HH|TT]):- Cons=='Cons',!,as_prolog(Depth,Self,H,HH),as_prolog(Depth,Self,T,TT).
as_prolog(Depth,Self,[List,H|T],O):- List=='::',!,maplist(as_prolog(Depth,Self),[H|T],L),!, O = L.
as_prolog(Depth,Self,[At,H|T],O):- At=='@',!,maplist(as_prolog(Depth,Self),[H|T],[HH|L]),atom(H),!, O =.. [HH|L].
as_prolog(Depth,Self,I,O):- is_list(I),!,maplist(as_prolog(Depth,Self),I,O).
as_prolog(_Dpth,_Slf,I,I).


try_adjust_arg_types(_Eq,RetType,Depth,Self,Params,X,Y):-
  as_prolog(Depth,Self,X,M),
  args_conform(Depth,Self,M,Params),!,
  set_type(Depth,Self,Y,RetType),
  into_typed_args(Depth,Self,Params,M,Y).
%adjust_args(Else,Eq,RetType,Depth,Self,_,X,Y):- is_list(X), !, maplist(eval_args(Depth,Self),X,Y).
%adjust_args(Else,Eq,RetType,Depth,Self,_,X,Y):- is_list(X), !, maplist(as_prolog(Depth,Self),X,Y),!.

adjust_args_9(Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted):-
  adjust_args(eval,Eq,RetType,ResIn,ResOut,Depth,Self,AE,More,Adjusted).

adjust_args(Else,_Eq,_RetType,Res,Res,_Dpth,Self,F,X,Y):- (X==[] ;
    is_special_op(Self,F); \+ iz_conz(X)),!,Y=X.
adjust_args(Else,Eq,RetType,Res,NewRes,Depth,Self,Op,X,Y):-
    if_or_else(adjust_argsA(Else,Eq,RetType,Res,NewRes,Depth,Self,Op,X,Y),
       adjust_argsB(Else,Eq,RetType,Res,NewRes,Depth,Self,Op,X,Y)).

adjust_argsA(Else,Eq,RetType,Res,NewRes,Depth,Self,Op,X,Y):-
  len_or_unbound(X,Len),
  get_operator_typedef(Self,Op,Len,ParamTypes,RRetType),
  (nonvar(NewRes)->CRes=NewRes;CRes=Res),
  RRetType = RetType,
  args_conform(Depth,Self,[CRes|X],[RRetType|ParamTypes]),
  into_typed_args(Depth,Self,[RRetType|ParamTypes],[Res|X],[NewRes|Y]).

adjust_argsB(Else,Eq,_RetType,Res,Res,Depth,Self,_,Args,Adjusted):- is_list(Args),!,
  maplist(eval_1_arg(Else,Eq,_,Depth,Self),Args,Adjusted).
adjust_argsB(Else,_Eq,_RetType,Res,Res,Depth,Self,_,X,Y):- call(Else,X,Y). % as_prolog(Depth,Self,X,Y),!.

eval_1_arg(Else,Eq,ReturnType,Depth,Self,Arg,Adjusted):-
  must_det_ll(if_or_else(eval(Eq,ReturnType,Depth,Self,Arg,Adjusted),call(Else,Arg,Adjusted))).


get_operator_typedef(Self,Op,ParamTypes,RetType):-
  len_or_unbound(ParamTypes,Len),
  get_operator_typedef(Self,Op,Len,ParamTypes,RetType).

reset_cache:- retractall(get_operator_typedef0(_,_,_,_,_)).

:- dynamic(get_operator_typedef0/5).
get_operator_typedef(Self,Op,Len,ParamTypes,RetType):-
 len_or_unbound(ParamTypes,Len),
 if_or_else(get_operator_typedef0(Self,Op,Len,ParamTypes,RetType),
 if_or_else(get_operator_typedef1(Self,Op,Len,ParamTypes,RetType),
            get_operator_typedef2(Self,Op,Len,ParamTypes,RetType))).

get_operator_typedef1(Self,Op,Len,ParamTypes,RetType):-
   len_or_unbound(ParamTypes,Len),
   if_t(nonvar(ParamTypes),append(ParamTypes,[RetType],List)),
   metta_type(Self,Op,['->'|List]),
   if_t(var(ParamTypes),append(ParamTypes,[RetType],List)),
   assert(get_operator_typedef0(Self,Op,Len,ParamTypes,RetType)).
get_operator_typedef2(Self,Op,Len,ParamTypes,RetType):-
  ignore('AnyRet'=RetType),
  maplist(is_eval_kind,ParamTypes),
  assert(get_operator_typedef0(Self,Op,Len,ParamTypes,RetType)).
  %nop(wdmsg(missing(get_operator_typedef2(Self,Op,ParamTypes,RetType)))),!,fail.


ignored_args_conform(Depth,Self,A,L):- ( \+ iz_conz(Args); \+ iz_conz(List)), !.
ignored_args_conform(Depth,Self,A,L):- maplist(ignored_arg_conform(Depth,Self),A,L).
ignored_arg_conform(Depth,Self,A,L):- nonvar(L), is_nonspecific_type(L),!.
ignored_arg_conform(Depth,Self,A,L):- get_type(Depth,Self,A,T), type_conform(T,L),!.
ignored_arg_conform(Depth,Self,_,_):- !.

args_conform(_Dpth,_Slf,Args,List):- ( \+ iz_conz(Args); \+ iz_conz(List)), !.
args_conform(Depth,Self,[A|Args],[L|List]):-
  arg_conform(Depth,Self,A,L), args_conform(Depth,Self,Args,List).

arg_conform(_Dpth,_Slf,_A,L):- nonvar(L), is_nonspecific_type(L),!.
    arg_conform(Depth,Self,A,L):- get_type(Depth,Self,A,T), type_conform(T,L),!.
%arg_conform(_Dpth,_Slf,_,_).
%arg_conform(Depth,Self,A,_):- get_type(Depth,Self,A,_),!.

type_conform(T,L):- T=L,!.
type_conform(T,L):- \+ \+ (is_nonspecific_type(T);is_nonspecific_type(L)),!.
type_conform(T,L):- can_assign(T,L).


:- dynamic(thrown_metta_return/1).
throw_metta_return(L):-
   asserta(thrown_metta_return(L)),
    (throw(metta_return(L))).


into_typed_args(_Dpth,_Slf,T,M,Y):- (\+ iz_conz(T); \+ iz_conz(M)),!, M=Y.
into_typed_args(Depth,Self,[T|TT],[M|MM],[Y|YY]):-
  into_typed_arg(Depth,Self,T,M,Y),
  into_typed_args(Depth,Self,TT,MM,YY).

into_typed_arg(_Dpth,Self,T,M,Y):- var(M),!,Y=M, nop(put_attr(M,metta_type,Self=T)).
into_typed_arg(Depth,Self,T,M,Y):- into_typed_arg0(Depth,Self,T,M,Y)*->true;M=Y.

into_typed_arg0(Depth,Self,T,M,Y):- var(T), !,
 must_det_ll((get_type(Depth,Self,M,T),
 (wants_eval_kind(T)->eval_args(Depth,Self,M,Y);Y=M))).

into_typed_arg0(Depth,Self,T,M,Y):- is_pro_eval_kind(T),!,eval_args(Depth,Self,M,Y).
into_typed_arg0(Depth,Self,T,M,Y):- ground(M),!, \+ arg_violation(Depth,Self,M,T),Y=M.
into_typed_arg0(_Dpth,_Slf,T,M,Y):- nonvar(T), is_non_eval_kind(T),!,M=Y.
into_typed_arg0(Depth,Self,_,M,Y):- eval_args(Depth,Self,M,Y).

wants_eval_kind(T):- nonvar(T), is_pro_eval_kind(T),!.
wants_eval_kind(_):- true.

metta_type:attr_unify_hook(Self=TypeList,NewValue):-
 attvar(NewValue),!,put_attr(NewValue,metta_type,Self=TypeList).
metta_type:attr_unify_hook(Self=TypeList,NewValue):-
   get_type(20,Self,NewValue,Was),
   can_assign(Was,Type).

%set_type(Depth,Self,Var,Type):- nop(set_type(Depth,Self,Var,Type)),!.
set_type(Depth,Self,Var,Type):- nop(set_type(Depth,Self,Var,Type)),!.
set_type(Depth,Self,Var,Type):-
  get_types(Depth,Self,Var,TypeL),
  add_type(Depth,Self,Var,TypeL,Type).

add_type(_Depth,_Self, Var,_TypeL,_Type):-
  \+ nonvar(Var),!.
add_type(_Depth,_Self,_Var,TypeL,Type):-
  \+ \+ (member(E,TypeL),E==Type),!.
add_type(_Depth,Self,_Var,TypeL,Type):-
    append([Type],TypeL,TypeList),
  put_attr(Var,metta_type,Self=TypeList).




can_assign(Was,Type):- (is_nonspecific_type(Was);is_nonspecific_type(Type)),!.
can_assign(Was,Type):- Was=Type,!.
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
is_non_eval_kind(Var):- var(Var),!.
is_non_eval_kind(Type):- nonvar(Type),Type\=='Any', is_nonspecific_type(Type),!.
is_non_eval_kind('Atom').

is_nonspecific_type(Any):- notrace(is_nonspecific_type0(Any)),!.
is_nonspecific_type0(Var):- var(Var),!,fail.
is_nonspecific_type0('%Undefined%').
is_nonspecific_type0('ErrorType').
%is_nonspecific_type([]).
is_nonspecific_type0('Atom').
is_nonspecific_type0(Any):- is_nonspecific_any(Any).

formated_data_type('Number').
formated_data_type('Symbol').
formated_data_type('Bool').
formated_data_type('Char').
formated_data_type('String').
formated_data_type([List|_]):- List=='List'.

is_nonspecific_any(Any):- notrace(is_nonspecific_any0(Any)),!.

is_nonspecific_any0(Any):- Any=='Any'.
is_nonspecific_any0(Any):- Any=='%Undefined%'.
%is_nonspecific_any0(Any):- Any=='Type'.
is_nonspecific_any0(Any):- Any=='AnyRet'.


is_nonspecific_type_na(NotAtom):- NotAtom\=='Atom', is_nonspecific_type(NotAtom).
narrow_types(RetType,RetType,RetType):- !.
narrow_types(Any,RetType,RetType):- nonvar(Any),is_nonspecific_any(Any),!.
narrow_types(Any,RetType,RetType):- nonvar(Any),is_nonspecific_any(Any),!.
narrow_types(Any,RetType,RetType):- nonvar(Any),is_nonspecific_type_na(Any),!.
narrow_types(RetType,Any,RetType):- nonvar(Any),is_nonspecific_type_na(Any),!.
narrow_types(RetType,Any,RetType):- is_type_list(Any,List),!,narrow_types([RetType|List],Out).
narrow_types(Any,RetType,RetType):- is_type_list(Any,List),!,narrow_types([RetType|List],Out).
narrow_types(Fmt,Fmt1,Fmt):- formated_data_type(Fmt),formated_data_type(Fmt1).
narrow_types(Fmt,Fmt1,Fmt):- formated_data_type(Fmt),!.
narrow_types(Fmt1,Fmt,Fmt):- formated_data_type(Fmt),!.
narrow_types(Fmt1,Fmt2,'NarrowTypeFn'(Fmt1,Fmt2)).

is_type_list('NarrowTypeFn'(Fmt1,Fmt2),List):- get_type_list('NarrowTypeFn'(Fmt1,Fmt2),List).

get_type_list('NarrowTypeFn'(Fmt1,Fmt2),List):- !,
   get_type_list(Fmt1,List1),get_type_list(Fmt2,List2),
   append(List1,List2,List).
get_type_list(A,[A]).

narrow_types(NL,Out):- \+ is_list(NL),!, Out=[NL].
narrow_types([A|List],Out):- var(A),!,narrow_types(List,LT),Out='NarrowTypeFn'(A,LT).
narrow_types([A,B|List],Out):- narrow_types([B|List],BL),narrow_types(A,BL,Out).
narrow_types([A],A).

is_pro_eval_kind(Var):- var(Var),!.
is_pro_eval_kind(SDT):- formated_data_type(SDT).
is_pro_eval_kind(A):- A=='Atom',!,fail.
is_pro_eval_kind(A):- A=='%Undefined%',!,fail.
is_pro_eval_kind(A):- is_nonspecific_any(A),!.

is_feo_f('Cons').

is_seo_f('{...}').
is_seo_f('[...]').
is_seo_f('{}').
is_seo_f('[]').
is_seo_f('StateMonad').
is_seo_f('State').
is_seo_f('Event').
is_seo_f('Concept').
is_seo_f(N):- number(N),!.

:- if( \+ current_predicate(is_absorbed_return_type/2)).
is_absorbed_return_type(Params,Var):- var(Var),!, \+ sub_var(Var,Params).
is_absorbed_return_type(_,'Bool').
is_absorbed_return_type(_,[Ar]):- !, Ar == (->).
is_absorbed_return_type(_,'EmptyType').
is_absorbed_return_type(_,'ReturnType').
is_absorbed_return_type(_,X):- is_self_return(X).

is_self_return('ErrorType').

is_non_absorbed_return_type(Params,Var):-
   \+ is_absorbed_return_type(Params,Var).

:- endif.

%is_user_defined_goal(Self,[H|_]):- is_user_defined_head(Eq,Self,H).

is_user_defined_head(Other,H):- is_user_defined_head(=,Other,H).
is_user_defined_head(Eq,Other,H):- mnotrace(is_user_defined_head0(Eq,Other,H)).
is_user_defined_head0(Eq,Other,[H|_]):- !, nonvar(H),!, is_user_defined_head_f(Eq,Other,H).
is_user_defined_head0(Eq,Other,H):- callable(H),!,functor(H,F,_), is_user_defined_head_f(Eq,Other,F).
is_user_defined_head0(Eq,Other,H):- is_user_defined_head_f(Eq,Other,H).

is_user_defined_head_f(Other,H):- is_user_defined_head_f(=,Other,H).
is_user_defined_head_f(Eq,Other,H):- is_user_defined_head_f1(Eq,Other,H).
is_user_defined_head_f(Eq,Other,H):- is_user_defined_head_f1(Eq,Other,[H|_]).

%is_user_defined_head_f1(Eq,Other,H):- metta_type(Other,H,_).
%s_user_defined_head_f1(Other,H):- get_metta_atom(Eq,Other,[H|_]).
is_user_defined_head_f1(Other,H):- is_user_defined_head_f1(=,Other,H).
is_user_defined_head_f1(Eq,Other,H):- metta_eq_def(Eq,Other,[H|_],_).
%is_user_defined_head_f(Eq,_,H):- is_metta_builtin(H).



is_special_op(Op):-  current_self(Self),is_special_op(Self,Op).

is_special_op(_Slf,F):- \+ atom(F), \+ var(F), !, fail.
%is_special_op(Self,Op):- get_operator_typedef(Self,Op,Params,_RetType),
%   maplist(is_non_eval_kind,Params).
%is_special_op(_Slf,Op):- is_special_builtin(Op).

is_eval_kind(ParamType):- ignore(ParamType='Any').

is_metta_data_functor(Eq,F):-
  current_self(Self),is_metta_data_functor(Eq,Self,F).

:- if( \+ current_predicate(get_operator_typedef/4)).
get_operator_typedef(Self,Op,ParamTypes,RetType):-
  get_operator_typedef(Self,Op,_,ParamTypes,RetType).
:- endif.

:- if( \+ current_predicate(get_operator_typedef1/4)).
get_operator_typedef1(Self,Op,ParamTypes,RetType):-
  get_operator_typedef1(Self,Op,_,ParamTypes,RetType).
:- endif.

:- if( \+ current_predicate(get_operator_typedef/5)).
get_operator_typedef(Self,Op,_,ParamTypes,RetType):-
  get_operator_typedef(Self,Op,ParamTypes,RetType).
:- endif.


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

% Comparison Operators in Prolog
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

% Arithmetic Operations
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
is_math_op('e', 0, exists).         % Euler's Number
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

%:- load_pfc_file('metta_ontology.pl.pfc').


