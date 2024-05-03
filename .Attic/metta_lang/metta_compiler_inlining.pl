

eval_for(_,Var,B,C):- var(Var),!, B=C.
eval_for(_, _, B, C):- B==C,!.
eval_for(b_C, A, B, C):- !, eval_for1(b_C,A,B,C), \+ \+ ((get_type(C,CT),can_assign(CT,A))).
eval_for(Why, A, B, C):- eval_for1(Why,A,B,C).

eval_for1(_Why,_,B,C):- \+ callable(B),!, B= C.
eval_for1(_Why,_,B,C):- compound(B),compound(C),B=C,!.
eval_for1(_Why,'Any',B,C):- !, eval(B,C).
eval_for1(_Why,'AnyRet',B,C):- !, eval(B,C).
eval_for1(b_6,'Atom',B,C):- !, eval(B,C).
eval_for1(_,'Atom',B,C):- !, B=C.
eval_for1(_Why,A,B,C):- eval_for(A,B,C).

why_call(_,Goal):- %println(Y),trace,
   call(Goal).


u_assign1(B,C):- u_assign5(B,C).
u_assign2(B,C):- u_assign5(B,C).
u_assign3(B,C):- u_assign5(B,C).
u_assign4(B,C):- u_assign5(B,C).
u_assign6(B,C):- u_assignI(B,C).
u_assign7(B,C):- u_assignI(B,C).
u_assign8(B,C):- u_assign5(B,C).
u_assign9(B,C):- u_assign5(B,C).
u_assignA(B,C):- u_assign5(B,C).
u_assignB(B,C):- u_assign5(B,C).
u_assignC(B,C):- u_assign5(B,C).
u_assign5(B,C):- \+ compound(B),!,B=C.
u_assign5(B,C):- u_assignI(B,C).

u_assignI(B,C):- var(B),!,B=C.
u_assignI(B,C):-  u_assign(B,C).

:- op(700,xfx,'=~').

:- discontiguous f2q/6.


f2p(RetResult,Convert, Converted):-
  f2p(my_head,_ANY_,RetResult,Convert, Converted).

f2p(HeadIs,RetResult,Convert, Converted):-
  f2p(HeadIs,_ANY_,RetResult,Convert, Converted),!.

f2p(HeadIs,RetType,RetResult,Convert, Converted):-
  f2p(40,HeadIs,RetType,RetResult,Convert, Converted).

f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted):-
   Depth2 is Depth-1,
  f2q(Depth2,HeadIs,RetType,RetResult,Convert, Converting),
  convert_fromi(Depth2,Converting, Converted),!.

%f2p(_Depth,_HeadIs,_RetType,RetResult,Convert, eval(Convert,RetResult)).


convert_fromi(_Depth,Converted, Converted):- !.
convert_fromi(_Depth,Converted, Converted):- is_ftVar(Converted),!.
convert_fromi(_Depth,Converted, Converted):- \+ compound(Converted),!.
%convert_fromi(_Depth, u_assign(E,R),  UA):-  !, u_assign(E,R)=UA.
convert_fromi(Depth,(A,B), (AA,BB)):- !, convert_fromi(Depth,A,AA), convert_fromi(Depth,B,BB).
convert_fromi(Depth,Converting, Converted):- is_list(Converting),!,maplist(convert_fromi(Depth),Converting, Converted).
convert_fromi(Depth,Converting, Converted):- compound_name_arguments(Converting,F,Args),!,
   maplist(convert_fromi(Depth),Args, NewArgs),!,
   compound_name_arguments(Converted,F,NewArgs).

%convert_fromi(Depth,Converting, Converted):- f2q(Depth,Converting, Converted).
is_fqVar(Var2):- is_ftVar(Var2),!.
is_fqVar(Var2):- symbol(Var2),!.


%f2q(_Depth,_HeadIs,RetType,Var1, Var2,  ((Var1=Var2))):-
%   is_fqVar(Var1),is_fqVar(Var2),!.


f2q(_Depth,_HeadIs, RetType,RetVar, Convert, true) :-
  is_ftVar(RetVar),is_ftVar(RetType),is_ftVar(Convert),
  RetVar=Convert,!. % Check if Convert is a variable

f2q(_Depth,_HeadIs, RetType,RetVar, Convert, eval_for(b_C,RetType,Convert,RetVar)) :-
  is_ftVar(Convert),!.% Check if Convert is a variable

f2q(_Depth,_HeadIs,RetType,RetVar, [C|Convert], eval_for(b_B,RetType,[C|Convert],RetVar)) :-
   is_ftVar(C),!.% Check if Convert is a variable

f2q(Depth,HeadIs,RetType,RetResult, eval(Convert), Code):-  !,
  DepthM1 is Depth-1, f2q(DepthM1,HeadIs,RetType,RetResult, Convert, Code).



f2q(_Depth,_HeadIs,_RetType,_RetResult, u_assign(E,R),  UA):-  !,
    u_assign2(E,R)=UA.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- % HeadIs\=@=Convert,
     is_arity_0(Convert,F), !, Converted = u_assign3([F],RetResult),!.

% If Convert is a ":-" (if) function, we convert it to the equivalent ":-" (if) predicate.
f2q(_Depth,_HeadIs,RetType,RetResult, Convert, true) :- ignore(RetType='Atom'),
                (Convert = (H:-B)),
                (RetResult= (H:-B)).


get_ret_type([F|Args],RetType):- is_list(Args),!,
    ((length(Args,Len),(PL = Len ; PL is Len + 1 ; PL is Len - 1),
      PL>=0,
      length(Params,PL),
      get_operator_typedef1(_Self,F,Params,RetType),
      RetType \== 'RetAny')*->true;RetType=_/*'%Undefined%'*/).
get_ret_type(F,RetType):- get_type(F,RetType).


f2q(Depth,HeadIs,RetType,RetVar, Data, CodeOut):- var(RetType), nonvar(Data),
   get_ret_type(Data,PRT),nonvar(PRT),!,RetType=PRT,
   f2q(Depth,HeadIs,RetType,RetVar, Data, CodeOut).

f2q(Depth,HeadIs,RetType,RetVar, Data, CodeOut):- var(RetType), nonvar(RetVar),
   get_ret_type(RetVar,PRT),nonvar(PRT),!,RetType=PRT,
   f2q(Depth,HeadIs,RetType,RetVar, Data, CodeOut).

f2q(Depth,HeadIs,RetType,RetVal,Convert,Code):-
   compound_non_cons(Convert),into_list_args(Convert,ConvertL),
   f2q(Depth,HeadIs,RetType,RetVal,ConvertL,Code),!.

f2q(Depth,HeadIs,RetType,C,Convert,CodeOut):-
     Convert =~ ['-',A,B],
    f2p(Depth,HeadIs,RetType,NewA, A, ACodeOut),
    f2p(Depth,HeadIs,RetType,NewB, B, BCodeOut),
    combine_code([ACodeOut,BCodeOut,'-'(NewA,NewB,C)],CodeOut).

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert,eval_true(Convert)):- fail,
   %nl,print(Convert),nl,
   as_functor_args(Convert,F,A,Args),
   \+ (member(Arg,Args),(is_list(Arg);compound(Arg))),
   is_absorbed_return_value(F,A,RResult),
   RResult=RetResult.

f2q(Depth,_HeadIs,_RetType,RetResult, Convert, u_assign4(Convert,RetResult)) :- Depth=<0,!.

f2q(_Depth,_HeadIs,_RetType,RetResult, Convert, true) :-
 (number(Convert)),RetResult=Convert,!.% Check if Convert is a ...


% If Convert is a number or an symbol, it is considered as already converted.
f2q(_Depth,_HeadIs,_RetType,RetResult, Convert, why_call(is_data, Convert = RetResult )) :- % HeadIs,RetType\=@=Convert,
    once(number(Convert); symbol(Convert); data_term(Convert)),  % Check if Convert is a number or an symbol
    !.  % Set RetResult to Convert as it is already in predicate form

data_term(Convert):- \+ compound(Convert),
  self_eval(Convert),!,
  (iz_conz(Convert) ;  \+ compound(Convert)).

f2q(_Depth,_HeadIs,_RetType,RetResult, Convert, (RetResult =~ Convert)) :-  data_term(Convert),!.% Check if Convert is a ...

f2q(_Depth,_HeadIs,_RetType,RetResult, Convert, true) :-
    (data_term(Convert)),RetResult=Convert,!.% Check if Convert is a ...

% If Convert is a variable, the corresponding predicate is just u_assign(Convert, RetResult)
f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, RetResultConverted) :-
     is_ftVar(Convert),!,% Check if Convert is a variable
     into_equals(RetResult,Convert,RetResultConverted).
    % Converted = u_assign(Convert, RetResult).  % Set Converted to u_assign(Convert, RetResult)

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, RetResultConverted) :-
     number(Convert),!,into_equals(RetResult,Convert,RetResultConverted).

% If Convert is a "," (and) function, we convert it to the equivalent "," (and) predicate.
f2q(Depth,HeadIs,_RetType,RetResult,SOR,CC) :-
      SOR =~ [LogOp, AsPredI, Convert], ',' == LogOp,
      RetResult = [LogOp,RetResult1, RetResult2],
      must_det_ll((f2p(Depth,HeadIs,_RetType1,RetResult1,AsPredI, AsPredO),
                   f2p(Depth,HeadIs,_RetType2,RetResult2,Convert, Converted))),!,
      combine_code(AsPredO,Converted,CC).


    f2q(Depth,HeadIs,RetType,RetResult,SOR,(AsPredO,Converted)) :-
          SOR =~ [LogOp, AsPredI, Convert], 'and' == LogOp,!,
        must_det_ll((f2p(Depth,HeadIs,'Bool','True',AsPredI, AsPredO),
                     f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))),!.

f2q(Depth,HeadIs,RetType,RetResult,SOR,CC) :-
      SOR =~ [LogOp, AsPredI, Convert], 'and' == LogOp,
    %RetType = 'Bool', RetResultB = 'True', RetResultA = 'True',
    must_det_ll((f2p(Depth,HeadIs,RetTypeA,RetResultA,AsPredI, AsPredO),
                 f2p(Depth,HeadIs,RetTypeB,RetResultB,Convert, Converted))),!,
    combine_code([ AsPredO, RetResult=RetResultA, Converted,
                       why_call(merge_rettypes,narrow_types([RetTypeA,RetTypeB],RetType)),
                       why_call(same_result,RetResultA==RetResultB),
                       why_call(return_val,RetResult=RetResultB)],CC).

    f2q(Depth,HeadIs,RetType,RetResult,SOR,(AsPredO;Converted)) :-
          SOR =~ [LogOp, AsPredI, Convert], 'or' == LogOp,!,
        must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
                     f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))),!.



f2q(Depth,HeadIs,RetType,RetResult,SOR,CC) :-
      SOR =~ [LogOp, AsPredI, Convert], 'or' == LogOp,
    % RetType = 'Bool',
    %RetResultB = 'True', RetResultA = 'True',
    must_det_ll((f2p(Depth,HeadIs,RetTypeA,RetResultA,AsPredI, AsPredO),
                 f2p(Depth,HeadIs,RetTypeB,RetResultB,Convert, Converted))),!,
    combine_code(( AsPredO, RetResult=RetResultA,RetType=RetTypeA);
                 (Converted,RetResult=RetResultB,RetType=RetTypeB),CC).


f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, eval(Convert,RetResult)):- fail,
   interpet_this(Convert),!.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert,Converted) :- fail, % dif_functors(HeadIs,Convert),
    Convert =~ [H|_], \+ symbol(H), \+ is_non_evaluatable(H),
    Converted = (Convert=RetResult),!.

f2q(Depth,HeadIs,RetType,Atom,Convert,Converted) :-
   Convert=~ match(Space,Q,T),Q==T,Atom=Q,!,
  f2p(Depth,HeadIs,RetType,Atom,'get-atoms'(Space),Converted).

f2q(Depth,HeadIs,_RetType,AtomsVar,Convert,Converted) :-
    Convert=~ 'get-atoms'(Space), Pattern = AtomsVar,
    compile_pattern(Depth,HeadIs,Space,Pattern,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert,Converted) :- %dif_functors(HeadIs,Convert),
    Convert =~ 'match'(ESpace,Pattern,Template),!,
  must_det_ll((
    f2p(Depth,HeadIs,_SpaceT,SpaceV,ESpace,Code),
    %term_variables(Template,TemplateVars),
    compile_pattern(Depth,HeadIs,SpaceV,Pattern,SpacePatternCode),
    f2p(Depth,HeadIs,RetType,RetResult,Template,TemplateCode),
    combine_code((Code,SpacePatternCode),TemplateCode,Converted))).

    compile_pattern(_Depth,_HeadIs,Space,Pattern,SpaceMatchCode):-
      SpaceMatchCode = metta_atom_iter(Space,Pattern).


/*
  f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  Convert =~ 'match'(_Space,Match,Template),!,
   must_det_ll((
    f2p(Depth,HeadIs,RetType,_,Match,MatchCode),
    into_equals(RetResult,Template,TemplateCode),
    combine_code(MatchCode,TemplateCode,Converted))).
*/


interpet_this(_Convert):-!, fail.

interpet_this(Convert):- as_functor_args(Convert,F,A,Args), interpet_this(Convert,F,A,Args).
interpet_this(_,F,_,_):- \+ symbolic(F),!.
interpet_this(_,F,_,_):- compile_this_s(F),!,fail.
interpet_this(_,F,_,_):- interpet_this_f(F),!.
% stable workarround until the '=~' bug is fixed for numbers
interpet_this(Convert,F,A,Args):- compile_this(Convert,F,A,Args),!,fail.
interpet_this(_,_,_,_).

interpet_this_f(_Convert):-!, fail.
interpet_this_f(F):- metta_atom_file_buffer_isa(F,'Compiled'),!,fail.
interpet_this_f(F):- metta_atom_file_buffer_isa(F,'Interpreted'),!.
interpet_this_f(F):- op_decl(F, [ 'Number', 'Number' ], 'Number').

compile_this(_):-!.
compile_this(Convert):- as_functor_args(Convert,F,A,Args), compile_this(Convert,F,A,Args).
compile_this(_,F,_,_):- \+ symbolic(F),!, fail.
compile_this(_,F,_,_):- compile_this_f(F),!.

    compile_this_f(_):-!.
compile_this_f(F):- metta_atom_file_buffer_isa(F,'Compiled').
compile_this_f(F):- interpet_this_f(F),!,fail.
compile_this_f(F):- compile_this_s(F),!.
compile_this_f(F):- metta_atom_file_buffer([':',F,[Ar|_]]), Ar=='->', !.
compile_this_s('superpose').
compile_this_s('match').
compile_this_s('do').
compile_this_s('do-all').


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail,  dif_functors(HeadIs,Convert),
  get_inline_def(Convert,NewDef),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,NewDef,Converted))).

f2q(Depth,HeadIs,RetType,RetResult,Convert, do(Converted)) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['do',Body],!,
  ignore(RetResult='Empty'),
  f2p(Depth,HeadIs,RetType,_RetResult,Body, Converted).

f2q(Depth,HeadIs,_RetTypeD,RetResult,Convert, (doall(Converted),RetResult='Empty')) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['do-all',Body],!,
  f2p(Depth,HeadIs,_RetTypeB,_RetResultB,Body, Converted).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-  % dif_functors(HeadIs,Convert),
  Convert =~ ['let',Var,Value1,Body],!,
    f2p(Depth,HeadIs,_,ResValue1,Value1,CodeForValue1),
    into_equals(Var,ResValue1,CodeEquals),
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
  combine_code([CodeForValue1,CodeEquals,BodyCode],Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['let',Var,Value1,Body],!,
  f2p(Depth,HeadIs,_,Var,Value1, BindingCode),
  f2p(Depth,HeadIs,RetType,RetResult,Body, BodyCode),
   combine_code(BindingCode,BodyCode,Converted).

is_Nil(Nil):- Nil==[],!.
is_Nil(Nil):- Nil=='Nil',!.
is_Nil(Nil):- Nil=='()',!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- %dif_functors(HeadIs,Convert),
  Convert =~ ['let*',Nil,Body],is_Nil(Nil), !,
   must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,Body, Converted))).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- %dif_functors(HeadIs,Convert),
  Convert =~ ['let*',AAAA,Body],AAAA=~[VE|Bindings],VE=~[V,E],
  f2q(Depth,HeadIs,RetType,RetResult,['let',V,E,['let*',Bindings,Body]], Converted).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail, dif_functors(HeadIs,Convert),
  Convert =~ ['let*',Bindings,Body],!,
   must_det_ll((
    maplist(compile_let_star(Depth,HeadIs,RetType),Bindings,CodeList),
    combine_code(CodeList,BindingCode),
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    combine_code(BindingCode,BodyCode,Converted))).

compile_let_star(Depth,HeadIs,RetType,NV,Converted):-
 must_det_ll((NV =~ [Expression,Var],
 (var(Var)-> f2p(Depth,HeadIs,RetType,Var,Expression,Converted);
 (var(Expression)-> f2p(Depth,HeadIs,RetType,Expression,Var,Converted);
 (f2p(Depth,HeadIs,RetType,Eval1Result,Expression,Code),
  into_equals(Eval1Result,Var,Eval1ResultVar),
  combine_code(Code,Eval1ResultVar,Converted)))))),!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['superpose',COL],compound_equals(COL,'collapse'(Value1)), !,
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    Converted = (find_ne(ResValue1,CodeForValue1,Gathered),member(RetResult,Gathered)).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
   Convert =~ ['sequential'|ValueL],
   ReConvert =~ ['superpose'|ValueL],!,
   f2q(Depth,HeadIs,RetType,RetResult,ReConvert, Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, (Converted)) :-
    Convert =~ ['sequential',ValueL],is_list(ValueL),!,
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    maplist(f2p_assign(Depth,HeadIs,RetType),RetResultL,ValueL,CodeForValueL),
    last(RetResultL,RetResult),
    combine_code(CodeForValueL,Converted),!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_list(ValueL),!,
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    must_det_ll(( ignore(cname_var('SP_Ret',RetResult)),
    maplist(f2p(Depth,HeadIs,RetType,RetResult),ValueL,CodeForValueL),
    list_to_disjuncts(CodeForValueL,Converted))),!.

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, (Converted)) :-
    Convert =~ ['superpose',ValueL],is_nsVar(ValueL),!,
    %maybe_unlistify(UValueL,ValueL,URetResult,RetResult),
    Converted = call('superpose'(ValueL,RetResult)),
    cname_var('MeTTa_SP_',ValueL).


:- op(700,xfx, =~).
f2q(Depth,HeadIs,RetType,RetResult,Convert, (Code1,Eval1Result=Result,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'chain'(Eval1,Result,Eval2),!,
   f2p(Depth,HeadIs,RetType,Eval1Result,Eval1,Code1),
   f2p(Depth,HeadIs,RetType,RetResult,Eval2,Converted).

f2q(Depth,HeadIs,RetType,ResValue2,Convert, (CodeForValue1,Converted)) :- % dif_functors(HeadIs,Convert),
   Convert =~ ['eval-in-space',Value1,Value2],
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
   Converted = with_space(ResValue1,CodeForValue2).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  once(Convert =~ 'if'(Cond,Then,Else);Convert =~ 'If'(Cond,Then,Else)),
  !,Test = is_True(CondResult),
  f2p(Depth,HeadIs,RetType,CondResult,Cond,CondCode),
  compile_test_then_else(Depth,RetResult,(CondCode,Test),Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),
  once(Convert =~ 'if'(Cond,Then);Convert =~ 'If'(Cond,Then)),
  f2p(Depth,HeadIs,RetType,CondResult,Cond,CondCode),
    f2p(Depth,HeadIs,RetType,RetResult,Then,ThenCode),
    combine_code([CondCode,is_True(CondResult),ThenCode],Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-error'(Value,Then,Else),!,Test = is_Error(ValueResult),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
  combine_code(ValueCode,Test,ValueCodeTest),
  compile_test_then_else(Depth,RetResult,ValueCodeTest,Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'if-empty'(Value,Then,Else),!,Test = is_Empty(ValueResult),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
  compile_test_then_else(Depth,RetResult,(ValueCode,Test),Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  (Convert =~ 'if-non-empty-expression'(Value,Then,Else)),!,
  (Test = ( \+ is_Empty(ValueResult))),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
  compile_test_then_else(Depth,RetResult,(ValueCode,Test),Then,Else,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ ['if-equals',Value1,Value2,Then,Else],!,Test = equal_enough(ResValue1,ResValue2),
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
  compile_test_then_else(Depth,RetResult,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).

cname_var(Sym,_Src):- var(Sym),!.
cname_var(Sym,Src):-  var(Src),!,must_det_ll((gensym(Sym,SrcV),Src='$VAR'(SrcV))).
cname_var(Sym,Src):-  Src='$VAR'(_),!,must_det_ll((gensym(Sym,SrcV),nb_setarg(1,Src,SrcV))).
cname_var(_Sym,_Src).
cname_var(Name=Var):- cname_var(Name,Var).
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['assertEqual',Value1,Value2],!,
    cname_var('Src_',Src),
    cname_var('FA_',ResValue1),
    cname_var('FA_',ResValue2),
    cname_var('FARL_',L1),
    cname_var('FARL_',L2),
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
    Converted =
              (Src = Convert,
               loonit_assert_source_tf_empty(Src,L1,L2,
                (findall_ne(ResValue1,CodeForValue1,L1),
                 findall_ne(ResValue2,CodeForValue2,L2)),
                 equal_enough_for_test(L1,L2),RetResult)).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['assertEqualToResult',Value1,Value2],!,
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    Src = Convert,
    Goal = findall_ne(ResValue1,CodeForValue1,L1),
    Converted = (
                loonit_assert_source_tf_empty(Src,L1,Value2,
                     Goal,
                     equal_enough_for_test(L1,Value2),RetResult)).

maybe_unlistify([UValueL],ValueL,RetResult,[URetResult]):- fail, is_list(UValueL),!,
  maybe_unlistify(UValueL,ValueL,RetResult,URetResult).
maybe_unlistify(ValueL,ValueL,RetResult,RetResult).

list_to_disjuncts([],false).
list_to_disjuncts([A],A):- !.
list_to_disjuncts([A|L],(A;D)):-  list_to_disjuncts(L,D).


%f2p_assign(Depth,_HeadIs,_RetType,V,Value,is_True(V)):- Value=='True'.
f2p_assign(_Depth,_HeadIs,_RetType,ValueR,Value,ValueR=Value):- is_nsVar(Value),!.
f2p_assign(_Depth,_HeadIs,_RetType,ValueR,Value,ValueR=Value):- \+ compound(Value),!.
f2p_assign(_Depth,_HeadIs,_RetType,ValueResult,Value,Converted):-
  f2p(Value,ValueResult,Converted),!.
f2p_assign(Depth,HeadIs,RetType,ValueResult,Value,Converted):-
   f2p(Depth,HeadIs,RetType,ValueResultR,Value,CodeForValue),
   %into_equals(ValueResultR,ValueResult,ValueResultRValueResult),
   ValueResultRValueResult = (ValueResultR=ValueResult),
   combine_code(CodeForValue,ValueResultRValueResult,Converted).


f2p_arg(_Depth,_HeadIs,_RetType,Value,Value,true):- is_nsVar(Value),!.
f2p_arg(_Depth,_HeadIs,_RetType,Value,Value,true):- \+ compound(Value),!.
f2p_arg(_Depth,_HeadIs,_RetType,ValueResult,Value,Converted):- h2p(Value,ValueResult,Converted),!.
f2p_arg(Depth,HeadIs,RetType,ValueResult,Value,Converted):-
   f2p_assign(Depth,HeadIs,RetType,ValueResult,Value,Converted).


f2q(Depth,HeadIs,RetType,RetResult,Convert, keep(Converted)) :-
  Convert =~ ['case',Value,PNil],[]==PNil,!,Converted = (ValueCode,RetResult=[]),
      f2p(Depth,HeadIs,RetType,_ValueResult,Value,ValueCode).


f2q(Depth,HeadIs,RetType,RetResult,Convert, (ValueCode, Converted)) :-
  Convert =~ ['case',Value|Options], \+ is_nsVar(Value),!,
  cname_var('CASE_VAR_',ValueResult),
  f2q(Depth,HeadIs,RetType,RetResult,['case',ValueResult|Options], Converted),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,Options],!,
   must_det_ll((
    maplist(compile_case_bodies(Depth,HeadIs,RetType),Options,Cases),
    cname_var('SWITCH_',AllCases),
    cname_var('CASE_RESULT_',RetResult),
    Converted =
           ( AllCases = Cases,
             select_case(AllCases,Value,RetResult)))).

select_case(AllCases,Value,BodyResult):-
       once((member(caseOption(MatchVar,MatchCode,BodyResult,BodyCode),AllCases),
             rtrace_on_error(MatchCode),unify_case(Value,MatchVar)))
             ,!,
       rtrace_on_error(BodyCode).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
  Convert =~ ['case',Value,[Opt|Options]],nonvar(Opt),!,
   must_det_ll((
    compile_case_bodies(Depth,HeadIs,RetType,Opt,caseOption(Value,If,RetResult,Then)),
    Converted = ( If -> Then ; Else ),
    ConvertCases =~ ['case',Value,Options],
    f2q(Depth,HeadIs,RetType,RetResult,ConvertCases,Else))).


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

compile_case_bodies(Depth,HeadIs,RetType,[Match,Body],caseOption(_,true,BodyResult,BodyCode)):- Match == '%void%',!,
      f2p(Depth,HeadIs,RetType,BodyResult,Body,BodyCode).
compile_case_bodies(Depth,HeadIs,RetType,[Match,Body],caseOption(MatchResult,If,BodyResult,BodyCode)):- !,
      f2p(Depth,HeadIs,RetType,MatchResultV,Match,MatchCode),
      combine_code(MatchCode,unify_case(MatchResult,MatchResultV),If),
      f2p(Depth,HeadIs,RetType,BodyResult,Body,BodyCode).
compile_case_bodies(Depth,HeadIs,RetType,MatchBody,CS):- compound(MatchBody), MatchBody =~ MB,compile_case_bodies(Depth,HeadIs,RetType,MB,CS).


compound_equals(COL1,COL2):- COL1=@=COL2,!,COL1=COL2.
compound_equals(COL1,COL2):- compound_equals1(COL1,COL2).
compound_equals1(COL1,COL2):- is_nsVar(COL1),!,is_nsVar(COL2),ignore(COL1=COL2),!.
compound_equals1(COL1,COL2):- compound(COL1),!,compound(COL2), COL1=COL2.


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['collapse',Value1],!,
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    Converted = (findall_ne(ResValue1,CodeForValue1,RetResult)).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :-
    Convert =~ ['compose',Value1],!,
    Convert2 =~ ['collapse',Value1],!,
    f2q(Depth,HeadIs,RetType,RetResult,Convert2, Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
  Convert =~ ['unify',Value1,Value2,Then,Else],!,Test = metta_unify(ResValue1,ResValue2),
    f2p(Depth,HeadIs,RetType,ResValue1,Value1,CodeForValue1),
    f2p(Depth,HeadIs,RetType,ResValue2,Value2,CodeForValue2),
  compile_test_then_else(Depth,RetResult,(CodeForValue1,CodeForValue2,Test),Then,Else,Converted).




f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- dif_functors(HeadIs,Convert),

   Convert =~ ['if-decons',Atom,Head,Tail,Then,Else],!,Test = unify_cons(AtomResult,ResHead,ResTail),
    f2p(Depth,HeadIs,RetType,AtomResult,Atom,AtomCode),
    f2p(Depth,HeadIs,RetType,ResHead,Head,CodeForHead),
    f2p(Depth,HeadIs,RetType,ResTail,Tail,CodeForTail),
    compile_test_then_else(Depth,RetResult,(AtomCode,CodeForHead,CodeForTail,Test),Then,Else,Converted).



f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, was_True(RetResult)) :- is_compiled_and(AND),
   Convert =~ [AND],!.

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body],!,
   f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    compile_test_then_else(Depth,RetResult,BodyCode,'True','False',Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(Depth,HeadIs,RetType,B1Res,Body1,Body1Code),
   f2p(Depth,HeadIs,RetType,RetResult,Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   Converted = (Body1Code,AE,Body2Code),!.


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2],!,
   f2p(Depth,HeadIs,RetType,B1Res,Body1,Body1Code),
   f2p(Depth,HeadIs,RetType,_,Body2,Body2Code),
   into_equals(B1Res,'True',AE),
   compile_test_then_else(Depth,RetResult,(Body1Code,AE,Body2Code),'True','False',Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- is_compiled_and(AND),
   Convert =~ [AND,Body1,Body2|BodyMore],!,
   And2 =~ [AND,Body2|BodyMore],
   Next =~ [AND,Body1,And2],
   f2q(Depth,HeadIs,RetType,RetResult, Next, Converted).

% If Convert is an "or" function, we convert it to the equivalent ";" (or) predicate.
f2q(Depth,HeadIs,RetType,RetResult,SOR,or(AsPredO, Converted)) :-
  SOR =~ or(AsPredI, Convert),
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))),!.
f2q(Depth,HeadIs,RetType,RetResult,or(AsPredI,Convert), (AsPredO *-> true; Converted)) :- fail, !,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))).
f2q(Depth,HeadIs,RetType,RetResult,(AsPredI; Convert), (AsPredO; Converted)) :- !,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO),
               f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))).

'True'(X):- ignore(is_True(X)).
'False'(X):- is_False(X).

get_inline_case_list(HeadDef,Quot,CaseList):-
   findall([HeadDef,NewDef],get_inline_def1(HeadDef,NewDef),DefList),DefList\==[],
   findall([Quot,NewDef],member([HeadDef,NewDef],DefList),CaseList).

get_inline_def(HeadDef,NewDef):-
   findall(NewDef,get_inline_def1(HeadDef,NewDef),EachDef), EachDef\==[],
   disj_def(EachDef,NewDef).



get_inline_def1(HeadDef,NewDef):-
   into_list_args(HeadDef,UHeadDef),
   copy_term(UHeadDef,CopyUHeadDef),
   [UHead|_UArgs] = UHeadDef, nonvar(UHead),
   metta_atom_file_buffer([Eq,UHeadDef|Body]),Eq=='=', once(xform_body(Body,NewDef)),
   (UHeadDef=@=CopyUHeadDef).

%xform_body([Body],Body):-!.
%xform_body(Items,[progn|Body]).

xform_body(Var,Var):-is_ftVar(Var), !.
xform_body([],call(true)):-!.
xform_body([Body],Body):-!.
xform_body([Body1,Body2],(Body1,Body2)):-!.
xform_body([Body1|Body2L],(Body1,Body2)):-xform_body(Body2L,Body2).

disj_def(Var,Var):-is_ftVar(Var), !.
disj_def([],call(fail)):-!.
disj_def([Body],Body):-!.
disj_def([Body1,Body2],(Body1;Body2)):-!.
disj_def([Body1|Body2L],(Body1;Body2)):-disj_def(Body2L,Body2).


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
f2q(Depth,HeadIs,RetType,RetResult,Convert, once(Converted)) :-
  Convert=~ first_of(Body),!, f2p(Depth,HeadIs,RetType,RetResult,Body,Converted).

f2q(Depth,HeadIs,RetType,RetResult,Convert, catch(BodyCode,Ex,HandlerCode)) :-
    Convert=~ catch(Body,E,Handler),!, s2p(E,Ex),
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    f2p(Depth,HeadIs,RetType,RetResult,Handler,HandlerCode).

f2q(Depth,HeadIs,RetType,RetResult,Convert, call_cleanup(BodyCode,HandlerCode)) :-
    Convert=~ finally(Body,Handler),!,
    f2p(Depth,HeadIs,RetType,RetResult,Body,BodyCode),
    f2p(Depth,HeadIs,RetType,RetResult,Handler,HandlerCode).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail, dif_functors(HeadIs,Convert),
  get_inline_def(Convert,InlineDef),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,InlineDef,Converted))).


% If Convert is a "not" function, we convert it to the equivalent ";" (or) predicate.
f2q(Depth,HeadIs,RetType,RetResult,Convert, \+ eval_true(AsPredO)) :-
  '=~'(Convert , (not(AsPredI))),
  must_det_ll(f2p(Depth,HeadIs,RetType,RetResult,AsPredI, AsPredO)).



get_first_p1(_,Cmpd,_):- \+ compound(Cmpd),!, fail.
get_first_p1(E,Cmpd,set_nth1(N1,Cmpd)):- is_list(Cmpd),   nth1(N1,Cmpd,E).
get_first_p1(E,Cmpd,Result)           :- is_list(Cmpd),!, member(Ele,Cmpd), get_first_p1(E,Ele,Result).
get_first_p1(_,Cmpd,_)                :- is_conz(Cmpd),!,fail.
get_first_p1(E,Cmpd,set_arg(N1,Cmpd)) :- arg(N1,Cmpd,E).
get_first_p1(E,Cmpd,Result)           :- arg(_,Cmpd,Ele),!,get_first_p1(E,Ele,Result).

non_simple_arg(E):- compound(E),!, \+ is_ftVar(E).


f2q(Depth,HeadIs,RetType,RetResult,Converting, (PreArgs,Converted)):- fail,
     as_functor_args(Converting,F,A,Args),
        \+ \+ (member(E,Args), non_simple_arg(E)),
          cname_var('Self',Self),
          %Self = '$VAR'('RetType'),
          maplist(type_fit_childs('=',Depth,Self),_RetTypes1,ArgsCode,Args,NewArgs),
            combine_code(ArgsCode,PreArgs),
            nop(non_compat_io(color_g_mesg('magenta',
              ((write_src(type_fit_childs('=',Depth,F,_RetTypes2,PreArgs,Args,NewArgs)),nl))))),
        as_functor_args(Convert,F,A,NewArgs),
        \+ (member(E,NewArgs), non_simple_arg(E)),!,
        f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted).


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

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail,
 must_det_ll((
  as_functor_args(Convert,F,A,Args),
  as_functor_args(Quot,quot,A,NewArgs),
  as_functor_args(QConvert,quot,A,Args))),
  get_inline_case_list([F|NewArgs],Quot,DefList),!,
  must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,case(QConvert,DefList),Converted))).

is_non_evaluatable(S):- \+ compound(S),!.
is_non_evaluatable(S):- is_ftVar(S),!.
is_non_evaluatable([H|_]):- \+ symbol(H), \+ is_non_evaluatable(H).
f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- fail, is_non_evaluatable(Convert),
   Converted = call_why(non_eval,Convert=RetResult),!.


f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
   Convert =~ 'bind!'(Var,Value),is_ftVar(Value),!,
   Converted = u_assign8('bind!'(Var,Value),RetResult).

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ 'bind!'(Var,Value), Value =~ 'new-space'(),!,
    Converted = eval('bind!'(Var,Value),RetResult).

f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- % dif_functors(HeadIs,Convert),
    Convert =~ 'bind!'(Var,Value), !,
    f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode),
    Eval = eval_args(['bind!',Var,ValueResult],RetResult),
   combine_code(ValueCode,Eval,Converted).


returns_empty('add-atom').
returns_empty('remove-atom').

f2q(_Depth,_HeadIs,_RetType,RetResult,Convert, Converted) :-
     (Convert =~ [EmptyResultFunction,Where,What,RetResult];
        Convert =~ [EmptyResultFunction,Where,What]),
     nonvar(EmptyResultFunction),
     returns_empty(EmptyResultFunction),
     current_predicate(EmptyResultFunction/2),
     =(What,WhatP),!,
     Converted = as_nop(call(EmptyResultFunction,Where,WhatP),RetResult).

f2q(Depth,HeadIs,RetType,RetResult,Convert,Converted) :-
  Convert =~ ['println!',Value],!,
  Converted = (ValueCode,eval(['println!',ValueResult], RetResult)),
  f2p(Depth,HeadIs,RetType,ValueResult,Value,ValueCode).



f2q(Depth,HeadIs,RetType,RetResult,Convert,CodeForValueConverted) :- fail,
    Convert =~ [Plus,N,Value], symbol(Plus), current_predicate(Plus/3), number(N),
    \+ number(Value), \+ is_nsVar(Value),!,
    f2p(Depth,HeadIs,RetType,ValueResult,Value,CodeForValue),!,
    Converted =.. [Plus,N,ValueResult,RetResult],
    combine_code(CodeForValue,Converted,CodeForValueConverted).
/*
% match(Space,f(1)=Y,Y)
f2q(Depth,HeadIs,RetType,Y,Convert,Converted) :- dif_functors(HeadIs,Convert),
  Convert=~ match(Space,AsFunctionY,YY),
    nonvar(AsFunctionY),( AsFunctionY =~ (AsFunction=Y)), nonvar(AsFunction),
    !, Y==YY,
    f2p(Depth,HeadIs,RetType,Y,AsFunction,Converted),!.
*/

metta_atom_iter(Space,Match):-
  metta_atom_iter('=',10,Space,Space,Match).

make_with_space(Space,MatchCode,MatchCode):- Space=='&self',!.
make_with_space(Space,MatchCode,with_space(Space,MatchCode)):- Space\=='&self'.

% If Convert is a Value, and RetResult is a Variable bind them together and mark the compiler used them
f2q(_Depth,_HeadIs,_RetType, _RetResult,(A =~ B), (A =~ B)) :-!.


% If Convert is an "u_assign" function, we convert it to the equivalent "is" predicate.
f2q(Depth,HeadIs,RetType,RetResult,EvalConvert,Converted):-
 EvalConvert =~ eval(Convert),  !,
   must_det_ll((f2p(Depth,HeadIs,RetType,RetResult,Convert, Converted))).


f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted):- fail,
    compound(Convert), Convert = u_assign(C, Var), compound_non_cons(C),into_list_args(C,CC),!,
    f2p(Depth,HeadIs,RetType,RetResult,u_assign(CC, Var), Converted).

f2q(_Depth,_HeadIs,_RetType,_RetResult,Convert, Converted):- fail,
    compound(Convert),
    Convert = u_assign(C, _Var),
    is_list(C),Converted = Convert,!.


f2q(_Depth,HeadIs,_RetType,RetResult,Convert, Converted) :- fail,
     symbol(Convert),  functional_predicate_arg(Convert,Nth,Nth2),
      Nth==1,Nth2==1,
      HeadIs\=@=Convert,
      Convert = F,!,
      must_det_ll((
        do_predicate_function_canonical(FP,F),
        compound_name_list(Converted,FP,[RetResult]))).


% If Convert is an "is" function, we convert it to the equivalent "is" predicate.
f2q(Depth,HeadIs,RetType,RetResult,is(Convert),(Converted,is(RetResult,Result))):- !,
   must_det_ll((f2p(Depth,HeadIs,RetType,Result,Convert, Converted))).

into_equals(Eval,Result,Code):-
  into_u_assign(Eval,Result,Code).

into_u_assign(Eval,Result,true):- is_nsVar(Eval), is_nsVar(Result), Eval=Result,!.
into_u_assign(Eval,Result,Code):- Result=='True',!,f2p(Eval,_Result,Code).
into_u_assign(Eval,Result,Code):- var(Eval), \+ var(Result), !, into_u_assign(Result,Eval,Code).
into_u_assign(Eval,Result,Code):- f2p(Eval,Result,Code),!.
into_u_assign(Eval,Result,Code):- Code = u_assign5(Eval,Result).

% check if this is a flow control operation
%f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted):-
%  compound(Convert), \+ compound_name_arity(Convert,_,0),
%  f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted),!.

f2q(Depth,HeadIs,RetType,RetResultL, ConvertL, Converted) :- is_list(ConvertL),
   ConvertL = [Convert],  is_list(Convert),
   f2p(Depth,HeadIs,RetType,RetResult,Convert, Code),!,
   into_equals(RetResultL,[RetResult],Equals),
   combine_code(Code,Equals,Converted).
f2q(_Depth,_HeadIs,_RetType,ResultVar,'cdr-atom'(Atom), 'cdr-atom'(Atom,ResultVar)) :- !.
f2q(_Depth,_HeadIs,_RetType,ResultVar,'car-atom'(Atom), 'car-atom'(Atom,ResultVar)) :- !.

% If Convert is a list, we convert it to its termified form and then proceed with the functs_to_preds conversion.
f2q(Depth,HeadIs,RetType,RetResult,Convert, Converted) :- fail, is_list(Convert),
   once((sexpr_s2p(Convert,IS), \+ IS=@=Convert)), !,  % Check if Convert is a list and not in predicate form
   must_det_ll((f2p(Depth,HeadIs,RetType,RetResult, IS, Converted))).  % Proceed with the conversion of the predicate form of the list.


f2q(Depth,HeadIs,RetType,RetResult, ConvertL, Converted) :- fail, is_list(ConvertL),
   maplist(f2p_assign(Depth,HeadIs,RetType),RetResultL,ConvertL, ConvertedL),
   combine_code(ConvertedL,Conjs),
   into_u_assign(RetResultL,RetResult,Code),
   combine_code(Conjs,Code,Converted).



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

f2q(_Depth,_HeadIs,_RetType,_RetResult,u_assign(Convert,Res), u_assignA(Convert,Res)):-!.



f2q(Depth,_HeadIs,RetType,RetVar, Data, CodeOut):-
    as_functor_args(Data,F,A,Args),
    current_self(Self),
    length(NewArgs,A),
    length(ParamTypes,A),
    most_true([get_operator_typedef(Self,F,ParamTypes,RetTypeF),
    can_assign(RetTypeF,RetType)]),
    if_t(F==(fL), println(Data)),
    narrow_types(RetTypeF,RetType,NarrowType),
    Call=[F|NewArgs],
    append(ParamTypes,[RetType|_],ParamTypesO),
    into_eval_for_l(Depth,Call,Self,F,1,ParamTypesO,Args,NewArgs,ParamCode),
    combine_code(ParamCode,eval_for(b_6,NarrowType,Call,RetVar),CodeOut).

f2q(_Depth,_HeadIs,RetType,RetVar,Data,eval_for(b_8,RetType,Data,RetVar)).

most_true([]):-!.
most_true([A|List]):- call(A),!,most_true(List).
most_true([A|List]):- most_true(List),ignore(A).


into_eval_for_l(Depth,HeadIs,Self,F,Nth,[PT|ParamTypes],[A|Args],[N|NewArgs],CCode):- !,
  into_eval_for(Depth,HeadIs,Self,F,Nth,PT,A,N,C),
  Nth1 is Nth+1,
  into_eval_for_l(Depth,HeadIs,Self,F,Nth1,ParamTypes,Args,NewArgs,Code),
  combine_code(C,Code,CCode).
into_eval_for_l(_Depth,_HeadIs,_Slf,_F,_Nth,[],Args,Args,true).
into_eval_for_l(_Depth,_HeadIs,_Slf,_F,_Nth,_ParamTypes,[],[],true).

into_eval_for(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,A,true):- number(A),!,ignore(PT='Number').
into_eval_for(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,eval_for(b_5,PT,A,N)):- nonvar(PT), PT\=='Atom',var(A),!.
into_eval_for(_Depth,_HeadIs,_Slf,F,Nth,PT,A,N,eval_for(b_4(Nth,F),PT,A,N)):- var(PT), var(A),!.
%into_eval_for(Depth,HeadIs,_Slf,_F,_Nth,RetType,[-,A,B],C,(ACodeOut,BCodeOut,-(NewA,NewB,C))):-
%f2p(Depth,HeadIs,RetType,NewA, A, ACodeOut),
%f2p(Depth,HeadIs,RetType,NewB, B, BCodeOut),!.

into_eval_for(Depth,HeadIs,_Slf,_F,_Nth,RetType,Arg,NewArg,CodeOut):- is_list(Arg),
   f2p(Depth,HeadIs,RetType,NewArg,Arg, CodeOut),!.

into_eval_for(Depth,HeadIs,Slf,F,Nth,RetType,Arg,NewArgO,CodeOut):-
   compound(Arg), as_functor_args(Arg,AF,_A,Args),
   Compile = [AF|Args], !, into_eval_for(Depth,HeadIs,Slf,F,Nth,RetType,Compile,NewArgO,CodeOut),!.

into_eval_for(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,eval_for(b_3,PT,A,N)):- var(PT), get_type(A,PT),nonvar(PT),!.
into_eval_for(_Depth,_HeadIs,_Slf,F,Nth,PT,A,N,eval_for(b_2(Nth,F),PT,A,N)):- var(PT), !.
into_eval_for(_Depth,_HeadIs,_Slf,_F,_Nth,PT,A,N,eval_for(b_1,PT,A,N)):- nonvar(PT),PT\=='Atom', !.
into_eval_for(_Depth,_HeadIs,_Slf,_F,_Nth,_PT,A,A,true).

