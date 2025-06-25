
always_fix_prolog_term(Why,Coded,Fixd):-
   maybe_fix_prolog_term(Why,Coded,Fixd),!.
always_fix_prolog_term(_,Coded,Coded):-!.

maybe_fix_prolog_term(Why,Coded,Fixd):-
  once(fix_prolog_term(Why,[],Coded,M)), Coded\=@=M,!,
  always_fix_prolog_term(Why,M,Fixd).

fix_prolog_term(_,_,Coded,Fixd):- \+ compound(Coded),!,Coded=Fixd.
fix_prolog_term(_,_,Coded,Fixd):- is_ftVar(Coded),!,Coded=Fixd.
fix_prolog_term(Y,FL,Coded,Fixd):-
   copy_term(Coded,CodedC),
   do_fix_prolog_term(Y,FL,Coded,Fixd),
   \+ ((CodedC\=@=CodedC,
       (debug_info(double_sided_unification,t(CodedC\=@=CodedC)))),ignore((trace,throw(double_sided_unification)))),!.
fix_prolog_term(Y,FL,Coded,Fixd):- is_list(Coded),
   maplist(fix_prolog_term(Y,[list()|FL]),Coded,Fixd),!.
fix_prolog_term(Y,FL,Coded,Fixd):-
   compound_name_arguments(Coded,F,Args),
   maplist(fix_prolog_term(Y,[F|FL]),Args,OArgs),
   compound_name_arguments(Fixd,F,OArgs), !.
fix_prolog_term(_,_,Prolog,Prolog).


do_fix_prolog_term(_Y,_FL,u_assign(NN,Var,Nonvar),u_assign(NN,Nonvar,Var)):- is_ftVar(Var), \+ is_ftVar(Nonvar).



:- dynamic(maybe_optimize_prolog_term/4).
:- dynamic(maybe_optimize_prolog_assertion/4).

try_optimize_prolog(Y,Convert,Optimized):-  fail,
   once(catch_warn(maybe_optimize_prolog_assertion(Y,[],Convert,OptimizedM))),Convert\=@=OptimizedM,!,
   try_optimize_prolog(Y, OptimizedM,Optimized).
try_optimize_prolog(_,Optimized,Optimized).


try_harder_optimize_prolog(Y,Convert,Optimized):-  fail,
   once(catch_warn(maybe_optimize_prolog_assertion(Y,[],Convert,OptimizedM))),Convert\=@=OptimizedM,!,
   try_harder_optimize_prolog(OptimizedM,Optimized).
try_harder_optimize_prolog(_,Optimized,Optimized).

/*
try_optimize_prolog(Y,Convert,Optimized):-
   catch_warn(maybe_optimize_prolog_assertion(Y,[],Convert,MaybeOptimized)),
   actual_change(Convert,MaybeOptimized),!,
   try_optimize_prolog(Y,MaybeOptimized,Optimized).
*/

optimize_prolog_term(_,_,Converted,Optimized):- \+ compound(Converted),!,Converted=Optimized.
optimize_prolog_term(Y,FL,Converted,Optimized):-
   copy_term(Converted,ConvertedC),
   maybe_optimize_prolog_term(Y,FL,Converted,Optimized),
   \+ ((ConvertedC\=@=ConvertedC,
       (debug_info(double_sided_unification,t(ConvertedC\=@=ConvertedC)))),ignore((trace,throw(double_sided_unification)))),!.
optimize_prolog_term(Y,FL,Converted,Optimized):- is_list(Converted),
   maplist(optimize_prolog_term(Y,[list()|FL]),Converted,Optimized),!.
optimize_prolog_term(Y,FL,Converted,Optimized):-
   compound_name_arguments(Converted,F,Args),
   maplist(optimize_prolog_term(Y,[F|FL]),Args,OArgs),
   compound_name_arguments(Optimized,F,OArgs), !.
optimize_prolog_term(_,_,Prolog,Prolog).


actual_change(Body,BodyNew):- copy_term(Body+BodyNew,BodyC+BodyNewC,_), BodyC\=@=BodyNewC.

maybe_optimize_prolog_assertion(Y,Stack,Cmpd,(:-BodyNew)):-  compound(Cmpd),(:-Body)=Cmpd,compound(Body),!,
  maybe_optimize_prolog_assertion(Y,Stack,(cl:-Body),(cl:-BodyNew)).

maybe_optimize_prolog_assertion(_,_,CmpdIn,(Cl:-BodyNew)):-
  compound(CmpdIn),subst_vars(CmpdIn,Cmpd),
  (Cl:-Body)=Cmpd,compound(Body),%copy_term(Body,BodyC),
  must_optimize_whole_body(Cl,Body,BodyNew).

must_optimize_whole_body(Head, Body, BodyNew) :-  %fail,
    term_variables(Body, Vars),
    member(Var,Vars),
    % Count variable usage across full term (Includes Head)
    var_count_in_term(Head+Body,Var,Count),
    %copy_term(Body,BodyC),
    inline_var_maybe(Var, Count, Body, BodyNew), Body \=@= BodyNew, !.

must_optimize_whole_body(_Cl, Body, BodyNew) :- fail,
   sub_term_safely(Sub, Body), compound(Sub), Sub = (L = R),
   L==R, subst001(Body, Sub , true, BodyNew), !.

must_optimize_whole_body(Cl, Body, BodyNew) :- % fail,
     optimize_body(Cl, Body, BodyNew).




must_optimize_body(A,B,CC):- once(optimize_body(A,B,C)), C \=@= B,!, must_optimize_body(A,C,CC).
must_optimize_body(_,B,C):- B =C.

optimize_body(_HB,Body,Body):- skip_mizer,!.
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

optimize_body_unit(I,O):-
   copy_term(I,II),
   optimize_unit1(I,Opt),I=@=II,!,
   optimize_body_unit(Opt,O).
optimize_body_unit(O,O).


ok_to_append('$VAR'):- !, fail.
ok_to_append(_).

number_wang(A,B,C):-
  (numeric(C);numeric(A);numeric(B)),!,
  maplist(numeric_or_var,[A,B,C]),
  maplist(decl_numeric,[A,B,C]),!.



optimize_u_assign_1(_,Var,_,_):- is_nsVar(Var),!,fail.


optimize_u_assign_1(_HB,[H|T],R,Code):- symbol(H),length([H|T],Arity),
   predicate_arity(F,A),Arity==A, \+ (predicate_arity(F,A2),A2\=A),
    append([H|T],[R],ArgsR),Code=..ArgsR,!.
optimize_u_assign_1(HB,Compound,R,Code):- \+ compound(Compound),!, optimize_u_assign(HB,Compound,R,Code).
optimize_u_assign_1(HB,[H|T],R,Code):- !, optimize_u_assign(HB,[H|T],R,Code).
optimize_u_assign_1(_ ,Compound,R,Code):-
   is_list(R),var(Compound),
   into_u_assign(R,Compound,Code),!.

optimize_u_assign_1(_,Compound,R,Code):- ar2p(Compound,R,Code),!.
optimize_u_assign_1(_,Compound,R,Code):-
  compound(Compound),
  as_functor_args(Compound,F,N0), N is N0 +1,
  (predicate_arity(F,N); functional_predicate_arg(F, N, N)),
   append_term_or_call(Compound,R,Code).
optimize_u_assign_1(HB,Compound,R,Code):- p2s(Compound,MeTTa),   optimize_u_assign(HB,MeTTa,R,Code).
%optimize_u_assign_1(_,[Pred| ArgsL], R, u_assign([Pred| ArgsL],R)).

append_term_or_call([F|Compound],R,Code):- symbol(F),
  is_list(Compound),append(Compound,[R],CodeL),
            Code=..[F|CodeL],!.
append_term_or_call(F,R,Code):- symbol(F),!, Code=..[F,R].
append_term_or_call(F,R,Code):- append_term(F,R,Code),!.
append_term_or_call(F,R,call(F,R)).


optimize_unit1(
  eval_true([==, ['get-metatype', A], 'Expression']),
  call('get-metatype',A,'Expression')).

optimize_unit1( eval_true([==, [GM, Eval], Val]), call(GM,Eval,Val)):-
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.

optimize_unit1( ==([GM,Eval],Val,C), call(GM,Eval,Val)):- C==Eval,
    symbol(GM), predicate_arity(GM,2), \+ predicate_arity(GM,1),
    symbol(Val),var(Eval),!.

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

p2s(P,S):- into_list_args(P,S).

get_decl_type(N,DT):- attvar(N),get_atts(N,AV),sub_term(DT,AV),symbol(DT).

numeric(N):- number(N),!.
numeric(N):- compound(N), !, fail.
numeric(N):- get_attr(N,'Number','Number').
numeric(N):- get_decl_type(N,DT),(DT=='Int',DT=='Number').
decl_numeric(N):- numeric(N),!.
decl_numeric(N):- ignore((var(N),put_attr(N,'Number','Number'))).
numeric_or_var(N):- var(N),!.
numeric_or_var(N):- numeric(N),!.
numeric_or_var(N):- \+ compound(N),!,fail.
numeric_or_var('$VAR'(_)).

non_compound(S):- \+ compound(S).

did_optimize_conj(Head,B1,B2,B12):- optimize_conj(Head,B1,B2,B12), B12\=@=(B1,B2),!.

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

optimize_conj(_Head, B1,B2,eval_true(E)):-
        B2 = is_True(True_Eval),
        B1 = eval(E,True_Eval1),
        True_Eval1 == True_Eval,!.

optimize_conj(HB, RR, C=A, RR):- compound(RR),is_nsVar(C),is_nsVar(A),
  as_functor_args(RR,_,_,Args),is_list(Args), member(CC,Args),var(CC), CC==C,
    count_var(HB,C,N),N=2,C=A,!.

optimize_conj(_, u_assign(Term, C), u_assign(True,CC), eval_true(Term)):-
   'True'==True, CC==C.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(HB, u_assign(Term, C), C=A, u_assign(Term,A)):- is_ftVar(C),is_ftVar(A),count_var(HB,C,N),N=2,!.
optimize_conj(_, u_assign(Term, C), is_True(CC), eval_true(Term)):- CC==C, !.
optimize_conj(HB, B1,BT,B1):- assumed_true(HB,BT),!.
optimize_conj(HB, BT,B1,B1):- assumed_true(HB,BT),!.
%optimize_conj(Head, u_assign(Term, C), u_assign(True,CC), Term):- 'True'==True,
%     optimize_conj(Head, u_assign(Term, C), is_True(CC), CTerm).
%optimize_conj(Head,B1,BT,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
%optimize_conj(Head,BT,B1,BN1):- assumed_true(HB,BT),!, optimize_body(Head,B1,BN1).
optimize_conj(Head,B1,B2,(BN1,BN2)):-
   optimize_body(Head,B1,BN1), optimize_body(Head,B2,BN2).

assumed_true(_,_):- skip_mizer,!,fail.
assumed_true(_ ,B2):- var(B2),!,fail.
assumed_true(HB,eval_true(B2)):-!,assumed_true(HB,B2).
assumed_true(_ ,B2):- B2==is_True('True').
assumed_true(_ ,B2):- B2=='True'.
assumed_true(_ ,B2):- B2== true,!.
assumed_true(_ ,B2):- B2==u_assign('True', '$VAR'('_')),!.
assumed_true(HB,X==Y):- !, assumed_true(HB,X=Y).
assumed_true( _,X=Y):- X==Y,!.
assumed_true(HB,X=Y):- is_nsVar(X),is_nsVar(Y),
  ( \+ (X\=Y)),(count_var(HB,Y,2);count_var(HB,X,2)),
  X=Y,!.

