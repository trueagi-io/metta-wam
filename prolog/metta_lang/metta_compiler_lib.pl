:- dynamic(transpiler_predicate_store/7).
:- discontiguous transpiler_predicate_store/7.
:- discontiguous transpiler_predicate_nary_store/9.
:- discontiguous compile_flow_control/8.

from_prolog_args(_,X,X).
:-dynamic(pred_uses_fallback/2).
:-dynamic(pred_uses_impl/2).

pred_uses_impl(F,A):- transpile_impl_prefix(F,A,Fn),current_predicate(Fn/A).

use_interpreter:- fail.
mc__1_fallback_unimpl(Fn,Arity,Args,Res):- \+ use_interpreter, !,
  (pred_uses_fallback(Fn,Arity); (length(Args,Len), \+ pred_uses_impl(Fn,Len))),!,
    get_operator_typedef_props(_,Fn,Arity,Types,_RetType0),
    current_self(Self),
    maybe_eval(Self,Types,Args,NewArgs),
    [Fn|NewArgs]=Res.

%mc__1_fallback_unimpl(Fn,_Arity,Args,Res):-  u_assign([Fn|Args], Res).

maybe_eval(_Self,_Types,[],[]):-!.
maybe_eval(Self,[T|Types],[A|Args],[N|NewArgs]):-
    into_typed_arg(30,Self,T,A,N),
    maybe_eval(Self,Types,Args,NewArgs).


%'mc__1_2_:'(Obj, Type, [':',Obj, Type]):- current_self(Self), sync_type(10, Self, Obj, Type). %freeze(Obj, get_type(Obj,Type)),!.
%sync_type(D, Self, Obj, Type):- nonvar(Obj), nonvar(Type), !, arg_conform(D, Self, Obj, Type).
%sync_type(D, Self, Obj, Type):- nonvar(Obj), var(Type), !, get_type(D, Self, Obj, Type).
%sync_type(D, Self, Obj, Type):- nonvar(Type), var(Obj), !, set_type(D, Self, Obj, Type). %, freeze(Obj, arg_conform(D, Self, Obj, Type)).
%sync_type(D, Self, Obj, Type):- freeze(Type,sync_type(D, Self, Obj, Type)), freeze(Obj, sync_type(D, Self, Obj, Type)),!.

transpiler_predicate_store(builtin, 'get-type', [1], '@doc', '@doc', [x(noeval,eager,[])], x(doeval,eager,[])).
%'mc__1_1_get-type'(Obj,Type) :-  attvar(Obj),current_self(Self),!,trace,get_attrs(Obj,Atts),get_type(10, Self, Obj,Type).
'mc__1_1_get-type'(Obj,Type) :- current_self(Self), !, get_type(10, Self, Obj,Type).

%%%%%%%%%%%%%%%%%%%%% if

transpiler_predicate_store(builtin, 'if', [3], '@doc', '@doc', [x(doeval,eager,[]),x(doeval,lazy,[]),x(doeval,lazy,[])], x(doeval,lazy,[])).
'mc__1_3_if'(If,Then,Else,Result) :- (If*->Result=Then;Result=Else).
transpiler_predicate_store(builtin, 'if', [2], '@doc', '@doc', [x(doeval,eager,[]),x(doeval,lazy,[])], x(doeval,lazy,[])).
'mc__1_2_if'(If,Then,Result) :- (If*->Result=Then;fail).

compile_flow_control(HeadIs,LazyVars,RetResult,RetResultN,LazyEval,Convert, Converted, ConvertedN) :-
  Convert = ['if',Cond,Then,Else],!,
  f2p(HeadIs,LazyVars,CondResult,CondResultN,LazyRetCond,Cond,CondCode,CondCodeN),
  lazy_impedance_match(LazyRetCond,x(doeval,eager,[]),CondResult,CondCode,CondResultN,CondCodeN,CondResult1,CondCode1),
  append(CondCode1,[[native(is_True),CondResult1]],If),
  compile_test_then_else(HeadIs,RetResult,RetResultN,LazyVars,LazyEval,If,Then,Else,Converted, ConvertedN).

compile_flow_control(HeadIs,LazyVars,RetResult,RetResultN,LazyEval,Convert, Converted, ConvertedN) :-
  Convert = ['if',Cond,Then],!,
  f2p(HeadIs,LazyVars,CondResult,CondResultN,LazyRetCond,Cond,CondCode,CondCodeN),
  lazy_impedance_match(LazyRetCond,x(doeval,eager,[]),CondResult,CondCode,CondResultN,CondCodeN,CondResult1,CondCode1),
  append(CondCode1,[[native(is_True),CondResult1]],If),
  compile_test_then_else(HeadIs,RetResult,RetResultN,LazyVars,LazyEval,If,Then,'Empty',Converted, ConvertedN).

compile_test_then_else(HeadIs,RetResult,RetResultN,LazyVars,LazyEval,If,Then,Else,Converted, ConvertedN):-
  f2p(HeadIs,LazyVars,ThenResult,ThenResultN,ThenLazyEval,Then,ThenCode,ThenCodeN),
  f2p(HeadIs,LazyVars,ElseResult,ElseResultN,ElseLazyEval,Else,ElseCode,ElseCodeN),
  arg_properties_widen(ThenLazyEval,ElseLazyEval,LazyEval),
  %(Else=='Empty' -> LazyEval=ThenLazyEval ; arg_properties_widen(ThenLazyEval,ElseLazyEval,LazyEval)),
  %lazy_impedance_match(ThenLazyEval,LazyEval,ThenResult,ThenCode,ThenResultN,ThenCodeN,ThenResult1,ThenCode1),
  %lazy_impedance_match(ElseLazyEval,LazyEval,ElseResult,ElseCode,ElseResultN,ElseCodeN,ElseResult1,ElseCode1),
  % cannnot use add_assignment here as might not want to unify ThenResult and ElseResult
  append(ThenCode,[[assign,RetResult,ThenResult]],T),
  append(ElseCode,[[assign,RetResult,ElseResult]],E),
  Converted=[[prolog_if,If,T,E]],
  append(ThenCodeN,[[assign,RetResultN,ThenResultN]],TN),
  append(ElseCodeN,[[assign,RetResultN,ElseResultN]],EN),
  ConvertedN=[[prolog_if,If,TN,EN]].

/*
transpiler_predicate_store(builtin, 'if-decons-expr', [5], '@doc', '@doc', [x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,eager,[]),x(doeval,lazy,[]),x(doeval,lazy,[])], x(doeval,lazy,[])).
'mc__1_5_if-decons-expr'(If,H,T,Then,Else,Result) :- (If=[H|T]*->Result=Then;Result=Else).
compile_flow_control(HeadIs,LazyVars,RetResult,RetResultN,LazyEval,Convert, Converted, ConvertedN) :-
  Convert = ['if-decons-expr',Cond,Head,Tail,Then,Else],!,
  f2p(HeadIs,LazyVars,CondResult,CondResultN,LazyRetCond,Cond,CondCode,CondCodeN),
  lazy_impedance_match(LazyRetCond,x(doeval,eager,[]),CondResult,CondCode,CondResultN,CondCodeN,CondResult1,CondCode1),
  f2p(HeadIs,LazyVars,HeadResult,HeadResultN,LazyRetHead,Head,HeadCode,HeadCodeN),
  lazy_impedance_match(LazyRetHead,x(doeval,eager,[]),HeadResult,HeadCode,HeadResultN,HeadCodeN,HeadResult1,HeadCode1),
  f2p(HeadIs,LazyVars,TailResult,TailResultN,LazyRetTail,Tail,TailCode,TailCodeN),
  lazy_impedance_match(LazyRetTail,x(doeval,eager,[]),TailResult,TailCode,TailResultN,TailCodeN,TailResult1,TailCode1),
  append([CondCode1,[[assign,CondResult1,list_with_tail([H],T)]],HeadCode1,[[assign,H,HeadResult1]],TailCode1,[[assign,T,TailResult1]]],If),
  compile_test_then_else(HeadIs,RetResult,RetResultN,LazyVars,LazyEval,If,Then,Else,Converted,ConvertedN).
*/
%%%%%%%%%%%%%%%%%%%%% case. NOTE: there is no library equivalent for this, as various parts of the structure have to be lazy

compile_flow_control(HeadIs,LazyVars,RetResult,RetResultN,LazyEval,Convert,Converted,ConvertedN) :-
   %trace,
   Convert=['case',Value,Cases],!,
   f2p(HeadIs,LazyVars,ValueResult,ValueResultN,LazyRetValue,Value,ValueCode,ValueCodeN),
   lazy_impedance_match(LazyRetValue,x(doeval,eager,[]),ValueResult,ValueCode,ValueResultN,ValueCodeN,ValueResult1,ValueCode1),
   lazy_impedance_match(LazyRetValue,x(doeval,eager,[]),ValueResult,ValueCode,ValueResultN,ValueCodeN,ValueResult1,ValueCode1),
   ValueCode1a=[[prolog_if,ValueCode1,[[assign,ValueResult1a,ValueResult1]],[[assign,ValueResult1a,'Empty']]]],
   compile_flow_control_case(HeadIs,LazyVars,RetResult,RetResultN,LazyEval,ValueResult1a,Cases,Converted0,Converted0N),
   append(ValueCode1a,Converted0,Converted),
   append(ValueCode1a,Converted0N,ConvertedN).

compile_flow_control_case(_,_,RetResult,RetResultN,x(doeval,eager,[]),_,[],[[assign,RetResult,'Empty']],[[assign,RetResultN,'Empty']]) :- !.
compile_flow_control_case(HeadIs,LazyVars,RetResult,RetResult,LazyEval,ValueResult,[[Match,Target]|Rest],Converted,ConvertedN) :-
   f2p(HeadIs,LazyVars,MatchResult,MatchResultN,LazyRetMatch,Match,MatchCode,MatchCodeN),
   lazy_impedance_match(LazyRetMatch,x(doeval,eager,[]),MatchResult,MatchCode,MatchResultN,MatchCodeN,MatchResult1,MatchCode1),
   f2p(HeadIs,LazyVars,TargetResult,TargetResultN,LazyEval0,Target,TargetCode,TargetCodeN),
   compile_flow_control_case(HeadIs,LazyVars,RestResult,RestResultN,LazyEval1,ValueResult,Rest,RestCode,RestCodeN),
   arg_properties_widen(LazyEval0,LazyEval1,LazyEval),
   lazy_impedance_match(LazyEval0,LazyEval,TargetResult,TargetCode,TargetResultN,TargetCodeN,TargetResult1,TargetCode1),
   lazy_impedance_match(LazyEval1,LazyEval,RestResult,RestCode,RestResultN,RestCodeN,RestResult1,RestCode1),
   append(TargetCode1,[[assign,RetResult,TargetResult1]],T),
   append(RestCode1,[[assign,RetResult,RestResult1]],R),
   append(MatchCode1,[[prolog_if,[[prolog_match,ValueResult,MatchResult1]],T,R]],Converted),
   append(TargetCodeN,[[assign,RetResult,TargetResultN]],TN),
   append(RestCodeN,[[assign,RetResult,RestResultN]],RN),
   append(MatchCode1,[[prolog_if,[[prolog_match,ValueResult,MatchResult1]],TN,RN]],ConvertedN).

/*
compile_flow_control(HeadIs,LazyVars,RetResult,LazyEval,Convert, Converted) :-
  Convert = ['case', Eval, CaseList],!,
  f2p(HeadIs, LazyVars, Var, x(doeval,eager,[]), Eval, CodeCanFail),
  case_list_to_if_list(Var, CaseList, IfList, [empty], IfEvalFails),
  compile_test_then_else(RetResult, LazyVars, LazyEval, CodeCanFail, IfList, IfEvalFails, Converted).

case_list_to_if_list(_Var, [], [empty], EvalFailed, EvalFailed) :-!.
case_list_to_if_list(Var, [[Pattern, Result] | Tail], Next, _Empty, EvalFailed) :-
    (Pattern=='Empty'; Pattern=='%void%'), !, % if the case Failed
    case_list_to_if_list(Var, Tail, Next, Result, EvalFailed).
case_list_to_if_list(Var, [[Pattern, Result] | Tail], Out, IfEvalFailed, EvalFailed) :-
    case_list_to_if_list(Var, Tail, Next, IfEvalFailed, EvalFailed),
    Out = ['if', [metta_unify, Var, Pattern], Result, Next].
*/

%%%%%%%%%%%%%%%%%%%%% arithmetic

transpiler_predicate_store(builtin, '+', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[number])).
'mc__1_2_+'(A,B,R) :- integer(A),integer(B),!,plus(A,B,R).
'mc__1_2_+'(A,B,R) :- number(A),number(B),!,R is A+B.
'mc__1_2_+'(A,B,['+',A,B]).

transpiler_predicate_store(builtin, '-', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[number])).
'mc__1_2_-'(A,B,R) :- integer(A),integer(B),!,plus(B,R,A).
'mc__1_2_-'(A,B,R) :- number(A),number(B),!,R is A-B.
'mc__1_2_-'(A,B,['-',A,B]).

transpiler_predicate_store(builtin, '*', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[number])).
'mc__1_2_*'(A,B,R) :- number(A),number(B),!,R is A*B.
'mc__1_2_*'(A,B,['*',A,B]).

transpiler_predicate_store(builtin, '/', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[number])).
'mc__1_2_/'(A,B,R) :- number(A),number(B),!,R is A/B.
'mc__1_2_/'(A,B,['/',A,B]).

transpiler_predicate_store(builtin, '%', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[number])).
'mc__1_2_%'(A,B,R) :- number(A),number(B),!,R is A mod B.
'mc__1_2_%'(A,B,['%',A,B]).

%%%%%%%%%%%%%%%%%%%%% logic

%transpiler_predicate_store(builtin, 'and', [2], '@doc', '@doc', [x(doeval,eager,[boolean]), x(doeval,eager,[boolean])], x(doeval,eager,[boolean])).
%mc__1_2_and(A,B,B) :- atomic(A), A\=='False', A\==0, !.
%mc__1_2_and(_,_,'False').

%transpiler_predicate_store(builtin, 'or', [2], '@doc', '@doc', [x(doeval,eager), x(doeval,eager,[boolean])], x(doeval,eager,[boolean])).
%mc__1_2_or(A,B,B):- (\+ atomic(A); A='False'; A=0), !.
%mc__1_2_or(_,_,'True').

transpiler_predicate_store(builtin, 'and', [2], '@doc', '@doc', [x(doeval,eager,[boolean]), x(doeval,lazy,[boolean])], x(doeval,eager,[boolean])).
mc__1_2_and(A,B,C) :- atomic(A), A\=='False', A\==0, !, as_p1_exec(B,C).
mc__1_2_and(_,_,'False').
compile_flow_control(HeadIs,LazyVars,RetResult,RetResultN,LazyEval,Convert, Converted, ConvertedN) :-
  Convert = ['and',A,B],!,
  LazyEval=x(doeval,eager,[boolean]),
  % eval case
  f2p(HeadIs,LazyVars,AResult,AResultN,LazyRetA,A,ACode,ACodeN),
  lazy_impedance_match(LazyRetA,x(doeval,eager,[boolean]),AResult,ACode,AResultN,ACodeN,AResult1,ACode1),
  f2p(HeadIs,LazyVars,BResult,BResultN,LazyRetB,B,BCode,BCodeN),
  lazy_impedance_match(LazyRetB,x(doeval,eager,[boolean]),BResult,BCode,BResultN,BCodeN,BResult1,BCode1),
  append(ACode1,[[native(is_True),AResult1]],ATest),
  append(BCode1,[[assign,RetResult,BResult1]],BTest),
  CodeIf=[[prolog_if,ATest,BTest,[[assign,RetResult,'False']]]],
  Converted=CodeIf,
  % noeval case
  maplist(f2p(HeadIs,LazyVars), _RetResultsParts, RetResultsPartsN, LazyResultParts, Convert, _ConvertedParts, ConvertedNParts),
  f2p_do_group(x(noeval,eager,[]),LazyResultParts,RetResultsPartsN,NoEvalRetResults,ConvertedNParts,NoEvalCodeCollected),
  assign_or_direct_var_only(NoEvalCodeCollected,RetResultN,list(NoEvalRetResults),ConvertedN).

transpiler_predicate_store(builtin, 'or', [2], '@doc', '@doc', [x(doeval,eager,[boolean]), x(doeval,lazy,[boolean])], x(doeval,eager,[boolean])).
mc__1_2_or(A,B,C):- (\+ atomic(A); A='False'; A=0), !, as_p1_exec(B,C).
mc__1_2_or(_,_,'True').
compile_flow_control(HeadIs,LazyVars,RetResult,RetResultN,LazyEval,Convert, Converted, ConvertedN) :-
  Convert = ['or',A,B],!,
  LazyEval=x(doeval,eager,[boolean]),
  % eval case
  f2p(HeadIs,LazyVars,AResult,AResultN,LazyRetA,A,ACode,ACodeN),
  lazy_impedance_match(LazyRetA,x(doeval,eager,[boolean]),AResult,ACode,AResultN,ACodeN,AResult1,ACode1),
  f2p(HeadIs,LazyVars,BResult,BResultN,LazyRetB,B,BCode,BCodeN),
  lazy_impedance_match(LazyRetB,x(doeval,eager,[boolean]),BResult,BCode,BResultN,BCodeN,BResult1,BCode1),
  append(ACode1,[[native(is_True),AResult1]],ATest),
  append(BCode1,[[assign,RetResult,BResult1]],BTest),
  CodeIf=[[prolog_if,ATest,[[assign,RetResult,'True']],BTest]],
  Converted=CodeIf,
  % noeval case
  maplist(f2p(HeadIs,LazyVars), _RetResultsParts, RetResultsPartsN, LazyResultParts, Convert, _ConvertedParts, ConvertedNParts),
  f2p_do_group(x(noeval,eager,[]),LazyResultParts,RetResultsPartsN,NoEvalRetResults,ConvertedNParts,NoEvalCodeCollected),
  assign_or_direct_var_only(NoEvalCodeCollected,RetResultN,list(NoEvalRetResults),ConvertedN).

transpiler_predicate_store(builtin, 'not', [1], '@doc', '@doc', [x(doeval,eager,[boolean])], x(doeval,eager,[boolean])).
mc__1_1_not(A,'False') :- atomic(A), A\=='False', A\==0, !.
mc__1_1_not(_,'True').

%%%%%%%%%%%%%%%%%%%%% comparison

% not sure about the signature for this one
transpiler_predicate_store(builtin, '==', [2], '@doc', '@doc', [x(doeval,eager,[]), x(doeval,eager,[])], x(doeval,eager,[boolean])).
%'mc__1_2_=='(A,B,TF):- eval_40(['==',A,B],TF).
'mc__1_2_=='(A,B,TF) :- var(A),!,as_tf(A==B,TF).
'mc__1_2_=='(A,B,TF) :- as_tf(A=@=B,TF).

transpiler_predicate_store(builtin, '<', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[boolean])).
'mc__1_2_<'(A,B,R) :- number(A),number(B),!,(A<B -> R='True' ; R='False').
'mc__1_2_<'(A,B,['<',A,B]).

transpiler_predicate_store(builtin, '>', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[boolean])).
'mc__1_2_>'(A,B,R) :- number(A),number(B),!,(A>B -> R='True' ; R='False').
'mc__1_2_>'(A,B,['>',A,B]).

transpiler_predicate_store(builtin, '>=', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[boolean])).
'mc__1_2_>='(A,B,R) :- number(A),number(B),!,(A>=B -> R='True' ; R='False').
'mc__1_2_>='(A,B,['>=',A,B]).

transpiler_predicate_store(builtin, '<=', [2], '@doc', '@doc', [x(doeval,eager,[number]), x(doeval,eager,[number])], x(doeval,eager,[boolean])).
'mc__1_2_<='(A,B,R) :- number(A),number(B),!,(A=<B -> R='True' ; R='False'). % note that Prolog has a different syntax '=<'
'mc__1_2_<='(A,B,['<=',A,B]).

%%%%%%%%%%%%%%%%%%%%% lists

transpiler_predicate_store(builtin, 'car-atom', [1], '@doc', '@doc', [x(noeval,eager,[list])], x(noeval,eager,[])).
'mc__1_1_car-atom'([H|_],H).

transpiler_predicate_store(builtin, 'cdr-atom', [1], '@doc', '@doc', [x(noeval,eager,[list])], x(noeval,eager,[list])).
'mc__1_1_cdr-atom'([_|T],T).

transpiler_predicate_store(builtin, 'cons-atom', [2], '@doc', '@doc', [x(noeval,eager,[]), x(noeval,eager,[list])], x(noeval,eager,[list])).
'mc__1_2_cons-atom'(A,B,[A|B]).

transpiler_predicate_store(builtin, 'decons-atom', [1], '@doc', '@doc', [x(noeval,eager,[list])], x(noeval,eager,[list])).
'mc__1_1_decons-atom'([A|B],[A,B]).

%transpiler_predicate_store(builtin, 'length', [1], '@doc', '@doc', [x(noeval,eager,[list])], x(noeval,eager,[number])).
%'mc__1_1_length'(L,S) :- length(L,S).

transpiler_predicate_store(builtin, 'size-atom', [1], '@doc', '@doc', [x(noeval,eager,[list])], x(noeval,eager,[number])).
'mc__1_1_size-atom'(L,S) :- length(L,S).

%%%%%%%%%%%%%%%%%%%%% set

lazy_member(P,R2) :- as_p1_exec(R2,P).

transpiler_predicate_store(builtin, subtraction, [2], '@doc', '@doc', [x(doeval,lazy,[]),x(doeval,lazy,[])], x(doeval,eager,[])).
% QUESTION: which one of these to use?
% * The first is more time efficient (calculates the set for S2 and stores in Avoid)
%'mc__1_2_subtraction'(S1,S2,R) :- 'mc__1_1_collapse'(S2,Avoid),as_p1_exec(S1,R), \+ member(R,Avoid).
% the second is more memory efficient (steps through S2 every time, but does not need to store anything)
'mc__1_2_subtraction'(S1,S2,R) :- as_p1_exec(S1,R), \+ lazy_member(R,S2).

transpiler_predicate_store(builtin, union, [2], '@doc', '@doc', [x(doeval,lazy,[]),x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_2_union'(S1,S2,R) :- as_p1_exec(S1,R) ; 'mc__1_2_subtraction'(S2,S1,R).

%transpiler_predicate_store(builtin, intersection, [2], '@doc', '@doc', [x(doeval,lazy,[]),x(doeval,lazy,[])], x(doeval,eager,[])).
%'mc__1_2_intersection'(S1,S2,R)

transpiler_predicate_store(builtin, unique, [1], '@doc', '@doc', [x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_1_unique'(S,R) :- 'mc__1_1_collapse'(S,S0),list_to_set(S0,R).

transpiler_predicate_store(builtin, 'unique-atom', [1], '@doc', '@doc', [x(doeval,eager,[])], x(doeval,eager,[])).
'mc__1_1_unique-atom'(S,R) :- list_to_set(S,R).

transpiler_predicate_store(builtin, limit, [2], '@doc', '@doc', [x(doeval,eager,[number]),x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_2_limit'(N,S,R) :- integer(N),N>=0,limit(N,as_p1_exec(S,R)).

transpiler_predicate_store(builtin, 'limit!', [2], '@doc', '@doc', [x(doeval,eager,[number]),x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_2_limit!'(N,S,R) :- integer(N),N>=0,limit(N,as_p1_exec(S,R)).

%%%%%%%%%%%%%%%%%%%%% superpose, collapse

transpiler_predicate_store(builtin, superpose, [1], '@doc', '@doc', [x(doeval,eager,[])], x(noeval,eager,[])).
'mc__1_1_superpose'(S,R) :- member(R,S).

transpiler_predicate_store(builtin, collapse, [1], '@doc', '@doc', [x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_1_collapse'(ispu(X),[X]) :- !.
'mc__1_1_collapse'(ispuU(Ret,Code),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc__1_1_collapse'(ispuU(X,true),[X]) :- !.
'mc__1_1_collapse'(ispuU(A,Code),X) :- atom(A),!,findall(_,Code,X),maplist(=(A),X).
'mc__1_1_collapse'(ispen(Ret,Code,_),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc__1_1_collapse'(ispeEn(X,true,_),[X]) :- !.
'mc__1_1_collapse'(ispeEn(A,Code,_),X) :- atom(A),!,findall(_,Code,X),maplist(=(A),X).
'mc__1_1_collapse'(ispeEnN(Ret,Code,_,_),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc__1_1_collapse'(ispeEnN(X,true,_,_),[X]) :- !.
'mc__1_1_collapse'(ispeEnN(A,Code,_,_),X) :- atom(A),!,findall(_,Code,X),maplist(=(A),X).
'mc__1_1_collapse'(ispeEnNC(Ret,Code,_,_,Common),R) :- fullvar(Ret),!,findall(Ret,(Common,Code),R).
'mc__1_1_collapse'(ispeEnNC(A,Code,_,_,Common),X) :- atom(A),!,findall(_,(Common,Code),X),maplist(=(A),X).
'mc__1_1_collapse'(X,_) :- format("Error in library collapse: ~w",[X]),throw(0).

%%%%%%%%%%%%%%%%%%%%% spaces

transpiler_predicate_store(builtin, 'bind!', [2], '@doc', '@doc', [x(noeval,eager,[]), x(doeval,eager,[])], x(doeval,eager,[])).
'mc__1_2_bind!'(Name,Expression,[]) :- nb_bind(Name,Expression).

transpiler_predicate_store(builtin, 'new-space', [0], '@doc', '@doc', [], x(doeval,eager,[])).
'mc__1_0_new-space'(Space) :- is_make_new_kb(['new-space'],Space,[]).

convert_space('&self','&top') :- !.
convert_space(S,S).

transpiler_predicate_store(builtin, 'add-atom', [2], '@doc', '@doc', [x(doeval,eager,[]), x(noeval,eager,[])], x(doeval,eager,[])).
'mc__1_2_add-atom'(Space,PredDecl,[]) :- convert_space(Space,Space1),A=metta_atom_asserted(Space1,PredDecl),(call(A) -> true ; assertz(A)).

transpiler_predicate_store(builtin, 'remove-atom', [2], '@doc', '@doc', [x(doeval,eager,[]), x(noeval,eager,[])], x(doeval,eager,[])).
'mc__1_2_remove-atom'(Space,PredDecl,[]) :- convert_space(Space,Space1),retractall(metta_atom_asserted(Space1,PredDecl)).

transpiler_predicate_store(builtin, 'get-atoms', [1], '@doc', '@doc', [x(noeval,eager,[])], x(noeval,eager,[])).
'mc__1_1_get-atoms'(Space,Atoms) :- metta_atom(Space, Atoms).

% This allows match to supply hits to the correct metta_atom/2 (Rather than sending a variable
match_pattern(Space, Pattern):-
    if_t(compound(Pattern),
       (functor(Pattern,F,A,Type), functor(Atom,F,A,Type))),
    metta_atom(Space, Atom),
    %unify_with_occurs_check(Atom,Pattern). % 0.262 secs.
    Atom=Pattern. % 0.170 secs
    %wocf(Atom=Pattern).
    %woc(Atom=Pattern). %  2.09 seconds.

transpiler_predicate_store(builtin, match, [3], '@doc', '@doc', [x(doeval,eager,[]), x(noeval,eager,[]), x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_3_match'(Space,P,P1,Ret) :- is_list(P),P=[Comma|Patterns],Comma==',',!,(maplist(match_aux(Space),Patterns) -> as_p1_exec(P1,Ret) ; fail).
'mc__1_3_match'(Space,Pattern,P1,Ret) :- match_pattern(Space, Atom),unify_with_occurs_check(Atom,Pattern),as_p1_exec(P1,Ret).
%'mc__1_3_match'(Space,Pattern,P1,Ret) :- match_pattern(Space, Atom),format("match1 ~w: ~w:\n",[Pattern,Atom]),Atom=Pattern,as_p1_exec(P1,Ret),format("match2 ~w:\n",[Ret]),trace.
%transpiler_predicate_store(builtin, match, [3], '@doc', '@doc', [x(doeval,eager,[]), x(doeval,lazy,[]), x(doeval,lazy,[])], x(doeval,eager,[])).
%'mc__1_3_match'(Space,Pattern,P1,Ret) :- match_pattern(Space, Atom),as_p1_exec(Pattern,Atom),as_p1_exec(P1,Ret).

match_aux(Space,Pattern) :- 'mc__1_3_match'(Space,Pattern,ispu(true),true).

% unify calls pattern matching if arg1 is a space
unify_pattern(Space,Pattern):- is_metta_space(Space),!, match_pattern(Space, Pattern).
% otherwise calls prolog unification (with occurs check later)
unify_pattern(Atom, Pattern):- metta_unify(Atom, Pattern).

metta_unify(Atom, Pattern):- Atom=Pattern.

% TODO FIXME: sort out the difference between unify and match
transpiler_predicate_store(builtin, unify, [3], '@doc', '@doc', [x(doeval,eager,[]), x(doeval,eager,[]), x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_3_unify'(Space,Pattern,P1,Ret) :- unify_pattern(Space, Atom),Atom=Pattern,as_p1_exec(P1,Ret).

transpiler_predicate_store(builtin, unify, [4], '@doc', '@doc', [x(doeval,eager,[]), x(doeval,eager,[]), x(doeval,lazy,[]), x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_4_unify'(Space,Pattern,Psuccess,PFailure,RetVal) :-
    (unify_pattern(Space,Pattern) -> as_p1_exec(Psuccess,RetVal) ; as_p1_exec(PFailure,RetVal)).

%%%%%%%%%%%%%%%%%%%%% variable arity functions

transpiler_predicate_nary_store(builtin, progn, 0, [], 'Atom', 'Atom', [], x(doeval,eager,[]), x(doeval,eager,[])).
'mc_n_0__progn'(List,Ret) :- append(_,[Ret],List).

transpiler_predicate_nary_store(builtin, 'call-fn!', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(noeval,eager,[]), x(doeval,eager,[])).
'mc_n_1__call-fn!'(Fn,List,Ret) :- append(List,[Ret],List2),apply(Fn,List2).

transpiler_predicate_nary_store(builtin, 'call-fn', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(doeval,eager,[]), x(doeval,eager,[])).
'mc_n_1__call-fn'(Fn,List,Ret) :- append(List,[Ret],List2),apply(Fn,List2).

transpiler_predicate_nary_store(builtin, 'call-p!', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(noeval,eager,[]), x(doeval,eager,[bool])).
'mc_n_1__call-p!'(Fn,List,Ret) :- (apply(Fn,List)->Ret='True';Ret='False').

transpiler_predicate_nary_store(builtin, 'call-p', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(doeval,eager,[]), x(doeval,eager,[bool])).
'mc_n_1__call-p'(Fn,List,Ret) :- (apply(Fn,List)->Ret='True';Ret='False').

%%%%%%%%%%%%%%%%%%%%% misc

transpiler_predicate_store(builtin, time, [1], '@doc', '@doc', [x(doeval,lazy,[])], x(doeval,eager,[])).
'mc__1_1_time'(P,Ret) :- wtime_eval(as_p1_exec(P,Ret)).

transpiler_predicate_store(builtin, empty, [0], '@doc', '@doc', [], x(doeval,eager,[])).
'mc__1_0_empty'(_) :- fail.

transpiler_predicate_store(builtin, 'eval', [1], '@doc', '@doc', [x(noeval,eager,[])], x(doeval,eager,[])).
'mc__1_1_eval'(X,R) :- transpile_eval(X,R).

transpiler_predicate_store(builtin, 'get-metatype', [1], '@doc', '@doc', [x(noeval,eager,[])], x(doeval,eager,[])).
'mc__1_1_get-metatype'(X,Y) :- 'get-metatype'(X,Y). % use the code in the interpreter for now

transpiler_predicate_store(builtin, 'println!', [1], '@doc', '@doc', [x(doeval,eager,[])], x(doeval,eager,[])).
'mc__1_1_println!'(X,[]) :- println_impl(X).

transpiler_predicate_store(builtin, 'format-args', [2], '@doc', '@doc', [x(doeval,eager,[]),x(noeval,eager,[])], x(doeval,eager,[])).
'mc__1_2_format-args'(EFormat,EArgs,Str) :-
    string_chars(EFormat, FormatChars), !,
    user_io(with_output_to_str( Str, format_nth_args(FormatChars, 0, EArgs))).

transpiler_predicate_store(builtin, 'stringToChars', [1], '@doc', '@doc', [x(doeval,eager,[])], x(doeval,eager,[])).
'mc__1_1_stringToChars'(S,C) :- string_chars(S,C).

transpiler_predicate_store(builtin, 'repr', [1], '@doc', '@doc', [x(doeval,eager,[])], x(doeval,eager,[])).
'mc__1_1_repr'(A,S) :- with_output_to_str(S, write_src_woi(A)).

transpiler_predicate_store(builtin, 'charsToString', [1], '@doc', '@doc', [x(doeval,eager,[])], x(doeval,eager,[])).
'mc__1_1_charsToString'(C,S) :- string_chars(S,C).


transpiler_predicate_store(builtin, 'assertEqual', [2], '@doc', '@doc', [x(doeval,lazy,[]),x(noeval,lazy,[])], x(doeval,eager,[])).
'mc__1_2_assertEqual'(A,B,C) :-
   loonit_assert_source_tf_empty(
        ['assertEqual',A,B],AA,BB,
        ('mc__1_1_collapse'(A,AA),
         'mc__1_1_collapse'(B,BB)),
         equal_enough_for_test_renumbered_l(strict_equals_allow_vn,AA,BB), C).

transpiler_predicate_store(builtin, 'assertEqualToResult', [2], '@doc', '@doc', [x(doeval,lazy,[]),x(noeval,eager,[])], x(doeval,eager,[])).
'mc__1_2_assertEqualToResult'(A,B,C) :-
   loonit_assert_source_tf_empty(
        ['assertEqualToResult',A,B],AA,B,
        ('mc__1_1_collapse'(A,AA)),
         equal_enough_for_test_renumbered_l(strict_equals_allow_vn,AA,B), C).


transpiler_predicate_store(builtin, 'assertAlphaEqual', [2], '@doc', '@doc', [x(doeval,lazy,[]),x(noeval,lazy,[])], x(doeval,eager,[])).
'mc__1_2_assertAlphaEqual'(A,B,C) :-
   loonit_assert_source_tf_empty(
        ['assertAlphaEqual',A,B],AA,BB,
        ('mc__1_1_collapse'(A,AA),
         'mc__1_1_collapse'(B,BB)),
         equal_enough_for_test_renumbered_l(alpha_equ,AA,BB), C).

transpiler_predicate_store(builtin, 'quote', [1], '@doc', '@doc', [x(noeval,eager,[])], x(noeval,eager,[])).
'mc__1_1_quote'(A,['quote',A]).
compile_flow_control(HeadIs,LazyVars,RetResult,RetResultN,LazyRetQuoted,Convert, QuotedCode1a, QuotedCode1N) :-
  Convert = ['quote',Quoted],!,
  f2p(HeadIs,LazyVars,QuotedResult,QuotedResultN,LazyRetQuoted,Quoted,QuotedCode,QuotedCodeN),
  lazy_impedance_match(LazyRetQuoted,x(noeval,eager,[]),QuotedResult,QuotedCode,QuotedResultN,QuotedCodeN,QuotedResult1,QuotedCode1),
  QuotedResult1a=['quote',QuotedResult1],
  lazy_impedance_match(x(noeval,eager,[]),LazyRetQuoted,QuotedResult1a,QuotedCode1,QuotedResult1a,QuotedCode1,QuotedResult2,QuotedCode2),
  assign_or_direct_var_only(QuotedCode2,RetResult,QuotedResult2,QuotedCode1a),
  assign_or_direct_var_only(QuotedCode2,RetResultN,QuotedResult2,QuotedCode1N).

%%%%%%%%%%%%%%%%%%%%% random number generation

transpiler_predicate_store(builtin, 'random-int', [3], '@doc', '@doc', [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).

'mc__1_3_random-int'(RNGId, Min, Max, N):-
    maplist(must_be(integer), [Min, Max]),
    MaxM1 is Max -1,
    with_random_generator(RNGId, random_between(Min, MaxM1, N) ).


transpiler_predicate_store(builtin, 'random-float', [3], '@doc', '@doc', [x(doeval, eager, []), x(doeval, eager, []), x(doeval, eager, [])], x(doeval, eager, [])).
% !(let $rg (new-random-generator 1) ((random-float $rg 1 7) (random-float $rg 1 7)))
'mc__1_3_random-float'(RNGId, Min, Max, N):-
    with_random_generator(RNGId, random_float_between(Min, Max, N)).


transpiler_predicate_store(builtin, 'set-random-seed', [2], '@doc', '@doc', [x(doeval, eager, []), x(doeval, eager, [])], x(noeval, eager, [])).
/*
    !(let $rg (new-random-generator 3) (((random-int $rg 3 7)(random-int $rg 3 7)(random-int $rg 3 7))
      (let $_ (set-random-seed $rg 3) ((random-int $rg 3 7)(random-int $rg 3 7)(random-int $rg 3 7)))))

    [((5 3 4) (5 3 4))]

*/
'mc__1_2_set-random-seed'(RNGId, Seed, RetVal):-
     with_random_generator(RNGId, set_random(seed(Seed))),
     RetVal = [].


transpiler_predicate_store(builtin, 'new-random-generator', [1], '@doc', '@doc', [x(doeval, eager, [])], x(doeval, eager, [])).

% !(new-random-generator 66)
'mc__1_1_new-random-generator'(Seed, RNG) :-
    S = getrand(Old),
    G = (set_random(seed(Seed)),
         getrand(New)
        ),
    C = setrand(Old)
    , setup_call_cleanup(S, G, C)
    , gensym('&rng_', RNGId)
    , RNG = rng(RNGId, New)
    , update_rng(RNG, New).





transpiler_predicate_store(builtin, 'reset-random-generator', [1], '@doc', '@doc', [x(doeval, eager, [])], x(doeval, eager, [])).
% !(reset-random-generator &rng_1) -> &rng_1
% Not tested.
'mc__1_1_reset-random-generator'(RNGId, RNGId ):-
   %getrnd(NewState), % Resets instance of random number generator (first argument) to its default behavior (StdRng::from_os_rng())
   % arg(2, RNGId, NewState) % maybe was previous state?
   update_rng(RNGId, _). % unbound RNG defaults to systems RNG until the first time it is used after reset
%reset_random_generator( rng(Id, StateOld, _StateNew), rng(Id, StateOld, StateOld) ).


%!  random_float_between(+Min, +Max, -Random) is det.
%
%   Get a Random float in the open interval (Min, Max).
%
%   This uses random/1 to generate a random R in the open interval
%   (0.0, 1.0) then multiplies R by the distance from Min to Max and
%   shifts the value of R by Min:
%   ==
%   random(R)
%   , Random is (Max - Min) * R + Min
%   ==
%
random_float_between(Min, Max, R_):-
         maplist(must_be(number), [Min, Max]), % the range does not need to be specified as floats
         random(R),
         R_ is (Max-Min) * R + Min.


%!    with_random_generator(+RNGId, ?Goal) is det.
%
%     Execute a Goal in the context of an RNGId.
%
%     keep RNGId changes local to the term being passed about.
%
with_random_generator('&rng', Call):- !, call(Call).
with_random_generator(RNGId, Call):-
    Setup = (getrand(OLD),
             into_rng(RNGId, Current),
             if_t(nonvar(Current), setrand(Current))),
    Cleanup = (getrand(New),
               update_rng(RNGId, New),
               setrand(OLD)),
    setup_call_cleanup(Setup, Call, Cleanup).

% Get RNG
into_rng(rng(_, Current), Current):-!.
into_rng(RNGId, Current):- nb_bound(RNGId, rng(_, Current)).

% Set RNG
update_rng(RNG, Current):- RNG = rng(RNGId, _), !, nb_setarg(2, RNG, Current), nb_setval(RNGId, RNG).
update_rng(RNGId, Current):- nb_setval(RNGId, rng(RNGId, Current)).

% fake a built in one
:- on_metta_setup(update_rng('&rng', _)).

%%%%%%%%%%%%%%%%%%%%% transpiler specific (non standard MeTTa)

transpiler_predicate_store(builtin, 'prolog-trace', [0], [], '', [], x(doeval,eager,[])).
'mc__1_0_prolog-trace'([]) :- trace.

listing_order(Order, [Origin1, Fn1, Arity1, _], [Origin2, Fn2, Arity2, _]) :-
    ( Origin1 \= Origin2 -> compare(Order, Origin1, Origin2)   % Compare first element
    ; Fn1 \= Fn2 -> compare(Order, Fn1, Fn2)                   % Compare second if first is equal
    ; compare(Order, Arity1, Arity2)                           % Compare third if first two are equal
    ).

transpiler_predicate_store(builtin, 'transpiler-listing', [0], [], '', [], x(doeval,eager,[])).
'mc__1_0_transpiler-listing'(Sorted) :-
  findall([Origin,Fn,Arity,[]],transpiler_predicate_store(Origin,Fn,Arity,_,_,_,_),Unsorted1),
  findall([Origin,Fn,Arity,['variable arity']],transpiler_predicate_nary_store(Origin,Fn,Arity,_,_,_,_,_,_),Unsorted2),
  append(Unsorted1,Unsorted2,Unsorted),
  predsort(listing_order,Unsorted,Sorted).





transpiler_predicate_store(builtin, 'metta-equals', [2], '@doc', '@doc', [x(noeval,eager,[]), x(noeval,eager,[])], x(doeval,eager,[boolean])).
'mc__1_2_metta-equals'(A,B,TF):- as_tf(A=@=B,TF).

transpiler_predicate_store(builtin, 'metta-unify', [2], '@doc', '@doc', [x(noeval,eager,[]), x(noeval,eager,[])], x(doeval,eager,[boolean])).
'mc__1_2_metta-unify'(A,B,TF):- as_tf(unify_with_occurs_check(A,B),TF).

transpiler_predicate_store(builtin, 'decons-ht', [3], '@doc', '@doc', [x(noeval,eager,[]),x(noeval,eager,[]),x(noeval,eager,[])],x(doeval,eager,[boolean])).
'mc__1_3_decons-ht'(E,H,T,TF):- as_tf(E=[H|T],TF).

transpiler_predicate_nary_store(builtin, 'py-atom-call', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(doeval,eager,[]), x(doeval,eager,[])).
'mc_n_1__py-atom-call'(SymRef,Args,Ret) :- 'mc_n_1__py-atom-call!'(SymRef,Args,Ret).

transpiler_predicate_nary_store(builtin, 'py-atom-call!', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(noeval,eager,[]), x(doeval,eager,[])).
'mc_n_1__py-atom-call!'(SymRef,Args,Ret) :-
    py_call_method_and_args(SymRef,Args,Res),
    py_metta_return_value(_RetType,Ret,Res).

transpiler_predicate_nary_store(builtin, 'py-dot-call', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(doeval,eager,[]), x(doeval,eager,[])).
'mc_n_1__py-dot-call'(SymRef,Args,Ret) :- 'mc_n_1__py-dot-call!'(SymRef,Args,Ret).

transpiler_predicate_nary_store(builtin, 'py-dot-call!', 1, ['Atom'], 'Atom', 'Atom', [x(doeval,eager,[])], x(noeval,eager,[]), x(doeval,eager,[])).
'mc_n_1__py-dot-call!'(SymRef,Args,Ret) :-
    eval_only_interp([['py-dot'|SymRef]|Args],Ret).
    %make_py_dot(Arg1,Arg2,Res)
    %py_call_method_and_args(SymRef,Args,Res),
    %py_metta_return_value(_RetType,Ret,Res).

this_is_in_compiler_lib.

metta_to_metta_macro_recurse(I,O):-
  metta_to_metta_macro(I,M),I\=@=M,!,
  metta_to_metta_macro_recurse(M,O).
metta_to_metta_macro_recurse(I,I).

metta_to_metta_macro(NoList,NoList):- \+ is_list(NoList),!.
metta_to_metta_macro([EQ,HeadIsN,AsBodyFnN], ['=',HeadIsC, AsBodyFnOut]):- EQ=='=', !,
 ((
 copy_term(AsBodyFnN+HeadIsN,AsBodyFnC+HeadIsC,_),
 number_vars_wo_conficts(AsBodyFnC+HeadIsC,AsBodyFn+HeadIs),
 (AsBodyFnC+HeadIsC=AsBodyFn+HeadIs),
    metta_body_macro(HeadIs, AsBodyFn, AsBodyFnOut),!,
    \+ \+ if_t(AsBodyFn\=@=AsBodyFnOut,
    ( debug_info(metta_macro_in,t(([=,HeadIs, AsBodyFn]))),!,
      debug_info(metta_macro_out,t(([=,HeadIs, AsBodyFnOut]))))))),!.
metta_to_metta_macro(Body,BodyOut):- metta_to_metta_macro(['=',[whatever],Body],['=',[whatever],BodyOut]),!.


%metta_body_macro(_HeadIs, AsBodyFn, AsBodyFnOut):-!, AsBodyFnOut=AsBodyFn.
metta_body_macro(HeadIs, AsBodyFn, AsBodyFnOut):-
   must_det_lls((
    nop((term_variables(HeadIs+AsBodyFn,Vars),copy_term(Vars,Copy),freeze_vars(Vars,Copy))),
    metta_body_macro1(HeadIs, [], AsBodyFn, AsBodyFnE),
    metta_body_macro2(HeadIs, [], AsBodyFnE, AsBodyFnMid),
   (AsBodyFn =@= AsBodyFnMid -> AsBodyFnMid = AsBodyFnOut ;
     metta_body_macro(HeadIs, AsBodyFnMid, AsBodyFnOut)),
    nop((unfreeze_vars(Vars,Copy))))).

variables_are_safe(Inp,Goal):-
    term_variables(Inp,Vars),copy_term(Inp+Vars,InpC+Copy),freeze_vars(Vars,Copy),
    call(Goal),Inp=@=InpC,
    unfreeze_vars(Vars,Copy).

freeze_var(Var,Copy):- put_attr(Var,cant_bind,Copy).
freeze_vars(Vars,Copy):- maplist(freeze_var,Vars,Copy).
cant_bind:attr_unify_hook(Copy,NewValue):- var(Copy),Copy=@=NewValue.
unfreeze_var(Var,_):- del_attr(Var,cant_bind),!.
%unfreeze_var(Var,Copy):- get_attr(Var,cant_bind,Now),Copy==Now,del_attr(Var,cant_bind),Var=@=Copy.
unfreeze_vars(Vars,Copy):- maplist(unfreeze_var,Vars,Copy).


metta_body_macro1(_HeadIs, _,_AsBodyFn, AsBodyFnO):- nonvar(AsBodyFnO),!,trace_break(nonvar(AsBodyFnO)).
metta_body_macro1(_HeadIs, _, AsBodyFn, AsBodyFnO):- \+ compound(AsBodyFn), !, AsBodyFn=AsBodyFnO.
metta_body_macro1(_HeadIs, _, AsBodyFn, AsBodyFnO):- \+ is_list(AsBodyFn), !, AsBodyFn=AsBodyFnO.
metta_body_macro1(HeadIs, Stack, [Op|AsBodyFn], AsBodyFnOut):- fail, \+ is_funcall_op(Op),  !, maplist(metta_body_macro1(HeadIs, Stack), [Op|AsBodyFn], AsBodyFnOut),!.

metta_body_macro1(HeadIs, Stack, [Op|AsBodyFn], AsBodyFnOut):-
   once((copy_term(AsBodyFn,AsBodyFnCopy),
   maplist( metta_body_macro1(HeadIs, Stack), AsBodyFn, AsBodyFnMid),
   AsBodyFn=@=AsBodyFnCopy)),
   [Op|AsBodyFnMid]=OpAsBodyMid,
   copy_term(OpAsBodyMid,OpAsBodyMidCopy),
   metta_body_macro_pass(e,OpAsBodyMid,AsBodyFnOut),
   OpAsBodyMid=@=OpAsBodyMidCopy,!.
metta_body_macro1(_HeadIs, _, AsBodyFn, AsBodyFn).

metta_body_macro2(_HeadIs, _,_AsBodyFn, AsBodyFnO):- nonvar(AsBodyFnO),!,trace_break(nonvar(AsBodyFnO)).
metta_body_macro2(_HeadIs, _, AsBodyFn, AsBodyFnO):- \+ compound(AsBodyFn), !, AsBodyFn=AsBodyFnO.
metta_body_macro2(_HeadIs, _, AsBodyFn, AsBodyFnO):- \+ is_list(AsBodyFn), !, AsBodyFn=AsBodyFnO.
metta_body_macro2(HeadIs, Stack, [Op|AsBodyFn], AsBodyFnOut):- fail, \+ is_funcall_op(Op),  !, maplist(metta_body_macro1(HeadIs, Stack), [Op|AsBodyFn], AsBodyFnOut),!.

metta_body_macro2(HeadIs, Stack, OpAsBody, AsBodyFnOutReally):-
   once((copy_term(OpAsBody,OpAsBodyMidCopy),
   metta_body_macro_pass(f, OpAsBody , AsBodyFnOut),
   OpAsBody=@=OpAsBodyMidCopy)),
   maplist( metta_body_macro2(HeadIs, Stack), AsBodyFnOut,AsBodyFnOutReally),!.
metta_body_macro2(_HeadIs, _, AsBodyFn, AsBodyFn).

metta_body_macro_pass(e,[NonOp|More], AsBodyFn):- \+ callable(NonOp),!,[NonOp|More]= AsBodyFn.
metta_body_macro_pass(e,['if-unify',Var1,Var2|Rest], [if,['metta-unify',Var1,Var2]|Rest]).
metta_body_macro_pass(e,['if-equal',Var1,Var2|Rest], [if,['metta-equal',Var1,Var2]|Rest]).
metta_body_macro_pass(e,['if-decons-expr',Expr,Head,Tail|Rest],[if,['decons-ht',Expr,Head,Tail]|Rest]).
metta_body_macro_pass(e,['if-decons',Expr,Head,Tail|Rest],[if,['decons-ht',Expr,Head,Tail]|Rest]).
metta_body_macro_pass(e,['chain',[Ceval,Eval],Var|Rest], ['let',Var,Eval|Rest]):- Ceval == eval,!.
metta_body_macro_pass(e,['chain',Eval,Var|Rest], ['let',Var,Eval|Rest]).

metta_body_macro_pass(f,[['py-dot'|Args]|Rest], ['py-dot-call',Args|Rest]).
metta_body_macro_pass(f,[['py-atom'|Args]|Rest], ['py-atom-call',Args|Rest]).

%metta_body_macro_pass(e,[eval,Next], Next).
metta_body_macro_pass(e,AsBodyFnOut, AsBodyFnOut).

metta_body_macro_pass(f,[NonOp|More], AsBodyFn):- \+ callable(NonOp),!,[NonOp|More]= AsBodyFn.
metta_body_macro_pass(f,[eval,Eval], Eval).

metta_body_macro_pass(f,['unique',Eval],
   ['let',Var,['call-fn!','no_repeat_var',variant_by_type],
     ['let',Res,Eval,['metta-unify',Var,Res],Res]]).

metta_body_macro_pass(_,AsBodyFnOut, AsBodyFnOut).





