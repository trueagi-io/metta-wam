:- dynamic(transpiler_clause_store/9).
:- discontiguous transpiler_clause_store/9.

from_prolog_args(_,X,X).
:-dynamic(pred_uses_fallback/2).
:-dynamic(pred_uses_impl/2).

pred_uses_impl(F,A):- transpile_impl_prefix(F,A,Fn),current_predicate(Fn/A).

use_interpreter:- fail.
mc_fallback_unimpl(Fn,Arity,Args,Res):- \+ use_interpreter, !,
  (pred_uses_fallback(Fn,Arity); (length(Args,Len), \+ pred_uses_impl(Fn,Len))),!,
    get_operator_typedef_props(_,Fn,Arity,Types,_RetType0),
    current_self(Self),
    maybe_eval(Self,Types,Args,NewArgs),
    [Fn|NewArgs]=Res.

%mc_fallback_unimpl(Fn,_Arity,Args,Res):-  u_assign([Fn|Args], Res).

maybe_eval(_Self,_Types,[],[]):-!.
maybe_eval(Self,[T|Types],[A|Args],[N|NewArgs]):-
    into_typed_arg(30,Self,T,A,N),
    maybe_eval(Self,Types,Args,NewArgs).


%'mc_2__:'(Obj, Type, [':',Obj, Type]):- current_self(Self), sync_type(10, Self, Obj, Type). %freeze(Obj, get_type(Obj,Type)),!.
%sync_type(D, Self, Obj, Type):- nonvar(Obj), nonvar(Type), !, arg_conform(D, Self, Obj, Type).
%sync_type(D, Self, Obj, Type):- nonvar(Obj), var(Type), !, get_type(D, Self, Obj, Type).
%sync_type(D, Self, Obj, Type):- nonvar(Type), var(Obj), !, set_type(D, Self, Obj, Type). %, freeze(Obj, arg_conform(D, Self, Obj, Type)).
%sync_type(D, Self, Obj, Type):- freeze(Type,sync_type(D, Self, Obj, Type)), freeze(Obj, sync_type(D, Self, Obj, Type)),!.


transpiler_clause_store('get-type', 2, 0, ['Atom'],'Atom', [x(noeval,eager)], x(doeval,eager), [], []).
%'mc_1__get-type'(Obj,Type):-  attvar(Obj),current_self(Self),!,trace,get_attrs(Obj,Atts),get_type(10, Self, Obj,Type).
'mc_1__get-type'(Obj,Type):- current_self(Self), !, get_type(10, Self, Obj,Type).

%%%%%%%%%%%%%%%%%%%%% arithmetic

transpiler_clause_store('+', 3, 0, ['Number', 'Number'],'Number', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__+'(A,B,R) :- number(A),number(B),!,plus(A,B,R).
'mc_2__+'(A,B,['+',A,B]).

transpiler_clause_store('-', 3, 0, ['Number', 'Number'],'Number', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__-'(A,B,R) :- number(A),number(B),!,plus(B,R,A).
'mc_2__-'(A,B,['-',A,B]).

transpiler_clause_store('*', 3, 0, ['Number', 'Number'],'Number', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__*'(A,B,R) :- number(A),number(B),!,R is A*B.
'mc_2__*'(A,B,['*',A,B]).

%%%%%%%%%%%%%%%%%%%%% logic

%transpiler_clause_store('and', 3, 0, ['Bool', 'LazyBool'],'Bool', [x(doeval,eager), x(doeval,lazy)], x(doeval,eager), [], []).
%mc_2__and(A,is_p1(_,CodeB,B),B) :- atomic(A), A\=='False', A\==0, !, call(CodeB).
%mc_2__and(_,_,'False').

%transpiler_clause_store('or', 3, 0, ['Bool', 'LazyBool'],'Bool', [x(doeval,eager), x(doeval,lazy)], x(doeval,eager), [], []).
%mc_2__or(A,is_p1(_,CodeB,B),B):- (\+ atomic(A); A='False'; A=0), !, call(CodeB).
%mc_2__or(_,_,'True').

transpiler_clause_store('and', 3, 0, ['Bool', 'Bool'],'Bool', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
mc_2__and(A,B,B):- atomic(A), A\=='False', A\==0, !.
mc_2__and(_,_,'False').

transpiler_clause_store('or', 3, 0, ['Bool', 'Bool'],'Bool', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
mc_2__or(A,B,B):- (\+ atomic(A); A='False'; A=0), !.
mc_2__or(_,_,'True').

transpiler_clause_store('not', 2, 0, ['Bool'],'Bool', [x(doeval,eager)], x(doeval,eager), [], []).
mc_1__not(A,'False') :- atomic(A), A\=='False', A\==0, !.
mc_1__not(_,'True').

%%%%%%%%%%%%%%%%%%%%% comparison

% not sure about the signature for this one
transpiler_clause_store('==', 3, 0, ['Any', 'Any'],'Bool', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__=='(A,A,1) :- !.
'mc_2__=='(_,_,0).

transpiler_clause_store('<', 3, 0, ['Number', 'Number'],'Bool', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__<'(A,B,R) :- number(A),number(B),!,(A<B -> R='True' ; R='False').
'mc_2__<'(A,B,['<',A,B]).

transpiler_clause_store('>', 3, 0, ['Number', 'Number'],'Bool', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__>'(A,B,R) :- number(A),number(B),!,(A>B -> R='True' ; R='False').
'mc_2__>'(A,B,['>',A,B]).

transpiler_clause_store('>=', 3, 0, ['Number', 'Number'],'Bool', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__>='(A,B,R) :- number(A),number(B),!,(A>=B -> R='True' ; R='False').
'mc_2__>='(A,B,['>=',A,B]).

transpiler_clause_store('<=', 3, 0, ['Number', 'Number'],'Bool', [x(doeval,eager), x(doeval,eager)], x(doeval,eager), [], []).
'mc_2__<='(A,B,R) :- number(A),number(B),!,(A=<B -> R='True' ; R='False'). % note that Prolog has a different syntax '=<'
'mc_2__<='(A,B,['<=',A,B]).

%%%%%%%%%%%%%%%%%%%%% lists

transpiler_clause_store('car-atom', 2, 0, ['Expression'],'Atom', [x(noeval,eager)], x(doeval,eager), [], []).
'mc_1__car-atom'(Cons,H):- Cons = [H|_] -> true ; throw(metta_type_error).

transpiler_clause_store('cdr-atom', 2, 0, ['Expression'],'Expression', [x(noeval,eager)], x(doeval,eager), [], []).
'mc_1__cdr-atom'([_|T],T).

transpiler_clause_store('cons-atom', 2, 3, ['Atom', 'Expression'],'Expression', [x(noeval,eager), x(noeval,eager)], x(doeval,eager), [], []).
'mc_2__cons-atom'(A,B,[A|B]).

transpiler_clause_store('decons-atom', 2, 0, ['Expression'],'Expression', [x(noeval,eager)], x(doeval,eager), [], []).
'mc_1__decons-atom'([A|B],[A,B]).

%%%%%%%%%%%%%%%%%%%%% set

lazy_member(R1,Code2,R2) :- call(Code2),R1=R2.

transpiler_clause_store(subtraction, 3, 0, ['Atom','Atom'], 'Atom', [x(doeval,lazy),x(doeval,lazy)], x(doeval,eager), [], []).
'mc_2__subtraction'(is_p1(_Type1,_Expr1,Code1,R1),is_p1(_Type2,_Expr2,Code2,R2),R1) :- !,
    call(Code1),
    \+ lazy_member(R1,Code2,R2).
'mc_2__subtraction'(is_p1(_,Code1,R1),is_p1(_,Code2,R2),R1) :-
    call(Code1),
    \+ lazy_member(R1,Code2,R2).

transpiler_clause_store(union, 3, 0, ['Atom','Atom'], 'Atom', [x(doeval,lazy),x(doeval,lazy)], x(doeval,eager), [], []).
'mc_2__union'(U1,is_p1(_Type1,_Expr1,Code2,R2),R) :- !, 'mc_2__subtraction'(U1,is_p1(_Type2,_Expr2,Code2,R2),R) ; call(Code2),R=R2.
'mc_2__union'(U1,is_p1(Expr,Code2,R2),R) :- 'mc_2__subtraction'(U1,is_p1(Expr,Code2,R2),R) ; call(Code2),R=R2.

%%%%%%%%%%%%%%%%%%%%% superpose, collapse

transpiler_clause_store(superpose, 2, 0, ['Expression'], 'Atom', [x(doeval,eager)], x(doeval,eager), [], []).
'mc_1__superpose'(S,R) :- member(R,S).

% put a fake transpiler_clause_store here, just to force the argument to be lazy
transpiler_clause_store(collapse, 2, 0, ['Atom'], 'Expression', [x(doeval,lazy)], x(doeval,eager), [], []).
'mc_1__collapse'(is_p1(_Type,_Expr,Code,Ret),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc_1__collapse'(is_p1(_Type,_Expr,true,X),[X]) :- !.
'mc_1__collapse'(is_p1(_,Code,Ret),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc_1__collapse'(is_p1(_,true,X),[X]).
'mc_1__collapse'(is_p1(Code,Ret),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc_1__collapse'(is_p1(true,X),[X]).


%%%%%%%%%%%%%%%%%%%%% spaces

transpiler_clause_store('add-atom', 3, 0, ['Atom', 'Atom'], '%Undefined%', [x(doeval,eager), x(noeval,eager)], x(doeval,eager), [], []).
'mc_2__add-atom'(Space,PredDecl,[]) :- 'add-atom'(Space,PredDecl).

transpiler_clause_store('remove-atom', 3, 0, ['Atom', 'Atom'], '%Undefined%', [x(doeval,eager), x(noeval,eager)], x(doeval,eager), [], []).
'mc_2__remove-atom'(Space,PredDecl,[]) :- 'remove-atom'(Space,PredDecl).

transpiler_clause_store('get-atoms', 2, 0, ['Atom'], 'Atom', [x(noeval,eager)], x(noeval,eager), [], []).
'mc_1__get-atoms'(Space,Atoms) :- metta_atom(Space, Atoms).

% put a fake transpiler_clause_store here, just to force the template to be lazy
transpiler_clause_store(match, 4, 0, ['Atom', 'Atom', 'Atom'], ' %Undefined%', [x(doeval,eager), x(doeval,eager), x(doeval,lazy)], x(doeval,eager), [], []).
'mc_3__match'(Space,Pattern,is_p1(_Type,_Expr,TemplateCode,TemplateRet),TemplateRet) :- match_pattern(Space, Pattern), call(TemplateCode).
'mc_3__match'(Space,Pattern,is_p1(_,TemplateCode,TemplateRet),TemplateRet) :- metta_atom(Space, Atom),Atom=Pattern,call(TemplateCode).


% This allows match to supply hits to the correct metta_atom/2 (Rather than sending a variable
match_pattern(Space, Pattern):- functor(Pattern,F,A), functor(Atom,F,A), metta_atom(Space, Atom), Atom=Pattern.

% TODO FIXME: ssort out the difference between unify and match
transpiler_clause_store(unify, 4, 0, ['Atom', 'Atom', 'Atom'], ' %Undefined%', [x(doeval,eager), x(doeval,eager), x(doeval,lazy)], x(doeval,eager), [], []).
'mc_3__unify'(Space,Pattern,is_p1(_TypeT,_ExprT,SuccessCode,RetVal),RetVal) :- !, unify_pattern(Space,Pattern), call(SuccessCode).
'mc_3__unify'(Space,Pattern,is_p1(_,TemplateCode,TemplateRet),TemplateRet) :- metta_atom(Space, Atom),Atom=Pattern,call(TemplateCode).

transpiler_clause_store(unify, 5, 0, ['Atom', 'Atom', 'Atom', 'Atom'], ' %Undefined%', [x(doeval,eager), x(doeval,eager), x(doeval,lazy), x(doeval,lazy)], x(doeval,eager), [], []).
'mc_4__unify'(Space,Pattern,is_p1(_TypeT,_ExprT,SuccessCode,RetVal),is_p1(_TypeF,_ExprF,FailureCode,RetVal),RetVal) :-
    (unify_pattern(Space,Pattern)->call(SuccessCode);call(FailureCode)).

% unify calls pattern matching if arg1 is a space
unify_pattern(Space,Pattern):- is_metta_space(Space),!, match_pattern(Space, Pattern).
% otherwise calls prolog unification (with occurs check later)
unify_pattern(Atom, Pattern):- metta_unify(Atom, Pattern).

metta_unify(Atom, Pattern):- Atom=Pattern.

%%%%%%%%%%%%%%%%%%%%% misc

% put a fake transpiler_clause_store here, just to force the argument to be lazy
transpiler_clause_store(time, 2, 0, ['Atom'], 'Atom', [x(doeval,lazy)], x(doeval,eager), [], []).
'mc_1__time'(is_p1(_Type,_Expr,Code,Ret),Ret) :- wtime_eval(Code).
'mc_1__time'(is_p1(_,Code,Ret),Ret) :- wtime_eval(Code).

transpiler_clause_store(empty, 1, 0, [], '%Undefined', [], x(doeval,eager), [], []).
'mc_0__empty'(_) :- fail.

transpiler_clause_store('eval', 2, 0, ['Atom'], 'Atom', [x(noeval,eager)], x(doeval,eager), [], []).
'mc_1__eval'(X,R) :- transpile_eval(X,R).

transpiler_clause_store('get-metatype', 2, 0, ['Atom'], 'Atom', [x(noeval,eager)], x(doeval,eager), [], []).
'mc_1__get-metatype'(X,Y) :- 'get-metatype'(X,Y). % use the code in the interpreter for now

transpiler_clause_store('println!', 2, 0, ['%Undefined'], '%Undefined', [x(noeval,eager)], x(doeval,eager), [], []).
'mc_1__println!'(X,[]) :- println_impl(X).

transpiler_clause_store('stringToChars', 2, 0, ['String'], 'Expression', [x(doeval,eager)], x(doeval,eager), [], []).
'mc_1__stringToChars'(S,C) :- string_chars(S,C).

transpiler_clause_store('charsToString', 2, 0, ['Expression'], 'String', [x(doeval,eager)], x(doeval,eager), [], []).
'mc_1__charsToString'(C,S) :- string_chars(S,C).

transpiler_clause_store('assertEqualToResult', 3, 0, ['Atom', 'Atom'], 'Atom', [x(doeval,eager),x(noeval,eager)], x(doeval,eager), [], []).
'mc_2__assertEqualToResult'(A, B, C) :- u_assign([assertEqualToResult, A, B], C).

% this is a hack to make 'quote' behave as expected (noeval rather than eval).
% the reason for this is that stubs are currently created with x(doeval,eager) by default.
% once the check and recompile loop is done (using transpiler_predicate_store/4, stubs will be correctly created with x(neval,eager), and this can go away.
transpiler_clause_store('quote', 2, 0, ['Expression'], 'Expression', [x(noeval,eager)], x(noeval,eager), [], []).
'mc_1__quote'(A,['quote',A]).
