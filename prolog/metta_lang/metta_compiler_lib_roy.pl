:- dynamic(transpiler_predicate_store/4).
:- discontiguous transpiler_predicate_store/4.

from_prolog_args(_,X,X).
:-dynamic(pred_uses_fallback/2).
:-dynamic(pred_uses_impl/2).

pred_uses_impl(F,A):- transpile_impl_prefix(F,Fn),current_predicate(Fn/A).

mc_fallback_unimpl(Fn,Arity,Args,Res):-
  (pred_uses_fallback(Fn,Arity);(length(Args,Len),\+pred_uses_impl(Fn,Len))),!,
    get_operator_typedef_props(_,Fn,Arity,Types,_RetType0),
    current_self(Self),
    maybe_eval(Self,Types,Args,NewArgs),
    [Fn|NewArgs]=Res.

maybe_eval(_Self,_Types,[],[]):-!.
maybe_eval(Self,[T|Types],[A|Args],[N|NewArgs]):-
    into_typed_arg(30,Self,T,A,N),
    maybe_eval(Self,Types,Args,NewArgs).


%'mc_2__:'(Obj, Type, [':',Obj, Type]):- current_self(Self), sync_type(10, Self, Obj, Type). %freeze(Obj, get_type(Obj,Type)),!.
%sync_type(D, Self, Obj, Type):- nonvar(Obj), nonvar(Type), !, arg_conform(D, Self, Obj, Type).
%sync_type(D, Self, Obj, Type):- nonvar(Obj), var(Type), !, get_type(D, Self, Obj, Type).
%sync_type(D, Self, Obj, Type):- nonvar(Type), var(Obj), !, set_type(D, Self, Obj, Type). %, freeze(Obj, arg_conform(D, Self, Obj, Type)).
%sync_type(D, Self, Obj, Type):- freeze(Type,sync_type(D, Self, Obj, Type)), freeze(Obj, sync_type(D, Self, Obj, Type)),!.


transpiler_predicate_store('get-type', 2, [x(noeval,eager)], x(doeval,eager)).
%'mc_1__get-type'(Obj,Type):-  attvar(Obj),current_self(Self),!,trace,get_attrs(Obj,Atts),get_type(10, Self, Obj,Type).
'mc_1__get-type'(Obj,Type):- current_self(Self), !, get_type(10, Self, Obj,Type).

from_prolog_args(_,X,X).
:-dynamic(pred_uses_fallback/2).
:-dynamic(pred_uses_impl/2).

pred_uses_impl(F,A):- transpile_impl_prefix(F,Fn),current_predicate(Fn/A).

mc_fallback_unimpl(Fn,Arity,Args,Res):-
  (pred_uses_fallback(Fn,Arity);(length(Args,Len),\+pred_uses_impl(Fn,Len))),!,
    get_operator_typedef_props(_,Fn,Arity,Types,_RetType0),
    current_self(Self),
    maybe_eval(Self,Types,Args,NewArgs),
    [Fn|NewArgs]=Res.

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

transpiler_predicate_store('+', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__+'(A,B,R) :- number(A),number(B),!,plus(A,B,R).
'mc_2__+'(A,B,['+',A,B]).

transpiler_predicate_store('-', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__-'(A,B,R) :- number(A),number(B),!,plus(B,R,A).
'mc_2__-'(A,B,['-',A,B]).

transpiler_predicate_store('*', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__*'(A,B,R) :- number(A),number(B),!,R is A*B.
'mc_2__*'(A,B,['*',A,B]).

%%%%%%%%%%%%%%%%%%%%% logic

transpiler_predicate_store('and', 3, [x(doeval,eager), x(doeval,lazy)], x(doeval,eager)).
mc_2__and(A,is_p1(_,CodeB,B),B) :- atomic(A), A\=='False', A\==0, !, call(CodeB).
mc_2__and(_,_,'False').

transpiler_predicate_store('or', 3, [x(doeval,eager), x(doeval,lazy)], x(doeval,eager)).
mc_2__or(A,is_p1(_,CodeB,B),B):- (\+ atomic(A); A='False'; A=0), !, call(CodeB).
mc_2__or(_,_,'True').

%transpiler_predicate_store('and', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
%mc_2__and(A,B,B) :- atomic(A), A\=='False', A\==0, !.
%mc_2__and(_,_,'False').

%transpiler_predicate_store('or', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
%mc_2__or(A,B,B):- (\+ atomic(A); A='False'; A=0), !.
%mc_2__or(_,_,'True').

transpiler_predicate_store('not', 2, [x(doeval,eager)], x(doeval,eager)).
mc_1__not(A,'False') :- atomic(A), A\=='False', A\==0, !.
mc_1__not(_,'True').

%%%%%%%%%%%%%%%%%%%%% comparison

% not sure about the signature for this one
transpiler_predicate_store('==', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__=='(A,A,1) :- !.
'mc_2__=='(_,_,0).

transpiler_predicate_store('<', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__<'(A,B,R) :- number(A),number(B),!,(A<B -> R='True' ; R='False').
'mc_2__<'(A,B,['<',A,B]).

transpiler_predicate_store('>', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__>'(A,B,R) :- number(A),number(B),!,(A>B -> R='True' ; R='False').
'mc_2__>'(A,B,['>',A,B]).

transpiler_predicate_store('>=', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__>='(A,B,R) :- number(A),number(B),!,(A>=B -> R='True' ; R='False').
'mc_2__>='(A,B,['>=',A,B]).

transpiler_predicate_store('<=', 3, [x(doeval,eager), x(doeval,eager)], x(doeval,eager)).
'mc_2__<='(A,B,R) :- number(A),number(B),!,(A=<B -> R='True' ; R='False'). % note that Prolog has a different syntax '=<'
'mc_2__<='(A,B,['<=',A,B]).

%%%%%%%%%%%%%%%%%%%%% lists

transpiler_predicate_store('car-atom', 2, [x(noeval,eager)], x(noeval,eager)).
'mc_1__car-atom'([H|_],H).

transpiler_predicate_store('cdr-atom', 2, [x(noeval,eager)], x(noeval,eager)).
'mc_1__cdr-atom'([_|T],T).

transpiler_predicate_store('cons-atom', 3, [x(noeval,eager), x(noeval,eager)], x(noeval,eager)).
'mc_2__cons-atom'(A,B,[A|B]).

transpiler_predicate_store('decons-atom', 2,  [x(noeval,eager)], x(noeval,eager)).
'mc_1__decons-atom'([A|B],[A,B]).

%%%%%%%%%%%%%%%%%%%%% set

lazy_member(R1,Code2,R2) :- call(Code2),R1=R2.

transpiler_predicate_store(subtraction, 3, [x(doeval,lazy),x(doeval,lazy)], x(doeval,eager)).
'mc_2__subtraction'(is_p1(_,Code1,R1),is_p1(_,Code2,R2),R1) :-
    call(Code1),
    \+ lazy_member(R1,Code2,R2).

transpiler_predicate_store(union, 3, [x(doeval,lazy),x(doeval,lazy)], x(doeval,eager)).
'mc_2__union'(U1,is_p1(Expr,Code2,R2),R) :- 'mc_2__subtraction'(U1,is_p1(Expr,Code2,R2),R) ; call(Code2),R=R2.

%%%%%%%%%%%%%%%%%%%%% superpose, collapse

transpiler_predicate_store(superpose, 2, [x(doeval,eager)], x(doeval,eager)).
'mc_1__superpose'(S,R) :- member(R,S).

transpiler_predicate_store(collapse, 2, [x(doeval,lazy)], x(doeval,eager)).
'mc_1__collapse'(is_p1(_,Code,Ret),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc_1__collapse'(is_p1(_,true,X),[X]).

%%%%%%%%%%%%%%%%%%%%% spaces

transpiler_predicate_store('add-atom', 3, [x(doeval,eager), x(noeval,eager)], x(doeval,eager)).
'mc_2__add-atom'(Space,PredDecl,[]) :- 'add-atom'(Space,PredDecl).

transpiler_predicate_store('remove-atom', 3, [x(doeval,eager), x(noeval,eager)], x(doeval,eager)).
'mc_2__remove-atom'(Space,PredDecl,[]) :- 'remove-atom'(Space,PredDecl).

transpiler_predicate_store('get-atoms', 2, [x(noeval,eager)], x(noeval,eager)).
'mc_1__get-atoms'(Space,Atoms) :- metta_atom(Space, Atoms).

transpiler_predicate_store(match, 4, [x(doeval,eager), x(doeval,eager), x(doeval,lazy)], x(doeval,eager)).
'mc_3__match'(Space,Pattern,is_p1(_,TemplateCode,TemplateRet),TemplateRet) :- metta_atom(Space, Atom),Atom=Pattern,call(TemplateCode).

% TODO FIXME: sort out the difference between unify and match
transpiler_predicate_store(unify, 4, [x(doeval,eager), x(doeval,eager), x(doeval,lazy)], x(doeval,eager)).
'mc_3__unify'(Space,Pattern,is_p1(_,TemplateCode,TemplateRet),TemplateRet) :- metta_atom(Space, Atom),Atom=Pattern,call(TemplateCode).

%%%%%%%%%%%%%%%%%%%%% misc

transpiler_predicate_store(time, 2, [x(doeval,lazy)], x(doeval,eager)).
'mc_1__time'(is_p1(_,Code,Ret),Ret) :- wtime_eval(Code).

transpiler_predicate_store(empty, 1, [], x(doeval,eager)).
'mc_0__empty'(_) :- fail.

transpiler_predicate_store('eval', 2, [x(noeval,eager)], x(doeval,eager)).
'mc_1__eval'(X,R) :- transpile_eval(X,R).

transpiler_predicate_store('get-metatype', 2, [x(noeval,eager)], x(doeval,eager)).
'mc_1__get-metatype'(X,Y) :- 'get-metatype'(X,Y). % use the code in the interpreter for now

transpiler_predicate_store('println!', 2, [x(noeval,eager)], x(doeval,eager)).
'mc_1__println!'(X,[]) :- println_impl(X).

transpiler_predicate_store('stringToChars', 2, [x(doeval,eager)], x(doeval,eager)).
'mc_1__stringToChars'(S,C) :- string_chars(S,C).

transpiler_predicate_store('charsToString', 2, [x(doeval,eager)], x(doeval,eager)).
'mc_1__charsToString'(C,S) :- string_chars(S,C).

transpiler_predicate_store('assertEqualToResult', 3, [x(doeval,eager),x(noeval,eager)], x(doeval,eager)).
'mc_2__assertEqualToResult'(A, B, C) :- u_assign([assertEqualToResult, A, B], C).

% this is a hack to make 'quote' behave as expected (noeval rather than eval).
% the reason for this is that stubs are currently created with x(doeval,eager) by default.
% once the check and recompile loop is done (using transpiler_predicate_store/4, stubs will be correctly created with x(neval,eager), and this can go away.
transpiler_predicate_store('quote', 2, [x(noeval,eager)], x(noeval,eager)).
'mc_1__quote'(A,['quote',A]).
