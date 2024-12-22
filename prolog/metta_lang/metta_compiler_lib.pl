:- dynamic(transpiler_clause_store/9).
:- discontiguous transpiler_clause_store/9.

:- discontiguous get_type_sig/3.


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


'mc__:'(Obj, Type, [':',Obj, Type]):- current_self(Self), sync_type(10, Self, Obj, Type). %freeze(Obj, get_type(Obj,Type)),!.
sync_type(D, Self, Obj, Type):- nonvar(Obj), nonvar(Type), !, arg_conform(D, Self, Obj, Type).
sync_type(D, Self, Obj, Type):- nonvar(Obj), var(Type), !, get_type(D, Self, Obj, Type).
sync_type(D, Self, Obj, Type):- nonvar(Type), var(Obj), !, set_type(D, Self, Obj, Type). %, freeze(Obj, arg_conform(D, Self, Obj, Type)).
sync_type(D, Self, Obj, Type):- freeze(Type,sync_type(D, Self, Obj, Type)), freeze(Obj, sync_type(D, Self, Obj, Type)),!.


%'mc__get-type'(Obj,Type):-  attvar(Obj),current_self(Self),!,trace,get_attrs(Obj,Atts),get_type(10, Self, Obj,Type).
'mc__get-type'(Obj,Type):- current_self(Self), !, get_type(10, Self, Obj,Type).

%%%%%%%%%%%%%%%%%%%%% arithmetic

'mc_2__+'(A,B,R) :- number(A),number(B),!,plus(A,B,R).
'mc_2__+'(A,B,['+',A,B]).

'mc_2__-'(A,B,R) :- number(A),number(B),!,plus(B,R,A).
'mc_2__-'(A,B,['-',A,B]).

'mc_2__*'(A,B,R) :- number(A),number(B),!,R is A*B.
'mc_2__*'(A,B,['*',A,B]).

%%%%%%%%%%%%%%%%%%%%% logic

mc_2__and(A,B,B):- atomic(A), A\=='False', A\==0, !.
mc_2__and(_,_,'False').

mc_2__or(A,B,B):- (\+ atomic(A); A='False'; A=0), !.
mc_2__or(_,_,'True').

mc_1__not(A,'False') :- atomic(A), A\=='False', A\==0, !.
mc_1__not(_,'True').

%%%%%%%%%%%%%%%%%%%%% comparison

'mc_2__=='(A,A,1) :- !.
'mc_2__=='(_,_,0).

'mc_2__<'(A,B,R) :- number(A),number(B),!,(A<B -> R='True' ; R='False').
'mc_2__<'(A,B,['<',A,B]).

'mc_2__>'(A,B,R) :- number(A),number(B),!,(A>B -> R='True' ; R='False').
'mc_2__>'(A,B,['>',A,B]).

'mc_2__>='(A,B,R) :- number(A),number(B),!,(A>=B -> R='True' ; R='False').
'mc_2__>='(A,B,['>=',A,B]).

'mc_2__<='(A,B,R) :- number(A),number(B),!,(A=<B -> R='True' ; R='False'). % note that Prolog has a different syntax '=<'
'mc_2__<='(A,B,['<=',A,B]).

%%%%%%%%%%%%%%%%%%%%% lists

'mc_1__car-atom'([H|_],H).

'mc_1__cdr-atom'([_|T],T).

'mc_2__cons-atom'(A,B,[A|B]).

'mc_1__decons-atom'([A|B],[A,B]).

%%%%%%%%%%%%%%%%%%%%% set

lazy_member(R1,Code2,R2) :- call(Code2),R1=R2.

transpiler_clause_store(subtraction, 3, 0, ['Atom','Atom'], 'Atom', [x(doeval,lazy),x(doeval,lazy)], x(doeval,eager), [], []).
'mc_2__subtraction'(is_p1(Code1,R1),is_p1(Code2,R2),R1) :-
    call(Code1),
    \+ lazy_member(R1,Code2,R2).

transpiler_clause_store(union, 3, 0, ['Atom','Atom'], 'Atom', [x(doeval,lazy),x(doeval,lazy)], x(doeval,eager), [], []).
'mc_2__union'(U1,is_p1(Code2,R2),R) :- 'mc_2__subtraction'(U1,is_p1(Code2,R2),R) ; call(Code2),R=R2.

%%%%%%%%%%%%%%%%%%%%% superpose, collapse

'mc_1__superpose'(S,R) :- member(R,S).

% put a fake transpiler_clause_store here, just to force the argument to be lazy
transpiler_clause_store(collapse, 2, 0, ['Atom'], 'Expression', [x(doeval,lazy)], x(doeval,eager), [], []).
'mc_1__collapse'(is_p1(Code,Ret),R) :- fullvar(Ret),!,findall(Ret,Code,R).
'mc_1__collapse'(is_p1(true,X),[X]).

%%%%%%%%%%%%%%%%%%%%% spaces

'mc_2__add-atom'(Space,PredDecl,[]) :- 'add-atom'(Space,PredDecl).

'mc_2__remove-atom'(Space,PredDecl,[]) :- 'remove-atom'(Space,PredDecl).

'mc_1__get-atoms'(Space,Atoms) :- metta_atom(Space, Atoms).

% put a fake transpiler_clause_store here, just to force the template to be lazy
transpiler_clause_store(match, 4, 0, ['Atom', 'Atom', 'Atom'], ' %Undefined%', [x(doeval,eager), x(doeval,eager), x(doeval,lazy)], x(doeval,eager), [], []).
'mc_3__match'(Space,Pattern,is_p1(TemplateCode,TemplateRet),TemplateRet) :- metta_atom(Space, Atom),Atom=Pattern,call(TemplateCode).

% TODO FIXME: sort out the difference between unify and match
transpiler_clause_store(unify, 4, 0, ['Atom', 'Atom', 'Atom'], ' %Undefined%', [x(doeval,eager), x(doeval,eager), x(doeval,lazy)], x(doeval,eager), [], []).
'mc_3__unify'(Space,Pattern,is_p1(TemplateCode,TemplateRet),TemplateRet) :- metta_atom(Space, Atom),Atom=Pattern,call(TemplateCode).

%%%%%%%%%%%%%%%%%%%%% misc

% put a fake transpiler_clause_store here, just to force the argument to be lazy
transpiler_clause_store(time, 2, 0, ['Atom'], 'Atom', [x(doeval,lazy)], x(doeval,eager), [], []).
'mc_1__time'(is_p1(Code,Ret),Ret) :- wtime_eval(Code).

'mc_0__empty'(_) :- fail.

'mc_1__eval'(X,R) :- transpile_eval(X,R).

'mc_1__get-metatype'(X,Y) :- 'get-metatype'(X,Y). % use the code in the interpreter for now

'mc_1__println!'(X,[]) :- println_impl(X).

'mc_1__stringToChars'(S,C) :- string_chars(S,C).

'mc_1__charsToString'(C,S) :- string_chars(S,C).

mc_2__assertEqualToResult(A, B, C) :- u_assign([assertEqualToResult, A, B], C).
