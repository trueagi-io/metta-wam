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
sync_type(D, Self, Obj, Type):- nonvar(Obj), var(Type), !, get_type(D, Self, Obj, Type).
sync_type(D, Self, Obj, Type):- nonvar(Type), var(Obj), !, set_type(D, Self, Obj, Type), freeze(Obj, arg_conform(D, Self, Obj, Type)).
sync_type(D, Self, Obj, Type):- nonvar(Type), nonvar(Obj), !, arg_conform(D, Self, Obj, Type).
sync_type(D, Self, Obj, Type):- freeze(Type,sync_type(D, Self, Obj, Type)), freeze(Obj, sync_type(D, Self, Obj, Type)),!.


'mc__get-type'(Obj,Type):-  attvar(Obj),current_self(Self),!,trace,get_attrs(Obj,Atts),get_type(10, Self, Obj,Type).
'mc__get-type'(Obj,Type):- current_self(Self), !, get_type(10, Self, Obj,Type).

%%%%%%%%%%%%%%%%%%%%% arithmetic

% get_type_sig('+',['Number','Number'],'Number').
'mc__+'(A,B,R) :- number(A),number(B),!,plus(A,B,R).
'mc__+'(A,B,['+',A,B]).

'mc__-'(A,B,R) :- number(A),number(B),!,plus(B,R,A).
'mc__-'(A,B,['-',A,B]).

'mc__*'(A,B,R) :- number(A),number(B),!,R is A*B.
'mc__*'(A,B,['*',A,B]).

%%%%%%%%%%%%%%%%%%%%% logic

mc__and(A,B,B):- atomic(A), A\=='False', A\==0.
mc__and(_,_,'False').

mc__or(A,B,B):- (\+ atomic(A); A='False'; A=0), !.
mc__or(_,_,'True').

%%%%%%%%%%%%%%%%%%%%% comparison

'mc__=='(A,B,TF) :- (var(A);var(B)),!,A=B, TF='True'.
'mc__=='(A,B,TF) :- as_tf(A=B,TF).
%'mc__=='(_,_,0).

'mc__<'(A,B,R) :- number(A),number(B),!,(A<B -> R=1 ; R=0).
'mc__<'(A,B,['<',A,B]).

%%%%%%%%%%%%%%%%%%%%% lists

'mc__car-atom'([H|_],H).

'mc__cdr-atom'([_|T],T).

'mc__cons-atom'(A,B,[A|B]).

%%%%%%%%%%%%%%%%%%%%%superpose,collapse

'mi__superpose'([H|_],H).
'mi__superpose'([_|T],R):-'mi__superpose'(T,R).

%%%%%%%%%%%%%%%%%%%%% misc

'mc__empty'(_) :- fail.

'mc__stringToChars'(S,C) :- string_chars(S,C).

'mc__charsToString'(C,S) :- string_chars(S,C).

mc__assertEqualToResult(A, B, C) :- u_assign([assertEqualToResult, A, B], C).
