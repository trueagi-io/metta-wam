:- discontiguous get_type_sig/3.


from_prolog_args(_,X,X).
:- dynamic(pred_uses_fallback/2).
:- dynamic(pred_uses_impl/2).
mc_fallback_unimpl(Fn,Arity,Args,Res):-
  ( pred_uses_fallback(Fn,Arity); (length(Args,Len), \+ pred_uses_impl(Fn,Len))), !,
   get_operator_typedef_props(_,Fn,Arity,Types,_RetType0),
   current_self(Self),
   maybe_eval(Self,Types,Args,NewArgs),
   [Fn|NewArgs]=Res.

maybe_eval(_Self,_Types,[],[]):- !.
maybe_eval(Self,[T|Types],[A|Args],[N|NewArgs]):-
   into_typed_arg(30, Self, T, A, N),
   maybe_eval(Self,Types,Args,NewArgs).

%%%%%%%%%%%%%%%%%%%%% arithmetic

% get_type_sig('+',['Number','Number'],'Number').
'mi__+'(A,B,R) :- number(A),number(B),!,plus(A,B,R).
%'mi__+'(A,B,['+',A,B]).
'mc__-'(A, B, C) :-
    (   'mi__-'(A, B, C)
    *-> true
    ;   mc_fallback_unimpl(-, [A, B], C)
    ).

'mi__-'(A,B,R) :- number(A),number(B),!,plus(B,R,A).
%'mi__-'(A,B,['-',A,B]).

'mi__*'(A,B,R) :- number(A),number(B),!,R is A*B.
%'mi__*'(A,B,['*',A,B]).

%%%%%%%%%%%%%%%%%%%%% logic

mi__and(A,B,B):- atomic(A), A\=='False', A\==0.
%mi__and(_,_,'False').

mi__or(A,B,B):- (\+ atomic(A); A='False'; A=0), !.
%mi__or(_,_,'True').

%%%%%%%%%%%%%%%%%%%%% comparison

'mc__=='(A,A,True) :- !, 'True' = True.
'mc__=='(_,_,'False').

'mi__<'(A,B,R) :- number(A),number(B),!,(A<B -> R=1 ; R=0).
%'mi__<'(A,B,['<',A,B]).

'mi__>'(A,B,R) :- number(A),number(B),!,(A>B -> R=1 ; R=0).
%'mi__>'(A,B,['>',A,B]).

%%%%%%%%%%%%%%%%%%%%% lists

'mc__car-atom'([H|_],H).

'mc__cdr-atom'([_|T],T).

'mc__cons-atom'(A,B,[A|B]).

%%%%%%%%%%%%%%%%%%%%% superpose, collapse

'mi__superpose'([H|_],H).
'mi__superpose'([_|T],R) :- 'mi__superpose'(T,R).

%%%%%%%%%%%%%%%%%%%%% misc

'mi__empty'(_) :- fail.

'mi__stringToChars'(S,C) :- string_chars(S,C).

'mi__charsToString'(C,S) :- string_chars(S,C).

mc__assertEqualToResult(A, B, C) :- u_assign([assertEqualToResult, A, B], C).
