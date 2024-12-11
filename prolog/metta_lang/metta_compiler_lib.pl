:- discontiguous get_type_sig/3.

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

'mc__=='(A,A,1) :- !.
'mc__=='(_,_,0).

'mc__<'(A,B,R) :- number(A),number(B),!,(A<B -> R=1 ; R=0).
'mc__<'(A,B,['<',A,B]).

%%%%%%%%%%%%%%%%%%%%% lists

'mc__car-atom'([H|_],H).

'mc__cdr-atom'([_|T],T).

'mc__cons-atom'(A,B,[AA|B]) :- as_p1(A,AA).

%%%%%%%%%%%%%%%%%%%%% misc

'mc__empty'(_) :- fail.

'mc__stringToChars'(S,C) :- string_chars(S,C).

'mc__charsToString'(C,S) :- string_chars(S,C).

mc__assertEqualToResult(A, B, C) :- u_assign([assertEqualToResult, A, B], C).
