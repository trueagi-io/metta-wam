:- discontiguous get_type_sig/3.

% get_type_sig('+',['Number','Number'],'Number').
'mc__+'(A,B,R) :- number(A),number(B),!,plus(A,B,R).
'mc__+'(A,B,['+',A,B]).

'mc__-'(A,B,R) :- number(A),number(B),!,plus(B,R,A).
'mc__-'(A,B,['-',A,B]).

'mc__<'(A,B,R) :- number(A),number(B),!,(A<B -> R=1 ; R=0).
'mc__<'(A,B,['<',A,B]).

'mc__car-atom'([H|_],H).

'mc__cdr-atom'([_|T],T).

'mc__=='(A,A,1) :- !.
'mc__=='(_,_,0).

'mc__cons-atom'(A,B,[A|B]).

'mc__stringToChars'(S,C) :- string_chars(S,C).

'mc__charsToString'(C,S) :- string_chars(S,C).
