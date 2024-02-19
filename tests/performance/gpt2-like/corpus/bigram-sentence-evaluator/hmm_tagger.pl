:- ['sigmas.pl'].
:- ['taus.pl'].
:- ['tags_count.pl'].


% The program is updated for tau() and sigma(). Calculating them inside the
% program using tau() sigma() and tags_count(). The probability variables are
% updated accordingly (Prob is TauProb*SigmaProb*Prob3
% ========================================================================


% Compute all possible "solution(Probability,TagList)" for a given WordList
% ===================================================
viterbi_all(WordList,solution(P,Sequence)) :-
          sequences(WordList,[solution(1,[@])],PSs),
          member(solution(P,Sequence1),PSs),
          reverse(Sequence1,[@|Sequence]).

% Base case: no more words to parse.
% ===================================================
sequences([],PSs,PSs).

% Recursive case: find max (of all taggings for Word * all transitions)
% ===================================================
sequences([Word|Words],PSs0,PSs) :-
     findall(PS2,
                   (tau(Prob1,Word,T2),
				    tag_count(C,T2),
					TauProb is Prob1/C,
                    findall(solution(Prob,[T2,T1|Ts]),
                                  (member(solution(Prob3,[T1|Ts]),PSs0),
                                     sigma(Prob2,T1,T2),
									 tag_count(D,T1),
									 SigmaProb is Prob2/D,
                                     Prob is TauProb*SigmaProb*Prob3),
                               PSs),
                    max_key(PSs,PS2)),
                PSs1),
sequences(Words,PSs1,PSs).

% Find tagging with maximum Probability (tail-recursive, with accumulator) 
% ===================================================
max_key(PList,Max):-  max_key(PList,solution(0,[]),Max).
max_key([],X,X).
max_key([solution(P,_)|Rest],solution(N,L),M):- N > P, max_key(Rest,solution(N,L),M).
max_key([solution(P,L)|Rest],solution(N,_),M):- P > N, max_key(Rest,solution(P,L),M).
