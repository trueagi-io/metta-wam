:- ['wordVectors.pl'].
:- ['trigramModel.pl'].


% Main predicate
%===============
similarity(X,Y,Z):-
	get_sum_vec(X,VectorXTotal),              % Sum context vectors to X's blank vector
	get_sum_vec(Y,VectorYTotal),              % Sum context vectors to Y's blank vector
    cosine(VectorXTotal,VectorYTotal,Z),!.   % Calculate cosine / euclidean between X's vector and Y's vector

% The cosine distance
%====================
cosine(X,Y,Z):-
    dotProd(X,Y,DotProd),                     % Find dot product of X and Y
	crossProd(X,Y,CrossProd),                 % Find cross product of X and Y
	not(CrossProd=:=0)-> Z is DotProd / CrossProd; false. % Divide dot & cross prod to get cosine dist. If cross prod is 0 return false

% Rule to find the dot product of to given vector
dotProd(X,Y,Z):-dotProd(X,Y,0,Z).
dotProd([Val1|X],[Val2|Y],Temp,Z):-Temp2 is Temp + (Val1*Val2), dotProd(X,Y,Temp2,Z).
dotProd([],[],Z,Z).

% Rule to find the L2 norm and cross product 
crossProd(X,Y,Z):-
    dotProd(X,X,X2),                          % Calculate L2 Norm - it is same as dot product of vector with itself
	dotProd(Y,Y,Y2),
	Z is sqrt(X2) * sqrt(Y2).


% Most Similar Words
% Find all the pairs for given word from the wordVector (except word with itself), calculate their cosine dist and store it in td(Dist, RelatedWord)
% Short all the td(Dist, RelatedWord) and traverse the list in descending order of consine distance
%===================
most_similar(X,Z):-
	findall(td(Dist, RelatedWord), (wordvec(RelatedWord,_), X \= RelatedWord, similarity(RelatedWord,X,Dist)) , RelWordList),
	sort(0, @>, RelWordList, L),
	traverse_list(L,Z).

% Traverse the sorted RelWordList 
traverse_list([td(_,Y)|_],Z):-Z=Y.
traverse_list([_|Y],Z):-traverse_list(Y,Z).



%  Create randomly indexed vector
%
get_sum_vec(X,VectorTotal):-
	length(Vector,150),                                    % Create list of size 150
    findall(Y,(member(Y,Vector), Y=0),Vector), % Fill list with 0s
	add_all_vecs_left(X,Vector,VectorTotal).   % Add all context vectors

add_all_vecs_left(Word,Vector,VectorTotal):-
	findall( pair(ContexWord,N), t(ContexWord,_,Word,N), L1),    % Word at position -2
	findall( pair(ContexWord,N), t(_,ContexWord,Word,N), L2),    % Word at position -1
	findall( pair(ContexWord,N), t(Word,ContexWord,_,N), L3),    % Word at position +1
	findall( pair(ContexWord,N), t(Word,_,ContexWord,N), L4),    % Word at position +2
	   add(Vector,L1,VTemp1),                                                          % Add all the above vectors
       add(VTemp1,L2,VTemp2),
       add(VTemp2,L3,VTemp3),
       add(VTemp3,L4,VectorTotal).

% Vector addition
%
add(Vector,[],Vector).
add(Vector1,[Pair|L],Vector):-
	add_V_N_times(Vector1,Pair,VectorR),
    add(VectorR,L,Vector).

% Done adding
%
add_V_N_times(Vector,pair(_,0),Vector).

% Add the vector given the number of times the trigram occurs 
%
add_V_N_times(Vector1,pair(CWord,N),VectorR):-
        N > 0,
        wordvec(CWord,Vector2),
        maplist(plus, Vector1,Vector2,Vector3),
        M is N-1,
	add_V_N_times(Vector3,pair(CWord,M),VectorR).
