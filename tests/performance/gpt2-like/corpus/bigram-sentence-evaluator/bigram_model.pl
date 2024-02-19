:-['unigrams.pl','bigrams.pl'].

/* Rules to check if unigram,bigram for given word is present or not and return 0 if 'not found' else return the respective value*/
calc_unigram(X,W):- unigram(Y,W) -> X is Y; X is 0.
calc_bigram(X,W1,W2):- bigram(Y,W1,W2) -> X is Y; X is 0.

/* Calculate probability of senstence - 14789 is the count of unigrams in my file*/
calc_prob(ListOfWords,SmoothedLog10Probability) :- calc_prob(ListOfWords,0,SmoothedLog10Probability).
calc_prob([_], N, N).
calc_prob([W1,W2|L], T, X) :- calc_unigram(U,W1), 
							  calc_bigram(B,W1,W2), 
							  M is T + log10((B+1)/(U+14789)), 
							  calc_prob([W2|L], M, X).
