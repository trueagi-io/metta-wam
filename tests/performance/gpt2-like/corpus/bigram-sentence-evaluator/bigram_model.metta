;                                           (convert_to_metta_file  bigram_model $_686472 tests/performance/gpt2-like/corpus/bigram-sentence-evaluator/bigram_model.pl tests/performance/gpt2-like/corpus/bigram-sentence-evaluator/bigram_model.metta)
  !(:: 
    (unigrams.pl *) 
    (bigrams.pl *))


  (= (calc-unigram $X $W)  
    (det-if-then-else 
      (unigram $Y $W) 
      (is $X $Y) 
      (is $X 0)))
; /* Rules to check if unigram,bigram for given word is present or not and return 0 if 'not found' else return the respective value*/

  (= (calc-bigram $X $W1 $W2)  
    (det-if-then-else 
      (bigram $Y $W1 $W2) 
      (is $X $Y) 
      (is $X 0)))


  (= (calc-prob $ListOfWords $SmoothedLog10Probability)  
    (calc-prob $ListOfWords 0 $SmoothedLog10Probability))
; /* Calculate probability of senstence - 14789 is the count of unigrams in my file*/
  (= (calc_prob  ($_) $N $N)  True)
  (= (calc-prob (Cons  $W1 (Cons  $W2 $L)) $T $X)  
    (calc-unigram $U $W1)
    (calc-bigram $B $W1 $W2)
    (is $M 
      (+ $T 
        (log10 (/ (+ $B 1) (+ $U 14789)))))
    (calc-prob 
      (Cons  $W2 $L) $M $X))

