;                                           (convert_to_metta_file  hmm_tagger $_600014 tests/performance/gpt2-like/corpus/bigram-sentence-evaluator/hmm_tagger.pl tests/performance/gpt2-like/corpus/bigram-sentence-evaluator/hmm_tagger.metta)

  !(:: (sigmas.pl *))
  !(:: (taus.pl *))
  !(:: (tags-count.pl *))


;
;  The program is updated for tau() and sigma(). Calculating them inside the
;
;  program using tau() sigma() and tags_count(). The probability variables are
;
;  updated accordingly (Prob is TauProb*SigmaProb*Prob3
;
;  ========================================================================


;
;  Compute all possible "solution(Probability,TagList)" for a given WordList
;
;  ===================================================

  (= (viterbi-all $WordList (solution $P $Sequence))  
    (sequences $WordList 
      (:: (solution 1 (:: @))) $PSs)
    (member 
      (solution $P $Sequence1) $PSs)
    (reverse $Sequence1 
      (Cons  @ $Sequence)))

;
;  Base case: no more words to parse.
;
;  ===================================================

  (= (sequences  () $PSs $PSs)  True)

;
;  Recursive case: find max (of all taggings for Word * all transitions)
;
;  ===================================================
  (= (sequences (Cons  $Word $Words) $PSs0 $PSs)  
    (findall $PS2 
      (, 
        (tau $Prob1 $Word $T2) 
        (tag-count $C $T2) 
        (is $TauProb 
          (/ $Prob1 $C)) 
        (findall 
          (solution $Prob 
            (Cons  $T2 
              (Cons  $T1 $Ts))) 
          (, 
            (member 
              (solution $Prob3 
                (Cons  $T1 $Ts)) $PSs0) 
            (sigma $Prob2 $T1 $T2) 
            (tag-count $D $T1) 
            (is $SigmaProb 
              (/ $Prob2 $D)) 
            (is $Prob 
              (* 
                (* $TauProb $SigmaProb) $Prob3))) $PSs) 
        (max-key $PSs $PS2)) $PSs1)
    (sequences $Words $PSs1 $PSs))

;
;  Find tagging with maximum Probability (tail-recursive, with accumulator) 
;
;  ===================================================

  (= (max-key $PList $Max)  
    (max-key $PList 
      (solution 0 Nil) $Max))
  (= (max_key  () $X $X)  True)
  (= (max-key (Cons  (solution $P $_) $Rest) (solution $N $L) $M)  
    (> $N $P)
    (max-key $Rest 
      (solution $N $L) $M))
  (= (max-key (Cons  (solution $P $L) $Rest) (solution $N $_) $M)  
    (> $P $N)
    (max-key $Rest 
      (solution $P $L) $M))

