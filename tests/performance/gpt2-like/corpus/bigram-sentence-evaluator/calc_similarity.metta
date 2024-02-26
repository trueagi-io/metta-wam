;                                           (convert_to_metta_file  calc_similarity $_440294 tests/performance/gpt2-like/corpus/bigram-sentence-evaluator/calc_similarity.pl tests/performance/gpt2-like/corpus/bigram-sentence-evaluator/calc_similarity.metta)

  !(:: (wordVectors.pl *))
  !(:: (trigramModel.pl *))


;
;  Main predicate
;
; ===============

  (= (similarity $X $Y $Z)  
    (get-sum-vec $X $VectorXTotal)
    (get-sum-vec $Y $VectorYTotal)
    (cosine $VectorXTotal $VectorYTotal $Z)
    (set-det))
; ; Sum context vectors to X's blank vector
; ; Sum context vectors to Y's blank vector   ;
;  Calculate cosine / euclidean between X's vector and Y's vector

;
;  The cosine distance
;
; ====================

  (= (cosine $X $Y $Z)  
    (det-if-then-else 
      (, 
        (dotProd $X $Y $DotProd) 
        (crossProd $X $Y $CrossProd) 
        (not (=:= $CrossProd 0))) 
      (is $Z 
        (/ $DotProd $CrossProd)) False))
; ; Find dot product of X and Y
; ; Find cross product of X and Y ;
;  Divide dot & cross prod to get cosine dist. If cross prod is 0 return false

;
;  Rule to find the dot product of to given vector

  (= (dotProd $X $Y $Z)  
    (dotProd $X $Y 0 $Z))
  (= (dotProd (Cons  $Val1 $X) (Cons  $Val2 $Y) $Temp $Z)  
    (is $Temp2 
      (+ $Temp 
        (* $Val1 $Val2)))
    (dotProd $X $Y $Temp2 $Z))
  (= (dotProd  () () $Z $Z)  True)

;
;  Rule to find the L2 norm and cross product 

  (= (crossProd $X $Y $Z)  
    (dotProd $X $X $X2)
    (dotProd $Y $Y $Y2)
    (is $Z 
      (* 
        (sqrt $X2) 
        (sqrt $Y2))))
; ; Calculate L2 Norm - it is same as dot product of vector with itself


;
;  Most Similar Words
;
;  Find all the pairs for given word from the wordVector (except word with itself), calculate their cosine dist and store it in td(Dist, RelatedWord)
;
;  Short all the td(Dist, RelatedWord) and traverse the list in descending order of consine distance
;
; ===================

  (= (most-similar $X $Z)  
    (findall 
      (td $Dist $RelatedWord) 
      (, 
        (wordvec $RelatedWord $_) 
        (\= $X $RelatedWord) 
        (similarity $RelatedWord $X $Dist)) $RelWordList)
    (sort 0 @> $RelWordList $L)
    (traverse-list $L $Z))

;
;  Traverse the sorted RelWordList 

  (= (traverse-list (Cons  (td $_ $Y) $_) $Z)  
    (= $Z $Y))
  (= (traverse-list (Cons  $_ $Y) $Z)  
    (traverse-list $Y $Z))



;
;   Create randomly indexed vector
;
; 

  (= (get-sum-vec $X $VectorTotal)  
    (length $Vector 150)
    (findall $Y 
      (, 
        (member $Y $Vector) 
        (= $Y 0)) $Vector)
    (add-all-vecs-left $X $Vector $VectorTotal))
; ; Create list of size 150
; ; Fill list with 0s   ;
;  Add all context vectors


  (= (add-all-vecs-left $Word $Vector $VectorTotal)  
    (findall 
      (pair $ContexWord $N) 
      (t $ContexWord $_ $Word $N) $L1)
    (findall 
      (pair $ContexWord $N) 
      (t $_ $ContexWord $Word $N) $L2)
    (findall 
      (pair $ContexWord $N) 
      (t $Word $ContexWord $_ $N) $L3)
    (findall 
      (pair $ContexWord $N) 
      (t $Word $_ $ContexWord $N) $L4)
    (add $Vector $L1 $VTemp1)
    (add $VTemp1 $L2 $VTemp2)
    (add $VTemp2 $L3 $VTemp3)
    (add $VTemp3 $L4 $VectorTotal))
; ; Word at position -2
; ; Word at position -1
; ; Word at position +1
; ; Word at position +2
; ; Add all the above vectors

;
;  Vector addition
;
; 

  (= (add  $Vector () $Vector)  True)
  (= (add $Vector1 (Cons  $Pair $L) $Vector)  
    (add-V-N-times $Vector1 $Pair $VectorR)
    (add $VectorR $L $Vector))

;
;  Done adding
;
; 

  (= (add_V_N_times  $Vector (pair  $_ 0) $Vector)  True)

;
;  Add the vector given the number of times the trigram occurs 
;
; 
  (= (add-V-N-times $Vector1 (pair $CWord $N) $VectorR)  
    (> $N 0)
    (wordvec $CWord $Vector2)
    (maplist plus $Vector1 $Vector2 $Vector3)
    (is $M 
      (- $N 1))
    (add-V-N-times $Vector3 
      (pair $CWord $M) $VectorR))

