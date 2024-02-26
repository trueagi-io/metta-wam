;
;                    generated: 19 November 1989

;
;                    option(s):

;
; 

;
;                      chat_parser

;
; 

;
;                      Fernando C. N. Pereira and David H. D. Warren



  (= 
    (top) 
    (chat-parser))


  (= 
    (go) 
    ( (statistics runtime 
        (:: $_ $_)) 
      (chat-parser) 
      (statistics runtime 
        (:: $_ $T)) 
      (write 'execution time is ') 
      (write $T) 
      (write milliseconds)))


  (= 
    (chat-parser) 
    (not (, (my-string $X) (not (determinate-say $X $Y)))))
;                       ; original code used a failure-driven loop; changed to use unrolled

;                       ; forall/2 so that unexpected failures will make chat_parser/0 fail



;
;                        query set



  (= 
    (my_string  
      (what rivers are there ?)) True)
  (= 
    (my_string  
      (does afghanistan border china ?)) True)
  (= 
    (my_string  
      (what is the capital of upper_volta ?)) True)
  (= 
    (my_string  
      (where is the largest country ?)) True)
  (= 
    (my_string  
      (which country ~ s capital is london ?)) True)
  (= 
    (my_string  
      (which countries are european ?)) True)
  (= 
    (my_string  
      (how large is the smallest american country ?)) True)
  (= 
    (my_string  
      (what is the ocean that borders african countries and that borders asian countries ?)) True)
  (= 
    (my_string  
      (what are the capitals of the countries bordering the baltic ?)) True)
  (= 
    (my_string  
      (which countries are bordered by two seas ?)) True)
  (= 
    (my_string  
      (how many countries does the danube flow through ?)) True)
  (= 
    (my_string  
      (what is the total area of countries south of the equator and not in australasia ?)) True)
  (= 
    (my_string  
      (what is the average area of the countries in each continent ?)) True)
  (= 
    (my_string  
      (is there more than one country in each continent ?)) True)
  (= 
    (my_string  
      (is there some ocean that does not border any country ?)) True)
  (= 
    (my_string  
      (what are the countries from which a river flows into the black_sea ?)) True)


;
;                                determinate_say



  (= 
    (determinate-say $X $Y) 
    ( (say $X $Y) (set-det)))


;
;                               -----------------------------------------------------------------------------

;
; 

;
;                                 xgrun

;
; 

;
;                                -----------------------------------------------------------------------------



  (= 
    (terminal  $T $S $S 
      (x  $_ terminal $T $X) $X) True)
  (= 
    (terminal $T 
      (Cons  $T $S) $S $X $X) 
    (gap $X))


  (= 
    (gap  
      (x  gap $_ $_ $_)) True)
  (= 
    (gap  ()) True)


  (= 
    (virtual  $NT 
      (x  $_ nonterminal $NT $X) $X) True)


;
;                                   ----------------------------------------------------------------------------

;
; 

;
;                                     clotab

;
; 

;
;                                    ----------------------------------------------------------------------------


;
;                                     normal form masks



  (= 
    (is_pp  
      (#  1 $_ $_ $_)) True)


  (= 
    (is_pred  
      (#  $_ 1 $_ $_)) True)


  (= 
    (is_trace  
      (#  $_ $_ 1 $_)) True)


  (= 
    (is_adv  
      (#  $_ $_ $_ 1)) True)


  (= 
    (trace1  
      (#  $_ $_ 1 $_) 
      (#  0 0 0 0)) True)

  (= 
    (trace1  
      (#  0 0 1 0)) True)


  (= 
    (adv  
      (#  0 0 0 1)) True)


  (= 
    (empty  
      (#  0 0 0 0)) True)


  (= 
    (np_all  
      (#  1 1 1 0)) True)


  (= 
    (s_all  
      (#  1 0 1 1)) True)


  (= 
    (np_no_trace  
      (#  1 1 0 0)) True)

;
;                                          mask operations



  (= 
    (myplus 
      (# $B1 $B2 $B3 $B4) 
      (# $C1 $C2 $C3 $C4) 
      (# $D1 $D2 $D3 $D4)) 
    ( (or $B1 $C1 $D1) 
      (or $B2 $C2 $D2) 
      (or $B3 $C3 $D3) 
      (or $B4 $C4 $D4)))


  (= 
    (minus 
      (# $B1 $B2 $B3 $B4) 
      (# $C1 $C2 $C3 $C4) 
      (# $D1 $D2 $D3 $D4)) 
    ( (anot $B1 $C1 $D1) 
      (anot $B2 $C2 $D2) 
      (anot $B3 $C3 $D3) 
      (anot $B4 $C4 $D4)))


  (= 
    (or  1 $_ 1) True)
  (= 
    (or  0 1 1) True)
  (= 
    (or  0 0 0) True)


  (= 
    (anot  $X 0 $X) True)
  (= 
    (anot  $X 1 0) True)

;
;                                          noun phrase position features



  (= 
    (role  subj $_ 
      (#  1 0 0)) True)
  (= 
    (role  compl $_ 
      (#  0 $_ $_)) True)
  (= 
    (role  undef main 
      (#  $_ 0 $_)) True)
  (= 
    (role  undef aux 
      (#  0 $_ $_)) True)
  (= 
    (role  undef decl $_) True)
  (= 
    (role  nil $_ $_) True)


  (= 
    (subj_case  
      (#  1 0 0)) True)

  (= 
    (verb_case  
      (#  0 1 0)) True)

  (= 
    (prep_case  
      (#  0 0 1)) True)

  (= 
    (compl_case  
      (#  0 $_ $_)) True)


;
;                                         ----------------------------------------------------------------------------

;
; 

;
;                                           newg

;
; 

;
;                                         ----------------------------------------------------------------------------



  (= 
    (say $X $Y) 
    (sentence $Y $X Nil Nil Nil))



  (= 
    (sentence $B $C $D $E $F) 
    ( (declarative $B $C $G $E $H) (terminator . $G $D $H $F)))
  (= 
    (sentence $B $C $D $E $F) 
    ( (wh-question $B $C $G $E $H) (terminator ? $G $D $H $F)))
  (= 
    (sentence $B $C $D $E $F) 
    ( (topic $C $G $E $H) 
      (wh-question $B $G $I $H $J) 
      (terminator ? $I $D $J $F)))
  (= 
    (sentence $B $C $D $E $F) 
    ( (yn-question $B $C $G $E $H) (terminator ? $G $D $H $F)))
  (= 
    (sentence $B $C $D $E $F) 
    ( (imperative $B $C $G $E $H) (terminator (set-det) $G $D $H $F)))



  (= 
    (pp $B $C $D $E $F $F $G $H) 
    (virtual 
      (pp $B $C $D $E) $G $H))
  (= 
    (pp 
      (pp $B $C) $D $E $F $G $H $I $J) 
    ( (prep $B $G $K $I $L) 
      (prep-case $M) 
      (np $C $N $M $O $D $E $F $K $H $L $J)))



  (= 
    (topic $B $C $D 
      (x gap nonterminal 
        (pp $E compl $F $G) $H)) 
    ( (pp $E compl $F $G $B $I $D $J) (opt-comma $I $C $J $H)))



  (= 
    (opt-comma $B $C $D $E) 
    (~ , $B $C $D $E))
  (= 
    (opt_comma  $B $B $C $C) True)



  (= 
    (declarative 
      (decl $B) $C $D $E $F) 
    (s $B $G $C $D $E $F))



  (= 
    (wh-question 
      (whq $B $C) $D $E $F $G) 
    ( (variable-q $B $H $I $J $D $K $F $L) (question $I $J $C $K $E $L $G)))



  (= 
    (np $B $C $D $E $F $G $H $I $I $J $K) 
    (virtual 
      (np $B $C $D $E $F $G $H) $J $K))
  (= 
    (np 
      (np $B $C Nil) $B $D def $E $F $G $H $I $J $K) 
    ( (is-pp $F) 
      (pers-pron $C $B $L $H $I $J $K) 
      (empty $G) 
      (role $L decl $D)))
  (= 
    (np 
      (np $B $C $D) $B $E $F $G $H $I $J $K $L $M) 
    ( (is-pp $H) 
      (np-head $C $B 
        (+ $F $N) $O $D $J $P $L $Q) 
      (np-all $R) 
      (np-compls $N $B $G $O $R $I $P $K $Q $M)))
  (= 
    (np 
      (part $B $C) 
      (+ 3 $D) $E indef $F $G $H $I $J $K $L) 
    ( (is-pp $G) 
      (determiner $B $D indef $I $M $K $N) 
      (~ of $M $O $N $P) 
      (s-all $Q) 
      (prep-case $R) 
      (np $C 
        (+ 3 plu) $R def $F $Q $H $O $J $P $L)))



  (= 
    (variable-q $B $C $D $E $F $G $H 
      (x gap nonterminal 
        (np $I $C $E $J $K $L $M) $N)) 
    ( (whq $B $C $I $D $F $G $H $N) (trace1 $L $M)))
  (= 
    (variable-q $B $C compl $D $E $F $G 
      (x gap nonterminal 
        (pp 
          (pp $H $I) compl $J $K) $L)) 
    ( (prep $H $E $M $G $N) 
      (whq $B $C $I $O $M $F $N $L) 
      (trace1 $J $K) 
      (compl-case $D)))
  (= 
    (variable-q $B $C compl $D $E $F $G 
      (x gap nonterminal 
        (adv-phrase 
          (pp $H 
            (np $C 
              (np-head 
                (int-det $B) Nil $I) Nil)) $J $K) $L)) 
    ( (context-pron $H $I $E $F $G $L) 
      (trace1 $J $K) 
      (verb-case $D)))
  (= 
    (variable-q $B $C compl $D $E $F $G 
      (x gap nonterminal 
        (predicate adj 
          (value $H 
            (wh $B)) $I) $J)) 
    ( (~ how $E $K $G $L) 
      (adj quant $H $K $F $L $J) 
      (empty $I) 
      (verb-case $D)))



  (= 
    (adv-phrase $B $C $D $E $E $F $G) 
    (virtual 
      (adv-phrase $B $C $D) $F $G))
  (= 
    (adv-phrase 
      (pp $B $C) $D $E $F $G $H $I) 
    ( (loc-pred $B $F $J $H $K) (pp (pp (prep of) $C) compl $D $E $J $G $K $I)))



  (= 
    (predicate $B $C $D $E $E $F $G) 
    (virtual 
      (predicate $B $C $D) $F $G))
  (= 
    (predicate $B $C $D $E $F $G $H) 
    (adj-phrase $C $D $E $F $G $H))
  (= 
    (predicate neg $B $C $D $E $F $G) 
    ( (s-all $H) (pp $B compl $H $C $D $E $F $G)))
  (= 
    (predicate $B $C $D $E $F $G $H) 
    ( (s-all $I) (adv-phrase $C $I $D $E $F $G $H)))



  (= 
    (whq $B $C $D undef $E $F $G $H) 
    ( (int-det $B $C $E $I $G $J) 
      (s-all $K) 
      (np $D $C $L $M subj $K $N $I $F $J $H)))
  (= 
    (whq $B 
      (+ 3 $C) 
      (np 
        (+ 3 $C) 
        (wh $B) Nil) $D $E $F $G $H) 
    (int-pron $D $E $F $G $H))



  (= 
    (int-det $B 
      (+ 3 $C) $D $E $F $G) 
    (whose $B $C $D $E $F $G))
  (= 
    (int-det $B 
      (+ 3 $C) $D $E $F $G) 
    (int-art $B $C $D $E $F $G))



  (= 
    (gen-marker $B $B $C $D) 
    (virtual gen-marker $C $D))
  (= 
    (gen-marker $B $C $D $E) 
    ( (~ ~ $B $F $D $G) (an-s $F $C $G $E)))



  (= 
    (whose $B $C $D $E $F 
      (x nogap nonterminal 
        (np-head0 
          (wh $B) $C proper) 
        (x nogap nonterminal gen-marker $G))) 
    (~ whose $D $E $F $G))



  (= 
    (question $B $C $D $E $F $G $H) 
    ( (subj-question $B) 
      (role subj $I $C) 
      (s $D $J $E $F $G $H)))
  (= 
    (question $B $C $D $E $F $G $H) 
    ( (fronted-verb $B $C $E $I $G $J) (s $D $K $I $F $J $H)))



  (= 
    (det $B $C $D $E $E $F $G) 
    (virtual 
      (det $B $C $D) $F $G))
  (= 
    (det 
      (det $B) $C $D $E $F $G $H) 
    ( (terminal $I $E $F $G $H) (det $I $C $B $D)))
  (= 
    (det  generic $B generic $C $C $D $D) True)



  (= 
    (int-art $B $C $D $E $F 
      (x nogap nonterminal 
        (det $G $C def) $H)) 
    (int-art $B $C $G $D $E $F $H))



  (= 
    (subj_question  subj) True)
  (= 
    (subj_question  undef) True)



  (= 
    (yn-question 
      (q $B) $C $D $E $F) 
    ( (fronted-verb nil $G $C $H $E $I) (s $B $J $H $D $I $F)))



  (= 
    (verb-form $B $C $D $E $F $F $G $H) 
    (virtual 
      (verb-form $B $C $D $E) $G $H))
  (= 
    (verb-form $B $C $D $E $F $G $H $I) 
    ( (terminal $J $F $G $H $I) (verb-form $J $B $C $D)))



  (= 
    (neg $B $C $D $D $E $F) 
    (virtual 
      (neg $B $C) $E $F))
  (= 
    (neg 
      (+ aux $B) neg $C $D $E $F) 
    (~ not $C $D $E $F))
  (= 
    (neg  $B pos $C $C $D $D) True)



  (= 
    (fronted-verb $B $C $D $E $F 
      (x gap nonterminal 
        (verb-form $G $H $I $J) 
        (x nogap nonterminal 
          (neg $K $L) $M))) 
    ( (verb-form $G $H $I $N $D $O $F $P) 
      (verb-type $G 
        (+ aux $Q)) 
      (role $B $J $C) 
      (neg $R $L $O $E $P $M)))



  (= 
    (imperative 
      (imp $B) $C $D $E $F) 
    ( (imperative-verb $C $G $E $H) (s $B $I $G $D $H $F)))



  (= 
    (imperative-verb $B $C $D 
      (x nogap terminal you 
        (x nogap nonterminal 
          (verb-form $E 
            (+ imp fin) 
            (+ 2 sin) main) $F))) 
    (verb-form $E inf $G $H $B $C $D $F))



  (= 
    (s 
      (s $B $C $D $E) $F $G $H $I $J) 
    ( (subj $B $K $L $G $M $I $N) 
      (verb $C $K $L $O $M $P $N $Q) 
      (empty $R) 
      (s-all $S) 
      (verb-args $L $O $D $R $T $P $U $Q $V) 
      (minus $S $T $W) 
      (myplus $S $T $X) 
      (verb-mods $E $W $X $F $U $H $V $J)))



  (= 
    (subj there $B 
      (+ $C be) $D $E $F $G) 
    (~ there $D $E $F $G))
  (= 
    (subj $B $C $D $E $F $G $H) 
    ( (s-all $I) 
      (subj-case $J) 
      (np $B $C $J $K subj $I $L $E $F $G $H)))



  (= 
    (np-head $B $C $D $E $F $G $H $I $J) 
    ( (np-head0 $K $L $M $G $N $I $O) (possessive $K $L $M $P $P $B $C $D $E $F $N $H $O $J)))



  (= 
    (np-head0 $B $C $D $E $E $F $G) 
    (virtual 
      (np-head0 $B $C $D) $F $G))
  (= 
    (np-head0 
      (name $B) 
      (+ 3 sin) 
      (+ def proper) $C $D $E $F) 
    (name $B $C $D $E $F))
  (= 
    (np-head0 
      (np-head $B $C $D) 
      (+ 3 $E) 
      (+ $F common) $G $H $I $J) 
    ( (determiner $B $E $F $G $K $I $L) 
      (adjs $C $K $M $L $N) 
      (noun $D $E $M $H $N $J)))
  (= 
    (np-head0 $B $C 
      (+ def proper) $D $E $F 
      (x nogap nonterminal gen-marker $G)) 
    (poss-pron $B $C $D $E $F $G))
  (= 
    (np-head0 
      (np-head $B Nil $C) 
      (+ 3 sin) 
      (+ indef common) $D $E $F $G) 
    (quantifier-pron $B $C $D $E $F $G))



  (= 
    (np-compls proper $B $C Nil $D $E $F $F $G $G) 
    (empty $E))
  (= 
    (np-compls common $B $C $D $E $F $G $H $I $J) 
    ( (np-all $K) 
      (np-mods $B $C $L $D $E $M $K $N $G $O $I $P) 
      (relative $B $L $M $N $F $O $H $P $J)))



  (= 
    (possessive $B $C $D Nil $E $F $G $H $I $J $K $L $M $N) 
    ( (gen-case $K $O $M $P) 
      (np-head0 $Q $R $S $O $T $P $U) 
      (possessive $Q $R $S $V 
        (Cons  
          (pp poss 
            (np $C $B $E)) $V) $F $G $H $I $J $T $L $U $N)))
  (= 
    (possessive  $B $C $D $E $F $B $C $D $E $F $G $G $H $H) True)



  (= 
    (gen-case $B $C $D 
      (x nogap terminal the $E)) 
    (gen-marker $B $C $D $E))



  (= 
    (an-s $B $C $D $E) 
    (~ s $B $C $D $E))
  (= 
    (an_s  $B $B $C $C) True)



  (= 
    (determiner $B $C $D $E $F $G $H) 
    (det $B $C $D $E $F $G $H))
  (= 
    (determiner $B $C $D $E $F $G $H) 
    (quant-phrase $B $C $D $E $F $G $H))



  (= 
    (quant-phrase 
      (quant $B $C) $D $E $F $G $H $I) 
    ( (quant $B $E $F $J $H $K) (number $C $D $J $G $K $I)))



  (= 
    (quant $B indef $C $D $E $F) 
    ( (neg-adv $G $B $C $H $E $I) 
      (comp-adv $G $H $J $I $K) 
      (~ than $J $D $K $F)))
  (= 
    (quant $B indef $C $D $E $F) 
    ( (~ at $C $G $E $H) 
      (sup-adv $I $G $D $H $F) 
      (sup-op $I $B)))
  (= 
    (quant the def $B $C $D $E) 
    (~ the $B $C $D $E))
  (= 
    (quant  same indef $B $B $C $C) True)



  (= 
    (neg-adv $B 
      (+ not $B) $C $D $E $F) 
    (~ not $C $D $E $F))
  (= 
    (neg_adv  $B $B $C $C $D $D) True)



  (= 
    (sup_op  least 
      (+  not less)) True)
  (= 
    (sup_op  most 
      (+  not more)) True)



  (= 
    (np-mods $B $C $D 
      (Cons  $E $F) $G $H $I $J $K $L $M $N) 
    ( (np-mod $B $C $E $G $O $K $P $M $Q) 
      (trace1 $R) 
      (myplus $R $O $S) 
      (minus $G $S $T) 
      (myplus $O $G $U) 
      (np-mods $B $C $D $F $T $H $U $J $P $L $Q $N)))
  (= 
    (np_mods  $B $C $D $D $E $E $F $F $G $G $H $H) True)



  (= 
    (np-mod $B $C $D $E $F $G $H $I $J) 
    (pp $D $C $E $F $G $H $I $J))
  (= 
    (np-mod $B $C $D $E $F $G $H $I $J) 
    (reduced-relative $B $D $E $F $G $H $I $J))



  (= 
    (verb-mods 
      (Cons  $B $C) $D $E $F $G $H $I $J) 
    ( (verb-mod $B $D $K $G $L $I $M) 
      (trace1 $N) 
      (myplus $N $K $O) 
      (minus $D $O $P) 
      (myplus $K $D $Q) 
      (verb-mods $C $P $Q $F $L $H $M $J)))
  (= 
    (verb_mods  () $B $C $C $D $D $E $E) True)



  (= 
    (verb-mod $B $C $D $E $F $G $H) 
    (adv-phrase $B $C $D $E $F $G $H))
  (= 
    (verb-mod $B $C $D $E $F $G $H) 
    ( (is-adv $C) 
      (adverb $B $E $F $G $H) 
      (empty $D)))
  (= 
    (verb-mod $B $C $D $E $F $G $H) 
    (pp $B compl $C $D $E $F $G $H))



  (= 
    (adjs 
      (Cons  $B $C) $D $E $F $G) 
    ( (pre-adj $B $D $H $F $I) (adjs $C $H $E $I $G)))
  (= 
    (adjs  () $B $B $C $C) True)



  (= 
    (pre-adj $B $C $D $E $F) 
    (adj $G $B $C $D $E $F))
  (= 
    (pre-adj $B $C $D $E $F) 
    (sup-phrase $B $C $D $E $F))



  (= 
    (sup-phrase 
      (sup most $B) $C $D $E $F) 
    (sup-adj $B $C $D $E $F))
  (= 
    (sup-phrase 
      (sup $B $C) $D $E $F $G) 
    ( (sup-adv $B $D $I $F $J) (adj quant $C $I $E $J $G)))



  (= 
    (comp-phrase 
      (comp $B $C $D) $E $F $G $H $I) 
    ( (comp $B $C $F $J $H $K) 
      (np-no-trace $L) 
      (prep-case $M) 
      (np $D $N $M $O compl $L $E $J $G $K $I)))



  (= 
    (comp $B $C $D $E $F $G) 
    ( (comp-adv $B $D $H $F $I) 
      (adj quant $C $H $J $I $K) 
      (~ than $J $E $K $G)))
  (= 
    (comp more $B $C $D $E $F) 
    ( (rel-adj $B $C $G $E $H) (~ than $G $D $H $F)))
  (= 
    (comp same $B $C $D $E $F) 
    ( (~ as $C $G $E $H) 
      (adj quant $B $G $I $H $J) 
      (~ as $I $D $J $F)))



  (= 
    (relative $B 
      (:: $C) $D $E $F $G $H $I $J) 
    ( (is-pred $D) (rel-conj $B $K $C $F $G $H $I $J)))
  (= 
    (relative  $B () $C $D $D $E $E $F $F) True)



  (= 
    (rel-conj $B $C $D $E $F $G $H $I) 
    ( (rel $B $J $K $F $L $H $M) (rel-rest $B $C $J $D $K $E $L $G $M $I)))



  (= 
    (rel-rest $B $C $D $E $F $G $H $I $J $K) 
    ( (conj $C $L $D $M $E $H $N $J $O) (rel-conj $B $L $M $G $N $I $O $K)))
  (= 
    (rel_rest  $B $C $D $D $E $E $F $F $G $G) True)



  (= 
    (rel $B 
      (rel $C $D) $E $F $G $H $I) 
    ( (myopen $F $J $H $K) 
      (variable $B $C $J $L $K $M) 
      (s $D $N $L $O $M $P) 
      (trace1 $Q) 
      (minus $N $Q $E) 
      (close $O $G $P $I)))



  (= 
    (variable $B $C $D $E $F 
      (x gap nonterminal 
        (np 
          (np $B 
            (wh $C) Nil) $B $G $H $I $J $K) $L)) 
    ( (~ that $D $E $F $L) (trace1 $J $K)))
  (= 
    (variable $B $C $D $E $F 
      (x gap nonterminal 
        (np $G $H $I $J $K $L $M) $N)) 
    ( (wh $C $B $G $H $I $D $E $F $N) (trace1 $L $M)))
  (= 
    (variable $B $C $D $E $F 
      (x gap nonterminal 
        (pp 
          (pp $G $H) compl $I $J) $K)) 
    ( (prep $G $D $L $F $M) 
      (wh $C $B $H $N $O $L $E $M $K) 
      (trace1 $I $J) 
      (compl-case $O)))



  (= 
    (wh $B $C 
      (np $C 
        (wh $B) Nil) $C $D $E $F $G $H) 
    ( (rel-pron $I $E $F $G $H) (role $I decl $D)))
  (= 
    (wh $B $C 
      (np $D $E 
        (:: (pp $F $G))) $D $H $I $J $K $L) 
    ( (np-head0 $E $D 
        (+ $M common) $I $N $K $O) 
      (prep $F $N $P $O $Q) 
      (wh $B $C $G $R $S $P $J $Q $L)))
  (= 
    (wh $B $C $D $E $F $G $H $I $J) 
    ( (whose $B $C $G $K $I $L) 
      (s-all $M) 
      (np $D $E $F def subj $M $N $K $H $L $J)))



  (= 
    (reduced-relative $B $C $D $E $F $G $H $I) 
    ( (is-pred $D) (reduced-rel-conj $B $J $C $E $F $G $H $I)))



  (= 
    (reduced-rel-conj $B $C $D $E $F $G $H $I) 
    ( (reduced-rel $B $J $K $F $L $H $M) (reduced-rel-rest $B $C $J $D $K $E $L $G $M $I)))



  (= 
    (reduced-rel-rest $B $C $D $E $F $G $H $I $J $K) 
    ( (conj $C $L $D $M $E $H $N $J $O) (reduced-rel-conj $B $L $M $G $N $I $O $K)))
  (= 
    (reduced_rel_rest  $B $C $D $D $E $E $F $F $G $G) True)



  (= 
    (reduced-rel $B 
      (reduced-rel $C $D) $E $F $G $H $I) 
    ( (myopen $F $J $H $K) 
      (reduced-wh $B $C $J $L $K $M) 
      (s $D $N $L $O $M $P) 
      (trace1 $Q) 
      (minus $N $Q $E) 
      (close $O $G $P $I)))



  (= 
    (reduced-wh $B $C $D $E $F 
      (x nogap nonterminal 
        (np 
          (np $B 
            (wh $C) Nil) $B $G $H $I $J $K) 
        (x nogap nonterminal 
          (verb-form be 
            (+ pres fin) $B main) 
          (x nogap nonterminal 
            (neg $L $M) 
            (x nogap nonterminal 
              (predicate $M $N $O) $P))))) 
    ( (neg $Q $M $D $R $F $S) 
      (predicate $M $N $O $R $E $S $P) 
      (trace1 $J $K) 
      (subj-case $G)))
  (= 
    (reduced-wh $B $C $D $E $F 
      (x nogap nonterminal 
        (np 
          (np $B 
            (wh $C) Nil) $B $G $H $I $J $K) 
        (x nogap nonterminal 
          (verb $L $M $N $O) $P))) 
    ( (participle $L $N $O $D $E $F $P) 
      (trace1 $J $K) 
      (subj-case $G)))
  (= 
    (reduced-wh $B $C $D $E $F 
      (x nogap nonterminal 
        (np $G $H $I $J $K $L $M) 
        (x gap nonterminal 
          (np 
            (np $B 
              (wh $C) Nil) $B $N $O $P $Q $R) $S))) 
    ( (s-all $T) 
      (subj-case $I) 
      (verb-case $N) 
      (np $G $H $U $J subj $T $V $D $E $F $S) 
      (trace1 $L $M) 
      (trace1 $Q $R)))



  (= 
    (verb $B $C $D $E $F $F $G $H) 
    (virtual 
      (verb $B $C $D $E) $G $H))
  (= 
    (verb 
      (verb $B $C 
        (+ $D fin) $E $F) $G $H $C $I $J $K $L) 
    ( (verb-form $M 
        (+ $D fin) $G $N $I $O $K $P) 
      (verb-type $M $Q) 
      (neg $Q $F $O $R $P $S) 
      (rest-verb $N $M $B $C $E $R $J $S $L) 
      (verb-type $B $H)))



  (= 
    (rest-verb aux have $B $C 
      (Cons  perf $D) $E $F $G $H) 
    ( (verb-form $I 
        (+ past part) $J $K $E $L $G $M) (have $I $B $C $D $L $F $M $H)))
  (= 
    (rest-verb aux be $B $C $D $E $F $G $H) 
    ( (verb-form $I $J $K $L $E $M $G $N) (be $J $I $B $C $D $M $F $N $H)))
  (= 
    (rest-verb aux do $B active Nil $C $D $E $F) 
    (verb-form $B inf $G $H $C $D $E $F))
  (= 
    (rest_verb  main $B $B active () $C $C $D $D) True)



  (= 
    (have be $B $C $D $E $F $G $H) 
    ( (verb-form $I $J $K $L $E $M $G $N) (be $J $I $B $C $D $M $F $N $H)))
  (= 
    (have  $B $B active () $C $C $D $D) True)



  (= 
    (be  
      (+  past part) $B $B passive () $C $C $D $D) True)
  (= 
    (be 
      (+ pres part) $B $C $D 
      (:: prog) $E $F $G $H) 
    (passive $B $C $D $E $F $G $H))



  (= 
    (passive be $B passive $C $D $E $F) 
    ( (verb-form $B 
        (+ past part) $G $H $C $D $E $F) 
      (verb-type $B $I) 
      (passive $I)))
  (= 
    (passive  $B $B active $C $C $D $D) True)



  (= 
    (participle 
      (verb $B $C inf $D $E) $F $C $G $H $I $J) 
    ( (neg $K $E $G $L $I $M) 
      (verb-form $B $N $O $P $L $H $M $J) 
      (participle $N $C $D) 
      (verb-type $B $F)))



  (= 
    (passive  
      (+  $B trans)) True)
  (= 
    (passive  
      (+  $B ditrans)) True)



  (= 
    (participle  
      (+  pres part) active 
      (prog)) True)
  (= 
    (participle  
      (+  past part) passive ()) True)



  (= 
    (close $B $B $C $D) 
    (virtual close $C $D))



  (= 
    (myopen  $B $B $C 
      (x  gap nonterminal close $C)) True)



  (= 
    (verb-args 
      (+ $B $C) $D $E $F $G $H $I $J $K) 
    ( (advs $E $L $M $H $N $J $O) (verb-args $C $D $L $F $G $N $I $O $K)))
  (= 
    (verb-args trans active 
      (:: (arg dir $B)) $C $D $E $F $G $H) 
    (verb-arg np $B $D $E $F $G $H))
  (= 
    (verb-args ditrans $B 
      (Cons  
        (arg $C $D) $E) $F $G $H $I $J $K) 
    ( (verb-arg np $D $L $H $M $J $N) (object $C $E $L $G $M $I $N $K)))
  (= 
    (verb-args be $B 
      (:: void) $C $C $D $E $F $G) 
    (terminal there $D $E $F $G))
  (= 
    (verb-args be $B 
      (:: (arg predicate $C)) $D $E $F $G $H $I) 
    (pred-conj $J $C $E $F $G $H $I))
  (= 
    (verb-args be $B 
      (:: (arg dir $C)) $D $E $F $G $H $I) 
    (verb-arg np $C $E $F $G $H $I))
  (= 
    (verb-args have active 
      (:: (arg dir $B)) $C $D $E $F $G $H) 
    (verb-arg np $B $D $E $F $G $H))
  (= 
    (verb-args $B $C Nil $D $D $E $E $F $F) 
    (no-args $B))



  (= 
    (object $B $C $D $E $F $G $H $I) 
    ( (adv $J) 
      (minus $J $D $K) 
      (advs $C $L $K $F $M $H $N) 
      (obj $B $L $D $E $M $G $N $I)))



  (= 
    (obj ind 
      (:: (arg dir $B)) $C $D $E $F $G $H) 
    (verb-arg np $B $D $E $F $G $H))
  (= 
    (obj  dir () $B $B $C $C $D $D) True)



  (= 
    (pred-conj $B $C $D $E $F $G $H) 
    ( (predicate $I $J $K $E $L $G $M) (pred-rest $B $J $C $K $D $L $F $M $H)))



  (= 
    (pred-rest $B $C $D $E $F $G $H $I $J) 
    ( (conj $B $K $C $L $D $G $M $I $N) (pred-conj $K $L $F $M $H $N $J)))
  (= 
    (pred_rest  $B $C $C $D $D $E $E $F $F) True)



  (= 
    (verb-arg np $B $C $D $E $F $G) 
    ( (s-all $H) 
      (verb-case $I) 
      (np $B $J $I $K compl $H $C $D $E $F $G)))



  (= 
    (advs 
      (Cons  $B $C) $D $E $F $G $H $I) 
    ( (is-adv $E) 
      (adverb $B $F $J $H $K) 
      (advs $C $D $E $J $G $K $I)))
  (= 
    (advs  $B $B $C $D $D $E $E) True)



  (= 
    (adj-phrase $B $C $D $E $F $G) 
    ( (adj $H $B $D $E $F $G) (empty $C)))
  (= 
    (adj-phrase $B $C $D $E $F $G) 
    (comp-phrase $B $C $D $E $F $G))



  (= 
    (no_args  trans) True)
  (= 
    (no_args  ditrans) True)
  (= 
    (no_args  intrans) True)



  (= 
    (conj 
      (conj $B $C) 
      (conj $B $D) $E $F 
      (conj $B $E $F) $G $H $I $J) 
    (conj $B $C $D $G $H $I $J))



  (= 
    (noun $B $C $D $E $F $G) 
    ( (terminal $H $D $E $F $G) (noun-form $H $B $C)))



  (= 
    (adj $B 
      (adj $C) $D $E $F $G) 
    ( (terminal $C $D $E $F $G) (adj $C $B)))



  (= 
    (prep 
      (prep $B) $C $D $E $F) 
    ( (terminal $B $C $D $E $F) (prep $B)))



  (= 
    (rel-adj 
      (adj $B) $C $D $E $F) 
    ( (terminal $G $C $D $E $F) (rel-adj $G $B)))



  (= 
    (sup-adj 
      (adj $B) $C $D $E $F) 
    ( (terminal $G $C $D $E $F) (sup-adj $G $B)))



  (= 
    (comp-adv less $B $C $D $E) 
    (~ less $B $C $D $E))
  (= 
    (comp-adv more $B $C $D $E) 
    (~ more $B $C $D $E))



  (= 
    (sup-adv least $B $C $D $E) 
    (~ least $B $C $D $E))
  (= 
    (sup-adv most $B $C $D $E) 
    (~ most $B $C $D $E))



  (= 
    (rel-pron $B $C $D $E $F) 
    ( (terminal $G $C $D $E $F) (rel-pron $G $B)))



  (= 
    (name $B $C $D $E $F) 
    ( (opt-the $C $G $E $H) 
      (terminal $B $G $D $H $F) 
      (name $B)))



  (= 
    (int-art $B plu 
      (quant same 
        (wh $B)) $C $D $E $F) 
    ( (~ how $C $G $E $H) (~ many $G $D $H $F)))
  (= 
    (int-art $B $C $D $E $F $G $H) 
    ( (terminal $I $E $F $G $H) (int-art $I $B $C $D)))



  (= 
    (int-pron $B $C $D $E $F) 
    ( (terminal $G $C $D $E $F) (int-pron $G $B)))



  (= 
    (adverb 
      (adv $B) $C $D $E $F) 
    ( (terminal $B $C $D $E $F) (adverb $B)))



  (= 
    (poss-pron 
      (pronoun $B) 
      (+ $C $D) $E $F $G $H) 
    ( (terminal $I $E $F $G $H) (poss-pron $I $B $C $D)))



  (= 
    (pers-pron 
      (pronoun $B) 
      (+ $C $D) $E $F $G $H $I) 
    ( (terminal $J $F $G $H $I) (pers-pron $J $B $C $D $E)))



  (= 
    (quantifier-pron $B $C $D $E $F $G) 
    ( (terminal $H $D $E $F $G) (quantifier-pron $H $B $C)))



  (= 
    (context-pron 
      (prep in) place $B $C $D $E) 
    (~ where $B $C $D $E))
  (= 
    (context-pron 
      (prep at) time $B $C $D $E) 
    (~ when $B $C $D $E))



  (= 
    (number 
      (nb $B) $C $D $E $F $G) 
    ( (terminal $H $D $E $F $G) (number $H $B $C)))



  (= 
    (terminator $B $C $D $E $F) 
    ( (terminal $G $C $D $E $F) (terminator $G $B)))



  (= 
    (opt_the  $B $B $C $C) True)
  (= 
    (opt-the $B $C $D $E) 
    (~ the $B $C $D $E))



  (= 
    (conj $B list list $C $D $E $F) 
    (terminal , $C $D $E $F))
  (= 
    (conj $B list end $C $D $E $F) 
    ( (terminal $B $C $D $E $F) (conj $B)))



  (= 
    (loc-pred $B $C $D $E $F) 
    ( (terminal $G $C $D $E $F) (loc-pred $G $B)))



  (= 
    (~ $B $C $D $E $F) 
    ( (terminal $B $C $D $E $F) (~ $B)))


;
;                                         ----------------------------------------------------------------------------

;
; 

;
;                                           newdic

;
; 

;
;                                         ----------------------------------------------------------------------------



  (= 
    (word $Word) 
    (~ $Word))
  (= 
    (word $Word) 
    (conj $Word))
  (= 
    (word $Word) 
    (adverb $Word))
  (= 
    (word $Word) 
    (sup-adj $Word $_))
  (= 
    (word $Word) 
    (rel-adj $Word $_))
  (= 
    (word $Word) 
    (adj $Word $_))
  (= 
    (word $Word) 
    (name $Word))
  (= 
    (word $Word) 
    (terminator $Word $_))
  (= 
    (word $Word) 
    (pers-pron $Word $_ $_ $_ $_))
  (= 
    (word $Word) 
    (poss-pron $Word $_ $_ $_))
  (= 
    (word $Word) 
    (rel-pron $Word $_))
  (= 
    (word $Word) 
    (verb-form $Word $_ $_ $_))
  (= 
    (word $Word) 
    (noun-form $Word $_ $_))
  (= 
    (word $Word) 
    (prep $Word))
  (= 
    (word $Word) 
    (quantifier-pron $Word $_ $_))
  (= 
    (word $Word) 
    (number $Word $_ $_))
  (= 
    (word $Word) 
    (det $Word $_ $_ $_))
  (= 
    (word $Word) 
    (int-art $Word $_ $_ $_))
  (= 
    (word $Word) 
    (int-pron $Word $_))
  (= 
    (word $Word) 
    (loc-pred $Word $_))


  (= 
    (~  how) True)
  (= 
    (~  whose) True)
  (= 
    (~  there) True)
  (= 
    (~  of) True)
  (= 
    (~  ~) True)		;
;                                          use ~ instead of ' to help assembler

  (= 
    (~  ,) True)
  (= 
    (~  s) True)
  (= 
    (~  than) True)
  (= 
    (~  at) True)
  (= 
    (~  the) True)
  (= 
    (~  not) True)
  (= 
    (~  as) True)
  (= 
    (~  that) True)
  (= 
    (~  less) True)
  (= 
    (~  more) True)
  (= 
    (~  least) True)
  (= 
    (~  most) True)
  (= 
    (~  many) True)
  (= 
    (~  where) True)
  (= 
    (~  when) True)


  (= 
    (conj  and) True)
  (= 
    (conj  or) True)


  (= 
    (int_pron  what undef) True)
  (= 
    (int_pron  which undef) True)
  (= 
    (int_pron  who subj) True)
  (= 
    (int_pron  whom compl) True)


  (= 
    (int_art  what $X $_ 
      (int_det  $X)) True)
  (= 
    (int_art  which $X $_ 
      (int_det  $X)) True)


  (= 
    (det  the $No 
      (the  $No) def) True)
  (= 
    (det  a sin a indef) True)
  (= 
    (det  an sin a indef) True)
  (= 
    (det  every sin every indef) True)
  (= 
    (det  some $_ some indef) True)
  (= 
    (det  any $_ any indef) True)
  (= 
    (det  all plu all indef) True)
  (= 
    (det  each sin each indef) True)
  (= 
    (det  no $_ no indef) True)


  (= 
    (number $W $I $Nb) 
    ( (tr-number $W $I) (ag-number $I $Nb)))


  (= 
    (tr_number  
      (nb  $I) $I) True)
  (= 
    (tr_number  one 1) True)
  (= 
    (tr_number  two 2) True)
  (= 
    (tr_number  three 3) True)
  (= 
    (tr_number  four 4) True)
  (= 
    (tr_number  five 5) True)
  (= 
    (tr_number  six 6) True)
  (= 
    (tr_number  seven 7) True)
  (= 
    (tr_number  eight 8) True)
  (= 
    (tr_number  nine 9) True)
  (= 
    (tr_number  ten 10) True)


  (= 
    (ag_number  1 sin) True)
  (= 
    (ag-number $N plu) 
    (> $N 1))


  (= 
    (quantifier_pron  everybody every person) True)
  (= 
    (quantifier_pron  everyone every person) True)
  (= 
    (quantifier_pron  everything every thing) True)
  (= 
    (quantifier_pron  somebody some person) True)
  (= 
    (quantifier_pron  someone some person) True)
  (= 
    (quantifier_pron  something some thing) True)
  (= 
    (quantifier_pron  anybody any person) True)
  (= 
    (quantifier_pron  anyone any person) True)
  (= 
    (quantifier_pron  anything any thing) True)
  (= 
    (quantifier_pron  nobody no person) True)
  (= 
    (quantifier_pron  nothing no thing) True)


  (= 
    (prep  as) True)
  (= 
    (prep  at) True)
  (= 
    (prep  of) True)
  (= 
    (prep  to) True)
  (= 
    (prep  by) True)
  (= 
    (prep  with) True)
  (= 
    (prep  in) True)
  (= 
    (prep  on) True)
  (= 
    (prep  from) True)
  (= 
    (prep  into) True)
  (= 
    (prep  through) True)


  (= 
    (noun-form $Plu $Sin plu) 
    (noun-plu $Plu $Sin))
  (= 
    (noun-form $Sin $Sin sin) 
    (noun-sin $Sin))
  (= 
    (noun_form  proportion proportion $_) True)
  (= 
    (noun_form  percentage percentage $_) True)


  (= 
    (root_form  
      (+  1 sin)) True)
  (= 
    (root_form  
      (+  2 $_)) True)
  (= 
    (root_form  
      (+  1 plu)) True)
  (= 
    (root_form  
      (+  3 plu)) True)


  (= 
    (verb_root  be) True)
  (= 
    (verb_root  have) True)
  (= 
    (verb_root  do) True)
  (= 
    (verb_root  border) True)
  (= 
    (verb_root  contain) True)
  (= 
    (verb_root  drain) True)
  (= 
    (verb_root  exceed) True)
  (= 
    (verb_root  flow) True)
  (= 
    (verb_root  rise) True)


  (= 
    (regular_pres  have) True)
  (= 
    (regular_pres  do) True)
  (= 
    (regular_pres  rise) True)
  (= 
    (regular_pres  border) True)
  (= 
    (regular_pres  contain) True)
  (= 
    (regular_pres  drain) True)
  (= 
    (regular_pres  exceed) True)
  (= 
    (regular_pres  flow) True)


  (= 
    (regular_past  had have) True)
  (= 
    (regular_past  bordered border) True)
  (= 
    (regular_past  contained contain) True)
  (= 
    (regular_past  drained drain) True)
  (= 
    (regular_past  exceeded exceed) True)
  (= 
    (regular_past  flowed flow) True)


  (= 
    (rel_pron  who subj) True)
  (= 
    (rel_pron  whom compl) True)
  (= 
    (rel_pron  which undef) True)


  (= 
    (poss_pron  my $_ 1 sin) True)
  (= 
    (poss_pron  your $_ 2 $_) True)
  (= 
    (poss_pron  his masc 3 sin) True)
  (= 
    (poss_pron  her fem 3 sin) True)
  (= 
    (poss_pron  its neut 3 sin) True)
  (= 
    (poss_pron  our $_ 1 plu) True)
  (= 
    (poss_pron  their $_ 3 plu) True)


  (= 
    (pers_pron  i $_ 1 sin subj) True)
  (= 
    (pers_pron  you $_ 2 $_ $_) True)
  (= 
    (pers_pron  he masc 3 sin subj) True)
  (= 
    (pers_pron  she fem 3 sin subj) True)
  (= 
    (pers_pron  it neut 3 sin $_) True)
  (= 
    (pers_pron  we $_ 1 plu subj) True)
  (= 
    (pers_pron  them $_ 3 plu subj) True)
  (= 
    (pers_pron  me $_ 1 sin 
      (compl  $_)) True)
  (= 
    (pers_pron  him masc 3 sin 
      (compl  $_)) True)
  (= 
    (pers_pron  her fem 3 sin 
      (compl  $_)) True)
  (= 
    (pers_pron  us $_ 1 plu 
      (compl  $_)) True)
  (= 
    (pers_pron  them $_ 3 plu 
      (compl  $_)) True)


  (= 
    (terminator  . $_) True)
  (= 
    (terminator  ? ?) True)
  (= 
    (terminator  ! !) True)


  (= 
    (name  $_) True)

;
;                                          ===========================================================================


;
;                                          specialised dictionary



  (= 
    (loc_pred  east 
      (prep  eastof)) True)
  (= 
    (loc_pred  west 
      (prep  westof)) True)
  (= 
    (loc_pred  north 
      (prep  northof)) True)
  (= 
    (loc_pred  south 
      (prep  southof)) True)


  (= 
    (adj  minimum restr) True)
  (= 
    (adj  maximum restr) True)
  (= 
    (adj  average restr) True)
  (= 
    (adj  total restr) True)
  (= 
    (adj  african restr) True)
  (= 
    (adj  american restr) True)
  (= 
    (adj  asian restr) True)
  (= 
    (adj  european restr) True)
  (= 
    (adj  great quant) True)
  (= 
    (adj  big quant) True)
  (= 
    (adj  small quant) True)
  (= 
    (adj  large quant) True)
  (= 
    (adj  old quant) True)
  (= 
    (adj  new quant) True)
  (= 
    (adj  populous quant) True)


  (= 
    (rel_adj  greater great) True)
  (= 
    (rel_adj  less small) True)
  (= 
    (rel_adj  bigger big) True)
  (= 
    (rel_adj  smaller small) True)
  (= 
    (rel_adj  larger large) True)
  (= 
    (rel_adj  older old) True)
  (= 
    (rel_adj  newer new) True)


  (= 
    (sup_adj  biggest big) True)
  (= 
    (sup_adj  smallest small) True)
  (= 
    (sup_adj  largest large) True)
  (= 
    (sup_adj  oldest old) True)
  (= 
    (sup_adj  newest new) True)


  (= 
    (noun_sin  average) True)
  (= 
    (noun_sin  total) True)
  (= 
    (noun_sin  sum) True)
  (= 
    (noun_sin  degree) True)
  (= 
    (noun_sin  sqmile) True)
  (= 
    (noun_sin  ksqmile) True)
  (= 
    (noun_sin  thousand) True)
  (= 
    (noun_sin  million) True)
  (= 
    (noun_sin  time) True)
  (= 
    (noun_sin  place) True)
  (= 
    (noun_sin  area) True)
  (= 
    (noun_sin  capital) True)
  (= 
    (noun_sin  city) True)
  (= 
    (noun_sin  continent) True)
  (= 
    (noun_sin  country) True)
  (= 
    (noun_sin  latitude) True)
  (= 
    (noun_sin  longitude) True)
  (= 
    (noun_sin  ocean) True)
  (= 
    (noun_sin  person) True)
  (= 
    (noun_sin  population) True)
  (= 
    (noun_sin  region) True)
  (= 
    (noun_sin  river) True)
  (= 
    (noun_sin  sea) True)
  (= 
    (noun_sin  seamass) True)
  (= 
    (noun_sin  number) True)


  (= 
    (noun_plu  averages average) True)
  (= 
    (noun_plu  totals total) True)
  (= 
    (noun_plu  sums sum) True)
  (= 
    (noun_plu  degrees degree) True)
  (= 
    (noun_plu  sqmiles sqmile) True)
  (= 
    (noun_plu  ksqmiles ksqmile) True)
  (= 
    (noun_plu  million million) True)
  (= 
    (noun_plu  thousand thousand) True)
  (= 
    (noun_plu  times time) True)
  (= 
    (noun_plu  places place) True)
  (= 
    (noun_plu  areas area) True)
  (= 
    (noun_plu  capitals capital) True)
  (= 
    (noun_plu  cities city) True)
  (= 
    (noun_plu  continents continent) True)
  (= 
    (noun_plu  countries country) True)
  (= 
    (noun_plu  latitudes latitude) True)
  (= 
    (noun_plu  longitudes longitude) True)
  (= 
    (noun_plu  oceans ocean) True)
  (= 
    (noun_plu  persons person) True)  
  (= 
    (noun_plu  people person) True)
  (= 
    (noun_plu  populations population) True)
  (= 
    (noun_plu  regions region) True)
  (= 
    (noun_plu  rivers river) True)
  (= 
    (noun_plu  seas sea) True)
  (= 
    (noun_plu  seamasses seamass) True)
  (= 
    (noun_plu  numbers number) True)


  (= 
    (verb-form $V $V inf $_) 
    (verb-root $V))
  (= 
    (verb-form $V $V 
      (+ pres fin) $Agmt) 
    ( (regular-pres $V) 
      (root-form $Agmt) 
      (verb-root $V)))
  (= 
    (verb-form $Past $Root 
      (+ past $_) $_) 
    (regular-past $Past $Root))

  (= 
    (verb_form  am be 
      (+  pres fin) 
      (+  1 sin)) True)
  (= 
    (verb_form  are be 
      (+  pres fin) 
      (+  2 sin)) True)
  (= 
    (verb_form  is be 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  are be 
      (+  pres fin) 
      (+  $_ plu)) True)
  (= 
    (verb_form  was be 
      (+  past fin) 
      (+  1 sin)) True)
  (= 
    (verb_form  were be 
      (+  past fin) 
      (+  2 sin)) True)
  (= 
    (verb_form  was be 
      (+  past fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  were be 
      (+  past fin) 
      (+  $_ plu)) True)
  (= 
    (verb_form  been be 
      (+  past part) $_) True)
  (= 
    (verb_form  being be 
      (+  pres part) $_) True)
  (= 
    (verb_form  has have 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  having have 
      (+  pres part) $_) True)
  (= 
    (verb_form  does do 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  did do 
      (+  past fin) $_) True)
  (= 
    (verb_form  doing do 
      (+  pres part) $_) True)
  (= 
    (verb_form  done do 
      (+  past part) $_) True)
  (= 
    (verb_form  flows flow 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  flowing flow 
      (+  pres part) $_) True)
  (= 
    (verb_form  rises rise 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  rose rise 
      (+  past fin) $_) True)
  (= 
    (verb_form  risen rise 
      (+  past part) $_) True)
  (= 
    (verb_form  borders border 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  bordering border 
      (+  pres part) $_) True)
  (= 
    (verb_form  contains contain 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  containing contain 
      (+  pres part) $_) True)
  (= 
    (verb_form  drains drain 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  draining drain 
      (+  pres part) $_) True)
  (= 
    (verb_form  exceeds exceed 
      (+  pres fin) 
      (+  3 sin)) True)
  (= 
    (verb_form  exceeding exceed 
      (+  pres part) $_) True)


  (= 
    (verb_type  have 
      (+  aux have)) True)
  (= 
    (verb_type  be 
      (+  aux be)) True)
  (= 
    (verb_type  do 
      (+  aux ditrans)) True)
  (= 
    (verb_type  rise 
      (+  main intrans)) True)
  (= 
    (verb_type  border 
      (+  main trans)) True)
  (= 
    (verb_type  contain 
      (+  main trans)) True)
  (= 
    (verb_type  drain 
      (+  main intrans)) True)
  (= 
    (verb_type  exceed 
      (+  main trans)) True)
  (= 
    (verb_type  flow 
      (+  main intrans)) True)


  (= 
    (adverb  yesterday) True)
  (= 
    (adverb  tomorrow) True)

