  !(encoding iso-latin-1)
  !(module pllm Nil)

;
;                    :- include(weightless_pllm).


  !((= $X 
      (, 
        (/ is-word 1) 
        (/ is-word 2) 
        (/ ngram 5) 
        (/ ngram 6) 
        (/ trigram 3) 
        (/ trigram 4) 
        (/ tok-split 3) 
        (/ tok-split 4))) 
    (dynamic $X) 
    (multifile $X))

  !(ensure-loaded trains-trigrams)
  !(ensure-loaded utils-pllm)


  (= 
    (compile-corpus) 
    (compile-corpus-in-mem))


  (= 
    (compile-corpus-in-mem) 
    ( (train-from-corpus) 
      (compute-corpus-extents) 
      (nop retrain-from-trigrams) 
      (set-det)))


  (= 
    (corpus_stat  corpus_training) True) 
  (= 
    (corpus_stat  corpus_nodes) True) 
  (= 
    (corpus_stat  corpus_node_overlap) True)
  (= 
    (corpus_stat  corpus_unique_toks) True) 
  (= 
    (corpus_stat  corpus_total_toks) True) 
  (= 
    (corpus_stat  sent_num) True) 
  (= 
    (corpus_stat  corpus_convos) True)


  (= 
    (set-last-oc $OC) 
    (nb-setval last-oc $OC))

  (= 
    (get-last-oc $OC) 
    (nb-current last-oc $OC))


  (= 
    (train-from-corpus) 
    ( (debugln "reading corpus...") 
      (set-last-oc (with_self  (with_self  0 0) 0)) 
      (absolute-file-name 
        (pldata corpus/self-dialogue-corpus/train-from-topic-star-wars.txt) $File 
        (:: (access read))) 
      (time (, (open $File read $In) (forall (corpus-stat $Stat) (set-flag $Stat 0)) (set-flag sent-num 0) (repeat) (det-if-then-else (at-end-of-stream $In) (set-det) (, (flag sent-num $X (+ $X 1)) (read-line-to-string $In $Str) (add-training $X $Str) (fail))) (forall (corpus-stat $Stat) (, (get-flag $Stat $Value) (debugln (= $Stat $Value))))))))



  (= 
    (retrain-from-trigrams) 
    ( (debugln "retrain from trigrams...") 
      (absolute-file-name 
        (library ../self-dialogue-corpus/train-from.txt) $File 
        (:: (access read))) 
      (time (, (open $File read $In) (forall (corpus-stat $Stat) (set-flag $Stat 0)) (set-flag sent-num 0) (repeat) (det-if-then-else (at-end-of-stream $In) (set-det) (, (flag sent-num $X (+ $X 1)) (read-line-to-string $In $Str) (add-training $X $Str) (fail))) (forall (corpus-stat $Stat) (, (get-flag $Stat $Value) (debugln (= $Stat $Value))))))))



  (= 
    (save-stat $G) 
    ( (det-if-then-else 
        (not $G) 
        (add-atom  &self $G) True) (nop (, (writeq $G) (writeln .)))))


  (= 
    (use_extent  is_word 1) True) 
  (= 
    (use_extent  tok_split 3) True) 
  (= 
    (use_extent  trigram 3) True) 
  (= 
    (use_extent  ngram 5) True)

  (= 
    (compute-corpus-extents) 
    ( (debugln "compute corpus extents...") (time (forall (use-extent $F $A) (compute-extent $F $A)))))



  (= 
    (min-of $X $Y $X) 
    ( (< $X $Y) (set-det))) 
  (= 
    (min_of  $_ $Y $Y) True)

  (= 
    (max-of $X $Y $X) 
    ( (> $X $Y) (set-det))) 
  (= 
    (max_of  $_ $Y $Y) True)

  (= 
    (inc-flag $F) 
    (flag $F $X 
      (+ $X 1)))

  (= 
    (compute-extent $F $A) 
    ( (functor $NGram $F $A) 
      (set-flag total-fa 0) 
      (set-flag min-fa 999999999) 
      (set-flag max-fa 0) 
      (forall $NGram 
        (, 
          (ngram-key $NGram $X) 
          (get-flag $X $NN) 
          (flag total-fa $Total 
            (+ $Total $NN)) 
          (get-flag min-fa $Min) 
          (min-of $Min $NN $NewMin) 
          (set-flag min-fa $NewMin) 
          (get-flag max-fa $Max) 
          (max-of $Max $NN $NewMax) 
          (set-flag max-fa $NewMax) 
          (append-term $NGram $NN $NGramStat) 
          (save-stat $NGramStat))) 
      (get-flag total-fa $Total) 
      (get-flag min-fa $Min) 
      (get-flag max-fa $Max) 
      (predicate-property $NGram 
        (number-of-clauses $Insts)) 
      (max-of $Insts 1 $Insts1) 
      (is $Mean 
        (round (/ $Total $Insts1))) 
      (is $High 
        (+ 
          (/ 
            (- $Max $Mean) 2) $Mean)) 
      (is $Low 
        (+ 
          (/ 
            (- $Mean $Min) 2) $Min)) 
      (set-flag med-high-fa $High) 
      (set-flag med-low-fa $Low) 
      (nop (, (set-flag above-mean-fa 0) (set-flag above-med-high-fa 0) (set-flag num-min-fa 0) (set-flag below-mean-fa 0) (set-flag below-med-low-fa 0) (append-term $NGram $NN $NGramStatN) (forall $NGramStatN (, (ignore (, (= $NN $Min) (inc-flag num-min-fa))) (ignore (, (> $NN $High) (inc-flag above-med-high-fa))) (ignore (, (< $NN $Low) (inc-flag below-med-low-fa))) (det-if-then-else (=< $NN $Mean) (inc-flag below-mean-fa) (inc-flag above-mean-fa)))) (get-flag num-min-fa $NEMin) (get-flag above-med-high-fa $NAMedHi) (get-flag below-mean-fa $NBMean) (get-flag above-mean-fa $NAMean) (get-flag below-med-low-fa $NBMedLo) (is $NAMeanNAMedHi (- $NAMean $NAMedHi)) (is $NBMeanNBMedLo (- $NBMean $NBMedLo)) (is $NBMedLoNEMin (- $NBMedLo $NEMin)) (set-det))) 
      (= $Props 
        (:: 
          (= 
            (det-if-then min min) $NEMin) 
          (= 
            (det-if-then min low) $NBMedLoNEMin) 
          (= 
            (det-if-then low mean) $NBMeanNBMedLo) 
          (= 
            (det-if-then mean high) $NAMeanNAMedHi) 
          (= 
            (det-if-then high max) $NAMedHi) 
          (= --------- ------------) 
          (= 
            (det-if-then min max) $Insts) nl 
          (= min $Min) 
          (= low $Low) 
          (= mean $Mean) 
          (= high $High) 
          (= max $Max) 
          (= total $Total))) 
      (maplist xthe-unbounds $Props) 
      (add-atom  &self 
        (extent_props  $F $A $Props)) 
      (debugln (:: (extent-props (/ $F $A)) $Props)) 
      (set-det)))
;                                         ; avoid division by zero

;                                         ; adds 20 seconds and is not yet used




  (= 
    (xthe-unbounds $X) 
    (ignore (= $X (= $_ x))))


  (= 
    (ngram-key 
      (tok-split $O $_ $_) $O) 
    (set-det))
  (= 
    (ngram-key 
      (is-word $O) $O) 
    (set-det))
  (= 
    (ngram-key 
      (trigram $A $B $C) $Key) 
    ( (set-det) (atomic-list-concat (:: $A $B $C) , $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C $D $_) $Key) 
    ( (set-det) (ngram-key (ngram $Loc $A $B $C $D) $Key)))
  (= 
    (ngram-key 
      (ngram $Loc 
        (oc $_) $B $C 
        (oc $_)) $Key) 
    ( (set-det) (atomic-list-concat (:: oc $B $C oc) , $Key)))
  (= 
    (ngram-key 
      (ngram $Loc 
        (oc $_) $A $B $C) $Key) 
    ( (set-det) (atomic-list-concat (:: oc $A $B $C) , $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C 
        (oc $_)) $Key) 
    ( (set-det) (atomic-list-concat (:: $A $B $C oc) , $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C $D) $Key) 
    (atomic-list-concat 
      (:: $A $B $C $D) , $Key))


  (= 
    (save-corpus-stats) 
    (time (, (tell plm.pl) (write '
 :- style-check(- discontiguous).
 :- X= (is-word/2,ngram/6),
    dynamic(X),multifile(X). 
') (listing (:: (/ is-word 2) (/ ngram 6))) (told))))


  (= 
    (qcompile-corpus) 
    ( (save-corpus-stats) 
      (debugln "Compiling now...") 
      (time (with_self  (pllm) (qcompile plm))) 
      (debugln "Loading now...") 
      (time (with_self  (pllm) (ensure-loaded plm))) 
      (debugln "Corpus Ready")))


  (= 
    (add-training $_ "XXXXXXXXXXX") 
    ( (flag corpus-convos $Z 
        (+ $Z 1)) 
      (is $Z10 
        (div $Z 10)) 
      (set-last-oc $Z) 
      (set-det) 
      (is $Y 
        (* 
          (+ $Z10 1) 100)) 
      (flag sent-num $_ $Y) 
      (set-det)))
;                                         ; every 10 conversations will be considered "close"


  (= 
    (add-training $X $Str) 
    ( (tokenize-atom $Str $Toks) 
      (maplist downcase-symbol $Toks $TokList) 
      (pretok $TokList $PreToks) 
      (set-det) 
      (maplist 
        (add-occurs is-word) $PreToks) 
      (add-training-toks $X $PreToks)))


  (= 
    (add-training-toks $_ Nil) 
    (set-det))
  (= 
    (add-training-toks $X 
      (:: $A)) 
    ( (set-det) (add-training-toks $X (:: $A .))))
  (= 
    (add-training-toks $X $PreToks) 
    ( (add-ngrams except-symbols trigram 3 skip $PreToks) 
      (dbltok oc $PreToks $ReToks) 
      (set-det) 
      (get-last-oc $LastOC) 
      (is $NewOC 
        (+ $X 1)) 
      (flag corpus-training $T 
        (+ $T 1)) 
      (set-last-oc $NewOC) 
      (append 
        (Cons  
          (oc $LastOC) $ReToks) 
        (:: (oc $NewOC)) $Grams) 
      (set-det) 
      (add-ngrams except-none ngram 4 $X $Grams)))
;                                         ;get_flag(corpus_convos,B),;source_location(_,L),




  (= 
    (add-ngrams $Except $F $N $Loc $Grams) 
    ( (length $NGram $N) 
      (append $NGram $_ $Mid) 
      (forall 
        (append $_ $Mid $Grams) 
        (assert-ngram $Except $F $Loc $NGram))))


  (= 
    (except_none  $_) True)

  (= 
    (assert-ngram $Except $F $Loc $List) 
    ( (or 
        (== $Except except-none) 
        (maplist $Except $List)) 
      (set-det) 
      (det-if-then-else 
        (== $Loc skip) 
        (=.. $W 
          (Cons  $F $List)) 
        (=.. $W 
          (Cons  $F 
            (Cons  $Loc $List)))) 
      (ngram-key $W $A) 
      (flag $A $X 
        (+ $X 1)) 
      (det-if-then-else 
        (== $Loc skip) 
        (det-if-then-else 
          (not $W) 
          (add-atom  &self $W) True) 
        (add-atom  &self $W)) 
      (det-if-then-else 
        (= $X 0) 
        (flag corpus-nodes $N 
          (+ $N 1)) 
        (flag corpus-node-overlap $O 
          (+ $O 1))) 
      (set-det)))


  (= 
    (add-occurs $F $Tok) 
    ( (=.. $P 
        (:: $F $Tok)) 
      (ignore (, (not $P) (add-atom  &self $P) (flag corpus-unique-toks $O (+ $O 1)))) 
      (flag $Tok $X 
        (+ $X 1)) 
      (flag corpus-total-toks $T 
        (+ $T 1))))


  (= 
    (except-symbols $X) 
    (not (, (upcase-atom $X $U) (downcase-atom $X $U))))


  (= 
    (pretok  () ()) True)
  (= 
    (pretok 
      (:: .) Nil) 
    (set-det))
  (= 
    (pretok 
      (Cons  $X 
        (Cons  $X 
          (Cons  $X $Nxt))) $O) 
    ( (set-det) 
      (atomic-list-concat 
        (:: $X $X $X) , $Y) 
      (pretok 
        (Cons  $Y $Nxt) $O)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  ' 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atom-concat $A $S $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  ´ 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atom-concat $A $S $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  ` 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atom-concat $A $S $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
  (= 
    (pretok 
      (Cons  , $Grams) $ReTok) 
    (pretok $Grams $ReTok))
  (= 
    (pretok 
      (Cons  - $Grams) $ReTok) 
    (pretok $Grams $ReTok))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  $B 
          (Cons  $C $Grams))) $ReTok) 
    ( (trigram $A $B $C $N) 
      (> $N 40) 
      (set-det) 
      (ngram-key 
        (trigram $A $B $C) $Key) 
      (pretok 
        (Cons  $Key $Grams) $ReTok)))
  (= 
    (pretok 
      (Cons  
        (set-det) $Grams) $ReTok) 
    (pretok 
      (Cons  . $Grams) $ReTok))
  (= 
    (pretok 
      (Cons  $S $Grams) 
      (Cons  $S $ReTok)) 
    (pretok $Grams $ReTok))


  (= 
    (dbltok oc Nil Nil) 
    (set-det))
  (= 
    (dbltok $Pre Nil 
      (:: $PS)) 
    ( (set-det) (atoms-join $Pre oc $PS)))
  (= 
    (dbltok $Pre 
      (Cons  $S $Grams) 
      (Cons  $PS $ReTok)) 
    ( (atoms-join $Pre $S $PS) (dbltok $S $Grams $ReTok)))


  (= 
    (atoms-join $A $B $O) 
    ( (tok-split $O $A $B) 
      (set-det) 
      (flag $O $X 
        (+ $X 1))))
  (= 
    (atoms-join $A $B $O) 
    ( (atomic-list-concat 
        (:: $A $B) : $O) 
      (set-det) 
      (add-atom  &self 
        (tok_split  $O $A $B)) 
      (flag $O $X 
        (+ $X 1))))

;
;                                          @TODO use average

;
;                                         as_good(T,X):- is_word(T,X),(Nxt>500->X=0;X is 500-Nxt).

;
;                                         ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).



  (= 
    (autoc $Sent) 
    (autoc 1 $Sent))
  (= 
    (autoc $N $Sent) 
    ( (remove-all-atoms  &self 
        (used_cl  
          (ngram  $_ $_ $_ $_))) 
      (add-blanks $N $Sent $Slotted) 
      (no-repeats (map-sent $Loc $Slotted)) 
      (debugln $Slotted)))


  (= 
    (add-blanks $_ Nil Nil) 
    (set-det))
  (= 
    (add-blanks $N 
      (Cons  
        (len $S) $Sent) $Slotted) 
    ( (integer $S) 
      (length $L $S) 
      (set-det) 
      (add-blanks $N $Sent $Mid) 
      (append $L $Mid $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) 
      (Cons  $S $Slotted)) 
    (add-blanks $N $Sent $Slotted))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) $Slotted) 
    ( (var $S) 
      (between 2 $N $L) 
      (length $S $L) 
      (add-blanks $N $Sent $Mid) 
      (append $S $Mid $Slotted)))


  (= 
    (loc-dists $Loc1 $Loc2 $NN) 
    (is $NN 
      (abs (- $Loc1 $Loc2))))
  (= 
    (loc-dists $Loc1 $Loc2 $Loc3 $NN) 
    (is $NN 
      (/ 
        (+ 
          (+ 
            (abs (- $Loc1 $Loc2)) 
            (abs (- $Loc3 $Loc2))) 
          (abs (- $Loc1 $Loc3))) 3)))

;
;                                         :- pllm:ensure_loaded(plm).

;
;                                          added for conversations


  (= 
    (ngram $Loc $A 
      (oc $X) $B $C $NN) 
    ( (nonvar $X) 
      (ngram $Loc $_ $_ $A 
        (oc $X) $_) 
      (ngram $ULoc 
        (oc $X) $B $C $_ $NN)))
  (= 
    (ngram $Loc $A $B 
      (oc $X) $C $NN) 
    ( (nonvar $X) 
      (ngram $Loc $_ $A $B 
        (oc $X) $_) 
      (ngram $ULoc 
        (oc $X) $C $_ $_ $NN)))


  !(add-history compile-corpus)


  (= 
    (good-toks $Key $E) 
    ( (functor $P ngram 6) 
      (arg 6 $P $E) 
      (no-repeats $Key 
        (, $P 
          (ngram-key $P $Key)))))


  !(if (not (prolog-load-context reloading True)))
  !compile-corpus
  !endif

  !fixup-exports

