

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
        (/ tok-split 3))) 
    (dynamic $X) 
    (multifile $X))

  !(ensure-loaded utils-pllm)



  (= 
    (compile-corpus) 
    (compile-corpus-in-mem))


  (= 
    (compile-corpus-in-mem) 
    ( (train-from-corpus) 
      (compute-corpus-extents) 
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
    (train-from-corpus) 
    ( (debugln "reading corpus...") 
      (absolute-file-name 
        (pldata corpus/self-dialogue-corpus/train-from.txt) $File 
        (:: (access read))) 
      (time (, (open $File read $In) (forall (corpus-stat $Stat) (set-flag $Stat 0)) (set-flag sent-num 0) (repeat) (det-if-then-else (at-end-of-stream $In) (set-det) (, (flag sent-num $X (+ $X 1)) (read-line-to-string $In $Str) (add-training $X $Str) (fail))) (forall (corpus-stat $Stat) (, (get-flag $Stat $Value) (debugln (= $Stat $Value))))))))
;                             ;absolute_file_name(library('../ext/self_dialogue_corpus/train_from.txt'),File,[access(read)]),




  (= 
    (save-stat $G) 
    ( (add-atom  &self $G) (nop (, (writeq $G) (writeln .)))))


  (= 
    (use_extent  is_word 1) True) 
  (= 
    (use_extent  tok_split 3) True) 
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
      (is $Avrg 
        (/ $Total 
          (+ $Insts 1))) 
      (= $Props 
        (:: 
          (= insts $Insts) 
          (= total $Total) 
          (= min $Min) 
          (= max $Max) 
          (= avrg $Avrg))) 
      (add-atom  &self 
        (extent_props  $F $A $Props)) 
      (debugln (:: (extent-props (/ $F $A)) $Props)) 
      (set-det)))



  (= 
    (ngram_key  
      (tok_split  $O $_ $_) $O) True)
  (= 
    (ngram_key  
      (is_word  $O) $O) True)
  (= 
    (ngram-key $P $K) 
    (ngram-key $P $_ $K))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C $D $_) $T $Key) 
    ( (set-det) (ngram-key (ngram $Loc $A $B $C $D) $T $Key)))
  (= 
    (ngram-key 
      (ngram $Loc 
        (oc $_) $A $B $C) $T $Key) 
    ( (= $T 
        (sgram $A $B $C)) 
      (set-det) 
      (term-to-atom $T $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C 
        (oc $_)) $T $Key) 
    ( (= $T 
        (sgram $A $B $C)) 
      (set-det) 
      (term-to-atom $T $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C $D) 
      (:: (trigram $A $B $C $D)) $Key) 
    ( (set-det) (term-to-atom (:: $A $B $C) $Key)))
  (= 
    (ngram-key 
      (trigram $A $B $C) Nil $Key) 
    (term-to-atom 
      (:: $A $B $C) $Key))



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
      (is $Y 
        (* 
          (+ $Z10 1) 100)) 
      (flag sent-num $_ $Y) 
      (set-det)))
;                                         ; every 10 conversations will be considered "close"


  (= 
    (add-training $X $Str) 
    ( (flag sent-num $Y $Y) 
      (tokenize-atom $Str $Toks) 
      (maplist downcase-symbol $Toks $TokList) 
      (pretok $TokList $PreToks) 
      (set-det) 
      (maplist 
        (add-occurs is-word) $PreToks) 
      (dbltok oc $PreToks $ReToks) 
      (set-det) 
      (append 
        (Cons  
          (oc $X) $ReToks) 
        (:: (oc $Y)) $Grams) 
      (set-det) 
      (flag corpus-training $T 
        (+ $T 1)) 
      (add-ngrams 4 $X $Grams)))


  (= 
    (add-ngrams $N $Loc $Grams) 
    ( (length $NGram $N) 
      (append $NGram $_ $Mid) 
      (forall 
        (append $_ $Mid $Grams) 
        (assert-ngram ngram $Loc $NGram))))


  (= 
    (assert-ngram $P $Loc $List) 
    ( (=.. $W 
        (Cons  $P 
          (Cons  $Loc $List))) 
      (ngram-key $W $A) 
      (flag $A $X 
        (+ $X 1)) 
      (assert-if-new $W) 
      (det-if-then-else 
        (= $X 0) 
        (flag corpus-nodes $N 
          (+ $N 1)) 
        (flag corpus-node-overlap $O 
          (+ $O 1)))))


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
    (pretok  () ()) True)
  (= 
    (pretok 
      (:: .) Nil) 
    (set-det))
  (= 
    (pretok 
      (:: (set-det)) Nil) 
    (set-det))
  (= 
    (pretok 
      (Cons  $X 
        (Cons  $X 
          (Cons  $X $Nxt))) $O) 
    ( (set-det) 
      (atomic-list-concat 
        (:: $X $X $X) ' ' $Y) 
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
      (Cons  , $Grams) $ReTok) 
    (pretok $Grams $ReTok))
  (= 
    (pretok 
      (Cons  $S $Grams) 
      (Cons  $S $ReTok)) 
    (pretok $Grams $ReTok))


  (= 
    (dbltok oc $X $X) 
    (set-det))
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
    ( (add-blanks $N $Sent $Slotted) 
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

