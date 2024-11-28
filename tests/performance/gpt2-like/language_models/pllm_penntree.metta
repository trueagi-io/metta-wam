
  !(encoding iso-latin-1)
  !(module pllm Nil)

;
;                    :- include(weightless_pllm).



  (= 
    (pllm_preds  
      ( (/  training 3) 
        (/  is_word 1) 
        (/  is_word 2) 
        (/  ngram 5) 
        (/  ngram 6) 
        (/  trigram 3) 
        (/  trigram 4) 
        (/  tok_split 3) 
        (/  tok_split 4) 
        (:  tmp 
          (/  buffer_training 2)))) True)


  (= 
    (declare-preds $X) 
    ( (dynamic $X) (multifile $X)))


  !((pllm-preds $L) (maplist declare-preds $L))

;
;                       :- ensure_loaded(trains_trigrams).

  !(ensure-loaded utils-pllm)
  !(ensure-loaded (library logicmoo-nlu))
  !(ensure-loaded (library (/ logicmoo-nlu parser-link-grammar)))

;
;                         compile_corpus:- functor(P,ngram,6), predicate_property(P,number_of_clauses(N)),N>2.


  (= 
    (compile-corpus) 
    (compile-corpus-in-mem))


  (= 
    (recompile-corpus) 
    ( (pllm-preds $L) 
      (maplist abolish $L) 
      (maplist declare-preds $L) 
      (compile-corpus-in-mem)))


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
    (corpus_stat  corpus_convos) True)


  (= 
    (set-last-oc $OC) 
    (nb-setval last-oc $OC))

  (= 
    (get-last-oc $OC) 
    (nb-current last-oc $OC))

;
;                                 train_from_corpus:- training(_,string,_),!,forall(training(XX,string,Val),add_training_str(XX,Val)).


  (= 
    (train-from-corpus) 
    (train-from-corpus (library ../self-dialogue-corpus/train-from.txt)))

  (= 
    (train-from-corpus $Path) 
    ( (debugln (:: "reading corpus..." $Path)) (setup-call-cleanup (must (absolute-file-name $Path $File (:: (access read)))) (time (, (open $File read $In) (forall (corpus-stat $Stat) (set-flag $Stat 0)) (set-flag file-line 0) (repeat) (det-if-then-else (at-end-of-stream $In) (set-det) (, (inc-flag file-line) (read-line-to-string $In $Str) (get-flag file-line $X) (add-training $X $Str) (fail))) (forall (corpus-stat $Stat) (, (get-flag $Stat $Value) (debugln (= $Stat $Value)))))) save-training)))


  !(add-history load-training)

  (= 
    (load-training) 
    ( (pllm-preds $L) (maplist load-training $L)))
  (= 
    (load-training (/ $F $A)) 
    ( (atomic-list-concat 
        (:: done- $F - $A .pl) $File) (det-if-then-else (exists-file $File) (ensure-loaded $File) True)))


  (= 
    (save-training) 
    ( (pllm-preds $L) (maplist save-training $L)))
  (= 
    (save-training (/ $F $A)) 
    ( (atomic-list-concat 
        (:: done- $F - $A .pl) $File) 
      (tell $File) 
      (writeln !(encoding iso-latin-1)) 
      (listing (/ $F $A)) 
      (told)))
;                                       ; functor(P,F,A),forall(P,(writeq(P),writeln('.'))),





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
      (is $A2 
        (+ $A 1)) 
      (functor $NGram2 $F $A2) 
      (dynamic $NGram2) 
      (set-flag total-fa 0) 
      (set-flag min-fa 999999999) 
      (set-flag max-fa 0) 
      (forall $NGram 
        (, 
          (ngram-val $NGram $NN) 
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
      (maplist 
        (save-extents $F $A) $Props) 
      (debugln (:: (extent-props (/ $F $A)) $Props)) 
      (set-det)))
;                                         ; avoid division by zero

;                                         ; adds 20 seconds and is not yet used




  (= 
    (save-extents $_ $_ 
      (= $_ x)) 
    (set-det))
  (= 
    (save-extents $F $A 
      (= $X $Y)) 
    ( (set-det) (add-atom  &self (extent_props  $F $A $X $Y)))) 
  (= 
    (save-extents $_ $_ $_) 
    (set-det))


  (= 
    (ngram-val $NGram $NN) 
    ( (ngram-key $NGram $Key) (get-flag $Key $NN)))


  (= 
    (ngram-inc $NGram) 
    (ngram-inc $NGram $NN))
  (= 
    (ngram-inc $NGram $NN) 
    ( (ngram-key $NGram $Key) (flag $Key $NN (+ $NN 1))))


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
    ( (set-det) (join-text (:: $A $B $C) $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C $D $_) $Key) 
    ( (set-det) (ngram-key (ngram $Loc $A $B $C $D) $Key)))
  (= 
    (ngram-key 
      (ngram $Loc 
        (oc $_) $B $C 
        (oc $_)) $Key) 
    ( (set-det) (join-text (:: oc $B $C oc) $Key)))
  (= 
    (ngram-key 
      (ngram $Loc 
        (oc $_) $A $B $C) $Key) 
    ( (set-det) (join-text (:: oc $A $B $C) $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C 
        (oc $_)) $Key) 
    ( (set-det) (join-text (:: $A $B $C oc) $Key)))
  (= 
    (ngram-key 
      (ngram $Loc $A $B $C $D) $Key) 
    (join-text 
      (:: $A $B $C $D) $Key))


  (= 
    (join-text $List $Key) 
    (atomic-list-concat $List , $Key))


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
    (add-training $X $Str) 
    ( (flag speech-act $A 
        (+ $A 1)) 
      (get-flag corpus-convos $Z) 
      (is $XX 
        (+ 
          (+ 
            (* 
              (+ $Z 1) 100000000000) 
            (* $A 10000000)) $X)) 
      (add-training-str $XX $Str)))


  (= 
    (add-training-str $XX "XXXXXXXXXXX") 
    ( (= $C 100000000000) 
      (is $Buffer 
        (+ 
          (* 
            (floor (/ $XX $C)) $C) 9911111111111)) 
      (ignore (add-conversation-training $Buffer)) 
      (inc-flag corpus-convos) 
      (set-det) 
      (set-flag speech-act 1)))
  (= 
    (add-training-str $XX $Str) 
    ( (is 1 
        (mod $XX 2)) 
      (set-det) 
      (add-training-said said "Al" $XX $Str) 
      (set-det))) 
  (= 
    (add-training-str $XX $Str) 
    ( (add-training-said said "Jo" $XX $Str) (set-det))) 


  (= 
    (add-training-said $_ $_ $_ Nil) 
    (set-det))
  (= 
    (add-training-said $Says $PERSON $XX $Str) 
    ( (string $Str) 
      (tokenize-atom $Str $Toks) 
      (set-det) 
      (pretok $Toks $PreToks) 
      (add-training-said $Says $PERSON $XX $PreToks)))
  (= 
    (add-training-said $Says $PERSON $XX $Toks) 
    ( (append $Left 
        (:: .) $Toks) 
      (set-det) 
      (add-training-said $Says $PERSON $XX $Left)))
  (= 
    (add-training-said $Says $PERSON $XX $Toks) 
    ( (append $Left 
        (Cons  $LE $Right) $Toks) 
      (\== $Right Nil) 
      (member $LE 
        (:: . ?)) 
      (append $Left 
        (:: $LE) $Said) 
      (set-det) 
      (add-training-said $Says $PERSON $XX $Said) 
      (add-training-said $Says $PERSON $XX $Right)))
  (= 
    (add-training-said said $PERSON $XX $Toks) 
    ( (append $Left 
        (:: ?) $Toks) 
      (set-det) 
      (add-training-said asks $PERSON $XX $Left)))
  (= 
    (add-training-said $Says $PERSON $XX $Toks) 
    ( (det-if-then-else 
        (== $Says asks) 
        (= $J ?) 
        (= $J .)) 
      (atomics-to-string $Toks ' ' $Str) 
      (atomics-to-string 
        (:: $Str $J) '' $StrP) 
      (sformat $S " ~w ~w, ~q " 
        (:: $PERSON $Says $StrP)) 
      (= $BB 
        (with_self  
          (tmp) 
          (buffer-training $XX $S))) 
      (add-atom  &self $BB) 
      (wdmsg $BB)))

 

  (= 
    (assert-training $XX $P $Parse) 
    ( (assert-if-new (training $XX $P $Parse)) 
      (dmsg (training $XX $P $Parse)) 
      (save-training (/ training 3))))


  (= 
    (do-training $XX $Str $F2) 
    ( (training $XX $F2 $_) (set-det)))
  (= 
    (do-training $XX $Str $F2) 
    ( (catch 
        (call $F2 $Str $Result) $E 
        (, 
          (dumpSt) 
          (format '%~~~~~ ERROR: ~p~n' 
            (:: (--> $E (call $F2 $Str $Result))))) fail) 
      (set-det) 
      (assert-training $XX $F2 $Result) 
      (set-det)))



  (= 
    (add-conversation-training $XX) 
    ( (wots $Str 
        (forall 
          (remove-atom  &self 
            (:  tmp 
              (buffer_training  $_ $S))) 
          (, 
            (write  : ) 
            (writeln $S)))) 
      (assert-training $XX convo $Str) 
      (do-training $XX $Str text-to-best-tree)))



  (= 
    (all-letters $X) 
    (not (, (upcase-atom $X $U) (downcase-atom $X $U))))
 ;
;                                         tokenize_atom(Str,Toks),

 ;
;                                         maplist(downcase_atom,Toks,TokList),pretok(TokList,PreToks),!,

 ;
;                                         assert_training(XX,tokenize_atom,PreToks),




  (= 
    (add-training-toks $_ Nil) 
    (set-det))
  (= 
    (add-training-toks $X 
      (:: $A)) 
    ( (set-det) (add-training-toks $X (:: $A .))))
  (= 
    (add-training-toks $XX $PreToks) 
    ( (maplist 
        (add-occurs is-word) $PreToks) 
      (inc-flag corpus-training) 
      (add-ngrams except-symbols trigram 3 skip $PreToks) 
      (dbltok oc $PreToks $ReToks) 
      (set-det) 
      (is $XX1 
        (+ $XX 1)) 
      (append 
        (Cons  
          (oc $XX) $ReToks) 
        (:: (oc $XX1)) $Grams) 
      (set-det) 
      (add-ngrams except-none ngram 4 $XX $Grams)))


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
      (ngram-inc $W $X) 
      (det-if-then-else 
        (== $Loc skip) 
        (det-if-then-else 
          (not $W) 
          (add-atom  &self $W) True) 
        (add-atom  &self $W)) 
      (det-if-then-else 
        (= $X 0) 
        (inc-flag corpus-nodes) 
        (inc-flag corpus-node-overlap)) 
      (set-det)))


  (= 
    (add-occurs $F $Tok) 
    ( (=.. $P 
        (:: $F $Tok)) 
      (ignore (, (not $P) (add-atom  &self $P) (inc-flag corpus-unique-toks))) 
      (ngram-inc $P) 
      (inc-flag corpus-total-toks)))


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
        (Cons  - 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atomic-list-concat 
        (:: $A $S) - $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  ' 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (all-letters $A) 
      (all-letters $S) 
      (atomic-list-concat 
        (:: $A $S) ' $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  ´ 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (all-letters $A) 
      (all-letters $S) 
      (atomic-list-concat 
        (:: $A $S) ' $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  ` 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (all-letters $A) 
      (all-letters $S) 
      (atomic-list-concat 
        (:: $A $S) ' $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
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

;
;                                          dbltok(_,X,X):-!.


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
      (ngram-inc (tok-split $O $A $B))))
  (= 
    (atoms-join $A $B $O) 
    ( (atomic-list-concat 
        (:: $A $B) : $O) 
      (set-det) 
      (add-atom  &self 
        (tok_split  $O $A $B)) 
      (ngram-inc (tok-split $O $A $B))))

;
;                                          @TODO use average

;
;                                         as_good(T,X):- is_word(T,X),(Nxt>500->X=0;X is 500-Nxt).

;
;                                         ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).



  (= 
    (add-blanks $N $S $Slotted) 
    ( (not (is-list $S)) 
      (set-det) 
      (add-blanks $N 
        (:: $S) $Slotted)))
  (= 
    (add-blanks $_ Nil Nil) 
    (set-det))

  (= 
    (add-blanks $N 
      (Cons  $A 
        (Cons  $B $Sent)) 
      (Cons  $O $Slotted)) 
    ( (tok-split $O $A $B) 
      (set-det) 
      (add-blanks $N $Sent $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) 
      (Cons  $O $Slotted)) 
    ( (not (not (tok-split $_ $S $_))) 
      (set-det) 
      (tok-split $O $S $_) 
      (add-blanks $N $Sent $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $O $Sent) 
      (Cons  $O $Slotted)) 
    ( (atom $O) 
      (tok-split $O $_ $_) 
      (set-det) 
      (add-blanks $N $Sent $Slotted)))

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
      (Cons  $A $Slotted)) 
    ( (string $S) 
      (atom-string $A $S) 
      (set-det) 
      (add-blanks $N $Sent $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) $Slotted) 
    ( (var $S) 
      (set-det) 
      (between 1 $N $L) 
      (add-blanks $N 
        (Cons  
          (- 1 $L) $Sent) $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  
        (- $Lo $Hi) $Sent) $Slotted) 
    ( (or 
        (integer $Lo) 
        (integer $Hi)) 
      (set-det) 
      (between $Lo $Hi $L) 
      (length $S $L) 
      (add-blanks $N $Sent $Mid) 
      (append $S $Mid $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) $Slotted) 
    ( (is-list $S) 
      (set-det) 
      (flatten $S $SL) 
      (append $SL $Sent $SLSent) 
      (set-det) 
      (add-blanks $N $SLSent $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) $Slotted) 
    ( (atom $S) 
      (into-mw $S $SL) 
      (set-det) 
      (append $SL $Sent $SLSent) 
      (set-det) 
      (add-blanks $N $SLSent $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) 
      (Cons  $S $Slotted)) 
    (add-blanks $N $Sent $Slotted))


  (= 
    (into-mw $S $SL) 
    ( (into-mw0 $S $SL) 
      (\== $SL 
        (:: $S)) 
      (set-det)))

  (= 
    (into-mw0 $S $SL) 
    ( (atomic-list-concat 
        (Cons  $M 
          (Cons  $_ $_)) : $S) 
      (set-det) 
      (into-mw0 $M $SL)))
  (= 
    (into-mw0 $S $SL) 
    (atomic-list-concat $SL , $S))
  (= 
    (into-mw0 $S $SL) 
    (atomic-list-concat $SL ' ' $S))
  (= 
    (into-mw0 $S $SL) 
    (atomic-list-concat $SL - $S))


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


  (= 
    (autoc $Sent) 
    (autoc 1 $Sent))
  (= 
    (autoc $N $Sent) 
    ( (remove-all-atoms  &self 
        (used_cl  
          (ngram  $_ $_ $_ $_))) 
      (add-blanks $N $Sent $Slotted) 
      (no-repeats (map-sent $_ $Loc $Slotted)) 
      (fmt-pllm $Slotted)))


  (= 
    (good-toks $Key $E) 
    ( (functor $P ngram 6) 
      (arg 6 $P $E) 
      (no-repeats $Key 
        (, $P 
          (ngram-key $P $Key)))))



  !(add-history recompile-corpus)

  !fixup-exports

  !(dynamic (/ used-cl 1))


  (= 
    (map-sent $_ $_ $Sent) 
    ( (ground $Sent) (set-det)))
  (= 
    (map-sent $LR $Loc $Sent) 
    ( (var $Sent) 
      (length $Sent 9) 
      (map-sent $LR $Loc $Sent)))
  (= 
    (map-sent $LR $Loc $List) 
    ( (= $LR lr) 
      (append $Left 
        (Cons  $X $More) $List) 
      (nonvar $X) 
      (\== $Left Nil) 
      (set-det) 
      (map-sent $LR $Loc 
        (Cons  $X $More)) 
      (map-sent rl $Loc $List)))
  (= 
    (map-sent $LR $Loc 
      (Cons  $A 
        (Cons  $B 
          (Cons  $C 
            (Cons  $D $More))))) 
    ( (some-ngram $Loc $A $B $C $D $Fire) (map-sent $LR $Loc (Cons  $C (Cons  $D $More)))))
  (= 
    (map-sent $LR $Loc 
      (Cons  $A 
        (Cons  $B 
          (Cons  $C 
            (Cons  $D $More))))) 
    ( (some-ngram $Loc $A $B $C $_ $Fire) (map-sent $LR $Loc (Cons  $B (Cons  $C (Cons  $D $More))))))
  (= 
    (map-sent $_ $Loc $List) 
    ( (= $ABCDO 
        (:: $_ $_ $_ $_ $Occurs)) 
      (append $List $_ $ABCDO) 
      (apply some-ngram 
        (Cons  $Loc $ABCDO))))



  (= 
    (some-ngram $PrevLoc $A $B $C $D $N) 
    ( (pick-ngram $Loc $A $B $C $D $N) (may-use $Loc $A $B $C $D $N)))


  (= 
    (pick-ngram $Loc $A $B $C $D $N) 
    (det-if-then-else 
      (maplist var 
        (:: $A $B $C $D)) 
      (rnd-ngram $Loc $A $B $C $D $N) 
      (ngram $Loc $A $B $C $D $N)))


  (= 
    (rnd-ngram $Loc $A $B $C $D $N) 
    ( (= $G 
        (ngram $Loc $A $B $C $D $N)) 
      (predicate-property $G 
        (number-of-clauses $R)) 
      (is $CN 
        (+ 
          (random $R) 1)) 
      (nth-clause $G $CN $Ref) 
      (clause $G $Body $Ref) $Body))



  !(style-check (- singleton))

  !(add-history (, (good-toks $Key $E) (> $E 20)))
  !(add-history (autoc (:: music:you (len 200))))
  !(add-history (autoc (:: oc music:you (len 200))))
  !(add-history (autoc (:: oc:music music:you (len 200))))
  !(add-history (autoc (:: music (len 200))))
  !(add-history (autoc (:: (len 10) music (len 200))))


  (= 
    (may-use $Loc $_ $B $C $D $_) 
    ( (not (used-cl (ngram $A $B $C $D))) 
      (assert 
        (used-cl (ngram $A $B $C $D)) $Cl2) 
      (undo (erase $Cl2)) 
      (set-det)))



  (= 
    (gen6 (= (:: $A $B $C $D $E $F $G $H) $N)) 
    ( (ngram $Loc1 $E $F $G $H $Z) 
      (ngram $Loc2 $C $D $E $F $Y) 
      (ngram $Loc3 $A $B $C $D $X) 
      (is $N 
        (+ 
          (+ $X $Y) $Z))))

  !fixup-exports

  !(if (not (prolog-load-context reloading True)))
  !load-training
  !compile-corpus
  !endif



