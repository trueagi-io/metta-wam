
  !(encoding iso-latin-1)
  !(module pllm Nil)
  !(encoding iso-latin-1)

;
;                     :- include(weightless_pllm).



  (= 
    (pllm_preds  
      ( (/  training 4) 
        (/  training 3) 
        (/  np_save 3) 
        (/  is_word 2) 
        (/  ngram 5) 
        (/  ngram 6) 
        (/  ngram 7) 
        (/  ngram 8) 
        (/  ngram 9) 
        (/  trigram 3) 
        (/  trigram 4) 
        (/  tok_split 3) 
        (/  tok_split 4))) True)


  (= 
    (declare-preds $X) 
    ( (dynamic $X) (multifile $X)))


  !((pllm-preds $L) (maplist declare-preds $L))

;
;                         :- ensure_loaded(trains_trigrams).

  !(use-module (library logicmoo-utils))

;
;                          debug printing


  (= 
    (debugln $X) 
    ( (debugln-xfrm $X $S) (dmsg $S)))

  (= 
    (fmt-pllm $X) 
    ( (debugln-xfrm $X $S) (fmt $S)))


  (= 
    (debugln-xfrm $Insts $S) 
    ( (var $Insts) 
      (set-det) 
      (sformat $S "~p" 
        (:: $Insts))))
  (= 
    (debugln-xfrm 
      (i $X) $S) 
    (det-if-then-else 
      (not (is-list $X)) 
      (debugln-xfrm $X $S) 
      (, 
        (maplist debugln-xfrm $X $Y) 
        (atomics-to-string $Y ' ' $S))))
  (= 
    (debugln-xfrm 
      (Cons  $N $A) $S) 
    ( (is-list $A) 
      (set-det) 
      (maplist debugln-xfrm 
        (Cons  $N $A) $Y) 
      (atomics-to-string $Y ' ' $S)))
  (= 
    (debugln-xfrm 
      (/ $F $A) $S) 
    ( (functor $P $F $A) 
      (predicate-property $P 
        (number-of-clauses $Insts)) 
      (set-det) 
      (sformat $S "~w=~:d~n" 
        (:: 
          (/ $F $A) $Insts))))
  (= 
    (debugln-xfrm 
      (w $E) $S) 
    ( (sformat $S ~p $E) (set-det)))
  (= 
    (debugln-xfrm 
      ($ $E) $S) 
    ( (get-flag $E $Insts) 
      (set-det) 
      (sformat $S "~w=~:d~n" 
        (:: $E $Insts))))
  (= 
    (debugln-xfrm 
      (= $N $V) $S) 
    ( (integer $V) 
      (set-det) 
      (sformat $S "~n\t~w\t= ~:d " 
        (:: $N $V))))
  (= 
    (debugln-xfrm 
      (= $N $V) $S) 
    ( (set-det) (sformat $S "~n\t~w\t= ~w " (:: $N $V))))
  (= 
    (debugln-xfrm 
      (:: $N) $S) 
    ( (set-det) (debugln-xfrm $N $S)))
  (= 
    (debugln-xfrm $C $S) 
    ( (if-defined (tok-split $_ $C $S $_)) (set-det)))
  (= 
    (debugln-xfrm $C $S) 
    ( (if-defined (tok-split $C $S $_)) (set-det)))
  (= 
    (debugln-xfrm $C $S) 
    ( (compound $C) 
      (set-det) 
      (sformat $S "~p" 
        (:: $C))))
;
;                                  debugln_xfrm(C,S):- compound(C),compound_name_arguments(C,N,A),debugln_xfrm([N|A],S).

  (= 
    (debugln-xfrm nl 
)   (set-det))
  (= 
    (debugln-xfrm Nil '') 
    (set-det))
  (= 
    (debugln_xfrm  $E $E) True)


  !(ensure-loaded (library logicmoo-nlu))
  !(ensure-loaded (library (/ logicmoo-nlu parser-link-grammar)))

;
;                                      compile_corpus:- functor(P,ngram,6), predicate_property(P,number_of_clauses(N)),N>2.


  (= 
    (compile-corpus) 
    ( (mmake) (compile-corpus-in-mem)))


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
;                                          train_from_corpus:- training(_,string,_),!,forall(training(XX,string,Val),add_training_str(XX,Val)).

;
;                                         train_from_corpus:- train_from_corpus(pldata('corpus/self_dialogue_corpus/train_from.txt')).


  (= 
    (train-from-corpus) 
    (train-from-corpus (pldata corpus/self-dialogue-corpus/train-from-topic-harry-potter.txt)))
;
;                                         train_from_corpus:- train_from_corpus(pldata('corpus/self_dialogue_corpus/train_from_topic_star_wars.txt')).



  (= 
    (in-temp-dir $G) 
    ( (must (absolute-file-name (pldata corpus/tmpdata) $Dir (:: (access read) (file-type directory)))) (setup-call-cleanup (working-directory $X $Dir) (must-or-rtrace $G) (working-directory $_ $X))))


  (= 
    (train-from-corpus $Path) 
    ( (must (absolute-file-name $Path $File (:: (access read)))) 
      (\== $Path $File) 
      (set-det) 
      (train-from-corpus $File)))

  (= 
    (train-from-corpus $File) 
    ( (atom-concat $File .pl $CFile) (train-from-corpus $File $CFile)))


  (= 
    (exists-file-time $File $Time) 
    ( (exists-file $File) 
      (set-det) 
      (time-file $File $Time)))
  (= 
    (exists-file-time $_ $Time) 
    (is $Time -1))


  (= 
    (train-from-corpus $File $CFile) 
    ( (fail) 
      (exists-file-time $CFile $C) 
      (exists-file-time $File $T) 
      (> $C $T) 
      (set-det) 
      (debugln (:: "Corpus up_to_date..." $CFile)) 
      (ensure-loaded $CFile)))

  (= 
    (train-from-corpus $File $CFile) 
    ( (debugln (:: "reading corpus..." $File)) (setup-call-cleanup (open $File read $In) (, (forall (corpus-stat $Stat) (set-flag $Stat 1)) (set-flag file-line 1) (repeat) (det-if-then-else (at-end-of-stream $In) (set-det) (, (inc-flag file-line) (read-line-to-string $In $Str) (get-flag file-line $X) (once (add-training $X $Str)) (fail))) (forall (corpus-stat $Stat) (, (get-flag $Stat $Value) (debugln (= $Stat $Value)))) (save-training-file $CFile) (debugln (:: "saving corpus..." $CFile))) (close $In))))


  (= 
    (save-training-file $File) 
    ( (tell $File) 
      (writeln ':- encoding(iso-latin-1).') 
      (writeln ':- style-check(- discontiguous).') 
      (writeln ':- style-check(- singleton).') 
      (format ~n~n Nil) 
      (pllm-preds $L) 
      (maplist save-listing $L) 
      (told)))


  (= 
    (save-listing (/ $F $A)) 
    ( (functor $P $F $A) 
      (format ~n~n Nil) 
      (format ':- discontiguous((~q)/~q).~n' 
        (:: $F $A)) 
      (format ':- multifile((~q)/~q).~n' 
        (:: $F $A)) 
      (format ':- dynamic((~q)/~q).~n' 
        (:: $F $A)) 
      (format ~n Nil) 
      (forall $P 
        (, 
          (write-term $P 
            (:: 
              (quote-non-ascii True) 
              (portrayed False) 
              (no-lists False) 
              (dotlists False) 
              (quoted True) 
              (character-escapes-unicode True) 
              (character-escapes True) 
              (quote-non-ascii True) 
              (brace-terms True) 
              (ignore-ops True))) 
          (writeln .))) 
      (format ~n~n Nil)))


  !(add-history load-training)

  (= 
    (load-training) 
    (in-temp-dir load-training0))

  (= 
    (load-training0) 
    ( (pllm-preds $L) (maplist load-training $L)))


  (= 
    (load-training $MFA) 
    ( (set-det) 
      (compute-module $MFA $M 
        (/ $F $A)) 
      (functor $P $F $A) 
      (= $MP 
        (with_self  $M $P)) 
      (atomic-list-concat-t 
        (:: done- $M - $F - $A .pl) $File) 
      (or 
        (predicate-property $MP 
          (number-of-clauses $Before)) 
        (= $Before 1)) 
      (set-det) 
      (ignore (det-if-then-else (exists-file $File) (ensure-loaded $File) True)) 
      (or 
        (predicate-property $MP 
          (number-of-clauses $After)) 
        (= $After 1)) 
      (set-det) 
      (debugln (= (with_self  $M (/ $F $A)) (det-if-then $Before $After)))))


  (= 
    (compute-module $MFA $M $FA) 
    ( (strip-module $MFA $M0 $FA) 
      (compute-m $M0 $M) 
      (set-det)))


  (= 
    (compute_m  user pllm) True)
  (= 
    (compute_m  $M $M) True)


  (= 
    (save-training) 
    (in-temp-dir save-training0))

  (= 
    (save-training0) 
    ( (pllm-preds $L) (maplist save-training $L)))

  (= 
    (save-training $MFA) 
    ( (set-det) 
      (compute-module $MFA $M 
        (/ $F $A)) 
      (atomic-list-concat-t 
        (:: done- $M - $F - $A .pl) $File) 
      (tell $File) 
      (writeq !(encoding iso-latin-1)) 
      (writeln .) 
      (save-listing (/ $F $A)) 
      (told)))
;                                         ; functor(P,F,A),forall(P,(writeq(P),writeln('.'))),





  (= 
    (save-stat $G) 
    ( (det-if-then-else 
        (not $G) 
        (add-atom  &self $G) True) (nop (, (writeq $G) (writeln .)))))


  !(dynamic (/ use-extent 2))
;
;                                         use_extent(is_word,1).


  (= 
    (use_extent  tok_split 3) True) 
  (= 
    (use_extent  trigram 3) True) 
  (= 
    (use_extent  ngram 5) True)
;
;                                         use_extent(trigram,4).

;
;                                         use_extent(ngram,4).

;
;                                         use_extent(ngram,5). use_extent(ngram,6). use_extent(ngram,7). use_extent(ngram,8).


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
      (dynamic (/ $F $A2)) 
      (set-flag total-fa 1) 
      (set-flag min-fa 999999999) 
      (set-flag max-fa 1) 
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
      (nop (, (set-flag above-mean-fa 1) (set-flag above-med-high-fa 1) (set-flag num-min-fa 1) (set-flag below-mean-fa 1) (set-flag below-med-low-fa 1) (append-term $NGram $NN $NGramStatN) (forall $NGramStatN (, (ignore (, (= $NN $Min) (inc-flag num-min-fa))) (ignore (, (> $NN $High) (inc-flag above-med-high-fa))) (ignore (, (< $NN $Low) (inc-flag below-med-low-fa))) (det-if-then-else (=< $NN $Mean) (inc-flag below-mean-fa) (inc-flag above-mean-fa)))) (get-flag num-min-fa $NEMin) (get-flag above-med-high-fa $NAMedHi) (get-flag below-mean-fa $NBMean) (get-flag above-mean-fa $NAMean) (get-flag below-med-low-fa $NBMedLo) (is $NAMeanNAMedHi (- $NAMean $NAMedHi)) (is $NBMeanNBMedLo (- $NBMean $NBMedLo)) (is $NBMedLoNEMin (- $NBMedLo $NEMin)) (set-det))) 
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
    (ngram-key $P $Key) 
    ( (=.. $P 
        (Cons  ngram 
          (Cons  $_ $Args))) 
      (set-det) 
      (join-text $Args $Key)))
  (= 
    (ngram-key $P $Key) 
    ( (=.. $P 
        (Cons  $_ $Args)) (join-text $Args $Key)))


  (= 
    (join-text $List $Key) 
    ( (include all-letters $List $LList) (atomic-list-concat-t $LList , $Key)))


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
    (add-punct $X $X) 
    ( (last $X $E) (member $E (:: ? . (set-det)))))
  (= 
    (add-punct $X $Y) 
    (append $X 
      (:: .) $Y))



  (= 
    (do-pre-buffer $XX) 
    ( (findall 
        (Cons  %swap 
          (Cons  $X $Grams)) 
        (remove-atom  &self 
          (training  $X temp_grams $Grams)) $GramsL) 
      (append $GramsL $GramsAll) 
      (add-trainng-graph $XX $GramsAll)))


  (= 
    (add-training-str $XX "XXXXXXXXXXX") 
    ( (do-pre-buffer $XX) 
      (inc-flag corpus-convos) 
      (set-flag np-num 1) 
      (set-flag speech-act 1) 
      (set-det)))
;                                         ;C = 100_000_000_000, Buffer is floor(XX/C)*C + 01111111111,

;                                         ;ignore(add_conversation_training(Buffer)), !,


;
;                                         add_training_str(XX,Str):- 1 is XX mod 2, !, add_training_said(said,"Al",XX,Str),!.

;
;                                         add_training_str(XX,Str):- add_training_said(said,"Jo",XX,Str),!.



  (= 
    (add-training-str $XX $Str) 
    (must-det-ll (, (string $Str) (assert-training $XX string $Str) (tokenize-atom $Str $Toks) (set-det) (pretok $Toks $PreToks0) (add-punct $PreToks0 $PreToks) (assert-training-tree $XX $PreToks))))
;                                         ;add_training_toks(XX,PreToks),





  (= 
    (assert-training-tree $XX $PreToks) 
    (must-det-ll (, (text-to-tree $PreToks $Tree) (assert-training-v $XX text-to-tree $Tree) (unphrasify $Tree $UTree1) (assert-training $XX unphrasify $UTree1) (flatten $UTree1 $UTree2) (exclude is-penn-tag-t $UTree2 $List) (tree-to-toks $List $PostToks) (set-det) (assert-training $XX tree-to-toks $PostToks) (add-training-toks $XX $PostToks))))
;                                         ;writeq(sample_tree(Tree)),writeln('.'),




  (= 
    (must-det-ll (, $A $B)) 
    ( (set-det) 
      (must-det-ll $A) 
      (must-det-ll $B)))
  (= 
    (must-det-ll $A) 
    ( (catch $A $E 
        (, 
          (wdmsg $E) 
          (fail))) (set-det)))
  (= 
    (must-det-ll $A) 
    (rtrace $A))

  (= 
    (tree-to-toks) 
    ( (mmake) (forall (sample-tree $Tree) (tree-to-toks1 $Tree))))
;                                         /* Old Way
;                                         add_training_str(XX,Str):-
;                                          tokenize_atom(Str,Toks),
;                                          maplist(downcase_atom,Toks,TokList),
;                                          pretok(TokList,PreToks),!,
;                                          add_training_toks(XX,PreToks).
; 
;                                         */



  (= 
    (sample_tree  
      (SEQBAR 
        (CORENLP (S (CC And) (ADVP (RB then)) (NP (NP (PRP$ her) (NN son)) (, ,) (NP (NNP Ben)) (, ,)) (VP (VP (VBZ turns) (NP (DT all) (NNP Sith))) (CC and) (VP (VBZ joins) (NP (DT the) (JJ dark) (NN side)))) (. .))) 
        (CORENLP (S (PRN (S (NP (DT That)) (VP (VBD had) (S (VP (TO to) (VP (VB have) (VP (VBN factored) (PP (IN into) (NP (PRP$ her) (NNS reasons))) (S (VP (TO to) (VP (VB stay) (ADVP (RB away)) (PP (IN from) (NP (NP (DT the) (NN call)) (PP (IN of) (NP (DT the) (NN force))))))))))))))) (, ,) (VB do) (RB not) (NP (PRP you)) (VP (VB think)) (. ?))))) True)
  (= 
    (sample_tree  
      (CORENLP 
        (S 
          (NP (PRP I)) 
          (VP 
            (VB hate) 
            (S (VP (TO to) (VP (VB say) (S (NP (PRP it)) (VP (VB buuut.)) (, ,)))))) 
          (. .)) 
        (S 
          (VP (, ,)) 
          (. .)))) True)
  (= 
    (sample_tree  
      (SEQBAR 
        (CORENLP (SBAR (NP (WP who)) (S (VP (MD would) (VP (VB pick) (NP (NN kylo))))) (. ?))) 
        (CORENLP (S (ADVP (RB definitely)) (ADVP (RB not)) (NP (PRP me)))))) True)
  (= 
    (sample_tree  
      (SEQBAR 
        (CORENLP (S (S (NP (PRP He)) (VP (VBD was) (NP (NP (NNP Luke) (POS ''s')) (NNP Padwan)))) (, ,) (CC but) (S (NP (PRP he)) (VP (VBD turned))) (. .))) 
        (SEQBAR 
          (S 
            (NP (PRP It)) 
            (VP 
              (AUX has) 
              (RB not) 
              (VP 
                (AUX been) 
                (VP 
                  (VBN shown) 
                  (FRAG (WHADVP (WRB why)))))) 
            (. .)) 
          (CORENLP (S (PRN (S (NP (PRP He)) (VP (VBZ is) (ADVP (RB no) (RBR longer)) (NP (NNP Jedi))))) (, ,) (NP (PRP he)) (VP (VBZ is) (ADJP (JJ sith)) (ADVP (RB now)))))))) True)
  (= 
    (sample_tree  
      (CORENLP (SBAR (INTJ (UH Well)) (, ,) (SBAR (IN if) (S (NP (PRP it)) (VP (VBZ is) (NP (NNP Rey))))) (, ,) (ADVP (RB then)) (WHADVP (WRB why)) (S (VBD did) (NP (PRP it)) (RB not) (VP (VB wake) (SBAR (WHADVP (WRB when)) (S (NP (NNP Klyo)) (VP (VBD came) (PP (IN into) (NP (NN power))))))))))) True)
  (= 
    (sample_tree  
      (CORENLP (SBAR (NP (WP Who)) (S (VBZ is) (NP (PRP$ your) (JJ favorite) (NN character))) (. ?)))) True)
  (= 
    (sample_tree  
      (SEQBAR 
        (CORENLP (S (INTJ (UH Well)) (, ,) (NP (PRP it)) (VP (VBZ ''s') (NP (DT a) (NN movie))) (. .))) 
        (CORENLP (S (NP (PRP He)) (VP (MD could) (VP (VB show) (PRT (RP up)))))))) True)
  (= 
    (sample_tree  
      (CORENLP (S (VB Are) (NP (PRP you)) (NP (NP (DT a) (NN fan)) (PP (IN of) (NP (DT the) (NML (NNP Star) (NNPS Wars)) (NN series)))) (. ?)))) True)
  (= 
    (sample_tree  
      (CORENLP (S (NP (PRP I)) (VP (VB think) (SBAR (S (NP (PRP he)) (VP (VBD was) (ADVP (RB just)) (VP (VBG giving) (NP (DT a) (JJ giant) (JJ middle) (NN finger)) (PP (IN to) (NP (DT the) (NN audience))))))))))) True)
  (= 
    (sample_tree  
      (CORENLP (S (ADVP (RB Obviously)) (NP (NNP Darth) (NNP Vader)) (VP (VBZ is) (NP (NP (DT the) (JJS best)) (CC and) (NP (NP (DT the) (JJ original) (JJ bad) (NN guy)) (PP (IN of) (NP (NNP Star) (NNPS Wars))))))))) True)
  (= 
    (sample_tree  
      (SEQBAR 
        (CORENLP (S (NP (NNP James) (NNP Earl) (NNP Jones)) (VP (VBZ does) (NP (DT the) (NN voice)) (, ,) (SBAR (RB even) (IN though) (S (NP (PRP he)) (VP (VBZ is) (RB not) (VP (VBN listed) (PP (IN in) (NP (DT the) (NNS credits)))))))) (. .))) 
        (CORENLP (S (NP (NNP David) (NNP Prowse)) (VP (VBD did) (NP (DT the) (NN acting))))))) True)
  (= 
    (sample_tree  
      (CORENLP (S (S (NP (PRP I)) (VP (VB ''m') (ADVP (RB still)) (ADJP (RB really) (JJ bummed) (PP (IN about) (NP (DT that)))))) (, ,) (CC but) (S (NP (PRP I)) (VP (VB ''m') (ADJP (JJ sure) (SBAR (S (NP (PRP they)) (VP (MD ''ll') (VP (VB figure) (NP (NN something)) (PRT (RP out)) (PP (IN for) (NP (NP (NNP Leia)) (PP (IN in) (NP (DT The) (JJ Last) (NNP Jedi))))))))))))))) True)

  (= 
    (tree-to-toks1 $Tree) 
    ( (print-tree-nl (= i $Tree)) 
      (unphrasify $Tree $UTree1) 
      (flatten $UTree1 $UTree2) 
      (exclude is-penn-tag-t $UTree2 $UTree) 
      (print-tree-nl (= (o) $UTree)) 
      (nop (, (visible-rtrace (:: (+ call) (+ exit)) (tree-to-toks $Tree $O)) (notrace (wdmsg $O))))))

  (= 
    (funphrasify $UTree3 $UTree) 
    ( (maplist funphrasify1 $UTree3 $UTree) (set-det)))
;                                         /*
; 
; 
;                                         tree_to_toks1(Tree):-
;                                          print_tree_nl(i=Tree),
;                                          unphrasify(Tree,UTree1),
;                                          maplist(unphrasify,UTree1,UTree2),
;                                          unphrasify(UTree2,UTree3),
;                                          funphrasify(UTree3,UTree),
;                                          print_tree_nl(o:-UTree),
;                                          nop((visible_rtrace([+call,+exit],tree_to_toks(Tree,O)),
;                                          notrace(wdmsg(O)))).
; 
;                                         */


  (= 
    (funphrasify $UTree3 $UTree) 
    (funphrasify1 $UTree3 $UTree))
;
;                                         funphrasify1([_,X],O):- funphrasify1(X,O).


  (= 
    (funphrasify1 
      (Cons  NP $X) $O) 
    (combined $X $O))
  (= 
    (funphrasify1 $List $O) 
    ( (is-list $List) 
      (maplist funphrasify1 $List $M) 
      (set-det) 
      (funphrasify $M $XO) 
      (combined $XO $O)))
  (= 
    (funphrasify1  
      ($O) $O) True)
  (= 
    (funphrasify1  $O $O) True)


  (= 
    (sub-combine $Ls $O) 
    ( (unphrasify $Ls $FlatL) 
      (combined $FlatL $O) 
      (set-det)))

;
;                                         contains_phrase(Ls):- sub_term(E,Ls),atom(E),(is_penn_long(E);E=='NP').


  (= 
    (contains-phrase $O) 
    ( (sub-term $Tag $O) 
      (or 
        (== $Tag NP) 
        (, 
          (compound $Tag) 
          (= $Tag 
            (npo $_)))) 
      (set-det)))
;
;                                         contains_phrase(Ls):- member(E,Ls),is_list(E),member(Sl,E),is_list(Sl).



  (= 
    (no-sub $Stuff $E) 
    (sub-var $E $Stuff))


  (= 
    (np_like  NML) True)
  (= 
    (np_like  NP) True)



  (= 
    (unphrasify $E $OO) 
    ( (unphrasify0 $E $EO) 
      (unphrasify1 $EO $EO2) 
      (unphrasify2 $EO2 $OO)))


  (= 
    (unphrasify0 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (:: $O $O)) 
      (subst $E $S $O $MM) 
      (set-det) 
      (unphrasify0 $MM $OO)))
  (= 
    (unphrasify0 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (:: . $O)) 
      (subst $E $S $O $MM) 
      (set-det) 
      (unphrasify0 $MM $OO)))
  (= 
    (unphrasify0 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (Cons  NP $O)) 
      (not (contains-phrase $O)) 
      (subst $E $S 
        (npo $O) $MM) 
      (set-det) 
      (unphrasify0 $MM $OO)))
  (= 
    (unphrasify0 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (Cons  NP $O)) 
      (contains-phrase $O) 
      (subst $E $S 
        (oo $O) $MM) 
      (set-det) 
      (unphrasify0 $MM $OO)))
;
;                                         unphrasify0(E,OO):- sub_term(S,E),S=[Tag,O|VP],Tag='VP',subst(E,S,[O|VP],MM),!,unphrasify0(MM,OO).

;
;                                         unphrasify0(E,OO):- sub_term(S,E),S=[Tag|O],Tag='VP',subst(E,S,O,MM),!,unphrasify0(MM,OO).

  (= 
    (unphrasify0 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (:: $NP $O)) 
      (atom $O) 
      (is-penn-tag-t $NP) 
      (subst $E $S 
        (:: $O) $MM) 
      (set-det) 
      (unphrasify0 $MM $OO)))
  (= 
    (unphrasify0 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (Cons  $NP $O)) 
      (atom $O) 
      (is-penn-tag-t $NP) 
      (subst $E $S $O $MM) 
      (set-det) 
      (unphrasify0 $MM $OO)))
  (= 
    (unphrasify0 $E $E) 
    (set-det))

;
;                                         unphrasify2(E,OO):- sub_term(S,E),S=[Tag|O],is_penn_long(Tag),subst(E,S,O,MM),!,unphrasify2(MM,OO).

;
;                                         unphrasify2(E,OO):- sub_term(S,E),S=[Tag|O],is_penn_long(Tag),subst(E,S,['S',O],MM),!,unphrasify2(MM,OO).



  (= 
    (unphrasify1 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (:: $Tag $O)) 
      (is-penn-long $Tag) 
      (subst $E $S $O $MM) 
      (set-det) 
      (unphrasify1 $MM $OO)))
  (= 
    (unphrasify1 $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (Cons  $NP $O)) 
      (is-penn-tag-t $NP) 
      (subst $E $S $O $MM) 
      (set-det) 
      (unphrasify1 $MM $OO)))
;
;                                         unphrasify1(List, FlatL) :- is_list(List),maplist(sorta_f,List,FlatL).

  (= 
    (unphrasify1 $E $OO) 
    ( (sub-term $S $E) 
      (is-list $S) 
      (flatten $S $O) 
      (\== $O $S) 
      (subst $E $S $O $MM) 
      (set-det) 
      (unphrasify1 $MM $OO)))
  (= 
    (unphrasify1 $F $FF) 
    (sorta-f $F $FF))


  (= 
    (sorta-f $F $FF) 
    (flatten $F $FF))
  (= 
    (sorta_f  $F $F) True)


  (= 
    (sorta-f2 $F $FF) 
    (flatten $F $FF))
  (= 
    (sorta_f2  $F $F) True)


  (= 
    (unphrasify2 $F $O) 
    ( (is-list $F) 
      (maplist unphrasify2 $F $FF) 
      (set-det) 
      (flatten $FF $O)))
  (= 
    (unphrasify2 
      (oo $O) 
      (o $OO)) 
    (unphrasify2 $O $OO))
  (= 
    (unphrasify2 
      (npo $O) 
      (np $OO)) 
    (unphrasify2 $O $OO))
  (= 
    (unphrasify2 $F $FF) 
    (sorta-f2 $F $FF))
;
;                                         unphrasify2(E,E):-!.


;
;                                          unphrasify1([S|Ls], FlatL) :- is_penn_long(S), unphrasify1(Ls, FlatL).

;
;                                          ;unphrasify1(E,OO):- sub_term(S,E),S=[Tag,O],is_penn_tag_t(Tag),\+ np_like(Tag),subst(E,S,O,MM),!,unphrasify1(MM,OO).


;
;                                          ;unphrasify1(E,OO):- sub_term(S,E),S=[NP,O],np_like(NP),atom(O),subst(E,S,O,MM),!,unphrasify1(MM,OO).

;
;                                          ;unphrasify1(E,OO):- sub_term(S,E),S=[Tag|OL],is_penn_tag_t(Tag),maplist(atom,OL),atomic_list_concat_t(OL,'-',O),subst(E,S,O,MM),!,unphrasify1(MM,OO).

;
;                                         unphrasify1(E,OO):- sub_term(S,E),S=['NP',O],atom(O),subst(E,S,O,MM),!,unphrasify1(MM,OO).

;
;                                         unphrasify1(E,OO):- sub_term(S,E),S=[Tag|O],is_penn_tag_t(Tag),Tag\=='NP',subst(E,S,O,MM),!,unphrasify1(MM,OO).

;
;                                         unphrasify1(E,OO):- sub_term(S,E),S=['VP',AV,N],unphrasify1([AV,N],O),subst(E,S,O,MM),!,unphrasify1(MM,OO).

;
;                                         unphrasify1(E,OO):- sub_term(S,E),S=['NP'|Stuff],\+ no_sub(Stuff,'NP'),tree_to_toks(Stuff,O),subst(E,S,O,MM),unphrasify1(MM,OO).


  (= 
    (no-dash $MM) 
    (not (atom-contains $MM -)))
;                                         /*
;                                         ;unphrasify1(['NP'|Ls], O) :- unphrasify1(Ls, FlatL),tree_to_toks(FlatL,O).
;                                         unphrasify1(['NP'|Ls1], O) :- sub_combine(Ls1,O).
;                                         ;unphrasify1(['ADVP'|Ls1], O) :- maplist(unphrasify1,Ls1,Ls), unphrasify1(Ls, FlatL),tree_to_toks(FlatL,O),!.
;                                         unphrasify1(['PP'|Ls1], O) :- sub_combine(Ls1,O).
;                                         unphrasify1(['ADVP'|Ls1], O) :- sub_combine(Ls1,O).
;                                         unphrasify1(['ADJP'|Ls1], O) :- sub_combine(Ls1,O).
;                                         ;unphrasify1(E,OO):- wdmsg(unphrasify1(E,OO)),fail.
;                                         unphrasify1(['VP'|Ls], FlatL) :- !, unphrasify1(Ls, FlatL).
;                                         unphrasify1(['PP'|Ls], FlatL) :- !, unphrasify1(Ls, FlatL).
;                                         unphrasify1([S|Ls], [mark(S)|FlatL]) :- (is_penn_long(S), contains_phrase(Ls)  ), unphrasify1(Ls, FlatL),!.
;                                         unphrasify1([S|Ls], FlatL) :- S=='NP', sub_var('NP', Ls), unphrasify1(Ls, FlatL).
;                                         unphrasify1([L|Ls], [L|NewLs]) :-
;                                             dont_flatten(L),!,
;                                             unphrasify1(Ls, NewLs),!.
;                                         unphrasify1([L|Ls], FlatL) :-
;                                             unphrasify1(L, NewL),
;                                             unphrasify1(Ls, NewLs),
;                                             append([NewL], NewLs, FlatL).
;                                         unphrasify1(L, O):- is_list(L),tree_to_toks_only(L,O),!.
;                                         */




  (= 
    (combine-dash $MM $O) 
    ( (maplist no-dash $MM) 
      (atomic-list-concat-t $MM - $O) 
      (set-det)))
  (= 
    (combine_dash  $MM $MM) True)


  (= 
    (combined $M $O) 
    ( (tree-to-toks-f $M $MM) (combine-dash $MM $O)))

  (= 
    (tree-to-toks-f $M0 $O) 
    ( (tree-to-toks $M0 $MM) 
      (flatten 
        (:: $MM) $O) 
      (set-det)))


  (= 
    (not-is-list $X) 
    (not (is-list $X)))


  (= 
    (dont-flatten (Cons  $_ $L)) 
    ( (sub-var NP $L) 
      (set-det) 
      (fail)))
  (= 
    (dont-flatten (Cons  $S $_)) 
    ( (is-penn-long $S) 
      (set-det) 
      (fail)))
  (= 
    (dont-flatten (Cons  $S $_)) 
    (is-penn-tag-t $S))


  (= 
    (tree-to-toks-only $X $Y) 
    ( (tree-to-toks s $X $YY) (cleanup-toks $YY $Y)))

  (= 
    (tree-to-toks $X $Y) 
    ( (notrace (unphrasify $X $XX)) (tree-to-toks-only $XX $Y)))
  (= 
    (tree-to-toks $C $X $Y) 
    ( (tree-to-tokz $C $X $M) 
      (set-det) 
      (notrace (flatten (:: $M) $Y))))


  (= 
    (cleanup_toks  () ()) True)
  (= 
    (cleanup-toks 
      (Cons  
        (mark $_) $YY) $Y) 
    ( (set-det) (cleanup-toks $YY $Y)))
  (= 
    (cleanup-toks 
      (Cons  np 
        (Cons  $X 
          (Cons  np $YY))) 
      (Cons  $X $Y)) 
    ( (set-det) (cleanup-toks $YY $Y)))
  (= 
    (cleanup-toks 
      (Cons  np $Rest) 
      (Cons  $X $Y)) 
    ( (append $Toks 
        (Cons  np $More) $Rest) 
      (include all-letters $Toks $ToksA) 
      (maplist no-dash $ToksA) 
      (\== $ToksA Nil) 
      (atomic-list-concat-t $ToksA - $X) 
      (set-det) 
      (cleanup-toks $More $Y)))
  (= 
    (cleanup-toks 
      (Cons  $X $YY) 
      (Cons  $X $Y)) 
    ( (set-det) (cleanup-toks $YY $Y)))


  (= 
    (too_long  CORENLP) True)
  (= 
    (too_long  VP) True)
  (= 
    (too_long  PP) True)
  (= 
    (too_long  NML) True)
  (= 
    (too_long  FRAG) True)
  (= 
    (too-long $X) 
    (atom-concat $_ BAR $X))
  (= 
    (too-long $X) 
    (atom-concat S $_ $X))

  (= 
    (is-penn-tag-t $S) 
    ( (atom $S) 
      (upcase-atom $S $S) 
      (not (downcase-atom $S $S)) 
      (\== $S I)))

  (= 
    (is-penn-long $S) 
    ( (is-penn-tag-t $S) (too-long $S)))


  (= 
    (tree-to-tokz $_ $Item $Item) 
    ( (atomic $Item) (set-det)))
  (= 
    (tree-to-tokz $C $E $OO) 
    ( (sub-term $S $E) 
      (= $S 
        (:: $Tag $O)) 
      (is-penn-tag-t $Tag) 
      (atom $O) 
      (subst $E $S $O $MM) 
      (set-det) 
      (tree-to-tokz $C $MM $OO)))
  (= 
    (tree-to-tokz $C 
      (mark $SBAR) $X) 
    ( (set-det) (tree-to-tokz $C $SBAR $X)))
;                                         /*
;                                         tree_to_tokz(C,['NP'|Items],X):- !, tree_l_to_toks(C,Items,List), notrace(undbltok(List,Un)), wrap_seg(np,Un,X).
;                                         tree_to_tokz(C,[mark(SBAR)|Items],X):- !, tree_to_tokz(C,[SBAR|Items],X).
;                                         */



;
;                                         tree_to_tokz(C,[_,Item],X):- !, tree_to_tokz(C,Item,X).

;
;                                         tree_to_tokz(C,[S|Items],List):- notrace(is_penn_long(S)), Items\==[], !, tree_to_tokz(C,Items,List).

;
;                                         tree_to_tokz(C,[S|Items],X):- notrace(is_penn_tag_t(S)), Items\==[], !, tree_l_to_toks(C,Items,List), =(S,D), wrap_seg(D,List,X).

;
;                                         tree_to_tokz(C,Items,Toks):-  is_list(Items),!,tree_l_to_toks(C,Items,List),!,flatten(List,Toks),!.

  (= 
    (tree-to-tokz $C $X $X) 
    (set-det))


  (= 
    (clean_innerd  () ()) True)
  (= 
    (clean-innerd 
      (Cons  $D 
        (Cons  $E 
          (Cons  $D $Inner))) 
      (Cons  $E $ReIn)) 
    ( (set-det) (clean-innerd $Inner $ReIn)))
  (= 
    (clean-innerd 
      (Cons  $S $Inner) 
      (Cons  $S $ReIn)) 
    (clean-innerd $Inner $ReIn))

  (= 
    (wrap-seg $O $List $X) 
    ( (\== $O np) (= $List $X)))
  (= 
    (wrap-seg $O $List $X) 
    ( (append 
        (Cons  $D $Inner) 
        (:: $D) $List) 
      (clean-innerd $Inner $ReIn) 
      (wrap-seg $O $ReIn $X)))
  (= 
    (wrap-seg $D $List $X) 
    ( (append 
        (Cons  $D $List) 
        (:: $D) $X) (set-det)))
;
;                                         wrap_seg(D,List,X):- dbltok(D,List,X).



  (= 
    (tree-l-to-toks $C $Items $O) 
    ( (maplist 
        (tree-to-toks $C) $Items $List) (flatten $List $O)))


  (= 
    (assert-training $XX $P $Parse) 
    ( (assert-if-new (training $XX $P $Parse)) (nop (save-training (/ training 3)))))

  (= 
    (assert-training-v $XX $P $Parse) 
    ( (assert-training $XX $P $Parse) (debugln (training $XX $P $Parse))))


  (= 
    (do-training $XX $Str $F2) 
    ( (training $XX $F2 $_) (set-det)))
  (= 
    (do-training $XX $Str $F2) 
    ( (catch 
        (call $F2 $Str $Result) $E 
        (, 
          (dumpST) 
          (format '% % % ERROR: ~p~n' 
            (:: (--> $E (call $F2 $Str $Result)))) 
          (fail))) 
      (set-det) 
      (assert-training $XX $F2 $Result) 
      (set-det)))


  (= 
    (text_to_tree  () ()) True)
  (= 
    (text-to-tree $TokList $Tree) 
    ( (not (string $TokList)) 
      (set-det) 
      (atomics-to-string $TokList ' ' $Text) 
      (set-det) 
      (text-to-tree $TokList $Text $Tree)))
  (= 
    (text-to-tree $Text $Tree) 
    ( (tokenize-atom $Text $TokList) (text-to-tree $TokList $Text $Tree)))

  (= 
    (text-to-tree $TokList $Text $Tree) 
    ( (member " $TokList) 
      (set-det) 
      (text-to-best-tree $Text $Tree)))
  (= 
    (text-to-tree $TokList $_ 
      (:: SEQBAR $X $Y)) 
    ( (append $Left 
        (Cons  $LE $Right) $TokList) 
      (\== $Right Nil) 
      (member $LE 
        (:: . ? 
          (set-det))) 
      (append $Left 
        (:: $LE) $Said) 
      (set-det) 
      (text-to-tree $Said $X) 
      (text-to-tree $Right $Y)))
  (= 
    (text-to-tree $TokList $Text $Tree) 
    ( (text-to-best-tree $Text $Tree) (set-det)))
  (= 
    (text-to-tree $TokList $Text $Tree) 
    ( (text-to-lgp-tree $Text $Tree) (set-det)))

;
;                                         all_letters(X):- compound(X),X=np(_),!.

;
;                                         all_letters(X):- compound(X),X=o(_),!.


  (= 
    (all-letters $X) 
    ( (not (compound $X)) (not (, (upcase-atom $X $U) (downcase-atom $X $U)))))


  (= 
    (retokify  () ()) True)
  (= 
    (retokify 
      (Cons  , $APreToks) $PreToks) 
    (retokify $APreToks $PreToks))
  (= 
    (retokify 
      (Cons  $X 
        (Cons  $X $APreToks)) $PreToks) 
    (retokify 
      (Cons  $X $APreToks) $PreToks))

  (= 
    (retokify 
      (Cons  $E $APreToks) 
      (Cons  $E $PreToks)) 
    ( (not (atomic $E)) 
      (set-det) 
      (retokify $APreToks $PreToks)))
  (= 
    (retokify 
      (Cons  $E $APreToks) $PreToks) 
    ( (or 
        (not (atomic $E)) 
        (is-penn-tag-t $E)) 
      (set-det) 
      (retokify $APreToks $PreToks)))
  (= 
    (retokify 
      (Cons  $E $APreToks) 
      (Cons  $F $PreToks)) 
    ( (downcase-atom $E $F) (retokify $APreToks $PreToks)))


  (= 
    (add-training-toks $_ Nil) 
    (set-det))
  (= 
    (add-training-toks $X 
      (:: $A)) 
    ( (set-det) (add-training-toks $X (:: $A .))))
  (= 
    (add-training-toks $XX $APreToks) 
    ( (must-det-l (, (notrace (, (retokify $APreToks $PreToks) (maplist (add-occurs is-word) $PreToks) (inc-flag corpus-training) (is $XX1 (+ $XX 1)))) (notrace (, (subst $PreToks . $XX $PreToks1) (subst $PreToks1 ? (:: ? $XX1) $PreToks3) (flatten $PreToks3 $PreToks333) (= $PreToks333 $PreToks33) (det-if-then-else (contains-var $XX1 $PreToks33) (append (Cons  $XX $PreToks33) Nil $ReToks) (append (Cons  $XX $PreToks33) (:: $XX1) $ReToks)))) (set-det) (predbltok $ReToks $Grams) (assert-training $XX temp-grams $PreToks33))) (set-det)))
;                                         ;subst(PreToks1,',','$dead',PreToks2),

;                                         ;get_flag(np_num,NPNUM),

;                                         ;set_np_nums(XX,NPNUM,PreToks333,PreToks33),


 


  (= 
    (add-trainng-graph $_ Nil) 
    (set-det))
  (= 
    (add-trainng-graph $X 
      (:: $A)) 
    ( (set-det) (add-trainng-graph $X (:: $A .))))
  (= 
    (add-trainng-graph $XX $APreToks) 
    ( (must-det-l (, (retokify $APreToks $ReToks) (predbltok $ReToks $Grams) (assert-training-v $XX grams $Grams))) 
      (set-det) 
      (forall 
        (between 3 7 $N) 
        (add-ngrams except-none ngram $N $XX $Grams)) 
      (set-det)))



  (= 
    (np-saver $XX $N $K $O) 
    ( (asserta-if-new (np-save $XX $N $K)) (= $O $N)))


  (= 
    (set_np_nums  $_ $_ () ()) True)
  (= 
    (set-np-nums $XX $N 
      (Cons  %dead $PreToks3) $PreToks33) 
    (set-np-nums $XX $N $PreToks3 $PreToks33))
  (= 
    (set-np-nums $XX $N 
      (Cons  
        (np $K) $PreToks3) 
      (Cons  
        (np $O) $PreToks33)) 
    ( (np-saver $XX $N $K $O) 
      (is $NN 
        (+ $N 1)) 
      (set-flag np-num $NN) 
      (set-np-nums $XX $NN $PreToks3 $PreToks33)))
  (= 
    (set-np-nums $XX $N 
      (Cons  
        (o $K) $PreToks3) 
      (Cons  
        (o $O) $PreToks33)) 
    ( (np-saver $XX $N $K $O) 
      (is $NN 
        (+ $N 1)) 
      (set-flag np-num $NN) 
      (set-np-nums $XX $NN $PreToks3 $PreToks33)))
  (= 
    (set-np-nums $XX $N 
      (Cons  $K $PreToks3) 
      (Cons  $K $PreToks33)) 
    (set-np-nums $XX $N $PreToks3 $PreToks33))



  (= 
    (add-ngrams $Except $F $N $Loc $Grams) 
    ( (length $Grams $L) 
      (< $L $N) 
      (set-det)))

  (= 
    (add-ngrams $Except $F $N $Loc $Grams) 
    ( (length $NGram $N) 
      (append $NGram $_ $Mid) 
      (forall 
        (append $_ $Mid $Grams) 
        (add-1ngram $Except $F skip $NGram))))

  (= 
    (except_none  $_) True)
;                                         /*
;                                         add_ngrams(Except,F,N,Loc,Grams):-
;                                          length(NGram,N),
;                                          append(NGram,_,Grams),
;                                          once(add_1ngram(Except,F,Loc,NGram)), fail.
;                                         add_ngrams(Except,F,N,Loc,Grams):-
;                                          length(NGram,N),
;                                          append(_,NGram,Grams),
;                                          once(add_1ngram(Except,F,Loc,NGram)), fail.
;                                         add_ngrams(_,_,_,_,_).
;                                         */



  (= 
    (add-1ngram $Except $F $Loc $List) 
    ( (\== $Except except-none) 
      (not (maplist $Except $List)) 
      (set-det)))
  (= 
    (add-1ngram $Except $F $Loc $List) 
    ( (det-if-then-else 
        (== $Loc skip) 
        (=.. $P 
          (Cons  $F 
            (Cons  $Was $List))) 
        (=.. $P 
          (Cons  $F 
            (Cons  $Was 
              (Cons  $Loc $List))))) 
      (det-if-then-else 
        (== $Loc skip) 
        (=.. $Q 
          (Cons  $F 
            (Cons  $N $List))) 
        (=.. $Q 
          (Cons  $F 
            (Cons  $N 
              (Cons  $Loc $List))))) 
      (ngram-inc $P) 
      (set-det) 
      (det-if-then-else 
        (clause $P True $Ref) 
        (, 
          (erase $Ref) 
          (is $N 
            (+ $Was 1)) 
          (add-atom  &self $Q) 
          (inc-flag corpus-node-overlap)) 
        (, 
          (= $N 1) 
          (add-atom  &self $Q) 
          (inc-flag corpus-nodes))) 
      (set-det)))


  (= 
    (add-occurs $F $Tok) 
    ( (=.. $P 
        (:: $F $Was $Tok)) 
      (=.. $Q 
        (:: $F $N $Tok)) 
      (det-if-then-else 
        (clause $P True $Ref) 
        (, 
          (erase $Ref) 
          (is $N 
            (+ $Was 1)) 
          (add-atom  &self $Q)) 
        (, 
          (= $N 1) 
          (add-atom  &self $Q) 
          (inc-flag corpus-unique-toks))) 
      (inc-flag corpus-total-toks)))


  (= 
    (except-symbols $X) 
    (not (, (upcase-atom $X $U) (downcase-atom $X $U))))


  (= 
    (atomic-list-concat-t $A $B $C) 
    ( (catch 
        (atomic-list-concat $A $B $C) $_ fail) (set-det)))
  (= 
    (atomic-list-concat-t $A $B $C) 
    ( (rtrace (atomic-list-concat $A $B $C)) (set-det)))
  (= 
    (atomic-list-concat-t $A $C) 
    ( (catch 
        (atomic-list-concat $A $C) $_ fail) (set-det)))
  (= 
    (atomic-list-concat-t $A $C) 
    ( (rtrace (atomic-list-concat $A $C)) (set-det)))

  (= 
    (pretok  () ()) True)
;
;                                         pretok(['.'],[]):-!.

  (= 
    (pretok 
      (Cons  $X $Nxt) 
      (Cons  $X $O)) 
    ( (compound $X) (pretok $Nxt $O)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  $X $Nxt)) 
      (Cons  $A 
        (Cons  $X $O))) 
    ( (compound $X) (pretok $Nxt $O)))

  (= 
    (pretok 
      (Cons  $X 
        (Cons  $X 
          (Cons  $X $Nxt))) $O) 
    ( (set-det) 
      (atomic-list-concat-t 
        (:: $X $X $X) , $Y) 
      (pretok 
        (Cons  $Y $Nxt) $O)))
  (= 
    (pretok 
      (Cons  $A 
        (Cons  - 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atomic-list-concat-t 
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
      (atomic-list-concat-t 
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
      (atomic-list-concat-t 
        (:: $A $S) ' $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
;
;                                          backtick

  (= 
    (pretok 
      (Cons  $A 
        (Cons  $B 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atom $B) 
      (name $B 
        (:: 96)) 
      (all-letters $A) 
      (all-letters $S) 
      (atomic-list-concat-t 
        (:: $A $S) ' $F) 
      (set-det) 
      (pretok $Grams $ReTok)))
;
;                                         pretok([','|Grams],ReTok):- pretok(Grams,ReTok).

;
;                                         pretok(['-'|Grams],ReTok):- pretok(Grams,ReTok).

;
;                                         pretok([A,B,C|Grams],ReTok):- trigram(A,B,C,N), N>40, !,ngram_key(trigram(A,B,C),Key),pretok([Key|Grams],ReTok).

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
    (predbltok  () ()) True)
  (= 
    (predbltok 
      (:: .) Nil) 
    (set-det))
  (= 
    (predbltok 
      (Cons  $X 
        (Cons  $X $Nxt)) $O) 
    ( (number $X) (predbltok (Cons  $X $Nxt) $O)))
  (= 
    (predbltok 
      (Cons  $X $Nxt) 
      (Cons  $X $O)) 
    ( (compound $X) (predbltok $Nxt $O)))
  (= 
    (predbltok 
      (Cons  $A 
        (Cons  $X $Nxt)) 
      (Cons  $A 
        (Cons  $X $O))) 
    ( (compound $X) (predbltok $Nxt $O)))
  (= 
    (predbltok 
      (Cons  $A 
        (Cons  $B 
          (Cons  $X $Nxt))) 
      (Cons  $A 
        (Cons  $B 
          (Cons  $X $O)))) 
    ( (compound $X) (predbltok $Nxt $O)))
  (= 
    (predbltok 
      (Cons  $X 
        (Cons  $Y $Nxt)) $O) 
    ( (number $X) 
      (number $Y) 
      (< $X $Y) 
      (set-det) 
      (predbltok 
        (Cons  $Y $Nxt) $O)))
  (= 
    (predbltok 
      (Cons  $X 
        (Cons  $X 
          (Cons  $X $Nxt))) $O) 
    ( (set-det) 
      (atomic-list-concat-t 
        (:: $X $X $X) , $Y) 
      (predbltok 
        (Cons  $Y $Nxt) $O)))
  (= 
    (predbltok 
      (Cons  $A 
        (Cons  - 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atomic-list-concat-t 
        (:: $A $S) - $F) 
      (set-det) 
      (predbltok $Grams $ReTok)))
  (= 
    (predbltok 
      (Cons  $A 
        (Cons  ' 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (all-letters $A) 
      (all-letters $S) 
      (atomic-list-concat-t 
        (:: $A $S) ' $F) 
      (set-det) 
      (predbltok $Grams $ReTok)))
  (= 
    (predbltok 
      (Cons  $A 
        (Cons  ´ 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (all-letters $A) 
      (all-letters $S) 
      (atomic-list-concat-t 
        (:: $A $S) ' $F) 
      (set-det) 
      (predbltok $Grams $ReTok)))
  (= 
    (predbltok 
      (Cons  $A 
        (Cons  $B 
          (Cons  $S $Grams))) 
      (Cons  $F $ReTok)) 
    ( (atom $B) 
      (name $B 
        (:: 96)) 
      (all-letters $A) 
      (all-letters $S) 
      (atomic-list-concat-t 
        (:: $A $S) ' $F) 
      (set-det) 
      (predbltok $Grams $ReTok)))

  (= 
    (predbltok 
      (Cons  , $Grams) $ReTok) 
    (predbltok $Grams $ReTok))
  (= 
    (predbltok 
      (Cons  
        (set-det) $Grams) $ReTok) 
    (predbltok 
      (Cons  . $Grams) $ReTok))
  (= 
    (predbltok 
      (Cons  $S $Grams) 
      (Cons  $S $ReTok)) 
    (predbltok $Grams $ReTok))

;
;                                          dbltok(_,X,X):-!.


  (= 
    (dbltok $_ 
      (:: $X) 
      (:: $X)) 
    ( (number $X) (set-det)))
;
;                                         dbltok(_,[S],[S]):- is_full_tok(S),!.

;
;                                         dbltok(Pre,[S],[PS]):- atoms_join(Pre,S,PS).

;
;                                         dbltok(Pre,[],[PS]):-!, atoms_join(Pre,,PS).

  (= 
    (dbltok $Pre 
      (Cons  $S $I) 
      (Cons  $S $O)) 
    ( (is-full-tok $S) 
      (set-det) 
      (dbltok $Pre $I $O)))
  (= 
    (dbltok $Pre 
      (Cons  $S $Grams) 
      (Cons  $PS $ReTok)) 
    ( (atoms-join $Pre $S $PS) (dbltok $S $Grams $ReTok)))




  (= 
    (undbltok $I $O) 
    ( (is-list $I) 
      (set-det) 
      (maplist undbltok $I $O)))
  (= 
    (undbltok $S $PS) 
    ( (into-mw $S 
        (Cons  $PS $_)) (set-det)))
  (= 
    (undbltok $S $S) 
    (set-det))


  (= 
    (is-full-tok $O) 
    ( (compound $O) (set-det)))
  (= 
    (is-full-tok $O) 
    ( (atom $O) (atomic-list-concat-t (Cons  $_ (Cons  $_ $_)) : $O)))


  (= 
    (atoms-join $A $B $O) 
    ( (tok-split $Was $O $A $B) 
      (set-det) 
      (is $N 
        (+ $Was 1)) 
      (add-atom  &self 
        (tok_split  $N $O $A $B))))
  (= 
    (atoms-join $A $B $O) 
    ( (atomic-list-concat-t 
        (:: $A $B) : $O) 
      (set-det) 
      (add-atom  &self 
        (tok_split  1 $O $A $B))))

;
;                                          @TODO use average

;
;                                         as_good(T,X):- is_word(T,X),(Nxt>500->X=1;X is 500-Nxt).

;
;                                         ngram_rate(A,B,C,D,N,NN):- ngram(N,Loc,A,B,C,D), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).



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
    ( (tok-split $Cnt $O $A $B) 
      (set-det) 
      (add-blanks $N $Sent $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $S $Sent) 
      (Cons  $O $Slotted)) 
    ( (not (not (tok-split $_ $_ $S $_))) 
      (set-det) 
      (tok-split $Cnt $O $S $_) 
      (add-blanks $N $Sent $Slotted)))
  (= 
    (add-blanks $N 
      (Cons  $O $Sent) 
      (Cons  $O $Slotted)) 
    ( (atom $O) 
      (tok-split $_ $O $_ $_) 
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
    ( (atomic-list-concat-t 
        (Cons  $M 
          (Cons  $_ $_)) : $S) 
      (set-det) 
      (into-mw0 $M $SL)))
  (= 
    (into-mw0 $S $SL) 
    (atomic-list-concat-t $SL , $S))
  (= 
    (into-mw0 $S $SL) 
    (atomic-list-concat-t $SL ' ' $S))
  (= 
    (into-mw0 $S $SL) 
    (atomic-list-concat-t $SL - $S))


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
    (ngraml1 $Loc $A $X $B $C $NN) 
    ( (nonvar $X) 
      (ngram $Loc $_ $_ $A $X $_) 
      (ngram $ULoc $X $B $C $_ $NN)))
  (= 
    (ngraml1 $Loc $A $B $X $C $NN) 
    ( (nonvar $X) 
      (ngram $Loc $_ $A $B $X $_) 
      (ngram $ULoc $X $C $_ $_ $NN)))


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


  (= 
    (lg $X) 
    ( (not (is-list $X)) 
      (set-det) 
      (between 22 66 $L) 
      (lg $L $X)))
  (= 
    (lg $X) 
    ( (lg0 $X) (write-phr $X)))

  (= 
    (lg $L $X) 
    ( (var $L) 
      (set-det) 
      (lg0 $X) 
      (length $X $L) 
      (write-phr $X)))
  (= 
    (lg $L $X) 
    ( (length $X $L) 
      (lg0 $X) 
      (write-phr $X)))


  (= 
    (write-phr $X) 
    ( (is-list $X) 
      (set-det) 
      (maplist write-phr0 $X) 
      (nl)))
  (= 
    (write-phr $X) 
    ( (write-phr0 $X) (nl)))


  (= 
    (write-phr0 $X) 
    ( (is-list $X) 
      (set-det) 
      (maplist write-phr0 $X)))
  (= 
    (write-phr0 (np $X)) 
    (write-phr0 $X))
  (= 
    (write-phr0 (o $X)) 
    (write-phr0 $X))
  (= 
    (write-phr0 $X) 
    (number $X))
  (= 
    (write-phr0 %swap) 
    (nl))
  (= 
    (write-phr0 $X) 
    ( (write $X) (write ' ')))


  (= 
    (lg0 $X) 
    ( (freeze $N 
        (number $N)) 
      (lg1 (Cons  $N $X)) 
      (last $X $E) 
      (number $E)))


  (= 
    (lg1 (:: $_ $N)) 
    ( (number $N) (set-det)))
  (= 
    (lg1 (:: $_ $_ $N)) 
    ( (number $N) (set-det)))
  (= 
    (lg1 (Cons  $Z (Cons  $X (Cons  $Y (Cons  $A (Cons  $B (Cons  $C $More))))))) 
    ( (lgram $Z $X $Y $A $B $C) (lg1 (Cons  $A (Cons  $B (Cons  $C $More))))))
  (= 
    (lg1 (Cons  $X (Cons  $Y (Cons  $A (Cons  $B (Cons  $C $More)))))) 
    ( (lgram $X $Y $A $B $C) (lg1 (Cons  $A (Cons  $B (Cons  $C $More))))))
  (= 
    (lg1 (Cons  $X (Cons  $Y (Cons  $A (Cons  $B $More))))) 
    ( (lgram $X $Y $A $B) (lg1 (Cons  $A (Cons  $B $More)))))

  (= 
    (lg1  
      ($_ $_ $_)) True)
  (= 
    (lg1  
      ($_ $_)) True)




  !(add-history recompile-corpus)


  (= 
    (is-word $_) 
    ( (dumpST) (break)))

  (= 
    (scene_info  Smallville_S03E14_scene_12_with_2_characters_Chloe_Clark 2 
      (Chloe Clark) 
      ($Chloe $Clark) 
      ( (:  $Chloe 
          (I am really sorry she went all Glenn Close on you .)) 
        (:  $Clark 
          (I should have told you about Alicia ''s' ability before .)) 
        (:  $Clark 
          (But she had asked me to keep it a secret .)) 
        (:  $Chloe 
          (Yeah , and I respect you for keeping her confidence .)) 
        (:  $Chloe 
          (But once she went psycho , all bets are off .)) 
        (:  $Clark 
          (So how do you look for someone who can disappear in the blink of an eye whenever she wants ?)) 
        (:  $Chloe 
          (Well , we know that Alicia has at least one weakness .)) 
        (:  $Chloe 
          (You .)) 
        (:  $Clark 
          (We may have one more .)) 
        (:  $Clark 
          ($Chloe , I have an idea .)) 
        (:  $Clark 
          (I am going to need your help .)))) True)
;                                         /*
;                                         how are you?  i am fine
;                                          V
;                                         1000 how are you ? 1001 i am fine 1002
;                                          V
;                                         1000 how:are are:you you:? 1001 i:am am:fine 1002
;                                         */




  (= 
    (two_way_convo  "\r\n  So, I watched the Force Awakens, I am lost.\r\n  By what? Good film.\r\n  I liked it, but was the force sleeping before?\r\n  No, it just means that a new Jedi had emerged.\r\n  Well, if it is Rey, then why did it not wake when Klyo came into power.\r\n  Klyo went to the dark side, he is not a Jedi.\r\n  If he is not a Jedi, why was he at Jedi school.\r\n  He was Luke's Padwan, but he turned. It has not been shown why. He is no longer Jedi, he is sith now.\r\n  Wait, I thought sith was a race.\r\n  No, Vader was human, well pretty much, but went from Jedi to Sith. Like Yoda is whatever he is, but is also a Jedi.\r\n") True)


  (= 
    (use-scene-info) 
    ( (= $G 
        (scene-info $Name $_ $Chars $Vars $Events)) (forall $G (use-scene-info $G))))

;
;                                          /opt/logicmoo_workspace/packs_xtra/logicmoo_chat/corpus/soap_opera_corpus/


  (= 
    (fix-scene-events 
      (with_self  $W 
        (action $Action)) 
      (with_self  $W 
        (:: (action $WAction)))) 
    ( (set-det) 
      (term-to-atom $W $WW) 
      (fix-scene-events 
        (Cons  $WW $Action) $WAction)))
  (= 
    (fix-scene-events 
      (with_self  $W $Says) 
      (with_self  $W $Said)) 
    ( (set-det) (fix-scene-events $Says $Said)))
  (= 
    (fix-scene-events $L $LL) 
    ( (is-list $L) 
      (maplist any-to-symbol $L $LLL) 
      (set-det) 
      (text-to-tree $LLL $LL)))
  (= 
    (fix-scene-events $Says $Said) 
    (text-to-tree $Says $Said))


  (= 
    (use-scene-info (scene-info $Name $_ $Chars $Vars $AllEvents)) 
    ( (numbervars $Vars 1 $_ 
        (:: (attvar skip))) 
      (maplist fix-scene-events $AllEvents $AllEvents2) 
      (= 
        (Cons  
          (with_self  $Who1 $Did) $Events) $AllEvents2) 
      (cobined-sers $Ser $_) 
      (combine-whos $Who1 
        (Cons  $Ser $Did) $Events $LinearEvents) 
      (use-linear-events $LinearEvents))) 


  (= 
    (use-linear-events $LE) 
    (wdmsg $LE))


  (= 
    (combine-whos $Who $Did Nil 
      (:: (with_self  $Who $Did))) 
    (set-det))
  (= 
    (combine-whos $Who $Did 
      (Cons  
        (with_self  $Who1 $Does) $Events) $LinearEvents) 
    ( (== $Who $Who1) 
      (set-det) 
      (combine-events $Did $Does $DidDoes) 
      (combine-whos $Who1 $DidDoes $Events $LinearEvents)))
  (= 
    (combine-whos $Who $Did 
      (Cons  
        (with_self  $Who1 $Does) $Events) 
      (Cons  
        (with_self  $Who $Did) $LinearEvents)) 
    ( (\== $Who $Who1) 
      (set-det) 
      (cobined-sers $Ser $_) 
      (combine-whos $Who1 
        (Cons  $Ser $Does) $Events $LinearEvents)))


  (= 
    (cobined-sers $Ser $Ser2) 
    ( (flag says $SayDo 
        (+ $SayDo 1)) 
      (is $Ser 
        (+ $SayDo 10000)) 
      (is $Ser2 
        (+ $Ser 2))))


  (= 
    (combine-events $Did $Does $DidDoes) 
    ( (not (is-list $Did)) 
      (set-det) 
      (combine-events 
        (:: $Did) $Does $DidDoes)))
  (= 
    (combine-events $Did $Does $DidDoes) 
    ( (not (is-list $Does)) 
      (set-det) 
      (combine-events $Did 
        (:: $Does) $DidDoes)))
  (= 
    (combine-events $Did $Does $DidDoes) 
    (append $Did $Does $DidDoes))


  (= 
    (lst_3_2  
      ($_ $_ $_)) True)
  (= 
    (lst_3_2  
      ($_ $_)) True)


  (= 
    (lst_2_3  
      ($_ $_)) True)
  (= 
    (lst_2_3  
      ($_ $_ $_)) True)

;
;                                         lst_4_1(NV):- nonvar(NV),!.


  (= 
    (lst_4_1  
      ($_ $_ $_ $_)) True)
  (= 
    (lst_4_1  
      ($_ $_ $_)) True)
  (= 
    (lst_4_1  
      ($_ $_)) True)
  (= 
    (lst_4_1  
      ($_)) True)


;
;                                          4 - 12


  (= 
    (s1 $List) 
    ( (rnd-cl 
        (lst-4-1 $A) 
        (lst-3-2 $B) 
        (lst-4-1 $C)) 
      (append 
        (:: $A $B $C) $List) 
      (sent $A $B $C)))

;
;                                          8 - 15


  (= 
    (s2 $List) 
    ( (rnd-cl 
        (lst-4-1 $A) 
        (lst-3-2 $C) 
        (lst-4-1 $E)) 
      (rnd-cl 
        (lst-3-2 $B) 
        (lst-3-2 $D)) 
      (append 
        (:: $A $B $C $D $E) $List) 
      (sent $A $B $C) 
      (sent $C $D $E)))


;
;                                          12 - 35


  (= 
    (s3 $List) 
    ( (rnd-cl 
        (lst-3-2 $C) 
        (lst-3-2 $E)) 
      (det-if-then-else 
        (nonvar $List) 
        (append 
          (:: $A $B $C $D $E $F $G) $List) True) 
      (sent $A $B $C) 
      (sent $C $D $E) 
      (sent $E $F $G) 
      (append 
        (:: $A $B $C $D $E $F $G) $List)))
;                                         ; rnd_cl(lst_4_1(A), lst_4_1(G)), rnd_cl(lst_3_2(B), lst_3_2(D), lst_3_2(F)),




  (= 
    (lgram $X) 
    (rnd-cl (lgram0 $X)))

  (= 
    (lgram0 (:: $X $Y $Z $A $B $C $D)) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A $B $C $D)))
  (= 
    (lgram0 (:: $X $Y $Z $A $B $C)) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A $B $C)))
  (= 
    (lgram0 (:: $X $Y $Z $A $B)) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A $B)))
  (= 
    (lgram0 (:: $X $Y $Z $A)) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A)))
  (= 
    (lgram0 (:: $X $Y $Z)) 
    (var-or-rand (ngram $_ $_ $X $Y $Z)))



  (= 
    (lgram $X $Y $Z $A $B $C $D) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A $B $C $D)))
  (= 
    (lgram $X $Y $Z $A $B $C) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A $B $C)))
  (= 
    (lgram $X $Y $Z $A $B) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A $B)))
  (= 
    (lgram $X $Y $Z $A) 
    (var-or-rand (ngram $_ $_ $X $Y $Z $A)))
  (= 
    (lgram $X $Y $Z) 
    (var-or-rand (ngram $_ $_ $X $Y $Z)))


  (= 
    (has-nonvar $P) 
    ( (arg $_ $P $E) 
      (nonvar $E) 
      (\== $E Nil) 
      (set-det)))

  (= 
    (var-or-rand $P) 
    (det-if-then-else 
      (has-nonvar $P) $P 
      (rnd-cl $P)))
;
;                                         var_or_rand(P):- rnd_cl(P).


  (= 
    (var-or-once $Mid $P) 
    (det-if-then-else 
      (var $Mid) 
      (rnd-cl $P) 
      (once $P)))


  (= 
    (sent $LeftMid $Left $Mid $Right $MidRight) 
    ( (var-or-once $Mid 
        (lst-3-2 $Mid)) 
      (var-or-once $Left 
        (lst-4-1 $Left)) 
      (append $Left $Mid $LeftMid) 
      (apply lgram $LeftMid) 
      (var-or-once $Right 
        (lst-4-1 $Right)) 
      (append $Mid $Right $MidRight) 
      (apply lgram $MidRight)))
  (= 
    (sent $Left $Mid $Right) 
    (sent $_ $Left $Mid $Right $_))


  (= 
    (sent $Sent) 
    (s1 $Sent))
  (= 
    (sent $Sent) 
    (s2 $Sent))
  (= 
    (sent $Sent) 
    (s3 $Sent))


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
    (some-ngram $N $PrevLoc $A $B $C $D) 
    ( (pick-ngram $N $Loc $A $B $C $D) (may-use $N $Loc $A $B $C $D)))


  (= 
    (pick-ngram $N $Loc $A $B $C $D) 
    (det-if-then-else 
      (maplist var 
        (:: $A $B $C $D)) 
      (rnd-ngram $N $Loc $A $B $C $D) 
      (ngram $N $Loc $A $B $C $D)))


  (= 
    (rnd-ngram $N $Loc $A $B $C $D) 
    (rnd-cl (ngram $N $Loc $A $B $C $D)))


  (= 
    (rnd-cl $G) 
    ( (predicate-property $G 
        (number-of-clauses $R)) 
      (randseq $R $R $Seq) 
      (member $CN $Seq) 
      (nth-clause $G $CN $Ref) 
      (clause $G $Body $Ref) $Body))

  (= 
    (rnd-cl $G1 $G2) 
    ( (predicate-property $G1 
        (number-of-clauses $N1)) 
      (predicate-property $G2 
        (number-of-clauses $N2)) 
      (rand-xy $N1 $N2 $List) 
      (member 
        (+ $X $Y) $List) 
      (nth-clause $G1 $X $Ref1) 
      (clause $G1 $Body1 $Ref1) 
      (nth-clause $G2 $Y $Ref2) 
      (clause $G2 $Body2 $Ref2) $Body1 $Body2))

  (= 
    (rnd-cl $G1 $G2 $G3) 
    ( (predicate-property $G1 
        (number-of-clauses $N1)) 
      (predicate-property $G2 
        (number-of-clauses $N2)) 
      (predicate-property $G3 
        (number-of-clauses $N3)) 
      (rand-xyz $N1 $N2 $N3 $List) 
      (member 
        (+ 
          (+ $X $Y) $Z) $List) 
      (nth-clause $G1 $X $Ref1) 
      (clause $G1 $Body1 $Ref1) 
      (nth-clause $G2 $Y $Ref2) 
      (clause $G2 $Body2 $Ref2) 
      (nth-clause $G3 $Z $Ref3) 
      (clause $G3 $Body3 $Ref3) $Body1 $Body2 $Body3))



  (= 
    (rand-x $X $Seq) 
    (randseq $X $X $Seq))

  (= 
    (rand-xy $X $Y $Rnd) 
    ( (rand-x $X $Seq) 
      (assign-rands $Seq $Y $List) 
      (flatten $List $Set) 
      (random-permutation $Set $Rnd)))

  (= 
    (rand-xyz $X $Y $Z $Rnd) 
    ( (rand-xy $X $Y $Seq) 
      (assign-rands $Seq $Z $List) 
      (flatten $List $Set) 
      (random-permutation $Set $Rnd)))


  (= 
    (assign-rand $E $R $EE) 
    ( (rand-x $R $Seq) (add-pair $Seq $E $EE)))

  (= 
    (add_pair  () $_ ()) True)
  (= 
    (add-pair 
      (Cons  $X $Seq) $Y 
      (Cons  
        (+ $Y $X) $ER)) 
    (add-pair $Seq $Y $ER))


  (= 
    (assign_rands  () $_ ()) True)
  (= 
    (assign-rands 
      (Cons  $E $L) $R 
      (Cons  $ER $LL)) 
    ( (assign-rand $E $R $ER) (assign-rands $L $R $LL)))



  !(style-check (- singleton))


  !(add-history (, (good-toks $Key $E) (> $E 20)))
  !(add-history (autoc (:: like:you (len 200))))
  !(add-history (autoc (:: :like like:you (len 200))))
  !(add-history (autoc (:: like (len 200))))
  !(add-history (autoc (:: (len 10) like (len 200))))
  !(add-history load-training)
  !(add-history tree-to-toks)
  !(add-history compile-corpus)


  (= 
    (may-use $Loc $_ $B $C $D $_) 
    ( (not (used-cl (lgram $A $B $C $D))) 
      (assert 
        (used-cl (lgram $A $B $C $D)) $Cl2) 
      (undo (erase $Cl2)) 
      (set-det)))



  (= 
    (gen6 (= (:: $A $B $C $D $E $F $G $H) $N)) 
    ( (ngram $_ $Loc1 $E $F $G $H $Z) 
      (ngram $_ $Loc2 $C $D $E $F $Y) 
      (ngram $_ $Loc3 $A $B $C $D $X) 
      (is $N 
        (+ 
          (+ $X $Y) $Z))))


  !(ensure-loaded trains-trigrams)

  !fixup-exports


  (= 
    (dotit) 
    (ignore (, (not (prolog-load-context reloading True)) (ignore load-training) (ignore compile-corpus))))

;
;                                         :- ensure_loaded(pldata('corpus/soap_opera_corpus/so_convert')).


  !(if (not (prolog-load-context reloading True)))
  !compile-corpus
  !endif


