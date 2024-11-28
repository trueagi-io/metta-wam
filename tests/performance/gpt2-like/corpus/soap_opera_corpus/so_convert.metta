;                                           (convert_to_metta_file  so_convert $_242230 tests/performance/gpt2-like/corpus/soap_opera_corpus/so_convert.pl tests/performance/gpt2-like/corpus/soap_opera_corpus/so_convert.metta)
  !(encoding iso-latin-1)
  !(module so-convert Nil)
  !(encoding iso-latin-1)


;
;  :- include(weightless_so_convert).


  (= (so_convert_preds  ((/  converting 3) (/  is_word 1) (/  is_word 2) (/  ngram 5) (/  ngram 6) (/  trigram 3) (/  trigram 4) (/  tok_split 3) (/  tok_split 4)))  True)


  (= (declare-preds $X)  
    (dynamic $X)
    (multifile $X))


  !((so-convert-preds $L) (maplist declare-preds $L))

  !(declare-preds (/ scene-info 5))
;
;  :- ensure_loaded(converts_trigrams).

  !(use-module (library logicmoo-utils))
  !(must (, (prolog-load-context directory $Dir) (absolute-file-name ../.. $PlData (:: (relative-to $Dir) (file-type directory))) (assert-if-new (file-search-path pldata $PlData))))


;
;  debug printing

  (= (debugln $X)  
    (debugln-xfrm $X $S)
    (dmsg $S))

  (= (fmt-pllm $X)  
    (debugln-xfrm $X $S)
    (fmt $S))


  (= (debugln-xfrm $Insts $S)  
    (var $Insts)
    (set-det)
    (sformat $S "~p" 
      (:: $Insts)))
  (= (debugln-xfrm (i $X) $S)  
    (det-if-then-else 
      (not (is-list $X)) 
      (debugln-xfrm $X $S) 
      (, 
        (maplist debugln-xfrm $X $Y) 
        (atomics-to-string $Y ' ' $S))))
  (= (debugln-xfrm (Cons  $N $A) $S)  
    (is-list $A)
    (set-det)
    (maplist debugln-xfrm 
      (Cons  $N $A) $Y)
    (atomics-to-string $Y ' ' $S))
  (= (debugln-xfrm (/ $F $A) $S)  
    (functor $P $F $A)
    (predicate-property $P 
      (number-of-clauses $Insts))
    (set-det)
    (sformat $S "~w=~d~n" 
      (:: 
        (/ $F $A) $Insts)))
  (= (debugln-xfrm (w $E) $S)  
    (sformat $S ~p $E)
    (set-det))
  (= (debugln-xfrm ($ $E) $S)  
    (get-flag $E $Insts)
    (set-det)
    (sformat $S "~w=~d~n" 
      (:: $E $Insts)))
  (= (debugln-xfrm (= $N $V) $S)  
    (integer $V)
    (set-det)
    (sformat $S "~n\t~w\t= ~d " 
      (:: $N $V)))
  (= (debugln-xfrm (= $N $V) $S)  
    (set-det)
    (sformat $S "~n\t~w\t= ~w " 
      (:: $N $V)))
  (= (debugln-xfrm (:: $N) $S)  
    (set-det)
    (debugln-xfrm $N $S))
  (= (debugln-xfrm $C $S)  
    (tok-split $C $S $_)
    (set-det))
  (= (debugln-xfrm $C $S)  
    (compound $C)
    (set-det)
    (sformat $S "~p" 
      (:: $C)))
;
; debugln_xfrm(C,S):- compound(C),compound_name_arguments(C,N,A),debugln_xfrm([N|A],S).
  (= (debugln-xfrm nl 
)   (set-det))
  (= (debugln-xfrm Nil '')  
    (set-det))
  (= (debugln_xfrm  $E $E)  True)


  (= (nlu)  
    (ensure-loaded (library logicmoo-nlu))
    (assert-if-new is-nlu))

  (= (nlu-stop)  
    ( (ensure-loaded (library logicmoo-nlu)) (remove-all-atoms  &self is_nlu)))
;
; :- ensure_loaded(library(logicmoo_nlu/parser_link_grammar)).

  !(nlu *)
;
; convert_corpus:- functor(P,ngram,6), predicate_property(P,number_of_clauses(N)),N>2.

  (= (convert-corpus-file)  
    (tell corpus-file.pl)
    (convert-corpus)
    (told))


  (= (convert-corpus)  
    (convert-corpus-in-mem))


  (= (reconvert-corpus)  
    (so-convert-preds $L)
    (maplist abolish $L)
    (maplist declare-preds $L)
    (convert-corpus-in-mem))


  (= (convert-corpus-in-mem)  
    (make)
    (convert-from-corpus)
    (compute-corpus-extents)
    (nop reconvert-from-trigrams)
    (set-det))


  (= (corpus_stat  corpus_converting)  True) 
  (= (corpus_stat  corpus_nodes)  True) 
  (= (corpus_stat  corpus_node_overlap)  True)
  (= (corpus_stat  corpus_unique_toks)  True) 
  (= (corpus_stat  corpus_total_toks)  True) 
  (= (corpus_stat  corpus_convos)  True)


  (= (set-last-oc $OC)  
    (nb-setval last-oc $OC))

  (= (get-last-oc $OC)  
    (nb-current last-oc $OC))

;
;  convert_from_corpus:- converting(_,string,_),!,forall(converting(XX,string,Val),add_converting_str(XX,Val)).

  (= (convert-from-corpus)  
    (convert-from-corpus (pldata corpus/soap-opera-corpus/soc-corpus.txt)))



  (= (in-temp-dir $G)  
    (must (absolute-file-name (pldata corpus/tmpdata) $Dir (:: (access read) (file-type directory))))
    (setup-call-cleanup 
      (working-directory $X $Dir) 
      (mor-rtrace $G) 
      (working-directory $_ $X)))
 


  (= (convert-from-corpus $Path)  
    (debugln (:: "reading corpus..." $Path))
    (must (absolute-file-name $Path $File (:: (access read))))
    (setup-call-cleanup 
      (forall 
        (corpus-stat $Stat) 
        (set-flag $Stat 0)) 
      (time (, (setup-call-cleanup (open $File read $In (:: (encoding iso-latin-1))) (convert-from-corpus-stream $In) (close $In)) (forall (corpus-stat $Stat) (, (get-flag $Stat $Value) (debugln (= $Stat $Value)))))) save-converting))


  (= (convert-from-corpus-stream $In)  
    (set-flag file-line 0)
    (repeat)
    (det-if-then-else 
      (at-end-of-stream $In) 
      (set-det) 
      (, 
        (inc-flag file-line) 
        (read-line-to-string $In $Str) 
        (get-flag file-line $X) 
        (nb-setval %last-text $Str) 
        (once (add-file-convert $X $Str)) 
        (fail))))



  !(add-history load-converting)

  (= (load-converting)  
    (in-temp-dir load-converting0))

  (= (load-converting0)  
    (so-convert-preds $L)
    (maplist load-converting $L))


  (= (load-converting $MFA)  
    ( (set-det) 
      (compute-module $MFA $M 
        (/ $F $A)) 
      (functor $P $F $A) 
      (with_self  
        (= $MP $M) $P) 
      (atomic-list-concat 
        (:: done- $M - $F - $A .pl) $File) 
      (or 
        (predicate-property $MP 
          (number-of-clauses $Before)) 
        (= $Before 0)) 
      (set-det) 
      (ignore (det-if-then-else (exists-file $File) (ensure-loaded $File) True)) 
      (or 
        (predicate-property $MP 
          (number-of-clauses $After)) 
        (= $After 0)) 
      (set-det) 
      (debugln (with_self  $M (= (/ $F $A) (det-if-then $Before $After))))))


  (= (compute-module $MFA $M $FA)  
    (strip-module $MFA $M0 $FA)
    (compute-m $M0 $M)
    (set-det))


  (= (compute_m  user so_convert)  True)
  (= (compute_m  $M $M)  True)


  (= (save-converting)  
    (in-temp-dir save-converting0))

  (= (save-converting0)  
    (so-convert-preds $L)
    (maplist save-converting $L))

  (= (save-converting $MFA)  
    (set-det)
    (compute-module $MFA $M 
      (/ $F $A))
    (atomic-list-concat 
      (:: done- $M - $F - $A .pl) $File)
    (tell $File)
    (writeq !(encoding iso-latin-1))
    (writeln .)
    (listing (/ $F $A))
    (told))
; ; functor(P,F,A),forall(P,(writeq(P),writeln('.'))),



  (= (save-stat $G)  
    (det-if-then-else 
      (not $G) 
      (add-is-symbol  &self $G) True)
    (nop (, (writeq $G) (writeln .))))


  (= (use_extent  is_word 1)  True) 
  (= (use_extent  tok_split 3)  True) 
  (= (use_extent  trigram 3)  True) 
  (= (use_extent  ngram 5)  True)

  (= (compute-corpus-extents)  
    (debugln "compute corpus extents...")
    (time (forall (use-extent $F $A) (compute-extent $F $A))))


  (= (min-of $X $Y $X)  
    (< $X $Y)
    (set-det)) 
  (= (min_of  $_ $Y $Y)  True)

  (= (max-of $X $Y $X)  
    (> $X $Y)
    (set-det)) 
  (= (max_of  $_ $Y $Y)  True)

  (= (inc-flag $F)  
    (flag $F $X 
      (+ $X 1)))

  (= (compute-extent $F $A)  
    (functor $NGram $F $A)
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
    (set-det))
; ; avoid division by zero; ; adds 20 seconds and is not yet used


  (= (save-extents $_ $_ (= $_ x))  
    (set-det))
  (= (save-extents $F $A (= $X $Y))  
    ( (set-det) (add-is-symbol  &self (extent_props  $F $A $X $Y)))) 
  (= (save-extents $_ $_ $_)  
    (set-det))


  (= (ngram-val $NGram $NN)  
    (ngram-key $NGram $Key)
    (get-flag $Key $NN))


  (= (ngram-inc $NGram)  
    (ngram-inc $NGram $NN))
  (= (ngram-inc $NGram $NN)  
    (ngram-key $NGram $Key)
    (flag $Key $NN 
      (+ $NN 1)))


  (= (ngram-key (tok-split $O $_ $_) $O)  
    (set-det))
  (= (ngram-key (is-word $O) $O)  
    (set-det))
  (= (ngram-key (trigram $A $B $C) $Key)  
    (set-det)
    (join-text 
      (:: $A $B $C) $Key))
  (= (ngram-key (ngram $Loc $A $B $C $D $_) $Key)  
    (set-det)
    (ngram-key 
      (ngram $Loc $A $B $C $D) $Key))
  (= (ngram-key (ngram $Loc (oc $_) $B $C (oc $_)) $Key)  
    (set-det)
    (join-text 
      (:: oc $B $C oc) $Key))
  (= (ngram-key (ngram $Loc (oc $_) $A $B $C) $Key)  
    (set-det)
    (join-text 
      (:: oc $A $B $C) $Key))
  (= (ngram-key (ngram $Loc $A $B $C (oc $_)) $Key)  
    (set-det)
    (join-text 
      (:: $A $B $C oc) $Key))
  (= (ngram-key (ngram $Loc $A $B $C $D) $Key)  
    (join-text 
      (:: $A $B $C $D) $Key))


  (= (join-text $List $Key)  
    (atomic-list-concat $List , $Key))


  (= (save-corpus-stats)  
    (time (, (tell plm.pl) (write '
 :- style-check(- discontiguous).
 :- X= (is-word/2,ngram/6),
    dynamic(X),multifile(X). 
') (listing (:: (/ is-word 2) (/ ngram 6))) (told))))


  (= (qconvert-corpus)  
    (save-corpus-stats)
    (debugln "Compiling now...")
    (time (with_self  (so-convert *) (qcompile plm)))
    (debugln "Loading now...")
    (time (with_self  (so-convert *) (ensure-loaded plm)))
    (debugln "Corpus Ready"))



  (= (add-file-convert $X $Str)  
    (atomic-list-concat 
      (Cons  $E 
        (Cons  $N $List)) 	 $Str)
    (set-det)
    (mor-rtrace (add-converting $X $E $N $List))
    (set-det))
  (= (add-file-convert $X $Str)  
    (atomic-list-concat 
      (Cons  $E $List) 	 $Str)
    (set-det)
    (mor-rtrace (add-converting $X $E '' $List))
    (set-det))


  (= (begin-scene)  
    ( (remove-all-atoms  &self 
        (:  tmp 
          (in_scene  $_))) 
      (nop (wdmsg begin-scene)) 
      (current-so-file $File) 
      (debugln (= file $File)) 
      (current-so-show $Show) 
      (current-so-file $File) 
      (atom-concat $Show -scene- $SS) 
      (gensym $SS $SceneName) 
      (nb-setval current-scene $SceneName) 
      (nop (format '~N:- ~q.~n' (:: (begin-scene $SceneName)))) 
      (set-det)))
; ;listing(tmp:in_scene/1),


  (= (delist-lists (:: $X) $Y)  
    (is-list $X)
    (set-det)
    (delist-lists $X $Y))
  (= (delist_lists  $X $X)  True)
;
; send_scene(actor(Info),SceneName):- writeq(Info),write(', '),
;
; send_scene(Info,SceneName):- fail, format('~N~q.~n',[in_scene(Info,SceneName)]).



  (= (tokenize-sents-ww $Who $What $S $S9)  
    (must-det-l (, (tokenize-atom $S $S2) (set-det) (fix-token-sents-pass1 $S2 $S3) (set-det) (fix-token-sents $Who $What $S3 $S4) (set-det) (flatten $S4 $S5) (maplist de-s $S5 $S6) (delist-lists $S6 $S9))))


  (= (toks_join1  -)  
    (empty))

  (= (toks-join $H1 $H1)  
    (toks-join1 $H1)
    (set-det))
  (= (toks-join $H1 .)  
    (join-with-dot $H1)
    (set-det))
  (= (toks_join  ' s)  True)
  (= (toks-join ' $A)  
    (nop (atom-length $A 1)))
;
; toks_join(_,'-').
;
; toks_join('-',_).
  (= (toks-join $H1 $H2 $H12)  
    (toks-join $H1 $H2)
    (atom-concat $H1 $H2 $H12))


  (= (never_toks_join1  ")  True)
  (= (never_toks_join1  ])  True)
  (= (never_toks_join1  [)  True)

  (= (never-toks-join $H1 $H2)  
    (or 
      (never-toks-join1 $H1) 
      (never-toks-join1 $H2))
    (set-det))
  (= (never_toks_join  $_ ')  True)
  (= (never_toks_join  ' $_)  True)
  (= (never_toks_join  . ')  True)
  (= (never_toks_join  ' .)  True)


  (= (a-number $A)  
    (number $A)
    (set-det))
  (= (a-number $A)  
    (atom $A)
    (atom-number $A $_))


  (= (dashes $H2)  
    (or 
      (== $H2 -) 
      (== $H2 --)))

;
; :- multifile(contraction/4).

  (= (register-all-lexical-items $A $B)  
    (wdmsg (register-all-lexical-items $A $B)))

  !(ensure-loaded /opt/logicmoo-workspace/packs-xtra/logicmoo-chat/npc/Assets/NL/contractions.prolog)

  (= (contraction $_ $A $B $C $D)  
    (contraction $A $B $C $D)) 
  (= (contraction r $U did $U d)  
    (downcase-atom $U $D)
    (contract-is $D)
    (set-det))
  (= (contraction r $U is $U s)  
    (downcase-atom $U $D)
    (contract-is $D)
    (set-det))
  (= (contraction r $U us $U s)  
    (downcase-atom $U $D)
    (contract-us $D)
    (set-det))
  (= (contraction r $U is $U s)  
    (downcase-atom $U $D)
    (contract-is $D)
    (set-det))
  (= (contraction  r $U am $U m)  True)
  (= (contraction  r $U are $U re)  True)
  (= (contraction  r $U are $U r)  True)
  (= (contraction  r $U have $U ve)  True)
  (= (contraction  r did not didn t)  True)
  (= (contraction  r $U them $U em)  True)
  (= (contraction  r $U has $U est)  True)
  (= (contraction  r $U has $U st)  True)
  (= (contraction  r $U will $U ll)  True)
  (= (contraction  r $U would $U d)  True)
  (= (contraction  r $U about $U bout)  True)
  (= (contraction  r you all y all)  True)
  (= (contraction  r you know y know)  True)
  (= (contraction r $Could not $Couldn t)  
    (atom-concat $Could n $Couldn)
    (nop (once (or (atom-concat $_ d $Could) (or (atom-concat $_ t $Could) (atom-concat $_ s $Could))))))
  (= (contraction $R $Watching $U $Watchin $U)  
    (\== $R c)
    (\== $U s)
    (atom-concat $A in $Watchin)
    (atom-length $A $L)
    (> $L 1)
    (atom-concat $Watchin g $Watching))
  (= (contraction  r do you d ya)  True)
  (= (contraction  r come on c mon)  True)
  (= (contraction r $A $B $C $D)  
    (downcase-atom $C $DC)
    (\== $C $DC)
    (contraction r $DA $B $DC $D)
    (toPropercase $DA $A))
  (= (contraction r $A $B $C $D)  
    (contraction c $A $B $C $D))
;
;  contraction(r,can, not, can, t)

  (= (contract_us  let)  True)

  (= (contract_is  it)  True)
  (= (contract_is  how)  True)
  (= (contract-is $X)  
    (atom-concat wh $_ $X))
  (= (contract-is $X)  
    (atom-concat th $_ $X))


  (= (fix-token-sents-pass1 (Cons  $H1 (Cons  - (Cons  $H2 $T))) (Cons  $H12 $Out))  
    (not (dashes $H1))
    (not (dashes $H2))
    (atomic-list-concat 
      (:: $H1 - $H2) $H12)
    (set-det)
    (fix-token-sents-pass1 $T $Out))
  (= (fix-token-sents-pass1 (Cons  - (Cons  - $T)) $Out)  
    (set-det)
    (fix-token-sents-pass1 
      (Cons  -- $T) $Out))
  (= (fix-token-sents-pass1 (Cons  $H1 (Cons  . (Cons  $H2 $T))) (Cons  $H12 $Out))  
    (a-number $H1)
    (a-number $H2)
    (atomic-list-concat 
      (:: $H1 . $H2) $H12)
    (set-det)
    (fix-token-sents-pass1 $T $Out))

  (= (fix-token-sents-pass1 (Cons  ' (Cons  s (Cons  $ING $T))) $Out)  
    (atom-concat $_ ing $ING)
    (fix-token-sents-pass1 
      (Cons  is 
        (Cons  $ING $T)) $Out))
  (= (fix-token-sents-pass1 (Cons  $A (Cons  ' (Cons  $B $T))) $Out)  
    (contraction r $C $D $A $B)
    (set-det)
    (fix-token-sents-pass1 
      (Cons  $C 
        (Cons  $D $T)) $Out))
;
; fix_token_sents_pass1([A,B|T],Out):- contraction(c,C,D,A,B),!,fix_token_sents_pass1([C,D|T],Out).
  (= (fix-token-sents-pass1 (Cons  ' (Cons  $Ve $T)) $Out)  
    (contraction r we $Have we $Ve)
    (fix-token-sents-pass1 
      (Cons  $Have $T) $Out))
  (= (fix-token-sents-pass1 (Cons  ' (Cons  s $T)) $Out)  
    (fix-token-sents-pass1 
      (Cons  ''s' $T) $Out))
  (= (fix-token-sents-pass1 (Cons  ' (Cons  t $T)) $Out)  
    (fix-token-sents-pass1 
      (Cons  ''t' $T) $Out))
  (= (fix-token-sents-pass1 (Cons  ( $T) $Out)  
    (fix-token-sents-pass1 
      (Cons  [ $T) $Out))
  (= (fix-token-sents-pass1 (Cons  ) $T) $Out)  
    (fix-token-sents-pass1 
      (Cons  ] $T) $Out))
  (= (fix-token-sents-pass1 (Cons  $E (Cons  . (Cons  $R (Cons  . $T)))) $Out)  
    (atom-length $E 1)
    (atom-length $R 1)
    (\== $R .)
    (atomic-list-concat 
      (:: $E . $R .) $ER)
    (set-det)
    (fix-token-sents-pass1 
      (Cons  $ER $T) $Out))
  (= (fix-token-sents-pass1 (Cons  . (Cons  . $T)) $Out)  
    (set-det)
    (fix-token-sents-pass1 
      (Cons  . $T) $Out))
  (= (fix-token-sents-pass1 (Cons  ? (Cons  ? $T)) $Out)  
    (set-det)
    (fix-token-sents-pass1 
      (Cons  ? $T) $Out))
  (= (fix-token-sents-pass1 (Cons  $H1 (Cons  : (Cons  $H2 $T))) (Cons  $H12 $Out))  
    (a-number $H1)
    (a-number $H2)
    (atomic-list-concat 
      (:: $H1 : $H2) $H12)
    (set-det)
    (fix-token-sents-pass1 $T $Out))
  (= (fix-token-sents-pass1 (Cons  $H1 (Cons  $H2 $T)) $Out)  
    (not (never-toks-join $H1 $H2))
    (toks-join $H1 $H2 $H12)
    (set-det)
    (fix-token-sents-pass1 
      (Cons  $H12 $T) $Out))
  (= (fix-token-sents-pass1 (Cons  $H1 (Cons  $H1 $T)) $Out)  
    (not (never-toks-join $H1 $H1))
    (set-det)
    (fix-token-sents-pass1 
      (Cons  $H1 $T) $Out))
  (= (fix-token-sents-pass1 (Cons  $H1 $T) (Cons  $H1 $TT))  
    (set-det)
    (fix-token-sents-pass1 $T $TT))
  (= (fix_token_sents_pass1  () ())  True)


  (= (join-with-dot $W)  
    (atom $W)
    (atom-number $W $_))
  (= (join_with_dot  St)  True)
  (= (join_with_dot  Dr)  True)
  (= (join_with_dot  Mr)  True)
  (= (join_with_dot  Ms)  True)
  (= (join_with_dot  Mrs)  True)
;
; join_with_dot(W):-atom_length(W,1).


  (= (s-puct $W)  
    (member $W 
      (:: -- . ? 
        (set-det) ])))


  (= (is_first_word  $_ [ action)  True)
  (= (is-first-word $W $R $W)  
    ( (with_self  
        (nldata-dictionary-some01 *) 
        (atom-or-string $R)) 
      (downcase-atom $R $D) 
      (\== $R $D)))

;
;  fix_token_sents(Who,What,[X],Y):- is_list(X),!,fix_token_sents(Who,What,X,Y).

  (= (fix-token-sents $_ $_ Nil Nil)  
    (set-det))
  (= (fix-token-sents $Who $What $In $Out)  
    (stack-depth $X)
    (> $X 5000)
    (dumpST)
    (wdmsg (fix-token-sents $Who $What $In $Out))
    (break))

  (= (fix-token-sents $_ $_ (Cons  $Who (Cons  and (Cons  $Actor2 (Cons  : $Rest)))) $Out)  
    (fix-token-sents 
      (together (:: $Who $Actor2)) speak $Rest $Out)
    (set-det))
  (= (fix-token-sents $_ $_ (Cons  $Who (Cons  $And (Cons  $Actor2 (Cons  : $Rest)))) $Out)  
    (fix-token-sents 
      (fle (:: $Who $And $Actor2)) speak $Rest $Out)
    (set-det))
  (= (fix-token-sents $_ $_ (Cons  $Who (Cons  $Actor2 (Cons  : $Rest))) $Out)  
    (fix-token-sents 
      (fl (:: $Who $Actor2)) speak $Rest $Out)
    (set-det))
  (= (fix-token-sents $_ $_ (Cons  $Who (Cons  : $Rest)) $Out)  
    (fix-token-sents $Who speak $Rest $Out)
    (set-det))

  (= (fix-token-sents $_ $_ (Cons  [ (Cons  $Who (Cons  and (Cons  $Actor2 $Rest)))) $Out)  
    (is-pers-name $Who)
    (fix-token-sents 
      (together (:: $Who $Actor2)) action 
      (Cons  [ $Rest) $Out)
    (set-det))
  (= (fix-token-sents $_ $_ (Cons  [ (Cons  $Who $Rest)) $Out)  
    (is-pers-name $Who)
    (fix-token-sents $Who action 
      (Cons  [ $Rest) $Out)
    (set-det))
  (= (fix-token-sents $Who $What (Cons  [ $Rest) (:: $Out1 $Out2))  
    (append $L 
      (Cons  ] $R) $Rest)
    (fix-token-sents $Who action $L $Out1)
    (set-det)
    (fix-token-sents $Who $What $R $Out2)
    (set-det))

  (= (fix-token-sents $Who $What $Rest (:: $Out1 $Out2))  
    (append $L 
      (Cons  [ $R) $Rest)
    (not (member ] $L))
    (\== $L Nil)
    (fix-token-sents $Who $What $L $Out1)
    (fix-token-sents $Who $What 
      (Cons  [ $R) $Out2)
    (set-det))
  (= (fix-token-sents $_ action (Cons  $Who (Cons  and (Cons  $Actor2 $Rest))) $Out)  
    (is-pers-name $Who)
    (fix-token-sents 
      (together (:: $Who $Actor2)) action $Rest $Out)
    (set-det))
; /*; fix_token_sents(Who,What,Rest,[Out1,Out2]):- append(L,[']'|R],Rest), \+ member('[',L),;   append(L,[']'],LR),;   fix_token_sents(Who,LR,Out1),;   fix_token_sents(Who,What,R,Out2), !.; */
  (= (fix-token-sents $_ action (Cons  $Who $Rest) $Out)  
    (is-pers-name $Who)
    (fix-token-sents $Who action $Rest $Out)
    (set-det))

  (= (fix-token-sents $Who $What $SSS (:: $Out1 $Out2))  
    (append $L 
      (Cons  $L2 
        (Cons  $W 
          (Cons  $R $RR))) $SSS)
    (not (join-with-dot $L2))
    (s-puct $W)
    (append $L 
      (:: $L2 $W) $LS)
    (is-first-word $What $R $Waht2)
    (fix-token-sents $Who $What $LS $Out1)
    (set-det)
    (fix-token-sents $Who $Waht2 
      (Cons  $R $RR) $Out2)
    (set-det))


  (= (fix-token-sents $Who $What (:: $X) $Y)  
    (is-list $X)
    (set-det)
    (fix-token-sents $Who $What $X $Y))
;
; ;fix_token_sents(Who,What,['[',Actor,and,Actor2|Rest], together([Actor,Actor2]):Out):- is_pers_name(Actor),
;
;    fix_token_sents(Who,What,['['|Rest],Out).

;
; fix_token_sents(_,What,['[',Actor|Rest],Out):- is_pers_name(Actor),fix_token_sents(Actor,What,['['|Rest],Out).


  (= (fix_token_sents  $Who $_ (Cons  [ $Rest) ((s  $Who action $Rest)))  True)
  (= (fix-token-sents unk action $Out1 $Out1)  
    (set-det))
  (= (fix-token-sents $Who $What $Out1 (s $Who $What $Out1))  
    (set-det))



  (= (de_s  $O $O)  True)

  (= (maybe-delist-into-action $Who (:: [ $Before ]) (with_self  $Who (action (:: $Before))))  
    (set-det))
  (= (maybe-delist-into-action unk $Before $Before)  
    (set-det))
  (= (maybe_delist_into_action  $Who $Before (:  $Who (action  $Before)))  True)

;
; fix_token_sents(Who,Info,Actor,After):- atomic_list_concat([Actor,After],' ',Info),!.

  (= (send-scene (action $Info) $After)  
    (atomic $Info)
    (tokenize-sents-ww world action $Info $Toks)
    (fix-token-sents unk action $Toks $After)
    (set-det))
  (= (send-scene (act-speak Airdate $_) Nil)  
    (set-det))
  (= (send-scene (act-speak $W $S) $SS)  
    (tokenize-sents-ww $W speak $S $SS)
    (set-det))
  (= (send-scene (narrate $W $S) $SS)  
    (tokenize-sents-ww $W narrate $S $SS)
    (set-det))
;
; send_scene(act_speak(W,S),_SceneName,W:speak:S)
;
; send_scene(Info,SceneName,unk:unk:SInfo):- append_term(Info,SceneName,SInfo).
  (= (send_scene  $Info $Info)  True)



  (= (write-scene-events (:: $E))  
    (writeqf $E)
    (set-det))
  (= (write-scene-events (Cons  (s $_ $_ Nil) $Events))  
    (write-scene-events $Events))
  (= (write-scene-events (Cons  $E $Events))  
    (writeqf $E)
    (set-det)
    (format ',~n   ')
    (write-scene-events $Events))
  (= (write-scene-events $E)  
    (writeqf $E))


  (= (name-dvars2 $A $O)  
    (toPropercase $A $P)
    (replace-in-string ' ' - $P $O))

;
; subst_name_vars_body(_,A,A):-!.
;
; subst_name_vars_body([],A,A):-!.

  (= (subst-name-vars-body $Vs $A $A)  
    (member-eq0 $A $Vs)
    (set-det))
  (= (subst-name-vars-body $_ $A $O)  
    (not (compound $A))
    (set-det)
    (= $A $O))
  (= (subst-name-vars-body $Vs $A $O)  
    (is-list $A)
    (set-det)
    (maplist 
      (subst-name-vars-body $Vs) $A $O))
  (= (subst-name-vars-body $Vs $A $O)  
    (compound-name-arguments $A $N $Args)
    (maplist 
      (subst-name-vars-body $Vs) $Args $ArgsO)
    (compound-name-arguments $O $N $ArgsO)
    (set-det))


  (= (writeqf (s $_ $_ Nil))  
    (set-det))
  (= (writeqf (s world action $Act))  
    (set-det)
    (writeqf (action $Act)))
  (= (writeqf (s $W action $Act))  
    (set-det)
    (writeqf (with_self  $W (action $Act))))

  (= (writeqf (s $A speak $F))  
    (set-det)
    (writeqf (with_self  $A $F)))
  (= (writeqf (with_self  $A $F))  
    (not (not (or (atomic $F) (, (sub-term $E $F) (compound $E) (\= $E $_) (not (is-list $E))))))
    (set-det)
    (ansicall yellow 
      (faf $A $F)))
  (= (writeqf (with_self  $A $F))  
    (set-det)
    (ansicall cyan 
      (faf $A $F)))
  (= (writeqf $E)  
    (ansicall yellow 
      (writeq $E)))


  (= (faf $A $F)  
    (format ~q:~t~15|~q 
      (:: $A $F)))

;
; finish_scene:- \+ nb_current(current_scene,_SceneName), once(begin_scene), fail.

  (= (finish-scene)  
    (mor-rtrace (, (nb-current current-scene $SceneName) (findall $Info (with_self  (tmp *) (in-scene (actor $Info))) $ActorsUL) (sort $ActorsUL $ActorsL) (length $ActorsL $L)))
    (dmsg (finish-scene $ActorsL))
    (< $L 20)
    (findall $Stuff 
      (, 
        (remove-is-symbol  &self 
          (:  tmp 
            (in_scene  $Info))) 
        (\= $Info 
          (actor $_)) 
        (send-scene $Info $Stuff)) $EventsL)
    (mor-rtrace (, (flatten $EventsL $Events) (\== $Events Nil) (atomic-list-concat $ActorsL - $AL) (atomic-list-concat (:: $SceneName with $L characters $AL) - $SceneNameA) (maplist name-dvars2 $ActorsL $ActorsLV) (subst-name-vars-body $ActorsL $Events $VEvents) (format "~Nscene_info( ~q, ~q, ~q, ~q, [~n~n   " (:: $SceneNameA $L $ActorsL $ActorsLV)) (write-scene-events $VEvents) (format "~N ]).~n") (nop (format '~N:- ~q.~n~n~n' (:: (end-scene $SceneName))))))
    (set-det)
    (nl)
    (nl))
; ;listing(tmp:in_scene/1),
  (= (finish-scene)  
    (dmsg finish-scene)
    (set-det))

;
; find_monologs:- 


  (= (scene-info)  
    (scene-info $A $B $Actors1 $ActorsLV $List)
    (> $B 1)
    (scene-info $AA $B $_ $ActorsLV $List)
    (@< $AA $A)
    (wdmsg (+ $A $AA))
    (= $Actors1 $ActorsLV)
    (wdmsg $List))





  (= (maybe-trim-white $W $A)  
    (always-trim-white $W $A)
    (\== $A $W)
    (set-det))

  (= (always-trim-white $W $A)  
    (split-string $W "" " \t\n" 
      (:: $S))
    (atom-string $A $S)
    (set-det))


  (= (set-show $Show)  
    (always-trim-white $Show $ShowT)
    (nb-setval so-show $ShowT)
    (set-det))


  (= (show-date (:: $M $DY) $ShowDate)  
    (atom $DY)
    (atomic-list-concat 
      (:: $D $Y) . $DY)
    (show-date 
      (:: $M $D $Y) $ShowDate))
  (= (show-date (:: $M $D $Y) $ShowDate)  
    (atom $Y)
    (atom-number $Y $YY)
    (atom-number $M $MM)
    (atom-number $D $DD)
    (show-date 
      (:: $MM $DD $YY) $ShowDate))
  (= (show-date (:: $M $D $Y) $ShowDate)  
    (number $Y)
    (> $Y 70)
    (sformat $ShowDate "19~|~`0t~d~2+_~|~`0t~d~2+_~|~`0t~d~2+" 
      (:: $Y $M $D)))
  (= (show-date (:: $M $D $Y) $ShowDate)  
    (number $Y)
    (< $Y 70)
    (sformat $ShowDate "20~|~`0t~d~2+_~|~`0t~d~2+_~|~`0t~d~2+" 
      (:: $Y $M $D)))
  (= (show-date (:: $S $E) $ShowDate)  
    (atom $S)
    (atom-number $S $SS)
    (atom-number $E $EE)
    (sformat $ShowDate "S~|~`0t~d~2+E~|~`0t~d~2+" 
      (:: $SS $EE)))
  (= (show-date $List $ShowDate)  
    (atomic-list-concat $List - $ShowDate))

  (= (more2  (Cons  $_ (Cons  $_ $_)))  True)

  (= (find-date $List)  
    (is-list $List)
    (set-det)
    (atomic-list-concat $List ' ' $Data)
    (set-det)
    (find-date $Data))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List season $Data)
    (last $List $Date)
    (set-det)
    (find-date $Date))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List -trans- $Data)
    (last $List $Date)
    (set-det)
    (find-date $Date))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List trans-da- $Data)
    (last $List $Date)
    (set-det)
    (find-date $Date))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List .shtml $Data)
    (= $List 
      (Cons  $Date $_))
    (set-det)
    (find-date $Date))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List .html $Data)
    (= $List 
      (Cons  $Date $_))
    (set-det)
    (find-date $Date))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List .htm $Data)
    (= $List 
      (Cons  $Date $_))
    (set-det)
    (find-date $Date))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List /ep $Data)
    (last $List $Date)
    (set-det)
    (find-date $Date))
  (= (find-date $Data)  
    (more2 $List)
    (atomic-list-concat $List / $Data)
    (last $List $Date)
    (set-det)
    (find-date $Date))
  (= (find-date $Date)  
    (more2 $List)
    (atomic-list-concat $List - $Date)
    (show-date $List $ShowDate)
    (set-det)
    (find-date $ShowDate))
  (= (find-date $Date)  
    (add-show-date $Date)
    (or 
      (nb-current so-show $Show) 
      (= $Show @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@))
    (set-det)
    (nop (debugln (= $Show $Date)))
    (current-so-file $File)
    (debugln (= file $File)))

  (= (add-scene-cline $A $E)  
    ( (atom-string $E $S) 
      (asserta-if-new (with_self  (tmp *) (in-scene (narrator $A)))) 
      (add-is-symbol  &self 
        (:  tmp 
          (in_scene  
            (action  
              (narrate  $A $S)))))))

  (= (add-scene-line $_ '')  
    (set-det))
  (= (add-scene-line Airdate $_)  
    (set-det))
  (= (add-scene-line $A $E)  
    ( (atom-string $E $S) 
      (asserta-if-new (with_self  (tmp *) (in-scene (actor $A)))) 
      (asserta-if-new (is-pers-name (actor $A))) 
      (add-is-symbol  &self 
        (:  tmp 
          (in_scene  
            (act_speak  $A $S))))))

  (= (add-scene-event $E)  
    ( (atom-string $E $S) (add-is-symbol  &self (:  tmp (in_scene  (action  $S))))))


  (= (add-show-date $Date)  
    (nb-setval so-date $Date))


  (= (current-so-show $SF)  
    (nb-current so-show $Show)
    (nb-current so-date $Date)
    (replace-in-string 
      (:: (= ' ' -)) $Show $ShowName)
    (format 
      (atom $SF) "~w_~w" 
      (:: $ShowName $Date)))

  (= (current-so-file $File)  
    (current-so-show $SF)
    (format 
      (atom $File) "~w.nldata" 
      (:: $SF)))


  (= (trans-change)  
    (nop (mor-rtrace finish-scene)))

  (= (scene-change)  
    (mor-rtrace finish-scene)
    (set-det)
    (mor-rtrace begin-scene)
    (set-det)) 

  (= (add-converting $X $LINEPiper $E $List)  
    (atom-concat 'LINE ' $Piper $LINEPiper)
    (set-det)
    (add-converting $X LINE $Piper 
      (Cons  $E $List)))
  (= (add-converting $X $LINEPiper $E $List)  
    (is-list $List)
    (\== $List Nil)
    (set-det)
    (atomic-list-concat $List ' ' $A)
    (set-det)
    (add-converting $X $LINEPiper $E $A))
  (= (add-converting $X $LINEPiper $W $List)  
    (maybe-trim-white $W $A)
    (set-det)
    (add-converting $X $LINEPiper $A $List))
  (= (add-converting $X $LINEPiper $E $W)  
    (maybe-trim-white $W $A)
    (set-det)
    (add-converting $X $LINEPiper $E $A))
  (= (add-converting $_ REG-FORMAT '' '')  
    (set-det)
    (trans-change))
  (= (add-converting $_ DECODE-FORMAT '' '')  
    (set-det)
    (trans-change))
  (= (add-converting $_ YEARINDEX $Show $_)  
    (set-show $Show)
    (set-det)
    (trans-change))
  (= (add-converting $_ PYEARINDEX $Show $_)  
    (set-show $Show)
    (set-det)
    (trans-change))
  (= (add-converting $_ TLINK $Show $Data)  
    (finish-scene)
    (set-show $Show)
    (find-date $Data)
    (set-det))
  (= (add-converting $_ TRANSCRIPT $Show $Data)  
    (set-show $Show)
    (find-date $Data)
    (set-det)
    (begin-scene))
  (= (add-converting $_ SCENE-EVENT [SCENE-CHANGE] '')  
    (set-det)
    (scene-change))
  (= (add-converting $_ SCENE-CHANGE $_ $_)  
    (set-det)
    (scene-change))
  (= (add-converting $_ SCENECHANGE $_ $_)  
    (set-det)
    (scene-change))
  (= (add-converting $_ $_ SCENECHANGE $_)  
    (set-det)
    (scene-change))
  (= (add-converting $_ $_ [SCENECHANGE] $_)  
    (set-det)
    (scene-change))
;
;  add_converting(X,E,N,List):- debugln(add_converting_now(X,E,N,List)),fail.
  (= (add-converting $_ SCENE-EVENT $Event '')  
    (mor-rtrace (add-scene-event $Event))
    (set-det))
  (= (add-converting $_ LINE $Piper $Said)  
    (mor-rtrace (add-scene-line $Piper $Said))
    (set-det))
  (= (add-converting $_ CLINE $CPiper $Said)  
    (mor-rtrace (add-scene-cline $CPiper $Said))
    (set-det))

  (= (add-converting $X $E $N $List)  
    (debugln (add-converting $X $E $N $List)))


  (= (mor-rtrace $G)  
    (once $G)
    (set-det))
  (= (mor-rtrace $G)  
    (rtrace $G))

  (= (add-punct $X $X)  
    (last $X $E)
    (member $E 
      (:: ? . 
        (set-det))))
; /*;  flag(speech_act,A,A+1),;  get_flag(corpus_convos,Z),;  XX is ((Z+1)*100_000_000_000)+(A*10_000_000)+X, ;  add_converting_str(XX,Str).; */
  (= (add-punct $X $Y)  
    (append $X 
      (:: .) $Y))


  (= (add-converting-str $_ "XXXXXXXXXXX")  
    (inc-flag corpus-convos)
    (set-flag speech-act 1)
    (set-det))
; ;C = 100_000_000_000, Buffer is floor(XX/C)*C + 01111111111,  ; ;ignore(add_conversation_converting(Buffer)), !,
;
; add_converting_str(XX,Str):- 1 is XX mod 2, !, add_converting_said(said,"Al",XX,Str),!. 
;
; add_converting_str(XX,Str):- add_converting_said(said,"Jo",XX,Str),!. 


  (= (add-converting-str $XX $Str)  
    (must-det-l (, (string $Str) (assert-converting-v $XX string $Str) (tokenize-atom $Str $Toks) (set-det) (pretok $Toks $PreToks0) (add-punct $PreToks0 $PreToks) (text-to-tree $PreToks $Tree) (assert-converting $XX text-to-tree $Tree) (unphrasify $Tree $List) (assert-converting $XX unphrasify $List) (tree-to-toks $List $PostToks) (set-det) (assert-converting $XX tree-to-toks $PostToks) (add-converting-toks $XX $PostToks))))
; ;writeq(sample_tree(Tree)),writeln('.'),

 

  (= (tree-to-toks)  
    (mmake)
    (forall 
      (sample-tree $Tree) 
      (tree-to-toks1 $Tree)))
; /* Old Way; add_converting_str(XX,Str):- ;  tokenize_atom(Str,Toks),;  maplist(downcase_atom,Toks,TokList), ;  pretok(TokList,PreToks),!,;  add_converting_toks(XX,PreToks).;  ; */

  (= (sample_tree  (SEQBAR (CORENLP (S (CC And) (ADVP (RB then)) (NP (NP (PRP$ her) (NN son)) (, ,) (NP (NNP Ben)) (, ,)) (VP (VP (VBZ turns) (NP (DT all) (NNP Sith))) (CC and) (VP (VBZ joins) (NP (DT the) (JJ dark) (NN side)))) (. .))) (CORENLP (S (PRN (S (NP (DT That)) (VP (VBD had) (S (VP (TO to) (VP (VB have) (VP (VBN factored) (PP (IN into) (NP (PRP$ her) (NNS reasons))) (S (VP (TO to) (VP (VB stay) (ADVP (RB away)) (PP (IN from) (NP (NP (DT the) (NN call)) (PP (IN of) (NP (DT the) (NN force))))))))))))))) (, ,) (VB do) (RB not) (NP (PRP you)) (VP (VB think)) (. ?)))))  True)
  (= (sample_tree  (CORENLP (S (NP (PRP I)) (VP (VB hate) (S (VP (TO to) (VP (VB say) (S (NP (PRP it)) (VP (VB buuut.)) (, ,)))))) (. .)) (S (VP (, ,)) (. .))))  True)
  (= (sample_tree  (SEQBAR (CORENLP (SBAR (NP (WP who)) (S (VP (MD would) (VP (VB pick) (NP (NN kylo))))) (. ?))) (CORENLP (S (ADVP (RB definitely)) (ADVP (RB not)) (NP (PRP me))))))  True)
  (= (sample_tree  (SEQBAR (CORENLP (S (S (NP (PRP He)) (VP (VBD was) (NP (NP (NNP Luke) (POS ''s')) (NNP Padwan)))) (, ,) (CC but) (S (NP (PRP he)) (VP (VBD turned))) (. .))) (SEQBAR (S (NP (PRP It)) (VP (AUX has) (RB not) (VP (AUX been) (VP (VBN shown) (FRAG (WHADVP (WRB why)))))) (. .)) (CORENLP (S (PRN (S (NP (PRP He)) (VP (VBZ is) (ADVP (RB no) (RBR longer)) (NP (NNP Jedi))))) (, ,) (NP (PRP he)) (VP (VBZ is) (ADJP (JJ sith)) (ADVP (RB now))))))))  True)
  (= (sample_tree  (CORENLP (SBAR (INTJ (UH Well)) (, ,) (SBAR (IN if) (S (NP (PRP it)) (VP (VBZ is) (NP (NNP Rey))))) (, ,) (ADVP (RB then)) (WHADVP (WRB why)) (S (VBD did) (NP (PRP it)) (RB not) (VP (VB wake) (SBAR (WHADVP (WRB when)) (S (NP (NNP Klyo)) (VP (VBD came) (PP (IN into) (NP (NN power)))))))))))  True)
  (= (sample_tree  (CORENLP (SBAR (NP (WP Who)) (S (VBZ is) (NP (PRP$ your) (JJ favorite) (NN character))) (. ?))))  True)
  (= (sample_tree  (SEQBAR (CORENLP (S (INTJ (UH Well)) (, ,) (NP (PRP it)) (VP (VBZ ''s') (NP (DT a) (NN movie))) (. .))) (CORENLP (S (NP (PRP He)) (VP (MD could) (VP (VB show) (PRT (RP up))))))))  True)
  (= (sample_tree  (CORENLP (S (VB Are) (NP (PRP you)) (NP (NP (DT a) (NN fan)) (PP (IN of) (NP (DT the) (NML (NNP Star) (NNPS Wars)) (NN series)))) (. ?))))  True)
  (= (sample_tree  (CORENLP (S (NP (PRP I)) (VP (VB think) (SBAR (S (NP (PRP he)) (VP (VBD was) (ADVP (RB just)) (VP (VBG giving) (NP (DT a) (JJ giant) (JJ middle) (NN finger)) (PP (IN to) (NP (DT the) (NN audience)))))))))))  True)
  (= (sample_tree  (CORENLP (S (ADVP (RB Obviously)) (NP (NNP Darth) (NNP Vader)) (VP (VBZ is) (NP (NP (DT the) (JJS best)) (CC and) (NP (NP (DT the) (JJ original) (JJ bad) (NN guy)) (PP (IN of) (NP (NNP Star) (NNPS Wars)))))))))  True)
  (= (sample_tree  (SEQBAR (CORENLP (S (NP (NNP James) (NNP Earl) (NNP Jones)) (VP (VBZ does) (NP (DT the) (NN voice)) (, ,) (SBAR (RB even) (IN though) (S (NP (PRP he)) (VP (VBZ is) (RB not) (VP (VBN listed) (PP (IN in) (NP (DT the) (NNS credits)))))))) (. .))) (CORENLP (S (NP (NNP David) (NNP Prowse)) (VP (VBD did) (NP (DT the) (NN acting)))))))  True)
  (= (sample_tree  (CORENLP (S (S (NP (PRP I)) (VP (VB ''m') (ADVP (RB still)) (ADJP (RB really) (JJ bummed) (PP (IN about) (NP (DT that)))))) (, ,) (CC but) (S (NP (PRP I)) (VP (VB ''m') (ADJP (JJ sure) (SBAR (S (NP (PRP they)) (VP (MD ''ll') (VP (VB figure) (NP (NN something)) (PRT (RP out)) (PP (IN for) (NP (NP (NNP Leia)) (PP (IN in) (NP (DT The) (JJ Last) (NNP Jedi)))))))))))))))  True)

  (= (tree-to-toks1 $Tree)  
    (print-tree-nl (= i $Tree))
    (unphrasify $Tree $UTree)
    (print-tree-nl (= (o)  $UTree))
    (nop (, (visible-rtrace (:: (+ call) (+ exit)) (tree-to-toks $Tree $O)) (notrace (wdmsg $O)))))



  (= (contains-phrase $Ls)  
    (sub-term $E $Ls)
    (atom $E)
    (or 
      (is-penn-long $E) 
      (== $E NP)))
  (= (contains-phrase $Ls)  
    (member $E $Ls)
    (is-list $E)
    (member $Sl $E)
    (is-list $Sl))


  (= (unphrasify Nil Nil)  
    (set-det))
;
; unphrasify([S|Ls], FlatL) :- is_penn_long(S), unphrasify(Ls, FlatL).
  (= (unphrasify (Cons  VP $Ls) $FlatL)  
    (set-det)
    (unphrasify $Ls $FlatL))
  (= (unphrasify (Cons  PP $Ls) $FlatL)  
    (set-det)
    (unphrasify $Ls $FlatL))
  (= (unphrasify (Cons  $S $Ls) (Cons  (mark $S) $FlatL))  
    (is-penn-long $S)
    (contains-phrase $Ls)
    (set-det)
    (unphrasify $Ls $FlatL))
  (= (unphrasify (Cons  $S $Ls) $FlatL)  
    (== $S NP)
    (sub-var NP $Ls)
    (unphrasify $Ls $FlatL))
  (= (unphrasify (Cons  $L $Ls) (Cons  $L $NewLs))  
    (dont-flatten $L)
    (set-det)
    (unphrasify $Ls $NewLs)
    (set-det))
  (= (unphrasify (Cons  $L $Ls) $FlatL)  
    (unphrasify $L $NewL)
    (unphrasify $Ls $NewLs)
    (append $NewL $NewLs $FlatL))
  (= (unphrasify  $L ($L))  True)


  (= (not-is-list $X)  
    (not (is-list $X)))


  (= (dont-flatten (Cons  $_ $L))  
    (sub-var NP $L)
    (set-det)
    (fail))
  (= (dont-flatten (Cons  $S $_))  
    (is-penn-long $S)
    (set-det)
    (fail))
  (= (dont-flatten (Cons  $S $_))  
    (is-penn-tag $S))


  (= (tree-to-toks $X $Y)  
    (notrace (unphrasify $X $XX))
    (tree-to-toks s $XX $YY)
    (cleanup-toks $YY $Y))
  (= (tree-to-toks $C $X $Y)  
    (tree-to-tokz $C $X $M)
    (set-det)
    (notrace (flatten (:: $M) $Y)))


  (= (cleanup_toks  () ())  True)
  (= (cleanup-toks (Cons  (mark $_) $YY) $Y)  
    (set-det)
    (cleanup-toks $YY $Y))
  (= (cleanup-toks (Cons  np (Cons  $X (Cons  np $YY))) (Cons  $X $Y))  
    (set-det)
    (cleanup-toks $YY $Y))
  (= (cleanup-toks (Cons  np $Rest) (Cons  $X $Y))  
    (append $Toks 
      (Cons  np $More) $Rest)
    (atomic-list-concat $Toks - $X)
    (set-det)
    (cleanup-toks $More $Y))
  (= (cleanup-toks (Cons  $X $YY) (Cons  $X $Y))  
    (set-det)
    (cleanup-toks $YY $Y))


  (= (too_long  CORENLP)  True)
  (= (too_long  VP)  True)
  (= (too_long  PP)  True)
  (= (too_long  NML)  True)
  (= (too_long  FRAG)  True)
  (= (too-long $X)  
    (atom-concat $_ BAR $X))
  (= (too-long $X)  
    (atom-concat S $_ $X))

  (= (is-penn-tag $S)  
    (atom $S)
    (upcase-atom $S $S)
    (\== $S I))

  (= (is-penn-long $S)  
    (is-penn-tag $S)
    (too-long $S))


  (= (tree-to-tokz $_ $Item $Item)  
    (atomic $Item)
    (set-det))
  (= (tree-to-tokz $C (Cons  NP $Items) $X)  
    (set-det)
    (tree-l-to-toks $C $Items $List)
    (notrace (undbltok $List $Un))
    (wrap-seg np $Un $X))
;
; tree_to_tokz(C,[_,Item],X):- !, tree_to_tokz(C,Item,X).
  (= (tree-to-tokz $C (Cons  $S $Items) $List)  
    (notrace (is-penn-long $S))
    (\== $Items Nil)
    (set-det)
    (tree-to-tokz $C $Items $List))
  (= (tree-to-tokz $C (Cons  $S $Items) $X)  
    (notrace (is-penn-tag $S))
    (\== $Items Nil)
    (set-det)
    (tree-l-to-toks $C $Items $List)
    (= $S $D)
    (wrap-seg $D $List $X))
  (= (tree-to-tokz $C $Items $Toks)  
    (is-list $Items)
    (set-det)
    (tree-l-to-toks $C $Items $List)
    (set-det)
    (flatten $List $Toks)
    (set-det))
  (= (tree-to-tokz $C $X $X)  
    (set-det))


  (= (clean_innerd  () ())  True)
  (= (clean-innerd (Cons  $D (Cons  $E (Cons  $D $Inner))) (Cons  $E $ReIn))  
    (set-det)
    (clean-innerd $Inner $ReIn))
  (= (clean-innerd (Cons  $S $Inner) (Cons  $S $ReIn))  
    (clean-innerd $Inner $ReIn))

  (= (wrap-seg $O $List $X)  
    (\== $O np)
    (= $List $X))
  (= (wrap-seg $O $List $X)  
    (append 
      (Cons  $D $Inner) 
      (:: $D) $List)
    (clean-innerd $Inner $ReIn)
    (wrap-seg $O $ReIn $X))
  (= (wrap-seg $D $List $X)  
    (append 
      (Cons  $D $List) 
      (:: $D) $X)
    (set-det))
;
; wrap_seg(D,List,X):- dbltok(D,List,X).


  (= (tree-l-to-toks $C $Items $O)  
    (maplist 
      (tree-to-toks $C) $Items $List)
    (flatten $List $O))


  (= (assert-converting $XX $P $Parse)  
    (assert-if-new (converting $XX $P $Parse))
    (nop (save-converting (/ converting 3))))

  (= (assert-converting-v $XX $P $Parse)  
    (assert-converting $XX $P $Parse)
    (dmsg (converting $XX $P $Parse)))


  (= (do-converting $XX $Str $F2)  
    (converting $XX $F2 $_)
    (set-det))
  (= (do-converting $XX $Str $F2)  
    (catch 
      (call $F2 $Str $Result) $E 
      (, 
        (dumpST) 
        (format '% % % ERROR: ~p~n' 
          (:: (--> $E (call $F2 $Str $Result)))) 
        (fail)))
    (set-det)
    (assert-converting $XX $F2 $Result)
    (set-det))


  (= (text_to_tree  () ())  True)
  (= (text-to-tree $TokList $Tree)  
    (not (string $TokList))
    (set-det)
    (atomics-to-string $TokList ' ' $Text)
    (set-det)
    (text-to-tree $TokList $Text $Tree))
  (= (text-to-tree $Text $Tree)  
    (tokenize-atom $Text $TokList)
    (text-to-tree $TokList $Text $Tree))

  (= (text-to-tree $TokList $Text $Tree)  
    (member " $TokList)
    (set-det)
    (text-to-best-tree $Text $Tree))
  (= (text-to-tree $TokList $_ (:: SEQBAR $X $Y))  
    (append $Left 
      (Cons  $LE $Right) $TokList)
    (\== $Right Nil)
    (member $LE 
      (:: . ? 
        (set-det)))
    (append $Left 
      (:: $LE) $Said)
    (set-det)
    (text-to-tree $Said $X)
    (text-to-tree $Right $Y))
  (= (text-to-tree $TokList $Text $Tree)  
    (text-to-best-tree $Text $Tree)
    (set-det))
  (= (text-to-tree $TokList $Text $Tree)  
    (text-to-lgp-tree $Text $Tree)
    (set-det))



  (= (all-letters $X)  
    (not (, (upcase-atom $X $U) (downcase-atom $X $U))))


  (= (retokify  () ())  True)
  (= (retokify (Cons  $E $APreToks) (Cons  sp $PreToks))  
    (not (atomic $E))
    (retokify $APreToks $PreToks))
  (= (retokify (Cons  $E $APreToks) (Cons  $F $PreToks))  
    (downcase-atom $E $F)
    (retokify $APreToks $PreToks))


  (= (add-converting-toks $_ Nil)  
    (set-det))
  (= (add-converting-toks $X (:: $A))  
    (set-det)
    (add-converting-toks $X 
      (:: $A .)))
  (= (add-converting-toks $XX $APreToks)  
    (retokify $APreToks $PreToks)
    (maplist 
      (add-occurs is-word) $PreToks)
    (inc-flag corpus-converting)
    (ignore (add-ngrams except-symbols trigram 3 skip $PreToks))
    (predbltok $PreToks $ReToks0)
    (dbltok oc $ReToks0 $ReToks)
    (set-det)
    (is $XX1 
      (+ $XX 1))
    (append 
      (Cons  
        (oc $XX) $ReToks) 
      (:: (oc $XX1)) $Grams)
    (set-det)
    (assert-converting-v $XX grams $Grams)
    (add-ngrams except-none ngram 4 $XX $Grams))


  (= (add-ngrams $Except $F $N $Loc $Grams)  
    (length $NGram $N)
    (append $NGram $_ $Mid)
    (forall 
      (append $_ $Mid $Grams) 
      (add-1ngram $Except $F $Loc $NGram)))


  (= (except_none  $_)  True)

  (= (add-1ngram $Except $F $Loc $List)  
    (or 
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
        (add-is-symbol  &self $W) True) 
      (add-is-symbol  &self $W))
    (det-if-then-else 
      (= $X 0) 
      (inc-flag corpus-nodes) 
      (inc-flag corpus-node-overlap))
    (set-det))


  (= (add-occurs $F $Tok)  
    (=.. $P 
      (:: $F $Tok))
    (ignore (, (not $P) (add-is-symbol  &self $P) (inc-flag corpus-unique-toks)))
    (ngram-inc $P)
    (inc-flag corpus-total-toks))


  (= (except-symbols $X)  
    (not (, (upcase-atom $X $U) (downcase-atom $X $U))))


  (= (pretok  () ())  True)
;
; pretok(['.'],[]):-!.
  (= (pretok (Cons  $X (Cons  $X (Cons  $X $Nxt))) $O)  
    (set-det)
    (atomic-list-concat 
      (:: $X $X $X) , $Y)
    (pretok 
      (Cons  $Y $Nxt) $O))
  (= (pretok (Cons  $A (Cons  - (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (atomic-list-concat 
      (:: $A $S) - $F)
    (set-det)
    (pretok $Grams $ReTok))
  (= (pretok (Cons  $A (Cons  ' (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (all-letters $A)
    (all-letters $S)
    (atomic-list-concat 
      (:: $A $S) ' $F)
    (set-det)
    (pretok $Grams $ReTok))
  (= (pretok (Cons  $A (Cons  ´ (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (all-letters $A)
    (all-letters $S)
    (atomic-list-concat 
      (:: $A $S) ' $F)
    (set-det)
    (pretok $Grams $ReTok))
  (= (pretok (Cons  $A (Cons  ` (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (all-letters $A)
    (all-letters $S)
    (atomic-list-concat 
      (:: $A $S) ' $F)
    (set-det)
    (pretok $Grams $ReTok))
;
; pretok([','|Grams],ReTok):- pretok(Grams,ReTok).
;
; pretok(['-'|Grams],ReTok):- pretok(Grams,ReTok).
;
; pretok([A,B,C|Grams],ReTok):- trigram(A,B,C,N), N>40, !,ngram_key(trigram(A,B,C),Key),pretok([Key|Grams],ReTok).
  (= (pretok (Cons  (set-det) $Grams) $ReTok)  
    (pretok 
      (Cons  . $Grams) $ReTok))
  (= (pretok (Cons  $S $Grams) (Cons  $S $ReTok))  
    (pretok $Grams $ReTok))


  (= (predbltok  () ())  True)
  (= (predbltok (:: .) Nil)  
    (set-det))
  (= (predbltok (Cons  $X (Cons  $X (Cons  $X $Nxt))) $O)  
    (set-det)
    (atomic-list-concat 
      (:: $X $X $X) , $Y)
    (predbltok 
      (Cons  $Y $Nxt) $O))
  (= (predbltok (Cons  $A (Cons  - (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (atomic-list-concat 
      (:: $A $S) - $F)
    (set-det)
    (predbltok $Grams $ReTok))
  (= (predbltok (Cons  $A (Cons  ' (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (all-letters $A)
    (all-letters $S)
    (atomic-list-concat 
      (:: $A $S) ' $F)
    (set-det)
    (predbltok $Grams $ReTok))
  (= (predbltok (Cons  $A (Cons  ´ (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (all-letters $A)
    (all-letters $S)
    (atomic-list-concat 
      (:: $A $S) ' $F)
    (set-det)
    (predbltok $Grams $ReTok))
  (= (predbltok (Cons  $A (Cons  ` (Cons  $S $Grams))) (Cons  $F $ReTok))  
    (all-letters $A)
    (all-letters $S)
    (atomic-list-concat 
      (:: $A $S) ' $F)
    (set-det)
    (predbltok $Grams $ReTok))
  (= (predbltok (Cons  , $Grams) $ReTok)  
    (predbltok $Grams $ReTok))
  (= (predbltok (Cons  (set-det) $Grams) $ReTok)  
    (predbltok 
      (Cons  . $Grams) $ReTok))
  (= (predbltok (Cons  $S $Grams) (Cons  $S $ReTok))  
    (predbltok $Grams $ReTok))

;
;  dbltok(_,X,X):-!.
;
; dbltok(oc,[],[]):-!.

  (= (dbltok $_ (:: $S) (:: $S))  
    (is-full-tok $S)
    (set-det))
;
; dbltok(Pre,[S],[PS]):- atoms_join(Pre,S,PS).
  (= (dbltok $Pre Nil (:: $PS))  
    (set-det)
    (atoms-join $Pre oc $PS))
  (= (dbltok $Pre (Cons  $S $I) (Cons  $S $O))  
    (is-full-tok $S)
    (set-det)
    (dbltok $Pre $I $O))
  (= (dbltok $Pre (Cons  $S $Grams) (Cons  $PS $ReTok))  
    (atoms-join $Pre $S $PS)
    (dbltok $S $Grams $ReTok))




  (= (undbltok $I $O)  
    (is-list $I)
    (set-det)
    (maplist undbltok $I $O))
  (= (undbltok $S $PS)  
    (into-mw $S 
      (Cons  $PS $_))
    (set-det))
  (= (undbltok $S $S)  
    (set-det))


  (= (is-full-tok $O)  
    (atom $O)
    (atomic-list-concat 
      (Cons  $_ 
        (Cons  $_ $_)) : $O))


  (= (atoms-join $A $B $O)  
    (tok-split $O $A $B)
    (set-det)
    (ngram-inc (tok-split $O $A $B)))
  (= (atoms-join $A $B $O)  
    ( (atomic-list-concat 
        (:: $A $B) : $O) 
      (set-det) 
      (add-is-symbol  &self 
        (tok_split  $O $A $B)) 
      (ngram-inc (tok-split $O $A $B))))

;
;  @TODO use average 
;
; as_good(T,X):- is_word(T,X),(Nxt>500->X=0;X is 500-Nxt).
;
; ngram_rate(A,B,C,D,N,NN):- ngram(Loc,A,B,C,D,N), maplist(as_good,[A,B,C,D],Num), sumlist(Num,NN).


  (= (add-blanks $N $S $Slotted)  
    (not (is-list $S))
    (set-det)
    (add-blanks $N 
      (:: $S) $Slotted))
  (= (add-blanks $_ Nil Nil)  
    (set-det))

  (= (add-blanks $N (Cons  $A (Cons  $B $Sent)) (Cons  $O $Slotted))  
    (tok-split $O $A $B)
    (set-det)
    (add-blanks $N $Sent $Slotted))
  (= (add-blanks $N (Cons  $S $Sent) (Cons  $O $Slotted))  
    (not (not (tok-split $_ $S $_)))
    (set-det)
    (tok-split $O $S $_)
    (add-blanks $N $Sent $Slotted))
  (= (add-blanks $N (Cons  $O $Sent) (Cons  $O $Slotted))  
    (atom $O)
    (tok-split $O $_ $_)
    (set-det)
    (add-blanks $N $Sent $Slotted))

  (= (add-blanks $N (Cons  (len $S) $Sent) $Slotted)  
    (integer $S)
    (length $L $S)
    (set-det)
    (add-blanks $N $Sent $Mid)
    (append $L $Mid $Slotted))
  (= (add-blanks $N (Cons  $S $Sent) (Cons  $A $Slotted))  
    (string $S)
    (atom-string $A $S)
    (set-det)
    (add-blanks $N $Sent $Slotted))
  (= (add-blanks $N (Cons  $S $Sent) $Slotted)  
    (var $S)
    (set-det)
    (between 1 $N $L)
    (add-blanks $N 
      (Cons  
        (- 1 $L) $Sent) $Slotted))
  (= (add-blanks $N (Cons  (- $Lo $Hi) $Sent) $Slotted)  
    (or 
      (integer $Lo) 
      (integer $Hi))
    (set-det)
    (between $Lo $Hi $L)
    (length $S $L)
    (add-blanks $N $Sent $Mid)
    (append $S $Mid $Slotted))
  (= (add-blanks $N (Cons  $S $Sent) $Slotted)  
    (is-list $S)
    (set-det)
    (flatten $S $SL)
    (append $SL $Sent $SLSent)
    (set-det)
    (add-blanks $N $SLSent $Slotted))
  (= (add-blanks $N (Cons  $S $Sent) $Slotted)  
    (atom $S)
    (into-mw $S $SL)
    (set-det)
    (append $SL $Sent $SLSent)
    (set-det)
    (add-blanks $N $SLSent $Slotted))
  (= (add-blanks $N (Cons  $S $Sent) (Cons  $S $Slotted))  
    (add-blanks $N $Sent $Slotted))


  (= (into-mw $S $SL)  
    (into-mw0 $S $SL)
    (\== $SL 
      (:: $S))
    (set-det))

  (= (into-mw0 $S $SL)  
    (atomic-list-concat 
      (Cons  $M 
        (Cons  $_ $_)) : $S)
    (set-det)
    (into-mw0 $M $SL))
  (= (into-mw0 $S $SL)  
    (atomic-list-concat $SL , $S))
  (= (into-mw0 $S $SL)  
    (atomic-list-concat $SL ' ' $S))
  (= (into-mw0 $S $SL)  
    (atomic-list-concat $SL - $S))


  (= (loc-dists $Loc1 $Loc2 $NN)  
    (is $NN 
      (abs (- $Loc1 $Loc2))))
  (= (loc-dists $Loc1 $Loc2 $Loc3 $NN)  
    (is $NN 
      (/ 
        (+ 
          (+ 
            (abs (- $Loc1 $Loc2)) 
            (abs (- $Loc3 $Loc2))) 
          (abs (- $Loc1 $Loc3))) 3)))

;
; :- so_convert:ensure_loaded(plm).
;
;  added for conversations

  (= (ngram $Loc $A (oc $X) $B $C $NN)  
    (nonvar $X)
    (ngram $Loc $_ $_ $A 
      (oc $X) $_)
    (ngram $ULoc 
      (oc $X) $B $C $_ $NN))
  (= (ngram $Loc $A $B (oc $X) $C $NN)  
    (nonvar $X)
    (ngram $Loc $_ $A $B 
      (oc $X) $_)
    (ngram $ULoc 
      (oc $X) $C $_ $_ $NN))


  (= (autoc $Sent)  
    (autoc 1 $Sent))
  (= (autoc $N $Sent)  
    ( (remove-all-atoms  &self 
        (used_cl  
          (ngram  $_ $_ $_ $_))) 
      (add-blanks $N $Sent $Slotted) 
      (no-repeats (map-sent $_ $Loc $Slotted)) 
      (fmt-so-convert $Slotted)))


  (= (good-toks $Key $E)  
    (functor $P ngram 6)
    (arg 6 $P $E)
    (no-repeats $Key 
      (, $P 
        (ngram-key $P $Key))))



  !(add-history reconvert-corpus)

  !(fixup-exports *)

  !(dynamic (/ used-cl 1))


  (= (map-sent $_ $_ $Sent)  
    (ground $Sent)
    (set-det))
  (= (map-sent $LR $Loc $Sent)  
    (var $Sent)
    (length $Sent 9)
    (map-sent $LR $Loc $Sent))
  (= (map-sent $LR $Loc $List)  
    (= $LR lr)
    (append $Left 
      (Cons  $X $More) $List)
    (nonvar $X)
    (\== $Left Nil)
    (set-det)
    (map-sent $LR $Loc 
      (Cons  $X $More))
    (map-sent rl $Loc $List))
  (= (map-sent $LR $Loc (Cons  $A (Cons  $B (Cons  $C (Cons  $D $More)))))  
    (some-ngram $Loc $A $B $C $D $Fire)
    (map-sent $LR $Loc 
      (Cons  $C 
        (Cons  $D $More))))
  (= (map-sent $LR $Loc (Cons  $A (Cons  $B (Cons  $C (Cons  $D $More)))))  
    (some-ngram $Loc $A $B $C $_ $Fire)
    (map-sent $LR $Loc 
      (Cons  $B 
        (Cons  $C 
          (Cons  $D $More)))))
  (= (map-sent $_ $Loc $List)  
    (= $ABCDO 
      (:: $_ $_ $_ $_ $Occurs))
    (append $List $_ $ABCDO)
    (apply some-ngram 
      (Cons  $Loc $ABCDO)))



  (= (some-ngram $PrevLoc $A $B $C $D $N)  
    (pick-ngram $Loc $A $B $C $D $N)
    (may-use $Loc $A $B $C $D $N))


  (= (pick-ngram $Loc $A $B $C $D $N)  
    (det-if-then-else 
      (maplist var 
        (:: $A $B $C $D)) 
      (rnd-ngram $Loc $A $B $C $D $N) 
      (ngram $Loc $A $B $C $D $N)))


  (= (rnd-ngram $Loc $A $B $C $D $N)  
    ( (= $G 
        (ngram $Loc $A $B $C $D $N)) 
      (predicate-property $G 
        (number-of-clauses $R)) 
      (is $CN 
        (+ 
          (random $R) 1)) 
      (nth-clause $G $CN $Ref) 
      (clause $G $Body $Ref) $Body))



  (= (cf)  
    (consult corpus-file))

  (= (qc)  
    (qcompile corpus-file))

;
; :- ignore(( \+ MeTTa_load_context(reloading, true),time(consult(corpus_file)))).


  !(style-check (- singleton))


;
; :- add_history((good_toks(Key,E),E>20)).
;
; :- add_history((autoc([ 'like:you',len(200)]))).
;
; :- add_history((autoc([oc,'like:you',len(200)]))).
;
; :- add_history((autoc([ 'oc:like', 'like:you',len(200)]))).
;
; :- add_history((autoc([ 'like',len(200)]))).
;
; :- add_history((autoc([len(10),like,len(200)]))).
  !(add-history load-converting)
  !(add-history convert-corpus)
  !(add-history tree-to-toks)
  !(add-history convert-corpus)


  (= (may-use $Loc $_ $B $C $D $_)  
    (not (used-cl (ngram $A $B $C $D)))
    (assert 
      (used-cl (ngram $A $B $C $D)) $Cl2)
    (undo (erase $Cl2))
    (set-det))



  (= (gen6 (= (:: $A $B $C $D $E $F $G $H) $N))  
    (ngram $Loc1 $E $F $G $H $Z)
    (ngram $Loc2 $C $D $E $F $Y)
    (ngram $Loc3 $A $B $C $D $X)
    (is $N 
      (+ 
        (+ $X $Y) $Z)))

  !(fixup-exports *)


  (= (dotit)  
    (ignore (, (not (prolog-load-context reloading True)) (ignore load-converting) (ignore convert-corpus))))


