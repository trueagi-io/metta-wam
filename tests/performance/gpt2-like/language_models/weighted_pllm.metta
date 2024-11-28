

  !(include training-bi)

  !(dynamic (/ used-cl 1))


  (= 
    (map-sent $_ $Sent) 
    ( (ground $Sent) (set-det)))
  (= 
    (map-sent $Loc $Sent) 
    ( (var $Sent) 
      (length $Sent 9) 
      (map-sent $Loc $Sent)))
  (= 
    (map-sent $Loc 
      (Cons  $A 
        (Cons  $B 
          (Cons  $C 
            (Cons  $D $More))))) 
    ( (map-sent $Loc 
        (Cons  $C 
          (Cons  $D $More))) (once-ngram $Loc $A $B $C $D $Fire)))
  (= 
    (map-sent $Loc 
      (Cons  $A 
        (Cons  $B 
          (Cons  $C 
            (Cons  $D $More))))) 
    ( (once-ngram $Loc $A $B $C $_ $Fire) (map-sent $Loc (Cons  $B (Cons  $C (Cons  $D $More))))))
  (= 
    (map-sent $Loc $List) 
    ( (= $ABCDO 
        (:: $_ $_ $_ $_ $Occurs)) 
      (append $List $_ $ABCDO) 
      (apply once-ngram 
        (Cons  $Loc $ABCDO))))


  (= 
    (once-ngram $PrevLoc $A $B $C $D $N) 
    ( (ngram $Loc $A $B $C $D $N) (may-use $Loc $A $B $C $D $N)))



  !(style-check (- singleton))

;
;                           may_use(Loc,_,B,C,D,_):- \+ used_cl(ngram(A,B,C,D)), assert(used_cl(ngram(A,B,C,D)),Cl2), undo(erase(Cl2)), !.


  (= 
    (may-use $Loc $_ $B $C $D $_) 
    ( (not (used-cl (ngram $A $B $C $D))) (add-atom  &self (used_cl  (ngram  $A $B $C $D)))))


  (= 
    (gen6 (= (:: $A $B $C $D $E $F $G $H) $N)) 
    ( (ngram $Loc1 $E $F $G $H $Z) 
      (ngram $Loc2 $C $D $E $F $Y) 
      (ngram $Loc3 $A $B $C $D $X) 
      (is $N 
        (+ 
          (+ $X $Y) $Z))))


  !fixup-exports



