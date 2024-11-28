
  !(use-module (library logicmoo-utils))

;
;                     debug printing


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
;                             debugln_xfrm(C,S):- compound(C),compound_name_arguments(C,N,A),debugln_xfrm([N|A],S).

  (= 
    (debugln-xfrm nl 
)   (set-det))
  (= 
    (debugln-xfrm Nil '') 
    (set-det))
  (= 
    (debugln_xfrm  $E $E) True)


