;
;                    generated: 19 June 1990

;
;                    option(s):

;
; 

;
;                       browse

;
; 

;
;                       Tep Dobry (from Lisp version by R. P. Gabriel)

;
; 

;
;                       (modified January 1987 by Herve' Touati)



  (= 
    (top) 
    ( (init 100 10 4 
        (:: 
          (:: a a a b b b b a a a a a b b a a a) 
          (:: a a b b b b a a 
            (:: a a) 
            (:: b b)) 
          (:: a a a b 
            (:: b a) b a b a)) $Symbols) 
      (randomize $Symbols $RSymbols 21) 
      (set-det) 
      (investigate $RSymbols 
        (:: 
          (:: 
            (star $SA) $B 
            (star $SB) $B a 
            (star $SA) a 
            (star $SB) 
            (star $SA)) 
          (:: 
            (star $SA) 
            (star $SB) 
            (star $SB) 
            (star $SA) 
            (:: (star $SA)) 
            (:: (star $SB))) 
          (:: $_ $_ 
            (star $_) 
            (:: b a) 
            (star $_) $_ $_)))))



  (= 
    (init $N $M $Npats $Ipats $Result) 
    (init $N $M $M $Npats $Ipats $Result))

  (= 
    (init 0 $_ $_ $_ $_ $_) 
    (set-det))
  (= 
    (init $N $I $M $Npats $Ipats 
      (Cons  $Symb $Rest)) 
    ( (fill $I Nil $L) 
      (get-pats $Npats $Ipats $Ppats) 
      (is $J 
        (- $M $I)) 
      (fill $J 
        (Cons  
          (pattern $Ppats) $L) $Symb) 
      (is $N1 
        (- $N 1)) 
      (det-if-then-else 
        (=:= $I 0) 
        (is $I1 $M) 
        (is $I1 
          (- $I 1))) 
      (init $N1 $I1 $M $Npats $Ipats $Rest)))


  (= 
    (fill 0 $L $L) 
    (set-det))
  (= 
    (fill $N $L 
      (Cons  
        (dummy Nil) $Rest)) 
    ( (is $N1 
        (- $N 1)) (fill $N1 $L $Rest)))


  (= 
    (randomize Nil Nil $_) 
    (set-det))
  (= 
    (randomize $In 
      (Cons  $X $Out) $Rand) 
    ( (length $In $Lin) 
      (is $Rand1 
        (mod 
          (* $Rand 17) 251)) 
      (is $N 
        (mod $Rand1 $Lin)) 
      (split $N $In $X $In1) 
      (randomize $In1 $Out $Rand1)))


  (= 
    (split 0 
      (Cons  $X $Xs) $X $Xs) 
    (set-det))
  (= 
    (split $N 
      (Cons  $X $Xs) $RemovedElt 
      (Cons  $X $Ys)) 
    ( (is $N1 
        (- $N 1)) (split $N1 $Xs $RemovedElt $Ys)))


  (= 
    (investigate Nil $_) 
    (set-det))
  (= 
    (investigate 
      (Cons  $U $Units) $Patterns) 
    ( (property $U pattern $Data) 
      (p-investigate $Data $Patterns) 
      (investigate $Units $Patterns)))


  (= 
    (get-pats $Npats $Ipats $Result) 
    (get-pats $Npats $Ipats $Result $Ipats))

  (= 
    (get-pats 0 $_ Nil $_) 
    (set-det))
  (= 
    (get-pats $N 
      (Cons  $X $Xs) 
      (Cons  $X $Ys) $Ipats) 
    ( (is $N1 
        (- $N 1)) (get-pats $N1 $Xs $Ys $Ipats)))
  (= 
    (get-pats $N Nil $Ys $Ipats) 
    (get-pats $N $Ipats $Ys $Ipats))


  (= 
    (property  () $_ $_) 
    (empty))	
  (= 
    (property 
      (Cons  $Prop $_) $P $Val) 
    ( (functor $Prop $P $_) 
      (set-det) 
      (arg 1 $Prop $Val)))
;                               /* don't really need this */


  (= 
    (property 
      (Cons  $_ $RProps) $P $Val) 
    (property $RProps $P $Val))


  (= 
    (p_investigate  () $_) True)
  (= 
    (p-investigate 
      (Cons  $D $Data) $Patterns) 
    ( (p-match $Patterns $D) (p-investigate $Data $Patterns)))


  (= 
    (p_match  () $_) True)
  (= 
    (p-match 
      (Cons  $P $Patterns) $D) 
    ( (or 
        (, 
          (match $D $P) 
          (fail)) True) (p-match $Patterns $D)))


  (= 
    (match Nil Nil) 
    (set-det))
  (= 
    (match 
      (Cons  $X $PRest) 
      (Cons  $Y $SRest)) 
    ( (var $Y) 
      (set-det) 
      (= $X $Y) 
      (match $PRest $SRest)))
  (= 
    (match $List 
      (Cons  $Y $Rest)) 
    ( (nonvar $Y) 
      (= $Y 
        (star $X)) 
      (set-det) 
      ($concat $X $SRest $List) 
      (match $SRest $Rest)))
  (= 
    (match 
      (Cons  $X $PRest) 
      (Cons  $Y $SRest)) 
    ( (det-if-then-else 
        (atom $X) 
        (= $X $Y) 
        (match $X $Y)) (match $PRest $SRest)))


  (= 
    ($concat  () $L $L) True)
  (= 
    ($concat 
      (Cons  $X $L1) $L2 
      (Cons  $X $L3)) 
    ($concat $L1 $L2 $L3))

