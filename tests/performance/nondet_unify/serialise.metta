;
;                        generated: 17 November 1989

;
;                         option(s):

;
; 

;
;                           serialise

;
; 

;
;                           David H. D. Warren

;
; 

;
;                            itemize (pick a "serial number" for each

;
;                            unique integer in) a list of 25 integers



  (= 
    (top) 
    (serialise))


  (= 
    (serialise) 
    ( (atom-codes 'ABLE WAS I ERE I SAW ELBA' $Codes) (serialise $Codes $_)))

  (= 
    (serialise $L $R) 
    ( (pairlists $L $R $A) 
      (arrange $A $T) 
      (numbered $T 1 $_)))


  (= 
    (pairlists 
      (Cons  $X $L) 
      (Cons  $Y $R) 
      (Cons  
        (pair $X $Y) $A)) 
    (pairlists $L $R $A))
  (= 
    (pairlists  () () ()) True)


  (= 
    (arrange 
      (Cons  $X $L) 
      (tree $T1 $X $T2)) 
    ( (split $L $X $L1 $L2) 
      (arrange $L1 $T1) 
      (arrange $L2 $T2)))
  (= 
    (arrange  () void) True)


  (= 
    (split 
      (Cons  $X $L) $X $L1 $L2) 
    ( (set-det) (split $L $X $L1 $L2)))
  (= 
    (split 
      (Cons  $X $L) $Y 
      (Cons  $X $L1) $L2) 
    ( (before $X $Y) 
      (set-det) 
      (split $L $Y $L1 $L2)))
  (= 
    (split 
      (Cons  $X $L) $Y $L1 
      (Cons  $X $L2)) 
    ( (before $Y $X) 
      (set-det) 
      (split $L $Y $L1 $L2)))
  (= 
    (split  () $_ () ()) True)


  (= 
    (before 
      (pair $X1 $_) 
      (pair $X2 $_)) 
    (< $X1 $X2))


  (= 
    (numbered 
      (tree $T1 
        (pair $_ $N1) $T2) $N0 $N) 
    ( (numbered $T1 $N0 $N1) 
      (is $N2 
        (+ $N1 1)) 
      (numbered $T2 $N2 $N)))
  (= 
    (numbered  void $N $N) True)

