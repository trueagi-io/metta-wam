
;
;                        Cryptoaddition:

;
;                         Find the unique answer to:

;
;                          SEND

;
;                         +MORE

;
;                         -----

;
;                         MONEY

;
;                          where each letter is a distinct digit.



  (= 
    (top) 
    ( (digit $D) 
      (digit $E) 
      (=\= $D $E) 
      (sumdigit 0 $D $E $Y $C1) 
      (digit $N) 
      (=\= $N $Y) 
      (=\= $N $E) 
      (=\= $N $D) 
      (digit $R) 
      (=\= $R $N) 
      (=\= $R $Y) 
      (=\= $R $E) 
      (=\= $R $D) 
      (sumdigit $C1 $N $R $E $C2) 
      (digit $O) 
      (=\= $O $R) 
      (=\= $O $N) 
      (=\= $O $Y) 
      (=\= $O $E) 
      (=\= $O $D) 
      (sumdigit $C2 $E $O $N $C3) 
      (leftdigit $S) 
      (=\= $S $O) 
      (=\= $S $R) 
      (=\= $S $N) 
      (=\= $S $Y) 
      (=\= $S $E) 
      (=\= $S $D) 
      (leftdigit $M) 
      (=\= $M $S) 
      (=\= $M $O) 
      (=\= $M $R) 
      (=\= $M $N) 
      (=\= $M $Y) 
      (=\= $M $E) 
      (=\= $M $D) 
      (sumdigit $C3 $S $M $O $M) 
      (set-det)))
	;
;                          write(' '),write(S),write(E),write(N),write(D),nl,

	;
;                          write('+'),write(M),write(O),write(R),write(E),nl,

	;
;                          write('-----'),nl,

	;
;                           write(M),write(O),write(N),write(E),write(Y),nl,nl.



  (= 
    (sumdigit $C $A $B $S $D) 
    ( (is $X 
        (+ 
          (+ $C $A) $B)) (det-if-then-else (< $X 10) (, (= $S $X) (= $D 0)) (, (is $S (- $X 10)) (= $D 1)))))


  (= 
    (digit  0) True)
  (= 
    (digit  1) True)
  (= 
    (digit  2) True)
  (= 
    (digit  3) True)
  (= 
    (digit  4) True)
  (= 
    (digit  5) True)
  (= 
    (digit  6) True)
  (= 
    (digit  7) True)
  (= 
    (digit  8) True)
  (= 
    (digit  9) True)


  (= 
    (leftdigit  1) True)
  (= 
    (leftdigit  2) True)
  (= 
    (leftdigit  3) True)
  (= 
    (leftdigit  4) True)
  (= 
    (leftdigit  5) True)
  (= 
    (leftdigit  6) True)
  (= 
    (leftdigit  7) True)
  (= 
    (leftdigit  8) True)
  (= 
    (leftdigit  9) True)

