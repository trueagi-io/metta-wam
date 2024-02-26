;
;                       generated: 10 November 1989

;
;                        option(s):

;
; 

;
;                          (queens) queens_8

;
; 

;
;                          from Sterling and Shapiro, "The Art of MeTTa," page 211.

;
; 

;
;                           solve the 8 queens problem



;
;                           This program solves the N queens problem:  place N pieces on an N

;
;                            by N rectangular board so that no two pieces are on the same line

;
;                            - horizontal, vertical, or diagonal.  (N queens so placed on an N

;
;                            by N chessboard are unable to attack each other in a single move

;
;                            under the rules of chess.)  The strategy is incremental generate-

;
;                            and-test.

;
; 

;
;                             A solution is specified by a permutation of the list of numbers 1 to

;
;                             N.  The first element of the list is the row number for the queen in

;
;                             the first column, the second element is the row number for the queen

;
;                             in the second column, et cetera.  This scheme implicitly incorporates

;
;                              the observation that any solution of the problem has exactly one queen

;
;                              in each column.

;
; 

;
;                              The program distinguishes symmetric solutions.  For example,

;
; 

;
;                               ?- queens(4, Qs).

;
; 

;
;                               produces

;
; 

;
;                               Qs = [3,1,4,2] ;

;
; 

;
;                                Qs = [2,4,1,3]



  (= 
    (top) 
    ( (queens 8 $Qs) (fail)))
  (= top True)


  (= 
    (queens $N $Qs) 
    ( (range 1 $N $Ns) (queens $Ns Nil $Qs)))

  (= 
    (queens  () $Qs $Qs) True)
  (= 
    (queens $UnplacedQs $SafeQs $Qs) 
    ( (select $UnplacedQs $UnplacedQs1 $Q) 
      (not-attack $SafeQs $Q) 
      (queens $UnplacedQs1 
        (Cons  $Q $SafeQs) $Qs)))


  (= 
    (not-attack $Xs $X) 
    (not-attack $Xs $X 1))

  (= 
    (not-attack Nil $_ $_) 
    (set-det))
  (= 
    (not-attack 
      (Cons  $Y $Ys) $X $N) 
    ( (=\= $X 
        (+ $Y $N)) 
      (=\= $X 
        (- $Y $N)) 
      (is $N1 
        (+ $N 1)) 
      (not-attack $Ys $X $N1)))


  (= 
    (select  
      (Cons  $X $Xs) $Xs $X) True)
  (= 
    (select 
      (Cons  $Y $Ys) 
      (Cons  $Y $Zs) $X) 
    (select $Ys $Zs $X))


  (= 
    (range $N $N 
      (:: $N)) 
    (set-det))
  (= 
    (range $M $N 
      (Cons  $M $Ns)) 
    ( (< $M $N) 
      (is $M1 
        (+ $M 1)) 
      (range $M1 $N $Ns)))



