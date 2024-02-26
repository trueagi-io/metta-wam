;
;                       generated: 30 October 1989

;
;                       option(s):

;
; 

;
;                          prover

;
; 

;
;                          Richard A. O'Keefe

;
; 

;
;                          MeTTa theorem prover

;
; 

;
;                           from "MeTTa Compared with Lisp?," SIGPLAN Notices, v. 18 #5, May 1983


;
;                         op/3 directives



  (= 
    (top) 
    (prover))



  !(op 950 xfy or)	;
;                           disjunction

  !(op 850 xfy and)	;
;                           conjunction

;
;                            comment out the next two directives as they clash with the standard

;
;                            definition of the + and - operators; they are also not required and

;
;                            interfere loading the "bench" example with some backends

;
;                           :- op(500, fx, +).	; assertion

;
;                           :- op(500, fx, -).	; denial



  (= 
    (prover) 
    ( (problem $_ $P $C) 
      (implies $P $C) 
      (fail)))
  (= prover True)

;
;                              problem set



  (= 
    (problem  1 
      (-  a) 
      (+  a)) True)

  (= 
    (problem  2 
      (+  a) 
      (and  
        (-  a) 
        (-  a))) True)

  (= 
    (problem  3 
      (-  a) 
      (or  
        (+  to_be) 
        (-  to_be))) True)

  (= 
    (problem  4 
      (and  
        (-  a) 
        (-  a)) 
      (-  a)) True)

  (= 
    (problem  5 
      (-  a) 
      (or  
        (+  b) 
        (-  a))) True)

  (= 
    (problem  6 
      (and  
        (-  a) 
        (-  b)) 
      (and  
        (-  b) 
        (-  a))) True)

  (= 
    (problem  7 
      (-  a) 
      (or  
        (-  b) 
        (and  
          (+  b) 
          (-  a)))) True)

  (= 
    (problem  8 
      (or  
        (-  a) 
        (or  
          (-  b) 
          (+  c))) 
      (or  
        (-  b) 
        (or  
          (-  a) 
          (+  c)))) True)

  (= 
    (problem  9 
      (or  
        (-  a) 
        (+  b)) 
      (or  
        (and  
          (+  b) 
          (-  c)) 
        (or  
          (-  a) 
          (+  c)))) True)

  (= 
    (problem  10 
      (and  
        (or  
          (-  a) 
          (+  c)) 
        (or  
          (-  b) 
          (+  c))) 
      (or  
        (and  
          (-  a) 
          (-  b)) 
        (+  c))) True)

;
;                                    MeTTa theorem prover



  (= 
    (implies $Premise $Conclusion) 
    ( (opposite $Conclusion $Denial) (add-conjunction $Premise $Denial (fs Nil Nil Nil Nil))))


  (= 
    (opposite 
      (and $F0 $G0) 
      (or $F1 $G1)) 
    ( (set-det) 
      (opposite $F0 $F1) 
      (opposite $G0 $G1)))
  (= 
    (opposite 
      (or $F1 $G1) 
      (and $F0 $G0)) 
    ( (set-det) 
      (opposite $F1 $F0) 
      (opposite $G1 $G0)))
  (= 
    (opposite 
      (+ $Atom) 
      (- $Atom)) 
    (set-det))
  (= 
    (opposite  
      (-  $Atom) 
      (+  $Atom)) True)


  (= 
    (add-conjunction $F $G $Set) 
    ( (expand $F $Set $Mid) 
      (expand $G $Mid $New) 
      (refute $New)))


  (= 
    (expand $_ refuted refuted) 
    (set-det))
  (= 
    (expand 
      (and $F $G) 
      (fs $D $_ $_ $_) refuted) 
    ( (includes $D 
        (and $F $G)) (set-det)))
  (= 
    (expand 
      (and $F $G) 
      (fs $D $C $P $N) 
      (fs $D $C $P $N)) 
    ( (includes $C 
        (and $F $G)) (set-det)))
  (= 
    (expand 
      (and $F $G) 
      (fs $D $C $P $N) $New) 
    ( (set-det) 
      (expand $F 
        (fs $D 
          (Cons  
            (and $F $G) $C) $P $N) $Mid) 
      (expand $G $Mid $New)))
  (= 
    (expand 
      (or $F $G) 
      (fs $D $C $P $N) $Set) 
    ( (set-det) 
      (opposite 
        (or $F $G) $Conj) 
      (extend $Conj $D $C $D1 
        (fs $D1 $C $P $N) $Set)))
  (= 
    (expand 
      (+ $Atom) 
      (fs $D $C $P $N) $Set) 
    ( (set-det) (extend $Atom $P $N $P1 (fs $D $C $P1 $N) $Set)))
  (= 
    (expand 
      (- $Atom) 
      (fs $D $C $P $N) $Set) 
    (extend $Atom $N $P $N1 
      (fs $D $C $P $N1) $Set))


  (= 
    (includes 
      (Cons  $Head $_) $Head) 
    (set-det))
  (= 
    (includes 
      (Cons  $_ $Tail) $This) 
    (includes $Tail $This))


  (= 
    (extend $Exp $_ $Neg $_ $_ refuted) 
    ( (includes $Neg $Exp) (set-det)))
  (= 
    (extend $Exp $Pos $_ $Pos $Set $Set) 
    ( (includes $Pos $Exp) (set-det)))
  (= 
    (extend  $Exp $Pos $_ 
      (Cons  $Exp $Pos) $Set $Set) True)


  (= 
    (refute refuted) 
    (set-det))
  (= 
    (refute (fs (Cons  (and $F1 $G1) $D) $C $P $N)) 
    ( (opposite $F1 $F0) 
      (opposite $G1 $G0) 
      (= $Set 
        (fs $D $C $P $N)) 
      (add-conjunction $F0 $G1 $Set) 
      (add-conjunction $F0 $G0 $Set) 
      (add-conjunction $F1 $G0 $Set)))

