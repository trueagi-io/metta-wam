;
;                      generated: 8 March 1990

;
;                      option(s):

;
; 

;
;                        meta_qsort

;
; 

;
;                         Ralph M. Haygood

;
; 

;
;                         meta-interpret Warren benchmark qsort

;
; 

;
;                       For any meta-variable ~X~, interpret(~X~) behaves as if

;
; 

;
;                         interpret(~X~) :- ~X~.

;
; 

;
;                         Thus, for example, interpret((foo(X), bar(X), !)) behaves as if

;
; 

;
;                          interpret((foo(X), bar(X), !)) :- foo(X), bar(X), !.

;
; 

;
;                          Note that though ~X~ may contain cuts, those cuts cannot escape from

;
;                          interpret(~X~) to effect the parent goal; interpret(!) is equivalent

;
;                          to true.

;
; 

;
;                           Cuts inside ~X~ are executed according to the rule that conjunction,

;
;                           disjunction, and if-then-else are transparent to cuts, and any other

;
;                           form is transparent to cuts if and only if it can be macro-expanded

;
;                           into a form involving only these three without interpret/1.  If-then

;
;                            and negation are the only such other forms currently recognized; ( A

;
;                            -> B) is equivalent to ( A -> B ; fail ), and \+ A is equivalent to

;
;                            ( A -> fail ; true ).



  (= 
    (top) 
    (meta-qsort))


  (= 
    (meta-qsort) 
    (interpret qsort))


  (= 
    (interpret $Goal) 
    ( (interpret $Goal $Rest) (or (, (nonvar $Rest) (set-det) (interpret $Rest)) True)))

  (= 
    (interpret $G $_) 
    ( (var $G) 
      (set-det) 
      (fail)))
  (= 
    (interpret 
      (, $A $B) $Rest) 
    ( (set-det) 
      (interpret $A $Rest0) 
      (det-if-then-else 
        (nonvar $Rest0) 
        (= $Rest 
          (, $Rest0 $B)) 
        (interpret $B $Rest))))
  (= 
    (interpret 
      (or $A $B) $Rest) 
    ( (set-det) (interpret-disjunction $A $B $Rest)))
  (= 
    (interpret 
      (det-if-then $A $B) $Rest) 
    ( (set-det) (interpret-disjunction (det-if-then $A $B) fail $Rest)))
  (= 
    (interpret 
      (not $A) $Rest) 
    ( (set-det) (interpret-disjunction (det-if-then $A fail) True $Rest)))
  (= 
    (interpret 
      (set-det) True) 
    (set-det))
  (= 
    (interpret $G $_) 
    ( (number $G) 
      (set-det) 
      (fail)))
  (= 
    (interpret $G $_) 
    ( (is-built-in $G) 
      (set-det) 
      (interpret-built-in $G)))
  (= 
    (interpret $G $_) 
    ( (define $G $Body) (interpret $Body)))


  (= 
    (interpret-disjunction 
      (det-if-then $A $B) $_ $Rest) 
    ( (interpret $A $Rest0) 
      (set-det) 
      (det-if-then-else 
        (nonvar $Rest0) 
        (= $Rest 
          (det-if-then $Rest0 $B)) 
        (interpret $B $Rest))))
  (= 
    (interpret-disjunction 
      (det-if-then $_ $_) $C $Rest) 
    ( (set-det) (interpret $C $Rest)))
  (= 
    (interpret-disjunction $A $_ $Rest) 
    (interpret $A $Rest))
  (= 
    (interpret-disjunction $_ $B $Rest) 
    (interpret $B $Rest))


  (= 
    (is_built_in  true) True)
  (= 
    (is_built_in  
      (=<  $_ $_)) True)


  (= 
    (interpret_built_in  true) True)
  (= 
    (interpret-built-in (=< $X $Y)) 
    (=< $X $Y))


  (= 
    (define  qsort 
      (qsort  
        (27 74 17 33 94 18 46 83 65 2 32 53 28 85 99 47 28 82 6 11 55 29 39 81 90 37 10 0 66 51 7 21 85 27 31 63 75 4 95 99 11 28 61 74 18 92 40 53 59 8) $_ ())) True)

  (= 
    (define  
      (qsort  
        (Cons  $X $L) $R $R0) 
      (,  
        (partition  $L $X $L1 $L2) 
        (,  
          (qsort  $L2 $R1 $R0) 
          (qsort  $L1 $R 
            (Cons  $X $R1))))) True)
  (= 
    (define  
      (qsort  () $R $R) true) True)

  (= 
    (define  
      (partition  
        (Cons  $X $L) $Y 
        (Cons  $X $L1) $L2) 
      (,  
        (=<  $X $Y) 
        (,  ! 
          (partition  $L $Y $L1 $L2)))) True)
  (= 
    (define  
      (partition  
        (Cons  $X $L) $Y $L1 
        (Cons  $X $L2)) 
      (partition  $L $Y $L1 $L2)) True)
  (= 
    (define  
      (partition  () $_ () ()) true) True)

