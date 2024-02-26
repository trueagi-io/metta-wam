;
;                       generated: 8 March 1990

;
;                       option(s): NO_TERM_COMPARE

;
; 

;
;                         (poly) poly_10

;
; 

;
;                          Ralph Haygood (based on MeTTa version by Rick McGeer

;
;                                         based on Lisp version by R. P. Gabriel)

;
; 

;
;                          raise a polynomial (1+x+y+z) to the 10th power (symbolically)



  !(op 700 xfx less-than)


  (= 
    (top) 
    (poly-10))


  (= 
    (poly-10) 
    ( (test-poly $P) (poly-exp 10 $P $_)))

;
;                           test polynomial definition



  (= 
    (test-poly $P) 
    ( (poly-add 
        (poly x 
          (:: 
            (term 0 1) 
            (term 1 1))) 
        (poly y 
          (:: (term 1 1))) $Q) (poly-add (poly z (:: (term 1 1))) $Q $P)))

;
;                            'less_than'/2 for x, y, z



  (= 
    (less_than  x y) True)
  (= 
    (less_than  y z) True)
  (= 
    (less_than  x z) True)

;
;                             polynomial addition



  (= 
    (poly-add 
      (poly $Var $Terms1) 
      (poly $Var $Terms2) 
      (poly $Var $Terms)) 
    ( (set-det) (term-add $Terms1 $Terms2 $Terms)))
  (= 
    (poly-add 
      (poly $Var1 $Terms1) 
      (poly $Var2 $Terms2) 
      (poly $Var1 $Terms)) 
    ( (less-than $Var1 $Var2) 
      (set-det) 
      (add-to-order-zero-term $Terms1 
        (poly $Var2 $Terms2) $Terms)))
  (= 
    (poly-add $Poly 
      (poly $Var $Terms2) 
      (poly $Var $Terms)) 
    ( (set-det) (add-to-order-zero-term $Terms2 $Poly $Terms)))
  (= 
    (poly-add 
      (poly $Var $Terms1) $C 
      (poly $Var $Terms)) 
    ( (set-det) (add-to-order-zero-term $Terms1 $C $Terms)))
  (= 
    (poly-add $C1 $C2 $C) 
    (is $C 
      (+ $C1 $C2)))

;
;                                term addition



  (= 
    (term-add Nil $X $X) 
    (set-det))
  (= 
    (term-add $X Nil $X) 
    (set-det))
  (= 
    (term-add 
      (Cons  
        (term $E $C1) $Terms1) 
      (Cons  
        (term $E $C2) $Terms2) 
      (Cons  
        (term $E $C) $Terms)) 
    ( (set-det) 
      (poly-add $C1 $C2 $C) 
      (term-add $Terms1 $Terms2 $Terms)))
  (= 
    (term-add 
      (Cons  
        (term $E1 $C1) $Terms1) 
      (Cons  
        (term $E2 $C2) $Terms2) 
      (Cons  
        (term $E1 $C1) $Terms)) 
    ( (< $E1 $E2) 
      (set-det) 
      (term-add $Terms1 
        (Cons  
          (term $E2 $C2) $Terms2) $Terms)))
  (= 
    (term-add $Terms1 
      (Cons  
        (term $E2 $C2) $Terms2) 
      (Cons  
        (term $E2 $C2) $Terms)) 
    (term-add $Terms1 $Terms2 $Terms))


  (= 
    (add-to-order-zero-term 
      (Cons  
        (term 0 $C1) $Terms) $C2 
      (Cons  
        (term 0 $C) $Terms)) 
    ( (set-det) (poly-add $C1 $C2 $C)))
  (= 
    (add_to_order_zero_term  $Terms $C 
      (Cons  
        (term  0 $C) $Terms)) True)

;
;                                    polynomial exponentiation



  (= 
    (poly-exp 0 $_ 1) 
    (set-det))
  (= 
    (poly-exp $N $Poly $Result) 
    ( (is $M 
        (>> $N 1)) 
      (is $N 
        (<< $M 1)) 
      (set-det) 
      (poly-exp $M $Poly $Part) 
      (poly-mul $Part $Part $Result)))
  (= 
    (poly-exp $N $Poly $Result) 
    ( (is $M 
        (- $N 1)) 
      (poly-exp $M $Poly $Part) 
      (poly-mul $Poly $Part $Result)))

;
;                                     polynomial multiplication



  (= 
    (poly-mul 
      (poly $Var $Terms1) 
      (poly $Var $Terms2) 
      (poly $Var $Terms)) 
    ( (set-det) (term-mul $Terms1 $Terms2 $Terms)))
  (= 
    (poly-mul 
      (poly $Var1 $Terms1) 
      (poly $Var2 $Terms2) 
      (poly $Var1 $Terms)) 
    ( (less-than $Var1 $Var2) 
      (set-det) 
      (mul-through $Terms1 
        (poly $Var2 $Terms2) $Terms)))
  (= 
    (poly-mul $P 
      (poly $Var $Terms2) 
      (poly $Var $Terms)) 
    ( (set-det) (mul-through $Terms2 $P $Terms)))
  (= 
    (poly-mul 
      (poly $Var $Terms1) $C 
      (poly $Var $Terms)) 
    ( (set-det) (mul-through $Terms1 $C $Terms)))
  (= 
    (poly-mul $C1 $C2 $C) 
    (is $C 
      (* $C1 $C2)))


  (= 
    (term-mul Nil $_ Nil) 
    (set-det))
  (= 
    (term-mul $_ Nil Nil) 
    (set-det))
  (= 
    (term-mul 
      (Cons  $Term $Terms1) $Terms2 $Terms) 
    ( (single-term-mul $Terms2 $Term $PartA) 
      (term-mul $Terms1 $Terms2 $PartB) 
      (term-add $PartA $PartB $Terms)))


  (= 
    (single-term-mul Nil $_ Nil) 
    (set-det))
  (= 
    (single-term-mul 
      (Cons  
        (term $E1 $C1) $Terms1) 
      (term $E2 $C2) 
      (Cons  
        (term $E $C) $Terms)) 
    ( (is $E 
        (+ $E1 $E2)) 
      (poly-mul $C1 $C2 $C) 
      (single-term-mul $Terms1 
        (term $E2 $C2) $Terms)))


  (= 
    (mul-through Nil $_ Nil) 
    (set-det))
  (= 
    (mul-through 
      (Cons  
        (term $E $Term) $Terms) $Poly 
      (Cons  
        (term $E $NewTerm) $NewTerms)) 
    ( (poly-mul $Term $Poly $NewTerm) (mul-through $Terms $Poly $NewTerms)))

