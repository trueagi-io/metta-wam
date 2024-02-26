;
;                         generated: 7 March 1990

;
;                         option(s):

;
; 

;
;                            (deriv) times10

;
; 

;
;                            David H. D. Warren

;
; 

;
;                            symbolic derivative of ((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x



  (= 
    (top) 
    (times10))


  (= 
    (times10) 
    (d 
      (* 
        (* 
          (* 
            (* 
              (* 
                (* 
                  (* 
                    (* 
                      (* x x) x) x) x) x) x) x) x) x) x $_))


  (= 
    (d 
      (+ $U $V) $X 
      (+ $DU $DV)) 
    ( (set-det) 
      (d $U $X $DU) 
      (d $V $X $DV)))
  (= 
    (d 
      (- $U $V) $X 
      (- $DU $DV)) 
    ( (set-det) 
      (d $U $X $DU) 
      (d $V $X $DV)))
  (= 
    (d 
      (* $U $V) $X 
      (+ 
        (* $DU $V) 
        (* $U $DV))) 
    ( (set-det) 
      (d $U $X $DU) 
      (d $V $X $DV)))
  (= 
    (d 
      (/ $U $V) $X 
      (/ 
        (- 
          (* $DU $V) 
          (* $U $DV)) 
        (^ $V 2))) 
    ( (set-det) 
      (d $U $X $DU) 
      (d $V $X $DV)))
  (= 
    (d 
      (^ $U $N) $X 
      (* 
        (* $DU $N) 
        (^ $U $N1))) 
    ( (set-det) 
      (integer $N) 
      (is $N1 
        (- $N 1)) 
      (d $U $X $DU)))
  (= 
    (d 
      (- $U) $X 
      (- $DU)) 
    ( (set-det) (d $U $X $DU)))
  (= 
    (d 
      (exp $U) $X 
      (* 
        (exp $U) $DU)) 
    ( (set-det) (d $U $X $DU)))
  (= 
    (d 
      (log $U) $X 
      (/ $DU $U)) 
    ( (set-det) (d $U $X $DU)))
  (= 
    (d $X $X 1) 
    (set-det))
  (= 
    (d  $_ $_ 0) True)

