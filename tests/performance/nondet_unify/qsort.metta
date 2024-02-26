;
;                       generated: 16 November 1989

;
;                       option(s): SOURCE_TRANSFORM_1

;
; 

;
;                          qsort

;
; 

;
;                          David H. D. Warren

;
; 

;
;                           quicksort a list of 50 integers



  (= 
    (top) 
    (qsort))


  (= 
    (qsort) 
    ( (data $List) (qsort $List $_ Nil)))

  (= 
    (qsort 
      (Cons  $X $L) $R $R0) 
    ( (partition $L $X $L1 $L2) 
      (qsort $L2 $R1 $R0) 
      (qsort $L1 $R 
        (Cons  $X $R1))))
  (= 
    (qsort  () $R $R) True)


  (= 
    (partition 
      (Cons  $X $L) $Y 
      (Cons  $X $L1) $L2) 
    ( (=< $X $Y) 
      (set-det) 
      (partition $L $Y $L1 $L2)))
  (= 
    (partition 
      (Cons  $X $L) $Y $L1 
      (Cons  $X $L2)) 
    (partition $L $Y $L1 $L2))
  (= 
    (partition  () $_ () ()) True)


  (= 
    (data  
      (27 74 17 33 94 18 46 83 65 2 32 53 28 85 99 47 28 82 6 11 55 29 39 81 90 37 10 0 66 51 7 21 85 27 31 63 75 4 95 99 11 28 61 74 18 92 40 53 59 8)) True)

