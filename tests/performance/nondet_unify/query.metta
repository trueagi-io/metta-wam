;
;                        generated: 17 November 1989

;
;                        option(s): SOURCE_TRANSFORM_1

;
; 

;
;                          query

;
; 

;
;                           David H. D. Warren

;
; 

;
;                           query population and area database to find coun-

;
;                           tries of approximately equal population density



  (= 
    (top) 
    (query))


  (= 
    (query) 
    ( (query $_) (fail)))
  (= query True)

  (= 
    (query (:: $C1 $D1 $C2 $D2)) 
    ( (density $C1 $D1) 
      (density $C2 $D2) 
      (> $D1 $D2) 
      (is $T1 
        (* 20 $D1)) 
      (is $T2 
        (* 21 $D2)) 
      (< $T1 $T2)))


  (= 
    (density $C $D) 
    ( (pop $C $P) 
      (area $C $A) 
      (is $D 
        (// 
          (* $P 100) $A))))

;
;                            populations in 100000's


  (= 
    (pop  china 8250) True)
  (= 
    (pop  india 5863) True)
  (= 
    (pop  ussr 2521) True)
  (= 
    (pop  usa 2119) True)
  (= 
    (pop  indonesia 1276) True)
  (= 
    (pop  japan 1097) True)
  (= 
    (pop  brazil 1042) True)
  (= 
    (pop  bangladesh 750) True)
  (= 
    (pop  pakistan 682) True)
  (= 
    (pop  w_germany 620) True)
  (= 
    (pop  nigeria 613) True)
  (= 
    (pop  mexico 581) True)
  (= 
    (pop  uk 559) True)
  (= 
    (pop  italy 554) True)
  (= 
    (pop  france 525) True)
  (= 
    (pop  philippines 415) True)
  (= 
    (pop  thailand 410) True)
  (= 
    (pop  turkey 383) True)
  (= 
    (pop  egypt 364) True)
  (= 
    (pop  spain 352) True)
  (= 
    (pop  poland 337) True)
  (= 
    (pop  s_korea 335) True)
  (= 
    (pop  iran 320) True)
  (= 
    (pop  ethiopia 272) True)
  (= 
    (pop  argentina 251) True)

;
;                                       areas in 1000's of square miles


  (= 
    (area  china 3380) True)
  (= 
    (area  india 1139) True)
  (= 
    (area  ussr 8708) True)
  (= 
    (area  usa 3609) True)
  (= 
    (area  indonesia 570) True)
  (= 
    (area  japan 148) True)
  (= 
    (area  brazil 3288) True)
  (= 
    (area  bangladesh 55) True)
  (= 
    (area  pakistan 311) True)
  (= 
    (area  w_germany 96) True)
  (= 
    (area  nigeria 373) True)
  (= 
    (area  mexico 764) True)
  (= 
    (area  uk 86) True)
  (= 
    (area  italy 116) True)
  (= 
    (area  france 213) True)
  (= 
    (area  philippines 90) True)
  (= 
    (area  thailand 200) True)
  (= 
    (area  turkey 296) True)
  (= 
    (area  egypt 386) True)
  (= 
    (area  spain 190) True)
  (= 
    (area  poland 121) True)
  (= 
    (area  s_korea 37) True)
  (= 
    (area  iran 628) True)
  (= 
    (area  ethiopia 350) True)
  (= 
    (area  argentina 1080) True)

