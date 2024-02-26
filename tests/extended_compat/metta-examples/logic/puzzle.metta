



  (= 
    (foo1) 
    ( (= $Employers 
        (:: $_ $_ $_)) 
      (member 
        (:: boris $_ has-sister) $Employers) 
      (nth1 1 $Employers 
        (:: $_ cashier no-sister)) 
      (nextto 
        (:: $_ controller $_) 
        (:: semyon $_ $_) $Employers) 
      (member 
        (:: ivan $_ $_) $Employers) 
      (member 
        (:: $_ supervisor $_) $Employers) 
      (writeq $Employers) 
      (nl) 
      (call halt)))
;                     ;   /name, position, has sister/

;                     ;    /Boris has sister/

;                     ;    /cashier is the shortest and has no sister/

;                     ;    /Semyon is higher than controller/




  (= 
    (nextto $X $Y 
      (Cons  $X 
        (Cons  $Y $_))) 
    (set-det))
  (= 
    (nextto $X $Y 
      (Cons  $_ $Zs)) 
    (nextto $X $Y $Zs))


  (= 
    (foo) 
    (time foo1))


  !foo




