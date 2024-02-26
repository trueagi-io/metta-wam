;
;                         Where does the zebra live?

;
;                          Puzzle solution written by Claude Sammut.


  (= 
    (top) 
    ( (houses $Houses) 
      (member 
        (house red english $_ $_ $_) $Houses) 
      (member 
        (house $_ spanish dog $_ $_) $Houses) 
      (member 
        (house green $_ $_ coffee $_) $Houses) 
      (member 
        (house $_ ukrainian $_ tea $_) $Houses) 
      (right-of 
        (house green $_ $_ $_ $_) 
        (house ivory $_ $_ $_ $_) $Houses) 
      (member 
        (house $_ $_ snails $_ winstons) $Houses) 
      (member 
        (house yellow $_ $_ $_ kools) $Houses) 
      (= $Houses 
        (:: $_ $_ 
          (house $_ $_ $_ milk $_) $_ $_)) 
      (= $Houses 
        (Cons  
          (house $_ norwegian $_ $_ $_) $_)) 
      (next-to 
        (house $_ $_ $_ $_ chesterfields) 
        (house $_ $_ fox $_ $_) $Houses) 
      (next-to 
        (house $_ $_ $_ $_ kools) 
        (house $_ $_ horse $_ $_) $Houses) 
      (member 
        (house $_ $_ $_ orange-juice lucky-strikes) $Houses) 
      (member 
        (house $_ japanese $_ $_ parliaments) $Houses) 
      (next-to 
        (house $_ norwegian $_ $_ $_) 
        (house blue $_ $_ $_ $_) $Houses) 
      (member 
        (house $_ $_ zebra $_ $_) $Houses) 
      (member 
        (house $_ $_ $_ water $_) $Houses)))
;
;                         	print_houses(Houses).



  (= 
    (houses  
      ( (house  $_ $_ $_ $_ $_) 
        (house  $_ $_ $_ $_ $_) 
        (house  $_ $_ $_ $_ $_) 
        (house  $_ $_ $_ $_ $_) 
        (house  $_ $_ $_ $_ $_))) True)


  (= 
    (right_of  $A $B 
      (Cons  $B 
        (Cons  $A $_))) True)
  (= 
    (right-of $A $B 
      (Cons  $_ $Y)) 
    (right-of $A $B $Y))


  (= 
    (next_to  $A $B 
      (Cons  $A 
        (Cons  $B $_))) True)
  (= 
    (next_to  $A $B 
      (Cons  $B 
        (Cons  $A $_))) True)
  (= 
    (next-to $A $B 
      (Cons  $_ $Y)) 
    (next-to $A $B $Y))


  (= 
    (member  $X 
      (Cons  $X $_)) True)
  (= 
    (member $X 
      (Cons  $_ $Y)) 
    (member $X $Y))


  (= 
    (print-houses (Cons  $A $B)) 
    ( (set-det) 
      (write $A) 
      (nl) 
      (print-houses $B)))
  (= 
    (print_houses  ()) True)

