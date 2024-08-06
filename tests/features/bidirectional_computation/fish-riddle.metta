; Einstein Riddle Example

(= (nth1 N (Cons $H $T))
  (if (== $N 1) $H
	 (nth1 (- $N 1) $T)))


(= (append Nil $L) $L)
(= (append (Cons $H $T) $L) (Cons $H (append $T $L))


(= (length (Cons  $_ $T) $T) (length (+ 1 (Cons  $_ $T))))


; constructors/constrainers
(= (color $Color (house $Color $_ $_ $_ $_))  True)
(= (nationality $Nationality (house $_ $Nationality $_ $_ $_))  True)
(= (beverage $Beverage (house $_ $_ $Beverage $_ $_))  True)
(= (cigarette $Cigarette (house $_ $_ $_ $Cigarette $_))  True)
(= (pet $Pet (house $_ $_ $_ $_ $Pet))  True)



;
; Next to

(= (next-to $X $Y) (append $_ (Cons $X (Cons $Y $_))))
(= (next-to $X $Y) (append $_ (Cons $Y (Cons $X $_))))

; Left of
(= (left-of $Left $Right) (Cons $Left (Cons $Right $_)))
(= (left-of $Left $Right (Cons $_ $Tail)) 
      (left-of $Left $Right $Tail))

; part of
(= (partOf $Item (Cons $Item $_))  True)
(= (partOf $Item (Cons $_ $Tail)) 
     (partOf $Item $Tail))

;
; Generates a neighborhood with a specified number of houses
(= (new-neighborhood $N) 
 (let $Neighborhood 
    (= $n (length $Neighborhood)) 
      $Neighborhood))
;
; Calculates the center position(s) in the neighborhood

(= (center-of $Neighborhood)    
  (= $CNth 
	(+ (div (length $Neighborhood) 2) 1)) 
  (nth1 $CNth $Neighborhood))

(= (center-of $Neighborhood) 
  (let $Length 
	   (length $Neighborhood) 
  (if (== (mod $Length 2) 0)
	(nth1 (div $Length 2) $Neighborhood))))

;
; Construct the neighboorhood using the constraints given
(= 
 (the-neighboors $Neighborhood) 

  ((= $Neighborhood (new-neighborhood 5))

  ;; 1. The Englishman lives in the red house.
  (nationality english $EnglishmansHouse) 
  (color red $EnglishmansHouse) 
  (partOf $EnglishmansHouse $Neighborhood) 

  ;; 2. The Swede keeps dogs as pets.
  (nationality swede $SwedesHouse) 
  (pet dogs $SwedesHouse) 
  (partOf $SwedesHouse $Neighborhood) 
  (nationality dane $DanesHouse) 

  ;; 3. The Dane drinks tea.
  (beverage tea $DanesHouse) 
  (partOf $DanesHouse $Neighborhood) 

  ;; 5. The green house is on the left of the white house.
  (color green $GreenHouse) 
  (color white $WhiteHouse) 

  ;; 6. The green house's owner drinks coffee.
  (left-of $GreenHouse $WhiteHouse $Neighborhood) 
  (beverage coffee $GreenHouse) 

  ;; 7. The person who smokes Pall Mall rears birds.
  (cigarette pallmall $PallMallHouse) 
  (pet birds $PallMallHouse) 
  (partOf $PallMallHouse $Neighborhood) 

  ;; 8. The owner of the yellow house smokes Dunhill.
  (color yellow $YellowHouse) 
  (cigarette dunhill $YellowHouse) 
  (partOf $YellowHouse $Neighborhood) 

  ;; 9. The man living in the center house drinks milk.
  (beverage milk (center-of $Neighborhood))

  ;; 10. The Norwegian lives in the first house.
  (nationality norwegian $NorwegiansHouse) 
  (nth1 1 $Neighborhood $NorwegiansHouse) 

  ;; 11. The man who smokes Blends lives next to the one who keeps cats.
  (cigarette blends $BlendsHouse) 
  (next-to $BlendsHouse $CatsHouse $Neighborhood) 
  (pet cats $CatsHouse) 
  (pet horse $HorseHouse) 

  ;; 12. The man who keeps the horse lives next to the one who smokes Dunhill.
  (cigarette dunhill $DunhillNeighbor) 
  (next-to $HorseHouse $DunhillNeighbor $Neighborhood) 

  ;; 13. The owner who smokes BlueMaster drinks beer.
  (cigarette bluemaster $BlueMasterHouse) 
  (beverage beer $BlueMasterHouse) 
  (partOf $BlueMasterHouse $Neighborhood) 

  ;; 14. The German smokes Prince.
  (nationality german $GermansHouse) 
  (cigarette prince $GermansHouse) 
  (partOf $GermansHouse $Neighborhood) 

  ;; 15. The Norwegian lives next to the blue house.
  (next-to $NorwegiansHouse $BlueNeighbor $Neighborhood) 
  (color blue $BlueNeighbor) 

  ;; 16. The man who smokes Blends has a neighbor who drinks water.
  (next-to $BlendsHouse $BlendsNeighor $Neighborhood) 
  (beverage water $BlendsNeighor)))


; Determine who owns the fish
!((the-neighboors $Neighborhood) 
  (pet fish $FishHouse)
  (partOf $FishHouse $Neighborhood)
  (nationality $FishOwner $FishHouse))



;;  Excersize
;
;  Rewrite the above that will experimenmtally remove constraints
;  to determine which are unnesisary for concluidng who has the fish


