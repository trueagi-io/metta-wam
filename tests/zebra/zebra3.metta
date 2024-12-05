!(add-logic
  ;; Main predicate to find the owner of the fish
  ((zebra-owner $Owner)
   :-
   ;; Initialize Houses and exists_props
   (init-houses 5)
   (set-global 'exists-props nil)

   ;; Clue 1: The Brit lives in the red house.
   (declare nationality $Brit brit)
   (declare color $Brit red)

   ;; Clue 2: The Swede keeps dogs as pets.
   (declare nationality $Swede swede)
   (declare pet $Swede dog)

   ;; Clue 3: The Dane drinks tea.
   (declare nationality $Dane dane)
   (declare drink $Dane tea)

   ;; Clue 4: The green house is immediately to the left of the white house.
   (left-of $Green $White)
   (declare color $Green green)
   (declare color $White white)

   ;; Clue 5: The owner of the green house drinks coffee.
   (declare color $Green green)
   (declare drink $Green coffee)

   ;; Clue 6: The person who smokes Pall Mall rears birds.
   (declare smokes $PallMallSmoker pallmall)
   (declare pet $PallMallSmoker birds)

   ;; Clue 7: The owner of the yellow house smokes Dunhill.
   (declare color $Yellow yellow)
   (declare smokes $Yellow dunhill)

   ;; Clue 8: The man living in the center house drinks milk.
   (center-house $CenterHouse)
   (declare drink $CenterHouse milk)

   ;; Clue 9: The Norwegian lives in the first house.
   (first-house $FirstHouse)
   (declare nationality $FirstHouse norwegian)

   ;; Clue 10: The man who smokes Blends lives next to the one who keeps cats.
   (declare smokes $SmokesBlends blends)
   (next-to $SmokesBlends $HouseCat)
   (declare pet $HouseCat cat)

   ;; Clue 11: The man who keeps horses lives next to the man who smokes Dunhill.
   (declare pet $HorseKeeper horse)
   (next-to $HorseKeeper $SmokesDunhills)
   (declare smokes $SmokesDunhills dunhill)

   ;; Clue 12: The owner who smokes BlueMaster drinks beer.
   (declare smokes $SmokesBlueMaster bluemaster)
   (declare drink $SmokesBlueMaster beer)

   ;; Clue 13: The German smokes Prince.
   (declare nationality $German german)
   (declare smokes $German prince)

   ;; Clue 14: The Norwegian lives next to the blue house.
   (declare nationality $Norwegian norwegian)
   (next-to $Norwegian $HouseBlue)
   (declare color $HouseBlue blue)

   ;; Clue 15: The man who smokes Blends has a neighbor who drinks water.
   (declare smokes $BlendsSmoker blends)
   (next-to $BlendsSmoker $WaterDrinker)
   (declare drink $WaterDrinker water)

   ;; Determine who owns the fish
   (declare pet $FishOwner fish)
   (declare nationality $FishOwner $Owner)
   ))

;; Initialize Houses as a list of uninitialized exists
!(add-logic
  ((init-houses $Num)
   :-
   (make-houses $Num $Houses)
   (set-global 'neighborhood $Houses)))

!(add-logic
  ((make-houses 0 nil))
  ((make-houses $N (Cons $House $Rest))
   :-
   (> $N 0)
   (- $N 1 $N1)
   (create-exists $House)
   (make-houses $N1 $Rest)))

!(add-logic
  ((create-exists (Cons _ _))))

;; Helper predicates

;; Predicate: L is immediately to the left of R in the list
!(add-logic
  ((left-of $L $R)
   :-
   (get-global 'neighborhood $Houses)
   (left-of-list $L $R $Houses)))

!(add-logic
  ((left-of-list $L $R (Cons $L (Cons $R _))))
  ((left-of-list $L $R (Cons _ $Rest))
   :-
   (left-of-list $L $R $Rest)))

;; Predicate: A and B are next to each other in the list
!(add-logic
  ((next-to $A $B)
   :-
   (left-of $A $B))
  ((next-to $A $B)
   :-
   (left-of $B $A)))

;; Predicate to get the first house in the list
!(add-logic
  ((first-house $First)
   :-
   (get-global 'neighborhood $Houses)
   (nth 0 $Houses $First)))

;; Predicate to get the center house in the list
!(add-logic
  ((center-house $Center)
   :-
   (get-global 'neighborhood $Houses)
   (nth 2 $Houses $Center)))

;; Property handling predicates

;; General property declaration predicate
!(add-logic
  ((declare $PropName $Exists $PropValue)
   :-
   (prop $PropName $Exists $PropValue)))

!(add-logic
  ((prop $PropName $Object $PropValue)
   :-
   (get-prop-num $PropName $Num)
   (nth $Num $Object $PropValue)
   (something-existing $Object)))

;; Ensure Exists is a member of Houses
!(add-logic
  ((something-existing $Exists)
   :-
   (get-global 'neighborhood $Houses)
   (member $Exists $Houses)))

;; Member predicate for lists
!(add-logic
  ((member $Elem (Cons $Elem _)))
  ((member $Elem (Cons _ $Tail))
   :-
   (member $Elem $Tail)))

;; Map property names to their positions (numbers)
!(add-logic
  ((get-prop-num $PropName $Num)
   :-
   (get-global 'exists-props $PropList)
   (prop-index $PropName $PropList 0 $Num)))

!(add-logic
  ((prop-index $PropName (Cons $PropName _) $Num $Num))
  ((prop-index $PropName (Cons _ $Rest) $Acc $Num)
   :-
   (+ $Acc 1 $Acc1)
   (prop-index $PropName $Rest $Acc1 $Num))
  ((prop-index $PropName nil $Acc $Acc)
   :-
   ;; If property not found, add it
   (get-global 'exists-props $PropList)
   (append $PropList (list $PropName) $NewPropList)
   (set-global 'exists-props $NewPropList)))

;; Zero-based nth predicate
!(add-logic
  ((nth 0 (Cons $Elem _) $Elem))
  ((nth $N (Cons _ $Rest) $Elem)
   :-
   (> $N 0)
   (- $N 1 $N1)
   (nth $N1 $Rest $Elem)))

;; Append two lists
!(add-logic
  ((append nil $List2 $List2))
  ((append (Cons $H $T) $List2 (Cons $H $T2))
   :-
   (append $T $List2 $T2)))

