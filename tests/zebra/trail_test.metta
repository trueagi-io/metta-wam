
!(import! &self dv-pl)


; !(bind! &exists-props (new-state Nil))
((context $ctx) :-
 (

   (same $Props (Props $_H6 $_T6))
   (same $ctx ($Houses $Props))

   (get-declare-num nationality $Num1 in the $ctx)
   (get-declare-num color $Num2 in the $ctx)
   (get-declare-num pet $Num3 in the $ctx)
   (get-declare-num drink $Num4 in the $ctx)
   (get-declare-num drink $Num5 in the $ctx)))


;; Main predicate to find the owner of the fish
((zebra-owner $Owner) :-
 (
   (context $ctx)
   (same $ctx ($Houses $Props))
   (same $Houses (Places (House $__202438 $_1_202439) (Places (House $__208019 $_1_208020) (Places (House $__211968 $_1_211969) (Places (House $__215037 $_1_215038) (Places (House $__217602 $_1_217603) Nil))))))

 ;; Clue 1: The Brit lives in the red house.
   (declare nationality $Brit brit in the $ctx)
   (declare color $Brit red in the $ctx)

 ;; Clue 2: The Swede keeps dogs as pets.
   (declare nationality $Swede swede in the $ctx)
   (declare pet $Swede dog in the $ctx)

 ;; Clue 3: The Dane drinks tea.
   (declare nationality $Dane dane in the $ctx)
   (declare drink $Dane tea in the $ctx)

 ;; Clue 4: The green house is immediately to the left of the white house.
   (left-of $Green $White in the $ctx)
   (declare color $Green green in the $ctx)
   (declare color $White white in the $ctx)

 ;; Clue 5: The owner of the green house drinks coffee.
   (declare color $Green green in the $ctx)
   (declare drink $Green coffee in the $ctx)

 ;; Clue 6: The person who smokes Pall Mall rears birds.
   (declare smokes $PallMallSmoker pallmall in the $ctx)
   (declare pet $PallMallSmoker birds in the $ctx)

 ;; Clue 7: The owner of the yellow house smokes Dunhill.
   (declare color $Yellow yellow in the $ctx)
   (declare smokes $Yellow dunhill in the $ctx)

 ;; Clue 8: The man living in the center house drinks milk.
   (center-house $CenterHouse in the $ctx)
   (declare drink $CenterHouse milk in the $ctx)

 ;; Clue 9: The Norwegian lives in the first house.
   (first-house $FirstHouse in the $ctx)
   (declare nationality $FirstHouse norwegian in the $ctx)

 ;; Clue 10: The man who smokes Blends lives next to the one who keeps cats.
   (declare smokes $SmokesBlends blends in the $ctx)
   (next-to $SmokesBlends $HouseCat in the $ctx)
   (declare pet $HouseCat cat in the $ctx)

 ;; Clue 11: The man who keeps horses lives next to the man who smokes Dunhill.
   (declare pet $HorseKeeper horse in the $ctx)
   (next-to $HorseKeeper $SmokesDunhills in the $ctx)
   (declare smokes $SmokesDunhills dunhill in the $ctx)

 ;; Clue 12: The owner who smokes BlueMaster drinks beer.
   (declare smokes $SmokesBlueMaster bluemaster in the $ctx)
   (declare drink $SmokesBlueMaster beer in the $ctx)

 ;; Clue 13: The German smokes Prince.
   (declare nationality $German german in the $ctx)
   (declare smokes $German prince in the $ctx)

 ;; Clue 14: The Norwegian lives next to the blue house.
   (declare nationality $Norwegian norwegian in the $ctx)
   (next-to $Norwegian $HouseBlue in the $ctx)
   (declare color $HouseBlue blue in the $ctx)

 ;; Clue 15: The man who smokes Blends has a neighbor who drinks water.
   (declare smokes $BlendsSmoker blends in the $ctx)
   (next-to $BlendsSmoker $WaterDrinker in the $ctx)
   (declare drink $WaterDrinker water in the $ctx)

 ;; Determine who owns the fish
   (declare pet $FishOwner fish in the $ctx)
   (declare nationality $FishOwner $Owner in the $ctx)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize Houses as a list of uninitialized exists
((init-houses $Num $Houses) :-
 (make-houses $Num $Houses))

((make-houses (S $N) ($Cons $House $Rest)) :-
 ((create-exists $House)
   (make-houses $N $Rest)))
(make-houses Z Nil)

(create-exists ($Cons $_ $_1))

;; Predicate: L is immediately to the left of R in the list
((left-of $L $R in the $ctx) :-
 ((same $ctx ($Houses $_))
   (left-of-list $L $R $Houses)))

(left-of-list $L $R ($Cons $L ($Cons $R $_)))
((left-of-list $L $R ($Cons $_ $Rest)) :-
 ((left-of-list $L $R $Rest)))

;; Predicate: A and B are next to each other in the list
((next-to $A $B in the $ctx) :- 
 ((left-of $A $B in the $ctx)))
((next-to $A $B in the $ctx) :-
 ((left-of $B $A in the $ctx)))

;; Predicate to get the first house in the list
((first-house $First in the $ctx) :-
 ((same $ctx ($Houses $_))
   (nat-nth Z $Houses $First)))

;; Predicate to get the center house in the list
((center-house $Center in the $ctx) :-
 ((same $ctx ($Houses $_))
   (nat-nth (S (S Z)) $Houses $Center)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property handling predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General property declaration predicate
((declare $PropName $Object $PropValue in the $ctx) :-
 ((get-declare-num $PropName $Num in the $ctx)
   (nat-nth $Num $Template $PropValue)
   (something-existing $Exists in the $ctx)
   (same $Template $Exists) (same $Template $Object)))

;; Ensure Exists is a member of Houses
((something-existing $Exists in the $ctx) :-
 ((same $ctx ($Houses $_))
   (member $Exists $Houses)))

;; Map property names to their positions (numbers)
((get-declare-num $PropName $Num in the $ctx) :-
  ((same ($_ $PropList) $ctx)
   (declare-index $PropName $PropList Z $Num)))

((get-declare-num-not-working $PropName $Num in the $ctx) :-
  ((same ($_ $PropList) $ctx)
   (eval= $dc (declare-index-m $PropName $PropList Z $Num))))

(: declare-index-m (-> Symbol Expression Nat Nat Bool))
(= (declare-index-m $PropName $PropList $Acc $Num)
  (case $PropList
	((($Cons $PropName $_) (nat-equal $Acc $Num))
     (($Cons $_ $Rest) (declare-index-m $PropName $Rest (S $Acc) $Num)))))

(declare-index $PropName ($Cons $PropName $_) $Num $Num)
((declare-index $PropName ($Cons $Nonvar $Rest) $Acc $Num) :- ( (nonvar $Nonvar) (declare-index $PropName $Rest (S $Acc) $Num)))

