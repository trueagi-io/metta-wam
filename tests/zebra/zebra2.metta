
!(import! &self dv-pl)

; Initialize a new state for exists-props
; !(bind! &exists-props (new-state Nil))
((context5 $ctx) :-
 (

   ; Define the structure of $Houses and initialize it
   (same $Houses 
      (Places (House $_1 $_2) 
      (Places (House $_3 $_4) 
      (Places (House $_5 $_6) 
      (Places (House $_7 $_8) 
      (Places (House $_9 $_10) Nil))))))
      
   (set-global &neighborhood $Houses)
       
   ; Define $Props structure for all properties
   (same $Props 
      (Props nationality
      (Props color
      (Props pet
      (Props drink
      (Props smokes Nil))))))
   
   
   ; Ensure $Props represents a specific structure
   (same $Props (Props $_H6 $_T6))
   ; Ensure $ctx is a combination of $Houses and $Props
   (same $ctx ($Houses $Props))

   ; Declare numerical properties for various attributes in the context
   (get-declare-num nationality $Num1)
   (get-declare-num color $Num2)
   (get-declare-num pet $Num3)
   (get-declare-num drink $Num4)
   (get-declare-num drink $Num5) ; this should not change the context since its already there
   (get-declare-num smokes $Num6)))


;; Main predicate to find the owner of the fish
((zebra-owner $Owner) :-
 (
   (set-global &exists-props $_Props)
   (init-houses (S (S (S (S (S Z))))))

      
 ;; Clue 8: The man in the center house drinks milk
   (center-house $CenterHouse)
   (declare drink $CenterHouse milk)   
   
 ;; Clue 9: The Norwegian lives in the first house
   (first-house $FirstHouse)
   (declare nationality $FirstHouse norwegian)

 ;; Clue 14: The Norwegian lives next to the blue house
   (declare nationality $Norwegian norwegian)
   (next-to $Norwegian $HouseBlue)
   (declare color $HouseBlue blue)

 ;; Clue 1: The Brit lives in the red house
   (declare nationality $Brit brit)
   (declare color $Brit red)

 ;; Clue 2: The Swede keeps dogs as pets
   (declare nationality $Swede swede)
   (declare pet $Swede dog)

 ;; Clue 3: The Dane drinks tea
   (declare nationality $Dane dane)
   (declare drink $Dane tea)

 ;; Clue 4: The green house is immediately to the left of the white house
   (left-of $Green $White)
   (declare color $Green green)
   (declare color $White white)

 ;; Clue 5: The owner of the green house drinks coffee
   (declare color $Green green)
   (declare drink $Green coffee)

 ;; Clue 6: The person who smokes Pall Mall rears birds
   (declare smokes $PallMallSmoker pallmall)
   (declare pet $PallMallSmoker birds)

 ;; Clue 7: The owner of the yellow house smokes Dunhill
   (declare color $Yellow yellow)
   (declare smokes $Yellow dunhill)

 ;; Clue 10: The man who smokes Blends lives next to the one who keeps cats
   (declare smokes $SmokesBlends blends)
   (next-to $SmokesBlends $HouseCat)
   (declare pet $HouseCat cat)

 ;; Clue 11: The man who keeps horses lives next to the man who smokes Dunhill
   (declare pet $HorseKeeper horse)
   (next-to $HorseKeeper $SmokesDunhills)
   (declare smokes $SmokesDunhills dunhill)

 ;; Clue 12: The owner who smokes BlueMaster drinks beer
   (declare smokes $SmokesBlueMaster bluemaster)
   (declare drink $SmokesBlueMaster beer)

 ;; Clue 13: The German smokes Prince
   (declare nationality $German german)
   (declare smokes $German prince)

 ;; Clue 15: The man who smokes Blends has a neighbor who drinks water
   (declare smokes $BlendsSmoker blends)
   (next-to $BlendsSmoker $WaterDrinker)
   (declare drink $WaterDrinker water)

 ;; Determine who owns the fish
   (declare pet $FishOwner fish)
   (declare nationality $FishOwner $Owner)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Initialize Houses as a list of uninitialized 'exists'
((init-houses $Num $Houses) :-
 (make-houses $Num $Houses))

;; Recursive creation of houses list based on number
((make-houses (S $N) ($Cons $House $Rest)) :-
 ((create-exists $House) ; Initialize a new 'exists' for the house
   (make-houses $N $Rest))) ; Continue for the remaining houses
(make-houses Z Nil) ; Base case: no houses left to create

;; Create an 'exists' structure for a house
(create-exists ($Cons $_1 $_1))

;; Predicate: $L is immediately to the left of $R in the list
((left-of $L $R) :-
 ((get-global &neighborhood $Houses) ; Verify context contains the houses list
   (left-of-list $L $R $Houses)))

;; Helper for 'left-of' within a list structure
(left-of-list $L $R ($Cons $L ($Cons $R $_1))) ; Base case: $L is followed by $R
((left-of-list $L $R ($Cons $_1 $Rest)) :-
 ((left-of-list $L $R $Rest))) ; Recursive case: continue checking the list

;; Predicate: $A and $B are next to each other in the list
((next-to $A $B) :- 
 ((left-of $A $B))) ; $A is left of $B
((next-to $A $B) :-
 ((left-of $B $A))) ; $B is left of $A

;; Predicate: Get the first house in the list
((first-house $First) :-
 ((get-global &neighborhood $Houses) ; Verify context contains houses
   (nat-nth Z $Houses $First))) ; First element (index 0)

;; Predicate: Get the center house in the list
((center-house $Center) :-
 ((get-global &neighborhood $Houses) ; Verify context contains houses
   (nat-nth (S (S Z)) $Houses $Center))) ; Center element for 5 houses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property handling predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General property declaration predicate
((declare $PropName $Object $PropValue) :-
 ((get-declare-num $PropName $Num) ; Get position for the property
   (nat-nth $Num $Template $PropValue) ; Assign value to property
   (something-existing $Exists) ; Ensure object exists in houses
   (same $Template $Exists) (same $Template $Object)))

;; Ensure $Exists is a member of Houses
((something-existing $Exists) :-
 ((get-global &neighborhood $Houses) ; Verify context contains houses
   (member $Exists $Houses))) ; Check if $Exists is in houses


;; Map property names to their positions (numbers)
((get-declare-num $PropName $Num) :-
  ((get-global &exists-props $PropList) ; Extract property list from context
   (declare-index $PropName $PropList Z $Num) ; Get index of property
   (set-global &exists-props $PropList))) ; Set index of property

;; Map property names using manual declaration with incremental indexing
(declare-index $PropName ($Cons $PropName $_1) $Num $Num) ; Found property
((declare-index $PropName ($Cons $Nonvar $Rest) $Acc $Num) :- 
 ((nonvar $Nonvar) (declare-index $PropName $Rest (S $Acc) $Num))) ; Increment index


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL TEST QUERIES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Solve the zebra-owner problem (Who owns the fish?)
!(query (zebra-owner $Owner)) ; Should find the owner of the fish

;; Basic declaration tests
;!(query (declare nationality $Brit brit)) ; Tests nationality declaration
;!(query (something-existing $Exists)) ; Tests if an entity exists in the context

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL LOGICAL RELATIONSHIPS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verify properties related to the zebra-owner problem;
;!(query (declare nationality $Owner norwegian)) ; Declare that $Owner is Norwegian



!(query (is $x (+ 1 1)))
!(query (same $ctx ($Houses $Props)))
!(query (context $ctx))

