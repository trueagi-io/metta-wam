
!(import! &self dv-pl)

; Initialize a new state for exists-props
; !(bind! &exists-props (new-state Nil))
((context5 $ctx) :-
 (

   ; Define the structure of $Houses and initialize it
   (same $Houses 
      (Places (House $_7 $_8) 
      (Places (House $_9 $_10) Nil))))
       
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
   (get-declare-num nationality $Num1 in the $ctx)
   (get-declare-num color $Num2 in the $ctx)
   (get-declare-num pet $Num3 in the $ctx)
   (get-declare-num drink $Num4 in the $ctx)
   (get-declare-num drink $Num5 in the $ctx) ; this should not change the context since its already there
   (get-declare-num smokes $Num6 in the $ctx)))


;; Main predicate to find the owner of the fish
((zebra-owner $Owner) :-
 (
   ; Define the structure of $Houses and initialize it
   (same $Houses 
      (Places (House $_1 $_2)
      (Places (House $_3 $_4))))
       
   ; Define $Props structure for all properties
   (same $Props 
      (Props nationality
      (Props color
      (Props pet
      (Props drink
      (Props smokes Nil))))))      
   
   ; Ensure the context is a combination of houses and properties
   (same $ctx ($Houses $Props))
      
   ; (is () (println! (Houses0= $ctx)))

   (same $FirstHouse (House $_1 $_2))
   ;; Clue 1: The Norwegian lives in the first house
   (first-house $FirstHouse in the $ctx)
   (declare nationality $FirstHouse norwegian in the $ctx)


   ;; Clue 2: The Brit lives in the red house
   (declare nationality $Brit brit in the $ctx)
   (declare color $Brit red in the $ctx)
   ;; Clue 3: The owner of the blue house drinks tea
   (declare color $BlueHouse blue in the $ctx)
   (declare drink $BlueHouse tea in the $ctx)

   ;; Clue 4: The owner of the red house keeps dogs
   (declare color $RedHouse red in the $ctx)
   (declare pet $RedHouse dog in the $ctx)

   ;; Clue 5: The owner of the tea-drinking house smokes Prince
   (declare drink $TeaHouse tea in the $ctx)
   (declare smokes $TeaHouse prince in the $ctx)

   ;; Determine who owns the fish
   (declare pet $FishOwner fish in the $ctx)
   (declare nationality $FishOwner $Owner in the $ctx)

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
((left-of $L $R in the $ctx) :-
 ((same $ctx ($Houses $_1)) ; Verify context contains the houses list
   (left-of-list $L $R $Houses)))

;; Helper for 'left-of' within a list structure
(left-of-list $L $R ($Cons $L ($Cons $R $_1))) ; Base case: $L is followed by $R
((left-of-list $L $R ($Cons $_1 $Rest)) :-
 ((left-of-list $L $R $Rest))) ; Recursive case: continue checking the list

;; Predicate: $A and $B are next to each other in the list
((next-to $A $B in the $ctx) :- 
 ((left-of $A $B in the $ctx))) ; $A is left of $B
((next-to $A $B in the $ctx) :-
 ((left-of $B $A in the $ctx))) ; $B is left of $A

;; Predicate: Get the first house in the list
((first-house $First in the $ctx) :-
 ((same $ctx ($Houses $_1)) ; Verify context contains houses
   (nat-nth Z $Houses $First))) ; First element (index 0)

;; Predicate: Get the center house in the list
((center-house $Center in the $ctx) :-
 ((same $ctx ($Houses $_1)) ; Verify context contains houses
   (nat-nth (S Z) $Houses $Center))) ; Center element for 3 houses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property handling predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General property declaration predicate
((declare $PropName $Object $PropValue in the $ctx) :-
 ((get-declare-num $PropName $Num in the $ctx) ; Get position for the property
   (nat-nth $Num $Template $PropValue) ; Assign value to property
   (something-existing $Exists in the $ctx) ; Ensure object exists in houses
   (same $Template $Exists) (same $Template $Object)))

;; Ensure $Exists is a member of Houses
((something-existing $Exists in the $ctx) :-
 ((same $ctx ($Houses $_1)) ; Verify context contains houses
   (member $Exists $Houses))) ; Check if $Exists is in houses


;; Map property names to their positions (numbers)
((get-declare-num $PropName $Num in the $ctx) :-
  ((same ($_1 $PropList) $ctx);Extract property list from context
   (declare-index $PropName $PropList Z $Num)));Get index of property

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
