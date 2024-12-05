
; !(bind! &exists-props (new-state Nil))

;; Main predicate to find the owner of the fish
((zebra-owner $Owner) :-
 (

   (same $Houses (Cons (Cons $__202438 $_1_202439) (Cons (Cons $__208019 $_1_208020) (Cons (Cons $__211968 $_1_211969) (Cons (Cons $__215037 $_1_215038) (Cons (Cons $__217602 $_1_217603) Nil))))))
   (same $Props (Cons $_H6 $_T6))
   (same $ctx ($Houses $Props))

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

((make-houses (S $N) (Cons $House $Rest)) :-
 ((create-exists $House)
   (make-houses $N $Rest)))
(make-houses Z Nil)

(create-exists (Cons $_ $_1))


;; Predicate: L is immediately to the left of R in the list
((left-of $L $R in the $ctx) :-
 ((same $ctx ($Houses $_))
   (left-of-list $L $R $Houses)))

(left-of-list $L $R (Cons $L (Cons $R $_)))
((left-of-list $L $R (Cons $_ $Rest)) :-
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
   (same $Template $Object) (same $Template $Object)))

;; Ensure Exists is a member of Houses
((something-existing $Exists in the $ctx) :-
 ((same $ctx ($Houses $_))
   (member $Exists $Houses)))

;; Member predicate for lists
(member $Elem (Cons $Elem $_))
((member $Elem (Cons $_ $Tail)) :-
 ((member $Elem $Tail)))

;; Map property names to their positions (numbers)
((get-declare-num $PropName $Num in the $ctx) :-
 ((same ($_ $PropList) $ctx)
   (declare-index $PropName $PropList Z $Num)))

(declare-index $PropName (Cons $PropName $_) $Num $Num)
((declare-index $PropName (Cons $_ $Rest) $Acc $Num) :-
 ((declare-index $PropName $Rest (S $Acc) $Num)))
((declare-index $PropName $PropList $Acc $Acc) :-  ;; If property not found, add it
   (cons-append $PropList (Cons $PropName $_Open) $_Ignored))

;; Zero-based nat-nth predicate
(nat-nth Z (Cons $Elem $_) $Elem)
((nat-nth (S $N) (Cons $_ $Rest) $Elem) :-
 ((nat-nth $N $Rest $Elem)))

;; Append two lists
(cons-append Nil $List2 $List2)
((cons-append (Cons $H $T) $List2 (Cons $H $T2)) :-
 ((cons-append $T $List2 $T2)))


(same $x $x)

;; last of list
(last $x (Cons $x Nil))
((last $x (Cons $_ $rest)) :- ( (last $x $rest) ) )

;; length list
(length Nil 0)
((length (Cons $x $rest) $out) :- (
	(length $rest $out2) 
	(is $out (+ $out2 1))
)
)

;; sum from 0 to n
(sum 0 0)
((sum $n $s) :- (
	(bool (> $n 0))
	(is $n1 (- $n 1))
	(sum $n1 $s1)
	(is $s (+ $n $s1))
))

;; list append
(append Nil $L $L)
((append (Cons $H $T) $L (Cons $H $R)) :- (
	(append $T $L $R)
))

;; negation as failure test
((bachelor $x) :- (
	(man $x)
	(naf (married $x))
))
(man John) (man Tim)
(married Tim)

((woman $x) :- (
	(naf (man $x))
))


;; First-order logic backward chainer using match

;; TODO: this is inefficient, we just need at least 1 match
(: has-match (-> Atom Atom Bool))
(= (has-match $space $g)
	(let $m (collapse (match $space $g True))
		(if (== $m ())
			False
			True
		)
	)
)

(: backward-chain (-> Atom Atom Atom Atom))
(: backward-chain1 (-> Atom Atom Atom Atom))
(: backward-chain2 (-> Atom Atom Atom Atom))

(= (backward-chain $goal $kb $rb) (backward-chain2 $goal $kb $rb))
(= (backward-chain $goal $kb $rb) (backward-chain1 $goal $kb $rb))

;; Base case
(= (backward-chain1 $goal $kb $rb)
	(case $goal (
		((is $a $b) (let $a $b (is $a $b))) ;; prolog eval & assign
		((bool $expr) (if $expr $goal (empty))) ;; if predicate is a boolean. This might be unnecessary
		((eval $expr) (let $_ $expr True))  ;; if predicate is a boolean. This might be unnecessary
		((naf $expr) (if (has-match $kb $expr) (empty) $goal)) ;; negation-as-failure
		($_ (match $kb $goal $goal))
	)) 
)

;; Recursive Case
(= (backward-chain2 $goal $kb $rb)
	(match $rb ($goal :- $body)
		(match-body $body $kb $rb $goal)
	)
)

(: match-body (-> Atom Atom Atom Atom Atom))
;; do matching for each atom in the body, then returns goal
(= (match-body $body $kb $rb $goal)
	(if (== $body ())
		$goal
		(let* (
			(($cur $rest) (decons-atom $body))
			 (() (println! (IN $cur $goal)))
			($cur (backward-chain $cur $kb $rb))  ;; chain on cur & unify back to propagate vars to $goal
			 (() (println! (OUT $cur $goal)))
		) 
			(match-body $rest $kb $rb $goal)
		)
	)
)

(: query (-> Atom Atom))
; !(bind! &baseall (new-space))
(= (query $goal)
	(backward-chain $goal &self &self)
)

;;;;;;;;;;;;;;;;;
;; TEST query ;;;
;;;;;;;;;;;;;;;;;;


; last element of list
!(query (last $x (Cons 1 (Cons 2 (Cons 3 Nil)))))

;; length of list
!(query (length (Cons 1 (Cons 2 (Cons 3 Nil))) $out))

;; sum up to 3
!(query (sum 3 $x))

;; append (multi-directional)
!(query (append (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 4 Nil)) $x))
!(query (append (Cons 1 (Cons 2 Nil)) $Out (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))
!(query (append $What $Out (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))

;; Negation as failure test
!(query (bachelor $x))
!(query (woman Tim)) ;; should be no
!(query (woman Jane)) ;; should succeed
!(query (woman $x)) ;; should be no


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL TEST QUERIES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if an element is a member of a list
;!(query (member 3 (Cons 1 (Cons 2 (Cons 3 Nil)))))
;!(query (member 5 (Cons 1 (Cons 2 (Cons 3 Nil))))) ;; Should fail
;!(query (member $x (Cons 1 (Cons 2 (Cons 3 Nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SOLVING FOR THE ZEBRA OWNER ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!(query (backward-chain (zebra-owner $Owner) &self &self)) ;; Solve for $Owner

;; Query for the zebra owner (who owns the fish)
!(query (zebra-owner $Owner)) ;; Solves for the owner of the fish

!(query (declare nationality $Brit brit))

!(query (something-existing $Exists))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL LOGICAL RELATIONSHIP QUERIES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verify properties related to the zebra-owner problem
!(query (declare nationality $Owner norwegian))

