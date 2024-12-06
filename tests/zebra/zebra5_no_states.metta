
; !(bind! &exists-props (new-state Nil))
((context $ctx) :-
 (

   (same $Props (Props $_H6 $_T6))
   (same $ctx ($Houses $Props))

   (get-declare-num nationality $Num1 in the $ctx)
   (get-declare-num color $Num2 in the $ctx)
   (get-declare-num pet $Num3 in the $ctx)
   (get-declare-num drink $Num4 in the $ctx)
   (get-declare-num drink $Num5 in the $ctx) ; this should not change the context since its already there
   (get-declare-num smokes $Num6 in the $ctx)))


;; Main predicate to find the owner of the fish
((zebra-owner $Owner) :-
 (
   ; (context $ctx)
   (same $Houses 
      (Places (House $__202438 $_39) 
      (Places (House $__208019 $_1_208020) 
      (Places (House $_1968 $_11969) 
      (Places (House $_5037 $_15038) 
      (Places (House $_7602 $_17603) Nil))))))
       
   (same $Props 
      (Props nationality
      (Props color
      (Props pet
      (Props drink
      (Props smokes Nil))))))      
   
   (same $ctx ($Houses $Props))
      
 ;; Clue 8: The man living in the center house drinks milk.
   (center-house $CenterHouse in the $ctx)
   (declare drink $CenterHouse milk in the $ctx)   
   
 ;; Clue 9: The Norwegian lives in the first house.
   (first-house $FirstHouse in the $ctx)
   (declare nationality $FirstHouse norwegian in the $ctx)

 ;; Clue 14: The Norwegian lives next to the blue house.
   (declare nationality $Norwegian norwegian in the $ctx)
   (next-to $Norwegian $HouseBlue in the $ctx)
   (declare color $HouseBlue blue in the $ctx)

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

;; Member predicate for lists
(member $Elem ($Cons $Elem $_))
((member $Elem ($Cons $_ $Tail)) :-
 ((member $Elem $Tail)))

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
;((declare-index $PropName ($Cons $_ $Rest) $Acc $Num) :- ((declare-index $PropName $Rest (S $Acc) $Num)))
;((declare-index $PropName $PropList $Acc $Acc) :-  ;; If property not found, add it
;   ((cons-append $_PropList ($Cons $PropName $_Open) $PropList)))

;; Zero-based nat-nth predicate
(nat-nth Z ($Cons $Elem $_) $Elem)
((nat-nth (S $N) ($Cons $_ $Rest) $Elem) :-
 ((nat-nth $N $Rest $Elem)))

;; Append two lists
(cons-append Nil $List2 $List2)
((cons-append ($Cons $H $T) $List2 ($Cons $H $T2)) :-
 ((cons-append $T $List2 $T2)))


(same $x $x)

;; last of list
(last $x ($Cons $x Nil))
((last $x ($Cons $_ $rest)) :- ( (last $x $rest) ) )

;; length list
(length Nil 0)
((length ($Cons $x $rest) $out) :- 
   ((length $rest $out2) 
	(is $out (+ $out2 1))))

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


(: cur= (-> Atom Atom))
(: retVal= (-> Atom Atom))
(: goal= (-> Atom Atom))

(: backward-chain (-> Atom Atom Atom Atom Atom))
(: backward-chain-q (-> Atom Atom Atom Atom Atom))
(: match-body (-> Atom Atom Atom Atom Atom Atom))
(: has-match (-> Atom Atom Bool))
(: has-fundef (-> Atom Atom Bool))
;; First-order logic backward chainer using match


(: Z Nat)
(: S (-> Nat Nat))
	  
(: nat-equal (-> Nat Nat Bool))
(= (nat-equal $A $A) True)

(: equalz (-> Atom Atom Bool))
(= (equalz $A $A) True)

(: quote-equalz (-> Atom Atom Bool))
(= (quote-equalz $A $A) True)
(= (quote-equalz $A $B) (equalz (quote $A) $B))
(= (quote-equalz $A $B) (equalz $A (quote $B)))

;; TODO: this is inefficient, we just need at least 1 match
(= (has-match $space $g)
	(let $m (collapse (match $space $g True))
		(if (== $m ())
			False
			True
		)
	)
)
(= (has-fundef $space $g)
	(let $m (collapse (match $space (= $g $_) True))
		(if (== $m ())
			False
			True
		)
	)
)


(= (backward-chain $info $goal $kb $rb)
  (backward-chain-q $info (quote $goal) $kb $rb))

;; Base case
(= (backward-chain-q $info (quote $goal) $kb $rb)
	(case (quote $goal) (
		((quote (is $a $b)) (let $a $b (quote $goal))) ;; prolog eval & assign
		((quote (bool $expr)) (if $expr (quote $goal) (empty))) ;; if predicate is a boolean. This might be unnecessary
		((quote (eval= $a $expr)) (let $a $expr (quote $goal))) ;; if predicate is a boolean. This might be unnecessary
		((quote (nonvar $var)) (if (== Variable (get-metatype $var)) (empty) (quote $goal)))
        ((quote (var $var)) (if (== Variable (get-metatype $var)) (quote $goal) (empty)))
		((quote (true)) (quote $goal))
		((quote (fail)) (empty))
	    ((quote (cut $_)) (if (equalz $info $goal) (quote $goal) (quote $goal)))
		((quote (naf $expr)) (if (has-match $kb $expr) (empty) (quote $goal))) ;; negation-as-failure
		($_ (match $kb $goal (quote $goal)))
	)) 
)

;; Recursive Case
(= (backward-chain-q $info  (quote $goal) $kb $rb)
	(match $rb ($goal :- $body)
		(match-body $info $body $kb $rb $goal))
)

(= (backward-chain $info (quote $goal) $kb $rb)
	 (if (has-fundef $kb $goal) $goal
		(let $r $goal (if (equalz $r $goal) (empty) $r))))


;; do matching for each atom in the body, then returns goal
(= (match-body $info $body $kb $rb $goal)
	(if (== $body ())
		(quote $goal)
		(let* (
			(($cur $rest) (decons-atom $body))
			(() (println! (IN (cur= $cur) (goal= $goal))))
		    ($retVal (backward-chain $info $cur $kb $rb))
			($m (collapse (equalz (quote $cur) $retVal)))
		    (() (if (== $m ()) (println! (BAD (cur= $cur) (retVal= $retVal))) ()))
		    ((quote $cur) $retVal) ;; chain on cur & unify back to propagate vars to $goal
			(() (println! (OUT (cur= $cur) (goal= $goal))))
		    ) 
			(match-body $info $rest $kb $rb $goal)
		)
    )
)


(: query (-> Atom Atom))
; !(bind! &baseall (new-space))
(= (query $goal)
    (let $m (collapse (backward-chain $info $goal &self &self))
		(if (== $m ()) (Fail $goal) (Succeed $m)))
)
(: query-f (-> Atom Atom))
(= (query-f $goal)
    (let $m (collapse (backward-chain $info $goal &self &self))
		(if (== $m ()) Passed-Negation (Failed $m)))
)

;;;;;;;;;;;;;;;;;
;; TEST query ;;;
;;;;;;;;;;;;;;;;;;

!("
; last element of list
!(query (last $x ($Cons 1 ($Cons 2 ($Cons 3 Nil)))))

;; length of list
!(query (length ($Cons 1 ($Cons 2 ($Cons 3 Nil))) $out))

;; sum up to 3
!(query (sum 3 $x))

;; append (multi-directional)
!(query (append ($Cons 1 ($Cons 2 Nil)) ($Cons 3 ($Cons 4 Nil)) $x))
!(query (append ($Cons 1 ($Cons 2 Nil)) $Out ($Cons 1 ($Cons 2 ($Cons 3 ($Cons 4 Nil))))))
!(query (append $What $Out ($Cons 1 ($Cons 2 ($Cons 3 ($Cons 4 Nil))))))
")

;; Negation as failure test
!(query (bachelor $x))

!(query-f (woman Tim)) ;; should be no
!(query (woman Jane)) ;; should succeed
!(query-f (woman $x)) ;; should be no


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL TEST QUERIES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check if an element is a member of a list
!(query (member 3 ($Cons 1 ($Cons 2 ($Cons 3 Nil)))))
!(query-f (member 5 ($Cons 1 ($Cons 2 ($Cons 3 Nil))))) ;; Should fail
!(query (member $x ($Cons 1 ($Cons 2 ($Cons 3 Nil)))))

!(query (is $x (+ 1 1)))
; !(query!(query (context $ctx))
;!(query (backward-chain (zebra-owner $Owner) &self &self)) ;; Solve for $Owner

!(query (nat-nth (S (S Z)) (SomethingAtThird $1 $2) Third))

;; Query for the zebra owner (who owns the fish)
!(query (zebra-owner $Owner)) ;; Solves for the owner of the fish

!("

;!(query (declare nationality $Brit brit))

;!(query (something-existing $Exists))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDITIONAL LOGICAL RELATIONSHIP QUERIES ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verify properties related to the zebra-owner problem
!(query (declare nationality $Owner norwegian))

")

