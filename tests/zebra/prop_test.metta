
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

(declare-index $PropName ($Cons $PropName $_) $Num $Num)
((declare-index $PropName ($Cons $_ $Rest) $Acc $Num) :- ((declare-index $PropName $Rest (S $Acc) $Num)))
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
			 (() (println! (IN $cur ))) ;; $goal
			($cur (backward-chain $cur $kb $rb))  ;; chain on cur & unify back to propagate vars to $goal
			 (() (println! (OUT $cur))) ;; $goal
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


!(query (context $ctx))

