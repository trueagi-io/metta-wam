
; !(bind! &exists-props (new-state Nil))
((context $ctx) :-
 (

   (same $Props (Props $_H6 $_T6))
   (same $ctx ($Houses $Props))

   (get-declare-num nationality $Num1 in the $ctx)))


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
   (eval (declare-index $PropName $PropList Z $Num))))

(: declare-index (-> Symbol Expression Nat Nat Bool))
(= (declare-index $PropName $PropList $Acc $Num)
  (case $PropList
	((($Cons $PropName $_) (nat-equal $Acc $Num))
     (($Cons $_ $Rest) (declare-index $PropName $Rest (S $Acc) $Num)))))

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

(= (backward-chain-hide $info $goal $kb $rb)
     (if (has-fundef $kb $goal) $goal
	    (let $r $goal (if (equalz $r $goal) (empty) $r))))

;; Base case
(= (backward-chain $info $goal $kb $rb)
    (case $goal (
        ((is $a $b) (let $a $b (is $a $b))) ;; prolog eval & assign
        ((bool $expr) (if $expr $goal (empty))) ;; if predicate is a boolean. This might be unnecessary
        ((naf $expr) (if (has-match $kb $expr) (empty) $goal)) ;; negation-as-failure
        ($_ (match $kb $goal $goal))
    )) 
)

;; Recursive Case
(= (backward-chain $info $goal $kb $rb)
    (match $rb ($goal :- $body)
        (match-body $info $body $kb $rb $goal))
)

;; do matching for each atom in the body, then returns goal
(= (match-body $info $body $kb $rb $goal)
    (if (== $body ())
        $goal
        (let* (
            (($cur $rest) (decons-atom $body))
            (() (println! (IN $cur $goal)))
            ($cur (backward-chain $info $cur $kb $rb))  ;; chain on cur & unify back to propagate vars to $goal
            (() (println! (OUT $cur $goal)))
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
!(query (same $ctx ($Houses $Props)))
!(query (context $ctx))


