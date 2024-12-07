
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
 ((same $ctx ($Houses $_U))
   (member $Exists $Houses)))

;; Member predicate for lists
(member $Elem ($Cons $Elem $_U))
((member $Elem ($Cons $_U $Tail)) :-
 ((member $Elem $Tail)))

;; Map property names to their positions (numbers)
((get-declare-num $PropName $Num in the $ctx) :-
  ((same ($_U $PropList) $ctx)
   (declare-index $PropName $PropList Z $Num)))

((get-declare-num-not-working $PropName $Num in the $ctx) :-
  ((same ($_U $PropList) $ctx)
   (eval= $dc (declare-index-m $PropName $PropList Z $Num))))

(: declare-index-m (-> Symbol Expression Nat Nat Bool))
(= (declare-index-m $PropName $PropList $Acc $Num)
  (case $PropList
	((($Cons $PropName $_U) (nat-equal $Acc $Num))
     (($Cons $_U $Rest) (declare-index-m $PropName $Rest (S $Acc) $Num)))))

(declare-index $PropName ($Cons $PropName $_U) $Num $Num)
((declare-index $PropName ($Cons $Nonvar $Rest) $Acc $Num) :- ( (nonvar $Nonvar) (declare-index $PropName $Rest (S $Acc) $Num)))



!(query (is $x (+ 1 1)))
!(query (same $ctx ($Houses $Props)))
!(query (context $ctx))

