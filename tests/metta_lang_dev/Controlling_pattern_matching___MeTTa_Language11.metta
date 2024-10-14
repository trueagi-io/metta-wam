
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (let $x (get-type Z) (get-metatype $x)) ; Nat is Symbol(= (age Bob) 5)
(= (age Sam) 8)
(= (age Ann) 3)
(= (age Tom) 5)
(= (of-same-age $who)
   (let $age (age $who)
        (match &amp;self (= (age $other) $age)
               $other)))
! (of-same-age Bob) ; [Bob, Tom]
; without `of-same-age`:
! (let $age (age Bob)
       (match &amp;self (= (age $other) $age)
              $other)) ; also [Bob, Tom]
! (match &amp;self (= (age $other) (age Bob))
         $other) ; does not pattern-match
; evaluating the whole pattern is a bad idea
! (let $pattern (= (age $other) (age Bob))
       $pattern) ; [(= 5 5), (= 8 5), (= 5 5), (= 3 5)]
! (let $pattern (= (age $other) (age Bob))
       (match &amp;self $pattern $other)) ; does not pattern-match

;  !(assertEqualToResult ( )  (  )  ) 
