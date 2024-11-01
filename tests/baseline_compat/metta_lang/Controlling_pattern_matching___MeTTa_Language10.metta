
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (age Bob) 5)
(= (age Sam) 8)
(= (age Ann) 3)
(= (age Tom) 5)
(= (of-same-age $who)
   (let $age (age $who)
        (match &self (= (age $other) $age)
               $other)))
; ! (of-same-age Bob) 
; [Bob, Tom]
!(println "1")
!(assertEqualToResult (  of-same-age Bob )  (  Bob  Tom  )  ) 
; without `of-same-age`:
;! (let $age (age Bob)
;       (match &self (= (age $other) $age)
;              $other)) 
              ; also [Bob, Tom]

!(println "2")
; ready works
!(assertEqualToResult ( let $age (age Bob) (match &self (= (age $other) $age) $other) ) (  Bob Tom  ) ) 
;! (match &self (= (age $other) (age Bob))
;         $other) 
         ; does not pattern-match
; evaluating the whole pattern is a bad idea
;!(assertEqualToResult ( )  (  )  ) 

;! (match &self (= (age $other) (age Bob))
;         $other) ; does not pattern-match

!(println "3")
; ready works
!(assertEqualToResult ( match &self (= (age $other) (age Bob)) $other ) ( () ) ) 

;! (let $pattern (= (age $other) (age Bob))
;       $pattern) 
       ; [(= 5 5), (= 8 5), (= 5 5), (= 3 5)]
!(println "4")
; ready works
!(assertEqualToResult ( let $pattern (= (age $other) (age Bob)) $pattern ) ( ( (= 5 5) (= 8 5) (= 5 5) (= 3 5) ) ) ) 

;!(assertEqualToResult ( )  (  )  ) 
;! (let $pattern (= (age $other) (age Bob))
;       (match &self $pattern $other)) 
       ; does not pattern-match
!(println "5")
; ready works
!(assertEqualToResult ( let $pattern (= (age $other) (age Bob)) (match &self $pattern $other) ) ( () ) ) 

; !(assertEqualToResult ( )  (  )  )       
;  !(assertEqualToResult ( )  (  )  ) 
