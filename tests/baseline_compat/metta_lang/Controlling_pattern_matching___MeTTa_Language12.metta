
; 20241014 r.v.v converted metta-lang.dev to metta 
(= (age Bob) 5)
(= (age Sam) 8)
(= (age Ann) 3)
(= (age Tom) 5)
(= (of-same-age $who)
   (let $age (age $who)
        (match &self (= (age $other) $age)
               $other)))


;! (of-same-age Bob) 
; [Bob, Tom]

!(println "xx 27 ") 
 ; ready works 
!(assertEqualToResult ( of-same-age Bob ) ( Bob Tom ) ) 


; without `of-same-age`:
;! (let $age (age Bob)       (match &self (= (age $other) $age)              $other)) 
; [Bob, Tom]

!(println "xx 28 ") 
 ; ready works  
!(assertEqualToResult ( let $age (age Bob) (match &self (= (age $other) $age) $other) ) ( Bob Tom ) ) 


;! (match &self (= (age $other) (age Bob))         $other) 
; does not pattern-match

!(println "xx 29 ") 
 ; ready works  
!(assertEqualToResult ( match &self (= (age $other) (age Bob)) $other ) (  ) ) 



; evaluating the whole pattern is a bad idea
;! (let $pattern (= (age $other) (age Bob))       $pattern) 
; [(= 5 5), (= 8 5), (= 5 5), (= 3 5)]

!(println "xx 30 notready") 
 ; notready 
!(assertEqualToResult ( let $pattern (= (age $other) (age Bob)) $pattern ) ( (= 5 5) (= 8 5) (= 5 5) (= 3 5) ) ) 


;! (let $pattern (= (age $other) (age Bob))       (match &self $pattern $other)) 
; does not pattern-match

!(println "xx 31 ") 
; ready works 
!(assertEqualToResult ( let $pattern (= (age $other) (age Bob)) (match &self $pattern $other) ) ( ) ) 



(= (message-to-user $task)   (Today you have $task))

(= (add-task-and-notify $task)
    (let () (add-atom &self (TASK $task))  (message-to-user $task)) )

;! (get-type add-atom) 
; (-> hyperon::space::DynSpace Atom (->))

!(println "xx 32 ") 
 ; ready works 
!(assertEqualToResult ( get-type add-atom ) ( (-> hyperon::space::DynSpace Atom (->)) ) ) 


;! (add-task-and-notify (Something to do))
;[(Today you have (Something to do))]

!(println "xx 33 ") 
 ; ready works  
!(assertEqualToResult ( add-task-and-notify (Something to do) ) ( (Today you have (Something to do)) ) ) 

;! (match &self (TASK $t) $t) # (Somthing to do)
;[(Something to do)]

!(println "xx 34 ") 
 ; ready works 
!(assertEqualToResult ( match &self (TASK $t) $t  (Somthing to do ) )   ((Something to do))  ) 













