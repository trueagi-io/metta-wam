
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(= (message-to-user $task)
   (Today you have $task))

(= (add-task-and-notify $task)
    (let () (add-atom &self (TASK $task))
            (message-to-user $task))
)



;! (get-type add-atom) 
; [(-> hyperon::space::DynSpace Atom (->))]
!(println "xx1") 
 ; ready works  
!(assertEqualToResult ( get-type add-atom ) ( ( -> hyperon::space::DynSpace Atom (->) ) ) ) 


;! (add-task-and-notify (Something to do))
; [(Today you have (Something to do))]
!(println "xx2") 
 ; ready works  
!(assertEqualToResult ( add-task-and-notify (Something to do) ) ( ( Today you have (Something to do) ) ) ) 

;p! (match &self (TASK $t) $t) 
;[(Something to do)]

!(println "xx3") 
; ready works 
!(assertEqualToResult ( match &self (TASK $t) $t ) ( ( Something to do ) ) ) 








;  !(assertEqualToResult ( )  (  )  ) 
