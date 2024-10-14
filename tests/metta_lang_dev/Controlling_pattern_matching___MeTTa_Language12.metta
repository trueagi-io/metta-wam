
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (message-to-user $task)
   (Today you have $task))
(= (add-task-and-notify $task)
    (let () (add-atom &amp;self (TASK $task))
            (message-to-user $task))
)
! (get-type add-atom) ; (-&gt; hyperon::space::DynSpace Atom (-&gt;))
! (add-task-and-notify (Something to do))
! (match &amp;self (TASK $t) $t) # (Somthing to do)

;  !(assertEqualToResult ( )  (  )  ) 
