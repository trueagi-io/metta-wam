
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 

(= (main $space)
   (let () (add-atom $space (Parent Bob Ann))
     (match $space (Parent $x $y) ($x $y))
   )
)


; ! (main (new-space)) 
; (Bob Ann)


!(println "xx 113 ") 
; notready 
!(assertEqualToResult ( main (new-space) ) ( (Bob Ann) ) ) 


; [(Bob Ann)]



;  !(assertEqualToResult ( )  (  )  ) 
