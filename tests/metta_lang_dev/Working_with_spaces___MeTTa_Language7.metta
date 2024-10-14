
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(= (main $space)
   (let () (add-atom $space (Parent Bob Ann))
     (match $space (Parent $x $y) ($x $y))
   )
)

;  !(assertEqualToResult ( )  (  )  ) 
