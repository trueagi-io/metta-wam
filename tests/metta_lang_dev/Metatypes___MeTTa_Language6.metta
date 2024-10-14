
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
(: my-if (-&gt; Bool Atom Atom Atom))
(= (my-if True $then $else) $then)
(= (my-if False $then $else) $else)
(= (loop) (loop))
(= (OK) OK!)
! (my-if (&gt; 0 1) (loop) (OK))

;  !(assertEqualToResult ( )  (  )  ) 
