
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (get-type (trace! (Expecting 3) (+ 1 2))) ; Number(: my-trace (-&gt; %Undefined% $a $a))
(= (my-trace $out $res)
   (let () (println! $out) $res))

;  !(assertEqualToResult ( )  (  )  ) 
