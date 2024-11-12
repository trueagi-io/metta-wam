
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 
! (quote (fruit $x)) ; (quote (fruit $x))(: trace-eval (-&gt; Atom Atom))
(= (trace-eval $expr)
   (let $result $expr
        (trace! (EVAL: (quote $expr) --&gt; $result)
                $result)))
(Fruit apple)
(= (fruit $x)
   (match &amp;self (Fruit $x) $x))
; (EVAL: (quote (fruit $x)) --&gt; apple) is printed to stdout
! (Overall result is (trace-eval (fruit $x))) ; (Overall result is apple)

;  !(assertEqualToResult ( )  (  )  ) 
