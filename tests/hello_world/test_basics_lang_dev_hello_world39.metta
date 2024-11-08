
! (unify (parent $x Bob) ; the first pattern
         (parent Tom $y) ; the second pattern
         ($x $y) ; the output for successful unification
         Fail) ; fallback


; [(Tom Bob)]

; Fault 
;[]
;% 9,952 inferences, 0.011 CPU in 0.012 seconds (95% CPU, 871538 Lips)