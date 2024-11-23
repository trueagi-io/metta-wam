
; 20241014 r.v.v converted metta-lang.dev to metta 


(: swap-arguments-atom (-> Atom Atom))
(= (swap-arguments-atom ($op $arg1 $arg2))
   ($op $arg2 $arg1)
)

; ! (swap-arguments-atom (- 15 8))
;[(swap-arguments 7)]


!(println "xx 41 ") 
 ; ready works 
!(assertEqualToResult ( swap-arguments-atom (- 15 8) ) ( (swap-arguments-atom 7) ) ) 


