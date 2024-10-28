
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(: swap-arguments-atom (-> Atom Atom))
(= (swap-arguments-atom ($op $arg1 $arg2))
   ($op $arg2 $arg1)
)

;! (swap-arguments-atom (- 15 8))
; [-7]



!(println "xx 39 ") 
; notready 
!(assertEqualToResult ( swap-arguments-atom (- 15 8) ) ( -7 ) ) 

;  !(assertEqualToResult ( )  (  )  ) 
