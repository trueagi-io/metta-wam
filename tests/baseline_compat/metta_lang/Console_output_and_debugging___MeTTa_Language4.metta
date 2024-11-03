
; 20241014 r.v.v converted metta-lang.dev to metta 

(= (add-bin $x)
   (let $r (+ $x 1)
        (trace! (quote ((add-bin $x) is $r))
                $r)))

(= (add-bin $x)
   (trace! (quote ((add-bin $x) is $x))
           $x))
; (quote ((add-bin 1) is 1)) and (quote ((add-bin 1) is 2)) will be printed

;! (add-bin 1) 
; [1, 2]

!(println "xx 11 ") 
; ready works
!(assertEqualToResult ( add-bin 1 ) ( 1 2 ) ) 


;[STDOUT] (quote ((add-bin 1) is 2))
;[STDOUT] (quote ((add-bin 1) is 1))
;[1, 2]

