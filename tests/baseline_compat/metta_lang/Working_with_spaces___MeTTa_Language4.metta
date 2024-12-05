
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(link A B)
(link B C)
(link C A)
(link C E)

;! (match &self (, (link $x $y)
;                  (link $y $z)
;                  (link $z $x))
;               (let () (remove-atom &self (link $x $y))
;                       (add-atom &self (link $y $x)))
;  ) 

; [(), (), ()]


!(println "xx 108 ") 
 ; ready works 
!(assertEqualToResult ( match &self (, (link $x $y) (link $y $z) (link $z $x)) (let () (remove-atom &self (link $x $y)) (add-atom &self (link $y $x))) ) ( () () () ) ) 


;! (match &self (link $x $y)
;    (link $x $y)) 
; [(link A C), (link C B), (link B A), (link C E)]


!(println "xx 109 ") 
; notready 
!(assertEqualToResult ( match &self (link $x $y) (link $x $y) ) ( (link A C) (link C B) (link B A) (link C E) ) ) 


;[(), (), ()]
;[(link B A), (link C E), (link C B), (link A C)]

;  !(assertEqualToResult ( )  (  )  ) 
