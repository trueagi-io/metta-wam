
; 20241014 r.v.v converted metta-lang.dev to metta 


(Fruit apple)
(= (fruit $x)
   (match &self (Fruit $x) $x))

;!(fruit $x) 
; apple

!(println "xx 8 ") 
; ready works 
!(assertEqualToResult ( fruit $x ) ( apple ) ) 

;!(quote (fruit $x)) 
; (quote (fruit $x))

!(println "xx 9 ") 
; ready works  
!(assertEqualToResult ( quote (fruit $x) ) ( (quote (fruit $x)) ) ) 


;[apple]
;[(quote (fruit $x))]


(: trace-eval (-> Atom Atom))
(= (trace-eval $expr)
   (let $result $expr
        (trace! (EVAL: (quote $expr) --> $result)
                $result)))

;!(trace-eval (+ 5 7))
; [(EVAL:    (quote 12) --> 12)]

!(println "xx 10 ") 
; ready works  
!(assertEqualToResult ( trace-eval (+ 5 7) ) ( 12 ) ) 

;(Fruit apple)
;(= (fruit $x)
;   (match &self (Fruit $x) $x))
; (EVAL: (quote (fruit $x)) --> apple) is printed to stdout
;! (Overall result is (trace-eval (fruit $x))) ; (Overall result is apple)


;[STDOUT] (EVAL: (quote (fruit $x)) --> apple)
;[(Overall result is apple)]
