
; 20241014 r.v.v converted metta-lang.dev to metta 

(Sam is Frog)
(Apple is Green)

(= (test $who)
   (case (match &self ($who is $x) $x)
    (
        (42   "The answer is 42!")
        (Frog "Do not ask me about frogs")
        ($a   ($who is $a))
    )))

;! (test Sam) 
; ["Do not ask me about frogs"]


!(println "xx 20 ") 
; ready works 
!(assertEqualToResult ( test Sam ) ( "Do not ask me about frogs" ) ) 


;! (test Apple) 
; [(Apple is Green)]

!(println "xx 21 ") 
 ; ready works  
!(assertEqualToResult ( test Apple ) ( (Apple is Green) ) ) 


;! (test Car) 
; [empty]

!(println "xx 22 ") 
 ; ready works  
!(assertEqualToResult ( test Car ) ( ) ) 



;["Do not ask me about frogs"]
;[(Apple is Green)]
;[]

