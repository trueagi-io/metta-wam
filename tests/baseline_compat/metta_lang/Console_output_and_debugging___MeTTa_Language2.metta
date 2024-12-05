
; 20241014 r.v.v converted metta-lang.dev to metta 

;! (println! "This is a string")
;! (println! ($v1 "string" 5))
;[STDOUT] "This is a string"
;[STDOUT] ($v1 "string" 5)


;[()]
;[()]


(Parent Bob Ann)

;! (match &self (Parent Bob Ann) (Ann is Bob`s child))
;[(Ann is Bob`s child)]
!(println "xx 1 ") 
; ready works  
!(assertEqualToResult ( match &self (Parent Bob Ann) (Ann is Bob`s child) ) ( (Ann is Bob`s child) ) ) 

;!(match &self (Parent Bob Ann) (Bob is Ann`s parent))
;[STDOUT] (Bob is Ann`s parent)
!(println "xx 2 ") 
; ready works 
!(assertEqualToResult ( match &self (Parent Bob Ann) (Bob is Ann`s parent) ) ( (Bob is Ann`s parent) ) ) 


;[()]

;! (get-type (trace! (Expecting 3) (+ 1 2))) 
;; Number

!(println "xx 3 ") 
; ready works  
!(assertEqualToResult ( get-type (trace! (Expecting 3) (+ 1 2)) ) ( Number ) ) 


;[Number, Number]

(: my-trace (-> %Undefined% $a $a))
(= (my-trace $out $res)   (let () (println! $out) $res))

;! (my-trace (Expecting 3) (+ 1 2))
;; (Expecting 3) (3)


!(println "xx 5 ") 
; ready works 
!(assertEqualToResult ( my-trace (Expecting 3) (+ 1 2) ) ( (3) ) ) 



;(Parent Bob Ann)
;! (trace! "Who is Anna`s parent?" 
;   (match &self (Parent $x Ann)          ($x is Ann`s parent)))  
;[(Bob is Ann`s parent)]

!(println "xx 6 ") 
; ready works  
!(assertEqualToResult ( trace! "Who is Anna`s parent?" (match &self (Parent $x Ann) ($x is Ann`s parent)) ) ( (Bob is Ann`s parent) ) ) 

;"Who is Anna`s parent?"
;!(trace! "Who is Bob`s child?"  
;   (match &self (Parent Bob $x)          ($x is Bob`s child))) 
; [(Ann is Bob`s child)]


!(println "xx 7 ") 
; ready works 
!(assertEqualToResult ( trace! "Who is Bob`s child?" (match &self (Parent Bob $x) ($x is Bob`s child)) ) ( (Ann is Bob`s child) ) ) 




;  "Who is Bob`s child?"


;[STDOUT] 
;[(Bob is Ann`s parent)]
;[(Ann is Bob`s child)]
;(Parent Bob Ann)

;!(trace! ((Expected: (Bob is Ann`s parent))
;           (Got: (match &self (Parent $x Ann) ($x is Ann`s parent)))     )
;   ())
; [ (Expected: (Bob is Ann`s parent)) (Got: (Bob is Ann`s parent)) ]
;!(println "xx 4 ") 
; ready works notready 
;!(assertEqualToResult ( trace! ((Expected: (Bob is Ann`s parent)) (Got: (match &self (Parent $x Ann) ($x is Ann`s parent))) ) () ) ( (Expected: (Bob is Ann`s parent)) (Got: (Bob is Ann`s parent)) ) ) 



  
