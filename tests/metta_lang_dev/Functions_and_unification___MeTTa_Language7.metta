
; 20241014 r.v.v converted metta-lang.dev to metta 

(Parent Tom Bob)
(Parent Bob Ann)

;(Parent Tom Bob)
;(Parent Pam Bob)
;(Parent Tom Liz)
;(Parent Bob Ann)
;(Parent Bob Pat)
;(Parent Pat Jim)


;! (unify (Parent $x Bob) (Parent Tom $y) ($x $y)   Fail ) 
;[(Tom Bob)]
!(println "xx 36 ") 
; notready 
!(assertEqualToResult ( unify (Parent $x Bob) (Parent Tom $y) ($x $y) Fail ) ( (Tom Bob) ) ) 
;! (unify &self (Parent $x Bob) $x Fail) 
; [Tom]

!(println "xx 37 ") 
; ready works 
!(assertEqualToResult ( unify &self (Parent $x Bob) $x Fail ) ( Tom ) ) 


(= (conclude (Parent Bob $y))   $result)
(= (conclude       $A)   (match &self (Implies $A $B)          (match &self $A $B)))

; $result will be 
;!(match &self (Implies (Parent Bob $y) $B)   (match &self (Parent Bob $y) $B))
;[]

!(println "xx 38 ") 
 ; ready works 
!(assertEqualToResult ( match &self (Implies (Parent Bob $y) $B) (match &self (Parent Bob $y) $B) ) ( ) ) 


;(Implies (Parent Bob $y)      $B      )
;(Implies (Parent $x  $y) (Child $y $x))
; The overall chain of transformations in the course of interpretation can be viewed as:
;;1. (conclude (Parent Bob $y))
;2. (match &self (Implies (Parent Bob $y) $B)
;          (match &self (Parent Bob $y) $B))
;3. (match &self (Parent Bob $y) (Child $y Bob))
;4. (Child Ann Bob)