

(= (brother Mike) Tom)
(= (brother Sam) Bob)
! (brother $x) 
; just Tom and Bob are returned
! ((brother $x) is the brother of $x) 
; the binding for $x is not lost
; Fault 
;[Bob, Tom]
;[(Tom is the brother of Mike), (Bob is the brother of Sam)]

; mettalog gives :
;[ (Tom is the brother of $_473784)
;, (Bob is the brother of $_473688)]