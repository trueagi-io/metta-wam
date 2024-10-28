
; 20241014 r.v.v converted metta-lang.dev to metta 
;  !(assertEqualToResult ( )  (  )  ) 


(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Jim)
(Parent Jim Lil)

(= (parent $x $y) (match &self (Parent $x $y) $x))
(= (predecessor $x $z) (parent $x $z))
(= (predecessor $x $z) (predecessor $x (parent $y $z)))
; Who are predecessors of Lil
; ! (predecessor $x Lil)
; [Pat, Pam, Tom, Bob, Jim]



!(println "xx 43 ") 
 ; notready   HANGS HANGS
!(assertEqualToResult ( predecessor $x Lil ) ( Pat Pam Tom Bob Jim ) ) 

;  !(assertEqualToResult ( )  (  )  ) 
