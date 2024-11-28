
(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Jim)

(= (get-parents $x)
   (match &self (Parent $y $x) $y))
(= (get-grand-parents $x)
   (...))
! (get-grand-parents Pat)

; [(...)]


;[(...)]
;% 21,921 inferences, 0.023 CPU in 0.024 seconds (95% CPU, 943313 Lips)