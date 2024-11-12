

(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(= (get-parent-entries $x $y)
   (match &self (Parent $x $y) (Parent $x $y)))
(= (get-parents $x)
   (match &self (Parent $y $x) $y))
! (get-parent-entries Tom $_)
! (get-parents Bob)


;[(Parent Tom Bob), (Parent Tom Liz)]
;[Pam, Tom]


;[(Parent Tom Bob), (Parent Tom Liz)] [Tom , Pam]
;% 31,052 inferences, 0.031 CPU in 0.032 seconds (95% CPU, 1001035 Lips)