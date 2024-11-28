
(Parent Tom Bob)
(Parent Pam Bob)
(Parent Tom Liz)
(Parent Bob Ann)
(Parent Bob Pat)
(Parent Pat Pat)

! (match &self (Parent $x $y) ($x $y))


;[(Pat Pat), (Pam Bob), (Bob Ann), (Bob Pat), (Tom Liz), (Tom Bob)]


;[(Bob Ann), (Tom Bob), (Pam Bob), (Tom Liz), (Pam (Parent Bob Pat))
;, (Bob Pat), (Pat Pat)]
;% 36,343 inferences, 0.038 CPU in 0.039 seconds (96% CPU, 963183 Lips)