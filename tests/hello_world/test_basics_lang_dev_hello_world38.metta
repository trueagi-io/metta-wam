


(Parent Tom Bob)
(Parent Bob Ann)
(Implies (Parent $x $y) (Child $y $x))
(= (deduce $B)
   (match &self (Implies $A $B)
          (match &self $A $B))
)
(= (conclude $A)
   (match &self (Implies $A $B)
          (match &self $A $B))
)
! (deduce (Child $x Tom))    ; [(Child Bob Tom)]
! (conclude (Parent Bob $y)) ; [(Child Ann Bob)]




;[(Child Bob Tom)]
;[(Child Ann Bob)]


;[(Child Bob Tom)]
;[(Child Ann Bob)]
;% 30,100 inferences, 0.034 CPU in 0.035 seconds (95% CPU, 891939 Lips)