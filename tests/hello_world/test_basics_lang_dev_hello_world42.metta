

! (conclude (Parent Bob $y))
! (match &self (Implies (Parent Bob $y) $B)
          (match &self (Parent Bob $y) $B))
! (match &self (Parent Bob $y) (Child $y Bob))
! (Child Ann Bob)


[$_5396

]
[]
[(Child Ann Bob)]
[(Child Ann Bob)]
% 23,717 inferences, 0.020 CPU in 0.021 seconds (95% CPU, 1192103 Lips)
true.