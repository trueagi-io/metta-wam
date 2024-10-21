
(= (check ($x $y $x)) ($x $y))
! (check (B A B)) ; reduced to (B A)
! (check (B A A)) ; not reduced

