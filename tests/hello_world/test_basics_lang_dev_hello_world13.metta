
(= (Car (Cons $x $xs)) $x)
(= (Second (Cons $x $xs)) (Car $xs))
! (Second (Cons A (Cons B Nil))) ; outputs B


; [B]
