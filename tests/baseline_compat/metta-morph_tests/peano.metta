(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

!(Add (S (S Z)) (S (S (S Z))))
