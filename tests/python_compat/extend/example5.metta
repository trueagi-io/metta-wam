!(extend-py! mettamorph)
!(compile! "
(= (Add $x Z) $x)
(= (Add $x (S $y)) (Add (S $x) $y))

(= (Sub) 2)
(= (Sub) 3)
(= (Mul) (superpose (4 5)))
")

!(Add (S (S (S Z))) (S (S (S Z))))
!(Sub)
!(Mul)
